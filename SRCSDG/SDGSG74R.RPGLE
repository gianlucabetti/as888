000100130906     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND SDG005)
000200000000     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*****************************************************************
000500000000      *
000600130906      *                Spesometro clienti/fornitori
000700000000      *                 Dati di testata
000800000000      *           ---------------------------------------
000900000000      *
001000000000     F*----------------------------------------------------*
001100130906     F* Spesometro - Movimenti estratti
001200130906     FSDGSCF00F UF A E             DISK
001300000000     F*------------
001400130906     FSDGSG74D  CF   E             WORKSTN
001500130906     F                                     RENAME(SG74D1:FMTD1)
001600130906     F                                     RENAME(SG74T1:FMTT1)
001700130906     F                                     RENAME(SG74Z1:FMTZ1)
001800000000     F* eventuali schermi supplementari
001900130906     F                                     RENAME(SG74D2:FMTD2)
002000000000     F                                     INFDS(DSFMT)
002100000000     F*------------
002200000000     D*----------------------------------------------------*
002300000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002400000000     D $ID             S             27    DIM(5) ALT($OZ)
002500000000     D* Schiere per la gestione dei tasti di funzione
002600000000     D $FM             S             27    DIM(24)
002700000000     D $FX             S             50    DIM(24)
002800000000     D $FC             S              1    DIM(24)
002900000000     D $FR             S              1    DIM(24)
003000000000     D* Video D1
003100000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
003200000000     D $FX1            S             50    DIM(24) ALT($FM1)
003300000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003400000000     D $FR1            S              1    DIM(24) ALT($FC1)
003500000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
003600000000     D* funzione personalizzati
003700000000     D $PGF1           S             10    DIM(25)
003800000000     D*----------------------------------------------------*
003900000000     D $D1L01          S              3  0
004000000000     D $D1L02          S              3  0
004100000000     D*-------------
004200000000     D* Passaggio Parametri
004300000000     D KPJBA         E DS
004400000000     D*-------------
004500000000     D* DS Esterna gestione messaggi
004600000000     D SDGMSGDS      E DS
004700000000     D  STS          E                     EXTFLD(MSGSTS)
004800000000     D                                     DIM(333)
004900000000     D  JBA          E                     EXTFLD(MSGJBA)
005000000000     D                                     DIM(502)
005100000000     D*-------------
005200000000     D* Reperimento Autorizzazioni
005300000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
005400000000     D*-------------
005500000000     D* Maschera dati autorizzazioni
005600000000     D CONCIVDS      E DS
005700000000     D*-------------
005800000000     D* Reperimento nome PGM
005900000000     D STATUS         SDS           333
006000000000     D  DSPGM            *PROC
006100000000     D  PARMS            *PARMS
006200000000     D*-------------
006300000000     D* Dati di ambiente ottenuti da SDGSOCDR
006400000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006500000000     D*-------------
006600000000     D* DS Interna per dati di output di SDGSOCDR
006700000000     D XSOCDS          DS          1000
006800000000     D*-------------
006900130906     D* Parametri modulo SDGSPECF
007000130906     D SDGPA08DS     E DS
007001160104R195 D*-------------
007002160104 "   D* Parametri modulo SDBLACK2
007003160104R195 D SDGPA11DS     E DS
007100130906     D*-------------
007200000000     D* Passaggio Parametri al PGM di manutenzione
007300130906     D SDGSG74DS     E DS
007400130906     D*-------------
007500130906     D* Parametri del PGM di lettura movimenti estratti
007600130906     D* Parametri di INPUT
007700130906     D SDG00502DS    E DS                  INZ
007800130906     D  RIC0502      E                     INZ('0')
007900130906     D*----------
008000130906     D* Parametri di OUTPUT
008100130906     D* per manutenzione di testata, quadri e dettaglio
008200130906     D SDG00592DS    E DS                  INZ
008300130906     D*--------------------
008400130906     D* Parametri di OUTPUT
008500130906     D* per lista quadri a video e creazione tracciato ministeriale
008600130906     D*---------
008700130906     D*---------
008800130906     D* formato AGGREGATO
008900130906     D*---
009000130906     D* SOGGETTI NAZIONALI: FATTURE, NOTE E DOCUMENTI RIEPILOGATIVI
009100130906     D*                     (NOLEGGIO/LEASING)
009200130906     D SDG005FADS    E DS                  INZ
009300130906     D*
009400130906     D*---
009500130906     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
009600130906     D SDG005SADS    E DS                  INZ
009700130906     D*
009800130906     D*---
009900130906     D* SOGGETTI NON RESIDENTI E ACQUISTI DI SERVIZI DA NON RESIDENTI
010000130906     D SDG005BLDS    E DS                  INZ
010100130906     D*---------
010200130906     D*---------
010300130906     D* formato DETTAGLIATO
010400130906     D*---
010500130906     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI EMESSI
010600130906     D*                     (NOLEGGIO/LEASING)
010700130906     D SDG005FEDS    E DS                  INZ
010800130906     D*---
010900130906     D* SOGGETTI NAZIONALI: FATTURE E DOCUMENTI RIEPILOGATIVI RICEVUTI
011000130906     D SDG005FRDS    E DS                  INZ
011100130906     D*---
011200130906     D* SOGGETTI NAZIONALI: NOTE EMESSE
011300130906     D SDG005NEDS    E DS                  INZ
011400130906     D*---
011500130906     D* SOGGETTI NAZIONALI: NOTE RICEVUTE
011600130906     D SDG005NRDS    E DS                  INZ
011700130906     D*---
011800130906     D* SOGGETTI NAZIONALI: OPERAZIONI SENZA FATTURA
011900130906     D*                     (NOLEGGIO/LEASING)
012000130906     D SDG005DFDS    E DS                  INZ
012100130906     D*---
012200130906     D* SOGGETTI NON RESIDENTI
012300130906     D*                     (NOLEGGIO/LEASING)
012400130906     D SDG005FNDS    E DS                  INZ
012500130906     D*---
012600130906     D* ACQUISTI DI SERVIZI DA NON RESIDENTI
012700130906     D SDG005SEDS    E DS                  INZ
012800131017R136 D*---
012900131017 "   D* SOGGETTI NON RESIDENTI: OPERAZIONI SENZA FATTURA
013000131017 "   D* LEGATE AL TURISMO
013100131017R136 D SDG005TUDS    E DS
013200130906     D*---
013300130906     D* RIEPILOGO
013400130906     D SDG005TADS    E DS                  INZ
013500000000     D*-------------
013600000000     D* Parametri per ricerca codice ANATB00F
013700000000     D XTABDS        E DS                  inz
013800000000     D*-------------
013900000000     D* Richiamo a SDGTABDR
014000000000     D SDGTABDS      E DS                  inz
014100000000     D*-------------
014200000000     D* Ricerca codici Stato
014300000000     D ANGG12DS      E DS
014400000000     D*-------------
014500000000     D* Ricerca codici province italiane
014600000000     D ANGC17DS      E DS
014700000000     D*-------------
014800000000     D* Campo chiave per tabella C17
014900000000     D KeyC17          S              6A   inz
015000000000     D*-------------
015100000000     D* ricerca soggetti
015200000000     D ANA040DS      E DS                  INZ
015300000000     D*-------------
015400000000     D* DS esterna per dati soggetti
015500000000     D DVASOG        E DS                  INZ
015600000000     D*-------------
015700000000     D* Definizioni parametri driver anagrafici
015800000000     D Dvaric          S              1
015900000000     D DvaSocieta      S              3
016000000000     D DvaUnita        S              8
016100000000     D DvaStrutt       S             10
016200000000     D DvaDtRif        S               D
016300000000     D DvaLenOut       S              9B 0
016400000000     D DvaSnatura      S              1
016500000000     D DvaCodice       S              8
016600000000     D DvaLineav       S              3
016700000000     D DvaFiliale      S              3  0
016800000000     D DvaSogg         S              8
016900000000     D DvaTpInd        S              2
017000000000     D DvaCdInd        S              3
017100000000     D DvaErrore       S              1
017200000000     D DvaOutput       S           4000
017300000000     D*-------------
017400000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
017500000000     D* alla funzione personalizzata
017600130906     D $SaveDS         S                   like(SDGSG74DS)
017700000000     D*-------------
017800000000     D* Ds per ricerca legende
017900000000     D ANA964DS      E DS                  INZ
018000000000     D*-------------
018100000000     D* Controlli validità per valore di una legenda
018200000000     D X07VALDS      E DS                  Inz
018300000000     D*-------------
018400000000     D DSFMT           DS
018500000000     D  $TASTO               369    369
018600000000     D  NRG                  370    370
018700000000     D  NCL                  371    371
018800000000     D* riga cursore window
018900000000     D  NRgWin               382    382
019000000000     D* colonna cursore window
019100000000     D  NClWin               383    383
019200000000     D*-------------
019300000000     D* DS per UserExit
019400000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
019500000000     D*-------------
019600000000     D* posizione cursore
019700000000     D CURSOR          DS
019800000000     D  RIRI                   1      2B 0 INZ
019900000000     D  R$$                    2      2
020000000000     D  COCO                   3      4B 0 INZ
020100000000     D  C$$                    4      4
020200000000     D*-------------
020300000000     D* Costanti
020400000000     D*-------------
020500000000     D* Tasti di funzione
020600000000     D F01             C                   CONST(X'31')
020700000000     D F02             C                   CONST(X'32')
020800000000     D F03             C                   CONST(X'33')
020900000000     D F04             C                   CONST(X'34')
021000000000     D F05             C                   CONST(X'35')
021100000000     D F06             C                   CONST(X'36')
021200000000     D F07             C                   CONST(X'37')
021300000000     D F08             C                   CONST(X'38')
021400000000     D F09             C                   CONST(X'39')
021500000000     D F10             C                   CONST(X'3A')
021600000000     D F11             C                   CONST(X'3B')
021700000000     D F12             C                   CONST(X'3C')
021800000000     D F13             C                   CONST(X'B1')
021900000000     D F14             C                   CONST(X'B2')
022000000000     D F15             C                   CONST(X'B3')
022100000000     D F16             C                   CONST(X'B4')
022200000000     D F17             C                   CONST(X'B5')
022300000000     D F18             C                   CONST(X'B6')
022400000000     D F19             C                   CONST(X'B7')
022500000000     D F20             C                   CONST(X'B8')
022600000000     D F21             C                   CONST(X'B9')
022700000000     D F22             C                   CONST(X'BA')
022800000000     D F23             C                   CONST(X'BB')
022900000000     D F24             C                   CONST(X'BC')
023000000000     D ENTER           C                   CONST(X'F1')
023100000000     D ROLDWN          C                   CONST(X'F4')
023200000000     D ROLLUP          C                   CONST(X'F5')
023300000000     D*-------------
023400000000     D* Sottotitolo di testata
023500000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
023600000000     D                                          COS2504')
023601151201R195 D*-------------
023602151201  "  D* Sottotitolo di testata
023603151201  "  D* "Quadri Spesometro"
023604151201  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
023605151201  "  D                                          SDC0090')
023606151201  "  D* Sottotitolo di testata Black List
023607151201  "  D* "Quadri Acquisti da RSM"
023608151201  "  D SOTTITT2        C                   CONST('SDGMSG    *LIBL-
023609151201  "  D                                          SDC0091')
023610151201  "  D* Sottotitolo di testata Black List
023611151201  "  D* "Quadri Black List"
023612151201  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
023613151201R195 D                                          SDC0092')
023700000000     D*-------------
023800000000     D* Variabili per gestione videate
023900000000     D $Gest           S              2A
024000000000     D $Fine           S              1A
024100000000     D $InzD1          S              1A
024200000000     D $LastG          S              2A
024300000000     D $LastV          S              2A
024400000000     D $FlgDef         S              1A
024500000000     D*-------------
024600000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
024700000000     D CurR            S              3S 0
024800000000     D CurC            S              3S 0
024900000000     D Win             S             99A
025000000000     D*-------------
025100000000     D* Variabili appoggio specifiche della singola anagrafica
025200000000     D*  Indici
025300000000     D X               S              5S 0
025400000000     D Y               S              5S 0
025500000000     D* Indice della schiera tasti funzione
025600000000     D PosFun          S              2  0
025700000000     D*-------------
025800000000     D* Campi di lavoro
025900130930     D WIn16           S              1
026000130926     D WIn17           S              1
026100000000     D UDateISO        S               D
026200000000     D WDtNasc         S               D
026300000000     D WSogg           S                   Like(DVSSogg)
026400000000     D WAnnoAlfa       S              4A
026500000000     D WDtAlfa         S             10A
026600000000     D*------
026700000000     D $Esito          S              1
026800000000     D $Prenot         S              1
026900130906     D $Sys            S                    Like(SCFSys    )
027000130906     D $NrAsReg        S                    Like(SCFNrAsReg)
027100130918     D*-------------
027200130918     D WStato          S                   Like(XScSta )
027300130918     D $CodUIC         S                   Like(CODUICG12)
027400130918     D $UICSoc         S                   Like(CODUICG12)
027500130918     D $NazSoc         S                   Like(NazG12 )
027600130918     D $NazISO         S                   Like(NazG12 )
027700131001     D $FineG12        S              1    Inz(*Off)
027800000000     C*----------------------------------------------------*
027900000000     C*                MAIN LINE PROGRAM
028000000000     C*----------------------------------------------------*
028100000000     C*
028200000000     C* inizializzazione variabili
028300000000     C                   EXSR      INZVAR
028400000000     C*
028500000000     C     $FINE         DOWEQ     *OFF
028600000000     C     $GEST         CASEQ     'D1'          GESD1
028700000000     C     $GEST         CASEQ     'D2'          GESD2
028800000000     C                   ENDCS
028900000000     C                   ENDDO
029000000000     C*
029100000000     C* fine programma
029200000000     C                   EXSR      ENDPGM
029300000000     C/EJECT
029400000000     C************************************************************
029500000000     C* Fine Programma
029600000000     C************************************************************
029700000000     C     ENDPGM        BEGSR
029800000000     C*
029900000000     C* se non esco per errore
030000130906    >C     ErrG74        IFEQ      '0'
030100000000     C*
030200000000     C* se il COMMIT non è demandato al pgm chiamante
030300130906    >C     CMTG74        IFEQ      '0'
030400000000     C*
030500000000     C* test se esistono transazioni attive ed eventualmente cosa fare
030600000000     C                   Z-ADD     0             EXECOD
030700000000     C                   CALL      'XRCMTI'
030800000000     C                   PARM                    EXECOD            1 0
030900000000     C*
031000000000     C                   SELECT
031100000000     C* Annulla richiesta
031200000000     C     EXECOD        WHENEQ    1
031300000000     C                   MOVEL     *OFF          $FINE
031400000000     C                   MOVEL     *HIVAL        $LASTV
031500000000     C* Conferma uscita
031600000000     C     EXECOD        WHENEQ    2
031700000000     C*                  ROLBK
031800000000     C*
031900000000     C                   ENDSL
032000000000     C*
032100000000     C                   ENDIF
032200000000     C*
032300000000     C                   ENDIF
032400000000     C*
032500000000     C* se ho confermato l'uscita proseguo
032600000000     C     $FINE         IFEQ      *ON
032700000000     C*
032800000000     C* disallocazione rcd nel caso di modifica/annullamento
032900130906    >C     OPZG74        IFEQ      '02'
033000130906    >C     OPZG74        OREQ      '04'
033100000000     C*
033200000000     C* chiudo in LR per disallocare record
033300000000     C                   SETON                                            LR
033400000000     C                   Else
033500000000     C                   SETON                                            RT
033600000000     C                   ENDIF
033700000000     C*
033800000000     C                   RETURN
033900000000     C*
034000000000     C                   ENDIF
034100000000     C*
034200000000     C                   ENDSR
034300000000     C/EJECT
034400000000     C************************************************************
034500000000     C* RICERCHE
034600000000     C************************************************************
034700000000     C     SEARCH        BEGSR
034800000000     C*
034900000000     C* determino Riga/Colonna del cursore
035000000000     C                   MOVE      NRG           R$$
035100000000     C                   MOVE      NCL           C$$
035200000000     C                   Z-ADD     RIRI          CURR
035300000000     C                   Z-ADD     COCO          CURC
035400000000     C*
035500000000     C                   SELECT
035600000000     C*
035700000000     C*  Soggetto
035800000000     C     H1NMFL        WHENEQ    'D1SOGG'
035900000000     C                   EXSR      F4_Sogg
036000130911     C*
036100130911     C*  Tipo natura giuridica
036200130911     C     H1NMFL        WHENEQ    'D1TPN '
036300130911     C                   EXSR      F4_TPN
036400000000     C*
036500000000     C*  Partita IVA
036600000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
036700000000     C                   EXSR      F4_PartIva
036800000000     C*
036900000000     C*  Codice fiscale
037000000000     C     H1NMFL        WHENEQ    'D1CDFISC'
037100000000     C                   EXSR      F4_CdFisc
037200000000     C*
037300000000     C*  Stato
037400000000     C     H1NMFL        WHENEQ    'D1STATO'
037500000000     C                   EXSR      F4_Stato
037600000000     C*
037700000000     C*  Provincia di nascita italiana
037800000000     C     H1NMFL        WHENEQ    'D1PRVNASC'
037900000000     C                   EXSR      F4_PrvNasc
038000000000     C*
038100000000     C* ricerca non abilitata in questa posizione
038200000000     C                   OTHER
038300000000     C                   SETON                                        9899
038400000000     C*
038500000000     C                   ENDSL
038600000000     C*
038700000000     C* imposto pos. del cursore
038800000000     C                   Z-ADD     CURR          H1RIGA
038900000000     C                   Z-ADD     CURC          H1COLO
039000000000     C*
039100000000     C                   MOVE      *HIVAL        $LASTV
039200000000     C*
039300000000     C                   ENDSR
039400000000     C/EJECT
039500000000     C************************************************************
039600000000     C* Ricerca codice soggetto
039700000000     C************************************************************
039800000000     C     F4_Sogg       BEGSR
039900000000     C*
040000000000     C                   RESET                   ANA040DS
040100000000     C*
040200000000     C                   MOVEL     '01'          OPZ040
040300000000     C                   MoveL     D1Sogg        CoD040
040400000000     C                   MOVEL     ANA040DS      KPJBU
040500000000     C                   CALL      'ANA040R'
040600000000     C                   PARM                    KPJBA
040700000000     C*
040800000000     C                   MOVEL     KPJBU         ANA040DS
040900000000     C*
041000000000     C                   if        Ret040     <> '2'
041100000000     C                   MoveL     CoD040        D1Sogg
041200000000     C                   endif
041300000000     C*
041400000000     C                   ENDSR
041500000000     C/EJECT
041600130911     C************************************************************
041700130911     C* Ricerca tipo natura giuridica
041800130911     C************************************************************
041900130911     C     F4_TpN        BEGSR
042000130911     C*
042100130911     C                   Reset                   ANA964DS
042200130911     C*
042300130911     C                   MoveL     '01'          Opz964
042400130911     C                   MoveL     'SDGSCF000'   Trc964
042500130911     C                   MoveL     'SCFTPN    '  Cam964
042600130911     C*
042700130911     C                   MoveL     ANA964DS      KPJBU
042800130911     C                   CALL      'ANA964R'
042900130911     C                   PARM                    KPJBA
043000130911     C                   MoveL     KPJBU         ANA964DS
043100130911     C*
043200130911     C                   if        Ret964     <> '2'
043300130911     C                   MoveL     Val964        D1TpN
043400130911     C                   endif
043500130911     C*
043600130911     C                   ENDSR
043700130911     C/EJECT
043800000000     C************************************************************
043900000000     C* Ricerca codice IVA
044000000000     C************************************************************
044100000000     C     F4_PartIva    BEGSR
044200000000     C*
044300000000     C                   RESET                   ANA040DS
044400000000     C*
044500000000     C                   MOVEL     '01'          OPZ040
044600000000     C                   MoveL     D1PartIva     IVA040
044700000000     C                   MoveL     D1Sogg        CoD040
044800000000     C                   MOVEL     ANA040DS      KPJBU
044900000000     C                   CALL      'ANA040R'
045000000000     C                   PARM                    KPJBA
045100000000     C*
045200000000     C                   MOVEL     KPJBU         ANA040DS
045300000000     C*
045400000000     C                   if        Ret040     <> '2'
045500000000     C*
045600000000     C                   MoveL     CoD040        WSogg
045700000000     C                   EXSR      CallDvaSog
045800000000     C*
045900000000     C                   if        DVAErrore  <> '1'
046000000000     C*
046100000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
046200000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
046300000000     C*
046400000000     C                   MoveL     DVSPartIVA    D1PartIva
046500000000     C*
046600000000     C                   endif
046700000000     C*
046800000000     C                   endif
046900000000     C*
047000000000     C                   ENDSR
047100000000      /EJECT
047200000000     C************************************************************
047300000000     C* Ricerca codice fiscale
047400000000     C************************************************************
047500000000     C     F4_CdFisc     BEGSR
047600000000     C*
047700000000     C                   RESET                   ANA040DS
047800000000     C*
047900000000     C                   MOVEL     '01'          OPZ040
048000000000     C                   MoveL     D1Sogg        CoD040
048100000000     C                   MoveL     D1CdFisc      CdF040
048200000000     C                   MOVEL     ANA040DS      KPJBU
048300000000     C                   CALL      'ANA040R'
048400000000     C                   PARM                    KPJBA
048500000000     C*
048600000000     C                   MOVEL     KPJBU         ANA040DS
048700000000     C*
048800000000     C                   if        Ret040     <> '2'
048900000000     C*
049000000000     C                   MoveL     CoD040        WSogg
049100000000     C                   EXSR      CallDvaSog
049200000000     C*
049300000000     C                   if        DVAErrore  <> '1'
049400000000     C*
049500000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
049600000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
049700000000     C*
049800000000     C                   MoveL     DVSCdFisc     D1CdFisc
049900000000     C*
050000000000     C                   endif
050100000000     C*
050200000000     C                   endif
050300000000     C*
050400000000     C                   ENDSR
050500000000     C/EJECT
050600000000     C************************************************************
050700000000     C* Ricerca codice Stato del conto
050800000000     C************************************************************
050900000000     C     F4_Stato      BEGSR
051000000000     C*
051100000000     C                   Reset                   XTabDS
051200000000     C*
051300000000     C                   Move      '01'          XTaOpz
051400000000     C                   Move      XScSoc        XTaAzi
051500000000     C                   Move      XScLin        XTaLin
051600000000     C                   Z-add     Curr          XTaRig
051700000000     C                   Z-add     Curc          XTaCol
051800000000     C                   Move      *Blank        XTaKey
051900000000     C*
052000000000     C                   Clear                   KPJBU
052100000000     C                   MoveL     XTabDS        KPJBU
052200000000     C*
052300000000     C                   CALL      'ANGG12R'
052400000000     C                   PARM                    KPJBA
052500000000     C                   MoveL     KPJBU         XTabDS
052600000000     C*
052700000000     C                   if        XTaErr     <> '1'      and
052800000000     C                             XTaKey     <> *Blank
052900000000     C                   Move      XTaKey        D1Stato
053000000000     C                   endif
053100000000     C*
053200000000     C                   ENDSR
053300000000      /EJECT
053400000000     C************************************************************
053500000000     C* Ricerca codice provincia italiana
053600000000     C************************************************************
053700000000     C     F4_PrvNasc    BEGSR
053800000000     C*
053900000000     C                   Reset                   XTabDS
054000000000     C*
054100000000     C                   Move      '01'          XTaOpz
054200000000     C                   Move      XScSoc        XTaAzi
054300000000     C                   Move      XScLin        XTaLin
054400000000     C                   Z-add     Curr          XTaRig
054500000000     C                   Z-add     Curc          XTaCol
054600000000     C                   Move      *Blank        XTaKey
054700000000     C*
054800000000     C                   Clear                   KPJBU
054900000000     C                   MoveL     XTabDS        KPJBU
055000000000     C*
055100000000     C                   CALL      'ANGC17R'
055200000000     C                   PARM                    KPJBA
055300000000     C                   MoveL     KPJBU         XTabDS
055400000000     C*
055500000000     C                   if        XTaErr     <> '1'      and
055600000000     C                             XTaKey     <> *Blank
055700000000     C                   Move      XTaKey        D1PrvNasc
055800000000     C                   endif
055900000000     C*
056000000000     C                   ENDSR
056100000000      /EJECT
056200000000     C************************************************************
056300000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
056400000000     C************************************************************
056500000000     C     GESPIE        BEGSR
056600000000     C*
056700000000     C                   SELECT
056800000000     C*
056900000000     C     $GEST         WHENEQ    'D1'
057000000000     C     $ULKD1        IFEQ      0
057100000000     C* riempimento opzioni
057200000000     C                   EXSR      F24D1
057300000000     C                   ELSE
057400000000     C                   WRITE     FMTZ1
057500000000     C                   ENDIF
057600000000     C*
057700000000     C     $GEST         WHENEQ    'D2'
057800000000     C     $ULKD1        IFEQ      0
057900000000     C* riempimento opzioni
058000000000     C                   EXSR      F24D2
058100000000     C                   ELSE
058200000000     C                   WRITE     FMTZ1
058300000000     C                   ENDIF
058400000000     C                   ENDSL
058500000000     C*
058600000000     C                   ENDSR
058700000000     C/EJECT
058800000000     C************************************************************
058900000000     C* Chiamata programma con tasto personalizzato
059000000000     C************************************************************
059100000000     C     FPerD1        BEGSR
059200000000     C*
059300000000     C* salvo la DS di ingresso
059400130906    >C                   MOVEL     SDGSG74DS     $SaveDS
059500000000     C*
059600130906    >C                   MOVE      xscsoc        SocietaG74
059700130906     C* passare nei campi appositi della DS SDGSG74DS il valore
059800000000     C* della chiave della VL univoca
059900130906     C                   Eval      SoggG74     = D1Sogg
060000130906     C                   Eval      PartIvaG74  = D1PartIva
060100130906     C                   Eval      CdFiscG74   = D1CdFisc
060200130906     C                   Eval      CogRagG74   = D1CogRag
060300130906     C                   Eval      NomeG74     = D1Nome
060400000000     C*
060500130906     C                   Eval      DtNascG74   = WDtNasc
060600000000     C*
060700130906     C                   Eval      LocNascG74  = D1LocNasc
060800130906     C                   Eval      PrvNascG74  = D1PrvNasc
060900130906     C                   Eval      StatoG74    = D1Stato
061000130906     C                   Eval      CodUICG74   = D1CodUIC
061100130906     C                   Eval      LocalitG74  = D1Localit
061200130906     C                   Eval      IndirEG74   = D1IndirE
061300000000     C*
061400130906    >C                   MOVEL     SDGSG74DS     KPJBU
061500000000     C                   CALL      $PGF1(PosFun)                        22
061600000000     C                   PARM                    KPJBA
061700000000     C                   PARM                    NOMPGM
061800000000     C*
061900000000     C* ripristino la DS di ingresso
062000130906    >C                   MOVEL     $SaveDS       SDGSG74DS
062100000000     C*
062200000000     C                   ENDSR
062300000000     C/EJECT
062400000000     C************************************************************
062500000000     C* GESTIONE VIDEO RECORD D1
062600000000     C************************************************************
062700000000     C     GESD1         BEGSR
062800000000     C* inizializzazione videata
062900000000   B1C     $INZD1        IFEQ      *ON
063000000000     C                   EXSR      INZD1
063100000000     C                   MOVE      *OFF          $INZD1
063200000000   E1C                   ENDIF
063300000000     C* emissione piede videata se proveniente da altra
063400000000     C* salvataggio valore formato di provenienza
063500000000   B1C     $LASTV        IFNE      'D1'
063600000000     C                   WRITE     FMTT1
063700000000     C                   EXSR      GESPIE
063800000000     C                   MOVE      $LASTV        $LASTG
063900000000     C                   MOVE      'D1'          $LASTV
064000000000   E1C                   ENDIF
064100000000     C*
064200000000     C* se esistono errori sulla videata
064300000000     C* emetto la write del formato a indicatori spenti per vedere
064400000000     C* le eventuali decodifiche
064500130926   B1C                   if        (*IN99       = *On   or
064600131001   B1C                              *In16       = *On      )  and
064700130926     C                             $GEST        = 'D1'
064800000000     C                   MOVEA     *IN           WIN
064900000000     C                   MOVE      *ALL'0'       IN5098           49
065000130926     C                   MOVEA     IN5098        *IN(50)
065100130926     C                   MOVEA     WIN           *IN
065200130930     C                   Eval      WIn16        = *In16
065300130930     C                   Eval      *In16        = *Off
065400000000     C                   WRITE     FMTD1
065500000000     C                   MOVEA     WIN           *IN
065600130930     C                   Eval      *In16        = WIn16
065700000000   E1C                   ENDIF
065800000000     C* Inizializzazione indice tasti funzione personalizzati
065900000000     C                   Eval      PosFun = 25
066000000000     C*
066100000000   B1C                   SELECT
066200000000     C* Annullamento
066300130906    >C     OPZG74        WHENEQ    '04'
066400000000     C* Visualizzazione
066500130906    >C     OPZG74        OREQ      '05'
066600000000     C                   WRITE     FMTD1
066700000000     C                   EXFMT     PROTECT
066800000000     C* Immissione/Modifica/Copia
066900000000   X1C                   OTHER
067000000000     C                   EXFMT     FMTD1
067100000000   E1C                   ENDSL
067200000000     C*
067300000000     C* azzero la posizione del cursore
067400000000     C                   Z-ADD     0             H1RIGA
067500000000     C                   Z-ADD     0             H1COLO
067600000000     C*
067700000000     C* controllo abilitazione tasti funzione
067800000000     C                   EXSR      CTRKD1
067900000000     C*
068000000000   B1C     *IN99         IFEQ      *OFF
068100000000     C*
068200000000   B2C                   SELECT
068300000000     C* ROLLUP
068400000000    >C     $TASTO        WHENEQ    ROLLUP
068500000000    >C                   MOVE      'D2'          $GEST
068600000000     C* F2=Decodifica
068700000000     C     $TASTO        WHENEQ    F02
068800000000     C                   EXSR      F02D1
068900000000     C* F3=Fine
069000000000     C     $TASTO        WHENEQ    F03
069100000000     C                   EXSR      F03D1
069200000000     C* F4=Ricerca
069300000000     C     $TASTO        WHENEQ    F04
069400000000     C                   EXSR      SEARCH
069500000000     C* F5=Refresh
069600000000     C     $TASTO        WHENEQ    F05
069700000000     C                   EXSR      F05D1
069800000000     C* F8=Successivo
069900000000     C     $TASTO        WHENEQ    F08
070000000000     C                   EXSR      F08D1
070100000000     C* F12=Ritorno
070200000000     C     $TASTO        WHENEQ    F12
070300000000     C                   EXSR      F12D1
070400000000     C* F24=Altri tasti
070500000000     C     $TASTO        WHENEQ    F24
070600000000     C                   EXSR      F24D1
070700000000     C* Personalizzata
070800130906     C* NB chiamata NON possibile perchè SDGSG74DS più lunga
070900000000     C* di KPJBU 256 caratteri                          a
071000000000     C**** $PGF1(PosFun) WHENNE    *BLANK
071100000000     C****               EXSR      FPerD1
071200000000     C*
071300000000   X2C                   OTHER
071400000000     C                   EXSR      CTRD1
071500000000     C* aggiungere le chiamate ai controlli delle videate supplementari
071600000000    >C     *IN99         IFEQ      *OFF
071700000000    >C                   EXSR      CTRD2
071800130926   B1C                   if        *IN99       = *On
071900000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
072000000000    >C* indicatori uguali in videate differenti, genererei conflitti
072100000000    >C                   MOVE      *ALL'0'       IN5098           49
072200000000    >C                   MOVEA     IN5098        *IN(50)
072300000000    >C                   SETON                                        9996                       96
072400000000    >C                   ENDIF
072500000000    >C                   ENDIF
072600000000     C*
072700000000   B3C     $TASTO        IFEQ      F06
072800000000     C     *IN99         ANDEQ     *OFF
072900000000     C* eseguo aggiornamento
073000000000     C                   EXSR      AGGANA
073100000000     C* eseguo operazioni del dopo-aggiornamento
073200000000   B4C     *IN99         IFEQ      *OFF
073300000000     C                   EXSR      GESAGG
073400000000   E4C                   ENDIF
073500000000   E3C                   ENDIF
073600000000     C*
073700000000   E2C                   ENDSL
073800000000     C*
073900000000   E1C                   ENDIF
074000000000     C*
074100000000     C                   ENDSR
074200000000     C/EJECT
074300000000     C************************************************************
074400000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
074500000000     C************************************************************
074600000000     C     CTRKD1        BEGSR
074700000000     C*
074800000000     C                   SETOFF                                       99
074900000000     C                   MOVEA     $FC1          $FC
075000000000     C                   EXSR      CTRKEY
075100000000     C     *IN99         IFEQ      *ON
075200000000     C                   SETON                                        97
075300000000     C                   ENDIF
075400000000     C*
075500000000     C                   ENDSR
075600000000     C/EJECT
075700000000     C************************************************************
075800000000     C* INIZIALIZZAZIONE VIDEATA DATI
075900000000     C************************************************************
076000000000     C     INZD1         BEGSR
076100000000     C*
076200000000     C                   CLEAR                   FMTD1
076300000000    >C                   CLEAR                   FMTD2
076400000000     C*
076500130906     C                   Eval      D1Anno      = AnnoG74
076600000000     C*
076700140401R133 C                   Select
076701151201R195 C* Acquisti da San Marino
076800151201R133 C                   When      TpComg74    = 'S'
076900140401 "   C                   Eval      *In12       = *On
077000140401 "   C                   Eval      D1Mese      = MeseG74
077100151201R133 C                   Other
077101151201R195 C* Spesometro o Black List
077200151201R133 C                   Eval      *In12       = *Off
077300140401 "   C                   endSl
077400140401R133 C*
077500000000     C* per tutte le opzioni NON immissione
077600130906     C     OPZG74        IFNE      '01'
077700130906     C                   Eval      SCFSocieta  = SocietaG74
077800130906     C                   Eval      SCFAnno     = AnnoG74
077900130906     C                   Eval      SCFCF       = CFG74
078000130906     C                   Eval      SCFSogg     = SoggG74
078100131206R141 C                   Eval      SCFCausale  = CausaleG74
078200130906     C                   Eval      SCFPartIva  = PartIvaG74
078300130906     C                   Eval      SCFCdFisc   = CdFiscG74
078400130906     C                   Eval      SCFCogRag   = CogRagG74
078500130906     C                   Eval      SCFNome     = NomeG74
078600130906     C                   Eval      SCFDtNasc   = DtNascG74
078700130906     C                   Eval      SCFLocNasc  = LocNascG74
078800130906     C                   Eval      SCFPrvNasc  = PrvNascG74
078900130906     C                   Eval      SCFStato    = StatoG74
079000130906     C                   Eval      SCFCodUIC   = CodUICG74
079100130906     C                   Eval      SCFLocalit  = LocalitG74
079200130906     C                   Eval      SCFIndirE   = IndirEG74
079300000000     C*
079400000000     C     *IN22         DOUEQ     *OFF
079500000000     C     MSGRTN        ORNE      'R'
079600000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
079700130906     C     OPZG74        IFEQ      '02'
079800130906    >C     OPZG74        OREQ      '04'
079900140422R133 C     OPZG74        OREQ      '05'
080000000000     C                   EXSR      AllocANA
080100000000     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
080200000000   X3C                   ELSE
080300000000   E3C                   ENDIF
080400000000     C*
080500000000   E2C                   ENDDO
080600000000     C*
080700000000     C* se il rcd da gestire non viene trovato
080800000000     C* oppure è allocato da altro pgm
080900000000     C* ritorno al pgm guida avvertendolo dell'errore
081000130906   B2C     Err0502       IFEQ      *ON
081100130906     C     End0502       OREQ      *ON
081200000000     C                   MOVE      *ON           $FINE
081300130906    >C                   MOVE      '1'           ERRG74
081400000000     C*
081500000000     C* altrimenti
081600000000   X2C                   ELSE
081700000000     C* riempio la videata
081800000000     C                   EXSR      RIED1
081900000000    >C* Aggiungere le routine di riempimento dei record succcessivi
082000000000    >C                   EXSR      RIED2
082100000000     C* richiamo routine dei ctrl per decodificarla
082200000000     C                   EXSR      CTRD1
082300000000    >C* Aggiungere le routine di controllo dei record succcessivi
082400000000    >C                   EXSR      CTRD2
082500000000     C                   MOVE      *ALL'0'       IN5098
082600000000     C                   MOVEA     IN5098        *IN(50)
082700000000   E2C                   ENDIF
082800000000     C*
082900000000     C* per immissione
083000000000   X1C                   ELSE
083100000000     C*
083200000000     C* riempio con i default di immissione  (se reperiti)
083300000000     C                   IF        $FlgDef     = *on
083400000000     C                   EXSR      RIEDEF
083500000000     C                   ENDIF
083600000000     C*
083700000000     C* imposto i campi chiave ricevuti anche se si è in immissione
083800000000     C* perchè potrebbero essere stati passati dei default di chiave
083900130906     C                   Eval      D1Sogg      = SoggG74
084000130906     C                   Eval      D1PartIva   = PartIvaG74
084100130906     C                   Eval      D1CdFisc    = CdFiscG74
084200130906     C                   Eval      D1CogRag    = CogRagG74
084300130906     C                   Eval      D1Nome      = NomeG74
084400000000     C*
084500130906     C                   MoveL     DtNascG74     WDtNasc
084600130906     C                   if        DtNascG74   = *Loval
084700000000     C                   Eval      D1DtNasc    = 0
084800000000     C                   else
084900000000     C*
085000000000     C                   Select
085100000000     C* formato mm/gg/aaaa
085200000000     C                   When      $DatFmt     = '*MDY'
085300130906     C     *USA          Move      DtNascG74     D1DtNasc
085400000000     C* formato aa/mm/gg
085500000000     C                   When      $DatFmt     = '*YMD'
085600130906     C     *ISO          Move      DtNascG74     D1DtNasc
085700000000     C* formato gg/mm/aaaa
085800000000     C                   When      $DatFmt     = '*DMY'
085900130906     C     *EUR          Move      DtNascG74     D1DtNasc
086000000000     C                   endSl
086100000000     C*
086200000000     C                   endif
086300000000     C*
086400130906     C                   Eval      D1LocNasc   = LocNascG74
086500130906     C                   Eval      D1PrvNasc   = PrvNascG74
086600130906     C                   Eval      D1Stato     = StatoG74
086700130906     C                   Eval      D1CodUIC    = CodUICG74
086800130906     C                   Eval      D1Localit   = LocalitG74
086900130906     C                   Eval      D1IndirE    = IndirEG74
087000000000     C*
087100000000     C* decodifico le costanti
087200000000     C                   EXSR      F02D1
087300000000   E1C                   ENDIF
087400000000     C*
087500000000     C* imposto quegli attributi della videata che sono modificabili
087600000000     C* al richiamo del determinato pgm
087700000000     C                   EXSR      ATRD1
087800000000     C*
087900000000     C                   ENDSR
088000000000     C/EJECT
088100000000     C************************************************************
088200000000     C* Allocazione primo record
088300000000     C************************************************************
088400000000     C     AllocAna      BEGSR
088500000000     C*
088600130906     C                   RESET                   SDG00502DS
088700000000     C*
088800130906     C                   Eval      Ann0502     = AnnG74
088900130906     C                   Eval      TpAnn0502   = TpAnnG74
089000130906     C                   if        TpAnn0502   = *Blank
089100130906     C                   MoveL     *Hival        TpAnn0502
089200110824     C                   endif
089300130906     C*
089400130906     C************       MOVE      $ORD          ORD0502
089500000000     C*
089600000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
089700000000     C* inizializzazione delle variabili SQL
089800130906     C                   MOVE      '0'           RIC0502
089900130906     C                   MOVE      '01'          OPZ0502
090000130906     C                   MOVE      '02'          TPROTT0502
090100130906     C                   MOVE      '01'          ROTT0502
090200130906     C                   MOVE      '01'          TpList0502
090300000000     C*
090400130906     C                   Eval      Soc0502     = SocietaG74
090500140401R133 C****               Eval      TpCom0502   = 'E'
090600140401R133 C                   Eval      TpCom0502   = TpComG74
090700130906     C                   Eval      Anno0502    = AnnoG74
090800140401R133 C                   Eval      Mese0502    = MeseG74
090900130906     C*
091000130919     C                   Eval      CF0502      = CFG74
091100130906     C                   if        CF0502      = *Blank
091200130906     C                   MoveL     *Hival        CF0502
091300130906     C                   endif
091400130906     C*
091500130906     C                   Eval      Sogg0502    = SoggG74
091600130906     C                   if        Sogg0502    = *Blank
091700130906     C                   MoveL     *Hival        Sogg0502
091800130906     C                   endif
091900131206R141 C*
092000131206 "   C                   Eval      Causal0502  = CausaleG74
092100131206 "   C                   if        Causal0502  = *Blank
092200131206 "   C                   MoveL     *Hival        Causal0502
092300131206R141 C                   endif
092400130919     C*
092500130919     C                   Eval      TpOpe0502   = TpOpeG74
092600130919     C                   if        TpOpe0502   = *Blank
092700130919     C                   MoveL     *Hival        TpOpe0502
092800130919     C                   endif
092900130906     C*
093000130906     C                   Eval      Quadro0502  = QuadroG74
093100130906     C                   if        Quadro0502  = *Blank
093200130906     C                   MoveL     *Hival        Quadro0502
093300130906     C                   endif
093400130906     C*
093500130906     C                   Eval      CogRag0502  = CogRagG74
093600130906     C                   Eval      Nome0502    = NomeG74
093700130906     C                   Eval      DtNasc0502  = DtNascG74
093800130906     C                   Eval      LocNas0502  = LocNascG74
093900130906     C                   Eval      PrvNas0502  = PrvNascG74
094000130906     C                   Eval      Stato0502   = StatoG74
094100130906     C                   Eval      CodUIC0502  = CodUICG74
094200130906     C                   Eval      Locali0502  = LocalitG74
094300130906     C                   Eval      IndirE0502  = IndirEG74
094400130906     C*
094500130906     C                   Eval      PIVA0502    = PartIvaG74
094600130906     C                   Eval      CdFisc0502  = CdFiscG74
094700110824     C*
094800130906     C                   MoveL     *Hival        TpN0502
094900130906     C                   MoveL     *Hival        COGRAR0502
095000130906     C                   MoveL     *Hival        NOMER0502
095100130906     C                   MoveL     *Hival        DTNASR0502
095200130906     C                   MoveL     *Hival        LOCNAR0502
095300130906     C                   MoveL     *Hival        PRVNAR0502
095400130906     C                   MoveL     *Hival        STATOR0502
095500130906     C                   MoveL     *Hival        CODUIR0502
095600130906     C                   MoveL     *Hival        LOCALR0502
095700130906     C                   MoveL     *Hival        INDIER0502
095800130906     C*
095900130906     C                   MoveL     *Hival        TpNR0502
096000130906     C                   MoveL     *Hival        PIVAR0502
096100130906     C                   MoveL     *Hival        CDFISR0502
096200130925     C                   MoveL     *Hival        TPOPER0502
096300130925     C                   MoveL     *Hival        NATOPR0502
096400130906     C                   MoveL     *Hival        LEASIR0502
096500130906     C                   MoveL     *Hival        DTREGR0502
096600130906     C                   MoveL     *Hival        NRREGR0502
096700130906     C                   MoveL     *Hival        SERRER0502
096800000000     C*
096900130906     C                   CALLB     'SDG00502R'
097000000000     C                   PARM                    KPJBA
097100130906     C                   PARM                    SDG00502DS
097200130906     C                   PARM                    SDG00592DS
097300130906     C                   PARM                    SDG005FADS
097400130906     C                   PARM                    SDG005SADS
097500130906     C                   PARM                    SDG005BLDS
097600130906     C                   PARM                    SDG005FEDS
097700130906     C                   PARM                    SDG005FRDS
097800130906     C                   PARM                    SDG005NEDS
097900130906     C                   PARM                    SDG005NRDS
098000130906     C                   PARM                    SDG005DFDS
098100130906     C                   PARM                    SDG005FNDS
098200130906     C                   PARM                    SDG005SEDS
098300131017R136 C                   PARM                    SDG005TUDS
098400130906     C                   PARM                    SDG005TADS
098500000000     C*
098600130906     C                   Eval      *In22       = Err0502
098700000000     C*
098800000000     C                   ENDSR
098900000000     C/EJECT
099000000000     C************************************************************
099100000000     C* RIEMPIMENTO VIDEATA  D1
099200000000     C************************************************************
099300000000     C     RIED1         BEGSR
099400000000     C*
099500000000     C* impostare i campi della videata dai campi del file
099600131001     C                   Eval      D1Sogg      = Sogg0592
099700131001     C                   Eval      D1TpN       = TpN0592
099800131001     C                   Eval      D1PartIva   = PIVA0592
099900131001     C                   Eval      D1CdFisc    = CdFisc0592
100000131001     C                   Eval      D1CogRag    = CogRag0592
100100131001     C                   Eval      D1Nome      = Nome0592
100200000000     C*
100300131001     C                   MoveL     DtNasc0592    WDtNasc
100400131001     C                   if        WDtNasc     = *Loval
100500000000     C                   Eval      D1DtNasc    = 0
100600000000     C                   else
100700000000     C*
100800000000     C                   Select
100900000000     C* formato mm/gg/aaaa
101000000000     C                   When      $DatFmt     = '*MDY'
101100131001     C     *USA          Move      WDtNasc       D1DtNasc
101200000000     C* formato aa/mm/gg
101300000000     C                   When      $DatFmt     = '*YMD'
101400131001     C     *ISO          Move      WDtNasc       D1DtNasc
101500000000     C* formato gg/mm/aaaa
101600000000     C                   When      $DatFmt     = '*DMY'
101700131001     C     *EUR          Move      WDtNasc       D1DtNasc
101800000000     C                   endSl
101900000000     C*
102000000000     C                   endif
102100000000     C*
102200131001     C                   Eval      D1LocNasc   = LocNas0592
102300131001     C                   Eval      D1PrvNasc   = PrvNas0592
102400131001     C                   Eval      D1Stato     = Stato0592
102500131001     C                   Eval      D1CodUIC    = CodUIC0592
102600131001     C                   Eval      D1Localit   = Locali0592
102700131001     C                   Eval      D1IndirE    = IndirE0592
102800000000     C*
102900000000     C                   ENDSR
103000000000     C************************************************************
103100000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
103200000000     C************************************************************
103300000000     C     ATRD1         BEGSR
103400000000     C*
103500000000    >C* in questa routine bisogna attivare indicatori o campi programma
103600000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
103700000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
103800000000    >C*  dovrebbero già arrivare dei flag
103900000000     C*
104000000000     C                   SETOFF                                       02
104100000000     C*
104200000000     C* in caso di manutenzione
104300130906    >C     OPZG74        IFEQ      '02'
104400000000     C* proteggo i campi chiave
104500000000     C                   SETON                                        02        PR campi key
104600000000   E3C                   ENDIF
104700130911     C*
104800130911     C* in caso di insertimento o copia
104900130911     C* abilito i campi D1SOGG e D1TPN
105000130911    >C     OPZG74        IFEQ      '01'
105100130911    >C     OPZG74        orEQ      '03'
105200130911     C                   SETON                                        11        PR campi key
105300130911   E3C                   ENDIF
105400000000     C*
105500000000     C                   ENDSR
105600000000     C/EJECT
105700000000     C************************************************************
105800000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
105900000000     C************************************************************
106000000000     C     RIEDEF        BEGSR
106100000000     C*
106200000000     C*
106300000000     C                   ENDSR
106400000000     C/EJECT
106500000000     C************************************************************
106600000000     C* GESTIONE F02 VIDEO D1
106700000000     C************************************************************
106800000000     C     F02D1         BEGSR
106900000000     C*
107000000000     C* per decodificare la videata richiamo la routine dei ctrl
107100000000     C* e poi disattivo gli eventuali messaggi di errore
107200000000     C                   EXSR      CTRD1
107300000000     C*
107400000000     C                   MOVE      *ALL'0'       IN5098
107500000000     C                   MOVEA     IN5098        *IN(50)
107600000000     C*
107700000000     C                   ENDSR
107800000000     C/EJECT
107900000000     C************************************************************
108000000000     C* GESTIONE F03 VIDEO D1
108100000000     C************************************************************
108200000000     C     F03D1         BEGSR
108300000000     C*
108400000000     C                   MOVE      *ON           $FINE
108500130906    >C                   MOVE      '1'           RETG74
108600000000     C* fine programma
108700000000     C                   EXSR      ENDPGM
108800000000     C*
108900000000     C                   ENDSR
109000000000     C/EJECT
109100000000     C************************************************************
109200000000     C* GESTIONE F05 VIDEO D1
109300000000     C************************************************************
109400000000     C     F05D1         BEGSR
109500000000     C*
109600000000     C                   MOVE      *ON           $INZD1
109700000000     C*
109800000000     C                   ENDSR
109900000000     C/EJECT
110000000000     C************************************************************
110100000000     C* GESTIONE F08 VIDEO D1
110200000000     C************************************************************
110300000000     C     F08D1         BEGSR
110400000000     C*
110500000000     C                   MOVE      *ON           $FINE
110600130906    >C                   MOVE      '0'           RETG74
110700000000     C*
110800000000     C                   ENDSR
110900000000     C/EJECT
111000000000     C************************************************************
111100000000     C* GESTIONE F12 VIDEO D1
111200000000     C************************************************************
111300000000     C     F12D1         BEGSR
111400000000     C*
111500000000     C                   MOVE      *ON           $FINE
111600130906    >C                   MOVE      '2'           RETG74
111700000000     C*
111800000000     C                   ENDSR
111900000000     C/EJECT
112000000000     C************************************************************
112100000000     C* GESTIONE F24 VIDEO D1
112200000000     C************************************************************
112300000000     C     F24D1         BEGSR
112400000000     C*
112500000000     C                   MOVEA     $FX1          $FX
112600000000     C                   MOVEA     $FC1          $FC
112700000000     C                   MOVEA     $FR1          $FR
112800000000     C                   Z-ADD     $ULKD1        $ULK
112900000000     C                   Z-ADD     $D1L01        $L01
113000000000     C                   Z-ADD     $D1L02        $L02
113100000000     C                   EXSR      RIEKEY
113200000000     C                   Z-ADD     $ULK          $ULKD1            3 0
113300000000     C                   MOVEA     $FC           $FC1
113400000000     C                   MOVEL     $KEY1         Z1KE1
113500000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
113600000000     C                   MOVEL     $KEY2         Z1KE2
113700000000     C                   WRITE     FMTZ1
113800000000     C*
113900000000     C                   ENDSR
114000000000     C/EJECT
114100000000     C************************************************************
114200000000     C* CONTROLLO VIDEATA
114300000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
114400000000     C* 95 96 97 98
114500000000     C************************************************************
114600000000     C     CTRD1         BEGSR
114700000000     C*
114800000000     C                   SETOFF                                       99
114900000000     C*
115000000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
115100000000     C*
115200130906     C     OPZG74        IFNE      '04'
115300000000     C*
115400130911     C                   if        OPZG74      = '01'   or
115500130911     C                             OPZG74      = '03'
115600000000     C                   EXSR      CtrSogg
115700130911     C                   endif
115800130911     C*
115900130911     C                   if        OPZG74      = '01'   or
116000130911     C                             OPZG74      = '03'
116100130911     C                   EXSR      CtrTpN
116200130911     C                   endif
116300000000     C*
116400130919     C                   EXSR      CtrPIVA_CDF
116500130919     C*
116600130919     C                   EXSR      CtrCogRag
116700000000     C*
116800000000     C                   EXSR      CtrNome
116900000000     C*
117000000000     C* Dati di nascita
117100000000     C                   EXSR      CtrDtNasc
117200000000     C                   EXSR      CtrLocNasc
117300000000     C                   EXSR      CtrPrvNasc
117400000000     C*
117500000000     C*
117600000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
117700130906     C     OPZG74        IFEQ      '01'
117800130906    >C     OPZG74        OREQ      '03'
117900000000     C*
118000000000     C* imposto i campi chiave e verifico l'esistenza del record
118100000000     C                   EXSR      SetAna
118200000000     C* se lo trovo significa che esiste già
118300130906     C     End0502       ifEQ      *Off
118400000000     C                   SETON                                        9099
118500000000     C                   ENDIF
118600000000     C*
118700000000     C                   ENDIF
118800000000     C*
118900000000     C                   ENDIF
119000000000     C*
119100000000     C                   ENDSR
119200000000     C/EJECT
119300130919     C***********************************************************
119400130919     C* Controllo codice soggetto
119500130919     C***********************************************************
119600130919     C     CtrSogg       BEGSR
119700130919     C*
119800130919     C                   Clear                   D1SoggD
119900130919     C*
120000130919     C     D1SOGG        IFNE      *BLANK
120100130919     C                   CALLb     'XALLINEA'
120200130919     C                   PARM                    D1SOGG
120300130919     C                   PARM      8             LEN               5 0
120400130919     C                   PARM                    XSCINP
120500130919     C                   ENDIF
120600130919     C*
120700130919     C                   MoveL     D1Sogg        WSogg
120800130919     C                   if        D1Sogg     <> *Blank
120900130919     C                   EXSR      CallDvaSog
121000130919     C*
121100130919     C                   if        DVAErrore  <> '1'
121200130919     C*
121300130919     C                   Eval      %subst(DVASOG:1:DVALENOUT)
121400130919     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
121500130919     C*
121600130919     C                   MoveL     DVSDes        D1SoggD
121700130919     C*
121800130919     C                   else
121900130919     C*
122000131001     C                   Eval      *In16       = *On
122100130919     C*
122200130919     C                   endif
122300130919     C*
122400130919     C                   endif
122500130919     C*
122600130919     C                   ENDSR
122700130919     C/EJECT
122800130919     C************************************************************
122900130919     C* Chiamata al driver dei soggetti
123000130919     C************************************************************
123100130919     C     CallDvaSog    BEGSR
123200130919     C*
123300130919     C                   EVAL      DVALenOut = %Size(DVASOG)
123400130919     C*
123500130919     C                   CALLB     'NDDVASOG'
123600130919     C                   PARM      '1'           DVARIC
123700130919     C                   PARM      XScSoc        DVASOCIETA
123800130919     C                   PARM                    DVAUNITA
123900130919     C                   PARM      'DVASOG    '  DVASTRUTT
124000130919     C                   PARM                    DVADTRIF
124100130919     C                   PARM                    DVALENOUT
124200130919     C* Se si accede con il codice cli/for impostare i campi seguenti
124300130919     C* se si accede con il soggetto ometterli e impostare gli altri
124400130919     C*****              PARM                    DVASNATURA
124500130919     C*****              PARM                    DVACODICE
124600130919     C*****              PARM                    DVALINEAV
124700130919     C*****              PARM                    DVAFILIALE
124800130919     C                   PARM                    *omit
124900130919     C                   PARM                    *omit
125000130919     C                   PARM                    *omit
125100130919     C                   PARM                    *omit
125200130919     C                   PARM      WSogg         DVSSOGG
125300130919     C                   PARM      *Blank        DVSTPIND
125400130919     C                   PARM      *Blank        DVSCDIND
125500130919     C                   PARM                    DVAERRORE
125600130919     C                   PARM                    DVAOUTPUT
125700130919     C*
125800130919     C                   ENDSR
125900130919     C/EJECT
126000130919     C***********************************************************
126100130919     C* Controllo la tipologia della sottonatura
126200130919     C***********************************************************
126300130919     C     CtrTpN        BEGSR
126400130919     C*
126500130919     C                   Clear                   D1TpND
126600130919     C                   Clear                   X07ValDs
126700130919     C*
126800130919     C                   if        D1TpN      <> *Blank
126900130919     C                   Move      '1'           X07Ric
127000130919     C                   Move      *Off          X07All
127100130919     C                   MoveL     'SDGSCF000'   X07TRc
127200130919     C                   MoveL     'SCFTPN   '   X07Cam
127300130919     C                   MoveL     D1TpN         X07VAL
127400130919     C*
127500130919     C                   CALLB     'X07VALR'
127600130919     C                   PARM                    X07ValDs
127700130919     C*
127800130919     C     X07ERR        IFEQ      *Off
127900130919     C                   MoveL     X07DES        D1TpND
128000130919     C                   else
128100130919     C                   SetOn                                        5299
128200130919     C                   endif
128300130919     C*
128400130919     C                   endif
128500130919     C*
128600130919     C                   ENDSR
128700130919     C/EJECT
128800130919     C***********************************************************
128900130919     C* Controllo partita IVA / codice fiscale
129000130919     C***********************************************************
129100130919     C     CtrPIVA_CDF   BEGSR
129200130919     C*
129300130919     C* se nazionale, sono obbligatori la partita IVA / codice fisc.
129400130919     C                   if        D1CodUIC    = $UICSoc   and
129500130919     C                             D1PartIVA   = *Blank    and
129600130919     C                             D1CdFisc    = *Blank
129700130919     C                   Seton                                        5399
129800130919     C                   endif
129900130919     C*
130000130919     C                   ENDSR
130100130919     C/EJECT
130200130919     C***********************************************************
130300130919     C* Controllo cognome/ragione sociale
130400130919     C***********************************************************
130500130919     C     CtrCogRag     BEGSR
130600130919     C*
130700130919     C                   if        D1CogRag    = *Blank
130800130919     C                   SetOn                                        5499
130900130919     C                   endif
131000130919     C*
131100130919     C     ''''          Scan      D1CogRag                               22
131200130919     C     *IN22         IFEQ      *On
131300130919     C                   SetOn                                        5899
131400130919     C                   endif
131500130919     C*
131600130919     C                   ENDSR
131700130919     C/EJECT
131800130919     C***********************************************************
131900130919     C* Controllo nome
132000130919     C***********************************************************
132100130919     C     CtrNome       BEGSR
132200130919     C*
132300130919     C     ''''          Scan      D1Nome                                 22
132400130919     C     *IN22         IFEQ      *On
132500130919     C                   SetOn                                        5999
132600130919     C                   endif
132700130919     C*
132800130919     C                   ENDSR
132900130919     C/EJECT
133000130919     C************************************************************
133100130919     C* Controllo data di nascita
133200130919     C************************************************************
133300130919     C     CtrDtNasc     BEGSR
133400130919     C*
133500130919     C                   Clear                   WDtNasc
133600130919     C*
133700130919     C                   if        D1DtNasc   <> 0
133800130919     C                   Select
133900130919     C* formato mm/gg/aaaa
134000130919     C                   When      $DatFmt     = '*MDY'
134100130919     C     *USA          TEST(D)                 D1DtNasc               56
134200130919     C* formato aa/mm/gg
134300130919     C                   When      $DatFmt     = '*YMD'
134400130919     C     *ISO          TEST(D)                 D1DtNasc               56
134500130919     C* formato gg/mm/aaaa
134600130919     C                   When      $DatFmt     = '*DMY'
134700130919     C     *EUR          TEST(D)                 D1DtNasc               56
134800130919     C                   endSl
134900130919     C* data corretta
135000130919     C                   if        *In56       = *Off
135100130919     C                   Select
135200130919     C* formato mm/gg/aaaa
135300130919     C                   When      $DatFmt     = '*MDY'
135400130919     C     *USA          Move      D1DtNasc      WDtNasc
135500130919     C* formato aa/mm/gg
135600130919     C                   When      $DatFmt     = '*YMD'
135700130919     C     *ISO          Move      D1DtNasc      WDtNasc
135800130919     C* formato gg/mm/aaaa
135900130919     C                   When      $DatFmt     = '*DMY'
136000130919     C     *EUR          Move      D1DtNasc      WDtNasc
136100130919     C                   endSl
136200130919     C*
136300130919     C                   else
136400130919     C*
136500130919     C                   SetOn                                          99
136600130919     C                   endif
136700130919     C*
136800130919     C                   endif
136900130919     C*
137000130919     C                   ENDSR
137100130919     C/EJECT
137200130919     C***********************************************************
137300130919     C* Controllo località di nascita
137400130919     C***********************************************************
137500130919     C     CtrLocNasc    BEGSR
137600130919     C*
137700130919     C     ''''          Scan      D1LocNasc                              22
137800130919     C     *IN22         IFEQ      *On
137900130919     C                   SetOn                                        6099
138000130919     C                   endif
138100130919     C*
138200130919     C                   ENDSR
138300130919     C/EJECT
138400130919     C***********************************************************
138500130919     C* Controllo provincia di nascita
138600130919     C***********************************************************
138700130919     C     CtrPrvNasc    BEGSR
138800130919     C*
138900130919     C                   Clear                   D1PrvNascD
139000130919     C*
139100130919     C                   Clear                   ANGC17DS
139200131001     C*
139300131001     C* non siamo nella situazione di provincia estera forzata per
139400131001     C* soggetti esteri che sono persona fisica
139500131001     C                   if        D1PrvNasc   = *Blank   and
139600131001     C                             D1TpN       = '1'      and
139700131001     C                             D1CodUIC   <> 0        and
139800131001     C                             D1CodUIC   <> $UICSoc
139900131001     C                   Eval      D1PrvNasc   = 'EE'
140000131001     C                   endif
140100130919     C*
140200131001     C                   if        D1PrvNasc  <> *Blank
140300130919     C*
140400130919     C                   RESET                   SDGTABDS
140500130919     C*
140600130919     C                   Eval      XTbRic      = '1'
140700130919     C                   Eval      XTbAzi      = XScSoc
140800130919     C                   Eval      XTbKey      = *Zero
140900130919     C                   Eval      XTbCod      = 'C17'
141000130919     C                   Move      D1PrvNasc     KeyC17
141100130919     C                   Movel     XScSta        KeyC17
141200130919     C                   Move      KeyC17        XTBKey
141300130919     C*
141400130919     C                   CALL      'SDGTABDR'
141500130919     C                   PARM                    SDGTABDS
141600130919     C*
141700130919     C                   if        XTbErr     <> '0'
141800131001     C* non siamo nella situazione di provincia estera forzata per
141900131001     C* soggetti esteri che sono persona fisica
142000131001     C                   if        not
142100131001     C                             (D1PrvNasc   = 'EE'    and
142200131001     C                              D1TpN       = '1'     and
142300131001     C                              D1CodUIC   <> 0       and
142400131001     C                              D1CodUIC   <> $UICSoc    )
142500130919     C                   SetOn                                        5799
142600131001     C                   endif
142700130919     C*
142800130919     C                   else
142900130919     C                   MoveL     XTbUni        ANGC17DS
143000130919     C                   MoveL     DesC17        D1PrvNascD
143100130919     C*
143200130919     C                   endif
143300130919     C*
143400130919     C                   endif
143500130919     C*
143600130919     C                   ENDSR
143700130919     C/EJECT
143800000000     C************************************************************
143900000000     C* Verifico esistenza record
144000000000     C************************************************************
144100000000     C     Setana        BEGSR
144200000000     C*
144300130906     C                   RESET                   SDG00502DS
144400110822     C*
144500130906     C                   Eval      Ann0502     = AnnG74
144600130906     C                   Eval      TpAnn0502   = TpAnnG74
144700130906     C                   Eval      TpAnn0502   = TpAnnG74
144800130906     C                   if        TpAnn0502   = *Blank
144900130906     C                   MoveL     *Hival        TpAnn0502
145000110824     C                   endif
145100130906     C*
145200130906     C**********         MOVE      $ORD          ORD0502
145300000000     C*
145400000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
145500000000     C* inizializzazione delle variabili SQL
145600130906     C                   MOVE      '0'           RIC0502
145700130906     C                   MOVE      '05'          OPZ0502
145800130906     C                   MOVE      '02'          TPROTT0502
145900130906     C                   MOVE      '01'          ROTT0502
146000130906     C                   MOVE      '01'          TpList0502
146100000000     C*
146200130906     C                   Eval      Soc0502     = SocietaG74
146300140401R133 C****               Eval      TpCom0502    = 'E'
146400140401R133 C                   Eval      SCFTpCom    = TpComG74
146500130906     C                   Eval      Anno0502    = D1Anno
146600140401R133 C                   Eval      Mese0502    = MeseG74
146700130906     C*
146800130906     C                   Eval      CF0502       = CFG74
146900130906     C                   if        CF0502      = *Blank
147000130906     C                   MoveL     *Hival        CF0502
147100130906     C                   endif
147200130906     C*
147300130906     C                   Eval      Sogg0502    = D1Sogg
147400130906     C                   if        Sogg0502    = *Blank
147500130906     C                   MoveL     *Hival        Sogg0502
147600130906     C                   endif
147700131206R141 C*
147800131206 "   C                   Eval      Causal0502  = CausaleG74
147900131206 "   C                   if        Causal0502  = *Blank
148000131206R141 C                   MoveL     *Hival        Causal0502
148100131206     C                   endif
148200130911     C*
148300130911     C                   Eval      TpN0502     = D1TpN
148400130911     C                   if        TpN0502     = *Blank
148500130911     C                   MoveL     *Hival        TpN0502
148600130911     C                   endif
148700130906     C*
148800130919     C                   Eval      TpOpe0502   = TpOpeG74
148900130919     C                   if        TpOpe0502   = *Blank
149000130919     C                   MoveL     *Hival        TpOpe0502
149100130906     C                   endif
149200130919     C*
149300130919     C                   Eval      Quadro0502  = QuadroG74
149400130919     C                   if        Quadro0502  = *Blank
149500130919     C                   MoveL     *Hival        Quadro0502
149600130919     C                   endif
149700130906     C*
149800130906     C                   Eval      CogRag0502  = D1CogRag
149900130906     C                   Eval      Nome0502    = D1Nome
150000130906     C                   Eval      DtNasc0502  = WDtNasc
150100130906     C                   Eval      LocNas0502  = D1LocNasc
150200130906     C                   Eval      PrvNas0502  = D1PrvNasc
150300130906     C                   Eval      Stato0502   = D1Stato
150400130906     C                   Eval      CodUIC0502  = D1CodUIC
150500130906     C                   Eval      Locali0502  = D1Localit
150600130906     C                   Eval      IndirE0502  = D1IndirE
150700130906     C                   Eval      PIVA0502    = D1PartIva
150800130906     C                   Eval      CdFisc0502  = D1CdFisc
150900130906     C*
151000130906     C                   MoveL     *Hival        TpN0502
151100130906     C                   MoveL     *Hival        COGRAR0502
151200130906     C                   MoveL     *Hival        NOMER0502
151300130906     C                   MoveL     *Hival        DTNASR0502
151400130906     C                   MoveL     *Hival        LOCNAR0502
151500130906     C                   MoveL     *Hival        PRVNAR0502
151600130906     C                   MoveL     *Hival        STATOR0502
151700130906     C                   MoveL     *Hival        CODUIR0502
151800130906     C                   MoveL     *Hival        LOCALR0502
151900130906     C                   MoveL     *Hival        INDIER0502
152000130906     C*
152100130906     C                   MoveL     *Hival        TpNR0502
152200130906     C                   MoveL     *Hival        PIVAR0502
152300130906     C                   MoveL     *Hival        CDFISR0502
152400130925     C                   MoveL     *Hival        TPOPER0502
152500130925     C                   MoveL     *Hival        NATOPR0502
152600130906     C                   MoveL     *Hival        LEASIR0502
152700130906     C                   MoveL     *Hival        DTREGR0502
152800130906     C                   MoveL     *Hival        NRREGR0502
152900130906     C                   MoveL     *Hival        SERRER0502
153000000000     C*
153100130906     C                   CALLB     'SDG00502R'
153200000000     C                   PARM                    KPJBA
153300130906     C                   PARM                    SDG00502DS
153400130906     C                   PARM                    SDG00592DS
153500130906     C                   PARM                    SDG005FADS
153600130906     C                   PARM                    SDG005SADS
153700130906     C                   PARM                    SDG005BLDS
153800130906     C                   PARM                    SDG005FEDS
153900130906     C                   PARM                    SDG005FRDS
154000130906     C                   PARM                    SDG005NEDS
154100130906     C                   PARM                    SDG005NRDS
154200130906     C                   PARM                    SDG005DFDS
154300130906     C                   PARM                    SDG005FNDS
154400130906     C                   PARM                    SDG005SEDS
154500130906     C                   PARM                    SDG005TADS
154600000000     C*
154700000000     C                   ENDSR
154800000000     C/EJECT
154900000000     C************************************************************
155000000000     C* GESTIONE VIDEO RECORD  D2
155100000000     C************************************************************
155200000000    >C     GESD2         BEGSR
155300000000     C* emissione piede videata se proveniente da altra
155400000000     C* salvataggio valore formato di provenienza
155500000000   B1C     $LASTV        IFNE      'D2'
155600000000     C                   WRITE     FMTT1
155700000000     C                   EXSR      GESPIE
155800000000     C                   MOVE      $LASTV        $LASTG
155900000000     C                   MOVE      'D2'          $LASTV
156000000000   E1C                   ENDIF
156100000000     C*
156200000000     C* se esistono errori sulla videata
156300000000     C* emetto la write del formato a indicatori spenti per vedere
156400000000     C* le eventuali decodifiche
156500131001   B1C                   if        (*IN99       = *On   or
156600131001   B1C                              *In17       = *On      )  and
156700130926     C                             $GEST        = 'D2'
156800000000     C                   MOVEA     *IN           WIN
156900000000     C                   MOVE      *ALL'0'       IN5098           49
157000000000     C                   MOVEA     IN5098        *IN(50)
157100131001     C                   Eval      WIn17        = *In17
157200131001     C                   Eval      *In17        = *Off
157300000000     C                   WRITE     FMTD2
157400000000     C                   MOVEA     WIN           *IN
157500131001     C                   Eval      *In17        = WIn17
157600000000   E1C                   ENDIF
157700000000     C* Inizializzazione indice tasti funzione personalizzati
157800000000     C                   Eval      PosFun = 25
157900000000     C*
158000000000   B1C                   SELECT
158100000000     C* Annullamento
158200130906    >C     OPZG74        WHENEQ    '04'
158300000000     C* Visualizzazione
158400130906    >C     OPZG74        OREQ      '05'
158500000000     C                   WRITE     FMTD2
158600000000     C                   EXFMT     PROTECT
158700000000     C* Immissione/Modifica/Copia
158800000000   X1C                   OTHER
158900000000     C                   EXFMT     FMTD2
159000000000   E1C                   ENDSL
159100000000     C*
159200000000     C* azzero la posizione del cursore
159300000000     C                   Z-ADD     0             H1RIGA
159400000000     C                   Z-ADD     0             H1COLO
159500000000     C*
159600000000     C* controllo abilitazione tasti funzione
159700000000     C                   EXSR      CTRKD1
159800000000     C*
159900000000   B1C     *IN99         IFEQ      *OFF
160000000000     C*
160100000000   B2C                   SELECT
160200000000     C* ROLDOWN
160300000000     C     $TASTO        WHENEQ    ROLDWN
160400000000     C                   MOVE      'D1'          $GEST
160500000000     C* F3=Fine
160600000000     C     $TASTO        WHENEQ    F03
160700000000     C                   EXSR      F03D1
160800000000     C* F4=Ricerca
160900000000     C     $TASTO        WHENEQ    F04
161000000000     C                   EXSR      SEARCH
161100000000     C* F5=Refresh
161200000000     C     $TASTO        WHENEQ    F05
161300000000     C                   EXSR      F05D2
161400000000     C* F8=Successivo
161500000000     C     $TASTO        WHENEQ    F08
161600000000     C                   EXSR      F08D1
161700000000     C* F12=Ritorno
161800000000     C     $TASTO        WHENEQ    F12
161900000000     C                   EXSR      F12D1
162000000000     C* F24=Altri tasti
162100000000     C     $TASTO        WHENEQ    F24
162200000000     C                   EXSR      F24D2
162300000000     C* Personalizzata
162400130906     C* NB chiamata NON possibile perchè SDGSG74DS più lunga
162500000000     C* di KPJBU 256 caratteri                          a
162600000000     C**** $PGF1(PosFun) Whenne    *BLANK
162700000000     C****               EXSR      FperD1
162800000000     C*
162900000000   X2C                   OTHER
163000000000     C                   EXSR      CTRD2
163100000000   B3C     *IN99         IFEQ      *OFF
163200000000     C                   EXSR      CTRD1
163300130926   B1C                   if        *IN99       = *On
163400000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
163500000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
163600000000     C* videate
163700000000     C                   MOVE      *ALL'0'       IN5098           49
163800000000     C                   MOVEA     IN5098        *IN(50)
163900000000     C                   SETON                                        9996                       96
164000000000     C                   ENDIF
164100000000     C                   ENDIF
164200000000     C*
164300000000     C*
164400000000   B3C     $TASTO        IFEQ      F06
164500000000     C     *IN99         ANDEQ     *OFF
164600000000     C* eseguo aggiornamento
164700000000     C                   EXSR      AGGANA
164800000000     C* eseguo operazioni del dopo-aggiornamento
164900000000   B4C     *IN99         IFEQ      *OFF
165000000000     C                   EXSR      GESAGG
165100000000   E4C                   ENDIF
165200000000   E3C                   ENDIF
165300000000     C*
165400000000   E2C                   ENDSL
165500000000     C*
165600000000   E1C                   ENDIF
165700000000     C*
165800000000    >C                   ENDSR
165900000000     C/EJECT
166000000000     C************************************************************
166100000000     C* RIEMPIMENTO VIDEATA D2
166200000000     C************************************************************
166300000000    >C     RIED2         BEGSR
166400000000     C*
166500000000     C*
166600000000    >C                   ENDSR
166700000000     C/EJECT
166800000000     C************************************************************
166900000000     C* GESTIONE F05 VIDEO D2
167000000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
167100000000     C* e il riposizionamento alla prima
167200000000     C************************************************************
167300000000    >C     F05D2         BEGSR
167400000000     C*
167500000000     C                   MOVE      *ON           $INZD1
167600000000     C                   MOVE      'D1'          $GEST
167700000000     C*
167800000000    >C                   ENDSR
167900000000     C************************************************************
168000000000     C* GESTIONE F24 VIDEO D2
168100000000     C************************************************************
168200000000    >C     F24D2         BEGSR
168300000000     C*
168400000000     C                   MOVEA     $FX1          $FX
168500000000     C                   MOVEA     $FC1          $FC
168600000000     C                   MOVEA     $FR1          $FR
168700000000     C                   Z-ADD     $ULKD1        $ULK
168800000000     C                   Z-ADD     $D1L01        $L01
168900000000     C                   Z-ADD     $D1L02        $L02
169000000000     C                   EXSR      RIEKEY
169100000000     C                   Z-ADD     $ULK          $ULKD1            3 0
169200000000     C                   MOVEA     $FC           $FC1
169300000000     C                   MOVEL     $KEY1         Z1KE1
169400000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
169500000000     C                   MOVEL     $KEY2         Z1KE2
169600000000     C                   WRITE     FMTZ1
169700000000     C*
169800000000    >C                   ENDSR
169900000000     C************************************************************
170000000000     C* CONTROLLO VIDEATA  D2
170100000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
170200000000     C* 95 96 97 98
170300000000     C************************************************************
170400000000    >C     CTRD2         BEGSR
170500000000     C*
170600000000     C                   SETOFF                                       99
170700000000     C*
170800000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
170900000000     C*
171000130906    >C     OPZG74        IFNE      '04'
171100000000     C*
171200000000     C                   EXSR      CtrStato
171300130930     C*
171400130930     C                   EXSR      CtrCodUIC
171500000000     C*
171600000000     C* Località di residenza
171700000000     C                   EXSR      CtrLocalit
171800000000     C*
171900000000     C* Indirizzo estero
172000000000     C                   EXSR      CtrIndirE
172100000000     C*
172200000000     C                   ENDIF
172300000000     C*
172400000000     C                   ENDSR
172500000000     C/EJECT
172600130919     C***********************************************************
172700130919     C* Controllo Stato
172800130919     C***********************************************************
172900130919     C     CtrStato      BEGSR
173000130919     C*
173100130919     C                   Clear                   D1StatoD
173200130919     C*
173300130919     C                   Clear                   AngG12DS
173400130919     C*
173500130923     C                   Clear                   SDGTABDS
173600131001     C*
173700131001     C                   if        D1Stato     = *Blank     and
173800131001     C                             D1CodUIC   <> 0
173900131001     C* deduco il codice Stato dal codice UIC in tab.G12
174000131001     C                   EXSR      RepStato
174100131001     C                   endif
174200130919     C*
174300130930     C                   if        D1Stato    <> *Blank
174400130919     C                   Eval      XTbRic      = '1'
174500130919     C                   Eval      XTbAzi      = XScSoc
174600130919     C                   Eval      XTbKey      = *Zero
174700130919     C                   Eval      XTbCod      = 'G12'
174800130919     C                   Move      D1Stato       XTbKey
174900130919     C*
175000130919     C                   CALL      'SDGTABDR'
175100130919     C                   PARM                    SDGTABDS
175200130919     C*
175300130919     C                   if        XTbErr     <> '0'
175400130930     C* AVVERTIMENTO
175500131001     C                   Eval      *In17       = *On
175600130919     C*
175700130919     C                   else
175800130919     C                   MoveL     XTbUni        AngG12DS
175900130919     C                   MoveL     DesG12        D1StatoD
176000130919     C*
176100130930     C                   if        D1CodUIC    = 0
176200130919     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
176300130919     C                   MoveL     *All'0'       D1CODUIC
176400130919     C                   else
176500130919     C                   MoveL     CodUICG12     D1CodUIC
176600130919     C                   endif
176700130930     C                   endif
176800130919     C*
176801151201R195 C* Acquisti da San Marino
176900140401R133 C                   if        TpComG74    = 'S'   and
177000140401 "   C                             D1CodUIC    <> 037
177100140401 "   C                   Seton                                        5599
177200140401 "   C                   endif
177300140401R133 C*
177400130919     C                   endif
177500130930     C*
177600130930     C                   endif
177700130930     C*
177800130930     C                   ENDSR
177900130930     C/EJECT
178000131001     C************************************************************
178100131001     C* Reperimento codice Stato da tab. G12
178200131001     C************************************************************
178300131001     C     RepStato      BEGSR
178400131001     C*
178500131001     C* posizionamento  e
178600131001     C* lettura sequenziale
178700131001     C                   Clear                   SDGTABDS
178800131001     C                   Eval      XTbRic      = '2'
178900131001     C                   EXSR      ReadTabG12
179000131001     C                   Eval      $FineG12    = XTbErr
179100131001     C*
179200131001     C                   DoW       $FineG12    = *Off
179300131001     C*
179400131001     C                   MoveL     XTbUni        ANGG12DS
179500131001     C                   if        CodUICG12   = D1CodUIC
179600131001     C                   Move      XTbKey        D1Stato
179700131001     C                   Eval      $FineG12    = *On
179800131001     C                   ITER
179900131001     C                   endif
180000131001     C*
180100131001     C* lettura sequenziale
180200131001     C                   Clear                   SDGTABDS
180300131001     C                   Eval      XTbRic      = '3'
180400131001     C                   EXSR      ReadTabG12
180500131001     C                   Eval      $FineG12    = XTbErr
180600131001     C                   endDo
180700131001     C*
180800131001     C                   ENDSR
180900131001     C/EJECT
181000131001     C********************************************************************
181100131001     C* Reperimento dati da tab.G12
181200131001     C********************************************************************
181300131001     C     ReadTabG12    BEGSR
181400131001     C*
181500131001     C                   Eval      XTbAzi      = XScSoc
181600131001     C****************   Eval      XTbKey      = *Zero
181700131001     C                   Eval      XTbCod      = 'G12'
181800131001     C*
181900131001     C                   CALL      'SDGTABDR'
182000131001     C                   PARM                    SDGTABDS
182100131001     C*
182200131001     C                   ENDSR
182300131001     C/EJECT
182400130930     C************************************************************
182500130930     C* Controllo codice UIC
182600130930     C************************************************************
182700130930     C     CtrCodUIC     BEGSR
182800130923     C*
182900130923     C                   if        D1CodUIC    = *All'0'
183000130923     C                   Seton                                        5499
183100130923     C                   endif
183200130919     C*
183300130919     C                   ENDSR
183400130919     C/EJECT
183500000000     C************************************************************
183600000000     C* Controllo Località di residenza
183700000000     C************************************************************
183800000000     C     CtrLocalit    BEGSR
183900000000     C*
184000000000     C     ''''          Scan      D1Localit                              22
184100000000     C     *IN22         IFEQ      *On
184200000000     C                   SetOn                                        6299
184300000000     C                   endif
184400000000     C*
184500000000     C                   ENDSR
184600000000     C/EJECT
184700000000     C************************************************************
184800000000     C* Controllo indirizzo estero
184900000000     C************************************************************
185000000000     C     CtrIndirE     BEGSR
185100000000     C*
185200000000     C     ''''          Scan      D1IndirE                               22
185300000000     C     *IN22         IFEQ      *On
185400000000     C                   SetOn                                        6399
185500000000     C                   endif
185600000000     C*
185700000000     C                   ENDSR
185800000000     C/EJECT
185900000000     C************************************************************
186000000000     C* AGGIORNAMENTO ANAGRAFICA
186100000000     C************************************************************
186200000000     C     AGGANA        BEGSR
186300000000     C*
186400000000     C                   SELECT
186500130906    >C     OPZG74        WHENEQ    '01'
186600130906    >C     OPZG74        OREQ      '03'
186700000000     C                   EXSR      RIEANA
186800130906    >C                   WRITE     SDGSCF000                            22
186900000000     C* se rcd non scrivibile attivo errore
187000000000     C     *IN22         IFEQ      *ON
187100000000     C                   SETON                                        9599
187200000000     C                   ENDIF
187300130906    >C     OPZG74        WHENEQ    '02'
187400130906     C                   EXSR      UpdSDGSCF
187500130906    >C     OPZG74        WHENEQ    '04'
187600130906     C                   EXSR      DelSDGSCF
187700000000     C                   ENDSL
187800000000     C*
187900000000     C* richiama pgm User Exit
188000000000     C     USRERR        IFEQ      *BLANK
188100000000     C* passa opzione ricevuta a user exit
188200130906    >C                   MOVEL     OPZG74        XUKOPZ
188300000000???? C* imposta chiave univoca  per user exit
188400000000???? C* la chiave univoca non esiste perchè sto lavorando
188500000000???? C* con raggruppamento di dati
188600000000     C                   MOVE      XSCSOC        XUKSOC
188700000000     C                   MOVE      XSCDUN        XUKDUN
188800000000     C                   MOVEL     DSPGM         XUKPGM
188900000000    >C                   MOVEL     *Blank        XUKFL1
189000000000    >C                   MOVEL     *Blank        XUKFL2
189100000000    >C* ho fino a 6 campi chiave passabili
189200000000     C                   MOVEL     XUKDS         KPJBU
189300000000     C     DSPGM         CAT       'U':0         USREXT           10
189400000000     C                   CALL      USREXT                               21
189500000000     C                   PARM                    KPJBA
189600000000     C     *IN21         IFEQ      *ON
189700000000     C                   MOVE      *ON           USRERR            1
189800000000     C                   ENDIF
189900000000     C                   ENDIF
190000000000     C*
190100000000     C                   ENDSR
190200000000     C/EJECT
190300000000     C************************************************************
190400000000     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
190500000000     C************************************************************
190600000000     C     RIEANA        BEGSR
190700000000     C*
190800130906     C                   if        OPZG74      = '01'   or
190900130906     C                             OPZG74      = '03'
191000000000     C*
191100130906     C                   CLEAR                   SDGSCF000
191200000000     C*
191300130906     C                   Eval      SCFORIGINE  = 'M'
191400140401R133 C****               Eval      SCFTpCom    = 'E'
191500140401R133 C                   Eval      SCFTpCom    = TpComG74
191600160104R195 X****               Eval      SCFFormato  = FormatoA08
191700000000     C*
191800000000     C* Reperisco il numero assoluto della registrazione fittizia
191900000000     C                   EXSR      RepNrAss
192000130906     C                   Eval      SCFSYS      = $Sys
192100130906     C                   Eval      SCFNRASREG  = $NrAsReg
192200130906     C                   Eval      SCFNRRIGA   = 1
192300000000     C*
192400000000     C* imposto la data e l'utente di immissione
192500130906     C                   Eval      SCFDtImm    = UDateIso
192600130906     C                   Eval      SCFUteImm   = XScCUt
192700000000     C*
192800130906     C                   Eval      SCFAnno     = AnnoG74
192900140401R133 C****               Eval      SCFMese     = 0
193000140401R133 C                   Eval      SCFMese     = MeseG74
193100110824     C*
193200130906     C                   if        OPZG74      = '01'
193300130918     C                   Eval      SCFTpDoc    = 'S'
193400130918     C                   Eval      SCFTpOpe    = 'A'
193500130911     C                   Eval      SCFNoIVA    = '0'
193600130911     C                   Eval      SCFMargine  = '0'
193700130911     C                   Eval      SCFReverse  = '0'
193800130911     C                   Eval      SCFAutofat  = '0'
193900130911     C                   Eval      SCFLeasing  = '0'
193901151203R195 C*
193902151203 "   C                   Select
193903151203 "   C* se Spesometro o Acquisti da San Marino
193904151203 "   C                   When      TpComG74    = 'E'        or
193905151203R195 C                             TpComG74    = 'S'
194000130911     C                   Eval      SCFNatOpe   = BeniSrvA08
194001151203R195 C*
194002151203 "   C                   When      TpComG74    = 'B'
194003151203 "   C                   Eval      SCFNatOpe   = BeniSrvA11
194004151203 "   C                   endsl
194005151203R195 C*
194100130911     C                   Eval      SCFTpAnn    = *Blank
194200130911     C                   Eval      SCFAnn      = *Blank
194300110824     C                   endif
194400130911     C                   Eval      SCFSogg     = D1Sogg
194500130911     C                   Eval      SCFTPN      = D1TpN
194600000000     C                   endif
194700000000     C*
194800000000     C* imposto la data e l'utente di ultima modifica
194900130906     C                   Eval      SCFDtUlMo   = UDateIso
195000130906     C                   Eval      SCFUteUlMo  = XScCUt
195100000000     C*
195200000000     C* imposto i campi del file da quelli video
195300130906     C                   MOVEL     XscSoc        SCFSocieta
195400130906     C                   Eval      SCFPartIva  = D1PartIva
195500130906     C                   Eval      SCFCdFisc   = D1CdFisc
195600130906     C                   Eval      SCFCogRag   = D1CogRag
195700130906     C                   Eval      SCFNome     = D1Nome
195800000000     C*
195900130906     C                   Eval      SCFDtNasc   = WDtNasc
196000130906     C                   Eval      SCFLocNasc  = D1LocNasc
196100130906     C                   Eval      SCFPrvNasc  = D1PrvNasc
196200130906     C                   Eval      SCFStato    = D1Stato
196300130906     C                   Eval      SCFCodUIC   = D1CodUIC
196400130906     C                   Eval      SCFLocalit  = D1Localit
196500130906     C                   Eval      SCFIndirE   = D1IndirE
196600130923     C*
196601160104R195 C                   Select
196602160104 "   C* Spesometro
196603160104R195 C                   When      TpComG74    = 'E'
196700130923     C                   EXSR      DefQuadri
196701160104R195 C*
196702160104 "   C* Acquisti da San Marino
196703160104 "   C                   When      TpComG74    = 'S'
196704160104 "   C                   EXSR      DefQuadroSE
196705160104 "   C*
196706160104 "   C* Black List
196707160104 "   C                   When      TpComG74    = 'B'
196708160104 "   C                   EXSR      DefQuadroBL
196709160104R195 C                   endSl
196800000000     C*
196900000000     C                   ENDSR
197000000000     C/EJECT
197100130923     C************************************************************
197200130923     C* Definizione dati per Quadri
197300130923     C************************************************************
197400130923     C     DefQuadri     BEGSR
197401160104R195 C*
197402160104R195 C                   Eval      SCFFormato  = FormatoA08
197500130923     C*
197600130923     C                   Select
197700130923     C*
197800130923     C*--------------------
197900130923     C* Formato AGGREGATO
198000160104     C                   When      FormatoA08  = 'A'
198100131017R136 C* controllo se documento legato al turismo (ovvero,
198200131017 "   C* codice fiscale non impostato)
198300131017R136 C                   if        SCFCdFisc  <> *Blank
198400130923     C                   Eval      SCFQuadro   = 'SA'
198500130923     C                   Eval      SCFSeqQua   = 02
198600131017R136 C*
198700131017 "   C* si tratta di operazioni legate al Turismo
198800131017 "   C                   else
198900131017 "   C                   Eval      SCFQuadro   = 'TU'
199000131017 "   C                   Eval      SCFSeqQua   = 11
199100131017R136 C                   endif
199200130923     C*
199300130923     C*
199400130923     C*--------------------
199500130923     C* Formato DETTAGLIO
199600160104     C                   When      FormatoA08  = 'D'
199700131017R136 C* controllo se documento legato al turismo (ovvero,
199800131017 "   C* codice fiscale non impostato)
199900131017R136 C                   if        SCFCdFisc  <> *Blank
200000130923     C                   Eval      SCFQuadro   = 'DF'
200100130923     C                   Eval      SCFSeqQua   = 08
200200131017R136 C*
200300131017 "   C* si tratta di operazioni legate al Turismo
200400131017 "   C                   else
200500131017 "   C                   Eval      SCFQuadro   = 'TU'
200600131017 "   C                   Eval      SCFSeqQua   = 11
200700131017R136 C                   endif
200800130923     C*
200900130923     C                   endSl
201000130923     C*
201100131017R136 C                   if        SCFCdFisc  <> *Blank
201200130923     C                   Eval      SCFCOGRAGR  = *Blank
201300130923     C                   Eval      SCFNOMER    = *Blank
201400130923     C                   Eval      SCFDTNASR   = *Loval
201500130923     C                   Eval      SCFLOCNASR  = *Blank
201600130923     C                   Eval      SCFPRVNASR  = *Blank
201700130923     C                   Eval      SCFSTATOR   = *Blank
201800130923     C                   Eval      SCFCODUICR  = 0
201900130923     C                   Eval      SCFLOCALIR  = *Blank
202000130923     C                   Eval      SCFINDIRER  = *Blank
202100130923     C                   Eval      SCFTpNR     = *Blank
202200130923     C                   Eval      SCFPIVAR    = *Blank
202300130923     C                   Eval      SCFCDFISCR  = D1CdFisc
202301160201R195 C                   if        SCFTpOpe    = 'P'
202302160201 "   C                   Eval      SCFTpOpeR   = 'P'
202303160201 "   C                   if        SCFNatOpe   = 'S'
202304160104 "   C                   Eval      SCFNatOpeR  = 'S'
202305160201 "   C                   endif
202308160201R195 C                   else
202400130925     C                   Eval      SCFTpOpeR   = *Blank
202500130925     C                   Eval      SCFNatOpeR  = *Blank
202501160104R195 C                   endif
202600130923     C                   Eval      SCFLeasinR  = *Blank
202700130923     C                   Eval      SCFDTREGR   = *Loval
202800130923     C                   Eval      SCFNRREGR   = 0
202900130923     C                   Eval      SCFSERREGR  = *Blank
203000131017R136 C*
203100131017 "   C* si tratta di operazioni legate al Turismo
203200131017 "   C                   else
203300131017 "   C*
203400131017 "   C                   Eval      SCFCOGRAGR  = SCFCOGRAG
203500131017 "   C                   if        SCFTpN      = '1'
203600131017 "   C                   Eval      SCFNOMER    = SCFNOME
203700131017 "   C                   Eval      SCFDTNASR   = SCFDTNASC
203800131017 "   C                   Eval      SCFLOCNASR  = SCFLOCNASC
203900131017 "   C                   Eval      SCFPRVNASR  = SCFPRVNASC
204000131017 "   C                   Eval      SCFSTATOR   = SCFSTATO
204100131017 "   C                   Eval      SCFCODUICR  = SCFCODUIC
204200131017 "   C                   Eval      SCFLOCALIR  = SCFLOCALIT
204300131017 "   C                   Eval      SCFINDIRER  = SCFINDIRE
204400131017 "   C                   else
204500131017 "   C                   endif
204600131017 "   C                   Eval      SCFTpNR     = SCFTPN
204700131017 "   C                   Eval      SCFPIVAR    = *Blank
204800131017 "   C                   Eval      SCFCDFISCR  = *Blank
204900131017 "   C                   Eval      SCFTpOpeR   = *Blank
205000131017 "   C                   Eval      SCFNatOpeR  = *Blank
205100131017 "   C                   Eval      SCFLeasinR  = *Blank
205200131017 "   C                   Eval      SCFDTREGR   = SCFDTREG
205300131017 "   C                   Eval      SCFNRREGR   = SCFNRREG
205400131017 "   C                   Eval      SCFSERREGR  = SCFSERREG
205500131017R136 C                   endif
205600130923     C*
205700130923     C                   ENDSR
205800130923     C/EJECT
205801160104R195 C************************************************************
205802160104 "   C* Definizione dati per Quadro SE per Aquisti da San Marino
205804160104 "   C************************************************************
205805160104R195 C     DefQuadroSE   BEGSR
205831160104     C*
205832160104     C                   Eval      SCFFormato  = 'D'
205845160104     C                   Eval      SCFQuadro   = 'SE'
205846160104     C                   Eval      SCFSeqQua   = 10
205847160104     C*
205848160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
205849160104     C                   if        SCFTpN      = '1'
205850160104     C                   Eval      SCFNOMER    = SCFNOME
205851160104     C                   Eval      SCFDTNASR   = SCFDTNASC
205852160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
205853160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
205854160104     C                   Eval      SCFSTATOR   = SCFSTATO
205855160104     C                   Eval      SCFCODUICR  = SCFCODUIC
205856160104     C                   else
205857160104     C                   Eval      SCFSTATOR   = SCFSTATO
205858160104     C                   Eval      SCFCODUICR  = SCFCODUIC
205859160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
205860160104     C                   Eval      SCFINDIRER  = SCFINDIRE
205861160104     C                   endif
205862160104     C                   Eval      SCFTpNR     = SCFTPN
205863160104     C                   Eval      SCFPIVAR    = SCFPartIVA
205864160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
205865160201     C                   Eval      SCFTpOpeR   = SCFTpOpe
205866160104     C                   Eval      SCFNatOpeR  = *Blank
205867160104     C                   Eval      SCFLeasinR  = *Blank
205868160104     C                   Eval      SCFDTREGR   = SCFDTREG
205869160104     C                   Eval      SCFNRREGR   = SCFNRREG
205870160104     C                   Eval      SCFSERREGR  = SCFSERREG
205901160104     C*
205902160104R195 C                   ENDSR
205903160104R195 C/EJECT
205904160104R195 C************************************************************
205905160104 "   C* Definizione dati per Quadro BL in comunicaz. Black List
205906160104 "   C************************************************************
205907160104R195 C     DefQuadroBL   BEGSR
205908160104     C*
205909160104     C                   Eval      SCFFormato  = 'A'
205910160104     C                   Eval      SCFQuadro   = 'BL'
205911160104     C                   Eval      SCFSeqQua   = 03
205912160104     C*
205913160104     C                   Eval      SCFCOGRAGR  = SCFCOGRAG
205914160104     C                   if        SCFTpN      = '1'
205915160104     C                   Eval      SCFNOMER    = SCFNOME
205916160104     C                   Eval      SCFDTNASR   = SCFDTNASC
205917160104     C                   Eval      SCFLOCNASR  = SCFLOCNASC
205918160104     C                   Eval      SCFPRVNASR  = SCFPRVNASC
205919160104     C                   Eval      SCFSTATOR   = SCFSTATO
205920160104     C                   Eval      SCFCODUICR  = SCFCODUIC
205921160104     C                   else
205922160104     C                   Eval      SCFSTATOR   = SCFSTATO
205923160104     C                   Eval      SCFCODUICR  = SCFCODUIC
205924160104     C                   Eval      SCFLOCALIR  = SCFLOCALIT
205925160104     C                   Eval      SCFINDIRER  = SCFINDIRE
205926160104     C                   endif
205927160104     C                   Eval      SCFTpNR     = SCFTpN
205928160104     C                   Eval      SCFPIVAR    = SCFPartIVA
205929160104     C                   Eval      SCFCDFISCR  = SCFCdFisc
205935160104     C                   Eval      SCFTpOpeR   = *Blank
205936160104     C                   Eval      SCFNatOpeR  = *Blank
205938160104     C                   Eval      SCFLeasinR  = *Blank
205939160104     C                   Eval      SCFDTREGR   = *Loval
205940160104     C                   Eval      SCFNRREGR   = *Zero
205941160104     C                   Eval      SCFSERREGR  = *Blank
205942160104     C*
205943160104R195 C                   ENDSR
205944160104R195 C/EJECT
205945000000     C************************************************************
206000130906     C* Aggiornamento  SDGSCF00F
206100000000     C************************************************************
206200130906     C     UpdSDGSCF     BEGSR
206300000000     C*
206400130906     C                   RESET                   SDG00502DS
206500110822     C*
206600130906     C                   Eval      Ann0502     = AnnG74
206700130906     C                   Eval      TpAnn0502   = TpAnnG74
206800130906     C                   if        TpAnn0502   = *Blank
206900130906     C                   MoveL     *Hival        TpAnn0502
207000110824     C                   endif
207100000000     C*
207200130906     C**********         MOVE      $ORD          ORD0502
207300000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
207400000000     C* inizializzazione delle variabili SQL
207500130906     C                   MOVE      '0'           RIC0502
207600130906     C                   MOVE      '02'          OPZ0502
207700130906     C                   MOVE      '02'          TPROTT0502
207800130906     C                   MOVE      '01'          ROTT0502
207900130906     C                   MOVE      '01'          TpList0502
208000000000     C*
208100130906     C                   Eval      Soc0502     = SocietaG74
208101151203R195 C*
208102151203 "   C                   Select
208103151203 "   C* se Spesometro
208104151203R195 C                   When      TpComG74    = 'E'
208200130918     C                   Eval      Format0502  = FormatoA08
208300131015R135 C                   Eval      DocRie0502  = DocRieA08
208301151203R195 C*
208302151203 "   C* se Acquisti da San Marino
208303151203 "   C                   When      TpComG74    = 'S'
208304151203 "   C                   Eval      Format0502  = 'D'
208305151203 "   C                   Eval      DocRie0502  = *Blank
208306151203 "   C*
208307151203 "   C* se Black List
208308151203 "   C                   When      TpComG74    = 'B'
208309151203 "   C                   Eval      Format0502  = 'A'
208310151203 "   C                   Eval      DocRie0502  = *Blank
208311151203 "   C*
208312151203 "   C                   endSl
208313151203R195 C*
208400130918     C                   Eval      UICSoc0502  = $UICSoc
208500151201R195 X****               Eval      TpCom0502    = 'E'
208501151201R195 C                   Eval      TpCom0502    = TpComG74
208600130906     C                   Eval      Anno0502    = AnnoG74
208700130906     C*
208800130906     C                   Eval      CF0502       = CFG74
208900130906     C                   if        CF0502      = *Blank
209000130906     C                   MoveL     *Hival        CF0502
209100130906     C                   endif
209200130906     C*
209300130906     C                   Eval      Sogg0502    = SoggG74
209400130906     C                   if        Sogg0502    = *Blank
209500130906     C                   MoveL     *Hival        Sogg0502
209600130906     C                   endif
209700131206R141 C*
209800131206 "   C                   Eval      Causal0502  = CausaleG74
209900131206 "   C                   if        Causal0502  = *Blank
210000131206 "   C                   MoveL     *Hival        Causal0502
210100131206R141 C                   endif
210200000000     C*
210300130919     C                   Eval      TpOpe0502   = TpOpeG74
210400130919     C                   if        TpOpe0502   = *Blank
210500130919     C                   MoveL     *Hival        TpOpe0502
210600130906     C                   endif
210700130919     C*
210800130919     C                   Eval      Quadro0502  = QuadroG74
210900130919     C                   if        Quadro0502  = *Blank
211000130919     C                   MoveL     *Hival        Quadro0502
211100130919     C                   endif
211200130906     C*
211300130906     C* nei parametri di input imposto i vecchi valori
211400130906     C                   Eval      CogRag0502  = CogRagG74
211500130906     C                   Eval      Nome0502    = NomeG74
211600130906     C                   Eval      DtNasc0502  = DtNascG74
211700130906     C                   Eval      LocNas0502  = LocNascG74
211800130906     C                   Eval      PrvNas0502  = PrvNascG74
211900130906     C                   Eval      Stato0502   = StatoG74
212000130906     C                   Eval      CodUIC0502  = CodUICG74
212100130906     C                   Eval      Locali0502  = LocalitG74
212200130906     C                   Eval      IndirE0502  = IndirEG74
212300130906     C                   Eval      PIVA0502    = PartIvaG74
212400130906     C                   Eval      CdFisc0502  = CdFiscG74
212500110824     C*
212600130906     C                   MoveL     *Hival        TpN0502
212700130906     C                   MoveL     *Hival        COGRAR0502
212800130906     C                   MoveL     *Hival        NOMER0502
212900130906     C                   MoveL     *Hival        DTNASR0502
213000130906     C                   MoveL     *Hival        LOCNAR0502
213100130906     C                   MoveL     *Hival        PRVNAR0502
213200130906     C                   MoveL     *Hival        STATOR0502
213300130906     C                   MoveL     *Hival        CODUIR0502
213400130906     C                   MoveL     *Hival        LOCALR0502
213500130906     C                   MoveL     *Hival        INDIER0502
213600130906     C*
213700130906     C                   MoveL     *Hival        TpNR0502
213800130906     C                   MoveL     *Hival        PIVAR0502
213900130906     C                   MoveL     *Hival        CDFISR0502
214000130925     C                   MoveL     *Hival        TPOPER0502
214100130925     C                   MoveL     *Hival        NATOPR0502
214200130906     C                   MoveL     *Hival        LEASIR0502
214300130906     C                   MoveL     *Hival        DTREGR0502
214400130906     C                   MoveL     *Hival        NRREGR0502
214500130906     C                   MoveL     *Hival        SERRER0502
214600000000     C*
214700000000     C* nei parametri di output imposto i nuovi valori
214800130906     C                   Eval      CogRag0592  = D1CogRag
214900130906     C                   Eval      Nome0592    = D1Nome
215000130906     C                   Eval      DtNasc0592  = WDtNasc
215100130906     C                   Eval      LocNas0592  = D1LocNasc
215200130906     C                   Eval      PrvNas0592  = D1PrvNasc
215300130906     C                   Eval      Stato0592   = D1Stato
215400130906     C                   Eval      CodUIC0592  = D1CodUIC
215500130906     C                   Eval      Locali0592  = D1Localit
215600130906     C                   Eval      IndirE0592  = D1IndirE
215700130906     C                   Eval      PIVA0592    = D1PartIva
215800130906     C                   Eval      CdFisc0592  = D1CdFisc
215900000000     C*
216000130906     C                   CALLB     'SDG00502R'
216100000000     C                   PARM                    KPJBA
216200130906     C                   PARM                    SDG00502DS
216300130906     C                   PARM                    SDG00592DS
216400130906     C                   PARM                    SDG005FADS
216500130906     C                   PARM                    SDG005SADS
216600130906     C                   PARM                    SDG005BLDS
216700130906     C                   PARM                    SDG005FEDS
216800130906     C                   PARM                    SDG005FRDS
216900130906     C                   PARM                    SDG005NEDS
217000130906     C                   PARM                    SDG005NRDS
217100130906     C                   PARM                    SDG005DFDS
217200130906     C                   PARM                    SDG005FNDS
217300130906     C                   PARM                    SDG005SEDS
217400130906     C                   PARM                    SDG005TADS
217500000000     C*
217600000000     C                   ENDSR
217700000000     C/EJECT
217800000000     C************************************************************
217900130906     C* Cancellazione  SDGSCF00F
218000000000     C************************************************************
218100130906     C     DelSDGSCF     BEGSR
218200000000     C*
218300130906     C                   RESET                   SDG00502DS
218400110822     C*
218500130906     C                   Eval      Ann0502     = AnnG74
218600130906     C                   Eval      TpAnn0502   = TpAnnG74
218700130906     C                   if        TpAnn0502   = *Blank
218800130906     C                   MoveL     *Hival        TpAnn0502
218900110824     C                   endif
219000000000     C*
219100130906     C**********         MOVE      $ORD          ORD0502
219200000000     C* richiedo al pgm di lettura il 1° rcd con la conseguente
219300000000     C* inizializzazione delle variabili SQL
219400130906     C                   MOVE      '0'           RIC0502
219500130906     C                   MOVE      '04'          OPZ0502
219600130906     C                   MOVE      '02'          TPROTT0502
219700130906     C                   MOVE      '01'          ROTT0502
219800130906     C                   MOVE      '01'          TpList0502
219900000000     C*
220000130906     C                   Eval      Soc0502     = SocietaG74
220100151201R195 X****               Eval      TpCom0502    = 'E'
220101151201R195 C                   Eval      TpCom0502    = TpComG74
220200130906     C                   Eval      Anno0502    = AnnoG74
220300130906     C*
220400130906     C                   Eval      CF0502       = CFG74
220500130906     C                   if        CF0502      = *Blank
220600130906     C                   MoveL     *Hival        CF0502
220700130906     C                   endif
220800130906     C*
220900130906     C                   Eval      Sogg0502    = SoggG74
221000130906     C                   if        Sogg0502    = *Blank
221100130906     C                   MoveL     *Hival        Sogg0502
221200130906     C                   endif
221300131206R141 C*
221400131206 "   C                   Eval      Causal0502  = CausaleG74
221500131206 "   C                   if        Causal0502  = *Blank
221600131206R141 C                   MoveL     *Hival        Causal0502
221700131206     C                   endif
221800130906     C*
221900130919     C                   Eval      TpOpe0502   = TpOpeG74
222000130919     C                   if        TpOpe0502   = *Blank
222100130919     C                   MoveL     *Hival        TpOpe0502
222200130906     C                   endif
222300130919     C*
222400130919     C                   Eval      Quadro0502  = QuadroG74
222500130919     C                   if        Quadro0502  = *Blank
222600130919     C                   MoveL     *Hival        Quadro0502
222700130919     C                   endif
222800000000     C*
222900130906     C* nei parametri di input imposto i vecchi valori
223000130906     C                   Eval      CogRag0502  = CogRagG74
223100130906     C                   Eval      Nome0502    = NomeG74
223200130906     C                   Eval      DtNasc0502  = DtNascG74
223300130906     C                   Eval      LocNas0502  = LocNascG74
223400130906     C                   Eval      PrvNas0502  = PrvNascG74
223500130906     C                   Eval      Stato0502   = StatoG74
223600130906     C                   Eval      CodUIC0502  = CodUICG74
223700130906     C                   Eval      Locali0502  = LocalitG74
223800130906     C                   Eval      IndirE0502  = IndirEG74
223900130906     C                   Eval      PIVA0502    = PartIvaG74
224000130906     C                   Eval      CdFisc0502  = CdFiscG74
224100130906     C*
224200130906     C                   MoveL     *Hival        TpN0502
224300130906     C                   MoveL     *Hival        COGRAR0502
224400130906     C                   MoveL     *Hival        NOMER0502
224500130906     C                   MoveL     *Hival        DTNASR0502
224600130906     C                   MoveL     *Hival        LOCNAR0502
224700130906     C                   MoveL     *Hival        PRVNAR0502
224800130906     C                   MoveL     *Hival        STATOR0502
224900130906     C                   MoveL     *Hival        CODUIR0502
225000130906     C                   MoveL     *Hival        LOCALR0502
225100130906     C                   MoveL     *Hival        INDIER0502
225200130906     C*
225300130906     C                   MoveL     *Hival        TpNR0502
225400130906     C                   MoveL     *Hival        PIVAR0502
225500130906     C                   MoveL     *Hival        CDFISR0502
225600130925     C                   MoveL     *Hival        TPOPER0502
225700130925     C                   MoveL     *Hival        NATOPR0502
225800130906     C                   MoveL     *Hival        LEASIR0502
225900130906     C                   MoveL     *Hival        DTREGR0502
226000130906     C                   MoveL     *Hival        NRREGR0502
226100130906     C                   MoveL     *Hival        SERRER0502
226200000000     C*
226300130906     C                   CALLB     'SDG00502R'
226400000000     C                   PARM                    KPJBA
226500130906     C                   PARM                    SDG00502DS
226600130906     C                   PARM                    SDG00592DS
226700130906     C                   PARM                    SDG005FADS
226800130906     C                   PARM                    SDG005SADS
226900130906     C                   PARM                    SDG005BLDS
227000130906     C                   PARM                    SDG005FEDS
227100130906     C                   PARM                    SDG005FRDS
227200130906     C                   PARM                    SDG005NEDS
227300130906     C                   PARM                    SDG005NRDS
227400130906     C                   PARM                    SDG005DFDS
227500130906     C                   PARM                    SDG005FNDS
227600130906     C                   PARM                    SDG005SEDS
227700130906     C                   PARM                    SDG005TADS
227800000000     C*
227900000000     C                   ENDSR
228000000000     C/EJECT
228100000000     C***********************************************************
228200000000     C* Reperisco il numero assoluto di registrazione
228300000000     C***********************************************************
228400000000     C     RepNrAss      BEGSR
228500000000     C*
228600000000     C                   CallB     'NDMVC101'
228700000000     C                   PARM      *Off          $Esito
228800000000     C                   PARM      0             $Sys
228900000000     C                   PARM      0             $NrAsReg
229000000000     C                   PARM      '1'           $Prenot
229100000000     C*
229200000000     C                   If        $Esito     =  *On
229300130906    >C                   MOVE      '1'           ErrG74
229400000000     C                   MOVE      *on           $Fine
229500000000     C                   EXSR      ENDPGM
229600000000     C                   endif
229700000000     C*
229800000000     C                   ENDSR
229900000000      /EJECT
230000000000     C************************************************************
230100000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
230200000000     C************************************************************
230300000000     C     GESAGG        BEGSR
230400000000     C*
230500000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
230600130906    >C     CMTG74        IFEQ      '0'
230700000000     C**************     COMMIT
230800000000     C                   ENDIF
230900000000     C*
231000000000     C* segnalo al pgm chiamante l'avvenuta conferma
231100130906    >C                   MOVE      *ON           OPRG74
231200000000     C* segnalo al pgm chiamante l'aver premuto l'F6
231300130906    >C                   MOVE      '0'           RETG74
231400000000     C*
231500000000     C                   SELECT
231600000000     C*
231700000000     C* nel caso di immissione
231800130906    >C     OPZG74        WHENEQ    '01'
231900130906    >C     OPZG74        orEQ      '03'
232000000000     C* ripristino la videata iniziale
232100000000     C                   MOVE      *ON           $INZD1
232200000000     C                   MOVE      'D1'          $GEST
232300000000     C*
232400000000     C* forzo il ritorno al pgm chiamante perchè devo chiamare
232500000000     C* i pgm di manutenzione del dettaglio
232600000000     C                   MOVE      *ON           $FINE
232700000000     C*
232800130906     C                   Eval      SysG74      = SCFSys
232900130906     C                   Eval      NrAsRegG74  = SCFNrAsReg
233000130906     C                   Eval      NrRigaG74   = SCFNrRiga
233100000000     C*
233200130906    >C                   MOVE      xscsoc        SocietaG74
233300130906     C* passare nei campi appositi della DS SDGSG74DS il valore
233400000000     C* della chiave della VL univoca
233500130906     C                   Eval      SoggG74     = D1Sogg
233600130906     C                   Eval      PartIvaG74  = D1PartIva
233700130906     C                   Eval      CdFiscG74   = D1CdFisc
233800130906     C                   Eval      CogRagG74   = D1CogRag
233900130906     C                   Eval      NomeG74     = D1Nome
234000000000     C*
234100130906     C                   Eval      DtNascG74   = WDtNasc
234200000000     C*
234300130906     C                   Eval      LocNascG74  = D1LocNasc
234400130906     C                   Eval      PrvNascG74  = D1PrvNasc
234500130906     C                   Eval      StatoG74    = D1Stato
234600130906     C                   Eval      CodUICG74   = D1CodUIC
234700130906     C                   Eval      LocalitG74  = D1Localit
234800130906     C                   Eval      IndirEG74   = D1IndirE
234900000000     C*
235000000000     C* nel caso di modifica
235100130906    >C     OPZG74        WHENEQ    '02'
235200000000     C* ripasso i campi che devono essere reimpostati sul sfl
235300130906     C                   Eval      PartIvaG74  = D1PartIva
235400130906     C                   Eval      CdFiscG74   = D1CdFisc
235500130906     C                   Eval      CogRagG74   = D1CogRag
235600130906     C                   Eval      NomeG74     = D1Nome
235700130906     C                   Eval      DtNascG74   = WDtNasc
235800130906     C                   Eval      LocNascG74  = D1LocNasc
235900130906     C                   Eval      PrvNascG74  = D1PrvNasc
236000130906     C                   Eval      StatoG74    = D1Stato
236100130906     C                   Eval      CodUICG74   = D1CodUIC
236200130906     C                   Eval      LocalitG74  = D1Localit
236300130906     C                   Eval      IndirEG74   = D1IndirE
236400000000     C* e ritorno al pgm chiamante
236500000000     C                   MOVE      *ON           $FINE
236600000000     C*
236700000000     C* per tutte le altre opzioni
236800000000     C                   OTHER
236900000000     C* ritorno al pgm chiamante
237000000000     C                   MOVE      *ON           $FINE
237100000000     C                   ENDSL
237200000000     C*
237300000000     C                   ENDSR
237400000000     C/EJECT
237500000000     C************************************************************
237600000000     C* CONTROLLO ABILITAZIONE TASTI
237700000000     C************************************************************
237800000000     C     CTRKEY        BEGSR
237900000000     C*
238000000000     C                   SELECT
238100000000     C     $TASTO        WHENEQ    F01
238200000000     C                   Z-ADD     01            PosFun
238300000000     C     $FC(1)        IFEQ      '0'
238400000000     C                   SETON                                        99
238500000000     C                   ENDIF
238600000000     C     $TASTO        WHENEQ    F02
238700000000     C                   Z-ADD     02            PosFun
238800000000     C     $FC(2)        IFEQ      '0'
238900000000     C                   SETON                                        99
239000000000     C                   ENDIF
239100000000     C     $TASTO        WHENEQ    F03
239200000000     C                   Z-ADD     03            PosFun
239300000000     C     $FC(3)        IFEQ      '0'
239400000000     C                   SETON                                        99
239500000000     C                   ENDIF
239600000000     C     $TASTO        WHENEQ    F04
239700000000     C                   Z-ADD     04            PosFun
239800000000     C     $FC(4)        IFEQ      '0'
239900000000     C                   SETON                                        99
240000000000     C                   ENDIF
240100000000     C     $TASTO        WHENEQ    F05
240200000000     C                   Z-ADD     05            PosFun
240300000000     C     $FC(5)        IFEQ      '0'
240400000000     C                   SETON                                        99
240500000000     C                   ENDIF
240600000000     C     $TASTO        WHENEQ    F06
240700000000     C                   Z-ADD     06            PosFun
240800000000     C     $FC(6)        IFEQ      '0'
240900000000     C                   SETON                                        99
241000000000     C                   ENDIF
241100000000     C     $TASTO        WHENEQ    F07
241200000000     C                   Z-ADD     07            PosFun
241300000000     C     $FC(7)        IFEQ      '0'
241400000000     C                   SETON                                        99
241500000000     C                   ENDIF
241600000000     C     $TASTO        WHENEQ    F08
241700000000     C                   Z-ADD     08            PosFun
241800000000     C     $FC(8)        IFEQ      '0'
241900000000     C                   SETON                                        99
242000000000     C                   ENDIF
242100000000     C     $TASTO        WHENEQ    F09
242200000000     C                   Z-ADD     09            PosFun
242300000000     C     $FC(09)       IFEQ      '0'
242400000000     C                   SETON                                        99
242500000000     C                   ENDIF
242600000000     C     $TASTO        WHENEQ    F10
242700000000     C                   Z-ADD     10            PosFun
242800000000     C     $FC(10)       IFEQ      '0'
242900000000     C                   SETON                                        99
243000000000     C                   ENDIF
243100000000     C     $TASTO        WHENEQ    F11
243200000000     C                   Z-ADD     11            PosFun
243300000000     C     $FC(11)       IFEQ      '0'
243400000000     C                   SETON                                        99
243500000000     C                   ENDIF
243600000000     C     $TASTO        WHENEQ    F12
243700000000     C                   Z-ADD     12            PosFun
243800000000     C     $FC(12)       IFEQ      '0'
243900000000     C                   SETON                                        99
244000000000     C                   ENDIF
244100000000     C     $TASTO        WHENEQ    F13
244200000000     C                   Z-ADD     13            PosFun
244300000000     C     $FC(13)       IFEQ      '0'
244400000000     C                   SETON                                        99
244500000000     C                   ENDIF
244600000000     C     $TASTO        WHENEQ    F14
244700000000     C                   Z-ADD     14            PosFun
244800000000     C     $FC(14)       IFEQ      '0'
244900000000     C                   SETON                                        99
245000000000     C                   ENDIF
245100000000     C     $TASTO        WHENEQ    F15
245200000000     C                   Z-ADD     15            PosFun
245300000000     C     $FC(15)       IFEQ      '0'
245400000000     C                   SETON                                        99
245500000000     C                   ENDIF
245600000000     C     $TASTO        WHENEQ    F16
245700000000     C                   Z-ADD     16            PosFun
245800000000     C     $FC(16)       IFEQ      '0'
245900000000     C                   SETON                                        99
246000000000     C                   ENDIF
246100000000     C     $TASTO        WHENEQ    F17
246200000000     C                   Z-ADD     17            PosFun
246300000000     C     $FC(17)       IFEQ      '0'
246400000000     C                   SETON                                        99
246500000000     C                   ENDIF
246600000000     C     $TASTO        WHENEQ    F18
246700000000     C                   Z-ADD     18            PosFun
246800000000     C     $FC(18)       IFEQ      '0'
246900000000     C                   SETON                                        99
247000000000     C                   ENDIF
247100000000     C     $TASTO        WHENEQ    F19
247200000000     C                   Z-ADD     19            PosFun
247300000000     C     $FC(19)       IFEQ      '0'
247400000000     C                   SETON                                        99
247500000000     C                   ENDIF
247600000000     C     $TASTO        WHENEQ    F20
247700000000     C                   Z-ADD     20            PosFun
247800000000     C     $FC(20)       IFEQ      '0'
247900000000     C                   SETON                                        99
248000000000     C                   ENDIF
248100000000     C     $TASTO        WHENEQ    F21
248200000000     C                   Z-ADD     21            PosFun
248300000000     C     $FC(21)       IFEQ      '0'
248400000000     C                   SETON                                        99
248500000000     C                   ENDIF
248600000000     C     $TASTO        WHENEQ    F22
248700000000     C                   Z-ADD     22            PosFun
248800000000     C     $FC(22)       IFEQ      '0'
248900000000     C                   SETON                                        99
249000000000     C                   ENDIF
249100000000     C     $TASTO        WHENEQ    F23
249200000000     C                   Z-ADD     23            PosFun
249300000000     C     $FC(23)       IFEQ      '0'
249400000000     C                   SETON                                        99
249500000000     C                   ENDIF
249600000000     C     $TASTO        WHENEQ    F24
249700000000     C                   Z-ADD     24            PosFun
249800000000     C     $FC(24)       IFEQ      '0'
249900000000     C                   SETON                                        99
250000000000     C                   ENDIF
250100000000     C                   ENDSL
250200000000     C*
250300000000     C                   ENDSR
250400000000     C/EJECT
250500000000     C******************************************************
250600000000     C* PGM PER GESTIONE MESSAGGI ERRORE
250700000000     C******************************************************
250800000000     C     MSGPGM        BEGSR
250900000000     C*
251000000000     C                   Z-ADD     2             MSGMSG
251100000000     C                   MOVEL     'S'           MSGRSP
251200000000     C                   MOVEL     'CR'          MSGTPR
251300000000     C                   MOVEL     'S'           MSGEMV
251400000000     C                   Z-ADD     10            MSGRGP
251500000000     C                   MOVEL     'S'           MSGLCK
251600000000     C                   MOVEA     STATUS        STS
251700000000     C                   MOVEL     'N'           MSGCNL
251800000000     C                   MOVEL     'N'           MSGVID
251900000000     C                   MOVEL     'N'           MSGOPE
252000000000     C                   MOVEL     'N'           MSGSTP
252100000000     C                   CALL      'SDGMSGR'
252200000000     C                   PARM                    SDGMSGDS
252300000000     C*
252400000000     C                   ENDSR
252500000000     C/EJECT
252600000000     C************************************************************
252700000000     C* OPERAZIONI INIZIALI
252800000000     C************************************************************
252900000000     C     *INZSR        BEGSR
253000000000     C*
253100000000     C* Reperimento parametri
253200000000     C*
253300000000     C     *ENTRY        PLIST
253400000000     C                   PARM                    KPJBA
253500130906     C                   PARM                    SDGSG74DS
253600000000     C*
253700000000     C* Reperimento dati società
253800000000     C*
253900000000     C                   MOVEL     'SOC001'      TIPXSC
254000000000     C                   MOVEL     *BLANK        SOCXSC
254100000000     C                   EXSR      REPSOC
254200000000     C*
254300000000     C* Controlli dati società
254400000000     C*
254500000000     C     RTNXSC        IFNE      '1'
254600000000     C                   MOVEL     XSOCDS        SOC001
254700000000     C                   ELSE
254800130906    >C                   MOVE      '1'           ErrG74
254900000000     C                   MOVE      *on           $Fine
255000000000     C                   EXSR      EndPgm
255100000000     C                   ENDIF
255200000000     C*
255300000000     C                   MOVE      *BLANK        AUTDS
255400000000     C*
255500000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
255600000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
255700000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
255800000000     C*
255900000000     C* Reperimento tasti di funzione
256000000000     C*
256100000000     C                   EXSR      REPKEY
256200000000     C*
256300000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
256400000000     C                   CAllb     'X46USREXT'
256500000000     C                   PARM                    *omit
256600000000     C                   PARM                    *omit
256700000000     C                   PARM                    *omit
256800000000     C                   PARM                    *omit
256900000000     C                   PARM                    *omit
257000000000     C                   PARM                    $FX1
257100000000     C                   PARM                    $FC1
257200000000     C                   PARM                    $PGF1
257300000000     C                   PARM                    DsPgm
257400000000     C                   PARM                    xsccut
257500000000     C                   PARM                    xscgrp
257600000000     C*
257700000000     C     *DMY          Move      UDate         UDateISO
257800000000     C*
257900000000     C* reperisco DATFMT del job
258000000000     C                   CallB     'XDATFMT'
258100000000     C                   Parm                    $DatFmt           4
258200000000     C*
258300000000     C                   ENDSR
258400000000     C/EJECT
258500000000     C************************************************************
258600000000     C* REPERIMENTO TASTI DI FUNZIONE
258700000000     C************************************************************
258800000000     C     REPKEY        BEGSR
258900000000     C*
259000000000     C                   MOVEA     $FM1          $FM
259100000000     C                   MOVEA     $FC1          $FC
259200000000     C                   EXSR      REPKKK
259300000000     C                   MOVEA     $FX           $FX1
259400000000     C*
259500000000     C                   ENDSR
259600000000     C/EJECT
259700000000     C************************************************************
259800000000     C* REPERIMENTO TASTI DI FUNZIONE
259900000000     C************************************************************
260000000000     C     REPKKK        BEGSR
260100000000     C*
260200000000     C                   CLEAR                   $FX
260300000000     C     1             DO        24            X
260400000000     C*
260500000000     C     $FM(X)        IFNE      *BLANK
260600000000     C     $FC(X)        ANDNE     '0'
260700000000     C*
260800000000     C                   MOVEL     $FM(X)        IDMSG
260900000000     C                   CALLB     'XRTVM'
261000000000     C                   PARM                    IDMSG            27
261100000000     C                   PARM                    TXTMSG          100
261200000000     C                   PARM                    ERRMSG            1
261300000000     C*
261400000000     C     ERRMSG        IFNE      *ON
261500000000     C                   MOVEL     TXTMSG        $FX(X)
261600000000     C                   ELSE
261700000000     C                   MOVEL     *ALL' '       $FX(X)
261800000000     C                   ENDIF
261900000000     C*
262000000000     C                   ENDIF
262100000000     C*
262200000000     C                   ENDDO
262300000000     C*
262400000000     C                   ENDSR
262500000000     C/EJECT
262600000000     C************************************************************
262700000000     C* INIZIALIZZAZIONE VARIABILI
262800000000     C************************************************************
262900000000     C     INZVAR        BEGSR
263000000000     C*
263100000000     C* Dati da PGM guida
263200130906    >C***********        MOVEL     KPJBU         SDGSG74DS
263300000000     C*
263400000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
263500000000     C* reperimento dati società
263600130906    >C                   If        SocietaG74 <> xscsoc    and
263700130906    >C                             OpzG74 <> *blank
263800000000     C                   MOVEL     'SOC001'      TIPXSC
263900130906    >C                   MOVEL     SocietaG74    SOCXSC
264000000000     C                   EXSR      REPSOC
264100000000     C     RTNXSC        IFNE      '1'
264200000000     C                   MOVEL     XSOCDS        SOC001
264300000000     C                   ELSE
264400130906    >C                   MOVE      '1'           ErrG74
264500000000     C                   MOVE      *on           $Fine
264600000000     C                   EXSR      EndPgm
264700000000     C                   ENDIF
264800000000     C                   endif
264900000000     C*
265000000000     C* Reperimento autorizzazioni
265100000000     C*
265200000000     C* le autorizzazioni potrebbero essere già state passate,
265300000000     C* nel qual caso non vanno reperite
265400000000    >C     AUTDS         IFEQ      *BLANK
265500000000    >C                   EXSR      REPAUT
265600000000    >C                   ELSE
265700000000    >C                   MOVEL     XCADDS        CONCIVDS
265800000000    >C                   ENDIF
265900000000     C*
266000000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
266100000000     C* le autorizzazione
266200000000     C*
266300000000    >C                   EXSR      TSTAUT
266400000000     C*
266500000000     C* Definizione dei tasti funzione utilizzabili a pgm
266600000000     C*
266700000000     C                   EXSR      DEFOPZ
266800000000     C*
266900000000     C* Variabili per gestione videate
267000000000     C*
267100000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
267200000000     C                   MOVE      *OFF          $FINE                          fine pgm
267300000000     C                   MOVE      *OFF          $FlgDef                        fine pgm
267400000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
267500000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
267600000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
267700000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
267800000000     C* azzero la posizione del cursore
267900000000     C                   Z-ADD     0             H1RIGA
268000000000     C                   Z-ADD     0             H1COLO
268100000000     C*
268200000000     C* Variabili appoggio
268300000000     C*
268400000000     C                   Z-ADD     0             CURR
268500000000     C                   Z-ADD     0             CURC
268600000000     C*
268700000000     C* Valorizzazione campi univoci testate
268800000000     C*
268900000000     C                   CLEAR                   FMTT1
269000000000     C                   MOVEL     KNSIF         NOMSIF
269100000000     C                   MOVEL     XSCDSI        NOMDIT
269200000000     C                   MOVEL     DSPGM         NOMPGM           10
269300000000     C                   MOVEL     KCDAZ         NOMAZN
269400000000     C* recupero il sottotitolo (se c'è)
269500000000     C                   MOVEL     *BLANK        T1TIT
269600000000     C     SOTTIT        IFNE      *BLANK
269700000000     C                   MOVEL     SOTTIT        IDMSG
269800000000     C                   CALLB     'XRTVM'
269900000000     C                   PARM                    IDMSG            27
270000000000     C                   PARM                    TXTMSG          100
270100000000     C                   PARM                    ERRMSG            1
270200000000     C                   PARM      *ON           CTRMSG            1            centratura
270300000000     C                   PARM      30            LENMSG            3 0          lun output.
270400000000     C     ERRMSG        IFNE      *ON
270500000000     C                   MOVEL     TXTMSG        T1TIT
270600000000     C                   ELSE
270700000000     C                   MOVEL     *ALL'?'       T1TIT
270800000000     C                   ENDIF
270900000000     C                   ENDIF
270901151201R195 C*
270902151201  "  C* Recupero il titolo con il tipo di comunicazione
270904151201  "  C                   MOVEL     *BLANK        T1TIT
270905151201  "  C*
270906151201  "  C                   Select
270907151201  "  C*
270908151201  "  C* Comunicazione Spesometro
270909151201  "  C                   When      TpComG74   = 'E'
270910151201  "  C                   if        SOTTITT1   <> *BLANK
270911151201  "  C                   MOVEL     SOTTITT1      IDMSG
270912151201  "  C                   endif
270913151201  "  C*
270914151201  "  C* Comunicazione Acquisti da San Marino
270915151201  "  C                   When      TpComG74   = 'S'
270916151201  "  C                   if        SOTTITT2   <> *BLANK
270917151201  "  C                   MOVEL     SOTTITT2      IDMSG
270918151201  "  C                   endif
270919151201  "  C*
270920151201  "  C* Comunicazione Black list
270921151201  "  C                   When      TpComG74   = 'B'
270922151201  "  C                   if        SOTTITT3   <> *BLANK
270923151201  "  C                   MOVEL     SOTTITT3      IDMSG
270924151201  "  C                   endif
270925151201  "  C*
270926151201  "  C                   endsl
270927151201  "  C*
270928151201  "  C                   CALLB     'XRTVM'
270929151201  "  C                   PARM                    IDMSG            27
270930151201  "  C                   PARM                    TXTMSG          100
270931151201  "  C                   PARM                    ERRMSG            1
270932151201  "  C                   PARM      *ON           CTRMSG            1            centratura
270933151201  "  C                   PARM      30            LENMSG            3 0          lun output.
270934151201  "  C     ERRMSG        IFNE      *ON
270935151201  "  C                   MOVEL     TXTMSG        T1TIT
270936151201  "  C                   ELSE
270937151201  "  C                   MOVEL     *ALL'?'       T1TIT
270938151201R195 C                   ENDIF
271000000000     C* recupero il tipo operazione
271100000000     C                   Z-ADD     1             X
271200130906    >C     OPZG74        LOOKUP    $OZ(X)                                 21
271300000000     C     *IN21         IFEQ      *ON
271400000000     C                   MOVEL     $ID(X)        IDMSG
271500000000     C                   CALLB     'XRTVM'
271600000000     C                   PARM                    IDMSG            27
271700000000     C                   PARM                    TXTMSG          100
271800000000     C                   PARM                    ERRMSG            1
271900000000     C                   PARM      *Off          CTRMSG            1            centratura
272000000000     C                   PARM      15            LENMSG            3 0          lun output.
272100000000     C     ERRMSG        IFNE      *ON
272200000000     C                   MOVEL     TXTMSG        T1OPE
272300000000     C                   ELSE
272400000000     C                   MOVEL     *ALL'?'       T1OPE
272500000000     C                   ENDIF
272600000000     C                   ENDIF
272700150331R178 C*
272800150318 "   C* Reperisco dati della società da tab. G12
272900150331R178 C                   EXSR      RepG12Soc
273000000000     C*
273100000000     C* Inizializzazione/Decodifica prima videata
273200000000     C                   EXSR      REPDEF
273300000000     C                   EXSR      INZD1
273400000000     C                   MOVE      *OFF          $INZD1
273500130918     C*
273600150331R178 X**** Reperisco dati della società da tab. G12
273700150331R178 X****               EXSR      RepG12Soc
273800000000     C*
273801151203R195 C*
273802160104 "   C                   Select
273803151203 "   C* se Spesometro o Acquisti da San marino
273804160104 "   C                   When      TpComG74    = 'E'     or
273805151203R195 C                             TpComG74    = 'S'
273900130918     C                   EXSR      RepSDGSPECF
273902160104R195 C* se Black List
273903160104 "   C                   When      TpComG74    = 'B'
273904160104 "   C                   EXSR      RepSDBLACK2
273905160104 "   C*
273906160104R195 C                   endSl
274000130918     C*
274100000000     C                   ENDSR
274200000000     C/EJECT
274300000000     C************************************************************
274400000000     C* REPERIMENTO DATI SOCIETA'
274500000000     C************************************************************
274600000000     C     REPSOC        BEGSR
274700000000     C*
274800000000     C                   CALL      'SDGSOCDR'
274900000000     C                   PARM                    TIPXSC            6
275000000000     C                   PARM                    SOCXSC            3
275100000000     C                   PARM                    CDSXSC            9 0
275200000000     C                   PARM                    MODXSC            3
275300000000     C                   PARM      *BLANKS       RTNXSC            1
275400000000     C                   PARM                    XSOCDS
275500000000     C                   PARM                    KPJBA
275600000000     C*
275700000000     C                   ENDSR
275800000000     C/EJECT
275900000000     C************************************************************
276000000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
276100000000     C************************************************************
276200000000     C     REPAUT        BEGSR
276300000000     C*
276400000000     C                   CLEAR                   AUTDS
276500000000     C                   Clear                   ConCivDs
276600000000     C*
276700000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
276800000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
276900000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
277000000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
277100000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
277200000000    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
277300000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
277400000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
277500000000     C*
277600000000     C                   CALLB     'XCHKAUT'
277700000000     C                   PARM                    AUTDS
277800000000     C*
277900000000     C* errori/non abilitazione
278000000000     C     XCARTN        IFNE      0
278100130906    >C                   MOVE      '2'           ErrG74
278200000000     C                   MOVE      *ON           $FINE
278300000000     C                   EXSR      ENDPGM
278400000000     C                   ELSE
278500000000    >C                   MOVEL     XCADDS        CONCIVDS
278600000000     C                   ENDIF
278700000000     C*
278800000000     C                   ENDSR
278900000000     C/EJECT
279000000000     C************************************************************
279100000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
279200000000     C************************************************************
279300000000     C     TSTAUT        BEGSR
279400000000     C*
279500000000     C                   SELECT
279600000000     C*
279700000000     C* se richiamato in immissione
279800130906    >C     OPZG74        WHENEQ    '01'
279900000000     C* ma l'utente non è abilitato
280000000000    >C     IMMALL        IFEQ      *off
280100000000     C* esco da pgm avvertendolo
280200130906    >C                   MOVE      '2'           ERRG74
280300000000     C                   MOVE      *ON           $FINE
280400000000     C                   EXSR      ENDPGM
280500000000     C                   ENDIF
280600000000     C*
280700000000     C* se richiamato in modifica
280800130906    >C     OPZG74        WHENEQ    '02'
280900000000     C* ma l'utente non è abilitato
281000000000    >C     MANALL        IFEQ      *OFF
281100000000     C* testo se l'utente è abilitato almeno alla visualizzazione
281200000000    >C     VISALL        IFEQ      *ON
281300130906    >C                   MOVE      '05'          OPZG74
281400000000     C                   ELSE
281500000000     C* altrimenti esco da pgm avvertendolo
281600130906    >C                   MOVE      '2'           ERRG74
281700000000     C                   MOVE      *ON           $FINE
281800000000     C                   EXSR      ENDPGM
281900000000     C                   ENDIF
282000000000     C                   ENDIF
282100000000     C*
282200000000     C* se richiamato in copia
282300130906    >C     OPZG74        WHENEQ    '03'
282400000000     C* ma l'utente non è abilitato
282500000000    >C     IMMALL        IFEQ      *OFF
282600000000     C* esco da pgm avvertendolo
282700130906    >C                   MOVE      '2'           ERRG74
282800000000     C                   MOVE      *ON           $FINE
282900000000     C                   EXSR      ENDPGM
283000000000     C                   ENDIF
283100000000     C*
283200000000     C* se richiamato in annullamento
283300130906    >C     OPZG74        WHENEQ    '04'
283400000000     C* ma l'utente non è abilitato
283500000000    >C     ANNALL        IFEQ      *OFF
283600000000     C* esco da pgm avvertendolo
283700130906    >C                   MOVE      '2'           ERRG74
283800000000     C                   MOVE      *ON           $FINE
283900000000     C                   EXSR      ENDPGM
284000000000     C                   ENDIF
284100000000     C*
284200000000     C* se richiamato in visualizzazione
284300130906    >C     OPZG74        WHENEQ    '05'
284400000000     C* ma l'utente non è abilitato
284500000000    >C     VISALL        IFEQ      *OFF
284600000000     C* esco da pgm avvertendolo
284700130906    >C                   MOVE      '2'           ERRG74
284800000000     C                   MOVE      *ON           $FINE
284900000000     C                   EXSR      ENDPGM
285000000000     C                   ENDIF
285100000000     C*
285200000000     C                   ENDSL
285300000000     C*
285400000000     C                   ENDSR
285500000000     C/EJECT
285600000000     C************************************************************
285700000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
285800000000     C************************************************************
285900000000     C     DEFOPZ        BEGSR
286000000000     C*
286100000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
286200000000     C* con S o N a seconda della utilizzabilità o meno
286300000000     C* del tasto funzione correlato
286400000000     C*
286500000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
286600000000     C* di compilazione
286700000000     C                   RESET                   $FC1
286800000000     C                   RESET                   $FR1
286900000000     C*
287000000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
287100000000     C* invalido alcune opzioni
287200000000     C                   SELECT
287300000000     C* Se pgm richiamato in interrogazione disattivo :
287400130906    >C     OPZG74        WHENEQ    '05'
287500000000     C* F2=Decodifica
287600000000     C                   Z-ADD     2             X
287700000000     C                   MOVE      '0'           $FC1(X)
287800000000     C* F4=Ricerca
287900000000     C                   Z-ADD     4             X
288000000000     C                   MOVE      '0'           $FC1(X)
288100000000     C* F5=Ripristino
288200000000     C                   Z-ADD     5             X
288300000000     C                   MOVE      '0'           $FC1(X)
288400000000     C* F6=Conferma
288500000000     C                   Z-ADD     06            X
288600000000     C                   MOVE      '0'           $FC1(X)
288700000000     C* Se pgm richiamato in annullamento disattivo :
288800130906    >C     OPZG74        WHENEQ    '04'
288900000000     C* F2=Decodifica
289000000000     C                   Z-ADD     2             X
289100000000     C                   MOVE      '0'           $FC1(X)
289200000000     C* F4=Ricerca
289300000000     C                   Z-ADD     4             X
289400000000     C                   MOVE      '0'           $FC1(X)
289500000000     C* F5=Ripristino
289600000000     C                   Z-ADD     5             X
289700000000     C                   MOVE      '0'           $FC1(X)
289800000000     C*
289900000000     C                   OTHER
290000000000     C*
290100000000     C                   ENDSL
290200000000     C*
290300000000     C                   ENDSR
290400000000     C/EJECT
290500000000     C************************************************************
290600000000     C* REPERIMENTO CAMPI DI DEFAULT
290700000000     C************************************************************
290800000000     C     REPDEF        BEGSR
290900000000     C*
291000000000     C                   Eval      $FlgDef     = *On
291100000000     C*
291200000000     C                   ENDSR
291300130918     C************************************************************
291400130918     C* Reperisco dati da G12 della società
291500130918     C************************************************************
291600130918     C     RepG12Soc     BEGSR
291700130918     C*
291800130918     C                   Eval      $UICSoc     = 0
291900130918     C                   Eval      $NazSoc     = *Blank
292000130918     C*
292100130918     C                   Eval      WStato      = XScSta
292200130918     C                   EXSR      RepTabG12
292300130918     C*
292400130918     C                   if        XTBErr     <> '0'
292500130918     C                   Eval      ERRG74      = '1'
292600130918     C                   EXSR      ENDPGM
292700130918     C                   endif
292800130918     C*
292900130918     C                   if        $CodUIC     = 0
293000130918     C                   Eval      ERRG74      = '1'
293100130918     C                   EXSR      ENDPGM
293200130918     C                   endif
293300130918     C*
293400130918     C                   Eval      $UICSoc     = $CodUIC
293500130918     C*
293600130918     C                   Eval      $NazSoc     = $NazISO
293700130918     C                   if        $NazSoc     = *Blank
293800130918     C                   Eval      $NazSoc     = XScSta
293900130918     C                   endif
294000130918     C*
294100130918     C                   ENDSR
294200130918      /EJECT
294300130918     C************************************************************
294400130918     C* Reperisco dati tabella G12
294500130918     C************************************************************
294600130918     C     RepTabG12     BEGSR
294700130918     C*
294800130918     C                   Eval      $CodUIC     = 0
294900130918     C                   Eval      $NazISO     = *Blank
295000130918     C*
295100130918     C                   Clear                   ANGG12DS
295200130918     C                   Clear                   SDGTABDS
295300130918     C*
295400130918     C                   Eval      XTbRic      = '1'
295500130918     C                   Eval      XTbAzi      = XScSoc
295600130918     C                   Eval      XTbKey      = *Zero
295700130918     C                   Eval      XTbCod      = 'G12'
295800130918     C                   Move      WStato        XTbKey
295900130918     C*
296000130918     C                   CALL      'SDGTABDR'
296100130918     C                   Parm                    SDGTABDS
296200130918     C*
296300130918     C                   if        XTBErr      = '0'
296400130918     C                   Eval      ANGG12DS    = XTbUni
296500130918     C*
296600130918     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
296700130918     C                   Z-Add     0             $CodUIC
296800130918     C                   else
296900130918     C                   Z-Add     CodUICG12     $CodUIC
297000130918     C                   endif
297100130918     C*
297200130918     C                   Eval      $NazISO     = NazG12
297300130918     C*
297400130918     C                   endif
297500130918     C*
297600130918     C                   ENDSR
297700130918      /EJECT
297800130918     C************************************************************
297900130906     C* Rerimento parametri di setup per spesometro
298000130918     C************************************************************
298100130906     C     RepSDGSPECF   BEGSR
298200120215     C*
298300130906     C                   Clear                   SDGPA08DS
298400120215     C*
298500120215     C                   CALLB     'XPAR'
298600120215     C                   Parm                    XScSoc
298700130906     C                   Parm      'SDGSPECF'    XPaPar            8
298800120215     C                   Parm                    XPaOut         2000
298900120215     C                   Parm      *OFF          XPaErr            1
299000120215     C                   Parm      ' '           XPaRic            1
299100120215     C*
299200120215     C                   if        XPaErr      = *On
299300120215     C                   EXSR      ENDPGM
299400120215     C                   else
299500130906     C                   MoveL     XPaOut        SDGPA08DS
299600120215     C                   endif
299700120215     C*
299800120215     C                   EndSr
299900130918     C/EJECT
299901160104R195 C************************************************************
299902160104 "   C* Reperimento parametri di setup per Black List
299903160104 "   C************************************************************
299904160104R195 C     RepSDBLACK2   BEGSR
299905160104     C*
299906160104     C                   Clear                   SDGPA11DS
299907160104     C*
299908160104     C                   CALLB     'XPAR'
299909160104     C                   Parm                    XScSoc
299910160104     C                   Parm      'SDBLACK2'    XPaPar            8
299911160104     C                   Parm                    XPaOut         2000
299912160104     C                   Parm      *OFF          XPaErr            1
299913160104     C                   Parm      ' '           XPaRic            1
299914160104     C*
299915160104     C                   if        XPaErr      = *On
299916160104     C                   EXSR      ENDPGM
299917160104     C                   else
299918160104     C                   MoveL     XPaOut        SDGPA11DS
299919160104     C                   endif
299939160104     C*
299940160104R195 C                   ENDSR
299941160104R195 C/EJECT
300000000000     C************************************************************
300100000000     C* DEFINIZIONE KLIST
300200000000     C************************************************************
300300000000     C     DefKlist      BEGSR
300400000000     C*
300500000000     C* klist
300600000000     C*
300700000000     C                   ENDSR
300800000000     C/EJECT
300900000000     C************************************************************
301000000000     C* RIEMPIMENTO TASTI FUNZIONE
301100000000     C************************************************************
301200000000     C     RIEKEY        BEGSR
301300000000     C*
301400000000     C* Input :
301500000000     C* - $FX = schiera con tutti i testi disponibili
301600000000     C* - $FC = schiere con i flag di validità associati ai testi
301700000000     C* - $FR = schiere con i flag di REVERSE IMAGE
301800000000     C* - $ULK = ultimo testo caricato
301900000000     C* - $L01 = Lunghezza prima riga
302000000000     C* - $L02 = Lunghezza seconda riga
302100000000     C* Output :
302200000000     C* - $ULK
302300000000     C* - $KEY1= 1° riga di tasti funzione
302400000000     C* - $KEY2= 2° riga di tasti funzione
302500000000     C*
302600000000     C                   CALLB     'XKEYMSG'
302700000000     C* input
302800000000     C                   PARM                    $FX
302900000000     C                   PARM                    $FC
303000000000     C                   PARM                    $FR
303100000000     C                   PARM                    $ULK              3 0
303200000000     C* output
303300000000     C                   PARM                    $KEY1            79
303400000000     C                   PARM                    $KEY2            79
303500000000     C* input
303600000000     C                   PARM                    $L01              3 0
303700000000     C                   PARM                    $L02              3 0
303800000000     C*
303900000000     C                   PARM                    $ALLFUNCT       480
304000000000     C*
304100000000     C                   ENDSR
304200000000** $OZ / $ID
30430000000001SDGMSG    *LIBL     SDC0002
30440000000002PROMSG    *LIBL     COS0002
30450000000003PROMSG    *LIBL     COS0003
30460000000004PROMSG    *LIBL     COS0004
30470000000005PROMSG    *LIBL     COS0005
304800000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
304900000000PROMSG    *LIBL     KEY0001  01
305000000000PROMSG    *LIBL     KEY0002  02
305100000000PROMSG    *LIBL     KEY0003  03
305200000000PROMSG    *LIBL     KEY0004  04
305300000000PROMSG    *LIBL     KEY0005  05
305400000000PROMSG    *LIBL     KEY0006  06
305500000000PROMSG    *LIBL     KEY0007  07
305600000000PROMSG    *LIBL     KEY0008  08
305700000000PROMSG    *LIBL     KEY0009  09
305800000000PROMSG    *LIBL     KEY0010  10
305900000000PROMSG    *LIBL     KEY0011  11
306000000000PROMSG    *LIBL     KEY0012  12
306100000000PROMSG    *LIBL     KEY0013  13
306200000000PROMSG    *LIBL     KEY0014  14
306300000000PROMSG    *LIBL     KEY0015  15
306400000000PROMSG    *LIBL     KEY0016  16
306500000000PROMSG    *LIBL     KEY0017  17
306600000000PROMSG    *LIBL     KEY0018  18
306700000000PROMSG    *LIBL     KEY0019  19
306800000000PROMSG    *LIBL     KEY0020  20
306900000000PROMSG    *LIBL     KEY0021  21
307000000000PROMSG    *LIBL     KEY0022  22
307100000000PROMSG    *LIBL     KEY0023  23
307200000000PROMSG    *LIBL     KEY0024  24
307300000000**  ABILITAZIONE D1 : $FC1 / $FR1
3074000000000N  01
3075000000000N  02
3076000000001N  03
3077000000001N  04
3078000000001N  05
3079000000001N  06
3080000000000N  07
3081000000001N  08
3082000000000N  09
3083000000000N  10
3084000000000N  11
3085000000001N  12
3086000000000N  13
3087000000000N  14
3088000000000N  15
3089000000000N  16
3090000000000N  17
3091000000000N  18
3092000000000N  19
3093000000000N  20
3094000000000N  21
3095000000000N  22
3096000000000N  23   Impostare a '1' se si utilizzano opzioni
3097000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
