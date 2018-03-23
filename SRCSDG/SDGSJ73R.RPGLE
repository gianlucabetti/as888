000100141218     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND)
000200141218     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500141218      *           -------------------------------------------
000600141218      *                Dichiarazioni di intento fornitori
000800141218      *           -------------------------------------------
000900000000      *
001200000000     F*----------------------------------------------------------------
001201141218     F* key: Soc/DtInter/NrInter   UNIQUE
001202141218     FSDGDIE01L UF   E           K DISK
001204141218     F*------------
001205141218     F* key: Soc/Anno/CogRag/Nome/PartIVA/CdFisc
001206141218     FSDGDIE02L IF   E           K DISK
001207141218     F                                     RENAME(SDGDIE000:SDGDIE02)
001209141218     F*------------
001300141218     FSDGSJ73D  CF   E             WORKSTN
001400141218     F                                     RENAME(SJ73D1:FMTD1)
001500141218     F                                     RENAME(SJ73D2:FMTD2)
001600141218     F                                     RENAME(SJ73D3:FMTD3)
001700141218     F                                     RENAME(SJ73S1:FMTS1)
001800141218     F                                     RENAME(SJ73C1:FMTC1)
001900141218     F                                     RENAME(SJ73T1:FMTT1)
002000141218     F                                     RENAME(SJ73Z1:FMTZ1)
002100141218     F                                     RENAME(SJ73Z2:FMTZ2)
002200141218     F                                     RENAME(SJ73Z3:FMTZ3)
002300000000     F                                     SFILE(FMTS1:S1NRR)
002400000000     F                                     INFDS(DSFMT)
002800000000     D*----------------------------------------------------*
002900000000     D* Schiere per la gestione delle opzioni
003000000000     D*
003100000000     D* dimensione della schiera $MS1, da non cambiare
003200000000     D $DIMOP          C                   CONST(30)
003300000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003400000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
003500000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
003600000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
003700000000     D* schiera contenente i nomi dei programma richiamati attraverso
003800000000     D* le opzioni personalizzate
003900000000     D $PGO1           S             10    DIM($dimop)
004000000000     D $TX             S             50    DIM(50)
004100000000     D $FL             S              1    DIM(50)
004200000000     D* Schiere generali per la gestione dei tasti di funzione
004300000000     D $FM             S             27    DIM(24)
004400000000     D $FX             S             50    DIM(24)
004500000000     D $FC             S              1    DIM(24)
004600000000     D $FR             S              1    DIM(24)
004700000000     D* Schiere per la gestione dei tasti di funzione
004800000000     D* Video D1
004900000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000000000     D $FX1            S             50    DIM(24) ALT($FM1)
005100000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200000000     D $FR1            S              1    DIM(24) ALT($FC1)
005300000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005400000000     D* funzione personalizzata S1
005500000000     D $PGF2           S             10    DIM(25)
005600000000     D* Schiere per la gestione dei tasti di funzione
005700000000     D* Video S1
005800000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
005900000000     D $FX2            S             50    DIM(24) ALT($FM2)
006000000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006100000000     D $FR2            S              1    DIM(24) ALT($FC2)
006200000000     D* Schiere per la gestione dei tasti di funzione
006300000000     D* Video D2
006400000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
006500000000     D $FX3            S             50    DIM(24) ALT($FM3)
006600000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
006700000000     D $FR3            S              1    DIM(24) ALT($FC3)
006800000000     D*
006900000000     D $D1L01          S              3  0
007000000000     D $D1L02          S              3  0
007100000000     D $S1L01          S              3  0
007200000000     D $S1L02          S              3  0
007300000000     D $C1Op1          S              3  0
007400000000     D $C1Op2          S              3  0
007500000000     D $D2L01          S              3  0
007600000000     D $D2L02          S              3  0
007700000000     D*
007800000000     D*----------------------------------------------------*
007900000000     D* Valori assunti
008000000000     D XMPEDS        E DS                  INZ
008100000000     D XMPEDS1       E DS                  INZ
008200000000     D DSMPE           DS                  INZ
008300000000     D  $LOP                   1      1  0
008400000000     D  $ORD                   2      3  0
008500000000     D*-------------
008600000000     D* Passaggio Parametri
008700000000     D KPJBA         E DS
008800110823     D*-------------
008900110823     D* Salvataggio Kpjbu per richiamo altri pgm
009000110823     D SavKPJBU        S            256
009400000000     D*-------------
009500000000     D* Reperimento Autorizzazioni
009600000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009700000000     D*-------------
009800000000     D* Dati per pgm XNOAUT
009900000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010000000000     D*-------------
010100000000     D* Passaggio Autorizzazioni
010200000000     D DSAUT           DS           512
010600000000     D*-------------
010700000000     D* Dati di ambiente ottenuti da SDGSOCDR
010800000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
010900000000     D    SIFSogg                     8    overlay(XScFGO:31)
011000000000     D*-------------
011100000000     D* DS Interna per dati di output di SDGSOCDR
011200000000     D XSOCDS          DS          1000
011300000000     D*-------------
011400000000     D* DS Esterna gestione messaggi
011500000000     D SDGMSGDS      E DS                  INZ
011600000000     D  STS          E                     EXTFLD(MSGSTS)
011700000000     D                                     DIM(333)
011800000000     D  JBA          E                     EXTFLD(MSGJBA)
011900000000     D                                     DIM(502)
012000141218     D*-------------
012100000000     D* campi di lavoro per emissione window di avvertimento
012200000000     D WXMSGInz        S              1A   INZ('1')
012300000000     D*-------------
012400000000     D* Richiamo a SDGTABDR
012500000000     D SDGTABDS      E DS                  inz
012600000000     D*-------------
012700000000     D* Parametri per ricerca codice ANATB00F
012800000000     D XTABDS        E DS                  INZ
015600000000     D*-------------
015700000000     D* Ricerca legende
015800000000     D ANA964DS      E DS                  INZ
015900000000     D*-------------
016000000000     D*Controlli validità per valore di una legenda
016100000000     D X07VALDS      E DS                  Inz
016800000000     D*-------------
016900000000     D* Parametri in input
017000141218    >D SDGSJ73DS     E DS                  inz
023200000000     D*-------------
023300130912     D* manutenzione  dati di testata
023400141218     D SDGSJ74DS     E DS                  inz
023500141218     D  RETJ74       E                     INZ('0')
023600141218     D  OPRJ74       E                     INZ('0')
023700141218     D  ERRJ74       E                     INZ('0')
023800141218     D  CMTJ74       E                     INZ('0')
023801150112     D*-------------
023802150112     D* Richiamo pgm Dichiarazione d'intenti
023803150112     D ANA311DS      E DS                  INZ
023804150112     D  CMT311       E                     INZ('0')
023805150112     D  RET311       E                     INZ('0')
023806150112     D  OPR311       E                     INZ('0')
023807150112     D  ERR311       E                     INZ('0')
026000000000     D*-------------
026100000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
026200000000     D* alla funzione personalizzata
026300141218    >D $SaveDS         S                   like(SDGSJ73DS)
026400000000     D*-------------
026500000000     D DSFMT           DS           512
026600000000     D  $TASTO               369    369
026700000000     D  NRG                  370    370
026800000000     D  NCL                  371    371
026900000000      * riga cursore window
027000000000     D  NRiWin               382    382
027100000000      * colonna cursore window
027200000000     D  NCoWin               383    383
027300000000     D  SFLNRR               378    379B 0
027400000000     D*-------------
027500000000     D* posizione cursore
027600000000     D CURSOR          DS
027700000000     D  RIRI                   1      2B 0 INZ
027800000000     D  R$$                    2      2
027900000000     D  COCO                   3      4B 0 INZ
028000000000     D  C$$                    4      4
028100000000     D*-------------
028200000000     D* Costanti
028300000000     D* nr.di righe del sfl (SFLPAG)
028400130927     D SFLPAG          C                   CONST(5)
028500000000     D*-------------
028600000000     D* Tasti di funzione
028700000000     D F01             C                   CONST(X'31')
028800000000     D F02             C                   CONST(X'32')
028900000000     D F03             C                   CONST(X'33')
029000000000     D F04             C                   CONST(X'34')
029100000000     D F05             C                   CONST(X'35')
029200000000     D F06             C                   CONST(X'36')
029300000000     D F07             C                   CONST(X'37')
029400000000     D F08             C                   CONST(X'38')
029500000000     D F09             C                   CONST(X'39')
029600000000     D F10             C                   CONST(X'3A')
029700000000     D F11             C                   CONST(X'3B')
029800000000     D F12             C                   CONST(X'3C')
029900000000     D F13             C                   CONST(X'B1')
030000000000     D F14             C                   CONST(X'B2')
030100000000     D F15             C                   CONST(X'B3')
030200000000     D F16             C                   CONST(X'B4')
030300000000     D F17             C                   CONST(X'B5')
030400000000     D F18             C                   CONST(X'B6')
030500000000     D F19             C                   CONST(X'B7')
030600000000     D F20             C                   CONST(X'B8')
030700000000     D F21             C                   CONST(X'B9')
030800000000     D F22             C                   CONST(X'BA')
030900000000     D F23             C                   CONST(X'BB')
031000000000     D F24             C                   CONST(X'BC')
031100000000     D ENTER           C                   CONST(X'F1')
031200000000     D ROLDWN          C                   CONST(X'F4')
031300000000     D ROLLUP          C                   CONST(X'F5')
031400000000     D*-------------
031500000000     D* Attributi video
031600000000     D Normale         C                   Const(X'32')
031700000000     D Protetto        C                   Const(X'A0')
031800000000     D Annul           C                   CONST(X'A1')
031900000000     D*-------------
032000000000     D* Sottotitolo di testata
032100000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
032200141218     D                                          COS0127')
032300000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
032400000000     D                                          SDC0001')
032500000000     D*-------------
032600000000     D* Variabili per gestione videate
032700000000     D $Gest           S              2A
032800000000     D $Fine           S              1A
032900000000     D $InzD1          S              1A
033000000000     D $InzS1          S              1A
033100000000     D $InzD2          S              1A
033200000000     D $LastG          S              2A
033300000000     D $LastV          S              2A
033400000000     D $EndFile        S              1A
033500000000     D $Esci           S              1A
033600000000     D $RcdOK          S              1A
033700000000     D $Unique         S              1A
033800000000     D*-------------
033900000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
034000000000     D S1Nrr           S                   like(C1Rcd)
034100000000     D WSfl            S                   like(C1Nrr)
034200000000     D WMax            S                   like(C1Rcd)
034300000000     D WPag            S                   like(C1Rcd)
034400000000     D WInzS1          S                   like($InzS1)
034500141218     D WOpr            S                   like(OprJ73)
034600000000     D CurR            S              3S 0
034700000000     D CurC            S              3S 0
034800000000     D Win             S             99A
034900000000     D W1Pos           S                   like(C1Pos)
035000000000     D WINKR           S              1A
035100000000     D WExf            S              1A
035200000000     D WSogg           S                   Like(DVSSogg)
035400000000     D*-------------
035500000000     D* Indice della schiera tasti funzione
035600000000     D PosFun          S              2  0
035700000000     D*-------------
035800000000     D*  Indici
035900000000     D X               S              5S 0
036000000000     D Y               S              5S 0
036100000000     D* Indice della schiera per le opzioni personalizzate
036200000000     D PosOpz          S              2  0
036300000000     D* numero max opzioni
036400000000     D NumOpz          S              2  0 Inz($DIMOP)
036500000000     D* numero effettive opzioni
036600000000     D OpzLun          S              2  0
036700000000     D*-------------
036800000000     D* ricerca soggetti
036900130829     D ANA040DS      E DS                  INZ
037300000000     D*-------------
037400000000     D* Reperimento nome PGM
037500000000     D STATUS         SDS           333
037600000000     D  DSPGM            *PROC
037700000000     D  PARMS            *PARMS
037800000000     D*-------------
037900000000     D* DS esterna per dati soggetti
038000000000     D DVASOG        E DS                  INZ
038100000000     D*-------------
038200000000     D* Definizioni parametri driver anagrafici
038300000000     D Dvaric          S              1
038400000000     D DvaSocieta      S              3
038500000000     D DvaUnita        S              8
038600000000     D DvaStrutt       S             10
038700000000     D DvaDtRif        S               D
038800000000     D DvaLenOut       S              9B 0
038900000000     D DvaSnatura      S              1
039000000000     D DvaCodice       S              8
039100000000     D DvaLineav       S              3
039200000000     D DvaFiliale      S              3  0
039300000000     D DvaSogg         S              8
039400000000     D DvaTpInd        S              2
039500000000     D DvaCdInd        S              3
039600000000     D DvaErrore       S              1
039700000000     D DvaOutput       S           4000
039800000000     D*-------------
039900000000     D* parametri per chiamata al driver dei contatti
040000000000     D $Errore         S              1
040100000000     D $Opz            S              1
040200000000     D $Len            S              9B 0
040300000000     D $Refresh        S              1
040400000000     D Esito           S              1
040500000000     D GesMsg          S              1
040600000000     D Output          S           4000
040700000000     D LenOut          S              9B 0
040800000000     D Struttura       S             10
040801141218     D*-------------
040802141218     D* Ricerca fornitori
040803141218     D ANA400DS      E DS                  INZ
040804141218     D*-------------
040805141218     D* DS esterna per dati fornitori
040806141218     D DVAFOR        E DS
040824141218     D*-------------
040825141218     D* Campi di lavoro
040826141218     D WIn17           S              1
040827141218     D UDateISO        S               D
040828141223     D*-------------
040829141223     D*  campi per parzializzazioni su rcd
040830141223     D Ann111          S                   like(DIEAnn  )
040831141223     D TpAnn111        S                   like(DIETpAnn  )
040836141223     D PIVA111         S                   like(DIEPartIVA)
040837141223     D CdFisc111       S                   like(DIECdFisc)
040867141223     D*-----
040868141223     D Ann999          S                   like(DIEAnn  )
040869141223     D TpAnn999        S                   like(DIETpAnn  )
040874141223     D PIVA999         S                   like(DIEPartIVA)
040875141223     D CdFisc999       S                   like(DIECdFisc)
041500000000     C*----------------------------------------------------*
041600000000     C*                MAIN LINE PROGRAM
041700000000     C*----------------------------------------------------*
041800000000     C*
041900000000     C* inizializzazione variabili
042000000000     C                   EXSR      INZVAR
042100000000     C*
042200000000     C     $FINE         DOWEQ     *OFF
042300000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
042400000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
042500000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
042600000000     C                   ENDCS
042700000000     C                   ENDDO
042800000000     C* fine programma
042900000000     C                   EXSR      ENDPGM
043000000000     C*
043100000000     C/EJECT
043200000000     C************************************************************
043300000000     C* FINE PROGRAMMA
043400000000     C************************************************************
043500000000     C     ENDPGM        BEGSR
043600000000     C*
043700000000     C* passaggio dati al pgm chiamante
043800141218    >C                   MOVEL     WOPR          OPRJ73                                     ne S1
043900141218    >C                   MOVEL     SDGSJ73DS     KPJBU
044000000000     C*
044100000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
044200141218    >C     OPZJ73        IFEQ      *BLANK
044300000000     C                   SETON                                            LR
044400000000     C* altrimenti chiudo in RT
044500000000     C                   ELSE
044600000000     C                   SETON                                            RT
044700000000     C                   ENDIF
044800000000     C                   RETURN
044900000000     C*
045000000000     C                   ENDSR
045100000000     C/EJECT
045200000000     C************************************************************
045300000000     C* RICERCHE
045400000000     C************************************************************
045500000000     C     SEARCH        BEGSR
045600000000     C*
045700000000     C* determino Riga/Colonna del cursore
045800000000     C                   MOVE      NRG           R$$
045900000000     C                   MOVE      NCL           C$$
046000000000     C                   Z-ADD     RIRI          CURR
046100000000     C                   Z-ADD     COCO          CURC
046200000000     C*
046300000000     C                   EXSR      CallAllinea
046400000000     C*
046500000000   B1C                   SELECT
047800000000     C*
047900000000     C*  Codice IVA
048000000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
048100000000     C                   EXSR      F4_PartIva
048200000000     C*
048300000000     C*  Codice fiscale
048400000000     C     H1NMFL        WHENEQ    'D1CDFISC'
048500000000     C                   EXSR      F4_CdFisc
048600000000     C*
048700000000     C*  Soggetto
048800000000     C     H1NMFL        WHENEQ    'D1SOGG'
048900000000     C                   EXSR      F4_Sogg
049400000000     C*
049500000000     C* FMTD1 - Dati attivi/non attivi
049600000000     C     H1NMFL        WHENEQ    'D1ANN  '
049700000000     C                   EXSR      F4_Ann
049800000000     C*
049900000000     C* FMTD1 - Tipo annullamento
050000000000     C     H1NMFL        WHENEQ    'D1TPANN'
050100000000     C                   EXSR      F4_TpAnn
050101141218     C*
050102141218     C* FMTD1 - fornitore
050103141218     C     H1NMFL        WHENEQ    'D1KSC   '
050104141218     C                   EXSR      F4_Ksc
050600130919     C*
050700130919     C* FMTD1 - tipo operazione
050800130919     C     H1NMFL        WHENEQ    'D1TPOPE '
050900130919     C                   EXSR      F4_TpOpe
051000000000     C*
051100000000     C* ricerca non abilitata in questa posizione
051200000000   X1C                   OTHER
051300000000     C                   SETON                                        9899
051400000000     C*
051500000000   E1C                   ENDSL
051600000000     C*
051700000000     C* imposto pos. del cursore
051800000000     C                   Z-ADD     CURR          H1RIGA
051900000000     C                   Z-ADD     CURC          H1COLO
052000000000     C*
052100000000     C                   MOVE      *HIVAL        $LASTV
052200000000     C*
052300000000     C                   ENDSR
052400000000     C/EJECT
052500000000     C************************************************************
052600000000     C* Richiamo programma per allineare i codici
052700000000     C************************************************************
052800000000     C     CallAllinea   BEGSR
052900000000     C*
053000000000     C     D1SOGG        IFNE      *BLANK
053100000000     C                   CALLb     'XALLINEA'
053200000000     C                   PARM                    D1SOGG
053300000000     C                   PARM      8             LEN               5 0
053400000000     C                   PARM                    XSCINP
053500000000     C                   ENDIF
053501141218     C*
053502141218     C     D1ksc         IFNE      *BLANKS
053503141218     C                   CALLB     'XALLINEA'
053504141218     C                   PARM                    D1ksc
053505141218     C                   PARM      8             LEN               5 0
053506141218     C                   PARM                    XSCINP
053507141218     C                   ENDIF
053600000000     C*
053700000000     C                   ENDSR
053800000000      /EJECT
061300000000     C************************************************************
061400000000     C* Ricerca partita IVA
061500000000     C************************************************************
061600000000     C     F4_PartIva    BEGSR
061700000000     C*
061800130829     C                   RESET                   ANA040DS
061900000000     C*
062000130829     C                   MOVEL     '01'          OPZ040
062100130829     C                   MoveL     D1PartIva     IVA040
062200130829     C                   MoveL     D1Sogg        CoD040
062300130829     C                   MOVEL     ANA040DS      KPJBU
062400130829     C                   CALL      'ANA040R'
062500000000     C                   PARM                    KPJBA
062600000000     C*
062700130829     C                   MOVEL     KPJBU         ANA040DS
062800000000     C*
062900130829     C                   if        Ret040     <> '2'
063000000000     C*
063100130829     C                   MoveL     CoD040        WSogg
063200000000     C                   EXSR      CallDvaSog
063300000000     C*
063400000000     C                   if        DVAErrore  <> '1'
063500000000     C*
063600000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
063700000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
063800000000     C*
063900000000     C                   MoveL     DVSPartIVA    D1PartIva
064000000000     C*
064100000000     C                   endif
064200000000     C*
064300000000     C                   endif
064400000000     C*
064500000000     C                   ENDSR
064600000000      /EJECT
064700000000     C************************************************************
064800000000     C* Ricerca codice fiscale
064900000000     C************************************************************
065000000000     C     F4_CdFisc     BEGSR
065100000000     C*
065200130829     C                   RESET                   ANA040DS
065300000000     C*
065400130829     C                   MOVEL     '01'          OPZ040
065500130829     C                   MoveL     D1CdFisc      CdF040
065600130829     C                   MoveL     D1Sogg        CoD040
065700130829     C                   MOVEL     ANA040DS      KPJBU
065800130829     C                   CALL      'ANA040R'
065900000000     C                   PARM                    KPJBA
066000000000     C*
066100130829     C                   MOVEL     KPJBU         ANA040DS
066200000000     C*
066300130829     C                   if        Ret040     <> '2'
066400000000     C*
066500130829     C                   MoveL     CoD040        WSogg
066600000000     C                   EXSR      CallDvaSog
066700000000     C*
066800000000     C                   if        DVAErrore  <> '1'
066900000000     C*
067000000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
067100000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
067200000000     C*
067300000000     C                   MoveL     DVSCdFisc     D1CdFisc                       *
067400000000     C*
067500000000     C                   endif
067600000000     C*
067700000000     C                   endif
067800000000     C*
067900000000     C                   ENDSR
068000000000     C/EJECT
068100000000     C************************************************************
068200000000     C* Ricerca codice soggetto
068300000000     C************************************************************
068400000000     C     F4_Sogg       BEGSR
068500000000     C*
068600130829     C                   RESET                   ANA040DS
068700000000     C*
068800130829     C                   MOVEL     '01'          OPZ040
068900130829     C                   MoveL     D1Sogg        CoD040
069000130829     C                   MOVEL     ANA040DS      KPJBU
069100130829     C                   CALL      'ANA040R'
069200000000     C                   PARM                    KPJBA
069300000000     C*
069400130829     C                   MOVEL     KPJBU         ANA040DS
069500000000     C*
069600130829     C                   if        Ret040     <> '2'
069700130829     C                   MoveL     CoD040        D1Sogg
069800000000     C                   endif
069900000000     C*
070000000000     C                   ENDSR
070100000000     C/EJECT
072500000000     C************************************************************
072600000000     C* Ricerca dati attivi/non attivi
072700000000     C************************************************************
072800000000     C     F4_Ann        BEGSR
072900000000     C*
073000000000     C                   reset                   ana964ds
073100000000     C*
073200000000     C                   movel     '01'          Opz964
073300000000     C                   movel     'ANDIZ000 '   Trc964
073400000000     C                   movel     'DSPATT   '   Cam964
073500000000     C*
073600000000     C                   movel     ana964ds      Kpjbu
073700000000     C                   call      'ANA964R'
073800000000     C                   parm                    Kpjba
073900000000     C                   movel     Kpjbu         Ana964ds
074000000000     C*
074100000000     C                   if        Ret964     <> '2'
074200000000     C                   movel     Val964        D1Ann
074300000000     C                   endif
074400000000     C*
074500000000     C                   ENDSR
074600000000     C/EJECT
074700000000     C************************************************************
074800000000     C* Ricerca tipo annullamento
074900000000     C************************************************************
075000000000     C     F4_TpAnn      BEGSR
075100000000     C*
075200000000     C                   reset                   ana964ds
075300000000     C*
075400000000     C                   movel     '01'          Opz964
075500141218     C                   movel     'SDGDIE000'   Trc964
075600141218     C                   movel     'DIETPANN'    Cam964
075700000000     C*
075800000000     C                   movel     ana964ds      Kpjbu
075900000000     C                   call      'ANA964R'
076000000000     C                   parm                    Kpjba
076100000000     C                   movel     Kpjbu         Ana964ds
076200000000     C*
076300000000     C                   if        Ret964     <> '2'
076400000000     C                   movel     Val964        D1TpAnn
076500000000     C                   endif
076600000000     C*
076700000000     C                   ENDSR
076800000000     C/EJECT
076801141218     C************************************************************
076802141218     C* Ricerca fornitore
076803141218     C************************************************************
076804141218     C     F4_Ksc        BEGSR
076805141218     C*
076809141218     C                   RESET                   ANA400DS
076810141218     C                   Movel     '01'          Opz400
076811141218     C                   Move      XSCSOC        Societa400
076812141218     C                   Move      D1KSC         KSC400
076813141218     C                   MoveL     ANA400DS      KPJBU
076814141218     C                   CALL      'ANA400R'
076815141218     C                   PARM                    KPJBA
076816141218     C                   MoveL     KPJBU         ANA400DS
076817141218      *
076818141218     C                   if        KSC400     <> *BLANK
076819141218     C                   MoveL     KSC400        D1KSC
076820141218     C                   endif
076821141218     C*
076822141218     C                   ENDSR
076823141218     C/EJECT
079200130919     C************************************************************
079300130919     C* Ricerca tipo operazione
079400130919     C************************************************************
079500130919     C     F4_TpOpe      BEGSR
079600130919     C*
079700130919     C                   reset                   ana964ds
079800130919     C*
079900130919     C                   movel     '01'          Opz964
080000141218     C                   MoveL     'SDGSJ73S '   Trc964
080100141218     C                   MoveL     'TPOPEJ73 '   Cam964
080200130919     C                   movel     *On           All964
080300130919     C*
080400130919     C                   movel     ana964ds      Kpjbu
080500130919     C                   call      'ANA964R'
080600130919     C                   parm                    Kpjba
080700130919     C                   movel     Kpjbu         Ana964ds
080800130919     C*
080900130919     C                   if        Ret964     <> '2'
081000130919     C                   movel     Val964        D1TpOpe
081100130919     C                   endif
081200130919     C*
081300130919     C                   ENDSR
081400130919     C/EJECT
081500000000     C************************************************************
081600000000     C* RIEMPIMENTO OPZIONI
081700000000     C************************************************************
081800000000     C     RIEOPZ        BEGSR
081900000000     C*
082000000000     C                   CALLB     'X04OPMSG'
082100000000     C* input
082200000000     C                   PARM                    $TX
082300000000     C                   PARM                    $FL
082400000000     C                   PARM                    WCOZ              3 0
082500000000     C                   PARM                    WMXO              3 0
082600000000     C* output
082700000000     C                   PARM                    WOPZ1            76
082800000000     C                   PARM                    WOPZ2            76
082900000000     C                   PARM                    FF23              1
083000000000     C* input
083100000000     C                   PARM                    $L01              3 0
083200000000     C                   PARM                    $L02              3 0
083300000000     C*
083400000000     C                   PARM                    $ALLOPT         960
083500000000     C*
083600000000     C                   ENDSR
083700000000     C/EJECT
083800000000     C************************************************************
083900000000     C* RIEMPIMENTO TASTI FUNZIONE
084000000000     C************************************************************
084100000000     C     RIEKEY        BEGSR
084200000000     C*
084300000000     C                   CALLB     'XKEYMSG'
084400000000     C* input
084500000000     C                   PARM                    $FX
084600000000     C                   PARM                    $FC
084700000000     C                   PARM                    $FR
084800000000     C                   PARM                    $ULK              3 0
084900000000     C* output
085000000000     C                   PARM                    $KEY1            79
085100000000     C                   PARM                    $KEY2            79
085200000000     C* input
085300000000     C                   PARM                    $L01              3 0
085400000000     C                   PARM                    $L02              3 0
085500000000     C*
085600000000     C                   PARM                    $ALLFUNCT       480
085700000000     C*
085800000000     C                   ENDSR
085900000000     C/EJECT
086000000000     C************************************************************
086100000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
086200000000     C************************************************************
086300000000     C     GESPIE        BEGSR
086400000000     C*
086500000000     C                   SELECT
086600000000     C* Parzializzazioni
086700000000     C     $GEST         WHENEQ    'D1'
086800000000     C     $ULKD1        IFEQ      0
086900000000     C* riempimento opzioni
087000000000     C                   EXSR      F24D1
087100000000     C                   ELSE
087200000000     C                   WRITE     FMTZ1
087300000000     C                   ENDIF
087400000000     C* Lista
087500000000     C     $GEST         WHENEQ    'S1'
087600000000     C     $ULKS1        IFEQ      0
087700000000     C                   EXSR      F24S1
087800000000     C                   ELSE
087900000000     C                   WRITE     FMTZ2
088000000000     C                   ENDIF
088100000000     C* Modifica valori assunti
088200000000     C     $GEST         WHENEQ    'D2'
088300000000     C     $ULKD2        IFEQ      0
088400000000     C                   EXSR      F24D2
088500000000     C                   ELSE
088600000000     C                   WRITE     FMTZ3
088700000000     C                   ENDIF
088800000000     C                   ENDSL
088900000000     C*
089000000000     C                   ENDSR
089100000000     C/EJECT
089200000000     C************************************************************
089300000000     C* GESTIONE PARZIALIZZAZIONI
089400000000     C************************************************************
089500000000     C     GESD1         BEGSR
089600000000     C*
089700000000     C* inizializzazione videata
089800000000     C     $INZD1        IFEQ      *ON
089900000000     C                   EXSR      INZD1
090000000000     C                   MOVE      *OFF          $INZD1
090100000000     C                   ENDIF
090200000000     C* emissione piede videata se proveniente da altra
090300000000     C* salvataggio valore formato di provenienza
090400000000     C     $LASTV        IFNE      'D1'
090500000000     C*
090600000000     C* recupero il sottotitolo (se c'è)
090700000000     C                   Clear                   D1TIT
090800000000     C     SOTTITD1      IFNE      *BLANK
090900000000     C                   MOVEL     SOTTITD1      IDMSG
091000000000     C                   CALLB     'XRTVM'
091100000000     C                   PARM                    IDMSG            27
091200000000     C                   PARM                    TXTMSG          100
091300000000     C                   PARM                    ERRMSG            1
091400000000     C                   PARM      *Off          CTRMSG            1            centratura
091500000000     C                   PARM      15            LENMSG            3 0          lun output.
091600000000     C     ERRMSG        IFNE      *ON
091700000000     C                   MOVEL     TXTMSG        D1TIT
091800000000     C                   ELSE
091900000000     C                   MOVEL     *ALL'?'       D1TIT
092000000000     C                   ENDIF
092100000000     C                   ENDIF
092200000000     C*
092300000000     C* salvo stato F17 per sapere cosa fare in caso di F12
092400000000     C                   MOVE      *INKR         WINKR
092500000000     C                   WRITE     FMTT1
092600000000     C                   EXSR      GESPIE
092700000000     C                   MOVE      $LASTV        $LASTG
092800000000     C                   MOVE      'D1'          $LASTV
092900000000     C                   ENDIF
093000000000     C*
093100000000     C* se esistono errori sulla videata
093200000000     C* emetto la write del formato a indicatori spenti per vedere
093300000000     C* le eventuali decodifiche
093400130930   B1C                   if        (*IN99       = *On   or
093500130930   B1C                              *In17       = *On      )
093600000000     C                   MOVEA     *IN           WIN
093700000000     C                   MOVE      *ALL'0'       IN5098           49
093800000000     C                   MOVEA     IN5098        *IN(50)
093801141218     C                   Eval      WIn17        = *In17
093802141218     C                   Eval      *In17        = *Off
094100000000     C                   WRITE     FMTD1
094200000000     C                   MOVEA     WIN           *IN
094201141218     C                   Eval      *In17        = WIn17
094400000000     C                   ENDIF
094500000000     C*
094600000000     C                   EXFMT     FMTD1
094700000000     C*
094800000000     C* azzero la posizione del cursore
094900000000     C                   Z-ADD     0             H1RIGA
095000000000     C                   Z-ADD     0             H1COLO
095100000000     C*
095200000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
095300000000     C* INPUT
095400000000     C* $FC1 = Abilitazioni videata D1
095500000000     C* OUTPUT
095600000000     C* *IN97 + *IN99 Errori
095700000000     C                   MOVEA     $FC1          $FC
095800000000     C                   EXSR      CTRKEY
095900000000     C*
096000000000     C     *IN99         IFEQ      *OFF
096100000000     C*
096200000000     C                   SELECT
096300000000     C* F2=Decodifica
096400000000     C     $TASTO        WHENEQ    F02
096500000000     C                   EXSR      F02D1
096600000000     C* F3=Fine
096700000000     C     $TASTO        WHENEQ    F03
096800000000     C                   EXSR      F03D1
096900000000     C* F4=Ricerca
097000000000     C     $TASTO        WHENEQ    F04
097100000000     C                   EXSR      SEARCH
097200000000     C* F5=Refresh
097300000000     C     $TASTO        WHENEQ    F05
097400000000     C                   EXSR      F05D1
097500000000     C* F8=Successivo
097600000000     C     $TASTO        WHENEQ    F08
097700000000     C                   EXSR      F08D1
097800000000     C* F10=Immissione
097900000000     C     $TASTO        WHENEQ    F10
098000000000     C                   EXSR      F10D1
098100000000     C* F12=Ritorno
098200000000     C     $TASTO        WHENEQ    F12
098300000000     C                   EXSR      F12D1
098400000000     C* F18=Modifica valori assunti
098500000000     C     $TASTO        WHENEQ    F18
098600000000     C                   EXSR      F18D1
098700000000     C* F24=Altri tasti
098800000000     C     $TASTO        WHENEQ    F24
098900000000     C                   EXSR      F24D1
099000000000     C*
099100000000     C                   OTHER
099200000000     C                   EXSR      CTRD1
099300000000     C     *IN99         IFEQ      *OFF
099400000000     C* definisco l'ordinamento
099500000000     C                   EXSR      DEFORD
099600000000     C* richiamo la lista
099700000000     C                   MOVE      'S1'          $GEST
099800000000     C                   MOVE      *ON           $INZS1
099900000000     C                   MOVE      *BLANK        W1POS
100000000000     C                   ENDIF
100100000000     C*
100200000000     C                   ENDSL
100300000000     C*
100400000000     C                   ENDIF
100500000000     C*
100600000000     C                   ENDSR
100700000000     C/EJECT
100800000000     C************************************************************
100900000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
101000000000     C************************************************************
101100000000     C     INZD1         BEGSR
101200000000     C*
101300000000     C                   CLEAR                   FMTD1
102100140327     C*
102200000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
102300000000     C*    parzializzazioni
102400000000     C                   SELECT
102500000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
102600000000     C     $Unique       WHENEQ    *ON
102700000000     C* il pgm è richiamato e
102800000000     C* sono stati passati dei valori di parzializzazione ma non
102900000000     C* la chiave univoca
103000141218     C     OPZJ73        WHENNE    *BLANK
103100000000     C* imposto i valori passati sui campi a video
103200141218     C                   MOVEL     AnnoJ73       D1Anno
103400141218     C                   MOVEL     PartIvaJ73    D1PartIva
103500141218     C                   MOVEL     CdFiscJ73     D1CdFisc
103600141218     C                   MOVEL     SoggJ73       D1Sogg
103800141218     C                   MOVEL     AnnJ73        D1Ann
104000000000     C* il pgm non è richiamato
104100141218     C     OPZJ73        WHENEQ    *BLANK
104400141218     C                   Eval      D1Anno      = *Year
104401141218      * solo gli attivi
104600141218     C                   Eval      D1Ann       = '2'
104800141218     C                   ENDSL
104900000000      *
105400141218     C                   ENDSR
105500000000      /EJECT
105600000000      ************************************************************
105700000000      * GESTIONE F02 VIDEO D1
105800000000      ************************************************************
105900000000     C     F02D1         BEGSR
106000000000     C*
106100000000     C* per decodificare la videata richiamo la routine dei ctrl
106200000000     C* e poi disattivo gli eventuali messaggi di errore
106300000000     C                   EXSR      CTRD1
106400000000     C*
106500000000     C                   MOVE      *ALL'0'       IN5098
106600000000     C                   MOVEA     IN5098        *IN(50)
106700000000     C*
106800000000     C                   ENDSR
106900000000     C/EJECT
107000000000     C************************************************************
107100000000     C* GESTIONE F03 VIDEO D1
107200000000     C************************************************************
107300000000     C     F03D1         BEGSR
107400000000     C*
107500000000     C                   MOVE      *ON           $FINE
107600141218    >C                   MOVE      '1'           RETJ73
107700000000     C* fine programma
107800000000     C                   EXSR      ENDPGM
107900000000     C*
108000000000     C                   ENDSR
108100000000     C/EJECT
108200000000     C************************************************************
108300000000     C* GESTIONE F05 VIDEO D1
108400000000     C************************************************************
108500000000     C     F05D1         BEGSR
108600000000     C*
108700000000     C                   MOVE      *ON           $INZD1
108800000000     C*
108900000000     C                   ENDSR
109000000000     C/EJECT
109100000000     C************************************************************
109200000000     C* GESTIONE F08 VIDEO D1
109300000000     C************************************************************
109400000000     C     F08D1         BEGSR
109500000000     C*
109600000000     C                   MOVE      *ON           $FINE
109700141218    >C                   MOVE      '0'           RETJ73
109800000000     C*
109900000000     C                   ENDSR
110000000000     C/EJECT
110100000000     C************************************************************
110200000000     C* GESTIONE F10 VIDEO D1
110300000000     C************************************************************
110400000000     C     F10D1         BEGSR
110500000000     C*
110600141218    >C                   RESET                   SDGSJ74DS
110700141218    >C                   MOVE      xscsoc        SocietaJ74
110800141218    >C                   MOVEL     '01'          OPZJ74
110900141218    >C     CMTJ73        IFEQ      '1'
111000141218    >C                   MOVEL     '1'           CMTJ74
111100000000     C                   ELSE
111200141218    >C                   MOVEL     '0'           CMTJ74
111300000000     C                   ENDIF
111400141218     C                   Eval      AnnoJ74     = D1Anno
111401141223     C                   Eval      PartIVAJ74  = D1PartIVA
111402141223     C                   Eval      CdFiscJ74   = D1CdFisc
111403141223     C                   Eval      SoggJ74     = D1Sogg
111404141223     C                   Eval      KscJ74      = D1Ksc
111405141223     C                   Eval      TpOpeJ74    = D1TpOpe
111500141218     C                   Eval      AnnJ74      = D1Ann
112400141218     C                   Eval      TpAnnJ74    = D1TpAnn
112700000000     C* passare anche le parzializzazioni ricevute relative alla chiave
112800000000     C* univoca :
112900000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
113000141218    >C                   IF        OPZJ73 <> *Blank
113100000000     C                   ENDIF
113200000000     C*
113300141218    >C                   CALL      'SDGSJ74R'
113400000000     C                   PARM                    KPJBA
113500141218    >C                   PARM                    SDGSJ74DS
113600000000     C* ritorno da PGM gestione
113700141218     C                   EXSR      RTNJ74
113701150113     C*
113702150113     C* imposto quegli attributi del sfl che sono modificabili
113703150113     C* al richiamo del determinato pgm
113704150113     C                   EXSR      ATRS1
113800000000     C*
113900000000     C                   MOVE      *HIVAL        $LASTV
114000000000     C*
114100000000     C                   ENDSR
114200000000     C/EJECT
114300000000     C************************************************************
114400000000     C* GESTIONE F12 VIDEO D1
114500000000     C************************************************************
114600000000     C     F12D1         BEGSR
114700000000     C*
114800000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
114900000000     C*  per F12 torno alla lista
115000000000     C     $LOP          IFEQ      2
115100000000     C     WINKR         OREQ      *ON
115200000000     C                   MOVE      'S1'          $GEST
115300000000     C*  altrimenti, per F12 torno a menu
115400000000     C                   ELSE
115500000000     C                   MOVE      *ON           $FINE
115600141218    >C                   MOVE      '2'           RETJ73
115700000000     C                   ENDIF
115800000000     C*
115900000000     C                   ENDSR
116000000000     C/EJECT
116100000000     C************************************************************
116200000000     C* GESTIONE F18 VIDEO D1
116300000000     C************************************************************
116400000000     C     F18D1         BEGSR
116500000000     C*
116600000000     C                   MOVE      'D2'          $GEST
116700000000     C                   MOVEL     *ON           $INZD2
116800000000     C*
116900000000     C                   ENDSR
117000000000     C/EJECT
117100000000     C************************************************************
117200000000     C* GESTIONE F24 VIDEO D1
117300000000     C* input
117400000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
117500000000     C* $FX1 =Testi tasti funzione
117600000000     C* $FC1 = Abilitazioni
117700000000     C* $FR1 = Inversioni di fondo
117800000000     C************************************************************
117900000000     C     F24D1         BEGSR
118000000000     C*
118100000000     C                   MOVEA     $FX1          $FX
118200000000     C                   MOVEA     $FC1          $FC
118300000000     C                   MOVEA     $FR1          $FR
118400000000     C                   Z-ADD     $ULKD1        $ULK
118500000000     C                   Z-ADD     $D1L01        $L01
118600000000     C                   Z-ADD     $D1L02        $L02
118700000000     C                   EXSR      RIEKEY
118800000000     C                   Z-ADD     $ULK          $ULKD1            3 0
118900000000     C                   MOVEA     $FC           $FC1
119000000000     C                   MOVEL     $KEY1         Z1KE1
119100000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
119200000000     C                   MOVEL     $KEY2         Z1KE2
119300000000     C                   WRITE     FMTZ1
119400000000     C*
119500000000     C*
119600000000     C                   ENDSR
119700000000     C/EJECT
119800000000     C************************************************************
119900000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
120000000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
120100000000     C* 93 94 96 97 98
120200000000     C************************************************************
120300000000     C     CTRD1         BEGSR
120400000000     C*
120500000000     C                   SETOFF                                       99
120600000000     C*
120700000000     C                   EXSR      CtrAnno
121600000000     C*
121700000000     C                   EXSR      CtrSogg
122000000000     C*
122100000000     C                   EXSR      CtrAnn
122400130919     C*
122401141229     C                   EXSR      CtrKsc
122402141218     C*
122500130919     C                   EXSR      CtrTpOpe
122501141218     C*
122502141218     C                   EXSR      CtrNrInter
122600130829     C*
122700000000     C                   ENDSR
122800000000     C/EJECT
123000000000     C* Controllo anno di competenza
123100000000     C***********************************************************
123200000000     C     CtrAnno       BEGSR
123300000000     C*
123400000000     C                   if        D1Anno = 0
123500141218     C                   SetOn                                        5199
123600000000     C                   endif
123700000000     C*
123800000000     C                   ENDSR
123900000000     C/EJECT
132300000000     C***********************************************************
132400000000     C* Controllo codice soggetto
132500000000     C***********************************************************
132600000000     C     CtrSogg       BEGSR
132700000000     C*
132800000000     C                   Clear                   D1SoggD
132900000000     C                   if        D1Sogg     <> *Blank
133000000000     C*
133100000000     C     D1SOGG        IFNE      *BLANK
133200000000     C                   CALLb     'XALLINEA'
133300000000     C                   PARM                    D1SOGG
133400000000     C                   PARM      8             LEN               5 0
133500000000     C                   PARM                    XSCINP
133600000000     C                   ENDIF
133700000000     C*
133800000000     C                   MoveL     D1Sogg        WSogg
133900000000     C                   EXSR      CallDvaSog
134000000000     C*
134100000000     C                   if        DVAErrore  <> '1'
134200000000     C*
134300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
134400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
134500000000     C*
134600000000     C                   MoveL     DVSDes        D1SoggD
134700000000     C*
134800000000     C                   else
134900000000     C*
135000130930     C                   Eval      *In17       = *On
135100000000     C*
135200000000     C                   endif
135300000000     C*
135400000000     C                   endif
135500000000     C*
135600000000     C                   ENDSR
135700000000     C/EJECT
135701141218     C************************************************************
135702141218     C* Controllo fornitore
135703141218     C************************************************************
135704141229     C     CtrKsc        BEGSR
135705141218     C*
135706141218     C                   Clear                   D1KscD
135707141218     C*
135708141218     C                   if        D1KSC      <> *Blanks
135709141218     C*
135710141218     C                   CALLB     'XALLINEA'
135711141218     C                   PARM                    D1KSC
135712141218     C                   PARM      8             LEN               5 0
135713141218     C                   PARM                    XSCINP
135714141218     C*
135715141218     C                   Eval      DVALenOut   = %Size(DVAFOR)
135716141218     C*
135717141218     C                   CALLB     'NDDVASOG'
135718141218     C                   PARM      '1'           DVARIC
135719141218     C                   PARM      XScSoc        DVASOCIETA
135720141218     C                   PARM                    DVAUNITA
135721141218     C                   PARM      'DVAFOR    '  DVASTRUTT
135722141218     C                   PARM                    DVADTRIF
135723141218     C                   PARM                    DVALENOUT
135724141218     C                   PARM      'F'           DVASNATURA
135725141218     C                   PARM      D1Ksc         DVACODICE
135726141218     C                   PARM                    DVALINEAV
135727141218     C                   PARM                    DVAFILIALE
135728141218     C                   PARM                    *Omit
135729141218     C                   PARM                    DVATPIND
135730141218     C                   PARM                    DVACDIND
135731141218     C                   PARM                    DVAERRORE
135732141218     C                   PARM                    DVAOUTPUT
135733141218     C*
135734141218     C                   if        DVAErrore   = *Off
135735141218     C*
135736141218     C                   Eval      %SUBST(DVAFOR:1:DVALENOUT)
135737141218     C                                 = %SUBST(DVAOUTPUT:1:DVALENOUT)
135738141218     C*
135739141218     C                   else
135740141218     C*
135741141218     C                   Clear                   DVAFOR
135742141218     C                   SetOn                                        5299
135743141218     C*
135744141218     C                   endif
135745141218     C*
135746141218     C                   MoveL     DVFDesBrev    D1KScD
135747141218     C*
135748141218     C                   endif
135749141218     C*
135750141218     C                   ENDSR
135751141218     C/EJECT
140300130919     C***********************************************************
140400130919     C* Controllo tipo operazione
140500130919     C***********************************************************
140600130919     C     CtrTpOpe      BEGSR
140700130919     C*
140800130919     C                   Clear                   D1TpOpeD
140900130919     C*
141000130919     C                   Clear                   X07ValDs
141100130919     C*
141200130919     C                   Move      '1'           X07Ric
141300130919     C                   Move      *On           X07All
141400141218     C                   Movel     'SDGSJ73S '   X07TRc
141500141218     C                   Movel     'TPOPEJ73 '   X07Cam
141600130919     C                   Movel     D1TpOpe       X07VAL
141700130919     C*
141800130919     C                   Callb     'X07VALR'
141900130919     C                   Parm                    X07ValDs
142000130919     C*
142100130919     C     X07ERR        IFEQ      *Off
142200130919     C                   MOVEL     X07DES        D1TpOpeD
142300130919     C                   else
142400130919     C                   SetOn                                        5599
142500130919     C                   endif
142600130919     C*
142700130919     C                   ENDSR
142800130919     C/EJECT
142900000000     C***********************************************************
143000141218     C* Controllo numeri interni
143100000000     C***********************************************************
143200141218     C     CtrNrInter    BEGSR
143201150120     C*
143202150120     C                   CALLB     'XALLINEA'
143203150120     C                   PARM                    D1NrInter1
143204150120     C                   PARM      16            LEN2              5 0
143205150120     C                   PARM      '2'           Input             1
143206150120     C*
143207150120     C                   CALLB     'XALLINEA'
143208150120     C                   PARM                    D1NrInter2
143209150120     C                   PARM      16            LEN2              5 0
143210150120     C                   PARM      '2'           Input             1
143300000000     C*
143301141229     C                   if        D1NrInter1  > D1NrInter2   and
143302150120     C                             D1NrInter2  > *Blank
143303141218     C                   SetOn                                        5399
143304141218     C                   endif
143305141218     C*
143306141218     C                   ENDSR
143307141218     C/EJECT
143308141218     C***********************************************************
143309141218     C* Controllo dati annullati
143310141218     C***********************************************************
143311141218     C     CtrAnn        BEGSR
143312141218     C*
143400000000     C                   Clear                   D1AnnD
143500000000     C*
143600000000     C                   Clear                   X07ValDs
143700000000     C                   Move      '1'           X07Ric
143800000000     C                   Move      *Off          X07All
143900000000     C                   Movel     'ANDIZ000 '   X07TRc
144000000000     C                   Movel     'DSPATT   '   X07Cam
144100000000     C                   Movel     D1Ann         X07VAL
144200000000     C                   Callb     'X07VALR'
144300000000     C                   Parm                    X07ValDs
144400000000     C*
144500000000     C     X07ERR        IFEQ      *Off
144600000000     C                   MOVEL     X07DES        D1AnnD
144700000000     C                   else
144800000000     C                   SetOn                                        5699
144900000000     C                   endif
145000000000     C*
145100000000     C                   Clear                   D1TpAnnD
145200000000     C                   if        D1TpAnn    <>  *blank
145300000000     C*  solo se richiesti solo gli annullati oppure con annullati
145400000000     C                   if        D1Ann      =   '1'    or
145500000000     C                             D1Ann      =   '3'
145600000000     C                   exsr      CtrTpAnn
145700000000     C                   else
145800000000     C                   SetOn                                        5899
145900000000     C                   endif
146000000000     C                   endif
146100000000     C*
146200000000     C                   ENDSR
146300000000     C/EJECT
146400000000     C***********************************************************
146500000000     C* Controllo tipo annullamento
146600000000     C***********************************************************
146700000000     C     CtrTpAnn      BEGSR
146800000000     C*
146900000000     C                   Clear                   X07ValDs
147000000000     C                   Move      '1'           X07Ric
147100000000     C                   Move      *Off          X07All
147200141218     C                   Movel     'SDGDIE000'   X07TRc
147300141218     C                   Movel     'DIETPANN'    X07Cam
147400000000     C                   Movel     D1TpAnn       X07VAL
147500000000     C                   Callb     'X07VALR'
147600000000     C                   Parm                    X07ValDs
147700000000     C*
147800000000     C     X07ERR        IFEQ      *Off
147900000000     C                   MOVEL     X07DES        D1TpAnnD
148000000000     C                   else
148100000000     C                   SetOn                                        5799
148200000000     C                   endif
148300000000     C*
148400000000     C                   ENDSR
148500000000     C/EJECT
151000000000     C************************************************************
151100000000     C* Chiamata al driver dei soggetti
151200000000     C************************************************************
151300000000     C     CallDvaSog    BEGSR
151400000000     C*
151500000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
151600000000     C*
151700000000     C                   CALLB     'NDDVASOG'
151800000000     C                   PARM      '1'           DVARIC
151900000000     C                   PARM      XSCSOC        DVASOCIETA
152000000000     C                   PARM                    DVAUNITA
152100000000     C                   PARM      'DVASOG    '  DVASTRUTT
152200000000     C                   PARM                    DVADTRIF
152300000000     C                   PARM                    DVALENOUT
152400000000     C* Se si accede con il codice cli/for impostare i campi seguenti
152500000000     C* se si accede con il soggetto ometterli e impostare gli altri
152600000000     C*                  PARM                    DVASNATURA
152700000000     C*                  PARM                    DVACODICE
152800000000     C*                  PARM                    DVALINEAV
152900000000     C*                  PARM                    DVAFILIALE
153000000000     C                   PARM                    *omit
153100000000     C                   PARM                    *omit
153200000000     C                   PARM                    *omit
153300000000     C                   PARM                    *omit
153400000000     C                   PARM      WSogg         DVSSOGG
153500000000     C                   PARM      *Blank        DVSTPIND
153600000000     C                   PARM      *Blank        DVSCDIND
153700000000     C                   PARM                    DVAERRORE
153800000000     C                   PARM                    DVAOUTPUT
153900000000     C*
154000000000     C                   ENDSR
154100000000     C/EJECT
154200000000     C************************************************************
154300000000     C* DEFINIZIONE DELL'ORDINAMENTO
154400000000     C************************************************************
154500000000     C     DEFORD        BEGSR
154600000000     C*
154700000000     C* definisco l'ordinamento della lista a seconda dei valori
154800000000     C* impostati nel video di parzializzazione
154900000000     C                   Z-ADD     1             $ORD
155000000000     C*
155100000000     C                   SELECT
155200000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
155300000000     C* rcd con la chiave passata
155400000000     C     $Unique       WHENEQ    *ON
155500000000     C                   Z-ADD     99            $ORD
155600000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
155700000000     C* utilizzerò l'ordinamento precedentemente memorizzato
155800000000     C* (temporaneamente o definitivamente) nei valori assunti
155900000000     C                   OTHER
156000000000     C                   ENDSL
156100000000     C                   ENDSR
156200000000     C/EJECT
156300000000     C************************************************************
156400000000     C* Chiamata programma con tasto personalizzato
156500000000     C************************************************************
156600000000     C     FPerS1        BEGSR
156700000000     C*
156800000000     C* salvo la DS di ingresso
156900141218    >C                   MOVEL     SDGSJ73DS     $SaveDS
157000000000     C*
157100141218    >C                   MOVEL     SDGSJ73DS     KPJBU
157200000000     C                   CALL      $PGF2(PosFun)                        22
157300000000     C                   PARM                    KPJBA
157400000000     C*
157500000000     C* ripristino la DS di ingresso
157600141218    >C                   MOVEL     $SaveDS       SDGSJ73DS
157700000000     C*
157800000000     C                   ENDSR
157900000000     C/EJECT
158000000000     C************************************************************
158100000000     C* GESTIONE LISTA
158200000000     C************************************************************
158300000000     C     GESS1         BEGSR
158400000000     C*
158500000000     C* inizializzazione videata
158600000000     C     $INZS1        IFEQ      *ON
158700000000     C                   EXSR      INZS1
158800000000     C                   MOVE      *OFF          $INZS1
158900000000     C                   ENDIF
159000000000     C* emissione piede videata se proveniente da altra
159100000000     C* salvataggio valore formato di provenienza
159200000000     C     $LASTV        IFNE      'S1'
159300000000     C                   WRITE     FMTT1
159400000000     C                   EXSR      GESPIE
159500000000     C                   MOVE      $LASTV        $LASTG
159600000000     C                   MOVE      'S1'          $LASTV
159700000000     C                   ENDIF
159800000000     C*
159900000000     C     WMAX          IFEQ      0
160000000000     C                   WRITE     FMTD3                                        NO RCD
160100000000     C                   ENDIF
160200000000     C* Inizializzazione indice tasti funzione personalizzati
160300000000     C                   Eval      PosFun = 25
160400000000     C*
160500000000     C                   EXFMT     FMTC1
160600000000     C*
160700000000     C* azzero la posizione del cursore
160800000000     C                   Z-ADD     0             H1RIGA
160900000000     C                   Z-ADD     0             H1COLO
161000000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
161100000000     C* di refresh dello sfondo
161200000000     C                   MOVE      '1'           WExf
161300000000     C*
161400000000     C     C1NRR         IFNE      0
161500000000     C                   Z-ADD     C1NRR         WSFL
161600000000     C                   ENDIF
161700000000     C                   IF        SFLNRR > 0
161800000000     C                   Z-ADD     SFLNRR        C1RCD
161900000000     C                   ENDIF
162000000000     C* inizializzazione indice opzioni personalizzate
162100000000     C                   Eval      PosOpz = 0
162200000000     C* CONTROLLO ABILITAZIONE TASTI
162300000000     C* INPUT
162400000000     C* $FC2 = Abilitazioni videata S1
162500000000     C* OUTPUT
162600000000     C* *IN97 + *IN99 Errori
162700000000     C                   MOVEA     $FC2          $FC
162800000000     C                   EXSR      CTRKEY
162900000000     C*
163000000000     C     *IN99         IFEQ      *OFF
163100000000     C*
163200000000     C                   SELECT
163300000000     C* F3=Fine
163400000000     C     $TASTO        WHENEQ    F03
163500000000     C                   EXSR      F03S1
163600000000     C* F8=Successivo
163700000000     C     $TASTO        WHENEQ    F08
163800000000     C                   EXSR      F08S1
163900000000     C* F10=Immissione
164000000000     C     $TASTO        WHENEQ    F10
164100000000     C                   EXSR      F10S1
164200000000     C* F12=Ritorno
164300000000     C     $TASTO        WHENEQ    F12
164400000000     C                   EXSR      F12S1
164500000000     C* F17=Parzializzazioni
164600000000     C     $TASTO        WHENEQ    F17
164700000000     C                   EXSR      F17S1
164800000000     C* F18=Modifica valori assunti
164900000000     C     $TASTO        WHENEQ    F18
165000000000     C                   EXSR      F18S1
165100000000     C* F23=Altre opzioni
165200000000     C     $TASTO        WHENEQ    F23
165300000000     C                   EXSR      F23S1
165400000000     C* F24=Altri tasti
165500000000     C     $TASTO        WHENEQ    F24
165600000000     C                   EXSR      F24S1
165700000000     C* ROLL-UP
165800000000     C     $TASTO        WHENEQ    ROLLUP
165900000000     C                   EXSR      ROLS1
166000000000     C* Personalizzata
166100000000     C     $PGF2(PosFun) Whenne    *BLANK
166200000000     C                   EXSR      FperS1
166300000000     C*
166400000000     C                   OTHER
166500000000     C* CONTROLLO DATI
166600000000     C                   EXSR      CTRC1
166700000000     C     *IN99         IFEQ      *OFF
166800000000     C                   EXSR      CTRS1
166900000000     C                   ENDIF
167000000000     C*
167100000000     C                   ENDSL
167200000000     C*
167300000000     C                   ENDIF
167400000000     C*
167500000000     C                   ENDSR
167600000000     C/EJECT
167700000000     C************************************************************
167800000000     C* Caricamento OPZIONI
167900000000     C************************************************************
168000000000    >C     CarOpz        BEGSR
168100000000     C*
168200000000     C* Reperimento tasti di funzione
168300000000     C                   EXSR      REPKEY
168400000000     C*
168500000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
168600000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
168700000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
168800000000     C     $ULKD1        IFEQ      0
168900000000     C                   EXSR      F24D1
169000000000     C                   ELSE
169100000000     C                   WRITE     FMTZ1
169200000000     C                   ENDIF
169300000000     C*
169400000000     C                   ENDSR
169500000000     C/EJECT
169600000000     C************************************************************
169700000000     C* INIZIALIZZAZIONE LISTA
169800000000     C************************************************************
169900000000    >C     INZS1         BEGSR
170000000000     C* pulizia SFL
170100000000     C                   SETOFF                                         3031
170200000000     C                   WRITE     FMTC1
170300000000     C                   SETON                                          31
170400000000     C*
170500000000     C* pulizia del sottotitolo
170600000000     C                   Clear                   D1TIT
170700000000     C*
170800000000     C                   eval      C1Anno  = D1Anno
171200000000     C*
171300000000     C* riempimento opzioni
171400000000     C                   MOVEA     $TX1          $TX
171500000000     C                   MOVEA     $FL1          $FL
171600000000     C                   Z-ADD     0             WCOZ
171700000000     C                   Z-ADD     0             WMXO
171800000000     C*  equivale al num.di elementi della schiera $TX1
171900000000     C                   Z-add     OpzLun        WMXO
172000000000     C                   Z-ADD     $C1Op1        $L01
172100000000     C                   Z-ADD     $C1Op2        $L02
172200000000     C                   EXSR      RIEOPZ
172300000000     C                   Z-ADD     23            X
172400000000     C                   MOVE      FF23          $FC2(X)
172500000000     C                   MOVEL     WOPZ1         C1OPZ1
172600000000     C                   MOVEL     $ALLOPT       H1ALLOPT
172700000000     C                   MOVEL     WOPZ2         C1OPZ2
172800000000     C                   Z-ADD     WCOZ          C1COZ
172900000000     C*
173000000000     C* CARICAMENTO SFL A PAGINE
173100000000     C*
173200000000     C                   Z-ADD     0             S1NRR
173300000000     C                   Z-ADD     0             WMAX
173400000000     C*
173500000000     C* Posizionamento su file pilota
173600000000     C                   CLEAR                   C1POS
173700000000     C                   EXSR      SETANA
173800000000     C* ripristino i valori del posizionamento
173900000000     C                   MOVE      W1POS         C1POS
174000000000     C*
174100000000     C* Lettura file pilota
174200000000     C                   EXSR      REDANA
174300000000     C*
174400000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
174500000000     C                   IF        $LastV = 'D1'
174600000000     C                   Z-ADD     SFLPAG        WPAG
174700000000     C                   ENDIF
174800000000     C                   EXSR      ROLS1
174900000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
175000000000     C                   Z-ADD     SFLPAG        WPAG
175100000000     C*
175200000000     C                   ENDSR
175300000000     C/EJECT
175400000000     C************************************************************
175500000000     C* CARICAMENTO PAGINA LISTA
175600000000     C************************************************************
175700000000     C     ROLS1         BEGSR
175800000000     C*
175900000000     C                   SETOFF                                       32
176000000000     C                   Z-ADD     0             Y
176100000000     C                   Z-ADD     WMAX          S1NRR
176200000000     C*
176300000000     C* Leggo dal file anagrafico per caricare la lista
176400000000     C*
176500000000     C     $EndFile      DOWEQ     *OFF
176600000000     C     Y             ANDLT     WPAG
176700000000     C*
176800000000     C                   EXSR      RIES1
176900000000     C*
177000000000     C                   ADD       1             S1NRR
177100000000     C                   ADD       1             Y
177200000000     C*
177300000000     C                   WRITE     FMTS1
177400000000     C*
177500000000     C                   EXSR      REDANA
177600000000     C*
177700000000     C                   ENDDO
177800000000     C*
177900000000     C                   Z-ADD     S1NRR         WMAX                 30
178000000000     C*
178100000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
178200000000     C*
178300000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
178400000000     C                   MVR                     RESTO             3 0
178500000000     C     PAGINE        MULT      SFLPAG        C1RCD
178600000000     C     RESTO         IFGT      0
178700000000     C                   ADD       1             C1RCD
178800000000     C                   ELSE
178900000000     C                   SUB       SFLPAG        C1RCD
179000000000     C                   ADD       1             C1RCD
179100000000     C                   ENDIF
179200000000     C*
179300000000     C                   ENDSR
179400000000     C/EJECT
179500000000     C************************************************************
179600000000     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
179700000000     C************************************************************
179800000000     C     SETANA        BEGSR
179801141218     C*
179804141218    >C                   Move      XscSoc        DIESocieta
179805141218     C*
179806141218     C                   SELECT
179807141218     C*
179811141218     C*
179812141218    >C* Ord.1 = chiave ordinamento 1
179813141218     C     $ORD          WHENEQ    1
179814141218     C*
179815141218    >C                   Z-ADD     D1Anno        DIEAnno
179818141218     C     W1POS         IFNE      *blanks
179819141218    >C                   MoveL     W1POS         DIECogRag
179820141218     C                   ELSE
179821141218    >C                   MoveL     *Blanks       DIECogRag
179822141218     C                   ENDIF
179823141218    >C     K03DIE02      SETLL     SDGDIE02L
179824141218     C*
179825141218     C                   ENDSL
191900000000     C*
192000000000     C                   ENDSR
192100000000     C/EJECT
192200000000     C************************************************************
192300000000     C* LETTURA RCD ARCHIVIO PILOTA
192400000000     C************************************************************
192500000000     C     REDANA        BEGSR
192600000000     C*
192700000000     C                   MOVEL     *OFF          $EndFile
192800000000     C                   MOVEL     *OFF          $RCDOK
192900000000     C*
193000000000     C     $EndFile      DOUEQ     *ON
193100000000     C     $RCDOK        OREQ      *ON
193101141218     C*
193102141218     C                   SELECT
193103141218     C*
193104141218     C     $ORD          WHENEQ    1
193110141218    >C                   Z-ADD     D1Anno        DIEAnno
193113141218    >C     K02DIE02      READE     SDGDIE02L                              21
193114141218     C                   ENDSL
193115141218     C*
193116141218     C                   Move      *IN21         $EndFile
195400000000     C*
195500000000     C     $EndFile      IFEQ      *OFF
195600000000     C                   EXSR      SELANA
195700000000     C                   ENDIF
195800000000     C*
195900000000     C                   ENDDO
196000000000     C*
196100000000     C                   MOVE      $EndFile      *IN33                          SflEnd
196200000000     C*
196300000000     C                   ENDSR
196400000000     C/EJECT
196500000000     C************************************************************
196600000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
196700000000     C************************************************************
196800000000     C     CLCPAG        BEGSR
196900000000     C* Input :
197000000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
197100000000     C*          cursore
197200000000     C* - SFLPAG = numero rcd per pagina sfl
197300000000     C* Output :
197400000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
197500000000     C*
197600000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
197700000000     C                   MVR                     RESTO             3 0
197800000000     C     RESTO         IFGT      0
197900000000     C                   ADD       1             PAGINE
198000000000     C                   ENDIF
198100000000     C     PAGINE        MULT      SFLPAG        WPAG
198200000000     C*
198300000000     C                   ENDSR
198400000000     C/EJECT
198500000000     C************************************************************
198600000000     C* SELEZIONI SULLA RIGA LETTA
198700000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
198800000000     C* non rientra nei parametri di parzializzazione
198900000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
199000000000     C* superiore stabilito nei parametri
199100000000     C*
199200000000     C************************************************************
199300000000     C     SELANA        BEGSR
199400000000     C*
199500000000     C                   MOVEL     *ON           $RCDOK
199600000000     C                   MOVE      *hival        ALL9             15
199602141223     C*
199603141223     C* creo limiti inferiore/superiore
199604141223     C                   EXSR      DefMinMax
199700000000     C*
199800000000     C* creo limite massimo parzializzazioni :
199900000000     C* per ogni valore alfanumerico creo il limite superiore
200000000000     C* per cui caricare, creando una variabile che contiene
200100000000     C* il valore digitato nel campo a video riempito di "9"
200200000000     C*
200300000000     C                   SELECT
200900000000     C*
201000000000     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
201001141223     C                   When      DIEPartIVA  < PIVA111       or
201002141223     C                             DIEPartIVA  > PIVA999
201003141223     C                   Eval      $RcdOk      = *Off
201004141223     C*
201005141223     C                   When      DIECdFisc   < CdFisc111     or
201006141223     C                             DIECdFisc   > CdFIsc999
201007141223     C                   Eval      $RcdOk      = *Off
201100000000     C*
201101141223     C                   When      D1Sogg     <> *Blank        and
201102141223     C                             DIESogg    <> D1Sogg
201103141223     C                   Eval      $RcdOk      = *Off
201104141223     C*
201105141223     C                   When      D1Ksc      <> *Blank        and
201106141223     C                             DIEKsc     <> D1Ksc
201107141223     C                   Eval      $RcdOk      = *Off
201108141223     C*
201109141223     C                   When      D1TpOpe    <> *Blank        and
201110141223     C                             DIETpOpe   <> D1TpOpe
201111141223     C                   Eval      $RcdOk      = *Off
201112141223     C*
201113141223     C                   When      DIENrInter  < D1NrInter1       or
201114141223     C                             (DIENrInter > D1NrInter2  and
201115150120     C                              D1NrInter2 > *Blank          )
201116141223     C                   Eval      $RcdOk      = *Off
201117141223     C*
201118141223     C* richiesti solo gli annullati e record attivo
201119141223     C                   When      DIEAnn     <> '1'         and
201120141223     C                             D1Ann       = '3'
201121141223     C                   Eval      $RcdOk      = *Off
201122141223     C*
201123141223     C* richiesti solo gli attivi e record annullato
201124141223     C                   When      DIEAnn      = '1'         and
201125141223     C                             D1Ann       = '2'
201126141223     C                   Eval      $RcdOk      = *Off
201127141223     C*
201128141223     C                   When      DIETpAnn    < TpAnn111      or
201129141223     C                             DIETpAnn    > TpAnn999
201130141223     C                   Eval      $RcdOk      = *Off
201131141223     C*
201200000000     C                   ENDSL
201300000000     C*
201400000000     C* FACILITAZIONE NEGLI ACCESSI
201500000000     C*
201600000000     C* nel caso che uno dei dati letti sia > del limite superiore
201700000000     C* oppure < del limite inferiore è necessario fare dei
201800000000     C* ragionamenti onde evitare altri accessi inutili;
201900000000     C* questi ragionamenti dovranno tenere conto dell'ordinamento
202000000000     C* attualmente vigente, per cui si potranno verificare i seguenti
202100000000     C* casi :
202200000000     C* 1. un campo è > del limite superiore ed è la 1a chiave della
202300000000     C*    klist dell'ordinamento vigente :
202400000000     C*    nessun altro rcd sarà valido per cui bisogna andare a fine
202500000000     C*    file mediante un *HIVAL SETLL;
202600000000     C* 2. un campo è > del limite superiore e NON è la 1a chiave della
202700000000     C*    klist dell'ordinamento vigente :
202800000000     C*    nessun rcd che verrà letto con gli stessi valori per la
202900000000     C*    precedente porzione di klist sarà valido, per cui
203000000000     C*    conviene saltarli facendo una SETGT apunto con la klist che
203100000000     C*    esclude il campo in esame;
203200000000     C* 3. un campo è < del limite inferiore e NON è la 1a chiave della
203300000000     C*    klist dell'ordinamento vigente :
203400000000     C*    bisogna arrivare al più presto al limite inferiore per cui
203500000000     C*    conviene fare una SETLL con una klist avente come ultimo
203600000000     C*    campo quello in esame, lasciando per gli altri campi gli
203700000000     C*    stessi valori letti ma sostituendo per quello in esame
203800000000     C*    il limite inferiore;
203900000000     C* non è possibile che un campo che è anche chiave primaria
204000000000     C* dell'ordinamento vigente sia < del limite inferiore perchè
204100000000     C* nella routine SETANA la setll viene appunto fatta con il
204200000000     C* limite inferiore;
204300000000     C* non si deve prendere in considerazione il caso che un campo
204400000000     C* non presente nella chiave dell'ordinamento vigente sia
204500000000     C* esterno ai limiti perchè non genererebbe migliorie negli
204600000000     C* accessi;
204700000000     C*
204800000000     C                   IF        $RcdOk = *off
204900000000     C*
205000000000     C                   ENDIF
205100000000     C*
205200000000     C                   ENDSR
205300000000     C/EJECT
205301141223     C************************************************************
205302141223     C* Definisco limiti minimi e massimi per le selezioni
205303141223     C************************************************************
205304141223     C     DefMinMax     BEGSR
205307141223     C*
205314141223     C                   Select
205315141223     C* chiesti attivi e non attivi
205316141223     C                   When      D1Ann       = '1'
205317141223     C                   Eval      Ann111      = *Blank
205318141223     C                   Eval      Ann999      = '1'
205319141223     C* chiesti solo attivi
205320141223     C                   When      D1Ann       = '2'
205321141223     C                   Eval      Ann111      = *Blank
205322141223     C                   Eval      Ann999      = *Blank
205323141223     C* chiesti solo non attivi
205324141223     C                   When      D1Ann       = '3'
205325141223     C                   Eval      Ann111      = '1'
205326141223     C                   Eval      Ann999      = '1'
205327141223     C                   endSl
205328141223     C*
205329141223     C                   if        D1TpAnn    <> *Blank
205330141223     C                   MoveL     D1TpAnn       TpAnn111
205331141223     C                   MoveL     D1TpAnn       TpAnn999
205332141223     C                   else
205333141223     C                   MoveL     *Blank        TpAnn111
205334141223     C                   MOVEL(P)  ALL9          TpAnn999
205335141223     C                   endif
205374141223     C*
205376141223     C                   if        D1PartIVA  <> *Blank
205377141223     C                   MoveL     D1PartIVA     PIVA111
205378141223     C     D1PartIVA     CAT       ALL9:0        PIVA999
205379141223     C                   else
205380141223     C                   MoveL     *Blank        PIVA111
205381141223     C                   MoveL(P)  ALL9          PIVA999
205382141223     C                   endif
205383141223     C*
205384141223     C                   if        D1CdFisc   <> *Blank
205385141223     C                   MoveL     D1CdFisc      CdFisc111
205386141223     C     D1CdFisc      CAT       ALL9:0        CdFisc999
205388141223     C                   else
205389141223     C                   MoveL     *Blank        CdFisc111
205390141223     C                   MoveL(P)  ALL9          CdFisc999
205391141223     C                   endif
205626141223     C*
205627141223     C                   ENDSR
205628141223     C/EJECT
205629000000     C************************************************************
205630000000     C* RIEMPIMENTO RIGA LISTA
205631000000     C************************************************************
205700000000     C     RIES1         BEGSR
205800000000     C*
205900000000     C                   CLEAR                   FMTS1
206000000000     C*
206100000000     C* riempio i campi del sfl, compresi gli hidden con i valori
206200000000     C* del rcd letto
206300141218     C                   MoveL     DIEPartIVA    S1PartIva
206400141218     C                   MoveL     DIECdFisc     S1CdFisc
206401150112     C                   MoveL     DIEKsc        H1Ksc
206500141218     C                   MoveL     DIECogRag     S1CogRag
206501141223     C                   MoveL     DIEDtInter    H1DtInter
206502141223     C                   if        DIEDtInter <> *Loval
206503141223     C     *JobRun       Move      DIEDtInter    S1DtInter
206504141223     C                   endif
206505150120     C                   Move      DIENrInter    S1NrInter
207400000000     C*
207500000000     C* azzero le memorizzazioni degli attributi non bottoni
207501141218     C                   MOVEL     *Blank        H1ANNA
207502141218     C                   if        DIEAnn      = '1'       or
207503141218     C                             DIEAnn      = '9'
207504141218     C                   MoveL     *ON           H1ANNA
207505141218     C                   endif
207506150113     C*
207507150113     C                   Eval      H1Origine   = DIEOrigine
207510141218     C*
208400000000     C* imposto quegli attributi del sfl che sono modificabili
208500000000     C* al richiamo del determinato pgm
208600000000     C                   EXSR      ATRS1
208700000000     C*
208800000000     C                   ENDSR
208900000000     C/EJECT
209000000000     C************************************************************
209100000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
209200000000     C************************************************************
209300000000     C     ATRS1         BEGSR
209400000000     C*
209500000000     C* in questa routine bisogna attivare indicatori o campi programma
209600000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
209700000000     C* attenzione : non è questo il posto in cui eseguire controlli,
209800000000     C*  dovrebbero già arrivare dei flag
209900000000     C*
210000000000     C* azzero gli attributi non bottoni
210100000000     C                   MOVEL     X'A0'         S1ANNA
210200000000     C*
210300000000     C     H1ANNA        IFEQ      *ON
210400000000     C                   MOVEL     ANNUL         S1ANNA
210500000000     C                   ENDIF
210501150113     C*
210502150113     C* se modificato manualmente, sottolineo i riferimenti
210503150113     C                   if        H1Origine   = 'M'
210504150113     C                   Eval      *In13       = *On
210505150113     C                   else
210506150113     C                   Eval      *In13       = *Off
210507150113     C                   endif
210600000000     C*
210700000000     C                   ENDSR
210800000000     C/EJECT
210900000000     C************************************************************
211000000000     C* GESTIONE F08 VIDEO S1
211100000000     C************************************************************
211200000000     C     F08S1         BEGSR
211300000000     C*
211400000000     C                   MOVE      *ON           $FINE
211500141218    >C                   MOVE      '0'           RETJ73
211600000000     C*
211700000000     C                   ENDSR
211800000000     C/EJECT
211900000000     C************************************************************
212000000000     C* GESTIONE F03 VIDEO S1
212100000000     C************************************************************
212200000000     C     F03S1         BEGSR
212300000000     C*
212400000000     C                   MOVE      *ON           $FINE
212500141218    >C                   MOVE      '1'           RETJ73
212600000000     C* fine programma
212700000000     C                   EXSR      ENDPGM
212800000000     C*
212900000000     C                   ENDSR
213000000000     C/EJECT
213100000000     C************************************************************
213200000000     C* GESTIONE F10 VIDEO S1
213300000000     C* AGGIUNTA RECORD
213400000000     C************************************************************
213500000000     C     F10S1         BEGSR
213600000000     C*
213700141218     C                   RESET                   SDGSJ74DS
213800141218     C                   MOVE      xscsoc        SocietaJ74
213900141218     C                   MOVEL     '01'          OPZJ74
214000141218     C     CMTJ73        IFEQ      '1'
214100141218     C                   MOVEL     '1'           CMTJ74
214200000000     C                   ELSE
214300141218     C                   MOVEL     '0'           CMTJ74
214400000000     C                   ENDIF
215701141223     C                   Eval      AnnoJ74     = D1Anno
215702141223     C                   Eval      PartIVAJ74  = D1PartIVA
215703141223     C                   Eval      CdFiscJ74   = D1CdFisc
215704141223     C                   Eval      SoggJ74     = D1Sogg
215705141223     C                   Eval      KscJ74      = D1Ksc
215706141223     C                   Eval      TpOpeJ74    = D1TpOpe
215707141223     C                   Eval      AnnJ74      = D1Ann
215708141223     C                   Eval      TpAnnJ74    = D1TpAnn
215800000000     C* passare anche le parzializzazioni ricevute relative alla chiave
215900000000     C* univoca :
216000000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
216100141218    >C                   IF        OPZJ73 <> *Blank
216200000000     C                   ENDIF
216300141218    >C                   CALL      'SDGSJ74R'
216400000000     C                   PARM                    KPJBA
216500141218    >C                   PARM                    SDGSJ74DS
216600000000     C* ritorno da PGM gestione
216700141218    >C                   EXSR      RTNJ74
216800000000     C     WINZS1        IFEQ      *ON
216900000000     C                   MOVE      *ON           $INZS1
217000000000     C* se esistevano già righe sul sfl
217100000000     C* calcolo pagina a cui deve posizionarsi
217200000000     C     WSFL          IFGT      0
217300000000     C                   EXSR      CLCPAG
217400000000     C* altrimenti carico solo la 1a pagina
217500000000     C                   ELSE
217600000000     C                   Z-ADD     SFLPAG        WPAG
217700000000     C                   ENDIF
217800000000     C                   ENDIF
217900000000     C*
218000000000     C                   MOVE      *HIVAL        $LASTV
218100000000     C*
218200000000     C                   ENDSR
218300000000     C/EJECT
218400000000     C************************************************************
218500000000     C* GESTIONE F12 VIDEO S1
218600000000     C************************************************************
218700000000     C     F12S1         BEGSR
218800000000     C*
218900000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
219000000000     C     $LOP          IFEQ      1
219100000000     C                   MOVE      'D1'          $GEST
219200000000     C*  se la 1a videata è questa, per F12 torno a menu
219300000000     C                   ELSE
219400000000     C                   MOVE      *ON           $FINE
219500141218    >C                   MOVE      '2'           RETJ73
219600000000     C                   ENDIF
219700000000     C*
219800000000     C                   ENDSR
219900000000     C/EJECT
220000000000     C************************************************************
220100000000     C* GESTIONE F17 VIDEO S1
220200000000     C************************************************************
220300000000     C     F17S1         BEGSR
220400000000     C*
220500000000     C                   MOVE      'D1'          $GEST
220600000000     C* se il pgm è stato richiamato con chiave univoca
220700000000     C* e dopo è stata richiesta una parzializzazione
220800000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
220900000000     C* valore; propongo come ordinamento il 1°)
221000000000     C     $Unique       IFEQ      *ON
221100000000     C                   MOVE      *OFF          $Unique
221200000000     C                   ENDIF
221300000000     C     $ORD          IFEQ      99
221400000000     C                   Z-ADD     1             $ORD
221500000000     C                   MOVE      *ON           $INZD1
221600000000     C                   ENDIF
221700000000     C*
221800000000     C                   ENDSR
221900000000     C/EJECT
222000000000     C************************************************************
222100000000     C* GESTIONE F18 VIDEO S1
222200000000     C************************************************************
222300000000     C     F18S1         BEGSR
222400000000     C*
222500000000     C                   MOVE      'D2'          $GEST
222600000000     C                   MOVEL     *ON           $INZD2
222700000000     C     $ORD          IFEQ      99
222800000000     C                   Z-ADD     1             $ORD
222900000000     C                   ENDIF
223000000000     C*
223100000000     C                   ENDSR
223200000000     C/EJECT
223300000000     C************************************************************
223400000000     C* GESTIONE F23 VIDEO S1
223500000000     C************************************************************
223600000000     C     F23S1         BEGSR
223700000000     C*
223800000000     C                   Z-ADD     $C1Op1        $L01
223900000000     C                   Z-ADD     $C1Op2        $L02
224000000000     C                   EXSR      RIEOPZ
224100000000     C                   Z-ADD     23            X
224200000000     C                   MOVE      FF23          $FC2(X)
224300000000     C                   MOVEL     WOPZ1         C1OPZ1
224400000000     C                   MOVEL     $ALLOPT       H1ALLOPT
224500000000     C                   MOVEL     WOPZ2         C1OPZ2
224600000000     C                   Z-ADD     WCOZ          C1COZ
224700000000     C*
224800000000     C                   ENDSR
224900000000     C/EJECT
225000000000     C************************************************************
225100000000     C* GESTIONE F24 VIDEO S1
225200000000     C************************************************************
225300000000     C     F24S1         BEGSR
225400000000     C*
225500000000     C                   MOVEA     $FX2          $FX
225600000000     C                   MOVEA     $FC2          $FC
225700000000     C                   MOVEA     $FR2          $FR
225800000000     C                   Z-ADD     $ULKS1        $ULK
225900000000     C                   Z-ADD     $S1L01        $L01
226000000000     C                   Z-ADD     $S1L02        $L02
226100000000     C                   EXSR      RIEKEY
226200000000     C                   Z-ADD     $ULK          $ULKS1            3 0
226300000000     C                   MOVEA     $FC           $FC2
226400000000     C                   MOVEL     $KEY1         Z2KE1
226500000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
226600000000     C                   MOVEL     $KEY2         Z2KE2
226700000000     C                   WRITE     FMTZ2
226800000000     C*
226900000000     C                   ENDSR
227000000000     C/EJECT
227100000000     C************************************************************
227200000000     C* CONTROLLO TESTATA LISTA
227300000000     C************************************************************
227400000000     C     CTRC1         BEGSR
227500000000     C*
227600000000     C                   MOVE      *OFF          *IN99
227700000000     C*
227800000000     C     C1POS         IFNE      W1POS
227900000000     C                   EXSR      POSS1
228000000000     C                   ENDIF
228100000000     C*
228200000000     C                   ENDSR
228300000000     C/EJECT
228400000000     C************************************************************
228500000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
228600000000     C************************************************************
228700000000     C     POSS1         BEGSR
228800000000     C*
228900000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
229000000000     C*
229100000000   B1C     WMAX          IFGT      0
229200000000     C                   READC     FMTS1                                  21
229300000000   B2C     *IN21         DOWEQ     *OFF
229400000000     C     *IN99         ANDEQ     *OFF
229500000000     C*
229600000000   B3C     S1OPZ         IFNE      0
229700000000     C                   SETON                                        92  99
229800000000   E3C                   ENDIF
229900000000     C* se rilevata opzione attiva, attivo sflnxtchg
230000000000   B3C     *IN99         IFEQ      *ON
230100000000     C                   MOVE      *ON           *IN32
230200000000     C*
230300000000     C* imposto quegli attributi del sfl che sono modificabili
230400000000     C* al richiamo del determinato pgm
230500000000     C                   EXSR      ATRS1
230600000000     C*
230700000000     C                   UPDATE    FMTS1
230800000000   X3C                   ELSE
230900000000     C                   READC     FMTS1                                  21
231000000000   E3C                   ENDIF
231100000000     C*
231200000000   E2C                   ENDDO
231300000000     C*
231400000000   E1C                   ENDIF
231500000000     C*
231600000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
231700000000     C* AL VALORE DI POSIZIONAMENTO :
231800000000     C* se questo è maggiore del valore del rispettivo campo di
231900000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
232000000000     C* con i valori di parzializzazione.
232100000000     C*
232200000000   B1C     *IN99         IFEQ      *OFF
232300000000    >C                   MOVE      *BLANK        W1POS
232400000000     C*
232500000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
232600000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
232700000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
232800000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
232900000000     C* verificata la condizione suddetta avverrà con posizionamento a
233000000000     C* partire dal valore di parzializzazione
233100000000     C*
233200000000   B2C                   SELECT
233300000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
233400000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
233500000000    >C* dei relativi campi
233600000000     C*
233700000000     C     $ORD          WHENEQ    1
233800000000     C*
233900000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
234000000000     C*
234100000000   E2C                   ENDSL
234200000000     C*
234300000000     C* richiedo l'inizializzazione sfl
234400000000     C                   MOVE      *ON           $INZS1
234500000000     C*
234600000000   E1C                   ENDIF
234700000000     C*
234800000000     C                   ENDSR
234900000000     C/EJECT
235000000000     C************************************************************
235100000000     C* CONTROLLO OPZIONI LISTA
235200000000     C************************************************************
235300000000     C     CTRS1         BEGSR
235400000000     C*
235500000000     C                   MOVEL     *OFF          $ESCI
235600000000     C                   SETOFF                                       99
235700000000     C                   Z-ADD     0             S1OPZ
235800000000     C*
235900000000     C* Leggo il sfl solo se ci sono rcd
236000000000   B1C     WMAX          IFGT      0
236100000000     C                   READC     FMTS1                                  21
236200000000     C*
236300000000     C* esce se fine sfl o errore che richiede l'uscita
236400000000   B2C     *IN21         DOWEQ     *OFF
236500000000     C     $ESCI         ANDEQ     *OFF
236600000000     C                   Z-ADD     S1NRR         C1RCD
236700000000     C* ctrl su riga
236800000000     C                   EXSR      RECS1
236900000000     C* gestione opzioni
237000000000   B3C     S1OPZ         IFNE      0
237100000000     C     *IN99         ANDEQ     *OFF
237200000000     C                   EXSR      OPZS1
237300000000   E3C                   ENDIF
237400000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
237500000000   B3C     *IN99         IFEQ      *ON
237600000000     C                   MOVE      *ON           *IN32
237700000000   E3C                   ENDIF
237800000000     C*
237900000000     C* imposto quegli attributi del sfl che sono modificabili
238000000000     C* al richiamo del determinato pgm
238100000000     C                   EXSR      ATRS1
238200000000     C*
238300000000     C                   Z-ADD     0             S1OPZ
238400000000     C*
238500000000     C                   UPDATE    FMTS1
238600000000     C                   MOVE      *OFF          *IN32
238700000000     C*
238800000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
238900000000   B3C     $ESCI         IFEQ      *OFF
239000000000     C                   READC     FMTS1                                  21
239100000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
239200000000   B4C     *IN21         IFEQ      *ON
239300000000     C                   MOVE      WINZS1        $INZS1
239400000000     C                   MOVE      *off          WINZS1
239500000000     C* se è avvenuta almeno una modifica
239600000000   B5C                   IF        $INZS1 = *on
239700000000     C* calcolo pagina a cui deve posizionarsi
239800000000     C     WSFL          IFGT      0
239900000000     C                   EXSR      CLCPAG
240000000000     C                   ELSE
240100000000     C                   Z-ADD     SFLPAG        WPAG
240200000000     C                   ENDIF
240300000000   E5C                   ENDIF
240400000000   E4C                   ENDIF
240500000000   E3C                   ENDIF
240600000000     C*
240700000000   E2C                   ENDDO
240800000000     C*
240900000000   E1C                   ENDIF
241000000000     C*
241100000000     C                   ENDSR
241200000000     C/EJECT
241300000000     C************************************************************
241400000000     C* CONTROLLO CAMPI I/O RIGA LISTA
241500000000     C************************************************************
241600000000     C     RECS1         BEGSR
241700000000     C*
241800000000     C* reset indicatori DSPATR
241900000000     C                   MOVE      *ALL'0'       IN4049           10
242000000000     C                   MOVEA     IN4049        *IN(40)
242100000000     C*
242200000000     C* Controllo se Utente Autorizzato alla scelta
242300000000     C     S1OPZ         IFNE      0
242400000000     C                   Z-ADD     1             X
242500000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
242600000000     C* mi salvo l'indice dell' opzione
242700000000     C   21              Z-add     X             PosOpz            2 0
242800000000     C     *IN21         IFEQ      *OFF
242900000000     C     $FL1(X)       OREQ      'N'
243000000000     C                   MOVE      *ON           $ESCI
243100000000     C                   SETON                                        409199
243200000000     C                   ENDIF
243300000000     C                   ENDIF
243400000000     C*
243500000000     C*
243600000000     C                   ENDSR
243700000000     C/EJECT
243800000000     C************************************************************
243900000000     C* GESTIONE OPZIONI LISTA
244000000000     C************************************************************
244100000000     C     OPZS1         BEGSR
244200000000     C*
244300000000     C                   Z-ADD     RIRI          CURR
244400000000     C                   Z-ADD     COCO          CURC
244500000000     C*
244600000000     C                   SELECT
244700000000     C*
244800000000     C* se opz=Selezione
244900000000     C* passo la chiave al pgm chiamante
245000000000     C     S1OPZ         WHENEQ    1
245100141218    >C                   MOVE      S1OPZ         OPZJ73
245200141218     C* passare nei campi appositi della DS SDGSJ73DS il valore
245300000000     C* della chiave della VL univoca (se c'è)
245400000000     C                   MOVE      *ON           $ESCI
245500000000     C                   MOVE      *ON           $FINE
245600000000     C*
245700000000     C                   MOVE      *HIVAL        $LASTV
246600000000     C*
246700000000     C* se opz=Annulla/Ripristina
246800000000     C     S1OPZ         WHENEQ    44
246900130829     C                   EXSR      AnnRipr
247000000000     C*
247100000000     C                   MOVE      *HIVAL        $LASTV
247101150112     C*
247102150112     C* se opz=Dichiar. intenti
247103150112     C     S1OPZ         WHENEQ    49
247104150112     C                   EXSR      DichIntenti
247105150112     C*
247106150112     C                   MOVE      *HIVAL        $LASTV
247200000000     C*
247300000000     C* se selezionata opzione personalizzata, richiamo il programma
247400000000     C* corrispondente
247500000000     C     $PGO1(PosOpz) Whenne    *Blank
247600141218    >C                   RESET                   SDGSJ74DS
247700141218    >C                   MOVE      xscsoc        SocietaJ74
247800141218    >C                   MOVEL     S1OPZ         OPZJ74
247900141218    >C     CMTJ73        IFEQ      '1'
248000141218    >C                   MOVEL     '1'           CMTJ74
248100000000     C                   ELSE
248200141218    >C                   MOVEL     '0'           CMTJ74
248300000000     C                   ENDIF
248400141218     C* passare nei campi appositi della DS SDGSJ74DS il valore
248500000000     C* della chiave della VL univoca
248600000000     C*
248700141218    >C                   MOVEL     SDGSJ74DS     KPJBU
248800000000     C                   CALL      $PGO1(PosOpz)                        22
248900000000     C                   PARM                    KPJBA
249000000000     C****               PARM                    NOMPGM
249100000000     C*
249200141218    >C                   MOVEL     KPJBU         SDGSJ74DS
249300000000     C* ritorno da PGM
249400141218    >C                   EXSR      RTNJ74
249500000000     C*
249600000000     C* se altre opzioni
249700000000     C* richiamo pgm gestione anagrafica
249800000000     C                   OTHER
249900000000     C*
250000141218     C                   RESET                   SDGSJ74DS
250100000000     C*
250200141218     C                   MOVEL     S1OPZ         OPZJ74
250300141218     C     CMTJ73        IFEQ      '1'
250400141218     C                   MOVEL     '1'           CMTJ74
250500000000     C                   ELSE
250600141218     C                   MOVEL     '0'           CMTJ74
250700000000     C                   ENDIF
250800141218     C                   Eval      AnnJ74      = D1Ann
250900141218     C                   Eval      TpAnnJ74    = D1TpAnn
251100141218     C                   Eval      TpOpeJ74    = D1TpOpe
251200000000     C*
251300141218     C                   Move      XScSoc        SocietaJ74
251400141218     C                   Eval      AnnoJ74     = D1Anno
252300141218     C                   Eval      PartIvaJ74  = S1PartIva
252400141218     C                   Eval      CdFiscJ74   = S1CdFisc
252500141218     C                   Eval      CogRagJ74   = S1CogRag
252504141223     C                   Eval      DtInterJ74  = H1DtInter
252505141223     C                   Eval      NrInterJ74  = S1NrInter
252506150113     C                   Eval      OrigineJ74  = H1Origine
253400000000     C*
253500141218     C                   CALL      'SDGSJ74R'
253600000000     C                   PARM                    KPJBA
253700141218    >C                   PARM                    SDGSJ74DS
253800000000     C*
253900000000     C*
254000000000     C* ritorno da PGM gestione
254100141218     C                   EXSR      RTNJ74
254200000000     C*
254300000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
254400000000     C     *IN99         IFEQ      *ON
254500000000     C                   SETON                                        40
254600000000     C                   ENDIF
254700000000     C*
254800000000     C                   MOVE      *HIVAL        $LASTV
254900000000     C                   ENDSL
255000000000     C*
255100000000     C                   ENDSR
255200000000     C/EJECT
259800000000     C************************************************************
259900000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
260000000000     C************************************************************
260100141218    >C     RTNJ74        BEGSR
260200000000     C*
260300000000     C* modo di ritorno
260400000000     C*
260500000000     C                   SELECT
260600000000     C* questi modi di utilizzo dei valori di ritorno dal
260700000000     C* pgm di manutenzione rcd di anagrafica sono delle
260800000000     C* proposte, normalmente sempre valide, ma modificabili
260900000000     C* per situazioni particolari
261000000000     C* 1 = F3
261100141218    >C     RETJ74        WHENEQ    '1'
261200000000     C                   MOVE      *ON           $ESCI                          sospese READC
261300000000     C                   MOVE      *ON           $FINE                          fine pgm
261400141218    >C                   MOVE      '1'           RETJ73
261500000000     C* 2 = F12
261600141218    >C     RETJ74        WHENEQ    '2'
261700000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
261800141218     c* questo test, altrimenti prevale il retJ74 e il sfl non viene
261900000000     C* ricaricato
262000141218    >C     OPRJ74        ANDNE     '1'
262100000000     C*
262200000000     C* se l'opzione con cui è stato chiamato non è la modifica
262300000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
262400141218    >C     OPZJ74        IFNE      '02'
262500000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
262503150113     C                   ELSE
262700000000     C                   MOVE      *ON           $ESCI                          sospese READC
262800000000     C                   endif
262900000000     C*
263000000000     C                   ENDSL
263100000000     C*
263200000000     C* operazione eseguite dal pgm chiamato
263300000000     C*
263400000000     C                   SELECT
263500000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
263600141218    >C     OPRJ74        WHENEQ    '1'
263700141218    >C                   MOVE      OPRJ74        WOPR                           avverto pgm chiamant
264300000000     C*
264400000000     C* se l'opzione con cui è stato chiamato non è la modifica
264500000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
264600141218    >C     OPZJ74        IFNE      '02'
264700000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
264800000000     C* se l'opzione con cui è stato chiamato è la modifica
264900000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
265000000000     C                   ELSE
265001150113     C                   EXSR      AGGS1
265200000000     C                   ENDIF
265300000000     C*
265400000000     C                   ENDSL
265500000000     C*
265600000000     C* funzione non eseguibile per errore :
265700000000     C*
265800000000     C                   SELECT
265900000000     C* 1 = funzione richiamata chiusa in errore
266000141218    >C     ERRJ74        WHENEQ    '1'
266100000000     C                   MOVE      *ON           $ESCI                          sospese READC
266200000000     C                   SETON                                        9399      emiss.SFLMSGID
266300000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
266400141218    >C     ERRJ74        WHENEQ    '2'
266500000000     C                   MOVE      *ON           $ESCI                          sospese READC
266600000000     C                   SETON                                        9499      emiss.SFLMSGID
266700000000     C*
266800000000     C                   ENDSL
267600000000     C*
267700000000     C                   ENDSR
267800000000     C/EJECT
292200000000     C************************************************************
292300000000     C* Annulla/Ripristina
292400000000     C************************************************************
292500130829     C     AnnRipr       BEGSR
292600000000     C*
292601141218     C                   Eval      DIESocieta  = XScSoc
292602141218     C                   Eval      DIEDtInter  = H1DtInter
292603141223     C                   Eval      DIENrInter  = S1NrInter
292604141218     C*
292605141218     C     K03DIE01      CHAIN     SDGDIE01L                          2122
292606141218     C                   if        *In21       = *Off
292607141218     C*
292608141218     C                   select
292609141218     C* Annulla
292610141218     C                   when      H1Anna      = *Blank
292611141218     C                   eval      DIEAnn      = '1'
292612141218     C                   Eval      DIETpAnn    = '00'
292613141218     C* Ripristina
292614141218     C                   when      H1Anna      = '1'
292615141223     C                   eval      DIEAnn      = '0'
292616141218     C                   Eval      DIETpAnn    = *Blank
292617141218     C                   endsl
292618141218     C*
292619141218     C                   move      *on           WOPR
292620141218     C                   move      *on           WINZS1
292621141218     C*
292622141218     C                   Eval      DIEDtUlMo   = UDateIso
292623141218     C                   Eval      DIEUteUlMo  = XScCUt
292624141218     C                   Eval      DIEOrigine  = 'M'
292625141218     C*
292626141218     C                   UPDATE    SDGDIE000
292627141218     C*
292628141218     C                   endif
305600000000     C*
305700000000     C                   ENDSR
305800000000     C/EJECT
317700000000     C************************************************************
317800150112     C* Manutenzione anagrafica delle dichiarazioni di intenti
317900000000     C************************************************************
318000150112     C     DichIntenti   BEGSR
318100000000     C*
318101150112     C                   RESET                   ANA311DS
318102150112     C*
318108150112     C                   MOVE      '02'          OPZ311
318128150112     C*
318129150112     C                   MOVE      'S'           FSC311
318131150112     C                   MOVEL     '1'           CMT311
318135150112     C                   MOVE      'F'           COF311
318136150112     C                   MOVE      H1Ksc         KSC311
318137150112     C                   MOVEL     ANA311DS      KPJBU
318138150112     C*
318139150112     C                   CALL      'ANA311R'
318140150112     C                   PARM                    KPJBA
318141150112     C                   PARM                    AUTDS
318142150112     C*
318143150112     C                   MOVEL     KPJBU         ANA311DS
318144150112     C*
318146150112     C                   EXSR      RTN311
318147150112     C*
318148150112     C                   ENDSR
318149150112     C/EJECT
318150150112     C************************************************************
318151150112     C* GESTIONE RITORNO DA PGM DI GESTIONE
318152150112     C************************************************************
318153150112     C     RTN311        BEGSR
318154150112     C*
318155150112     C* modo di ritorno
318156150112     C*
318157150112     C                   SELECT
318158150112     C* questi modi di utilizzo dei valori di ritorno dal
318159150112     C* pgm di manutenzione rcd di anagrafica sono delle
318160150112     C* proposte, normalmente sempre valide, ma modificabili
318161150112     C* per situazioni particolari
318162150112     C* 1 = F3
318163150112     C     RET311        WHENEQ    '1'
318164150112     C                   MOVE      *ON           $ESCI                          sospese READC
318165150112     C                   MOVE      *ON           $FINE                          fine pgm
318166150112     C                   MOVE      '2'           RETJ73
318167150112     C* 2 = F12
318168150112     C     RET311        WHENEQ    '2'
318169150112     C                   MOVE      *ON           $ESCI                          sospese READC
318170150112     C*
318171150112     C                   ENDSL
318172150112     C*
318173150112     C* operazione eseguite dal pgm chiamato
318174150112     C*
318175150112     C                   SELECT
318176150112     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
318177150112     C     OPR311        WHENEQ    '1'
318178150112     c                   move      opr311        wopr
318182150112     C*
318183150112     C                   ENDSL
318184150112     C*
318185150112     C* funzione non eseguibile per errore :
318186150112     C*
318187150112     C                   SELECT
318188150112     C* 1 = funzione richiamata chiusa in errore
318189150112     C     ERR311        WHENEQ    '1'
318190150112     C                   MOVE      *ON           $ESCI                          sospese READC
318191150112     C                   SETON                                        5299      emiss.SFLMSGID
318192150112     C*
318193150112     C                   ENDSL
318197150112     C*
318198150112     C                   ENDSR
318199150112     C/EJECT
318200150112     C************************************************************
318201150112     C* AGGIORNAMENTO RIGA LISTA
318202150112     C************************************************************
318203150112     C     AGGS1         BEGSR
318204150112     C*
318205000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
318300000000     C* di manutenzione dopo una opzione 02
318400000000     C*
318500141218     C                   Eval      S1PartIva   = PartIvAJ74
318600141218     C                   Eval      S1CdFisc    = CdFiscJ74
318601150112     C                   Eval      H1Ksc       = KscJ74
318700141218     C                   Eval      S1CogRag    = CogRagJ74
318701150113     C                   Eval      H1Origine   = OrigineJ74
319600000000     C*
319700000000     C                   ENDSR
319800000000     C/EJECT
319900000000     C************************************************************
320000000000     C* GESTIONE VALORI ASSUNTI
320100000000     C************************************************************
320200000000     C     GESD2         BEGSR
320300000000     C*
320400000000   B1C     $INZD2        IFEQ      *ON
320500000000     C                   EXSR      INZD2
320600000000     C                   MOVE      *OFF          $INZD2
320700000000   E1C                   ENDIF
320800000000     C*
320900000000   B1C     $LASTV        IFNE      'D2'
321000000000     C                   EXSR      GESPIE
321100000000   B1C     $LASTV        IFNE      *HIVAL
321200000000     C                   MOVE      $LASTV        $LASTG
321300000000     C                   ENDIF
321400000000     C                   MOVE      'D2'          $LASTV
321500000000   E1C                   ENDIF
321600000000     C*
321700000000     C* se esistono errori sulla videata
321800000000     C* emetto la write del formato a indicatori spenti per vedere
321900000000     C* le eventuali decodifiche
322000000000   B1C     *IN99         IFEQ      *ON
322100000000     C                   MOVEA     *IN           WIN
322200000000     C                   MOVE      *ALL'0'       IN5098           49
322300000000     C                   MOVEA     IN5098        *IN(50)
322400000000     C                   WRITE     FMTD2
322500000000     C                   MOVEA     WIN           *IN
322600000000   E1C                   ENDIF
322700000000     C*
322800000000     C                   EXFMT     FMTD2
322900000000     C*
323000000000     C* azzero la posizione del cursore
323100000000     C                   Z-ADD     0             H1RIGA
323200000000     C                   Z-ADD     0             H1COLO
323300000000     C*
323400000000     C* CONTROLLO ABILITAZIONE TASTI
323500000000     C* INPUT
323600000000     C* $FC3 = Abilitazioni videata D2
323700000000     C* OUTPUT
323800000000     C* *IN97 + *IN99 Errori
323900000000     C                   MOVEA     $FC3          $FC
324000000000     C                   EXSR      CTRKEY
324100000000     C*
324200000000   B1C     *IN99         IFEQ      *OFF
324300000000     C*
324400000000   B2C                   SELECT
324500000000     C* F3=Fine
324600000000     C     $TASTO        WHENEQ    F03
324700000000     C                   EXSR      F03D2
324800000000     C* F12=Ritorno
324900000000     C     $TASTO        WHENEQ    F12
325000000000     C                   EXSR      F12D2
325100000000     C* F24=Altri tasti
325200000000     C     $TASTO        WHENEQ    F24
325300000000     C                   EXSR      F24D2
325400000000     C*
325500000000     C                   OTHER
325600000000     C                   EXSR      CTRD2
325700000000   B3C     *IN99         IFEQ      *OFF
325800000000     C                   MOVE      $LASTG        $GEST
325900000000   B4C                   SELECT
326000000000     C     $LASTG        WHENEQ    'S1'
326100000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
326200000000   B5C     $ORD          IFNE      D2ORD
326300000000     C                   MOVE      *ON           $INZS1
326400000000     C                   MOVE      *BLANK        W1POS
326500000000   E5C                   ENDIF
326600000000     C     $LASTG        WHENEQ    'D1'
326700000000     C* per ora niente
326800000000   E4C                   ENDSL
326900000000     C* definisco i nuovi valori di default
327000000000     C                   Z-ADD     D2LOP         $LOP
327100000000     C                   Z-ADD     D2ORD         $ORD
327200000000     C*
327300000000     C* aggiorno valori assunti
327400000000     C* se richiesto il salvataggio
327500000000   B4C     D2BSV         IFEQ      1
327600000000     C                   EXSR      AGGMPE
327700000000   E4C                   ENDIF
327800000000     C*
327900000000   E3C                   ENDIF
328000000000     C*
328100000000   E2C                   ENDSL
328200000000     C*
328300000000   E1C                   ENDIF
328400000000     C*
328500000000     C                   ENDSR
328600000000     C/EJECT
328700000000     C************************************************************
328800000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
328900000000     C************************************************************
329000000000     C     INZD2         BEGSR
329100000000     C*
329200000000     C                   CLEAR                   FMTD2
329300000000     C                   EVAL      D2BSVA = Normale
329400000000     C* se sono entrato con chiave univoca
329500000000     C* impedisco il salvataggio valori assunti
329600000000     C     $Unique       IFEQ      *ON
329700000000     C                   EVAL      D2BSVA = Protetto
329800000000     C                   ENDIF
329900000000     C                   MOVEL     $LOP          D2LOP
330000000000     C                   MOVE      $ORD          D2ORD
330100000000     C*
330200000000     C                   ENDSR
330300000000     C/EJECT
330400000000     C************************************************************
330500000000     C* GESTIONE F03 VIDEO D2
330600000000     C************************************************************
330700000000     C     F03D2         BEGSR
330800000000     C*
330900000000     C                   MOVE      *ON           $FINE
331000000000     C* fine programma
331100000000     C                   EXSR      ENDPGM
331200000000     C*
331300000000     C                   ENDSR
331400000000     C/EJECT
331500000000     C************************************************************
331600000000     C* GESTIONE F12 VIDEO D2
331700000000     C************************************************************
331800000000     C     F12D2         BEGSR
331900000000     C*
332000000000     C                   MOVE      $LASTG        $GEST
332100000000     C*
332200000000     C                   ENDSR
332300000000     C/EJECT
332400000000     C************************************************************
332500000000     C* GESTIONE F24 VIDEO D2
332600000000     C************************************************************
332700000000     C     F24D2         BEGSR
332800000000     C*
332900000000     C                   MOVEA     $FX3          $FX
333000000000     C                   MOVEA     $FC3          $FC
333100000000     C                   MOVEA     $FR3          $FR
333200000000     C                   Z-ADD     $ULKD2        $ULK
333300000000     C                   Z-ADD     $D2L01        $L01
333400000000     C                   Z-ADD     $D2L02        $L02
333500000000     C                   EXSR      RIEKEY
333600000000     C                   Z-ADD     $ULK          $ULKD2            3 0
333700000000     C                   MOVEA     $FC           $FC3
333800000000     C                   MOVEL     $KEY1         Z3KE1
333900000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
334000000000     C                   WRITE     FMTZ3
334100000000     C*
334200000000     C                   ENDSR
334300000000     C/EJECT
334400000000     C************************************************************
334500000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
334600000000     C************************************************************
334700000000     C     CTRD2         BEGSR
334800000000     C*
334900000000     C                   SETOFF                                       99
335000000000     C*
335100000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
335200000000     C     D2LOP         IFEQ      0
335300000000     C                   SETON                                          5099
335400000000     C                   ENDIF
335500000000     C*
335600000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
335700000000     C     D2ORD         IFEQ      0
335800000000     C                   SETON                                          5199
335900000000     C                   ENDIF
336000000000     C*
336100000000     C                   ENDSR
336200000000     C/EJECT
336300000000     C************************************************************
336400000000     C* CONTROLLO ABILITAZIONE TASTI
336500000000     C* INPUT
336600000000     C* $TASTO =Tasto funzione premuto
336700000000     C* $FC = Tasti abilitati
336800000000     C* OUTPUT
336900000000     C* IN99 = ERrore
337000000000     C************************************************************
337100000000     C     CTRKEY        BEGSR
337200000000     C*
337300000000     C                   SETOFF                                       99
337400000000     C*
337500000000     C                   SELECT
337600000000     C     $TASTO        WHENEQ    F01
337700000000     C                   z-add     01            posfun
337800000000     C     $FC(1)        IFEQ      '0'
337900000000     C                   SETON                                        9997
338000000000     C                   ENDIF
338100000000     C     $TASTO        WHENEQ    F02
338200000000     C                   z-add     02            posfun
338300000000     C     $FC(2)        IFEQ      '0'
338400000000     C                   SETON                                        9997
338500000000     C                   ENDIF
338600000000     C     $TASTO        WHENEQ    F03
338700000000     C                   z-add     03            posfun
338800000000     C     $FC(3)        IFEQ      '0'
338900000000     C                   SETON                                        9997
339000000000     C                   ENDIF
339100000000     C     $TASTO        WHENEQ    F04
339200000000     C                   z-add     04            posfun
339300000000     C     $FC(4)        IFEQ      '0'
339400000000     C                   SETON                                        9997
339500000000     C                   ENDIF
339600000000     C     $TASTO        WHENEQ    F05
339700000000     C                   z-add     05            posfun
339800000000     C     $FC(5)        IFEQ      '0'
339900000000     C                   SETON                                        9997
340000000000     C                   ENDIF
340100000000     C     $TASTO        WHENEQ    F06
340200000000     C                   z-add     06            posfun
340300000000     C     $FC(6)        IFEQ      '0'
340400000000     C                   SETON                                        9997
340500000000     C                   ENDIF
340600000000     C     $TASTO        WHENEQ    F07
340700000000     C                   z-add     07            posfun
340800000000     C     $FC(7)        IFEQ      '0'
340900000000     C                   SETON                                        9997
341000000000     C                   ENDIF
341100000000     C     $TASTO        WHENEQ    F08
341200000000     C                   z-add     08            posfun
341300000000     C     $FC(8)        IFEQ      '0'
341400000000     C                   SETON                                        9997
341500000000     C                   ENDIF
341600000000     C     $TASTO        WHENEQ    F09
341700000000     C                   z-add     09            posfun
341800000000     C     $FC(09)       IFEQ      '0'
341900000000     C                   SETON                                        9997
342000000000     C                   ENDIF
342100000000     C     $TASTO        WHENEQ    F10
342200000000     C                   z-add     10            posfun
342300000000     C     $FC(10)       IFEQ      '0'
342400000000     C                   SETON                                        9997
342500000000     C                   ENDIF
342600000000     C     $TASTO        WHENEQ    F11
342700000000     C                   z-add     11            posfun
342800000000     C     $FC(11)       IFEQ      '0'
342900000000     C                   SETON                                        9997
343000000000     C                   ENDIF
343100000000     C     $TASTO        WHENEQ    F12
343200000000     C                   z-add     12            posfun
343300000000     C     $FC(12)       IFEQ      '0'
343400000000     C                   SETON                                        9997
343500000000     C                   ENDIF
343600000000     C     $TASTO        WHENEQ    F13
343700000000     C                   z-add     13            posfun
343800000000     C     $FC(13)       IFEQ      '0'
343900000000     C                   SETON                                        9997
344000000000     C                   ENDIF
344100000000     C     $TASTO        WHENEQ    F14
344200000000     C                   z-add     14            posfun
344300000000     C     $FC(14)       IFEQ      '0'
344400000000     C                   SETON                                        9997
344500000000     C                   ENDIF
344600000000     C     $TASTO        WHENEQ    F15
344700000000     C                   z-add     15            posfun
344800000000     C     $FC(15)       IFEQ      '0'
344900000000     C                   SETON                                        9997
345000000000     C                   ENDIF
345100000000     C     $TASTO        WHENEQ    F16
345200000000     C                   z-add     16            posfun
345300000000     C     $FC(16)       IFEQ      '0'
345400000000     C                   SETON                                        9997
345500000000     C                   ENDIF
345600000000     C     $TASTO        WHENEQ    F17
345700000000     C                   z-add     17            posfun
345800000000     C     $FC(17)       IFEQ      '0'
345900000000     C                   SETON                                        9997
346000000000     C                   ENDIF
346100000000     C     $TASTO        WHENEQ    F18
346200000000     C                   z-add     18            posfun
346300000000     C     $FC(18)       IFEQ      '0'
346400000000     C                   SETON                                        9997
346500000000     C                   ENDIF
346600000000     C     $TASTO        WHENEQ    F19
346700000000     C                   z-add     19            posfun
346800000000     C     $FC(19)       IFEQ      '0'
346900000000     C                   SETON                                        9997
347000000000     C                   ENDIF
347100000000     C     $TASTO        WHENEQ    F20
347200000000     C                   z-add     20            posfun
347300000000     C     $FC(20)       IFEQ      '0'
347400000000     C                   SETON                                        9997
347500000000     C                   ENDIF
347600000000     C     $TASTO        WHENEQ    F21
347700000000     C                   z-add     21            posfun
347800000000     C     $FC(21)       IFEQ      '0'
347900000000     C                   SETON                                        9997
348000000000     C                   ENDIF
348100000000     C     $TASTO        WHENEQ    F22
348200000000     C                   z-add     22            posfun
348300000000     C     $FC(22)       IFEQ      '0'
348400000000     C                   SETON                                        9997
348500000000     C                   ENDIF
348600000000     C     $TASTO        WHENEQ    F23
348700000000     C                   z-add     23            posfun
348800000000     C     $FC(23)       IFEQ      '0'
348900000000     C                   SETON                                        9997
349000000000     C                   ENDIF
349100000000     C     $TASTO        WHENEQ    F24
349200000000     C                   z-add     24            posfun
349300000000     C     $FC(24)       IFEQ      '0'
349400000000     C                   SETON                                        9997
349500000000     C                   ENDIF
349600000000     C                   ENDSL
349700000000     C*
349800000000     C                   ENDSR
349900000000     C/EJECT
350000000000     C************************************************************
350100000000     C* AGGIORNAMENTO VALORI ASSUNTI
350200000000     C************************************************************
350300000000     C     AGGMPE        BEGSR
350400000000     C*
350500000000     C* preparo i parametri di chiamata al driver
350600000000     C                   CLEAR                   XMPEDS
350700000000     C                   EVAL      TpChiamata = '03'
350800000000     C                   EVAL      NChiavi = 5
350900000000     C                   EVAL      VLogica = 2
351000000000     C*
351100000000     C* preparo le chiavi per la chiamata al driver
351200000000     C                   CLEAR                   XMPEDS1
351300000000     C                   MOVEL     XSCSOC        DSMPESOC
351400000000     C                   MOVEL     DSPGM         DSMPEPGM
351500000000     C                   MOVEL     KNMUS         DSMPEPRF
351600000000     C*
351700000000     C* Si memorizzano i dati di default a seconda delle modalità con
351800000000     C* cui il pgm è usato
351900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
352000000000     C* MPETIP = 'PV'  pgm richiamato da menù
352100000000     C* MPETIP = 'PB'  x lancio Batch
352200000000     C*
352300000000     C                   SELECT
352400141218    >C     OPZJ73        WHENEQ    '01'
352500000000     C                   MOVEL     'PX'          DSMPETIP
352600000000     C                   OTHER
352700000000     C                   MOVEL     'PV'          DSMPETIP
352800000000     C                   ENDSL
352900000000     C* dati
353000000000     C                   MOVEL     DSMPE         DSMPEPAR
353100000000     C*
353200000000     C                   CALLB     'XMPE'
353300000000     C                   PARM                    XMPEDS
353400000000     C                   PARM                    XMPEDS1
353500000000     C*
353600000000     C                   ENDSR
353700000000     C/EJECT
353800000000     C************************************************************
353900000000     C* OPERAZIONI INIZIALI
354000000000     C************************************************************
354100000000     C     *INZSR        BEGSR
354200000000     C*
354300000000     C* Reperimento parametri
354400000000     C*
354500000000     C     *ENTRY        PLIST
354600000000     C                   PARM                    KPJBA
354700000000     C                   PARM                    DSAUT
354800000000     C*
354900000000     C* testo se sono stati passati i dati di autorizzazione
355000000000     C*
355100000000     C     PARMS         IFLT      2
355200000000     C                   MOVE      *BLANK        AUTDS
355300000000     C                   ELSE
355400000000     C                   MOVEL     DSAUT         AUTDS
355500000000     C                   ENDIF
355600000000     C*
355700000000     C* Reperimento dati società
355800000000     C*
355900000000     C                   MOVEL     'SOC001'      TIPXSC
356000000000     C                   MOVEL     *BLANK        SOCXSC
356100000000     C                   EXSR      REPSOC
356200000000     C*
356300000000     C* Controlli dati società
356400000000     C*
356500000000     C     RTNXSC        IFNE      '1'
356600000000     C                   MOVEL     XSOCDS        SOC001
356700000000     C                   ELSE
356800000000     C                   MOVE      *ON           $FINE
356900141218    >C                   MOVE      '1'           ErrJ73
357000000000     C                   EXSR      ENDPGM
357100000000     C                   ENDIF
357200000000     C*
357300000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
357400000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
357500000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
357600000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
357700000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
357800000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
357900000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
358000000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
358100000000     C                   EVAL      $D2L02 = 0
358200000000     C*
358300000000     C* Reperimento opzioni
358400000000     C*
358500000000     C                   EXSR      REPOPZ
358600000000     C*
358700000000     C* Reperimento tasti di funzione
358800000000     C*
358900000000     C                   EXSR      REPKEY
359000000000     C* Chiamata a routine per il reperimento delle opzioni
359100000000     C* personalizzate
359200000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
359300000000    >C* ripeterla passando le schiere dei tasti funzione
359400000000     C                   CAllb     'X46USREXT'
359500000000     C                   PARM                    $OP1
359600000000     C                   PARM                    $TX1
359700000000     C                   PARM                    $FL1
359800000000     C                   PARM                    $PGO1
359900000000     C                   PARM      $dimOp        NumOpz
360000000000     C                   PARM                    $FX2
360100000000     C                   PARM                    $FC2
360200000000     C                   PARM                    $PGF2
360300000000     C                   PARM                    DsPgm
360400000000     C                   PARM                    xsccut
360500000000     C                   PARM                    xscgrp
360600000000     C*
360700000000     C                   Z-add     1             OpzLun
360800000000     C     *blank        Lookup    $TX1(OpzLun)                           21
360900000000     C                   if        *in21 = *on
361000000000     C                   sub       1             OpzLun
361100000000     C                   else
361200000000     C                   Z-add     $DimOp        OpzLun
361300000000     C                   endif
361400000000     C*
361500000000     C                   ENDSR
361600000000     C/EJECT
361700000000     C************************************************************
361800000000     C* INIZIALIZZAZIONE VARIABILI
361900000000     C************************************************************
362000000000     C     INZVAR        BEGSR
362100000000     C*
362200000000     C* Dati da PGM chiamante
362300000000     C*
362400141218    >C                   MOVEL     KPJBU         SDGSJ73DS
362600000000     C*
362700000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
362800000000     C* reperimento dati società
362900141218    >C                   IF        SocietaJ73 <> xscsoc  and
363000141218    >C                             OpzJ73 <>*blank
363100000000     C                   MOVEL     'SOC001'      TIPXSC
363200141218    >C                   MOVEL     SocietaJ73    SOCXSC
363300000000     C                   EXSR      REPSOC
363400000000     C     RTNXSC        IFNE      '1'
363500000000     C                   MOVEL     XSOCDS        SOC001
363600000000     C                   ELSE
363700000000     C                   MOVE      *ON           $FINE
363800141218    >C                   MOVE      '1'           ErrJ73
363900000000     C                   EXSR      ENDPGM
364000000000     C                   ENDIF
364100000000     C                   ENDIF
364500000000     C*
364600000000     C* Reperimento autorizzazioni
364700000000     C*
364800000000     C* le autorizzazioni potrebbero essere già state passate,
364900000000     C* nel qual caso non vanno reperite
365000141229    >C************       EXSR      REPAUT
365100000000     C*
365200000000     C*
365300000000     C* Reperimento valori default utente
365400000000     C*
365500000000     C                   EXSR      REPDFT
365600000000     C*
365700000000     C* Definizione delle opzioni utilizzabili a pgm
365800000000     C*
365900000000     C                   EXSR      DEFOPZ
366000000000     C*
366100000000     C* Variabili per gestione videate
366200000000     C*
366300000000     C                   MOVE      *OFF          $FINE                          fine pgm
366400000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
366500000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
366600000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
366700000000     C                   MOVE      *OFF          $EndFile                       memo fine file
366800000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
366900000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
367000000000     C                   MOVE      *HIVAL        $LASTV
367100000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
367200000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
367300000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
367400000000     C* azzero la posizione del cursore
367500000000     C                   Z-ADD     0             H1RIGA
367600000000     C                   Z-ADD     0             H1COLO
367700000000     C*
367800000000     C                   MOVE      *ON           $INZD1
367900000000     C                   MOVE      *ON           $INZS1
368000000000     C*
368100000000     C* se pgm richiamato con chiave univoca impostata,
368200000000     C* forzo la emissione della lista e imposto l'ordinamento
368300000000    >C*  in questo esempio la chiave univoca per la posta è Codice
368400000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
368500000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
368600000000    >C*  ricevere pieno)
368700000000     C                   MOVE      *OFF          $Unique
368800000000     C*
368900000000     C* Variabili appoggio
369000000000     C*
369100000000     C                   Z-ADD     SFLPAG        WPAG
369200000000     C                   MOVEL     *OFF          WOPR
369300000000     C                   CLEAR                   W1POS
369400000000     C*
369500000000     C* definisco qual'è la prima videata
369600000000     C* (se la prima videata risulta la lista, devo comunque caricare
369700000000     C*  la videata di parzializzazione e gestire la definizione
369800000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
369900000000     C*  passati dei dati, può variare il caricamento)
370000000000     C                   SELECT
370100000000     C     $LOP          WHENEQ    1
370200000000     C                   MOVE      'D1'          $GEST
370300000000     C     $LOP          WHENEQ    2
370400000000     C                   MOVE      'S1'          $GEST
370500000000     C                   EXSR      INZD1
370600000000     C                   MOVE      *OFF          $INZD1
370700000000     C                   EXSR      DEFORD
370800000000     C                   ENDSL
370900000000     C*
371000000000     C* Valorizzazione campi univoci testate
371100000000     C*
371200000000     C                   CLEAR                   FMTT1
371300000000     C                   MOVEL     KNSIF         NOMSIF
371400000000     C                   MOVEL     XSCDSI        NOMDIT
371500000000     C                   MOVEL     KCDAZ         NOMAZN
371600000000     C****               MOVEL     DSPGM         NOMPGM
371700000000     C* recupero il sottotitolo (se c'è)
371800000000     C                   MOVEL     *BLANK        T1TIT
371900140401     C*
372000140401     C     SOTTIT        IFNE      *BLANK
372100140401     C                   MOVEL     SOTTIT        IDMSG
372200000000     C                   CALLB     'XRTVM'
372300000000     C                   PARM                    IDMSG            27
372400000000     C                   PARM                    TXTMSG          100
372500000000     C                   PARM                    ERRMSG            1
372600000000     C                   PARM      *ON           CTRMSG            1            centratura
372700000000     C                   PARM      30            LENMSG            3 0          lun output.
372800000000     C     ERRMSG        IFNE      *ON
372900000000     C                   MOVEL     TXTMSG        T1TIT
373000000000     C                   ELSE
373100000000     C                   MOVEL     *ALL'?'       T1TIT
373200000000     C                   ENDIF
373300140401     C                   ENDIF
373400000000     C*
373500000000     C                   ENDSR
373600000000     C/EJECT
373700000000     C************************************************************
373800000000     C* REPERIMENTO DATI SOCIETA'
373900000000     C************************************************************
374000000000     C     REPSOC        BEGSR
374100000000     C*
374200000000     C                   CALL      'SDGSOCDR'
374300000000     C                   PARM                    TIPXSC            6
374400000000     C                   PARM                    SOCXSC            3
374500000000     C                   PARM                    CDSXSC            9 0
374600000000     C                   PARM                    MODXSC            3
374700000000     C                   PARM      *BLANKS       RTNXSC            1
374800000000     C                   PARM                    XSOCDS
374900000000     C                   PARM                    KPJBA
375000000000     C*
375100000000     C                   ENDSR
375200000000     C/EJECT
382100000000     C************************************************************
382200000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
382300000000     C************************************************************
382400000000     C     REPAUT        BEGSR
382500000000     C*
382600000000     C                   CLEAR                   AUTDS
382700141229     C*********          Clear                   ConCivDs
382800000000     C*
382900000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
383000000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
383100000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
383200000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
383300000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
383400000000     C                   MOVEL     *ZERO         XCAVFU
383500000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
383600000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
383700000000     C*
383800000000     C                   CALLB     'XCHKAUT'
383900000000     C                   PARM                    AUTDS
384000000000     C*
384100000000     C* errori/non abilitazione
384200000000     C     XCARTN        IFNE      0
384300000000     C*
384400000000     C                   ELSE
384500141229    >C*********          MOVEL     XCADDS        CONCIVDS
384600000000     C                   ENDIF
384700000000     C*
384800000000     C                   ENDSR
384900000000     C/EJECT
385000000000     C************************************************************
385100000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
385200000000     C************************************************************
385300000000     C     REPDFT        BEGSR
385400000000     C*
385500000000     C* preparo i parametri di chiamata al driver
385600000000     C                   CLEAR                   XMPEDS
385700000000     C                   EVAL      TpChiamata = '01'
385800000000     C                   EVAL      NChiavi = 5
385900000000     C                   EVAL      VLogica = 2
386000000000     C*
386100000000     C* preparo le chiavi per la chiamata al driver
386200000000     C                   CLEAR                   XMPEDS1
386300000000     C                   MOVEL     XSCSOC        DSMPESOC
386400000000     C                   MOVEL     DSPGM         DSMPEPGM
386500000000     C                   MOVEL     KNMUS         DSMPEPRF
386600000000     C* Si reperiscono i dati di default a seconda delle modalità con
386700000000     C* cui il pgm è usato
386800000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
386900000000     C* MPETIP = 'PV'  pgm richiamato da menù
387000000000     C* MPETIP = 'PB'  x lancio Batch
387100000000     C                   SELECT
387200141218    >C     OPZJ73        WHENEQ    '01'
387300000000     C                   MOVEL     'PX'          DSMPETIP
387400000000     C                   OTHER
387500000000     C                   MOVEL     'PV'          DSMPETIP
387600000000     C                   ENDSL
387700000000     C*
387800000000     C                   CALLB     'XMPE'
387900000000     C                   PARM                    XMPEDS
388000000000     C                   PARM                    XMPEDS1
388100000000     C*
388200000000     C     Risultato     IFEQ      '0'
388300000000     C                   MOVEL     DSMPEPAR      DSMPE
388400000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
388500000000     C* i suoi valori di default vengono decisi a pgm
388600000000     C                   ELSE
388700000000     C                   Z-ADD     1             $LOP
388800000000     C                   Z-ADD     1             $ORD
388900000000     C                   ENDIF
389000000000     C*
389100000000     C                   ENDSR
389200000000     C/EJECT
389300000000     C************************************************************
389400000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
389500000000     C************************************************************
389600000000     C     DEFOPZ        BEGSR
389700000000     C*
389800000000     C* Questa routine deve valorizzare la schiera $FL1
389900000000     C* con S o N a seconda della utilizzabilità o meno
390000000000     C* dell'opzione correlata
390100000000     C*
390200000000     C* Inizialmente si considerano tutte le opzioni come valide
390300000000     C     1             DO        $DIMOP        X
390400000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
390500000000     C                   If        $TX1(X)  <> *Blank
390600000000     C                   MOVE      'S'           $FL1(X)
390700000000     C                   Else
390800000000     C                   Leave
390900000000     C                   EndIf
391000000000     C                   ENDDO
391100000000     C*
391200000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
391300000000     C* con 1 o 0 a seconda della utilizzabilità o meno
391400000000     C* del tasto funzione correlato
391500000000     C*
391600000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
391700000000     C* di compilazione
391800000000     C                   RESET                   $FC1
391900000000     C                   RESET                   $FR1
392000000000     C                   RESET                   $FC2
392100000000     C                   RESET                   $FR2
392200000000     C                   RESET                   $FC3
392300000000     C                   RESET                   $FR3
392400000000     C*
392500000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
392600000000     C* invalido alcune opzioni
392700000000     C                   SELECT
392800000000     C* Se pgm richiamato in ricerca disattivo :
392900141218    >C     OPZJ73        WHENEQ    '01'
393000000000     C* F3=Fine
393100000000     C                   Z-ADD     3             X
393200000000     C                   MOVE      '0'           $FC1(X)
393300000000     C                   MOVE      '0'           $FC2(X)
393400000000     C                   MOVE      '0'           $FC3(X)
393500000000     C*
393600000000     C*
393700000000     C* Se pgm richiamato in interrogazione disattivo :
393800141218    >C     OPZJ73        WHENEQ    '05'
393900000000     C* F10=Immissione
394000000000     C                   Z-ADD     10            X
394100000000     C                   MOVE      '0'           $FC1(X)
394200000000     C                   MOVE      '0'           $FC2(X)
394300000000     C* 1=Selezione
394400000000     C                   Z-ADD     1             X
394500000000     C     01            LOOKUP    $OP1(X)                                21
394600000000     C     *IN21         IFEQ      *ON
394700000000     C                   MOVE      'N'           $FL1(X)
394800000000     C                   ENDIF
394900000000     C* 2=Modifica
395000000000     C                   Z-ADD     1             X
395100000000     C     02            LOOKUP    $OP1(X)                                21
395200000000     C     *IN21         IFEQ      *ON
395300000000     C                   MOVE      'N'           $FL1(X)
395400000000     C                   ENDIF
395500000000     C* 3=Copia
395600000000     C                   Z-ADD     1             X
395700000000     C     03            LOOKUP    $OP1(X)                                21
395800000000     C     *IN21         IFEQ      *ON
395900000000     C                   MOVE      'N'           $FL1(X)
396000000000     C                   ENDIF
396100000000     C* 4=Annullamento
396200000000     C                   Z-ADD     1             X
396300000000     C     04            LOOKUP    $OP1(X)                                21
396400000000     C     *IN21         IFEQ      *ON
396500000000     C                   MOVE      'N'           $FL1(X)
396600000000     C                   ENDIF
396700000000     C*
396800141218    >C     OPZJ73        WHENEQ    *BLANKS
396900000000     C* Se pgm non richiamato, disattivo subito :
397000000000     C* F8=Selezione seguente
397100000000     C                   Z-ADD     8             X
397200000000     C                   MOVE      '0'           $FC1(X)
397300000000     C                   MOVE      '0'           $FC2(X)
397400000000     C* 1=Selezione
397500000000     C                   Z-ADD     1             X
397600000000     C     01            LOOKUP    $OP1(X)                                21
397700000000     C     *IN21         IFEQ      *ON
397800000000     C                   MOVE      'N'           $FL1(X)
397900000000     C                   ENDIF
398000000000     C*
398100000000     C* In tutti gli altri casi, disattivo subito :
398200000000     C                   OTHER
398300000000     C* 1=Selezione
398400000000     C                   Z-ADD     1             X
398500000000     C     01            LOOKUP    $OP1(X)                                21
398600000000     C     *IN21         IFEQ      *ON
398700000000     C                   MOVE      'N'           $FL1(X)
398800000000     C                   ENDIF
398900000000     C*
399000000000     C                   ENDSL
403500000000     C*
403600000000     C                   ENDSR
403700000000     C/EJECT
403800000000     C************************************************************
403900000000     C* REPERIMENTO OPZIONI
404000000000     C************************************************************
404100000000     C     REPOPZ        BEGSR
404200000000     C*
404300000000     C     1             DO        $DIMOP        X
404400000000     C*
404500000000     C     $MS1(X)       IFNE      *BLANK
404600000000     C*
404700000000     C                   MOVEL     $MS1(X)       IDMSG
404800000000     C                   CALLB     'XRTVM'
404900000000     C                   PARM                    IDMSG            27
405000000000     C                   PARM                    TXTMSG          100
405100000000     C                   PARM                    ERRMSG            1
405200000000     C*
405300000000     C     ERRMSG        IFNE      *ON
405400000000     C                   MOVEL     TXTMSG        $TX1(X)
405500000000     C                   ELSE
405600000000     C                   MOVEL     *ALL'?'       $TX1(X)
405700000000     C                   ENDIF
405800000000     C*
405900000000     C                   ENDIF
406000000000     C*
406100000000     C                   ENDDO
406200000000     C*
406300000000     C                   ENDSR
406400000000     C/EJECT
406500000000     C************************************************************
406600000000     C* REPERIMENTO TASTI DI FUNZIONE
406700000000     C************************************************************
406800000000     C     REPKEY        BEGSR
406900000000     C                   MOVEA     $FM1          $FM
407000000000     C                   MOVEA     $FC1          $FC
407100000000     C                   EXSR      REPKKK
407200000000     C                   MOVEA     $FX           $FX1
407300000000     C*
407400000000     C                   MOVEA     $FM2          $FM
407500000000     C                   MOVEA     $FC2          $FC
407600000000     C                   EXSR      REPKKK
407700000000     C                   MOVEA     $FX           $FX2
407800000000     C*
407900000000     C                   MOVEA     $FM3          $FM
408000000000     C                   MOVEA     $FC3          $FC
408100000000     C                   EXSR      REPKKK
408200000000     C                   MOVEA     $FX           $FX3
408300000000     C                   ENDSR
408400000000     C/EJECT
408500000000     C************************************************************
408600000000     C* REPERIMENTO TASTI DI FUNZIONE
408700000000     C************************************************************
408800000000     C     REPKKK        BEGSR
408900000000     C*
409000000000     C                   CLEAR                   $FX
409100000000     C*
409200000000     C     1             DO        24            X
409300000000     C*
409400000000     C     $FM(X)        IFNE      *BLANK
409500000000     C     $Fc(X)        andne     '0'
409600000000     C*
409700000000     C                   MOVEL     $FM(X)        IDMSG
409800000000     C                   CALLB     'XRTVM'
409900000000     C                   PARM                    IDMSG            27
410000000000     C                   PARM                    TXTMSG          100
410100000000     C                   PARM                    ERRMSG            1
410200000000     C*
410300000000     C     ERRMSG        IFNE      *ON
410400000000     C                   MOVEL     TXTMSG        $FX(X)
410500000000     C                   ELSE
410600000000     C                   MOVEL     *ALL' '       $FX(X)
410700000000     C                   ENDIF
410800000000     C*
410900000000     C                   ENDIF
411000000000     C*
411100000000     C                   ENDDO
411200000000     C*
411300000000     C                   ENDSR
411400000000     C/EJECT
411500000000     C************************************************************
411600000000     C* DEFINIZIONE KLIST
411700000000     C************************************************************
411800000000     C     DefKlist      BEGSR
411900000000     C*
412000000000     C* klist
412001141218     C*
412002141218     C* SDGDIE01L
412003141218     C     K03DIE01      KLIST
412004141218     C                   KFLD                    DIESocieta
412005141218     C                   KFLD                    DIEDtInter
412006141218     C                   KFLD                    DIENrInter
412007141218     C*
412008141218     C* SDGDIE02L
412009141218     C     K03DIE02      KLIST
412010141218     C                   KFLD                    DIESocieta
412011141218     C                   KFLD                    DIEAnno
412012141218     C                   KFLD                    DIECogRag
412100000000     C*
412101141218     C     K02DIE02      KLIST
412102141218     C                   KFLD                    DIESocieta
412103141218     C                   KFLD                    DIEAnno
412105141218     C*
412200000000     C                   ENDSR
412300000000     C/EJECT
412400000000** $MS1 / $TX1
412500000000PROMSG    *LIBL     OPZ0001
412600141218PROMSG    *LIBL     OPZ0002
412601141223PROMSG    *LIBL     OPZ0003
412602150112PROMSG    *LIBL     OPZ0004
412700130828PROMSG    *LIBL     OPZ0005
413000000000SDGMSG    *LIBL     SDO0024
413001150112PROMSG    *LIBL     OPZ0122
413100000000** $OP1 / $FL1
41320000000001S      <Selezione
41330014121802S      <Modifica
41330114122303S      <Copia
41330215011204S      <Cancella
41340013082805S      <Visualizzazione
41370000000044S      <Annulla/Ripristina
41370115011249S      <Dichiar. intenti
413800000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
413900000000PROMSG    *LIBL     KEY0001  01
414000000000PROMSG    *LIBL     KEY0002  02
414100000000PROMSG    *LIBL     KEY0003  03
414200000000PROMSG    *LIBL     KEY0004  04
414300000000PROMSG    *LIBL     KEY0005  05
414400000000PROMSG    *LIBL     KEY0006  06
414500000000PROMSG    *LIBL     KEY0007  07
414600000000PROMSG    *LIBL     KEY0008  08
414700000000PROMSG    *LIBL     KEY0009  09
414800000000PROMSG    *LIBL     KEY0010  10
414900000000PROMSG    *LIBL     KEY0011  11
415000000000PROMSG    *LIBL     KEY0012  12
415100000000PROMSG    *LIBL     KEY0013  13
415200000000PROMSG    *LIBL     KEY0014  14
415300000000SDGMSG    *LIBL     SDK0003  15
415400000000PROMSG    *LIBL     KEY0016  16
415500000000SDGMSG    *LIBL     SDK0001  17
415600000000PROMSG    *LIBL     KEY0018  18
415700000000PROMSG    *LIBL     KEY0019  19
415800000000PROMSG    *LIBL     KEY0020  20
415900000000PROMSG    *LIBL     KEY0021  21
416000000000PROMSG    *LIBL     KEY0022  22
416100000000PROMSG    *LIBL     KEY0023  23
416200000000PROMSG    *LIBL     KEY0024  24
416300000000**  ABILITAZIONE D1 : $FC1 / $FR1
4164000000000N  01
4165000000001N  02
4166000000001N  03
4167000000001N  04
4168000000000N  05
4169000000000N  06
4170000000000N  07
4171000000001N  08
4172000000000N  09
4173000000000N  10
4174000000000N  11
4175000000001N  12
4176000000000N  13
4177000000000N  14
4178000000000N  15
4179000000000N  16
4180000000000N  17
4181000000000N  18
4182000000000N  19
4183000000000N  20
4184000000000N  21
4185000000000N  22
4186000000000N  23   Impostare a '1' se si utilizzano opzioni
4187000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
418800000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
418900000000PROMSG    *LIBL     KEY0001  01
419000000000PROMSG    *LIBL     KEY0002  02
419100000000PROMSG    *LIBL     KEY0003  03
419200000000PROMSG    *LIBL     KEY0004  04
419300110823SDGMSG    *LIBL     SDK0013  05
419400000000PROMSG    *LIBL     KEY0006  06
419500000000PROMSG    *LIBL     KEY0007  07
419600000000PROMSG    *LIBL     KEY0008  08
419700000000PROMSG    *LIBL     KEY0009  09
419800000000PROMSG    *LIBL     KEY0010  10
419900110826SDGMSG    *LIBL     SDK0014  11
420000000000PROMSG    *LIBL     KEY0012  12
420100000000PROMSG    *LIBL     KEY0013  13
420200000000PROMSG    *LIBL     KEY0014  14
420300000000PROMSG    *LIBL     KEY0015  15
420400000000PROMSG    *LIBL     KEY0016  16
420500000000SDGMSG    *LIBL     SDK0001  17
420600000000PROMSG    *LIBL     KEY0018  18
420700000000PROMSG    *LIBL     KEY0019  19
420800000000PROMSG    *LIBL     KEY0020  20
420900000000PROMSG    *LIBL     KEY0021  21
421000000000PROMSG    *LIBL     KEY0022  22
421100000000PROMSG    *LIBL     KEY0023  23
421200000000PROMSG    *LIBL     KEY0024  24
421300000000**  ABILITAZIONE S1 : $FC2 / $FR2
4214000000000N  01
4215000000000N  02
4216000000001N  03
4217000000000N  04
4218001309120N  05
4219000000000N  06
4220000000000N  07
4221000000001N  08
4222000000000N  09
4223000000001N  10
4224001309110N  11
4225000000001N  12
4226000000000N  13
4227000000000N  14
4228000000000N  15
4229000000000N  16
4230000000001N  17
4231000000000N  18
4232000000000N  19
4233000000000N  20
4234000000000N  21
4235000000000N  22
4236000000001N  23   Impostare a '1' se si utilizzano opzioni
4237000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
423800000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
423900000000PROMSG    *LIBL     KEY0001  01
424000000000PROMSG    *LIBL     KEY0002  02
424100000000PROMSG    *LIBL     KEY0003  03
424200000000PROMSG    *LIBL     KEY0004  04
424300000000PROMSG    *LIBL     KEY0005  05
424400000000PROMSG    *LIBL     KEY0006  06
424500000000PROMSG    *LIBL     KEY0007  07
424600000000PROMSG    *LIBL     KEY0008  08
424700000000PROMSG    *LIBL     KEY0009  09
424800000000PROMSG    *LIBL     KEY0010  10
424900000000PROMSG    *LIBL     KEY0011  11
425000000000PROMSG    *LIBL     KEY0012  12
425100000000PROMSG    *LIBL     KEY0013  13
425200000000PROMSG    *LIBL     KEY0014  14
425300000000PROMSG    *LIBL     KEY0015  15
425400000000PROMSG    *LIBL     KEY0016  16
425500000000SDGMSG    *LIBL     SDK0001  17
425600000000PROMSG    *LIBL     KEY0018  18
425700000000PROMSG    *LIBL     KEY0019  19
425800000000PROMSG    *LIBL     KEY0020  20
425900000000PROMSG    *LIBL     KEY0021  21
426000000000PROMSG    *LIBL     KEY0022  22
426100000000PROMSG    *LIBL     KEY0023  23
426200000000PROMSG    *LIBL     KEY0024  24
426300000000**  ABILITAZIONE D3 : $FC3 / $FR3
4264000000000N  01
4265000000000N  02
4266000000001N  03
4267000000000N  04
4268000000000N  05
4269000000000N  06
4270000000000N  07
4271000000000N  08
4272000000000N  09
4273000000000N  10
4274000000000N  11
4275000000001N  12
4276000000000N  13
4277000000000N  14
4278000000000N  15
4279000000000N  16
4280000000000N  17
4281000000000N  18
4282000000000N  19
4283000000000N  20
4284000000000N  21
4285000000000N  22
4286000000000N  23   Impostare a '1' se si utilizzano opzioni
4287000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
