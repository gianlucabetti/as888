000100100215     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200100215     H*PARMS CVTOPT(*NONE)
000300100215     H DECEDIT('0,') DATEDIT(*DMY.)
000400100215     F*----------------------------------------------------*
000500100215      *
000600100902      *           Associazioni libri/riferimenti IVA
000700100902      *          escluse dalla comunicazione Black List
000800100902      *          --------------------------------------
000900100215      *
001000100215     F*----------------------------------------------------*
001100100215     F*------------
001200100902     FSDGBLA01L UF A E           K DISK
001300100902     F*------------
001400100902     F* SOC/TPR/LIB/ALI/RIF  omit ANN=1
001500100902     FANALR02L  IF   E           K DISK
001600100902     F*------------
001700100902     FANDLI01L  IF   E           K DISK
001800100902     F*-------------
001900100902     FANRIV01L  IF   E           K DISK
002000100215     F*------------
002100100902     FSDGSD17D  CF   E             WORKSTN
002200100902     F                                     RENAME(SD17D1:FMTD1)
002300100902     F                                     RENAME(SD17D2:FMTD2)
002400100902     F                                     RENAME(SD17D3:FMTD3)
002500100902     F                                     RENAME(SD17S1:FMTS1)
002600100902     F                                     RENAME(SD17C1:FMTC1)
002700100902     F                                     RENAME(SD17T1:FMTT1)
002800100902     F                                     RENAME(SD17Z1:FMTZ1)
002900100902     F                                     RENAME(SD17Z2:FMTZ2)
003000100902     F                                     RENAME(SD17Z3:FMTZ3)
003100100215     F                                     SFILE(FMTS1:S1NRR)
003200100215     F                                     INFDS(DSFMT)
003300100215     D*----------------------------------------------------*
003400100215     D* Schiere per la gestione delle opzioni
003500100215     D*
003600100215     D* dimensione della schiera $MS1, da non cambiare
003700100215     D $DIMOP          C                   CONST(30)
003800100215     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003900100215     D $TX1            S             50    DIM($dimop) ALT($MS1)
004000100215     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004100100215     D $FL1            S              1    DIM($dimop) ALT($OP1)
004200100215     D* schiera contenente i nomi dei programma richiamati attraverso
004300100215     D* le opzioni personalizzate
004400100215     D $PGO1           S             10    DIM($dimop)
004500100215     D $TX             S             50    DIM(50)
004600100215     D $FL             S              1    DIM(50)
004700100215     D* Schiere generali per la gestione dei tasti di funzione
004800100215     D $FM             S             27    DIM(24)
004900100215     D $FX             S             50    DIM(24)
005000100215     D $FC             S              1    DIM(24)
005100100215     D $FR             S              1    DIM(24)
005200100215     D* Schiere per la gestione dei tasti di funzione
005300100215     D* Video D1
005400100215     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005500100215     D $FX1            S             50    DIM(24) ALT($FM1)
005600100215     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005700100215     D $FR1            S              1    DIM(24) ALT($FC1)
005800100215     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005900100215     D* funzione personalizzata S1
006000100215     D $PGF2           S             10    DIM(25)
006100100215     D* Schiere per la gestione dei tasti di funzione
006200100215     D* Video S1
006300100215     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006400100215     D $FX2            S             50    DIM(24) ALT($FM2)
006500100215     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006600100215     D $FR2            S              1    DIM(24) ALT($FC2)
006700100215     D* Schiere per la gestione dei tasti di funzione
006800100215     D* Video D2
006900100215     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007000100215     D $FX3            S             50    DIM(24) ALT($FM3)
007100100215     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007200100215     D $FR3            S              1    DIM(24) ALT($FC3)
007300100215     D*
007400100215     D $D1L01          S              3  0
007500100215     D $D1L02          S              3  0
007600100215     D $S1L01          S              3  0
007700100215     D $S1L02          S              3  0
007800100215     D $C1Op1          S              3  0
007900100215     D $C1Op2          S              3  0
008000100215     D $D2L01          S              3  0
008100100215     D $D2L02          S              3  0
008200100215     D*-------------
008300100215     D* Indice della schiera tasti funzione
008400100215     D PosFun          S              2  0
008500100215     D/EJECT
008600100902     D*-------------
008700100215     D* Valori assunti
008800100215     D XMPEDS        E DS                  INZ
008900100215     D XMPEDS1       E DS                  INZ
009000100215     D DSMPE           DS                  INZ
009100100215     D  $LOP                   1      1  0
009200100215     D  $ORD                   2      3  0
009300100215     D*-------------
009400100215     D* Passaggio Parametri
009500100215     D KPJBA         E DS
009600100215     D*-------------
009700100215     D* Reperimento Autorizzazioni
009800100215     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009900100215     D*-------------
010000100215     D* Passaggio Autorizzazioni
010100100215     D DSAUT           DS           512
010200100215     D*-------------
010300100215     D* Maschera dati autorizzazioni
010400100902     D CONCIVDS      E DS
010500100215     D*-------------
010600100215     D* Dati di ambiente ottenuti da SDGSOCDR
010700100215     D SOC001        E DS                  EXTNAME(SDGSOCDS)
010800100215     D*-------------
010900100215     D* DS Interna per dati di output di SDGSOCDR
011000100215     D XSOCDS          DS          1000
011100100215     D*-------------
011200100215     D* Parametri in ricezione
011300100902     D SDGSD17DS     E DS
011400100902     D*-------------
011500100902     D* Ricerca libro iva
011600100902     D ANA062DS      E DS                  INZ
011700100902     D  RET062       E                     INZ('0')
011800100902     D  OPR062       E                     INZ('0')
011900100902     D  ERR062       E                     INZ('0')
012000100902     D  CMT062       E                     INZ('0')
012100100902     D*-------------
012200100902     D* Ricerca riferimenti e aliquote
012300100902     D ANA064DS      E DS                  INZ
012400100902     D  Ret064       E                     Inz('0')
012500100902     D  Opr064       E                     Inz('0')
012600100902     D  Err064       E                     Inz('0')
012700100902     D*-------------
012800100902     D* Ricerche legende
012900100902     D ANA964DS      E DS                  INZ
013000100902     D*-------------
013100100902     D* Controlli validità per valore di una legenda
013200100902     D X07VALDS      E DS                  INZ
013300100215     D*-------------
013400100215     D* campo di salvataggio per la DS d'ingresso durante la chiamata
013500100215     D* alla funzione personalizzata
013600100902     D $SaveDS         S                   like(SDGSD17DS)
013700100215     D*-------------
013800100215     D DSFMT           DS           512
013900100215     D  $TASTO               369    369
014000100215     D  NRG                  370    370
014100100215     D  NCL                  371    371
014200100215     D  SFLNRR               378    379B 0
014300100215     D*-------------
014400100215     D* posizione cursore
014500100215     D CURSOR          DS
014600100215     D  RIRI                   1      2B 0 INZ
014700100215     D  R$$                    2      2
014800100215     D  COCO                   3      4B 0 INZ
014900100215     D  C$$                    4      4
015000100215     D*-------------
015100100215     D* Reperimento nome PGM
015200100215     D STATUS         SDS           333
015300100215     D  DSPGM            *PROC
015400100215     D  PARMS            *PARMS
015500100215     D*-------------
015600100215     D* Costanti
015700100215     D* nr.di righe del sfl (SFLPAG)
015800100215     D* lo imposto = 9999 se F13
015900100215     D SFLPAG          S              4S 0
016000100902     D RIGPAG          C                   CONST(13)
016100100215     D*-------------
016200100215     D* Tasti di funzione
016300100215     D F01             C                   CONST(X'31')
016400100215     D F02             C                   CONST(X'32')
016500100215     D F03             C                   CONST(X'33')
016600100215     D F04             C                   CONST(X'34')
016700100215     D F05             C                   CONST(X'35')
016800100215     D F06             C                   CONST(X'36')
016900100215     D F07             C                   CONST(X'37')
017000100215     D F08             C                   CONST(X'38')
017100100215     D F09             C                   CONST(X'39')
017200100215     D F10             C                   CONST(X'3A')
017300100215     D F11             C                   CONST(X'3B')
017400100215     D F12             C                   CONST(X'3C')
017500100215     D F13             C                   CONST(X'B1')
017600100215     D F14             C                   CONST(X'B2')
017700100215     D F15             C                   CONST(X'B3')
017800100215     D F16             C                   CONST(X'B4')
017900100215     D F17             C                   CONST(X'B5')
018000100215     D F18             C                   CONST(X'B6')
018100100215     D F19             C                   CONST(X'B7')
018200100215     D F20             C                   CONST(X'B8')
018300100215     D F21             C                   CONST(X'B9')
018400100215     D F22             C                   CONST(X'BA')
018500100215     D F23             C                   CONST(X'BB')
018600100215     D F24             C                   CONST(X'BC')
018700100215     D ENTER           C                   CONST(X'F1')
018800100215     D ROLDWN          C                   CONST(X'F4')
018900100215     D ROLLUP          C                   CONST(X'F5')
019000100215     D*-------------
019100100215     D* Attributi video
019200100215     D Normale         C                   Const(X'32')
019300100215     D Protetto        C                   Const(X'A0')
019400100215     D*-------------
019500100215     D* Sottotitolo di testata
019600100215     D SOTTIT          C                   CONST(' ')
019700100902     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
019800100902     D                                          SDC0001')
019900100215     D*-------------
020000100215     D* Variabili per gestione videate
020100100215     D $Gest           S              2A
020200100215     D $Fine           S              1A
020300100215     D $InzD1          S              1A
020400100215     D $InzS1          S              1A
020500100215     D $InzD2          S              1A
020600100215     D $LastG          S              2A
020700100215     D $LastV          S              2A
020800100215     D $EndFile        S              1A
020900100215     D $Esci           S              1A
021000100215     D $RcdOK          S              1A
021100100215     D*-------------
021200100215     D* Variabili appoggio sempre presenti in tutte le anagrafiche
021300100215     D S1Nrr           S                   like(C1Rcd)
021400100215     D WSfl            S                   like(C1Nrr)
021500100215     D WMax            S                   like(C1Rcd)
021600100215     D WPag            S                   like(C1Rcd)
021700100215     D WInzS1          S                   like($InzS1)
021800100902     D WOpr            S                   like(OprD17)
021900100215     D CurR            S              2S 0
022000100215     D CurC            S              2S 0
022100100215     D Win             S             99A
022200100215     D W1Pos           S                   like(C1Pos)
022300100215     D WINKR           S              1A
022400100215     D WExf            S              1A
022500100215     D*-------------
022600100215     D* Variabili appoggio specifiche della singola anagrafica
022700100215     D*  campi per parzializzazioni su rcd (solo alfanumerici)
022800100215     D*  Indici
022900100215     D X               S              3S 0
023000100215     D Y               S              3S 0
023100100215     D* Indice della schiera per le opzioni personalizzate
023200100215     D PosOpz          S              2  0
023300100215     D* numero max opzioni
023400100215     D NumOpz          S              2  0 Inz($DIMOP)
023500100215     D* numero effettive opzioni
023600100215     D OpzLen          S              2  0
023700100215     D*-------------
023800100215     D WPosIni         S              3  0
023900100902     D*-------------
024000100215     D* fase di lettura:
024100100215     D* 1 = anagrafica
024200100215     D* 2 = proposte
024300100902     D $Fase           S              3  0
024400100902     D*---------------------------------
024500100902     D* campi di lavoro
024600100902     D WAliq           S                   Like(RIVAliq   )
024700100902     D WRifIVA         S                   Like(RIVRifIVA )
024800100902     D WRifIVAOk       S              1
024900100902     D TpReg999        S                   Like(BLATpReg  )
025000100902     D Libro999        S                   Like(BLALibro  )
025100100902     D RifIVA999       S                   Like(BLARifIVA )
025200100215     C*----------------------------------------------------*
025300100215     C*                MAIN LINE PROGRAM
025400100215     C*----------------------------------------------------*
025500100215     C*
025600100215     C*
025700100215     C                   EXSR      INZVAR
025800100215     C*
025900100215     C     $FINE         DOWEQ     *OFF
026000100215     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
026100100215     C     $GEST         CASEQ     'S1'          GESS1                          Lista
026200100215     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
026300100215     C                   ENDCS
026400100215     C                   ENDDO
026500100215     C*
026600100215     C                   EXSR      ENDPGM
026700100215     C*
026800100215     C/EJECT
026900100215     C************************************************************
027000100215     C* FINE PROGRAMMA
027100100215     C************************************************************
027200100215     C     ENDPGM        BEGSR
027300100215     C*
027400100215     C* passaggio dati al pgm chiamante
027500100902     C                   MOVEL     WOPR          OPRD17                                     ne S1
027600100902     C                   MOVEL     SDGSD17DS     KPJBU
027700100215     C*
027800100215     C* Se pgm Guida non è stato richiamato posso chiudere in LR
027900100902     C     OPZD17        IFEQ      *BLANK
028000100215     C                   SETON                                            LR
028100100215     C* altrimenti chiudo in RT
028200100215     C                   ELSE
028300100215     C                   SETON                                            RT
028400100215     C                   ENDIF
028500100215     C                   RETURN
028600100215     C*
028700100215     C                   ENDSR
028800100215     C/EJECT
028900100215     C************************************************************
029000100215     C* RICERCHE
029100100215     C************************************************************
029200100215     C     SEARCH        BEGSR
029300100215     C*
029400100215     C* determino Riga/Colonna del cursore
029500100215     C                   MOVE      NRG           R$$
029600100215     C                   MOVE      NCL           C$$
029700100215     C                   Z-ADD     RIRI          CURR
029800100215     C                   Z-ADD     COCO          CURC
029900100215     C*
030000100215     C                   SELECT
030100100215     C*
030200100902     C* FMTD1 - Tipo registro
030300100902     C     H1NMFL        WHENEQ    'D1TPREG'
030400100902     C                   EXSR      F4_TpReg
030500100215     C*
030600100902     C* FMTD1 - Libro
030700100902     C     H1NMFL        WHENEQ    'D1LIBRO  '
030800100902     C                   EXSR      F4_Libro
030900100902     C*
031000100902     C* FMTD1 - Aliquota/Riferimento IVA
031100100902     C                   When      H1NMFL    = 'D1ALIQ   '  or
031200100902     C                             H1NMFL    = 'D1RIFIVA '
031300100902     C                   EXSR      F4_AliqRifIVA
031400100215     C*
031500100215     C* ricerca non abilitata in questa posizione
031600100215     C                   OTHER
031700100215     C                   SETON                                        9899
031800100215     C*
031900100215     C                   ENDSL
032000100215     C*
032100100215     C* imposto pos. del cursore
032200100215     C                   Z-ADD     CURR          H1RIGA
032300100215     C                   Z-ADD     CURC          H1COLO
032400100215     C*
032500100215     C                   MOVE      *HIVAL        $LASTV
032600100215     C*
032700100215     C                   ENDSR
032800100215     C/EJECT
032900100215     C************************************************************
033000100902     C* Ricerca tipo registro
033100100215     C************************************************************
033200100902     C     F4_TpReg      BEGSR
033300100215     C*
033400100902     C                   RESET                   ANA964DS
033500100902     C*
033600100902     C                   MoveL     '01'          OPZ964
033700100902     C                   MoveL     'ANDLI000'    TRC964
033800110302R053 X****               MoveL     D1TpReg       CAM964
033900110302R053 C                   MoveL     'DLITPREG'    CAM964
034000100902     C*
034100100902     C                   MoveL     ANA964DS      KPJBU
034200100902     C                   CALL      'ANA964R'
034300100902     C                   PARM                    KPJBA
034400100902     C                   MoveL     KPJBU         ANA964DS
034500100902     C*
034600100902     C                   if        Ret964     <> '2'
034700100902     C                   MoveL     VAL964        D1TpReg
034800100902     C                   endif
034900100215     C*
035000100215     C                   ENDSR
035100100215     C/EJECT
035200100215     C************************************************************
035300100902     C* Ricerca Libro IVA
035400100215     C************************************************************
035500100902     C     F4_Libro      BEGSR
035600100215     C*
035700100902     C                   RESET                   ANA062DS
035800100902     C*
035900100902     C                   MoveL     '01'          Opz062
036000100902     C                   MOVE      'C'           FSC062
036100100902     C                   MOVE      D1TpReg       TPR062
036200100902     C*
036300100902     C                   MoveL     ANA062DS      KPJBU
036400100902     C                   CALL      'ANA062R'
036500100902     C                   PARM                    KPJBA
036600100902     C                   MoveL     KPJBU         ANA062DS
036700100902     C*
036800100902     C                   if        Err062      = '0'
036900100902     C                   if        Lib062     <> *Blank
037000100902     C                   Eval      D1Libro     = Lib062
037100100902     C                   endif
037200100902     C                   endif
037300100215     C*
037400100215     C                   ENDSR
037500100215     C/EJECT
037600100902     C************************************************************
037700100902     C* Ricerca aliquota/Riferimento IVA
037800100902     C************************************************************
037900100902     C     F4_AliqRifIVA BEGSR
038000100902     C*
038100100902     C                   Reset                   ANA064DS
038200100902     C*
038300100902     C                   Move      '01'          OPZ064
038400100902     C************       Move      XscSoc        SOC064
038500100902     C*
038600100902     C                   Z-Add     D1Aliq        Ali064
038700100902     C                   MoveL     D1RifIVA      RfI064
038800100902     C*
038900100902     C                   MoveL     D1TpReg       TpR064
039000100902     C*
039100100902     C                   MoveL     ANA064DS      KPJBU
039200100902     C*
039300100902     C                   CALL      'ANA064R'
039400100902     C                   PARM                    KPJBA
039500100902     C*
039600100902     C                   MOVEL     KPJBU         ANA064DS
039700100902     C*
039800100902     C                   if        Ali064     <> 0           or
039900100902     C                             RfI064     <> *Blank
040000100902     C*
040100100902     C                   Z-Add     Ali064        D1Aliq
040200100902     C                   MoveL     RfI064        D1RifIVA
040300100902     C*
040400100902     C                   endif
040500100902     C*
040600100902     C                   ENDSR
040700100902     C/EJECT
040800100215     C************************************************************
040900100215     C* RIEMPIMENTO OPZIONI
041000100215     C************************************************************
041100100215     C     RIEOPZ        BEGSR
041200100215     C*
041300100215     C* Input :
041400100215     C* - $TX = schiera con tutti i testi disponibili
041500100215     C* - $FL = schiere con i flag di validità associati ai testi
041600100215     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
041700100215     C*          opzione usato
041800100215     C* - WMXO = variabile contenente il numero massimo di testi
041900100215     C*          opzione usabili
042000100215     C* - $L01 = Lunghezza prima riga
042100100215     C* - $L02 = Lunghezza seconda riga
042200100215     C* Output :
042300100215     C* - WOPZ1 = variabile contenente la prima riga opzioni
042400100215     C* - WOPZ2 = variabile contenente la seconda riga opzioni
042500100215     C* - FF23  = flag che indica se c'è bisogno del tasto F23
042600100215     C*
042700100215     C                   CALLB     'X04OPMSG'
042800100215     C* input
042900100215     C                   PARM                    $TX
043000100215     C                   PARM                    $FL
043100100215     C                   PARM                    WCOZ              3 0
043200100215     C                   PARM                    WMXO              3 0
043300100215     C* output
043400100215     C                   PARM                    WOPZ1            76
043500100215     C                   PARM                    WOPZ2            76
043600100215     C                   PARM                    FF23              1
043700100215     C* input
043800100215     C                   PARM                    $L01              3 0
043900100215     C                   PARM                    $L02              3 0
044000100215     C*
044100100215     C                   PARM                    $ALLOPT         960
044200100215     C*
044300100215     C                   ENDSR
044400100215     C/EJECT
044500100215     C************************************************************
044600100215     C* RIEMPIMENTO TASTI FUNZIONE
044700100215     C************************************************************
044800100215     C     RIEKEY        BEGSR
044900100215     C*
045000100215     C* Input :
045100100215     C* - $FX = schiera con tutti i testi disponibili
045200100215     C* - $FC = schiere con i flag di validità associati ai testi
045300100215     C* - $FR = schiere con i flag di REVERSE IMAGE
045400100215     C* - $ULK = ultimo testo caricato
045500100215     C* - $L01 = Lunghezza prima riga
045600100215     C* - $L02 = Lunghezza seconda riga
045700100215     C* Output :
045800100215     C* - $ULK
045900100215     C* - $KEY1= 1° riga di tasti funzione
046000100215     C* - $KEY2= 2° riga di tasti funzione
046100100215     C*
046200100215     C                   CALLB     'XKEYMSG'
046300100215     C* input
046400100215     C                   PARM                    $FX
046500100215     C                   PARM                    $FC
046600100215     C                   PARM                    $FR
046700100215     C                   PARM                    $ULK              3 0
046800100215     C* output
046900100215     C                   PARM                    $KEY1            79
047000100215     C                   PARM                    $KEY2            79
047100100215     C* input
047200100215     C                   PARM                    $L01              3 0
047300100215     C                   PARM                    $L02              3 0
047400100215     C*
047500100215     C                   PARM                    $ALLFUNCT       480
047600100215     C*
047700100215     C                   ENDSR
047800100215     C/EJECT
047900100215     C************************************************************
048000100215     C* GESTIONE EMISSIONE TASTI FUNZIONE
048100100215     C************************************************************
048200100215     C     GESPIE        BEGSR
048300100215     C*
048400100215     C                   SELECT
048500100215     C* Parzializzazioni
048600100215     C     $GEST         WHENEQ    'D1'
048700100215     C     $ULKD1        IFEQ      0
048800100215     C* riempimento opzioni
048900100215     C                   EXSR      F24D1
049000100215     C                   ELSE
049100100215     C                   WRITE     FMTZ1
049200100215     C                   ENDIF
049300100215     C* Lista
049400100215     C     $GEST         WHENEQ    'S1'
049500100215     C     $ULKS1        IFEQ      0
049600100215     C                   EXSR      F24S1
049700100215     C                   ELSE
049800100215     C                   WRITE     FMTZ2
049900100215     C                   ENDIF
050000100215     C* Modifica valori assunti
050100100215     C     $GEST         WHENEQ    'D2'
050200100215     C     $ULKD2        IFEQ      0
050300100215     C                   EXSR      F24D2
050400100215     C                   ELSE
050500100215     C                   WRITE     FMTZ3
050600100215     C                   ENDIF
050700100215     C                   ENDSL
050800100215     C*
050900100215     C                   ENDSR
051000100215     C/EJECT
051100100215     C************************************************************
051200100215     C* GESTIONE PARZIALIZZAZIONI
051300100215     C************************************************************
051400100215     C     GESD1         BEGSR
051500100902     C*
051600100215     C* inizializzazione videata
051700100215     C     $INZD1        IFEQ      *ON
051800100215     C                   EXSR      INZD1
051900100215     C                   MOVE      *OFF          $INZD1
052000100215     C                   ENDIF
052100100902     C*
052200100215     C* emissione piede videata se proveniente da altra
052300100215     C* salvataggio valore formato di provenienza
052400100215     C     $LASTV        IFNE      'D1'
052500100902     C*
052600100902     C* recupero il sottotitolo (se c'è)
052700100902     C                   Clear                   D1TIT
052800100902     C     SOTTITD1      IFNE      *BLANK
052900100902     C                   MOVEL     SOTTITD1      IDMSG
053000100902     C                   CALLB     'XRTVM'
053100100902     C                   PARM                    IDMSG            27
053200100902     C                   PARM                    TXTMSG          100
053300100902     C                   PARM                    ERRMSG            1
053400100902     C                   PARM      *Off          CTRMSG            1            centratura
053500100902     C                   PARM      15            LENMSG            3 0          lun output.
053600100902     C     ERRMSG        IFNE      *ON
053700100902     C                   MOVEL     TXTMSG        D1TIT
053800100902     C                   ELSE
053900100902     C                   MOVEL     *ALL'?'       D1TIT
054000100902     C                   ENDIF
054100100902     C                   ENDIF
054200100902     C*
054300100215     C* salvo stato F17 per sapere cosa fare in caso di F12
054400100215     C                   MOVE      *INKR         WINKR
054500100215     C                   WRITE     FMTT1
054600100215     C                   EXSR      GESPIE
054700100215     C                   MOVE      $LASTV        $LASTG
054800100215     C                   MOVE      'D1'          $LASTV
054900100215     C                   ENDIF
055000100215     C*
055100100215     C* se esistono errori sulla videata
055200100215     C* emetto la write del formato a indicatori spenti per vedere
055300100215     C* le eventuali decodifiche
055400100215     C     *IN99         IFEQ      *ON
055500100215     C                   MOVEA     *IN           WIN
055600100215     C                   MOVE      *ALL'0'       IN5098           49
055700100215     C                   MOVEA     IN5098        *IN(50)
055800100215     C                   WRITE     FMTD1
055900100215     C                   MOVEA     WIN           *IN
056000100215     C                   ENDIF
056100100215     C*
056200100215     C                   EXFMT     FMTD1
056300100215     C*
056400100215     C* azzero la posizione del cursore
056500100215     C                   Z-ADD     0             H1RIGA
056600100215     C                   Z-ADD     0             H1COLO
056700100215     C*
056800100215     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
056900100215     C* INPUT
057000100215     C* $FC1 = Abilitazioni videata D1
057100100215     C* OUTPUT
057200100215     C* *IN97 + *IN99 Errori
057300100215     C                   MOVEA     $FC1          $FC
057400100215     C                   EXSR      CTRKEY
057500100215     C*
057600100215     C     *IN99         IFEQ      *OFF
057700100215     C*
057800100215     C                   SELECT
057900100215     C* F2=Decodifica
058000100215     C     $TASTO        WHENEQ    F02
058100100215     C                   EXSR      F02D1
058200100215     C* F3=Fine
058300100215     C     $TASTO        WHENEQ    F03
058400100215     C                   EXSR      F03D1
058500100215     C* F4=Ricerca
058600100215     C     $TASTO        WHENEQ    F04
058700100215     C                   EXSR      SEARCH
058800100215     C* F5=Refresh
058900100215     C     $TASTO        WHENEQ    F05
059000100215     C                   EXSR      F05D1
059100100215     C* F8=Successivo
059200100215     C     $TASTO        WHENEQ    F08
059300100215     C                   EXSR      F08D1
059400100215     C* F12=Ritorno
059500100215     C     $TASTO        WHENEQ    F12
059600100215     C                   EXSR      F12D1
059700100215     C* F18=Modifica valori assunti
059800100215     C     $TASTO        WHENEQ    F18
059900100215     C                   EXSR      F18D1
060000100215     C* F24=Altri tasti
060100100215     C     $TASTO        WHENEQ    F24
060200100215     C                   EXSR      F24D1
060300100215     C*
060400100215     C                   OTHER
060500100215     C                   EXSR      CTRD1
060600100215     C     *IN99         IFEQ      *OFF
060700100215     C* definisco l'ordinamento
060800100215     C                   EXSR      DEFORD
060900100215     C* richiamo la lista
061000100215     C                   MOVE      'S1'          $GEST
061100100215     C                   MOVE      *ON           $INZS1
061200100215     C                   MOVE      *BLANK        W1POS
061300100215     C                   ENDIF
061400100215     C*
061500100215     C                   ENDSL
061600100215     C*
061700100215     C                   ENDIF
061800100215     C*
061900100215     C                   ENDSR
062000100215     C/EJECT
062100100215     C************************************************************
062200100215     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
062300100215     C************************************************************
062400100215     C     INZD1         BEGSR
062500100215     C*
062600100215     C                   CLEAR                   FMTD1
062700100215     C*
062800100215     C                   SELECT
062900100215     C*
063000100902     C     OPZD17        WHENNE    *BLANK
063100100902     C                   Eval      D1TpReg     = TpRegD17
063200100902     C                   Eval      D1Libro     = LibroD17
063300100902     C                   Eval      D1Aliq      = AliqD17
063400100902     C                   Eval      D1RifIVA    = RifIVAD17
063500100902     C*
063600100215     C* il pgm non è richiamato
063700100902     C     OPZD17        WHENEQ    *BLANK
063800100215     C                   ENDSL
063900100215     C*
064000100215     C                   ENDSR
064100100215     C/EJECT
064200100215     C************************************************************
064300100215     C* GESTIONE F02 VIDEO D1
064400100215     C************************************************************
064500100215     C     F02D1         BEGSR
064600100215     C*
064700100215     C* per decodificare la videata richiamo la routine dei ctrl
064800100215     C* e poi disattivo gli eventuali messaggi di errore
064900100215     C                   EXSR      CTRD1
065000100215     C*
065100100215     C                   MOVE      *ALL'0'       IN5098
065200100215     C                   MOVEA     IN5098        *IN(50)
065300100215     C*
065400100215     C                   ENDSR
065500100215     C/EJECT
065600100215     C************************************************************
065700100215     C* GESTIONE F03 VIDEO D1
065800100215     C************************************************************
065900100215     C     F03D1         BEGSR
066000100215     C*
066100100215     C                   MOVE      *ON           $FINE
066200100902     C                   MOVE      '1'           RETD17
066300100215     C*
066400100215     C                   EXSR      ENDPGM
066500100215     C*
066600100215     C                   ENDSR
066700100215     C/EJECT
066800100215     C************************************************************
066900100215     C* GESTIONE F05 VIDEO D1
067000100215     C************************************************************
067100100215     C     F05D1         BEGSR
067200100215     C*
067300100215     C                   MOVE      *ON           $INZD1
067400100215     C*
067500100215     C                   ENDSR
067600100215     C/EJECT
067700100215     C************************************************************
067800100215     C* GESTIONE F08 VIDEO D1
067900100215     C************************************************************
068000100215     C     F08D1         BEGSR
068100100215     C*
068200100215     C                   MOVE      *ON           $FINE
068300100902     C                   MOVE      '0'           RETD17
068400100215     C*
068500100215     C                   ENDSR
068600100215     C/EJECT
068700100215     C************************************************************
068800100215     C* GESTIONE F12 VIDEO D1
068900100215     C************************************************************
069000100215     C     F12D1         BEGSR
069100100215     C*
069200100215     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
069300100215     C*  per F12 torno alla lista
069400100215     C     $LOP          IFEQ      2
069500100215     C     WINKR         OREQ      *ON
069600100215     C                   MOVE      'S1'          $GEST
069700100215     C*  altrimenti, per F12 torno a menu
069800100215     C                   ELSE
069900100215     C                   MOVE      *ON           $FINE
070000100902     C                   MOVE      '2'           RETD17
070100100215     C                   ENDIF
070200100215     C*
070300100215     C                   ENDSR
070400100215     C/EJECT
070500100215     C************************************************************
070600100215     C* GESTIONE F18 VIDEO D1
070700100215     C************************************************************
070800100215     C     F18D1         BEGSR
070900100215     C*
071000100215     C                   MOVE      'D2'          $GEST
071100100215     C                   MOVEL     *ON           $INZD2
071200100215     C*
071300100215     C                   ENDSR
071400100215     C/EJECT
071500100215     C************************************************************
071600100215     C* GESTIONE F24 VIDEO D1
071700100215     C* input
071800100215     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
071900100215     C* $FX1 =Testi tasti funzione
072000100215     C* $FC1 = Abilitazioni
072100100215     C* $FR1 = Inversioni di fondo
072200100215     C************************************************************
072300100215     C     F24D1         BEGSR
072400100215     C*
072500100215     C                   MOVEA     $FX1          $FX
072600100215     C                   MOVEA     $FC1          $FC
072700100215     C                   MOVEA     $FR1          $FR
072800100215     C                   Z-ADD     $ULKD1        $ULK
072900100215     C                   Z-ADD     $D1L01        $L01
073000100215     C                   Z-ADD     $D1L02        $L02
073100100215     C                   EXSR      RIEKEY
073200100215     C                   Z-ADD     $ULK          $ULKD1            3 0
073300100215     C                   MOVEA     $FC           $FC1
073400100215     C                   MOVEL     $KEY1         Z1KE1
073500100215     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
073600100215     C                   MOVEL     $KEY2         Z1KE2
073700100215     C                   WRITE     FMTZ1
073800100215     C*
073900100215     C                   ENDSR
074000100215     C/EJECT
074100100215     C************************************************************
074200100215     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
074300100215     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
074400100215     C* 93 94 96 97 98
074500100215     C************************************************************
074600100215     C     CTRD1         BEGSR
074700100215     C*
074800100215     C                   SETOFF                                       99
074900100902     C*
075000100902     C                   EXSR      CtrTpReg
075100100902     C                   EXSR      CtrLibro
075200100902     C                   EXSR      CtrAliq
075300100902     C                   EXSR      CtrRifIVA
075400100215     C*
075500100215     C                   ENDSR
075600100215     C/EJECT
075700100215     C************************************************************
075800100902     C* Controllo il tipo registro
075900100215     C************************************************************
076000100902     C     CtrTpReg      BEGSR
076100100215     C*
076200100902     C                   Clear                   D1TpRegD
076300100902     C*
076400100902     C                   if        D1TpReg    <> *Blank
076500100902     C*
076600100902     C                   Reset                   X07VALDS
076700100902     C*
076800100902     C                   Move      '1'           X07RIC
076900100902     C                   MoveL     'ANDLI000'    X07TRC
077000100902     C                   MoveL     'DLITPREG'    X07CAM
077100100902     C                   MoveL     D1TpReg       X07VAL
077200100902     C*
077300100902     C                   CALLB     'X07VALR'
077400100902     C                   PARM                    X07VALDS
077500100902     C*
077600100902     C                   if        X07Err     <> '0'
077700100902     C***************    SetOn                                          5299
077800100902     C                   else
077900100902     C                   Eval      D1TpRegD    = X07Des
078000100902     C                   endif
078100100902     C*
078200100902     C                   endif
078300100902     C*
078400100902     C                   ENDSR
078500100902     C/EJECT
078600100902     C************************************************************
078700100902     C* Controllo libro IVA
078800100902     C************************************************************
078900100902     C     CtrLibro      BEGSR
079000100902     C*
079100100902     C                   if        D1Libro    <> *Blank
079200100902     C*
079300100902     C                   Eval      DLISocieta  = XScSoc
079400100902     C                   Eval      DLITpReg    = D1TpReg
079500100902     C                   Eval      DLILibro    = D1Libro
079600100902     C     K03DLI01      CHAIN     ANDLI01L                           21
079700100902     C                   if        *In21       = *On
079800100902     C                   else
079900100902     C                   Eval      D1LibroD    = DLIDesBrev
080000100902     C                   endif
080100100902     C*
080200100902     C                   endif
080300100902     C*
080400100902     C                   ENDSR
080500100902     C/EJECT
080600100902     C************************************************************
080700100902     C* Controllo aliquota
080800100902     C************************************************************
080900100902     C     CtrAliq       BEGSR
081000100902     C*
081100100902     C                   Eval      D1AliqD     = *Blank
081200100902     C*
081300100902     C                   if        D1Aliq     <> 0
081400100902     C*
081500100902     C                   Eval      WAliq       = D1Aliq
081600100902     C                   Eval      WRifIVA     = D1RifIVA
081700100902     C*
081800100902     C                   EXSR      CtrANRIV
081900100902     C                   if        WRifIVAOk  <> *On
082000100902     C                   else
082100100902     C                   Eval      D1AliqD     = RIVDesBrev
082200100902     C                   endif
082300100902     C*
082400100902     C                   endif
082500100902     C*
082600100902     C                   ENDSR
082700100902     C/EJECT
082800100902     C************************************************************
082900100902     C* Controllo riferimento IVA
083000100902     C************************************************************
083100100902     C     CtrRifIVA     BEGSR
083200100902     C*
083300100902     C                   Eval      D1RifIVAD   = *Blank
083400100902     C*
083500100902     C                   if        D1RifIVA   <> *Blank
083600100902     C*
083700100902     C                   Eval      WAliq       = D1Aliq
083800100902     C                   Eval      WRifIVA     = D1RifIVA
083900100902     C*
084000100902     C                   EXSR      CtrANRIV
084100100902     C                   if        WRifIVAOk  <> *On
084200100902     C*************      SetOn                                        42
084300100902     C                   else
084400100902     C                   Eval      D1RifIVAD   = RIVDesBrev
084500100902     C                   endif
084600100902     C*
084700100902     C                   endif
084800100902     C*
084900100902     C                   ENDSR
085000100902     C/EJECT
085100100902     C************************************************************
085200100902     C* Controllo aliquota e riferimento IVA
085300100902     C************************************************************
085400100902     C     CtrANRIV      BEGSR
085500100902     C*
085600100902     C                   Eval      WRifIVAOk   = *On
085700100902     C*
085800100902     C                   Eval      RIVSocieta  = XscSoc
085900100902     C                   Eval      RIVTpReg    = D1TpReg
086000100902     C                   Eval      RIVALiq     = WAliq
086100100902     C                   Eval      RIVRifIVA   = WRifIVA
086200100902     C*
086300100902     C     K04RIV01      CHAIN     ANRIV01L                           21
086400100902     C                   if        *In21       = *On
086500100902     C*************      SETON                                        5099
086600100902     C                   Eval      WRifIVAOk   = *Off
086700100902     C                   endif
086800100902     C*
086900100902     C                   ENDSR
087000100902     C/EJECT
087100100902     C************************************************************
087200100902     C* DEFINIZIONE DELL'ORDINAMENTO
087300100902     C************************************************************
087400100902     C     DEFORD        BEGSR
087500100902     C*
087600100215     C*
087700100215     C                   ENDSR
087800100215     C/EJECT
087900100215     C************************************************************
088000100215     C* Chiamata programma con tasto personalizzato
088100100215     C************************************************************
088200100215     C     FPerS1        BEGSR
088300100215     C*
088400100215     C* salvo la DS di ingresso
088500100902     C                   MOVEL     SDGSD17DS     $SaveDS
088600100215     C*
088700100902     C                   MOVEL     SDGSD17DS     KPJBU
088800100215     C                   CALL      $PGF2(PosFun)                        22
088900100215     C                   PARM                    KPJBA
089000100215     C                   PARM                    NOMPGM
089100100215     C*
089200100215     C* ripristino la DS di ingresso
089300100902     C                   MOVEL     $SaveDS       SDGSD17DS
089400100215     C*
089500100215     C                   ENDSR
089600100215     C/EJECT
089700100215     C************************************************************
089800100215     C* GESTIONE LISTA
089900100215     C************************************************************
090000100215     C     GESS1         BEGSR
090100100215     C*
090200100215     C* inizializzazione videata
090300100215     C     $INZS1        IFEQ      *ON
090400100215     C                   EXSR      INZS1
090500100215     C                   MOVE      *OFF          $INZS1
090600100215     C                   ENDIF
090700100215     C* emissione piede videata se proveniente da altra
090800100215     C* salvataggio valore formato di provenienza
090900100215     C     $LASTV        IFNE      'S1'
091000100215     C                   WRITE     FMTT1
091100100215     C                   EXSR      GESPIE
091200100215     C                   MOVE      $LASTV        $LASTG
091300100215     C                   MOVE      'S1'          $LASTV
091400100215     C                   ENDIF
091500100215     C*
091600100215     C     WMAX          IFEQ      0
091700100215     C                   WRITE     FMTD3                                        NO RCD
091800100215     C                   ENDIF
091900100215     C* Inizializzazione indice tasti funzione personalizzati
092000100215     C                   Eval      PosFun = 25
092100100215     C*
092200100215     C                   EXFMT     FMTC1
092300100215     C*
092400100215     C* azzero la posizione del cursore
092500100215     C                   Z-ADD     0             H1RIGA
092600100215     C                   Z-ADD     0             H1COLO
092700100215     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
092800100215     C* di refresh dello sfondo
092900100215     C                   MOVE      '1'           WExf
093000100215     C*
093100100215     C     C1NRR         IFNE      0
093200100215     C                   Z-ADD     C1NRR         WSFL
093300100215     C                   ENDIF
093400100215     C                   IF        SFLNRR > 0
093500100215     C                   Z-ADD     SFLNRR        C1RCD
093600100215     C                   ENDIF
093700100215     C* inizializzazione indice opzioni personalizzate
093800100215     C                   Eval      PosOpz = 0
093900100215     C* CONTROLLO ABILITAZIONE TASTI
094000100215     C* INPUT
094100100215     C* $FC2 = Abilitazioni videata S1
094200100215     C* OUTPUT
094300100215     C* *IN97 + *IN99 Errori
094400100215     C                   MOVEA     $FC2          $FC
094500100215     C                   EXSR      CTRKEY
094600100215     C*
094700100215     C     *IN99         IFEQ      *OFF
094800100215     C*
094900100215     C                   SELECT
095000100215     C* F3=Fine
095100100215     C     $TASTO        WHENEQ    F03
095200100215     C                   EXSR      F03S1
095300100215f13  C* F5=Refresh
095400100215f13  C     $TASTO        WHENEQ    F05
095500100215f13  C                   EXSR      F05S1
095600100215     C* F8=Successivo
095700100215     C     $TASTO        WHENEQ    F08
095800100215     C                   EXSR      F08S1
095900100215     C* F10=Immissione
096000100215     C     $TASTO        WHENEQ    F10
096100100215     C                   EXSR      F10S1
096200100215     C* F12=Ritorno
096300100215     C     $TASTO        WHENEQ    F12
096400100215     C                   EXSR      F12S1
096500100215f13  C* F13=Selezione multipla
096600100215f13  C     $TASTO        WHENEQ    F13
096700100215f13  C                   EXSR      F13S1
096800100215     C* F17=Parzializzazioni
096900100215     C     $TASTO        WHENEQ    F17
097000100215     C                   EXSR      F17S1
097100100215     C* F18=Modifica valori assunti
097200100215     C     $TASTO        WHENEQ    F18
097300100215     C                   EXSR      F18S1
097400100215     C* F23=Altre opzioni
097500100215     C     $TASTO        WHENEQ    F23
097600100215     C                   EXSR      F23S1
097700100215     C* F24=Altri tasti
097800100215     C     $TASTO        WHENEQ    F24
097900100215     C                   EXSR      F24S1
098000100215     C* ROLL-UP
098100100215     C     $TASTO        WHENEQ    ROLLUP
098200100215     C                   EXSR      ROLS1
098300100215     C* Personalizzata
098400100215     C     $PGF2(PosFun) Whenne    *BLANK
098500100215     C                   EXSR      FperS1
098600100215     C*
098700100215     C                   OTHER
098800100215     C* CONTROLLO DATI
098900100215     C                   EXSR      CTRC1
099000100215     C     *IN99         IFEQ      *OFF
099100100215     C                   EXSR      CTRS1
099200100215     C                   ENDIF
099300100215     C*
099400100215     C                   ENDSL
099500100215     C*
099600100215     C                   ENDIF
099700100215     C*
099800100215     C                   ENDSR
099900100215     C/EJECT
100000100215     C************************************************************
100100100215     C* INIZIALIZZAZIONE LISTA
100200100215     C************************************************************
100300100215     C     INZS1         BEGSR
100400100215     C*
100500100215     C* inizializzo il contatore di fase
100600100902     C                   Z-Add     1             $Fase
100700100215     C*
100800100215     C* pulizia SFL
100900100215     C                   SETOFF                                         3031
101000100215     C                   WRITE     FMTC1
101100100215     C                   SETON                                          31
101200100906     C*
101300100906     C* pulizia del sottotitolo
101400100906     C                   Clear                   D1TIT
101500100215     C*
101600100215     C* riempimento campi control
101700100215     C*
101800100215     C*
101900100215     C* riempimento opzioni
102000100215     C                   MOVEA     $TX1          $TX
102100100215     C                   MOVEA     $FL1          $FL
102200100215     C                   Z-ADD     0             WCOZ
102300100215     C                   Z-ADD     0             WMXO
102400100215     C*  equivale al num.di elementi della schiera $TX1
102500100215     C                   Z-add     OpzLen        WMXO
102600100215     C                   Z-ADD     $C1Op1        $L01
102700100215     C                   Z-ADD     $C1Op2        $L02
102800100215     C                   EXSR      RIEOPZ
102900100215     C                   Z-ADD     23            X
103000100215     C                   MOVE      FF23          $FC2(X)
103100100215     C                   MOVEL     WOPZ1         C1OPZ1
103200100215     C                   MOVEL     $ALLOPT       H1ALLOPT
103300100215     C                   MOVEL     WOPZ2         C1OPZ2
103400100215     C                   Z-ADD     WCOZ          C1COZ
103500100215     C*
103600100215     C* CARICAMENTO SFL A PAGINE
103700100215     C*
103800100215     C                   Z-ADD     0             S1NRR
103900100215     C                   Z-ADD     0             WMAX
104000100215     C*
104100100215     C* Posizionamento su file pilota
104200100215     C                   CLEAR                   C1POS
104300100215     C                   EXSR      SETANA
104400100215     C* ripristino i valori del posizionamento
104500100215     C                   MOVE      W1POS         C1POS
104600100215     C*
104700100215     C* Lettura file pilota
104800100215     C                   EXSR      REDANA
104900100215     C*
105000100215     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
105100100215     C                   IF        $LastV = 'D1'
105200100215     C                   Z-ADD     SFLPAG        WPAG
105300100215     C                   ENDIF
105400100215     C                   EXSR      ROLS1
105500100215     C* imposto il caricamento sfl del numero massimo di righe per pagina
105600100215     C                   Z-ADD     SFLPAG        WPAG
105700100215     C*
105800100215     C                   ENDSR
105900100215     C/EJECT
106000100215     C************************************************************
106100100215     C* CARICAMENTO PAGINA LISTA
106200100215     C************************************************************
106300100215     C     ROLS1         BEGSR
106400100215     C*
106500100215     C                   SETOFF                                       32
106600100215     C                   Z-ADD     0             Y
106700100215     C                   Z-ADD     WMAX          S1NRR
106800100215     C*
106900100215     C* Leggo dal file anagrafico per caricare la lista
107000100215     C*
107100100215     C     $EndFile      DOWEQ     *OFF
107200100215     C     Y             ANDLT     WPAG
107300100215     C*
107400100215     C                   EXSR      RIES1
107500100215     C*
107600100215     C                   ADD       1             S1NRR
107700100215     C                   ADD       1             Y
107800100215     C*
107900100215     C                   WRITE     FMTS1
108000100215     C*
108100100215     C                   EXSR      REDANA
108200100215     C*
108300100215     C                   ENDDO
108400100215     C*
108500100215     C                   Z-ADD     S1NRR         WMAX                 30
108600100215     C*
108700100215     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
108800100215     C*
108900100215     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
109000100215     C                   MVR                     RESTO             3 0
109100100215     C     PAGINE        MULT      SFLPAG        C1RCD
109200100215     C     RESTO         IFGT      0
109300100215     C                   ADD       1             C1RCD
109400100215     C                   ELSE
109500100215     C                   SUB       SFLPAG        C1RCD
109600100215     C                   ADD       1             C1RCD
109700100215     C                   ENDIF
109800100215     C*
109900100215     C                   ENDSR
110000100215     C/EJECT
110100100215     C************************************************************
110200100215     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
110300100215     C************************************************************
110400100215     C     SETANA        BEGSR
110500100215     C*
110600100215     C                   SELECT
110700100215     C*
110800100215     C* Ord.1 = chiave ordinamento 1
110900100215     C     $ORD          WHENEQ    1
111000100215     C                   EXSR      SETFIL01
111100100215     C*
111200100215     C                   ENDSL
111300100215     C*
111400100215     C                   ENDSR
111500100215     C/EJECT
111600100215     C************************************************************
111700100215     C* LETTURA RCD ARCHIVIO PILOTA
111800100215     C************************************************************
111900100215     C     REDANA        BEGSR
112000100215     C*
112100100215     C                   MOVEL     *OFF          $EndFile
112200100215     C                   MOVEL     *OFF          $RCDOK
112300100215     C*
112400100215     C     $EndFile      DOUEQ     *ON
112500100215     C     $RCDOK        OREQ      *ON
112600100215     C*
112700100215     C                   SELECT
112800100215     C*
112900100215     C     $ORD          WHENEQ    1
113000100215     C                   EXSR      REDFIL01
113100100215     C                   ENDSL
113200100215     C*
113300100215     C                   MOVE      *IN21         $EndFile
113400100215     C*
113500100215     C     $EndFile      IFEQ      *OFF
113600100215     C                   EXSR      SELANA
113700100215     C                   ENDIF
113800100215     C*
113900100215     C                   ENDDO
114000100215     C*
114100100215     C                   MOVE      $EndFile      *IN33                          SflEnd
114200100215     C*
114300100215     C                   ENDSR
114400100215     C/EJECT
114500100215     C************************************************************
114600100215     C* POSIZIONAMENTO RCD (ORDINAMENTO 01)
114700100215     C************************************************************
114800100215     C     SETFIL01      BEGSR
114900100215     C*
115000100902     C                   Z-Add     1             $Fase
115100100215     C*
115200100902     C                   MoveL     XScSoc        BLASocieta
115300100902     C                   MoveL     D1TpReg       BLATpReg
115400100902     C     K02BLA01      SETLL     SDGBLA01L
115500100215     C*
115600100215     C                   ENDSR
115700100215     C/EJECT
115800100215     C************************************************************
115900100215     C* LETTURA RCD ARCHIVIO PILOTA (ORDINAMENTO 01)
116000100215     C************************************************************
116100100215     C     REDFIL01      BEGSR
116200100215     C*
116300100215     C                   Select
116400100215     C*
116500100215     C*----------------------------------------
116600100215     C* l'ultimo record letto era da anagrafica
116700100215     C*----------------------------------------
116800100902     C     $Fase         WhenEq    1
116900100902     C     XScSoc        READE(N)  SDGBLA01L                              21
117000100902     C* se non ci sono più record, cambio la fase e
117100100215     C* comincio a caricare le proposte
117200100902     C                   if        *In21       = *On
117300100902     C                   Z-Add     2             $Fase
117400100215     C                   EXSR      SetPro01
117500100215     C                   EXSR      RedPro01
117600100902     C                   endif
117700100215     C*
117800100215     C*---------------------------------------
117900100215     C* l'ultimo record letto era una proposta
118000100215     C*---------------------------------------
118100100902     C     $Fase         WhenEq    2
118200100215     C                   EXSR      RedPro01
118300100215     C*
118400100902     C                   endSl
118500100215     C*
118600100215     C                   ENDSR
118700100215     C/EJECT
118800100215     C************************************************************
118900100215     C* POSIZIONAMENTO PROPOSTE (ORDINAMENTO 01)
119000100215     C************************************************************
119100100215     C     SETPRO01      BEGSR
119200100215     C*
119300100902     C                   MoveL     XScSoc        ALRSocieta
119400100902     C                   MoveL     D1TpReg       ALRTpReg
119500100902     C     K02ALR02      SETLL     ANALR02L
119600100215     C*
119700100215     C                   ENDSR
119800100215     C/EJECT
119900100215     C************************************************************
120000100215     C* LETTURA PROPOSTE (ORDINAMENTO 01)
120100100215     C************************************************************
120200100215     C     REDPRO01      BEGSR
120300100215     C*
120400100902     C     XScSoc        READE     ANALR02L                               21
120500100215     C*
120600100215     C                   ENDSR
120700100215     C/EJECT
120800100215     C************************************************************
120900100215     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
121000100215     C************************************************************
121100100215     C     CLCPAG        BEGSR
121200100215     C* Input :
121300100215     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
121400100215     C*          cursore
121500100215     C* - SFLPAG = numero rcd per pagina sfl
121600100215     C* Output :
121700100215     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
121800100215     C*
121900100215     C     WSFL          DIV       SFLPAG        PAGINE            4 0
122000100215     C                   MVR                     RESTO             3 0
122100100215     C     RESTO         IFGT      0
122200100215     C                   ADD       1             PAGINE
122300100215     C                   ENDIF
122400100215     C     PAGINE        MULT      SFLPAG        WPAG
122500100215     C*
122600100215     C                   ENDSR
122700100215     C/EJECT
122800100215     C************************************************************
122900100215     C* SELEZIONI SULLA RIGA LETTA
123000100215     C************************************************************
123100100215     C     SELANA        BEGSR
123200100215     C*
123300100215     C                   MOVEL     *ON           $RCDOK
123400100215     C                   MOVEL     *ALL'9'       ALL9             15
123500100902     C*
123600100902     C     D1TpReg       CAT       ALL9:0        TpReg999
123700100902     C     D1Libro       CAT       ALL9:0        Libro999
123800100902     C     D1RifIVA      CAT       ALL9:0        RifIVA999
123900100215     C*
124000100215     C                   SELECT
124100100215     C*
124200100215     C* lettura da anagrafica
124300100902     C     $Fase         WHENEQ    1
124400100902     C*
124500100902     C                   SELECT
124600100902     C*
124700100902     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
124800100902     C*
124900100902     C                   When      BLATpReg    < D1TpReg    or
125000100902     C                             BLATpReg    > TpReg999
125100100902     C                   MoveL     *Off          $RCDOK
125200100902     C*
125300100902     C                   When      BLALibro    < D1Libro    or
125400100902     C                             BLALibro    > Libro999
125500100902     C                   MoveL     *Off          $RCDOK
125600100902     C*
125700100902     C                   When      BLAAliq    <> D1Aliq     and
125800100902     C                             D1Aliq      > 0
125900100902     C                   MoveL     *Off          $RCDOK
126000100902     C*
126100100902     C                   When      BLARifIVA   < D1RifIVA   or
126200100902     C                             BLARifIVA   > RifIVA999
126300100902     C                   MoveL     *Off          $RCDOK
126400100902     C*
126500100902     C                   ENDSL
126600100215     C*
126700100215     C* lettura proposte
126800100902     C     $Fase         WHENEQ    2
126900100902     C                   Eval      BLASocieta  = ALRSocieta
127000100902     C                   Eval      BLATpReg    = ALRTpReg
127100100902     C                   Eval      BLALibro    = ALRLibro
127200100902     C                   Eval      BLAAliq     = ALRAliq
127300100902     C                   Eval      BLARifIVA   = ALRRifIVA
127400100902     C     K05BLA01      CHAIN(N)  SDGBLA01L                          24
127500100215     C     *IN24         IFEQ      *OFF
127600100215     C                   MOVE      *OFF          $RCDOK
127700100902     C*
127800100902     C                   ELSE
127900100902     C*
128000100902     C                   SELECT
128100100902     C*
128200100902     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
128300100902     C*
128400100902     C                   When      ALRTpReg    < D1TpReg    or
128500100902     C                             ALRTpReg    > TpReg999
128600100902     C                   MoveL     *Off          $RCDOK
128700100902     C*
128800100902     C                   When      ALRLibro    < D1Libro    or
128900100902     C                             ALRLibro    > Libro999
129000100902     C                   MoveL     *Off          $RCDOK
129100100902     C*
129200100902     C                   When      ALRAliq    <> D1Aliq     and
129300100902     C                             D1Aliq      > 0
129400100902     C                   MoveL     *Off          $RCDOK
129500100902     C*
129600100902     C                   When      ALRRifIVA   < D1RifIVA   or
129700100902     C                             ALRRifIVA   > RifIVA999
129800100902     C                   MoveL     *Off          $RCDOK
129900100902     C*
130000100902     C                   ENDSL
130100100215     C*
130200100902     C                   ENDIF
130300100902     C*
130400100215     C                   ENDSL
130500100215     C*
130600100215     C                   ENDSR
130700100215     C/EJECT
130800100215     C************************************************************
130900100215     C* RIEMPIMENTO RIGA LISTA
131000100215     C************************************************************
131100100215     C     RIES1         BEGSR
131200100215     C*
131300100215     C                   CLEAR                   FMTS1
131400100215     C*
131500100215     C* riempio i campi del sfl, compresi gli hidden con i valori
131600100215     C* del rcd letto
131700100215     C                   SELECT
131800100902     C*
131900100902     C     $Fase         WHENEQ    1
132000100902     C                   Eval      S1TpReg     = BLATpReg
132100100902     C                   Eval      S1Libro     = BLALibro
132200100902     C                   Eval      S1Aliq      = BLAAliq
132300100902     C                   Eval      S1RifIVA    = BLARifIVA
132400100215     C                   MOVEL     *Off          H1Temp
132500100902     C*
132600100902     C     $Fase         WHENEQ    2
132700100902     C                   Eval      S1TpReg     = ALRTpReg
132800100902     C                   Eval      S1Libro     = ALRLibro
132900100902     C                   Eval      S1Aliq      = ALRAliq
133000100902     C                   Eval      S1RifIVA    = ALRRifIVA
133100100215     C                   MOVEL     *On           H1Temp
133200100902     C*
133300100215     C                   ENDSL
133400100215     C*
133500100215     C* imposto anche l'opzione perchè se avessi premuto l'F13
133600100215     C* dovrei caricare l'opzione da replicare
133700100215     C                   Z-Add     WOpz          S1Opz
133800100215     C* rec caricato direttamente con un'opz:
133900100215     C* la readc lo deve leggere, quindi attivo il sflnxtchg
134000100215     C                   if        S1Opz      <> 0
134100100215     C                   SetOn                                        32
134200100215     C                   endif
134300100215     C*
134400100215     C* imposto quegli attributi del sfl che sono modificabili
134500100215     C* al richiamo del determinato pgm
134600100215     C                   EXSR      ATRS1
134700100215     C*
134800100215     C                   ENDSR
134900100215     C/EJECT
135000100215     C************************************************************
135100100215     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
135200100215     C************************************************************
135300100215     C     ATRS1         BEGSR
135400100215     C*
135500100215     C* in questa routine bisogna attivare indicatori o campi programma
135600100215     C* che gestiscono gli attributi di uno o più campi di questa videata;
135700100215     C* attenzione : non è questo il posto in cui eseguire controlli,
135800100215     C*  dovrebbero già arrivare dei flag
135900100215     C     H1TEMP        IFEQ      *ON
136000100902     C                   SETOff                                       51
136100100902     C                   ELSE
136200100902     C                   SETOn                                        51
136300100215     C                   ENDIF
136400100215     C*
136500100215     C                   ENDSR
136600100215     C/EJECT
136700100215     C************************************************************
136800100215     C* GESTIONE F08 VIDEO S1
136900100215     C************************************************************
137000100215     C     F08S1         BEGSR
137100100215     C*
137200100215     C                   MOVE      *ON           $FINE
137300100902     C                   MOVE      '0'           RETD17
137400100215     C*
137500100215     C                   ENDSR
137600100215     C/EJECT
137700100215     C************************************************************
137800100215     C* GESTIONE F03 VIDEO S1
137900100215     C************************************************************
138000100215     C     F03S1         BEGSR
138100100215     C*
138200100215     C                   MOVE      *ON           $FINE
138300100902     C                   MOVE      '1'           RETD17
138400100215     C*
138500100215     C                   EXSR      ENDPGM
138600100215     C*
138700100215     C                   ENDSR
138800100215     C/EJECT
138900100215     C************************************************************
139000100215     C* GESTIONE F05 VIDEO S1
139100100215     C************************************************************
139200100215     C     F05S1         BEGSR
139300100215     C*
139400100215     C                   MOVE      *ON           $INZS1
139500100215     C*
139600100215     C                   ENDSR
139700100215     C/EJECT
139800100215     C************************************************************
139900100215     C* GESTIONE F10 VIDEO S1
140000100215     C* AGGIUNTA RECORD
140100100215     C************************************************************
140200100215     C     F10S1         BEGSR
140300100215     C*
140400100215     C*
140500100215     C                   MOVE      *HIVAL        $LASTV
140600100215     C*
140700100215     C                   ENDSR
140800100215     C/EJECT
140900100215     C************************************************************
141000100215     C* GESTIONE F12 VIDEO S1
141100100215     C************************************************************
141200100215     C     F12S1         BEGSR
141300100215     C*
141400100215     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
141500100215     C     $LOP          IFEQ      1
141600100215     C                   MOVE      'D1'          $GEST
141700100215     C*  se la 1a videata è questa, per F12 torno a menu
141800100215     C                   ELSE
141900100215     C                   MOVE      *ON           $FINE
142000100902     C                   MOVE      '2'           RETD17
142100100215     C                   ENDIF
142200100215     C*
142300100215     C                   ENDSR
142400100215     C/EJECT
142500100215f13  C************************************************************
142600100215f13  C* GESTIONE F13 VIDEO S1
142700100215f13  C* E' simile alla funzione F13 del PDM: riporta l'opzione della
142800100215f13  C* riga del sfl su cui è posizionato il cursore dalla riga
142900100215f13  C* successiva fino alla fine del sfl stesso,
143000100215f13  C* senza sovrapporsi alle opzioni preesistenti.
143100100215f13  C************************************************************
143200100215f13  C     F13S1         BEGSR
143300100215f13  C*
143400100215f13  C* reset indicatori DSPATR
143500100215f13  C                   MOVE      *ALL'0'       IN4149            9
143600100215f13  C                   MOVEA     IN4149        *IN(41)
143700100215f13  C*
143800100215f13  C* il controllo lo faccio solo se il sfl NON è vuoto
143900100215f13  C                   if        WMax    <>    0
144000100215f13  C*
144100100215f13  C* determino Riga/Colonna del cursore
144200100215f13  C                   MOVE      NRG           R$$
144300100215f13  C                   MOVE      NCL           C$$
144400100215f13  C                   Z-ADD     RIRI          CURR
144500100215f13  C                   Z-ADD     COCO          CURC
144600100215f13  C*
144700100215f13  C* se sono posizionato sul sfl
144800100902f13 >C     H1NmRc        IFEQ      'SD17S1'
144900100215f13  C*
145000100215f13  C* salvo il numero del record su cui sono posizionato
145100100215f13  C                   Z-Add     C1NrR         WC1Rcd            3 0
145200100215f13  C*
145300100215f13  C* leggo il rcd su cui sono posizionato
145400100215f13  C     C1NrR         CHAIN     FMTS1                              21
145500100215f13  C*
145600100215f13  C                   if        S1Opz       > 0
145700100215f13  C*
145800100215f13  C* Salvo opz e nr relativo in cui si trova l'opz da ripetere
145900100215f13  C                   EXSR      F13SavPos
146000100215f13  C*
146100100215f13  C* Aggiorno le righe caricate fino ad ora sul sfl,
146200100215f13  C* con l'opz. da ripetere (solo se non ne esiste già un'altra)
146300100215f13  C                   EXSR      F13FinoMax
146400100215f13  C*
146500100215f13  C*
146600100215f13  C* Carico tutte le righe possibili con l'opz da ripetere
146700100215f13  C                   Eval      SflPag      = 9999
146800100215f13  C                   Eval      WPag        = 9999
146900100215f13  C                   EXSR      RolS1
147000100215f13  C*
147100100215f13  C* Reimposto sflpag
147200100215f13  C                   Eval      SflPag      = RigPag
147300100215f13  C                   Eval      WPag        = SflPag
147400100215f13  C*
147500100215f13  C* Ripristino la posizione iniziale del record
147600100215f13  C                   Z-Add     WC1Rcd        C1Rcd
147700100215f13  C*
147800100215f13  C* Ripristino il campo di salvataggio dell'opzione per non sporca
147900100215f13  C* re il campo S1Opz in caso di ricaricamento del sfl
148000100215f13  C                   Z-Add     0             WOpz
148100100215f13  C*
148200100215f13  C                   endif
148300100215f13  C*
148400100215f13  C                   endif
148500100215f13  C*
148600100215f13  C                   endif
148700100215f13  C*
148800100215f13  C                   ENDSR
148900100215f13  C/EJECT
149000100215f13  C************************************************************
149100100215f13  C* Salvo opzione e numero relativo in cui si trova l'opz
149200100215f13  C* da ripetere
149300100215f13  C************************************************************
149400100215f13  C     F13SavPos     BEGSR
149500100215f13  C*
149600100215f13  C                   Z-Add     S1Opz         WOpz              2 0
149700100215f13  C                   Z-Add     C1Nrr         $X                3 0
149800100215f13  C*
149900100215f13  C* riaggiorno il rcd con il SflNxtChg attivato
150000100215f13  C                   EXSR      ATRS1
150100100215f13  C                   SetOn                                        32
150200100215f13  C                   UPDATE    FMTS1
150300100215f13  C                   SetOff                                       32
150400100215f13  C*
150500100215f13  C                   ENDSR
150600100215f13  C/EJECT
150700100215f13  C************************************************************
150800100215f13  C* Aggiorno le righe caricate fino ad ora sul sfl,
150900100215f13  C* con l'opz. da ripetere (solo se non ne esiste già un'altra)
151000100215f13  C************************************************************
151100100215f13  C     F13FinoMax    BEGSR
151200100215f13  C*
151300100215f13  C                   ADD       1             $X
151400100215f13  C*
151500100215f13  C                   DoW       $X         <= WMax
151600100215f13  C*
151700100215f13  C     $X            Chain     FmtS1                              21
151800100215f13  C   21              Leave
151900100215f13  C*
152000100215f13  C                   if        S1Opz       = 0
152100100215f13  C                   Z-Add     WOpz          S1Opz
152200100215f13  C                   endif
152300100215f13  C*
152400100215f13  C                   EXSR      ATRS1
152500100215f13  C                   SetOn                                        32
152600100215f13  C                   UPDATE    FMTS1
152700100215f13  C                   SetOff                                       32
152800100215f13  C*
152900100215f13  C                   Add       1             $X
153000100215f13  C                   EndDo
153100100215f13  C*
153200100215f13  C                   ENDSR
153300100215f13  C/EJECT
153400100215     C************************************************************
153500100215     C* GESTIONE F17 VIDEO S1
153600100215     C************************************************************
153700100215     C     F17S1         BEGSR
153800100215     C*
153900100215     C                   MOVE      'D1'          $GEST
154000100215     C*
154100100215     C                   ENDSR
154200100215     C/EJECT
154300100215     C************************************************************
154400100215     C* GESTIONE F18 VIDEO S1
154500100215     C************************************************************
154600100215     C     F18S1         BEGSR
154700100215     C*
154800100215     C                   MOVE      'D2'          $GEST
154900100215     C                   MOVEL     *ON           $INZD2
155000100215     C*
155100100215     C                   ENDSR
155200100215     C/EJECT
155300100215     C************************************************************
155400100215     C* GESTIONE F23 VIDEO S1
155500100215     C************************************************************
155600100215     C     F23S1         BEGSR
155700100215     C*
155800100215     C                   Z-ADD     $C1Op1        $L01
155900100215     C                   Z-ADD     $C1Op2        $L02
156000100215     C                   EXSR      RIEOPZ
156100100215     C                   Z-ADD     23            X
156200100215     C                   MOVE      FF23          $FC2(X)
156300100215     C                   MOVEL     WOPZ1         C1OPZ1
156400100215     C                   MOVEL     $ALLOPT       H1ALLOPT
156500100215     C                   MOVEL     WOPZ2         C1OPZ2
156600100215     C                   Z-ADD     WCOZ          C1COZ
156700100215     C*
156800100215     C                   ENDSR
156900100215     C/EJECT
157000100215     C************************************************************
157100100215     C* GESTIONE F24 VIDEO S1
157200100215     C************************************************************
157300100215     C     F24S1         BEGSR
157400100215     C*
157500100215     C                   MOVEA     $FX2          $FX
157600100215     C                   MOVEA     $FC2          $FC
157700100215     C                   MOVEA     $FR2          $FR
157800100215     C                   Z-ADD     $ULKS1        $ULK
157900100215     C                   Z-ADD     $S1L01        $L01
158000100215     C                   Z-ADD     $S1L02        $L02
158100100215     C                   EXSR      RIEKEY
158200100215     C                   Z-ADD     $ULK          $ULKS1            3 0
158300100215     C                   MOVEA     $FC           $FC2
158400100215     C                   MOVEL     $KEY1         Z2KE1
158500100215     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
158600100215     C                   MOVEL     $KEY2         Z2KE2
158700100215     C                   WRITE     FMTZ2
158800100215     C*
158900100215     C                   ENDSR
159000100215     C/EJECT
159100100215     C************************************************************
159200100215     C* CONTROLLO TESTATA LISTA
159300100215     C************************************************************
159400100215     C     CTRC1         BEGSR
159500100215     C*
159600100215     C                   MOVE      *OFF          *IN99
159700100215     C*
159800100215     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
159900100215     C* L'INIZIALIZZAZIONE LISTA
160000100215     C*
160100100215     C     C1POS         IFNE      W1POS
160200100215     C                   EXSR      POSS1
160300100215     C                   ENDIF
160400100215     C*
160500100215     C                   ENDSR
160600100215     C/EJECT
160700100215     C************************************************************
160800100215     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
160900100215     C************************************************************
161000100215     C     POSS1         BEGSR
161100100215     C*
161200100215     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
161300100215     C*
161400100215   B1C     WMAX          IFGT      0
161500100215     C                   READC     FMTS1                                  21
161600100215   B2C     *IN21         DOWEQ     *OFF
161700100215     C     *IN99         ANDEQ     *OFF
161800100215     C*
161900100215   B3C     S1OPZ         IFNE      0
162000100215     C                   SETON                                        92  99
162100100215   E3C                   ENDIF
162200100215     C* se rilevata opzione attiva, attivo sflnxtchg
162300100215   B3C     *IN99         IFEQ      *ON
162400100215     C                   MOVE      *ON           *IN32
162500100215     C*
162600100215     C* imposto quegli attributi del sfl che sono modificabili
162700100215     C* al richiamo del determinato pgm
162800100215     C                   EXSR      ATRS1
162900100215     C*
163000100215     C                   UPDATE    FMTS1
163100100215   X3C                   ELSE
163200100215     C                   READC     FMTS1                                  21
163300100215   E3C                   ENDIF
163400100215     C*
163500100215   E2C                   ENDDO
163600100215     C*
163700100215   E1C                   ENDIF
163800100215     C*
163900100215     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
164000100215     C* AL VALORE DI POSIZIONAMENTO :
164100100215     C* se questo è maggiore del valore del rispettivo campo di
164200100215     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
164300100215     C* con i valori di parzializzazione.
164400100215     C*
164500100215   B1C     *IN99         IFEQ      *OFF
164600100215     C                   MOVE      *BLANK        W1POS
164700100215     C*
164800100215     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
164900100215     C* è maggiore del relativo valore di parzializzazione, nel qual caso
165000100215     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
165100100215     C* richiedo comunque un ricaricamento del sfl che, se non si è
165200100215     C* verificata la condizione suddetta avverrà con posizionamento a
165300100215     C* partire dal valore di parzializzazione
165400100215     C*
165500100215   B2C                   SELECT
165600100215     C*
165700100215     C     $ORD          WHENEQ    1
165800100215     C*
165900100215   E2C                   ENDSL
166000100215     C*
166100100215     C* richiedo l'inizializzazione sfl
166200100215     C                   MOVE      *ON           $INZS1
166300100215     C*
166400100215   E1C                   ENDIF
166500100215     C*
166600100215     C                   ENDSR
166700100215     C/EJECT
166800100215     C************************************************************
166900100215     C* CONTROLLO OPZIONI LISTA
167000100215     C************************************************************
167100100215     C     CTRS1         BEGSR
167200100215     C*
167300100215     C                   MOVEL     *OFF          $ESCI
167400100215     C                   SETOFF                                       99
167500100215     C                   Z-ADD     0             S1OPZ
167600100215     C*
167700100215     C* Leggo il sfl solo se ci sono rcd
167800100215   B1C     WMAX          IFGT      0
167900100215     C                   READC     FMTS1                                  21
168000100215     C*
168100100215     C* esce se fine sfl o errore che richiede l'uscita
168200100215   B2C     *IN21         DOWEQ     *OFF
168300100215     C     $ESCI         ANDEQ     *OFF
168400100215     C                   Z-ADD     S1NRR         C1RCD
168500100215     C* ctrl su riga
168600100215     C                   EXSR      RECS1
168700100215     C* gestione opzioni
168800100215   B3C     S1OPZ         IFNE      0
168900100215     C     *IN99         ANDEQ     *OFF
169000100215     C                   EXSR      OPZS1
169100100215   E3C                   ENDIF
169200100215     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
169300100215   B3C     *IN99         IFEQ      *ON
169400100215     C                   MOVE      *ON           *IN32
169500100215   E3C                   ENDIF
169600100215     C*
169700100215     C* imposto quegli attributi del sfl che sono modificabili
169800100215     C* al richiamo del determinato pgm
169900100215     C                   EXSR      ATRS1
170000100215     C*
170100100215     C                   Z-ADD     0             S1OPZ
170200100215     C*
170300100215     C                   UPDATE    FMTS1
170400100215     C                   MOVE      *OFF          *IN32
170500100215     C*
170600100215     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
170700100215   B3C     $ESCI         IFEQ      *OFF
170800100215     C                   READC     FMTS1                                  21
170900100215     C* a fine ciclo ripristino il flag richiesta iniz.sfl
171000100215   B4C     *IN21         IFEQ      *ON
171100100215     C                   MOVE      WINZS1        $INZS1
171200100215     C                   MOVE      *off          WINZS1
171300100215     C* se è avvenuta almeno una modifica
171400100215   B5C                   IF        $INZS1 = *on
171500100215     C* calcolo pagina a cui deve posizionarsi
171600100215     C     WSFL          IFGT      0
171700100215     C                   EXSR      CLCPAG
171800100215     C                   ELSE
171900100215     C                   Z-ADD     SFLPAG        WPAG
172000100215     C                   ENDIF
172100100215   E5C                   ENDIF
172200100215   E4C                   ENDIF
172300100215   E3C                   ENDIF
172400100215     C*
172500100215   E2C                   ENDDO
172600100215     C*
172700100215   E1C                   ENDIF
172800100215     C*
172900100215     C                   ENDSR
173000100215     C/EJECT
173100100215     C************************************************************
173200100215     C* CONTROLLO CAMPI I/O RIGA LISTA
173300100215     C************************************************************
173400100215     C     RECS1         BEGSR
173500100215     C*
173600100215     C* reset indicatori DSPATR
173700100215     C                   MOVE      *ALL'0'       IN4049           10
173800100215     C                   MOVEA     IN4049        *IN(40)
173900100215     C*
174000100215     C* Controllo se Utente Autorizzato alla scelta
174100100215     C     S1OPZ         IFNE      0
174200100215     C                   Z-ADD     1             X
174300100215     C     S1OPZ         LOOKUP    $OP1(X)                                21
174400100215     C* mi salvo l'indice dell' opzione
174500100215     C   21              Z-add     X             PosOpz            2 0
174600100215     C     *IN21         IFEQ      *OFF
174700100215     C     $FL1(X)       OREQ      'N'
174800100215     C                   MOVE      *ON           $ESCI
174900100215     C                   SETON                                        409199
175000100215     C                   ENDIF
175100100215     C                   ENDIF
175200100215     C*
175300100215     C                   ENDSR
175400100215     C/EJECT
175500100215     C************************************************************
175600100215     C* GESTIONE OPZIONI LISTA
175700100215     C************************************************************
175800100215     C     OPZS1         BEGSR
175900100215     C*
176000100215     C                   Z-ADD     RIRI          CURR
176100100215     C                   Z-ADD     COCO          CURC
176200100215     C*
176300100215     C                   SELECT
176400100902     C*
176500100902     C* se opz=Escludi/Includi
176600100902     C     S1OPZ         WHENEQ    4
176700100902     C                   Eval      BLASocieta  = XScSoc
176800100902     C                   Eval      BLATpReg    = S1TpReg
176900100902     C                   Eval      BLALibro    = S1Libro
177000100902     C                   Eval      BLAAliq     = S1Aliq
177100100902     C                   Eval      BLARifIVA   = S1RifIVA
177200100902     C                   if        H1Temp      = *On
177300100902     C                   WRITE     SDGBLA000                            22
177400100902     C**********         Eval      H1Temp      = *Off
177500100902     C                   else
177600100902     C     K05BLA01      DELETE    SDGBLA000                            22
177700100902     C**********         Eval      H1Temp      = *On
177800100902     C                   endif
177900100902     C*
178000100902     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
178100100902     C*
178200100902     C                   MOVE      *HIVAL        $LASTV
178300100215     C*
178400100215     C* se selezionata opzione personalizzata, richiamo il programma
178500100215     C* corrispondente
178600100215     C     $PGO1(PosOpz) Whenne    *Blank
178700100215     C*
178800100215     C* se altre opzioni
178900100215     C* richiamo pgm gestione anagrafica
179000100215     C                   OTHER
179100100215     C*
179200100215     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
179300100215     C     *IN99         IFEQ      *ON
179400100215     C                   SETON                                        40
179500100215     C                   ENDIF
179600100215     C*
179700100215     C                   MOVE      *HIVAL        $LASTV
179800100215     C                   ENDSL
179900100215     C*
180000100215     C                   ENDSR
180100100215     C/EJECT
180200100215     C************************************************************
180300100215     C* AGGIORNAMENTO RIGA LISTA
180400100215     C************************************************************
180500100215     C     AGGS1         BEGSR
180600100215     C*
180700100215     C* riempio i campi del sfl che sono stati ripassati dal pgm
180800100215     C* di manutenzione dopo una opzione 02
180900100215     C*
181000100215     C                   ENDSR
181100100215     C/EJECT
181200100215     C************************************************************
181300100215     C* GESTIONE VALORI ASSUNTI
181400100215     C************************************************************
181500100215     C     GESD2         BEGSR
181600100215     C*
181700100215   B1C     $INZD2        IFEQ      *ON
181800100215     C                   EXSR      INZD2
181900100215     C                   MOVE      *OFF          $INZD2
182000100215   E1C                   ENDIF
182100100215     C*
182200100215   B1C     $LASTV        IFNE      'D2'
182300100215     C                   EXSR      GESPIE
182400100215   B1C     $LASTV        IFNE      *HIVAL
182500100215     C                   MOVE      $LASTV        $LASTG
182600100215     C                   ENDIF
182700100215     C                   MOVE      'D2'          $LASTV
182800100215   E1C                   ENDIF
182900100215     C*
183000100215     C* se esistono errori sulla videata
183100100215     C* emetto la write del formato a indicatori spenti per vedere
183200100215     C* le eventuali decodifiche
183300100215   B1C     *IN99         IFEQ      *ON
183400100215     C                   MOVEA     *IN           WIN
183500100215     C                   MOVE      *ALL'0'       IN5098           49
183600100215     C                   MOVEA     IN5098        *IN(50)
183700100215     C                   WRITE     FMTD2
183800100215     C                   MOVEA     WIN           *IN
183900100215   E1C                   ENDIF
184000100215     C*
184100100215     C                   EXFMT     FMTD2
184200100215     C*
184300100215     C* azzero la posizione del cursore
184400100215     C                   Z-ADD     0             H1RIGA
184500100215     C                   Z-ADD     0             H1COLO
184600100215     C*
184700100215     C* CONTROLLO ABILITAZIONE TASTI
184800100215     C* INPUT
184900100215     C* $FC3 = Abilitazioni videata D2
185000100215     C* OUTPUT
185100100215     C* *IN97 + *IN99 Errori
185200100215     C                   MOVEA     $FC3          $FC
185300100215     C                   EXSR      CTRKEY
185400100215     C*
185500100215   B1C     *IN99         IFEQ      *OFF
185600100215     C*
185700100215   B2C                   SELECT
185800100215     C* F3=Fine
185900100215     C     $TASTO        WHENEQ    F03
186000100215     C                   EXSR      F03D2
186100100215     C* F12=Ritorno
186200100215     C     $TASTO        WHENEQ    F12
186300100215     C                   EXSR      F12D2
186400100215     C* F24=Altri tasti
186500100215     C     $TASTO        WHENEQ    F24
186600100215     C                   EXSR      F24D2
186700100215     C*
186800100215     C                   OTHER
186900100215     C                   EXSR      CTRD2
187000100215   B3C     *IN99         IFEQ      *OFF
187100100215     C                   MOVE      $LASTG        $GEST
187200100215   B4C                   SELECT
187300100215     C     $LASTG        WHENEQ    'S1'
187400100215     C* se è stato modificato l'ordinamento di default, ricarico la lista
187500100215   B5C     $ORD          IFNE      D2ORD
187600100215     C                   MOVE      *ON           $INZS1
187700100215     C                   MOVE      *BLANK        W1POS
187800100215   E5C                   ENDIF
187900100215     C     $LASTG        WHENEQ    'D1'
188000100215     C* per ora niente
188100100215   E4C                   ENDSL
188200100215     C* definisco i nuovi valori di default
188300100215     C                   Z-ADD     D2LOP         $LOP
188400100215     C                   Z-ADD     D2ORD         $ORD
188500100215     C*
188600100215     C* aggiorno valori assunti
188700100215     C* se richiesto il salvataggio
188800100215   B4C     D2BSV         IFEQ      1
188900100215     C                   EXSR      AGGMPE
189000100215   E4C                   ENDIF
189100100215     C*
189200100215   E3C                   ENDIF
189300100215     C*
189400100215   E2C                   ENDSL
189500100215     C*
189600100215   E1C                   ENDIF
189700100215     C*
189800100215     C                   ENDSR
189900100215     C/EJECT
190000100215     C************************************************************
190100100215     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
190200100215     C************************************************************
190300100215     C     INZD2         BEGSR
190400100215     C*
190500100215     C                   CLEAR                   FMTD2
190600100215     C                   EVAL      D2BSVA = Normale
190700100215     C                   MOVEL     $LOP          D2LOP
190800100215     C                   MOVE      $ORD          D2ORD
190900100215     C*
191000100215     C                   ENDSR
191100100215     C/EJECT
191200100215     C************************************************************
191300100215     C* GESTIONE F03 VIDEO D2
191400100215     C************************************************************
191500100215     C     F03D2         BEGSR
191600100215     C*
191700100215     C                   MOVE      *ON           $FINE
191800100215     C*
191900100215     C                   EXSR      ENDPGM
192000100215     C*
192100100215     C                   ENDSR
192200100215     C/EJECT
192300100215     C************************************************************
192400100215     C* GESTIONE F12 VIDEO D2
192500100215     C************************************************************
192600100215     C     F12D2         BEGSR
192700100215     C*
192800100215     C                   MOVE      $LASTG        $GEST
192900100215     C*
193000100215     C                   ENDSR
193100100215     C/EJECT
193200100215     C************************************************************
193300100215     C* GESTIONE F24 VIDEO D2
193400100215     C************************************************************
193500100215     C     F24D2         BEGSR
193600100215     C*
193700100215     C                   MOVEA     $FX3          $FX
193800100215     C                   MOVEA     $FC3          $FC
193900100215     C                   MOVEA     $FR3          $FR
194000100215     C                   Z-ADD     $ULKD2        $ULK
194100100215     C                   Z-ADD     $D2L01        $L01
194200100215     C                   Z-ADD     $D2L02        $L02
194300100215     C                   EXSR      RIEKEY
194400100215     C                   Z-ADD     $ULK          $ULKD2            3 0
194500100215     C                   MOVEA     $FC           $FC3
194600100215     C                   MOVEL     $KEY1         Z3KE1
194700100215     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
194800100215     C                   WRITE     FMTZ3
194900100215     C*
195000100215     C                   ENDSR
195100100215     C/EJECT
195200100215     C************************************************************
195300100215     C* CONTROLLO VIDEATA VALORI ASSUNTI
195400100215     C************************************************************
195500100215     C     CTRD2         BEGSR
195600100215     C*
195700100215     C                   SETOFF                                       99
195800100215     C*
195900100215     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
196000100215     C     D2LOP         IFEQ      0
196100100215     C                   SETON                                          5099
196200100215     C                   ENDIF
196300100215     C*
196400100215     C* OBBLIGATORIO VALORE PER ORDINAMENTO
196500100215     C     D2ORD         IFEQ      0
196600100215     C                   SETON                                          5199
196700100215     C                   ENDIF
196800100215     C*
196900100215     C                   ENDSR
197000100215     C/EJECT
197100100215     C************************************************************
197200100215     C* CONTROLLO ABILITAZIONE TASTI
197300100215     C* INPUT
197400100215     C* $TASTO =Tasto funzione premuto
197500100215     C* $FC = Tasti abilitati
197600100215     C* OUTPUT
197700100215     C* IN99 = ERrore
197800100215     C************************************************************
197900100215     C     CTRKEY        BEGSR
198000100215     C*
198100100215     C                   SETOFF                                       99
198200100215     C*
198300100215     C                   SELECT
198400100215     C     $TASTO        WHENEQ    F01
198500100215     C                   z-add     01            posfun
198600100215     C     $FC(1)        IFEQ      '0'
198700100215     C                   SETON                                        9997
198800100215     C                   ENDIF
198900100215     C     $TASTO        WHENEQ    F02
199000100215     C                   z-add     02            posfun
199100100215     C     $FC(2)        IFEQ      '0'
199200100215     C                   SETON                                        9997
199300100215     C                   ENDIF
199400100215     C     $TASTO        WHENEQ    F03
199500100215     C                   z-add     03            posfun
199600100215     C     $FC(3)        IFEQ      '0'
199700100215     C                   SETON                                        9997
199800100215     C                   ENDIF
199900100215     C     $TASTO        WHENEQ    F04
200000100215     C                   z-add     04            posfun
200100100215     C     $FC(4)        IFEQ      '0'
200200100215     C                   SETON                                        9997
200300100215     C                   ENDIF
200400100215     C     $TASTO        WHENEQ    F05
200500100215     C                   z-add     05            posfun
200600100215     C     $FC(5)        IFEQ      '0'
200700100215     C                   SETON                                        9997
200800100215     C                   ENDIF
200900100215     C     $TASTO        WHENEQ    F06
201000100215     C                   z-add     06            posfun
201100100215     C     $FC(6)        IFEQ      '0'
201200100215     C                   SETON                                        9997
201300100215     C                   ENDIF
201400100215     C     $TASTO        WHENEQ    F07
201500100215     C                   z-add     07            posfun
201600100215     C     $FC(7)        IFEQ      '0'
201700100215     C                   SETON                                        9997
201800100215     C                   ENDIF
201900100215     C     $TASTO        WHENEQ    F08
202000100215     C                   z-add     08            posfun
202100100215     C     $FC(8)        IFEQ      '0'
202200100215     C                   SETON                                        9997
202300100215     C                   ENDIF
202400100215     C     $TASTO        WHENEQ    F09
202500100215     C                   z-add     09            posfun
202600100215     C     $FC(09)       IFEQ      '0'
202700100215     C                   SETON                                        9997
202800100215     C                   ENDIF
202900100215     C     $TASTO        WHENEQ    F10
203000100215     C                   z-add     10            posfun
203100100215     C     $FC(10)       IFEQ      '0'
203200100215     C                   SETON                                        9997
203300100215     C                   ENDIF
203400100215     C     $TASTO        WHENEQ    F11
203500100215     C                   z-add     11            posfun
203600100215     C     $FC(11)       IFEQ      '0'
203700100215     C                   SETON                                        9997
203800100215     C                   ENDIF
203900100215     C     $TASTO        WHENEQ    F12
204000100215     C                   z-add     12            posfun
204100100215     C     $FC(12)       IFEQ      '0'
204200100215     C                   SETON                                        9997
204300100215     C                   ENDIF
204400100215     C     $TASTO        WHENEQ    F13
204500100215     C                   z-add     13            posfun
204600100215     C     $FC(13)       IFEQ      '0'
204700100215     C                   SETON                                        9997
204800100215     C                   ENDIF
204900100215     C     $TASTO        WHENEQ    F14
205000100215     C                   z-add     14            posfun
205100100215     C     $FC(14)       IFEQ      '0'
205200100215     C                   SETON                                        9997
205300100215     C                   ENDIF
205400100215     C     $TASTO        WHENEQ    F15
205500100215     C                   z-add     15            posfun
205600100215     C     $FC(15)       IFEQ      '0'
205700100215     C                   SETON                                        9997
205800100215     C                   ENDIF
205900100215     C     $TASTO        WHENEQ    F16
206000100215     C                   z-add     16            posfun
206100100215     C     $FC(16)       IFEQ      '0'
206200100215     C                   SETON                                        9997
206300100215     C                   ENDIF
206400100215     C     $TASTO        WHENEQ    F17
206500100215     C                   z-add     17            posfun
206600100215     C     $FC(17)       IFEQ      '0'
206700100215     C                   SETON                                        9997
206800100215     C                   ENDIF
206900100215     C     $TASTO        WHENEQ    F18
207000100215     C                   z-add     18            posfun
207100100215     C     $FC(18)       IFEQ      '0'
207200100215     C                   SETON                                        9997
207300100215     C                   ENDIF
207400100215     C     $TASTO        WHENEQ    F19
207500100215     C                   z-add     19            posfun
207600100215     C     $FC(19)       IFEQ      '0'
207700100215     C                   SETON                                        9997
207800100215     C                   ENDIF
207900100215     C     $TASTO        WHENEQ    F20
208000100215     C                   z-add     20            posfun
208100100215     C     $FC(20)       IFEQ      '0'
208200100215     C                   SETON                                        9997
208300100215     C                   ENDIF
208400100215     C     $TASTO        WHENEQ    F21
208500100215     C                   z-add     21            posfun
208600100215     C     $FC(21)       IFEQ      '0'
208700100215     C                   SETON                                        9997
208800100215     C                   ENDIF
208900100215     C     $TASTO        WHENEQ    F22
209000100215     C                   z-add     22            posfun
209100100215     C     $FC(22)       IFEQ      '0'
209200100215     C                   SETON                                        9997
209300100215     C                   ENDIF
209400100215     C     $TASTO        WHENEQ    F23
209500100215     C                   z-add     23            posfun
209600100215     C     $FC(23)       IFEQ      '0'
209700100215     C                   SETON                                        9997
209800100215     C                   ENDIF
209900100215     C     $TASTO        WHENEQ    F24
210000100215     C                   z-add     24            posfun
210100100215     C     $FC(24)       IFEQ      '0'
210200100215     C                   SETON                                        9997
210300100215     C                   ENDIF
210400100215     C                   ENDSL
210500100215     C*
210600100215     C                   ENDSR
210700100215     C/EJECT
210800100215     C************************************************************
210900100215     C* AGGIORNAMENTO VALORI ASSUNTI
211000100215     C************************************************************
211100100215     C     AGGMPE        BEGSR
211200100215     C*
211300100215     C* preparo i parametri di chiamata al driver
211400100215     C                   CLEAR                   XMPEDS
211500100215     C                   EVAL      TpChiamata = '03'
211600100215     C                   EVAL      NChiavi = 5
211700100215     C                   EVAL      VLogica = 2
211800100215     C*
211900100215     C* preparo le chiavi per la chiamata al driver
212000100215     C                   CLEAR                   XMPEDS1
212100100215     C                   MOVEL     XSCSOC        DSMPESOC
212200100215     C                   MOVEL     DSPGM         DSMPEPGM
212300100215     C                   MOVEL     KNMUS         DSMPEPRF
212400100215     C*
212500100215     C* Si memorizzano i dati di default a seconda delle modalità con
212600100215     C* cui il pgm è usato
212700100215     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
212800100215     C* MPETIP = 'PV'  pgm richiamato da menù
212900100215     C* MPETIP = 'PB'  x lancio Batch
213000100215     C*
213100100215     C                   SELECT
213200100902     C     OPZD17        WHENEQ    '01'
213300100215     C                   MOVEL     'PX'          DSMPETIP
213400100215     C                   OTHER
213500100215     C                   MOVEL     'PV'          DSMPETIP
213600100215     C                   ENDSL
213700100215     C* dati
213800100215     C                   MOVEL     DSMPE         DSMPEPAR
213900100215     C*
214000100215     C                   CALLB     'XMPE'
214100100215     C                   PARM                    XMPEDS
214200100215     C                   PARM                    XMPEDS1
214300100215     C*
214400100215     C                   ENDSR
214500100215     C/EJECT
214600100215     C************************************************************
214700100215     C* OPERAZIONI INIZIALI
214800100215     C************************************************************
214900100215     C     *INZSR        BEGSR
215000100215     C*
215100100215     C* Reperimento parametri
215200100215     C*
215300100215     C     *ENTRY        PLIST
215400100215     C                   PARM                    KPJBA
215500100215     C                   PARM                    DSAUT
215600100215     C*
215700100215     C* testo se sono stati passati i dati di autorizzazione
215800100215     C*
215900100215     C     PARMS         IFLT      2
216000100215     C                   MOVE      *BLANK        AUTDS
216100100215     C                   ELSE
216200100215     C                   MOVEL     DSAUT         AUTDS
216300100215     C                   ENDIF
216400100215     C*
216500100215     C* Reperimento dati società
216600100215     C*
216700100215     C                   MOVEL     'SOC001'      TIPXSC
216800100215     C                   MOVEL     *BLANK        SOCXSC
216900100215     C                   EXSR      REPSOC
217000100215     C*
217100100215     C* Controlli dati società
217200100215     C*
217300100215     C     RTNXSC        IFNE      '1'
217400100215     C                   MOVEL     XSOCDS        SOC001
217500100215     C                   ELSE
217600100215     C                   MOVE      *ON           $FINE
217700100902     C                   MOVE      '1'           ErrD17
217800100215     C                   EXSR      ENDPGM
217900100215     C                   ENDIF
218000100215     C*
218100100215     C* reperisco le dimensioni delle variabili che conterranno i tasti
218200100215     C                   EVAL      $D1L01 = %Size(Z1Ke1)
218300100215     C                   EVAL      $D1L02 = %Size(Z1Ke2)
218400100215     C                   EVAL      $S1L01 = %Size(Z2Ke1)
218500100215     C                   EVAL      $S1L02 = %Size(Z2Ke2)
218600100215     C                   EVAL      $C1Op1 = %Size(C1Opz1)
218700100215     C                   EVAL      $C1Op2 = %Size(C1Opz2)
218800100215     C                   EVAL      $D2L01 = %Size(Z3Ke1)
218900100215     C                   EVAL      $D2L02 = 0
219000100215     C*
219100100215     C* Reperimento opzioni
219200100215     C*
219300100215     C                   EXSR      REPOPZ
219400100215     C*
219500100215     C* Reperimento tasti di funzione
219600100215     C*
219700100215     C                   EXSR      REPKEY
219800100215     C* Chiamata a routine per il reperimento delle opzioni
219900100215     C* personalizzate
220000100215     C                   CAllb     'X46USREXT'
220100100215     C                   PARM                    $OP1
220200100215     C                   PARM                    $TX1
220300100215     C                   PARM                    $FL1
220400100215     C                   PARM                    $PGO1
220500100215     C                   PARM      $dimOp        NumOpz
220600100215     C                   PARM                    $FX2
220700100215     C                   PARM                    $FC2
220800100215     C                   PARM                    $PGF2
220900100215     C                   PARM                    DsPgm
221000100215     C                   PARM                    xsccut
221100100215     C                   PARM                    xscgrp
221200100215     C*
221300100215     C                   Z-add     1             OPZLEN
221400100215     C     *blank        Lookup    $TX1(opzlen)                           21
221500100215     C                   if        *in21 = *on
221600100215     C                   sub       1             OPZLEN
221700100215     C                   else
221800100215     C                   Z-add     $DimOp        OPZLEN
221900100215     C                   endif
222000100215     C*
222100100215     C                   ENDSR
222200100215     C/EJECT
222300100215     C************************************************************
222400100215     C* INIZIALIZZAZIONE VARIABILI
222500100215     C************************************************************
222600100215     C     INZVAR        BEGSR
222700100215     C*
222800100215     C* Dati da PGM chiamante
222900100215     C*
223000100902     C                   MOVEL     KPJBU         SDGSD17DS
223100100215     C*
223200100215     C* Reperimento autorizzazioni
223300100215     C*
223400100215     C* le autorizzazioni potrebbero essere già state passate,
223500100215     C* nel qual caso non vanno reperite
223600100215     C     AUTDS         IFEQ      *BLANK
223700100215     C                   EXSR      REPAUT
223800100215     C                   ELSE
223900100902     C                   MOVEL     XCADDS        CONCIVDS
224000100215     C                   ENDIF
224100100215     C*
224200100215     C* Reperimento valori default utente
224300100215     C*
224400100215     C                   EXSR      REPDFT
224500100215     C*
224600100215     C* Definizione delle opzioni utilizzabili a pgm
224700100215     C*
224800100215     C                   EXSR      DEFOPZ
224900100215     C*
225000100215     C* Variabili per gestione videate
225100100215     C*
225200100215     C                   MOVE      *OFF          $FINE                          fine pgm
225300100215     C                   MOVE      *OFF          $INZD1                         inizializzione D1
225400100215     C                   MOVE      *OFF          $INZS1                         inizializzione S1
225500100215     C                   MOVE      *OFF          $INZD2                         inizializzione D2
225600100215     C                   MOVE      *OFF          $EndFile                       memo fine file
225700100215     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
225800100215     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
225900100215     C                   MOVE      *HIVAL        $LASTV
226000100215     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
226100100215     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
226200100215     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
226300100215     C* azzero la posizione del cursore
226400100215     C                   Z-ADD     0             H1RIGA
226500100215     C                   Z-ADD     0             H1COLO
226600100215     C*
226700100215     C                   MOVE      *ON           $INZD1
226800100215     C                   MOVE      *ON           $INZS1
226900100215     C*
227000100215     C* Variabili appoggio
227100100215     C*
227200100215f13  C* inizialmente imposto il numero di righe per pagina                      F13
227300100215f13  C                   Z-Add     RigPag        SflPag                         F13
227400100215     C                   Z-ADD     SFLPAG        WPAG
227500100215     C                   MOVEL     *OFF          WOPR
227600100215     C                   CLEAR                   W1POS
227700100215     C*
227800100215     C* definisco qual'è la prima videata
227900100215     C* (se la prima videata risulta la lista, devo comunque caricare
228000100215     C*  la videata di parzializzazione e gestire la definizione
228100100215     C*  dell'ordinamento, perchè, dai default oppure se sono stati
228200100215     C*  passati dei dati, può variare il caricamento)
228300100215     C                   SELECT
228400100215     C     $LOP          WHENEQ    1
228500100215     C                   MOVE      'D1'          $GEST
228600100215     C     $LOP          WHENEQ    2
228700100215     C                   MOVE      'S1'          $GEST
228800100215     C                   EXSR      INZD1
228900100215     C                   MOVE      *OFF          $INZD1
229000100215     C                   EXSR      DEFORD
229100100215     C                   ENDSL
229200100215     C*
229300100215     C* Valorizzazione campi univoci testate
229400100215     C*
229500100215     C                   CLEAR                   FMTT1
229600100215     C                   MOVEL     KNSIF         NOMSIF
229700100215     C                   MOVEL     XSCDSI        NOMDIT
229800100215     C                   MOVEL     DSPGM         NOMPGM           10
229900100215     C                   MOVEL     KCDAZ         NOMAZN
230000100215     C* recupero il sottotitolo (se c'è)
230100100215     C                   MOVEL     *BLANK        T1TIT
230200100215     C     SOTTIT        IFNE      *BLANK
230300100215     C                   MOVEL     SOTTIT        IDMSG
230400100215     C                   CALLB     'XRTVM'
230500100215     C                   PARM                    IDMSG            27
230600100215     C                   PARM                    TXTMSG          100
230700100215     C                   PARM                    ERRMSG            1
230800100215     C                   PARM      *ON           CTRMSG            1            centratura
230900100215     C                   PARM      30            LENMSG            3 0          lun output.
231000100215     C     ERRMSG        IFNE      *ON
231100100215     C                   MOVEL     TXTMSG        T1TIT
231200100215     C                   ELSE
231300100215     C                   MOVEL     *ALL'?'       T1TIT
231400100215     C                   ENDIF
231500100215     C                   ENDIF
231600100215     C*
231700100215     C                   ENDSR
231800100215     C/EJECT
231900100215     C************************************************************
232000100215     C* REPERIMENTO DATI SOCIETA'
232100100215     C************************************************************
232200100215     C     REPSOC        BEGSR
232300100215     C*
232400100215     C                   CALL      'SDGSOCDR'
232500100215     C                   PARM                    TIPXSC            6
232600100215     C                   PARM                    SOCXSC            3
232700100215     C                   PARM                    CDSXSC            9 0
232800100215     C                   PARM                    MODXSC            3
232900100215     C                   PARM      *BLANKS       RTNXSC            1
233000100215     C                   PARM                    XSOCDS
233100100215     C                   PARM                    KPJBA
233200100215     C*
233300100215     C                   ENDSR
233400100215     C/EJECT
233500100215     C************************************************************
233600100215     C* REPERIMENTO AUTORIZZAZIONI UTENTE
233700100215     C************************************************************
233800100215     C     REPAUT        BEGSR
233900100215     C*
234000100215     C                   CLEAR                   AUTDS
234100100215     C*
234200100215     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
234300100215     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
234400100215     C                   Z-ADD     0             XCAGRP                         GRUPPO
234500100215     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
234600100902     C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
234700100215     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
234800100215     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
234900100215     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
235000100215     C*
235100100215     C                   CALLB     'XCHKAUT'
235200100215     C                   PARM                    AUTDS
235300100215     C*
235400100215     C* errori/non abilitazione
235500100215     C     XCARTN        IFNE      0
235600100902     C                   MOVE      '2'           ErrD17
235700100215     C                   EXSR      ENDPGM
235800100215     C                   ELSE
235900100902     C                   MOVEL     XCADDS        CONCIVDS
236000100215     C                   ENDIF
236100100215     C*
236200100215     C                   ENDSR
236300100215     C/EJECT
236400100215     C************************************************************
236500100215     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
236600100215     C************************************************************
236700100215     C     REPDFT        BEGSR
236800100215     C*
236900100215     C* preparo i parametri di chiamata al driver
237000100215     C                   CLEAR                   XMPEDS
237100100215     C                   EVAL      TpChiamata = '01'
237200100215     C                   EVAL      NChiavi = 5
237300100215     C                   EVAL      VLogica = 2
237400100215     C*
237500100215     C* preparo le chiavi per la chiamata al driver
237600100215     C                   CLEAR                   XMPEDS1
237700100215     C                   MOVEL     XSCSOC        DSMPESOC
237800100215     C                   MOVEL     DSPGM         DSMPEPGM
237900100215     C                   MOVEL     KNMUS         DSMPEPRF
238000100215     C* Si reperiscono i dati di default a seconda delle modalità con
238100100215     C* cui il pgm è usato
238200100215     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
238300100215     C* MPETIP = 'PV'  pgm richiamato da menù
238400100215     C* MPETIP = 'PB'  x lancio Batch
238500100215     C                   SELECT
238600100902     C     OPZD17        WHENEQ    '01'
238700100215     C                   MOVEL     'PX'          DSMPETIP
238800100215     C                   OTHER
238900100215     C                   MOVEL     'PV'          DSMPETIP
239000100215     C                   ENDSL
239100100215     C*
239200100215     C                   CALLB     'XMPE'
239300100215     C                   PARM                    XMPEDS
239400100215     C                   PARM                    XMPEDS1
239500100215     C*
239600100215     C     Risultato     IFEQ      '0'
239700100215     C                   MOVEL     DSMPEPAR      DSMPE
239800100215     C* nel caso sia la prima volta che l'utente apre questa anagrafica
239900100215     C* i suoi valori di default vengono decisi a pgm
240000100215     C                   ELSE
240100100215     C                   Z-ADD     1             $LOP
240200100215     C                   Z-ADD     1             $ORD
240300100215     C                   ENDIF
240400100215     C*
240500100215     C                   ENDSR
240600100215     C/EJECT
240700100215     C************************************************************
240800100215     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
240900100215     C************************************************************
241000100215     C     DEFOPZ        BEGSR
241100100215     C*
241200100215     C* Questa routine deve valorizzare la schiera $FL1
241300100215     C* con S o N a seconda della utilizzabilità o meno
241400100215     C* dell'opzione correlata
241500100215     C*
241600100215     C* Inizialmente si considerano tutte le opzioni come valide
241700100215     C     1             DO        $DIMOP        X
241800100215     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
241900100215     C                   If        $TX1(X)  <> *Blank
242000100215     C                   MOVE      'S'           $FL1(X)
242100100215     C                   Else
242200100215     C                   Leave
242300100215     C                   EndIf
242400100215     C                   ENDDO
242500100215     C*
242600100215     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
242700100215     C* con 1 o 0 a seconda della utilizzabilità o meno
242800100215     C* del tasto funzione correlato
242900100215     C*
243000100215     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
243100100215     C* di compilazione
243200100215     C                   RESET                   $FC1
243300100215     C                   RESET                   $FR1
243400100215     C                   RESET                   $FC2
243500100215     C                   RESET                   $FR2
243600100215     C                   RESET                   $FC3
243700100215     C                   RESET                   $FR3
243800100215     C*
243900100215     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
244000100215     C* invalido alcune opzioni
244100100215     C                   SELECT
244200100215     C* Se pgm richiamato in ricerca disattivo :
244300100902     C     OPZD17        WHENEQ    '01'
244400100215     C* F3=Fine
244500100215     C                   Z-ADD     3             X
244600100215     C                   MOVE      '0'           $FC1(X)
244700100215     C                   MOVE      '0'           $FC2(X)
244800100215     C                   MOVE      '0'           $FC3(X)
244900100215     C*
245000100215     C*
245100100215     C* Se pgm richiamato in interrogazione disattivo :
245200100902     C     OPZD17        WHENEQ    '05'
245300100215     C* F10=Immissione
245400100215     C                   Z-ADD     10            X
245500100215     C                   MOVE      '0'           $FC1(X)
245600100215     C                   MOVE      '0'           $FC2(X)
245700100215     C* 2=Modifica
245800100215     C                   Z-ADD     1             X
245900100215     C     02            LOOKUP    $OP1(X)                                21
246000100215     C     *IN21         IFEQ      *ON
246100100215     C                   MOVE      'N'           $FL1(X)
246200100215     C                   ENDIF
246300100215     C* 3=Copia
246400100215     C                   Z-ADD     1             X
246500100215     C     03            LOOKUP    $OP1(X)                                21
246600100215     C     *IN21         IFEQ      *ON
246700100215     C                   MOVE      'N'           $FL1(X)
246800100215     C                   ENDIF
246900100215     C* 4=Annullamento
247000100215     C                   Z-ADD     1             X
247100100215     C     04            LOOKUP    $OP1(X)                                21
247200100215     C     *IN21         IFEQ      *ON
247300100215     C                   MOVE      'N'           $FL1(X)
247400100215     C                   ENDIF
247500100215     C*
247600100902     C     OPZD17        WHENEQ    *BLANKS
247700100215     C* Se pgm non richiamato, disattivo subito :
247800100215     C* F8=Selezione seguente
247900100215     C                   Z-ADD     8             X
248000100215     C                   MOVE      '0'           $FC1(X)
248100100215     C                   MOVE      '0'           $FC2(X)
248200100215     C*
248300100215     C* In tutti gli altri casi, disattivo subito :
248400100215     C                   OTHER
248500100215     C*
248600100215     C                   ENDSL
248700100215     C*
248800100215     C* poi, disattivo a seconda delle autorizzazioni :
248900100215     C* - immissione/copia
249000100902     C     IMMALL        IFEQ      *OFF
249100100215     C* F10=Immissione
249200100215     C                   Z-ADD     10            X
249300100215     C                   MOVE      '0'           $FC1(X)
249400100215     C                   MOVE      '0'           $FC2(X)
249500100215     C* 3=Copia
249600100215     C                   Z-ADD     1             X
249700100215     C     03            LOOKUP    $OP1(X)                                21
249800100215     C     *IN21         IFEQ      *ON
249900100215     C                   MOVE      'N'           $FL1(X)
250000100215     C                   ENDIF
250100100215     C                   ENDIF
250200100215     C* - manutenzione
250300100902     C     MANALL        IFEQ      *OFF
250400100215     C* 2=Modifica
250500100215     C                   Z-ADD     1             X
250600100215     C     02            LOOKUP    $OP1(X)                                21
250700100215     C     *IN21         IFEQ      *ON
250800100215     C                   MOVE      'N'           $FL1(X)
250900100215     C                   ENDIF
251000100215     C                   ENDIF
251100100215     C* - annullamento
251200100902     C     ANNALL        IFEQ      *OFF
251300100215     C* 4=Annullamento
251400100215     C                   Z-ADD     1             X
251500100215     C     04            LOOKUP    $OP1(X)                                21
251600100215     C     *IN21         IFEQ      *ON
251700100215     C                   MOVE      'N'           $FL1(X)
251800100215     C                   ENDIF
251900100215     C                   ENDIF
252000100215     C* - visualizzazione
252100100902     C     VISALL        IFEQ      *OFF
252200100215     C* 5=Visualizzazione
252300100215     C                   Z-ADD     1             X
252400100215     C     05            LOOKUP    $OP1(X)                                21
252500100215     C     *IN21         IFEQ      *ON
252600100215     C                   MOVE      'N'           $FL1(X)
252700100215     C                   ENDIF
252800100215     C                   ENDIF
252900100902     C*
253000100902     C* accensione colonna di esclusione
253100100902     C                   seton                                        51
253200100215     C*
253300100215     C                   ENDSR
253400100215     C/EJECT
253500100215     C************************************************************
253600100215     C* REPERIMENTO OPZIONI
253700100215     C************************************************************
253800100215     C     REPOPZ        BEGSR
253900100215     C*
254000100215     C     1             DO        $DIMOP        X
254100100215     C*
254200100215     C     $MS1(X)       IFNE      *BLANK
254300100215     C*
254400100215     C                   MOVEL     $MS1(X)       IDMSG
254500100215     C                   CALLB     'XRTVM'
254600100215     C                   PARM                    IDMSG            27
254700100215     C                   PARM                    TXTMSG          100
254800100215     C                   PARM                    ERRMSG            1
254900100215     C*
255000100215     C     ERRMSG        IFNE      *ON
255100100215     C                   MOVEL     TXTMSG        $TX1(X)
255200100215     C                   ELSE
255300100215     C                   MOVEL     *ALL'?'       $TX1(X)
255400100215     C                   ENDIF
255500100215     C*
255600100215     C                   ENDIF
255700100215     C*
255800100215     C                   ENDDO
255900100215     C*
256000100215     C                   ENDSR
256100100215     C/EJECT
256200100215     C************************************************************
256300100215     C* REPERIMENTO TASTI DI FUNZIONE
256400100215     C************************************************************
256500100215     C     REPKEY        BEGSR
256600100215     C                   MOVEA     $FM1          $FM
256700100215     C                   MOVEA     $FC1          $FC
256800100215     C                   EXSR      REPKKK
256900100215     C                   MOVEA     $FX           $FX1
257000100215     C*
257100100215     C                   MOVEA     $FM2          $FM
257200100215     C                   MOVEA     $FC2          $FC
257300100215     C                   EXSR      REPKKK
257400100215     C                   MOVEA     $FX           $FX2
257500100215     C*
257600100215     C                   MOVEA     $FM3          $FM
257700100215     C                   MOVEA     $FC3          $FC
257800100215     C                   EXSR      REPKKK
257900100215     C                   MOVEA     $FX           $FX3
258000100215     C                   ENDSR
258100100215     C/EJECT
258200100215     C************************************************************
258300100215     C* REPERIMENTO TASTI DI FUNZIONE
258400100215     C************************************************************
258500100215     C     REPKKK        BEGSR
258600100215     C*
258700100215     C                   CLEAR                   $FX
258800100215     C     1             DO        24            X
258900100215     C*
259000100215     C     $FM(X)        IFNE      *BLANK
259100100215     C     $Fc(X)        andne     '0'
259200100215     C*
259300100215     C                   MOVEL     $FM(X)        IDMSG
259400100215     C                   CALLB     'XRTVM'
259500100215     C                   PARM                    IDMSG            27
259600100215     C                   PARM                    TXTMSG          100
259700100215     C                   PARM                    ERRMSG            1
259800100215     C*
259900100215     C     ERRMSG        IFNE      *ON
260000100215     C                   MOVEL     TXTMSG        $FX(X)
260100100215     C                   ELSE
260200100215     C                   MOVEL     *ALL' '       $FX(X)
260300100215     C                   ENDIF
260400100215     C*
260500100215     C                   ENDIF
260600100215     C*
260700100215     C                   ENDDO
260800100215     C*
260900100215     C                   ENDSR
261000100215     C/EJECT
261100100215     C************************************************************
261200100215     C* DEFINIZIONE KLIST
261300100215     C************************************************************
261400100215     C     DefKlist      BEGSR
261500100215     C*
261600100215     C* klist
261700100215     C*
261800100902     C* SDGBLA01L
261900100902     C     K05BLA01      KLIST
262000100902     C                   KFLD                    BLASocieta
262100100902     C                   KFLD                    BLATpReg
262200100902     C                   KFLD                    BLALibro
262300100902     C                   KFLD                    BLAAliq
262400100902     C                   KFLD                    BLARifIVA
262500100902     C*
262600100902     C     K02BLA01      KLIST
262700100902     C                   KFLD                    BLASocieta
262800100902     C                   KFLD                    BLATpReg
262900100902     C*
263000100902     C* ANDLI01L
263100100902     C     K03DLI01      KLIST
263200100902     C                   KFLD                    DLISocieta
263300100902     C                   KFLD                    DLITpReg
263400100902     C                   KFLD                    DLILibro
263500100902     C*
263600100902     C* ANRIV01L
263700100902     C     K04RIV01      KLIST
263800100902     C                   KFLD                    RIVSocieta
263900100902     C                   KFLD                    RIVTpReg
264000100902     C                   KFLD                    RIVAliq
264100100902     C                   KFLD                    RIVRifIVA
264200100902     C*
264300100902     C* ANALR02L
264400100902     C     K02ALR02      KLIST
264500100902     C                   KFLD                    ALRSocieta
264600100902     C                   KFLD                    ALRTpReg
264700100215     C*
264800100215     C                   ENDSR
264900100215     C/EJECT
265000100215** $MS1 / $TX1
265100100902PROMSG    *LIBL     OPZ0495
265200100215** $OP1 / $FL1
26530010090204S      <Escludi/Includi
265400100215** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
265500100215PROMSG    *LIBL     KEY0001  01
265600100215PROMSG    *LIBL     KEY0002  02
265700100215PROMSG    *LIBL     KEY0003  03
265800100215PROMSG    *LIBL     KEY0004  04
265900100215PROMSG    *LIBL     KEY0005  05
266000100215PROMSG    *LIBL     KEY0006  06
266100100215PROMSG    *LIBL     KEY0007  07
266200100215PROMSG    *LIBL     KEY0008  08
266300100215PROMSG    *LIBL     KEY0009  09
266400100215PROMSG    *LIBL     KEY0010  10
266500100215PROMSG    *LIBL     KEY0011  11
266600100215PROMSG    *LIBL     KEY0012  12
266700100215PROMSG    *LIBL     KEY0013  13
266800100215PROMSG    *LIBL     KEY0014  14
266900100215PROMSG    *LIBL     KEY0015  15
267000100215PROMSG    *LIBL     KEY0016  16
267100100215SDGMSG    *LIBL     SDK0001  17
267200100215PROMSG    *LIBL     KEY0018  18
267300100215PROMSG    *LIBL     KEY0019  19
267400100215PROMSG    *LIBL     KEY0020  20
267500100215PROMSG    *LIBL     KEY0021  21
267600100215PROMSG    *LIBL     KEY0022  22
267700100215PROMSG    *LIBL     KEY0023  23
267800100215PROMSG    *LIBL     KEY0024  24
267900100215**  ABILITAZIONE D1 : $FC1 / $FR1
2680001002150N  01
2681001002151N  02
2682001002151N  03
2683001002151N  04
2684001002150N  05
2685001002150N  06
2686001002150N  07
2687001002151N  08
2688001002150N  09
2689001002150N  10
2690001002150N  11
2691001002151N  12
2692001002150N  13
2693001002150N  14
2694001002150N  15
2695001002150N  16
2696001002150N  17
2697001002150N  18
2698001002150N  19
2699001002150N  20
2700001002150N  21
2701001002150N  22
2702001002150N  23   Impostare a '1' se si utilizzano opzioni
2703001002151N  24   Lasciare sempre impostata a '1' il valore della riga 24
270400100215** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
270500100215PROMSG    *LIBL     KEY0001  01
270600100215PROMSG    *LIBL     KEY0002  02
270700100215PROMSG    *LIBL     KEY0003  03
270800100215PROMSG    *LIBL     KEY0004  04
270900100215PROMSG    *LIBL     KEY0005  05
271000100215PROMSG    *LIBL     KEY0006  06
271100100215PROMSG    *LIBL     KEY0007  07
271200100215PROMSG    *LIBL     KEY0008  08
271300100215PROMSG    *LIBL     KEY0009  09
271400100215PROMSG    *LIBL     KEY0010  10
271500100215PROMSG    *LIBL     KEY0011  11
271600100215PROMSG    *LIBL     KEY0012  12
271700100215PROMSG    *LIBL     KEY0013  13
271800100215PROMSG    *LIBL     KEY0014  14
271900100215PROMSG    *LIBL     KEY0015  15
272000100215PROMSG    *LIBL     KEY0016  16
272100100215SDGMSG    *LIBL     SDK0001  17
272200100215PROMSG    *LIBL     KEY0018  18
272300100215PROMSG    *LIBL     KEY0019  19
272400100215PROMSG    *LIBL     KEY0020  20
272500100215PROMSG    *LIBL     KEY0021  21
272600100215PROMSG    *LIBL     KEY0022  22
272700100215PROMSG    *LIBL     KEY0023  23
272800100215PROMSG    *LIBL     KEY0024  24
272900100215**  ABILITAZIONE S1 : $FC2 / $FR2
2730001002150N  01
2731001002150N  02
2732001002151N  03
2733001002150N  04
2734001002151N  05                                           f13
2735001002150N  06
2736001002150N  07
2737001002151N  08
2738001002150N  09
2739001009020N  10
2740001002150N  11
2741001002151N  12
2742001002151N  13                                           f13
2743001002150N  14
2744001002150N  15
2745001002150N  16
2746001002151N  17
2747001002150N  18
2748001002150N  19
2749001002150N  20
2750001002150N  21
2751001002150N  22
2752001002151N  23   Impostare a '1' se si utilizzano opzioni
2753001002151N  24   Lasciare sempre impostata a '1' il valore della riga 24
275400100215** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
275500100215PROMSG    *LIBL     KEY0001  01
275600100215PROMSG    *LIBL     KEY0002  02
275700100215PROMSG    *LIBL     KEY0003  03
275800100215PROMSG    *LIBL     KEY0004  04
275900100215PROMSG    *LIBL     KEY0005  05
276000100215PROMSG    *LIBL     KEY0006  06
276100100215PROMSG    *LIBL     KEY0007  07
276200100215PROMSG    *LIBL     KEY0008  08
276300100215PROMSG    *LIBL     KEY0009  09
276400100215PROMSG    *LIBL     KEY0010  10
276500100215PROMSG    *LIBL     KEY0011  11
276600100215PROMSG    *LIBL     KEY0012  12
276700100215PROMSG    *LIBL     KEY0013  13
276800100215PROMSG    *LIBL     KEY0014  14
276900100215PROMSG    *LIBL     KEY0015  15
277000100215PROMSG    *LIBL     KEY0016  16
277100100215SDGMSG    *LIBL     SDK0001  17
277200100215PROMSG    *LIBL     KEY0018  18
277300100215PROMSG    *LIBL     KEY0019  19
277400100215PROMSG    *LIBL     KEY0020  20
277500100215PROMSG    *LIBL     KEY0021  21
277600100215PROMSG    *LIBL     KEY0022  22
277700100215PROMSG    *LIBL     KEY0023  23
277800100215PROMSG    *LIBL     KEY0024  24
277900100215**  ABILITAZIONE D3 : $FC3 / $FR3
2780001002150N  01
2781001002150N  02
2782001002151N  03
2783001002150N  04
2784001002150N  05
2785001002150N  06
2786001002150N  07
2787001002150N  08
2788001002150N  09
2789001002150N  10
2790001002150N  11
2791001002151N  12
2792001002150N  13
2793001002150N  14
2794001002150N  15
2795001002150N  16
2796001002150N  17
2797001002150N  18
2798001002150N  19
2799001002150N  20
2800001002150N  21
2801001002150N  22
2802001002150N  23   Impostare a '1' se si utilizzano opzioni
2803001002151N  24   Lasciare sempre impostata a '1' il valore della riga 24
