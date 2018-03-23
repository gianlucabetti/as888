000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200000000     H*PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500110825      *         ------------------------------------------------
000600130823      *                   Spesometro Clienti/Fornitori
000700110825      *                       Regole di estrazione
000800110825      *         ------------------------------------------------
000900000000      *
001000000000      *
001100000000      *
001200000000     F*----------------------------------------------------------------
001300000000     F* associazione libri/riferim-aliquote
001400000000     FANALR01L  IF   e           K DISK
001500000000     F*-------------
001600000000     F* libri
001700000000     FANDLI01L  IF   E           K DISK
001800000000     F*-------------
001900000000     F* voce
002000000000     FANVCI01L  IF   E           K DISK
002100000000     F*-------------
002200000000     F* riferimenti IVA per Aliquota
002300000000     FANRIV02L  IF   E           K DISK
002400000000     F                                     RENAME(ANRIV000:ANRIV02)
002500000000     F*------------
002600000000     F* Key: soc/associazione/causale/conto/voce
002700130823     FSDGSPR04L IF   E           K DISK
002800130823     F                                     RENAME(SDGSPR000:SDGSPR04)
002900000000     F*-------------
003000000000     F* riferimenti IVA per codice di esenzione
003100000000     FANRIV03L  IF   E           K DISK
003200000000     F                                     RENAME(ANRIV000:ANRIV03)
003300000000     F*-------------
003400130823     F* Key: Soc/TpReg/STpReg/NrRiga   Unique
003500130823     FSDGSPR01L UF A E           K DISK
003600000000     F                                     USROPN
003700000000     F                                     COMMIT
003800130823     F                                     RENAME(SDGSPR000:SDGSPR01)
003900000000     F*------------
004000130823     FSDGSG70D  CF   E             WORKSTN
004100130823     F                                     RENAME(SG70D1:FMTD1)
004200130823     F                                     RENAME(SG70S1:FMTS1)
004300130823     F                                     RENAME(SG70C1:FMTC1)
004400130823     F                                     RENAME(SG70T1:FMTT1)
004500130823     F                                     RENAME(SG70Z1:FMTZ1)
004600130823     F                                     RENAME(SG70Z2:FMTZ2)
004700000000     F                                     SFILE(FMTS1:S1NrR)
004800000000     F                                     INFDS(DSFMT)
004900000000     D*----------------------------------------------------*
005000000000     D* dimensione della schiera $MS1, da non cambiare
005100000000     D $DIMOP          C                   CONST(30)
005200000000     D*----------------------------------------------------*
005300000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005400000000     D $ID             S             27    DIM(5) ALT($OZ)
005500000000     D* Schiere generali per la gestione dei tasti di funzione
005600000000     D $FM             S             27    DIM(24)
005700000000     D $FX             S             50    DIM(24)
005800000000     D $FC             S              1    DIM(24)
005900000000     D $FR             S              1    DIM(24)
006000000000     D* Schiere per la gestione dei tasti di funzione
006100000000     D* Video D1
006200000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006300000000     D $FX1            S             50    DIM(24) ALT($FM1)
006400000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006500000000     D $FR1            S              1    DIM(24) ALT($FC1)
006600000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006700000000     D* funzione personalizzata S1
006800000000     D $PGF2           S             10    DIM(25)
006900000000     D* Schiere per la gestione dei tasti di funzione
007000000000     D* Video S1
007100000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
007200000000     D $FX2            S             50    DIM(24) ALT($FM2)
007300000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
007400000000     D $FR2            S              1    DIM(24) ALT($FC2)
007500000000     D*
007600000000     D $D1L01          S              3  0
007700000000     D $D1L02          S              3  0
007800000000     D $S1L01          S              3  0
007900000000     D $S1L02          S              3  0
008000000000     D $C1Op1          S              3  0
008100000000     D $C1Op2          S              3  0
008200000000     D* Inserire qui la definizione di altre variabili per reperire
008300000000     D* la lunghezza di altre videate che usano un piede diverso da
008400000000     D* STD
008500000000     D*-------------
008600000000     D* Indice della schiera tasti funzione
008700000000     D PosFun          S              2  0
008800000000     D*-------------
008900000000     D* Valori assunti
009000000000     D XMPEDS        E DS                  INZ
009100000000     D XMPEDS1       E DS                  INZ
009200000000     D DSMPE           DS                  INZ
009300000000     D  $LOP                   1      1  0
009400000000     D  $ORD                   2      3  0
009500000000     D*-------------
009600000000     D* Passaggio Parametri
009700000000     D KPJBA         E DS
009800000000     D*-------------
009900000000     D* DS Esterna gestione messaggi
010000000000     D SDGMSGDS      E DS
010100000000     D  STS          E                     EXTFLD(MSGSTS)
010200000000     D                                     DIM(333)
010300000000     D  JBA          E                     EXTFLD(MSGJBA)
010400000000     D                                     DIM(502)
010500000000     D*-------------
010600000000     D* Reperimento Autorizzazioni
010700000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010800000000     D*-------------
010900000000     D* Passaggio Autorizzazioni
011000000000     D DSAUT           DS           512
011100000000     D*-------------
011200000000     D XPAOUT          DS          2000                                         *
011300000000     D*-------------
011400000000     D* Ricerca legende
011500000000     D Ana964DS      E DS
011600000000     D*-------------
011700000000     D* Ricerca libri IVA
011800000000     D Ana062DS      E DS                  Inz
011900000000     D  Ret062       E                     Inz('0')
012000000000     D  Opr062       E                     Inz('0')
012100000000     D  Err062       E                     Inz('0')
012200000000     D*-------------
012300000000     D* Ricerca riferimenti e aliquote
012400000000     D Ana064DS      E DS                  Inz
012500000000     D  Ret064       E                     Inz('0')
012600000000     D  Opr064       E                     Inz('0')
012700000000     D  Err064       E                     Inz('0')
012800000000     D*-------------
012900000000     D* Ricerca associazione libro-riferimento IVA
013000000000     D ANA068DS      E DS                  INZ
013100000000     D*-------------
013200000000     D* Ricerca causale
013300000000     D ANA070DS      E DS                  INZ
013400000000     D  RET070       E                     INZ('0')
013500000000     D  OPR070       E                     INZ('0')
013600000000     D  CMT070       E                     INZ('0')
013700000000     D  ERR070       E                     INZ('0')
013800000000     D  FSC070       E                     INZ('S')
013900000000     D*-------------
014000000000     D* Ricerca capoconto
014100000000     D ANA200DS      E DS                  INZ
014200000000     D  RET200       E                     INZ('0')
014300000000     D  OPR200       E                     INZ('0')
014400000000     D  ERR200       E                     INZ('0')
014500000000     D  CMT200       E                     INZ('0')
014600000000     D*-------------
014700000000     D* Ricerca sottoconti
014800000000     D ANA201DS      E DS                  INZ
014900000000     D*-------------
015000000000     D* Ricerca voce
015100000000     D ANA280DS      E DS                  INZ
015200000000     D  RET280       E                     INZ('0')
015300000000     D  OPR280       E                     INZ('0')
015400000000     D  ERR280       E                     INZ('0')
015500000000     D  CMT280       E                     INZ('0')
015600000000     D*-------------
015700000000     D* Ds per tabella classe contabile
015800000000     D ANGG81DS      E DS                  INZ
015900000000     D*-------------
016000000000     D* Parametri per ricerca codice ANATB00F
016100000000     D XTABDS        E DS                  inz
016200000000     D*-------------
016300000000     D* Parametri per controllo codice ANATB00F
016400000000     D SDGTABDS      E DS                  INZ
016500000000     D*-------------
016600000000     D* Controllo coerenza dati ctb analitica
016700000000     D X45SIMANDS    E DS
016800000000     D*-------------
016900000000     D* Controlli validità legende
017000000000     D X07ValDS      E DS
017100000000     D*-------------
017200000000     D* DS per controllo conto
017300000000     D ND002DS       E DS                  INZ
017400000000     D*-------------
017500000000     D* Definizioni controllo causale
017600000000     D Anope00f      e ds                  inz
017700000000     D Refresh         S              1
017800000000     D Uso             S              1
017900000000     D Output          S           4000
018000000000     D Causale         S              4
018100000000     D*----------------------------------------------------*
018200000000     D* Definizioni parametri standard chiamate moduli
018300000000     D Esito           s              1
018400000000     D GesMsg          s              1
018500000000     D LenOut          s              9B 0
018600000000     D StrutturaO      s             10
018700000000     D Codoper         s              1
018800000000     D Data            S               D
018900000000     D*-------------
019000000000     D* Parametri in ricezione
019100130823     D SDGSG70DS     E DS                  INZ
019200000000     D*-------------------------
019300000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
019400000000     D* alla funzione personalizzata
019500130823     D $SaveDS         S                   like(SDGSG70DS)
019600000000     D*-------------
019700000000     D* Maschera dati autorizzazioni
019800000000     D***------DS       E DS
019900000000     D*-------------
020000000000     D DSFMT           DS           512
020100000000     D  $TASTO               369    369
020200000000     D  NRG                  370    370
020300000000     D  NCL                  371    371
020400000000     D  SflNrR               378    379B 0
020500000000     D*-------------
020600000000     D* posizione cursore
020700000000     D CURSOR          DS
020800000000     D  RIRI                   1      2B 0 INZ
020900000000     D  R$$                    2      2
021000000000     D  COCO                   3      4B 0 INZ
021100000000     D  C$$                    4      4
021200000000     D*-------------
021300000000     D* DS per UserExit
021400000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
021500000000     D*-------------
021600000000     D* Reperimento nome PGM
021700000000     D STATUS         SDS           333
021800000000     D  DSPGM            *PROC
021900000000     D  PARMS            *PARMS
022000000000     D*-------------
022100000000     D* Dati di ambiente ottenuti da XSOC
022200000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
022300000000     D*-------------
022400000000     D* DS Interna per dati di output di XSOC
022500000000     D XSOCDS          DS          1000
022600000000     D*-------------
022700000000     D* Costanti
022800000000     D* nr.di righe del sfl (SflPag)
022900000000     D SFLPAG          S              4S 0 Inz(12)
023000000000     D*-------------
023100000000     D* Schiera per il controllo delle opzioni
023200000000     D $OPZ            S              5  0 DIM(200) inz(0)
023300000000     D*-------------
023400000000     D* Tasti di funzione
023500000000     D F01             C                   CONST(X'31')
023600000000     D F02             C                   CONST(X'32')
023700000000     D F03             C                   CONST(X'33')
023800000000     D F04             C                   CONST(X'34')
023900000000     D F05             C                   CONST(X'35')
024000000000     D F06             C                   CONST(X'36')
024100000000     D F07             C                   CONST(X'37')
024200000000     D F08             C                   CONST(X'38')
024300000000     D F09             C                   CONST(X'39')
024400000000     D F10             C                   CONST(X'3A')
024500000000     D F11             C                   CONST(X'3B')
024600000000     D F12             C                   CONST(X'3C')
024700000000     D F13             C                   CONST(X'B1')
024800000000     D F14             C                   CONST(X'B2')
024900000000     D F15             C                   CONST(X'B3')
025000000000     D F16             C                   CONST(X'B4')
025100000000     D F17             C                   CONST(X'B5')
025200000000     D F18             C                   CONST(X'B6')
025300000000     D F19             C                   CONST(X'B7')
025400000000     D F20             C                   CONST(X'B8')
025500000000     D F21             C                   CONST(X'B9')
025600000000     D F22             C                   CONST(X'BA')
025700000000     D F23             C                   CONST(X'BB')
025800000000     D F24             C                   CONST(X'BC')
025900000000     D ENTER           C                   CONST(X'F1')
026000000000     D ROLDWN          C                   CONST(X'F4')
026100000000     D ROLLUP          C                   CONST(X'F5')
026200000000     D*-------------
026201170822R258 D* Intestazione Comunicazione Polivalente
026202170822  "  D INT1            C                   CONST(' Comunicazione Polivalente')
026203170822  "  D* Intestazione Spesometro Trimestrale
026204170822  "  D INT4            C                   CONST('     Regole Estrazione    ')
026205170822R258 D*-------------
026300000000     D* Sottotitolo di testata
026400000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
026500000000     D                                          COS0002')
026600000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
026700000000     D                                          SDC0001')
026701151120R195 D*-------------
026702151120  "  D* Sottotitolo di testata
026703151120  "  D* "Spesometro"
026704151120  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
026705151120  "  D                                          SDC0088')
026707151120  "  D* Sottotitolo di testata Black List
026708151120  "  D* "Black List"
026709151120  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
026710151120R195 D                                          SDC0089')
026711170822R258 D* Sottotitolo di testata Spesometro trimestrale
026712170822  "  D* "Black List"
026713170822  "  D SOTTITT4        C                   CONST('SDGMSG    *LIBL-
026714170822R258 D                                          SDC0114')
026800000000     D*-------------
026900000000     D* Variabili per gestione videate
027000000000     D $Gest           S              2A
027100000000     D $Fine           S              1A
027200000000     D $InzD1          S              1A
027300000000     D $InzS1          S              1A
027400000000     D $LastV          S              2A
027500000000     D $LastG          S              2A
027600000000     D $RcdOK          S              1A
027700000000     D $EndFile        S              1A
027800000000     D*-------------
027900000000     D* Variabile appoggio per n. record
028000000000     D Win             S             99A
028100000000     D*-------------
028200000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
028300000000     D S1NrR           S                   like(C1Rcd)
028400000000     D WSfl            S                   like(C1Nrr)
028500000000     D WMax            S                   like(C1Rcd)
028600000000     D Resto           S              4  0
028700000000     D Risul           S              4  0
028800000000     D WInzS1          S                   like($InzS1)
028900130823     D WOpr            S                   like(OprG70)
029000000000     D CurR            S              2S 0
029100000000     D CurC            S              2S 0
029200000000     D Wrkpos          S             15S 0
029300000000     D WINKR           S              1A
029400000000     D WExf            S              1A
029500000000     D*-------------
029600000000     D*  Indici
029700000000     D $X              S              4S 0
029800000000     D $Y              S              3S 0
029900000000     D* Indice della schiera per le opzioni personalizzate
030000000000     D PosOpz          S              2  0
030100000000     D* numero max opzioni
030200000000     D NumOpz          S              2  0 Inz($DIMOP)
030300000000     D* numero effettive opzioni
030400000000     D OpzLen          S              2  0
030500000000     D*------------
030600110830     D* Aliq + RifIva
030700110830     D AliqRifIva      DS
030800110830     D  S1Aliq
030900110830     D  S1RifIva
031000110830     D*------------
031100110830     D* S1Kcc + S1Ksc
031200110830     D KccKsc          DS
031300110830     D  S1Kcc
031400110830     D  S1Ksc
031500110830     D*------------
031600110830     D* S1TpReg S1Libro
031700110830     D TpRegLibro      DS
031800110830     D  S1TpReg
031900110830     D  S1Libro
032000110830     D*------------
032100000000     D* campi di lavoro
032200000000     D $Progr          S              4S 0
032300000000     D SavNrRiga       S                   Like(H1NrRiga)
032400130823     D ContaRighe      S                   Like(SPRNrRiga)
032500130823     D SavTpReg        S                   Like(SPRTpReg)
032600130823     D SavLibro        S                   Like(SPRLibro)
032700130823     D SavAliq         S                   Like(SPRAliq)
032800130823     D SavRifIVA       S                   Like(SPRRifIVA)
032900130823     D SavCausale      S                   Like(SPRCausale)
033000130823     D SavKcc          S                   Like(SPRKcc)
033100130823     D SavKsc          S                   Like(SPRKsc)
033200130823     D SavVoce         S                   Like(SPRVoce)
033300130823     D SavClasse       S                   Like(SPRClasse)
033400000000     D $FineS1         S              1
033500000000     D $FineLista      S              1
033600130823     D $FineSPR        S              1
033700000000     D $AlmenoUna      S              1
033800000000     D $RigaV          S              1
033900000000     D WrkTit          S                   Like(D1TipRegD)
034000000000     D LenStr          S              3  0
034100000000     D LenTit          S              3  0
034101131031R140 D*----------
034102131031 "   D WData           S               D
034103131031 "   D WAnno           S              4  0
034104131031 "   D WDtAlfa         S             10A
034105131031 "   D DataDs          DS
034106131031 "   D  AnnoDs                        4A
034107131031 "   D   Tratt1                       1A   Inz('-')
034108131031 "   D  MeseDs                        2A
034109131031 "   D   Tratt2                       1A   Inz('-')
034110131031R140 D  GiornoDs                      2A
034200000000     C*----------------------------------------------------*
034300000000     C*                MAIN LINE PROGRAM
034400000000     C*----------------------------------------------------*
034500000000     C*
034600000000     C* inizializzazione variabili
034700000000     C                   EXSR      INZVAR
034800000000     C*
034900000000     C     $FINE         DOWEQ     *OFF
035000000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
035100000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
035200000000     C                   ENDCS
035300000000     C                   ENDDO
035400000000     C* fine programma
035500000000     C                   EXSR      ENDPGM
035600000000     C*
035700000000     C/EJECT
035800000000     C************************************************************
035900000000     C* FINE PROGRAMMA
036000000000     C************************************************************
036100000000     C     ENDPGM        BEGSR
036200000000     C*
036300000000     C                   ROLBK
036400000000     C*
036500130823     C                   MOVEL     SDGSG70DS     KPJBU
036600000000     C*
036700000000     C                   SetOn                                            LR
036800000000     C                   RETURN
036900000000     C*
037000000000     C                   ENDSR
037100000000     C/EJECT
037200000000     C************************************************************
037300000000     C* RIEMPIMENTO TASTI FUNZIONE
037400000000     C************************************************************
037500000000     C     RIEKEY        BEGSR
037600000000     C*
037700000000     C* Input :
037800000000     C* - $FX = schiera con tutti i testi disponibili
037900000000     C* - $FC = schiere con i flag di validità associati ai testi
038000000000     C* - $FR = schiere con i flag di REVERSE IMAGE
038100000000     C* - $ULK = ultimo testo caricato
038200000000     C* - $L01 = Lunghezza prima riga
038300000000     C* - $L02 = Lunghezza seconda riga
038400000000     C* Output :
038500000000     C* - $ULK
038600000000     C* - $KEY1= 1° riga di tasti funzione
038700000000     C* - $KEY2= 2° riga di tasti funzione
038800000000     C*
038900000000     C                   CALL      'XKEYMSG'
039000000000     C* input
039100000000     C                   PARM                    $FX
039200000000     C                   PARM                    $FC
039300000000     C                   PARM                    $FR
039400000000     C                   PARM                    $ULK              3 0
039500000000     C* output
039600000000     C                   PARM                    $KEY1            79
039700000000     C                   PARM                    $KEY2            79
039800000000     C* input
039900000000     C                   PARM                    $L01              3 0
040000000000     C                   PARM                    $L02              3 0
040100000000     C*
040200000000     C                   PARM                    $ALLFUNCT       480
040300000000     C*
040400000000     C                   ENDSR
040500000000     C/EJECT
040600000000     C************************************************************
040700000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
040800000000     C************************************************************
040900000000     C     GESPIE        BEGSR
041000000000     C*
041100000000     C                   SELECT
041200000000     C* Parzializzazioni
041300000000     C     $GEST         WHENEQ    'D1'
041400000000     C     $ULKD1        IFEQ      0
041500000000     C* riempimento opzioni
041600000000     C                   EXSR      F24D1
041700000000     C                   ELSE
041800000000     C                   WRITE     FMTZ1
041900000000     C                   ENDIF
042000000000     C* Lista
042100000000     C     $GEST         WHENEQ    'S1'
042200000000     C     $ULKS1        IFEQ      0
042300000000     C                   EXSR      F24S1
042400000000     C                   ELSE
042500000000     C                   WRITE     FMTZ2
042600000000     C                   ENDIF
042700000000      *
042800000000     C                   ENDSL
042900000000     C*
043000000000     C                   ENDSR
043100000000     C/EJECT
043200000000     C************************************************************
043300000000     C* GESTIONE PARZIALIZZAZIONI
043400000000     C************************************************************
043500000000     C     GESD1         BEGSR
043600000000     C* inizializzazione videata
043700000000     C     $INZD1        IFEQ      *ON
043800000000     C                   EXSR      INZD1
043900000000     C                   MOVE      *OFF          $INZD1
044000000000     C                   ENDIF
044100000000     C*
044200000000     C* pulitura sottotitolo
044300000000     C                   MOVEL     *BLANK        D1TIT
044400000000     C***                MOVEL     *BLANK        T1TIT
044500000000     C* emissione piede videata se proveniente da altra
044600000000     C* salvataggio valore formato di provenienza
044700000000     C     $LASTV        IFNE      'D1'
044800000000     C* recupero il sottotitolo (se c'è)
044900000000     C                   Clear                   D1TIT
045000000000     C     SOTTITD1      IFNE      *BLANK
045100000000     C                   MOVEL     SOTTITD1      IDMSG
045200000000     C                   CALLB     'XRTVM'
045300000000     C                   PARM                    IDMSG            27
045400000000     C                   PARM                    TXTMSG          100
045500000000     C                   PARM                    ERRMSG            1
045600000000     C                   PARM      *Off          CTRMSG            1            centratura
045700000000     C                   PARM      15            LENMSG            3 0          lun output.
045800000000     C     ERRMSG        IFNE      *ON
045900000000     C                   MOVEL     TXTMSG        D1TIT
046000000000     C                   ELSE
046100000000     C                   MOVEL     *ALL'?'       D1TIT
046200000000     C                   ENDIF
046300000000     C                   ENDIF
046400000000     C* salvo stato F17 per sapere cosa fare in caso di F12
046500000000     C                   MOVE      *INKR         WINKR
046600000000     C                   WRITE     FMTT1
046700000000     C                   EXSR      GESPIE
046800000000     C                   MOVE      $LASTV        $LASTG
046900000000     C                   MOVE      'D1'          $LASTV
047000000000     C                   ENDIF
047100000000     C*
047200000000     C* se esistono errori sulla videata
047300000000     C* emetto la write del formato a indicatori spenti per vedere
047400000000     C* le eventuali decodifiche
047500000000     C     *IN99         IFEQ      *ON
047600000000     C                   MOVEA     *IN           WIN
047700000000     C                   MOVE      *ALL'0'       IN5098           49
047800000000     C                   MOVEA     IN5098        *IN(50)
047900000000     C                   WRITE     FMTD1
048000000000     C                   MOVEA     WIN           *IN
048100000000     C                   ENDIF
048200000000     C*
048300000000     C                   EXFMT     FMTD1
048400000000     C*
048500000000     C* azzero la posizione del cursore
048600000000     C                   Z-ADD     0             H1RIGA
048700000000     C                   Z-ADD     0             H1COLO
048800000000     C*
048900000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
049000000000     C* INPUT
049100000000     C* $FC1 = Abilitazioni videata D1
049200000000     C* OUTPUT
049300000000     C* *IN97 + *IN99 Errori
049400000000     C                   MOVEA     $FC1          $FC
049500000000     C                   EXSR      CTRKEY
049600000000     C*
049700000000     C     *IN99         IFEQ      *OFF
049800000000     C*
049900000000     C                   SELECT
050000000000     C* F2=Decodifica
050100000000     C     $TASTO        WHENEQ    F02
050200000000     C                   EXSR      F02D1
050300000000     C* F3=Fine
050400000000     C     $TASTO        WHENEQ    F03
050500000000     C                   EXSR      F03D1
050600000000     C* F4=Ricerca
050700000000     C     $TASTO        WHENEQ    F04
050800000000     C                   EXSR      SEARCH
050900000000     C* F10=Immissione
051000000000     C     $TASTO        WHENEQ    F10
051100000000     C                   EXSR      F10D1
051200000000     C* F12=Ritorno
051300000000     C     $TASTO        WHENEQ    F12
051400000000     C                   EXSR      F12D1
051500000000     C* F24=Altri tasti
051600000000     C     $TASTO        WHENEQ    F24
051700000000     C                   EXSR      F24D1
051800000000     C*
051900000000     C                   OTHER
052000000000     C                   EXSR      CTRD1
052100000000     C     *IN99         IFEQ      *OFF
052200000000     C* definisco l'ordinamento
052300000000     C                   EXSR      DEFORD
052400000000     C* richiamo la lista
052500000000     C                   MOVE      'S1'          $GEST
052600000000     C                   MOVE      *ON           $INZS1
052700130823     C                   MOVE      '02'          OpzG70
052800000000     C                   ENDIF
052900000000     C*
053000000000     C                   ENDSL
053100000000     C*
053200000000     C                   ENDIF
053300000000     C*
053400000000     C                   ENDSR
053500000000     C/EJECT
053600000000     C************************************************************
053700000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
053800000000     C************************************************************
053900000000     C     INZD1         BEGSR
054000000000     C*
054100000000     C                   CLEAR                   FMTD1
054200000000     C*
054300000000     C                   ENDSR
054400000000     C/EJECT
054500000000     C************************************************************
054600000000     C* GESTIONE F02 VIDEO D1
054700000000     C************************************************************
054800000000     C     F02D1         BEGSR
054900000000     C*
055000000000     C* per decodificare la videata richiamo la routine dei ctrl
055100000000     C* e poi disattivo gli eventuali messaggi di errore
055200000000     C                   EXSR      CTRD1
055300000000     C*
055400000000     C                   MOVE      *ALL'0'       IN5098
055500000000     C                   MOVEA     IN5098        *IN(50)
055600000000     C*
055700000000     C                   ENDSR
055800000000     C/EJECT
055900000000     C************************************************************
056000000000     C* GESTIONE F03 VIDEO D1
056100000000     C************************************************************
056200000000     C     F03D1         BEGSR
056300000000     C*
056400000000     C                   MOVE      *ON           $FINE
056500130823     C                   MOVE      '1'           RETG70
056600000000     C* fine programma
056700000000     C                   EXSR      ENDPGM
056800000000     C*
056900000000     C                   ENDSR
057000000000     C/EJECT
057100000000     C************************************************************
057200000000     C* GESTIONE F10 VIDEO D1
057300000000     C************************************************************
057400000000     C     F10D1         BEGSR
057500000000     C*
057600000000     C                   MOVE      'S1'          $GEST
057700000000     C*
057800000000     C                   MOVE      *HIVAL        $LASTV
057900000000     C*
058000000000     C                   ENDSR
058100000000     C/EJECT
058200000000     C************************************************************
058300000000     C* GESTIONE F12 VIDEO D1
058400000000     C************************************************************
058500000000     C     F12D1         BEGSR
058600000000     C*
058700000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
058800000000     C*  per F12 torno alla lista
058900000000     C     $LOP          IFEQ      2
059000000000     C     WINKR         OREQ      *ON
059100000000     C                   MOVE      'S1'          $GEST
059200000000     C*  altrimenti, per F12 torno a menu
059300000000     C                   ELSE
059400000000     C                   MOVE      *ON           $FINE
059500130823     C                   MOVE      '2'           RETG70
059600000000     C                   ENDIF
059700000000     C*
059800000000     C                   ENDSR
059900000000     C/EJECT
060000000000     C************************************************************
060100000000     C* GESTIONE F24 VIDEO D1
060200000000     C* input
060300000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
060400000000     C* $FX1 =Testi tasti funzione
060500000000     C* $FC1 = Abilitazioni
060600000000     C* $FR1 = Inversioni di fondo
060700000000     C************************************************************
060800000000     C     F24D1         BEGSR
060900000000     C*
061000000000     C                   MOVEA     $FX1          $FX
061100000000     C                   MOVEA     $FC1          $FC
061200000000     C                   MOVEA     $FR1          $FR
061300000000     C                   Z-ADD     $ULKD1        $ULK
061400000000     C                   Z-ADD     $D1L01        $L01
061500000000     C                   Z-ADD     $D1L02        $L02
061600000000     C                   EXSR      RIEKEY
061700000000     C                   Z-ADD     $ULK          $ULKD1            3 0
061800000000     C                   MOVEA     $FC           $FC1
061900000000     C                   MOVEL     $KEY1         Z1KE1
062000000000     C                   MOVEL     $KEY2         Z1KE2
062100000000     C                   WRITE     FMTZ1
062200000000     C*
062300000000     C*
062400000000     C                   ENDSR
062500000000     C/EJECT
062600000000     C************************************************************
062700000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
062800000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
062900000000     C* 93 94 96 97 98
063000000000     C************************************************************
063100000000     C     CTRD1         BEGSR
063200000000     C*
063300000000     C                   SETOFF                                       99
063400000000     C*
063401170822R258 X*** ex R195        If        TpComG70 = 'B'
063402170822  "  C                   Select
063403170822  "  C                   When      TpComG70 = 'T'
063404170822  "  C                   EXSR      CtrTipRegT
063405170822R258 C                   When      TpComG70 = 'B'
063406170822R195 C                   EXSR      CtrTipRegB
063407170822R258 X*** ex R195        Else
063408170822R258 C                   Other
063500000000     C                   EXSR      CtrTipReg
063501170822R258 X*** ex R195        Endif
063502170822R258 C                   Endsl
063600000000     C*
063601170822R258 C
063602170822  "  C                   Select
063603170822  "  C                   When      TpComG70 = 'T'
063604170822  "  C                   EXSR      CtrStpRegT
063605170822R258 C                   Other
063700000000     C                   EXSR      CtrStpReg
063701170822R258 C                   EndSL
063800000000     C*
063900000000     C                   ENDSR
064000000000     C/EJECT
064100000000     C***********************************************************
064200000000     C* Controllo TipReg
064300000000     C***********************************************************
064400000000     C     CtrTipReg     BEGSR
064500000000     C*
064600000000     C                   Clear                   D1TipRegD
064700000000     C*
064800000000     C                   Clear                   X07ValDs
064900000000     C                   Move      '1'           X07Ric
065000000000     C                   Move      *Off          X07All
065100130823     C                   Movel     'SDGSPR000'   X07TRc
065200130823     C                   Movel     'SPRTIPREG'   X07Cam
065300000000     C                   Movel     D1TipReg      X07VAL
065400000000     C                   Callb     'X07VALR'
065500000000     C                   Parm                    X07ValDs
065600000000     C*
065700000000     C     X07ERR        IFEQ      *Off
065800000000     C                   MOVEL     X07DES        D1TipRegD
065900000000     C                   else
066000000000     C                   SetOn                                        5099
066100000000     C                   endif
066200000000     C*
066300000000     C                   ENDSR
066301151007R195 C/EJECT
066302151007  "  C***********************************************************
066303151007  "  C* Controllo TipReg per Black List
066304151007  "  C***********************************************************
066305151007R195 C     CtrTipRegB    BEGSR
066306151007     C*
066307151007     C                   Clear                   D1TipRegD
066308151007     C*
066309151007     C                   Clear                   X07ValDs
066310151007     C                   Move      '1'           X07Ric
066311151007     C                   Move      *Off          X07All
066312151007     C                   Movel     'ANDIZ000 '   X07TRc
066313151007     C                   Movel     'SDGTIPREGB'  X07Cam
066314151007     C                   Movel     D1TipReg      X07VAL
066315151007     C                   Callb     'X07VALR'
066316151007     C                   Parm                    X07ValDs
066317151007     C*
066318151007     C     X07ERR        IFEQ      *Off
066319151007     C                   MOVEL     X07DES        D1TipRegD
066320151007     C                   else
066321151007     C                   SetOn                                        5099
066322151007     C                   endif
066323151007     C*
066324151007R195 C                   ENDSR
066325170822R258 C/EJECT
066326170822  "  C***********************************************************
066327170822  "  C* Controllo TipReg per Spesometro Trimestrale
066328170822  "  C***********************************************************
066329170822R258 C     CtrTipRegT    BEGSR
066330170822     C*
066331170822     C                   Clear                   D1TipRegD
066332170822     C*
066333170822     C                   Clear                   X07ValDs
066334170822     C                   Move      '1'           X07Ric
066335170822     C                   Move      *Off          X07All
066336170822     C                   Movel     'ANDIZ000 '   X07TRc
066337170822     C                   Movel     'SDGTIPREGT'  X07Cam
066338170822     C                   Movel     D1TipReg      X07VAL
066339170822     C                   Callb     'X07VALR'
066340170822     C                   Parm                    X07ValDs
066341170822     C*
066342170822     C     X07ERR        IFEQ      *Off
066343170822     C                   MOVEL     X07DES        D1TipRegD
066344170822     C                   else
066345170822     C                   SetOn                                        5099
066346170822     C                   endif
066347170822     C*
066348170822R258 C                   ENDSR
066400000000     C/EJECT
066500000000     C***********************************************************
066600000000     C* Controllo StpReg
066700000000     C***********************************************************
066800000000     C     CtrStpReg     BEGSR
066900000000     C*
067000000000     C                   Clear                   D1StpRegD
067100000000     C*
067200000000     C                   Clear                   X07ValDs
067300000000     C                   Move      '1'           X07Ric
067400000000     C                   Move      *Off          X07All
067500000000     C                   Movel     'ANDIZ000'    X07TRc
067600000000     C                   select
067700000000     C                   when      D1TipReg = '1'
067800130823     C                   Movel     'SDGSTPREG1'  X07Cam
067900000000     C                   when      D1TipReg = '2'
068000130823     C                   Movel     'SDGSTPREG2'  X07Cam
068100000000     C                   when      D1TipReg = '3'
068200130823     C                   Movel     'SDGSTPREG3'  X07Cam
068300130823     C                   when      D1TipReg = '4'
068301151222R195 C                   If        TpComG70 = 'B'
068302151222 "   C                   Movel     'SDGSTPRE4B'  X07Cam
068303151222R195 C                   Else
068400130823     C                   Movel     'SDGSTPREG4'  X07Cam
068401151222R195 C                   Endif
068402151007R195 C                   when      D1TipReg = '5'
068403151007R195 C                   Movel     'SDGSTPREG5'  X07Cam
068404170822     C                   endsl
068405170822     C                   Movel     D1StpReg      X07VAL
068406170822     C                   Callb     'X07VALR'
068407170822     C                   Parm                    X07ValDs
068408170822     C*
068409170822     C     X07ERR        IFEQ      *Off
068410170822     C                   MOVEL     X07DES        D1StpRegD
068411170822     C                   else
068412170822     C                   SetOn                                        5199
068413170822     C                   endif
068414170822     C*
068415170822     C                   ENDSR
068416170822     C/EJECT
068417170822R258 C***********************************************************
068418170822  "  C* Controllo StpReg
068419170822  "  C***********************************************************
068420170822R258 C     CtrStpRegT    BEGSR
068421170822     C*
068422170822     C                   Clear                   D1StpRegD
068423170822     C*
068424170822     C                   Clear                   X07ValDs
068425170822     C                   Move      '1'           X07Ric
068426170822     C                   Move      *Off          X07All
068427170822     C                   Movel     'ANDIZ000'    X07TRc
068428170822     C                   select
068429170822     C                   when      D1TipReg = '1'
068430170822     C                   Movel     'SDGSTPRE1T'  X07Cam
068431170822     C                   when      D1TipReg = '2'
068432170822     C                   Movel     'SDGSTPRE2T'  X07Cam
068433170822     C                   when      D1TipReg = '4'
068434170822     C                   Movel     'SDGSTPRE4T'  X07Cam
068435170822     C                   when      D1TipReg = '6'
068436170822     C                   Movel     'SDGSTPRE6T'  X07Cam
068500000000     C                   endsl
068600000000     C                   Movel     D1StpReg      X07VAL
068700000000     C                   Callb     'X07VALR'
068800000000     C                   Parm                    X07ValDs
068900000000     C*
069000000000     C     X07ERR        IFEQ      *Off
069100000000     C                   MOVEL     X07DES        D1StpRegD
069200000000     C                   else
069300000000     C                   SetOn                                        5199
069400000000     C                   endif
069500000000     C*
069501170822R258 C                   ENDSR
069502170822R258 C/EJECT
069800130823     C************************************************************
069900110830     C* GESTIONE LISTA
070000110830     C************************************************************
070100110830     C     GESS1         BEGSR
070200110830     C*
070300000000     C* inizializzazione videata
070400000000     C     $InzS1        IFEQ      *ON
070500000000     C                   EXSR      INZS1
070600000000     C                   MOVE      *OFF          $INZS1
070700000000     C                   ENDIF
070800000000     C*
070900000000     C* emissione piede videata se proveniente da altra
071000000000     C* salvataggio valore formato di provenienza
071100000000     C     $LASTV        IFNE      'S1'
071200000000     C*
071300000000     C* riempimento sottotitolo
071400000000     C     SOTTIT        IFNE      *BLANK
071500000000     C                   MOVEL     SOTTIT        IDMSG
071600000000     C                   CALLB     'XRTVM'
071700000000     C                   PARM                    IDMSG            27
071800000000     C                   PARM                    TXTMSG          100
071900000000     C                   PARM                    ERRMSG            1
072000000000     C                   PARM      *OFF          CTRMSG            1            centratura
072100000000     C                   PARM      30            LENMSG            3 0          lun output.
072200000000     C     ERRMSG        IFNE      *ON
072300000000     C                   MOVEL     TXTMSG        D1TIT
072400000000     C                   ELSE
072500000000     C                   MOVEL     *ALL'?'       D1TIT
072600000000     C                   ENDIF
072700000000     C                   ENDIF
072800000000      *
072900000000     C                   WRITE     FMTT1
073000000000     C                   EXSR      GESPIE
073100000000     C*                  MOVE      $LASTV        $LASTG
073200000000     C                   MOVE      'S1'          $LASTV
073300000000     C                   ENDIF
073400000000     C*
073500000000     C                   Eval      *In09       = C1Mode
073600000000     C*
073700000000     C* Errori
073800000000   B1C     *IN99         IFEQ      *ON
073900000000     C                   MOVEA     *IN           WIN
074000000000     C                   MOVE      *ALL'0'       IN5098           49
074100000000     C                   MOVEA     IN5098        *IN(50)
074200000000     C                   WRITE     FMTC1
074300000000     C                   MOVEA     WIN           *IN
074400000000   E1C                   ENDIF
074500000000     C*
074600000000   B1C                   SELECT
074700000000     C* Annullamento
074800130823     C     OPZG70        WHENEQ    '04'
074900000000     C* Visualizzazione
075000130823     C     OPZG70        OREQ      '05'
075100000000     C                   WRITE     FMTC1
075200000000     C                   EXFMT     PROTECT
075300000000     C* Immissione/Modifica/Copia
075400000000   X1C                   OTHER
075500000000     C                   EXFMT     FMTC1
075600000000   E1C                   ENDSL
075700000000     C*
075800000000     C*
075900000000     C* inizializzazione indice opzioni personalizzate
076000000000     C                   Eval      PosOpz      = 0
076100000000     C                   if        SflNrR     <> 0
076200000000     C                   Z-Add     SflNrR        C1Rcd
076300000000     C                   endif
076400000000     C*
076500000000     C* CONTROLLO ABILITAZIONE TASTI
076600000000     C* INPUT
076700000000     C* $FC2 = Abilitazioni videata S1
076800000000     C* OUTPUT
076900000000     C* *IN97 + *IN99 Errori
077000000000     C                   MOVEA     $FC2          $FC
077100000000     C                   EXSR      CTRKEY
077200000000     C*
077300000000     C     *IN99         IFEQ      *OFF
077400000000     C*
077500000000     C                   SELECT
077600000000     C* F3=Fine
077700000000     C     $TASTO        WHENEQ    F03
077800000000     C                   EXSR      F03S1
077900000000     C* F4=Ricerca
078000000000     C     $TASTO        WHENEQ    F04
078100000000     C                   EXSR      SEARCH
078200000000     C* F5=Ripristina
078300000000     C     $TASTO        WHENEQ    F05
078400000000     C                   EXSR      F05S1
078500000000     C* F8=Successivo
078600000000     C     $TASTO        WHENEQ    F08
078700000000     C                   EXSR      F08S1
078800000000     C* F12=Ritorno
078900000000     C     $TASTO        WHENEQ    F12
079000000000     C                   EXSR      F12S1
079100000000     C* F24=Altri tasti
079200000000     C     $TASTO        WHENEQ    F24
079300000000     C                   EXSR      F24S1
079400000000     C* ROLL-UP
079500000000     C     $TASTO        WHENEQ    ROLLUP
079600000000     C                   EXSR      ROLS1
079700000000     C*
079800000000     C                   OTHER
079900000000     C*
080000000000     C* Controllo dati nella testata subfile
080100000000     C                   EXSR      CTRC1
080200000000     C*
080300000000     C* Controllo dati nella lista subfile
080400000000     C                   EXSR      CTRS1
080500000000     C*
080600000000     C     $TASTO        IFEQ      F06
080700000000     C     *IN99         ANDEQ     *OFF
080800000000     C*
080900000000     C* Salvataggio e controllo
081000000000     C                   EXSR      AGGANA
081100000000     C                   ENDIF
081200000000     C*
081300000000     C                   ENDSL
081400000000     C*
081500000000     C                   ENDIF
081600000000     C*
081700000000     C                   ENDSR
081800000000     C/EJECT
081900000000     C************************************************************
082000000000     C* INIZIALIZZAZIONE LISTA
082100000000     C************************************************************
082200000000     C     INZS1         BEGSR
082300000000     C* pulizia SFL
082400000000     C                   SETOFF                                         3031
082500000000     C                   WRITE     FMTC1
082600000000     C                   SETON                                          3031
082700000000     C*
082800000000     C                   Z-ADD     0             S1NRR
082900000000     C                   Z-ADD     0             WMAX
083000000000     C*
083100000000     C* Riempimento testata
083200000000     C                   EXSR      RIEC1
083300000000     C*
083400000000     C* Imposto gli attributi del subfile control
083500000000     C                   EXSR      ATRC1
083600000000     C*
083700000000     C*
083800000000     C* carico tutte le righe di dettaglio, fino al max del sfl
083900000000     C                   Eval      SflPag      = 9999
084000000000     C*
084100130823     C                   EXSR      ReadSDGSPR
084200000000     C*
084300000000     C* Caricamento pagina intera vuota
084400000000     C                   EXSR      ROLS1
084500000000     C                   Z-ADD     1             C1RCD
084600000000     C*
084700000000     C                   ENDSR
084800000000     C/EJECT
084900000000     C************************************************************
085000000000     C* Riempimento dati di testata
085100000000     C************************************************************
085200000000     C     RIEC1         BEGSR
085300000000     C*
085400000000     C                   eval      C1Tit = %trim(D1TipRegD) + ' - ' +
085500000000     C                                     %trim(D1StpRegD)
085600000000     C*
085700000000     C                   ENDSR
085800000000     C/EJECT
085900000000     C************************************************************
086000000000     C* CARICAMENTO PAGINA VUOTA
086100000000     C************************************************************
086200000000     C     ROLS1         BEGSR
086300000000     C*
086400000000     C                   SETOFF                                       32
086500000000     C*
086600000000     C                   Z-ADD     WMAX          S1NRR
086700000000     C*
086800000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
086900000000     C                   Clear                   FMTS1
087000000000     C*
087100000000     C* Carico righe rimanenti o pagina vuota
087200000000     C     S1NRR         DIV       SFLPAG        RISUL
087300000000     C                   MVR                     RESTO
087400000000     C                   ADD       1             RESTO
087500000000     C*
087600000000     C     RESTO         DO        SFLPAG        RESTO
087700000000     C                   ADD       1             S1NRR
087800000000     C                   Z-ADD     S1Nrr         H1NrRiga
087900000000     C* Attributi sfl
088000000000     C                   EXSR      ATRS1
088100000000     C                   WRITE     FMTS1
088200000000     C                   Clear                   FMTS1
088300000000     C                   ENDDO
088400000000     C*
088500000000     C                   Z-ADD     S1NRR         WMAX
088600000000     C                   Z-ADD     S1NRR         C1RCD
088700000000      *
088800000000      * POSIZIONAMENTO AL 1° RCD DELLA PAGINA
088900000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
089000000000     C                   MVR                     RESTO
089100000000     C     PAGINE        MULT      SFLPAG        C1RCD
089200000000     C     RESTO         IFGT      0
089300000000     C                   ADD       1             C1RCD
089400000000     C                   ELSE
089500000000     C                   SUB       SFLPAG        C1RCD
089600000000     C                   ADD       1             C1RCD
089700000000     C                   ENDIF
089800000000     C*
089900000000     C                   ENDSR
090000000000     C/EJECT
090100000000     C************************************************************
090200000000     C* Lettura associazioni
090300000000     C************************************************************
090400130823     C     ReadSDGSPR    BEGSR
090500000000     C*
090600000000     C                   MOVEL     *OFF          $EndFile
090700000000     C                   MOVEL     *OFF          $RCDOK
090800000000     C                   Z-ADD     0             $Y
090900000000     C*
091000130823     C                   Eval      SPRSocieta  = XScSoc
091100130823     C                   Eval      SPRTipReg   = D1TipReg
091200130823     C                   Eval      SPRStpReg   = D1StpReg
091300130823     C     K03SPR01      SETLL     SDGSPR01L
091400000000     C*
091500000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
091600130823     C     OPZG70        IFEQ      '02'
091700130823     C     OPZG70        OREQ      '04'
091800130823     C     K03SPR01      READE     SDGSPR01L                              21
091900000000     C                   else
092000130823     C     K03SPR01      READE(N)  SDGSPR01L                              21
092100000000     C                   endif
092200000000     C*
092300000000     C                   Eval      $EndFile    = *In21
092400000000     C*
092500000000     C                   DOW       $EndFile    = *Off    and
092600000000     C                             $Y          < SflPag
092700000000     C*
092800000000     C                   EXSR      SELANA
092900000000     C*
093000000000     C                   ADD       1             S1NrR
093100000000     C                   EXSR      RIES1
093200000000     C* Attributi sfl
093300000000     C                   EXSR      ATRS1
093400000000     C*
093500000000     C* attivo sempre sflnxtchg
093600000000     C                   Move      *ON           *IN32
093700000000     C                   WRITE     FMTS1
093800000000     C*
093900000000     C                   ADD       1             $Y
094000000000     C*
094100000000     C*
094200000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
094300130823     C     OPZG70        IFEQ      '02'
094400130823     C     OPZG70        OREQ      '04'
094500130823     C     K03SPR01      READE     SDGSPR01L                              21
094600000000     C                   else
094700130823     C     K03SPR01      READE(N)  SDGSPR01L                              21
094800000000     C                   endif
094900000000     C*
095000000000     C                   Eval      $EndFile    = *In21
095100000000     C                   ENDDO
095200000000     C*
095300000000     C*
095400000000     C                   Z-ADD     S1Nrr         WMax
095500000000     C*
095600000000     C* Reimposto il numero di righe per pagina al valore iniziale
095700000000     C                   Reset                   SflPag
095800000000     C*
095900000000     C                   ENDSR
096000000000     C/EJECT
096100000000     C************************************************************
096200000000     C* SELEZIONI SULLA RIGA LETTA
096300000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
096400000000     C* non rientra nei parametri di parzializzazione
096500000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
096600000000     C* superiore stabilito nei parametri
096700000000     C*
096800000000     C************************************************************
096900000000     C     SELANA        BEGSR
097000000000     C*
097100000000     C                   MOVEL     *ON           $RCDOK
097200000000     C*
097300000000     C                   ENDSR
097400000000     C/EJECT
097500000000     C************************************************************
097600000000     C* RIEMPIMENTO RIGA LISTA
097700000000     C************************************************************
097800000000     C     RIES1         BEGSR
097900000000     C*
098000000000     C                   CLEAR                   FMTS1
098100000000     C*
098200000000     C                   Z-ADD     S1Nrr         H1NrRiga
098300000000     C*
098400130823     C                   Eval      S1TpReg     = SPRTpReg
098500130823     C                   MoveL     SPRLibro      S1Libro
098600130823     C                   Z-Add     SPRAliq       S1Aliq
098700130823     C                   Eval      S1RifIVA    = SPRRifIVA
098800130823     C                   Eval      S1Causale   = SPRCausale
098900130823     C                   Eval      S1Nota      = SPRNota
099000130823     C                   Eval      S1Kcc       = SPRKcc
099100130823     C                   Eval      S1Ksc       = SPRKsc
099200130823     C                   Eval      S1Voce      = SPRVoce
099300130823     C                   Eval      S1Classe    = SPRClasse
099400000000     C*
099500000000     C                   ENDSR
099600000000     C/EJECT
099700000000     C************************************************************
099800000000     C* RIPRISTINA GLI ATTRIBUTI DELLA TESTATA DEL SFL
099900000000     C************************************************************
100000000000     C     ATRC1         BEGSR
100100000000     C*
100200000000     C*
100300000000     C                   ENDSR
100400000000     C/EJECT
100500000000     C************************************************************
100600000000     C* CONTROLLO TESTATA LISTA
100700000000     C************************************************************
100800000000     C     CTRC1         BEGSR
100900000000     C*
101000000000     C                   MOVE      *OFF          *IN99
101100000000     C*
101200000000     C*
101300000000     C                   ENDSR
101400000000     C/EJECT
101500000000     C************************************************************
101600000000     C* CONTROLLO DI RIGHE DEL SUBFILE
101700000000     C************************************************************
101800000000     C     CTRS1         BEGSR
101900000000     C*
102000000000     C                   Eval      $AlmenoUna  = *Off
102100000000     C                   Eval      $RigaV      = *Off
102200000000     C*
102300000000     C*
102400000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
102500000000     C*
102600130823     C     OPZG70        IFNE      '04'
102700000000     C*
102800000000     C                   Eval      $Progr      = 1
102900000000     C     $Progr        CHAIN     FMTS1                              21
103000000000     C                   Eval      $FineLista  = *In21
103100000000     C*
103200000000     C* esce se fine sfl o errore che richiede l'uscita
103300000000     C                   DOW       $FineLista  = *Off
103400000000     C*
103500000000     C* Ctrl su riga
103600000000     C                   EXSR      RECS1
103700000000     C*
103800000000     C* se rilevato errore lo segnalo e
103900000000     C     *IN99         IFEQ      *ON
104000000000     C* porto il cursore sulla riga
104100000000     C                   Z-ADD     S1NrR         C1Rcd
104200000000     C                   Eval      $FineLista  = *On
104300000000     C                   ENDIF
104400000000     C*
104500000000     C* imposto gli attributi
104600000000     C                   EXSR      ATRS1
104700000000     C*
104800000000     C* Attivo sempre sflnxtchg
104900000000     C                   MOVE      *ON           *IN32
105000000000     C                   UPDATE    FMTS1
105100000000     C                   MOVE      *OFF          *IN32
105200000000     C*
105300000000     C*
105400000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
105500000000     C                   if        $FineLista  = *Off
105600000000     C                   Add       1             $Progr
105700000000     C     $Progr        CHAIN     FMTS1                              21
105800000000     C                   Eval      $FineLista  = *In21
105900000000     C                   endif
106000000000     C                   ENDDO
106100000000     C*
106200000000     C                   ENDIF
106300000000     C*
106400000000     C                   ENDSR
106500000000     C/EJECT
106600000000     C************************************************************
106700000000     C* CONTROLLO CAMPI I/O RIGA LISTA
106800000000     C************************************************************
106900000000     C     RECS1         BEGSR
107000000000     C*
107100000000     C* Reset indicatori DSPATR(PC)
107200000000     C                   MOVE      *ALL'0'       IN4049           10
107300000000     C                   MOVEA     IN4049        *IN(40)
107400000000     C*
107500000000     C* controllo che almeno un campo sia valorizzato
107600000000     C                   if        S1TpReg    <> *Blank   or
107700000000     C                             S1Libro    <> *Blank   or
107800000000     C                             S1Aliq     <> *Zero    or
107900000000     C                             S1RifIva   <> *Blank   or
108000000000     C                             S1Causale  <> *Blank   or
108100000000     C                             S1Nota     <> *Blank   or
108200000000     C                             S1Kcc      <> *Blank   or
108300000000     C                             S1Ksc      <> *Blank   or
108400000000     C                             S1Voce     <> *Blank   or
108500000000     C                             S1Classe   <> *Blank
108600000000     C*
108700000000     C                   Eval      $AlmenoUna  = *On
108800000000     C*
108900000000     C* controllo se riga già inserita con gli stessi valori
109000000000     C                   if        H1NrRiga    > 1
109100000000     C                   EXSR      CtrRigaDoppia
109200000000     C                   endif
109300000000     C*
109400000000     C                   if        D1TipReg = '1' or
109500000000     C                             D1TipReg = '2'
109600000000     C                   EXSR      CtrRigaEsiste
109700000000     C                   endif
109800000000     C*
109900000000     C                   EXSR      CtrTpReg
110000000000     C*
110100000000     C                   EXSR      CtrLibro
110200000000     C*
110300000000     C                   EXSR      CtrAliq_Rif
110400000000     C*
110500000000     C                   if        S1TpReg    <> *Blank       and
110600000000     C                             S1Libro    <> *Blank       and
110700000000     C                             (S1Aliq    <> 0       or
110800000000     C                              S1RifIVA  <> *Blank     ) and
110900000000     C                             *In99       = *Off
111000000000     C                   EXSR      CtrAssoc
111100000000     C                   endif
111200000000     C*
111300110830     C                   EXSR      CtrTpRegole
111400110830     C*
111500000000     C                   EXSR      CtrCausale
111600000000     C*
111700000000     C                   EXSR      CtrS1Nota
111800000000     C*
111900000000     C                   EXSR      CtrConto
112000000000     C*
112100000000     C                   EXSR      CtrVoce
112200000000     C*
112300000000     C                   EXSR      CtrClasse
112400000000     C*
112500000000     C                   else
112600000000     C*
112700000000     C                   if        $RigaV      = *Off
112800000000     C                   Eval      $RigaV      = *On
112900000000     C                   endif
113000000000     C*
113100000000     C                   endif
113200000000     C*
113300000000     C                   ENDSR
113400000000     C/EJECT
113500000000     C************************************************************
113600000000     C* CONTROLLO riga doppia
113700000000     C************************************************************
113800000000     C     CtrRigaDoppia BEGSR
113900000000     C*
114000000000     C* salvo i dati dell'ultima riga letta
114100000000     C                   Eval      SavNrRiga   = H1NrRiga
114200000000     C                   Eval      SavTpReg    = S1TpReg
114300000000     C                   Eval      SavLibro    = S1Libro
114400000000     C                   Eval      SavAliq     = S1Aliq
114500000000     C                   Eval      SavRifIVA   = S1RifIVA
114600000000     C                   Eval      SavCausale  = S1Causale
114700000000     C                   Eval      SavKcc      = S1Kcc
114800000000     C                   Eval      SavKsc      = S1Ksc
114900000000     C                   Eval      SavVoce     = S1Voce
115000000000     C                   Eval      SavClasse   = S1Classe
115100000000     C*
115200000000     C* mi posiziono sulla 1° riga e scorro la lista fino a che non ne
115300000000     C* trovo una con gli stessi valori oppure non arrivo alla riga
115400000000     C* corrente
115500000000     C                   Eval      $Progr      = 1
115600000000     C     $Progr        CHAIN     FMTS1                              21
115700000000     C                   Eval      $FineS1     = *In21
115800000000     C*
115900000000     C                   DOW       $FineS1     = *OFF         and
116000000000     C                             H1NrRiga    < SavNrRiga
116100000000     C*
116200000000     C                   if        SavTpReg    = S1TpReg      and
116300000000     C                             SavLibro    = S1Libro      and
116400000000     C                             SavAliq     = S1Aliq       and
116500000000     C                             SavRifIVA   = S1RifIVA     and
116600000000     C                             SavCausale  = S1Causale    and
116700000000     C                             SavKcc      = S1Kcc        and
116800000000     C                             SavKsc      = S1Ksc        and
116900000000     C                             SavVoce     = S1Voce       and
117000000000     C                             SavClasse   = S1Classe
117100000000     C*
117200000000     C* mi riposiziono sulla riga corrente
117300000000     C                   Eval      $Progr      = SavNrRiga
117400000000     C     $Progr        CHAIN     FMTS1                              21
117500000000     C*
117600000000     C* riga doppia
117700000000     C                   SetOn                                        5999
117800130823     C*
117900130823     C                   select
118000130823     C*
118100130823     C*-----------------
118200130823     C* definizione dell'operazione
118300130823     C* 1=Reverse charge
118400130823     C* 2=Regime del margine
118401170822R258 C* 3=Esclusione art. 15(N1)
118402170822 "   C* 4=IVA assolta in altro Stato(N7)
118403170822R258 C* 5=Scissione dei pagamenti
118500130918     C                   when      D1TipReg = '1'     and
118600130823     C                             (D1StpReg = '1' or
118700130918     C                              D1StpReg = '2'   )
118800130823     C* TPREG/LIBRO IVA/ALIQ/
118900130823     C                   SetOn                                        404142
119000130823     C* RIF.IVA/CAUSALE/CLI-FOR
119100130823     C                   SetOn                                        434446
119200130823     C*
119300130823     C*-----------------
119400130823     C* natura dell'operazione
119500130823     C* 1=Beni
119600130823     C* 2=Servizi
119700130918     C                   when      D1TipReg = '2'     and
119800000000     C                             (D1StpReg = '1' or
119900000000     C                              D1StpReg = '2')
120000130823     C* CAUSALE/CONTROPARTITA/VOCE
120100110708     C                   SetOn                                        444647
120200130823     C*
120300130823     C*-----------------
120400130823     C* esclusioni
120500130823     C* 1=Causale
120600130918     C                   when      D1TipReg = '3'     and
120700000000     C                             D1StpReg = '1'
120800130823     C* CAUSALE
120900000000     C                   SetOn                                        44
121000130823     C*
121100130823     C* 2=Contropartita
121200130918     C                   when      D1TipReg = '3'     and
121300000000     C                             D1StpReg = '2'
121400130823     C* C/CONTROPARTITA
121500000000     C                   SetOn                                        46
121600130823     C*
121700130823     C* 3=Libro IVA
121800130918     C                   when      D1TipReg = '3'     and
121900000000     C                             D1StpReg = '3'
122000130823     C* TIPO REGISTRO/LIBRO IVA
122100000000     C                   SetOn                                        4041
122200130823     C*
122300130823     C* 4=Classe
122400130918     C                   when      D1TipReg = '3'     and
122500000000     C                             D1StpReg = '4'
122600130823     C* CLASSE
122700000000     C                   SetOn                                        48
122800130823     C*
122801151023R195 C* esclusioni
122802151023  "  C* 1=Causale
122803151008  "  C                   when      D1TipReg = '5'     and
122804151008  "  C                             D1StpReg = '1'
122805151008  "  C* CAUSALE
122806151008  "  C                   SetOn                                        44
122807151008  "  C*
122808151008  "  C* 2=Contropartita
122809151008  "  C                   when      D1TipReg = '5'     and
122810151008  "  C                             D1StpReg = '2'
122811151008  "  C* C/CONTROPARTITA
122812151008  "  C                   SetOn                                        46
122813151008  "  C*
122814151008  "  C* 3=Libro IVA
122815151008  "  C                   when      D1TipReg = '5'     and
122816151008  "  C                             D1StpReg = '3'
122817151008  "  C* TIPO REGISTRO/LIBRO IVA
122818151008  "  C                   SetOn                                        4041
122819151008  "  C*
122820151008  "  C* 4=Classe
122821151008  "  C                   when      D1TipReg = '5'     and
122822151008  "  C                             D1StpReg = '4'
122823151008  "  C* CLASSE
122824151008  "  C                   SetOn                                        48
122825151008R195 C*
122826170822R258 C* esclusioni  per spesometro trimestrale
122827170822  "  C* 1=Causale
122828170822  "  C                   when      D1TipReg = '6'     and
122829170822  "  C                             D1StpReg = '1'
122830170822  "  C* CAUSALE
122831170822  "  C                   SetOn                                        44
122832170822  "  C*
122833170822  "  C* 2=Contropartita
122834170822  "  C                   when      D1TipReg = '6'     and
122835170822  "  C                             D1StpReg = '2'
122836170822  "  C* C/CONTROPARTITA
122837170822  "  C                   SetOn                                        46
122838170822  "  C*
122839170822  "  C* 3=Libro IVA
122840170822  "  C                   when      D1TipReg = '6'     and
122841170822  "  C                             D1StpReg = '3'
122842170822  "  C* TIPO REGISTRO/LIBRO IVA
122843170822  "  C                   SetOn                                        4041
122844170822  "  C*
122845170822  "  C* 4=Classe
122846170822  "  C                   when      D1TipReg = '6'     and
122847170822  "  C                             D1StpReg = '4'
122848170822  "  C* CLASSE
122849170822  "  C                   SetOn                                        48
122850170822R258 C*
122900130823     C*-----------------
123000130823     C* tipo documento
123100130823     C* 1=Nota di addebito
123200130918     C                   when      D1TipReg = '4'     and
123300130823     C                             D1StpReg = '1'
123400130823     C* CAUSALE
123500130823     C                   SetOn                                        44
123600130823     C*
123700130823     C* 2=Documento riepilogativo
123800130918     C                   when      D1TipReg = '4'     and
123900130823     C                             D1StpReg = '2'
124000130823     C* CAUSALE/CLI-FOR
124100130823     C                   SetOn                                        4446
124200130828     C*
124300130828     C* 3=Noleggio/leasing
124400130918     C                   when      D1TipReg = '4'     and
124500130828     C                             D1StpReg = '3'
124600130828     C* CAUSALE/CLI-FOR
124700130828     C                   SetOn                                        4446
124800130823     C*
124900000000     C                   endsl
125000000000     C*
125100130823     C*
125200000000     C* porto il cursore sulla riga
125300000000     C                   Z-ADD     S1NrR         C1Rcd
125400000000     C*
125500000000     C* dico di uscire dal ciclo
125600000000     C                   Eval      $FineS1     = *On
125700000000     C                   ITER
125800000000     C                   endif
125900000000     C*
126000000000     C*
126100000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
126200000000     C                   Add       1             $Progr            4 0
126300000000     C     $Progr        CHAIN     FMTS1                              21
126400000000     C                   Eval      $FineS1     = *In21
126500000000     C                   ENDDO
126600000000     C*
126700000000     C                   ENDSR
126800000000     C/EJECT
126900000000     C************************************************************
127000000000     C* CONTROLLO se è già stata inserita una riga con stesse
127100000000     C* impostazioni
127200000000     C************************************************************
127300000000     C     CtrRigaEsiste BEGSR
127400000000     C*
127500000000     C* imposto i campi chiave e verifico l'esistenza del record
127600000000     C                   if        S1TpReg    <> *Blank   or
127700000000     C                             S1Libro    <> *Blank   or
127800000000     C                             S1Aliq     <> *Zero    or
127900000000     C                             S1RifIVA   <> *Blank   or
128000000000     C                             S1Causale  <> *Blank   or
128100000000     C                             S1Nota     <> *Blank   or
128200000000     C                             S1Kcc      <> *Blank   or
128300000000     C                             S1Ksc      <> *Blank   or
128400000000     C                             S1Voce     <> *Blank   or
128500000000     C                             S1Classe   <> *Blank
128600000000     C*
128700130823     C                   MOVEL     XScSoc        SPRSocieta
128800130823     C                   MOVEL     D1TipReg      SPRTiPreg
128900000000     C                   if        D1StpReg  = '1'
129000130823     C                   MOVEL     '2'           SPRStpReg
129100000000     C                   else
129200130823     C                   MOVEL     '1'           SPRStpReg
129300000000     C                   endif
129400130823     C                   MOVEL     S1TpReg       SPRTpReg
129500130823     C                   MOVEL     S1Libro       SPRLibro
129600130823     C                   MOVEL     S1Aliq        SPRAliq
129700130823     C                   MOVEL     S1RifIVA      SPRRifIVA
129800130823     C                   MOVEL     S1Causale     SPRCausale
129900130823     C                   MOVEL     S1Kcc         SPRKcc
130000130823     C                   MOVEL     S1Ksc         SPRKsc
130100130823     C                   MOVEL     S1Voce        SPRVoce
130200130823     C                   MOVEL     S1Classe      SPRClasse
130300130823     C     K12SPR04      SETLL     SDGSPR04L                              21
130400130823     C     K12SPR04      READE     SDGSPR04L                              21
130500130823     C                   Eval      $FineSPR    = *In21
130600000000     C*
130700130823     C                   DOW       $FineSPR    = *Off
130800000000     C*
130900000000     C                   SETON                                        6099
131000000000     C                   SetOn                                        404142
131100000000     C                   SetOn                                        4344
131200110708     C                   SetOn                                        46
131300110708     C                   if        D1TipReg  = '2'
131400110708     C                   SetOn                                        47
131500110708     C                   endif
131600000000     C*
131700130823     C     K12SPR04      READE     SDGSPR04L                              21
131800130823     C                   Eval      $FineSPR    = *In21
131900000000     C                   ENDDO
132000000000     C*
132100000000     C                   endif
132200000000     C*
132300000000     C                   ENDSR
132400000000     C/EJECT
132500000000     C************************************************************
132600000000     C* CONTROLLO Tipo registro
132700000000     C************************************************************
132800000000     C     CtrTpReg      BEGSR
132900000000     C*
133000000000     C                   Select
133100000000     C*
133200000000     C*
133300000000     C                   other
133400000000     C*
133500000000     C                   if        S1TpReg     = *Blank
133600000000     C*
133700000000     C                   else
133800000000     C*
133900000000     C                   Clear                   X07ValDs
134000000000     C*
134100000000     C                   Move      '1'           X07Ric
134200000000     C                   Movel     'ANDLI000'    X07TRc
134300000000     C                   Movel     'DLITPREG '   X07Cam
134400000000     C                   Movel     S1TpReg       X07VAL
134500000000     C*
134600000000     C                   Callb     'X07VALR'
134700000000     C                   Parm                    X07ValDs
134800000000     C*
134900000000     C     X07ERR        IFEQ      *Off
135000000000     C                   else
135100000000     C                   SetOn                                        6299
135200000000     C                   SetOn                                        40
135300000000     C                   endif
135400000000     C*
135500000000     C                   endif
135600000000     C*
135700000000     C                   endSl
135800000000     C*
135900110825     C                   If        S1TpReg       > *blanks  and
136000110825     C                             S1Libro       = *blanks
136100110825     C                   SetOn                                        7499
136200110825     C                   EndIf
136300110825     C*
136400000000     C                   ENDSR
136500000000     C/EJECT
136600000000     C************************************************************
136700000000     C* CONTROLLO libro (esistente nel registro)
136800000000     C************************************************************
136900000000     C     CtrLibro      BEGSR
137000000000     C*
137100000000     C                   Select
137200000000     C*
137300000000     C*
137400000000     C                   other
137500000000     C*
137600000000     C                   if        S1Libro     = *Blank
137700000000     C*
137800000000     C                   if        S1TpReg    <> *Blank
137900000000     C                   endif
138000000000     C*
138100000000     C                   else
138200000000     C*
138300000000     C                   Eval      DLISocieta  = XScSoc
138400000000     C                   Eval      DLITpReg    = S1TpReg
138500000000     C                   Eval      DLILibro    = S1Libro
138600000000     C     K03DLI01      CHAIN     ANDLI01L                           21
138700000000     C*
138800000000     C                   if        *In21       = *On
138900000000     C                   SetOn                                        6499
139000000000     C                   SetOn                                        4041
139100000000     C                   endif
139200000000     C*
139300000000     C                   endif
139400000000     C*
139500000000     C                   endSl
139600000000     C*
139700110825     C                   If        S1Libro       > *blanks  and
139800110825     C                             S1TpReg       = *blanks
139900110825     C                   SetOn                                        7499
140000110825     C                   EndIf
140100110825     C*
140200000000     C                   ENDSR
140300000000     C/EJECT
140400000000     C************************************************************
140500000000     C* CONTROLLO Aliquota e riferimento IVA
140600000000     C************************************************************
140700000000     C     CtrAliq_Rif   BEGSR
140800000000     C*
140900000000     C                   Eval      RIVSocieta  = XScSoc
141000000000     C                   Eval      RIVAliq     = S1Aliq
141100000000     C                   Eval      RIVRifIVA   = S1RifIVA
141200000000     C*
141300000000     C* controllo esistenza aliquota
141400000000     C                   if        S1Aliq     <> 0
141500000000     C*
141600000000     C     K02RIV02      CHAIN     ANRIV02L                           21
141700000000     C                   if        *In21       = *On
141800000000     C                   SetOn                                        5799
141900000000     C                   SetOn                                        42
142000000000     C                   endif
142100000000     C                   endif
142200000000     C*
142300000000     C* controllo esistenza codice di esenzione
142400000000     C                   if        S1RifIVA   <> *Blank
142500000000     C*
142600000000     C     K02RIV03      CHAIN     ANRIV03L                           21
142700000000     C                   if        *In21       = *On
142800000000     C                   SetOn                                        5299
142900000000     C                   SetOn                                        43
143000000000     C                   endif
143100000000     C                   endif
143200000000     C*
143300000000     C                   if        S1Aliq     <> *Zero    and
143400000000     C                             S1RifIVA   <> *Blank
143500000000     C* sono in alternativa
143600000000     C                   SetOn                                        5499
143700000000     C                   SetOn                                        4243
143800000000     C                   endif
143900000000     C*
144000000000     C                   ENDSR
144100000000     C/EJECT
144200000000     C************************************************************
144300000000     C* CONTROLLO esistenza associazione
144400000000     C************************************************************
144500000000     C     CtrAssoc      BEGSR
144600000000     C*
144700000000     C                   move      XScSoc        ALRSocieta
144800000000     C                   move      S1TpReg       ALRTpReg
144900000000     C                   move      S1Libro       ALRLibro
145000000000     C                   z-add     S1Aliq        ALRAliq
145100000000     C                   move      S1RifIva      ALRRifIva
145200000000     C*
145300000000     C     K05ALR01      chain     ANALR01L                           21
145400000000     C*
145500000000     C* associazione inesistente in anagraf.
145600000000     C                   if        *In21      = *On
145700000000     C                   SetOn                                        8099
145800000000     C                   SetOn                                        4041
145900000000     C*
146000000000     C                   if        S1Aliq     = 0        and
146100000000     C                             S1RifIVA   = *Blank
146200000000     C                   SetOn                                        4243
146300000000     C                   endif
146400000000     C*
146500000000     C                   if        S1Aliq    <> 0        and
146600000000     C                             S1RifIVA   = *Blank
146700000000     C                   SetOn                                        42
146800000000     C                   endif
146900000000     C*
147000000000     C                   if        S1Aliq     = 0        and
147100000000     C                             S1RifIVA  <> *Blank
147200000000     C                   SetOn                                        43
147300000000     C                   endif
147400000000     C*
147500000000     C                   endif
147600000000     C*
147700000000     C                   ENDSR
147800000000     C/EJECT
147900000000     C************************************************************
148000000000     C* CONTROLLO esistenza causale
148100000000     C************************************************************
148200000000     C     CtrCausale    BEGSR
148300000000     C*
148400000000     C                   if        S1Causale  <> *Blank
148500000000     C*
148600000000     C                   Clear                   Anope00f
148601131031R140 C*
148602131031 "   C* la causale deve essere valida, controllo con data
148603131031 "   C* di 10 anni in meno rispetto all'1/01/Anno corrente
148605131031 "   C                   Eval      WAnno       = *Year
148606131031 "   C                   MoveL     WAnno         AnnoDs
148607131031 "   C                   MoveL     '01'          MeseDs
148608131031 "   C                   MoveL     '01'          GiornoDs
148609131031 "   C                   Move      DataDs        WDtAlfa
148610131031 "   C                   Move      WDtAlfa       WData
148611131031 "   C     WData         SubDur    10:*Y         WData
148612131031 "   C                   Move      WData         WDtAlfa
148613131031R140 C*
148700000000     C*
148800000000     C                   eval      lenout = %size(anope00f)
148900000000     C*
149000000000     C                   callb     'NDMVC003'
149100000000     C                   parm                    XScSoc
149200000000     C                   parm                    S1Causale
149300131031R140 X****               parm                    *Omit
149301131031R140 C                   parm                    WDtAlfa
149400000000     C                   parm                    Uso
149500000000     C                   parm      *off          GesMsg
149600000000     C                   parm      *off          Esito
149700000000     C                   parm      'ANOPE     '  StrutturaO
149800000000     C                   parm                    Anope00f
149900000000     C                   parm                    LenOut
150000000000     C                   parm      *off          Refresh
150100000000     C*
150200000000     C                   if        Esito     = *on
150300000000     C                   SetOn                                        5699
150400000000     C                   SetOn                                        44
150500000000     C                   else
150600000000     C*
150700000000     C* deve essere per registrazioni IVA
150800000000     C                   if        OPETPC    <> '1'
150900000000     C                   SetOn                                        6199
151000000000     C                   SetOn                                        44
151100000000     C                   endif
151200000000     C*
151300000000     C                   endif
151400000000     C*
151500000000     C                   endif
151600000000     C*
151700000000     C                   ENDSR
151800000000     C/EJECT
151900000000     C************************************************************
152000000000     C* CONTROLLO conto
152100000000     C************************************************************
152200000000     C     CtrConto      BEGSR
152300000000     C*
152400000000     C*
152500000000     C                   if        S1Kcc      <> 'C     ' and
152600000000     C                             S1Kcc      <> 'F     ' and
152700000000     C                             S1Kcc      <> *Blank
152800000000     C                   CALLB     'XALLINEA'
152900000000     C                   PARM                    S1Kcc
153000000000     C                   PARM      6             LEN               5 0
153100000000     C                   PARM                    XSCINP
153200000000     C                   endif
153300000000     C*
153400000000     C                   if        S1Ksc      <> *Blank
153500000000     C                   CALLB     'XALLINEA'
153600000000     C                   PARM                    S1Ksc
153700000000     C                   PARM      8             LEN               5 0
153800000000     C                   PARM                    XSCINP
153900000000     C                   endif
154000000000     C*
154100000000     C                   if        S1Kcc       = 'C     ' or
154200000000     C                             S1Kcc       = 'F     '
154300000000     C                   CALLB     'XCAPCLIFOR'
154400000000     C                   PARM                    XScSoc
154500000000     C                   PARM                    S1Kcc
154600000000     C                   PARM                    S1Ksc
154700000000     C                   endif
154800000000     C*
154900000000     C*
155000000000     C                   if        S1Kcc      <> *Blank   or
155100000000     C                             S1Ksc      <> *Blank
155200000000     C*
155300000000     C                   Clear                   ND002DS
155400000000     C*
155500000000     C                   Eval      LenOut = %Size(ND002DS)
155600000000     C                   CallB     'NDMVC002'
155700000000     C                   PARM                    XScSoc
155800000000     C                   PARM                    S1Kcc
155900000000     C                   PARM                    S1Ksc
156000160701R2311X**                 PARM                    *Omit
156001160701R2311C                   Parm      *loval        Wdata
156100000000     C                   PARM      *OFF          GesMsg
156200000000     C                   PARM      *OFF          Esito
156300000000     C                   PARM      'ND002DS'     StrutturaO
156400000000     C                   PARM                    ND002DS
156500000000     C                   PARM                    LenOut
156600000000     C*
156700000000     C                   if        Esito       = *On
156800000000     C                   SetOn                                        5599
156900000000     C*
157000000000     C                   else
157100000000     C*
157200000000     C                   if        D1TipReg    = '2'  or
157300111031R071 C                             (
157400000000     C                              D1TipReg    = '3'  and
157500000000     C                              D1StpReg    = '2'
157600111031R071 C                                                     )
157601151008R195 C                                                     or
157603151008  "  C                             (D1TipReg    = '5'  and
157604151008R195 C                              D1StpReg    = '2'     )
157700000000     C*
157800000000     C                   if         ((Natura002  = '3'   or
157900000000     C                                Natura002  = '4')  or
158000000000     C*
158100000000     C                               ((Natura002  = '1'   or
158200130823     C                                 Natura002  = '2')  and
158300130823     C                                (SNatura002  = '4'  or
158400130823     C                                 SNatura002  = '9')))
158500130823     C                   else
158600000000     C                   seton                                        6899
158700000000     C                   seton                                        46
158800000000     C                   endif
158900000000     C                   endif
159000000000     C*
159100000000     C                   endif
159200000000     C*
159300130823     C*
159400130828     C                   if        D1TipReg    = '1'        or
159500130823     C                             (
159600130823     C                              D1TipReg    = '4'  and
159700130823     C                              D1StpReg    = '2'
159800130828     C                                                     )or
159900130828     C                             (
160000130828     C                              D1TipReg    = '4'  and
160100130828     C                              D1StpReg    = '3'
160200130828     C                                                     )
160300130823     C*
160400130823     C                   if        SNatura002  = 'C'   or
160500130823     C                             SNatura002  = 'F'
160600130823     C                   else
160700000000     C                   SetOn                                        7399
160800000000     C                   seton                                        46
160900000000     C                   endif
161000130823     C*
161100130823     C                   endif
161200000000     C*
161300000000     C                   endif
161400000000     C*
161500000000     C                   ENDSR
161600000000     C/EJECT
161700000000     C************************************************************
161800000000     C* CONTROLLO voce
161900000000     C************************************************************
162000000000     C     CtrVoce       BEGSR
162100000000     C*
162200000000     C                   if        S1Voce     <> *blank
162300000000     C                   eval      VciSocieta  = XscSoc
162400000000     C                   eval      VciVoce     = S1Voce
162500000000     C     K02Vci01      chain     AnVci01L
162600000000     C                   if        not %found(Anvci01l)
162700000000     C                   seton                                        6799
162800000000     C                   seton                                        47
162900000000     C                   else
162901151117R195 C* Congruenza se il conto è presente
162902151117R195 C                   If        S1Kcc <> *blanks or S1ksc <> *blanks
163000000000     C                   EXSR      CallX45
163001151117R195 C                   endif
163100000000     C                   endif
163200000000     C                   endif
163300000000     C*
163400000000     C                   ENDSR
163500000000     C/EJECT
163600000000     C************************************************************
163700000000     C* CONTROLLO congruenza conto/voce
163800000000     C************************************************************
163900000000     C     CallX45       BEGSR
164000000000     C*
164100000000     C                   Clear                   X45SIMANDS
164200000000     C                   Eval      X45Societa = XScSoc
164300000000     C                   Eval      X45Kcc     = S1Kcc
164400000000     C                   Eval      X45Ksc     = S1Ksc
164500000000     C                   Eval      X45Voce    = S1Voce
164600000000     C                   CALL      'X45SIMAN'
164700000000     C                   PARM                    X45SIMANDS
164800000000     C                   If        X45Err = *On
164900000000     C                   Select
165000000000     C                   When      X45Esito = '1' or
165100000000     C                             X45Esito = '2'
165200000000     C                   Seton                                        709947
165300000000     C                   When      X45Esito = 'A'
165400000000     C                   Seton                                        719947
165500000000     C                   When      X45Esito = '5'
165600000000     C                   Seton                                        729946
165700000000     C                   Endsl
165800000000     C                   Endif
165900000000     C*
166000000000     C                   ENDSR
166100000000     C/EJECT
166200000000     C***********************************************************
166300000000     C* Controllo classe
166400000000     C***********************************************************
166500000000     C     CtrClasse     BEGSR
166600000000     C*
166700000000     C                   if        S1Classe <> *blank
166800000000     C                   RESET                   SDGTABDS
166900000000     C                   Eval      XTbRic      = '1'
167000000000     C                   Eval      XTbAzi      = XScSoc
167100000000     C                   Eval      XTbKey      = *Zero
167200000000     C                   Eval      XTbCod      = 'G81'
167300000000     C                   Move      S1Classe      XTbKey
167400000000     C                   CALL      'SDGTABDR'
167500000000     C                   PARM                    SDGTABDS
167600000000     C                   if        XTbErr     <> '0'
167700000000     C                   seton                                        699948
167800000000     C                   endif
167900000000     C                   endif
168000000000     C*
168100000000     C                   ENDSR
168200000000     C/EJECT
168300130823     C************************************************************
168400130823     C* CONTROLLO descrizione
168500130823     C************************************************************
168600130823     C     CtrS1Nota     BEGSR
168700130823     C*
168800130823     C                   if        S1TpReg     = *Blank   and
168900130823     C                             S1Libro     = *Blank   and
169000130823     C                             S1Aliq      = *Zero    and
169100130823     C                             S1RifIVA    = *Blank   and
169200130823     C                             S1Causale   = *Blank   and
169300130823     C                             S1Kcc       = *Blank   and
169400130823     C                             S1Ksc       = *Blank   and
169500130823     C                             S1Voce      = *Blank   and
169600130823     C                             S1Classe    = *Blank
169700130823     C                   Clear                   S1Nota
169800130823     C                   endif
169900130823     C*
170000130823     C                   ENDSR
170100130823     C/EJECT
170200000000     C************************************************************
170300000000     C* Imposto gli attributi della riga di sfl
170400000000     C************************************************************
170500110830     C     CtrTpRegole   BEGSR
170600110830     C*
170700110830     C                   Select
170800110830     C*
170900110830     C* Controllo combinazioni tipo regola '1'
171000110830     C                   When      D1TipReg       = '1'
171100110830     C                   EXSR      CtrTpReg1
171200110830     C*
171300110830     C* Controllo combinazioni tipo regola '2'
171400110830     C                   When      D1TipReg       = '2'
171500110830     C                   EXSR      CtrTpReg2
171600110830     C*
171700110830     C* Controllo combinazioni tipo regola '3'
171800110830     C                   When      D1TipReg       = '3'
171900110830     C                   EXSR      CtrTpReg3
172000130823     C*
172100130823     C* Controllo combinazioni tipo regola '4'
172200130823     C                   When      D1TipReg       = '4'
172300130823     C                   EXSR      CtrTpReg4
172400110830     C*
172402151008R195 C* Controllo combinazioni tipo regola '5'
172403151008  "  C                   When      D1TipReg       = '5'
172404151008  "  C                   EXSR      CtrTpReg5
172405151008R195 C*
172500110830     C                   EndSl
172600110830     C*
172700110830     C                   ENDSR
172800110830     C/EJECT
172900130823     C************************************************************
173000130823     C* Controllo combinazioni tipo regola 1
173100130823     C************************************************************
173200130823     C     CtrTpReg1     BEGSR
173300130823     C*
173301130923     C                   Select
173302170822R258 X***** reverse charge
173303170822 "   X****               When      D1StpReg = '1'
173304170822 "   X****               If        (TpRegLibro   <> *blanks and
173305170822 "   X****                          (S1Aliq       <> 0       or
173306170822 "   X****                           S1RifIVA     <> *blanks    ) and
173307170822 "   X****                          S1Causale    <> *blanks and
173308170822 "   X****                          S1Kcc        <> *blanks and
173309170822 "   X****                          S1Ksc        <> *blanks )
173310170822 "   X****                                                     or
173311170822 "   X****                         (TpRegLibro   <> *blanks and
173312170822 "   X****                          (S1Aliq       <> 0       or
173313170822 "   X****                           S1RifIVA     <> *blanks    ) and
173314170822 "   X****                          S1Causale    <> *blanks and
173315170822 "   X****                          S1Kcc         = *blanks and
173316170822 "   X****                          S1Ksc         = *blanks )
173317170822 "   X****                                                     or
173318170822 "   X****                         (TpRegLibro   <> *blanks and
173319170822 "   X****                          (S1Aliq       <> 0       or
173320170822 "   X****                           S1RifIVA     <> *blanks    ) and
173321170822 "   X****                          S1Causale    =  *blanks and
173322170822 "   X****                          S1Kcc        <> *blanks and
173323170822 "   X****                          S1Ksc        <> *blanks )
173324170822 "   X****                                                     or
173325170822 "   X****                         (TpRegLibro   <> *blanks and
173326170822 "   X****                          (S1Aliq       <> 0       or
173327170822 "   X****                           S1RifIVA     <> *blanks    ) and
173328170822 "   X****                          S1Causale    =  *blanks and
173329170822 "   X****                          KccKsc       =  *blanks )
173330170822 "   X****                                                     or
173331170822 "   X****                         (TpRegLibro   =  *blanks and
173332170822 "   X****                          (S1Aliq       <> 0       or
173333170822 "   X****                           S1RifIVA     <> *blanks    ) and
173334170822 "   X****                          S1Causale    =  *blanks and
173335170822 "   X****                          KccKsc       =  *blanks )
173336170822 "   X****                                                     or
173337170822 "   X****                         (TpRegLibro   <> *blanks and
173338170822 "   X****                          (S1Aliq       =  0       and
173339170822 "   X****                           S1RifIVA     =  *blanks    ) and
173340170822 "   X****                          S1Causale    =  *blanks and
173341170822 "   X****                          KccKsc       =  *blanks )
173342170822 "   X****               ElSe
173343170822 "   X****               SetOn                                        4999
173344170822R258 X****               EndIf
177401130923     C*
177402130923     C* regime del margine
177403130923     C                   When      D1StpReg = '2'
177404170822R258 C                             OR
177405170822 "   C* reverse charge
177406170822 "   C                             D1StpReg = '1'
177407170822 "   C                             OR
177408170822 "   C* esclusione art. 15 (N1)
177409170822 "   C                             D1StpReg = '3'
177410170822 "   C                             OR
177411170822 "   C* IVA assolta in altro stato (N7)
177412170822 "   C                             D1StpReg = '4'
177413170822R258 C*
177414130923     C                   If        (TpRegLibro   <> *blanks and
177415130923     C                              (S1Aliq       <> 0       or
177416130923     C                               S1RifIVA     <> *blanks    ) and
177417130923     C                              S1Causale    <> *blanks and
177418130923     C                              S1Kcc        <> *blanks and
177419130923     C                              S1Ksc        <> *blanks )
177420130923     C                                                         or
177421130923     C                             (TpRegLibro   <> *blanks and
177422130923     C                              (S1Aliq       <> 0       or
177423130923     C                               S1RifIVA     <> *blanks    ) and
177424130923     C                              S1Causale    <> *blanks and
177425130923     C                              S1Kcc         = *blanks and
177426130923     C                              S1Ksc         = *blanks )
177427130923     C                                                         or
177428130923     C                             (TpRegLibro   <> *blanks and
177429130923     C                              (S1Aliq       <> 0       or
177430130923     C                               S1RifIVA     <> *blanks    ) and
177431130923     C                              S1Causale    =  *blanks and
177432130923     C                              S1Kcc        <> *blanks and
177433130923     C                              S1Ksc        <> *blanks )
177434130923     C                                                         or
177435130923     C                             (TpRegLibro   <> *blanks and
177436130923     C                              (S1Aliq       <> 0       or
177437130923     C                               S1RifIVA     <> *blanks    ) and
177438130923     C                              S1Causale    =  *blanks and
177439130923     C                              KccKsc       =  *blanks )
177440130923     C                                                         or
177441130923     C                             (TpRegLibro   =  *blanks and
177442130923     C                              (S1Aliq       <> 0       or
177443130923     C                               S1RifIVA     <> *blanks    ) and
177444130923     C                              S1Causale    =  *blanks and
177445130923     C                              KccKsc       =  *blanks )
177446130923     C                                                         or
177447130923     C                             (TpRegLibro   <> *blanks and
177448130923     C                              (S1Aliq       =  0       and
177449130923     C                               S1RifIVA     =  *blanks    ) and
177450130923     C                              S1Causale    =  *blanks and
177451130923     C                              KccKsc       =  *blanks )
177452130923     C                                                         or
177453130923     C                             (TpRegLibro    = *blanks and
177454130923     C                              (S1Aliq       =  0       and
177455130923     C                               S1RifIVA     =  *blanks    ) and
177456130923     C                              S1Causale   <>  *blanks and
177457130923     C                              KccKsc      <>  *blanks )
177458130923     C                                                         or
177459130923     C                             (TpRegLibro    = *blanks and
177460130923     C                              (S1Aliq       =  0       and
177461130923     C                               S1RifIVA     =  *blanks    ) and
177462130923     C                              S1Causale   <>  *blanks and
177463130923     C                              KccKsc       =  *blanks )
177464130923     C                                                         or
177465130923     C                             (TpRegLibro    = *blanks and
177466130923     C                              (S1Aliq       =  0       and
177467130923     C                               S1RifIVA     =  *blanks    ) and
177468130923     C                              S1Causale    =  *blanks and
177469130923     C                              KccKsc      <>  *blanks )
177470170822R258 C                                                         or
177471170822 "   C                             (TpRegLibro   <> *blanks and
177472170822 "   C                              (S1Aliq       =  0       and
177473170822 "   C                               S1RifIVA     =  *blanks    ) and
177474170822 "   C                              S1Causale   <>  *blanks and
177475170822 "   C                              KccKsc      <>  *blanks )
177476170822 "   C                                                         or
177477170822 "   C                             (TpRegLibro   <> *blanks and
177478170822 "   C                              (S1Aliq       =  0       and
177479170822 "   C                               S1RifIVA     =  *blanks    ) and
177480170822 "   C                              S1Causale    =  *blanks and
177481170822R258 C                              KccKsc      <>  *blanks )
177482130923     C                   ElSe
177483130923     C                   SetOn                                        4999
177484130923     C                   EndIf
177500130823     C*
177501130923     C                   EndSl
177502130923     C*
177600130823     C                   ENDSR
177700130823     C/EJECT
177800130823     C************************************************************
177900130823     C* Controllo combinazioni tipo regola 2
178000130823     C************************************************************
178100130823     C     CtrTpReg2     BEGSR
178200130823     C*
178300130823     C                   If        (S1Causale    <> *blanks and
178400130823     C                              S1Kcc        <> *blanks and
178500130823     C                              S1Ksc        <> *blanks and
178600130823     C                              S1Voce       <> *blanks )
178700130823     C                                                         or
178800130823     C                             (S1Causale    <> *blanks and
178900130823     C                              S1Kcc        <> *blanks and
179000130823     C                              S1Ksc        =  *blanks and
179100130823     C                              S1Voce       <> *blanks )
179200130823     C                                                         or
179300130823     C                             (S1Causale    <> *blanks and
179400130823     C                              S1Kcc        <> *blanks and
179500130823     C                              S1Ksc        <> *blanks and
179600130823     C                              S1Voce       =  *blanks )
179700130823     C                                                         or
179800130823     C                             (S1Causale    <> *blanks and
179900130823     C                              S1Kcc        <> *blanks and
180000130823     C                              S1Ksc        =  *blanks and
180100130823     C                              S1Voce       =  *blanks )
180200130823     C                                                         or
180300130823     C                             (S1Causale    <> *blanks and
180400130823     C                              KccKsc       =  *blanks and
180500130823     C                              S1Voce       =  *blanks )
180501160321R2194C                                                         or
180502160321 "   C                             (S1Causale    =  *blanks and
180503160321 "   C                              S1Kcc        <> *blanks and
180504160321 "   C                              S1Ksc        <> *blanks and
180505160321R2194C                              S1Voce       <> *blanks )
180600130823     C                                                         or
180700130823     C                             (S1Causale    =  *blanks and
180800130823     C                              S1Kcc        <> *blanks and
180900130823     C                              S1Ksc        <> *blanks and
181000130823     C                              S1Voce       =  *blanks )
181001160321R2194C                                                         or
181002160321 "   C                             (S1Causale    =  *blanks and
181003160321 "   C                              S1Kcc        <> *blanks and
181004160321 "   C                              S1Ksc        =  *blanks and
181005160321R2194C                              S1Voce       <> *blanks )
181100130823     C                                                         or
181200130823     C                             (S1Causale    =  *blanks and
181300130823     C                              S1Kcc        <> *blanks and
181400130823     C                              S1Ksc        =  *blanks and
181500130823     C                              S1Voce       =  *blanks )
181600130823     C                                                         or
181700130823     C                             (S1Causale    =  *blanks and
181800130823     C                              KccKsc       =  *blanks and
181900130823     C                              S1Voce       <> *blanks )
182000130823     C
182100130823     C                   ElSe
182200130823     C                   SetOn                                        5099
182300130823     C                   EndIf
182400130823     C*
182500130823     C                   ENDSR
182600130823     C/EJECT
182700130823     C************************************************************
182800130823     C* Controllo combinazioni tipo regola 3
182900130823     C************************************************************
183000130823     C     CtrTpReg3     BEGSR
183100130823     C*
183200130823     C* StpReg 2: visualizzo solo il campo relativo al conto
183300130823     C                   If        D1StpReg = '2'
183400130823     C*
183500130823     C*
183600130823     C                   If         S1Kcc        =  *blanks
183601151117R195 C                   If         S1Voce       =  *blanks
183602151117R195 X***                SetOn                                        5199
183700151117R195 C                   SetOn                                        5099
183800130823     C                   EndIf
183801151117R195 C                   EndIf
183900130823     C*
184000130823     C                   EndIf
184100130823     C*
184200130823     C                   ENDSR
184300130823     C/EJECT
184400130823     C************************************************************
184500130823     C* Controllo combinazioni tipo regola 4
184600130823     C************************************************************
184700130823     C     CtrTpReg4     BEGSR
184800130823     C*
184900130823     C*
185000130823     C                   ENDSR
185100130823     C/EJECT
185101151008R195 C************************************************************
185102151008  "  C* Controllo combinazioni tipo regola 3
185103151008  "  C************************************************************
185104151008R195 C     CtrTpReg5     BEGSR
185105151008     C*
185106151008     C* StpReg 2: visualizzo solo il campo relativo al conto
185107151008     C                   If        D1StpReg = '2'
185108151008     C*
185109151008     C*
185110151008     C                   If         S1Kcc        =  *blanks
185113151117     C                   If         S1Voce       =  *blanks
185115151117     C                   SetOn                                        5099
185116151117     C                   EndIf
185118151008     C                   EndIf
185119151117     C*
185120151117     C                   EndIf
185121151008     C*
185122151008R195 C                   ENDSR
185123151008R195 C/EJECT
185200110830     C************************************************************
185300110830     C* Imposto gli attributi della riga di sfl
185400110830     C************************************************************
185500110830     C     ATRS1         BEGSR
185600000000     C*
185700000000     C                   setoff                                       343536
185800000000     C                   setoff                                       373839
185900000000     C*
186000000000     C*         Associazioni IVA
186100000000     C*  tp.reg  libro  aliquota  riferim. Causale  Conto  Voce  Classe
186200000000     C*    37      37       38       38      34       35    36      39
186300000000     C*
186400000000     C* Tipo regola 1: visualizzo tutti i campi tranne voce e classe
186500000000     C                   If        D1TipReg = '1'  and
186600000000     C                             (D1StpReg = '1' or
186700000000     C                              D1StpReg = '2' or
186701170824R258GC                              D1StpReg = '3' or
186702170824 "   C                              D1StpReg = '4' or
186703170824R258GC                              D1StpReg = '5')
186900000000     C                   SetOn                                        3639
187000000000     C                   EndIf
187100000000     C*
187200000000     C* Tipo regola 2: visualizzo Causale-Conto-Voce
187300000000     C                   If        D1TipReg = '2'
187400000000     C                   SetOn                                        373839
187500000000     C                   EndIf
187600000000     C*
187700000000     C* Tipo regola 3: esclusione
187800000000     C                   If        D1TipReg = '3'
187801151023R195 C                             Or D1TipReg = '5'
187802170824R258GC                             Or D1TipReg = '6'
187900000000     C*
188000000000     C* StpReg 1: visualizzo solo il campo della causale
188100000000     C                   If        D1StpReg = '1'
188200000000     C                   SetOn                                        373536
188300000000     C                   SetOn                                        3839
188400000000     C                   EndIf
188500000000     C*
188600000000     C* StpReg 2: visualizzo solo il campo relativo al conto
188601151117R195 C*           e il campo relativo alla voce
188700000000     C                   If        D1StpReg = '2'
188800151117     C                   SetOn                                        3734
188900000000     C                   SetOn                                        3839
189000000000     C                   EndIf
189100000000     C*
189200000000     C* StpReg 3: visualizzo solo i campo relativi al tipo registro/libro
189300000000     C                   If        D1StpReg = '3'
189400000000     C                   SetOn                                        383435
189500000000     C                   SetOn                                        3639
189600000000     C                   EndIf
189700000000     C*
189800000000     C* StpReg 4: visualizzo solo il campo della classe
189900000000     C                   If        D1StpReg = '4'
190000000000     C                   SetOn                                        373436
190100000000     C                   SetOn                                        3835
190200000000     C                   EndIf
190300000000     C*
190400000000     C                   EndIf
190500130823     C*
190600130823     C* Tipo regola 4: Note di addebito
190700130823     C                   If        D1TipReg = '4'
190800130823     C*
190900130823     C* StpReg 1: visualizzo solo il campo della causale
191000130823     C                   If        D1StpReg = '1'
191100130823     C                   SetOn                                        373536
191200130823     C                   SetOn                                        3839
191300130823     C                   EndIf
191400130823     C*
191500130823     C* StpReg 2: visualizzo solo i campi causale e conto
191600130823     C                   If        D1StpReg = '2'
191700130823     C                   SetOn                                        3736
191800130823     C                   SetOn                                        3839
191900130823     C                   EndIf
192000130828     C*
192100130828     C* StpReg 3: visualizzo solo i campi causale e conto
192200130828     C                   If        D1StpReg = '3'
192300130828     C                   SetOn                                        3736
192400130828     C                   SetOn                                        3839
192500130828     C                   EndIf
192600130823     C*
192700130823     C                   EndIf
192800000000     C*
192900000000     C                   ENDSR
193000000000     C/EJECT
193100000000     C************************************************************
193200000000     C* GESTIONE F03 VIDEO S1
193300000000     C************************************************************
193400000000     C     F03S1         BEGSR
193500000000     C*
193600000000     C                   MOVE      *ON           $FINE
193700130823     C                   MOVE      '1'           RETG70
193800000000     C* fine programma
193900000000     C                   EXSR      ENDPGM
194000000000     C*
194100000000     C                   ENDSR
194200000000     C/EJECT
194300000000     C************************************************************
194400000000     C* RICERCHE
194500000000     C************************************************************
194600000000     C     SEARCH        BEGSR
194700000000     C*
194800000000     C* determino Riga/Colonna del cursore
194900000000     C                   MOVE      NRG           R$$
195000000000     C                   MOVE      NCL           C$$
195100000000     C                   Z-ADD     RIRI          CURR
195200000000     C                   Z-ADD     COCO          CURC
195300000000     C*
195400000000     C     S1KCC         IFNE      'C     '
195500000000     C     S1KCC         andne     'F     '
195600000000     C     S1KCC         ANDNE     *Blanks
195700000000     C                   CALLB     'XALLINEA'
195800000000     C                   PARM                    S1KCC
195900000000     C                   PARM      6             LEN               5 0
196000000000     C                   PARM                    XSCINP
196100000000     C                   ENDIF
196200000000     C*
196300000000   B1C     S1KSC         IFNE      *BLANKS
196400000000     C                   CALLB     'XALLINEA'
196500000000     C                   PARM                    S1KSC
196600000000     C                   PARM      8             LEN               5 0
196700000000     C                   PARM                    XSCINP
196800000000     C                   endif
196900000000     C*
197000000000     C                   SELECT
197100000000     C*
197200000000     C* FMTD1 - TipReg
197300000000     C     H1NMFL        WHENEQ    'D1TIPREG'
197301170822R195 X*** EX R195        If        TPCOMG70 = 'B'
197302170822R258 C                   Select
197303170822  "  C                   When      TPCOMG70 = 'T'
197304170822  "  C                   EXSR      F4_TipRegT
197305170822R258 C                   When      TPCOMG70 = 'B'
197306170822R195 C                   EXSR      F4_TipRegB
197307170822R195 X*** EX R195        Else
197308170822R258 C                   OTher
197400000000     C                   EXSR      F4_TipReg
197401170822R258 C                   EndSL
197402170822R195 X*** EX R195        Endif
197500000000     C*
197600000000     C* FMTD1 - StpReg
197700000000     C     H1NMFL        WHENEQ    'D1STPREG'
197701170822R258 C                   Select
197702170822  "  C                   When      TPCOMG70 = 'T'
197703170822  "  C                   EXSR      F4_StpRegT
197704170822R258 C                   Other
197800000000     C                   EXSR      F4_StpReg
197801170822R258 C                   Endsl
197900000000     C*
198000000000     C* FMTD1 - Tipo registro
198100000000     C     H1NMFL        WHENEQ    'S1TPREG'
198200000000     C     C1NRR         CHAIN     FMTS1                              21
198300000000     C*
198400000000     C                   EXSR      F4_TpReg
198500000000     C*
198600000000     C* FMTD1 - libro IVA
198700000000     C     H1NMFL        WHENEQ    'S1LIBRO'
198800000000     C     C1NRR         CHAIN     FMTS1                              21
198900000000     C*
199000000000     C                   if        S1TpReg     = *Blank      or
199100000000     C                             (S1Aliq     = 0       and
199200000000     C                              S1RifIVA   = *Blank     )
199300000000     C                   EXSR      F4_Libro
199400000000     C                   else
199500000000     C                   EXSR      F4_Assoc
199600000000     C                   endif
199700000000     C*
199800000000     C* FMTD1 - Riferimento IVA
199900000000     C     H1NMFL        WHENEQ    'S1RIFIVA'
200000000000     C     H1NMFL        OREQ      'S1ALIQ'
200100000000     C*
200200000000     C     C1NRR         CHAIN     FMTS1                              21
200300000000     C*
200400000000     C                   if        S1TpReg   <> *Blank  and
200500000000     C                             S1Libro   <> *Blank
200600000000     C                   EXSR      F4_Assoc
200700000000     C                   else
200800000000     C                   EXSR      F4_RifIVA
200900000000     C                   endif
201000000000     C*
201100000000     C* FMTD1 - Causale
201200000000     C     H1NMFL        WHENEQ    'S1CAUSALE'
201300000000     C     C1NRR         CHAIN     FMTS1                              21
201400000000     C                   EXSR      F4_Causale
201500000000     C*
201600000000     C* FMTD1 - Voce
201700000000     C     H1NMFL        WHENEQ    'S1VOCE'
201800000000     C     C1NRR         CHAIN     FMTS1                              21
201900000000     C                   EXSR      F4_Voce
202000000000     C*
202100000000     C* FMTD1 - Capoconto
202200000000     C     H1NMFL        WHENEQ    'S1KCC'
202300000000     C     C1NRR         CHAIN     FMTS1                              21
202400000000     C                   EXSR      F4_Kcc
202500000000     C*
202600000000     C* FMTD1 - Sottoconto
202700000000     C     H1NMFL        WHENEQ    'S1KSC'
202800000000     C     C1NRR         CHAIN     FMTS1                              21
202900000000     C                   EXSR      F4_Ksc
203000000000     C*
203100000000     C* FMTD1 - Classe
203200000000     C     H1NMFL        WHENEQ    'S1CLASSE'
203300000000     C     C1NRR         CHAIN     FMTS1                              21
203400000000     C                   EXSR      F4_Classe
203500000000     C*
203600000000     C* ricerca non abilitata in questa posizione
203700000000   X1C                   OTHER
203800000000     C                   SETON                                        9899
203900000000     C*
204000000000   E1C                   ENDSL
204100000000     C*
204200000000     C* imposto pos. del cursore
204300000000     C                   Z-ADD     CURR          H1RIGA
204400000000     C                   Z-ADD     CURC          H1COLO
204500000000     C*
204600000000     C                   MOVE      *HIVAL        $LASTV
204700000000     C*
204800000000     C                   ENDSR
204900000000     C/EJECT
205000000000     C************************************************************
205100000000     C* Ricerca TipReg
205200000000     C************************************************************
205300000000     C     F4_TipReg     BEGSR
205400000000     C*
205500000000     C                   reset                   ana964ds
205600000000     C*
205700000000     C                   movel     '01'          Opz964
205800130823     C                   movel     'SDGSPR000'   Trc964
205900130823     C                   movel     'SPRTIPREG'   Cam964
206000000000     C*
206100000000     C                   movel     ana964ds      Kpjbu
206200000000     C                   call      'ANA964R'
206300000000     C                   parm                    Kpjba
206400000000     C                   movel     Kpjbu         Ana964ds
206500000000     C*
206600000000     C                   if        Ret964     <> '2'
206700000000     C                   movel     Val964        D1TipReg
206800000000     C                   endif
206900000000     C*
207000000000     C                   ENDSR
207001151007R195 C/EJECT
207002151007  "  C************************************************************
207003151007  "  C* Ricerca TipReg per Black List
207004151007  "  C************************************************************
207005151007R!95 C     F4_TipRegB    BEGSR
207006151007     C*
207007151007     C                   reset                   ana964ds
207008151007     C*
207009151007     C                   movel     '01'          Opz964
207010151007     C                   movel     'ANDIZ000 '   Trc964
207011151007     C                   movel     'SDGTIPREGB'  Cam964
207012151007     C*
207013151007     C                   movel     ana964ds      Kpjbu
207014151007     C                   call      'ANA964R'
207015151007     C                   parm                    Kpjba
207016151007     C                   movel     Kpjbu         Ana964ds
207017151007     C*
207018151007     C                   if        Ret964     <> '2'
207019151007     C                   movel     Val964        D1TipReg
207020151007     C                   endif
207021151007     C*
207022151007R195 C                   ENDSR
207023170822R258 C/EJECT
207024170822  "  C************************************************************
207025170822  "  C* Ricerca TipReg per Spesometro Trimestrale
207026170822  "  C************************************************************
207027170822R258 C     F4_TipRegT    BEGSR
207028170822     C*
207029170822     C                   reset                   ana964ds
207030170822     C*
207031170822     C                   movel     '01'          Opz964
207032170822     C                   movel     'ANDIZ000 '   Trc964
207033170822     C                   movel     'SDGTIPREGT'  Cam964
207034170822     C*
207035170822     C                   movel     ana964ds      Kpjbu
207036170822     C                   call      'ANA964R'
207037170822     C                   parm                    Kpjba
207038170822     C                   movel     Kpjbu         Ana964ds
207039170822     C*
207040170822     C                   if        Ret964     <> '2'
207041170822     C                   movel     Val964        D1TipReg
207042170822     C                   endif
207043170822     C*
207044170822R258 C                   ENDSR
207100000000     C/EJECT
207200000000     C************************************************************
207300000000     C* Ricerca StpReg
207400000000     C************************************************************
207500000000     C     F4_StpReg     BEGSR
207600000000     C*
207700000000     C                   select
207800000000     C* Tipo regola 1 (natura dell'operazione)
207900000000     C                   when      D1TipReg = '1' and
208000000000     C                             H1NMFL   = 'D1STPREG'
208100000000     C                   reset                   ana964ds
208200000000     C                   movel     '01'          Opz964
208300000000     C                   movel     'ANDIZ000'    Trc964
208400130823     C                   movel     'SDGSTPREG1'  Cam964
208500000000     C                   movel     ana964ds      Kpjbu
208600000000     C                   call      'ANA964R'
208700000000     C                   parm                    Kpjba
208800000000     C                   movel     Kpjbu         Ana964ds
208900000000     C                   if        Ret964     <> '2'
209000000000     C                   movel     Val964        D1StpReg
209100000000     C                   endif
209200000000     C*
209300000000     C* Tipo regola 2 (tipologia dell'operazione)
209400000000     C                   when      D1TipReg = '2' and
209500000000     C                             H1NMFL   = 'D1STPREG'
209600000000     C                   reset                   ana964ds
209700000000     C                   movel     '01'          Opz964
209800000000     C                   movel     'ANDIZ000'    Trc964
209900130823     C                   movel     'SDGSTPREG2'  Cam964
210000000000     C                   movel     ana964ds      Kpjbu
210100000000     C                   call      'ANA964R'
210200000000     C                   parm                    Kpjba
210300000000     C                   movel     Kpjbu         Ana964ds
210400000000     C                   if        Ret964     <> '2'
210500000000     C                   movel     Val964        D1StpReg
210600000000     C                   endif
210700000000     C*
210800000000     C* Tipo regola 3 (esclusionel'operazione)
210900000000     C                   when      D1TipReg = '3' and
211000000000     C                             H1NMFL   = 'D1STPREG'
211100000000     C                   reset                   ana964ds
211200000000     C                   movel     '01'          Opz964
211300000000     C                   movel     'ANDIZ000'    Trc964
211400130823     C                   movel     'SDGSTPREG3'  Cam964
211500000000     C                   movel     ana964ds      Kpjbu
211600000000     C                   call      'ANA964R'
211700000000     C                   parm                    Kpjba
211800000000     C                   movel     Kpjbu         Ana964ds
211900000000     C                   if        Ret964     <> '2'
212000000000     C                   movel     Val964        D1StpReg
212100000000     C                   endif
212200130823     C*
212300130823     C* Tipo regola 4 (note di addebito      )
212400130823     C                   when      D1TipReg = '4' and
212500130823     C                             H1NMFL   = 'D1STPREG'
212600130823     C                   reset                   ana964ds
212700130823     C                   movel     '01'          Opz964
212800130823     C                   movel     'ANDIZ000'    Trc964
212801151222R195 C                   If        TpComG70 = 'B'
212802151222 "   C                   movel     'SDGSTPRE4B'  Cam964
212803151222R195 C                   Else
212900130823     C                   movel     'SDGSTPREG4'  Cam964
212901151222R195 C                   Endif
213000130823     C                   movel     ana964ds      Kpjbu
213100130823     C                   call      'ANA964R'
213200130823     C                   parm                    Kpjba
213300130823     C                   movel     Kpjbu         Ana964ds
213400130823     C                   if        Ret964     <> '2'
213500130823     C                   movel     Val964        D1StpReg
213600130823     C                   endif
213601151007R195 C*
213602151007  "  C* Tipo regola 5 (esclusioni Black List )
213603151007  "  C                   when      D1TipReg = '5' and
213604151007  "  C                             H1NMFL   = 'D1STPREG'
213605151007  "  C                   reset                   ana964ds
213606151007  "  C                   movel     '01'          Opz964
213607151007  "  C                   movel     'ANDIZ000'    Trc964
213608151007  "  C                   movel     'SDGSTPREG5'  Cam964
213609151007  "  C                   movel     ana964ds      Kpjbu
213610151007  "  C                   call      'ANA964R'
213611151007  "  C                   parm                    Kpjba
213612151007  "  C                   movel     Kpjbu         Ana964ds
213613151007  "  C                   if        Ret964     <> '2'
213614151007  "  C                   movel     Val964        D1StpReg
213615151007R195 C                   endif
213616170822R258 C*
213617170822  "  C* Tipo regola 6 (esclusioni Spesometro Trimestrale)
213618170822  "  C                   when      D1TipReg = '6' and
213619170822  "  C                             H1NMFL   = 'D1STPREG'
213620170822  "  C                   reset                   ana964ds
213621170822  "  C                   movel     '01'          Opz964
213622170822  "  C                   movel     'ANDIZ000'    Trc964
213623170822  "  C                   movel     'SDGSTPREG6'  Cam964
213624170822  "  C                   movel     ana964ds      Kpjbu
213625170822  "  C                   call      'ANA964R'
213626170822  "  C                   parm                    Kpjba
213627170822  "  C                   movel     Kpjbu         Ana964ds
213628170822  "  C                   if        Ret964     <> '2'
213629170822  "  C                   movel     Val964        D1StpReg
213630170822R258 C                   endif
213700000000     C*
213800000000     C                   endsl
213900000000     C*
214000000000     C                   ENDSR
214001170822R258 C/EJECT
214002170822  "  C************************************************************
214003170822  "  C* Ricerca StpReg per spesometro trimestrale
214004170822  "  C************************************************************
214005170822R258 C     F4_StpRegT    BEGSR
214006170822     C*
214007170822     C                   select
214008170822     C* Tipo regola 1 (definizione dell'operazione)
214009170822     C                   when      D1TipReg = '1' and
214010170822     C                             H1NMFL   = 'D1STPREG'
214011170822     C                   reset                   ana964ds
214012170822     C                   movel     '01'          Opz964
214013170822     C                   movel     'ANDIZ000'    Trc964
214014170822     C                   movel     'SDGSTPRE1T'  Cam964
214015170822     C                   movel     ana964ds      Kpjbu
214016170822     C                   call      'ANA964R'
214017170822     C                   parm                    Kpjba
214018170822     C                   movel     Kpjbu         Ana964ds
214019170822     C                   if        Ret964     <> '2'
214020170822     C                   movel     Val964        D1StpReg
214021170822     C                   endif
214022170822     C*
214023170822     C* Tipo regola 2 (natura dell'operazione)
214024170822     C                   when      D1TipReg = '2' and
214025170822     C                             H1NMFL   = 'D1STPREG'
214026170822     C                   reset                   ana964ds
214027170822     C                   movel     '01'          Opz964
214028170822     C                   movel     'ANDIZ000'    Trc964
214029170822     C                   movel     'SDGSTPRE2T'  Cam964
214030170822     C                   movel     ana964ds      Kpjbu
214031170822     C                   call      'ANA964R'
214032170822     C                   parm                    Kpjba
214033170822     C                   movel     Kpjbu         Ana964ds
214034170822     C                   if        Ret964     <> '2'
214035170822     C                   movel     Val964        D1StpReg
214036170822     C                   endif
214037170822     C*
214038170822     C* Tipo regola 4 (Tipo documento        )
214039170822     C                   when      D1TipReg = '4' and
214040170822     C                             H1NMFL   = 'D1STPREG'
214041170822     C                   reset                   ana964ds
214042170822     C                   movel     '01'          Opz964
214043170822     C                   movel     'ANDIZ000'    Trc964
214044170822     C                   movel     'SDGSTPRE4T'  Cam964
214045170822     C                   movel     ana964ds      Kpjbu
214046170822     C                   call      'ANA964R'
214047170822     C                   parm                    Kpjba
214048170822     C                   movel     Kpjbu         Ana964ds
214049170822     C                   if        Ret964     <> '2'
214050170822     C                   movel     Val964        D1StpReg
214051170822     C                   endif
214052170822     C*
214053170822     C* Tipo regola 6 (esclusione)
214054170822     C                   when      D1TipReg = '6' and
214055170822     C                             H1NMFL   = 'D1STPREG'
214056170822     C                   reset                   ana964ds
214057170822     C                   movel     '01'          Opz964
214058170822     C                   movel     'ANDIZ000'    Trc964
214059170822     C                   movel     'SDGSTPRE6T'  Cam964
214060170822     C                   movel     ana964ds      Kpjbu
214061170822     C                   call      'ANA964R'
214062170822     C                   parm                    Kpjba
214063170822     C                   movel     Kpjbu         Ana964ds
214064170822     C                   if        Ret964     <> '2'
214065170822     C                   movel     Val964        D1StpReg
214066170822     C                   endif
214067170822     C*
214068170822     C                   endsl
214069170822     C*
214070170822R258 C                   ENDSR
214100000000     C/EJECT
214200000000     C************************************************************
214300000000     C* Ricerca tipo registro
214400000000     C************************************************************
214500000000     C     F4_TpReg      BEGSR
214600000000     C*
214700000000     C                   RESET                   ANA964DS
214800000000     C*
214900000000     C                   MOVEL     '01'          OPZ964
215000000000     C                   MOVEL     'ANDLI000'    TRC964
215100000000     C                   MOVEL     'DLITPREG'    CAM964
215200000000     C*
215300000000     C                   MOVEL     ANA964DS      KPJBU
215400000000     C                   CALL      'ANA964R'
215500000000     C                   PARM                    KPJBA
215600000000     C*
215700000000     C                   MOVEL     KPJBU         ANA964DS
215800000000     C*
215900000000     C                   if        Ret964     <> '2'
216000000000     C                   MoveL     Val964        S1TpReg
216100000000     C*
216200000000     C                   move      *on           *in32
216300000000     C* imposto gli attributi del sfl
216400000000     C                   EXSR      AtrS1
216500000000     C                   UPDATE    FMTS1
216600000000     C                   move      *off          *in32
216700000000     C                   endif
216800000000     C*
216900000000     C                   ENDSR
217000000000     C/EJECT
217100000000     C************************************************************
217200000000     C* Ricerca libro IVA
217300000000     C************************************************************
217400000000     C     F4_Libro      BEGSR
217500000000     C*
217600000000     C                   Reset                   Ana062DS
217700000000     C*
217800000000     C                   Move      '01'          OPZ062
217900000000     C                   MoveL     S1TpReg       TpR062
218000000000     C                   MoveL     S1Libro       Lib062
218100000000     C*
218200000000     C                   Movel     Ana062DS      KPJBU
218300000000     C*
218400000000     C                   CALL      'ANA062R'
218500000000     C                   PARM                    KPJBA
218600000000     C*
218700000000     C                   MOVEL     KPJBU         ANA062DS
218800000000     C*
218900000000     C                   if        Ret062     <> '2'
219000000000     C                   MoveL     TpR062        S1TpReg
219100000000     C                   MoveL     Lib062        S1Libro
219200000000     C*
219300000000     C                   move      *on           *in32
219400000000     C* imposto gli attributi del sfl
219500000000     C                   EXSR      AtrS1
219600000000     C                   UPDATE    FMTS1
219700000000     C                   move      *off          *in32
219800000000     C                   endif
219900000000     C*
220000000000     C                   ENDSR
220100000000     C/EJECT
220200000000     C************************************************************
220300000000     C* Ricerca riferimento IVA
220400000000     C************************************************************
220500000000     C     F4_RifIVA     BEGSR
220600000000     C*
220700000000     C                   Reset                   Ana064DS
220800000000     C*
220900000000     C                   Move      '01'          OPZ064
221000000000     C                   Movel     S1RifIVA      RfI064
221100000000     C                   Z-Add     S1Aliq        Ali064
221200000000     C                   MoveL     S1TpReg       TpR064
221300000000     C*
221400000000     C                   Movel     Ana064DS      KPJBU
221500000000     C*
221600000000     C                   CALL      'ANA064R'
221700000000     C                   PARM                    KPJBA
221800000000     C*
221900000000     C                   MOVEL     KPJBU         ANA064DS
222000000000     C*
222100000000     C                   if        Ret064     <> '2'
222200000000     C                   Z-Add     Ali064        S1Aliq
222300000000     C                   MoveL     RfI064        S1RifIVA
222400000000     C*
222500000000     C                   move      *on           *in32
222600000000     C* imposto gli attributi del sfl
222700000000     C                   EXSR      AtrS1
222800000000     C                   UPDATE    FMTS1
222900000000     C                   move      *off          *in32
223000000000     C                   endif
223100000000     C*
223200000000     C                   ENDSR
223300000000     C/EJECT
223400000000     C************************************************************
223500000000     C* Ricerca associazione libro-riferimento IVA
223600000000     C************************************************************
223700000000     C     F4_Assoc      BEGSR
223800000000     C*
223900000000     C                   RESET                   ANA068DS
224000000000     C*
224100000000     C                   MOVE      '01'          OPZ068
224200000000     C                   MOVE      XscSoc        SOC068
224300000000     C                   MOVE      S1TpReg       TPR068
224400000000     C                   MOVE      S1Libro       LIB068
224500000000     C                   MOVE      S1Aliq        ALI068
224600000000     C                   MOVE      S1RifIVA      RIF068
224700000000     C                   MOVEL     ANA068DS      KPJBU
224800000000     C*
224900000000     C                   CALL      'ANA068R'
225000000000     C                   PARM                    KPJBA
225100000000     C*
225200000000     C                   MOVEL     KPJBU         ANA068DS
225300000000     C*
225400000000     C                   IF        Ret068     <> '2'
225500000000     C*
225600000000     C                   MOVE      TpR068        S1TpReg
225700000000     C                   Z-Add     ALI068        S1Aliq
225800000000     C                   MOVE      RIF068        S1RifIVA
225900000000     C                   MOVE      Lib068        S1Libro
226000000000     C*
226100000000     C                   move      *on           *in32
226200000000     C* imposto gli attributi del sfl
226300000000     C                   EXSR      AtrS1
226400000000     C                   UPDATE    FMTS1
226500000000     C                   move      *off          *in32
226600000000     C*
226700000000     C                   ENDIF
226800000000     C*
226900000000     C                   ENDSR
227000000000     C/EJECT
227100000000     C************************************************************
227200000000     C* Ricerca causale contabile
227300000000     C************************************************************
227400000000     C     F4_Causale    BEGSR
227500000000     C*
227600000000     C                   RESET                   ANA070DS
227700000000     C*
227800000000     C                   MOVE      '01'          OPZ070
227900000000     C                   MOVE      '1'           tpc070
228000000000     C                   CLEAR                   KPJBU
228100000000     C                   MOVEL     ANA070DS      KPJBU
228200000000     C*
228300000000     C                   CALL      'ANA070R'
228400000000     C                   PARM                    KPJBA
228500000000     C*
228600000000     C                   MOVEL     KPJBU         ANA070DS
228700000000     C*
228800000000     C     Cau070        IFNE      *BLANK
228900000000     C                   MOVEL     Cau070        S1Causale
229000000000     C                   move      *on           *in32
229100000000     C*
229200000000     C* imposto gli attributi del sfl
229300000000     C                   EXSR      AtrS1
229400000000     C                   UPDATE    FMTS1
229500000000     C                   move      *off          *in32
229600000000     C                   ENDIF
229700000000     C*
229800000000     C                   ENDSR
229900000000      /EJECT
230000000000     C************************************************************
230100000000     C* Capoconto di riferimento
230200000000     C************************************************************
230300000000     C     F4_Kcc        BEGSR
230400000000     C*
230500000000     C                   RESET                   ANA200DS
230600000000     C                   MOVEL     '01'          OPZ200
230700000000     C*
230800000000     C                   SELECT
230900000000     C     H1NMFL        WHENEQ    'S1KCC     '
231000000000     C                   MOVEL     S1Kcc         KCC200
231100000000     C                   ENDSL
231200000000     C                   MOVEL     ANA200DS      KPJBU
231300000000     C                   CALL      'ANA200R'
231400000000     C                   PARM                    KPJBA
231500000000     C                   MOVEL     KPJBU         ANA200DS
231600000000     C*
231700000000     C     KCC200        IFNE      *BLANK
231800000000     C*
231900000000     C                   SELECT
232000000000     C     H1NMFL        WHENEQ    'S1KCC     '
232100000000     C                   Eval      S1Kcc    =    KCC200
232200000000     C                   ENDSL
232300000000     C*
232400000000     C                   move      *on           *in32
232500000000     C* imposto gli attributi del sfl
232600000000     C                   EXSR      AtrS1
232700000000     C                   UPDATE    FMTS1
232800000000     C                   move      *off          *in32
232900000000     C                   ENDIF
233000000000     C*
233100000000     C                   ENDSR
233200000000     C/EJECT
233300000000     C************************************************************
233400000000     C* Sottoconto di riferimento
233500000000     C************************************************************
233600000000     C     F4_Ksc        BEGSR
233700000000     C*
233800000000     C                   RESET                   ANA201DS
233900000000     C*
234000000000     C                   MOVEL     XSCSOC        SOC201
234100000000     C*
234200000000     C                   SELECT
234300000000     C     H1NMFL        WHENEQ    'S1KSC     '
234400000000     C                   MOVEL     S1Kcc         KCC201
234500000000     C                   MOVEL     S1Ksc         KSC201
234600000000     C                   ENDSL
234700000000     C*
234800000000     C                   MOVEL     ANA201DS      KPJBU
234900000000     C                   CALL      'ANA201R'
235000000000     C                   PARM                    KPJBA
235100000000     C                   MOVEL     KPJBU         ANA201DS
235200000000     C*
235300000000     C     KSC201        IFNE      *BLANK
235400000000     C*
235500000000     C                   SELECT
235600000000     C     H1NMFL        WHENEQ    'S1KSC     '
235700000000     C                   Eval      S1Kcc      =  KCC201
235800000000     C                   Eval      S1Ksc      =  KSC201
235900000000     C                   ENDSL
236000000000     C*
236100000000     C                   move      *on           *in32
236200000000     C* imposto gli attributi del sfl
236300000000     C                   EXSR      AtrS1
236400000000     C                   UPDATE    FMTS1
236500000000     C                   move      *off          *in32
236600000000     C*
236700000000     C                   ENDIF
236800000000     C*
236900000000     C                   ENDSR
237000000000     C/EJECT
237100000000     C************************************************************
237200000000     C* Ricerca voce
237300000000     C************************************************************
237400000000     C     F4_Voce       BEGSR
237500000000     C*
237600000000     C                   RESET                   ANA280DS
237700000000     C                   MOVE      '01'          OPZ280
237800000000     C                   MOVE      'S'           FSC280
237900000000     C                   if        Analit002 <>'3'
238000000000     C                   eval      kcc280 = S1kcc
238100000000     C                   eval      ksc280 = S1ksc
238200000000     C                   else
238300000000     C                   clear                   kcc280
238400000000     C                   clear                   Ksc280
238500000000     C                   endif
238600000000     C                   eval      anl280 = analit002
238700000000     C                   clear                   KPJBU
238800000000     C                   MOVEL     ANA280DS      KPJBU
238900000000     C                   CALL      'ANA280R'
239000000000     C                   PARM                    KPJBA
239100000000     C                   MOVEL     KPJBU         ANA280DS
239200000000     C     Voc280        IFNE      *BLANK
239300000000     C                   MOVEL     Voc280        S1Voce
239400000000     C                   move      *on           *in32
239500000000     C* imposto gli attributi del sfl
239600000000     C                   EXSR      AtrS1
239700000000     C                   UPDATE    FMTS1
239800000000     C                   move      *off          *in32
239900000000     C                   ENDIF
240000000000     C*
240100000000     C                   ENDSR
240200000000     C/EJECT
240300000000     C************************************************************
240400000000     C* Ricerca classe
240500000000     C************************************************************
240600000000     C     F4_Classe     BEGSR
240700000000     C*
240800000000     C                   RESET                   XTABDS
240900000000     C*
241000000000     C                   MOVE      '01'          XTAOPZ
241100000000     C                   MOVE      XSCSOC        XTAAZI
241200000000     C                   MOVE      XSCLIN        XTALIN
241300000000     C                   Z-ADD     CURR          XTARIG
241400000000     C                   Z-ADD     CURC          XTACOL
241500000000     C                   MOVE      *ZERO         XTaKey
241600000000     C*
241700000000     C                   MoveL     XTABDS        KPJBU
241800000000     C                   CALL      'ANGG81R'
241900000000     C                   PARM                    KPJBA
242000000000     C*
242100000000     C                   MoveL     KPJBU         XTABDS
242200000000     C                   if        XTaErr <> '1'  and
242300000000     C                             XTaKey <> *zeros
242400000000     C                   Move      XTaKey        S1Classe
242500000000     C                   move      *on           *in32
242600000000     C* imposto gli attributi del sfl
242700000000     C                   EXSR      AtrS1
242800000000     C                   UPDATE    FMTS1
242900000000     C                   move      *off          *in32
243000000000     C                   ENDIF
243100000000     C*
243200000000     C                   ENDSR
243300000000     C/EJECT
243400000000     C************************************************************
243500000000     C* GESTIONE F05 VIDEO S1
243600000000     C************************************************************
243700000000     C     F05S1         BEGSR
243800000000     C*
243900000000     C                   Move      *On           $InzS1
244000000000     C                   ROLBK
244100000000     C*
244200000000     C                   ENDSR
244300000000     C/EJECT
244400000000     C************************************************************
244500000000     C* GESTIONE F08 VIDEO S1
244600000000     C************************************************************
244700000000     C     F08S1         BEGSR
244800000000     C*
244900000000     C                   MOVE      *ON           $FINE
245000130823     C                   MOVE      '0'           RETG70
245100000000     C*
245200000000     C                   ENDSR
245300000000     C/EJECT
245400000000     C************************************************************
245500000000     C* GESTIONE F12 VIDEO S1
245600000000     C************************************************************
245700000000     C     F12S1         BEGSR
245800000000     C*
245900000000     C                   MOVE      'D1'          $GEST
246000000000     C*
246100000000     C                   ENDSR
246200000000     C/EJECT
246300000000     C************************************************************
246400000000     C* GESTIONE F24 VIDEO S1
246500000000     C************************************************************
246600000000     C     F24S1         BEGSR
246700000000     C*
246800000000     C                   MOVEA     $FX2          $FX
246900000000     C                   MOVEA     $FC2          $FC
247000000000     C                   MOVEA     $FR2          $FR
247100000000     C                   Z-ADD     $ULKS1        $ULK
247200000000     C                   Z-ADD     $S1L01        $L01
247300000000     C                   Z-ADD     $S1L02        $L02
247400000000     C                   EXSR      RIEKEY
247500000000     C                   Z-ADD     $ULK          $ULKS1            3 0
247600000000     C                   MOVEA     $FC           $FC2
247700000000     C                   MOVEL     $KEY1         Z2KE1
247800000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
247900000000     C                   MOVEL     $KEY2         Z2KE2
248000000000     C                   WRITE     FMTZ2
248100000000     C*
248200000000     C                   ENDSR
248300000000     C/EJECT
248400000000     C************************************************************
248500000000     C* GESTIONE F06 VIDEO S1
248600000000     C************************************************************
248700000000     C     AGGANA        BEGSR
248800000000     C*
248900000000     C                   SELECT
249000000000     C*
249100130823     C     OPZG70        WHENEQ    '01'
249200130823     C     OPZG70        OREQ      '03'
249300000000     C                   EXSR      WrtDett
249400130823     C                   WRITE     SDGSPR01                             22
249500000000     C* se rcd non scrivibile attivo errore
249600000000     C     *IN22         IFEQ      *ON
249700000000     C                   SETON                                        9599
249800000000     C                   ENDIF
249900000000     C*
250000130823     C     OPZG70        WHENEQ    '02'
250100000000     C*
250200000000     C* cancello i record di dettaglio
250300000000     C                   EXSR      DelDett
250400000000     C*
250500000000     C                   EXSR      WrtDett
250600000000     C*
250700000000     C*
250800130823    >C     OPZG70        WHENEQ    '04'
250900000000     C* cancello i record di dettaglio
251000000000     C                   EXSR      DelDett
251100000000     C*
251200000000     C                   ENDSL
251300000000     C*
251400000000     C* richiama pgm User Exit
251500000000     c*
251600000000     C     USRERR        IFEQ      *BLANK
251700000000     C* passa opzione ricevuta a user exit
251800130823    >C                   MOVEL     OPZG70        XUKOPZ
251900000000     C* imposta chiave univoca  per user exit
252000000000     C                   MOVE      XSCSOC        XUKSOC
252100000000     C                   MOVE      XSCDUN        XUKDUN
252200000000     C                   MOVEL     DSPGM         XUKPGM
252300000000    >C                   MOVEL     D1TipReg      XUKFL1
252400000000    >C                   MOVEL     D1StpReg      XUKFL2
252500000000    >C                   MOVEL     *Blank        XUKFL3
252600000000     C                   MOVEL     XUKDS         KPJBU
252700000000     C     DSPGM         CAT       'U':0         USREXT           10
252800000000     C                   CALL      USREXT                               21
252900000000     C                   PARM                    KPJBA
253000000000     C     *IN21         IFEQ      *ON
253100000000     C                   MOVE      *ON           USRERR            1
253200000000     C                   ENDIF
253300000000     C                   ENDIF
253400000000     C*
253500000000     C*
253600000000     C                   COMMIT
253700000000     C*
253800000000     C* Segnalo al pgm chiamante l'avvenuta conferma
253900130823     C                   Move      '1'           OPRG70
254000000000     C*
254100000000     C* Segnalo al pgm chiamante l'aver premuto l'F6
254200130823     C                   Move      '0'           RETG70
254300000000     C*
254400000000     C                   Select
254500130823     C     OPZG70        WhenEQ    '01'
254600130823     C     OPZG70        oreq      '02'
254700000000     C*
254800000000     C* Riparto
254900000000     C                   MOVE      *On           $InzS1
255000000000     C                   MOVE      'S1'          $LastV
255100000000     C*
255200000000     C                   Other
255300000000     C*
255400000000     C                   MOVE      *On           $Fine
255500000000     C*
255600000000     C                   endsl
255700000000     C*
255800000000     C                   ENDSR
255900000000     C/EJECT
256000000000     C************************************************************
256100130823     C* Cancello righe SDGSPR00F di dettaglio
256200000000     C************************************************************
256300000000     C     DelDett       BEGSR
256400000000     C*
256500130823     C                   Eval      SPRSocieta  = XScSoc
256600130823     C                   Eval      SPRTiPreg   = D1TipReg
256700130823     C                   Eval      SPRStpReg   = D1StpReg
256800130823     C     K03SPR01      SETLL     SDGSPR01L
256900130823     C     K03SPR01      READE     SDGSPR01L                              21
257000000000     C                   Eval      $EndFile    = *In21
257100000000     C*
257200000000     C                   DOW       $EndFile    = *Off
257300130823     C                   DELETE    SDGSPR01
257400000000     C*
257500130823     C     K03SPR01      READE     SDGSPR01L                              21
257600000000     C                   Eval      $EndFile    = *In21
257700000000     C                   ENDDO
257800000000     C*
257900000000     C                   ENDSR
258000000000     C/EJECT
258100000000     C************************************************************
258200130823     C* Aggiorno file SDGSPR00F da sfl
258300000000     C************************************************************
258400000000     C     WrtDett       BEGSR
258500000000     C*
258600000000     C                   Z-Add     0             ContaRighe
258700000000     C*
258800000000     C                   DO        *Hival        $X                4 0
258900000000     C     $X            CHAIN     FMTS1                              21
259000000000     C*
259100000000     C* Fine lettura subfile
259200000000     C                   if        *In21       = *On
259300000000     C                   Leave
259400000000     C                   endif
259500000000     C*
259600000000     C* leggo riga successiva se dati non significativi
259700000000     C                   if        S1TpReg    = *Blank  and
259800000000     C                             S1Libro    = *Blank  and
259900000000     C                             S1Aliq     = *Zeros  and
260000000000     C                             S1RifIVA   = *Blank  and
260100000000     C                             S1Causale  = *Blank  and
260200000000     C                             S1Kcc      = *blank  and
260300000000     C                             S1Ksc      = *blank  and
260400000000     C                             S1Voce     = *blank  and
260500000000     C                             S1Classe   = *blank
260600000000     C                   iter
260700000000     C                   endif
260800000000     C*
260900000000     C                   EXSR      RieANA
261000000000     C*
261100130823     C                   WRITE     SDGSPR01
261200000000     C*
261300000000     C                   ENDDO
261400000000     C*
261500000000     C                   ENDSR
261600000000     C/EJECT
261700000000     C************************************************************
261800130823     C* Aggiorno file SDGSPR00F da sfl - dettaglio
261900000000     C************************************************************
262000000000     C     RieANA        BEGSR
262100000000     C*
262200000000     C                   Add       1             ContaRighe
262300000000     C*
262400130823     C                   CLEAR                   SDGSPR01
262500000000     C*
262600130823     C                   Move      XScCUt        SPRUteImm
262700130823     C     *Eur          Move      *Date         SPRDtImm
262800130823     C                   Move      XScCUt        SPRUteUlMo
262900130823     C     *Eur          Move      *Date         SPRDtUlMo
263000000000     C*
263100130823     C                   Move      XScSoc        SPRSocieta
263200130823     C                   MoveL     D1TipReg      SPRTipReg
263300130823     C                   MoveL     D1StpReg      SPRStpReg
263400130823     C                   Z-ADD     ContaRighe    SPRNrRiga
263500130823     C                   MoveL     S1Nota        SPRNota
263600000000     C*
263700130823     C                   MoveL     S1TpReg       SPRTpReg
263800130823     C                   MoveL     S1Libro       SPRLibro
263900130823     C                   Z-ADD     S1Aliq        SPRAliq
264000130823     C                   MoveL     S1RifIVA      SPRRifIVA
264100130823     C                   MoveL     S1Causale     SPRCausale
264200130823     C                   MoveL     S1Kcc         SPRKcc
264300130823     C                   MoveL     S1Ksc         SPRKsc
264400130823     C                   MoveL     S1Voce        SPRVoce
264500130823     C                   MoveL     S1Classe      SPRClasse
264600000000     C*
264700000000     C                   ENDSR
264800000000     C/EJECT
264900000000     C************************************************************
265000000000     C* CONTROLLO ABILITAZIONE TASTI
265100000000     C* INPUT
265200000000     C* $TASTO =Tasto funzione premuto
265300000000     C* $FC = Tasti abilitati
265400000000     C* OUTPUT
265500000000     C* IN99 = ERrore
265600000000     C************************************************************
265700000000     C     CTRKEY        BEGSR
265800000000     C*
265900000000     C                   SETOFF                                       99
266000000000     C*
266100000000     C                   SELECT
266200000000     C     $TASTO        WHENEQ    F01
266300000000     C                   z-add     01            posfun
266400000000     C     $FC(1)        IFEQ      '0'
266500000000     C                   SETON                                        9997
266600000000     C                   ENDIF
266700000000     C     $TASTO        WHENEQ    F02
266800000000     C                   z-add     02            posfun
266900000000     C     $FC(2)        IFEQ      '0'
267000000000     C                   SETON                                        9997
267100000000     C                   ENDIF
267200000000     C     $TASTO        WHENEQ    F03
267300000000     C                   z-add     03            posfun
267400000000     C     $FC(3)        IFEQ      '0'
267500000000     C                   SETON                                        9997
267600000000     C                   ENDIF
267700000000     C     $TASTO        WHENEQ    F04
267800000000     C                   z-add     04            posfun
267900000000     C     $FC(4)        IFEQ      '0'
268000000000     C                   SETON                                        9997
268100000000     C                   ENDIF
268200000000     C     $TASTO        WHENEQ    F05
268300000000     C                   z-add     05            posfun
268400000000     C     $FC(5)        IFEQ      '0'
268500000000     C                   SETON                                        9997
268600000000     C                   ENDIF
268700000000     C     $TASTO        WHENEQ    F06
268800000000     C                   z-add     06            posfun
268900000000     C     $FC(6)        IFEQ      '0'
269000000000     C                   SETON                                        9997
269100000000     C                   ENDIF
269200000000     C     $TASTO        WHENEQ    F07
269300000000     C                   z-add     07            posfun
269400000000     C     $FC(7)        IFEQ      '0'
269500000000     C                   SETON                                        9997
269600000000     C                   ENDIF
269700000000     C     $TASTO        WHENEQ    F08
269800000000     C                   z-add     08            posfun
269900000000     C     $FC(8)        IFEQ      '0'
270000000000     C                   SETON                                        9997
270100000000     C                   ENDIF
270200000000     C     $TASTO        WHENEQ    F09
270300000000     C                   z-add     09            posfun
270400000000     C     $FC(09)       IFEQ      '0'
270500000000     C                   SETON                                        9997
270600000000     C                   ENDIF
270700000000     C     $TASTO        WHENEQ    F10
270800000000     C                   z-add     10            posfun
270900000000     C     $FC(10)       IFEQ      '0'
271000000000     C                   SETON                                        9997
271100000000     C                   ENDIF
271200000000     C     $TASTO        WHENEQ    F11
271300000000     C                   z-add     11            posfun
271400000000     C     $FC(11)       IFEQ      '0'
271500000000     C                   SETON                                        9997
271600000000     C                   ENDIF
271700000000     C     $TASTO        WHENEQ    F12
271800000000     C                   z-add     12            posfun
271900000000     C     $FC(12)       IFEQ      '0'
272000000000     C                   SETON                                        9997
272100000000     C                   ENDIF
272200000000     C     $TASTO        WHENEQ    F13
272300000000     C                   z-add     13            posfun
272400000000     C     $FC(13)       IFEQ      '0'
272500000000     C                   SETON                                        9997
272600000000     C                   ENDIF
272700000000     C     $TASTO        WHENEQ    F14
272800000000     C                   z-add     14            posfun
272900000000     C     $FC(14)       IFEQ      '0'
273000000000     C                   SETON                                        9997
273100000000     C                   ENDIF
273200000000     C     $TASTO        WHENEQ    F15
273300000000     C                   z-add     15            posfun
273400000000     C     $FC(15)       IFEQ      '0'
273500000000     C                   SETON                                        9997
273600000000     C                   ENDIF
273700000000     C     $TASTO        WHENEQ    F16
273800000000     C                   z-add     16            posfun
273900000000     C     $FC(16)       IFEQ      '0'
274000000000     C                   SETON                                        9997
274100000000     C                   ENDIF
274200000000     C     $TASTO        WHENEQ    F17
274300000000     C                   z-add     17            posfun
274400000000     C     $FC(17)       IFEQ      '0'
274500000000     C                   SETON                                        9997
274600000000     C                   ENDIF
274700000000     C     $TASTO        WHENEQ    F18
274800000000     C                   z-add     18            posfun
274900000000     C     $FC(18)       IFEQ      '0'
275000000000     C                   SETON                                        9997
275100000000     C                   ENDIF
275200000000     C     $TASTO        WHENEQ    F19
275300000000     C                   z-add     19            posfun
275400000000     C     $FC(19)       IFEQ      '0'
275500000000     C                   SETON                                        9997
275600000000     C                   ENDIF
275700000000     C     $TASTO        WHENEQ    F20
275800000000     C                   z-add     20            posfun
275900000000     C     $FC(20)       IFEQ      '0'
276000000000     C                   SETON                                        9997
276100000000     C                   ENDIF
276200000000     C     $TASTO        WHENEQ    F21
276300000000     C                   z-add     21            posfun
276400000000     C     $FC(21)       IFEQ      '0'
276500000000     C                   SETON                                        9997
276600000000     C                   ENDIF
276700000000     C     $TASTO        WHENEQ    F22
276800000000     C                   z-add     22            posfun
276900000000     C     $FC(22)       IFEQ      '0'
277000000000     C                   SETON                                        9997
277100000000     C                   ENDIF
277200000000     C     $TASTO        WHENEQ    F23
277300000000     C                   z-add     23            posfun
277400000000     C     $FC(23)       IFEQ      '0'
277500000000     C                   SETON                                        9997
277600000000     C                   ENDIF
277700000000     C     $TASTO        WHENEQ    F24
277800000000     C                   z-add     24            posfun
277900000000     C     $FC(24)       IFEQ      '0'
278000000000     C                   SETON                                        9997
278100000000     C                   ENDIF
278200000000     C                   ENDSL
278300000000     C*
278400000000     C                   ENDSR
278500000000     C/EJECT
278600000000     C************************************************************
278700000000     C* DEFINIZIONE DELL'ORDINAMENTO
278800000000     C************************************************************
278900000000     C     DEFORD        BEGSR
279000000000     C*
279100000000     C* definisco l'ordinamento della lista a seconda dei valori
279200000000     C* impostati nel video di parzializzazione
279300000000     C*
279400000000     C                   SELECT
279500000000      *  nel caso di più viste logiche e quindi più campi di
279600000000      *  parzializzazione, qui vanno definiti i criteri per
279700000000      *  determinare quale VL va letta, a seconda di quali campi
279800000000      *  sono stati valorizzati
279900000000     C     D1TipReg      WHENNE    *BLANK
280000000000     C                   Z-ADD     1             $ORD
280100000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
280200000000     C* rcd con la chiave passata
280300000000     C*    $Unique       WHENEQ    *ON
280400000000     C*                  Z-ADD     99            $ORD
280500000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
280600000000     C* utilizzerò l'ordinamento precedentemente memorizzato
280700000000     C* (temporaneamente o definitivamente) nei valori assunti
280800000000     C                   OTHER
280900000000     C                   ENDSL
281000000000     C                   ENDSR
281100000000     C/EJECT
281200000000     C************************************************************
281300000000     C* OPERAZIONI INIZIALI
281400000000     C************************************************************
281500000000     C     *INZSR        BEGSR
281600000000     C*
281700000000     C* Reperimento parametri
281800000000     C*
281900000000     C     *ENTRY        PLIST
282000000000     C                   PARM                    KPJBA
282100000000     C                   PARM                    DSAUT
282200000000     C*
282201170822R258 C* Attiva Commitment control se necessario
282202170822 "   C*
282203170822 "   C                   CALLB     'XSTRCMT'
282204170822 "   C                   OPEN      SDGSPR01L
282205170822R258 C*
282300000000     C* Reperimento dati società
282400000000     C*
282500000000     C                   MOVEL     'SOC001'      TIPXSC
282600000000     C                   MOVEL     *BLANK        SOCXSC
282700000000     C                   EXSR      REPSOC
282800000000     C*
282900000000     C* Controlli dati società
283000000000     C*
283100000000     C     RTNXSC        IFNE      '1'
283200000000     C                   MOVEL     XSOCDS        SOC001
283300000000     C                   ELSE
283400130823     C                   MOVE      '1'           ErrG70
283500000000     C                   MOVE      *On           $Fine
283600000000     C                   EXSR      ENDPGM
283700000000     C                   ENDIF
283800000000     C*
283900000000     C* testo se sono stati passati i dati di autorizzazione
284000000000     C     PARMS         IFLT      2
284100000000     C                   MOVE      *BLANK        AUTDS
284200000000     C                   ELSE
284300000000     C                   MOVEL     DSAUT         AUTDS
284400000000     C                   ENDIF
284500000000     C*
284600000000     C* Controlli dati società
284700000000     C                   IF        XSOCDS <> *BLANKS
284800000000     C                   MOVEL     XSOCDS        SOC001
284900000000     C                   ENDIF
285000000000      *
285100000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
285200000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
285300000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
285400000000     C                   Eval      $S1L01 = %Size(Z1Ke1)
285500000000     C                   Eval      $S1L02 = %Size(Z1Ke2)
285600000000     C*
285700000000     C* Reperimento tasti di funzione
285800000000     C*
285900000000     C                   EXSR      REPKEY
286000000000     C*
286100000000     C                   Z-add     1             OPZLEN
286200000000     C*
286201170822R258 X***** Attiva Commitment control se necessario
286202170822 "   X****
286203170822 "   X****               CALLB     'XSTRCMT'
286204170822R258 X****               OPEN      SDGSPR01L
286700000000     C*
286800000000     C                   ENDSR
286900000000     C/EJECT
287000000000     C************************************************************
287100000000     C* INIZIALIZZAZIONE VARIABILI
287200000000     C************************************************************
287300000000     C     INZVAR        BEGSR
287400000000     C*
287500000000     C* Dati da PGM chiamante
287600000000     C*
287700130823     C                   MOVEL     KPJBU         SDGSG70DS
287800000000     C*
287900000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
288000000000     C* reperimento dati società
288100130823     C                   If        SocietaG70 <> XScSoc    and
288200130823     C                             OPZG70 <> *blank
288300000000     C                   MOVEL     'SOC001'      TIPXSC
288400130823     C                   MOVEL     SocietaG70    SOCXSC
288500000000     C                   EXSR      REPSOC
288600000000     C     RTNXSC        IFNE      '1'
288700000000     C                   MOVEL     XSOCDS        SOC001
288800000000     C                   ELSE
288900130823     C                   MOVE      '1'           ERRG70
289000000000     C                   MOVE      *On           $Fine
289100000000     C                   EXSR      ENDPGM
289200000000     C                   ENDIF
289300000000     C                   endif
289400000000     C*
289500000000     C* Reperimento autorizzazioni
289600000000     C*
289700000000      *  nel caso l'anagrafica non gestisse le autorizzazioni
289800000000      *   asteriscare la chiamata alla subroutine REPAUT
289900000000     C* le autorizzazioni potrebbero essere già state passate,
290000000000     C* nel qual caso non vanno reperite
290100000000     C**** AUTDS         IFEQ      *BLANK
290200000000     C****               EXSR      REPAUT
290300000000     C****               ELSE
290400000000     C*****              MOVEL     XCADDS        ------DS
290500000000     C****               ENDIF
290600000000     C*
290700000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
290800000000     C* le autorizzazione
290900000000     C*
291000000000      *  nel caso l'anagrafica non gestisse le autorizzazioni
291100000000      *   asteriscare la chiamata alla subroutine TSTAUT
291200000000     C******             EXSR      TSTAUT
291300000000     C*
291400000000     C* Variabili per gestione videate
291500000000     C*
291600000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
291700000000     C                   MOVE      *On           $INZD1                         inizializzione D1
291800000000     C                   MOVE      *On           $InzS1                         fmt in gestione
291900000000     C                   MOVE      *OFF          $FINE                          fine pgm
292000000000     C*                  MOVE      *OFF          $FlgDef                        fine pgm
292100000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
292200000000     C*
292300000000     C* Subfile chiuso
292400000000     C                   Eval      C1Mode      = *On
292500000000     C*
292600000000     C* Variabili appoggio
292700000000     C*
292800000000     C                   Z-ADD     0             CURR
292900000000     C                   Z-ADD     0             CURC
293000000000     C*
293100000000     C* Valorizzazione campi univoci testate
293200000000     C*
293300000000     C                   CLEAR                   FMTT1
293400000000     C                   MOVEL     KNSIF         NOMSIF
293500000000     C                   MOVEL     XSCDSI        NOMDIT
293600000000     C                   MOVEL     KCDAZ         NOMAZN
293700000000     C* recupero il tipo operazione
293800000000     C                   Z-ADD     1             $X
293900130823     C     OPZG70        LOOKUP    $OZ($X)                                21
294000000000     C     *In21         IFEQ      *ON
294100000000     C                   MOVEL     $ID($X)       IDMSG
294200000000     C                   CALLB     'XRTVM'
294300000000     C                   PARM                    IDMSG            27
294400000000     C                   PARM                    TXTMSG          100
294500000000     C                   PARM                    ERRMSG            1
294600000000     C                   PARM      *ON           CTRMSG            1            centratura
294700000000     C                   PARM      30            LENMSG            3 0          lun output.
294800000000     C     ERRMSG        IFNE      *ON
294900000000     C***                MOVEL     TXTMSG        T1TIT
295000000000     C                   ELSE
295100000000     C***                MOVEL     *ALL'?'       T1TIT
295200000000     C                   ENDIF
295300000000     C                   ENDIF
295301170822R258 C* Valorizzo l'intestazione di default (Comunicazione Polivalente)
295302170822R258 C                   Movel     Int1          T1Int
295303151120R195 C*
295304151120  "  C* Recupero il titolo con il tipo di comunicazione
295305151120  "  C* recupero il titolo con il tipo comunicazione
295306151120  "  C                   MOVEL     *BLANK        T1TIT
295307151120  "  C*
295308170822R195 C* Comunicazione Black list
295309170822R258 X*** Ex R195        If        TpComG70 = 'B'
295310170822R258 C                   Select
295311170822  "  C                   When      TpComG70 = 'T'
295312170822  "  C* Valorizzo l'intestazione per spesometro trimestrale
295313170822  "  C                   Movel     Int4          T1Int
295316151120  "  C*
295317170822  "  C                   if        SOTTITT4   <> *BLANK
295318170822  "  C                   MOVEL     SOTTITT4      IDMSG
295319170822  "  C                   endif
295320170822R258 X*** Ex R195        If        TpComG70 = 'B'
295321170822R258 C                   When      TpComG70 = 'B'
295322170822R195 C*
295323151120  "  C                   if        SOTTITT3   <> *BLANK
295324151120  "  C                   MOVEL     SOTTITT3      IDMSG
295325151120  "  C                   endif
295326170822R195 C*
295327170822R258 X*** Ex R195        else
295328170822R258 C                   Other
295329170822R195 C*
295333151120  "  C* Comunicazione Spesometro
295334151120  "  C                   if        SOTTITT1   <> *BLANK
295335151120  "  C                   MOVEL     SOTTITT1      IDMSG
295336151120  "  C                   endif
295337170822R195 C*
295338170822R258 C                   endSl
295339170822R258 X*** Ex R195        endif
295340170822R195 C*
295344151120  "  C                   CALLB     'XRTVM'
295345151120  "  C                   PARM                    IDMSG            27
295346151120  "  C                   PARM                    TXTMSG          100
295347151120  "  C                   PARM                    ERRMSG            1
295348151120  "  C                   PARM      *ON           CTRMSG            1            centratura
295349151120  "  C                   PARM      30            LENMSG            3 0          lun output.
295350151120  "  C     ERRMSG        IFNE      *ON
295351151120  "  C                   MOVEL     TXTMSG        T1TIT
295352151120  "  C                   ELSE
295353151120  "  C                   MOVEL     *ALL'?'       T1TIT
295354151201R195 C                   ENDIF
295400000000     C*
295500000000     C                   ENDSR
295600000000     C/EJECT
295700000000     C************************************************************
295800000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
295900000000     C************************************************************
296000000000     C     REPAUT        BEGSR
296100000000     C*
296200000000     C                   CLEAR                   AUTDS
296300000000     C*
296400000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
296500000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
296600000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
296700000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
296800000000     C                   MOVEL     '------'      XCAFNC                         FUNZIONE
296900000000     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
297000000000     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
297100000000     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
297200000000     C*
297300000000     C                   CALLB     'XCHKAUT'
297400000000     C                   PARM                    AUTDS
297500000000     C*
297600000000     C* errori/non abilitazione
297700000000     C     XCARTN        IFNE      0
297800130823     C                   MOVE      '2'           ErrG70
297900000000     C                   MOVE      *ON           $FINE
298000000000     C                   EXSR      ENDPGM
298100000000     C                   ELSE
298200000000     C********           MOVEL     XCADDS        ------DS
298300000000     C                   ENDIF
298400000000     C*
298500000000     C                   ENDSR
298600000000     C/EJECT
298700000000     C************************************************************
298800000000     C* REPERIMENTO DATI SOCIETA'
298900000000     C************************************************************
299000000000     C     REPSOC        BEGSR
299100000000     C*
299200000000     C                   CALL      'SDGSOCDR'
299300000000     C                   PARM                    TIPXSC            6
299400000000     C                   PARM                    SOCXSC            3
299500000000     C                   PARM                    CDSXSC            9 0
299600000000     C                   PARM                    MODXSC            3
299700000000     C                   PARM      *BLANKS       RTNXSC            1
299800000000     C                   PARM                    XSOCDS
299900000000     C                   PARM                    KPJBA
300000000000     C*
300100000000     C                   ENDSR
300200000000     C/EJECT
300300000000     C************************************************************
300400000000     C* REPERIMENTO TASTI DI FUNZIONE
300500000000     C************************************************************
300600000000     C     REPKEY        BEGSR
300700000000     C                   MOVEA     $FM1          $FM
300800000000     C                   MOVEA     $FC1          $FC
300900000000     C                   EXSR      REPKKK
301000000000     C                   MOVEA     $FX           $FX1
301100000000     C*
301200000000     C                   MOVEA     $FM2          $FM
301300000000     C                   MOVEA     $FC2          $FC
301400000000     C                   EXSR      REPKKK
301500000000     C                   MOVEA     $FX           $FX2
301600000000     C*
301700000000     C                   ENDSR
301800000000     C/EJECT
301900000000     C************************************************************
302000000000     C* REPERIMENTO TASTI DI FUNZIONE
302100000000     C************************************************************
302200000000     C     REPKKK        BEGSR
302300000000     C*
302400000000     C                   CLEAR                   $FX
302500000000     C     1             DO        24            $X
302600000000     C*
302700000000     C     $FM($X)       IFNE      *BLANK
302800000000     C     $Fc($X)       andne     '0'
302900000000     C*
303000000000     C                   MOVEL     $FM($X)       IDMSG
303100000000     C                   CALL      'XRTVM'
303200000000     C                   PARM                    IDMSG            27
303300000000     C                   PARM                    TXTMSG          100
303400000000     C                   PARM                    ERRMSG            1
303500000000     C*
303600000000     C     ERRMSG        IFNE      *ON
303700000000     C                   MOVEL     TXTMSG        $FX($X)
303800000000     C                   ELSE
303900000000     C                   MOVEL     *ALL' '       $FX($X)
304000000000     C                   ENDIF
304100000000     C*
304200000000     C                   ENDIF
304300000000     C*
304400000000     C                   ENDDO
304500000000     C*
304600000000     C                   ENDSR
304700000000     C/EJECT
304800000000     C************************************************************
304900000000     C* DEFINIZIONE KLIST
305000000000     C************************************************************
305100000000     C     DefKlist      BEGSR
305200000000     C*
305300000000     C* klist
305400000000     C*
305500130823     C* SDGSPR01L
305600130823     C     K03SPR01      KLIST
305700130823     C                   KFLD                    SPRSocieta
305800130823     C                   KFLD                    SPRTipReg
305900130823     C                   KFLD                    SPRStpReg
306000000000     C*
306100130823     C* SDGSPR01L
306200130823     C     K04SPR01      KLIST
306300130823     C                   KFLD                    SPRSocieta
306400130823     C                   KFLD                    SPRTipReg
306500130823     C                   KFLD                    SPRStpReg
306600130823     C                   KFLD                    SPRNrRiga
306700000000     C*
306800130823     C* SDGSPR04L
306900130823     C     K12SPR04      KLIST
307000130823     C                   KFLD                    SPRSocieta
307100130823     C                   KFLD                    SPRTipReg
307200130823     C                   KFLD                    SPRStpReg
307300130823     C                   KFLD                    SPRTpReg
307400130823     C                   KFLD                    SPRLibro
307500130823     C                   KFLD                    SPRAliq
307600130823     C                   KFLD                    SPRRifIVA
307700130823     C                   KFLD                    SPRCausale
307800130823     C                   KFLD                    SPRKcc
307900130823     C                   KFLD                    SPRKsc
308000130823     C                   KFLD                    SPRVoce
308100130823     C                   KFLD                    SPRClasse
308200000000     C*
308300000000     C* ANALR01L
308400000000     C     K05ALR01      klist
308500000000     C                   kfld                    ALRSocieta
308600000000     C                   kfld                    ALRTpReg
308700000000     C                   kfld                    ALRLibro
308800000000     C                   kfld                    ALRAliq
308900000000     C                   kfld                    ALRRifIva
309000000000     C*
309100000000     C* ANRIV02L
309200000000     C     K02RIV02      KLIST
309300000000     C                   KFLD                    RIVSocieta
309400000000     C                   KFLD                    RIVAliq
309500000000     C*
309600000000     C* ANRIV03L
309700000000     C     K02RIV03      KLIST
309800000000     C                   KFLD                    RIVSocieta
309900000000     C                   KFLD                    RIVRifIVA
310000000000     C*
310100000000     C* ANDLI01L
310200000000     C     K03DLI01      klist
310300000000     C                   kfld                    DLISocieta
310400000000     C                   kfld                    DLITpReg
310500000000     C                   kfld                    DLILibro
310600000000     C*
310700000000     C* ANVCI01L
310800000000     C     K02VCI01      klist
310900000000     C                   kfld                    VCISocieta
311000000000     C                   kfld                    VCIVoce
311100000000     C*
311200000000     C                   ENDSR
311300000000     C/EJECT
311400000000** $OZ / $ID
31150000000001PROMSG    *LIBL     COS0001
31160000000002PROMSG    *LIBL     COS0002
31170000000003PROMSG    *LIBL     COS0003
31180000000004PROMSG    *LIBL     COS0004
31190000000005PROMSG    *LIBL     COS0005
312000000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
312100000000PROMSG    *LIBL     KEY0001  01
312200000000PROMSG    *LIBL     KEY0002  02
312300000000PROMSG    *LIBL     KEY0003  03
312400000000PROMSG    *LIBL     KEY0004  04
312500000000PROMSG    *LIBL     KEY0005  05
312600000000PROMSG    *LIBL     KEY0006  06
312700000000PROMSG    *LIBL     KEY0007  07
312800000000PROMSG    *LIBL     KEY0008  08
312900000000PROMSG    *LIBL     KEY0009  09
313000000000PROMSG    *LIBL     KEY0010  10
313100000000PROMSG    *LIBL     KEY0011  11
313200000000PROMSG    *LIBL     KEY0012  12
313300000000PROMSG    *LIBL     KEY0013  13
313400000000PROMSG    *LIBL     KEY0014  14
313500000000PROMSG    *LIBL     KEY0015  15
313600000000PROMSG    *LIBL     KEY0016  16
313700000000PROMSG    *LIBL     KEY0017  17
313800000000PROMSG    *LIBL     KEY0018  18
313900000000PROMSG    *LIBL     KEY0019  19
314000000000PROMSG    *LIBL     KEY0020  20
314100000000PROMSG    *LIBL     KEY0021  21
314200000000PROMSG    *LIBL     KEY0022  22
314300000000PROMSG    *LIBL     KEY0023  23
314400000000PROMSG    *LIBL     KEY0024  24
314500000000**  ABILITAZIONE D1 : $FC1 / $FR1
3146000000000N  01
3147000000001N  02
3148000000001N  03
3149000000001N  04
3150000000000N  05
3151000000000N  06
3152000000000N  07
3153000000000N  08
3154000000000N  09
3155000000000N  10
3156000000000N  11
3157000000001N  12
3158000000000N  13
3159000000000N  14
3160000000000N  15
3161000000000N  16
3162000000000N  17
3163000000000N  18
3164000000000N  19
3165000000000N  20
3166000000000N  21
3167000000000N  22
3168000000000N  23   Impostare a '1' se si utilizzano opzioni
3169000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
317000000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
317100000000PROMSG    *LIBL     KEY0001  01
317200000000PROMSG    *LIBL     KEY0002  02
317300000000PROMSG    *LIBL     KEY0003  03
317400000000PROMSG    *LIBL     KEY0004  04
317500000000PROMSG    *LIBL     KEY0005  05
317600000000PROMSG    *LIBL     KEY0006  06
317700000000PROMSG    *LIBL     KEY0114  07
317800000000PROMSG    *LIBL     KEY0008  08
317900000000PROMSG    *LIBL     KEY0009  09
318000000000PROMSG    *LIBL     KEY0010  10
318100000000PROMSG    *LIBL     KEY0011  11
318200000000PROMSG    *LIBL     KEY0012  12
318300000000PROMSG    *LIBL     KEY0013  13
318400000000PROMSG    *LIBL     KEY0014  14
318500000000PROMSG    *LIBL     KEY0015  15
318600000000PROMSG    *LIBL     KEY0016  16
318700000000PROMSG    *LIBL     KEY0017  17
318800000000PROMSG    *LIBL     KEY0018  18
318900000000PROMSG    *LIBL     KEY0019  19
319000000000PROMSG    *LIBL     KEY0020  20
319100000000PROMSG    *LIBL     KEY0021  21
319200000000PROMSG    *LIBL     KEY0022  22
319300000000PROMSG    *LIBL     KEY0023  23
319400000000PROMSG    *LIBL     KEY0024  24
319500000000**  ABILITAZIONE S1 : $FC2 / $FR2
3196000000000N  01
3197000000000N  02
3198000000001N  03
3199000000001N  04
3200000000001N  05
3201000000001N  06
3202000000000N  07
3203000000001N  08
3204000000001N  09
3205000000000N  10
3206000000000N  11
3207000000001N  12
3208000000000N  13
3209000000000N  14
3210000000000N  15
3211000000000N  16
3212000000000N  17
3213000000000N  18
3214000000000N  19
3215000000000N  20
3216000000000N  21
3217000000000N  22
3218000000000N  23   Impostare a '1' se si utilizzano opzioni
3219000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
