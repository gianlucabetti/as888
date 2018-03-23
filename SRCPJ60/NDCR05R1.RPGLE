000100981016     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200981221     H*PARMS CVTOPT(*datetime)  ACTGRP(PNOTA)
000300170222     H DECEDIT('0,') DATEDIT(*DMY.) ACTGRP('PNOTA') BNDDIR('PJXBND':'PJCBND')
000400940117      *
000500950922      *            MANUTENZIONE RITENUTE D'ACCONTO
000600950922      *           ---------------------------------
000700981116      * C0766 : Gestione altri tributi
000800981116      *        INPS/ENPALS
000900940223      *
001000041116      * D1700 : Modificati i test sul campo CALR105 in quanto ora
001100041116      *       : può valere 'P' e blank (come prima) e 'S' se chiamato
001200041116      *       : dal saldaconto in fase pagamento, nel qual caso non
001300041116      *       : deve gestire gli altri tributi anche se presenti
001400041117      *       : e non deve fare alcune cose
001500981013      *----------------------------------------------------*
001600981013      * indicatori particolari:
001700981020      *12 on registrato da primanota
001800981013      *13 reverse image campo ritenuta d'acconto
001900981013      *14 reverse image data limite versamento
002000981016      *15 reverse image campo ritenuta d'acconto in divisa
002100981016      *16 reverse image campo imponibile soggetto mc
002200010129      *17 reverse image campo imponibile soggetto divisa
002300981013
002400950922      *----------------------------------------------------*
002500951005     Fndmra00F  UF A E             DISK    USROPN
002600951005     F                                     COMMIT
002700951010     Fndmra08L  IF   E           K DISK
002800951005     F                                     RENAME(ndmra000:ndmra08)
002900951005     F                                     INFDS(DSFMTMRA)
003000951010     Fantri01L  IF   E           K DISK
003100951010     Fanope01L  IF   E           K DISK
003200040225C1728FNdPpa01L  IF   E           K DISK
003201170222R235 F*------------
003202170222 "   F* Storico percipienti a regime agevolato
003203170222 "   F* key: Soc/Kcc/Ksc/DtIni(desc)    UNIQUE
003204170222R235 FSDGSPA01L IF   E           K DISK
003300951003      *------------
003400950922     Fndcr05D1  CF   E             WORKSTN
003500950922     F                                     RENAME(cr05D1:FMTD1)
003600981013     F                                     RENAME(cr05D2:FMTD2)
003700950922     F                                     RENAME(cr05T1:FMTT1)
003800950922     F                                     RENAME(cr05Z1:FMTZ1)
003900940201     F                                     INFDS(DSFMT)
004000950922      *----------------------------------------------------*
004100950404     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004200950404     D $ID             S             27    DIM(5) ALT($OZ)
004300950922      * Schiere per la gestione dei tasti di funzione
004400940506     D $FM             S             27    DIM(24)
004500940506     D $FX             S             50    DIM(24)
004600940506     D $FC             S              1    DIM(24)
004700940506     D $FR             S              1    DIM(24)
004800950922      * Video D1
004900940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005000940506     D $FX1            S             50    DIM(24) ALT($FM1)
005100940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005200940506     D $FR1            S              1    DIM(24) ALT($FC1)
005300950928      *Decodifica stati
005400950928     D XSTA            S             16    DIM(3) CTDATA PERRCD(1)
005500950922      *----------------------------------------------------*
005600950726     D $D1L01          S              3  0
005700950726     D $D1L02          S              3  0
005800950922      *-------------
005900950922      * Passaggio Parametri
006000940127     D KPJBA         E DS
006100981221     D*-------------
006200981221     D* ds per passaggio dati
006300981221     D NDMRA         E DS                  EXTNAME(NDMRa00F)
006400981013      *-------------
006500951010      * Passaggio Parametri per ricerca tributi
006600951010     D ANA095DS      E DS
006700981014      *-------------
006800981014     D* Ricerca causale
006900981014     D ANA070DS      E DS                  INZ
007000981014     D  RET070       E                     INZ('0')
007100981014     D  OPR070       E                     INZ('0')
007200981014     D  CMT070       E                     INZ('0')
007300981014     D  ERR070       E                     INZ('0')
007400981014     D  FSC070       E                     INZ('S')
007500981014      *-------------
007600981014     D* Ricerca su tabelle
007700981014     D XTabDS        E DS
007800981014     D*-------------
007900981014     D* Ricerca divise
008000981014     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
008100981014     D*-------------
008200950922      * DS Esterna gestione messaggi per COMIT
008300040225c1728x*XMSGDS        E DS
008400040225c1728D XMSGDS        E DS                  inz
008500940318     D  STS          E                     EXTFLD(MSGSTS)
008600940721     D                                     DIM(333)
008700940318     D  JBA          E                     EXTFLD(MSGJBA)
008800940721     D                                     DIM(502)
008900950928      *
009000040225c1728D*-------------
009100040225  "  D* Ricerca partite
009200040225C1728D NDC085DS      E DS                   inz
009300981016      *---------------------------
009400981016     D* appoggio per calcoli
009500981016     D W3015           S             30s15
009600981019     D W1903           S             19P 3
009700010212D0845D WImpInp         S                   Like(ImporD0202)
009800010212D0845D WImpOut         S                   Like(ImporD0202)
009900951003      *---------------------------
010000950922      * Reperimento Autorizzazioni
010001170222      * AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010400950922      *-------------
010500950922      * Passaggio Autorizzazioni
010501170222D2666D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010502170222  "   * Maschera dati autorizzazioni
010503170222D2666D CONRITDS      E DS
010504170222      *-------------
010505170222      * Passaggio Autorizzazioni
010506170222C1728D DSAUT           DS           512
010700950925      *-------------
010800950925      * Maschera dati autorizzazioni
010900950925      ** ______DS      E DS
011000950925      *-------------
011100950925      * DS gestione gruppo  o azienda
011200950301     D XPAOUT          DS          2000
011300950925      *-------------
011400950925      * Reperimento nome PGM
011500950126     D STATUS         SDS           333
011600950126     D  DSPGM            *PROC
011700950411     D  PARMS            *PARMS
011800001218D0796D  Job_Num              264    269S 0
011900950925      *-------------
012000950925      * Dati di ambiente ottenuti da XSOC
012100950922     D SOC001        E DS                  EXTNAME(XSOC001DS)
012200981016     D    XScNrDecMC                  1S 0 Overlay(XScSkP:6)
012300950925      *-------------
012400950925      * DS Interna per dati di output di XSOC
012500941209     D XSOCDS          DS          1000
012600950925      *-------------
012700981013      *  Parametri in entrata
012800950922     D ndcr05DS1     E DS
012900981021      *-------------
013000981021      *  Ds per reperire numero riga
013100981021     D ndcr09DS      E DS
013200950925      *-------------
013300981216      * Passaggio Parametri al programma guida gestione INPS e altri tributi
013400981216C0766D NDCR01DS      E DS                  inz
013500981116      *-------------
013600940201     D DSFMT           DS
013700940506     D  $TASTO               369    369
013800940201     D  NRG                  370    370
013900940201     D  NCL                  371    371
014000950314     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
014100950925      *-------------
014200950925      * posizione cursore
014300940201     D CURSOR          DS
014400940223     D  RIRI                   1      2B 0 INZ
014500940201     D  R$$                    2      2
014600940223     D  COCO                   3      4B 0 INZ
014700940201     D  C$$                    4      4
014800950925      *-------------
014900950925      * Costanti
015000981116     D*-------------
015100070122C2002D DATMIN          C                   CONST('0001-01-01')
015200981116C0766D LenIn1          S              9B 0
015300981221C0766D LenIn2          S              9B 0
015400981116     D*-------------
015500981116     D* chiamata a XJobC
015600981116     D Pgm             S             10
015700981116     D $Societa        S                   like(MRASocieta)
015800001218D0796D Job_Num_A       S              6A
015900001218D0796D USName          S             10A
016000981116     D*-------------
016100950925      * Tasti di funzione
016200940506     D F01             C                   CONST(X'31')
016300940506     D F02             C                   CONST(X'32')
016400940506     D F03             C                   CONST(X'33')
016500940506     D F04             C                   CONST(X'34')
016600940506     D F05             C                   CONST(X'35')
016700940506     D F06             C                   CONST(X'36')
016800940506     D F07             C                   CONST(X'37')
016900940506     D F08             C                   CONST(X'38')
017000940506     D F09             C                   CONST(X'39')
017100940506     D F10             C                   CONST(X'3A')
017200940506     D F11             C                   CONST(X'3B')
017300940506     D F12             C                   CONST(X'3C')
017400940506     D F13             C                   CONST(X'B1')
017500940506     D F14             C                   CONST(X'B2')
017600940506     D F15             C                   CONST(X'B3')
017700940506     D F16             C                   CONST(X'B4')
017800940506     D F17             C                   CONST(X'B5')
017900940506     D F18             C                   CONST(X'B6')
018000940506     D F19             C                   CONST(X'B7')
018100940506     D F20             C                   CONST(X'B8')
018200940506     D F21             C                   CONST(X'B9')
018300940506     D F22             C                   CONST(X'BA')
018400940506     D F23             C                   CONST(X'BB')
018500940506     D F24             C                   CONST(X'BC')
018600940506     D ENTER           C                   CONST(X'F1')
018700940506     D ROLDWN          C                   CONST(X'F4')
018800940506     D ROLLUP          C                   CONST(X'F5')
018900950927     D*-------------
019000950606     D* Sottotitolo di testata
019100950922     D SOTTIT          C                   CONST('               -
019200950922     D                                            ')
019300950928      *-------------
019400950928      * Variabili per gestione videate
019500950606     D $Gest           S              2A
019600950606     D $Fine           S              1A
019700950606     D $InzD1          S              1A
019800950606     D $LastG          S              2A
019900950606     D $LastV          S              2A
020000950928      *-------------
020100950928      * Variabili appoggio sempre presenti in tutte le anagrafiche
020200950606     D CurR            S              2S 0
020300950606     D CurC            S              2S 0
020400950606     D Win             S             99A
020500950928      *-------------
020600950928      * Variabili appoggio specifiche della singola anagrafica
020700950928      *  Indici
020800950606     D X               S              3S 0
020900950606     D Y               S              3S 0
021000950928     D WIMS            S                   like(D1IMS)
021100950928     D WRAC            S                   like(D1RAC)
021200981016     D Wtota           S                   like(D1totdoc)
021300981016     D WIMSD           S                   like(D1IMSD)
021400981016     D WRACD           S                   like(D1RACD)
021500010212D0845X***totaD          S                   like(D1totdocD)
021600951010     D WWtributo       S                   like(TRIBUTO095)
021700951010     D Wwtriprog       S                   like(TRIPROG095)
021800981016     D Wnrdec          S                   like(xscNrDecMc)
021900981109     D mranrpsav       S                   like(mranrp)
022000981109     D mrasersav       S                   like(mraserpar)
022100981109     D mradrpsav       S                   like(d1drp)
022200000719D0668D MMPagPerc       S              2  0 inz
022300010129D0836D*-----------
022400010129  "  D* indica il valore dell'indicatore 17
022500010129  "  D $Ind17          S              1A
022600010129D0836D*-----------
022700010212D0845D* indica il valore dell'indicatore 15 (acceso per errore
022800010212  "  D* su calcolo ritenuta d'acconto)
022900010212  "  D $Ind15          S              1A
023000010212D0845D*-----------
023100010511D0927D* indica il valore dell'indicatore 18 (acceso per errore nel caso
023200010511  "  D* in cui la somma degli imponibili sogg. e NON sia diversa dal
023300010511  "  D* valore dell'imponibile assoggettabile)
023400010511  "  D $Ind18          S              1A
023500010511D0927D*-----------
023501170222R235 D* Servono a individuare se il percipiente è in regime agevolato
023502170222 "   D OkRegAgev       S              1
023503170222 "   D $FineSPA        S              1A
023504170222 "   D D1DDcAMG        S             10A
023505170222 "   D* Costante per reperimento messaggi
023506170222 "   D TxtIdmsg        C                   CONST('SDMMSG    *LIBL-
023507170222 "   D                                          xxxxxxx')
023508170222R235 D*-----------
023600950928      *
023700950928      * Campo appoggio per date
023800950928     D                 DS
023900950928     D  DATAGG                 1      2  0
024000950928     D  DATAMM                 3      4  0
024100950928     D  DATAAA                 5      6  0
024200950928     D  DATAGGMMAA             1      6  0
024300950928      *
024400950928      * Campo appoggio per date
024500950928     D                 DS
024600950928     D  WTRIDLVmm              1      2  0
024700950928     D  WTRIDLVgg              3      4  0
024800950928     D  WTRIDLV                1      4  0
024900951005      *-------------
025000951003      * DS esterna per dati fornitori
025100951002     D DVAFOR        E DS                  INZ
025200951002     D* Definizioni parametri driver anagrafici
025300951002     DDvaric           S              1
025400951002     DDvaSocieta       S              3
025500951002     DDvaUnita         S              8
025600951002     DDvaStrutt        S             10
025700951002     DDvaDtRif         S               D
025800951002     DDvaLenOut        S              9B 0
025900951002     DDvaSnatura       S              1
026000951002     DDvaCodice        S              8
026100951002     DDvaLineav        S              3
026200951002     DDvaFiliale       S              3  0
026300951002     DDvaSogg          S              8
026400951002     DDvaTpInd         S              2
026500951002     DDvaCdInd         S              3
026600951002     DDvaErrore        S              1
026700951002     DDvaOutput        S           4000
026800951005      *Nr. relativo di record per file NDMRA08l
026900951005     D DSFMTMRA        DS
027000951005     D  NRRMRA               397    400B 0
027100951005      *
027200951005     D Esito           S              1
027300951005     D GesMsg          S              1
027400001221C1447D NoCtrCmb        S              1A
027500001221C1447D DatiGest        S              1A
027600000117C1309D ZDateISO        S               D
027700070122C2002D*-------------
027800070122  "  D* situazione ritenute d'acconto
027900070122C2002D XCHKMRADS     E DS                  INZ
028000001221C1447D*-------------
028100001221  "  D* campi di comodo per calcolo controvalore in m.c.
028200001221  "  D WImporD         S                   Like(ImporD0202)
028300001221C1447D WImporM         S                   Like(ImporM0202)
028400981105     D*-------------
028500981105     D* Definizione DS per recupero msg generati dai moduli
028600981105     D QLenMsg         S              9B 0
028700981105     D QFormat         S              8
028800981105     D QCallMsg        S             10
028900981105     D QCallStack      S              9B 0
029000981105     D QMsgTyp         S             10
029100981105     D QMsgKey         S              4
029200981105     D QWait           S              9B 0
029300981105     D QAction         S             10
029400981105     D QErrCod         DS
029500981105     D  Q1ErrLen                      9B 0 INZ(16)
029600981105     D  Q1LenRet                      9B 0 INZ(16)
029700981105     D  Q1ErrId                       7
029800981105     D QInfo           DS
029900981105     D  Q2BytRet                      9B 0
030000981105     D  Q2BytAva                      9B 0
030100981105     D  Q2Severity                    9B 0
030200981105     D  Q2MsgId                       7
030300981105     D  Q2MsgTyp                      2
030400981105     D  Q2MsgKey                      4
030500981105     D  Q2Reserve                    15
030600981105     D  Q2MsgLen                      9B 0
030700981105     D  Q2MsgLenAv                    9B 0
030800981105     D  Q2MsgTxt                     80
030900981016     D*-------------
031000040225C1728D WExf            S              1A
031100040225     D*-------------
031200981016     D* PROTOTIPAZIONE DELLE PROCEDURE
031300981103     D/COPY *LIBL/SRCCPY,PJX002PR
031400981016     D*-------------
031500950925      *----------------------------------------------------*
031600950925      *                MAIN LINE PROGRAM
031700950925      *----------------------------------------------------*
031800950925      *
031900950925      * inizializzazione variabili
032000940223     C                   EXSR      INZVAR
032100950925      *
032200940223     C     $FINE         DOWEQ     *OFF
032300940202     C     $GEST         CASEQ     'D1'          GESD1
032400981013     C     $GEST         CASEQ     'D2'          GESD2
032500941027     C                   ENDCS
032600941027     C                   ENDDO
032700950925      *
032800950925      * fine programma
032900940325     C                   EXSR      ENDPGM
033000940325     C/EJECT
033100950925      ************************************************************
033200951003      * RICERCHE                                                 *
033300950925      ************************************************************
033400940325     C     ENDPGM        BEGSR
033500061211     c*
033600950925      *
033700950925      * se il COMMIT non è demandato al pgm chiamante
033800981221     C     CMTR105       IFEQ      '0'
033900981221
034000950925      * test se esistono transazioni attive ed eventualmente cosa fare
034100981221     C                   Z-ADD     0             EXECOD
034200981221     C                   CALL      'XRCMTI'
034300981221     C                   PARM                    EXECOD            1 0
034400950925      *
034500981221     C                   SELECT
034600981221      * Annulla richiesta
034700981221     C     EXECOD        WHENEQ    1
034800981221     C                   MOVEL     *OFF          $FINE
034900981221     C                   MOVEL     *HIVAL        $LASTV
035000950925      * Conferma uscita
035100981221     C     EXECOD        WHENEQ    2
035200981221     C                   ROLBK
035300981221
035400981221     C                   ENDSL
035500950925      *
035600981221     C                   ENDIF
035700950925      *
035800950925      * se ho confermato l'uscita proseguo
035900941214     C     $FINE         IFEQ      *ON
036000950925      *
036100950925      * disallocazione rcd nel caso di modifica/annullamento
036200950922     C     OPZr105       IFEQ      '02'
036300950922     C     OPZr105       OREQ      '04'
036400950929     C                   UNLOCK    ndmra00F                             22
036500941027     C                   ENDIF
036600950922      *
036700950922      * passaggio dati al pgm chiamante
036800950922     C                   MOVEL     ndcr05DS1     KPJBU
036900950922      *
037000940325     C                   SETON                                            RT
037100940616     C                   RETURN
037200950922      *
037300941214     C                   ENDIF
037400950922      *
037500940325     C                   ENDSR
037600940325     C/EJECT
037700950922      ************************************************************
037800951003      * RICERCHE                                                 *
037900950922      ************************************************************
038000940127     C     SEARCH        BEGSR
038100950922      *
038200950922      * determino Riga/Colonna del cursore
038300940127     C                   MOVE      NRG           R$$
038400940127     C                   MOVE      NCL           C$$
038500940127     C                   Z-ADD     RIRI          CURR
038600940127     C                   Z-ADD     COCO          CURC
038700950922      *
038800950922      * I campi che al momento dell'F4 risultano protetti non devono
038900950922      * essere abilitati alla funzione di ricerca
039000950922      *
039100941027     C                   SELECT
039200950922      *
039300951010     C* FMTD1  - Tributo
039400951010     C     H1NMFL        WHENEQ    'D1TRI'
039500951010     C     H1NMFL        oreq      'D1PTR'
039600951010     C                   RESET                   ana095ds
039700951010     C                   MOVEL     '01'          OPZ095
039800951010     C                   MOVEL     ana095ds      KPJBU
039900951010     C                   CALL      'ANA095R'
040000951010     C                   PARM                    KPJBA
040100951010     C                   MOVEL     KPJBU         ana095ds
040200951010     C                   MOVEL     tributo095    WWtributo
040300951010     C                   MOVEL     triprog095    WWtriprog
040400951010B03  C     WWtributo     IFNE      *BLANK
040500951010     C                   movel     WWtributo     D1tri
040600951010     C                   movel     WWtriprog     D1ptr
040700951010     C                   movel     *blank        D1trid
040800951010     C                   ENDIF
040900981014     C*
041000981014     C* FMTD1 - Causale principale
041100981014     C     H1NMFL        WHENEQ    'D1CAU'
041200981020     c* e non registrato in primanota
041300981020     c     *in12         andne     *on
041400981014     C                   RESET                   ANA070DS
041500981014     C                   MOVE      '01'          OPZ070
041600981014     C                   MOVE      '1'           tpc070
041700981014     C                   MOVE      '0'           trg070
041800981014     C                   CLEAR                   KPJBU
041900981014     C                   MOVEL     ANA070DS      KPJBU
042000981014     C                   CALL      'ANA070R'
042100981014     C                   PARM                    KPJBA
042200981014     C                   MOVEL     KPJBU         ANA070DS
042300981014     C     Cau070        IFNE      *BLANK
042400981014     C                   MOVEL     Cau070        D1Cau
042500981014     C                   ENDIF
042600981014     C*
042700981014     C* FMTD1  - SERIE
042800981014     C     H1NMFL        WHENEQ    'D1SERREG'
042900981020     c* e non registrato da primanota
043000981020     c     *in12         andne     *on
043100981014     C                   RESET                   XTABDS
043200981014     C                   MOVE      '01'          XTAOPZ
043300981014     C                   MOVE      xscSOC        XTAAZI
043400981014     C                   Z-ADD     CURR          XTARIG
043500981014     C                   Z-ADD     CURC          XTACOL
043600981014     C                   CLEAR                   KPJBU
043700981014     C                   MOVEL     XTABDS        KPJBU
043800981014     C                   CALL      'ANGG13R'
043900981014     C                   PARM                    KPJBA
044000981014     C                   MOVEL     KPJBU         XTABDS
044100981014     C*
044200981014     C     XTAERR        IFNE      '1'
044300981014     C     XTAKEY        ANDNE     *BLANK
044400981014     C                   MOVE      XtaKey        D1SerReg
044500981014     C                   ENDIF
044600981014     C*
044700981014     C* FMTD1  - Divisa
044800981014     C     H1NMFL        WHENEQ    'D1DIV'
044900981020     c* e non registrato da primanota
045000981020     c     *in12         andne     *on
045100981014     C                   RESET                   A015DS
045200981014     C                   MOVEL     '01'          OPZ015
045300981014     C                   MOVEL     A015DS        KPJBU
045400981014     C                   CALL      'ANA015R'
045500981014     C                   PARM                    KPJBA
045600981014     C                   MOVEL     KPJBU         A015DS
045700981014     C     Div015        IFNE      *BLANK
045800981014     C                   MOVEL     Div015        D1Div
045900981014     C                   ENDIF
046000040225C1728c* RICERCA PARTITA
046100040225  "  C     H1NMFL        WHENEQ    'D1NRP'
046200040225  "  C     H1NMFL        oreq      'D1DRP'
046300040225  "  C     H1NMFL        oreq      'D1SERPAR'
046400040225  "  c* SOLO IN IMMISSIONE
046500040225  "  C                   IF        OPZR105  = '01'
046600040225  "  C                   RESET                   NDC085DS
046700040225  "  C                   MOVE      '01'          OPZ085
046800040225  "  C                   MOVE      D1Kcc         KCC085
046900040225  "  C                   MOVE      D1Ksc         KSC085
047000040225  "  C                   MOVE      'CG'          Ctb085
047100040225  "  C                   eval      modalit085 =  '1'
047200040225  "  c                   eval      movdef085 = *on
047300040225  "  c                   eval      movprov085 = *on
047400040225  "  c                   eval      movges085  = *on
047500040225  "  c                   eval      ord085     = 'D'
047600040225  "  c                   eval      divvis085  = '1'
047700040225  "  C                   eval      divsel085  = 'I'
047800040225  "  C                   eval      divisa085  =  D1DIV
047900040225  "  C                   eval      estmul085  = *off
048000040225  "  C                   eval      sitpar085  = *off
048100040225  "  C                   eval      ripsald085 = *off
048200040225  "  C                   MOVE      '0'           Multip085
048300040225  "  C                   CLEAR                   DSAUT
048400040225  "  C                   MOVEL     NDC085DS      KPJBU
048500040225  "  C                   CALL      'NDC085R'
048600040225  "  C                   PARM                    KPJBA
048700040225  "  C                   PARM                    DSAUT
048800040225  "  C                   MOVEL     KPJBU         NDC085DS
048900040225  "  C                   if        Ret085 <> '2'
049000040225  "  c                   eval      d1nrp = NrPar085
049100040225  "  c                   eval      d1serpar=SerPar085
049200040225  "  C                   CALLB     'XDT4000'
049300040225  "  C                   PARM      dtpar085      XDTAMG           10
049400040225  "  C     d1drp         PARM                    XDTGMA            6 0
049500040225  "  C                   PARM      2             XDTSTA            1 0
049600040225  "  C                   endif
049700040225C1728C                   endif
049800950922      * ricerca non abilitata in questa posizione
049900941027     C                   OTHER
050000940926     C                   SETON                                        9899
050100950922      *
050200941027     C                   ENDSL
050300950922      *
050400950922      * imposto pos. del cursore
050500950612     C                   Z-ADD     CURR          H1RIGA
050600950612     C                   Z-ADD     CURC          H1COLO
050700950922      *
050800940506     C                   MOVE      *HIVAL        $LASTV
050900950922      *
051000940127     C                   ENDSR
051100940225     C/EJECT
051200950925      ************************************************************
051300951003      * GESTIONE EMISSIONE TASTI FUNZIONE                        *
051400950925      ************************************************************
051500940225     C     GESPIE        BEGSR
051600950925      *
051700940225     C                   SELECT
051800950925      *
051900940506     C     $GEST         WHENEQ    'D1'
052000940506     C     $ULKD1        IFEQ      0
052100950925      * riempimento opzioni
052200940506     C                   EXSR      F24D1
052300940506     C                   ELSE
052400940506     C                   WRITE     FMTZ1
052500940506     C                   ENDIF
052600981013     C*
052700981013     C     $GEST         WHENEQ    'D2'
052800981013     C     $ULKD1        IFEQ      0
052900981013     C* riempimento tasti
053000981013     C                   EXSR      F24D1
053100981013     C                   ELSE
053200981013     C                   WRITE     FMTZ1
053300981013     C                   ENDIF
053400950925      *
053500940225     C                   ENDSL
053600950925      *
053700940225     C                   ENDSR
053800940131     C/EJECT
053900950925      ************************************************************
054000950926      * GESTIONE VIDEO RECORD D1                                 *
054100950925      ************************************************************
054200940127     C     GESD1         BEGSR
054300950925      * inizializzazione videata
054400941117   B1C     $INZD1        IFEQ      *ON
054500940127     C                   EXSR      INZD1
054600940223     C                   MOVE      *OFF          $INZD1
054700941117   E1C                   ENDIF
054800950925      * emissione piede videata se proveniente da altra
054900950925      * salvataggio valore formato di provenienza
055000941117   B1C     $LASTV        IFNE      'D1'
055100940207     C                   WRITE     FMTT1
055200940225     C                   EXSR      GESPIE
055300940202     C                   MOVE      $LASTV        $LASTG
055400940202     C                   MOVE      'D1'          $LASTV
055500941117   E1C                   ENDIF
055600950925      *
055700950925      * se esistono errori sulla videata
055800950925      * emetto la write del formato a indicatori spenti per vedere
055900950925      * le eventuali decodifiche
056000941117   B1C     *IN99         IFEQ      *ON
056100940707     C     $GEST         ANDEQ     'D1'
056200010129D0836C     *In17         OREQ      *On
056300010129D0836C     $GEST         ANDEQ     'D1'
056400010212D0845C     *In15         OREQ      *On
056500010212D0845C     $GEST         ANDEQ     'D1'
056600010511D0927C     *In18         OREQ      *On
056700010511D0927C     $GEST         ANDEQ     'D1'
056800940707     C                   MOVEA     *IN           WIN
056900981109     C                   MOVE      *ALL'0'       IN3098           69
057000070122     C                   MOVEA     IN3098        *IN(30)
057100010129D0836C                   Eval      $Ind17     =  *In17
057200010129D0836C                   SetOff                                       17
057300010212D0845C                   Eval      $Ind15     =  *In15
057400010212D0845C                   SetOff                                       15
057500010511D0927C                   Eval      $Ind18     =  *In18
057600010511D0927C                   SetOff                                       18
057700940707     C                   WRITE     FMTD1
057800940707     C                   MOVEA     WIN           *IN
057900010129D0836C                   Eval      *In17      =  $Ind17
058000010212D0845C                   Eval      *In15      =  $Ind15
058100010511D0927C                   Eval      *In18      =  $Ind18
058200941117   E1C                   ENDIF
058300950925      *
058400941117   B1C                   SELECT
058500950925      * Annullamento
058600950922     C     OPZr105       WHENEQ    '04'
058700950925      * Visualizzazione
058800950922     C     OPZr105       OREQ      '05'
058900940203     C                   WRITE     FMTD1
059000940203     C                   EXFMT     PROTECT
059100950925      * Immissione/Modifica/Copia
059200941117   X1C                   OTHER
059300940203     C                   EXFMT     FMTD1
059400941117   E1C                   ENDSL
059500040225     c*
059600040225c1728C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
059700040225c1728C                   MOVEL     '1'           WEXF
059800950925      * controllo abilitazione tasti funzione
059900950925      *
060000981013     C                   EXSR      CTRKD1
060100950925      *
060200941117   B1C     *IN99         IFEQ      *OFF
060300950925      *
060400941117   B2C                   SELECT
060500981013     C* ROLLUP
060600981013     C     $TASTO        WHENEQ    ROLLUP
060700981013     C                   MOVE      'D2'          $GEST
060800950925      * F2=Decodifica
060900940916     C     $TASTO        WHENEQ    F02
061000940916     C                   EXSR      F02D1
061100950925      * F3=Fine
061200940506     C     $TASTO        WHENEQ    F03
061300940309     C                   EXSR      F03D1
061400950925      * F4=Ricerca
061500940506     C     $TASTO        WHENEQ    F04
061600940127     C                   EXSR      SEARCH
061700950925      * F5=Refresh
061800940506     C     $TASTO        WHENEQ    F05
061900940309     C                   EXSR      F05D1
062000950925      * F8=Successivo
062100940506     C     $TASTO        WHENEQ    F08
062200940309     C                   EXSR      F08D1
062300950925      * F12=Ritorno
062400940506     C     $TASTO        WHENEQ    F12
062500940309     C                   EXSR      F12D1
062600950925      * F24=Altri tasti
062700940506     C     $TASTO        WHENEQ    F24
062800940315     C                   EXSR      F24D1
062900950925      *
063000941117   X2C                   OTHER
063100940127     C                   EXSR      CTRD1
063200981013     C     *IN99         IFEQ      *OFF
063300981013     C                   EXSR      CTRD2
063400981013     C     *IN99         IFEQ      *ON
063500070122C2002C* Spengo gli indicatori di errore perchè, nel caso si siano usati
063600070122  "  C* indicatori uguali in videate differenti, genererei conflitti
063700070122  "  C                   MOVE      *ALL'0'       IN3098
063800070122C2002C                   MOVEA     IN3098        *IN(30)
063900981013     C                   SETON                                            96
064000981013     C                   ENDIF
064100981013     C                   ENDIF
064200950925      * aggiungere le chiamate ai controlli delle videate supplementari
064300950925      *
064400941117   B3C     $TASTO        IFEQ      F06
064500940207     C     *IN99         ANDEQ     *OFF
064600040225     c*
064700040225C1728c                   eval      msgrtn = 'S'
064800040225  "  c* controllo non bloccante di partita inesistente solo immissione
064900040225  "  C     OPZr105       IFEQ      '01'
065000040225  "  C                   exsr      CtrPartita
065100040225  "  c                   endif
065200040225  "  c* se chiesto di continuare
065300040225C1728c                   if        msgrtn = 'S'
065400950925      * eseguo aggiornamento
065500941214     C                   EXSR      AGGANA
065600950925      * eseguo operazioni del dopo-aggiornamento
065700941117   B4C     *IN99         IFEQ      *OFF
065800940318     C                   EXSR      GESAGG
065900941117   E4C                   ENDIF
066000040225C1728C                   ENDIF
066100040225     C*
066200941117   E3C                   ENDIF
066300950925      *
066400941117   E2C                   ENDSL
066500950925      *
066600941117   E1C                   ENDIF
066700950925      *
066800940117     C                   ENDSR
066900940506     C/EJECT
067000950925      ************************************************************
067100951004      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1                  *
067200950925      ************************************************************
067300981013     C     CTRKD1        BEGSR
067400950925      *
067500940506     C                   SETOFF                                       99
067600940506     C                   MOVEA     $FC1          $FC
067700940506     C                   EXSR      CTRKEY
067800940506     C     *IN99         IFEQ      *ON
067900940506     C                   SETON                                        97
068000940506     C                   ENDIF
068100950925      *
068200940506     C                   ENDSR
068300941209     C/EJECT
068400950925      ************************************************************
068500950925      * INIZIALIZZAZIONE VIDEATA DATI                            *
068600950925      ************************************************************
068700940127     C     INZD1         BEGSR
068800950925      *
068900940131     C                   CLEAR                   FMTD1
069000981013     C                   CLEAR                   FMTD2
069100951004      *
069200950922   B C     OPZr105       IFNE      '01'
069300950929      * L'accesso avviene per numero relativo di record
069400941201   B2C     *IN22         DOUEQ     *OFF
069500940318     C     MSGRTN        ORNE      'R'
069600950925      * se Modifica/Annullamento, il rcd che leggo viene bloccato
069700950922   B C     OPZr105       IFEQ      '02'
069800950922     C     OPZr105       OREQ      '04'
069900951005     C     NRRMRAR105    CHAIN     ndmra000                           2122
070000950925      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
070100950929   X3C                   ELSE
070200951005     C     NRRMRAR105    CHAIN(N)  ndmra000                           2122
070300941201   E3C                   ENDIF
070400950925      *
070500950925      * Gestione RCD allocato
070600941201   B3C     *IN22         IFEQ      *ON
070700950922     C                   MOVEL     'NDMRA00F'    MSGFIL
070800940318     C                   EXSR      MSGPGM
070900941201   E3C                   ENDIF
071000941201   E2C                   ENDDO
071100950925      *
071200950925      * se il rcd da gestire non viene trovato
071300950925      * oppure è allocato da altro pgm
071400950925      * ritorno al pgm guida avvertendolo dell'errore
071500941201   B2C     *IN21         IFEQ      *ON
071600940318     C     *IN22         OREQ      *ON
071700940224     C                   MOVE      *ON           $FINE
071800950922     C                   MOVE      '1'           ERRr105
071900950925      *
072000951003      * Altrimenti
072100941201   X2C                   ELSE
072200981013      * Riempio le videate
072300940224     C                   EXSR      RIED1
072400040427c1741c* se provengo da primanota e stato ritenuta ='1'
072500040427  "  c* uso il totale documento che mi arriva, poichè è quello giusto
072600041116c1741c* nel caso sia stato modificato dalla coge
072700041116D1700X***                if        CalR105   =  'P'        and
072800041116D1700C                   if        CalR105   <> ' '        and
072900041116c1741C                             MRASta    =  '1'
073000081224D2462c                   if        Dar105='A'
073100081224C1741c                   eval      d1Totdoc = TotDocR105
073200081224c1741c                   eval      d1Totdocd= TotDodR105
073300081224D2462c                   else
073400081224  "  c                   eval      d1Totdoc = TotDocR105 * -1
073500081224  "  c                   eval      d1Totdocd= TotDodR105 * -1
073600081224D2462C                   endif
073700040427C1741C                   endif
073800951010      * Calcolo totale documento
073900951010     C                   if        d1totdoc = 0     and
074000951010C    c                             mranasregf <> 0
074100951010     C                   add       d1ims         d1totdoc
074200951010     C                   add       d1imn         d1totdoc
074300951010     C                   add       d1sns         d1totdoc
074400951010     C                   add       d1iva         d1totdoc
074500981014     C                   add       d1imsD        d1totdocD
074600981014     C                   add       d1imnD        d1totdocD
074700981014     C                   add       d1snsD        d1totdocD
074800981014     C                   add       d1ivaD        d1totdocD
074900951010C    c                   endif
075000010126D0836X**** ex D0300      If        CalR105       ='P'       and
075100010126  "  X**** ex D0300                d1totdoc <>   TotDocR105 and
075200010126D0836X**** ex D0300                MRASta    = '0'
075300010126D0836C*
075400041116D1700x***                if        CalR105   =  'P'        and
075500041116D1700C                   if        CalR105   <> ' '        and
075600080117D2325x**** ex d0836                MRASta    =  '0'        and
075700080117D2325c                             (MRASta   =  '0' or MraSTa='1') and
075800080117D0836C                             ((DAR105    =  'D'   and
075900030905D0836C                               D1TotDoc  <> TotDocR105 * -1) or
076000030905D1433C                              (DAR105    =  'A'   and
076100030905D0836C                              D1TotDoc  <> TotDocR105          )
076200030905D1433C                             )
076300030905D0836C*
076400010126  "  C* azzero gli imponibili soggetti, gli imponibili NON soggetti
076500010126  "  C* e gli importi di ritenuta d'acconto
076600010126  "  C                   Z-Add     0             D1IMSD
076700010126  "  C                   Z-Add     0             D1IMS
076800010126  "  C                   Z-Add     0             D1IMND
076900010126  "  C                   Z-Add     0             D1IMN
077000010126  "  C                   Z-Add     0             D1RACD
077100010126  "  C                   Z-Add     0             D1RAC
077200010126D0836C*
077300991105D0300C                   EXSR      RIEDEF
077400991105D0300c                   endif
077500951003      * Definizione indicatori su video
077600940224     C                   SETOFF                                       02
077700941201   B3C                   SELECT
077800950922     C     OPZr105       WHENEQ    '02'
077900940224     C                   SETON                                        02        PR campi key
078000950922     C     OPZr105       WHENEQ    '05'
078100941201   E3C                   ENDSL
078200950925      *
078300941201   E2C                   ENDIF
078400950925      *
078500950102   X1C                   ELSE
078600951005      * IMMISSIONE
078700981020      * Riempio con i dati passati
078800950403     C                   EXSR      RIEDEF
078900950925      *
079000950102   E1C                   ENDIF
079100981022     c*
079200990617      *Proteggo alcuni campi se provengo da pnota e sono in immissione
079300041117     c                   if        calr105 = 'P' and opzr105 = '01'
079400981022     C                   seton                                        11
079500981022     c                   else
079600981022     C                   setoff                                       11
079700981022     C                   endif
079800981020     c* se ritenuta scritta da prima nota, proteggo alcuni campi
079900981020     C                   if        h1nasregf     <> 0
080000981020     C                   seton                                        12
080100981020     c                   else
080200981020     C                   setoff                                       12
080300981020     C                   endif
080400951010      *Decodifiche
080500951010     C                   exsr      ctrd1
080600981013     C                   EXSR      ctrd2
080700981013     c* spengo indicatori di errore
080800981109     C                   MOVE      *ALL'0'       IN3098
080900070122     C                   MOVEA     IN3098        *in(30)
081000951004      *
081100940117     C                   ENDSR
081200940207     C/EJECT
081300950925      ************************************************************
081400950925      * RIEMPIMENTO VIDEATA  D1                                  *
081500950925      ************************************************************
081600940207     C     RIED1         BEGSR
081700950925      *
081800950925      * impostare i campi della videata dai campi del file
081900950925      *
082000950926     C                   eval      d1kcc     =   mrakcc
082100950926     C                   eval      d1ksc     =   mraksc
082200981013     c*
082300981013     C                   eval      d1sta     =   mrasta
082400981013     C                   eval      d1tri     =   mratri
082500981013     C                   eval      d1ptr     =   mraptr
082600981013     C                   eval      d1cau     =   mracod
082700981013     c* registrazione
082800950926     C                   eval      d1nrg     =   mranrg
082900950926     C                   eval      d1serreg  =   mraserreg
083000981013     C                   CALLB     'XDT4000'
083100981013     C                   PARM      mradrg        XDTAMG           10
083200981013     C     d1drg         PARM                    XDTGMA            6 0
083300981013     C                   PARM      2             XDTSTA            1 0
083400981013     c* documento
083500950926     C                   eval      d1ndc     =   mrandc
083600950926     C                   eval      d1serdoc  =   mraserdoc
083700981013     C                   CALLB     'XDT4000'
083800981014     C                   PARM      mraddc        XDTAMG
083900981014     C     d1ddc         PARM                    XDTGMA
084000981014     C                   PARM      2             XDTSTA
084100981013      * partita
084200950926     C                   eval      d1nrp     =   mranrp
084300951010     C                   eval      d1serpar  =   mraserpar
084400981013     C                   CALLB     'XDT4000'
084500981014     C                   PARM      mradrp        XDTAMG
084600981014     C     d1drp         PARM                    XDTGMA
084700981014     C                   PARM      2             XDTSTA
084800981109     c* salvo estremi partita
084900981109     C                   eval      mranrpsav =   mranrp
085000981109     C                   eval      mrasersav =   mraserpar
085100981109     C                   eval      mradrpsav =   d1drp
085200981013      * importi
085300981014     C                   eval      d1div     =   mradiv
085400981014     C                   eval      d1cmo     =   mracmo
085500981014     C                   eval      d1cmp     =   mracmp
085600010801d0973x***                eval      d1imn     =   mraimn
085700010801  "  x***                eval      d1sns     =   mrasns
085800010801  "  x***                eval      d1iva     =   mraiva
085900010801  "  x***                eval      d1ims     =   mraims
086000010801  "  x***                eval      d1rac     =   mrarac
086100010801  "  C                   z-add     mraimn        d1imn
086200010801  "  C                   z-add     mrasns        d1sns
086300010801  "  C                   z-add     mraiva        d1iva
086400010801  "  C                   z-add     mraims        d1ims
086500010801d0973C                   z-add     mrarac        d1rac
086600981013     C                   eval      d1pai     =   mrapai
086700950926     C                   eval      d1pra     =   mrapra
086800981014     C                   eval      d1imnd    =   mraimnd
086900981014     C                   eval      d1snsd    =   mrasnsd
087000981014     C                   eval      d1ivad    =   mraivad
087100981014     C                   eval      d1imsd    =   mraimsd
087200981014     C                   eval      d1racd    =   mraracd
087300981013      *
087400981013     C                   if        d1impass = 0
087500981013     C     d1ims         add       d1imn         d1impass
087600981013     C                   endif
087700981014     C                   if        d1impassD= 0
087800981014     C     d1imsD        add       d1imnD        d1impassD
087900981014     C                   endif
088000981013      *
088100981013      * VIDEATA D2
088200981013      *
088300981013      * data pagamento percipiente
088400981013     C                   CALLB     'XDT4000'
088500981014     C                   PARM      mradpf        XDTAMG
088600981014     C     d1dpf         PARM                    XDTGMA
088700981014     C                   PARM      2             XDTSTA
088800981013      * data limite versamento
088900981013     C                   CALLB     'XDT4000'
089000981014     C                   PARM      mradlv        XDTAMG
089100981014     C     d1dlv         PARM                    XDTGMA
089200981014     C                   PARM      2             XDTSTA
089300981013      * anno competenza
089400951012     C                   eval      d1acf     =   mraacf
089500981013     c* distinta
089600981013     C                   eval      d1ndt     =   mrandt
089700981013     C                   CALLB     'XDT4000'
089800981014     C                   PARM      mraddt        XDTAMG
089900981014     C     d1ddt         PARM                    XDTGMA
090000981014     C                   PARM      2             XDTSTA
090100981013      * quietanza
090200981013     C                   eval      d1nqv     =   mranqv
090300981013     C                   eval      d1sqv     =   mrasqv
090400981013     C                   CALLB     'XDT4000'
090500981014     C                   PARM      mradqv        XDTAMG
090600981014     C     d1dqv         PARM                    XDTGMA
090700981014     C                   PARM      2             XDTSTA
090800981013     C                   eval      d1tpq     =   mratpq
090900981013     c* certificazione
091000981013     C                   eval      d1nrcerti =   mranrcerti
091100981014     C                   CALLB     'XDT4000'
091200981014     C                   PARM      mradtcerti    XDTAMG
091300981014     C     d1dtcerti     PARM                    XDTGMA
091400981014     C                   PARM      2             XDTSTA
091500981014     c* tipo movimento ritenuta
091600981014     C                   eval      d1tpm     =   mratpm
091700981014     c* tributo plurimo
091800981014     C                   eval      d1plurim  =   mraplurim
091900981013      * campi nascosti
092000951010     C                   eval      h1nasregf =   mranasregf
092100951201     C                   eval      h1Sysf =   mrasysf
092200950926      *
092300981020      *  Dati da dvasog
092400951005     C                   exsr      redsog
092500040225c1728c* controlla esistenza partita
092600040225C1728C                   exsr      CtrPartita
092700951011      *
092800951002     C                   ENDSR
092900950403     C/EJECT
093000950925      ************************************************************
093100981020      * RIEMPIMENTO CAMPI DA ds in entrata                       *
093200950925      ************************************************************
093300950403     C     RIEDEF        BEGSR
093400950925      *
093500950926     C                   eval      d1kcc     =   kccr105
093600950926     C                   eval      d1ksc     =   kscr105
093700981222     c*
093800950929     C                   eval      d1serreg  =   serregr105
093900950926     C                   eval      d1nrg     =   nrgr105
094000950926      *
094100950926     C                   CALLB     'XDT4000'
094200950926     C                   PARM      drgr105       XDTAMG           10
094300950926     C     d1drg         PARM                    XDTGMA            6 0
094400950929     C                   PARM      2             XDTSTA            1 0
094500950926      *
094600950926     C                   eval      d1serdoc  =   serdocr105
094700950926     C                   eval      d1ndc     =   ndcr105
094800950926      *
094900950926     C                   CALLB     'XDT4000'
095000950926     C                   PARM      ddcr105       XDTAMG           10
095100950926     C     d1ddc         PARM                    XDTGMA            6 0
095200950929     C                   PARM      2             XDTSTA            1 0
095300950926      *
095400950926     C                   eval      d1serpar     =   serparr105
095500950926     C                   eval      d1nrp     =   nrpr105
095600950926      *
095700950926     C                   CALLB     'XDT4000'
095800950926     C                   PARM      drpr105       XDTAMG           10
095900950926     C     d1drp         PARM                    XDTGMA            6 0
096000950929     C                   PARM      2             XDTSTA            1 0
096100950926      *
096200951003     C                   eval      d1cau    =  codr105
096300010801d0973x***                eval      d1impass =  impassr105
096400010801  "  x***                eval      d1sns    =  snsr105
096500010801  "  x***                eval      d1iva    =  ivar105
096600010801  "  C                   z-add     impassr105    d1impass
096700010801  "  C                   z-add     snsr105       d1sns
096800010801d0973C                   z-add     ivar105       d1iva
096900951003     C                   eval      d1totdoc =  totdocr105
097000951010     C                   eval      h1nasregf =  nasrgfr105
097100951201     C                   eval      h1sysf =  sysfr105
097200981015     C                   eval      d1impassD=  impasDr105
097300981015     C                   eval      d1snsD   =  snsDr105
097400981015     C                   eval      d1ivaD   =  ivaDr105
097500981015     C                   eval      d1totdocD=  totdoDr105
097600981015     C                   eval      d1div    =  divisar105
097700981016     C                   eval      d1cmo    =  cambior105
097800950925      *
097900981020      *  Dati da dvasog
098000951003     C                   exsr      redsog
098100981019     c* se divisa blank, imposto quella della società
098200981019     c                   if        d1div = *blanks
098300981019     c                   eval      d1div = xscdiv
098400981019     c                   endif
098500981020      * se f10 da programma lista, imposto movimento provvisorio
098600981020     c* tipo movimento ritenuta
098700041117     c                   if        opzr105 = '01' and calr105 <> 'P'
098800981020     C                   eval      d1tpm     =   'P'
098900981020     c                   endif
099000951004      *
099100951004     C     DAr105        IFEQ      'D'                                          * A CREDITO
099200951004     C     -1            MULT      d1impass      d1impass                       *IMP.ASSOGGETT.
099300960410     C**** -1            MULT      d1imn         d1imn                          * "  NON SOGG.
099400951004     C     -1            MULT      d1sns         d1sns                          *SPESE NON SOGG
099500951004     C     -1            MULT      d1iva         d1iva                          *I.V.A.
099600951004     C     -1            MULT      d1totdoc      d1totdoc                       *I.V.A.
099700981016     C     -1            MULT      d1impassD     d1impassD                      *IMP.ASSOGGETT.
099800981016     C**** -1            MULT      d1imnD        d1imnD                         * "  NON SOGG.
099900981016     C     -1            MULT      d1snsD        d1snsD                         *SPESE NON SOGG
100000981016     C     -1            MULT      d1ivaD        d1ivaD                         *I.V.A.
100100981016     C     -1            MULT      d1totdocD     d1totdocD                      *I.V.A.
100200951004     C                   ENDIF                                                  *
100300951002      *
100400950403     C                   ENDSR
100500981013     C/EJECT
100600981013     C************************************************************
100700981013     C* GESTIONE VIDEO RECORD  D2
100800981013     C************************************************************
100900981013     C     GESD2         BEGSR
101000981013     C* emissione piede videata se proveniente da altra
101100981013     C* salvataggio valore formato di provenienza
101200981013   B1C     $LASTV        IFNE      'D2'
101300981013     C                   WRITE     FMTT1
101400981013     C                   EXSR      GESPIE
101500981013     C                   MOVE      $LASTV        $LASTG
101600981013     C                   MOVE      'D2'          $LASTV
101700981013   E1C                   ENDIF
101800981013     C*
101900981013     C* se esistono errori sulla videata
102000981013     C* emetto la write del formato a indicatori spenti per vedere
102100981013     C* le eventuali decodifiche
102200981013   B1C     *IN99         IFEQ      *ON
102300981013     C     $GEST         ANDEQ     'D2'
102400981013     C                   MOVEA     *IN           WIN
102500981109     C                   MOVE      *ALL'0'       IN3098
102600070122     C                   MOVEA     IN3098        *in(30)
102700981013     C                   WRITE     FMTD2
102800981013     C                   MOVEA     WIN           *IN
102900981013   E1C                   ENDIF
103000981013     C*
103100981013   B1C                   SELECT
103200981013     C* Annullamento
103300981013     C     OPZR105       WHENEQ    '04'
103400981013     C* Visualizzazione
103500981013     C     OPZR105       OREQ      '05'
103600981013     C                   WRITE     FMTD2
103700981013     C                   EXFMT     PROTECT
103800981013     C* Immissione/Modifica/Copia
103900981013   X1C                   OTHER
104000981013     C                   EXFMT     FMTD2
104100981013   E1C                   ENDSL
104200981013     C* controllo abilitazione tasti funzione
104300981013     C*
104400981013     C                   EXSR      CTRKD1
104500981013     C*
104600981013   B1C     *IN99         IFEQ      *OFF
104700981013     C*
104800981013   B2C                   SELECT
104900981013     C* ROLDOWN
105000981013     C     $TASTO        WHENEQ    ROLDWN
105100981013     C                   MOVE      'D1'          $GEST
105200981013     C* F3=Fine
105300981013     C     $TASTO        WHENEQ    F03
105400981013     C                   EXSR      F03D1
105500981013     C* F4=Ricerca
105600981013     C     $TASTO        WHENEQ    F04
105700981013     C                   EXSR      SEARCH
105800981013     C* F5=Refresh
105900981013     C     $TASTO        WHENEQ    F05
106000981013     C                   EXSR      F05D1
106100981013     C* F8=Successivo
106200981013     C     $TASTO        WHENEQ    F08
106300981013     C                   EXSR      F08D1
106400981013     C* F12=Ritorno
106500981013     C     $TASTO        WHENEQ    F12
106600981013     C                   EXSR      F12D1
106700981013     C* F24=Altri tasti
106800981013     C     $TASTO        WHENEQ    F24
106900981013     C                   EXSR      F24D1
107000981013     C*
107100981013   X2C                   OTHER
107200981013     C                   EXSR      CTRD2
107300981013   B3C     *IN99         IFEQ      *OFF
107400981013     C                   EXSR      CTRD1
107500981013   B3C     *IN99         IFEQ      *ON
107600070122C2002C* Spengo gli indicatori di errore per il controllo su altri schermi
107700070122  "  C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
107800070122  "  C* videate
107900070122  "  C                   MOVE      *ALL'0'       IN3098
108000070122C2002C                   MOVEA     IN3098        *IN(30)
108100981013     C                   SETON                                            96
108200981013   E4C                   ENDIF
108300981013   E3C                   ENDIF
108400981013     C*
108500981013   B3C     $TASTO        IFEQ      F06
108600981013     C     *IN99         ANDEQ     *OFF
108700040225     c*
108800040225C1728c                   eval      msgrtn = 'S'
108900040225  "  c* controllo non bloccante di partita inesistente solo immissione
109000040225  "  C     OPZr105       IFEQ      '01'
109100040225  "  C                   exsr      CtrPartita
109200040225  "  C                   ENDIF
109300040225  "  c* se chiesto di continuare
109400040225c1728c                   if        msgrtn = 'S'
109500981013     C* eseguo aggiornamento
109600981013     C                   EXSR      AGGANA
109700981013     C* eseguo operazioni del dopo-aggiornamento
109800981013   B4C     *IN99         IFEQ      *OFF
109900981013     C                   EXSR      GESAGG
110000981013   E4C                   ENDIF
110100040225C1728C                   ENDIF
110200040225     c*
110300981013   E3C                   ENDIF
110400981013     C*
110500981013   E2C                   ENDSL
110600981013     C*
110700981013   E1C                   ENDIF
110800981013     C*
110900981013     C                   ENDSR
111000981013     C/EJECT
111100981013     C************************************************************
111200981013     C* CONTROLLO VIDEATA  D2
111300981013     C************************************************************
111400981013     C     CTRD2         BEGSR
111500981013     C*
111600981013     C                   SETOFF                                       9914
111700981013     C* non imposto pos. del cursore
111800981013     C                   Z-ADD     0             H1RIGA
111900981013     C                   Z-ADD     0             H1COLO
112000981013     C*
112100981013     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
112200981013     C*
112300981013   B1C     OPZR105       IFNE      '04'
112400981013      *
112500981013      *Impostazione date
112600981013      *
112700981013 b02 C                   if        d1dqv  <> 0
112800981013 b03 C                   if        d1dpf  =  0
112900981013     C                   eval      d1dpf = d1dqv
113000981013 e03 C                   endif
113100981013      *
113200981013 b03 C                   if        d1dlv  =  0
113300981013     C                   eval      d1dlv = d1dqv
113400981013 e03 C                   endif
113500981013      *
113600981013 b03 C                   if        d1ndt  =  0
113700981013     C                   eval      d1ndt = *all'9'
113800981013 e03 C                   endif
113900981013      * *
114000981013     C                   else
114100981013      * *
114200981013 b03 C                   if        d1dpf  <> 0
114300981013     C                   eval      DATAggmmaa = d1dpf
114400000719D0668C                   eval      MMPagPerc  = DATAmm
114500981013     C                   eval      WTRIDLV    = tridtlver
114600981013     C                   eval      DATAgg     = WTRIDLVgg
114700981013 b04 C                   if        WTRIDLVmm <> 0
114800981013     C                   eval      DATAmm = WTRIDLVmm
114900000719D0668C                   If        DATAmm <  MMPagPerc
115000981013     C                   add       1             DATAaa
115100000719D0668C                   endif
115200981013     C                   else
115300981013     C                   add       1             DATAmm
115400981013 b05 C                   if        DATAmm > 12
115500981013     C                   add       1             DATAaa
115600981013     C                   z-add     1             DATAmm
115700981013 e05 C                   endif
115800981013 e04 C                   endif
115900040930D1666C* controllo giorno finale per mesi <> da 31 giorni
116000040930D1666c                   EXSR      CtrGiorno
116100981013     C                   eval      d1dlv  = DATAggmmaa
116200981013 e03 C                   endif
116300981013 b03 C                   if        d1dqv = 0 and d1dpf = 0
116400981013     C                   eval      d1dlv  = 0
116500981013 e03 C                   endif
116600981013 e02 C                   endif
116700981013      *
116800981013      *Controllo formale date
116900070122C2002C                   Eval      D1DPFIso    = DatMin
117000981013 b02 C                   if        d1dpf <> 0
117100981013     C                   CALLB     'XDT4000'
117200981013     C                   PARM                    XDTAMG           10
117300981013     C                   PARM      d1dpf         XDTGMA            6 0
117400981013     C                   PARM      3             XDTSTA            1 0
117500981013 b03 C                   if        xdtsta <> 0
117600981013     C                   seton                                        9976
117700981013     C                   else
117800981013     C                   move      xdtamg        d1dpfiso         10
117900981013 e03 C                   endif
118000981013 e02 C                   endif
118100981013      *
118200981013 b02 C                   if        d1dqv <> 0
118300981013     C                   CALLB     'XDT4000'
118400981013     C                   PARM                    XDTAMG           10
118500981013     C                   PARM      d1dqv         XDTGMA            6 0
118600981013     C                   PARM      3             XDTSTA            1 0
118700981013 b03 C                   if        xdtsta <> 0
118800981013     C                   seton                                        9977
118900981013 e03 C                   endif
119000981013 e02 C                   endif
119100981013      *
119200981013      *
119300981013 b02 C                   if        d1ndt <> 0  and d1dlv = 0
119400981013     C                   seton                                          14
119500981013 e02 C                   endif
119600981013      *
119700981013 b02 C                   if        d1dqv <> 0  and d1nqv = *blank
119800981013     C                   seton                                        9979
119900981013 e02 C                   endif
120000981013      *
120100981013 b02 C                   if        d1dqv = 0  and d1nqv <> *blank
120200981013     C                   seton                                        9977
120300981013 e02 C                   endif
120400981013      *
120500981013 b02 C                   if        *in79 <> *on and *in77 <> *on
120600981013     C                             and d1dqv <> 0 and d1tpq = *blank
120700981013     C                   seton                                        9980
120800981013 e02 C                   endif
120900981013      *
121000981013 b02 C                   if        d1tpq <> ' ' and d1tpq <> 'E'
121100981013     C                             and d1tpq <> 'T' and d1tpq <> 'P'
121200981013     C                             and d1tpq <> 'B'
121300981013     C                   seton                                        9958
121400981013 e02 C                   endif
121500981016     c* tipo movimento ritenuta
121600981016 b02 C                   if        d1tpm <> ' ' and d1tpm <> 'P'
121700981016     C                   seton                                        9957
121800981016 e02 C                   endif
121900981013      *
122000981013      *Controllo stato del movimento
122100981013      *
122200981013 b02 C     d1dlv         IFNE      0                                            *
122300981013     C                   MOVE      '1'           d1sta                          *
122400981013     C                   MOVEL     XSTA(1)       d1stad                         *
122500981013     C                   ELSE                                                   *
122600981013     C                   MOVE      '0'           d1sta                          *
122700981013     C                   MOVE      *BLANKS       d1stad                         *
122800981013     C                   MOVEL     'IMMESSO'     d1stad                         *
122900981013 e02 C                   END                                                    *
123000981013 b02 C     d1ndt         IFNE      0                                            *
123100981013     C                   MOVE      '2'           d1sta                          *
123200981013     C                   MOVEL     XSTA(2)       d1stad                         *
123300981013 e02 C                   END                                                    *
123400981013 b02 C     d1nqv         IFNE      *BLANKS                                      *
123500981013     C                   MOVE      '3'           D1STA                          *
123600981013     C                   MOVEL     XSTA(3)       d1stad                         *
123700981013 e02 C                   END                                                    *
123800981013      *
123900981013      *Anno competenza fiscale
124000981013      *
124100981013     C                   Z-ADD     0             d1acf                          *
124200981013 b02 C     d1dpf         IFNE      0                                            *
124300981013     C                   MOVEL     d1dpfiso      d1acf                          *
124400981013 e02 C                   END                                                    *
124500981013     c*
124600981013     C                   if        d1ndt <> 0 and d1ddt = 0
124700981013     C                   eval      D1DDT = D1DQV
124800981013     C                   endif
124900981013      *
125000981013     C                   if        d1ndt = 0 and d1ddt <> 0
125100981013     C                   seton                                        9988
125200981013     C                   endif
125300981013      *
125400981013     C                   if        d1ndt <> 0 and d1ddt =  0
125500981013     C                   seton                                        9989
125600981013     C                   endif
125700981013      *
125800981013 b02 C                   if        d1ddt <> 0
125900981013     C                   CALLB     'XDT4000'
126000981013     C                   PARM                    XDTAMG           10
126100981013     C                   PARM      d1ddt         XDTGMA            6 0
126200981013     C                   PARM      3             XDTSTA            1 0
126300981013 b03 C                   if        xdtsta <> 0
126400981013     C                   seton                                        9990
126500981013 e03 C                   endif
126600981013 e02 C                   endif
126700981013     c*
126800981013     C                   ENDIF
126900981013     C*
127000981013     c                   endsr
127100040930D1666C************************************************************
127200040930  "  C* COTNROLLI Sul giorno finale per mesi <> da 31
127300040930D1666C************************************************************
127400040930     C     CtrGiorno     BEGSR
127500040930     C*
127600040930     c                   if        dataMM = 04 or DataMm = 06 or dataMM
127700040930     c                             = 09 or dataMM = 11
127800040930     c                   If        dataGG = 31
127900040930     c                   z-add     30            DataGG
128000040930    6c                   endif
128100040930     c                   endif
128200040930     c*
128300040930     C* controllo giorno finale per febbraio
128400040930     c                   if        dataMM = 02
128500060118d1932c                             AND (dataGG = 31  or datagg=30)
128600040930     c* controllo se anno bisestile
128700040930     C                   EXSR      BISEST
128800040930     c                   if        flgbis = *off
128900040930     c                   z-add     28            DataGg
129000040930     c                   else
129100040930     c                   z-add     29            DataGG
129200040930     c                   endif
129300040930     c                   endif
129400040930     C*
129500040930D1666c                   endsr
129600040930D1666C************************************************************
129700040930  "  C* ROUTINE PER VEDERE SE ANNO BISESTILE
129800040930D1666C************************************************************
129900040930     C     BISEST        BEGSR
130000040930     C*
130100040930     C* Da $A = anno
130200040930     C* restituisce $G = giorno
130300040930     C*
130400040930     C     DataAA        IFGE      70
130500040930     C                   MOVEL     19            $AA               4 0
130600040930     C                   ELSE
130700040930     C                   MOVEL     20            $AA
130800040930     C                   ENDIF
130900040930     C                   MOVE      DataAA        $AA
131000040930     C     $AA           DIV       4             RIS               2 0
131100040930     C                   MVR                     RESTO             2 0
131200040930     C     RESTO         IFEQ      0
131300040930     C                   MOVE      *on           flgbis            1
131400040930     C                   ELSE
131500040930     C                   MOVE      *off          flgbis
131600040930     C                   ENDIF
131700040930     C*
131800040930D1666C                   ENDSR
131900940916     C/EJECT
132000950925      ************************************************************
132100950925      * GESTIONE F02 VIDEO D1                                    *
132200950925      ************************************************************
132300940916     C     F02D1         BEGSR
132400950925      *
132500950925      * per decodificare la videata richiamo la routine dei ctrl
132600950925      * e poi disattivo gli eventuali messaggi di errore
132700940916     C                   EXSR      CTRD1
132800950925      *
132900981109     C                   MOVE      *ALL'0'       IN3098
133000070122     C                   MOVEA     IN3098        *in(30)
133100950925      *
133200940916     C                   ENDSR
133300940309     C/EJECT
133400950925      ************************************************************
133500950925      * GESTIONE F03 VIDEO D1                                    *
133600950925      ************************************************************
133700940309     C     F03D1         BEGSR
133800061211     c*
133900061211c1988 *Se chiamato dalla pnota e si è in immissione e non si è
134000061211  "  c* dato f6, messaggio di avvertimento
134100061211  "  c                   if        Calr105 = 'P' and opzr105 = '01'
134200061211  "  c                             and OprR105 <> *on
134300061211  "  c                   exsr      MsgAvvert
134400061211  "  c* se chiesto di continuare
134500061211c1988c                   if        msgrtn = 'S'
134600940309     C                   MOVE      *ON           $FINE
134700950926     C                   MOVE      '1'           RETr105
134800940325     C                   EXSR      ENDPGM
134900061211C1988c                   endif
135000061211  "  c                   else
135100061211  "  C                   MOVE      *ON           $FINE
135200061211  "  C                   MOVE      '1'           RETr105
135300061211  "   * fine programma
135400061211  "  C                   EXSR      ENDPGM
135500061211C1988c                   endif
135600950925      *
135700940325     C                   ENDSR
135800940309     C/EJECT
135900950925      ************************************************************
136000950925      * GESTIONE F05 VIDEO D1                                    *
136100950925      ************************************************************
136200940309     C     F05D1         BEGSR
136300950925      *
136400940309     C                   MOVE      *ON           $INZD1
136500950925      *
136600940309     C                   ENDSR
136700940309     C/EJECT
136800950925      ************************************************************
136900950925      * GESTIONE F08 VIDEO D1                                    *
137000950925      ************************************************************
137100940309     C     F08D1         BEGSR
137200061211c1988 *Se chiamato dalla pnota e si è in immissione e non si è
137300061211  "  c* dato f6, messaggio di avvertimento
137400061211  "  c                   if        Calr105 = 'P' and opzr105 = '01'
137500061211  "  c                             and OprR105 <> *on
137600061211  "  c                   exsr      MsgAvvert
137700061211  "  c* se chiesto di continuare
137800061211c1988c                   if        msgrtn = 'S'
137900950925      *
138000940309     C                   MOVE      *ON           $FINE
138100950926     C                   MOVE      '0'           RETr105
138200061211c1988c                   endif
138300061211  "  c                   else
138400061211  "  C                   MOVE      *ON           $FINE
138500061211  "  C                   MOVE      '0'           RETr105
138600061211c1988c                   endif
138700950925      *
138800940309     C                   ENDSR
138900940309     C/EJECT
139000950925      ************************************************************
139100950925      * GESTIONE F12 VIDEO D1                                    *
139200950925      ************************************************************
139300940309     C     F12D1         BEGSR
139400061211c1988 *Se chiamato dalla pnota e si è in immissione e non si è
139500061211  "  c* dato f6, messaggio di avvertimento
139600061211  "  c                   if        Calr105 = 'P' and opzr105 = '01'
139700061211  "  c                             and OprR105 <> *on
139800061211  "  c                   exsr      MsgAvvert
139900061211  "  c* se chiesto di continuare
140000061211c1988c                   if        msgrtn = 'S'
140100950925      *
140200940309     C                   MOVE      *ON           $FINE
140300950926     C                   MOVE      '2'           RETr105
140400061211c1988c                   endif
140500061211  "  c                   else
140600061211  "  C                   MOVE      *ON           $FINE
140700061211  "  C                   MOVE      '2'           RETr105
140800061211c1988c                   endif
140900950925      *
141000940309     C                   ENDSR
141100940309     C/EJECT
141200950925      ************************************************************
141300950925      * GESTIONE F24 VIDEO D1                                    *
141400950925      ************************************************************
141500940309     C     F24D1         BEGSR
141600950925      *
141700940506     C                   MOVEA     $FX1          $FX
141800940506     C                   MOVEA     $FC1          $FC
141900940506     C                   MOVEA     $FR1          $FR
142000940506     C                   Z-ADD     $ULKD1        $ULK
142100950726     C                   Z-ADD     $D1L01        $L01
142200950726     C                   Z-ADD     $D1L02        $L02
142300940506     C                   EXSR      RIEKEY
142400940506     C                   Z-ADD     $ULK          $ULKD1            3 0
142500940720     C                   MOVEA     $FC           $FC1
142600940506     C                   MOVEL     $KEY1         Z1KE1
142700060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
142800940506     C                   MOVEL     $KEY2         Z1KE2
142900940506     C                   WRITE     FMTZ1
143000950925      *
143100940309     C                   ENDSR
143200940207     C/EJECT
143300950925      ************************************************************
143400951004      * CONTROLLO VIDEATA                                        *
143500950925      * non usare i seguenti indicatori (perchè relativi a D1MSG):
143600951004      * 95 96 97 98                                              *
143700950925      ************************************************************
143800940127     C     CTRD1         BEGSR
143900950925      *
144000981016     C                   SETOFF                                       991315
144100010129     C                   SETOFF                                       1617
144200950925      * non imposto pos. del cursore
144300940127     C                   Z-ADD     0             H1RIGA
144400940127     C                   Z-ADD     0             H1COLO
144500950925      *
144600950925      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
144700950925      *
144800951003 b01 C     OPZr105       IFNE      '04'
144900951003      * Tributo
145000950927     C                   eval      d1trid = *blank
145100951003 b02 C                   if        d1tri <> *blank
145200950927     C                   eval      tritributo = d1tri
145300950927     C                   eval      tritriprog = d1ptr
145400950927     C     k03tri01      chain     antri01l                           21
145500951003 b03 C                   if        *in21 = *off
145600950927     C                   eval      d1trid = tridescr
145700971224     C     d1pai         ifeq      0
145800951004     C                   eval      d1pai = triassimp
145900971224 e03 C                   endif
146000971224     C     d1pra         ifeq      0
146100951004     C                   eval      d1pra = triritacc
146200971224 e03 C                   endif
146300950927     C                   else
146400950927     C                   seton                                        9960
146500950927     C                   eval      d1pai = 0
146600950927     C                   eval      d1pra = 0
146700951003 e03 C                   endif
146800950927     C                   else
146900950927     C                   seton                                        9961
147000951003 e02 C                   endif
147100981013      *
147200981013      *Controllo stato del movimento
147300981013      *
147400981013 b02 C     d1dlv         IFNE      0                                            *
147500981013     C                   MOVE      '1'           d1sta                          *
147600981013     C                   MOVEL     XSTA(1)       d1stad                         *
147700981013     C                   ELSE                                                   *
147800981013     C                   MOVE      '0'           d1sta                          *
147900981013     C                   MOVE      *BLANKS       d1stad                         *
148000981013     C                   MOVEL     'IMMESSO'     d1stad                         *
148100981013 e02 C                   END                                                    *
148200981013 b02 C     d1ndt         IFNE      0                                            *
148300981013     C                   MOVE      '2'           d1sta                          *
148400981013     C                   MOVEL     XSTA(2)       d1stad                         *
148500981013 e02 C                   END                                                    *
148600981013 b02 C     d1nqv         IFNE      *BLANKS                                      *
148700981013     C                   MOVE      '3'           D1STA                          *
148800981013     C                   MOVEL     XSTA(3)       d1stad                         *
148900981013 e02 C                   END                                                    *
149000951003      *
149100951003      * Causale
149200950928     C                   eval      d1caud = *blank
149300951003 b02 C                   if        d1cau <> *blank
149400950929     C                   eval      opecausale = d1cau
149500950927     C     k02ope01      chain     anope01l                           21
149600951003 b03 C                   if        *in21 = *off
149700950927     C                   eval      d1caud = opedesbrev
149800950927     C                   else
149900950927     C                   seton                                        9962
150000951003 e03 C                   endif
150100950927     C                   else
150200950927     C                   seton                                        9963
150300951003 e02 C                   endif
150400951003      *
150500951003      * Riferimento documento
150600950927      *
150700951003 b02 C                   if        d1ndc = 0
150800950928     C                   seton                                        9965
150900951003 e02 C                   endif
151000950927      *
151100951003      * Riferimento registrazione
151200951003 b02 C                   if        d1serreg = *blank
151300950928     C                   seton                                        9968
151400951005     C                   else
151500951005     C                   CallB     'NDMVC108'
151600951005     C                   PARM                    d1serreg
151700951005     C                   PARM                    d1cau
151800951005     C                   PARM                    xscsoc
151900951005     C                   PARM                    *omit
152000951005     C                   PARM                    *omit
152100951005     C                   PARM                    *omit
152200951005     C                   PARM      *off          GesMsg
152300951005     C                   PARM                    Esito
152400951005     C                   PARM                    *omit
152500951005     C                   PARM                    *omit
152600951005     C                   PARM                    *omit
152700951005     C     Esito         IFEQ      *ON
152800951005     C                   SETON                                        9299
152900951005     C                   endif
153000951003 e02 C                   endif
153100950927      *
153200951003 b02 C                   if        d1nrg = 0
153300950928     C                   seton                                        9969
153400951003 e02 C                   endif
153500951018      *
153600951018 b02 C                   if        d1ddc = 0
153700951018     C                   seton                                        9966
153800951018     C                   else
153900951018     C                   CALLB     'XDT4000'
154000951018     C                   PARM                    XDTAMG           10
154100951018     C                   PARM      d1ddc         XDTGMA            6 0
154200951018     C                   PARM      3             XDTSTA            1 0
154300951018 b03 C                   if        xdtsta <> 0
154400951018     C                   seton                                        9967
154500951018 e03 C                   endif
154600951018 e02 C                   endif
154601170222R235 C*
154602170222  "  C* controllo se si tratta di percipiente in regime agevolato
154603170222  "  C                   EXSR      CtrRegAgev
154604170222R235 C*
154800951003 b02 C                   if        d1drg = 0
154900981016     C                   seton                                        9971
155000950927     C                   else
155100950927     C                   CALLB     'XDT4000'
155200950927     C                   PARM                    XDTAMG           10
155300950927     C                   PARM      d1drg         XDTGMA            6 0
155400950929     C                   PARM      3             XDTSTA            1 0
155500951003 b03 C                   if        xdtsta <> 0
155600981016     C                   seton                                        9970
155700120904A1357C                   else
155800120904  "  C     OPZR105       ifeq      '01'
155900120904  "  C     OPZR105       oreq      '03'
156000120904  "  C                   if        DVFDtFine <> *loval
156100120904  "  C                   move      DVFDtFine     XDTAMG
156200120904  "  C                   CALLb     'XDT4000'
156300120904  "  C                   PARM                    XDTAMG           10
156400120904  "  C                   PARM                    XDTGMA            6 0
156500120904  "  C                   PARM      1             XDTSTA            1 0
156600120904  "  C     XDTSTA        ifeq      0
156700120904  "  C     D1Drg         andge     XDtGma
156800120904  "  C                   Seton                                        8599
156900120904  "  C                   endif
157000120904  "  C                   endif
157100120904A1357C                   endif
157200951003 e03 C                   endif
157300951003 e02 C                   endif
157400950927      *
157500951003      * Riferimento partita
157600981013      *
157700961203 b02 C                   if        d1serpar = *blank  and d1nrp = 0
157800961203     C                   eval      d1serpar = d1serdoc
157900951005     C                   eval      d1nrp = d1ndc
158000951005 e02 C                   endif
158100951005      *
158200950928      *
158300951003 b02 C                   if        d1nrp = 0
158400950928     C                   seton                                        9973
158500951003 e02 C                   endif
158600950927      *
158700951003 b02 C                   if        d1drp =  0
158800950928     C                   eval      d1drp = d1ddc
158900951003 e02 C                   endif
159000951003 b02 C                   if        d1drp <> 0
159100950927     C                   CALLB     'XDT4000'
159200951115     C     D1DrpAMG      PARM                    XDTAMG           10
159300950927     C                   PARM      d1drp         XDTGMA            6 0
159400950929     C                   PARM      3             XDTSTA            1 0
159500951003 b03 C                   if        xdtsta <> 0
159600951003     C                   seton                                        9974
159700951003 e03 C                   endif
159800950928     C                   else
159900951003     C                   seton                                        9975
160000951003 e02 C                   endif
160100981019     c* DIvisa
160200981020     c                   if        d1div = *blanks
160300981109     c                   seton                                        3999
160400981019     c                   endif
160500981019     C* reperisco decimali solo se diversa da mc
160600981019     c* e se data documento     ok
160700981016     c                   if        d1div    <> xscdiv and
160800981019     c                             d1div <> *blanks   and *in66 = *off
160900981019     c                             and *in67 = *off
161000981019     c* uso data documento
161100981016     C                   CALLB     'XDT4000'
161200981016     C                   PARM                    XDTAMG           10
161300981019     C                   PARM      d1ddc         XDTGMA            6 0
161400981019     C                   PARM      4             XDTSTA            1 0
161500981016     C                   IF        X0201RpDVS(D1Div     :xdtamg   :
161600981016     C                                        ANDVS00F: %size(ANDVS00F):
161700981016     C                                        ANDVX00F: %size(ANDVX00F)) = 0
161800981016     c                   z-add     dvxnrdec      Wnrdec
161900981019     c                   else
162000981019     c                   seton                                        5099
162100981016     C                   ENDIF
162200981016     c                   else
162300981016     c                   z-add     xscNrDecMc    Wnrdec
162400981016     C                   ENDIF
162500981016      *
162600981016      * congruenza stato                                   *
162700981016 b03 C     D1STA         IFNE      '0'                                          *
162800981016     C     opzr105       ANDEQ     '04'                                         *
162900981016     C                   SETON                                        8799      *
163000981016 e03 C                   ENDIF                                                  *
163100981016      *-                                                   *
163200981016 b03  *-   D1STA         IFEQ      '0'                     *                    *
163300981016      *-   opzr105       ANDEQ     '01'                    *                    *
163400981016      *-                 SETON                             *            99      *
163500981016 e03  *-                 ENDIF                             *                    *
163600951003      *-                                                   *
163700991112D0300C***  TRICNTIMPM    IFNE      'S'
163800001215D0791C**** ex D3000     TRICNTIMPM    IFNE      '1'
163900981020      * controllo importi/cambio
164000981020     c                   exsr      ctrImporti
164100981020      * Calcolo delle ritenute- DIVISA                     *
164200010212D0845X****               EXSR      CalcRitDiv
164300010212D0845C*
164400010212D0845C                   EXSR      CalcRitDiv2
164500981019      * Calcolo delle ritenute- mc     se divisa <> da m.c.*
164600981019 b03 c                   if        d1div <> xscdiv
164700001221C1447X****               EXSR      CalcRitMC
164800001221C1447C*
164900001221C1447C                   EXSR      CalcRitMC2
165000981019     c                   else
165100981019     c                   exsr      CampiMc
165200981019 e03 c                   endif
165300001215D0791c****               endif
165400070122C2002C*
165500070122  "  C                   if        *In99      = *Off
165600070125  "  C*
165700070125  "  C                   EXSR      CtrVincAPE
165800070125  "  C*
165900070122  "  C                   if        D1Sta      = '0'   or
166000070122  "  C                             D1Sta      = '1'
166100070125  "  C                   EXSR      CtrVincMRA
166200070122  "  C                   endif
166300070125  "  C*
166400070122  "  C                   endif
166500070122C2002C*
166600981106      *                                                    *
166700981106      *Se non ci sono errori controllo
166800981106      *che non stia inserendo un record doppio per percipiente/partita
166900981106     C                   If        *in99 = *off
167000981106     C                   Exsr      CtlRecDup
167100981106     C                   If        RecDup = 1
167200981106     C                   Seton                                        5599
167300981106     C                   Endif
167400981106     C                   Endif
167500981130      *                                                    *
167600981130 e01 c                   endif
167700981019      *                                                    *
167800981016     c                   ENDSR
167900981016     C/EJECT
167901170222R235 C************************************************************
167902170222  "  C* Controllo percipiente a regime agevolato
167903170222  "  C************************************************************
167904170222R235 C     CtrRegAgev    BEGSR
167905170222     C*
167906170222     C* controllo se si tratta di percipiente in regime agevolato
167907170222     C                   Eval      OkRegAgev  = *Off
167908170222     C*
167909170222     C                   CALLB     'XDT4000'
167910170222     C     D1DDcAMG      PARM                    XDTAMG           10
167911170222     C                   PARM      D1DDc         XDTGMA            6 0
167912170222     C                   PARM      3             XDTSTA            1 0
167913170222     C*
167914170222     C                   Eval      SPASocieta = XScSoc
167915170222     C                   Eval      SPAKcc     = D1Kcc
167916170222     C                   Eval      SPAKsc     = D1Ksc
167917170222     C     K03SPA01      SETLL     SDGSPA01L
167918170222     C     K03SPA01      READE     SDGSPA01L                              21
167919170222     C                   Eval      $FineSPA   = *In21
167920170222     C                   DoW       $FineSPA   = *Off
167921170222     C*
167922170222     C* data documento compresa nel periodo del regime agevolato
167923170222     C                   if        NOT(
167924170222     C                             D1DDcAMG <> *Loval AND (
167925170222     C                              (SPADtFin < D1DDcAMG And
167926170222     C                               SPADtFin <> *Hival)
167927170222     C                                OR
167928170222     C                              (SPADtIni > D1DDcAMG And
167929170222     C                               SPADtIni <> *Loval)))
167930170222     C*
167931170222     C                   eval      OkRegAgev  = *On
167932170222     C                   Eval      $FineSPA   = *On
167933170222     C*
167934170222     C                   endif
167935170222     C*
167936170222     C                   if        $FineSPA   = *Off
167937170222     C     K03SPA01      READE     SDGSPA01L                              21
167938170222     C                   Eval      $FineSPA   = *In21
167939170222     C                   endif
167940170222     C*
167941170222     C                   endDo
167942170222     C*
167943170222     C                   if        OkRegAgev   = *On
167944170222     C* emetto wdw per avvertire che il percipiente è a regime agevolato
167945170222     C* e quindi non deve essere inserita la ritenuta
167946170222     C                   EXSR      MsgAvvert2
167947170222     C                   endif
167948170222     C*
167949170222R235 C                   ENDSR
167950170222R235 C/EJECT
168000001221C1447X************************************************************
168100001221  "  X***** Calcolo ritenuta in moneta di conto                      *
168200001221  "  X************************************************************
168300001221  "  X**** CalcRitMc     BEGSR
168400001221  "  X*****
168500001221  "  X****               Z-ADD     0             WRAC                           *
168600001221  "  X****               Z-ADD     0             WIMS                           *
168700001221  "  X*****
168800001221  "  X***** controlla i decimali dei campi immessi a video prima
168900001221  "  X***** di eseguire i calcoli
169000001221  "  X****               exsr      CtrDecMc
169100001221  "  X*****
169200001221  "  X***** calcola l'imponibile soggetto dall'imponibile assoggettato
169300001221  "  X***** mediante la percentuale se presente
169400001221  "  X**** D1IMPASS      IFNE      0                                            *  - 1 -
169500001221  "  X**** d1pai         ANDEQ     0                                            *
169600001221  "  X****               Z-ADD     D1IMPASS      WIMS                           *
169700001221  "  X****               ELSE                                                   *  E 1 E
169800001221  "  X**** D1IMPASS      IFNE      0                                            *  - 2 -
169900001221  "  X**** D1IMPASS      MULT      d1pai         WIMS                           *
170000001221  "  X**** WIMS          DIV       100           WIMS                           *
170100001221  "  X***** eseguo arrotondamento sul valore finale
170200001221  "  X****               Z-ADD     Wims          W3015
170300001221  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
170400001221  "  X****                                    XscNrDecMC)
170500001221  "  X****               Z-ADD     w3015         Wims
170600001221  "  X****               ENDIF                                                  *  - 2 -
170700001221  "  X****               END                                                    *  - 1 -
170800001221  "  X*****-
170900001221  "  X**** D1IMS         IFEQ      0                                            *
171000001221  "  X**** ex D0256     D1PAI         IfNE      0
171100001221  "  X****               Z-ADD     WIMS          D1IMS                          *
171200001221  "  X**** ex D0256                   ENDIF                                     *
171300001221  "  X****               ELSE                                                   *
171400001221  "  X**** WIMS          COMP      D1IMS                              1616      *
171500001221  "  X****               ENDIF                                                  *
171600001221  "  X*****
171700001221  "  X*****  calcola l'imponibile non soggetto togliendo dall'imponibile assogg.
171800001221  "  X***** l'imponibile soggetto
171900001221  "  X**** D1IMN         IFEQ      0                                            *
172000001221  "  X**** D1IMPASS      SUB       D1IMS         D1IMN                          *
172100001221  "  X**** ex D0256     D1PAI         IfEq      0
172200001221  "  X**** ex D0256     D1PRA         AndEq     0
172300001221  "  X**** ex D0256                   Z-Add     0             D1IMN
172400001221  "  X**** ex D0256                   endif
172500001221  "  X****               ENDIF                                                  *
172600001221  "  X*****
172700001221  "  X***** Controlli sull'imponibile assoggettato e le percentuali
172800001221  "  X**** D1IMPASS      IFEQ      0                                            *
172900001221  "  X**** D1PRA         IFNE      0                                            *
173000001221  "  X**** D1PAI         ORNE      0                                            *
173100001221  "  X****               SETON                                        8499      *
173200001221  "  X****               ENDIF                                                  *
173300001221  "  X****               ENDIF                                                  *
173400001221  "  X**** D1IMPASS      IFNE      0                                            *
173500001221  "  X**** D1PRA         ANDEQ     0                                            *
173600001221C1447X**** D1PAI         ANDEQ     0                                            *
173700991011D0256C****               SETON                                        8499      *
173800001221C1447X****               ENDIF                                                  *
173900001221  "  X*****                                                *
174000001221  "  X****  N84              DO                                                 *  - 1 -
174100001221  "  X*****                                                *
174200001221  "  X**** D1IMPASS      IFEQ      0                                            *  - 2 -
174300001221  "  X****               Z-ADD     0             D1IMS                          *
174400001221  "  X****               ENDIF                                                  *
174500001221  "  X*****
174600001221  "  X***** Calcola la ritenuta se non immessa a video
174700001221  "  X**** D1RAC         IFEQ      0                                            *  - 2 -
174800001221  "  X**** D1IMS         MULT      D1PRA         D1RAC                          *
174900001221  "  X**** D1RAC         DIV       100           D1RAC                          *
175000001221  "  X***** eseguo arrotondamento sul valore finale
175100001221  "  X****               Z-ADD     D1Rac         W3015
175200001221  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
175300001221  "  X****                                    XscNrDecMC)
175400001221  "  X****               Z-ADD     w3015         d1rac
175500001221  "  X****               ENDif                                                  *  - 2 -
175600001221  "  X*****
175700001221  "  X***** se immessa a video, calcola la ritenuta e la confronta con quella
175800001221  "  X***** immessa a video
175900001221  "  X**** D1IMS         MULT      D1PRA         WRAC                           *  - 1 -
176000001221  "  X**** WRAC          DIV       100           WRAC                           *
176100001221  "  X***** eseguo arrotondamento sul valore finale
176200001221  "  X****               Z-ADD     Wrac          W3015
176300001221  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
176400001221  "  X****                                    XscNrDecMC)
176500001221  "  X****               Z-ADD     w3015         Wrac
176600001221  "  X****               ENDDO                                                  *  - 1 -
176700001221  "  X*****
176800001221  "  X***** se diverse, errore                                 *
176900001221  "  X**** D1RAC         IFNE      WRAC                                         *
177000001221  "  X****               SETON                                        13        *
177100001221  "  X****               ENDIF                                                  *
177200001221  "  X*****
177300001221  "  X***** congruenza ritenuta e percentuali
177400001221  "  X**** D1RAC         IFEQ      0                                            *
177500001221  "  X**** D1PRA         IFNE      0                                            *
177600001221  "  X**** D1PAI         ORNE      0                                            *
177700001221  "  X****               SETON                                        8699      *
177800001221  "  X****               ENDIF                                                  *
177900001221  "  X****               ENDIF                                                  *
178000001221  "  X**** D1RAC         IFNE      0                                            *
178100001221  "  X**** D1PRA         ANDEQ     0                                            *
178200001221  "  X**** D1PAI         ANDEQ     0                                            *
178300001221  "  X****               SETON                                        8699      *
178400001221  "  X****               ENDIF                                                  *
178500001221  "  X***** conguenza importi                                  *
178600001221  "  X**** D1IMPASS      IFEQ      0                                            *
178700001221  "  X**** D1IMN         ANDEQ     0                                            *
178800001221  "  X**** D1SNS         ANDEQ     0                                            *
178900001221  "  X****               SETON                                        8299      *
179000001221  "  X****               ENDIF                                                  *
179100001221  "  X*****-                                                   *
179200001221  "  X*****- Controllo del totale a video  con il totale del   *
179300001221  "  X*****- documento passato automaticamente dalle funzioni  *
179400001221  "  X*****- di Prima Nota Generica o Prima Nota Fatture       *
179500001221  "  X*****- eseguo il controllo solo se stato ritenuta = 0    *
179600001221  "  X*****- e se ritenuta in divisa, perchè potrei avuto dover rivalutare
179700001221  "  X*****- gli importi in moneta di conto, e quindi non quadrerebbe più
179800001221  "  X*****
179900001221  "  X****               if        d1totdoc <> 0                                *
180000001221  "  X****               if        d1div = xscdiv or
180100001221  "  X****                         d1div<> xscdiv and
180200001221  "  X****                         d1sta = '0'
180300001221  "  X****               Z-ADD     D1IMS         WTOTA                          *IM.SOGG.
180400001221  "  X****               ADD       D1IMN         WTOTA                          *IM.NO SSOG.
180500001221  "  X****               ADD       D1SNS         WTOTA                          *SPESE NO SO
180600001221  "  X****               ADD       D1IVA         WTOTA                          *I.V.A.
180700001221  "  X**** D1TOTDOC      IFNE      WTOTA                                        *
180800001221  "  X****               Seton                                        8399      *
180900001221  "  X****               ENDIF                                                  *
181000001221  "  X****               ENDIF
181100001221  "  X****               ENDIF
181200001221  "  X*****
181300001221  "  X****               ENDSR
181400001221C1447X****/EJECT
181500001221C1447C************************************************************
181600001221  "  C* Calcolo ritenuta in moneta di conto 2
181700001221  "  C************************************************************
181800001221C1447C     CalcRitMC2    BEGSR
181900001221     C*
182000001221     C*
182100001221     C*--------------- A T T E N Z I O N E -----------------
182200001221     C* i controvalori in m.c. sono ottenuti dai valori in divisa
182300001221     C* già calcolati
182400001221     C*-----------------------------------------------------
182500001221     C*
182600001221     C                   Clear                   WImS
182700001221     C                   Clear                   WRAC
182800001221     C*
182900001221     C* controlla i decimali dei campi immessi a video prima
183000001221     C* di eseguire i calcoli
183100001221     C                   EXSR      CtrDecMC
183200010212D0845C*
183300010212  "  C*-------
183400010212  "  C* imposta IVA
183500010212  "  C                   if        D1IVAD     <> 0     and
183600010212  "  C                             D1IVA       = 0
183700010212  "  C                   Eval      WImporD     = D1IVAD
183800010801d0845C                   EXSR      RepImpMC
183900010801d0973x***ex d0845        Eval      D1IVA       = WImporM
184000010801d0973C                   Z-add     WImporM       D1IVA
184100010801d0845C                   endif
184200010212  "  C*
184300010212  "  C*-------
184400010212  "  C* SPESE NON SOGGETTE
184500010212  "  C                   if        D1SNSD     <> 0     and
184600010212  "  C                             D1SNS       = 0
184700010212  "  C                   Eval      WImporD     = D1SNSD
184800010801d0845C                   EXSR      RepImpMC
184900010801d0973x***ex d0845        Eval      D1SNS       = WImporM
185000010801d0973C                   z-add     WImporM       D1SNS
185100010801D0845C                   endif
185200010801     C*
185300010801     C*-------
185400010801     C* CONGRUENZA tra imponibile assoggettabile e percentuali
185500001221     C                   if        D1ImpAss    = 0        and
185600001221     C                             (D1PRA      <> 0   or
185700001221     C                              D1PAI      <> 0      )
185800001221     C                   SetOn                                        8499
185900001221     C                   endif
186000001221     C*
186100001221     C*
186200001221     C*-------
186300001221     C* imponibile SOGGETTO
186400001221     C                   if        D1ImSD     <> 0
186500001221     C                   Eval      WImporD     = D1ImSD
186600001221     C                   EXSR      RepImpMC
186700010801d0973x****               Eval      WImS        = WImporM
186800010801d0973C                   Z-add     WImporM       WImS
186900001221     C                   endif
187000001221     C*
187100001221     C* imponibile soggetto NON valorizzato
187200001221     C                   IF        D1ImS       = 0
187300001221     C*
187400001221     C* % su imponibile soggetto valorizzata
187500001221     C                   if        D1PAI      <> 0
187600001221     C                   Z-Add     WImS          D1ImS                          *
187700001221     C                   endif                                                  *
187800001221     C*
187900001221     C                   ELSE
188000001221     C*
188100001221     C                   if        D1ImpAss    = 0                              *  - 2 -
188200001221     C                   Eval      D1ImS       = 0                              *
188300001221     C                   endif                                                  *
188400001221     C                   ENDIF
188500001221     C*
188600001221     C* CONGRUENZA tra il valore a video e quello calcolato
188700041005     c                   if        D1ImS      <> WImS
188800001221     C                   SetOn                                        16
188900001221     C                   endif
189000001221     C*
189100001221     C*
189200001221     C*-------
189300001221     C* imponib.NON SOGGETTO = imponib. assoggettab. - imponib.soggetto
189400001221     C* solo se valorizzata % per imponib. o % per ritenuta d'acconto
189500110215D2668C                   if        D1ImN       = 0
189600110215D2668X**                 if        D1ImN       = 0          and
189700110215D2668X**                           (D1PAI      <> 0    or
189800110215D2668X**                            D1PRA      <> 0       )
189900010801d0973x***                Eval      D1ImN       = D1ImpAss - D1ImS
190000010801d0973C     D1ImpAss      Sub       D1ImS         D1ImN
190100001221     C                   endif
190200001221     C*
190300001221     C* CONGRUENZA importi
190400001221     C                   if        D1ImpAss     = 0  and
190500001221     C                             D1ImN        = 0  and
190600001221     C                             D1SNS        = 0
190700001221     C                   SetOn                                        8299      *
190800001221     C                   endif
190900001221     C*
191000001221     C*
191100001221     C*-------
191200001221     C* RITENUTA D'ACCONTO
191300001221     C                   if        D1RACD     <> 0
191400001221     C                   Eval      WImporD     = D1RACD
191500001221     C                   EXSR      RepImpMC
191600010801d0973x***                Eval      WRAC        = WImporM
191700010801d0973C                   Z-add     WImporM       WRAC
191800001221     C                   endif
191900001221     C*
192000001221     C                   if        D1RAC       = 0
192100001221     C                   Eval      D1RAC       = WRAC
192200001221     C                   else
192300001221     C                   if        D1RAC      <> WRAC
192400001221     C                   SetOn                                        13
192500001221     C                   endif
192600001221     C                   endif
192700001221     C*
192800001221     C* CONGRUENZA tra ritenuta e percentuali
192900001221     C                   if        D1RAC       = 0        and
193000001221     C                             (D1PRA      <> 0   or
193100001221     C                              D1PAI      <> 0      )
193200001221     C                   SetOn                                        8699
193300001221     C                   endif
193400001221     C*
193500001221     C                   if        D1RAC      <> 0     and
193600001221     C                             D1PRA       = 0     and
193700001221     C                             D1PAI       = 0
193800001221     C                   SetOn                                        8699
193900001221     C                   endif
194000001221     C*
194100001221     C*
194200001221     C*-------
194300001221     C* QUADRATURA tra il totale a video in m.c. e
194400001221     C*                quello del documento passato dalla PN
194500001221     C* SOLO SE
194600001221     C* stato ritenuta = '0'      E
194700001221     C* divisa  <>  m.c.
194800001221     C*
194900001221     C                   if        D1TotDoc   <> 0                  and
195000001221     C                             (D1Div       = XScDiv        or
195100001221     C                              (D1Div     <> XScDiv   and
195200001221     C                               D1Sta      = '0'          )  )
195300001221     C*
195400001221     C                   Eval      WTotA       = D1Ims   +
195500001221     C                                           D1ImN   +
195600001221     C                                           D1SNS   +
195700001221     C                                           D1IVA
195800001221     C*
195900001221     C                   if        D1TotDoc   <> WTotA
196000001221     C                   SetOn                                        8399      *
196100001221     C                   endif
196200001221     C*
196300001221     C                   endif
196400001221     C*
196500001221C1447C                   ENDSR
196600001221C1447C/EJECT
196700001221C1447C************************************************************
196800001221  "  C* Reperimento controvalore in m.c.
196900001221  "  C************************************************************
197000001221C1447C     RepImpMC      BEGSR
197100001221     C*
197200001221     C                   Eval      WImporM    = 0
197300001221     C*
197400001221     C                   Reset                   PJX00202DS
197500001221     C                   Movel     D1Div         Divisa0202
197600001221     C                   Movel     XScDiv        DivMC0202
197700001221     C                   Z-Add     WImporD       ImporD0202
197800001221     C                   Z-Add     0             ImporM0202
197900001221     C                   Z-Add     D1CMO         Cambio0202
198000010213D0845C                   if        XDtSta     <> 0
198100010213  "  C                   MoveL     ZDateIso      DtRif0202
198200010213D0845C                   else
198300001221     C                   MoveL     XDtAMG        DtRif0202
198400010213D0845C                   endif
198500001221     C                   Move      *Off          NoCtrCmb
198600001221     C* (La richiesta dei dati gestionali serve solo per far accettare
198700001221     C*  alla routine anche eventuali importi negativi)
198800010126D0836X****               Move      *Off          DatiGest
198900010126D0836C                   Move      *On           DatiGest
199000001221     C* valorizzo gli altri parametri non in DS
199100001221     C                   Movel     *On           GesMsg
199200001221     C                   Movel     *Blank        Esito
199300001221     C*
199400001221     C                   CALLP     X0202TestC(PJX00202DS :
199500001221     C                                        %size(PJX00202DS) :
199600001221     C                                        GesMsg : Esito :
199700001221     C                                        NoCtrCmb : DatiGest)
199800001221     C*
199900001221     C                   if        Esito      =  *On
200000001221     C                   EXSR      ErrTestC
200100001221     C                   else
200200001221     C                   Z-Add     ImporM0202    WImporM
200300001221     C                   endif
200400001221     C*
200500001221C1477C                   ENDSR
200600001221C1447C/EJECT
200700001221C1447C************************************************************
200800001221  "  C* Gestione errori emessi dalla testC
200900001221  "  C************************************************************
201000001221C1447C     ErrTestC      BEGSR
201100001221     C*
201200001221     C                   Clear                   TextMsg
201300001221     C*
201400001221     C* gestisco la loro emissione
201500001221     C                   DOU       Q2BytAva    = 0
201600001221     C                   EXSR      RestoreMsg
201700001221     C*
201800001221     C                   IF        Q2BytAva    > 0
201900001221     C*
202000001221     C                   Eval      IdMsg = 'PROMSG    *LIBL     xxxxxxx'
202100001221     C                   Move      Q2MsgId       IdMsg
202200001221     C*
202300001221     C                   CallB     'XRTVM'
202400001221     C                   Parm                    IdMsg            27
202500001221     C                   Parm                    TxtMsg          100
202600001221     C                   Parm                    ErrMsg            1
202700001221     C                   Parm                    *Omit
202800001221     C                   Parm                    *Omit
202900001221     C                   Parm      Q2MsgTxt      MsgDta          100
203000001221     C*
203100001221     C                   if        ErrMsg     <> *On
203200001221     C                   CAT       TxtMsg:1      TextMsg
203300001221     C                   SetOn                                        5099
203400001221     C                   endif
203500001221     C*
203600001221     C                   ENDIF
203700001221     C*
203800001221     C                   ENDDO
203900001221     C*
204000001221C1447C                   ENDSR
204100001221C1447C/EJECT
204200981019      ************************************************************
204300981019      * Controllo decimali campi mc
204400981019      ************************************************************
204500981019     C     CtrDecMc      BEGSR
204600981019     c* impon. assoggettato
204700981019     C                   if        D1Impass <> 0
204800981019     c                   z-add     D1Impass      W1903
204900981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc)= *on
205000981019     c                   seton                                        5399
205100981019     c                   endif
205200981019     c                   endif
205300981019     C* impon. non soggetto
205400981019     C                   if        D1Imn    <> 0
205500981019     c                   z-add     D1Imn         W1903
205600981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc) = *on
205700981019     c                   seton                                        5699
205800981019     c                   endif
205900981019     c                   endif
206000981019     C*
206100981019     C* spese  non soggette
206200981019     C                   if        D1sns    <> 0
206300981019     c                   z-add     D1Sns         W1903
206400981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc) = *on
206500981019     c                   seton                                        5699
206600981019     c                   endif
206700981019     c                   endif
206800981019     C*
206900981019     C* IVA
207000981019     C                   if        D1IVA    <> 0
207100981019     c                   z-add     D1IVA         W1903
207200981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc) = *on
207300981019     c                   seton                                        6499
207400981019     c                   endif
207500981019     c                   endif
207600981019     C* Impon. soggetto
207700981019     C                   if        D1IMS    <> 0
207800981019     c                   z-add     D1IMS         W1903
207900981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc) = *on
208000981019     c                   seton                                        7299
208100981019     c                   endif
208200981019     c                   endif
208300981019     C* ritenuta
208400981019     C                   if        D1rac    <> 0
208500981019     c                   z-add     D1rac         W1903
208600981019     C                   IF        X0201NrDec(W1903     :xscNrDecMc) = *on
208700981019     c                   seton                                        8199
208800981019     c                   endif
208900981019     c                   endif
209000981019     C*
209100981019     c                   endsr
209200981016     C/EJECT
209300070129C2002C************************************************************
209400070129  "  C* Controllo vincoli tributo anagrafico
209500070129  "  C************************************************************
209600070129C2002C     CtrVincAPE    BEGSR
209700070129     C*
209800070129     C                   Clear                   XCHKMRADS
209900070129     C*
210000070129     C                   Eval      XCMSocieta  = XScSoc
210100070129     C* controllo dati anagrafici del conto
210200070129     C                   Eval      XCMMODO     = '1'
210300070129     C                   Eval      XCMKCC      = D1Kcc
210400070129     C                   Eval      XCMKSC      = D1Ksc
210500070129     C                   Eval      XCMTributo  = *Blank
210600070129     C                   Eval      XCMVarTrib  = *Blank
210700070129     C                   MoveL     ZDateIso      XCMDATA
210800070129     C*
210900070129     C                   MoveL     XCHKMRADS     KPJBU
211000070129     C                   CALL      'XCHKMRA'
211100070129     C                   PARM                    KPJBA
211200070129     C*
211300070129     C                   MoveL     KPJBU         XCHKMRADS
211400070129     C*
211500070129     C                   if        XCMErrore   = *On
211600070129     C*
211700070129     C                   Select
211800070129     C*
211900070129     C*---------------
212000070129     C* tributo con vincolo 1 e persona fisica con almeno 35 anni
212100070129     C* oppure
212200070129     C* tributo con vincolo 1 e persona non fisica
212300070129     C* oppure
212400070129     C* data di nascita non impostata
212500070129     C                   When      XCMTpErr    = '014'  or
212600070129     C                             XCMTpErr    = '011'  or
212700070129     C                             XCMTpErr    = '013'
212800070129     C                   SetOn                                        5799
212900070129     C*
213000070129     C*---------------
213100070129     C* tributo con vincolo 2 e persona fisica con meno di 35 anni
213200070129     C* oppure
213300070129     C* tributo con vincolo 2 e persona non fisica
213400070129     C* oppure
213500070129     C* data di nascita non impostata
213600070129     C                   When      XCMTpErr    = '016'  or
213700070129     C                             XCMTpErr    = '015'  or
213800070129     C                             XCMTpErr    = '017'
213900070129     C                   SetOn                                        5899
214000070129     C*
214100070129     C                   other
214200070129     C* si sono verificati errori in fase di reperimento dati per
214300070129     C* controllo vincoli su tributo
214400070129     C                   SetOn                                        7699
214500070129     C                   endsl
214600070129     C*
214700070129     C                   endif
214800070129     C*
214900070129C2002C                   ENDSR
215000070129C2002C/EJECT
215100070122C2002C************************************************************
215200070122  "  C* Controllo vincoli e stato della ritenuta
215300070122  "  C************************************************************
215400070125C2002C     CtrVincMRA    BEGSR
215500070122     C*
215600070122     C                   Clear                   XCHKMRADS
215700070122     C*
215800070122     C                   Eval      XCMSocieta  = XScSoc
215900070129     C* controllo specifici dati passati
216000070122     C                   Eval      XCMMODO     = '3'
216100070122     C                   Eval      XCMKCC      = D1Kcc
216200070122     C                   Eval      XCMKSC      = D1Ksc
216300070122     C                   Eval      XCMTributo  = D1Tri
216400070122     C                   Eval      XCMVarTrib  = D1PTr
216500070122     C                   if        D1DPFIso    = DatMin
216600070122     C                   MoveL     ZDateIso      XCMDATA
216700070122     C                   else
216800070122     C                   Eval      XCMDATA     = D1DPFIso
216900070122     C                   endif
217000070122     C*
217100070122     C                   MoveL     XCHKMRADS     KPJBU
217200070122     C                   CALL      'XCHKMRA'
217300070122     C                   PARM                    KPJBA
217400070122     C*
217500070122     C                   MoveL     KPJBU         XCHKMRADS
217600070122     C*
217700070122     C                   if        XCMErrore   = *On
217800070122     C*
217900070122     C                   Select
218000070122     C*
218100070122     C*---------------
218200070122     C* tributo con vincolo 1 e persona fisica con almeno 35 anni
218300070122     C* oppure
218400070122     C* tributo con vincolo 1 e persona non fisica
218500070129     C* oppure
218600070129     C* data di nascita non impostata
218700070123     C                   When      XCMTpErr    = '014'  or
218800070129     C                             XCMTpErr    = '011'  or
218900070129     C                             XCMTpErr    = '013'
219000070122     C                   SetOn                                        8899
219100070122     C*
219200070122     C*---------------
219300070122     C* tributo con vincolo 2 e persona fisica con meno di 35 anni
219400070122     C* oppure
219500070122     C* tributo con vincolo 2 e persona non fisica
219600070129     C* oppure
219700070129     C* data di nascita non impostata
219800070123     C                   When      XCMTpErr    = '016'  or
219900070129     C                             XCMTpErr    = '015'  or
220000070129     C                             XCMTpErr    = '017'
220100070122     C                   SetOn                                        8999
220200070122     C*
220300070122     C                   other
220400070122     C* si sono verificati errori in fase di reperimento dati per
220500070122     C* controllo vincoli su tributo
220600070129     C                   SetOn                                        7799
220700070122     C                   endsl
220800070122     C*
220900070122     C                   endif
221000070122     C*
221100070122C2002C                   ENDSR
221200070122C2002C/EJECT
221300010212D0845X************************************************************
221400010212  "  X***** Calcolo ritenuta in divisa
221500010212  "  X************************************************************
221600010212  "  X**** CalcRitDiv    BEGSR
221700010212  "  X*****
221800010212  "  X****               Z-ADD     0             WRACD                          *
221900010212  "  X****               Z-ADD     0             WIMSD                          *
222000010212  "  X*****
222100010212  "  X***** controlla i decimali dei campi immessi a video prima
222200010212  "  X***** di eseguire i calcoli
222300010212  "  X****               exsr      CtrDecDiv
222400010212  "  X***** calcola l'imponibile soggetto dall'imponibile assoggettato
222500010212  "  X***** mediante la percentuale se presente
222600010212  "  X**** D1IMPASSD     IFNE      0                                            *  - 1 -
222700010212  "  X**** d1pai         ANDEQ     0                                            *
222800010212  "  X****               Z-ADD     D1IMPASSD     WIMSD                          *
222900010212  "  X****               ELSE                                                   *  E 1 E
223000010212  "  X**** D1IMPASSD     IFNE      0                                            *  - 2 -
223100010212  "  X**** D1IMPASSD     MULT      d1pai         WIMSD                          *
223200010212  "  X**** WIMSD         DIV       100           WIMSD                          *
223300010212  "  X***** eseguo arrotondamento sul valore finale
223400010212  "  X****               Z-ADD     WImsD         W3015
223500010212  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
223600010212  "  X****                                      WNrDec  )
223700010212  "  X****               Z-ADD     w3015         WImsD
223800010212  "  X****               ENDIF                                                  *  - 2 -
223900010212  "  X****               END                                                    *  - 1 -
224000010212  "  X*****-
224100010212  "  X**** D1IMSD        IFEQ      0                                            *
224200010212  "  X****               Z-ADD     WIMSD         D1IMSD                         *
224300010212  "  X**** ex D0256     D1PAI         IfEq      0
224400010212  "  X**** ex D0256     D1PRA         AndEq     0
224500010212  "  X**** ex D0256                   z-add     0             D1IMSD
224600010212  "  X**** ex D0256                   EndIf
224700010212  "  X****               ELSE                                                   *
224800010212  "  X**** WIMSD         COMP      D1IMSD                             1717      *
224900010212  "  X****               ENDIF                                                  *
225000010212  "  X*****
225100010212  "  X*****  calcola l'imponibile non soggetto togliendo dall'imponibile assogg.
225200010212  "  X***** l'imponibile soggetto
225300010212  "  X**** D1IMND        IFEQ      0                                            *
225400010212  "  X**** D1IMPASSD     SUB       D1IMSD        D1IMND                         *
225500010212  "  X**** ex D0256     D1PAI         IfEq      0
225600010212  "  X**** ex D0256     D1PRA         AndEq     0
225700010212  "  X**** ex D0256                   z-add     0             D1IMND            *
225800010212  "  X**** ex D0256                   EndIf
225900010212  "  X****               ENDIF                                                  *
226000010212  "  X*****
226100010212  "  X***** Controlli sull'imponibile assoggettato e le percentuali
226200010212  "  X**** D1IMPASSD     IFEQ      0                                            *
226300010212  "  X**** D1PRA         IFNE      0                                            *
226400010212  "  X**** D1PAI         ORNE      0                                            *
226500010212  "  X****               SETON                                        5299      *
226600010212  "  X****               ENDIF                                                  *
226700010212  "  X****               ENDIF                                                  *
226800010212  "  X**** D1IMPASSD     IFNE      0                                            *
226900010212  "  X**** D1PRA         ANDEQ     0                                            *
227000010212D0845X**** D1PAI         ANDEQ     0                                            *
227100010212D0256C******             SETON                                        5299      *
227200010212D0845X****               ENDIF                                                  *
227300010212  "  X*****-                                               *
227400010212  "  X****  N52              DO                                                 *  - 1 -
227500010212  "  X******-                                                   *
227600010212  "  X**** D1IMPASSD     IFEQ      0                                            *  - 2 -
227700010212  "  X****               Z-ADD     0             D1IMSD                         *
227800010212  "  X****               ENDIF                                                  *
227900010212  "  X*****
228000010212  "  X***** Calcola la ritenuta se non immessa a video
228100010212  "  X**** D1RACD        IFEQ      0                                            *  - 2 -
228200010212  "  X**** D1IMSD        MULT      D1PRA         D1RACD                         *
228300010212  "  X**** D1RACD        DIV       100           D1RACD                         *
228400010212  "  X***** eseguo arrotondamento sul valore finale
228500010212  "  X****               Z-ADD     D1RacD        W3015
228600010212  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
228700010212  "  X****                                      WNrDec  )
228800010212  "  X****               Z-ADD     w3015         d1racD
228900010212  "  X****               ENDif                                                  *  - 2 -
229000010212  "  X*****
229100010212  "  X***** se immessa a video, calcola la ritenuta e la confronta con quella
229200010212  "  X***** immessa a video
229300010212  "  X**** D1IMSD        MULT      D1PRA         WRACD                          *  - 1 -
229400010212  "  X**** WRACD         DIV       100           WRACD                          *
229500010212  "  X***** eseguo arrotondamento sul valore finale
229600010212  "  X****               Z-ADD     WRACD         W3015
229700010212  "  X****               CALLP     X0201Arrot(W3015 : 'H' :
229800010212  "  X****                                    Wnrdec    )
229900010212  "  X****               Z-ADD     w3015         WRaCD
230000010212  "  X****               ENDDO                                                  *  - 1 -
230100010212  "  X*****
230200010212  "  X***** se diverse, errore                                 *
230300010212  "  X**** D1RACD        IFNE      WRACD                                        *
230400010212  "  X****               SETON                                        15        *
230500010212  "  X****               ENDIF                                                  *
230600010212  "  X*****
230700010212  "  X***** congruenza ritenuta e percentuali
230800010212  "  X**** D1RACD        IFEQ      0                                            *
230900010212  "  X**** D1PRA         IFNE      0                                            *
231000010212  "  X**** D1PAI         ORNE      0                                            *
231100010212  "  X****               SETON                                        7899      *
231200010212  "  X****               ENDIF                                                  *
231300010212  "  X****               ENDIF                                                  *
231400010212  "  X**** D1RACD        IFNE      0                                            *
231500010212  "  X**** D1PRA         ANDEQ     0                                            *
231600010212  "  X**** D1PAI         ANDEQ     0                                            *
231700010212  "  X****               SETON                                        7899      *
231800010212  "  X****               ENDIF                                                  *
231900010212  "  X*****
232000010212  "  X***** congruenza importi
232100010212  "  X**** D1IMPASSD     IFEQ      0                                            *
232200010212  "  X**** D1IMND        ANDEQ     0                                            *
232300010212  "  X**** D1SNSD        ANDEQ     0                                            *
232400010212  "  X****               SETON                                        5199      *
232500010212  "  X****               ENDIF                                                  *
232600010212  "  X*****-                                                   *
232700010212  "  X*****- Controllo del totale digitato con il totale del   *
232800010212  "  X*****- documento passato automaticamente dalle funzioni  *
232900010212  "  X*****- di Prima Nota Generica o Prima Nota Fatture       *
233000010212  "  X*****-                                                   *
233100010212  "  X****               if        d1totdocD<> 0                                *
233200010212  "  X****               if        d1div = xscdiv or
233300010212  "  X****                         d1div<> xscdiv and
233400010212  "  X****                         d1sta = '0'
233500010212  "  X****               Z-ADD     D1IMSD        WTOTAD                         *IM.SOGG.
233600010212  "  X****               ADD       D1IMND        WTOTAD                         *IM.NO SSOG.
233700010212  "  X****               ADD       D1SNSD        WTOTAD                         *SPESE NO SO
233800010212  "  X****               ADD       D1IVAD        WTOTAD                         *I.V.A.
233900010212  "  X**** D1TOTDOCD     IFNE      WTOTAD                                       *
234000010212  "  X****               SETON                                        5499      *
234100010212  "  X****               ENDIF                                                  *
234200010212  "  X****               ENDIF                                                  *
234300010212  "  X****               ENDIF                                                  *
234400010212  "  X*****
234500010212  "  X*****              ENDSR
234600010212D0845X****/EJECT
234700010212D0845C************************************************************
234800010212  "  C* Calcolo ritenuta in divisa
234900010212  "  C************************************************************
235000010212D0845C     CalcRitDiv2   BEGSR
235100010212     C*
235200010212     C* controlla i decimali dei campi immessi a video prima
235300010212     C* di eseguire i calcoli
235400010212     C                   EXSR      CtrDecDiv
235500010212     C*
235600010212     C*-----------
235700010212     C* CONGRUENZA TRA IMPONIBILE ASSOGGETTABILE E PERCENTUALI
235800010212     C*
235900010212     C                   if        D1ImpAssD  =  0
236000010212     C                   Z-Add     0             D1ImSD
236100010212     C*
236200010212     C                   if        D1PRA     <>  0         or
236300010212     C                             D1PAI     <>  0
236400010212     C                   SetOn                                        5299
236500010212     C                   endif
236600010212     C*
236700010212     C                   endif
236800010212     C*
236900010212     C*-----------
237000010212     C* IMPONIBILE SOGGETTO
237100010212     C                   Eval      *In17      =  *Off
237200010212     C*
237300010212     C                   Z-Add     0             WImSD                          *
237400010212     C                   if        D1ImpAssD  <> 0
237500010212     C*
237600010212     C* (valore calcolato)
237700010212     C                   if        D1PAI      =  0
237800010212     C                   Eval      WImSD      =  D1ImpAssD
237900010830D0975C*
238000010830  "  C                   if        D1PAI      =  0     and
238100010830  "  C                             D1PRA      =  0
238200010830  "  C                   Z-Add     0             WImSD
238300010830D0975C                   endif
238400010212     C*
238500010212     C                   else
238600010212     C*
238700010212     C                   Z-Add     D1ImpAssD     WImpInp
238800010212     C                   CallP     X0201MultC(WImpInp: D1PAI: 100:
238900010212     C                                        'H': '0': WNrDec: WImpOut)
239000010212     C                   Z-Add     WImpOut       WImSD
239100010212     C*
239200010212     C                   endif
239300010212     C*-
239400010212     C* (valore a video)
239500010212     C                   if        D1ImSD     =  0
239600010212     C                   Eval      D1ImSD     =  WImSD
239700010212     C                   endif
239800010212     C*
239900010212     C*-
240000010212     C* confronto l'imponibile soggetto calcolato con quello a video
240100050823D1858c***ex D1669        if        TriAssImp <>100
240200050823D1858c                   if        D1PAI     <>100
240300010212     C                   if        WImSD     <>  D1ImSD
240400010212     C                   Eval      *In17      =  *On
240500041027     C                   endif
240600041027D1669C                   else
240700041027  "  C                   if        WImSD     <>  D1ImSD
240800041027  "  C                                           + D1ImnD
240900041027  "  C                   Eval      *In17      =  *On
241000041027  "  C                   endif
241100041027D1669C                   endif
241200010212     C*
241300010212     C                   endif
241400010212     C*
241500010212     C*
241600010212     C*-----------
241700010212     C* IMPONIBILE NON SOGGETTO
241800010212     C                   if        D1ImND     =  0                              *
241900010212     C                   Eval      D1ImND     =  D1ImpAssD - D1ImSD
242000110215D2668x*
242100110215  "  x*                  if        D1PAI      =  0     and
242200110215  "  x*                            D1PRA      =  0
242300110215  "  x*  ex D1073        Z-Add     0             D1IMND                         *
242400110215  "  x*
242500110215  "  x*                  EndIf
242600110215D2668C*
242700010212     C                   endif
242800010212     C*
242900010212     C*-----------
243000010212     C* RITENUTA D'ACCONTO
243100010212     C                   Eval      *In15      =  *Off
243200010212     C*
243300010212     C* (valore calcolato)
243400010212     C                   Z-Add     0             WRACD                          *
243500010212     C                   Z-Add     D1ImSD        WImpInp
243600010212     C                   CallP     X0201MultC(WImpInp: D1PRA: 100:
243700010212     C                                        'H': '0': WNrDec: WImpOut)
243800010212     C                   Z-Add     WImpOut       WRACD
243900010212     C*
244000010212     C* (valore a video)
244100010212     C                   if        D1RACD     =  0
244200010212     C                   Eval      D1RACD     =  WRACD
244300010212     C                   endif
244400010212     C*
244500010212     C*-
244600010212     C* confronto la ritenuta d'acconto calcolata con quella a video
244700010212     C                   if        WRACD     <>  D1RACD
244800010212     C                   Eval      *In15      =  *On
244900010212     C                   endif
245000010212     C*
245100010212     C*-----------
245200010212     C* CONGRUENZA TRA RITENUTA D'ACCONTO E PERCENTUALI
245300010212     C*
245400010212     C                   if        D1RACD     =  0       and
245500010212     C                             (D1PRA     <>  0   or
245600010212     C                              D1PAI     <>  0     )
245700010212     C                   SetOn                                        7899
245800010212     C                   endif
245900010212     C*
246000010212     C                   if        D1RACD     <> 0    and
246100010212     C                             D1PRA      =  0    and
246200010212     C                             D1PAI      =  0
246300010212     C                   SetOn                                        7899
246400010212     C                   endif
246500010212     C*
246600010212     C*-----------
246700010212     C* CONGRUENZA TRA IMPORTI
246800010212     C                   if        D1ImpAssD  =  0    and
246900010212     C                             D1ImND     =  0    and
247000010212     C                             D1SNSD     =  0
247100010212     C                   SetOn                                        5199
247200010212     C                   endif
247300010510D0927C
247400010510  "  C* imponib. assogg.le= imponib. NON sogg. + imponib. sogg.
247500010511  "  C                   Eval      *In18      =  *Off
247600010510  "  C                   if        D1ImpAssD <>
247700010510  "  C                             D1ImND  + D1IMSD
247800010511  "  C                   Eval      *In18      =  *On
247900010510D0927C                   endif
248000010212     C*
248100010212     C*-----------
248200010212     C* CONFRONTO IL TOTALE DEI VALORI A VIDEO CON QUELLO PASSATO
248300010212     C* automaticamente dalle funzioni di PrimaNota (generica e di IVA)
248400010226     C*
248500010226     C* SOLO SE importo valorizzato E
248600010226     C* stato ritenuta = '0'      E
248700010226     C* divisa  <>  m.c.
248800010226     C*
248900010226     C                   if        D1TotDocD  <> 0                  and
249000010226     C                             (D1Div       = XScDiv        or
249100010226     C                              (D1Div     <> XScDiv   and
249200010226     C                               D1Sta      = '0'          )  )
249300010212     C*
249400010212     C                   if        (D1ImSD  +  D1ImND  +
249500010212     C                              D1SNSD  +  D1IVAD    )
249600010212     C                             <>  D1TotDocD
249700010212     C                   SetOn                                        5499
249800010226     C                   endif
249900010226     C*
250000010212     C                   endif
250100010212     C*
250200010212D0845C                   ENDSR
250300010212D0845C/EJECT
250400981019      ************************************************************
250500981019      * Controllo decimali campi divisa
250600981019      ************************************************************
250700981019     C     CtrDecDiv     BEGSR
250800981019     c* impon. assoggettato
250900981019     C                   if        D1ImpassD<> 0
251000981019     c                   z-add     D1ImpassD     W1903
251100981019     C                   IF        X0201NrDec(W1903     :WnrDec    )= *on
251200981019     c                   seton                                        4199
251300981019     c                   endif
251400981019     c                   endif
251500981019     C* impon. non soggetto
251600981019     C                   if        D1ImnD   <> 0
251700981019     c                   z-add     D1ImnD        W1903
251800981019     C                   IF        X0201NrDec(W1903     :WnrDec    ) = *on
251900981019     c                   seton                                        4299
252000981019     c                   endif
252100981019     c                   endif
252200981019     C*
252300981019     C* spese  non soggette
252400981019     C                   if        D1snsD   <> 0
252500981019     c                   z-add     D1SnsD        W1903
252600981019     C                   IF        X0201NrDec(W1903     :wnrDec    ) = *on
252700981019     c                   seton                                        4399
252800981019     c                   endif
252900981019     c                   endif
253000981019     C*
253100981019     C* IVA
253200981019     C                   if        D1IVAD   <> 0
253300981019     c                   z-add     D1IVAD        W1903
253400981019     C                   IF        X0201NrDec(W1903     :WnrDec    ) = *on
253500981019     c                   seton                                        4499
253600981019     c                   endif
253700981019     c                   endif
253800981019     C* Impon. soggetto
253900981019     C                   if        D1IMSD   <> 0
254000981019     c                   z-add     D1IMSD        W1903
254100981019     C                   IF        X0201NrDec(W1903     :WnrDec    ) = *on
254200981019     c                   seton                                        4599
254300981019     c                   endif
254400981019     c                   endif
254500981019     C* ritenuta
254600981019     C                   if        D1racD   <> 0
254700981019     c                   z-add     D1racD        W1903
254800981019     C                   IF        X0201NrDec(W1903     :WnrDec    ) = *on
254900981019     c                   seton                                        4699
255000981019     c                   endif
255100981019     c                   endif
255200981019     C*
255300981019     c                   endsr
255400940224     C/EJECT
255500981019      ************************************************************
255600981019      * Imposta campi divisa da quelli della mc se divise uguali *
255700981019      ************************************************************
255800981019     C     CampiMc       BEGSR
255900981019     C*
256000010801d0973x***                eval      D1impAss  = D1ImpAssD
256100010801  "  x***                eval      D1Imn     = D1ImnD
256200010801  "  x***                eval      D1sns     = D1SnsD
256300010801  "  x***                eval      D1Iva     = D1IvaD
256400010801  "  x***                eval      D1Ims     = D1ImsD
256500010801  "  x***                eval      D1Rac     = D1RacD
256600010801  "  C                   z-add     D1impAssD     D1ImpAss
256700010801  "  C                   z-add     D1ImnD        D1Imn
256800010801  "  C                   z-add     D1snsD        D1Sns
256900010801  "  C                   z-add     D1IvaD        D1Iva
257000010801  "  C                   z-add     D1ImsD        D1Ims
257100010801d0973C                   z-add     D1RacD        D1Rac
257200010801     C                   eval      d1cmo     = 1
257300981019     C*
257400981019     c                   endsr
257500981019     C/EJECT
257600981019     C************************************************************
257700981019     C* Ricavo imponibile e cambio dall'imponibile assoggettato in divisa
257800981019     C************************************************************
257900981019     C     CtrImporti    BEGSR
258000981019     c* uso data documento
258100981019     C                   CALLB     'XDT4000'
258200981019     C                   PARM                    XDTAMG           10
258300981019     C                   PARM      d1ddc         XDTGMA            6 0
258400000222D0461C***                PARM      4             XDTSTA            1 0
258500000222D0461C                   PARM      3             XDTSTA            1 0
258600981019     C*
258700981019     c                   if        xdtsta = 0
258800981019     C* controllo congruenza importi/cambio
258900981019     C                   RESET                   PJX00202DS
259000981019     C                   MOVEL     D1div         Divisa0202
259100981019     C                   MOVEL     XscDiv        DivMC0202
259200051108D1892X****               move      *off          $neg1             1
259300051108  "  X****               move      *off          $neg2             1
259400051108  "  X***** passo valori assoluti
259500051108  "  X****               if        d1impassD <0
259600051108  "  X****               move      *on           $neg1
259700051108  "  X****               Z-sub     d1impassD     ImporD0202
259800051108D1892X****               else
259900981105     C                   Z-add     d1impassD     ImporD0202
260000051108D1892X****               endif
260100051108  "  X****               if        d1impass <0
260200051108  "  X****               move      *on           $neg2
260300051108  "  X****               Z-sub     d1impass      ImporM0202
260400051108D1892X****               else
260500051108     C                   Z-ADD     d1impass      ImporM0202
260600051108D1892X****               endif
260700981020     c* se lo stato è '0' uso cambio originale, se maggiore di 0
260800981020     c* uso campi pagamento
260900981020     c                   if        d1sta > '0'
261000981020     C                   Z-ADD     d1cmp         Cambio0202
261100981020     c                   else
261200981020     C                   Z-ADD     d1cmo         Cambio0202
261300981020     c                   endif
261400981019     C                   MOVEL     xdtamg        DtRif0202
261500051108D1892C*
261600051108  "  C                   Move      *Off          NoCtrCmb
261700051108  "  C*
261800051108  "  C* (La richiesta dei dati gestionali serve solo per far accettare
261900051108  "  C*  alla routine anche eventuali importi negativi)
262000051108D1892C                   Move      *On           DatiGest
262100981019     C*
262200990616     C                   MOVEL     *oN           GesMsg
262300981019     C*
262400051108D1892X****               CALLP     X0202TestC(PJX00202DS :
262500051108  "  X****                                    %size(PJX00202DS) :
262600051108  "  X****                                    GesMsg : Esito :
262700051108D1892X****                                    *omit  : *omit)
262800051108D1892C*
262900051108  "  C                   CALLP     X0202TestC(PJX00202DS :
263000051108  "  C                                        %size(PJX00202DS) :
263100051108  "  C                                        GesMsg : Esito :
263200051108D1892C                                        NoCtrCmb : DatiGest)
263300981105     C* se non ci sono errori
263400981019   B1C                   IF        Esito = *off
263500981020     c*
263600981020     c* se provengo da prima nota NON cambio i cambi
263700981020     c                   if        *in12 = *off
263800981020     c                   if        d1sta > '0'
263900981020     C                   EVAL      D1cmp     = Cambio0202
264000981020     c                   else
264100981020     C                   EVAL      D1cmo     = Cambio0202
264200981020     c                   endif
264300981020     c                   endif
264400981020     c*
264500051108D1892X****               if        $neg1     = *on
264600051108  "  X****               EVAL      D1ImpassD = ImporD0202 * -1
264700051108D1892X****               else
264800981105     C                   EVAL      D1ImpassD = ImporD0202
264900051108D1892X****               endif
265000051108D1892X****               if        $neg2     = *on
265100010801d0973x****               EVAL      D1Impass  = ImporM0202 * -1
265200051108D1892X*ex D0973    -1            mult      ImporM0202    D1Impass
265300051108D1892X****               else
265400010801d0973x***                EVAL      D1Impass  = ImporM0202
265500010801d0973C                   z-add     ImporM0202    D1Impass
265600051108D1892X****               endif
265700981019     c                   else
265800981105     c* ci sono errori
265900981105     C* gestisco la loro emissione
266000981105     c                   clear                   TextMsg
266100981105   B2C                   DOU       Q2BytAva = 0
266200981105     C                   EXSR      RestoreMsg
266300981105   B3C                   IF        Q2BytAva > 0
266400981105   B4C                   SELECT
266500981105     C                   WHEN      Q2MsgId = 'MVC0067'
266600981105     c                   if        d1sta > '0'
266700981105     c                   seton                                        4899
266800981105     c                   else
266900981105     c                   seton                                        4799
267000981105     c                   endif
267100981105     C                   WHEN      Q2MsgId = 'MVC0065'
267200981105     c                   if        d1sta > '0'
267300981105     c                   seton                                        4099
267400981105     c                   else
267500981105     c                   seton                                        4999
267600981105     c                   endif
267700981105   O4C                   OTHER
267800981105     C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
267900981105     C                   MOVE      Q2MsgId       IdMsg
268000981105     C                   CALLB     'XRTVM'
268100981105     C                   PARM                    IDMSG            27
268200981105     C                   PARM                    TXTMSG          100
268300981105     C                   PARM                    ERRMSG            1
268400981105     C                   PARM                    *omit
268500981105     C                   PARM                    *omit
268600981105     C                   PARM      Q2msgtxt      Msgdta          100
268700981105   B5C                   IF        ErrMsg <> *on
268800981105     C                   CAT       TxtMsg:1      TextMsg
268900981105     C                   SETON                                        5099
269000981105   E5C                   ENDIF
269100981105   E4C                   ENDSL
269200981105   E3C                   ENDIF
269300981105   E2C                   ENDDO
269400981019   E1C                   ENDIF
269500981019     C*
269600981019     C                   ENDIF
269700981019     C                   ENDSR
269800981105     C/EJECT
269900981105     C************************************************************
270000981105     C* Recupero i msg sono stati generati dal modulo chiamato
270100981105     C************************************************************
270200981105     C     RestoreMsg    BEGSR
270300981105     C*
270400981105     C                   CALL      'QMHRCVPM'
270500981105     C                   PARM                    QInfo
270600981105     C                   PARM      113           QLenMsg
270700981105     C                   PARM      'RCVM0100'    QFormat
270800981105     C                   PARM      '*'           QCallMsg
270900981105     C                   PARM      0             QCallStack
271000981105     C                   PARM      '*DIAG'       QMsgTyp
271100981105     C                   PARM                    QMsgKey
271200981105     C                   PARM      0             QWait
271300981105     C                   PARM      '*REMOVE'     QAction
271400981105     C                   PARM                    QErrCod
271500981105     C*
271600981105     C                   ENDSR
271700040225     C/EJECT
271800040225C1728 ************************************************************
271900040225      * controllo non bloccante di inesistenza partita
272000040225C1728 ************************************************************
272100040225     C     CtrPartita    BEGSR
272200040225     c*
272300040225     c                   setoff                                       19
272400040225     c*
272500040225     c                   eval      ppasocieta = xscsoc
272600040225     c                   eval      ppactb     = 'CG'
272700040225     c                   eval      ppakcc     = d1kcc
272800040225     c                   eval      ppaksc     = d1ksc
272900040225     C                   eval      ppanrpar   = d1nrp
273000040225     C                   CALLb     'XDT4000'
273100040225     C     ppadtpar      PARM                    XDTAMG           10
273200040225     C                   PARM      d1Drp         XDTGMA            6 0
273300040225     C                   PARM      4             XDTSTA            1 0
273400040225     C                   eval      ppaserpar  = d1serpar
273500040225     C     K07Ppa01      chain     NDPPA01L
273600040225     C                   if        not %found(ndppa01l)
273700040225     c* se immissione chiama xmsgr
273800040225     c                   if        OpzR105 = '01'
273900040225     c                   exsr      Callxmsgr
274000040225     c                   else
274100040225     c* altrimenti mette la partita in HI
274200040225     c                   seton                                        19
274300040225     c                   endif
274400040225     c                   endif
274500040225     c*
274600040225C1728c                   ENDSR
274700040225C1728 ************************************************************
274800040225      * Chiama xmsgr  per emettere segnalazione
274900040225C1728 ************************************************************
275000040225     C     Callxmsgr     BEGSR
275100040225     c*
275200040225     c                   reset                   xmsgds
275300040225     c                   eval      xmsgcd1 = 'COS2528'
275400040225     c                   eval      xmsgcd2 = 'COS2491'
275500040225     c                   eval      msgrtn  = 'S'
275600040225     c                   eval      msgtpr  = 'SN'
275700040225     C                   eval      msgrsp = 'S'
275800040225     C                   eval      msgemv = 'S'
275900040225     C                   eval      msgmsg = 0
276000040225     C                   eval      msgcnl = 'N'
276100040225     C                   eval      msgvid = 'N'
276200040225     C                   eval      msgope = 'N'
276300040225     C                   eval      msgstp = 'N'
276400040225     C                   eval      msglck = 'N'
276500040225     c                   Move      d1drp         ZDataPar          6 0
276600040225     c                   Move      ZdataPar      ZDataParX         6
276700040225     c                   Move      d1nrp         ZNrpX             9
276800040225     c                   eval      varmsg1= ZNRPX + zdataparx + d1serpar
276900040225     c                   MOVEL     WEXF          xmsginz
277000040225     C                   CALL      'XMSGR'
277100040225     C                   PARM                    XMSGDS
277200040225     C                   PARM                    XMStxt          135
277300040225     C                   PARM                    XMSGinz           1
277400040225     C                   PARM                    XMSGCd1           7
277500040225     C                   PARM                    XMSGCd2           7
277600040225     C                   PARM                    XMSGCd3           7
277700040225     C                   PARM                    XMSGSn1           7
277800040225     C                   PARM                    XMSGCdTxT        63
277900040225     C                   PARM                    MSGCdTit         14
278000040225     C                   PARM                    MSGClPrtf         1
278100040225     C                   PARM                    VarMsg1         100
278200040225     C                   PARM                    VarMsg2         100
278300040225     C                   PARM                    VarMsg3         100
278400040225     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
278500040225     C* di sola finestra
278600040225     C                   MOVEL     '0'           WEXF
278700040225     C*
278800040225C1728C                   ENDSR
278900981019     C/EJECT
279000061211C1988 ************************************************************
279100061211      * Chiama xmsgr  per emettere segnalazione di mancata registraz.
279200061211C1988 ************************************************************
279300061211     C     MsgAvvert     BEGSR
279400061211     c*
279500061211     c                   reset                   xmsgds
279600061211     c                   eval      xmsgcd1 = 'COS2653'
279700061212     c                   eval      xmsgcd2 = 'COS0172'
279800061211     c                   eval      msgrtn  = 'S'
279900061211     c                   eval      msgtpr  = 'SN'
280000061211     C                   eval      msgrsp = 'S'
280100061211     C                   eval      msgemv = 'S'
280200061211     C                   eval      msgmsg = 0
280300061211     C                   eval      msgcnl = 'N'
280400061211     C                   eval      msgvid = 'N'
280500061211     C                   eval      msgope = 'N'
280600061211     C                   eval      msgstp = 'N'
280700061211     C                   eval      msglck = 'N'
280800061211     c                   MOVEL     WEXF          xmsginz
280900061211     C                   CALL      'XMSGR'
281000061211     C                   PARM                    XMSGDS
281100061211     C                   PARM                    XMStxt          135
281200061211     C                   PARM                    XMSGinz           1
281300061211     C                   PARM                    XMSGCd1           7
281400061211     C                   PARM                    XMSGCd2           7
281500061211     C                   PARM                    XMSGCd3           7
281600061211     C                   PARM                    XMSGSn1           7
281700061211     C                   PARM                    XMSGCdTxT        63
281800061211     C                   PARM                    MSGCdTit         14
281900061211     C                   PARM                    MSGClPrtf         1
282000061211     C                   PARM                    VarMsg1         100
282100061211     C                   PARM                    VarMsg2         100
282200061211     C                   PARM                    VarMsg3         100
282300061211     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
282400061211     C* di sola finestra
282500061211     C                   MOVEL     '0'           WEXF
282600061211     C*
282700061211C1988C                   ENDSR
282701170222R235 C************************************************************
282702170222 "   C* Chiama xmsgr per emettere segnalazione di regime agevolato
282703170222R235 C************************************************************
282704170222     C     MsgAvvert2    BEGSR
282705170222     C*
282706170222     C* Alla data documento, il percipiente risulta in regime agevolato.
282707170222     C* In regime agevolato, la ritenuta d'acconto non deve essere applicata,
282708170222     C* nè gestita a importo 0.
282709170222     C*
282710170222     C                   Reset                   XMSGDS
282711170222     C*
282712170222     C                   Eval      IdMsg = 'SDGMSG    *LIBL     xxxxxxx'
282713170222     C*
282714170222     C                   Move      'SDC0101'     IdMsg
282715170222     C                   CallB     'XRTVM'
282716170222     C                   Parm                    IdMsg            27
282717170222     C                   Parm                    TxtMsg          100
282718170222     C                   Parm                    ErrMsg            1
282719170222     C*
282720170222     C                   if        ErrMsg     <> *On
282721170222     C                   MoveL     TxTMsg        MsgMs1
282722170222     C                   endif
282723170222     C*
282724170222     C                   Move      'SDC0102'     IdMsg
282725170222     C                   CallB     'XRTVM'
282726170222     C                   Parm                    IdMsg            27
282727170222     C                   Parm                    TxtMsg          100
282728170222     C                   Parm                    ErrMsg            1
282729170222     C*
282730170222     C                   if        ErrMsg     <> *On
282731170222     C                   MoveL     TxTMsg        MsgMs2
282732170222     C                   endif
282733170222     C*
282734170222     C                   Move      'SDC0103'     IdMsg
282735170222     C                   CallB     'XRTVM'
282736170222     C                   Parm                    IdMsg            27
282737170222     C                   Parm                    TxtMsg          100
282738170222     C                   Parm                    ErrMsg            1
282739170222     C*
282740170222     C                   if        ErrMsg     <> *On
282741170222     C                   MoveL     TxTMsg        MsgMs3
282742170222     C                   endif
282743170222     C*
282744170222     C                   Eval      MsgRtn      = 'N'
282745170222     C                   Eval      MsgTpR      = '  '
282746170222     C                   Eval      MsgRsp      = 'N'
282747170222     C                   Eval      MsgEmV      = 'S'
282748170222     C                   Eval      MsgMsg      = 0
282749170222     C                   Eval      MsgCnl      = 'N'
282750170222     C                   Eval      MsgVid      = 'N'
282751170222     C                   Eval      MsgOpe      = 'N'
282752170222     C                   Eval      MsgStp      = 'N'
282753170222     C                   Eval      MsgLck      = 'N'
282754170222     C*
282755170222     C                   MOVEL     WEXF          XMsgInz
282756170222     C*
282757170222     C                   CALL      'XMSGR  '
282758170222     C                   PARM                    XMSGDS
282759170222     C                   PARM                    XMStxt          135
282760170222     C                   PARM                    XMSGinz           1
282761170222     C                   PARM                    XMSGCd1           7
282762170222     C                   PARM                    XMSGCd2           7
282763170222     C                   PARM                    XMSGCd3           7
282764170222     C                   PARM                    XMSGSn1           7
282765170222     C                   PARM                    XMSGCdTxT        63
282766170222     C                   PARM                    MSGCdTit         14
282767170222     C                   PARM                    MSGClPrtf         1
282768170222     C                   PARM                    VarMsg1         100
282769170222     C                   PARM                    VarMsg2         100
282770170222     C                   PARM                    VarMsg3         100
282771170222     C*
282772170222     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
282773170222     C* di sola finestra
282774170222     C                   MOVEL     '0'           WEXF
282775170222     C*
282776170222R235 C                   ENDSR
282777170222R235 C/EJECT
282800950925      ************************************************************
282900951004      * AGGIORNAMENTO ANAGRAFICA                                 *
283000950925      ************************************************************
283100941214     C     AGGANA        BEGSR
283200040225     c*
283300940207     C                   SELECT
283400950926     C     OPZr105       WHENEQ    '01'
283500950926     C     OPZr105       OREQ      '03'
283600941214     C                   EXSR      RIEANA
283700950926     C                   WRITE     ndmra000                             22
283800950925      * se rcd non scrivibile attivo errore
283900940318     C     *IN22         IFEQ      *ON
284000941201     C                   SETON                                        9599
284100941027     C                   ENDIF
284200950926     C     OPZr105       WHENEQ    '02'
284300941214     C                   EXSR      RIEANA
284400950926     C                   UPDATE    ndmra000
284500950926     C     OPZr105       WHENEQ    '04'
284600950926     C                   DELETE    ndmra000
284700940207     C                   ENDSL
284800950925      *
284900950925      * richiama pgm User Exit
285000950925      *
285100950511     C     USRERR        IFEQ      *BLANK
285200950925      * imposta chiave univoca  per user exit
285300950314     C                   MOVE      XSCSOC        XUKSOC
285400950512     C                   MOVE      XSCDUN        XUKDUN
285500950328     C                   MOVEL     DSPGM         XUKPGM
285600950404     C                   MOVEL     XUKDS         KPJBU
285700950328     C     DSPGM         CAT       'U':0         USREXT           10
285800950310     C                   CALL      USREXT                               21
285900950310     C                   PARM                    KPJBA
286000950310     C     *IN21         IFEQ      *ON
286100950310     C                   MOVE      *ON           USRERR            1
286200950310     C                   ENDIF
286300950310     C                   ENDIF
286400940203     C                   ENDSR
286500940224     C/EJECT
286600950925      ************************************************************
286700951004      * RIEMPIMENTO FILE IN AGGIORNAMENTO                        *
286800950925      ************************************************************
286900941214     C     RIEANA        BEGSR
287000981222     C                   IF        OPZr105= '01' or opzr105 = '03'
287100981222     c                   clear                   ndmra000
287200981222     c                   endif
287300950925      *
287400950925      * imposto i campi del file da quelli video
287500950925      *
287600950926     C                   eval      mrakcc     =   d1kcc
287700950926     C                   eval      mraksc     =   d1ksc
287800981014     C                   eval      mrasta     =   d1sta
287900950926     C                   eval      mratri     =   d1tri
288000950926     C                   eval      mraptr     =   d1ptr
288100981014     C                   eval      mracod     =   d1cau
288200981014     c* registrazione
288300950926     C                   eval      mranrg     =   d1nrg
288400950926     C                   eval      mraserreg  =   d1serreg
288500981014     C                   CALLB     'XDT4000'
288600981014     C     mradrg        PARM                    XDTAMG           10
288700981014     C                   PARM      d1drg         XDTGMA            6 0
288800981014     C                   PARM      4             XDTSTA            1 0
288900981014     c* documento
289000950926     C                   eval      mrandc     =   d1ndc
289100950926     C                   eval      mraserdoc  =   d1serdoc
289200981014     C                   CALLB     'XDT4000'
289300981014     C     mraddc        PARM                    XDTAMG           10
289400981014     C                   PARM      d1ddc         XDTGMA            6 0
289500981014     C                   PARM      4             XDTSTA            1 0
289600981014     c* partita
289700950926     C                   eval      mranrp     =   d1nrp
289800950926     C                   eval      mraserpar  =   d1serpar
289900981014     C                   CALLB     'XDT4000'
290000981014     C     mradrp        PARM                    XDTAMG
290100981014     C                   PARM      d1drp         XDTGMA
290200981014     C                   PARM      4             XDTSTA
290300981014     c* importi
290400981014     C                   eval      mraDIV     =   d1div
290500981014     C                   eval      mracmo     =   d1cmo
290600981014     C                   eval      mracmp     =   d1cmp
290700981014     C                   eval      mraimn     =   d1imn
290800950926     C                   eval      mraims     =   d1ims
290900981014     C                   eval      mrasns     =   d1sns
291000981014     C                   eval      mraiva     =   d1iva
291100950926     C                   eval      mrarac     =   d1rac
291200981014     C                   eval      mraimnD    =   d1imnD
291300981014     C                   eval      mraimsD    =   d1imsD
291400981014     C                   eval      mrasnsD    =   d1snsD
291500981014     C                   eval      mraivaD    =   d1ivaD
291600981014     C                   eval      mraracD    =   d1racD
291700981014     C                   eval      mrapra     =   d1pra
291800950926     C                   eval      mrapai     =   d1pai
291900981014      * data pagamento percipiente
292000981014     C                   CALLB     'XDT4000'
292100981014     C     mradpf        PARM                    XDTAMG           10
292200981014     C                   PARM      d1dpf         XDTGMA            6 0
292300981014     C                   PARM      4             XDTSTA            1 0
292400981014     c* data limite versamento
292500981014     C                   CALLB     'XDT4000'
292600981014     C     mradlv        PARM                    XDTAMG           10
292700981014     C                   PARM      d1dlv         XDTGMA            6 0
292800981014     C                   PARM      4             XDTSTA            1 0
292900981014     c* anno competenza
293000981014     C                   eval      mraacf     =   d1acf
293100981014     c*
293200950926     C                   eval      mratpq     =   d1tpq
293300981014     c* distinta
293400981014     C                   eval      mrandt     =   d1ndt
293500981014     C                   CALLB     'XDT4000'
293600981014     C     mraddt        PARM                    XDTAMG           10
293700981014     C                   PARM      d1ddt         XDTGMA            6 0
293800981014     C                   PARM      4             XDTSTA
293900981014     c* quietanza
294000981014     C                   eval      mranqv     =   d1nqv
294100981014     C                   eval      mrasqv     =   d1sqv
294200981014     C                   CALLB     'XDT4000'
294300981014     C     mradqv        PARM                    XDTAMG           10
294400981014     C                   PARM      d1dqv         XDTGMA            6 0
294500981014     C                   PARM      4             XDTSTA            1 0
294600981014     c* certificazione
294700981014     C                   eval      mranrcerti =   d1nrcerti
294800981014     C                   CALLB     'XDT4000'
294900981014     C     mradtcerti    PARM                    XDTAMG
295000981021     C                   PARM      d1dtcerti     XDTGMA
295100981014     C                   PARM      4             XDTSTA
295200981014     c* tipo movimento ritenuta
295300981014     C                   eval      mratpm    =   d1tpm
295400981014     c* tributo plurimo
295500981014     C                   eval      mraplurim  =   d1plurim
295600981014     c* altri campi
295700951003     C                   eval      mrasocieta =   xscsoc
295800951003     C                   CALLB     'XDT4000'
295900951003     C     mradtulmo     PARM                    XDTAMG           10
296000000117C1309C*****              PARM      udate         XDTGMA            6 0
296100000117C1309C                   PARM      ZDate         XDTGMA            6 0
296200951004     C                   PARM      4             XDTSTA            1 0
296300951003     C                   eval      mrauteulmo = xsccut
296400981014      *Nr. assoluto reg. fattura
296500951010     C                   eval      mranasregf = h1nasregf
296600951201     C                   eval      mrasysf    = h1sysf
296700981021     c                   move      '1'           mratprec
296800981021     c*
296900981021     c* se siamo in immissione
297000981021     c                   if        opzr105 = '01'
297100981021     c* reperisco numero riga
297200981021     c                   reset                   ndcr09ds
297300981021     C                   eval      SocietaR09 = mrasocieta
297400981021     C                   eval      kccR09     = mrakcc
297500981021     C                   eval      kscR09     = mraksc
297600981021     C                   eval      drpR09     = mradrp
297700981021     C                   eval      nrpR09     = mranrp
297800981021     C                   eval      serparr09  = mraserpar
297900981021     c                   eval      kpjbu = ndcr09ds
298000981021     c                   CALL      'NDCR09R'
298100981021     C                   parm                    kpjba
298200981021     c                   eval      ndcr09ds = kpjbu
298300981021     c                   z-add     NRRigaR09     MraNrRiga
298400981021     c* imposto campo
298500041117     c                   if        calr105 = 'P'
298600981015     c                   eval      mrafma = '1'
298700981015     c                   else
298800981015     c                   eval      mrafma = '0'
298900981015     c                   endif
299000981015     c                   endif
299100951003      *
299200940207     C                   ENDSR
299300940224     C/EJECT
299400950925      ************************************************************
299500951004      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO                   *
299600950925      ************************************************************
299700940224     C     GESAGG        BEGSR
299800950925      *
299900950925      * eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
300000981221     C     CMTr105       IFEQ      '0'
300100981221     C                   COMMIT
300200981221     C                   ENDIF
300300950925      *
300400950925      * segnalo al pgm chiamante l'avvenuta conferma
300500950926     C                   MOVE      *ON           OPRr105
300600950925      * segnalo al pgm chiamante l'aver premuto l'F6
300700950926     C                   MOVE      '0'           RETr105
300800981116      *
300900981221      *
301000001218      * vedo se esiste codice sequenza altri tributi per altre gestioni
301100981216      * INPS/ENPALS ecc.. per chiamare pgm guida
301200981215C0766 *
301300001218C0766C                   IF        dvftinps <> *blanks
301400041116D1700c* da saldaconto non vanno gestiti
301500041116D1700C                             AND CALR105 <> 'S'
301600001218D0796 *
301700001218  "   * viene passata (a parte in un altro userspace) anche la KPJBA per
301800001218  "   *  fare in modo che il primo pgm (XJOBR2) che imposta l'ambiente nel
301900001218  "   *  nuovo job chiamando XSOC lo possa fare con le informazioni in essa
302000001218  "   *  contenute
302100001218  "  C                   Call      'XJOBR1KPJ'
302200001218  "  C                   Parm                    MRASocieta
302300001218D0796C                   Parm                    KPJBA
302400001218C0766 *
302500981216"     *   Valorizzo la DS di passaggio ndcr01ds
302600981215"     *
302700001218"    c                   reset                   NDCR01DS
302800981216"    C                   eval      opzr01 =opzr105
302900981216"    C                   eval      societar01 =mrasocieta
303000981216"    C                   eval      kccr01 =mrakcc
303100981216"    C                   eval      kscr01 =mraksc
303200981216"    C                   eval      dtparr01 =mradrp
303300981216"    C                   eval      nrparr01 =mranrp
303400981216"    C                   eval      serparR01 =mraserpar
303500981216"    C                   eval      nrrigar01 =mranrriga
303600981223"    C                   eval      tributiR01=dvftinps
303700981221"    C                   eval      descrr01=d1descr
303800981216"    C                   MOVEL     ndcr01ds      KPJBU
303900981215"     *
304000981215"     *  cambio job e chiamo guida gestione altri tributi
304100981215"     *
304200981116"    C                   EVAL      LenIn1 = %size(Kpjba)
304300981221"    C                   EVAL      LenIn2 = %size(ndmra)
304400981116"    C                   CALL      'XJOBR1'
304500981216"    C                   PARM      'NDCR01R'     Pgm
304600981216"    C                   PARM      societar01    $Societa
304700981116"    C                   PARM                    KPJBA
304800981116"    C                   PARM                    LenIn1
304900981221"    C                   PARM                    NDMRA
305000981221"    C                   PARM                    LenIn2
305100001218"    C                   MOVEL     KPJBU         NDCR01DS
305200001218"    C                   IF        RetR01 = '1'
305300001218"    c                   move      retR01        RetR105
305400001218"    c                   move      *ON           $FINE
305500001218"    C                   EXSR      endpgm
305600001218C0766c                   endif
305700001218D0796 *
305800001218  "   * cancello userspace
305900001218D0796C                   ExSr      DelUsrSpc
306000001218C0766 *
306100981116C0766C                   ENDIF
306200950925      *
306300941027     C                   SELECT
306400950925      *
306500950925      * nel caso di immissione
306600950926     C     OPZr105       WHENEQ    '01'
306700041117     c                   if        calr105 = 'P'
306800951010      *   ritorno al pgm chiamante
306900951010     C                   MOVE      *ON           $FINE
307000951010     C                   else
307100950925      * ripristino la videata iniziale
307200940224     C                   MOVE      *ON           $INZD1
307300941228     C                   MOVE      'D1'          $GEST
307400951010     C                   endif
307500950925      *
307600950925      * nel caso di modifica
307700950926     C     OPZr105       WHENEQ    '02'
307800950925      * ripasso i campi che devono essere reimpostati sul sfl
307900950926     C                   eval      nrgr105  = d1nrg
308000950926     C                   eval      serregr105  = d1serreg
308100950926     C                   eval      ndcr105  = d1ndc
308200950926     C                   eval      serdocr105  = d1serdoc
308300981105     C                   eval      imsDR105    = d1imsD
308400981105     C                   eval      racDR105    = d1racD
308500950926     C                   eval      star105  = d1sta
308600981105     C                   eval      divisar105 = d1div
308700950928      *
308800950926     C                   CALLB     'XDT4000'
308900950926     C     ddcr105       PARM                    XDTAMG           10
309000950926     C                   PARM      d1ddc         XDTGMA            6 0
309100950929     C                   PARM      4             XDTSTA            1 0
309200950926      *
309300950926     C                   CALLB     'XDT4000'
309400950926     C     drgr105       PARM                    XDTAMG           10
309500950926     C                   PARM      d1drg         XDTGMA            6 0
309600950929     C                   PARM      4             XDTSTA            1 0
309700950926      *
309800950925      * e ritorno al pgm chiamante
309900950301     C                   MOVE      *ON           $FINE
310000950925      *
310100950925      * per tutte le altre opzioni
310200940224     C                   OTHER
310300950925      * ritorno al pgm chiamante
310400940224     C                   MOVE      *ON           $FINE
310500941027     C                   ENDSL
310600950925      *
310700940224     C                   ENDSR
310800941205     C/EJECT
310900001218D0796 ************************************************************
311000001218  "   * Cancellazione userspace parametri creati da transfer-job *
311100001218  "   ************************************************************
311200001218D0796C     DelUsrSpc     BEGSR
311300001218      *
311400001218     C                   MoveL     Job_Num       Job_Num_A
311500001218     C                   Eval      USName = 'XJ' + Job_Num_A
311600001218      * - parametri
311700001218     C                   Clear                   Cmd
311800001218     C                   Eval      Cmd = 'DLTUSRSPC USRSPC(QGPL/' +
311900001218     C                                   USNAme + ')'
312000001218     C                   Call      'QCMDEXC'                            22
312100001218     C                   Parm                    Cmd             100
312200001218     C                   Parm      100           Len              15 5
312300001218      * - KPJBA
312400001218     C                   Clear                   Cmd
312500001218     C                   Eval      Cmd = 'DLTUSRSPC USRSPC(QGPL/' +
312600001218     C                                   %TrimR(USNAme) + 'KP)'
312700001218     C                   Call      'QCMDEXC'                            22
312800001218     C                   Parm                    Cmd             100
312900001218     C                   Parm      100           Len              15 5
313000001218      *
313100001218D0796C                   ENDSR
313200950925      ************************************************************
313300951004      * CONTROLLO ABILITAZIONE TASTI                             *
313400950925      ************************************************************
313500941205     C     CTRKEY        BEGSR
313600950925      *
313700941205     C                   SELECT
313800941205     C     $TASTO        WHENEQ    F01
313900941205     C     $FC(1)        IFEQ      '0'
314000941205     C                   SETON                                        99
314100941205     C                   ENDIF
314200941205     C     $TASTO        WHENEQ    F02
314300941205     C     $FC(2)        IFEQ      '0'
314400941205     C                   SETON                                        99
314500941205     C                   ENDIF
314600941205     C     $TASTO        WHENEQ    F03
314700941205     C     $FC(3)        IFEQ      '0'
314800941205     C                   SETON                                        99
314900941205     C                   ENDIF
315000941205     C     $TASTO        WHENEQ    F04
315100941205     C     $FC(4)        IFEQ      '0'
315200941205     C                   SETON                                        99
315300941205     C                   ENDIF
315400941205     C     $TASTO        WHENEQ    F05
315500941205     C     $FC(5)        IFEQ      '0'
315600941205     C                   SETON                                        99
315700941205     C                   ENDIF
315800941205     C     $TASTO        WHENEQ    F06
315900941205     C     $FC(6)        IFEQ      '0'
316000941205     C                   SETON                                        99
316100941205     C                   ENDIF
316200941205     C     $TASTO        WHENEQ    F07
316300941205     C     $FC(7)        IFEQ      '0'
316400941205     C                   SETON                                        99
316500941205     C                   ENDIF
316600941205     C     $TASTO        WHENEQ    F08
316700941205     C     $FC(8)        IFEQ      '0'
316800941205     C                   SETON                                        99
316900941205     C                   ENDIF
317000941205     C     $TASTO        WHENEQ    F09
317100941205     C     $FC(09)       IFEQ      '0'
317200941205     C                   SETON                                        99
317300941205     C                   ENDIF
317400941205     C     $TASTO        WHENEQ    F10
317500941205     C     $FC(10)       IFEQ      '0'
317600941205     C                   SETON                                        99
317700941205     C                   ENDIF
317800941205     C     $TASTO        WHENEQ    F11
317900941205     C     $FC(11)       IFEQ      '0'
318000941205     C                   SETON                                        99
318100941205     C                   ENDIF
318200941205     C     $TASTO        WHENEQ    F12
318300941205     C     $FC(12)       IFEQ      '0'
318400941205     C                   SETON                                        99
318500941205     C                   ENDIF
318600941205     C     $TASTO        WHENEQ    F13
318700941205     C     $FC(13)       IFEQ      '0'
318800941205     C                   SETON                                        99
318900941205     C                   ENDIF
319000941205     C     $TASTO        WHENEQ    F14
319100941205     C     $FC(14)       IFEQ      '0'
319200941205     C                   SETON                                        99
319300941205     C                   ENDIF
319400941205     C     $TASTO        WHENEQ    F15
319500941205     C     $FC(15)       IFEQ      '0'
319600941205     C                   SETON                                        99
319700941205     C                   ENDIF
319800941205     C     $TASTO        WHENEQ    F16
319900941205     C     $FC(16)       IFEQ      '0'
320000941205     C                   SETON                                        99
320100941205     C                   ENDIF
320200941205     C     $TASTO        WHENEQ    F17
320300941205     C     $FC(17)       IFEQ      '0'
320400941205     C                   SETON                                        99
320500941205     C                   ENDIF
320600941205     C     $TASTO        WHENEQ    F18
320700941205     C     $FC(18)       IFEQ      '0'
320800941205     C                   SETON                                        99
320900941205     C                   ENDIF
321000941205     C     $TASTO        WHENEQ    F19
321100941205     C     $FC(19)       IFEQ      '0'
321200941205     C                   SETON                                        99
321300941205     C                   ENDIF
321400941205     C     $TASTO        WHENEQ    F20
321500941205     C     $FC(20)       IFEQ      '0'
321600941205     C                   SETON                                        99
321700941205     C                   ENDIF
321800941205     C     $TASTO        WHENEQ    F21
321900941205     C     $FC(21)       IFEQ      '0'
322000941205     C                   SETON                                        99
322100941205     C                   ENDIF
322200941205     C     $TASTO        WHENEQ    F22
322300941205     C     $FC(22)       IFEQ      '0'
322400941205     C                   SETON                                        99
322500941205     C                   ENDIF
322600941205     C     $TASTO        WHENEQ    F23
322700941205     C     $FC(23)       IFEQ      '0'
322800941205     C                   SETON                                        99
322900941205     C                   ENDIF
323000941205     C     $TASTO        WHENEQ    F24
323100941205     C     $FC(24)       IFEQ      '0'
323200941205     C                   SETON                                        99
323300941205     C                   ENDIF
323400941205     C                   ENDSL
323500950925      *
323600941205     C                   ENDSR
323700940318     C/EJECT
323800950925      ******************************************************
323900950925      * PGM PER GESTIONE MESSAGGI ERRORE                   *
324000950925      ******************************************************
324100940318     C     MSGPGM        BEGSR
324200950925      *
324300940318     C                   Z-ADD     2             MSGMSG
324400940318     C                   MOVEL     'S'           MSGRSP
324500940318     C                   MOVEL     'CR'          MSGTPR
324600940318     C                   MOVEL     'S'           MSGEMV
324700940318     C                   Z-ADD     10            MSGRGP
324800940318     C                   MOVEL     'S'           MSGLCK
324900940318     C                   MOVEA     STATUS        STS
325000940318     C                   MOVEL     'N'           MSGCNL
325100940318     C                   MOVEL     'N'           MSGVID
325200940318     C                   MOVEL     'N'           MSGOPE
325300940318     C                   MOVEL     'N'           MSGSTP
325400950522     C                   CALL      'XMSGR'
325500940318     C                   PARM                    XMSGDS
325600950925      *
325700940318     C                   ENDSR
325800940207     C/EJECT
325900950925      ************************************************************
326000951004      * OPERAZIONI INIZIALI                                      *
326100950925      ************************************************************
326200940131     C     *INZSR        BEGSR
326300000117C1309C     *DMY          Move      Udate         ZDateISO
326400000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
326500950925      *
326600950925      * Reperimento parametri
326700950925      *
326800940117     C     *ENTRY        PLIST
326900940117     C                   PARM                    KPJBA
327000950928      ****               PARM                    DSAUT
327100950925      *
327200950928      *    PARMS         IFLT      2
327300950928      *                  MOVE      *BLANK        AUTDS
327400950928      *                  ELSE
327500950928      *                  MOVEL     DSAUT         AUTDS
327600950928      *                  ENDIF
327700950925      * reperisco le dimensioni delle variabili che conterranno i tasti
327800950726     C                   EVAL      $D1L01 = %Size(Z1Ke1)
327900950726     C                   EVAL      $D1L02 = %Size(Z1Ke2)
328000950925      *
328100950925      * Reperimento tasti di funzione
328200950925      *
328300940506     C                   EXSR      REPKEY
328400950925      *
328500951005      * Poichè il chiamante usa un altro actgrp :
328600951005     C                   CALLB     'XSTRCMT'
328700951005     C                   OPEN      NDMRA00F
328800940223     C                   ENDSR
328900940223     C/EJECT
329000950925      ************************************************************
329100951004      * REPERIMENTO TASTI DI FUNZIONE                            *
329200950925      ************************************************************
329300940506     C     REPKEY        BEGSR
329400940506     C                   MOVEA     $FM1          $FM
329500950929     C                   MOVEA     $Fc1          $FC
329600940506     C                   EXSR      REPKKK
329700940506     C                   MOVEA     $FX           $FX1
329800950925      *
329900940614     C                   ENDSR
330000940506     C/EJECT
330100950925      ************************************************************
330200950925      * REPERIMENTO TASTI DI FUNZIONE
330300950925      ************************************************************
330400940506     C     REPKKK        BEGSR
330500950925      *
330600940506     C                   CLEAR                   $FX
330700940506     C     1             DO        24            X
330800950925      *
330900940506     C     $FM(X)        IFNE      *BLANK
331000950721     C     $Fc(X)        andne     '0'
331100950925      *
331200940506     C                   MOVEL     $FM(X)        IDMSG
331300950522     C                   CALLB     'XRTVM'
331400940506     C                   PARM                    IDMSG            27
331500940506     C                   PARM                    TXTMSG          100
331600940506     C                   PARM                    ERRMSG            1
331700950925      *
331800940506     C     ERRMSG        IFNE      *ON
331900940506     C                   MOVEL     TXTMSG        $FX(X)
332000940506     C                   ELSE
332100940506     C                   MOVEL     *ALL' '       $FX(X)
332200941027     C                   ENDIF
332300950925      *
332400941027     C                   ENDIF
332500950925      *
332600941027     C                   ENDDO
332700950925      *
332800940506     C                   ENDSR
332900940509     C/EJECT
333000950925      ************************************************************
333100951004      * INIZIALIZZAZIONE VARIABILI                               *
333200950925      ************************************************************
333300940223     C     INZVAR        BEGSR
333400950925      *
333500950925      * Dati da PGM guida
333600950925      *
333700950928     C                   MOVEL     KPJBU         ndcr05DS1
333800951005      *
333900951005      *Gestione richiamo da prima nota se non immissione
334000041117     C* DA SALDACONTO NON LO DEVE FARE
334100041117     C                   if        calr105 = 'P'
334200951005     C                   exsr      GESCALPN
334300951005     C                   endif
334400950925      *
334500950925      * Reperimento dati società
334600950925      *
334700950928     C                   MOVEL     'SOC001'      TIPXSC
334800941115     C                   MOVEL     *BLANK        SOCXSC
334900941115     C                   EXSR      REPSOC
335000941115     C     RTNXSC        IFNE      '1'
335100941115     C                   MOVEL     XSOCDS        SOC001
335200941115     C                   ELSE
335300941115     C                   RETURN
335400941115     C                   ENDIF
335500950925      *
335600950926      * Reperimento autorizzazioni
335700110211D2666X***  AUTDS         IFEQ      *BLANK
335800110211  "  C                   EXSR      REPAUT
335900110211  "  X***                ELSE
336000110211  "  X***                MOVEL     XCADDS        ______DS
336100110211D2666X***                ENDIF
336200950925      *
336300950925      * Ctrl se l'opzione con cui è stato richiamato è congruente con
336400950925      * le autorizzazione
336500950925      *
336600110211D2666C                   EXSR      TSTAUT
336700950925      *
336800950925      * Definizione dei tasti funzione utilizzabili a pgm
336900950925      *
337000940715     C                   EXSR      DEFOPZ
337100950925      *
337200950925      * Variabili per gestione videate
337300950925      *
337400940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
337500940223     C                   MOVE      *OFF          $FINE                          fine pgm
337600940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
337700940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
337800940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
337900940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
338000950925      *
338100950925      * Variabili appoggio
338200950925      *
338300940224     C                   Z-ADD     0             CURR
338400940224     C                   Z-ADD     0             CURC
338500070122C2002C                   Eval      D1DPFIso    = DatMin
338600950925      *
338700950925      * Valorizzazione campi univoci testate
338800950925      *
338900940207     C                   CLEAR                   FMTT1
339000940207     C                   MOVEL     KNSIF         NOMSIF
339100941110     C                   MOVEL     XSCDSI        NOMDIT
339200941027     C                   MOVEL     DSPGM         NOMPGM
339300950925      * recupero il sottotitolo (se c'è)
339400950331     C                   MOVEL     *BLANK        T1TIT
339500950331     C     SOTTIT        IFNE      *BLANK
339600950331     C                   MOVEL     SOTTIT        IDMSG
339700950522     C                   CALLB     'XRTVM'
339800950331     C                   PARM                    IDMSG            27
339900950331     C                   PARM                    TXTMSG          100
340000950331     C                   PARM                    ERRMSG            1
340100950331     C                   PARM      *ON           CTRMSG            1            centratura
340200950331     C                   PARM      30            LENMSG            3 0          lun output.
340300950331     C     ERRMSG        IFNE      *ON
340400950331     C                   MOVEL     TXTMSG        T1TIT
340500950331     C                   ELSE
340600950331     C                   MOVEL     *ALL'?'       T1TIT
340700950331     C                   ENDIF
340800950331     C                   ENDIF
340900950925      * recupero il tipo operazione
341000950331     C                   Z-ADD     1             X
341100950922    >C     OPZr105       LOOKUP    $OZ(X)                                 21
341200950331     C     *IN21         IFEQ      *ON
341300950331     C                   MOVEL     $ID(X)        IDMSG
341400950522     C                   CALLB     'XRTVM'
341500950331     C                   PARM                    IDMSG            27
341600950331     C                   PARM                    TXTMSG          100
341700950331     C                   PARM                    ERRMSG            1
341800950331     C                   PARM      *ON           CTRMSG            1            centratura
341900950331     C                   PARM      30            LENMSG            3 0          lun output.
342000950331     C     ERRMSG        IFNE      *ON
342100950331     C                   MOVEL     TXTMSG        T1OPE
342200950331     C                   ELSE
342300950331     C                   MOVEL     *ALL'?'       T1OPE
342400950331     C                   ENDIF
342500950331     C                   ENDIF
342600950925      *
342700950925      * Reperisco e memorizzo default in caso di immissione
342800950925      *
342900950929      **** OPZr105       IFEQ      '01'
343000950929      ************       MOVE      WWWSOC        ___SOC
343100950929      ************       MOVE      DF1r105       ___OBJ
343200950929      ************       MOVE      DF1r105       ___TYP
343300950929      **** K03mra01      CHAIN     ndmra01L                           21
343400950929      **** *IN21**       IFEQ      *OFF
343500950929      ************       EXSR      REPDEF
343600950929      ************       ENDIF
343700950929      ************       ENDIF
343800950925      *
343900950925      * Inizializzazione/Decodifica prima videata
344000950925      *
344100940224     C                   EXSR      INZD1
344200940224     C                   MOVE      *OFF          $INZD1
344300950925      *
344400940117     C                   ENDSR
344500941209     C/EJECT
344600950925      ************************************************************
344700951004      * REPERIMENTO DATI SOCIETA'                                *
344800950925      ************************************************************
344900941209     C     REPSOC        BEGSR
345000950925      *
345100950522     C                   CALLB     'XSOC'
345200941209     C                   PARM                    TIPXSC            6
345300941209     C                   PARM                    SOCXSC            3
345400941209     C                   PARM                    CDSXSC            9 0
345500941209     C                   PARM                    MODXSC            3
345600941209     C                   PARM      *BLANKS       RTNXSC            1
345700941209     C                   PARM                    XSOCDS
345800941209     C                   PARM                    KPJBA
345900950925      *
346000941209     C                   ENDSR
346100950301     C/EJECT
346200110211D2666C************************************************************
346300110211  "  C* REPERIMENTO AUTORIZZAZIONI UTENTE                        *
346400110222  "  C************************************************************
346500110211D2666C     REPAUT        BEGSR
346600110211     C*
346700110211     C                   CLEAR                   AUTDS
346800110211     C                   CLEAR                   CONRITDS
346900110211     C*
347000110211     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
347100110211     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
347200110211     C                   Z-ADD     0             XCAGRP                         GRUPPO
347300110211     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
347400110211    >C                   MOVEL     'CONRIT'      XCAFNC                         FUNZIONE
347500110211     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
347600110211    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
347700110211     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
347800110211     C*
347900110211     C                   CALLB     'XCHKAUT'
348000110211     C                   PARM                    AUTDS
348100110211     C*
348200110211     C* errori/non abilitazione
348300110211     C     XCARTN        IFNE      0
348400110211     C                   MOVE      '2'           ERRR105
348500110211     C                   MOVE      *ON           $FINE
348600110211     C                   EXSR      ENDPGM
348700110211     C                   ELSE
348800110211    >C                   MOVEL     XCADDS        CONRITDS
348900110211     C                   ENDIF
349000110211     C*
349100110211D2666C                   ENDSR
349200110211  "  C/EJECT
349300110211  "  C************************************************************
349400110211  "  C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
349500110211  "  C************************************************************
349600110211D2666C     TSTAUT        BEGSR
349700110211     C*
349800110211     C                   SELECT
349900110211     C*
350000110211     C* se richiamato in immissione
350100110211    >C     OPZR105       WHENEQ    '01'
350200110211     C* ma l'utente non è abilitato
350300110211    >C     IMMRIT        ANDEQ     *OFF
350400110211     C* esco da pgm avvertendolo
350500110211    >C                   MOVE      '2'           ERRR105
350600110211     C                   MOVE      *ON           $FINE
350700110211     C                   EXSR      ENDPGM
350800110211     C*
350900110211     C* se richiamato in modifica
351000110211    >C     OPZR105       WHENEQ    '02'
351100110211     C* ma l'utente non è abilitato
351200110211    >C     MANRIT        ANDEQ     *OFF
351300110211     C* testo se l'utente è abilitato almeno alla visualizzazione
351400110211    >C     VISRIT        IFEQ      *ON
351500110211    >C                   MOVE      '05'          OPZR105
351600110211     C                   ELSE
351700110211     C* altrimenti esco da pgm avvertendolo
351800110211    >C                   MOVE      '2'           ERRR105
351900110211     C                   MOVE      *ON           $FINE
352000110211     C                   EXSR      ENDPGM
352100110211     C                   ENDIF
352200110211     C*
352300110211     C* se richiamato in copia
352400110211    >C     OPZR105       WHENEQ    '03'
352500110211     C* ma l'utente non è abilitato
352600110211    >C     IMMRIT        ANDEQ     *OFF
352700110211     C* esco da pgm avvertendolo
352800110211    >C                   MOVE      '2'           ERRR105
352900110211     C                   MOVE      *ON           $FINE
353000110211     C                   EXSR      ENDPGM
353100110211     C*
353200110211     C* se richiamato in annullamento
353300110211    >C     OPZR105       WHENEQ    '04'
353400110211     C* ma l'utente non è abilitato
353500110211    >C     ANNRIT        ANDEQ     *OFF
353600110211     C* esco da pgm avvertendolo
353700110211    >C                   MOVE      '2'           ERRR105
353800110211     C                   MOVE      *ON           $FINE
353900110211     C                   EXSR      ENDPGM
354000110211     C*
354100110211     C* se richiamato in visualizzazione
354200110211    >C     OPZR105       WHENEQ    '05'
354300110211     C* ma l'utente non è abilitato
354400110211    >C     VISRIT        ANDEQ     *OFF
354500110211     C* esco da pgm avvertendolo
354600110211    >C                   MOVE      '2'           ERRR105
354700110211     C                   MOVE      *ON           $FINE
354800110211     C                   EXSR      ENDPGM
354900110211     C*
355000110211     C                   ENDSL
355100110211D2666C*
355200110211  "  C                   ENDSR
355300110211D2666C/EJECT
355400950925      ************************************************************
355500951004      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM            *
355600950925      ************************************************************
355700940715     C     DEFOPZ        BEGSR
355800950925      *
355900950925      * Questa routine deve valorizzare la schiera $FC1/$FR1
356000950925      * con S o N a seconda della utilizzabilità o meno
356100950925      * del tasto funzione correlato
356200950925      *
356300950925      * Inizialmente ripristino le abilitazioni come da schiera a tempo
356400950925      * di compilazione
356500940715     C                   RESET                   $FC1
356600940715     C                   RESET                   $FR1
356700950925      *
356800950925      * Dopodichè, a seconda del richiamo e delle autorizzazioni
356900950925      * invalido alcune opzioni
357000940715     C                   SELECT
357100950925      * Se pgm richiamato in interrogazione disattivo :
357200950928     C     OPZr105       WHENEQ    '05'
357300950925      * F4=Ricerca
357400940715     C                   Z-ADD     4             X
357500940720     C                   MOVE      '0'           $FC1(X)
357600950925      * F5=Ripristino
357700940715     C                   Z-ADD     5             X
357800940720     C                   MOVE      '0'           $FC1(X)
357900950925      * F6=Conferma
358000940715     C                   Z-ADD     06            X
358100940720     C                   MOVE      '0'           $FC1(X)
358200950925      * Se pgm richiamato in annullamento disattivo :
358300950928     C     OPZr105       WHENEQ    '04'
358400950925      * F4=Ricerca
358500940715     C                   Z-ADD     4             X
358600940720     C                   MOVE      '0'           $FC1(X)
358700950925      * F5=Ripristino
358800940715     C                   Z-ADD     5             X
358900940720     C                   MOVE      '0'           $FC1(X)
359000950925      *
359100940715     C                   OTHER
359200950925      *
359300940715     C                   ENDSL
359400950925      *
359500040326c1741x* non cambia niente per il pregresso anche se c'è F8 da prima not
359600040326  "  x* a, ma cambia per questo sviluppo in quanto serve F8 anche da
359700040326  "  x* prima nota
359800040326  "  x*                  if        CALR105 = 'P'
359900040326  "  x* Se il programma è rich. da P.N. elimino selezioni seguenti
360000040326  "  x*                  Z-ADD     8             X
360100040326  "  x*                  MOVE      '0'           $FC1(X)
360200040326c1741x*                  ENDIF
360300951018      *
360400940715     C                   ENDSR
360500950925      ************************************************************
360600951004      * DEFINIZIONE KLIST                                        *
360700950925      ************************************************************
360800950606     C     DefKlist      BEGSR
360900950925      *
361000950925      * klist
361100950925      *
361200950928      * ANOPE01L
361300950928     C     K02ope01      KLIST
361400950928     C                   KFLD                    xscsoc
361500950928     C                   KFLD                    opecausale
361600950928      * ANTRI01L
361700950928     C     K03tri01      KLIST
361800950928     C                   KFLD                    xscsoc
361900950928     C                   KFLD                    tritributo
362000950928     C                   KFLD                    tritriprog
362100951005      * ndmra08L
362200951005     C     K03MRA08      KLIST
362300951005     C                   KFLD                    MraSysf
362400951005     C                   KFLD                    MraNasregf
362500951005     C                   KFLD                    MraSta
362600950925      *
362700951002     C* PLIST per NDVASOG
362800951002     C     PDVASOG       PLIST
362900951002     C                   PARM                    DVARIC
363000951002     C                   PARM                    DVASOCIETA
363100951002     C                   PARM                    DVAUNITA
363200951002     C                   PARM                    DVASTRUTT
363300951002     C                   PARM                    DVADTRIF
363400951002     C                   PARM                    DVALENOUT
363500951002     C* Se si accede con il codice cli/for impostare i campi seguenti
363600951002     C* se si accede con il soggetto ometterli e impostare gli altri
363700951002     C                   PARM                    DVASNATURA
363800951002     C                   PARM                    DVACODICE
363900951002     C                   PARM                    DVALINEAV
364000951002     C                   PARM                    DVAFILIALE
364100951002     C                   PARM                    *omit
364200951002     C*                  PARM                    *omit
364300951002     C*                  PARM                    *omit
364400951002     C*                  PARM                    DVSSOGG
364500951002     C                   PARM                    DVATPIND
364600951002     C                   PARM                    DVACDIND
364700951002     C                   PARM                    DVAERRORE
364800951002     C                   PARM                    DVAOUTPUT
364900040225c1728C* NDPPA01L                                           *
365000040225  "  C     K07PPA01      KLIST
365100040225  "  C                   KFLD                    ppasocieta
365200040225  "  C                   KFLD                    PPACTB
365300040225  "  C                   KFLD                    ppakcc
365400040225  "  C                   KFLD                    ppaksc
365500040225  "  C                   KFLD                    ppadtpar
365600040225  "  C                   KFLD                    ppanrpar
365700040225c1728C                   KFLD                    ppaserpar
365701170222R235 C*
365702170222 "   C* SDGSPA01L
365703170222 "   C     K03SPA01      KLIST
365704170222 "   C                   KFLD                    SPASocieta
365705170222 "   C                   KFLD                    SPAKcc
365706170222R235 C                   KFLD                    SPAKsc
365800951002      *
365900940117     C                   ENDSR
366000951002      ************************************************************
366100951003      * Accesso ad ansogg                                        *
366200951002      ************************************************************
366300951003     C     redsog        BEGSR
366400951002     C                   MOVE      '1'           DVARIC
366500951002     C                   MOVE      xscsoc        DVASOCIETA
366600951002     C                   MOVE      'F'           DVASNATURA
366700951002     C                   MOVE      d1ksc         DVAcodice
366800951002     C                   MOVEL     'DVAFOR'      DVAstrutt
366900951002     C                   EVAL      DVALenOut = %Size(DVAFOR)
367000951002      *
367100951002     C                   CALLB     'NDDVASOG'    PDVASOG
367200951002      *
367300951002     C                   if        DVAerrore <> *ON
367400951002     C                   eval      %subst(DVAFOR:1:DVALENOUT)
367500951002     C                             = %subst(DVAOUTPUT:1:DVALENOUT)
367600960215     C                   eval      d1descr   = dvfdes
367700960215     C                   eval      d1indriz  = dvfindriz
367800951003     C                   eval      d1localit = dvflocalit
367900951003     C                   eval      d1cap     = dvfcap
368000951003     C                   eval      d1prov    = dvfprov
368100951003     C                   eval      d1tristd  = dvftributo
368200951003     C                   eval      d1ptrstd  = dvfvartrib
368300960312     c                   if        d1tri = *blanks
368400951003     C                   eval      d1tri      = dvftributo
368500951003     C                   eval      d1ptr      = dvfvartrib
368600960312     c                   endif
368700981020     c* se divisa non valorizzata, la prendo dal fornitore
368800981019     c                   if        d1div = *blanks
368900981019     C                   eval      d1div      = dvfdivisa
369000981019     c                   endif
369100951003      *
369200991011D0256C***                eval      tritributo = d1tristd
369300991011D0256C***                eval      tritriprog = d1ptrstd
369400991011D0256C                   eval      tritributo = d1tri
369500991011D0256C                   eval      tritriprog = d1ptr
369600951003     C     k03tri01      chain     antri01l                           21
369700951003     C                   IF        *in21     = *off
369800951003     C                   eval      d1trid    = tridescr
369900971224     C     d1pai         ifeq      0
370000951004     C                   eval      d1pai     = triassimp
370100971224     C                   endif
370200971224     C     d1pra         ifeq      0
370300951004     C                   eval      d1pra     = triritacc
370400971224     C                   endif
370500951003     C                   ENDIF
370600951002     C                   endif
370700951002     C                   endsr
370800940509     C/EJECT
370900950925      ************************************************************
371000951004      * RIEMPIMENTO TASTI FUNZIONE                               *
371100950925      ************************************************************
371200940509     C     RIEKEY        BEGSR
371300950925      *
371400950925      * Input :
371500950925      * - $FX = schiera con tutti i testi disponibili
371600950925      * - $FC = schiere con i flag di validità associati ai testi
371700950925      * - $FR = schiere con i flag di REVERSE IMAGE
371800950925      * - $ULK = ultimo testo caricato
371900950925      * - $L01 = Lunghezza prima riga
372000950925      * - $L02 = Lunghezza seconda riga
372100950925      * Output :
372200950925      * - $ULK
372300950925      * - $KEY1= 1° riga di tasti funzione
372400950925      * - $KEY2= 2° riga di tasti funzione
372500950925      *
372600950522     C                   CALLB     'XKEYMSG'
372700950925      * input
372800940509     C                   PARM                    $FX
372900940509     C                   PARM                    $FC
373000940509     C                   PARM                    $FR
373100940509     C                   PARM                    $ULK              3 0
373200950925      * output
373300950927     C                   PARM                    $KEY1            79
373400940509     C                   PARM                    $KEY2            79
373500950925      * input
373600950726     C                   PARM                    $L01              3 0
373700950726     C                   PARM                    $L02              3 0
373800950925      *
373900060224C1913C                   PARM                    $ALLFUNCT       480
374000060224C1913C*
374100940509     C                   ENDSR
374200951005     C/EJECT
374300951005      ************************************************************
374400951005      * GESTIONE RICHIAMO DA PRIMA NOTA                          *
374500951005      ************************************************************
374600951005     C     GESCALPN      BEGSR
374700951005      *
374800951005     C                   eval      mranasregf = nasrgfr105
374900951005     C                   eval      mrasta     = star105
375000951005     C                   eval      mrasysf    = sysfr105
375100951005     C     k03mra08      chain     ndmra08l                           21
375200951005     C                   if        *in21 = *off
375300951005     C                   z-add     NRRMRA        NRRMRAR105
375400951005     C                   eval      opzr105 = '02'
375500951005     C                   else
375600951005     C                   eval      opzr105 = '01'
375700951005     C                   endif
375800951005      *
375900951005     C                   ENDSR
376000951115     C/EJECT
376100951115      ************************************************************
376200951115      * Controllo record doppio per percipiente partita          *
376300951115      ************************************************************
376400951115     C     CtlRecDup     BEGSR
376500981126      *
376600981109     c                   move      opzr105       Operazione
376700981109      * se in modifica cambiano gli estremi partita, passo al
376800981109      *programma di controllo record doppio ritenuta operazione '01'
376900981109     c                   if        d1nrp <> mranrpsav or
377000981109     c                             d1serpar <> mrasersav or
377100981109     C                             mradrpsav <> d1drp
377200981109     c                   eval      operazione = '01'
377300981109     c                   endif
377400951115     C                   Call      'NDCR05R2'
377500951115     C                   PARM                    Xscsoc
377600951115     C                   PARM                    D1kcc
377700951115     C                   PARM                    D1ksc
377800951115     C                   PARM                    D1DrpAmg         10
377900951115     C                   PARM                    D1Nrp
378000951115     C                   PARM                    D1serpar
378100981109     C                   PARM                    Operazione        2
378200951115     C                   PARM                    RecDup            1 0
378300951115      *
378400951115     C                   ENDSR
378500941214**
37860095040401PROMSG    *LIBL     COS0001
37870095092202PROMSG    *LIBL     COS0002
37880095040403PROMSG    *LIBL     COS0003
37890095040404PROMSG    *LIBL     COS0004
37900095040405PROMSG    *LIBL     COS0005
379100940509** TASTI DI FUNZIONE  D1
379200940509PROMSG    *LIBL     KEY0001  01
379300940509PROMSG    *LIBL     KEY0002  02
379400940509PROMSG    *LIBL     KEY0003  03
379500940509PROMSG    *LIBL     KEY0004  04
379600940509PROMSG    *LIBL     KEY0005  05
379700940509PROMSG    *LIBL     KEY0006  06
379800940509PROMSG    *LIBL     KEY0007  07
379900940509PROMSG    *LIBL     KEY0008  08
380000940509PROMSG    *LIBL     KEY0009  09
380100940509PROMSG    *LIBL     KEY0010  10
380200940509PROMSG    *LIBL     KEY0011  11
380300940509PROMSG    *LIBL     KEY0012  12
380400940509PROMSG    *LIBL     KEY0013  13
380500940509PROMSG    *LIBL     KEY0014  14
380600940509PROMSG    *LIBL     KEY0015  15
380700940509PROMSG    *LIBL     KEY0016  16
380800940509PROMSG    *LIBL     KEY0017  17
380900940509PROMSG    *LIBL     KEY0018  18
381000940509PROMSG    *LIBL     KEY0019  19
381100940509PROMSG    *LIBL     KEY0020  20
381200940509PROMSG    *LIBL     KEY0021  21
381300940509PROMSG    *LIBL     KEY0022  22
381400940509PROMSG    *LIBL     KEY0023  23
381500940509PROMSG    *LIBL     KEY0024  24
381600940509**  ABILITAZIONE D1
3817009407200N  01
3818009407200N  02
3819009407201N  03
3820009407201N  04
3821009407201N  05
3822009407201N  06
3823009407200N  07
3824009407201N  08
3825009407200N  09
3826009411160N  10
3827009407200N  11
3828009407201N  12
3829009407200N  13
3830009407200N  14
3831009407200N  15
3832009407200N  16
3833009407200N  17
3834009407200N  18
3835009407200N  19
3836009407200N  20
3837009407200N  21
3838009407200N  22
3839009507260N  23   Impostare a '1' se si utilizzano opzioni
3840009507261N  24   Lasciare sempre impostata a '1' il valore della riga 24
384100950928**    SCHIERA -- XSTA --
384200950928DA VERSARE
384300950928VERS.RICHIESTO
384400950928VERS.EFFETTUATO
