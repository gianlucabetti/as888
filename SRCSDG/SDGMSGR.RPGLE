000100091005     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200091005     H*PARMS CVTOPT(*none)
000300091005     H DECEDIT('0,') DATEDIT(*DMY.)
000400091005     F*
000500091019     F* Emissione messaggi in finestra
000600091005     F*
000700091019     F* ----------------------------------------------------
000800091019     F* Schema situazioni predefinite
000900091005     F*
001000091019     F* 12 = *ON : rolldown
001100091019     F* 14 = *ON : Mostra altri messaggi
001200091019     F* 15 = *ON : implementa il "segue..."
001300091019     F* 16 = *ON : rollup
001400091019     F* 17 = *ON : messaggio che richiede risposta da parte operatore
001500091019     F* 18 = *ON : file bloccato (classico..)
001600091019     F* 19 = *ON : Verifica situazione di errore
001700091019     F* 71 = *ON : messaggi di 2° livello
001800091019     F*
001900091019     F* Gli Indicatori 41-49 prenotati per 9 botton
002000091019     F*
002100091019     F* ----------------------------------------------------
002200091005     FSDGMSGD   CF   E             WORKSTN USROPN
002300091005     F                                     INFDS(DSFMT)
002400091005     FSDGMSGP   O    E             PRINTER OFLIND(*IN99)
002500930507     F                                     USROPN
002600900508     F*-------
002700930607     D*-----------------------------------------------------*          ***
002800930608     D LCB             S              1    DIM(10)                              Lavoro che blocca
002900001020     D* schiera tipi risposta
003000930513     D TPR             S              1    DIM(10)                              risposte possibili
003100001020     D* schiera messaggi da mandare a operatore, stampa, lavoro
003200091005     D MSO             S            130    DIM(6)                               Msg batch
003300091019     D* sk costanti usare a programma
003400091019     D CST             S             27    DIM(7) CTDATA PERRCD(1)
003500091019     D* sk msg preimpostati
003600091019     D MSG             S             27    DIM(10) CTDATA PERRCD(1)
003700091019     D* sk per utilizzo stampa
003800091019     D SK              S             66    DIM(10) CTDATA PERRCD(1)
003900091019     D* sk cmd
004000091019     D CMD             S             80    DIM(8) CTDATA PERRCD(1)
004100930607     D*-----------------------------------------------------*          ***
004200950728     D* Testi bottoni
004300950728     D                 DS
004400950728     D  D1TXT1                 1     15
004500950728     D  D1TXT2                16     30
004600950728     D  D1TXT3                31     45
004700950728     D  D1TXT4                46     60
004800950728     D  D1TXT5                61     75
004900950728     D  D1TXT6                76     90
005000950728     D  D1TXT7                91    105
005100950728     D  D1TXT8               106    120
005200950728     D  D1TXT9               121    135
005300950728     D  TXT                    1    135
005400950728     D                                     DIM(9)                               testi risposte
005500001030     D*-------------
005600001030     D* codici bottoni
005700001030     D                 DS
005800001030     D  W1Cod1                 1      7
005900001030     D  W1Cod2                 8     14
006000001030     D  W1Cod3                15     21
006100001030     D  W1Cod4                22     28
006200001030     D  W1Cod5                29     35
006300001030     D  W1cod6                36     42
006400001030     D  W1Cod7                43     49
006500001030     D  W1Cod8                50     56
006600001030     D  W1Cod9                57     63
006700001030     D  W1Cod                  1     63
006800091005     D*-------------
006900091005     D* Reperimento nome PGM
007000091005     D STATUS         SDS           333
007100091005     D  DSPGM            *PROC
007200091005     D  PARMS            *PARMS
007300930604     D*------------------------
007400091005     D* Parametri
007500930604     D*------------------------
007600091005     D SDGMSGDS      E DS
007700091019     D  STW          E                     EXTFLD(MSGSTS)
007800930602     D                                     DIM(333)
007900930513     D  JBA          E                     EXTFLD(MSGJBA)
008000930513     D                                     DIM(502)                                           bili
008100930604     D*------------------------
008200091019     D* DS Print KPJBA
008300930604     D*------------------------
008400930513     D PRTJBA          DS
008500930513     D  PRTDT1                 1    100
008600930513     D  PRTDT2               101    200
008700930513     D  PRTDT3               201    300
008800930513     D  PRTDT4               301    400
008900930513     D  PRTDT5               401    502
009000091019     D*------------------------
009100091019     D* Ds per STW : individuo campi
009200091019     D*------------------------
009300091019     D STWDS           DS
009400091019     D  STWMS1                91    130
009500091019     D  STWMS2               131    170
009600091019     D  STWCOD               209    213
009700091019     D  STWJOB               244    253
009800091019     D  STWUSR               254    263
009900091019     D  STWNBR               264    269
010000930604     D*-----------------------------------*
010100091019     D* Ds per emessione video di riposte previste
010200930604     D*-----------------------------------*
010300930513     D                 DS
010400930513     D  DTPR                   1     20
010500930513     D                                     DIM(20)                              risposte possib. vid
010600930513     D  D1TPR                  1     20
010700930607     D*-------------------------*
010800091019     D* Ds per invio msg
010900930607     D*-------------------------*
011000930513     D                 DS
011100930602     D  CMD1                   1    160
011200930602     D  NBR1                  23     28
011300930602     D  USR1                  30     39
011400091005     D  JOB1                  41     50
011500091005     D  CAUSA                 52     89
011600091005     D*----------
011700091005     D                 DS
011800091005     D  CMD2                   1    160
011900091005     D  MSG2                  15    124
012000091005     D  MSQ2                 135    144
012100091005     D*----------
012200091005     D                 DS
012300091005     D  CMD3                   1    160
012400091005     D  USR3                  16     25
012500091005     D  Msg14471              29     80
012600091005     D  Msg14472              81    106
012700091005     D  MSQ3                 117    126
012800091005     D*----------
012900091005     D                 DS
013000091005     D  CMD4                   1    160
013100091005     D  MSG4                  21    100
013200091005     D                 DS
013300091005     D  FILE                  20     29
013400971010     D*-------------
013500091005     D ZDateISO        S               D
013600000118     D*-------------
013700091005     D* posizione cursore
013800091005     D CURSOR          DS
013900091005     D  RIRI                   1      2B 0 INZ
014000091005     D  R$$                    2      2
014100091005     D  COCO                   3      4B 0 INZ
014200091005     D  C$$                    4      4
014300091005     D DSFMT           DS           512
014400091005     D  $TASTO               369    369
014500091005     D  NRG                  370    370
014600091005     D  NCL                  371    371
014700091005     D* Campi di lavoro
014800091005     D WCtrmsg         s              1
014900091005     D W1Msg           s            130
015000091005     D W1Ms1           s            130
015100091005     D W1Ms2           s            130
015200091005     D W1Ms3           s            130
015300091005     D WMsgSend        s            130
015400091005     D WMsgS3000       s           3000
015500091005     D WwMsg3000       s           3000
015600091005     D W1Msg3000       s           3000
015700091005     D W1Ms13000       s           3000
015800091005     D W1Ms23000       s           3000
015900091005     D W1Ms33000       s           3000
016000091005     D W1Cd13000       s           3000
016100091005     D W1Cd23000       s           3000
016200091005     D W1Cd33000       s           3000
016300091005     D W1Cd43000       s           3000
016400091005     D W1Cd53000       s           3000
016500091005     D W1Cd63000       s           3000
016600091005     D W1Cd73000       s           3000
016700091005     D W1Cd83000       s           3000
016800091005     D W1Cd93000       s           3000
016900091005     D WrkMsg3000      s           3000
017000091005     D SavCdTit        s              7
017100091005     D SavCdTes        s              7
017200091005     D Primavolta      s              1
017300091005     D H1Rig           s              3  0
017400091005     D H1Rig1          s              3  0
017500091005     D H1Rig2          s              3  0
017600091005     D H1Rig3          s              3  0
017700091005     D*-------------
017800091005     D* Variabili appoggio sempre presenti in tutte le anagrafiche
017900091005     D CurR            S              2S 0
018000091005     D CurC            S              2S 0
018100091005     D*-------------
018200091005     D* campi a video per messaggio di secondo livello
018300091005     D D6TESTO         DS
018400091005     D  D6RIG1                 1     75
018500091005     D  D6RIG2                76    150
018600091005     D  D6RIG3               151    225
018700091005     D  D6RIG4               226    300
018800091005     D  D6RIG5               301    375
018900091005     D  D6RIG6               376    450
019000091005     D  D6RIG7               451    525
019100091005     D  D6RIG8               526    600
019200091005     D  D6RIG9               601    675
019300091005     D  D6RIG10              676    750
019400091005     D  D6RIG11              751    825
019500091005     D  D6RIG12              826    900
019600091005     D  D6RIG13              901    975
019700091005     D  D6RIG14              976   1050
019800091005     D  D6RIG15             1051   1125
019900091005     D  D6RIG16             1126   1200
020000091005     D  D6RIG17             1201   1275
020100091005     D  D6RIG18             1276   1350
020200091005     D  D6RIG19             1351   1425
020300091019     D*---
020400091005     D* Costante per reperimento messaggi
020500091019     D TxtIdmsg        C                   CONST('XXXXXX    *LIBL-
020600091005     D                                          xxxxxxx')
020700091019     D*----
020800091019     D* Campo di lavoro per messaggi
020900091019     D WrkIdMsg                      27                                         Messaggi
021000091019     D*----
021100091019     D WIdMsg          DS                                                       Messaggi
021200091019     D  IDMSGF                 1     10
021300091019     D  IDCdMsg               21     22
021400091019     D
021500091005     C*----------------------------------------------------*           ***
021600930607     C/EJECT
021700930607     C*----------------------------------------------------*           ***
021800091019     C*-                                                   *
021900091019     C                   Exsr      InzVar                                       inizializza
022000930607     C*-                                                   *
022100091019     C     MsgStp        IFEQ      'S'                                          *
022200930608     C     OPEPRT        IFNE      'SI'                                         *
022300091005     C                   OPEN      SDGMSGP                                      *
022400930608     C                   MOVE      'SI'          OPEPRT            2            *
022500001019     C                   endif                                                  *
022600091019     C                   Exsr      StpErrori                                    *
022700001019     C                   endif                                                  *
022800930607     C*                                                    *
022900001025     C* Gestione messaggi coda operatore                   *
023000091005     C*                                                    *
023100091005     C     MSGOPE        IFEQ      'S'                                          *
023200091019     C     MsgSnd        ifne      *BLANKS
023300091005     C     WmsgSend      orne      *BLANKS
023400091005     C                   CLEAR                   MSO
023500091005     C                   MOVEL     CMD(3)        CMD2                           *
023600091005     C                   MOVE      CMD(4)        CMD2                           *
023700091019     c                   if        MsgSnd <> *blanks
023800091019     C                   MoveL     MsgSnd        MSG2                           *
023900091019     C                   MoveL     MsgSnd        MSO(1)
024000091005     c                   else
024100091005     C                   MOVEL     WmsgSend      MSG2                           *
024200091005     C                   MOVEL     WmsgSend      MSO(1)
024300091005     c                   endif
024400930607     C                   MOVE      CMD2          CMDX                           *
024500930830     C                   MOVE      *BLANK        QDU                            *
024600930608     C                   MOVEL     'QSYSOPR'     QDU
024700930608     C                   MOVE      ' '           RTN
024800930608     C                   MOVE      ' '           RSP
024900091019     C                   EXSR      InvioMsg                                     *
025000091005     c                   endif
025100091005     C                   ENDIF                                                  *
025200091005     C*-
025300091005     C* Gestione messaggi coda display o coda utente
025400091019     C     MsgVid        IfEq      'S'                                          *
025500091019     C     MsgSnd        ifne      *BLANKS
025600091005     C     WmsgSend      orne      *BLANKS
025700091005     C                   CLEAR                   MSO
025800930607     C                   MOVEL     CMD(3)        CMD2                           *
025900091005     C                   MOVE      CMD(4)        CMD2                           *
026000091019     c                   if        MsgSnd <> *blanks
026100091019     C                   MOVEL     MsgSnd        MSG2                           *
026200091019     C                   MOVEL     MsgSnd        MSO(1)
026300091005     c                   else
026400091005     C                   MOVEL     WmsgSend      MSG2                           *
026500091005     C                   MOVEL     WmsgSend      MSO(1)
026600091005     c                   endif
026700091005     C                   MOVEA     JBA(20)       MSQ2                           *
026800091005     C                   MOVE      CMD2          CMDX                           *
026900930608     C                   MOVE      ' '           RTN
027000930608     C                   MOVE      ' '           RSP
027100930830     C                   MOVE      *BLANK        QDU                            *
027200930608     C     TIPO          IFEQ      '1'
027300930608     C                   MOVEA     JBA(20)       QDU
027400930608     C                   ELSE
027500930608     C                   MOVEA     JBA(1)        QDU
027600001019     C                   endif
027700930608     C     QDU           IFNE      *BLANKS
027800091019     C                   EXSR      InvioMsg                                     *
027900001019     C                   endif                                                  *
028000930607     C*-                                                   *
028100091005     C                   endif                                                  *
028200001019     C                   ENDIF                                                  *
028300930607     C*-                                                   *
028400930607     C* DUE STRADE IN ALTERNATIVA :                        *
028500930607     C* 1) LAVORO INTERATTIVO                              *
028600930607     C*-                                                   *
028700930608     C     TIPO          IFEQ      '1'                                          * 1 IF
028800930608     C     MSGEMV        ANDEQ     'S'                                          *
028900930607     C*-                                                   *
029000930607     C     OPEVID        IFNE      'SI'                                         *
029100091005     C                   OPEN      SDGMSGD                                      *
029200930607     C                   MOVE      'SI'          OPEVID            2            *
029300001019     C                   endif                                                  *
029400930607     C*-                                                   *
029500001025     C* Gestione messaggi a video
029600930607     C*-                                                   *
029700001023     C                   EXSR      GesD1                                        *
029800930607     C*-                                                   *
029900930608     C                   ELSE                                                   * 1 ELSE
030000930607     C*-                                                   *
030100930608     C* 2) LAVORO BATCH O INTERATTIVO CON 'N' EMV     .    *
030200930607     C*-                                                   *
030300930608     C* Invio messaggio operatore con analisi problema     *
030400930608     C* se richiesto             (FLAG MSGLCK = 'S')       *
030500091019     C* e se passato status      (STWDS <> BLANK   )       *
030600930921     C*                                                    *
030700930608     C     MSGLCK        IFEQ      'S'                                          * 2 IF
030800091019     C     STWDS         ANDNE     *BLANK                                       *
030900930607     C*-                                                   *
031000091019     C                   MOVEL     STWMS1        MSG4                           *
031100091019     C                   MOVE      STWMS2        MSG4                           *
031200930608     C                   MOVEL     CMD(3)        CMD4                           *
031300930608     C                   MOVE      CMD(4)        CMD4                           *
031400930607     C                   MOVEL     CMD4          CMDX                           *
031500930921     C                   CLEAR                   MSO                            *
031600091019     C                   MOVEL     STWMS1        MSO(1)                         *
031700091019     C                   MOVE      STWMS2        MSO(2)                         *
031800930830     C                   MOVE      *BLANK        QDU                            *
031900930921     C                   MOVEL     'QSYSOPR'     QDU                            *
032000930921     C                   MOVE      ' '           RTN                            *
032100930608     C                   MOVE      ' '           RSP
032200091019     C                   EXSR      InvioMsg                                     *
032300930607     C*-                                                   *
032400001019     C                   ENDIF                                                  * 2 END
032500930607     C*-                                                   *
032600930608     C* GESTIONE INVII MESSAGGI
032700930608     C* Invio messaggio operatore con testi messaggi se    *
032800930608     C* passati e                                          *
032900930608     C* e richiesta risposta se prevista                   *
033000930608     C*
033100091005     C     W1MSG         IFNE      *BLANK                                       * 2 IF
033200091005     C     MSGRSP        ANDEQ     'S'                                          *
033300091005     C     W1MS1         ORNE      *BLANK                                       *
033400091005     C     MSGRSP        ANDEQ     'S'                                          *
033500091005     C     W1MS2         ORNE      *BLANK                                       *
033600091005     C     MSGRSP        ANDEQ     'S'                                          *
033700091005     C     W1MS3         ORNE      *BLANK                                       *
033800091005     C     MSGRSP        ANDEQ     'S'                                          *
033900091005     C*-                                                   *
034000091005     C                   Z-ADD     1             X                              *
034100930607     C                   CLEAR                   MSO                            *
034200930607     C*-                                                   *
034300091005     C     W1MSG         IFNE      *BLANK                                       *
034400091005     C                   MOVEL     W1MSG         MSO(X)                         *
034500091005     C                   ADD       1             X                              *
034600091005     C                   ENDIF                                                  *
034700091005     C*-                                                   *
034800091005     C     W1MS1         IFNE      *BLANK                                       *
034900091005     C                   MOVEL     W1MS1         MSO(X)                         *
035000091005     C                   ADD       1             X                              *
035100091005     C                   ENDIF                                                  *
035200091005     C*-                                                   *
035300091005     C     W1MS2         IFNE      *BLANK                                       *
035400091005     C                   MOVEL     W1MS2         MSO(X)                         *
035500091005     C                   ADD       1             X                              *
035600091005     C                   ENDIF                                                  *
035700091005     C*-
035800091005     C     W1MS3         IFNE      *BLANK                                       *
035900091005     C                   MOVEL     W1MS3         MSO(X)                         *
036000091005     C                   ADD       1             X                              *
036100091005     C                   ENDIF                                                  *
036200091005     C*-
036300001025     C* Segnalo le risposte possibili se richiesta risposta
036400001025     C*-
036500930607     C     MSGRSP        IFEQ      'S'                                          * 3 IF
036600001025     C*- segnalo oggetto allocato solo se presente
036700091006     C     MSGFIL        IfNe      *blanks                                      * 3 IF
036800091019     C                   MoveL     CST(3)        IdMsg
036900091005     c                   z-add     130           LenMsg
037000091005     c                   clear                   MsgDta
037100091005     c                   exsr      PrendiMsg
037200091005     c     WWMSG         cat       MSGFIL:1      WWMSG
037300091005     C                   MOVEL     WWmsg         MSO(X)                         *
037400091005     C                   ADD       1             X
037500091005     c                   endif
037600091019     C                   MoveL     CST(1)        IdMsg
037700091005     c                   z-add     130           LenMsg
037800091005     c                   clear                   MsgDta
037900091005     c                   exsr      PrendiMsg
038000091005     C                   MOVEL     WWMSG         MSO(X)                         *
038100091005     C                   MOVE      D1TPR         MSO(X)                         *
038200091005     C*-                                                   *
038300091005     C                   ENDIF                                                  * 3 END
038400091005     C*-                                                   *
038500091005     C     RIEMET        TAG
038600091005     C                   MOVE      MSGRSP        RSP
038700091005     C                   MOVE      *BLANK        RTN                            *
038800930830     C                   MOVE      *BLANK        QDU                            *
038900930608     C                   MOVEL     'QSYSOPR'     QDU
039000930607     C                   CALL      'XMSGC'                                      *
039100961023     C                   PARM                    MSO                            *
039200930607     C                   PARM                    RSP               1            *
039300930607     C                   PARM                    RTN               1            *
039400930608     C                   PARM                    QDU              10            *
039500930608     C*-                                                   *
039600930608     C* Controllo risposta: se non esiste nelle riposte possibili
039700930608     C* riemetto messaggio
039800930608     C     RTN           IFNE      ' '
039900961023     C* alcune lettere minuscole vengono trasformate in maiuscole
040000961023     c                   select
040100961023     c                   when      rtn = 'c'
040200961023     c                   eval      RTN = 'C'
040300961023     c                   when      rtn = 'i'
040400961023     c                   eval      RTN = 'I'
040500961023     c                   when      rtn = 'g'
040600961023     c                   eval      RTN = 'G'
040700961023     c                   when      rtn = 'r'
040800961023     c                   eval      RTN = 'R'
040900961023     c                   endsl
041000930608     C     RTN           LOOKUP    TPR                                    13    *
041100930608     C     *IN13         IFNE      '1'
041200091019     C                   MoveL     CST(2)        IdMsg
041300091005     c                   z-add     130           LenMsg
041400091005     c                   clear                   MsgDta
041500091005     c                   exsr      PrendiMsg
041600091005     C                   MOVEL     WWMSG         MSO(X)                         *
041700091005     C                   MOVE      D1TPR         MSO(X)                         *
041800091005     C                   GOTO      RIEMET
041900091005     C                   ENDIF
042000091005     C                   ENDIF
042100091005     C*-                                                   *
042200091005     C                   MOVE      RTN           MSGRTN                         *
042300930607     C*-                                                   *
042400001019     C                   ENDIF                                                  * 2 END
042500001019     C                   ENDIF                                                  * 1 END
042600930607     C*-                                                   *
042700930607     C                   SETON                                        RT        *
042800930607     C*-                                                   *
042900930607     C*----------------------------------------------------*           ***
043000930607     C/EJECT
043100930607     C*----------------------------------------------------*           ***
043200930607     C* Definizioni e inizializzazioni                     *
043300930607     C*                                                    *
043400950728     C     INZVAR        BEGSR                                                  *
043500930607     C*                                                    *
043600930607     C     *ENTRY        PLIST                                                  riceve
043700091005     C                   PARM                    SDGMSGDS                       parametro
043800010618     c* testi (lunghi 15 ciascuno) dei 9 bottoni
043900950728     C                   PARM                    MSGTXT          135
044000010618     c* inizializzazione o meno
044100951030     C                   PARM                    MSGINZ            1
044200010618     c* codici dei 3 messaggi possibili da emettere a video
044300091005     C                   PARM                    MSGCd1            7
044400091005     C                   PARM                    MSGCd2            7
044500091005     C                   PARM                    MSGCd3            7
044600091005     c* codice del messaggio da mandare alla coda dell'operatore
044700091005     C                   PARM                    MSGSn1            7
044800091005     c* codici(lunghi 7  ciascuno) dei 9 bottoni
044900091005     C                   PARM                    MSGCdTxt         63
045000091005     c* codici(lunghi 7  ciascuno) dei 2 titoli (titolo e sottotitolo)
045100091005     c* per stampa e video
045200091005     C                   PARM                    MSGCdTit         14
045300010618     c* flag per chiusura prtf
045400091005     C                   PARM                    MSGClPrtf         1
045500091005     C* variabili per messaggi
045600091005     C                   PARM                    VarMsg1         100
045700091005     C                   PARM                    VarMsg2         100
045800091005     C                   PARM                    VarMsg3         100
045900091005     c*
046000091005     C     PARMS         IFLT      2
046100091005     C                   MOVE      *BLANKS       TXT
046200091005     C                   ELSE
046300091005     C                   MOVEA     MSGTXT        TXT
046400950728     C                   ENDIF
046500951030     C*
046600951030     C     PARMS         IFLT      3
046700990202     C                   MOVE      *On           INZ               1
046800951030     C                   ELSE
046900951030     C                   MOVE      MSGINZ        INZ
047000951030     C                   ENDIF
047100971125      *                                                    *
047200971125      * Sostituisco apostrofi nella stringa passata con "
047300091019     C                   If        MsgSnd <> *blank
047400091019     C     '''':'"'      XLate     MsgSnd        MsgSnd
047500971125     C                   Endif
047600971125     C
047700950726     C* SE APERTO, CHIUDO VIDEO (PER NON FAR RIAPPARIRE VECCHIO
047800951030     C*- SFONDO DA PGM CHIAMANTE) e se richiesto da pgm chiamante
047900951030     C* perchè se chiamato due volte consecutivamente, perde lo sfondo il
048000951030     c* pgm chiamante
048100950726     C     OPEVID        IFEQ      'SI'
048200951030     C     INZ           ANDEQ     *on
048300091005     C                   CLOSE     SDGMSGD
048400950726     C                   CLEAR                   OPEVID
048500950726     C                   ENDIF
048600001025     C*-
048700001025     C* SOLO LA PRIMA VOLTA : VERIFICO IL TIPO DI LAVORO
048800001025     C*-
048900930607     C     PRIMO         IFNE      'NO'                                         *
049000930607     C                   EXSR      CTRJOB                                       *
049100930607     C                   MOVE      'NO'          PRIMO             2            *
049200001019     C                   endif                                                  *
049300001025     C*-
049400001025     C* Inizializza campi video/stampa
049500001025     C*-
049600950728     C                   MOVE      *BLANKS       D1RTN             1            *
049700930921     C                   Z-ADD     MSGMSG        $X                3 0          *
049800091005     C                   CLEAR                   W1msg
049900091005     C                   CLEAR                   W1ms1
050000091005     C                   CLEAR                   W1ms2
050100091005     C                   CLEAR                   W1ms3
050200091005     C                   CLEAR                   WmsgSend
050300091005     C                   CLEAR                   WmsgS3000
050400091005     C                   CLEAR                   W1msg3000
050500091005     C                   CLEAR                   W1ms13000
050600091005     C                   CLEAR                   W1ms23000
050700091005     C                   CLEAR                   W1ms33000
050800091005     C                   CLEAR                   W1cd13000
050900091005     C                   CLEAR                   W1cd23000
051000091005     C                   CLEAR                   W1cd33000
051100091005     C                   CLEAR                   W1cd43000
051200091005     C                   CLEAR                   W1cd53000
051300091005     C                   CLEAR                   W1cd63000
051400091005     C                   CLEAR                   W1cd73000
051500091005     C                   CLEAR                   W1cd83000
051600091005     C                   CLEAR                   W1cd93000
051700091005     C                   CLEAR                   d1msg
051800951117     C                   CLEAR                   d1ms1
051900951117     C                   CLEAR                   d1ms2
052000951117     C                   CLEAR                   d1ms3
052100091005     C                   CLEAR                   d1ms4
052200091005     C                   CLEAR                   d1ms5
052300091005     C                   CLEAR                   d1ms6
052400091005     C                   CLEAR                   d1ms7
052500091005     C                   CLEAR                   d1fil
052600091005     C                   CLEAR                   d1sce
052700091005     c                   setoff                                           12
052800091005     c                   setoff                                       141516
052900091005     c                   setoff                                       171819
053000091005     c                   setoff                                       414243
053100960319     c                   setoff                                       444546
053200960319     c                   setoff                                       474849
053300001109     c*
053400091019     C                   IF        MsgMsF =  *blanks                            *
053500091020     C                   MoveL     'PROMSG'      MsgMsf                         *
053600091019     C                   EndiF                                                  *
053700091019     C*-
053800091019     C                   MoveL     TxtIdmsg      WrkIdmsg                       *
053900091019     C                   MoveL     MsgMsF        WrkIdmsg                       *
054000091006     c*
054100001109     c* deve essere impostato per tutti i casi di invio di messaggio
054200091005     C                   MOVEL     MSGFIL        D1FIL                          *
054300091005     C*-
054400091005     c                   Select
054500091005     C* se passati uno dei 3 vecchi campi, considero quelli
054600091005     C                   When      MsgMs1 <> *blanks or
054700091005     C                             MsgMs2 <> *blanks or
054800091005     C                             MsgMs3 <> *blanks
054900091005     C*-
055000091005     c* nella prima riga ci va il messaggio precodificato
055100091005     c* se c'è, altrimenti blank
055200091005     C     MSGMSG        IFNE      0                                            *
055300091005     C                   MoveL     Msg($X)       IdMsg
055400091005     c                   z-add     130           LenMsg
055500091005     c                   clear                   MsgDta
055600091005     c                   exsr      PrendiMsg
055700091005     C                   MOVEL     WWMSG         W1MSG                          operazione richiesta
055800091005     C                   MOVEL     WWMSG3000     W1MSG3000                      operazione richiesta
055900091005     C                   endif                                                  *
056000091005     C                   MOVEL     MsgMS1        W1MS1                          operazione richiesta
056100091005     C                   MOVEL     MsgMS2        w1MS2                          operazione richiesta
056200091005     C                   MOVEL     MsgMS3        W1MS3                          operazione richiesta
056300091005     c*
056400091005     C* se passati uno dei 3 nuovi campi, considero quelli
056500091005     C                   When      %addr(MsgCd1) <> *null   or
056600091005     C                             %addr(msgCd2) <> *null   or
056700091005     C                             %addr(msgCd3) <> *null
056800091005     c* nella prima riga ci va sempre il messaggio precodificato se
056900091005     c* presente, altrimenti blank
057000091005     C     MSGMSG        IFNE      0                                            *
057100091005     C                   MoveL     Msg($X)       IdMsg
057200091005     c                   z-add     130           LenMsg
057300091005     c                   clear                   MsgDta
057400091005     c                   exsr      PrendiMsg
057500091005     C                   MOVEL     WWMSG         W1MSG                          operazione richiesta
057600091005     C                   MOVEL     WWMSG3000     W1MSG3000                      operazione richiesta
057700091005     C                   endif                                                  *
057800091005     c* primo messaggio
057900091005     C                   if        %addr(MsgCd1) <> *null
058000091005     c                   if        MsgCd1 <> *blanks
058100091019     c                   eval      Idmsg=WrkIdmsg
058200091005     C                   Move      MsgCd1        IdMsg
058300091005     c                   z-add     130           LenMsg
058400091005     c                   clear                   MsgDta
058500091005     c                   if        %addr(Varmsg1) <> *null
058600091005     c                   move      Varmsg1       MsgDta
058700091005     c                   endif
058800091005     c                   exsr      PrendiMsg
058900091005     C                   MOVEL     WWMSG         W1MS1                          operazione richiesta
059000091005     C                   MOVEL     WWMSG3000     W1MS13000                      operazione richiesta
059100091005     C                   endif                                                  *
059200091005     C                   endif                                                  *
059300091005     c* secondo messaggio
059400091005     C                   if        %addr(MsgCd2) <> *null
059500091005     c                   if        MsgCd2 <> *blanks
059600091019     c                   eval      Idmsg=WrkIdmsg
059700091005     C                   Move      MsgCd2        IdMsg
059800091005     c                   z-add     130           LenMsg
059900091005     c                   clear                   MsgDta
060000091005     c                   if        %addr(Varmsg2) <> *null
060100091005     c                   move      Varmsg2       MsgDta
060200091005     c                   endif
060300091005     c                   exsr      PrendiMsg
060400091005     C                   MOVEL     WWMSG         W1MS2                          operazione richiesta
060500091005     C                   MOVEL     WWMSG3000     W1MS23000                      operazione richiesta
060600091005     C                   endif                                                  *
060700091005     C                   endif                                                  *
060800091005     c* terzo messaggio
060900091005     C                   if        %addr(MsgCd3) <> *null
061000091005     c                   if        MsgCd3 <> *blanks
061100091019     c                   eval      Idmsg=WrkIdmsg
061200091005     C                   Move      MsgCd3        IdMsg
061300091005     c                   z-add     130           LenMsg
061400091005     c                   clear                   MsgDta
061500091005     c                   if        %addr(Varmsg3) <> *null
061600091005     c                   move      Varmsg3       MsgDta
061700091005     c                   endif
061800091005     c                   exsr      PrendiMsg
061900091005     C                   MOVEL     WWMSG         W1MS3                          operazione richiesta
062000091005     C                   MOVEL     WWMSG3000     W1MS33000                      operazione richiesta
062100091005     C                   endif                                                  *
062200091005     C                   endif                                                  *
062300091005     C*-                                                   *
062400091005     c                   other
062500091005     c* altrimenti considero solo il messaggio precodificato se
062600091005     c* presente
062700091005     c* e lo metto nella seconda riga (stacco dal titolo)
062800091005     C     MSGMSG        IFNE      0                                            *
062900091005     C                   MoveL     Msg($X)       IdMsg
063000091005     c                   z-add     130           LenMsg
063100091005     c                   clear                   MsgDta
063200091005     c                   exsr      PrendiMsg
063300091005     C                   MOVEL     WWMSG         W1MS1                          operazione richiesta
063400091005     C                   MOVEL     WWMSG3000     W1MS13000                      operazione richiesta
063500001019     C                   endif                                                  *
063600930921     C*-                                                   *
063700001019     C                   endsl                                                  *
063800001025     c* Avvicino i tre messaggi, altrimenti si rishia di emettere segue con
063900001025     c* i messaggi solo nella seconda pagina e non nella prima
064000001025     c                   if        W1ms1 = *blanks and W1ms2 <> *blanks
064100001025     c                   eval      W1ms1 = W1ms2
064200001025     c                   eval      W1ms13000 = W1ms23000
064300001025     c                   clear                   W1ms2
064400001025     c                   clear                   W1ms23000
064500001025     c                   endif
064600001025     c                   if        W1ms2 = *blanks and W1ms3 <> *blanks
064700001025     c                   eval      W1ms2 = W1ms3
064800001025     c                   eval      W1ms23000 = W1ms33000
064900001025     c                   clear                   W1ms3
065000001025     c                   clear                   W1ms33000
065100001025     c                   endif
065200001025     c*
065300091005     c* Reperisco testo messaggio da mandare coda operatore o sistema
065400091019     c* da utilizzare al posto di MsgSnd
065500091005     C                   if        %addr(MsgSn1) <> *null
065600091005     c                   if        MsgSn1 <> *blanks
065700091019     c                   eval      Idmsg=WrkIdmsg
065800091005     C                   Move      MsgSn1        IdMsg
065900091005     c                   z-add     130           LenMsg
066000091005     c                   clear                   MsgDta
066100091005     c                   exsr      PrendiMsg
066200091005     C                   MOVEL     WWMSG         WMsgSend
066300091005     C                   MOVEL     WWMSG3000     WMsgS3000
066400091005      * Sostituisco apostrofi nella stringa trovata con "
066500091005     C                   If        WMsgSend <> *blank
066600091005     C     '''':'"'      XLate     WMsgSend      WmsgSend
066700091005     C                   Endif
066800091005     C                   endif
066900091005     C                   endif
067000091005     C*-                                                   *
067100091005     C* Imposta risposte possibili con spazio tra una e l'altra
067200091005     C*-                                                   *
067300001030     C                   MOVEA     MSGTPR        TPR
067400960319     c*
067500001030     C                   Z-ADD     1             X                 3 0
067600001030     C     MSGRSP        IFEQ      'S'
067700930607     C*-                                                   *
067800001030     C                   DO        9             $X                3 0
067900001030     C                   MOVE      TPR($X)       DTPR(X)
068000001030     C                   ADD       1             X
068100001030     C                   MOVE      ' '           DTPR(X)
068200001030     C                   ADD       1             X
068300001030     C                   enddo
068400001030     C*-
068500001030     C                   SETON                                        17
068600001030     C*-
068700971010     C* Reperisco testi risposte di default
068800091019     C                   ExSr      DefTesti
068900001102     c*
069000001102     C                   MOVEA     *OFF          *IN(40)
069100001030     C*-
069200001030     C* Reperisco testi dei 9 messaggi se passati i codici
069300091005     C                   If        %addr(MsgCdTxt)> *null
069400091005     C                   If        MsgCdTxt <> *blanks
069500091005     c                   eval      W1cod = MsgCdTxt
069600091019     c                   exsr      CodTesti
069700091005     c                   endif
069800091005     c                   endif
069900091005     C*-
070000091019     C* Imposta txt risposte
070100091005     C*-
070200091005     C                   DO        9             $X                3 0          *
070300950728     C     TPR($X)       IFNE      *BLANKS
070400950728     C     $X            ADD       40            $Y                3 0
070500950728     C                   MOVE      *ON           *IN($Y)
070600950728     C*imposta valori default risposte se non passati
070700950728     C                   SELECT
070800950728     C     TPR($X)       WHENEQ    'S'
070900950728     C     TXT($X)       ANDEQ     *BLANKS
071000971010     C                   MOVEL     TxtSiNo       TXT($X)
071100950728     C     TPR($X)       WHENEQ    'N'
071200950728     C     TXT($X)       ANDEQ     *BLANKS
071300971010     C                   MOVE      TxtSiNo       TXT($X)
071400950728     C     TPR($X)       WHENEQ    'C'
071500950728     C     TXT($X)       ANDEQ     *BLANKS
071600971010     C                   MOVEL     TxtCanRip     TXT($X)
071700950728     C     TPR($X)       WHENEQ    'R'
071800950728     C     TXT($X)       ANDEQ     *BLANKS
071900971010     C                   MOVE      TxtCanRip     TXT($X)
072000950728     C                   ENDSL
072100950728     c*
072200950728     C     TXT($X)       IFEQ      *BLANKS
072300950728     C                   MOVE      *ALL'?'       TXT($X)
072400950728     C                   endif
072500950728     C*
072600950728     C                   ENDIF                                                  *
072700950728     C                   ENDDO                                                  *
072800930607     C                   END                                                    *
072900001030     C*-
073000091019     C* Muovo STW in una DS per scomporre  campi
073100091019     C                   MOVEA     STW           STWDS                          *
073200001030     C*-
073300001030     C* ESEGUO QUESTE OPERAZIONI SOLO SE LAVORO INTERATTIVO
073400001030     C*-
073500930607     C     TIPO          IFEQ      '1'                                          *
073600001030     C*-
073700001030     C* Oggetto allocato
073800001030     C*-
073900930607     C     MSGFIL        IFNE      *BLANKS                                      *
074000930607     C                   SETON                                        18        *
074100930607     C                   MOVEL     MSGFIL        D1FIL                          *
074200001019     C                   endif                                                  *
074300001030     C*-
074400001030     C* Controllo sulla riga di partenza finestra
074500001030     C*-
074600930607     C     MSGEMV        IFEQ      'S'                                          *
074700930607     C*-                                                   *
074800930607     C                   Z-ADD     MSGRGP        D1RIG                          *
074900930607     C*-                                                   *
075000930607     C     D1RIG         IFGT      13                                           posizionamento rice-
075100930607     C                   Z-ADD     13            D1RIG             3 0          vuta:1 < x > 19
075200001019     C                   endif                                                  *
075300930607     C     D1RIG         IFLE      0                                            *
075400930607     C                   Z-ADD     8             D1RIG                          *
075500001019     C                   endif                                                  *
075600930607     C*-                                                   *
075700930607     C     D1RIG         ADD       2             D2RIG             3 0          *
075800930607     C     D2RIG         ADD       1             D3RIG             3 0          *
075900930607     C*-                                                   *
076000001019     C                   endif                                                  *
076100930607     C*-                                                   *
076200930607     C* Imposta KPJBA nella ds per stampa                  *
076300930607     C*-                                                   *
076400091019     C     MsgStp        IFEQ      'S'                                          *
076500930607     C                   MOVEA     JBA           PRTJBA                         *
076600001019     C                   endif                                                  *
076700930607     C*                                                    *
076800930607     C* Abilita tasto F2 per gestione messaggi             *
076900091019     C* solo se STW e' stato passato                       *
077000930607     C*                                                    *
077100930607     C     MSGLCK        IFEQ      'S'                                          *
077200930607     C     MSGEMV        ANDEQ     'S'                                          *
077300091019     C     STWDS         ANDNE     *BLANK                                       *
077400930607     C                   SETON                                        19        *
077500001019     C                   endif                                                  *
077600930607     C*-                                                   *
077700010118     C                   END                                                    *
077800010118     c*
077900091019     C* reperisco titoli di stampa se non passato titolo, uso costante
078000091005     C                   if        %addr(MsgCdTit) <> *null
078100091005     c                   movel     MsgCdTit      Titolo            7
078200091005     c                   move      MsgCdTit      Testata           7
078300091005     C                   if        titolo = *blanks
078400091019     c*
078500091019     C* caso elaborazione batch
078600091005     c                   if        msgemv <> 'S'
078700091019     c                   eval      titolo = 'SDC0028'
078800091005     c                   else
078900091019     c                   eval      titolo = 'SDC0029'
079000091005     c                   endif
079100091005     c                   endif
079200091005     c*
079300091005     c                   else
079400091005     c*
079500091005     c                   if        msgemv <> 'S'
079600091019     c                   eval      titolo = 'SDC0028'
079700091019     c                   else
079800091019     c                   eval      titolo = 'SDC0029'
079900091005     c                   endif
080000091005     c*
080100091005     c                   endif
080200091005     C*
080300091005     c* se il titolo in entrata è diverso da quello salvato, si tratta
080400091005     c* di un cambio di stampa nello stesso job, quindi cambio valore
080500091005     c* del flag di stampa prima pagina
080600091005     c* salvo primo titolo in entrata
080700091005     c                   if        Titolo <> SavCdTit
080800091005     c                   eval      PrimaVolta = *off
080900091005     c                   eval      SavCdTit = Titolo
081000091019     c                   eval      Idmsg=WrkIdmsg
081100091005     C                   Move      Titolo        IdMsg
081200091005     C* è sempre lungo 70
081300091005     c                   z-add     70            LenMsg
081400091005     c                   eval      WctrMsg = *on
081500091005     c                   clear                   MsgDta
081600091005     c                   exsr      PrendiMsg
081700091005     c                   eval      WctrMsg = *off
081800091005     C                   MOVEL     WWMSG         T1Titolo                       operazione richiesta
081900091005     C                   MOVEL     WWMSG         D1Titolo                       operazione richiesta
082000091005     c                   endif
082100091005     c*
082200091005     c* testate campi
082300091005     c                   if        Testata  <> *blanks and
082400091005     c                             Testata  <> SavCdTes
082500091005     c                   eval      SavCdTes = Testata
082600091019     c                   eval      Idmsg=WrkIdmsg
082700091005     C                   Move      Testata       IdMsg
082800091005     c                   if        MsgEmv <>'S'
082900091005     c                   z-add     130           LenMsg
083000091005     c                   else
083100091005     c                   z-add     70            LenMsg
083200091005     c* lo centro solo se interattivo
083300091005     c                   eval      WctrMsg = *on
083400091005     c                   endif
083500091005     c                   clear                   MsgDta
083600091005     c                   exsr      PrendiMsg
083700091005     c                   eval      WctrMsg = *off
083800091005     C                   MOVEL     WWMSG         T1Testata                      operazione richiesta
083900091005     C                   MOVEL     WWMSG         D1Testata                      operazione richiesta
084000091019     C                   EndIf                                                  *
084100091005     C*                                                    *
084200091019     C                   EndSr                                                  *
084300091005     C*----------------------------------------------------*           ***
084400091005      /EJECT
084500091005     C*----------------------------------------------------*           ***
084600091019     C* Emissione windows                                  *
084700091019     C*----------------------------------------------------*           ***
084800001023     C     GesD1         BEGSR                                                  *
084900001023     C*                                                    *
085000091005     c* valorizzo messaggi a video
085100091005     c                   exsr      Ried1
085200091005     C*                                                    *
085300091005     C     D1EXF         TAG                                                    *
085400091005     c*
085500091005     c* se esiste almeno un messaggio di secondo livello, emetto
085600091005     c* messaggio avvertimento
085700091005     c                   if        w1msg3000 <> *blanks or
085800091005     c                             w1ms13000 <> *blanks or
085900091005     c                             w1ms23000 <> *blanks or
086000091005     c                             w1ms33000 <> *blanks or
086100091005     c                             w1cd13000 <> *blanks or
086200091005     c                             w1cd23000 <> *blanks or
086300091005     c                             w1cd33000 <> *blanks or
086400091005     c                             w1cd43000 <> *blanks or
086500091005     c                             w1cd53000 <> *blanks or
086600091005     c                             w1cd63000 <> *blanks or
086700091005     c                             w1cd73000 <> *blanks or
086800091005     c                             w1cd83000 <> *blanks or
086900091005     c                             w1cd93000 <> *blanks
087000091005     c*devo eseguire write per l'indicatore 14
087100091005     C                   write     SMSGD1                                       *
087200091005     c                   seton                                        71
087300091005     c*devo eseguire write per l'indicatore 71
087400091005     C                   write     SMSGD1                                       *
087500091005     c                   endif
087600091005     C                   EXFMT     SMSGD1                                       *
087700091005     C*                                                    *
087800091005     C* Tasto F2 gestione analisi problema                 *
087900091005     C     *INKB         IFEQ      '1'                                          *
088000091019     C                   EXSR      Analizz                                      *
088100930618     C                   GOTO      D1EXF
088200001019     C                   ENDIF                                                  *
088300091005     C*                                                    *
088400091005     C* Tasto rollup                                       *
088500091005     C     *IN16         IFEQ      '1'                                          *
088600091005     c* faccio vedere gli altri messaggi
088700091005     C                   seton                                        14        *
088800091005     C                   setoff                                       15        *
088900091005     C                   GOTO      D1EXF
089000091005     C                   ENDIF                                                  *
089100091005     C*                                                    *
089200091005     C* Tasto roldown                                      *
089300091005     C     *IN12         IFEQ      '1'                                          *
089400091005     c* faccio vedere gli altri messaggi
089500091005     C                   setoff                                       14        *
089600091005     C                   seton                                        15        *
089700091005     C                   GOTO      D1EXF
089800091005     C                   ENDIF                                                  *
089900091005     C*                                                    *
090000091005     C* Tasto F4 per messaggio di secondo livello          *
090100091005     C     *INKA         IFEQ      '1'                                          *
090200091005     c                   exsr      GesD6
090300091005     c                   endif
090400091005     C*                                                    *
090500091005     C* Controllo risposta                                 *
090600091005     C     MSGRSP        IFEQ      'S'                                          *
090700091005     C*                                                    *
090800091005     C     D1SCE         IFNE      0
090900091005     C                   Z-ADD     D1SCE         $X
091000091005     C                   MOVE      TPR($X)       D1RTN
091100950728     C                   endif
091200950728     C*                                                    *
091300950728     C     D1RTN         IFEQ      ' '                                          *
091400950728     C                   SETON                                        70        *
091500950728     C                   GOTO      D1EXF                                        *
091600001019     C                   ENDIF                                                  *
091700930607     C*                                                    *
091800930607     C                   MOVE      D1RTN         MSGRTN                         *
091900950726     C* Solo per record allocato
092000950726     C     MSGLCK        IFEQ      'S'
092100091005     C                   WRITE     SMSGD4                                       *
092200950726     C                   ENDIF
092300930607     C*                                                    *
092400001019     C                   ENDIF                                                  *
092500950727     C* Rimuovo finestra                                   *
092600091005     C                   WRITE     SMSGD5                                       *
092700950727     C*                                                    *
092800091019     C                   EndSr                                                  *
092900001023     C*----------------------------------------------------*           ***
093000001023      /EJECT
093100001023     C*----------------------------------------------------*           ***
093200091019     C* Riempimento msg a video. in caso di lenght > 70
093300091019     c* utilizzo la riga successiva
093400001023     C*----------------------------------------------------*           ***
093500091005     C     Ried1         BEGSR                                                  *
093600001023     c*
093700001023     C* 1° messaggio
093800001025     c* se presente solo questo messaggio(quello precodificato) lo metto
093900001025     c* nella terza riga per rispettare quello che faceva il programma
094000001025     c* prima, altrimenti nella prima riga
094100001023     c                   if        W1msg <> *blanks
094200001025     c                   if        W1ms1 = *blanks and W1ms2 = *blanks
094300001025     c                             and w1ms3 = *blanks
094400001025     c                   movel     W1msg         d1ms2
094500001025     c     msgrgp        add       4             h1Rig
094600001025     c* se + lungo di 70, il resto lo metto sul quarto  campo
094700001025     c                   if        %subst(w1msg:71) <> *blanks
094800001025     c                   eval      d1ms3 = %subst(w1msg:71)
094900001025     c                   endif
095000001025     c                   else
095100001023     c                   movel     W1msg         d1msg
095200001025     c     msgrgp        add       2             h1Rig
095300001023     c* se + lungo di 70, il resto lo metto sul secondo campo
095400001023     c                   if        %subst(w1msg:71) <> *blanks
095500001023     c                   eval      d1ms1 = %subst(w1msg:71)
095600001023     c                   endif
095700001025     c                   endif
095800001023     c                   endif
095900001023     C* 2° messaggio
096000001023     c                   if        W1Ms1 <> *blanks
096100001023     c                   select
096200001023     c                   when      d1ms1 = *blanks
096300001023     c                   movel     W1ms1         d1ms1
096400001025     c     msgrgp        add       3             h1Rig1
096500001023     c* se + lungo di 70, il resto lo metto sul terzo campo
096600001023     c                   if        %subst(w1ms1:71) <> *blanks
096700001023     c                   eval      d1ms2 = %subst(w1ms1:71)
096800001023     c                   endif
096900001023     c                   other
097000001023     c                   movel     W1ms1         d1ms2
097100001025     c     msgrgp        add       4             h1Rig1
097200001023     c* se + lungo di 70, il resto lo metto sul terzo  campo
097300001023     c                   if        %subst(w1ms1:71) <> *blanks
097400001023     c                   eval      d1ms3 = %subst(w1ms1:71)
097500001023     c                   endif
097600001023     c                   endsl
097700001023     c                   endif
097800001023     C* 3° messaggio
097900001023     c                   if        W1ms2 <> *blanks
098000001023     c                   select
098100001023     c                   when      d1ms2 = *blanks
098200001023     c                   movel     W1ms2         d1ms2
098300001025     c     msgrgp        add       4             h1Rig2
098400001023     c* se + lungo di 70, il resto lo metto sul quarto campo
098500001023     c                   if        %subst(w1ms2:71) <> *blanks
098600001023     c                   eval      d1ms3 = %subst(w1ms2:71)
098700001023     c                   endif
098800001023     c                   when      d1ms3 = *blanks
098900001023     c                   movel     W1ms2         d1ms3
099000001025     c     msgrgp        add       5             h1Rig2
099100001023     c* se + lungo di 70, il resto lo metto sul quinto campo
099200001023     c                   if        %subst(w1ms2:71) <> *blanks
099300001023     c                   eval      d1ms4 = %subst(w1ms2:71)
099400001023     c                   endif
099500001023     c                   other
099600001023     c                   movel     W1ms2         d1ms4
099700001025     c     msgrgp        add       2             h1Rig2
099800001023     c* se + lungo di 70, il resto lo metto sul quinto campo
099900001023     c                   if        %subst(w1ms2:71) <> *blanks
100000001023     c                   eval      d1ms5 = %subst(w1ms2:71)
100100001023     c                   endif
100200001023     c                   endsl
100300001023     c                   endif
100400001023     C* 4° messaggio
100500001023     c                   if        w1ms3 <> *blanks
100600001023     c                   select
100700001023     c                   when      d1ms3 = *blanks
100800001023     c                   movel     W1ms3         d1ms3
100900001025     c     msgrgp        add       5             h1Rig3
101000001023     c* se + lungo di 70, il resto lo metto sul quarto campo
101100001023     c                   if        %subst(w1ms3:71) <> *blanks
101200001023     c                   eval      d1ms4 = %subst(w1ms3:71)
101300001023     c                   endif
101400001023     c                   when      d1ms4 = *blanks
101500001023     c                   movel     W1ms3         d1ms4
101600001025     c     msgrgp        add       2             h1Rig3
101700001023     c* se + lungo di 70, il resto lo metto sul quinto campo
101800001023     c                   if        %subst(w1ms3:71) <> *blanks
101900001023     c                   eval      d1ms5 = %subst(w1ms3:71)
102000001023     c                   endif
102100001023     c                   when      d1ms5 = *blanks
102200001023     c                   movel     W1ms3         d1ms5
102300001025     c     msgrgp        add       3             h1Rig3
102400001023     c* se + lungo di 70, il resto lo metto sul quinto campo
102500001023     c                   if        %subst(w1ms3:71) <> *blanks
102600001023     c                   eval      d1ms6 = %subst(w1ms3:71)
102700001023     c                   endif
102800001023     c                   other
102900001023     c                   movel     W1ms3         d1ms6
103000001025     c     msgrgp        add       4             h1Rig3
103100001023     c* se + lungo di 70, il resto lo metto sul quinto campo
103200001023     c                   if        %subst(w1ms3:71) <> *blanks
103300001023     c                   eval      d1ms7 = %subst(w1ms3:71)
103400001023     c                   endif
103500001023     c                   endsl
103600001023     c                   endif
103700001025     c*
103800001025     C* se  riempito d1msg4, accendo indicatore 15 per segue...
103900001025     c                   if        d1ms4 <> *blanks
104000001025     c                   seton                                        15
104100001025     c                   endif
104200001023     C*
104300091005     c                   endsr
104400001023     C/EJECT
104500001023     C************************************************************
104600001023     C* visualizzazione messaggi di secondo livello
104700001023     C************************************************************
104800091005     C     GesD6         BEGSR
104900001023     c*
105000001024     c                   clear                   d6testo
105100001023     C*
105200001023     C* determino Riga/Colonna del cursore
105300001023     C                   MOVE      NRG           R$$
105400001023     C                   MOVE      NCL           C$$
105500001023     C                   Z-ADD     RIRI          CURR
105600001023     C                   Z-ADD     COCO          CURC
105700091019     C* 1° msg
105800091019     C     Riri          Ifeq      h1rig
105900001024     c                   movel     W1msg3000     d6testo
106000001023     c                   endif
106100091019     C* 2° msg
106200091019     C     Riri          Ifeq      h1rig1
106300091005     c     W1ms13000     andne     *blanks
106400091005     c                   movel     W1ms13000     d6testo
106500091005     c                   endif
106600091019     C* 3° msg
106700091019     C     Riri          Ifeq      h1rig2
106800091005     c     W1ms23000     andne     *blanks
106900091005     c                   movel     W1ms23000     d6testo
107000091005     c                   endif
107100091019     C* 4° msg
107200091019     C     Riri          Ifeq      h1rig3
107300091005     c     W1ms33000     andne     *blanks
107400091005     c                   movel     W1ms33000     d6testo
107500091005     c                   endif
107600091005     c*
107700091005     C* 1° bottone
107800091005     C                   if        Riri = msgrgp + 7 and
107900091005     c                             coco >=10 and coco <=25
108000091005     c                   movel     W1cd13000     d6testo
108100001030     c                   endif
108200001030     C* 2° bottone
108300001030     C                   if        Riri = msgrgp + 8 and
108400091005     c                             coco >=10 and coco <=25
108500091005     c                   movel     W1cd23000     d6testo
108600091005     c                   endif
108700091005     C* 3° bottone
108800091005     C                   if        Riri = msgrgp + 9 and
108900091005     c                             coco >=10 and coco <=25
109000091005     c                   movel     W1cd33000     d6testo
109100091005     c                   endif
109200091005     C* 4° bottone
109300091005     C                   if        Riri = msgrgp + 7 and
109400091005     c                             coco >=35 and coco <=50
109500091005     c                   movel     W1cd43000     d6testo
109600091005     c                   endif
109700091005     C* 5° bottone
109800091005     C                   if        Riri = msgrgp + 8 and
109900091005     C                             coco >=35 and coco <=50
110000091005     c                   movel     W1cd53000     d6testo
110100091005     c                   endif
110200091005     C* 6° bottone
110300091005     C                   if        Riri = msgrgp + 9 and
110400091005     C                             coco >=35 and coco <=50
110500091005     c                   movel     W1cd63000     d6testo
110600091005     c                   endif
110700091005     C* 7° bottone
110800091005     C                   if        Riri = msgrgp + 7 and
110900091005     C                             coco >=56 and coco <=71
111000091005     c                   movel     W1cd73000     d6testo
111100091005     c                   endif
111200091005     C* 8° bottone
111300091005     C                   if        Riri = msgrgp + 8 and
111400091005     C                             coco >=56 and coco <=71
111500091005     c                   movel     W1cd83000     d6testo
111600091005     c                   endif
111700091005     C* 9° bottone
111800091005     C                   if        Riri = msgrgp + 9 and
111900091005     C                             coco >=56 and coco <=71
112000091005     c                   movel     W1cd93000     d6testo
112100091005     c                   endif
112200091005     c*
112300091019     C* Msg del programma
112400001030     C                   if        Riri = msgrgp + 11
112500001030     c                   movel     WrkMsg3000    d6testo
112600001030     c                   endif
112700001030     c*
112800001031     c                   if        d6Testo <> *blanks
112900091005     c                   exfmt     SMSGD6
113000091019     C* Tolgo finestra                                   *
113100091005     C                   WRITE     SMSGD5                                       *
113200001031     c                   endif
113300001103     c*
113400091019     C* imposto geografia del cursore
113500001103     C                   Z-ADD     CURR          H1RIGA
113600001103     C                   Z-ADD     CURC          H1COLO
113700001103     C*
113800091019     c                   EndSr
113900001019     C*----------------------------------------------------*           ***
114000001019      /EJECT
114100930607     C*----------------------------------------------------*           ***
114200091019     C* Gestione video / Verifica errore e emissione msg   *
114300930607     C*----------------------------------------------------*           ***
114400091019     C     Analizz       BEGSR                                                  *
114500930607     C*                                                    *
114600930607     C* Abilita tasto F7 per MSG LAVORO solo se record allocato
114700091019     C     STWCOD        IFEQ      '01218'                                      *
114800930607     C*-                                                   *
114900091019     C                   MoveL     CST(4)        IdMsg
115000091005     c                   z-add     130           LenMsg
115100091005     c                   clear                   MsgDta
115200091005     c                   exsr      PrendiMsg
115300091005     c                   movel     WwMsg         DesCau           30
115400091005     C                   SETON                                        23        *
115500091005     C*-                                                   *
115600091005     C                   ELSE                                                   *
115700091005     C*-                                                   *
115800091005     C                   SETOFF                                       23        *
115900930607     C                   MOVE      *BLANK        DESCAU                         *
116000930607     C*-                                                   *
116100001019     C                   ENDIF                                                  *
116200930607     C*-                                                   *
116300930607     C     *INKL         DOWEQ     '0'                                          *
116400930607     C*                                                    *
116500091005     C                   EXFMT     SMSGD2                                       *
116600930607     C*-                                                   *
116700930607     C* GESTIONE MSG OPERATORE (KE)                        *
116800930607     C*-                                                   *
116900930607     C     *INKE         IFEQ      '1'                                          *
117000930607     C*-                                                   *
117100930607     C                   MOVEL     CMD(1)        CMD1                           *
117200930607     C                   MOVE      CMD(2)        CMD1                           *
117300930607     C*-                                                   *
117400930607     C                   MOVEL     DESCAU        CAUSA                          *
117500930607     C*-                                                   *
117600930607     C                   MOVEL     JOB           JOB1                           *
117700930607     C                   MOVEL     USER          USR1                           *
117800930607     C                   MOVEL     NBR           NBR1                           *
117900930607     C                   MOVEL     CMD1          CMDX                           *
118000091019     C                   EXSR      InvioMsg                                     *
118100001019     C                   ENDIF                                                  *
118200930607     C*-                                                   *
118300930607     C* GESTIONE MSG LAVORO    (KG)                        *
118400930607     C*-                                                   *
118500930607     C     *INKG         IFEQ      '1'                                          *
118600091019     C                   Exsr      GesMsgJob                                    *
118700001019     C                   ENDIF                                                  *
118800930607     C*-                                                   *
118900001019     C                   ENDDO                                                  *
119000930607     C*                                                    *
119100930607     C                   ENDSR                                                  *
119200930607     C*                                                    *
119300930607     C*----------------------------------------------------*           ***
119400930607     C/SPACE 5
119500930607     C*----------------------------------------------------*           ***
119600091019     C* Gestione invio msg al job bloccante                *
119700930607     C*----------------------------------------------------*           ***
119800091019     C     GesMsgJob     BEGSR                                                  *
119900001109     C*-
120000040302     C* ESEGUO LA SR PER REPERIRE IL NOME DEL VIDEO  A CUI
120100040302     C* MANDARE UN MESSAGGIO
120200040302     C*-
120300091019     C                   EXSR      RepVideo                                     *
120400001109     C*-
120500001109     C* ESEGUO LA SR PER REPERIRE IL NOME DEL LAVORO A CUI
120600001109     C* MANDARE UN MESSAGGIO
120700001109     C*-
120800930921     C                   EXSR      RICNOM                                       *
120900001109     C*-
121000001109     C* SOLO SE TROVATO IL NOME VIDEO O IL NOME UTENTE
121100001109     C*-
121200040302     C     NOMVID        IFNE      *BLANK                                       *
121300040302     C                   MOVEL     NOMVID        PGLAV            10            *
121400040302     C                   ELSE                                                   *
121500040302     C                   MOVEL     NOMLAV        PGLAV                          *
121600040302     C                   ENDIF                                                  *
121700001109     C*-
121800040302     C     PGLAV         IFNE      *BLANK                                       *
121900001109     C*-
122000001109     C* QUI SI POTREBBE GESTIRE IL MSG DA MANDARE A VIDEO
122100001109     C*-
122200930607     C                   MOVEL     CMD(5)        CMD3                           *
122300930607     C                   MOVE      CMD(6)        CMD3                           *
122400091005     C                   MOVEL     WrkMsg1447    Msg14471
122500091005     C                   MOVE      WrkMsg1447    Msg14472
122600091005     C*-
122700091005     C                   MOVEL     USER          USR3                           *
122800091005     C                   MOVEL     PGLAV         MSQ3                           *
122900091005     C*-
123000091005     C                   MOVEL     CMD3          CMDX                           *
123100091019     C                   EXSR      InvioMsg                                     *
123200091005     C*-
123300091005     C                   ELSE                                                   *
123400001109     C*-
123500001109     C* GESTISCO ERRORE INVIO MSG SE NON TROVATO NOME JOB
123600001109     C*-
123700091005     C                   EXFMT     SMSGD3                                       *
123800001109     C*-
123900001019     C                   ENDIF                                                  *
124000930607     C*-                                                   **
124100930607     C                   ENDSR                                                  **
124200930607     C*----------------------------------------------------*           ***
124300930607     C/SPACE 5
124400930607     C*----------------------------------------------------*           ***
124500930607     C* SR X VERIFICA TIPO DI LAVORO                       *
124600930607     C*----------------------------------------------------*           ***
124700930607     C*                                                    *
124800930607     C     CTRJOB        BEGSR                                                  *
124900930607     C*-                                                   *
125000930607     C* RICHIAMO IL PGM XMSGC2 PER INFORMAZIONI SUL JOB    *
125100930607     C*-                                                   *
125200930607     C                   CALL      'XMSGC2'                                     *
125300930607     C                   PARM                    JOB              10            *
125400930607     C                   PARM                    USER             10            *
125500930607     C                   PARM                    NBR               6            *
125600930607     C                   PARM                    TIPO              1            *
125700930607     C*-                                                   *
125800930607     C                   ENDSR                                                  *
125900930607     C*----------------------------------------------------*           ***
126000930607     C/SPACE 5
126100930607     C*----------------------------------------------------*           ***
126200930607     C* RICERCA NOME LAVORO CHE BLOCCA                     *
126300930607     C*----------------------------------------------------*           ***
126400930607     C*                                                    *
126500930607     C     RICNOM        BEGSR                                                  *
126600930607     C*-                                                   *
126700091019     C* REPERISCO IL NOME DEL LAVORO CHE BLOCCA DA STW     *
126800930607     C*-                                                   *
126900930607     C                   MOVE      *BLANK        LCB                            *
127000930921     C                   MOVE      *BLANK        NOMLAV           10            *
127100930607     C                   MOVE      *ZERO         H                 3 0          *
127200930607     C*-                                                   *
127300930607     C                   Z-ADD     91            Y                 3 0          *
127400091019     C     '/'           LOOKUP    STW(Y)                                 29    *
127500930607     C*-                                                   *
127600930921     C     *IN29         IFEQ      '1'                                          * 1 IF
127700930607     C*-                                                   *
127800930607     C                   ADD       1             Y                              *
127900930607     C                   Z-ADD     Y             J                 3 0          *
128000930607     C*-                                                   *
128100091019     C     '/'           LOOKUP    STW(J)                                 29    *
128200930607     C*-                                                   *
128300930607     C     *IN29         IFEQ      '1'                                          * 2 IF
128400930607     C*-                                                   *
128500930921     C                   SUB       1             J                              *
128600930607     C*-                                                   *
128700930921     C     Y             DO        J             K                 3 0          * 3 DO
128800930607     C*-                                                   *
128900930921     C     H             IFLT      10                                           * 4 IF
129000930921     C                   ADD       1             H                              *
129100091019     C                   MOVE      STW(K)        LCB(H)                         *
129200930921     C                   END                                                    * 4 END
129300930607     C*-                                                   *
129400930921     C                   MOVEA     LCB           NOMLAV                         *
129500930607     C                   END                                                    * 3 END
129600930607     C                   END                                                    * 2 END
129700930607     C                   END                                                    * 1 END
129800930607     C*-                                                   *
129900930607     C                   ENDSR                                                  *
130000930607     C*----------------------------------------------------*           ***
130100930607     C/SPACE 5
130200040302     C*----------------------------------------------------*           ***
130300091019     C* Reperimento video bloccante                        *
130400040302     C*----------------------------------------------------*           ***
130500091019     C     RepVideo      BEGSR                                                  *
130600040302     C*-                                                   *
130700091019     C* Reperisce nome job bloccante
130800040302     C*-                                                   *
130900040302     C                   MOVE      *BLANK        LCB                            *
131000040302     C                   MOVE      *BLANK        NOMVID           10            *
131100040302     C                   MOVE      *ZERO         H                 3 0          *
131200040302     C*-                                                   *
131300040302     C                   Z-ADD     91            Y                 3 0          *
131400091019     C     '/'           LOOKUP    STW(Y)                                 29    *
131500040302     C*-                                                   *
131600040302     C     *IN29         IFEQ      '1'                                          * 1 IF
131700040302     C*-                                                   *
131800040302     C                   ADD       1             Y                              *
131900040302     C                   Z-ADD     Y             J                 3 0          *
132000040302     C*-                                                   *
132100091019     C     '/'           LOOKUP    STW(J)                                 29    *
132200040302     C*-                                                   *
132300040302     C     *IN29         IFEQ      '1'                                          * 2 IF
132400040302     C*-                                                   *
132500040302     C                   ADD       1             J                              *
132600040302     C     J             ADD       9             J1                3 0          *
132700040302     C*-                                                   *
132800040302     C     J             DO        J1            K                 3 0          * 3 DO
132900040302     C*-                                                   *
133000040302     C     H             IFLT      10                                           * 4 IF
133100040302     C                   ADD       1             H                              *
133200091019     C     STW(K)        IFNE      '.'                                          * 5 IF
133300091019     C                   MOVE      STW(K)        LCB(H)                         *
133400040302     C                   END                                                    * 5 END
133500040302     C                   END                                                    * 4 END
133600040302     C*-                                                   *
133700040302     C                   MOVEA     LCB           NOMVID                         *
133800040302     C                   END                                                    * 3 END
133900040302     C                   END                                                    * 2 END
134000040302     C                   END                                                    * 1 END
134100040302     C*-                                                   *
134200040302     C                   ENDSR                                                  *
134300040302     C*----------------------------------------------------*           ***
134400930921     C/SPACE 5
134500930607     C*----------------------------------------------------*           ***
134600091019     C* Invio Messaggi                                     *
134700930607     C*----------------------------------------------------*           ***
134800091019     C     InvioMsg      BEGSR                                                  *
134900930607     C*-                                                   *
135000091019     C*- Se interattivo eseguo QCMDEXC, se batch call ad un *
135100091019     C*- Se batch chiama un CLP che gestisce invio msg
135200930608     C*-                                                   *
135300930608     C     TIPO          IFEQ      '1'                                          *
135400930607     C                   CALL      'QCMDEXC'                            21      *
135500930607     C                   PARM                    CMDX            160            *
135600930607     C                   PARM      160           LNG              15 5          *
135700930608     C                   ELSE                                                   *
135800930608     C                   CALL      'XMSGC'                              21      *
135900930608     C                   PARM                    MSO                            *
136000930608     C                   PARM                    RSP                            *
136100930608     C                   PARM                    RTN                            *
136200930608     C                   PARM                    QDU                            *
136300001019     C                   ENDIF                                                  *
136400930607     C*-                                                   *
136500930608     C* Gestisto errore invio MSG                          *
136600930608     C* solo se sono interattivo                           *
136700930607     C*-                                                   *
136800930607     C     *IN21         IFEQ      '1'                                          *
136900930607     C     TIPO          ANDEQ     '1'                                          *
137000930608     C     OPEVID        ANDEQ     'SI'                                         *
137100091005     C                   EXFMT     SMSGD3                                       *
137200001019     C                   ENDIF                                                  *
137300930607     C*-                                                   *
137400930607     C                   ENDSR                                                  *
137500930607     C*----------------------------------------------------*           ***
137600091019     C* Stampa errori                                      *
137700930607     C*----------------------------------------------------*           ***
137800091019     C     STPErrori     BEGSR                                                  *
137900010118     c*
138000091019     c* Testata
138100091005     c                   if        *in99 = *on or PrimaVolta <> *on
138200091005     c                   write     TESERR                                       *
138300091005     c                   eval      PrimaVolta = *on
138400091005     c                   setoff                                       99
138500091005     c                   endif
138600091019     c* Riga errore
138700091005     c                   movel     w1msg         P1msg
138800091005     C                   if        P1msg <> *blanks
138900091005     c                   write     prtErr0                                      *
139000091005     c                   endif
139100091005     C                   if        W1ms1 <> *blanks
139200091005     c                   write     prtErr1                                      *
139300091005     c                   endif
139400091005     C                   if        W1ms2 <> *blanks
139500091005     c                   write     prtErr2                                      *
139600091005     c                   endif
139700091005     C                   if        W1ms3 <> *blanks
139800091005     c                   write     prtErr3                                      *
139900091005     c                   endif
140000091005     c*
140100091019     c* Stp kpjba (se utilizzata)
140200091005     c                   if        prtjba <> *blanks
140300091005     c                   if        *in99 = *on
140400091005     c                   write     TESERR                                       *
140500091005     c                   setoff                                       99
140600091005     c                   endif
140700091005     c                   write     PrtKpjba                                     *
140800091005     C                   endif
140900091005     c*
141000091005     C* chiusura prtf se richiesta
141100091005     C                   if        %addr(msgClPrtf)<> *null
141200091005     C                   if        MsgClPrtf = *on
141300091005     C                   Close     SDGMSGP
141400091005     C                   CLEAR                   OPEPRT
141500091005     c                   endif
141600091005     c                   endif
141700091005     c*
141800091005     C                   ENDSR                                                  *
141900091005     C************************************************************
142000091019     C* Testo risposte default (i soliti 'Si', 'No', 'Prova'..)
142100971010     C************************************************************
142200091019     C     DefTesti      BEGSR
142300971010     C*
142400971010     C* Si/No
142500091019     C                   MoveL     CST(5)        IdMsg
142600091005     c                   z-add     135           LenMsg
142700091005     c                   clear                   MsgDta
142800091005     c                   exsr      PrendiMsg
142900091005     c                   movel     WwMsg         TxtSiNo          30
143000091005     C*
143100091005     C* Cancella/Riprova
143200091019     C                   MoveL     CST(6)        IdMsg
143300091005     c                   z-add     135           LenMsg
143400091005     c                   clear                   MsgDta
143500091005     c                   exsr      PrendiMsg
143600091005     c                   movel     WwMsg         TxtCanRip        30
143700091005     C*
143800091005     C                   ENDSR
143900091005     C************************************************************
144000091005     C* Testi messaggi bottoni
144100091005     C************************************************************
144200091019     C     CodTesti      BEGSR
144300091005     C*
144400091005     c                   if        W1cod1 <> *blanks
144500091019     c                   eval      Idmsg=WrkIdmsg
144600001030     C                   Move      W1Cod1        IdMsg
144700091005     c                   z-add     130           LenMsg
144800091005     c                   clear                   MsgDta
144900091005     c                   exsr      PrendiMsg
145000091005     C                   MOVEL     WWMSG         D1Txt1                         operazione richiesta
145100091005     C                   MOVEL     WWMSG3000     W1Cd13000                      operazione richiesta
145200091005     c                   endif
145300091005     C*
145400091005     c                   if        W1cod2 <> *blanks
145500091019     c                   eval      Idmsg=WrkIdmsg
145600091005     C                   Move      W1Cod2        IdMsg
145700091005     c                   z-add     130           LenMsg
145800091005     c                   clear                   MsgDta
145900091005     c                   exsr      PrendiMsg
146000091005     C                   MOVEL     WWMSG         D1Txt2                         operazione richiesta
146100091005     C                   MOVEL     WWMSG3000     W1Cd23000                      operazione richiesta
146200091005     c                   endif
146300091005     C*
146400091005     c                   if        W1cod3 <> *blanks
146500091019     c                   eval      Idmsg=WrkIdmsg
146600091005     C                   Move      W1Cod3        IdMsg
146700001030     c                   z-add     130           LenMsg
146800091005     c                   clear                   MsgDta
146900091005     c                   exsr      PrendiMsg
147000091005     C                   MOVEL     WWMSG         D1Txt3                         operazione richiesta
147100091005     C                   MOVEL     WWMSG3000     W1Cd33000                      operazione richiesta
147200091005     c                   endif
147300091005     C*
147400091005     c                   if        W1cod4 <> *blanks
147500091019     c                   eval      Idmsg=WrkIdmsg
147600091005     C                   Move      W1Cod4        IdMsg
147700091005     c                   z-add     130           LenMsg
147800091005     c                   clear                   MsgDta
147900091005     c                   exsr      PrendiMsg
148000091005     C                   MOVEL     WWMSG         D1Txt4                         operazione richiesta
148100091005     C                   MOVEL     WWMSG3000     W1Cd43000                      operazione richiesta
148200091005     c                   endif
148300091005     C*
148400091005     c                   if        W1cod5 <> *blanks
148500091019     c                   eval      Idmsg=WrkIdmsg
148600091005     C                   Move      W1Cod5        IdMsg
148700091005     c                   z-add     130           LenMsg
148800091005     c                   clear                   MsgDta
148900091005     c                   exsr      PrendiMsg
149000091005     C                   MOVEL     WWMSG         D1Txt5                         operazione richiesta
149100091005     C                   MOVEL     WWMSG3000     W1Cd53000                      operazione richiesta
149200091005     c                   endif
149300091005     C*
149400091005     c                   if        W1cod6 <> *blanks
149500091019     c                   eval      Idmsg=WrkIdmsg
149600091005     C                   Move      W1Cod6        IdMsg
149700091005     c                   z-add     130           LenMsg
149800091005     c                   clear                   MsgDta
149900091005     c                   exsr      PrendiMsg
150000091005     C                   MOVEL     WWMSG         D1Txt6                         operazione richiesta
150100091005     C                   MOVEL     WWMSG3000     W1Cd63000                      operazione richiesta
150200091005     c                   endif
150300091005     C*
150400091005     c                   if        W1cod7 <> *blanks
150500091019     c                   eval      Idmsg=WrkIdmsg
150600091005     C                   Move      W1Cod7        IdMsg
150700091005     c                   z-add     130           LenMsg
150800091005     c                   clear                   MsgDta
150900091005     c                   exsr      PrendiMsg
151000091005     C                   MOVEL     WWMSG         D1Txt7                         operazione richiesta
151100091005     C                   MOVEL     WWMSG3000     W1Cd73000                      operazione richiesta
151200091005     c                   endif
151300091005     C*
151400091005     c                   if        W1cod8 <> *blanks
151500091019     c                   eval      Idmsg=WrkIdmsg
151600091005     C                   Move      W1Cod8        IdMsg
151700091005     c                   z-add     130           LenMsg
151800091005     c                   clear                   MsgDta
151900091005     c                   exsr      PrendiMsg
152000091005     C                   MOVEL     WWMSG         D1Txt8                         operazione richiesta
152100091005     C                   MOVEL     WWMSG3000     W1Cd83000                      operazione richiesta
152200091005     c                   endif
152300091005     C*
152400091005     c                   if        W1cod9 <> *blanks
152500091019     c                   eval      Idmsg=WrkIdmsg
152600091005     C                   Move      W1Cod9        IdMsg
152700091005     c                   z-add     130           LenMsg
152800091005     c                   clear                   MsgDta
152900091005     c                   exsr      PrendiMsg
153000091005     C                   MOVEL     WWMSG         D1Txt9                         operazione richiesta
153100091005     C                   MOVEL     WWMSG3000     W1Cd93000                      operazione richiesta
153200091005     c                   endif
153300091005     C*
153400091005     C                   ENDSR
153500091005     C************************************************************
153600091005     C* Chiamata a xrtvm
153700091005     C************************************************************
153800091005     C     PrendiMsg     BEGSR
153900091005     C*
154000091019     C                   Movel     IDmsg         SavIDMSG         27
154100091019     C                   Movel     IDmsg         WIdMsg
154200091019     C                   If        IdCdMsg       ='SD'
154300091019     C                   Movel     'SDGMSG'      IDMsgf
154400091020     C                   Else
154500091020     C                   Movel     'PROMSG'      IDMsgf
154600091020     C                   EndIf
154700091019     C                   Movel     WIDmsg        IdMsg
154800091019     C*
154900091005     C                   clear                   Wwmsg
155000091005     C                   clear                   Wwmsg3000
155100091005     c                   if        Wctrmsg = *blanks
155200091005     c                   eval      WctrMsg = *off
155300091005     c                   endif
155400091005     C*
155500091005     C                   CALLB     'XRTVM'
155600091005     C                   PARM                    IDMSG            27
155700001019     C                   PARM                    TXTMSG          100
155800001019     C                   PARM                    ERRMSG            1
155900010206     C                   PARM      WCtrMsg       CTRMSG            1            centratura
156000001019     C                   PARM                    LENMSG            3 0          lun output.
156100010619     C                   PARM                    MSgDta          100            lun output.
156200001023     C                   PARM                    TxtMsg30         30            lun output.
156300001020     C                   PARM                    TxtMsg3000     3000            lun output.
156400001019     C     ERRMSG        IFNE      *ON
156500001023     C                   MOVEL     TxTMsg        WWMSG           130            *
156600001023     C                   MOVE      TxTMsg30      WWMSG                          *
156700001023     C                   MOVE      TxTMsg3000    WWMsg3000                      *
156800001019     C                   ELSE
156900001019     C                   MOVEL     *ALL'?'       WWMSG
157000001019     C                   ENDIF
157100001019     C*
157200091019     C                   Movel     SavIDmsg      IDMSG
157300091019     C*
157400091005     C                   ENDSR
157500091005     C************************************************************
157600091005     C* OPERAZIONI INIZIALI
157700091005     C************************************************************
157800091019     C     *InzSr        BEGSR
157900091005      *
158000091005     C     *DMY          Move      Udate         ZDateISO
158100091005     C     *JobRun       Move      ZDateISO      ZDate             6 0
158200091005      *
158300091005     c* reperisco testo messaggio secondo livello del messaggio
158400091005     c* del programma , in quanto NON SI PUO'USARE ALTHELP, poichè
158500091005     c* l'F1 È UN TASTO QUALUNQUE.
158600091005     c* Esistono messaggi di secondo livello
158700091019     c                   eval      Idmsg=WrkIdmsg
158800091019     C                   Move      'SDC0030'     IdMsg
158900091005     c                   z-add     130           LenMsg
159000091005     c                   clear                   MsgDta
159100091005     c                   exsr      PrendiMsg
159200091005     C                   MOVEL     WWMSG3000     WrkMsg3000                     operazione richiesta
159300091005     c*
159400091005     c* reperisco testo messaggio 'Tieni bloccato un record di cui
159500091005     c* ho bisogno, per favore termina la tua operazione' (pro1447)
159600091019     C                   MoveL     CST(7)        IdMsg
159700091005     c                   z-add     130           LenMsg
159800091005     c                   clear                   MsgDta
159900091005     c                   exsr      PrendiMsg
160000091005     C                   MOVEL     WWMSG         WrkMsg1447       78            operazione richiesta
160100091005     c*
160200091005     C*  reperisco nome sistema informatico per stampa
160300091005     C                   MOVEL     DSPGM         NOMPGM           10
160400091005     C                   CALL      'XNETA'                              21
160500091005     C                   PARM                    NOMSYS           10
160600091005     C*
160700010118     C                   ENDSR
160800091005     C************************************************************
160900091019** CST  Utilizzo costanti
161000091019SDGMSG    *LIBL     SDC0014    Immettere una delle seguenti risposte:
161100091019SDGMSG    *LIBL     SDC0015    Risposta errata. Rispondere:
161200091019SDGMSG    *LIBL     SDC0016    Oggetto in allocazione
161300091019SDGMSG    *LIBL     SDC0017    è fermo per record allocato
161400091019SDGMSG    *LIBL     SDC0018    cancella riprova
161500091019SDGMSG    *LIBL     SDC0019    riprova ttoni 2
161600091019SDGMSG    *LIBL     SDC0020    Dato bloccato
161700091019** MSG  Msg preimpostati
161800091019SDGMSG    *LIBL     SDC0021    ATTENZIONE: aggiornamento precedente non
161900091019SDGMSG    *LIBL     SDC0022    Record già allocato da altro lavoro
162000091019SDGMSG    *LIBL     SDC0023    Errori in stampa documenti
162100091019SDGMSG    *LIBL     SDC0024    Confermi operazione ?
162200091019SDGMSG    *LIBL     SDC0025    Archivio già impegnato da altro Job
162300091019SDGMSG    *LIBL     SDC0026    Non trovato record
162400091019SDGMSG    *LIBL     SDC0027    Non trovato comando OVRPRTF (tab.g75)
162500930513
162600930513
162700930513
162800930507** SK schere per stampa
162900930507                     ***------------------------------------------ SK1
163000930507-------------------------------------***        XX/XX/XX   Pag. xx SK2
163100091019                     ** ERRORE ERRORE ERRORE ERRORE ERRORE ERRORE  SK3
163200091019ERRORE ERRORE ERRORE ERRORE ERRORE ER **                           SK4
163300930507                     ***------------------------------------------ SK5
163400930507-------------------------------------***                           SK6
163500930507*----------------------------------------------------------------- SK7
163600930507-----------------------------------------------------------------* SK8
163700930607                                                                   SK9
163800930607                                                                   SK10
163900930513** CMD
164000010115SNDMSG MSG('     Job  123456/1234567890/1234567890
164100010115         ') TOMSGQ(*SYSOPR   ) MSGTYPE(*INFO)
164200010115SNDMSG   MSG('
164300010115                                            ') TOMSGQ(*SYSOPR   ) MSGTYPE(*INFO)
164400001109SNDMSG MSG('DA 1234567890 : messaggio di record bloccato, codice PRO1447
164500001109                          ') TOMSGQ(*SYSOPR   ) MSGTYPE(*INFO)
