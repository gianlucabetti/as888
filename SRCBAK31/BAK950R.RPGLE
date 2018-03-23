000100021115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200021115     H*PARMS CVTOPT(*NONE)
000300021115     H DECEDIT('0,') DATEDIT(*DMY/)
000400930908     H*---------------------------------------------------------------*
000500930908     H* BAK950R                                                       *
000600930908     H*                                                               *
000700010810     H* MANUTENZIONE AREA DATI DI CONTROLLO A.B.P.                    *
000800930908     H*                                                               *
000900010810     H*---------------------------------------------------------------*
001000930908     H*                                                               *
001100930908     H* 31  AREA DATI ALLOCATA DA UN ALTRO LAVORO                     *
001200930908     H*                                                               *
001300930908     H*---------------------------------------------------------------*
001400031002X0192FKPPRF01L  IF   E           K DISK
001500031002X0192D*-------------
001600930908     FBAK950D   CF   E             WORKSTN
001700031002X0192F                                     INFDS(DSFMT)
001800031002     D*-------------
001900930908     D KPJBA         E DS
002000090203X0305D*-------------
002100090203  "  D* DS Esterna gestione messaggi
002200090203  "  D XMSGDS        E DS
002300090203  "  D  STS          E                     EXTFLD(MSGSTS)
002400090203  "  D                                     DIM(333)
002500090203  "  D  JBA          E                     EXTFLD(MSGJBA)
002600090203X0305D                                     DIM(502)
002700031002     D*-------------
002800930908     D KPDT1         E DS           512
002900090202X0305D savKPDT1        S                   Like(KPDT1)
003000990910X0026D*-------------
003100990907  "  D* Dati di ambiente ottenuti da XSOC
003200990907  "  D SOC001        E DS                  EXTNAME(XSOC001DS)
003300990907  "  D  SALDOC       E                     EXTFLD(XSCSALDOC)
003400990907  "  D  SALDOA       E                     EXTFLD(XSCSALDOA)
003500990907  "  D*-------------
003600990907  "  D* DS Interna per dati di output di XSOC
003700990907  "  D XSOCDS          DS          1000
003800990907  "  D*-------------
003900990907  "  D* REPERISCO NOME PGM
004000990907  "  D STATUS         SDS           333
004100990910X0026D  DSPGM            *PROC
004200040505X0222D*-------------
004300040505  "  D ROLLDOWN        C                   CONST(X'F4')
004400040505X0222D ROLLUP          C                   CONST(X'F5')
004500031002X0192D*-------------
004600031002  "  D DSFMT           DS
004700031002  "  D  $TASTO               369    369
004800031002  "  D  NRG                  370    370
004900031002  "  D  NCL                  371    371
005000031002  "  D*-------------
005100031002  "  D* posizione cursore
005200031002  "  D CURSOR          DS
005300031002  "  D  RIRI                   1      2B 0 INZ
005400031002  "  D  R$$                    2      2
005500031002  "  D  COCO                   3      4B 0 INZ
005600031002  "  D  C$$                    4      4
005700031002  "  D*-------------
005800031002  "  D CurR            S              2S 0
005900031002  "  D CurC            S              2S 0
006000031002  "  D*-------------
006100031002  "  D* Ricerca profilo
006200031002X0192D BAK980DS      E DS                  INZ
006300030709X0185D*-------------
006400030709  "  D* Campi di lavoro
006500030709  "  D $Pgm            S             10A
006600030709  "  D $Lib            S             10A
006700030709  "  D $ErrPgm         S              1A
006800030919X0185D $ErrLib         S              1A
006900030919X0197D $JobQ           S             10A
007000030919X0197D $ErrJobQ        S              1A
007100030919X0185D $Cmd            S             80A
007200030709  "  D $CmdLen         S             15  5
007300030709  "  D $CmdLib         S             10A
007400030709  "  D $CmdObj         S             10A
007500030709  "  D $CmdType        S             10A
007600030709  "  D $ErrObj         S              1A
007700030709X0185D*-------------
007800100621X0309D KPDT0         E DS           100
007900930908     C*----------------------------------------------------*
008000930908     C*-  FLUSSO PRINCIPALE                               -*
008100930908     C*----------------------------------------------------*
008200930908     C*
008300930908     C     $FINE         DOWEQ     *OFF
008400930908     C     $GEST         CASEQ     'D01'         GESD01
008500040505X0222C     $GEST         CASEQ     'D02'         GESD02
008600930908     C                   ENDCS
008700930908     C                   ENDDO
008800030709     C*
008900930908     C                   SETON                                          LR
009000030709     C*
009100930908     C*---GESD01  -----------------------------------------*
009200040505     C*-   GESTIONE D1                                    -*
009300930908     C*----------------------------------------------------*
009400930908     C     GESD01        BEGSR
009500930908     C*
009600930908     C                   EXFMT     K950D1
009700930908     C*
009800030709     C                   SELECT
009900040505X0222C* ROLLUP
010000040505  "  C     $TASTO        WHENEQ    ROLLUP
010100040505X0222C                   MOVE      'D02'         $GEST
010200930908     C* F3=FINE LAVORO
010300930908     C     *INKC         WHENEQ    *ON
010400031002X0026C* F12=RITORNO
010500990910X0026C     *INKL         OREQ      *ON
010600930908     C                   MOVE      *ON           $FINE
010700031006X0192C* F4=RICERCHE
010800031002  "  C     *INKD         WHENEQ    *ON
010900031002X0192C                   EXSR      SEARCH
011000100621      *
011100100621X0309C* F14=imposto/visualizzo kpdt0
011200100621  "  C     *INKN         WHENEQ    *ON
011300100621X0309C                   Exsr      Wdw954
011400100621      *
011500030709X0185C* Controlli
011600030709  "  C                   OTHER
011700040505X0185C                   ExSr      CtrD1
011800040505X0222C                   If        *In99 = *Off
011900040505  "  C                   ExSr      CtrD2
012000040505  "  C                   If        *In99 = *On
012100040506  "  C                   SetOn                                        9996
012200040505  "  C                   EndIf
012300040505X0222C                   EndIf
012400040505X0185C                   ENDSL
012500030709X0185C*
012600930908     C* F6=CONFERMA
012700030709X0185X******INKF         WHENEQ    *ON
012800030709  "  C     *INKF         IFEQ      *ON
012900030709X0185C     *IN99         ANDEQ     *OFF
013000040505X0222X* ALLOCAZIONE DTAARA
013100040505  "  X******LOCK         IN        KPDT1                                31
013200040505  "  X* RIUSCITA ALLOCAZIONE
013300040505  "  X******IN31         IFEQ      *OFF
013400040505  "  X*****              MOVE      D1OFF         KP1OFF
013500040505  "  X*****              MOVE      D1CNL         KP1CNL
013600040505  "  X*****              MOVE      D1NGG         KP1NGG
013700040505  "  X*****              MOVE      D1STL         KP1STL
013800040505  "  X***** ex-X0018     MOVE      D1SPI         KP1SPI
013900040505  "  X*****      "       MOVE      D1SPF         KP1SPF
014000040505  "  X*****      "       MOVE      D1PRI         KP1PRI
014100040505  "  X***** ex-X0018     MOVE      D1PRF         KP1PRF
014200040505  "  X***** ex-X0116     MOVE      D1ISW         KP1ISW
014300040505  "  X***** ex-X0154     MOVE      D1LOA         KP1LOA
014400040505  "  X***** ex-X0154     MOVE      D1LOS         KP1LOS
014500040505  "  X***** ex-X0184     MOVE      D1SPN         KP1SPN
014600040505  "  X*****      "       MOVE      D1PRN         KP1PRN
014700040505  "  X*****      "       MOVE      D1GID         KP1GID
014800040505  "  X*****      "       MOVE      D1GAI         KP1GAI
014900040505  "  X*****      "       MOVE      D1GFD         KP1GFD
015000040505  "  X*****      "       MOVE      D1GAF         KP1GAF
015100040505  "  X*****      "       MOVE      D1GND         KP1GND
015200040505  "  X***** ex-X0184     MOVE      D1GAN         KP1GAN
015300040505  "  X***** ex-X0185     MOVE      D1OFD         KP1OFD
015400040505  "  X*****      "       MOVE      D1PGM         KP1PGM
015500040505  "  X***** ex-X0185     MOVE      D1LIB         KP1LIB
015600040505  "  X***** ex-X0192     MOVE      D1USD         KP1USD
015700040505  "  X***** ex-X0197     MOVE      D1MSG         KP1MSG
015800040505  "  X*****      "       MOVE      D1SEV         KP1SEV
015900040505  "  X*****      "       MOVE      D1QMO         KP1QMO
016000040505  "  X***** ex-X0197     MOVE      D1LMO         KP1LMO
016100040505  "  X* AGGIORNAMENTO DTAARA
016200040505  "  X*****              OUT       KPDT1
016300040505  "  C                   ExSr      AggKPDT1
016400040505X0222C                   If        *In99 = *Off
016500930908     C                   MOVE      *ON           $FINE
016600040505X0222C                   EndIf
016700040505X0222X*****              ENDIF
016800030709X0185C                   ENDIF
016900030709  "  X*
017000030709X0185X*****              ENDSL
017100030709     C*
017200930908     C                   ENDSR
017300040505X0222C*---GESD02  -----------------------------------------*
017400040505  "  C*-   GESTIONE D2                                    -*
017500040505  "  C*----------------------------------------------------*
017600040505X0222C     GESD02        BEGSR
017700040505     C*
017800040505     C                   EXFMT     K950D2
017900040505     C*
018000040505     C                   SELECT
018100040505     C* ROLLDOWN
018200040505     C     $TASTO        WHENEQ    ROLLDOWN
018300040505     C                   MOVE      'D01'         $GEST
018400040505     C* F3=FINE LAVORO
018500040505     C     *INKC         WHENEQ    *ON
018600040505     C* F12=RITORNO
018700040505     C     *INKL         OREQ      *ON
018800040505     C                   MOVE      *ON           $FINE
018900040505     C* F4=RICERCHE
019000040505     C     *INKD         WHENEQ    *ON
019100040505     C                   EXSR      SEARCH
019200040505     C* Controlli
019300040505     C                   OTHER
019400040505     C                   ExSr      CtrD2
019500040505     C                   If        *In99 = *Off
019600040505     C                   ExSr      CtrD1
019700040505     C                   If        *In99 = *On
019800040506     C                   SetOn                                        9996
019900040505     C                   EndIf
020000040505     C                   EndIf
020100040505     C                   ENDSL
020200040505     C*
020300040505     C* F6=CONFERMA
020400040505     C     *INKF         IFEQ      *ON
020500040505     C     *IN99         ANDEQ     *OFF
020600040505     C                   ExSr      AggKPDT1
020700040505     C                   If        *In99 = *Off
020800040505     C                   MOVE      *ON           $FINE
020900040505     C                   EndIf
021000040505     C                   ENDIF
021100040505     C*
021200040505X0222C                   ENDSR
021300040505X0222C*----------------------------------------------------*
021400040505  "  C*- Aggiornamento data area KPDT1                    -*
021500040505  "  C*----------------------------------------------------*
021600040505X0222C     AggKPDT1      BEGSR
021700040505     C*
021800040505     C* ALLOCAZIONE DTAARA
021900040505     C     *LOCK         IN        KPDT1                                31
022000090202X0305C                   Eval      savKPDT1 = KPDT1
022100040505     C* RIUSCITA ALLOCAZIONE
022200040505     C     *IN31         IFEQ      *OFF
022300060829X0259X***                MOVE      D1OFF         KP1OFF
022400060828  "  C                   MOVE      D1OF1         KP1OF1
022500060828  "  C                   MOVE      D1OF2         KP1OF2
022600060828  "  C                   MOVE      D1OF3         KP1OF3
022700060828  "  C                   MOVE      D1OF4         KP1OF4
022800060828  "  C                   MOVE      D1OF5         KP1OF5
022900060828  "  C                   MOVE      D1OF6         KP1OF6
023000060829  "  C                   MOVE      D1OF7         KP1OF7
023100060829  "  C                   MOVE      D1OF1D        KP1OF1D
023200060829  "  C                   MOVE      D1OF2D        KP1OF2D
023300060829  "  C                   MOVE      D1OF3D        KP1OF3D
023400060829  "  C                   MOVE      D1OF4D        KP1OF4D
023500060829  "  C                   MOVE      D1OF5D        KP1OF5D
023600060829  "  C                   MOVE      D1OF6D        KP1OF6D
023700060829X0259C                   MOVE      D1OF7D        KP1OF7D
023800040505     C                   MOVE      D1CNL         KP1CNL
023900040505     C                   MOVE      D1NGG         KP1NGG
024000040505     C                   MOVE      D1STL         KP1STL
024100040505     C                   MOVE      D1SPI         KP1SPI
024200040505     C                   MOVE      D1SPF         KP1SPF
024300040505     C                   MOVE      D1PRI         KP1PRI
024400040505     C                   MOVE      D1PRF         KP1PRF
024500040505     C                   MOVE      D1ISW         KP1ISW
024600040505     C                   MOVE      D1LOA         KP1LOA
024700040505     C                   MOVE      D1LOS         KP1LOS
024800040505     C                   MOVE      D1SPN         KP1SPN
024900040505     C                   MOVE      D1PRN         KP1PRN
025000040505     C                   MOVE      D1GID         KP1GID
025100040505     C                   MOVE      D1GAI         KP1GAI
025200040505     C                   MOVE      D1GFD         KP1GFD
025300040505     C                   MOVE      D1GAF         KP1GAF
025400040505     C                   MOVE      D1GND         KP1GND
025500040505     C                   MOVE      D1GAN         KP1GAN
025600060829X0259X***                MOVE      D1OFD         KP1OFD
025700040505     C                   MOVE      D1PGM         KP1PGM
025800040505     C                   MOVE      D1LIB         KP1LIB
025900040505     C                   MOVE      D1USD         KP1USD
026000040505     C                   MOVE      D1MSG         KP1MSG
026100040505     C                   MOVE      D1SEV         KP1SEV
026200040505     C                   MOVE      D1QMO         KP1QMO
026300040505     C                   MOVE      D1LMO         KP1LMO
026400040524     C                   MOVE      D1RAQ         KP1RAQ
026500060920X0266C                   MOVE      D1BLU         KP1BLU
026600061206X0261C                   MOVE      D1TIPS        KP1TIPS
026700061206X0261C                   MOVE      D1DEVS        KP1DEVS
026800040505     C* AGGIORNAMENTO DTAARA
026900040505     C                   OUT       KPDT1
027000100831X0305C*                  EXSR      WRTLOG
027100040505     C* NON RIUSCITA ALLOCAZIONE
027200040505     C                   ELSE
027300040505     C                   SETON                                        9799
027400040505     C                   ENDIF
027500040505     C*
027600040505X0222C                   ENDSR
027700090202X0305C****************************************************************
027800090202  "  C* Scrittura log
027900090202  "  C****************************************************************
028000090202X0305C     WRTLOG        BEGSR
028100090202     C*
028200090202     C* carico le immagini prima / dopo
028300090202     C                   Eval      Bef951 = SavKPDT1
028400090202     C                   Eval      Aft951 = KPDT1
028500090202     C*
028600090202     C* richiamo programma di scrittura log
028700090203     C                   Call      'BAK951R'                            23
028800090202     C                   Parm                    KPJBA
028900090202     C                   Parm                    Bef951          512
029000090202     C                   Parm                    Aft951          512
029100090203     C*
029200090203     C* errore durante la scrittura del file di log
029300090203     C                   If        *In23 = *On
029400090203     C                   Reset                   XMSGDS
029500090203     C                   Eval      IdMsg = 'PROBAS    *LIBL     PRO0130'
029600090203     C                   Eval      DtaMsg = *Blanks
029700090203     C                   ExSr      RtvMsg
029800090203     C                   ExSr      MsgPgm
029900090203     C                   EndIf
030000090202     C*
030100090202X0305C                   ENDSR
030200090203X0305C******************************************************
030300090203  "  C* REPERIMENTO MESSAGGIO
030400090203  "  C******************************************************
030500090203X0305C     RTVMSG        BEGSR
030600090203     C*
030700090203     C                   Call      'XRTVM'
030800090203     C                   Parm                    IDMSG            27
030900090203     C                   Parm                    TXTMSG          100
031000090203     C                   Parm                    ERRMSG            1
031100090203     C                   Parm      *Off          CTRMSG            1
031200090203     C                   Parm                    LENMSG            3 0
031300090203     C                   Parm                    DTAMSG          100
031400090203     C*
031500090203     C                   If        ERRMSG <> *On
031600090203     C                   Movel     TXTMSG        MsgMs1
031700090203     C                   Else
031800090203     C                   Movel     *ALL'?'       MsgMs1
031900090203     C                   EndIf
032000090203     C*
032100090203X0305C                   ENDSR
032200090203X0305C******************************************************
032300090203  "  C* PGM PER GESTIONE MESSAGGI ERRORE                   *
032400090203  "  C******************************************************
032500090203X0305C     MSGPGM        BEGSR
032600090203     C*
032700090203     C                   Z-ADD     0             MSGMSG
032800090203     C                   MOVEL     'N'           MSGRSP
032900090203     C                   MOVEL     'S'           MSGEMV
033000090203     C                   Z-ADD     10            MSGRGP
033100090203     C                   MOVEL     'N'           MSGLCK
033200090203     C                   MOVEL     'N'           MSGCNL
033300090203     C                   MOVEL     'N'           MSGVID
033400090203     C                   MOVEL     'N'           MSGOPE
033500090203     C                   MOVEL     'N'           MSGSTP
033600090203     C*
033700090203     C                   CALL      'XMSGR'
033800090203     C                   PARM                    XMSGDS
033900090203     C*
034000090203X0305C                   ENDSR
034100031002X0192C****************************************************************
034200031002  "  C* RICERCHE
034300031002  "  C****************************************************************
034400031002X0192C     SEARCH        BEGSR
034500031002     C*
034600031002     C* determino Riga/Colonna del cursore
034700031002     C                   MOVE      NRG           R$$
034800031002     C                   MOVE      NCL           C$$
034900031002     C                   Z-ADD     RIRI          CURR
035000031002     C                   Z-ADD     COCO          CURC
035100031002     C*
035200031002     C                   SELECT
035300031002     C* Profilo utente
035400031002     C     H1NMFL        WHENEQ    'D1USD'
035500031002     C                   Clear                   D1UsDD
035600031002     C                   RESET                   BAK980DS
035700031002     C                   MOVE      '01'          OPZ980
035800031002     C                   MOVEL     BAK980DS      KPJBU
035900031002     C                   CALL      'BAK980R'
036000031002     C                   PARM                    KPJBA
036100031002     C                   MOVEL     KPJBU         BAK980DS
036200031002     C     PROFILO980    IFNE      *BLANK
036300031002     C                   MOVEL     PROFILO980    D1UsD
036400031002     C                   MOVEL     DESCR980      D1UsDD
036500031002     C                   ENDIF
036600031002     C*
036700031002     C* ricerca non abilitata in questa posizione
036800031002     C                   OTHER
036900031002     C                   SETON                                        9899
037000031002     C*
037100031002     C                   ENDSL
037200031002     C*
037300031002     C* imposto pos. del cursore
037400031002     C                   Z-ADD     CURR          H1RIGA
037500031002     C                   Z-ADD     CURC          H1COLO
037600031002     C*
037700031002X0192C                   ENDSR
037800990910X0026C****************************************************************
037900990907  "  C* INIZIALIZZAZIONE TESTATA DEL VIDEO
038000990907  "  C****************************************************************
038100990907  "  C     INZD1         BEGSR
038200030709  "  C*
038300990907  "  C* Reperisco dati società
038400990907  "  C                   CLEAR                   XSOCDS
038500990907  "  C                   CALL      'BAK500R'
038600990907  "  C                   PARM                    XSOCDS
038700990907  "  C*
038800990907  "  C* Controlli dati società
038900990907  "  C     XSOCDS        IFNE      *BLANKS
039000990907  "  C                   MOVEL     XSOCDS        SOC001
039100990907  "  C                   ENDIF
039200990907  "  C*
039300990907  "  C* Valorizzazione campi univoci testate
039400990907  "  C                   MOVEL     KNSIF         NOMSIF
039500990907  "  C                   MOVEL     XSCDSI        NOMDIT
039600990907  "  C                   MOVEL     DSPGM         NOMPGM
039700030709  "  C*
039800030709X0026C                   ENDSR
039900930908     C****************************************************************
040000930908     C* DESCRIZIONE  - QUESTA SBR ESEGUE LE OPERAZIONI NECESSARIE
040100930908     C*                ALL'INIZIO DEL PROGRAMMA, CIOE':
040200930908     C*                - ASSEGNA VALORI DIVERSI DAI DEFAULT AI CAMPI
040300930908     C*                  DI LAVORO UTILIZZATI DAL PROGRAMMA
040400930908     C****************************************************************
040500930908     C     *INZSR        BEGSR
040600930908     C*>>>>>RICEVIMENTO PARAMETRI
040700930908     C     *ENTRY        PLIST
040800930908     C                   PARM                    KPJBA
040900040505X0026C* INIZIALIZZAZIONE VIDEO
041000990910X0026C                   EXSR      INZD1
041100930908     C*>>>>>CAMPI DI CONTROLLO
041200930908     C                   MOVE      *OFF          $FINE             1            fine lavoro
041300930908     C                   MOVE      'D01'         $GEST             3            video da gestir
041400930908     C                   MOVE      *ON           $INZS1            1
041500930908     C*
041600930908     C     *DTAARA       DEFINE                  KPDT1
041700930908     C                   IN        KPDT1
041800930908     C                   MOVE      KP1NRA        D1NRA
041900930908     C                   MOVE      KP1NOT        D1NOT
042000060829X0259X***                MOVE      KP1OFF        D1OFF
042100060828  "  C                   MOVE      KP1OF1        D1OF1
042200060828  "  C                   MOVE      KP1OF2        D1OF2
042300060828  "  C                   MOVE      KP1OF3        D1OF3
042400060828  "  C                   MOVE      KP1OF4        D1OF4
042500060828  "  C                   MOVE      KP1OF5        D1OF5
042600060828  "  C                   MOVE      KP1OF6        D1OF6
042700060829  "  C                   MOVE      KP1OF7        D1OF7
042800060829  "  C                   MOVE      KP1OF1D       D1OF1D
042900060829  "  C                   MOVE      KP1OF2D       D1OF2D
043000060829  "  C                   MOVE      KP1OF3D       D1OF3D
043100060829  "  C                   MOVE      KP1OF4D       D1OF4D
043200060829  "  C                   MOVE      KP1OF5D       D1OF5D
043300060829  "  C                   MOVE      KP1OF6D       D1OF6D
043400060829X0259C                   MOVE      KP1OF7D       D1OF7D
043500940303     C                   MOVE      KP1CNL        D1CNL
043600940303     C                   MOVE      KP1NGG        D1NGG
043700940303     C                   MOVE      KP1STL        D1STL
043800990719X0018C                   MOVE      KP1SPI        D1SPI
043900990719  "  C                   MOVE      KP1SPF        D1SPF
044000990719  "  C                   MOVE      KP1PRI        D1PRI
044100990719X0018C                   MOVE      KP1PRF        D1PRF
044200010810X0116C                   MOVE      KP1ISW        D1ISW
044300021119X0154C                   MOVE      KP1LOA        D1LOA
044400021119X0154C                   MOVE      KP1LOS        D1LOS
044500030709X0184C                   MOVE      KP1SPN        D1SPN
044600030709  "  C                   MOVE      KP1PRN        D1PRN
044700030709  "  C                   MOVE      KP1GID        D1GID
044800030709  "  C                   MOVE      KP1GAI        D1GAI
044900030709  "  C                   MOVE      KP1GFD        D1GFD
045000030709  "  C                   MOVE      KP1GAF        D1GAF
045100030709  "  C                   MOVE      KP1GND        D1GND
045200030709X0184C                   MOVE      KP1GAN        D1GAN
045300060829X0259X*** ex-X0185       MOVE      KP1OFD        D1OFD
045400060828X0185C                   MOVE      KP1PGM        D1PGM
045500030709X0185C                   MOVE      KP1LIB        D1LIB
045600031002X0192C                   MOVE      KP1USD        D1USD
045700030919X0197C                   MOVE      KP1MSG        D1MSG
045800030919  "  C                   MOVE      KP1SEV        D1SEV
045900030919  "  C                   MOVE      KP1QMO        D1QMO
046000030919X0197C                   MOVE      KP1LMO        D1LMO
046100040505X0222C                   MOVE      KP1RAQ        D1RAQ
046200060920X0266C                   MOVE      KP1BLU        D1BLU
046300061206X0261C                   MOVE      KP1TIPS       D1TIPS
046400061206X0261C                   MOVE      KP1DEVS       D1DEVS
046500021119     C*
046600100621X0309C                   Setoff                                       14
046700100621 "   C     *DTAARA       DEFINE                  KPDT0
046800100621 "   C                   IN        KPDT0
046900100621 "   C                   If        KP0NSUFILE = '1'
047000100621 "   C                   Seton                                        14
047100100621X0309C                   Endif
047200100621      *
047300930908     C                   ENDSR
047400030709X0185C****************************************************************
047500030709  "  C* CONTROLLI VIDEO D1
047600030709  "  C****************************************************************
047700030709X0185C     CTRD1         BEGSR
047800030709     C*
047900030709     C                   Eval      *In99 = *Off
048000040108X0213C*
048100040108  "  C* Nr. gg prima cancellazione file di log
048200040108  "  C*
048300040108  "  C                   If        D1NGG = 0 and
048400040108  "  C                             (D1CnL = '1' or D1CnL = '2')
048500040108  "  C                   SetOn                                        5699
048600040108X0213C                   EndIf
048700030709     C*
048800030709     C* Pgm/Lib avvio sistema
048900030709     C*
049000030709     C                   If        D1Pgm <> *Blank or D1Lib <> *Blank
049100030709     C* valore speciale '*NONE'
049200030709     C                   If        D1Pgm = '*NONE'
049300030709     C                   If        D1Lib <> *Blank
049400030709     C                   SetOn                                        5199
049500030709     C                   EndIf
049600030709     C* controllo esistenza
049700030709     C                   Else
049800030710     C                   If        D1Lib = *Blank
049900030710     C                   Eval      D1Lib = '*LIBL'
050000030710     C                   EndIf
050100030709     C                   Eval      $Pgm = D1Pgm
050200030709     C                   Eval      $Lib = D1Lib
050300030709     C                   ExSr      CtrPgmLib
050400030709     C* - pgm non trovato
050500030709     C                   If        $ErrPgm = *On
050600030709     C                   Seton                                        5099
050700030709     C                   EndIf
050800030709     C* - lib non trovata
050900030709     C                   If        $ErrLib = *On
051000030709     C                   Seton                                        5199
051100030709     C                   EndIf
051200030709     C                   EndIf
051300030709     C* default
051400030709     C                   Else
051500030709     C                   Eval      D1Pgm = 'QSTRUP'
051600040401X0219X*****              Eval      D1Lib = 'QSYS'
051700040401X0219C                   Eval      D1Lib = '*LIBL'
051800030709     C                   EndIf
051900040506X0222X***** ex-X0192
052000040506  "  X*****      "   utente default per creazione profili
052100040506  "  X*****      "
052200040506  "  X*****      "       Clear                   D1UsDD
052300040506  "  X*****      "       If        D1UsD <> *Blank
052400040506  "  X*****D1UsD "       Chain     KPPRF01L                           54
052500040506  "  X***54      "       SetOn                                        99
052600040506  "  X**N54      "       MoveL     PRFTxt        D1UsDD
052700040506X0222X***** ex-X0192     EndIf
052800060829X0259X*
052900060829X0259X*** ATTENZIONE: spostati in D2 controllo campi D1QMO, D1LMO e D1SEV
053000030709     C*
053100030709X0185C                   ENDSR
053200040505X0222C****************************************************************
053300040505  "  C* CONTROLLI VIDEO D2
053400040505  "  C****************************************************************
053500040505X0222C     CTRD2         BEGSR
053600040505     C*
053700040505     C                   Eval      *In99 = *Off
053800060829X0259C*
053900060828  "  C* JobQ/Lib esecuzione monitor QSYSOPR
054000060828  "  C*
054100060828  "  C                   If        D1QMO <> *Blank or D1LMO <> *Blank
054200060828  "  C* valore speciale '*CTLSBS'
054300060828  "  C                   If        D1QMO = '*CTLSBS'
054400060828  "  C                   If        D1LMO <> *Blank
054500060828  "  C                   SetOn                                        5399
054600060828  "  C                   EndIf
054700060828  "  C* controllo esistenza
054800060828  "  C                   Else
054900060828  "  C                   If        D1LMO = *Blank
055000060828  "  C                   Eval      D1LMO = '*LIBL'
055100060828  "  C                   EndIf
055200060828  "  C                   Eval      $JobQ = D1QMO
055300060828  "  C                   Eval      $Lib = D1LMO
055400060828  "  C                   ExSr      CtrJobQLib
055500060828  "  C* - jobq non trovata
055600060828  "  C                   If        $ErrJobQ = *On
055700060828  "  C                   Seton                                        5299
055800060828  "  C                   EndIf
055900060828  "  C* - lib non trovata
056000060828  "  C                   If        $ErrLib = *On
056100060828  "  C                   Seton                                        5399
056200060828  "  C                   EndIf
056300060828  "  C                   EndIf
056400060828  "  C* default
056500060828  "  C                   Else
056600060828  "  C                   Eval      D1QMO = '*CTLSBS'
056700060828  "  C                   EndIf
056800060828  "  C*
056900060828  "  C* Gravità minima messaggi
057000060828  "  C*
057100060828  "  C                   If        D1Sev <> *Blank
057200060828  "  C                   If        D1Sev < '00' or D1Sev > '99'
057300060828  "  C                   Seton                                        5599
057400060828  "  C                   EndIf
057500060828  "  C* default
057600060828  "  C                   Else
057700060828  "  C                   Eval      D1Sev = '40'
057800060829X0259C                   EndIf
057900040506     C*
058000040506     C* utente default per creazione profili
058100040506     C*
058200040506     C                   Clear                   D1UsDD
058300040506     C                   If        D1UsD <> *Blank
058400040506     C     D1UsD         Chain     KPPRF01L                           54
058500040506     C   54              SetOn                                        99
058600040506     C  N54              MoveL     PRFTxt        D1UsDD
058700040506     C                   EndIf
058800061206X0261C*
058900061206  "  C* device salvataggi notturni
059000061206  "  C*
059100061206  "  C* - obbligatorio se esecuzione salvataggi con ABP
059200061206  "  C                   If        D1DevS = *Blank and D1TipS <> '0'
059300061206  "  C                   SetOn                                        5799
059400061206  "  C                   EndIf
059500061206  "  C* - controllo esistenza device
059600061206  "  C                   If        D1DevS <> *Blank
059700061206  "  C                   Eval      $CmdLib = '*LIBL'
059800061206  "  C                   Eval      $CmdObj = D1DevS
059900061206  "  C                   Eval      $CmdType = '*DEVD'
060000061206  "  C                   ExSr      ChkObj
060100061206  "  C                   If        $ErrObj = *On
060200061206  "  C                   SetOn                                        5799
060300061206  "  C                   EndIf
060400061206X0261C                   EndIf
060500040505     C*
060600040527X0222C                   ENDSR
060700030709X0185C**********************************************************************
060800030709  "  C* Controllo validità su Programma e libreria
060900030709  "  C**********************************************************************
061000030709X0185C     CtrPgmLib     BEGSR
061100030709     C*
061200030709     C                   Eval      $ErrPgm = *Off
061300030709     C                   Eval      $ErrLib = *Off
061400030709     C*
061500030709     C* Controllo esistenza libreria
061600030709     C                   If        $Lib <> '*LIBL'
061700030709     C                   Eval      $CmdLib = 'QSYS'
061800030709     C                   Eval      $CmdObj = $Lib
061900030709     C                   Eval      $CmdType = '*LIB'
062000030709     C                   ExSr      ChkObj
062100030709     C                   Eval      $ErrLib = $ErrObj
062200030709     C                   EndIf
062300030709     C*
062400030709     C                   If        $ErrLib = *Off
062500030709     C*
062600030709     C* Controllo esistenza programma
062700030709     C                   Eval      $CmdLib = $Lib
062800030709     C                   Eval      $CmdObj = $Pgm
062900030709     C                   Eval      $CmdType = '*PGM'
063000030709     C                   ExSr      ChkObj
063100030709     C                   Eval      $ErrPgm = $ErrObj
063200030709     C*
063300030709     C                   EndIf
063400030709     C*
063500030709X0185C                   ENDSR
063600030919X0197C**********************************************************************
063700030919  "  C* Controllo validità su JobQ e libreria
063800030919  "  C**********************************************************************
063900030919X0197C     CtrJobQLib    BEGSR
064000030919     C*
064100030919     C                   Eval      $ErrJobQ = *Off
064200030919     C                   Eval      $ErrLib = *Off
064300030919     C*
064400030919     C* Controllo esistenza libreria
064500030919     C                   If        $Lib <> '*LIBL'
064600030919     C                   Eval      $CmdLib = 'QSYS'
064700030919     C                   Eval      $CmdObj = $Lib
064800030919     C                   Eval      $CmdType = '*LIB'
064900030919     C                   ExSr      ChkObj
065000030919     C                   Eval      $ErrLib = $ErrObj
065100030919     C                   EndIf
065200030919     C*
065300030919     C                   If        $ErrLib = *Off
065400030919     C*
065500030919     C* Controllo esistenza jobq
065600030919     C                   Eval      $CmdLib = $Lib
065700030919     C                   Eval      $CmdObj = $JobQ
065800030919     C                   Eval      $CmdType = '*JOBQ'
065900030919     C                   ExSr      ChkObj
066000030919     C                   Eval      $ErrJobQ = $ErrObj
066100030919     C*
066200030919     C                   EndIf
066300030919     C*
066400030919X0197C                   ENDSR
066500030709X0185C****************************************************************
066600030709  "  C* Controllo esistenza oggetto
066700030709  "  C****************************************************************
066800030709X0185C     ChkObj        BEGSR
066900030709     C*
067000030709     C                   Eval      $ErrObj = *Off
067100030709     C*
067200030709     C                   Eval      $Cmd = 'ChkObj Obj('
067300030709     C                   Cat       $CmdLib:0     $Cmd
067400030709     C                   Cat       '/':0         $Cmd
067500030709     C                   Cat       $CmdObj:0     $Cmd
067600030709     C                   Cat       ')':0         $Cmd
067700030709     C                   Cat       'OBJTYPE(':1  $Cmd
067800030709     C                   Cat       $CmdType:0    $Cmd
067900030709     C                   Cat       ')':0         $Cmd
068000030709     C*
068100030709     C                   Call      'QCMDEXC'                            22
068200030709     C                   Parm                    $Cmd
068300030709     C                   Parm      80            $CmdLen
068400030709     C*
068500030709     C                   Eval      $ErrObj = *In22
068600030709     C*
068700030709X0185C                   ENDSR
068800100621X0309C****************************************************************
068900100621  "  C* Imposto data area kpdt0
069000100621  "  C****************************************************************
069100100621X0309C     Wdw954        BEGSR
069200100621     C*
069300100621     C* richiamo programma di scrittura log
069400100621     C                   Call      'BAK954R'
069500100621     C                   Parm                    KPJBA
069600100621      *
069700100621     C                   Setoff                                       14
069800100621     C                   IN        KPDT0
069900100621     C                   If        KP0NSUFILE = '1'
070000100621     C                   Seton                                        14
070100100621     C                   Endif
070200100621      *
070300100621X0309C                   ENDSR
