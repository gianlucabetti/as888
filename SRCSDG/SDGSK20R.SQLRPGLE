000100160825     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200160825     H*PARMS BNDDIR(PJXBND)
000300160825     H*PARMS COMMIT(*NONE) DATFMT(*ISO)
000400160825     H*PARMS DYNUSRPRF(*OWNER) RDB(*NONE)
000500160825     H DECEDIT('0,') DATEDIT(*DMY.)
000600160825     F*----------------------------------------------------*
000601161020      *
000700160825      *         Emissione video avviso di utilizzo percipiente
000701161020      *
000702161020R235  * Pgm creato con il task R235 CU Percipiente in regime agevolato
000703161020R235  *
000800160825     F*----------------------------------------------------*
001200160829     FSDGSK20D  CF   E             WORKSTN
001300160825     F                                     RENAME(SK20D1:FMTD1)
001400160825     F                                     RENAME(SK20Z1:FMTZ1)
001600160825     F                                     INFDS(DSFMT)
001700160825     D*----------------------------------------------------*
001800160825     D* Schiere per la gestione dei tasti di funzione
001900160825     D $FM             S             27    DIM(24)
002000160825     D $FX             S             50    DIM(24)
002100160825     D $FC             S              1    DIM(24)
002200160825     D $FR             S              1    DIM(24)
002300160825     D* Video D1
002400160825     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002500160825     D $FX1            S             50    DIM(24) ALT($FM1)
002600160825     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
002700160825     D $FR1            S              1    DIM(24) ALT($FC1)
002800160825     D*-------------
002900160829     D* file per controllo utilizzo codice percipientie
003000160829     D SDGSPA        E DS                  EXTNAME(SDGSPA00F)
003100160829     D NDMRA         E DS                  EXTNAME(NDMRA00F)
003200160829     D ND770         E DS                  EXTNAME(ND77000F)
003300160829     D SDGN77        E DS                  EXTNAME(SDGN7700F) prefix(S77:3)
004000160825     D*-------------
004100160825     D* Passaggio Parametri
004200160825     D KPJBA         E DS
004300160825     D*-------------
004400160825     D DSFMT           DS
004500160825     D  $TASTO               369    369
004600160825     D  NRG                  370    370
004700160825     D  NCL                  371    371
004800160825     D*-------------
004900160825     D* posizione cursore
005000160825     D CURSOR          DS
005100160825     D  RIRI                   1      2B 0 INZ
005200160825     D  R$$                    2      2
005300160825     D  COCO                   3      4B 0 INZ
005400160825     D  C$$                    4      4
005500160825     D*-------------
005600160825     D* Reperimento nome PGM
005700160825     D STATUS         SDS           333
005800160825     D  DSPGM            *PROC
005900160825     D*-------------
006000160825     D* Costanti
006300160825     D* DS Esterna gestione messaggi                                            *
006400160829     D SDGMSGDS      E DS                  inz                                  *
006500160825     D  STS          E                     EXTFLD(MSGSTS)                       *
006600160825     D                                     DIM(333)                             *
006700160825     D  JBA          E                     EXTFLD(MSGJBA)                       *
006800160825     D                                     DIM(502)                             *
006900160825     D  MSGRSP       E                     INZ('S')
007000160825     D  MSGTPR       E                     INZ('SN')
007100160825     D  MSGEMV       E                     INZ('S')
007200160825     D  MSGMSG       E                     INZ(0)
007300160825     D  MSGRGP       E                     INZ(10)
007400160825     D  MSGCNL       E                     INZ('N')
007500160825     D  MSGVID       E                     INZ('N')
007600160825     D  MSGOPE       E                     INZ('N')
007700160825     D  MSGSTP       E                     INZ('N')
007800160825     D  MSGLCK       E                     INZ('N')
007900160825     D*-------------
008000160825     D* Tasti di funzione
008100160825     D F01             C                   CONST(X'31')
008200160825     D F02             C                   CONST(X'32')
008300160825     D F03             C                   CONST(X'33')
008400160825     D F04             C                   CONST(X'34')
008500160825     D F05             C                   CONST(X'35')
008600160825     D F06             C                   CONST(X'36')
008700160825     D F07             C                   CONST(X'37')
008800160825     D F08             C                   CONST(X'38')
008900160825     D F09             C                   CONST(X'39')
009000160825     D F10             C                   CONST(X'3A')
009100160825     D F11             C                   CONST(X'3B')
009200160825     D F12             C                   CONST(X'3C')
009300160825     D F13             C                   CONST(X'B1')
009400160825     D F14             C                   CONST(X'B2')
009500160825     D F15             C                   CONST(X'B3')
009600160825     D F16             C                   CONST(X'B4')
009700160825     D F17             C                   CONST(X'B5')
009800160825     D F18             C                   CONST(X'B6')
009900160825     D F19             C                   CONST(X'B7')
010000160825     D F20             C                   CONST(X'B8')
010100160825     D F21             C                   CONST(X'B9')
010200160825     D F22             C                   CONST(X'BA')
010300160825     D F23             C                   CONST(X'BB')
010400160825     D F24             C                   CONST(X'BC')
010500160825     D ENTER           C                   CONST(X'F1')
010600160825     D ROLDWN          C                   CONST(X'F4')
010700160825     D ROLLUP          C                   CONST(X'F5')
011700160825     D*-------------
012000160829     D $EsisteUno      S              1    Inz(*Off)
012100160825     D*  bottone presente
012200160825     D PRES            C                   CONST(X'A1')
012300160825     C*----------------------------------------------------*
012400160825     C*                MAIN LINE PROGRAM
012500160825     C*----------------------------------------------------*
012600160825     C*
012700160825     C* inizializzazione variabili
012800160825     C                   EXSR      INZVAR
012900160825     C*
013000160825     C     $FINE         DOWEQ     *OFF
013100160825     C     $GEST         CASEQ     'D1'          GESD1
013200160825     C                   ENDCS
013300160825     C                   ENDDO
013400160825     C*
013500160825     C* fine programma
013600160825     C                   EXSR      ENDPGM
013700160825     C/EJECT
013800160825     C************************************************************
013900160825     C* RICERCHE
014000160825     C************************************************************
014100160825     C     ENDPGM        BEGSR
014200160825     C*
014300160825     C                   SETON                                            RT
014400160825     C                   RETURN
014500160825     C*
014600160825     C                   ENDSR
014700160825     C/EJECT
014800160825     C************************************************************
014900160825     C* RICERCHE
015000160825     C************************************************************
015100160825     C     SEARCH        BEGSR
015200160825     C*
015300160825     C* determino Riga/Colonna del cursore
015400160825     C                   MOVE      NRG           R$$
015500160825     C                   MOVE      NCL           C$$
015600160825     C                   Z-ADD     RIRI          CURR
015700160825     C                   Z-ADD     COCO          CURC
015800160825     C*
015900160825     C* I campi che al momento dell'F4 risultano protetti non devono
016000160825     C* essere abilitati alla funzione di ricerca
016100160825     C*
016200160825     C                   SELECT
016300160825     C*
016400160825     C* ricerca non abilitata in questa posizione
016500160825     C                   OTHER
016600160825     C* imposto pos. del cursore
016700160825     C                   Z-ADD     CURR          H1RIGA
016800160825     C                   Z-ADD     CURC          H1COLO
016900160825     C                   SETON                                        9899
017000160825     C*
017100160825     C                   ENDSL
017200160825     C*
017300160825     C                   MOVE      *HIVAL        $LASTV
017400160825     C                   ENDSR
017500160825     C/EJECT
017600160825     C************************************************************
017700160825     C* GESTIONE EMISSIONE TASTI FUNZIONE
017800160825     C************************************************************
017900160825     C     GESPIE        BEGSR
018000160825     C*
018100160825     C                   SELECT
018200160825     C*
018300160825     C     $GEST         WHENEQ    'D1'
018400160825     C     $ULKD1        IFEQ      0
018500160825     C* riempimento opzioni
018600160825     C                   EXSR      F24D1
018700160825     C                   ELSE
018800160825     C                   WRITE     FMTZ1
018900160825     C                   ENDIF
019000160825     C*
019100160825     C                   ENDSL
019200160825     C*
019300160825     C                   ENDSR
019400160825     C/EJECT
019500160825     C************************************************************
019600160825     C* GESTIONE VIDEO RECORD D1
019700160825     C************************************************************
019800160825     C     GESD1         BEGSR
019900160825     C* inizializzazione videata
020000160825   B1C     $INZD1        IFEQ      *ON
020100160825     C                   EXSR      INZD1
020200160825     C                   MOVE      *OFF          $INZD1
020300160825   E1C                   ENDIF
020400160825     C* emissione piede videata se proveniente da altra
020500160825     C* salvataggio valore formato di provenienza
020600160825   B1C     $LASTV        IFNE      'D1'
020700160825     C*******            WRITE     FMTT1
020800160825     C                   EXSR      GESPIE
020900160825     C                   MOVE      $LASTV        $LASTG
021000160825     C                   MOVE      'D1'          $LASTV
021100160825   E1C                   ENDIF
021200160825     C*
021300160825     C* se esistono errori sulla videata
021400160825     C* emetto la write del formato a indicatori spenti per vedere
021500160825     C* le eventuali decodifiche
021600160825   B1C     *IN99         IFEQ      *ON
021700160825     C     $GEST         ANDEQ     'D1'
021800160825     C                   MOVEA     *IN           WIN
021900160825     C                   MOVE      *ALL'0'       IN5098           49
022000160825     C                   MOVEA     IN5098        *IN(50)
022100160825     C                   WRITE     FMTD1
022200160825     C                   MOVEA     WIN           *IN
022300160825   E1C                   ENDIF
022400160825     C*
022600160825     C                   EXFMT     FMTD1
023000160825     C* controllo abilitazione tasti funzione
023100160825     C*
023200160825     C                   EXSR      CTRKD1
023300160825     C*
023400160825   B1C     *IN99         IFEQ      *OFF
023500160825     C*
023600160825   B2C                   SELECT
023700160825     C* F2=Decodifica
023800160825     C     $TASTO        WHENEQ    F02
023900160825     C                   EXSR      F02D1
024000160825     C* F3=Fine
024100160825     C     $TASTO        WHENEQ    F03
024200160825     C                   EXSR      F03D1
024300160825     C* F4=Ricerca
024400160825     C     $TASTO        WHENEQ    F04
024500160825     C                   EXSR      SEARCH
024600160825     C* F5=Refresh
024700160825     C     $TASTO        WHENEQ    F05
024800160825     C                   EXSR      F05D1
024801160829     C* F6=Conferma
024802160829     C     $TASTO        WHENEQ    F06
024803160829     C                   EXSR      F06D1
024900160825     C* F8=Successivo
025000160825     C     $TASTO        WHENEQ    F08
025100160825     C                   EXSR      F08D1
025200160825     C* F12=Ritorno
025300160825     C     $TASTO        WHENEQ    F12
025400160825     C                   EXSR      F12D1
025500160825     C* F24=Altri tasti
025600160825     C     $TASTO        WHENEQ    F24
025700160825     C                   EXSR      F24D1
025800160825     C*
025900160825   X2C                   OTHER
026000160825     C                   EXSR      CTRD1
027000160825     C*
027100160825   E2C                   ENDSL
027200160825     C*
027300160825   E1C                   ENDIF
027400160825     C*
027500160825     C                   ENDSR
027600160825     C/EJECT
027700160825     C************************************************************
027800160825     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
027900160825     C************************************************************
028000160825     C     CTRKD1        BEGSR
028100160825     C                   SETOFF                                       99
028200160825     C                   MOVEA     $FC1          $FC
028300160825     C                   EXSR      CTRKEY
028400160825     C     *IN99         IFEQ      *ON
028500160825     C                   SETON                                        97
028600160825     C                   ENDIF
028601160829     C*
028700160825     C                   ENDSR
028701160829     C/EJECT
028800160829     C************************************************************
028900160829     C* INIZIALIZZAZIONE VIDEATA DATI
029000160829     C************************************************************
029100160825     C     INZD1         BEGSR
029200160825     C*
029300160825     C                   CLEAR                   FMTD1
029400160825     C* azzero i bottoni
029500160829     C                   MOVEL     X'A0'         D1SPAA
029600160825     C                   MOVEL     X'A0'         D1MRAA
030500160829     C                   Reset                   $EsisteUno
030600160829     C                   Eval      D1APEKcc    = KccK20
030601160829     C                   Eval      D1APEKsc    = KscK20
030602160829     C                   Eval      D1APEKscD   = KscDK20
030900160825     C*
031000160825     C                   Z-ADD     1             $PIED1
031001160829     C*
031100160825     C* controllo se presenti periodi a regime agevolato
031200160825     C                   EXSR      CtrRegAgev
031201160829     C*
031300160825     C* Controllo se presenti importi di ritenuta d'acconto
031400160825     C                   EXSR      CtrRitAcc
031401160829     C*
031402160829     C* Controllo se presenti movimenti in Certificazione Unica
031403160829     C                   EXSR      CtrCertUnica
031404160829     C*
031405160829     C* Controllo se presenti movimenti in 770
031406160829     C                   EXSR      CtrMov770
033100160825     C*
033200160825     C                   ENDSR
033300160825     C/EJECT
033400160825     C************************************************************
033500160825     C* Controllo esistenza periodo agevolato per percipiente
033600160825     C************************************************************
033700160829     C     CtrRegAgev    BEGSR
033800160825     C*
033900160829     C                   Eval      SqlCod = 0
034000160825     C/EXEC SQL
034100160825     C+   DECLARE A1 CURSOR FOR
034200160825     C+     SELECT  * FROM SDGSPA00F
034300160825     C+      WHERE       SPASocieta =: SocK20
034400160825     C+      AND         SPAKcc  =: KccK20
034401160825     C+      AND         SPAKsc  =: KscK20
034402160830     C+     ORDER BY SPADtIni DESC
034600160825     C/END-EXEC
034700160829     C*
034800160825     C/EXEC SQL
034900160825     C+     CLOSE A1
035000160825     C/END-EXEC
035100160829     C*
035200160825     C/EXEC SQL
035300160825     C+     OPEN A1
035400160825     C/END-EXEC
035500160829     C*
035600160825     C/EXEC SQL
035700160825     C+     FETCH A1  INTO :SDGSPA
035800160825     C/END-EXEC
035900160825     C                   If        Sqlcod = 0
036000160829     C                   Move      *On           $EsisteUno
036100160829     C                   Move      Pres          D1SPAA
036101160829     C     *JobRun       Move      SPADtIni      D1DtIni
036102160829     C                   if        SPADtFin   <> *Hival
036103160829     C     *JobRun       Move      SPADtFin      D1DtFin
037200160825     C                   endif
037201160829     C                   endif
037300160829     C*
037400160829     C                   ENDSR
037500160825     C/EJECT
037600160825     C************************************************************
037700160829     C* Controllo esistenza ritenute d'acconto
037800160825     C************************************************************
037900160829     C     CtrRitAcc     BEGSR
038000160825     C*
038100160829     C                   Eval      SqlCod = 0
038200160825     C/EXEC SQL
038300160825     C+   DECLARE A2 CURSOR FOR
038401160829     C+     SELECT  * FROM NDMRA00F
038402160829     C+      WHERE       MRASocieta =: SocK20
038403160829     C+      AND         MRAKcc  =: KccK20
038404160829     C+      AND         MRAKsc  =: KscK20
038405160830     C+     ORDER BY MRADDC DESC, MRANDC DESC, MRASerDoc
038800160825     C/END-EXEC
038900160825     c*
039000160825     C/EXEC SQL
039100160825     C+     CLOSE A2
039200160825     C/END-EXEC
039300160825     c*
039400160825     C/EXEC SQL
039500160825     C+     OPEN A2
039600160825     C/END-EXEC
039700160825     c*
039800160825     C/EXEC SQL
039900160829     C+     FETCH A2  INTO :NDMRA
040000160825     C/END-EXEC
040001160829     C                   If        Sqlcod = 0
040002160829     C                   Move      *On           $EsisteUno
040003160829     C                   Move      Pres          D1MRAA
040004160829     C     *JobRun       Move      MRADDC        D1DDC
040005160829     C                   Z-Add     MRANDC        D1NDC
040006160829     C                   MoveL     MRASerDoc     D1SerDoc
040007160829     C                   endif
040800160829     C*
040900160829     C                   ENDSR
041000160825     C/EJECT
041001160829     C************************************************************
041002160829     C* Controllo esistenza certificazione unica
041003160829     C************************************************************
041004160829     C     CtrCertUnica  BEGSR
041005160829     C*
041006160829     C                   Eval      SqlCod = 0
041007160829     C/EXEC SQL
041008160829     C+   DECLARE A3 CURSOR FOR
041009160829     C+     SELECT  * FROM ND77000F
041010160829     C+      WHERE       N77Societa =: SocK20
041011160829     C+      AND         N77Qua  =  'AU'
041012160829     C+      AND         N77Kcc  =: KccK20
041013160829     C+      AND         N77Ksc  =: KscK20
041014160829     C+     ORDER BY N77ACF DESC
041015160829     C/END-EXEC
041016160829     c*
041017160829     C/EXEC SQL
041018160829     C+     CLOSE A3
041019160829     C/END-EXEC
041020160829     c*
041021160829     C/EXEC SQL
041022160829     C+     OPEN A3
041023160829     C/END-EXEC
041024160829     c*
041025160829     C/EXEC SQL
041026160829     C+     FETCH A3  INTO :ND770
041027160829     C/END-EXEC
041028160829     C                   If        Sqlcod = 0
041029160829     C                   Move      *On           $EsisteUno
041030160829     C                   Move      Pres          D1CUA
041031160829     C                   Z-Add     N77ACF        D1AACU
041032160829     C                   endif
041033160829     C*
041034160829     C                   ENDSR
041035160829     C/EJECT
041036160829     C************************************************************
041037160829     C* Controllo esistenza movimenti 770
041038160829     C************************************************************
041039160829     C     CtrMov770     BEGSR
041040160829     C*
041041160829     C                   Eval      SqlCod = 0
041042160829     C/EXEC SQL
041043160829     C+   DECLARE A4 CURSOR FOR
041044160829     C+     SELECT  * FROM SDGN7700F
041045160829     C+      WHERE       N77Societa =: SocK20
041046160829     C+      AND         N77Qua  =  'SY'
041047160829     C+      AND         N77Kcc  =: KccK20
041048160829     C+      AND         N77Ksc  =: KscK20
041049160829     C+     ORDER BY N77ACF DESC
041050160829     C/END-EXEC
041051160829     c*
041052160829     C/EXEC SQL
041053160829     C+     CLOSE A4
041054160829     C/END-EXEC
041055160829     c*
041056160829     C/EXEC SQL
041057160829     C+     OPEN A4
041058160829     C/END-EXEC
041059160829     c*
041060160829     C/EXEC SQL
041061160829     C+     FETCH A4  INTO :SDGN77
041062160829     C/END-EXEC
041063160829     C                   If        Sqlcod = 0
041064160829     C                   Move      *On           $EsisteUno
041065160829     C                   Move      Pres          D1770A
041066160829     C                   Z-Add     S77ACF        D1AA770
041067160829     C                   endif
041068160829     C*
041069160829     C                   ENDSR
041070160829     C/EJECT
072200160825     C************************************************************
072300160825     C* GESTIONE F02 VIDEO D1
072400160825     C************************************************************
072500160825     C     F02D1         BEGSR
072600160825     C*
072700160825     C* per decodificare la videata richiamo la routine dei ctrl
072800160825     C* e poi disattivo gli eventuali messaggi di errore
072900160825     C                   EXSR      CTRD1
073000160825     C*
073100160825     C                   MOVE      *ALL'0'       IN5098
073200160825     C                   MOVEA     IN5098        *IN(50)
073300160825     C*
073400160825     C                   ENDSR
073500160825     C/EJECT
073600160825     C************************************************************
073700160825     C* GESTIONE F03 VIDEO D1
073800160825     C************************************************************
073900160825     C     F03D1         BEGSR
074000160825     C*
074100160825     C                   MOVE      *ON           $FINE
074200160829     C                   MOVE      '1'           RetK20
074300160825     C* fine programma
074400160825     C                   EXSR      ENDPGM
074500160825     C*
074600160825     C                   ENDSR
074700160825     C/EJECT
074800160825     C************************************************************
074900160825     C* GESTIONE F05 VIDEO D1
075000160825     C************************************************************
075100160825     C     F05D1         BEGSR
075200160825     C*
075300160825     C                   MOVE      *ON           $INZD1
075400160825     C*
075500160825     C                   ENDSR
075600160825     C/EJECT
075700160825     C************************************************************
075800160829     C* GESTIONE F06 VIDEO D1
075900160825     C************************************************************
076000160829     C     F06D1         BEGSR
076100160825     C*
076200160825     C                   MOVE      *ON           $FINE
076300160829     C                   MOVE      '0'           RetK20
076400160825     C*
076500160825     C                   ENDSR
076600160825     C/EJECT
076601160829     C************************************************************
076602160829     C* GESTIONE F08 VIDEO D1
076603160829     C************************************************************
076604160829     C     F08D1         BEGSR
076605160829     C*
076606160829     C                   MOVE      *ON           $FINE
076607160829     C                   MOVE      '2'           RetK20
076608160829     C*
076609160829     C                   ENDSR
076610160829     C/EJECT
076700160825     C************************************************************
076800160825     C* GESTIONE F12 VIDEO D1
076900160825     C************************************************************
077000160825     C     F12D1         BEGSR
077100160825     C*
077200160825     C                   MOVE      *ON           $FINE
077300160829     C                   MOVE      '2'           RetK20
077400160825     C*
077500160825     C                   ENDSR
077600160825     C/EJECT
077700160825     C************************************************************
077800160825     C* GESTIONE F24 VIDEO D1
077900160825     C************************************************************
078000160825     C     F24D1         BEGSR
078200160825     C*
078300160825     C                   MOVEA     $FX1          $FX
078400160825     C                   MOVEA     $FC1          $FC
078500160825     C                   MOVEA     $FR1          $FR
078600160825     C                   Z-ADD     $ULKD1        $ULK
078700160825     C                   EXSR      RIEKEY
078800160825     C                   Z-ADD     $ULK          $ULKD1            3 0
078900160825     C                   MOVEA     $FC           $FC1
079000160825     C                   MOVEL     $KEY1         Z1KE1
079100160825     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
079200160825     C                   MOVEL     $KEY2         Z1KE2
079300160825     C                   WRITE     FMTZ1
079400160825     C*
079500160825     C                   ENDSR
079600160825     C/EJECT
079700160825     C************************************************************
079800160825     C* CONTROLLO VIDEATA
079900160825     C************************************************************
080000160825     C     CTRD1         BEGSR
080100160825     C*
080200160825     C                   SETOFF                                       99
080300160825     C* non imposto pos. del cursore
080400160825     C                   Z-ADD     0             H1RIGA
080500160825     C                   Z-ADD     0             H1COLO
080600160825     C*
080700160825     C*
080800160825     C                   ENDSR
080900160825     C/EJECT
081700160825     C************************************************************
081800160825     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
081900160825     C************************************************************
082000160825     C     GESAGG        BEGSR
082100160825     C*
082400160825     C*
082500160825     C                   ENDSR
082600160825     C/EJECT
082700160825     C************************************************************
082800160825     C* OPERAZIONI INIZIALI
082900160825     C************************************************************
083000160825     C     *INZSR        BEGSR
083300160825     C*
083400160825     C* Reperimento parametri
083600160825     C     *ENTRY        PLIST
083700160829     C                   PARM                    SocK20            3
083701160829     C                   PARM                    KccK20            6
083800160829     C                   PARM                    KscK20            8
083801160829     C                   PARM                    KscDK20          20
083900160829     C                   PARM                    RetK20            1
084100160825     C*
084200160825     C* Reperimento tasti di funzione
084300160825     C*
084400160825     C                   EXSR      REPKEY
084500160825     C*
084600160825     C                   ENDSR
084700160825     C/EJECT
084800160825     C************************************************************
084900160825     C* REPERIMENTO TASTI DI FUNZIONE
085000160825     C************************************************************
085100160825     C     REPKEY        BEGSR
085200160825     C                   MOVEA     $FM1          $FM
085300160825     C                   MOVEA     $FC1          $FC
085400160825     C                   EXSR      REPKKK
085500160825     C                   MOVEA     $FX           $FX1
085600160825     C*
085700160825     C                   ENDSR
085800160825     C/EJECT
085900160825     C************************************************************
086000160825     C* REPERIMENTO TASTI DI FUNZIONE
086100160825     C************************************************************
086200160825     C     REPKKK        BEGSR
086300160825     C*
086400160825     C                   CLEAR                   $FX
086500160825     C     1             DO        24            X
086600160825     C*
086700160825     C     $FM(X)        IFNE      *BLANK
086800160825     C     $FC(X)        ANDNE     '0'
086900160825     C*
087000160825     C                   MOVEL     $FM(X)        IDMSG
087100160825     C                   CALLb     'XRTVM'
087200160825     C                   PARM                    IDMSG            27
087300160825     C                   PARM                    TXTMSG          100
087400160825     C                   PARM                    ERRMSG            1
087500160825     C*
087600160825     C     ERRMSG        IFNE      *ON
087700160825     C                   MOVEL     TXTMSG        $FX(X)
087800160825     C                   ELSE
087900160825     C                   MOVEL     *ALL' '       $FX(X)
088000160825     C                   ENDIF
088100160825     C*
088200160825     C                   ENDIF
088300160825     C*
088400160825     C                   ENDDO
088500160825     C*
088600160825     C                   ENDSR
088700160825     C/EJECT
088800160825     C*
088900160825     C************************************************************
089000160825     C* inizializzazioni
089100160825     C************************************************************
089200160825     C     INZVAR        BEGSR
089300160825     c*
089400160825     C* emetto domanda se si desidera il controllo dell'utilizzo
089500160829     C                   Reset                   SDGMSGDS
089600160825     C* Domanda
089700160829     c                   eval                    Xmsgcd1='SDC0104'
089800160825     c*
089900160829     C                   CALL      'SDGMSGR'
090000160829     C                   PARM                    SDGMSGDS
090100160825     C                   PARM                    XMSGtxt         135
090200160825     C                   PARM                    XMSGinz           1
090300160825     C                   PARM                    XMSGCd1           7
090400160825     C                   PARM                    XMSGCd2           7
090500160825     C                   PARM                    XMSGCd3           7
090600160825     C                   PARM                    XMSGSn1           7
090700160825     C                   PARM                    XMSGCdTxT        63
090800160825     C                   PARM                    MSGCdTit         14
090900160825     C                   PARM                    MSGClPrtf         1
091000160825     C                   PARM                    VarMsg1         100
091100160825     C                   PARM                    VarMsg2         100
091200160825     C                   PARM                    VarMsg3         100
091300160829     C*
091400160829     C                   if        MsgRtn = 'N'
091401160829     C* dico di cancellare il percipiente senza mostrare dove è evenutalment usato
091402160829     C                   Eval      RetK20      = '0'
091500160829     C                   EXSR      Endpgm
091600160829     C                   endif
091700160825     C*
091800160825     C* Definizione dei tasti funzione utilizzabili a pgm
091900160825     C*
092000160825     C                   EXSR      DEFOPZ
092100160825     C*
092200160825     C* Variabili per gestione videate
092300160825     C*
092400160825     C                   MOVE      *ZEROS        WIN              99
092500160825     C                   Z-ADD     0             CURR              2 0
092600160825     C                   Z-ADD     0             CURC              2 0
092700160825     C                   MOVE      'D1'          $GEST                          fmt in gestione
092800160825     C                   MOVE      *OFF          $FINE                          fine pgm
092900160825     C                   MOVE      *ON           $INZD1                         inizializzione D1
093000160825     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
093100160825     C                   MOVE      *BLANK        $LASTV                         ultima emissione
093200160825     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
093300160825     C*
093400160825     C* Variabili appoggio
093500160825     C*
093600160825     C                   Z-ADD     0             CURR
093700160825     C                   Z-ADD     0             CURC
093800160825     C*
093900160825     C* Inizializzazione/Decodifica prima videata
094000160825     C*
094100160825     C                   EXSR      INZD1
094200160825     C                   MOVE      *OFF          $INZD1
094300160825     C*
094400160825     C                   ENDSR
094500160825     C/EJECT
094600160825     C************************************************************
094700160825     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
094800160825     C************************************************************
094900160825     C     DEFOPZ        BEGSR
095000160825     C*
095100160825     C* Questa routine deve valorizzare la schiera $FC1/$FR1
095200160825     C* con S o N a seconda della utilizzabilità o meno
095300160825     C* del tasto funzione correlato
095400160825     C*
095500160825     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
095600160825     C* di compilazione
095700160825     C                   RESET                   $FC1
095800160825     C                   RESET                   $FR1
095900160825     C*
096000160825     C                   ENDSR
096100160825     C************************************************************
096200160825     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
096300160825     C************************************************************
096400160825     C     DEFCAM        BEGSR
098000160825     C*
098100160825     C* Variabili per gestione videate
098300160825     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
098400160825     C                   MOVE      *BLANK        $FINE             1            fine pgm
098500160825     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
098600160825     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
098700160825     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
098800160825     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
099100160825     C*
099200160825     C* Indici
099300160825     C*
099400160825     C                   Z-ADD     0             X                 3 0
099500160825     C                   Z-ADD     0             Y                 3 0
099600160825     C*
099700160825     C                   ENDSR
099800160825     C************************************************************
099900160825     C/EJECT
100000160825     C************************************************************
100100160825     C* CONTROLLO ABILITAZIONE TASTI
100200160825     C************************************************************
100300160825     C     CTRKEY        BEGSR
100400160825     C                   SELECT
100500160825     C     $TASTO        WHENEQ    F01
100600160825     C     $FC(1)        IFEQ      '0'
100700160825     C                   SETON                                        99
100800160825     C                   ENDIF
100900160825     C     $TASTO        WHENEQ    F02
101000160825     C     $FC(2)        IFEQ      '0'
101100160825     C                   SETON                                        99
101200160825     C                   ENDIF
101300160825     C     $TASTO        WHENEQ    F03
101400160825     C     $FC(3)        IFEQ      '0'
101500160825     C                   SETON                                        99
101600160825     C                   ENDIF
101700160825     C     $TASTO        WHENEQ    F04
101800160825     C     $FC(4)        IFEQ      '0'
101900160825     C                   SETON                                        99
102000160825     C                   ENDIF
102100160825     C     $TASTO        WHENEQ    F05
102200160825     C     $FC(5)        IFEQ      '0'
102300160825     C                   SETON                                        99
102400160825     C                   ENDIF
102500160825     C     $TASTO        WHENEQ    F06
102600160825     C     $FC(6)        IFEQ      '0'
102700160825     C                   SETON                                        99
102800160825     C                   ENDIF
102900160825     C     $TASTO        WHENEQ    F07
103000160825     C     $FC(7)        IFEQ      '0'
103100160825     C                   SETON                                        99
103200160825     C                   ENDIF
103300160825     C     $TASTO        WHENEQ    F08
103400160825     C     $FC(8)        IFEQ      '0'
103500160825     C                   SETON                                        99
103600160825     C                   ENDIF
103700160825     C     $TASTO        WHENEQ    F09
103800160825     C     $FC(09)       IFEQ      '0'
103900160825     C                   SETON                                        99
104000160825     C                   ENDIF
104100160825     C     $TASTO        WHENEQ    F10
104200160825     C     $FC(10)       IFEQ      '0'
104300160825     C                   SETON                                        99
104400160825     C                   ENDIF
104500160825     C     $TASTO        WHENEQ    F11
104600160825     C     $FC(11)       IFEQ      '0'
104700160825     C                   SETON                                        99
104800160825     C                   ENDIF
104900160825     C     $TASTO        WHENEQ    F12
105000160825     C     $FC(12)       IFEQ      '0'
105100160825     C                   SETON                                        99
105200160825     C                   ENDIF
105300160825     C     $TASTO        WHENEQ    F13
105400160825     C     $FC(13)       IFEQ      '0'
105500160825     C                   SETON                                        99
105600160825     C                   ENDIF
105700160825     C     $TASTO        WHENEQ    F14
105800160825     C     $FC(14)       IFEQ      '0'
105900160825     C                   SETON                                        99
106000160825     C                   ENDIF
106100160825     C     $TASTO        WHENEQ    F15
106200160825     C     $FC(15)       IFEQ      '0'
106300160825     C                   SETON                                        99
106400160825     C                   ENDIF
106500160825     C     $TASTO        WHENEQ    F16
106600160825     C     $FC(16)       IFEQ      '0'
106700160825     C                   SETON                                        99
106800160825     C                   ENDIF
106900160825     C     $TASTO        WHENEQ    F17
107000160825     C     $FC(17)       IFEQ      '0'
107100160825     C                   SETON                                        99
107200160825     C                   ENDIF
107300160825     C     $TASTO        WHENEQ    F18
107400160825     C     $FC(18)       IFEQ      '0'
107500160825     C                   SETON                                        99
107600160825     C                   ENDIF
107700160825     C     $TASTO        WHENEQ    F19
107800160825     C     $FC(19)       IFEQ      '0'
107900160825     C                   SETON                                        99
108000160825     C                   ENDIF
108100160825     C     $TASTO        WHENEQ    F20
108200160825     C     $FC(20)       IFEQ      '0'
108300160825     C                   SETON                                        99
108400160825     C                   ENDIF
108500160825     C     $TASTO        WHENEQ    F21
108600160825     C     $FC(21)       IFEQ      '0'
108700160825     C                   SETON                                        99
108800160825     C                   ENDIF
108900160825     C     $TASTO        WHENEQ    F22
109000160825     C     $FC(22)       IFEQ      '0'
109100160825     C                   SETON                                        99
109200160825     C                   ENDIF
109300160825     C     $TASTO        WHENEQ    F23
109400160825     C     $FC(23)       IFEQ      '0'
109500160825     C                   SETON                                        99
109600160825     C                   ENDIF
109700160825     C     $TASTO        WHENEQ    F24
109800160825     C     $FC(24)       IFEQ      '0'
109900160825     C                   SETON                                        99
110000160825     C                   ENDIF
110100160825     C                   ENDSL
110200160825     C                   ENDSR
110300160825     C/EJECT
110400160825     C************************************************************
110500160825     C* RIEMPIMENTO TASTI FUNZIONE
110600160825     C************************************************************
110700160825     C     RIEKEY        BEGSR
110800160825     C*
110900160825     C* Input :
111000160825     C* - $FX = schiera con tutti i testi disponibili
111100160825     C* - $FC = schiere con i flag di validità associati ai testi
111200160825     C* - $FR = schiere con i flag di REVERSE IMAGE
111300160825     C* - $ULK = ultimo testo caricato
111400160825     C* - $L01 = Lunghezza prima riga
111500160825     C* - $L02 = Lunghezza seconda riga
111600160825     C* Output :
111700160825     C* - $ULK
111800160825     C* - $KEY1= 1° riga di tasti funzione
111900160825     C* - $KEY2= 2° riga di tasti funzione
112000160825     C*
112100160825     C                   CALLb     'XKEYMSG'
112200160825     C* input
112300160825     C                   PARM                    $FX
112400160825     C                   PARM                    $FC
112500160825     C                   PARM                    $FR
112600160825     C                   PARM                    $ULK              3 0
112700160825     C* output
112800160825     C                   PARM                    $KEY1            79
112900160825     C                   PARM                    $KEY2            79
113000160825     C* input
113100160825     C                   PARM      79            $L01              3 0
113200160825     C                   PARM      79            $L02              3 0
113300160825     C*
113400160829     C                   PARM                    $ALLFUNCT       480
113500160829     C*
113600160825     C                   ENDSR
113601160829     C/EJECT
113700160825** TASTI DI FUNZIONE  D1
113800160825PROMSG    *LIBL     KEY0001  01
113900160825PROMSG    *LIBL     KEY0002  02
114000160825PROMSG    *LIBL     KEY0003  03
114100160825PROMSG    *LIBL     KEY0004  04
114200160825PROMSG    *LIBL     KEY0005  05
114300160825PROMSG    *LIBL     KEY0006  06
114400160825PROMSG    *LIBL     KEY0007  07
114500160825PROMSG    *LIBL     KEY0008  08
114600160825PROMSG    *LIBL     KEY0009  09
114700160825PROMSG    *LIBL     KEY0010  10
114800160825PROMSG    *LIBL     KEY0011  11
114900160825PROMSG    *LIBL     KEY0012  12
115000160825PROMSG    *LIBL     KEY0013  13
115100160825PROMSG    *LIBL     KEY0014  14
115200160825PROMSG    *LIBL     KEY0015  15
115300160825PROMSG    *LIBL     KEY0016  16
115400160825PROMSG    *LIBL     KEY0017  17
115500160825PROMSG    *LIBL     KEY0018  18
115600160825PROMSG    *LIBL     KEY0019  19
115700160825PROMSG    *LIBL     KEY0020  20
115800160825PROMSG    *LIBL     KEY0021  21
115900160825PROMSG    *LIBL     KEY0022  22
116000160825PROMSG    *LIBL     KEY0023  23
116100160825PROMSG    *LIBL     KEY0024  24
116200160825**  ABILITAZIONE D1
1163001608250N  01
1164001608250N  02
1165001608250N  03
1166001608250N  04
1167001608250N  05
1168001608291N  06
1169001608250N  07
1170001608290N  08
1171001608250N  09
1172001608250N  10
1173001608250N  11
1174001608251N  12
1175001608250N  13
1176001608250N  14
1177001608250N  15
1178001608250N  16
1179001608250N  17
1180001608250N  18
1181001608250N  19
1182001608250N  20
1183001608250N  21
1184001608250N  22
1185001608250N  23   Impostare a '1' se si utilizzano opzioni
1186001608251N  24   Lasciare sempre impostata a '1' il valore della riga 24
