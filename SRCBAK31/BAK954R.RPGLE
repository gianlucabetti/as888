000100021115     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES)
000200021115     H*PARMS CVTOPT(*NONE)
000300021115     H DECEDIT('0,') DATEDIT(*DMY/)
000400930908     H*---------------------------------------------------------------*
000500100607     H* BAK954R                                                       *
000600930908     H*                                                               *
000700100607     H* MANUTENZIONE AREA DATI DI CONTROLLO A.B.P.   kpdt0            *
000800930908     H*                                                               *
000900010810     H*---------------------------------------------------------------*
001000930908     H*                                                               *
001100930908     H* 31  AREA DATI ALLOCATA DA UN ALTRO LAVORO                     *
001200930908     H*                                                               *
001300930908     H*---------------------------------------------------------------*
001400100608X0309 * File numero azione
001500100621X0309FKpNaz01L  IF A E           K DISK    usropn
001600100608      *
001700100607     FBAK954D   CF   E             WORKSTN
001800100607     F                                     INFDS(DSFMT)
001900031002     D*-------------
002000930908     D KPJBA         E DS
002100031002     D*-------------
002200100607     D KPDT0         E DS           100
002300100607     D savKPDT0        S                   Like(KPDT0)
002400100607     D*-------------
002500100607     D* REPERISCO NOME PGM
002600100607     D STATUS         SDS           333
002700100607     D  DSPGM            *PROC
002800100608X0309D*-------------
002900100608  "  D NrAzione        DS                  Inz
003000100608  "  D  DsSerie                1      1  0
003100100608  "  D  DsNr                   2      9  0
003200100608X0309D  DsNrAz                 1      9  0
003300100608  "  D SkNrAz          S              9S 0 Inz Dim(10)
003400100607     D*-------------
003500100607     D ROLLDOWN        C                   CONST(X'F4')
003600100607     D ROLLUP          C                   CONST(X'F5')
003700100607     D*-------------
003800100607     D DSFMT           DS
003900100607     D  $TASTO               369    369
004000100607     D  NRG                  370    370
004100100607     D  NCL                  371    371
004200100607     D*-------------
004300100607     D* posizione cursore
004400100607     D CURSOR          DS
004500100607     D  RIRI                   1      2B 0 INZ
004600100607     D  R$$                    2      2
004700100607     D  COCO                   3      4B 0 INZ
004800100607     D  C$$                    4      4
004900100607     D*-------------
005000100607     D CurR            S              2S 0
005100100607     D CurC            S              2S 0
005200100607     D*-------------
005300100607     D* Campi di lavoro
005400930908     C*----------------------------------------------------*
005500930908     C*-  FLUSSO PRINCIPALE                               -*
005600930908     C*----------------------------------------------------*
005700100607      *
005800100607     C                   Exsr      Inzvar
005900100607      *
006000930908     C     $FINE         DOWEQ     *OFF
006100930908     C     $GEST         CASEQ     'D01'         GESD01
006200930908     C                   ENDCS
006300930908     C                   ENDDO
006400100607      *
006500930908     C                   SETON                                          LR
006600100607      *
006700930908     C*---GESD01  -----------------------------------------*
006800040505     C*-   GESTIONE D1                                    -*
006900930908     C*----------------------------------------------------*
007000930908     C     GESD01        BEGSR
007100930908     C*
007200100607     C                   EXFMT     K954D1
007300930908     C*
007400030709     C                   SELECT
007500930908     C* F3=FINE LAVORO
007600930908     C     *INKC         WHENEQ    *ON
007700100607     C* F12=RITORNO
007800100607     C     *INKL         OREQ      *ON
007900930908     C                   MOVE      *ON           $FINE
008000100607     C* F4=RICERCHE
008100100607     C     *INKD         WHENEQ    *ON
008200100607     C                   EXSR      SEARCH
008300100607     C* Controlli
008400100607     C                   OTHER
008500100607     C                   ExSr      CtrD1
008600100607     C                   If        *In99 = *Off
008700100607     C* F6=CONFERMA
008800100607     C     *INKF         IFEQ      *ON
008900100621     C                   ExSr      WrtKpnaz
009000100621     C                   ExSr      AggKPDT0
009100100607     C                   If        *In99 = *Off
009200100607     C                   MOVE      *ON           $FINE
009300100607     C                   EndIf
009400100607     C                   ENDIF
009500100607     C                   EndIf
009600100607     C                   ENDSL
009700100607     C*
009800030709     C*
009900930908     C                   ENDSR
010000100607     C*----------------------------------------------------*
010100100607     C*- Aggiornamento data area KPDT0                    -*
010200100607     C*----------------------------------------------------*
010300100607     C     AggKPDT0      BEGSR
010400040505     C*
010500040505     C* ALLOCAZIONE DTAARA
010600100607     C     *LOCK         IN        KPDT0                                31
010700100607     C                   Eval      savKPDT0 = KPDT0
010800100607     C*
010900040505     C* RIUSCITA ALLOCAZIONE
011000040505     C     *IN31         IFEQ      *OFF
011100100607     C                   MOVE      D1NSUFILE     KP0NSUFILE
011200100607     C                   MOVE      D1SERIEAT     KP0SERIEAT
011300040505     C* AGGIORNAMENTO DTAARA
011400100607     C                   OUT       KPDT0
011500100607     C*
011600040505     C* NON RIUSCITA ALLOCAZIONE
011700040505     C                   ELSE
011800040505     C                   SETON                                        9799
011900040505     C                   ENDIF
012000040505     C*
012100100607     C                   ENDSR
012200100607     C****************************************************************
012300100607     C* RICERCHE
012400100607     C****************************************************************
012500100607     C     SEARCH        BEGSR
012600031002     C*
012700031002     C* determino Riga/Colonna del cursore
012800031002     C                   MOVE      NRG           R$$
012900031002     C                   MOVE      NCL           C$$
013000031002     C                   Z-ADD     RIRI          CURR
013100031002     C                   Z-ADD     COCO          CURC
013200031002     C*
013300031002     C                   SELECT
013400031002     C*
013500031002     C* ricerca non abilitata in questa posizione
013600031002     C                   OTHER
013700031002     C                   SETON                                        9899
013800031002     C*
013900031002     C                   ENDSL
014000031002     C*
014100031002     C* imposto pos. del cursore
014200100607     C***                Z-ADD     CURR          H1RIGA
014300100607     C***                Z-ADD     CURC          H1COLO
014400031002     C*
014500100607     C                   ENDSR
014600100607     C****************************************************************
014700100607     C* INIZIALIZZAZIONE TESTATA DEL VIDEO
014800100607     C****************************************************************
014900100607     C     INZD1         BEGSR
015000100607     C*
015100100621      * inizializzo la videata
015200100621     C                   Eval      D1Ser0     =  0
015300100621     C                   Eval      D1Ser1     =  1
015400100621     C                   Eval      D1Ser2     =  2
015500100621     C                   Eval      D1Ser3     =  3
015600100621     C                   Eval      D1Ser4     =  4
015700100621     C                   Eval      D1Ser5     =  5
015800100621     C                   Eval      D1Ser6     =  6
015900100621     C                   Eval      D1Ser7     =  7
016000100621     C                   Eval      D1Ser8     =  8
016100100621     C                   Eval      D1Ser9     =  9
016200100621     C*
016300100621      * Carico da area dati kpdt0
016400100621     C     *DTAARA       DEFINE                  KPDT0
016500100621     C                   IN        KPDT0
016600100607     C                   MOVE      KP0NSUFILE    D1NSUFILE
016700100607     C                   MOVE      KP0SERIEAT    D1Serieat
016800100621      *
016900100621      * Se non gestito ancora il num su file permetto il passaggio
017000100621      * altrimenti proteggo tutto e carico il file in visualizzazione
017100100621     C                   Move      *off          *in02
017200100621     C                   If        KP0NSUFILE = *on
017300100621     C                   Move      *on           *in02
017400100607     C*
017500100608      * Reperisco il numeratore delle azioni
017600100608 "   C                   Exsr      RepKpNaz
017700100621      *
017800100621     C                   Move      SkNrAZ(1)     DsNrAZ
017900100621     C                   Eval      D1Ser0     =  DsSerie
018000100621     C                   Eval      D1NrSerie0 =  DsNr
018100100621      *
018200100621     C                   Move      SkNrAZ(2)     DsNrAZ
018300100621     C                   Eval      D1Ser1     =  DsSerie
018400100621     C                   Eval      D1NrSerie1 =  DsNr
018500100621      *
018600100621     C                   Move      SkNrAZ(3)     DsNrAZ
018700100621     C                   Eval      D1Ser2     =  DsSerie
018800100621     C                   Eval      D1NrSerie2 =  DsNr
018900100621      *
019000100621     C                   Move      SkNrAZ(4)     DsNrAZ
019100100621     C                   Eval      D1Ser3     =  DsSerie
019200100621     C                   Eval      D1NrSerie3 =  DsNr
019300100621      *
019400100621     C                   Move      SkNrAZ(5)     DsNrAZ
019500100621     C                   Eval      D1Ser4     =  DsSerie
019600100621     C                   Eval      D1NrSerie4 =  DsNr
019700100621      *
019800100621     C                   Move      SkNrAZ(6)     DsNrAZ
019900100621     C                   Eval      D1Ser5     =  DsSerie
020000100621     C                   Eval      D1NrSerie5 =  DsNr
020100100621      *
020200100621     C                   Move      SkNrAZ(7)     DsNrAZ
020300100621     C                   Eval      D1Ser6     =  DsSerie
020400100621     C                   Eval      D1NrSerie6 =  DsNr
020500100621      *
020600100621     C                   Move      SkNrAZ(8)     DsNrAZ
020700100621     C                   Eval      D1Ser7     =  DsSerie
020800100621     C                   Eval      D1NrSerie7 =  DsNr
020900100621      *
021000100621     C                   Move      SkNrAZ(9)     DsNrAZ
021100100621     C                   Eval      D1Ser8     =  DsSerie
021200100621     C                   Eval      D1NrSerie8 =  DsNr
021300100621      *
021400100621     C                   Move      SkNrAZ(10)    DsNrAZ
021500100621     C                   Eval      D1Ser9     =  DsSerie
021600100621     C                   Eval      D1NrSerie9 =  DsNr
021700100621     C                   Endif
021800100621     C*
021900100621     C                   ENDSR
022000100621X0309C************************************************************
022100100621 "   C* Scrivo kpnaz
022200100621 "   C************************************************************
022300100621X0309C     WrtKpNaz      BEGSR
022400100621      *
022500100621      * Reperimento file
022600100621     C                   if        not %open(KpNaz01L)
022700100621     C                   Open      KpNaz01L
022800100621     C                   endif
022900100621      *
023000100621     C                   Movel     D1Ser0        NazSerie
023100100621     C                   Movel     D1NrSerie0    NazNr
023200100621     C                   Write     Kpnaz000
023300100621      *
023400100621     C                   Movel     D1Ser1        NazSerie
023500100621     C                   Movel     D1NrSerie1    NazNr
023600100621     C                   Write     Kpnaz000
023700100621      *
023800100621     C                   Movel     D1Ser2        NazSerie
023900100621     C                   Movel     D1NrSerie2    NazNr
024000100621     C                   Write     Kpnaz000
024100100621      *
024200100621     C                   Movel     D1Ser3        NazSerie
024300100621     C                   Movel     D1NrSerie3    NazNr
024400100621     C                   Write     Kpnaz000
024500100621      *
024600100621     C                   Movel     D1Ser4        NazSerie
024700100621     C                   Movel     D1NrSerie4    NazNr
024800100621     C                   Write     Kpnaz000
024900100621      *
025000100621     C                   Movel     D1Ser5        NazSerie
025100100621     C                   Movel     D1NrSerie5    NazNr
025200100621     C                   Write     Kpnaz000
025300100621      *
025400100621     C                   Movel     D1Ser6        NazSerie
025500100621     C                   Movel     D1NrSerie6    NazNr
025600100621     C                   Write     Kpnaz000
025700100621      *
025800100621     C                   Movel     D1Ser7        NazSerie
025900100621     C                   Movel     D1NrSerie7    NazNr
026000100621     C                   Write     Kpnaz000
026100100621      *
026200100621     C                   Movel     D1Ser8        NazSerie
026300100621     C                   Movel     D1NrSerie8    NazNr
026400100621     C                   Write     Kpnaz000
026500100621      *
026600100621     C                   Movel     D1Ser9        NazSerie
026700100621     C                   Movel     D1NrSerie9    NazNr
026800100621     C                   Write     Kpnaz000
026900100621      *
027000100607     C                   ENDSR
027100100608X0309C************************************************************
027200100608 "   C* Lettura KpNaz
027300100608 "   C************************************************************
027400100608X0309C     RepKpNaz      BEGSR
027500100608      *
027600100608     C                   Clear                   SKNrAz
027700100621     C                   Clear                   i                 3 0
027800100608      *
027900100608      * Reperimento file
028000100608     C                   if        not %open(KpNaz01L)
028100100608     C                   Open      KpNaz01L
028200100608     C                   endif
028300100608      *
028400100608     C     *loval        SETLL     Kpnaz01L
028500100608     C                   Do        *hival
028600100608     C                   Read      KpNaz01L                             2221
028700100608     C                   If        %Eof(KpNaz01L)
028800100608     C                   Leave
028900100608     C                   Endif
029000100608      *
029100100608      * Preparo il nr azione per l'esterno
029200100608     C                   Z-ADD     NazSerie      DsSerie
029300100608     C                   Z-ADD     NazNr         DsNr
029400100608      *
029500100608     C                   Add       1             i
029600100608     C                   Move      DsNrAz        SkNrAZ(i)
029700100608     C                   Enddo
029800100608      *
029900100608X0309C                   ENDSR
030000100607     C****************************************************************
030100100607     C* INIZIO PGM
030200100607     C****************************************************************
030300930908     C     *INZSR        BEGSR
030400100607      *
030500930908     C     *ENTRY        PLIST
030600930908     C                   PARM                    KPJBA
030700100607      *
030800930908     C                   MOVE      *OFF          $FINE             1            fine lavoro
030900930908     C                   MOVE      'D01'         $GEST             3            video da gestir
031000930908     C                   MOVE      *ON           $INZS1            1
031100100607      *
031200100607     C                   ENDSR
031300100607      *
031400100607     C****************************************************************
031500100607     C* INIZIO PGM
031600100607     C****************************************************************
031700100607     C     inzvar        BEGSR
031800100607      *
031900100607      * Verifico se esiste l'area dati kpdt0 e nel caso la creo
032000100607     C                   EXSR      CheckDT0
032100100607      *
032200100607      * INIZIALIZZAZIONE VIDEO
032300100607     C                   EXSR      INZD1
032400100607      *
032500930908     C                   ENDSR
032600100607     C****************************************************************
032700100607     C* CheckDT0
032800100607     C****************************************************************
032900100607     C     CheckDT0      BEGSR
033000100607     C*
033100100607     C* CHKOBJ OBJ(*LIBL/KPDT0) OBJTYPE(*DTAARA)
033200100607     C                   Eval      Cmd ='CHKOBJ OBJ(*LIBL/KPDT0) +
033300100607     C                               OBJTYPE(*DTAARA) '
033400100607      *
033500100607     C                   Call      'QCMDEXC'                            21
033600100609     C                   Parm                    Cmd             100
033700100609     C                   Parm      100           LeN              15 5
033800100607      *
033900100607      * Se non esiste la creo
034000100607     C                   If        *in21 = *on
034100100607      * CRTDTAARA  DTAARA(PROBAS/KPDT0) TYPE(*CHAR) LEN(100)
034200100609     C                   Eval      Cmd ='CRTDTAARA DTAARA(PROBAS/KPDT0) +
034300100609     C                               TYPE(*CHAR) LEN(100) VALUE(''00'') +
034400100609     C                               TEXT(''Numeratore azioni'') '
034500100607     C                   Call      'QCMDEXC'                            21
034600100609     C                   Parm                    Cmd             100
034700100609     C                   Parm      100           LeN              15 5
034800100607     C                   Endif
034900100607     C*
035000100607     C                   ENDSR
035100100607     C****************************************************************
035200100607     C* CONTROLLI VIDEO D1
035300100607     C****************************************************************
035400100607     C     CTRD1         BEGSR
035500030709     C*
035600030709     C                   Eval      *In99 = *Off
035700100607     C*
035800100607     C* Valore obbligatorio
035900100607     C*
036000100621     C                   If        D1NSUFILE <> '1' and D1NSUFILE <> '0'
036100100607     C                   SetOn                                        5099
036200100607     C                   EndIf
036300030709     C*
036400100607     C                   ENDSR
