000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300020207     H* Verifica Agenti "Unificanti"
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900011206     Ftis7c2P   O    E             PRINTER OFLIND(*IN05)
001000140616     FAZCMM01L  IF   E           K DISK
001100970526     D*------------------------------------------------------------------------*
001200970526     D* SCHIERE
001300970526     D*------------------------------------------------------------------------*
001400991214     D*------------
001500970526     D* ARCHITETTURA
001600970526     D*------------
001700970526     D KPJBA         E DS
001800000310     D*---------------------
001900000310     D* DS PER REPERIRE I PARAMETRI UTENTE
002000000310     D*---------------------
002100000310     D CARCNT          DS
002200000310     D  FL12CN                 1      2
002300000310     D  FL34CN                 3      4
002400000310     D  TIPCNT                 5      6
002500000310     D  FLU                   66     97
002600000310     D                                     DIM(32)                              FLAG PAR.UTENT
002700000310     D  TCU                  398    697
002800000310     D                                     DIM(50)                              *TAB FLG 8  TP.CN
002900000310     D  KCU                  698    847P 0
003000000310     D                                     DIM(50)                              *TAB CAPICONTO
003100000310     D                                     PACKEVEN
003200991214     D*------------------
003300991214     D* DS X PASSAGGIO PARAMETRI
003400991214     D*------------------
003500991214     D PARAM           DS                  INZ
003600011206     D  PARORD                 1      1
003700000929     D*-------------------
003800011206     D* DS DI PROCEDURA
003900000929     D*-------------------
004000020207     D DSSQL           DS
004100140616     D  AGEUNI                        7  0
004200020207     D  DESCR                        25
004300140616     D  CODAGE                        7  0
004400011206     D*-------------------
004500011206     D* VARIABILI DI WRK
004600011206     D*-------------------
004700011206     D  SQLline        s           5000    varying
004800011206     D  countAgf       s              9  0 INZ(*zeros)
004900011206     D  wrkAge         s              7  0 INZ(*zeros)
005000970526     C*------------------------------------------------------------------------*
005100991214     C* MAIN LINES
005200970526     C*------------------------------------------------------------------------*
005300970526     C*
005400991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
005500991214     C                   EXSR      ESEGUI
005600000306     C*
005700970526     C                   SETON                                        LR
005800970526     C*------------------------------------------------------------------------*
005900991217     C* ESEGUI - OPERAZIONI PRINCIPALI
006000970526     C*------------------------------------------------------------------------*
006100991214     C     ESEGUI        BEGSR
006200011205     C*
006300011205     C* Poi stampo l'intestazione
006400011205     C                   seton                                        05
006500011205     C                   exsr      stpTES
006600011205     C*
006700011205     C* Quindi la routine di elaborazione
006800011206     C                   exsr      exeAGEUNI
006900011205     C*
007000011205     C* Al termine chiudo il prospetto
007100011206     C                   write     s7c2RI
007200011206     C                   write     s7c2FI
007300970526     C*
007400970526     C                   ENDSR
007500011205     C*------------------------------------------------------------------------*
007600011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
007700011205     C*------------------------------------------------------------------------*
007800011205     C     STPTES        BEGSR
007900011205     C*
008000011205     C                   if        *in05 = *on
008100011206     C                   write     s7c2TE
008200011205     C                   setoff                                       05
008300011205     C                   endif
008400011205     C*
008500011205     C                   ENDSR
008600000929     C*------------------------------------------------------------------------*
008700140616     C* EXEAGEUNI - Routine di selezione record Agenti Unificanti ordinati secondo lancio
008800000929     C*------------------------------------------------------------------------*
008900011206     C     EXEAGEUNI     BEGSR
009000011206     C*
009100140616     C                   movel     *loval        depAgp           25
009200140616     C* Costruisco la stringa SQL a seconda dei parametri ricevuti dal lancio
009300140616     C                   EVAL      SQLLINE = 'select cmmuni as ageuni, '       +
009400140616     C                                       'cmmdes as descr, '               +
009500140616     C                                       'cmmcod '                         +
009600140617     C                                       'from azcmm01l '                  +
009700140617     C                                       'where cmmatb = '' '''
009800140616     C*
009900140616     C                   if        PARORD = 'D'
010000140616     C                   eval      SQLline = %trim(SQLline)+' order by descr,'+
010100140616     C                                       ' ageuni, cmmcod'
010200140616     C                   else
010300140616     C                   eval      SQLline = %trim(SQLline)+' order by ageuni'+
010400140616     C                                       ', cmmcod'
010500140616     C                   endif
010600140616     C*
010700140616     C/EXEC SQL
010800140616     C+ prepare S1 from :SQLline
010900140616     C/END-EXEC
011000140616     C*
011100140616     C*
011200140616     C/EXEC SQL
011300140616     C+ declare C1 cursor for S1
011400140616     C/END-EXEC
011500140616     C*
011600140616     C*
011700140616     C/EXEC SQL
011800140616     C+ open C1
011900140616     C/END-EXEC
012000140616     C
012100140616     C/EXEC SQL
012200140616     C+ Fetch C1 into :DSSQL
012300140616     C/END-EXEC
012400140616     C*
012500140616     C                   dow       sqlcod = *zeros
012600140616     C                   exsr      exePRTP
012700140616     C*
012800140616     C/EXEC SQL
012900140616     C+ Fetch C1 into :DSSQL
013000140616     C/END-EXEC
013100140616     C*
013200140616     C                   enddo
013300140616     C*
013400140616     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
013500140616     C                   exsr      exePRTP
013600140616     C*
013700140616     C/EXEC SQL
013800140616     C+ close C1
013900140616     C/END-EXEC
014000140616     C
014100140616     C*
014200140616     C                   ENDSR
014300011205     C*------------------------------------------------------------------------*
014400011205     C* EXEPRTP - STAMPA IL DETTAGLIO PADRI
014500011205     C*------------------------------------------------------------------------*
014600011205     C     EXEPRTP       BEGSR
014700020207     C*
014800020207     C* "Numerizzo" i campi alfa reperiti dalla fetch SQL
014900020207     C                   movel     ageuni        Nageuni           7 0
015000011206     C*
015100011206     C* Verifico l'avvenuta rottura x agente "padre"
015200020207     C                   if        PARORD = 'D'
015300140616     C                   eval      WdepAgp = %editc(ageuni:'Z') + descr
015400020207     C                   else
015500140616     C                   movel     *blank        WdepAgp          25
015600140616     C                   eval      %subst(WdepAgp:1:7) = %editc(ageuni:'Z')
015700140616     C                   endif
015800020207     C*
015900020207     C                   if        WdepAgp = depAgp
016000011206     C                   eval      countAgf = countAgf + 1
016100011205     C*
016200011206     C* Eseguo operazioni per gli agenti "figli" legati
016300011205     C                   exsr      exePRTF
016400011205     C                   else
016500011206     C*
016600011206     C* Verifico se ci sono ancora dei figli da stampare del padre precedente
016700011206     C                   if        wrkPosAgf > 0
016800011206     C                   exsr      stpTES
016900011206     C                   write     s7c2DE
017000011206     C                   clear                   s7c2DE
017100011206     C                   endif
017200011206     C*
017300020207     C* A rottura dell'agente "padre" lascio una riga vuota e re-inizializzo il contatore
017400011206     C                   eval      countAgf = 1
017500011205     C*
017600140616     C* Reperisco le informazioni dell'agente "padre"
017700020207     C                   if        PARORD = 'D'
017800140616     C                   eval      depAgp = %editc(ageuni:'Z') + descr
017900020207     C                   else
018000140616     C                   movel     *blank        depAgp
018100140616     C                   eval      %subst(depAgp:1:7) = %editc(ageuni:'Z')
018200020207     C                   endif
018300020207     C*
018400020207     C                   movel(p)  ageuni        wrkAge
018500011205     C                   exsr      repinfo
018600020207     C                   eval      stpCOP = Nageuni
018700020207     C                   eval      stpDCOP = descr
018800011205     C*
018900011206     C* Eseguo operazioni per gli agenti "figli" legati
019000011205     C                   exsr      exePRTF
019100011205     C                   endif
019200011205     C
019300011205     C*
019400011205     C                   ENDSR
019500011205     C*------------------------------------------------------------------------*
019600011205     C* EXEPRTF - STAMPA IL DETTAGLIO FIGLI
019700011205     C*------------------------------------------------------------------------*
019800011205     C     EXEPRTF       BEGSR
019900011205     C*
020000020207     C                   movel(p)  codage        wrkAge
020100011205     C                   exsr      repinfo
020200011205     C*
020300020207     C     countAgf      DIV       4             wrkNumAgf         9 0
020400011206     C                   MVR                     wrkPosAgf         1 0
020500011205     C*
020600011206     C                   if        wrkPosAgf = 1
020700140616     C                   movel     CMMCod        stpCOF1
020800140616     C                   eval      stpDCOF1 = CMMDes
020900140616     C                   eval      stpPIT1 = CMMPLI
021000140616     C                   eval      stpPES1 = CMMPLE
021100011205     C                   endif
021200011206     C*
021300011206     C                   if        wrkPosAgf = 2
021400140616     C                   movel     CMMCod        stpCOF2
021500140616     C                   eval      stpDCOF2 = CMMDes
021600140616     C                   eval      stpPIT2 = CMMPLI
021700140616     C                   eval      stpPES2 = CMMPLE
021800011206     C                   endif
021900011206     C*
022000011206     C                   if        wrkPosAgf = 3
022100140616     C                   movel     CMMCod        stpCOF3
022200140616     C                   eval      stpDCOF3 = CMMDes
022300140616     C                   eval      stpPIT3 = CMMPLI
022400140616     C                   eval      stpPES3 = CMMPLE
022500011206     C                   endif
022600011206     C*
022700011206     C                   if        wrkPosAgf = 0
022800140616     C                   movel     CMMCod        stpCOF4
022900140616     C                   eval      stpDCOF4 = CMMDes
023000140616     C                   eval      stpPIT4 = CMMPLI
023100140616     C                   eval      stpPES4 = CMMPLE
023200011206     C                   endif
023300011205     C*
023400011206     C                   if        wrkPosAgf = 0
023500011206     C                   exsr      stpTES
023600011206     C                   write     s7c2DE
023700011206     C                   clear                   s7c2DE
023800011206     C                   endif
023900011206     C*
024000011205     C                   ENDSR
024100011205     C*------------------------------------------------------------------------*
024200140616     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIE
024300011205     C*------------------------------------------------------------------------*
024400011205     C     REPINFO       BEGSR
024500011205     C*
024600011206     C* Reperisco le informazioini dell'agente commerciale
024700140616     C                   EVAL      CMMCod = wrkAge
024800140616     C     CMMCod        CHAIN     AZCMM01l
024900140616     C                   IF        not %found(AZCMM01L)
025000140616     C                   RESET                   AZCMM000
025100011205     C                   ENDIF
025200011206     C*
025300011205     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500970526     C* *INZSR - OPERAZIONI INIZIALI
025600970526     C*------------------------------------------------------------------------*
025700970526     C     *INZSR        BEGSR
025800970526     C*---
025900970526     C* RICEVIMENTO PARAMETRI
026000970526     C*---
026100970526     C     *ENTRY        PLIST
026200970612     C                   PARM                    KPJBA
026300991214     C                   MOVEL     KPJBU         PARAM
026400970526     C*---
026500991214     C* CHIAVI DI LETTURA
026600991214     C*---
026700991214     C*
026800970526     C                   ENDSR
