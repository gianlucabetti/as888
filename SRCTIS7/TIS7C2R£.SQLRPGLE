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
004400011205     D DS01          E DS
004500011206     D*-------------------
004600011206     D* VARIABILI DI WRK
004700011206     D*-------------------
004800011206     D  SQLline        s           5000    varying
004900011206     D  countAgf       s              9  0 INZ(*zeros)
005000011206     D  wrkAge         s              7  0 INZ(*zeros)
005100970526     C*------------------------------------------------------------------------*
005200991214     C* MAIN LINES
005300970526     C*------------------------------------------------------------------------*
005400970526     C*
005500991214     C* ESEGUE ROUTINE DI CREAZIONE E STAMPA
005600991214     C                   EXSR      ESEGUI
005700000306     C*
005800970526     C                   SETON                                        LR
005900970526     C*------------------------------------------------------------------------*
006000991217     C* ESEGUI - OPERAZIONI PRINCIPALI
006100970526     C*------------------------------------------------------------------------*
006200991214     C     ESEGUI        BEGSR
006300011205     C*
006400011205     C* Poi stampo l'intestazione
006500011205     C                   seton                                        05
006600011205     C                   exsr      stpTES
006700011205     C*
006800011205     C* Quindi la routine di elaborazione
006900011206     C                   exsr      exeAGEUNI
007000011205     C*
007100011205     C* Al termine chiudo il prospetto
007200011206     C                   write     s7c2RI
007300011206     C                   write     s7c2FI
007400970526     C*
007500970526     C                   ENDSR
007600011205     C*------------------------------------------------------------------------*
007700011205     C* STPTES - ESEGUE LA STAMPA DELLA TESTATA QUANDO NECESSARIO
007800011205     C*------------------------------------------------------------------------*
007900011205     C     STPTES        BEGSR
008000011205     C*
008100011205     C                   if        *in05 = *on
008200011206     C                   write     s7c2TE
008300011205     C                   setoff                                       05
008400011205     C                   endif
008500011205     C*
008600011205     C                   ENDSR
008700000929     C*------------------------------------------------------------------------*
008800140616     C* EXEAGEUNI - Routine di selezione record Agenti Unificanti ordinati secondo lancio
008900000929     C*------------------------------------------------------------------------*
009000011206     C     EXEAGEUNI     BEGSR
009100011206     C*
009200140616     C                   movel     *loval        depAgp           25
009300140616     C* Costruisco la stringa SQL a seconda dei parametri ricevuti dal lancio
009400140616     C                   EVAL      SQLLINE = 'select cmmuni as ageuni, '       +
009500140616     C                                       'cmmdes as descr, '               +
009600140616     C                                       'cmmcod '                         +
009700140617     C                                       'from azcmm01l '                  +
009800140617     C                                       'where cmmatb = '' '''
009900140616     C*
010000140616     C                   if        PARORD = 'D'
010100140616     C                   eval      SQLline = %trim(SQLline)+' order by descr,'+
010200140616     C                                       ' ageuni, cmmcod'
010300140616     C                   else
010400140616     C                   eval      SQLline = %trim(SQLline)+' order by ageuni'+
010500140616     C                                       ', cmmcod'
010600140616     C                   endif
010700140616     C*
010800140616     C/EXEC SQL
010900140616     C+ prepare S1 from :SQLline
011000140616     C/END-EXEC
011100140616     C*
011200140616     C*
011300140616     C/EXEC SQL
011400140616     C+ declare C1 cursor for S1
011500140616     C/END-EXEC
011600140616     C*
011700140616     C*
011800140616     C/EXEC SQL
011900140616     C+ open C1
012000140616     C/END-EXEC
012100140616     C
012200140616     C/EXEC SQL
012300140616     C+ Fetch C1 into :DSSQL
012400140616     C/END-EXEC
012500140616     C*
012600140616     C                   dow       sqlcod = *zeros
012700140616     C                   exsr      exePRTP
012800140616     C*
012900140616     C/EXEC SQL
013000140616     C+ Fetch C1 into :DSSQL
013100140616     C/END-EXEC
013200140616     C*
013300140616     C                   enddo
013400140616     C*
013500140616     C* A fine file rieseguo la routine di stampa x scaricare l'ultimo record letto
013600140616     C                   exsr      exePRTP
013700140616     C*
013800140616     C/EXEC SQL
013900140616     C+ close C1
014000140616     C/END-EXEC
014100140616     C
014200140616     C*
014300140616     C                   ENDSR
014400011205     C*------------------------------------------------------------------------*
014500011205     C* EXEPRTP - STAMPA IL DETTAGLIO PADRI
014600011205     C*------------------------------------------------------------------------*
014700011205     C     EXEPRTP       BEGSR
014800020207     C*
014900020207     C* "Numerizzo" i campi alfa reperiti dalla fetch SQL
015000020207     C                   movel     ageuni        Nageuni           7 0
015100011206     C*
015200011206     C* Verifico l'avvenuta rottura x agente "padre"
015300020207     C                   if        PARORD = 'D'
015400140616     C                   eval      WdepAgp = %editc(ageuni:'Z') + descr
015500020207     C                   else
015600140616     C                   movel     *blank        WdepAgp          25
015700140616     C                   eval      %subst(WdepAgp:1:7) = %editc(ageuni:'Z')
015800140616     C                   endif
015900020207     C*
016000020207     C                   if        WdepAgp = depAgp
016100011206     C                   eval      countAgf = countAgf + 1
016200011205     C*
016300011206     C* Eseguo operazioni per gli agenti "figli" legati
016400011205     C                   exsr      exePRTF
016500011205     C                   else
016600011206     C*
016700011206     C* Verifico se ci sono ancora dei figli da stampare del padre precedente
016800011206     C                   if        wrkPosAgf > 0
016900011206     C                   exsr      stpTES
017000011206     C                   write     s7c2DE
017100011206     C                   clear                   s7c2DE
017200011206     C                   endif
017300011206     C*
017400020207     C* A rottura dell'agente "padre" lascio una riga vuota e re-inizializzo il contatore
017500011206     C                   eval      countAgf = 1
017600011205     C*
017700140616     C* Reperisco le informazioni dell'agente "padre"
017800020207     C                   if        PARORD = 'D'
017900140616     C                   eval      depAgp = %editc(ageuni:'Z') + descr
018000020207     C                   else
018100140616     C                   movel     *blank        depAgp
018200140616     C                   eval      %subst(depAgp:1:7) = %editc(ageuni:'Z')
018300020207     C                   endif
018400020207     C*
018500020207     C                   movel(p)  ageuni        wrkAge
018600011205     C                   exsr      repinfo
018700020207     C                   eval      stpCOP = Nageuni
018800020207     C                   eval      stpDCOP = descr
018900011205     C*
019000011206     C* Eseguo operazioni per gli agenti "figli" legati
019100011205     C                   exsr      exePRTF
019200011205     C                   endif
019300011205     C
019400011205     C*
019500011205     C                   ENDSR
019600011205     C*------------------------------------------------------------------------*
019700011205     C* EXEPRTF - STAMPA IL DETTAGLIO FIGLI
019800011205     C*------------------------------------------------------------------------*
019900011205     C     EXEPRTF       BEGSR
020000011205     C*
020100020207     C                   movel(p)  codage        wrkAge
020200011205     C                   exsr      repinfo
020300011205     C*
020400020207     C     countAgf      DIV       4             wrkNumAgf         9 0
020500011206     C                   MVR                     wrkPosAgf         1 0
020600011205     C*
020700011206     C                   if        wrkPosAgf = 1
020800140616     C                   movel     CMMCod        stpCOF1
020900140616     C                   eval      stpDCOF1 = CMMDes
021000140616     C                   eval      stpPIT1 = CMMPLI
021100140616     C                   eval      stpPES1 = CMMPLE
021200011205     C                   endif
021300011206     C*
021400011206     C                   if        wrkPosAgf = 2
021500140616     C                   movel     CMMCod        stpCOF2
021600140616     C                   eval      stpDCOF2 = CMMDes
021700140616     C                   eval      stpPIT2 = CMMPLI
021800140616     C                   eval      stpPES2 = CMMPLE
021900011206     C                   endif
022000011206     C*
022100011206     C                   if        wrkPosAgf = 3
022200140616     C                   movel     CMMCod        stpCOF3
022300140616     C                   eval      stpDCOF3 = CMMDes
022400140616     C                   eval      stpPIT3 = CMMPLI
022500140616     C                   eval      stpPES3 = CMMPLE
022600011206     C                   endif
022700011206     C*
022800011206     C                   if        wrkPosAgf = 0
022900140616     C                   movel     CMMCod        stpCOF4
023000140616     C                   eval      stpDCOF4 = CMMDes
023100140616     C                   eval      stpPIT4 = CMMPLI
023200140616     C                   eval      stpPES4 = CMMPLE
023300011206     C                   endif
023400011205     C*
023500011206     C                   if        wrkPosAgf = 0
023600011206     C                   exsr      stpTES
023700011206     C                   write     s7c2DE
023800011206     C                   clear                   s7c2DE
023900011206     C                   endif
024000011206     C*
024100011205     C                   ENDSR
024200011205     C*------------------------------------------------------------------------*
024300140616     C* REPINFO - REPERISCE TUTTE LE INFORMAZIONI NECESSARIE
024400011205     C*------------------------------------------------------------------------*
024500011205     C     REPINFO       BEGSR
024600011205     C*
024700011206     C* Reperisco le informazioini dell'agente commerciale
024800140616     C                   EVAL      CMMCod = wrkAge
024900140616     C     CMMCod        CHAIN     AZCMM01l
025000140616     C                   IF        not %found(AZCMM01L)
025100140616     C                   RESET                   AZCMM000
025200011205     C                   ENDIF
025300011206     C*
025400011205     C                   ENDSR
025500970526     C*------------------------------------------------------------------------*
025600970526     C* *INZSR - OPERAZIONI INIZIALI
025700970526     C*------------------------------------------------------------------------*
025800970526     C     *INZSR        BEGSR
025900970526     C*---
026000970526     C* RICEVIMENTO PARAMETRI
026100970526     C*---
026200970526     C     *ENTRY        PLIST
026300970612     C                   PARM                    KPJBA
026400991214     C                   MOVEL     KPJBU         PARAM
026500970526     C*---
026600991214     C* CHIAVI DI LETTURA
026700991214     C*---
026800991214     C*
026900970526     C                   ENDSR
