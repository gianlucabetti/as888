000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300060412     H* Creazione wrk-file x Geo-Referenziazione Indirizzi
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900060411     Fwfgri00f  UF A E             DISK
001000970526     D*------------------------------------------------------------------------*
001100030808     D* SCHIERE
001200030808     D*------------------------------------------------------------------------*
001300991214     D*------------
001400970526     D* ARCHITETTURA
001500970526     D*------------
001600970526     D KPJBA         E DS
001700051227     D*------------
001800030808     D* DS X PASSAGGIO PARAMETRI
001900060411     D*------------
002000030808     D PARAM           DS
002100060411     D  PARDAD                        8  0
002200060411     D  PARDAA                        8  0
002300060411     D  PARTBL1                       2
002400060411     D  PARTBL2                       2
002500060411     D  PARTBL3                       2
002600060411     D  PARTBL4                       2
002700060411     D  PARTBL5                       2
002800060411     D  PARLNA1                       3  0
002900060411     D  PARLNA2                       3  0
003000060411     D  PARLNA3                       3  0
003100060411     D  PARLNA4                       3  0
003200060411     D  PARLNA5                       3  0
003300030808     D*------------------
003400060411     D* DS DI PROCEDURA
003500030808     D*------------------
003600030808     D  DSTAS        E DS                  extname(TITAS00F)
003700060411     D  TISI95DS     E DS
003800011206     D*-------------------
003900011206     D* VARIABILI DI WRK
004000011206     D*-------------------
004100970526     C*------------------------------------------------------------------------*
004200991214     C* MAIN LINES
004300970526     C*------------------------------------------------------------------------*
004400030808     C*
004500030808     C* ESEGUE LA ROUTINE PRINCIPALE
004600991214     C                   EXSR      ESEGUI
004700060411     C*
004800060411     C* Esegue lancio TISI95R solo x chiusura
004900060411     C                   CLEAR                   TISI95DS
005000060411     C                   EVAL      I95TLA = 'C'
005100060411     C                   CALL      'TISI95R'
005200060411     C                   PARM                    TISI95DS
005300000306     C*
005400970526     C                   SETON                                        LR
005500970526     C*------------------------------------------------------------------------*
005600991217     C* ESEGUI - OPERAZIONI PRINCIPALI
005700970526     C*------------------------------------------------------------------------*
005800991214     C     ESEGUI        BEGSR
005900011205     C*
006000060411     C                   exsr      dltWGRI
006100060411     C                   exsr      exeGRI
006200970526     C*
006300970526     C                   ENDSR
006400030808     C*------------------------------------------------------------------------*
006500060411     C* DLTWGRI - Routine di eliminazione record wrk-file dello stesso periodo
006600030808     C*------------------------------------------------------------------------*
006700060411     C     DLTWGRI       BEGSR
006800030808     C*
006900030808     C/EXEC SQL
007000060411     C+ DELETE FROM WFGRI00F
007100060411     C+ WHERE GRIAAS*10000+GRIMGS BETWEEN :PARDAD AND :PARDAA AND
007200060411     C+ GRILNA IN (:PARLNA1, :PARLNA2, :PARLNA3, :PARLNA4, :PARLNA5) AND
007300060411     C+ GRITBL IN (:PARTBL1, :PARTBL2, :PARTBL3, :PARTBL4, :PARTBL5)
007400030808     C/END-EXEC
007500030808     C*
007600030808     C                   ENDSR
007700000929     C*------------------------------------------------------------------------*
007800060411     C* EXEGRI - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione
007900000929     C*------------------------------------------------------------------------*
008000060411     C     EXEGRI        BEGSR
008100030808     C*
008200030808     C/EXEC SQL
008300030808     C+ declare C1 cursor for
008400060411     C+ SELECT * FROM TITAS00F
008500060411     C+ WHERE TASAAS*10000+TASMGS BETWEEN :PARDAD AND :PARDAA AND
008600060411     C+ TASLNA IN (:PARLNA1, :PARLNA2, :PARLNA3, :PARLNA4, :PARLNA5) AND
008700060411     C+ TASTBL IN (:PARTBL1, :PARTBL2, :PARTBL3, :PARTBL4, :PARTBL5)
008800030808     C+ union
008900060411     C+ SELECT * FROM TITAS10F
009000060411     C+ WHERE TASAAS*10000+TASMGS BETWEEN :PARDAD AND :PARDAA AND
009100060411     C+ TASLNA IN (:PARLNA1, :PARLNA2, :PARLNA3, :PARLNA4, :PARLNA5) AND
009200060411     C+ TASTBL IN (:PARTBL1, :PARTBL2, :PARTBL3, :PARTBL4, :PARTBL5)
009300030808     C+ for read only
009400030808     C/END-EXEC
009500030808     C
009600030808     C/EXEC SQL
009700030808     C+ open C1
009800030808     C/END-EXEC
009900030808     C
010000030808     C/EXEC SQL
010100030808     C+ Fetch C1 into :DSTAS
010200030808     C/END-EXEC
010300030808     C*
010400060411     C                   dow       sqlcod = *zeros
010500060412     C*
010600060412     C* Inizializzo il formato d output
010700060412     C                   clear                   wfgri000
010800060411     C*
010900060411     C* Inizializzo il flag che determina la scrittura o meno del record bolla corrente
011000060411     C                   movel     'S'           wRecOK            1
011100060411     C*
011200060411     C* Se la bolla NN è in fermo deposito devo reperire l'ambito dell'instradamento
011300060411     C                   if        tasFFD <> 'S'
011400060411     C                   exsr      exeTISI95
011500060411     C                   else
011600060411     C                   eval      griAIN = 'F'
011700060411     C                   endif
011800060411     C*
011900060411     C                   if        wRecOK = 'S'
012000060411     C                   exsr      wriGRI
012100060411     C                   endif
012200030808     C*
012300030808     C/EXEC SQL
012400030808     C+ Fetch C1 into :DSTAS
012500030808     C/END-EXEC
012600030808     C*
012700030808     C                   enddo
012800030808     C*
012900030808     C/EXEC SQL
013000030808     C+ close C1
013100030808     C/END-EXEC
013200000929     C*
013300000929     C                   ENDSR
013400060411     C*------------------------------------------------------------------------*
013500060411     C* EXETISI95 - Routine di reperimento dati instradamento per recupero ambito
013600060411     C*------------------------------------------------------------------------*
013700060411     C     EXETISI95     BEGSR
013800060411     C*
013900060411     C                   CLEAR                   TISI95DS
014000060411     C                   EVAL      I95TCN = '7'
014100060411     C                   EVAL      I95DAT = tasAAS*10000+tasMGS
014200060411     C                   EVAL      I95NAR = tasNZD
014300060411     C                   EVAL      I95PRV = tasPRD
014400060411     C                   EVAL      I95CAP = tasCAD
014500060411     C                   EVAL      I95LOC = tasLOD
014600060411     C                   EVAL      I95LKG = tasPKB
014700060411     C                   EVAL      I95LMC = tasVLB
014800060411     C                   EVAL      I95FFD = tasFFD
014900060411     C                   EVAL      I95TSP = tasTSP
015000060411     C                   EVAL      I95FRE = 'S'
015100060412     C                   EVAL      I95TFP = tasTFP
015200060411     C                   CALL      'TISI95R'
015300060411     C                   PARM                    TISI95DS
015400060411     C*
015500060411     C* Quindi verifico, in ordine, reperimento x LOC - ECC - CAP
015600060411     C                   IF        O95ERR <> *blanks
015700060412     C***                MOVEL     'N'           wRecOK
015800060412     C                   EVAL      griERR = 'P'
015900060411     C                   ELSE
016000060411     C*
016100060411     C* X determinare l'ambito della linea/zona d arrivo dell'instradamento reperito
016200060411     C* testo nell'ordine: LOC - ECC - CAP
016300060411     C*
016400060411     C* => LOC:
016500060411     C                   IF        O95LIV = 'L'
016600060411     C* LNA/ZNC Standard
016700060411     C                   IF        O95LNA = O95LLA AND
016800060411     C                             O95ZNC = O95LZC
016900060411     C                   EVAL      griAIN = '='
017000060411     C                   ENDIF
017100060411     C* LNA/ZNC Sotto
017200060411     C                   IF        O95LNA = O95LLS AND
017300060411     C                             O95ZNC = O95LZS
017400060411     C                   EVAL      griAIN = '<'
017500060411     C                   ENDIF
017600060411     C* LNA/ZNC Oltre
017700060411     C                   IF        O95LNA = O95LLO AND
017800060411     C                             O95ZNC = O95LZO
017900060411     C                   EVAL      griAIN = '>'
018000060411     C                   ENDIF
018100060411     C*
018200060411     C                   ELSE
018300060411     C*
018400060411     C* => ECC:
018500060411     C                   IF        O95TFP > *zeros
018600060411     C* LNA/ZNC Standard
018700060411     C                   IF        O95LNA = O95ELA AND
018800060411     C                             O95ZNC = O95EZC
018900060411     C                   EVAL      griAIN = '='
019000060411     C                   ENDIF
019100060411     C* LNA/ZNC Sotto
019200060411     C                   IF        O95LNA = O95ELS AND
019300060411     C                             O95ZNC = O95EZS
019400060411     C                   EVAL      griAIN = '<'
019500060411     C                   ENDIF
019600060411     C* LNA/ZNC Oltre
019700060411     C                   IF        O95LNA = O95ELO AND
019800060411     C                             O95ZNC = O95EZO
019900060411     C                   EVAL      griAIN = '>'
020000060411     C                   ENDIF
020100060411     C*
020200060411     C                   ELSE
020300060411     C*
020400060411     C* => CAP:
020500060411     C* LNA/ZNC Standard
020600060411     C                   IF        O95LNA = O95CLA AND
020700060411     C                             O95ZNC = O95CZC
020800060411     C                   EVAL      griAIN = '='
020900060411     C                   ENDIF
021000060411     C* LNA/ZNC Sotto
021100060411     C                   IF        O95LNA = O95CLS AND
021200060411     C                             O95ZNC = O95CZS
021300060411     C                   EVAL      griAIN = '<'
021400060411     C                   ENDIF
021500060411     C* LNA/ZNC Oltre
021600060411     C                   IF        O95LNA = O95CLO AND
021700060411     C                             O95ZNC = O95CZO
021800060411     C                   EVAL      griAIN = '>'
021900060411     C                   ENDIF
022000060411     C*
022100060411     C                   ENDIF
022200060411     C                   ENDIF
022300060411     C                   ENDIF
022400060411     C*
022500060411     C                   ENDSR
022600030808     C*------------------------------------------------------------------------*
022700060411     C* WRIGRI - Routine di valorizzazione wrk-file
022800030808     C*------------------------------------------------------------------------*
022900060411     C     WRIGRI        BEGSR
023000031111     C*
023100060412     C* Valorizzo il buffer d output
023200060412     C                   EVAL      griAAS = tasAAS
023300060412     C                   EVAL      griLNP = tasLNP
023400060412     C                   EVAL      grINRS = tasNRS
023500060412     C                   EVAL      griNSP = tasNSP
023600060412     C                   EVAL      griTBL = tasTBL
023700060412     C                   EVAL      griCBO = tasCBO
023800060412     C                   EVAL      griMGS = tasMGS
023900060412     C                   EVAL      griLNA = tasLNA
024000060412     C                   EVAL      griNCL = tasNCL
024100060412     C                   EVAL      griPKF = tasPKF
024200060412     C                   EVAL      griVLF = tasVLF
024300060412     C                   EVAL      griTSP = tasTSP
024400060412     C                   EVAL      griIND = tasIND
024500060412     C                   EVAL      griCAD = tasCAD
024600060412     C                   EVAL      griLOD = tasLOD
024700060412     C                   EVAL      griPRD = tasPRD
024800060412     C                   EVAL      griNZD = tasNZD
024900060412     C                   EVAL      griFIN = tasFIN
025000060412     C                   EVAL      griPDC = tasPDC
025100060412     C                   EVAL      griDDC = tasDDC
025200060412     C                   EVAL      griSTP = tasSTP
025300060412     C                   EVAL      griDCM = tasDCM
025400060412     C                   EVAL      griHMC = tasHMC
025500060412     C                   EVAL      griCCA = tasCCA
025600060412     C                   EVAL      griFFD = tasFFD
025700060412     C                   EVAL      griFTC = tasFTC
025800060412     C                   EVAL      griTC2 = tasTC2
025900060412     C                   EVAL      griGC1 = tasGC1
026000060412     C                   EVAL      griGC2 = tasGC2
026100060412     C                   EVAL      griTCR = tasTCR
026200060412     C                   EVAL      griDCR = tasDCR
026300060412     C                   EVAL      griHCR = tasHCR
026400060412     C                   EVAL      griFBR = tasFBR
026500060412     C                   EVAL      griLL1 = tasLL1
026600060412     C*
026700060412     C* Effettuo considerazioni x stabilire se trattasi d spedizione diretta a supermercato
026800060412     C* o disagiato
026900060412     C                   IF        tasFTC = 'D' OR
027000060412     C                             tasTC2 = 'D'
027100060412     C                   EVAL      griDST = 'D'
027200060412     C                   ENDIF
027300060412     C                   IF        tasFTC = 'S' OR
027400060412     C                             tasTC2 = 'S'
027500060412     C                   EVAL      griDST = 'S'
027600060412     C                   ENDIF
027700060412     C*
027800060412     C* Quindi scarico il buffer d output
027900060412     C                   WRITE     wfgri000
028000030808     C*
028100030808     C                   ENDSR
028200970526     C*------------------------------------------------------------------------*
028300970526     C* *INZSR - OPERAZIONI INIZIALI
028400970526     C*------------------------------------------------------------------------*
028500970526     C     *INZSR        BEGSR
028600970526     C*---
028700970526     C* RICEVIMENTO PARAMETRI
028800970526     C*---
028900970526     C     *ENTRY        PLIST
029000970612     C                   PARM                    KPJBA
029100991214     C                   MOVEL     KPJBU         PARAM
029200991214     C*
029300970526     C                   ENDSR
