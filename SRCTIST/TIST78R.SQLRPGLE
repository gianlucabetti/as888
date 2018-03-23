000100970526     H*------------------------------------------------------------------------*
000200060412     H* Creazione wrk-file x Geo-Referenziazione Indirizzi
000300970526     H*------------------------------------------------------------------------*
000400000920     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060411     Fwfgri00f  UF A E             DISK
000900060630     FwfgriT0f  UF A E             DISK    rename(wfgri000:wfgrit00)
001000060630     Fcnaco00f  IF   E           K DISK
001100060630     Fcnind00f  IF   E           K DISK
001200060630     Ftitaa30c  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400030808     D* SCHIERE
001500030808     D*------------------------------------------------------------------------*
001600991214     D*------------
001700970526     D* ARCHITETTURA
001800970526     D*------------
001900970526     D KPJBA         E DS
002000051227     D*------------
002100030808     D* DS X PASSAGGIO PARAMETRI
002200060411     D*------------
002300030808     D PARAM           DS
002400060411     D  PARDAD                        8  0
002500060411     D  PARDAA                        8  0
002600060411     D  PARTBL1                       2
002700060411     D  PARTBL2                       2
002800060411     D  PARTBL3                       2
002900060411     D  PARTBL4                       2
003000060411     D  PARTBL5                       2
003100060411     D  PARLNA1                       3  0
003200060411     D  PARLNA2                       3  0
003300060411     D  PARLNA3                       3  0
003400060411     D  PARLNA4                       3  0
003500060411     D  PARLNA5                       3  0
003600060630     D  PARFILE                       1
003700030808     D*------------------
003800060411     D* DS DI PROCEDURA
003900030808     D*------------------
004000030808     D  DSTAS        E DS                  extname(TITAS00F)
004100060411     D  TISI95DS     E DS
004200011206     D*-------------------
004300011206     D* VARIABILI DI WRK
004400011206     D*-------------------
004500060630     D SQLline         S           5000    VARYING
004600060630     D aPARDAD         S              8    INZ
004700060630     D aPARDAA         S              8    INZ
004800060630     D aPARLNA1        S              3    INZ
004900060630     D aPARLNA2        S              3    INZ
005000060630     D aPARLNA3        S              3    INZ
005100060630     D aPARLNA4        S              3    INZ
005200060630     D aPARLNA5        S              3    INZ
005300060630     D aCCA1           S              1    INZ('1')
005400060630     D aCCA2           S              1    INZ('2')
005500060630     D api             S              1    INZ('''')
005600970526     C*------------------------------------------------------------------------*
005700991214     C* MAIN LINES
005800970526     C*------------------------------------------------------------------------*
005900030808     C*
006000030808     C* ESEGUE LA ROUTINE PRINCIPALE
006100991214     C                   EXSR      ESEGUI
006200060411     C*
006300060411     C* Esegue lancio TISI95R solo x chiusura
006400060411     C                   CLEAR                   TISI95DS
006500060411     C                   EVAL      I95TLA = 'C'
006600060411     C                   CALL      'TISI95R'
006700060411     C                   PARM                    TISI95DS
006800000306     C*
006900970526     C                   SETON                                        LR
007000970526     C*------------------------------------------------------------------------*
007100991217     C* ESEGUI - OPERAZIONI PRINCIPALI
007200970526     C*------------------------------------------------------------------------*
007300991214     C     ESEGUI        BEGSR
007400011205     C*
007500060411     C                   exsr      dltWGRI
007600060411     C                   exsr      exeGRI
007700970526     C*
007800970526     C                   ENDSR
007900030808     C*------------------------------------------------------------------------*
008000060411     C* DLTWGRI - Routine di eliminazione record wrk-file dello stesso periodo
008100030808     C*------------------------------------------------------------------------*
008200060411     C     DLTWGRI       BEGSR
008300060630     C*
008400060630     C                   IF        PARFILE = '1' OR PARFILE = '3'
008500030808     C*
008600030808     C/EXEC SQL
008700060411     C+ DELETE FROM WFGRI00F
008800060411     C+ WHERE GRIAAS*10000+GRIMGS BETWEEN :PARDAD AND :PARDAA AND
008900060411     C+ GRILNA IN (:PARLNA1, :PARLNA2, :PARLNA3, :PARLNA4, :PARLNA5) AND
009000060411     C+ GRITBL IN (:PARTBL1, :PARTBL2, :PARTBL3, :PARTBL4, :PARTBL5)
009100030808     C/END-EXEC
009200030808     C*
009300060630     C                   ENDIF
009400060630     C*
009500060630     C                   IF        PARFILE = '2' OR PARFILE = '3'
009600060630     C*
009700060630     C/EXEC SQL
009800060630     C+ DELETE FROM WFGRIT0F
009900060630     C+ WHERE GRIAAS*10000+GRIMGS BETWEEN :PARDAD AND :PARDAA AND
010000060630     C+ GRILNA IN (:PARLNA1, :PARLNA2, :PARLNA3, :PARLNA4, :PARLNA5) AND
010100060630     C+ GRITBL IN (:PARTBL1, :PARTBL2, :PARTBL3, :PARTBL4, :PARTBL5)
010200060630     C/END-EXEC
010300060630     C*
010400060630     C                   ENDIF
010500060630     C*
010600030808     C                   ENDSR
010700000929     C*------------------------------------------------------------------------*
010800060411     C* EXEGRI - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione
010900000929     C*------------------------------------------------------------------------*
011000060411     C     EXEGRI        BEGSR
011100060630     C*
011200060630     C                   MOVE(P)   PARDAD        aPARDAD
011300060630     C                   MOVE(P)   PARDAA        aPARDAA
011400060630     C                   MOVE(P)   PARLNA1       aPARLNA1
011500060630     C                   MOVE(P)   PARLNA2       aPARLNA2
011600060630     C                   MOVE(P)   PARLNA3       aPARLNA3
011700060630     C                   MOVE(P)   PARLNA4       aPARLNA4
011800060630     C                   MOVE(P)   PARLNA5       aPARLNA5
011900060630     C*
012000060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
012100060630     C                             'SELECT * FROM TITAS00F '                  +
012200060630     C                             'WHERE TASAAS*10000+TASMGS BETWEEN '       +
012300060630     C                             aPARDAD + ' AND '  + aPARDAA + ' AND '
012400060630     C* Se inserite LNA in selezione => gestisco
012500060630     C                   IF        PARLNA1 <> *zeros OR
012600060630     C                             PARLNA2 <> *zeros OR
012700060630     C                             PARLNA3 <> *zeros OR
012800060630     C                             PARLNA4 <> *zeros OR
012900060630     C                             PARLNA5 <> *zeros
013000060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
013100060630     C                             'TASLNA IN ('                              +
013200060630     C                             aPARLNA1 + ', ' +
013300060630     C                             aPARLNA2 + ', ' +
013400060630     C                             aPARLNA3 + ', ' +
013500060630     C                             aPARLNA4 + ', ' +
013600060630     C                             aPARLNA5 + ') AND '
013700060630     C                   ENDIF
013800060630     C*
013900060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
014000060630     C                             'TASTBL IN ('                              +
014100060630     C                             api + PARTBL1 + api + ', '                 +
014200060630     C                             api + PARTBL2 + api + ', '                 +
014300060630     C                             api + PARTBL3 + api + ', '                 +
014400060630     C                             api + PARTBL4 + api + ', '                 +
014500060630     C                             api + PARTBL5 + api + ') AND '             +
014600060630     C                             'TASCCA NOT IN ('                          +
014700060630     C                             api + aCCA1 + api + ', '                   +
014800060630     C                             api + aCCA2 + api + ') '                   +
014900060630     C                             ' UNION  '
015000060630     C*
015100060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
015200060630     C                             'SELECT * FROM TITAS10F '                  +
015300060630     C                             'WHERE TASAAS*10000+TASMGS BETWEEN '       +
015400060630     C                             aPARDAD + ' AND '  + aPARDAA + ' AND '
015500060630     C* Se inserite LNA in selezione => gestisco
015600060630     C                   IF        PARLNA1 <> *zeros OR
015700060630     C                             PARLNA2 <> *zeros OR
015800060630     C                             PARLNA3 <> *zeros OR
015900060630     C                             PARLNA4 <> *zeros OR
016000060630     C                             PARLNA5 <> *zeros
016100060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
016200060630     C                             'TASLNA IN ('                              +
016300060630     C                             aPARLNA1 + ', ' +
016400060630     C                             aPARLNA2 + ', ' +
016500060630     C                             aPARLNA3 + ', ' +
016600060630     C                             aPARLNA4 + ', ' +
016700060630     C                             aPARLNA5 + ') AND '
016800060630     C                   ENDIF
016900060630     C*
017000060630     C                   EVAL      SQLline = %trim(SQLline) + ' '             +
017100060630     C                             'TASTBL IN ('                              +
017200060630     C                             api + PARTBL1 + api + ', '                 +
017300060630     C                             api + PARTBL2 + api + ', '                 +
017400060630     C                             api + PARTBL3 + api + ', '                 +
017500060630     C                             api + PARTBL4 + api + ', '                 +
017600060630     C                             api + PARTBL5 + api + ') AND '             +
017700060630     C                             'TASCCA NOT IN ('                          +
017800060630     C                             api + aCCA1 + api + ', '                   +
017900060630     C                             api + aCCA2 + api + ') '                   +
018000060630     C                             ' FOR READ ONLY '
018100060630     C*
018200060630     C/EXEC SQL
018300060630     C+ prepare S1 from :SQLline
018400060630     C/END-EXEC
018500060630     C*
018600030808     C*
018700030808     C/EXEC SQL
018800060630     C+ declare C1 cursor for S1
018900030808     C/END-EXEC
019000030808     C
019100030808     C/EXEC SQL
019200030808     C+ open C1
019300030808     C/END-EXEC
019400030808     C
019500030808     C/EXEC SQL
019600060630     C+ Fetch next from C1 into :DSTAS
019700030808     C/END-EXEC
019800030808     C*
019900060411     C                   dow       sqlcod = *zeros
020000060412     C*
020100060412     C* Inizializzo il formato d output
020200060412     C                   clear                   wfgri000
020300060630     C                   clear                   wfgrit00
020400060411     C*
020500060411     C* Inizializzo il flag che determina la scrittura o meno del record bolla corrente
020600060411     C                   movel     'S'           wRecOK            1
020700060411     C*
020800060411     C* Se la bolla NN è in fermo deposito devo reperire l'ambito dell'instradamento
020900060411     C                   if        tasFFD <> 'S'
021000060411     C                   exsr      exeTISI95
021100060411     C                   else
021200060411     C                   eval      griAIN = 'F'
021300060411     C                   endif
021400060411     C*
021500060411     C                   if        wRecOK = 'S'
021600060411     C                   exsr      wriGRI
021700060411     C                   endif
021800030808     C*
021900030808     C/EXEC SQL
022000030808     C+ Fetch C1 into :DSTAS
022100030808     C/END-EXEC
022200030808     C*
022300030808     C                   enddo
022400030808     C*
022500030808     C/EXEC SQL
022600030808     C+ close C1
022700030808     C/END-EXEC
022800000929     C*
022900000929     C                   ENDSR
023000060411     C*------------------------------------------------------------------------*
023100060411     C* EXETISI95 - Routine di reperimento dati instradamento per recupero ambito
023200060411     C*------------------------------------------------------------------------*
023300060411     C     EXETISI95     BEGSR
023400060411     C*
023500060411     C                   CLEAR                   TISI95DS
023600060411     C                   EVAL      I95TCN = '7'
023700060411     C                   EVAL      I95DAT = tasAAS*10000+tasMGS
023800060411     C                   EVAL      I95NAR = tasNZD
023900060411     C                   EVAL      I95PRV = tasPRD
024000060411     C                   EVAL      I95CAP = tasCAD
024100060411     C                   EVAL      I95LOC = tasLOD
024200060411     C                   EVAL      I95LKG = tasPKB
024300060411     C                   EVAL      I95LMC = tasVLB
024400060411     C                   EVAL      I95FFD = tasFFD
024500060411     C                   EVAL      I95TSP = tasTSP
024600060411     C                   EVAL      I95FRE = 'S'
024700060412     C                   EVAL      I95TFP = tasTFP
024800060411     C                   CALL      'TISI95R'
024900060411     C                   PARM                    TISI95DS
025000060411     C*
025100060411     C* Quindi verifico, in ordine, reperimento x LOC - ECC - CAP
025200060411     C                   IF        O95ERR <> *blanks
025300060412     C***                MOVEL     'N'           wRecOK
025400060412     C                   EVAL      griERR = 'P'
025500060411     C                   ELSE
025600060411     C*
025700060411     C* X determinare l'ambito della linea/zona d arrivo dell'instradamento reperito
025800060411     C* testo nell'ordine: LOC - ECC - CAP
025900060411     C*
026000060411     C* => LOC:
026100060411     C                   IF        O95LIV = 'L'
026200060411     C* LNA/ZNC Sotto
026300060411     C                   IF        O95LNA = O95LLS AND
026400060411     C                             O95ZNC = O95LZS
026500060411     C                   EVAL      griAIN = '<'
026600060411     C                   ENDIF
026700060411     C* LNA/ZNC Oltre
026800060411     C                   IF        O95LNA = O95LLO AND
026900060411     C                             O95ZNC = O95LZO
027000060411     C                   EVAL      griAIN = '>'
027100060411     C                   ENDIF
027200060508     C* LNA/ZNC Standard
027300060508     C                   IF        O95LNA = O95LLA AND
027400060508     C                             O95ZNC = O95LZC
027500060508     C                   EVAL      griAIN = '='
027600060508     C                   ENDIF
027700060411     C*
027800060411     C                   ELSE
027900060411     C*
028000060411     C* => ECC:
028100060411     C                   IF        O95TFP > *zeros
028200060411     C* LNA/ZNC Sotto
028300060411     C                   IF        O95LNA = O95ELS AND
028400060411     C                             O95ZNC = O95EZS
028500060411     C                   EVAL      griAIN = '<'
028600060411     C                   ENDIF
028700060411     C* LNA/ZNC Oltre
028800060411     C                   IF        O95LNA = O95ELO AND
028900060411     C                             O95ZNC = O95EZO
029000060411     C                   EVAL      griAIN = '>'
029100060411     C                   ENDIF
029200060508     C* LNA/ZNC Standard
029300060508     C                   IF        O95LNA = O95ELA AND
029400060508     C                             O95ZNC = O95EZC
029500060508     C                   EVAL      griAIN = '='
029600060508     C                   ENDIF
029700060411     C*
029800060411     C                   ELSE
029900060411     C*
030000060411     C* => CAP:
030100060411     C* LNA/ZNC Sotto
030200060411     C                   IF        O95LNA = O95CLS AND
030300060411     C                             O95ZNC = O95CZS
030400060411     C                   EVAL      griAIN = '<'
030500060411     C                   ENDIF
030600060411     C* LNA/ZNC Oltre
030700060411     C                   IF        O95LNA = O95CLO AND
030800060411     C                             O95ZNC = O95CZO
030900060411     C                   EVAL      griAIN = '>'
031000060411     C                   ENDIF
031100060508     C* LNA/ZNC Standard
031200060508     C                   IF        O95LNA = O95CLA AND
031300060508     C                             O95ZNC = O95CZC
031400060508     C                   EVAL      griAIN = '='
031500060508     C                   ENDIF
031600060411     C*
031700060411     C                   ENDIF
031800060411     C                   ENDIF
031900060411     C                   ENDIF
032000060411     C*
032100060411     C                   ENDSR
032200030808     C*------------------------------------------------------------------------*
032300060411     C* WRIGRI - Routine di valorizzazione wrk-file
032400030808     C*------------------------------------------------------------------------*
032500060411     C     WRIGRI        BEGSR
032600031111     C*
032700060412     C* Valorizzo il buffer d output
032800060508     C                   EVAL      griAAS  = tasAAS
032900060508     C                   EVAL      griLNP  = tasLNP
033000060508     C                   EVAL      grINRS  = tasNRS
033100060508     C                   EVAL      griNSP  = tasNSP
033200060508     C                   EVAL      griTBL  = tasTBL
033300060508     C                   EVAL      griCBO  = tasCBO
033400060508     C                   EVAL      griMGS  = tasMGS
033500060508     C                   EVAL      griLNA  = tasLNA
033600060508     C                   EVAL      griNCL  = tasNCL
033700060508     C                   EVAL      griPKF  = tasPKF
033800060508     C                   EVAL      griVLF  = tasVLF
033900060508     C                   EVAL      griTSP  = tasTSP
034000060508     C                   EVAL      griIND  = tasIND
034100060508     C                   EVAL      griCAD  = tasCAD
034200060508     C                   EVAL      griLOD  = tasLOD
034300060508     C                   EVAL      griLODN = O95LOC
034400060508     C                   EVAL      griPRD  = tasPRD
034500060508     C                   EVAL      griNZD  = tasNZD
034600060508     C                   EVAL      griFIN  = tasFIN
034700060630     C                   MOVEL     tasPDC        griFDC
034800060508     C                   EVAL      griPDC  = tasPDC
034900060630     C                   EVAL      griNDC  = tasNDC
035000060508     C                   EVAL      griDDC  = tasDDC
035100060508     C                   EVAL      griSTP  = tasSTP
035200060508     C                   EVAL      griDCM  = tasDCM
035300060508     C                   EVAL      griHMC  = tasHMC
035400060508     C                   EVAL      griCCA  = tasCCA
035500060508     C                   EVAL      griFFD  = tasFFD
035600060508     C                   EVAL      griFTC  = tasFTC
035700060508     C                   EVAL      griTC2  = tasTC2
035800060508     C                   EVAL      griGC1  = tasGC1
035900060508     C                   EVAL      griGC2  = tasGC2
036000060508     C                   EVAL      griTCR  = tasTCR
036100060508     C                   EVAL      griDCR  = tasDCR
036200060508     C                   EVAL      griHCR  = tasHCR
036300060508     C                   EVAL      griFBR  = tasFBR
036400060508     C                   EVAL      griLL1  = tasLL1
036500060630     C*
036600060630     C* Reperisco i dati anagrafici del mittente
036700060630     C                   MOVE(P)   tasCCM        wCOD              4
036800060630     C                   IF        wCOD = *all'8'                               * NN codificati
036900060630     C     KEYtaa30_c    CHAIN     titaa30c
037000060630     C                   IF        %found(titaa30c)
037100060630     C                   EVAL      griRSM = taaRSC
037200060630     C                   EVAL      griINM = taaIND
037300060630     C                   EVAL      griCAM = taaCAP
037400060630     C                   EVAL      griLOM = taaLOC
037500060630     C                   EVAL      griPRM = taaPRV
037600060630     C                   EVAL      griNZM = taaNAZ
037700060630     C                   ELSE
037800060630     C                   EVAL      griRSM = *all'*'
037900060630     C                   EVAL      griINM = *all'*'
038000060630     C                   EVAL      griCAM = *all'*'
038100060630     C                   EVAL      griLOM = *all'*'
038200060630     C                   EVAL      griPRM = *all'*'
038300060630     C                   EVAL      griNZM = *all'*'
038400060630     C                   ENDIF
038500060630     C*
038600060630     C                   ELSE                                                   * Codificati
038700060630     C                   EVAL      ACOKUT = 1
038800060630     C                   EVAL      ACOKCC = 151
038900060630     C                   EVAL      ACOKSC = tasCCM
039000060630     C     KEYaco_c      CHAIN     cnaco00f
039100060630     C                   IF        %found(cnaco00f)
039200060630     C     KEYaco_c      CHAIN     cnind00f
039300060630     C                   IF        %found(cnind00f)
039400060630     C                   EVAL      griRSM = acoRAG
039500060630     C                   EVAL      griINM = indVIA
039600060630     C                   MOVEL(P)  indCAP        griCAM
039700060630     C                   EVAL      griLOM = indCIT
039800060630     C                   EVAL      griPRM = indPRV
039900060630     C                   EVAL      griNZM = indSTA
040000060630     C                   ELSE
040100060630     C                   EVAL      griRSM = *all'*'
040200060630     C                   EVAL      griINM = *all'*'
040300060630     C                   EVAL      griCAM = *all'*'
040400060630     C                   EVAL      griLOM = *all'*'
040500060630     C                   EVAL      griPRM = *all'*'
040600060630     C                   EVAL      griNZM = *all'*'
040700060630     C                   ENDIF
040800060630     C                   ENDIF
040900060630     C                   ENDIF
041000060412     C*
041100060412     C* Effettuo considerazioni x stabilire se trattasi d spedizione diretta a supermercato
041200060412     C* o disagiato
041300060412     C                   IF        tasFTC = 'D' OR
041400060412     C                             tasTC2 = 'D'
041500060412     C                   EVAL      griDST = 'D'
041600060412     C                   ENDIF
041700060412     C                   IF        tasFTC = 'S' OR
041800060412     C                             tasTC2 = 'S'
041900060412     C                   EVAL      griDST = 'S'
042000060412     C                   ENDIF
042100060412     C*
042200060412     C* Quindi scarico il buffer d output
042300060630     C                   IF        PARFILE = '1' OR PARFILE = '3'
042400060412     C                   WRITE     wfgri000
042500060630     C                   ENDIF
042600060630     C                   IF        PARFILE = '2' OR PARFILE = '3'
042700060630     C                   WRITE     wfgrit00
042800060630     C                   ENDIF
042900030808     C*
043000030808     C                   ENDSR
043100970526     C*------------------------------------------------------------------------*
043200970526     C* *INZSR - OPERAZIONI INIZIALI
043300970526     C*------------------------------------------------------------------------*
043400970526     C     *INZSR        BEGSR
043500970526     C*---
043600970526     C* RICEVIMENTO PARAMETRI
043700970526     C*---
043800970526     C     *ENTRY        PLIST
043900970612     C                   PARM                    KPJBA
044000991214     C                   MOVEL     KPJBU         PARAM
044100060630     C*
044200060630     C* Definizione chiavi
044300060630     C*
044400060630     C* TITAA30C - Completa
044500060630     C     KEYtaa30_c    KLIST
044600060630     C                   KFLD                    tasAAS
044700060630     C                   KFLD                    tasLNP
044800060630     C                   KFLD                    tasNRS
044900060630     C                   KFLD                    tasNSP
045000060630     C                   KFLD                    taaTRC
045100060630     C*
045200060630     C* CNACO00F - Completa
045300060630     C     KEYaco_c      KLIST
045400060630     C                   KFLD                    acoKUT
045500060630     C                   KFLD                    acoKCC
045600060630     C                   KFLD                    acoKSC
045700060630     C*
045800060630     C                   EVAL      taaTRC = 'M'                                 * anagr. mittenti
045900060630     C*
046000060630     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
046100060630     C
046200060630     C/EXEC SQL
046300060630     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
046400060630     C/END-EXEC
046500060630     C*
046600991214     C*
046700970526     C                   ENDSR
