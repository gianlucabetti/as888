000100000000     H*--------------------------------------------------------------------------------------------*
000200040819     H* Pulizia archivio TIMAL00F - Log BartMailing
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600000000     F* Data base
000700000000     F*--------------------------------------------------------------------------------------------*
000800140107     Ftimal01l  UF   E           K DISK
000900100202     Ftimal11l  UF   E           K DISK
001000040406     Ftabel00f  IF   E           K DISK
001100040406     D*----------------
001200000000     D* Data structure
001300040406     D*----------------
001400040819     D kpjba         E DS                                                       *architettura
001500040406     D ds5as1        E DS
001600000000     D*----------------
001700040819     D* Variabili a programma
001800000000     D*----------------
001900040406     D FlgElab         S              1    INZ('S')                             *flag si/no elaboraz
002000040819     D DATAORACOR      S               Z
002100040906     D DATAORACORLOG   S               Z
002200040906     D DATAORACORIFS   S               Z
002300040906     D DATAMAXLOG_A    S             23
002400040906     D DATAMAXIFS_A    S             23
002500000000     C*--------------------------------------------------------------------------------------------*
002600000000     C* Main lines
002700000000     C*--------------------------------------------------------------------------------------------*
002800100202
002900100202     C*
003000100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003100100202     C
003200100202     C/EXEC SQL
003300100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003400100202     C/END-EXEC
003500100202     C*
003600150417     C* Innanzitutto effettuo storicizzazione record d log
003700150417     C* più vecchi d 2 mesi
003800100202     C/EXEC SQL
003900100202     C+  INSERT INTO UNITRAGRU/TIMAL10F
004000100202     C+  SELECT * FROM timal00f
004100100202     C+  WHERE
004200161220     C+      date(maldaor) < curdate() - 1 month
004300150417     C***+  and malsts = '1'
004400100202     C/END-EXEC
004500100202     C*
004600100202     C/EXEC SQL
004700100202     C+  DELETE FROM timal00f
004800100202     C+  WHERE
004900161220     C+      date(maldaor) < curdate() - 1 month
005000150417     C***+  and malsts = '1'
005100100202     C/END-EXEC
005200160411     C*
005300160411     C* Poi storicizzo il file di TRACE con data inserimento
005400161220     C* più vecchia di 1 mesi
005500130208     C*
005600160411     C/EXEC SQL
005700160411     C+  INSERT INTO UNITRAGRU/TIMAT10F
005800160411     C+  SELECT * FROM timaT00f
005900160411     C+   WHERE
006000160411     C+     matmomins = 0 or matmomins = 1 or
006100160411     C+     matmomins > 1 and
006200160411     C+     date(substr(char(matmomins) , 1 , 4) concat '-' concat
006300160411     C+          substr(char(matmomins) , 5 , 2) concat '-' concat
006400160411     C+          substr(char(matmomins) , 7 , 2))
006500161220     C+     < curdate() - 1 month
006600160411     C/END-EXEC
006700160411     C*
006800160411     C/EXEC SQL
006900160411     C+  DELETE FROM timaT00f
007000160411     C+  WHERE
007100160411     C+     matmomins = 0 or matmomins = 1 or
007200160411     C+     matmomins > 1 and
007300160411     C+     date(substr(char(matmomins) , 1 , 4) concat '-' concat
007400160411     C+          substr(char(matmomins) , 5 , 2) concat '-' concat
007500160411     C+          substr(char(matmomins) , 7 , 2))
007600161220     C+     < curdate() - 1 month
007700160411     C/END-EXEC
007800160411     C*
007900130208     C* Imposto come in errore i log
008000161220     C* più vecchi di 10 giorni con situazione anomala
008100130208     C/EXEC SQL
008200130208     C+ update timal00f set malsts = '2'
008300130208     C+ where malprog in (
008400130208     C+ select distinct malprog from (
008500130208     C+ select malprog, decimal(max(maltotatt), 2, 0) as TOT ,
008600130208     C+ count(*) as TOT0 from timal00f
008700161220     C+ where malsts = '0' and date(maldaor) < curdate() - 10 days
008800130208     C+ group by malprog ) as TOT_RGR
008900130208     C+ where tot <> TOT0
009000130208     C+ order by malprog)
009100130208     C/END-EXEC
009200100202     C*
009300020918     C*
009400050706     C* Reperisco il numero d giorni d mantanimento record in file TIMAL
009500040406     C                   EXSR      cartab
009600020918     C*
009700050706     C* Se tutto ok => procedo con la cancellazione fisica dei record dal file TIMAL
009800040406     C                   IF        FlgElab = 'S'
009900040406     C                   EXSR      exeelab
010000040406     C                   ENDIF
010100971230     C*
010200040406     C                   SETON                                        LR
010300000306     C*--------------------------------------------------------------------------------------------*
010400040406     C* exeelab - legge il file fisico e determina se deletare i record
010500000306     C*--------------------------------------------------------------------------------------------*
010600040406     C     exeelab       BEGSR
010700040819     C*
010800040906     C* Determino la data limite al di sotto della quale cancellare i record d log
010900040906     C     DATAORACOR    SUBDUR    §5AMALL:*D    DATAORACORLOG
011000040906     C                   EVAL      DATAMAXLOG_A = %char(DATAORACORLOG)
011100040906     C*
011200040906     C* Determino la data limite al di sotto della quale cancellare i file nell'IFS
011300040906     C     DATAORACOR    SUBDUR    §5AMALI:*D    DATAORACORIFS
011400040906     C                   EVAL      DATAMAXIFS_A = %char(DATAORACORIFS)
011500000000     C*
011600140107     C* Scorro tutto il file TIMAL 00
011700140107     C     *LOVAL        SETLL     timal01l
011800140107     C                   IF        %found(timal01l)
011900140107     C                   READ      timal01l
012000140107     C                   DOW       not %eof(timal01l)
012100041125     C* Deleto solo se il record è già stato elaborato con successo
012200140107     C***                IF        malSTS =  '1'
012300040906     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - IFS
012400101206     C***                IF        malDAOR <= DATAMAXIFS_A
012500040819     C* ...se sì per prima cosa elimino il file relativo nell'IFS AS/400
012600101206     C***                EVAL      OPZIONE = 'O04'
012700101206     C***                CALL      'TRTCM4C'
012800101206     C***                PARM                    OPZIONE           3
012900101206     C***                PARM                    malFNAM
013000101206     C***                PARM                    malJNAM
013100101206     C***                PARM                    maLUNAM
013200101206     C***                PARM                    malJNUM
013300101206     C***                PARM                    malFNUM
013400101206     C***                PARM                    ESITO             1
013500101206     C***                ENDIF
013600040906     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - LOG
013700040906     C                   IF        malDAOR <= DATAMAXLOG_A
013800140107     C                   DELETE    timal01l
013900040406     C                   ENDIF
014000140107     C***                ENDIF
014100040406     C*
014200140107     C                   READ      timal01l
014300040406     C                   ENDDO
014400040406     C                   ENDIF
014500140107     C*
014600140107     C* Scorro tutto il file TIMAL 10
014700140107     C     *LOVAL        SETLL     timal11l
014800140107     C                   IF        %found(timal11l)
014900140107     C                   READ      timal11l
015000140107     C                   DOW       not %eof(timal11l)
015100140107     C* Deleto solo se il record è già stato elaborato con successo
015200140107     C***                IF        malSTS =  '1'
015300140107     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - IFS
015400140107     C***                IF        malDAOR <= DATAMAXIFS_A
015500140107     C* ...se sì per prima cosa elimino il file relativo nell'IFS AS/400
015600140107     C***                EVAL      OPZIONE = 'O04'
015700140107     C***                CALL      'TRTCM4C'
015800140107     C***                PARM                    OPZIONE           3
015900140107     C***                PARM                    malFNAM
016000140107     C***                PARM                    malJNAM
016100140107     C***                PARM                    maLUNAM
016200140107     C***                PARM                    malJNUM
016300140107     C***                PARM                    malFNUM
016400140107     C***                PARM                    ESITO             1
016500140107     C***                ENDIF
016600140107     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - LOG
016700140107     C                   IF        malDAOR <= DATAMAXLOG_A
016800140107     C                   DELETE    timal11l
016900140107     C                   ENDIF
017000140107     C***                ENDIF
017100140107     C*
017200140107     C                   READ      timal11l
017300140107     C                   ENDDO
017400140107     C                   ENDIF
017500040406     C*
017600000000     C                   ENDSR
017700040406     C*--------------------------------------------------------------------------------------------*
017800000000     C* cartab - carica le tabelle occorrenti
017900040406     C*--------------------------------------------------------------------------------------------*
018000000000     C     cartab        BEGSR
018100040406     C*
018200040406     C                   EVAL      tblkut = 1
018300040406     C                   EVAL      tblcod = '5A'
018400040406     C                   EVAL      tblkey = 'SEDE1'
018500020905     C*
018600040406     C     KEYTAB        CHAIN     tabel00f
018700040406     C                   IF        %found(tabel00f) AND
018800040406     C                             tblflg = *blanks
018900040406     C                   EVAL      ds5as1 = tbluni
019000040406     C                   ELSE
019100040406     C                   EVAL      FlgElab = 'N'
019200020905     C                   ENDIF
019300000000     C*
019400000000     C                   ENDSR
019500040406     C*--------------------------------------------------------------------------------------------*
019600000000     C* *inzsr - operazioni iniziali
019700040406     C*--------------------------------------------------------------------------------------------*
019800000000     C     *inzsr        BEGSR
019900040406     C*
020000000000     C* ricevimento parametri
020100040406     C     *ENTRY        PLIST
020200040406     C                   PARM                    kpjba
020300000000     C*
020400040819     C* calcola la data corrente
020500040819     C                   TIME                    DATAORACOR
020600000000     C*
020700040406     C* definizione chiavi
020800040406     C*
020900040406     C* - TABEL00F - Completa
021000040406     C     KEYTAB        KLIST
021100040406     C                   KFLD                    tblkut
021200040406     C                   KFLD                    tblcod
021300040406     C                   KFLD                    tblkey
021400000000     C*
021500000000     C                   ENDSR
