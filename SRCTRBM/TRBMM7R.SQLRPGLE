000100000000     H*--------------------------------------------------------------------------------------------*
000200040819     H* Pulizia archivio TIMAL00F - Log BartMailing
000300000000     H*--------------------------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*--------------------------------------------------------------------------------------------*
000600000000     F* Data base
000700000000     F*--------------------------------------------------------------------------------------------*
000800100202     Ftimal11l  UF   E           K DISK
000900040406     Ftabel00f  IF   E           K DISK
001000040406     D*----------------
001100000000     D* Data structure
001200040406     D*----------------
001300040819     D kpjba         E DS                                                       *architettura
001400040406     D ds5as1        E DS
001500000000     D*----------------
001600040819     D* Variabili a programma
001700000000     D*----------------
001800040406     D FlgElab         S              1    INZ('S')                             *flag si/no elaboraz
001900040819     D DATAORACOR      S               Z
002000040906     D DATAORACORLOG   S               Z
002100040906     D DATAORACORIFS   S               Z
002200040906     D DATAMAXLOG_A    S             23
002300040906     D DATAMAXIFS_A    S             23
002400000000     C*--------------------------------------------------------------------------------------------*
002500000000     C* Main lines
002600000000     C*--------------------------------------------------------------------------------------------*
002700100202
002800100202     C*
002900100202     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
003000100202     C
003100100202     C/EXEC SQL
003200100202     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
003300100202     C/END-EXEC
003400100202     C*
003500100202     C* Innanzitutto effettuo storicizzazione record d log (a stato OK)
003600100202     C* più vecchi d 6 mesi
003700100202     C/EXEC SQL
003800100202     C+  INSERT INTO UNITRAGRU/TIMAL10F
003900100202     C+  SELECT * FROM timal00f
004000100202     C+  WHERE
004100100202     C+      date(maldaor) < curdate() - 2 month
004200100202     C+  and malsts = '1'
004300100202     C/END-EXEC
004400100202     C*
004500100202     C/EXEC SQL
004600100202     C+  DELETE FROM timal00f
004700100202     C+  WHERE
004800100202     C+      date(maldaor) < curdate() - 6 month
004900100202     C+  and malsts = '1'
005000100202     C/END-EXEC
005100100202     C*
005200020918     C*
005300050706     C* Reperisco il numero d giorni d mantanimento record in file TIMAL
005400040406     C                   EXSR      cartab
005500020918     C*
005600050706     C* Se tutto ok => procedo con la cancellazione fisica dei record dal file TIMAL
005700040406     C                   IF        FlgElab = 'S'
005800040406     C                   EXSR      exeelab
005900040406     C                   ENDIF
006000971230     C*
006100040406     C                   SETON                                        LR
006200000306     C*--------------------------------------------------------------------------------------------*
006300040406     C* exeelab - legge il file fisico e determina se deletare i record
006400000306     C*--------------------------------------------------------------------------------------------*
006500040406     C     exeelab       BEGSR
006600040819     C*
006700040906     C* Determino la data limite al di sotto della quale cancellare i record d log
006800040906     C     DATAORACOR    SUBDUR    §5AMALL:*D    DATAORACORLOG
006900040906     C                   EVAL      DATAMAXLOG_A = %char(DATAORACORLOG)
007000040906     C*
007100040906     C* Determino la data limite al di sotto della quale cancellare i file nell'IFS
007200040906     C     DATAORACOR    SUBDUR    §5AMALI:*D    DATAORACORIFS
007300040906     C                   EVAL      DATAMAXIFS_A = %char(DATAORACORIFS)
007400000000     C*
007500050706     C* Scorro tutto il file TIMAL
007600100202     C     *LOVAL        SETLL     timal11l
007700100202     C                   IF        %found(timal11l)
007800100202     C                   READ      timal11l
007900100202     C                   DOW       not %eof(timal11l)
008000041125     C* Deleto solo se il record è già stato elaborato con successo
008100041125     C                   IF        malSTS =  '1'
008200040906     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - IFS
008300040906     C                   IF        malDAOR <= DATAMAXIFS_A
008400040819     C* ...se sì per prima cosa elimino il file relativo nell'IFS AS/400
008500040819     C                   EVAL      OPZIONE = 'O04'
008600100219     C                   CALL      'TRBMM4C'
008700040819     C                   PARM                    OPZIONE           3
008800040819     C                   PARM                    malFNAM
008900040819     C                   PARM                    malJNAM
009000040819     C                   PARM                    maLUNAM
009100040819     C                   PARM                    malJNUM
009200040819     C                   PARM                    malFNUM
009300040819     C                   PARM                    ESITO             1
009400040906     C                   ENDIF
009500040906     C* Verifico se la data del record d log è da cencellare rispetto ai gg. in tab. 5A - LOG
009600040906     C                   IF        malDAOR <= DATAMAXLOG_A
009700100202     C                   DELETE    timal11l
009800040406     C                   ENDIF
009900041125     C                   ENDIF
010000040406     C*
010100100202     C                   READ      timal11l
010200040406     C                   ENDDO
010300040406     C                   ENDIF
010400040406     C*
010500000000     C                   ENDSR
010600040406     C*--------------------------------------------------------------------------------------------*
010700000000     C* cartab - carica le tabelle occorrenti
010800040406     C*--------------------------------------------------------------------------------------------*
010900000000     C     cartab        BEGSR
011000040406     C*
011100040406     C                   EVAL      tblkut = 1
011200040406     C                   EVAL      tblcod = '5A'
011300040406     C                   EVAL      tblkey = 'SEDE1'
011400020905     C*
011500040406     C     KEYTAB        CHAIN     tabel00f
011600040406     C                   IF        %found(tabel00f) AND
011700040406     C                             tblflg = *blanks
011800040406     C                   EVAL      ds5as1 = tbluni
011900040406     C                   ELSE
012000040406     C                   EVAL      FlgElab = 'N'
012100020905     C                   ENDIF
012200000000     C*
012300000000     C                   ENDSR
012400040406     C*--------------------------------------------------------------------------------------------*
012500000000     C* *inzsr - operazioni iniziali
012600040406     C*--------------------------------------------------------------------------------------------*
012700000000     C     *inzsr        BEGSR
012800040406     C*
012900000000     C* ricevimento parametri
013000040406     C     *ENTRY        PLIST
013100040406     C                   PARM                    kpjba
013200000000     C*
013300040819     C* calcola la data corrente
013400040819     C                   TIME                    DATAORACOR
013500000000     C*
013600040406     C* definizione chiavi
013700040406     C*
013800040406     C* - TABEL00F - Completa
013900040406     C     KEYTAB        KLIST
014000040406     C                   KFLD                    tblkut
014100040406     C                   KFLD                    tblcod
014200040406     C                   KFLD                    tblkey
014300000000     C*
014400000000     C                   ENDSR
