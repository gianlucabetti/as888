000100000405     h*--------------------------------------------------------------------------------------------*
000200001102     h* Reperisce "Numero Spedizione" da "Riferimento Mittente".
000300000405     h*--------------------------------------------------------------------------------------------*
000400000405      /TITLE Carica dati dettaglio spedizione
000500000405     hdatedit(*DMY)
000600090904
000700090904     D RESULT_TABLE_EMPTY...
000800090904     D                 C                   '02000'
000900090904     D RESULT_TABLE_WITH_MORE_THAN_1_ROW...
001000090904     D                 C                   '21000'
001100090904
001200090904     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
001300090904
001400000405     d*--------------------------------------------------------------------------------------------*
001500000405     d* Data structure
001600000405     d*--------------------------------------------------------------------------------------------*
001700001102     d*---
001800001102     d* Variabili riferite al data base
001900001102     d*---
002000090904     D ktaksu          S              8A
002100090904     D ktarmn          S             15P 0
002200001102     d*---
002300001102     d* Variabili di wrk
002400001102     d*---
002500001102     d lung            s              8  0 inz(*zeros)
002600090904     D rpyEsito        S             10I 0
002700100223     d esito           s              1
002800100223     D rma             S             15A   VARYING
002900100311     D dsp             S              8P 0
003000000405     d*---
003100000405     d* Ds
003200000405     d*---
003300001102     d* DS contenente i dati di INPUT
003400001102     d tis175dsi     e DS
003500001102     d* DS contenente i dati della pagina: ds attesa dal server di STRATEGI
003600001102     d tis175dso     e DS
003700090904     D tis7700i1     E DS                  QUALIFIED
003800090904     D                                     INZ(*EXTDFT)
003900090904     D tis7700o1     E DS                  QUALIFIED
004000090904     D                                     INZ(*EXTDFT)
004100090904
004200000405     c*--------------------------------------------------------------------------------------------*
004300000405     c* Main lines
004400000405     c*--------------------------------------------------------------------------------------------*
004500090904     c                   SETON                                        LR
004600000405     c*
004700090904     c* Errori in input, inutile continuare.
004800090904     c                   if        esito <> '0'
004900090904     c                   RETURN
005000090904     c                   endif
005100090904
005200090904     C                   EVAL      tis7700i1.ksu = ktaKsu
005300090904     C                   EVAL      tis7700i1.sun = %EDITC(rqsCidiI75 : 'X')
005400090904     C                   CALLP     Selettore_bolla_subUnificante('GETSUBKSUN'
005500090904     C                             : rpyEsito
005600090904     C                             : 'TIS7700I1' : tis7700i1 : %SIZE(tis7700i1)
005700090904     C                             : 'TIS7700O1' : tis7700o1 : %SIZE(tis7700o1)
005800090904     C                             )
005900100224
006000100224     ** Prima cerco col codice cliente (figlio).
006100100224     C                   IF        tis7700o1.subkSun = *BLANK
006200100224     C                   IF        docMitI75 > *ZERO
006300100224     C/EXEC SQL
006400100311     C+ SELECT DISTINCT TIVTA10F.VTADSP
006500100311     C+               , DIGITS(TIVTA10F.VTALNP) CONCAT
006600100311     C+                 DIGITS(TIVTA10F.VTANRS) CONCAT
006700100311     C+                 DIGITS(TIVTA10F.VTANSP)
006800100311     C+ INTO :dsp
006900100311     C+    , :nSpedizO75
007000100224     C+ FROM TIVTA10F
007100100224     C+ WHERE TIVTA10F.VTACLI = :ktaKsu
007200100224     C+   AND TIVTA10F.VTARMN = :ktaRmn
007300100224     C/END-EXEC
007400100224     C                   ELSE
007500100224     C/EXEC SQL
007600100311     C+ SELECT DISTINCT TIVTA10F.VTADSP
007700100311     C+               , DIGITS(TIVTA10F.VTALNP) CONCAT
007800100311     C+                 DIGITS(TIVTA10F.VTANRS) CONCAT
007900100311     C+                 DIGITS(TIVTA10F.VTANSP)
008000100311     C+ INTO :dsp
008100100311     C+    , :nSpedizO75
008200100311     C+ FROM TIVTA10F
008300100224     C+ WHERE TIVTA10F.VTACLI = :ktaKsu
008400100224     C+   AND TIVTA10F.VTARMA = :rma
008500100224     C/END-EXEC
008600100224     C                   ENDIF
008700100224     C                   ELSE
008800100224     C                   IF        docMitI75 > *ZERO
008900100224     C/EXEC SQL
009000100311     C+ SELECT DISTINCT TIVTA10F.VTADSP
009100100311     C+               , DIGITS(TIVTA10F.VTALNP) CONCAT
009200100311     C+                 DIGITS(TIVTA10F.VTANRS) CONCAT
009300100311     C+                 DIGITS(TIVTA10F.VTANSP)
009400100311     C+ INTO :dsp
009500100311     C+    , :nSpedizO75
009600100311     C+ FROM TIVTA10F
009700100224     C+ WHERE TIVTA10F.VTACLI = :ktaKsu
009800100224     C+   AND TIVTA10F.VTARMN = :ktaRmn
009900100224     C+   AND TIVTA10F.VTASUBK = :tis7700o1.subkSun
010000100224     C/END-EXEC
010100100224     C                   ELSE
010200100224     C/EXEC SQL
010300100311     C+ SELECT DISTINCT TIVTA10F.VTADSP
010400100311     C+               , DIGITS(TIVTA10F.VTALNP) CONCAT
010500100311     C+                 DIGITS(TIVTA10F.VTANRS) CONCAT
010600100311     C+                 DIGITS(TIVTA10F.VTANSP)
010700100311     C+ INTO :dsp
010800100311     C+    , :nSpedizO75
010900100311     C+ FROM TIVTA10F
011000100224     C+ WHERE TIVTA10F.VTACLI = :ktaKsu
011100100224     C+   AND TIVTA10F.VTARMA = :rma
011200100224     C+   AND TIVTA10F.VTASUBK = :tis7700o1.subkSun
011300100224     C/END-EXEC
011400100224     C                   ENDIF
011500100224     C                   ENDIF
011600100224
011700100224     C                   SELECT
011800100224     C                   WHEN      sqlState = RESULT_TABLE_WITH_MORE_THAN_1_ROW
011900100311     C                   EVAL      esito = '2'
012000100311     C                   RETURN
012100100224     C                   WHEN      sqlState = RESULT_TABLE_EMPTY
012200100311     C                   WHEN      sqlState > '03'
012300100311     C                   DUMP(A)
012400100311     C                   EVAL      esito = '1'
012500100311     C                   RETURN
012600100224     C                   OTHER
012700100309     C                   EVAL      esito = '0'
012800100224     C                   RETURN
012900100224     C                   ENDSL
013000090904
013100100224     ** Poi cerco col codice unificante (padre).
013200090904     C                   IF        tis7700o1.subkSun = *BLANK
013300100223     C                   IF        docMitI75 > *ZERO
013400090904     C/EXEC SQL
013500090904     C+ SELECT DIGITS(TIVTA10F.VTALNP) CONCAT
013600090904     C+        DIGITS(TIVTA10F.VTANRS) CONCAT
013700090904     C+        DIGITS(TIVTA10F.VTANSP)
013800090904     C+ INTO :nSpedizO75
013900090904     C+ FROM TIVTA10F
014000090904     C+ WHERE TIVTA10F.VTAKSU = :ktaKsu
014100090904     C+   AND TIVTA10F.VTARMN = :ktaRmn
014200090904     C/END-EXEC
014300090904     C                   ELSE
014400100223     C/EXEC SQL
014500100223     C+ SELECT DIGITS(TIVTA10F.VTALNP) CONCAT
014600100223     C+        DIGITS(TIVTA10F.VTANRS) CONCAT
014700100223     C+        DIGITS(TIVTA10F.VTANSP)
014800100223     C+ INTO :nSpedizO75
014900100223     C+ FROM TIVTA10F
015000100223     C+ WHERE TIVTA10F.VTAKSU = :ktaKsu
015100100223     C+   AND TIVTA10F.VTARMA = :rma
015200100223     C/END-EXEC
015300100223     C                   ENDIF
015400100223     C                   ELSE
015500100223     C                   IF        docMitI75 > *ZERO
015600090904     C/EXEC SQL
015700090904     C+ SELECT DIGITS(TIVTA10F.VTALNP) CONCAT
015800090904     C+        DIGITS(TIVTA10F.VTANRS) CONCAT
015900090904     C+        DIGITS(TIVTA10F.VTANSP)
016000090904     C+ INTO :nSpedizO75
016100090904     C+ FROM TIVTA10F
016200090904     C+ WHERE TIVTA10F.VTAKSU = :ktaKsu
016300090904     C+   AND TIVTA10F.VTARMN = :ktaRmn
016400090904     C+   AND TIVTA10F.VTASUBK = :tis7700o1.subkSun
016500090904     C/END-EXEC
016600100223     C                   ELSE
016700100223     C/EXEC SQL
016800100223     C+ SELECT DIGITS(TIVTA10F.VTALNP) CONCAT
016900100223     C+        DIGITS(TIVTA10F.VTANRS) CONCAT
017000100223     C+        DIGITS(TIVTA10F.VTANSP)
017100100223     C+ INTO :nSpedizO75
017200100223     C+ FROM TIVTA10F
017300100223     C+ WHERE TIVTA10F.VTAKSU = :ktaKsu
017400100223     C+   AND TIVTA10F.VTARMA = :rma
017500100223     C+   AND TIVTA10F.VTASUBK = :tis7700o1.subkSun
017600100223     C/END-EXEC
017700090904     C                   ENDIF
017800100223     C                   ENDIF
017900090904
018000090904     C                   SELECT
018100100224     C                   WHEN      sqlState = RESULT_TABLE_WITH_MORE_THAN_1_ROW
018200100224     C                   EVAL      esito = '2'
018300090904     C                   WHEN      sqlState = RESULT_TABLE_EMPTY
018400090904     C                   EVAL      esito = '3'
018500100311     C                   WHEN      sqlState > '03'
018600100311     C                   DUMP(A)
018700100311     C                   EVAL      esito = '1'
018800100311     C                   RETURN
018900090904     C                   OTHER
019000090904     C                   EVAL      esito = '0'
019100090904     C                   ENDSL
019200090904
019300000405     c*--------------------------------------------------------------------------------------------*
019400001102     c* Operazioni iniziali
019500000405     c*--------------------------------------------------------------------------------------------*
019600000405     c     *inzsr        BEGSR
019700000405     c*
019800000405     c* ricevimento parametri
019900000405     c     *ENTRY        PLIST
020000000405     c                   PARM                    esito
020100001102     c                   PARM                    tis175dsi
020200001102     c                   PARM                    tis175dso
020300001108     c*
020400001108     c                   MOVEL     '0'           esito
020500090904     c                   CLEAR                   tis175dso
020600001108     c*
020700001108     c* verifico i valori passati in input
020800100223     c                   IF        KSCI75 = *blanks
020900100223     c                   MOVEL     '1'           esito
021000100223     c                   LEAVESR
021100100223     c                   ENDIF
021200100223     c                   IF        docMitI75 = *ZERO AND rmaI75 = *BLANK
021300100223     c                   MOVEL     '1'           esito
021400100223     c                   LEAVESR
021500100223     c                   ENDIF
021600001102     c*
021700001102     c* valorizzo subito i campi chiave dai parametri ricevuti in input
021800100223     c                   Z-ADD     docMitI75     ktarmn
021900100415     c                   EVAL      rma = %TRIM(rmaI75)
022000001102     c                   MOVEL     *all'0'       ktaksu
022100001102     c                   EVAL      lung  = %len(%trim(KSCI75))
022200001102     c                   EVAL      %subst(ktaksu:(8+1-lung):lung)=%trim(KSCI75)
022300001103     c                   EVAL      KSCI75 = ktaksu
022400000405     c*
022500090904     c                   ENDSR
