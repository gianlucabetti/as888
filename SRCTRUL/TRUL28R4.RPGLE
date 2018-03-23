000100990908     H dftactgrp(*yes)
000200990908
000300010213
000400090916     D  TRUL28DSX    e ds
000500020904     D  SkChkDgt       s              1    DIM(35)
000600000830
000700010201
000800000613     C*
000900090916     C                   SELECT
001000090916     C                   WHEN      I284STD = 'E10'
001100040706     C                   EXSR      CHKDGTEAN10
001200090916     C                   WHEN      I284STD = 'E13'
001300090916     C                   EXSR      CHKDGTEAN13
001400090916     C                   ENDSL
001500000616     C*
001600040706     C                   SETON                                        LR
001700000613
001800020904
001900020904
002000020904     C*--------------------------------------------------------
002100040706     C* CHKDGTEAN10 - ROUTINE CALCOLO CHECK-DIGITS     (EAN 10)
002200020904     C*--------------------------------------------------------
002300040706     C     CHKDGTEAN10   BEGSR
002400020904     C*
002500020904     C                   CLEAR                   SkChkDgt
002600040709     C                   MOVEA     I284COD       SkChkDgt
002700020904     C* Inizializzo variabili di wrk
002800020904     C                   Z-ADD     *zeros        WrkChkDgt         1 0
002900020904     C                   Z-ADD     *zeros        WrkChkDgtNum      1 0
003000020904     C                   MOVEL     *blanks       WrkChkDgtAlfa     1
003100040709     C                   Z-ADD     *zeros        SommaPari         9 0
003200040709     C                   Z-ADD     *zeros        SommaDispari      9 0
003300040709     C                   Z-ADD     *zeros        SommaTot          9 0
003400040709     C                   Z-ADD     *zeros        Risultato         9 1
003500020904     C*
003600020904     C* Effettuo il calcolo
003700040706     C     1             DO        35            I                 2 0
003800020904     C                   IF        SkChkDgt(I) <> *blanks
003900020904     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
004000040706     C* Stabilisco se trattasi di cifra in posizione pari o dispari
004100020904     C                   EVAL      Risultato = I/2
004200020904     C                   Z-ADD     Risultato     RestoDec          1 1
004300020904     C                   IF        RestoDec = *zeros                            * è pari
004400020904     C                   ADD       WrkChkDgt     SommaPari
004500020904     C                   ELSE                                                   * è dispari
004600020904     C                   ADD       WrkChkDgt     SommaDispari
004700020904     C                   ENDIF
004800020904     C                   ENDIF
004900020904     C                   ENDDO
005000020904     C*
005100040709     C                   EVAL      SommaTot = (SommaDispari*3) + (SommaPari*1)
005200040709     C                   Z-ADD     *zeros        Multiplo10        9 0
005300020904     C                   DOW       Multiplo10 < SommaTot
005400020904     C                   EVAL      Multiplo10 = Multiplo10 + 10
005500020904     C                   ENDDO
005600020904     C*
005700040706     C* Determino qundi il check-digits
005800020904     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
005900020904     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
006000040709     C                   EVAL      O284CKD = WrkChkDgtAlfa
006100040706     C*
006200040706     C* Compongo il codice finale comprensivo d check-digits
006300040709     C                   EVAL      O284COD = %trim(I284COD) + WrkChkDgtAlfa
006400040706     C*
006500040706     C* Se check-digits così calcolato nn è valorizzato restituisco esito d errore
006600040706     C                   IF        WrkChkDgtAlfa = *blanks
006700040709     C                   EVAL      O284ERR = '1'
006800040706     C                   ENDIF
006900020904     C*
007000020904     C                   ENDSR
007100020904     C***
007200090916
007300020904
007400090916
007500090916     C*--------------------------------------------------------
007600090916     C* CHKDGTEAN13 - ROUTINE CALCOLO CHECK-DIGITS     (EAN 7/13)
007700090916     C*--------------------------------------------------------
007800090916     C     CHKDGTEAN13   BEGSR
007900090916     C*
008000090916     C                   CLEAR                   SkChkDgt
008100090916     C                   MOVEA     I284COD       SkChkDgt
008200090916     C* Inizializzo variabili di wrk
008300090916     C                   Z-ADD     *zeros        WrkChkDgt         1 0
008400090916     C                   Z-ADD     *zeros        WrkChkDgtNum      1 0
008500090916     C                   MOVEL     *blanks       WrkChkDgtAlfa     1
008600090916     C                   Z-ADD     *zeros        SommaPari         9 0
008700090916     C                   Z-ADD     *zeros        SommaDispari      9 0
008800090916     C                   Z-ADD     *zeros        SommaTot          9 0
008900090916     C                   Z-ADD     *zeros        Risultato         9 1
009000090916     C*
009100090916     C* Effettuo il calcolo
009200090916     C     1             DO        35            I                 2 0
009300090916     C                   IF        SkChkDgt(I) <> *blanks
009400090916     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
009500090916     C* Stabilisco se trattasi di cifra in posizione pari o dispari
009600090916     C                   EVAL      Risultato = I/2
009700090916     C                   Z-ADD     Risultato     RestoDec          1 1
009800090916     C                   IF        RestoDec = *zeros                            * è pari
009900090916     C                   ADD       WrkChkDgt     SommaPari
010000090916     C                   ELSE                                                   * è dispari
010100090916     C                   ADD       WrkChkDgt     SommaDispari
010200090916     C                   ENDIF
010300090916     C                   ENDIF
010400090916     C                   ENDDO
010500090916     C*
010600090916     C                   EVAL      SommaTot = (SommaPari*3) + (SommaDispari*1)
010700090916     C                   Z-ADD     *zeros        Multiplo10        9 0
010800090916     C                   DOW       Multiplo10 < SommaTot
010900090916     C                   EVAL      Multiplo10 = Multiplo10 + 10
011000090916     C                   ENDDO
011100090916     C*
011200090916     C* Determino qundi il check-digits
011300090916     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
011400090916     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
011500090916     C                   EVAL      O284CKD = WrkChkDgtAlfa
011600090916     C*
011700090916     C* Compongo il codice finale comprensivo d check-digits
011800090916     C                   EVAL      O284COD = %trim(I284COD) + WrkChkDgtAlfa
011900090916     C*
012000090916     C* Se check-digits così calcolato nn è valorizzato restituisco esito d errore
012100090916     C                   IF        WrkChkDgtAlfa = *blanks
012200090916     C                   EVAL      O284ERR = '1'
012300090916     C                   ENDIF
012400090916     C*
012500090916     C                   ENDSR
012600090916     C***
012700090916
012800090916
012900020904
013000990910
013100000613     C     *inzsr        BEGSR
013200990910     C*
013300990910     C     *entry        plist
013400090916     C                   parm                    TRUL28DSX
013500000830     C*
013600000613     C                   ENDSR
013700040706     C***
