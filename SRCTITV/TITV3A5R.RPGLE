000100170829     H DFTACTGRP(*NO) ACTGRP('BARTVAS')
000200170829     H DECEDIT('0,') DATEDIT(*DMY.)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500170829     Fwsplit0f  O    E             DISK    rename(wsplit0f:wsplit00)
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300010330     D esito           s              1
001400010330     D prmlit          s             10
001500010330     D prmfir          s             10
001600010330     D wrkesito        s                   like(esito)
001700010330     D rrnum           s              6  0 INZ(*zeros)
001800030715     D*------------------
001900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002000030715     D*------------------
002100030715     D SkSplitFLD      S             10    DIM(100)
002200030715     D SkSplitCSV      S            256    DIM(100)
002300030715     D CharCSV         S              1
002400030715     D CharTXT         S              1
002500030715     D CharNUM         S              1
002600090403     D CharSOS         S              1
002700030715     D posDa           S              3  0 INZ(*zeros)
002800030715     D posA            S              3  0 INZ(*zeros)
002900030715     D i               s              3  0 INZ(1)
003000030715     D wGiro           s              1  0 INZ(*zeros)
003100030822     D*------------------
003200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003300030822     D*------------------
003400030822     D posDaDft        S              3  0 INZ(*zeros)
003500030822     D posADft         S              3  0 INZ(*zeros)
003600030822     D j               s              3  0 INZ(1)
003700030822     D wGiroDft        s              1  0 INZ(*zeros)
003800170829     D*------------------
003900170829     D* Costanti
004000170829     D*------------------
004100170829     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004200170829     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004300010330
004400010330
004500010330
004600000913     C                   reset                   rrnum
004700990921     C                   reset                   esito
004800990921     C                   reset                   wrkesito
004900010601     C*
005000010601     C                   exsr      opeini
005100170829     C                   exsr      rwFIL
005200010601     C*
005300010601     C                   seton                                        lr
005400010601
005500010601
005600010330     C*--------------------------------------------------------
005700170829     C* RWFIL   LEGGE TIVIN00R E SCRIVE WSPLIT0F              *
005800010330     C*--------------------------------------------------------
005900170829     C     RWFIL         BEGSR
006000170829     C*
006100170829     C                   if        not %open(tivin00r)
006200170829     C                   open      tivin00r
006300170829     C                   endif
006400170829     C*
006500030822     C                   do        *HIVAL
006600010330     C*
006700030822     C                   read      tivin00r                               70
006800010618     C*
006900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
007000040510     C                   z-add     *zeros        wLenVINDTA        4 0
007100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
007200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
007300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
007400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
007500040510     C                   z-add     1             wFlgVINDTA
007600040510     C                   leave
007700040510     C                   endif
007800040510     C                   enddo
007900040510     C*
008000040218     C                   if        vindta > *blanks AND
008100040510     C                             wFlgVINDTA = 1
008200010330     C                   add       1             rrnum
008300010330     C*
008400010601     C                   if        *in70 = *off and
008500010330     C                             (vinflg = *blanks
008600010330     C                              or vinflg = '0'
008700010330     C                              or vinflg = '2')
008800010330     C*
008900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009000010711     C                   if        vinflg = *blanks or vinflg = '0'
009100010711     C                   clear                   vinmsg
009200010711     C                   endif
009300010601     C*
009400010330     C                   exsr      inzvar
009500040714     C                   exsr      impvat
009600010601     C*
009700010604     C* Ebbene...
009800010604     C*
009900030822     C                   if        wGiro = 2
010000170829     C                   write     WSPLIT00
010100030822     C                   endif
010200010604     C*
010300010604     C                   if        *in31 = *off and
010400010604     C                             *in32 = *off
010500010604     C                   eval      vinflg = '1'
010600010604     C                   else
010700010604     C                   eval      vinflg = '2'
010800010604     C                   endif
010900010601     C*
011000010604     C                   endif
011100010604     C*
011200010330     C                   else
011300010330     C                   eval      vinflg = '1'
011400010330     C                   endif
011500010601     C*
011600010601     C  N70              update    tivin000
011700010330     C*
011800030822     C  N70              enddo
011900010601     C
012000010601     C*
012100010330     C* ... restituisco esito OK.
012200010330     C                   eval      wrkesito = '0'
012300010330     C*
012400010330     C                   if        %open(tivin00r)
012500010330     C                   close     tivin00r
012600010330     C                   endif
012700010330     C*
012800010330     C                   ENDSR
012900010330     C***
013000010601
013100010601
013200010601
013300010330     C*----------------------------------------------------*
013400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
013500010330     C*----------------------------------------------------*
013600010330     C     INZVAR        BEGSR
013700010330     C*
013800030822     C* Inizializzo variabili di wrk
013900020204     C                   Z-ADD     *zeros        Num5_0            5 0
014000030822     C*
014100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
014200170829     C                   CLEAR                   wsplit00
014300030822     C                   CLEAR                   SkSplitCSV
014400030822     C                   EVAL      i = 1
014500030822     C                   EVAL      posDa = *zeros
014600030822     C                   EVAL      posA  = *zeros
014700030822     C*
014800030822     C* Reimposto i valori di default
014900030822     C                   EXSR      DEFCAM
015000030822     C*
015100010330     C                   ENDSR
015200010330     C*----------------------------------------------------*
015300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
015400010330     C*----------------------------------------------------*
015500010330     C     DEFCAM        BEGSR
015600030822     C*
015700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
015800030715     C* e delimitatore testo.
015900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
016000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
016100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
016200090403     C*
016300090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
016400090403     C                   EVAL      CharSOS = CharNUM
016500020204     C*
016600010330     C                   ENDSR
016700010607     C*----------------------------------------------------*
016800040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
016900010607     C*----------------------------------------------------*
017000040714     C     IMPVAT        BEGSR
017100010607     C*
017200010607     C                   SETOFF                                       3132
017300010607     C*
017400030715     C**********
017500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
017600030715     C**********
017700030715     C                   IF        wGiro = *zeros
017800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
017900031201     C     minu:maiu     XLATE     vindta        vindta
018000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
018100040324     C                   DOW       posDa <= %len(%trimr(vindta))
018200030715     C*
018300030715     C* Gestisco il 1° campo
018400030715     C                   IF        i = 1
018500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
018600030715     C                   EVAL      posDa = 1
018700030715     C* Eseguo lo scan x trovare la fine del primo campo
018800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
018900030715     C* A questo punto "estraggo" il campo corrente
019000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
019100030715     C                                                    (posA-posDa))
019200030715     C* X i campi successivi al 1°
019300030715     C                   ELSE
019400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
019500030715     C                   EVAL      posDa = posA + 1
019600030715     C* Eseguo lo scan x trovare la fine del campo corrente
019700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
019800030715     C* Gestisco l'ultimo campo
019900030715     C                   IF        posA = *zeros
020000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
020100030715     C* A questo punto "estraggo" il campo corrente
020200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
020300030715     C                                                    (posA-posDa))
020400030715     C                   LEAVE
020500030715     C                   ELSE
020600030715     C* A questo punto "estraggo" il campo corrente
020700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
020800030715     C                                                    (posA-posDa))
020900030715     C                   ENDIF
021000030715     C                   ENDIF
021100030715     C* Incremento il contatore d campo
021200030715     C                   EVAL      i = i +1
021300030715     C                   ENDDO
021400030715     C*
021500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
021600030715     C* delimitazione testo
021700030715     C                   EVAL      i = 1
021800030715     C                   DOW       i < %elem(SkSplitFLD)
021900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
022000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
022100030715     C                   EVAL      i = i + 1
022200030715     C                   ENDDO
022300030715     C                   EVAL      wGiro = 1
022400030715     C                   ELSE
022500030715     C                   EVAL      wGiro = 2
022600031204     C**********
022700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
022800031204     C* d separatore campo
022900031204     C**********
023000040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
023100040304     C                   EVAL      vindta = ' ' + vindta
023200040304     C                   ENDIF
023300031204     C*
023400031204     C                   Z-ADD     *zeros        lunghInput        4 0
023500040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
023600040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
023700040304     C                   EVAL      vindta = vindta + ' '
023800040304     C                   ENDIF
023900030715     C**********
024000030715     C* Effettuo lo split del campo dati d input
024100030715     C**********
024200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
024300040324     C                   DOW       posDa <= %len(%trimr(vindta))
024400030715     C*
024500030715     C* Gestisco il 1° campo
024600030715     C                   IF        i = 1
024700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
024800030715     C                   EVAL      posDa = 1
024900030715     C* Eseguo lo scan x trovare la fine del primo campo
025000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
025100030715     C* A questo punto "estraggo" il campo corrente
025200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
025300030715     C                                                    (posA-posDa))
025400030715     C* X i campi successivi al 1°
025500030715     C                   ELSE
025600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
025700030715     C                   EVAL      posDa = posA + 1
025800031204     C* Verifico che nn vi sia il campo nn valorizzato
025900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
026000031204     C* Se campo nn valorizzato skippo al prossimo
026100031204     C                   EVAL      SkSplitCSV(i) = *blanks
026200031204     C                   EVAL      posA  = posA + 1
026300031204     C                   ELSE
026400030715     C* Eseguo lo scan x trovare la fine del campo corrente
026500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
026600030715     C* Gestisco l'ultimo campo
026700030715     C                   IF        posA = *zeros
026800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
026900030715     C* A questo punto "estraggo" il campo corrente
027000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
027100030715     C                                                    (posA-posDa))
027200030715     C                   LEAVE
027300030715     C                   ELSE
027400030715     C* A questo punto "estraggo" il campo corrente
027500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
027600030715     C                                                    (posA-posDa))
027700030715     C                   ENDIF
027800031204     C                   ENDIF
027900030715     C                   ENDIF
028000030715     C* Incremento il contatore d campo
028100030715     C                   EVAL      i = i +1
028200030715     C                   ENDDO
028300030715     C*
028400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
028500030715     C* delimitazione testo
028600030715     C                   EVAL      i = 1
028700030715     C                   DOW       i < %elem(SkSplitCSV)
028800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
028900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
029000030715     C                   EVAL      i = i + 1
029100030715     C                   ENDDO
029200030715     C*
029300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
029400030715     C                   EVAL      i = 1
029500030715     C                   DOW       i < %elem(SkSplitFLD)
029600030715     C*
029700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
029800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
029900030715     C                             SkSplitCSV(i) <> *blanks
030000170829     C*** SPIIVA
030100170829     C                   IF        %trim(SkSplitFLD(i)) = 'SPIIVA'
030200170829     C                   EVAL      SPIIVA = SkSplitCSV(i)
030300030721     C                   ENDIF
030400170829     C*** SPIRAGSOC
030500170829     C                   IF        %trim(SkSplitFLD(i)) = 'SPIRAGSOC'
030600170829     C                   EVAL      SPIRAGSOC = SkSplitCSV(i)
030700170829     C                   ENDIF
030800030715     C*
030900030715     C                   ENDIF
031000030715     C                   EVAL      i = i + 1
031100030715     C                   ENDDO
031200020204     C*
031300030715     C                   ENDIF
031400020204     C*
031500010607     C                   ENDSR
031600010607     C*----------------------------------------------------*
031700010601
031800010601
031900010601      /TITLE Invio dei dati al punto operativo.
032000010601     C     opeini        BEGSR
032100010601     C*
032200010601     C* Inizializzo flag e contatori operativi
032300010601     C                   movel     '0'           flgGiro           1
032400010601     C                   movel     '0'           flgMulti          1
032500010601     C                   movel     '1'           flgStato          1
032600010615     C                   movel     '0'           flgOk             1
032700010601     C*
032800010601     C                   ENDSR
032900010601     C***
033000010330
033100010330
033200000613     C     *inzsr        BEGSR
033300990910     C*
033400990910     C     *entry        plist
033500990920     C                   parm                    tivlrds
033600990921     C                   parm      wrkesito      esito
033700000724     C                   parm                    prmlit
033800000710     C                   parm                    prmfir
033900000613     C*
034000000613     C                   ENDSR
034100000613     C***
