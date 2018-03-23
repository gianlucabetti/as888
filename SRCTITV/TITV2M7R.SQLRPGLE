000100121026      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121029     H DECEDIT('0.') DATEDIT(*DMY.)
000300990908     H dftactgrp(*yes)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600121026     FFIVABwwr  O    E             DISK    usropn
000700000313     D*
000800040726     D*------------
000900040726     D* SCHIEDE A PROGRAMMA
001000040726     D*------------
001100040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900121026     D fivab00f      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002600030715     D*------------------
002700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002800030715     D*------------------
002900030715     D SkSplitFLD      S             10    DIM(100)
003000030715     D SkSplitCSV      S            256    DIM(100)
003100030715     D CharCSV         S              1
003200030715     D CharTXT         S              1
003300030715     D CharNUM         S              1
003400030715     D posDa           S              3  0 INZ(*zeros)
003500030715     D posA            S              3  0 INZ(*zeros)
003600030715     D i               s              3  0 INZ(1)
003700030715     D wGiro           s              1  0 INZ(*zeros)
003800030822     D*------------------
003900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004000030822     D*------------------
004100030822     D posDaDft        S              3  0 INZ(*zeros)
004200030822     D posADft         S              3  0 INZ(*zeros)
004300030822     D j               s              3  0 INZ(1)
004400030822     D wGiroDft        s              1  0 INZ(*zeros)
004500121026
004600121026     D*------------------
004700121026     D* Costanti
004800121026     D*------------------
004900121026     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000121026     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100121026
005200010330
005300010330
005400120309     C*
005500120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005600120309     C
005700120309     C/EXEC SQL
005800120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
005900120309     C/END-EXEC
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300010601     C*
006400010601     C                   exsr      opeini
006500121026     C                   exsr      rwvab
006600010601     C*
006700010601     C                   seton                                        lr
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007300010601     C*--------------------------------------------------------
007400010601     C     PREELA        BEGSR
007500010601     C*
007600010601     C* SE OCCORRE SPEDIRE IN FILIALE
007700010601     C                   if        invfil <> *zeros and
007800010601     C                             flgGiro = '0'
007900010601     C*
008000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008100010601     C                   eval      flgGiro = '1'
008200010601     C*
008300010601     C                   endif
008400010601     C*
008500010601     C                   ENDSR
008600010601     C***
008700010601
008800010601
008900010601     C*--------------------------------------------------------
009000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009100010601     C*--------------------------------------------------------
009200010601     C     ENDELA        BEGSR
009300000616     C*
009400010601     C                   ENDSR
009500010601     C***
009600010601
009700010601
009800010330     C*--------------------------------------------------------
009900121026     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010000010330     C*--------------------------------------------------------
010100121026     C     RWVAB         BEGSR
010200010330     C*
010300010330     C                   if        not %open(tivin00r)
010400010330     C                   open      tivin00r
010500010330     C                   endif
010600121026     C                   if        not %open(fivabwwr)
010700121026     C                   open      fivabwwr
010800010330     C                   endif
010900010330     C*
011000010604     C                   clear                   §CTROK
011100010604     C                   clear                   §CTRMO
011200010604     C                   clear                   §CTRNO
011300040726     C*
011400040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011500040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011600040726     C                   exsr      inzvar
011700121026     C                   exsr      impvab
011800010330     C*
011900030822     C                   do        *HIVAL
012000010330     C*
012100030822     C                   read      tivin00r                               70
012200010618     C*
012300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012400010618     C                   if        vinflg = '1'
012500010618     C                   eval      flgOk = '1'
012600010618     C                   endif
012700010618     C*
012800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012900040510     C                   z-add     *zeros        wLenVINDTA        4 0
013000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013400040510     C                   z-add     1             wFlgVINDTA
013500040510     C                   leave
013600040510     C                   endif
013700040510     C                   enddo
013800040510     C*
013900040218     C                   if        vindta > *blanks AND
014000040510     C                             wFlgVINDTA = 1
014100010330     C                   add       1             rrnum
014200010330     C*
014300010601     C                   if        *in70 = *off and
014400010330     C                             (vinflg = *blanks
014500010330     C                              or vinflg = '0'
014600010330     C                              or vinflg = '2')
014700010330     C*
014800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014900010711     C                   if        vinflg = *blanks or vinflg = '0'
015000010711     C                   clear                   vinmsg
015100010711     C                   endif
015200010601     C*
015300010330     C                   exsr      inzvar
015400121026     C                   exsr      impvab
015500010601     C*
015600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
015700121026     C                   eval      depfil = VABLNP
015800010601     C                   exsr      repfil
015900010601     C                   if        depfil = invfil
016000021025     C                   if        vlrpoi = 999
016100121026     C                   move(P)   invfil        VABFGS
016200021025     C                   else
016300121026     C                   move(P)   vlrpoi        VABFGS
016400021025     C                   endif
016500010601     C*
016600010601     C                   exsr      PREELA
016700010601     C*
016800010604     C* Ebbene...
016900010604     C*
017000030822     C  N31              add       1             §CTROK            7 0
017100030822     C   32              add       1             §CTRMO            7 0
017200030822     C   31              add       1             §CTRNO            7 0
017300030822     C                   if        wGiro = 2
017400110222     C*
017500110222     C                   if        not *in30
017600121026 xxx C*
017700121026     C*
017800030822     C                   endif
017900121026     C                   endif
018000010604     C*
018100010604     C                   if        *in31 = *off and
018200121029     C                             *in32 = *off
018300010604     C                   eval      vinflg = '1'
018400010604     C                   else
018500010604     C                   eval      vinflg = '2'
018600010604     C                   endif
018700010604     C                   endif
018800010601     C*
018900010604     C                   endif
019000010604     C*
019100010330     C                   else
019200010330     C                   eval      vinflg = '1'
019300010330     C                   endif
019400010601     C*
019500010601     C  N70              update    tivin000
019600010330     C*
019700030822     C  N70              enddo
019800010601     C
019900010601     C*
020000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
020100010601     C                   if        cntNonEl = *zeros or
020200010601     C                             flgMulti = '0'
020300010330     C* Se non ci sono record con errori ...
020400010601     C                   if        §ctrno = 0 and
020500010604     C                             §ctrmo = 0 and
020600010601     C                             flgStato <> '2'
020700010330     C* ... restituisco esito OK.
020800010330     C                   eval      wrkesito = '0'
020900010330     C                   else
021000010330     C                   if        §ctrok > 0
021100010330     C                   eval      wrkesito = '1'
021200010330     C                   else
021300010615     C                   if        flgOk = '0'
021400010615     C                   eval      wrkesito = '2'
021500010615     C                   else
021600010615     C                   eval      wrkesito = '6'
021700010615     C                   endif
021800010330     C                   endif
021900010330     C                   endif
022000010601     C                   else
022100010601     C                   eval      wrkesito = '9'
022200010601     C                   endif
022300010330     C*
022400010330     C                   if        %open(tivin00r)
022500010330     C                   close     tivin00r
022600010330     C                   endif
022700121026     C                   if        %open(fivabwwr)
022800121026     C                   close     fivabwwr
022900010330     C                   endif
023000010601     C*
023100010601     C                   if        vlrpoi <> 999
023200010601     C                   eval      invfil = vlrpoi
023300010601     C                   endif
023400010330     C*
023500010330     C                   if        §ctrok > 0
023600010601     C                             and invfil > *zeros
023700010330     C                   exsr      invio
023800010330     C                   endif
023900010601     C*
024000010618     C                   if        flgGiro = '1'
024100010601     C                   exsr      endela
024200010618     C                   endif
024300010330     C*
024400010330     C                   ENDSR
024500010330     C***
024600010601
024700010601
024800010601
024900010330     C*----------------------------------------------------*
025000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
025100010330     C*----------------------------------------------------*
025200010330     C     INZVAR        BEGSR
025300010330     C*
025400030822     C* Inizializzo variabili di wrk
025500020204     C                   Z-ADD     *zeros        Num5_0            5 0
025600030822     C*
025700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
025800121026     C                   CLEAR                   FIVAB000
025900030822     C                   CLEAR                   SkSplitCSV
026000030822     C                   EVAL      i = 1
026100030822     C                   EVAL      posDa = *zeros
026200030822     C                   EVAL      posA  = *zeros
026300110222     C*
026400110222     C                   MOVEL     *blanks       wRSD             35
026500110222     C                   MOVEL     *blanks       wRSD_1           35
026600110222     C                   MOVEL     *blanks       wRSD_2           35
026700110222     C                   MOVEL     *blanks       wNOT_A_1         35
026800110222     C                   MOVEL     *blanks       wNOT_A_2         35
026900110222     C                   MOVEL     *blanks       wNOT_B_1         35
027000110222     C                   MOVEL     *blanks       wNOT_B_2         35
027100030822     C*
027200030822     C* Reimposto i valori di default
027300030822     C                   EXSR      DEFCAM
027400030822     C*
027500010330     C                   ENDSR
027600010330     C*----------------------------------------------------*
027700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
027800010330     C*----------------------------------------------------*
027900010330     C     DEFCAM        BEGSR
028000030822     C*
028100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
028200030715     C* e delimitatore testo.
028300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
028400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
028500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
028600030822     C*
028700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
028800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
028900030715     C                   SETON                                        50
029000030715     C                   ELSE
029100030715     C                   SETOFF                                       50
029200030715     C                   ENDIF
029300030822     C*
029400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
029500030822     C                   EVAL      posDaDft = 1
029600030822     C                   EVAL      posADft  = 0
029700030822     C                   EVAL      wGiroDft = 0
029800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
029900030822     C                             posDaDft > 0
030000030822     C*
030100030822     C* Gestisco il 1° giro
030200030822     C                   IF        wGiroDft = 0
030300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
030400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
030500030822     C* Incremento il contatore dei "giri"
030600030822     C                   EVAL      wGiroDft = 1
030700030822     C                   ELSE
030800030822     C                   EVAL      posDaDft = posADft
030900030822     C                   ENDIF
031000030822     C* Eseguo lo scan x trovare la fine del campo corrente
031100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
031200030822     C*
031300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
031400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
031500030822     C                   IF        posDaDft > 0 AND
031600030822     C                             posADft  > 0
031700030822     C* CCM
031800030822     C                   IF        %subst(
031900030822     C                             %subst(vlrppt:posDaDft+1:
032000030822     C                             posADft-posDaDft-1):1:3)
032100030822     C                             = 'CCM'
032200030822     C                   EVAL      PiStr=%trim(%subst(
032300030822     C                             %subst(vlrppt:posDaDft+1:
032400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032500030822     C                   EXSR      CHKNUM
032600030822     C                   IF        PiInt=*on
032700121026     C                   Z-ADD     PiVal         VABCCM
032800030822     C                   ENDIF
032900030822     C                   ENDIF
033000030822     C* LNP
033100030822     C                   IF        %subst(
033200030822     C                             %subst(vlrppt:posDaDft+1:
033300030822     C                             posADft-posDaDft-1):1:3)
033400030822     C                             = 'LNP'
033500030822     C                   EVAL      PiStr=%trim(%subst(
033600030822     C                             %subst(vlrppt:posDaDft+1:
033700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033800030822     C                   EXSR      CHKNUM
033900030822     C                   IF        PiInt=*on
034000121026     C                   Z-ADD     PiVal         VABLNP
034100030822     C                   ENDIF
034200030822     C                   ENDIF
034300030822     C* ...
034400030822     C                   ENDIF
034500030822     C                   ENDDO
034600020204     C*
034700010330     C                   ENDSR
034800010607     C*----------------------------------------------------*
034900121026     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035000010607     C*----------------------------------------------------*
035100121026     C     IMPVAB        BEGSR
035200010607     C*
035300110222     C                   SETOFF                                       313230
035400010607     C*
035500030715     C**********
035600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
035700030715     C**********
035800030715     C                   IF        wGiro = *zeros
035900040726     C*
036000040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
036100040726     C                   EVAL      depvinDTA = vinDTA
036200040726     C*
036300040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
036400040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
036500040726     C                                      %trim(intcol(3))
036600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
036700031201     C     minu:maiu     XLATE     vindta        vindta
036800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
036900040324     C                   DOW       posDa <= %len(%trimr(vindta))
037000030715     C*
037100030715     C* Gestisco il 1° campo
037200030715     C                   IF        i = 1
037300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
037400030715     C                   EVAL      posDa = 1
037500030715     C* Eseguo lo scan x trovare la fine del primo campo
037600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037700030715     C* A questo punto "estraggo" il campo corrente
037800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037900030715     C                                                    (posA-posDa))
038000030715     C* X i campi successivi al 1°
038100030715     C                   ELSE
038200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
038300030715     C                   EVAL      posDa = posA + 1
038400030715     C* Eseguo lo scan x trovare la fine del campo corrente
038500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
038600030715     C* Gestisco l'ultimo campo
038700030715     C                   IF        posA = *zeros
038800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
038900030715     C* A questo punto "estraggo" il campo corrente
039000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039100030715     C                                                    (posA-posDa))
039200030715     C                   LEAVE
039300030715     C                   ELSE
039400030715     C* A questo punto "estraggo" il campo corrente
039500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039600030715     C                                                    (posA-posDa))
039700030715     C                   ENDIF
039800030715     C                   ENDIF
039900030715     C* Incremento il contatore d campo
040000030715     C                   EVAL      i = i +1
040100030715     C                   ENDDO
040200030715     C*
040300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
040400030715     C* delimitazione testo
040500030715     C                   EVAL      i = 1
040600030715     C                   DOW       i < %elem(SkSplitFLD)
040700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
040800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
040900030715     C                   EVAL      i = i + 1
041000030715     C                   ENDDO
041100030715     C                   EVAL      wGiro = 1
041200040726     C* Ripristino la 1° riga originale
041300040726     C                   EVAL      vinDTA = depvinDTA
041400030715     C                   ELSE
041500030715     C                   EVAL      wGiro = 2
041600031204     C**********
041700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
041800031204     C* d separatore campo
041900031204     C**********
042000040726     C                   EVAL      vindta = %trim(vindta)
042100040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
042200040726     C                   EVAL      vindta = %subst(vindta:2)
042300040726     C                   ENDDO
042400031204     C*
042500031204     C                   Z-ADD     *zeros        lunghInput        4 0
042600040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
042700040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
042800040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
042900040726     C                   EVAL      lunghInput = %len(%trim(vindta))
043000040726     C                   ENDDO
043100030715     C**********
043200030715     C* Effettuo lo split del campo dati d input
043300030715     C**********
043400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
043500040324     C                   DOW       posDa <= %len(%trimr(vindta))
043600030715     C*
043700030715     C* Gestisco il 1° campo
043800030715     C                   IF        i = 1
043900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
044000030715     C                   EVAL      posDa = 1
044100030715     C* Eseguo lo scan x trovare la fine del primo campo
044200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044300030715     C* A questo punto "estraggo" il campo corrente
044400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
044500030715     C                                                    (posA-posDa))
044600030715     C* X i campi successivi al 1°
044700030715     C                   ELSE
044800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
044900030715     C                   EVAL      posDa = posA + 1
045000031204     C* Verifico che nn vi sia il campo nn valorizzato
045100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
045200031204     C* Se campo nn valorizzato skippo al prossimo
045300031204     C                   EVAL      SkSplitCSV(i) = *blanks
045400031204     C                   EVAL      posA  = posA + 1
045500031204     C                   ELSE
045600030715     C* Eseguo lo scan x trovare la fine del campo corrente
045700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
045800030715     C* Gestisco l'ultimo campo
045900030715     C                   IF        posA = *zeros
046000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
046100030715     C* A questo punto "estraggo" il campo corrente
046200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046300030715     C                                                    (posA-posDa))
046400030715     C                   LEAVE
046500030715     C                   ELSE
046600030715     C* A questo punto "estraggo" il campo corrente
046700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046800030715     C                                                    (posA-posDa))
046900030715     C                   ENDIF
047000031204     C                   ENDIF
047100030715     C                   ENDIF
047200030715     C* Incremento il contatore d campo
047300030715     C                   EVAL      i = i +1
047400030715     C                   ENDDO
047500030715     C*
047600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
047700030715     C* delimitazione testo
047800030715     C                   EVAL      i = 1
047900030715     C                   DOW       i < %elem(SkSplitCSV)
048000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
048100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
048200030715     C                   EVAL      i = i + 1
048300030715     C                   ENDDO
048400030715     C*
048500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
048600030715     C                   EVAL      i = 1
048700030715     C                   DOW       i < %elem(SkSplitFLD)
048800030715     C*
048900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
049000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
049100030715     C                             SkSplitCSV(i) <> *blanks
049200121026     C*** TIC
049300121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
049400121026     C                   EVAL      VABTIC = SkSplitCSV(i)
049500030715     C                   ENDIF
049600121026     C*** RMA
049700121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
049800121026     C                   EVAL      VABRMA = SkSplitCSV(i)
049900030721     C                   ENDIF
050000030721     C*
050100030721     C* Reperisco quindi i campi numerici...
050200121026     C*** AAS
050300121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABAAS'
050400121026     C                   EVAL      PiStr=SkSplitCSV(i)
050500121026     C                   EXSR      CHKNUM
050600121026     C                   IF        PiInt=*on
050700121026     C                   Z-ADD     PiVal         VABAAS
050800121026     C                   ELSE
050900121026     C                   SETON                                        31
051000121026     C                   EVAL      vinmsg = %trimr(vinmsg)
051100121026     C                             + ' ' + 'VABAAS'
051200121026     C                   ENDIF
051300121026     C                   ENDIF
051400030721     C*** NSP
051500121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
051600030721     C                   EVAL      PiStr=SkSplitCSV(i)
051700030721     C                   EXSR      CHKNUM
051800040726     C                   IF        PiInt=*on
051900121026     C                   Z-ADD     PiVal         VABNSP
052000030721     C                   ELSE
052100121026     C                   SETON                                        31
052200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
052300121026     C                             + ' ' + 'VABNSP'
052400030721     C                   ENDIF
052500030721     C                   ENDIF
052600030721     C*** CCM
052700121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
052800030721     C                   EVAL      PiStr=SkSplitCSV(i)
052900030721     C                   EXSR      CHKNUM
053000030721     C                   IF        PiInt=*on      AND
053100030721     C                             PiVal<=9999999 AND
053200030721     C                             PiVal>*zeros
053300121026     C                   Z-ADD     PiVal         VABCCM
053400030721     C                   ELSE
053500121026     C                   SETON                                        31
053600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
053700121026     C                             + ' ' + 'VABCCM'
053800030721     C                   ENDIF
053900030721     C                   ENDIF
054000121026     C*** RMN
054100121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
054200121026     C                   EVAL      PiStr=SkSplitCSV(i)
054300121026     C                   EXSR      CHKNUM
054400121026     C                   IF        PiInt=*on
054500121026     C                   Z-ADD     PiVal         VABRMN
054600121026     C                   ELSE
054700121026     C                   SETON                                        31
054800121026     C                   EVAL      vinmsg = %trimr(vinmsg)
054900121026     C                             + ' ' + 'VABNSP'
055000121026     C                   ENDIF
055100121026     C                   ENDIF
055200121026     C*** CAS
055300121026     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
055400121029     C                   EVAL      VABCAS = *zeros
055500121026     C                   IF        SkSplitCSV(i) <> '0000000,00'
055600121026     C                   EVAL      PiStr=SkSplitCSV(i)
055700121026     C                   EXSR      CHKNUM
055800121127     C                   IF        PiNum=*on
055900121026     C                   Z-ADD     PiVal         VABCAS
056000121026     C                   ELSE
056100121026     C                   EVAL      vinmsg = %trimr(vinmsg)
056200121026     C                             + ' ' + 'VABCAS'
056300121026     C                   ENDIF
056400121026     C                   ENDIF
056500121026     C                   ENDIF
056600110222     C*
056700110222     C***  ===>  Gestione campi "particolari" dentro ciclo
056800110222     C*
056900110222     C***  <===  -----------------------------------------
057000030715     C*
057100030715     C                   ENDIF
057200030715     C                   EVAL      i = i + 1
057300030715     C                   ENDDO
057400121026     C*
057500121026     C* Solo se tutto ok => cerco di aggiornare la testata spedizione
057600121026     C                   IF        not *IN31
057700110222     C*
057800110222     C/EXEC SQL
057900121026     C+ update filtragru/fivab00f
058000121026     C+ set   vabcas = :VABCAS, vabtic = :VABTIC,
058100121026     C+       vabrmn = :VABRMN, vabrma = :VABRMA
058200121026     C+ where vablnp = 131     and vabnrs = 05
058300121026     C+   and vabccm = :VABCCM and vabaas = :VABAAS
058400121026     C+   and vabnsp = :VABNSP
058500110222     C/END-EXEC
058600110222     C
058700040726     C                   ENDIF
058800020204     C*
058900030715     C                   ENDIF
059000020204     C*
059100010607     C                   ENDSR
059200010607     C*----------------------------------------------------*
059300040714
059400010330
059500010330
059600010330     C*----------------------------------------------------*
059700010330     C*  CONTROLLO NUMERICITA' CAMPI
059800010330     C*----------------------------------------------------*
059900010330     C     CHKNUM        BEGSR
060000010330     C*
060100010606     C                   IF        PiDecChr = *blanks
060200030715     C                   EVAL      PiDecChr = CharNUM
060300010606     C                   ENDIF
060400010606     C*
060500010606     C                   CALL(e)   'ISNUMERIC'
060600010330     C                   PARM                    PiStr            30
060700010606     C                   PARM                    PiDecChr          1
060800010330     C                   PARM      *ZEROS        PiVal            30 9
060900010330     C                   PARM      '0'           PiInt             1
061000010330     C                   PARM      '0'           PiNum             1
061100010330     C                   IF        %error
061200010606     C                   EVAL      PiNum=*off
061300010330     C                   ENDIF
061400010330     C*
061500010330     C                   ENDSR
061600010330     C***
061700010601
061800010601
061900010601
062000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
062100010601     C     repfil        BEGSR
062200010601     C*
062300010601     C                   if        invfil = *zeros and
062400010601     C                             depfil > *zeros and
062500010629     C                             (vinflg = *blanks or
062600010629     C                              vinflg = *zeros)
062700010601     C
062800010601     C                   eval      invfil = depfil
062900010601     C                   endif
063000010601     C*
063100010601     C                   if        depfil <> invfil and
063200010601     C                             invfil > *zeros
063300010601     C                   eval      flgMulti = '1'
063400010601     C                   if        vinflg = *blanks
063500010601     C                   add       1             cntNonEl
063600010601     C                   endif
063700010601     C                   endif
063800010601     C*
063900010601     C                   if        vinflg = '2'
064000010601     C                   eval      flgStato = '2'
064100010601     C                   endif
064200010601     C*
064300010601     C                   ENDSR
064400010601     C***
064500010330
064600010330
064700010330
064800990920      /TITLE Invio dei dati al punto operativo.
064900000613     C     invio         BEGSR
065000990920     C*
065100990920     C                   reset                   dscmz
065200010601     C                   move      invfil        cmzdst
065300990920     C                   eval      cmzfld = vlrfou
065400990920     C                   eval      cmzmbd = vlrhdl
065500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600000710     C                   if        prmfir = *blanks
065700121114     C                   eval      cmzfla = 'FIVAB00F'
065800121114     C                   eval      cmzmba = 'FIVAB00F'
065900000710     C                   else
066000000710     C                   eval      cmzfla = prmfir
066100000710     C                   eval      cmzmba = prmfir
066200000710     C                   endif
066300990920     C                   eval      cmznrr = *zeros
066400990920     C                   move      §ctrok        cmznrr
066500021018     C                   eval      cmzlba = vlrfl1
066600990920     C                   call(e)   'TIS711C'
066700990920     C                   parm                    dscmz
066800990921     C                   parm      *blanks       esito
066900990920     C                   if        %error
067000990920     C                             or cmzerr = '1'
067100990921     C                             or esito  = '1'
067200000710     C                   eval      wrkesito = '3'
067300990920     C                   endif
067400990920     C*
067500000613     C                   ENDSR
067600010601
067700010601
067800010601
067900010601      /TITLE Invio dei dati al punto operativo.
068000010601     C     opeini        BEGSR
068100010601     C*
068200010601     C* Inizializzo flag e contatori operativi
068300010601     C                   movel     '0'           flgGiro           1
068400010601     C                   movel     '0'           flgMulti          1
068500010601     C                   movel     '1'           flgStato          1
068600010615     C                   movel     '0'           flgOk             1
068700010601     C                   z-add     *zeros        cntNonEl         10 0
068800010601     C                   z-add     *zeros        depfil            3 0
068900010601     C                   z-add     *zeros        invfil            3 0
069000010601     C*
069100010601     C                   ENDSR
069200010601     C***
069300010601
069400050421
069500050421
069600121029     C***  *pssr         BEGSR
069700050421     C*
069800121029     C***                eval      wrkesito = '2'
069900050421     C*
070000121029     C***                ENDSR     '*CANCL'
070100050421     C***
070200010601
070300010330
070400010330
070500000613     C     *inzsr        BEGSR
070600990910     C*
070700990910     C     *entry        plist
070800990920     C                   parm                    tivlrds
070900990921     C                   parm      wrkesito      esito
071000000724     C                   parm                    prmlit
071100000710     C                   parm                    prmfir
071200010330     C*
071300010330     C* CALCOLA LA DATA CORRENTE
071400121026     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
071500121026     C                   eval      datcor = %dec(%date() : *ISO)
071600000613     C*
071700000613     C                   ENDSR
071800000613     C***
071900040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
072000121026VABCCM;VABAAS;VABNSP;VABCAS;VABTIC;VABRMN;VABRMA;
072100121026
072200040726
