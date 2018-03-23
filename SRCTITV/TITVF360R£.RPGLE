000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500130724     FTNEFR00F  O    E             DISK
000600000313     D*
000700040726     D*------------
000800040726     D* SCHIEDE A PROGRAMMA
000900040726     D*------------
001000040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002200030715     D*------------------
002300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002400030715     D*------------------
002500030715     D SkSplitFLD      S             10    DIM(100)
002600030715     D SkSplitCSV      S            256    DIM(100)
002700030715     D CharCSV         S              1
002800030715     D CharTXT         S              1
002900030715     D CharNUM         S              1
003000130724     D posDa           S              4  0 INZ(*zeros)
003100130724     D posA            S              4  0 INZ(*zeros)
003200130724     D i               s              4  0 INZ(1)
003300030715     D wGiro           s              1  0 INZ(*zeros)
003400130729     D Data8A          s              8
003500130729     D wEFRNFT         s                   like(EFRNFT)
003600130730     D wEFRPES1        s                   like(EFRPES)
003700130730     D wEFRPES2        s                   like(EFRPES)
003800130729     D result          s             50
003900130729     D wK              s              3s 0
004000130729     D wX              s              3s 0
004100130729     D wY              s              3s 0
004200030822     D*------------------
004300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004400030822     D*------------------
004500130724     D posDaDft        S              4  0 INZ(*zeros)
004600130724     D posADft         S              4  0 INZ(*zeros)
004700130724     D j               s              4  0 INZ(1)
004800030822     D wGiroDft        s              1  0 INZ(*zeros)
004900110222
005000031201     D*------------------
005100031201     D* Costanti
005200031201     D*------------------
005300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005500130729     D cifre           c                   const('0123456789')
005600010330
005700010330
005800010330
005900990921     C                   reset                   esito
006000990921     C                   reset                   wrkesito
006100010601     C*
006200010601     C                   exsr      opeini
006300130724     C                   exsr      rwfile
006400010601     C*
006500010601     C                   seton                                        lr
006600010601
006700010601
006800010601
006900010601
007000010601     C*--------------------------------------------------------
007100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007200010601     C*--------------------------------------------------------
007300010601     C     PREELA        BEGSR
007400010601     C*
007500130724     C                   if        flgGiro = '0'
007600010601     C*
007700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010601     C                   eval      flgGiro = '1'
007900010601     C*
008000010601     C                   endif
008100010601     C*
008200010601     C                   ENDSR
008300010601     C***
008400010601
008500010601
008600010601
008700010601     C*--------------------------------------------------------
008800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008900010601     C*--------------------------------------------------------
009000010601     C     ENDELA        BEGSR
009100000616     C*
009200010601     C                   ENDSR
009300010601     C***
009400010601
009500130724
009600010601
009700010330     C*--------------------------------------------------------
009800130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
009900010330     C*--------------------------------------------------------
010000130724     C     RWFILE        BEGSR
010100010330     C*
010200010330     C                   if        not %open(tivin00r)
010300010330     C                   open      tivin00r
010400010330     C                   endif
010500010330     C*
010600010604     C                   clear                   §CTROK
010700010604     C                   clear                   §CTRMO
010800010604     C                   clear                   §CTRNO
010900130729     C                   EVAL      wEFRNFT=*blank
011000040726     C*
011100040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011200040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011300130724 xxx C***                exsr      inzvar
011400130724     C***                exsr      impvat
011500010330     C*
011600030822     C                   do        *HIVAL
011700010330     C*
011800030822     C                   read      tivin00r                               70
011900010618     C*
012000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012100010618     C                   if        vinflg = '1'
012200010618     C                   eval      flgOk = '1'
012300010618     C                   endif
012400010618     C*
012500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012600040510     C                   z-add     *zeros        wLenVINDTA        4 0
012700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013100040510     C                   z-add     1             wFlgVINDTA
013200040510     C                   leave
013300040510     C                   endif
013400040510     C                   enddo
013500040510     C*
013600040218     C                   if        vindta > *blanks AND
013700040510     C                             wFlgVINDTA = 1
013800010330     C*
013900010601     C                   if        *in70 = *off and
014000010330     C                             (vinflg = *blanks
014100010330     C                              or vinflg = '0'
014200010330     C                              or vinflg = '2')
014300010330     C*
014400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014500010711     C                   if        vinflg = *blanks or vinflg = '0'
014600010711     C                   clear                   vinmsg
014700010711     C                   endif
014800130729     C*
014900130729     C* siccome sappiamo che la prima colonna sarà la maggior parte delle volte vuota,
015000130729     C* mettiamo un carattere non vuoto per evitare che vengano elise le colonne vuote
015100130729     C                   eval      vindta  = '*' + %trim(vindta)
015200010601     C*
015300010330     C                   exsr      inzvar
015400130724     C                   exsr      impfile
015500010601     C*
015600010601     C                   exsr      PREELA
015700010601     C*
015800010604     C* Ebbene...
015900010604     C*
016000030822     C  N31              add       1             §CTROK            7 0
016100030822     C   32              add       1             §CTRMO            7 0
016200030822     C   31              add       1             §CTRNO            7 0
016300030822     C                   if        wGiro = 2
016400130729     C*
016500130729     C* se il riferimento originale è blank (riga di continuazione), salto la riga
016600130730     C                   IF        EFRRFP <> *blank
016700130724     C  N31              write     TNEFR000
016800130729     C                   ENDIF
016900130729     C*
017000030822     C                   endif
017100010604     C*
017200010604     C                   if        *in31 = *off and
017300010604     C                             *in32 = *off
017400010604     C                   eval      vinflg = '1'
017500010604     C                   else
017600010604     C                   eval      vinflg = '2'
017700010604     C                   endif
017800130724     C*
017900130724     C                   endif
018000010604     C*
018100010330     C                   else
018200010330     C                   eval      vinflg = '1'
018300010330     C                   endif
018400010601     C*
018500010601     C  N70              update    tivin000
018600010330     C*
018700030822     C  N70              enddo
018800010601     C
018900010601     C*
019000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019100130724     C                   if        cntNonEl = *zeros
019200010330     C* Se non ci sono record con errori ...
019300010601     C                   if        §ctrno = 0 and
019400130724     C                             §ctrmo = 0
019500010330     C* ... restituisco esito OK.
019600010330     C                   eval      wrkesito = '0'
019700010330     C                   else
019800010330     C                   if        §ctrok > 0
019900010330     C                   eval      wrkesito = '1'
020000010330     C                   else
020100010615     C                   if        flgOk = '0'
020200010615     C                   eval      wrkesito = '2'
020300010615     C                   else
020400010615     C                   eval      wrkesito = '6'
020500010615     C                   endif
020600010330     C                   endif
020700010330     C                   endif
020800010601     C                   else
020900010601     C                   eval      wrkesito = '9'
021000010601     C                   endif
021100010330     C*
021200010330     C                   if        %open(tivin00r)
021300010330     C                   close     tivin00r
021400010330     C                   endif
021500010601     C*
021600010618     C                   if        flgGiro = '1'
021700010601     C                   exsr      endela
021800010618     C                   endif
021900010330     C*
022000010330     C                   ENDSR
022100010330     C***
022200010601
022300010601
022400010601
022500010330     C*----------------------------------------------------*
022600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022700010330     C*----------------------------------------------------*
022800010330     C     INZVAR        BEGSR
022900010330     C*
023000030822     C* Inizializzo variabili di wrk
023100130724     C
023200030822     C*
023300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023400130724     C                   CLEAR                   TNEFR000
023500030822     C                   CLEAR                   SkSplitCSV
023600030822     C                   EVAL      i = 1
023700030822     C                   EVAL      posDa = *zeros
023800030822     C                   EVAL      posA  = *zeros
023900130730     C                   EVAL      wEFRPES1=0
024000130730     C                   EVAL      wEFRPES2=0
024100030822     C*
024200030822     C* Reimposto i valori di default
024300030822     C                   EXSR      DEFCAM
024400030822     C*
024500010330     C                   ENDSR
024600130724
024700130724
024800130724
024900010330     C*----------------------------------------------------*
025000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025100010330     C*----------------------------------------------------*
025200010330     C     DEFCAM        BEGSR
025300030822     C*
025400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025500030715     C* e delimitatore testo.
025600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
025700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
025800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
025900030822     C*
026000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026100130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
026200030715     C                   ENDIF
026300030822     C*
026400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026500030822     C                   EVAL      posDaDft = 1
026600030822     C                   EVAL      posADft  = 0
026700030822     C                   EVAL      wGiroDft = 0
026800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
026900030822     C                             posDaDft > 0
027000030822     C*
027100030822     C* Gestisco il 1° giro
027200030822     C                   IF        wGiroDft = 0
027300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
027400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027500030822     C* Incremento il contatore dei "giri"
027600030822     C                   EVAL      wGiroDft = 1
027700030822     C                   ELSE
027800030822     C                   EVAL      posDaDft = posADft
027900030822     C                   ENDIF
028000030822     C* Eseguo lo scan x trovare la fine del campo corrente
028100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028200030822     C*
028300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
028400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028500030822     C                   IF        posDaDft > 0 AND
028600030822     C                             posADft  > 0
028700130724     C* NTW
028800130724     C                   IF        %subst(
028900130724     C                             %subst(vlrppt:posDaDft+1:
029000130724     C                             posADft-posDaDft-1):1:3)
029100130724     C                             = 'NTW'
029200130724     C                   EVAL      EFRNTW=%trim(%subst(
029300130724     C                             %subst(vlrppt:posDaDft+1:
029400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029500130724     C                   ENDIF
029600130724     C* TPD
029700130724     C                   IF        %subst(
029800130724     C                             %subst(vlrppt:posDaDft+1:
029900130724     C                             posADft-posDaDft-1):1:3)
030000130724     C                             = 'TPD'
030100130724     C                   EVAL      EFRTPD=%trim(%subst(
030200130724     C                             %subst(vlrppt:posDaDft+1:
030300130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030400130724     C                   ENDIF
030500130724     C* CAU
030600130724     C                   IF        %subst(
030700130724     C                             %subst(vlrppt:posDaDft+1:
030800130724     C                             posADft-posDaDft-1):1:3)
030900130724     C                             = 'CAU'
031000130724     C                   EVAL      EFRCAU=%trim(%subst(
031100130724     C                             %subst(vlrppt:posDaDft+1:
031200130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031300130724     C                   ENDIF
031400130724     C* GRP
031500130724     C                   IF        %subst(
031600130724     C                             %subst(vlrppt:posDaDft+1:
031700130724     C                             posADft-posDaDft-1):1:3)
031800130724     C                             = 'GRP'
031900130724     C                   EVAL      EFRGRP=%trim(%subst(
032000130724     C                             %subst(vlrppt:posDaDft+1:
032100130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032200130724     C                   ENDIF
032300030822     C* ...
032400030822     C                   ENDIF
032500030822     C                   ENDDO
032600020204     C*
032700010330     C                   ENDSR
032800130724
032900130724
033000130724
033100010607     C*----------------------------------------------------*
033200130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
033300010607     C*----------------------------------------------------*
033400130724     C     IMPFILE       BEGSR
033500010607     C*
033600130724     C                   SETOFF                                       3132
033700130724     C*
033800130724     C                   MOVEL     datcor        EFRDTR
033900010607     C*
034000030715     C**********
034100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
034200030715     C**********
034300030715     C                   IF        wGiro = *zeros
034400040726     C*
034500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
034600040726     C                   EVAL      depvinDTA = vinDTA
034700040726     C*
034800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
034900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
035000040726     C                                      %trim(intcol(3))
035100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
035200031201     C     minu:maiu     XLATE     vindta        vindta
035300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
035400040324     C                   DOW       posDa <= %len(%trimr(vindta))
035500030715     C*
035600030715     C* Gestisco il 1° campo
035700030715     C                   IF        i = 1
035800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
035900030715     C                   EVAL      posDa = 1
036000030715     C* Eseguo lo scan x trovare la fine del primo campo
036100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
036200030715     C* A questo punto "estraggo" il campo corrente
036300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
036400030715     C                                                    (posA-posDa))
036500030715     C* X i campi successivi al 1°
036600030715     C                   ELSE
036700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
036800030715     C                   EVAL      posDa = posA + 1
036900030715     C* Eseguo lo scan x trovare la fine del campo corrente
037000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037100030715     C* Gestisco l'ultimo campo
037200030715     C                   IF        posA = *zeros
037300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
037400030715     C* A questo punto "estraggo" il campo corrente
037500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037600030715     C                                                    (posA-posDa))
037700030715     C                   LEAVE
037800030715     C                   ELSE
037900030715     C* A questo punto "estraggo" il campo corrente
038000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038100030715     C                                                    (posA-posDa))
038200030715     C                   ENDIF
038300030715     C                   ENDIF
038400030715     C* Incremento il contatore d campo
038500030715     C                   EVAL      i = i +1
038600030715     C                   ENDDO
038700030715     C*
038800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
038900030715     C* delimitazione testo
039000030715     C                   EVAL      i = 1
039100030715     C                   DOW       i < %elem(SkSplitFLD)
039200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
039300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
039400030715     C                   EVAL      i = i + 1
039500030715     C                   ENDDO
039600030715     C                   EVAL      wGiro = 1
039700040726     C* Ripristino la 1° riga originale
039800040726     C                   EVAL      vinDTA = depvinDTA
039900030715     C                   ELSE
040000030715     C                   EVAL      wGiro = 2
040100031204     C**********
040200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
040300031204     C* d separatore campo
040400031204     C**********
040500040726     C                   EVAL      vindta = %trim(vindta)
040600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
040700040726     C                   EVAL      vindta = %subst(vindta:2)
040800040726     C                   ENDDO
040900031204     C*
041000031204     C                   Z-ADD     *zeros        lunghInput        4 0
041100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
041200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
041300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
041400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
041500040726     C                   ENDDO
041600030715     C**********
041700030715     C* Effettuo lo split del campo dati d input
041800030715     C**********
041900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
042000040324     C                   DOW       posDa <= %len(%trimr(vindta))
042100030715     C*
042200030715     C* Gestisco il 1° campo
042300030715     C                   IF        i = 1
042400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
042500030715     C                   EVAL      posDa = 1
042600030715     C* Eseguo lo scan x trovare la fine del primo campo
042700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042800030715     C* A questo punto "estraggo" il campo corrente
042900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043000030715     C                                                    (posA-posDa))
043100030715     C* X i campi successivi al 1°
043200030715     C                   ELSE
043300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
043400030715     C                   EVAL      posDa = posA + 1
043500031204     C* Verifico che nn vi sia il campo nn valorizzato
043600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
043700031204     C* Se campo nn valorizzato skippo al prossimo
043800031204     C                   EVAL      SkSplitCSV(i) = *blanks
043900031204     C                   EVAL      posA  = posA + 1
044000031204     C                   ELSE
044100030715     C* Eseguo lo scan x trovare la fine del campo corrente
044200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044300030715     C* Gestisco l'ultimo campo
044400030715     C                   IF        posA = *zeros
044500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
044600030715     C* A questo punto "estraggo" il campo corrente
044700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
044800030715     C                                                    (posA-posDa))
044900030715     C                   LEAVE
045000030715     C                   ELSE
045100030715     C* A questo punto "estraggo" il campo corrente
045200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045300030715     C                                                    (posA-posDa))
045400030715     C                   ENDIF
045500031204     C                   ENDIF
045600030715     C                   ENDIF
045700030715     C* Incremento il contatore d campo
045800030715     C                   EVAL      i = i +1
045900030715     C                   ENDDO
046000030715     C*
046100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
046200030715     C* delimitazione testo
046300030715     C                   EVAL      i = 1
046400030715     C                   DOW       i < %elem(SkSplitCSV)
046500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
046600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
046700030715     C                   EVAL      i = i + 1
046800030715     C                   ENDDO
046900030715     C*
047000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
047100030715     C                   EVAL      i = 1
047200030715     C                   DOW       i < %elem(SkSplitFLD)
047300030715     C*
047400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
047500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
047600030715     C                             SkSplitCSV(i) <> *blanks
047700130724     C*** RIF
047800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
047900130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
048000130724     C                   ENDIF
048100130724     C*** RFP
048200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
048300130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
048400130724     C                   ENDIF
048500130724     C*** NFT
048600130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
048700130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
048800130724     C                   ENDIF
048900130724     C*
049000130724     C* Reperisco quindi i campi numerici...
049100130724     C*** DFT
049200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
049300130724     C                   EVAL      PiStr=SkSplitCSV(i)
049400130724     C                   EXSR      CHKNUM
049500130724     C                   IF        PiInt=*on
049600130724     C                   Z-ADD     PiVal         EFRDFT
049700130724     C                   ELSE
049800130724     C                   SETON                                        31
049900130724     C                   EVAL      EFRDFT = *zeros
050000130724     C                   EVAL      vinmsg = %trimr(vinmsg)
050100130724     C                             + ' ' + 'EFRDFT'
050200130724     C                   ENDIF
050300130724     C                   ENDIF
050400130724     C*** IMP
050500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
050600130724     C                   EVAL      PiStr=SkSplitCSV(i)
050700130724     C                   EXSR      CHKNUM
050800130724     C                   IF        PiNum=*on
050900130724     C                   Z-ADD     PiVal         EFRIMP
051000130724     C                   ELSE
051100130724     C                   SETON                                        31
051200130724     C                   EVAL      EFRIMP = *zeros
051300130724     C                   EVAL      vinmsg = %trimr(vinmsg)
051400130724     C                             + ' ' + 'EFRIMP'
051500130724     C                   ENDIF
051600130724     C                   ENDIF
051700130724     C*** PES
051800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
051900130724     C                   EVAL      PiStr=SkSplitCSV(i)
052000130724     C                   EXSR      CHKNUM
052100130724     C                   IF        PiNum=*on
052200130724     C                   Z-ADD     PiVal         EFRPES
052300130724     C                   ELSE
052400130724     C                   SETON                                        31
052500130724     C                   EVAL      EFRPES = *zeros
052600130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052700130724     C                             + ' ' + 'EFRPES'
052800130724     C                   ENDIF
052900130724     C                   ENDIF
053000130724     C*** VOL
053100130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
053200130724     C                   EVAL      PiStr=SkSplitCSV(i)
053300130724     C                   EXSR      CHKNUM
053400130724     C                   IF        PiNum=*on
053500130724     C                   Z-ADD     PiVal         EFRVOL
053600130724     C                   ELSE
053700130724     C                   SETON                                        31
053800130724     C                   EVAL      EFRVOL = *zeros
053900130724     C                   EVAL      vinmsg = %trimr(vinmsg)
054000130724     C                             + ' ' + 'EFRVOL'
054100130724     C                   ENDIF
054200130724     C                   ENDIF
054300110222     C*
054400130729     C***  ===>  Gestione campi "particolari" dentro ciclo
054500130729
054600110222     C*
054700130729      *** W_NFT: c'è solo nella prima riga della fattura
054800130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_NFT'
054900130730     C* siccome metto * nel 1° carattere che fa parte del nr.fattura, non posso testare blank
055000130730     C                             and SkSplitCSV(i)<>'*'
055100130730     C                   EVAL      wEFRNFT=%trim(%subst(SkSplitCSV(i) : 2 : 15))
055200130729     C                   ENDIF
055300130729
055400130729     C*
055500130729      *** W_RIF: normalizzare eliminando tutti i caratteri non numerici, quindi compattare il dato e
055600130729     C*          tenere solamente i primi 14 bytes (occhio che ci sono dei casi in cui il dato
055700130729     C*          rimanente è lungo meno di 14 bytes)
055800130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF'
055900130729     C                   EVAL      wX = 1
056000130729     C                   EVAL      result = %trim(SkSplitCSV(i))
056100130729       // salvo il risultato perché la variabile result non è fissa e non mi fido
056200130729     C                   EVAL      wY = %checkR(' ' : result)
056300130729     C* trovo l'ultimo carattere non blank
056400130729     C                   FOR       wK = 1 TO wY
056500130729     C                   EVAL      wX = %check(cifre : result  : wX)
056600130729     C                   if        wX > 0
056700130729     C                   EVAL      result = %replace( '' : result : wX : 1)
056800130729     C                   ELSE
056900130729     C                   LEAVE
057000130729     C                   ENDIF
057100130729     C                   ENDFOR
057200130801     C* se il risultato del compattamento è blank, do un warning
057300130801     C                   IF        result = *blank
057400130801     C                   SETON                                        32
057500130801     C                   EVAL      EFRRIF = *blank
057600130801     C                   EVAL      vinmsg = %trimr(vinmsg)
057700130801     C                             + ' ' + 'EFRRIF'
057800130801     C                   ELSE
057900130729     C                   EVAL      EFRRIF = %subst(result : 1 : 14)
058000130801     C                   ENDIF
058100130729     C                   ENDIF
058200130730
058300130730     C*** W_PES1 e 2
058400130730     C                   IF        %trim(SkSplitFLD(i)) = 'W_PES1'
058500130730     C                   EVAL      PiStr=SkSplitCSV(i)
058600130730     C                   EXSR      CHKNUM
058700130730     C                   IF        PiNum=*on
058800130730     C                   Z-ADD     PiVal         wEFRPES1
058900130730     C                   ELSE
059000130730     C                   EVAL      wEFRPES1 = *zeros
059100130730     C                   ENDIF
059200130730     C                   ENDIF
059300130730     C                   IF        %trim(SkSplitFLD(i)) = 'W_PES2'
059400130730     C                   EVAL      PiStr=SkSplitCSV(i)
059500130730     C                   EXSR      CHKNUM
059600130730     C                   IF        PiNum=*on
059700130730     C                   Z-ADD     PiVal         wEFRPES2
059800130730     C                   ELSE
059900130730     C                   EVAL      wEFRPES2 = *zeros
060000130730     C                   ENDIF
060100130730     C                   ENDIF
060200130729
060300130729     C*
060400130729      *** W_RFP: escludere i primi 3 caratteri fissi 'SAL'
060500130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_RFP'
060600130729     C                   EVAL      EFRRFP = %subst(SkSplitCSV(i) : 4 : 35)
060700130729     C                   ENDIF
060800130729
060900110222     C***  <===  -----------------------------------------
061000030715     C*
061100030715     C                   ENDIF
061200030715     C                   EVAL      i = i + 1
061300030715     C                   ENDDO
061400130724     C*
061500130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
061600130729
061700130729     C                   EVAL      EFRNFT=wEFRNFT
061800130730
061900130730     C                   EVAL      EFRPES=wEFRPES1
062000130730     C                   IF        EFRPES = 0
062100130730     C                   EVAL      EFRPES=wEFRPES2
062200130730     C                   ENDIF
062300130724     C***  <===  -----------------------------------------
062400020204     C*
062500030715     C                   ENDIF
062600020204     C*
062700010607     C                   ENDSR
062800010607     C*----------------------------------------------------*
062900040714
063000010330
063100010330
063200010330     C*----------------------------------------------------*
063300010330     C*  CONTROLLO NUMERICITA' CAMPI
063400010330     C*----------------------------------------------------*
063500010330     C     CHKNUM        BEGSR
063600010330     C*
063700010606     C                   IF        PiDecChr = *blanks
063800030715     C                   EVAL      PiDecChr = CharNUM
063900010606     C                   ENDIF
064000010606     C*
064100010606     C                   CALL(e)   'ISNUMERIC'
064200010330     C                   PARM                    PiStr            30
064300010606     C                   PARM                    PiDecChr          1
064400010330     C                   PARM      *ZEROS        PiVal            30 9
064500010330     C                   PARM      '0'           PiInt             1
064600010330     C                   PARM      '0'           PiNum             1
064700010330     C                   IF        %error
064800010606     C                   EVAL      PiNum=*off
064900010330     C                   ENDIF
065000010330     C*
065100010330     C                   ENDSR
065200010330     C***
065300010601
065400010601
065500010601
065600010601      /TITLE Invio dei dati al punto operativo.
065700010601     C     opeini        BEGSR
065800010601     C*
065900010601     C* Inizializzo flag e contatori operativi
066000010601     C                   movel     '0'           flgGiro           1
066100010615     C                   movel     '0'           flgOk             1
066200010601     C                   z-add     *zeros        cntNonEl         10 0
066300010601     C*
066400010601     C                   ENDSR
066500010601     C***
066600010601
066700050421
066800050421
066900050421     C     *pssr         BEGSR
067000050421     C*
067100050421     C                   eval      wrkesito = '2'
067200050421     C*
067300050421     C                   ENDSR     '*CANCL'
067400050421     C***
067500010601
067600010330
067700010330
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500010330     C*
068600010330     C* CALCOLA LA DATA CORRENTE
068700130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068800130724     C                   eval      datcor = %dec(%date() : *ISO)
068900000613     C*
069000000613     C                   ENDSR
069100000613     C***
069200040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
069300130730W_NFT;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;W_RFP;W_RIF;X;X;X;W_PES2;EFRVOL;
069400130730X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;EFRIMP;X;X;X;X;X;X;X;X;W_PES1;X;X;X;
069500130528
