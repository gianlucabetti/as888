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
003500030822     D*------------------
003600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003700030822     D*------------------
003800130724     D posDaDft        S              4  0 INZ(*zeros)
003900130724     D posADft         S              4  0 INZ(*zeros)
004000130724     D j               s              4  0 INZ(1)
004100030822     D wGiroDft        s              1  0 INZ(*zeros)
004200110222
004300031201     D*------------------
004400031201     D* Costanti
004500031201     D*------------------
004600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004800010330
004900010330
005000010330
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300010601     C*
005400010601     C                   exsr      opeini
005500130724     C                   exsr      rwfile
005600010601     C*
005700010601     C                   seton                                        lr
005800010601
005900010601
006000010601
006100010601
006200010601     C*--------------------------------------------------------
006300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006400010601     C*--------------------------------------------------------
006500010601     C     PREELA        BEGSR
006600010601     C*
006700130724     C                   if        flgGiro = '0'
006800010601     C*
006900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007000010601     C                   eval      flgGiro = '1'
007100010601     C*
007200010601     C                   endif
007300010601     C*
007400010601     C                   ENDSR
007500010601     C***
007600010601
007700010601
007800010601
007900010601     C*--------------------------------------------------------
008000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008100010601     C*--------------------------------------------------------
008200010601     C     ENDELA        BEGSR
008300000616     C*
008400010601     C                   ENDSR
008500010601     C***
008600010601
008700130724
008800010601
008900010330     C*--------------------------------------------------------
009000130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
009100010330     C*--------------------------------------------------------
009200130724     C     RWFILE        BEGSR
009300010330     C*
009400010330     C                   if        not %open(tivin00r)
009500010330     C                   open      tivin00r
009600010330     C                   endif
009700010330     C*
009800010604     C                   clear                   §CTROK
009900010604     C                   clear                   §CTRMO
010000010604     C                   clear                   §CTRNO
010100040726     C*
010200040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
010300040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
010400130724 xxx C***                exsr      inzvar
010500130724     C***                exsr      impvat
010600010330     C*
010700030822     C                   do        *HIVAL
010800010330     C*
010900030822     C                   read      tivin00r                               70
011000010618     C*
011100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011200010618     C                   if        vinflg = '1'
011300010618     C                   eval      flgOk = '1'
011400010618     C                   endif
011500010618     C*
011600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
011700040510     C                   z-add     *zeros        wLenVINDTA        4 0
011800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
011900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
012100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
012200040510     C                   z-add     1             wFlgVINDTA
012300040510     C                   leave
012400040510     C                   endif
012500040510     C                   enddo
012600040510     C*
012700040218     C                   if        vindta > *blanks AND
012800040510     C                             wFlgVINDTA = 1
012900010330     C*
013000010601     C                   if        *in70 = *off and
013100010330     C                             (vinflg = *blanks
013200010330     C                              or vinflg = '0'
013300010330     C                              or vinflg = '2')
013400010330     C*
013500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
013600010711     C                   if        vinflg = *blanks or vinflg = '0'
013700010711     C                   clear                   vinmsg
013800010711     C                   endif
013900010601     C*
014000010330     C                   exsr      inzvar
014100130724     C                   exsr      impfile
014200010601     C*
014300010601     C                   exsr      PREELA
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700030822     C  N31              add       1             §CTROK            7 0
014800030822     C   32              add       1             §CTRMO            7 0
014900030822     C   31              add       1             §CTRNO            7 0
015000030822     C                   if        wGiro = 2
015100130724     C  N31              write     TNEFR000
015200030822     C                   endif
015300010604     C*
015400010604     C                   if        *in31 = *off and
015500010604     C                             *in32 = *off
015600010604     C                   eval      vinflg = '1'
015700010604     C                   else
015800010604     C                   eval      vinflg = '2'
015900010604     C                   endif
016000130724     C*
016100130724     C                   endif
016200010604     C*
016300010330     C                   else
016400010330     C                   eval      vinflg = '1'
016500010330     C                   endif
016600010601     C*
016700010601     C  N70              update    tivin000
016800010330     C*
016900030822     C  N70              enddo
017000010601     C
017100010601     C*
017200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017300130724     C                   if        cntNonEl = *zeros
017400010330     C* Se non ci sono record con errori ...
017500010601     C                   if        §ctrno = 0 and
017600130724     C                             §ctrmo = 0
017700010330     C* ... restituisco esito OK.
017800010330     C                   eval      wrkesito = '0'
017900010330     C                   else
018000010330     C                   if        §ctrok > 0
018100010330     C                   eval      wrkesito = '1'
018200010330     C                   else
018300010615     C                   if        flgOk = '0'
018400010615     C                   eval      wrkesito = '2'
018500010615     C                   else
018600010615     C                   eval      wrkesito = '6'
018700010615     C                   endif
018800010330     C                   endif
018900010330     C                   endif
019000010601     C                   else
019100010601     C                   eval      wrkesito = '9'
019200010601     C                   endif
019300010330     C*
019400010330     C                   if        %open(tivin00r)
019500010330     C                   close     tivin00r
019600010330     C                   endif
019700010601     C*
019800010618     C                   if        flgGiro = '1'
019900010601     C                   exsr      endela
020000010618     C                   endif
020100010330     C*
020200010330     C                   ENDSR
020300010330     C***
020400010601
020500010601
020600010601
020700010330     C*----------------------------------------------------*
020800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020900010330     C*----------------------------------------------------*
021000010330     C     INZVAR        BEGSR
021100010330     C*
021200030822     C* Inizializzo variabili di wrk
021300130724     C
021400030822     C*
021500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
021600130724     C                   CLEAR                   TNEFR000
021700030822     C                   CLEAR                   SkSplitCSV
021800030822     C                   EVAL      i = 1
021900030822     C                   EVAL      posDa = *zeros
022000030822     C                   EVAL      posA  = *zeros
022100030822     C*
022200030822     C* Reimposto i valori di default
022300030822     C                   EXSR      DEFCAM
022400030822     C*
022500010330     C                   ENDSR
022600130724
022700130724
022800130724
022900010330     C*----------------------------------------------------*
023000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
023100010330     C*----------------------------------------------------*
023200010330     C     DEFCAM        BEGSR
023300030822     C*
023400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
023500030715     C* e delimitatore testo.
023600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
023700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
023800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
023900030822     C*
024000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
024100130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
024200030715     C                   ENDIF
024300030822     C*
024400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
024500030822     C                   EVAL      posDaDft = 1
024600030822     C                   EVAL      posADft  = 0
024700030822     C                   EVAL      wGiroDft = 0
024800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
024900030822     C                             posDaDft > 0
025000030822     C*
025100030822     C* Gestisco il 1° giro
025200030822     C                   IF        wGiroDft = 0
025300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
025400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
025500030822     C* Incremento il contatore dei "giri"
025600030822     C                   EVAL      wGiroDft = 1
025700030822     C                   ELSE
025800030822     C                   EVAL      posDaDft = posADft
025900030822     C                   ENDIF
026000030822     C* Eseguo lo scan x trovare la fine del campo corrente
026100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
026200030822     C*
026300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
026400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
026500030822     C                   IF        posDaDft > 0 AND
026600030822     C                             posADft  > 0
026700130724     C* NTW
026800130724     C                   IF        %subst(
026900130724     C                             %subst(vlrppt:posDaDft+1:
027000130724     C                             posADft-posDaDft-1):1:3)
027100130724     C                             = 'NTW'
027200130724     C                   EVAL      EFRNTW=%trim(%subst(
027300130724     C                             %subst(vlrppt:posDaDft+1:
027400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
027500130724     C                   ENDIF
027600130724     C* TPD
027700130724     C                   IF        %subst(
027800130724     C                             %subst(vlrppt:posDaDft+1:
027900130724     C                             posADft-posDaDft-1):1:3)
028000130724     C                             = 'TPD'
028100130724     C                   EVAL      EFRTPD=%trim(%subst(
028200130724     C                             %subst(vlrppt:posDaDft+1:
028300130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
028400130724     C                   ENDIF
028500130724     C* CAU
028600130724     C                   IF        %subst(
028700130724     C                             %subst(vlrppt:posDaDft+1:
028800130724     C                             posADft-posDaDft-1):1:3)
028900130724     C                             = 'CAU'
029000130724     C                   EVAL      EFRCAU=%trim(%subst(
029100130724     C                             %subst(vlrppt:posDaDft+1:
029200130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029300130724     C                   ENDIF
029400130724     C* GRP
029500130724     C                   IF        %subst(
029600130724     C                             %subst(vlrppt:posDaDft+1:
029700130724     C                             posADft-posDaDft-1):1:3)
029800130724     C                             = 'GRP'
029900130724     C                   EVAL      EFRGRP=%trim(%subst(
030000130724     C                             %subst(vlrppt:posDaDft+1:
030100130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030200130724     C                   ENDIF
030300030822     C* ...
030400030822     C                   ENDIF
030500030822     C                   ENDDO
030600020204     C*
030700010330     C                   ENDSR
030800130724
030900130724
031000130724
031100010607     C*----------------------------------------------------*
031200130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
031300010607     C*----------------------------------------------------*
031400130724     C     IMPFILE       BEGSR
031500010607     C*
031600130724     C                   SETOFF                                       3132
031700130724     C*
031800130724     C                   MOVEL     datcor        EFRDTR
031900010607     C*
032000030715     C**********
032100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
032200030715     C**********
032300030715     C                   IF        wGiro = *zeros
032400040726     C*
032500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
032600040726     C                   EVAL      depvinDTA = vinDTA
032700040726     C*
032800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
032900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
033000040726     C                                      %trim(intcol(3))
033100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
033200031201     C     minu:maiu     XLATE     vindta        vindta
033300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
033400040324     C                   DOW       posDa <= %len(%trimr(vindta))
033500030715     C*
033600030715     C* Gestisco il 1° campo
033700030715     C                   IF        i = 1
033800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
033900030715     C                   EVAL      posDa = 1
034000030715     C* Eseguo lo scan x trovare la fine del primo campo
034100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
034200030715     C* A questo punto "estraggo" il campo corrente
034300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
034400030715     C                                                    (posA-posDa))
034500030715     C* X i campi successivi al 1°
034600030715     C                   ELSE
034700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
034800030715     C                   EVAL      posDa = posA + 1
034900030715     C* Eseguo lo scan x trovare la fine del campo corrente
035000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
035100030715     C* Gestisco l'ultimo campo
035200030715     C                   IF        posA = *zeros
035300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
035400030715     C* A questo punto "estraggo" il campo corrente
035500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
035600030715     C                                                    (posA-posDa))
035700030715     C                   LEAVE
035800030715     C                   ELSE
035900030715     C* A questo punto "estraggo" il campo corrente
036000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
036100030715     C                                                    (posA-posDa))
036200030715     C                   ENDIF
036300030715     C                   ENDIF
036400030715     C* Incremento il contatore d campo
036500030715     C                   EVAL      i = i +1
036600030715     C                   ENDDO
036700030715     C*
036800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
036900030715     C* delimitazione testo
037000030715     C                   EVAL      i = 1
037100030715     C                   DOW       i < %elem(SkSplitFLD)
037200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
037300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
037400030715     C                   EVAL      i = i + 1
037500030715     C                   ENDDO
037600030715     C                   EVAL      wGiro = 1
037700040726     C* Ripristino la 1° riga originale
037800040726     C                   EVAL      vinDTA = depvinDTA
037900030715     C                   ELSE
038000030715     C                   EVAL      wGiro = 2
038100031204     C**********
038200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
038300031204     C* d separatore campo
038400031204     C**********
038500040726     C                   EVAL      vindta = %trim(vindta)
038600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
038700040726     C                   EVAL      vindta = %subst(vindta:2)
038800040726     C                   ENDDO
038900031204     C*
039000031204     C                   Z-ADD     *zeros        lunghInput        4 0
039100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
039200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
039300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
039400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
039500040726     C                   ENDDO
039600030715     C**********
039700030715     C* Effettuo lo split del campo dati d input
039800030715     C**********
039900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
040000040324     C                   DOW       posDa <= %len(%trimr(vindta))
040100030715     C*
040200030715     C* Gestisco il 1° campo
040300030715     C                   IF        i = 1
040400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
040500030715     C                   EVAL      posDa = 1
040600030715     C* Eseguo lo scan x trovare la fine del primo campo
040700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040800030715     C* A questo punto "estraggo" il campo corrente
040900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
041000030715     C                                                    (posA-posDa))
041100030715     C* X i campi successivi al 1°
041200030715     C                   ELSE
041300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
041400030715     C                   EVAL      posDa = posA + 1
041500031204     C* Verifico che nn vi sia il campo nn valorizzato
041600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
041700031204     C* Se campo nn valorizzato skippo al prossimo
041800031204     C                   EVAL      SkSplitCSV(i) = *blanks
041900031204     C                   EVAL      posA  = posA + 1
042000031204     C                   ELSE
042100030715     C* Eseguo lo scan x trovare la fine del campo corrente
042200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042300030715     C* Gestisco l'ultimo campo
042400030715     C                   IF        posA = *zeros
042500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
042600030715     C* A questo punto "estraggo" il campo corrente
042700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
042800030715     C                                                    (posA-posDa))
042900030715     C                   LEAVE
043000030715     C                   ELSE
043100030715     C* A questo punto "estraggo" il campo corrente
043200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043300030715     C                                                    (posA-posDa))
043400030715     C                   ENDIF
043500031204     C                   ENDIF
043600030715     C                   ENDIF
043700030715     C* Incremento il contatore d campo
043800030715     C                   EVAL      i = i +1
043900030715     C                   ENDDO
044000030715     C*
044100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044200030715     C* delimitazione testo
044300030715     C                   EVAL      i = 1
044400030715     C                   DOW       i < %elem(SkSplitCSV)
044500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
044600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
044700030715     C                   EVAL      i = i + 1
044800030715     C                   ENDDO
044900030715     C*
045000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
045100030715     C                   EVAL      i = 1
045200030715     C                   DOW       i < %elem(SkSplitFLD)
045300030715     C*
045400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
045500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
045600030715     C                             SkSplitCSV(i) <> *blanks
045700130724     C*** RIF
045800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
045900130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
046000130724     C                   ENDIF
046100130724     C*** RFP
046200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
046300130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
046400130724     C                   ENDIF
046500130724     C*** NFT
046600130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
046700130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
046800130724     C                   ENDIF
046900130724     C*
047000130724     C* Reperisco quindi i campi numerici...
047100130724     C*** DFT
047200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
047300130724     C                   EVAL      PiStr=SkSplitCSV(i)
047400130724     C                   EXSR      CHKNUM
047500130724     C                   IF        PiInt=*on
047600130724     C                   Z-ADD     PiVal         EFRDFT
047700130724     C                   ELSE
047800130724     C                   SETON                                        31
047900130724     C                   EVAL      EFRDFT = *zeros
048000130724     C                   EVAL      vinmsg = %trimr(vinmsg)
048100130724     C                             + ' ' + 'EFRDFT'
048200130724     C                   ENDIF
048300130724     C                   ENDIF
048400130724     C*** IMP
048500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
048600130724     C                   EVAL      PiStr=SkSplitCSV(i)
048700130724     C                   EXSR      CHKNUM
048800130724     C                   IF        PiNum=*on
048900130724     C                   Z-ADD     PiVal         EFRIMP
049000130724     C                   ELSE
049100130724     C                   SETON                                        31
049200130724     C                   EVAL      EFRIMP = *zeros
049300130724     C                   EVAL      vinmsg = %trimr(vinmsg)
049400130724     C                             + ' ' + 'EFRIMP'
049500130724     C                   ENDIF
049600130724     C                   ENDIF
049700130724     C*** PES
049800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
049900130724     C                   EVAL      PiStr=SkSplitCSV(i)
050000130724     C                   EXSR      CHKNUM
050100130724     C                   IF        PiNum=*on
050200130724     C                   Z-ADD     PiVal         EFRPES
050300130724     C                   ELSE
050400130724     C                   SETON                                        31
050500130724     C                   EVAL      EFRPES = *zeros
050600130724     C                   EVAL      vinmsg = %trimr(vinmsg)
050700130724     C                             + ' ' + 'EFRPES'
050800130724     C                   ENDIF
050900130724     C                   ENDIF
051000130724     C*** VOL
051100130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
051200130724     C                   EVAL      PiStr=SkSplitCSV(i)
051300130724     C                   EXSR      CHKNUM
051400130724     C                   IF        PiNum=*on
051500130724     C                   Z-ADD     PiVal         EFRVOL
051600130724     C                   ELSE
051700130724     C                   SETON                                        31
051800130724     C                   EVAL      EFRVOL = *zeros
051900130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052000130724     C                             + ' ' + 'EFRVOL'
052100130724     C                   ENDIF
052200130724     C                   ENDIF
052300110222     C*
052400110222     C***  ===>  Gestione campi "particolari" dentro ciclo
052500110222     C*
052600130729      *** W_DFT: la data è in formato gg/mm/aaaa alfanum. e va trasformata in 8 S
052700130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
052800130729     C* il partner può mandare la data con l'anno di 2 o di 4, bisogna gestirla
052900130729     C                   IF        %len(%trim(SkSplitCSV(i)))=10
053000130729     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
053100130729     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
053200130729     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
053300130729     C                   MOVEL     Data8A        EFRDFT
053400130729     C                   ELSE
053500130729     C* se la data non fosse né di 6 né di 8, valorizzo EFTDFT come se mi arrivasse di 6
053600130729     C* così se il valore è impossibile va in errore e ce ne accorgiamo
053700130729     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
053800130729     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
053900130729     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
054000130729     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
054100130729     C                   MOVEL     Data8A        EFRDFT
054200130729     C                   ENDIF
054300130729     C                   ENDIF
054400110222     C***  <===  -----------------------------------------
054500030715     C*
054600030715     C                   ENDIF
054700030715     C                   EVAL      i = i + 1
054800030715     C                   ENDDO
054900130724     C*
055000130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
055100130724     C
055200130724     C***  <===  -----------------------------------------
055300020204     C*
055400030715     C                   ENDIF
055500020204     C*
055600010607     C                   ENDSR
055700010607     C*----------------------------------------------------*
055800040714
055900010330
056000010330
056100010330     C*----------------------------------------------------*
056200010330     C*  CONTROLLO NUMERICITA' CAMPI
056300010330     C*----------------------------------------------------*
056400010330     C     CHKNUM        BEGSR
056500010330     C*
056600010606     C                   IF        PiDecChr = *blanks
056700030715     C                   EVAL      PiDecChr = CharNUM
056800010606     C                   ENDIF
056900010606     C*
057000010606     C                   CALL(e)   'ISNUMERIC'
057100010330     C                   PARM                    PiStr            30
057200010606     C                   PARM                    PiDecChr          1
057300010330     C                   PARM      *ZEROS        PiVal            30 9
057400010330     C                   PARM      '0'           PiInt             1
057500010330     C                   PARM      '0'           PiNum             1
057600010330     C                   IF        %error
057700010606     C                   EVAL      PiNum=*off
057800010330     C                   ENDIF
057900010330     C*
058000010330     C                   ENDSR
058100010330     C***
058200010601
058300010601
058400010601
058500010601      /TITLE Invio dei dati al punto operativo.
058600010601     C     opeini        BEGSR
058700010601     C*
058800010601     C* Inizializzo flag e contatori operativi
058900010601     C                   movel     '0'           flgGiro           1
059000010615     C                   movel     '0'           flgOk             1
059100010601     C                   z-add     *zeros        cntNonEl         10 0
059200010601     C*
059300010601     C                   ENDSR
059400010601     C***
059500010601
059600050421
059700050421
059800050421     C     *pssr         BEGSR
059900050421     C*
060000050421     C                   eval      wrkesito = '2'
060100050421     C*
060200050421     C                   ENDSR     '*CANCL'
060300050421     C***
060400010601
060500010330
060600010330
060700000613     C     *inzsr        BEGSR
060800990910     C*
060900990910     C     *entry        plist
061000990920     C                   parm                    tivlrds
061100990921     C                   parm      wrkesito      esito
061200000724     C                   parm                    prmlit
061300000710     C                   parm                    prmfir
061400010330     C*
061500010330     C* CALCOLA LA DATA CORRENTE
061600130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
061700130724     C                   eval      datcor = %dec(%date() : *ISO)
061800000613     C*
061900000613     C                   ENDSR
062000000613     C***
062100040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
062200130729EFRRFP;EFRRIF;X;X;X;X;X;X;X;X;EFRPES;X;X;X;X;X;X;X;X;X;X;X;EFRNFT;
062300130729X;W_DFT;EFRIMP;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
062400130528
