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
003500130813     D Data8N          s              8s 0
003600130813     D wEFRRIF2        s                   like(EFRRIF)
003700130813     D result          s             50
003800130813     D wK              s              3s 0
003900130813     D wX              s              3s 0
004000130813     D wY              s              3s 0
004100030822     D*------------------
004200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004300030822     D*------------------
004400130724     D posDaDft        S              4  0 INZ(*zeros)
004500130724     D posADft         S              4  0 INZ(*zeros)
004600130724     D j               s              4  0 INZ(1)
004700030822     D wGiroDft        s              1  0 INZ(*zeros)
004800110222
004900031201     D*------------------
005000031201     D* Costanti
005100031201     D*------------------
005200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005400130813     D cifre           c                   const('0123456789')
005500010330
005600010330
005700010330
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200130724     C                   exsr      rwfile
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400130724     C                   if        flgGiro = '0'
007500010601     C*
007600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007700010601     C                   eval      flgGiro = '1'
007800010601     C*
007900010601     C                   endif
008000010601     C*
008100010601     C                   ENDSR
008200010601     C***
008300010601
008400010601
008500010601
008600010601     C*--------------------------------------------------------
008700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008800010601     C*--------------------------------------------------------
008900010601     C     ENDELA        BEGSR
009000000616     C*
009100010601     C                   ENDSR
009200010601     C***
009300010601
009400130724
009500010601
009600010330     C*--------------------------------------------------------
009700130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
009800010330     C*--------------------------------------------------------
009900130724     C     RWFILE        BEGSR
010000010330     C*
010100010330     C                   if        not %open(tivin00r)
010200010330     C                   open      tivin00r
010300010330     C                   endif
010400010330     C*
010500010604     C                   clear                   §CTROK
010600010604     C                   clear                   §CTRMO
010700010604     C                   clear                   §CTRNO
010800040726     C*
010900040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011000040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011100130724 xxx C***                exsr      inzvar
011200130724     C***                exsr      impvat
011300010330     C*
011400030822     C                   do        *HIVAL
011500010330     C*
011600030822     C                   read      tivin00r                               70
011700010618     C*
011800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011900010618     C                   if        vinflg = '1'
012000010618     C                   eval      flgOk = '1'
012100010618     C                   endif
012200010618     C*
012300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012400040510     C                   z-add     *zeros        wLenVINDTA        4 0
012500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
012800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
012900040510     C                   z-add     1             wFlgVINDTA
013000040510     C                   leave
013100040510     C                   endif
013200040510     C                   enddo
013300040510     C*
013400040218     C                   if        vindta > *blanks AND
013500040510     C                             wFlgVINDTA = 1
013600010330     C*
013700010601     C                   if        *in70 = *off and
013800010330     C                             (vinflg = *blanks
013900010330     C                              or vinflg = '0'
014000010330     C                              or vinflg = '2')
014100010330     C*
014200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014300010711     C                   if        vinflg = *blanks or vinflg = '0'
014400010711     C                   clear                   vinmsg
014500010711     C                   endif
014600130813     C*
014700130813     C* Aggiungo un byte a blanks all'inizio d ogni record
014800130813     C* siccome sappiamo che la prima colonna è vuota per le righe di totale
014900130813     C* mettiamo un carattere non vuoto per evitare che vengano elisi i blank
015000130813     C                   eval      vindta  = '*' + %trim(vindta)
015100010601     C*
015200010330     C                   exsr      inzvar
015300130724     C                   exsr      impfile
015400010601     C*
015500010601     C                   exsr      PREELA
015600010601     C*
015700010604     C* Ebbene...
015800010604     C*
015900030822     C  N31              add       1             §CTROK            7 0
016000030822     C   32              add       1             §CTRMO            7 0
016100030822     C   31              add       1             §CTRNO            7 0
016200030822     C                   if        wGiro = 2
016300130813     C* scrivo se il nr. fattura è valorizzato
016400130813     C                   IF        EFRNFT <> *blank
016500130724     C  N31              write     TNEFR000
016600130813     C                   ENDIF
016700030822     C                   endif
016800010604     C*
016900010604     C                   if        *in31 = *off and
017000010604     C                             *in32 = *off
017100010604     C                   eval      vinflg = '1'
017200010604     C                   else
017300010604     C                   eval      vinflg = '2'
017400010604     C                   endif
017500130724     C*
017600130724     C                   endif
017700010604     C*
017800010330     C                   else
017900010330     C                   eval      vinflg = '1'
018000010330     C                   endif
018100010601     C*
018200010601     C  N70              update    tivin000
018300010330     C*
018400030822     C  N70              enddo
018500010601     C
018600010601     C*
018700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018800130724     C                   if        cntNonEl = *zeros
018900010330     C* Se non ci sono record con errori ...
019000010601     C                   if        §ctrno = 0 and
019100130724     C                             §ctrmo = 0
019200010330     C* ... restituisco esito OK.
019300010330     C                   eval      wrkesito = '0'
019400010330     C                   else
019500010330     C                   if        §ctrok > 0
019600010330     C                   eval      wrkesito = '1'
019700010330     C                   else
019800010615     C                   if        flgOk = '0'
019900010615     C                   eval      wrkesito = '2'
020000010615     C                   else
020100010615     C                   eval      wrkesito = '6'
020200010615     C                   endif
020300010330     C                   endif
020400010330     C                   endif
020500010601     C                   else
020600010601     C                   eval      wrkesito = '9'
020700010601     C                   endif
020800010330     C*
020900010330     C                   if        %open(tivin00r)
021000010330     C                   close     tivin00r
021100010330     C                   endif
021200010601     C*
021300010618     C                   if        flgGiro = '1'
021400010601     C                   exsr      endela
021500010618     C                   endif
021600010330     C*
021700010330     C                   ENDSR
021800010330     C***
021900010601
022000010601
022100010601
022200010330     C*----------------------------------------------------*
022300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400010330     C*----------------------------------------------------*
022500010330     C     INZVAR        BEGSR
022600010330     C*
022700030822     C* Inizializzo variabili di wrk
022800130724     C
022900030822     C*
023000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023100130724     C                   CLEAR                   TNEFR000
023200030822     C                   CLEAR                   SkSplitCSV
023300030822     C                   EVAL      i = 1
023400030822     C                   EVAL      posDa = *zeros
023500030822     C                   EVAL      posA  = *zeros
023600030822     C*
023700030822     C* Reimposto i valori di default
023800030822     C                   EXSR      DEFCAM
023900030822     C*
024000010330     C                   ENDSR
024100130724
024200130724
024300130724
024400010330     C*----------------------------------------------------*
024500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
024600010330     C*----------------------------------------------------*
024700010330     C     DEFCAM        BEGSR
024800030822     C*
024900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025000030715     C* e delimitatore testo.
025100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
025200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
025300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
025400030822     C*
025500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
025600130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
025700030715     C                   ENDIF
025800030822     C*
025900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026000030822     C                   EVAL      posDaDft = 1
026100030822     C                   EVAL      posADft  = 0
026200030822     C                   EVAL      wGiroDft = 0
026300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
026400030822     C                             posDaDft > 0
026500030822     C*
026600030822     C* Gestisco il 1° giro
026700030822     C                   IF        wGiroDft = 0
026800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
026900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027000030822     C* Incremento il contatore dei "giri"
027100030822     C                   EVAL      wGiroDft = 1
027200030822     C                   ELSE
027300030822     C                   EVAL      posDaDft = posADft
027400030822     C                   ENDIF
027500030822     C* Eseguo lo scan x trovare la fine del campo corrente
027600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
027700030822     C*
027800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
027900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028000030822     C                   IF        posDaDft > 0 AND
028100030822     C                             posADft  > 0
028200130724     C* NTW
028300130724     C                   IF        %subst(
028400130724     C                             %subst(vlrppt:posDaDft+1:
028500130724     C                             posADft-posDaDft-1):1:3)
028600130724     C                             = 'NTW'
028700130724     C                   EVAL      EFRNTW=%trim(%subst(
028800130724     C                             %subst(vlrppt:posDaDft+1:
028900130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029000130724     C                   ENDIF
029100130724     C* TPD
029200130724     C                   IF        %subst(
029300130724     C                             %subst(vlrppt:posDaDft+1:
029400130724     C                             posADft-posDaDft-1):1:3)
029500130724     C                             = 'TPD'
029600130724     C                   EVAL      EFRTPD=%trim(%subst(
029700130724     C                             %subst(vlrppt:posDaDft+1:
029800130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029900130724     C                   ENDIF
030000130724     C* CAU
030100130724     C                   IF        %subst(
030200130724     C                             %subst(vlrppt:posDaDft+1:
030300130724     C                             posADft-posDaDft-1):1:3)
030400130724     C                             = 'CAU'
030500130724     C                   EVAL      EFRCAU=%trim(%subst(
030600130724     C                             %subst(vlrppt:posDaDft+1:
030700130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030800130724     C                   ENDIF
030900130724     C* GRP
031000130724     C                   IF        %subst(
031100130724     C                             %subst(vlrppt:posDaDft+1:
031200130724     C                             posADft-posDaDft-1):1:3)
031300130724     C                             = 'GRP'
031400130724     C                   EVAL      EFRGRP=%trim(%subst(
031500130724     C                             %subst(vlrppt:posDaDft+1:
031600130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031700130724     C                   ENDIF
031800030822     C* ...
031900030822     C                   ENDIF
032000030822     C                   ENDDO
032100020204     C*
032200010330     C                   ENDSR
032300130724
032400130724
032500130724
032600010607     C*----------------------------------------------------*
032700130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
032800010607     C*----------------------------------------------------*
032900130724     C     IMPFILE       BEGSR
033000010607     C*
033100130724     C                   SETOFF                                       3132
033200130724     C*
033300130724     C                   MOVEL     datcor        EFRDTR
033400010607     C*
033500030715     C**********
033600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
033700030715     C**********
0338001308131    C                   IF        wGiro = *zeros
033900130813     C* scarto anche la II riga di intestazioni
034000130813     C                             or wGiro = 1
034100130813     C                   IF        wGiro = 1
034200130813     C* devo mettere un valore diverso da 0 o 1 (perché esegua l'ELSE) o 2 (perché non faccia la
034300130813     C*  prossima write)
034400130813     C                   EVAL      wGiro = 3
034500130813     C                   LEAVESR
034600130813     C                   ENDIF
034700040726     C*
034800040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
034900040726     C                   EVAL      depvinDTA = vinDTA
035000040726     C*
035100040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
035200040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
035300040726     C                                      %trim(intcol(3))
035400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
035500031201     C     minu:maiu     XLATE     vindta        vindta
035600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
0357001308132    C                   DOW       posDa <= %len(%trimr(vindta))
035800030715     C*
035900030715     C* Gestisco il 1° campo
0360001308133    C                   IF        i = 1
036100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
036200030715     C                   EVAL      posDa = 1
036300030715     C* Eseguo lo scan x trovare la fine del primo campo
036400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
036500030715     C* A questo punto "estraggo" il campo corrente
036600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
036700030715     C                                                    (posA-posDa))
036800030715     C* X i campi successivi al 1°
0369001308133e   C                   ELSE
037000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037100030715     C                   EVAL      posDa = posA + 1
037200030715     C* Eseguo lo scan x trovare la fine del campo corrente
037300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037400030715     C* Gestisco l'ultimo campo
0375001308134    C                   IF        posA = *zeros
037600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
037700030715     C* A questo punto "estraggo" il campo corrente
037800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037900030715     C                                                    (posA-posDa))
038000030715     C                   LEAVE
0381001308134e   C                   ELSE
038200030715     C* A questo punto "estraggo" il campo corrente
038300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038400030715     C                                                    (posA-posDa))
0385001308134-   C                   ENDIF
0386001308133-   C                   ENDIF
038700030715     C* Incremento il contatore d campo
038800030715     C                   EVAL      i = i +1
0389001308132-   C                   ENDDO
039000030715     C*
039100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
039200030715     C* delimitazione testo
039300030715     C                   EVAL      i = 1
0394001308132    C                   DOW       i < %elem(SkSplitFLD)
039500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
039600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
039700030715     C                   EVAL      i = i + 1
0398001308132-   C                   ENDDO
039900030715     C                   EVAL      wGiro = 1
040000040726     C* Ripristino la 1° riga originale
040100040726     C                   EVAL      vinDTA = depvinDTA
0402001308131e   C                   ELSE
040300030715     C                   EVAL      wGiro = 2
040400031204     C**********
040500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
040600031204     C* d separatore campo
040700031204     C**********
040800040726     C                   EVAL      vindta = %trim(vindta)
0409001308132    C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
041000040726     C                   EVAL      vindta = %subst(vindta:2)
0411001308132-   C                   ENDDO
041200031204     C*
041300031204     C                   Z-ADD     *zeros        lunghInput        4 0
041400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
0415001308132    C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
041600040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
041700040726     C                   EVAL      lunghInput = %len(%trim(vindta))
0418001308132-   C                   ENDDO
041900030715     C**********
042000030715     C* Effettuo lo split del campo dati d input
042100030715     C**********
042200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
0423001308132    C                   DOW       posDa <= %len(%trimr(vindta))
042400030715     C*
042500030715     C* Gestisco il 1° campo
0426001308133    C                   IF        i = 1
042700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
042800030715     C                   EVAL      posDa = 1
042900030715     C* Eseguo lo scan x trovare la fine del primo campo
043000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043100030715     C* A questo punto "estraggo" il campo corrente
043200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043300030715     C                                                    (posA-posDa))
043400030715     C* X i campi successivi al 1°
0435001308133e   C                   ELSE
043600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
043700030715     C                   EVAL      posDa = posA + 1
043800031204     C* Verifico che nn vi sia il campo nn valorizzato
0439001308134    C                   IF        %subst(vindta:posDa:1) = CharCSV
044000031204     C* Se campo nn valorizzato skippo al prossimo
044100031204     C                   EVAL      SkSplitCSV(i) = *blanks
044200031204     C                   EVAL      posA  = posA + 1
0443001308134e   C                   ELSE
044400030715     C* Eseguo lo scan x trovare la fine del campo corrente
044500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044600030715     C* Gestisco l'ultimo campo
0447001308135    C                   IF        posA = *zeros
044800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
044900030715     C* A questo punto "estraggo" il campo corrente
045000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045100030715     C                                                    (posA-posDa))
045200030715     C                   LEAVE
0453001308135e   C                   ELSE
045400030715     C* A questo punto "estraggo" il campo corrente
045500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045600030715     C                                                    (posA-posDa))
0457001308135-   C                   ENDIF
0458001308134-   C                   ENDIF
0459001308133-   C                   ENDIF
046000030715     C* Incremento il contatore d campo
046100030715     C                   EVAL      i = i +1
0462001308132-   C                   ENDDO
046300030715     C*
046400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
046500030715     C* delimitazione testo
046600030715     C                   EVAL      i = 1
0467001308132    C                   DOW       i < %elem(SkSplitCSV)
046800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
046900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
047000030715     C                   EVAL      i = i + 1
0471001308132-   C                   ENDDO
047200030715     C*
047300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
047400030715     C                   EVAL      i = 1
0475001308132    C                   DOW       i < %elem(SkSplitFLD)
047600030715     C*
047700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
0478001308133    C                   IF        SkSplitFLD(i) <> *blanks AND
047900030715     C                             SkSplitCSV(i) <> *blanks
048000130724     C*** RIF
048100130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
048200130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
048300130724     C                   ENDIF
048400130724     C*** RFP
048500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
048600130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
048700130724     C                   ENDIF
048800130724     C*** NFT
048900130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
049000130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
049100130724     C                   ENDIF
049200130724     C*
049300130724     C* Reperisco quindi i campi numerici...
049400130724     C*** DFT
049500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
049600130724     C                   EVAL      PiStr=SkSplitCSV(i)
049700130724     C                   EXSR      CHKNUM
049800130724     C                   IF        PiInt=*on
049900130724     C                   Z-ADD     PiVal         EFRDFT
050000130724     C                   ELSE
050100130724     C                   SETON                                        31
050200130724     C                   EVAL      EFRDFT = *zeros
050300130724     C                   EVAL      vinmsg = %trimr(vinmsg)
050400130724     C                             + ' ' + 'EFRDFT'
050500130724     C                   ENDIF
050600130724     C                   ENDIF
050700130724     C*** IMP
050800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
050900130724     C                   EVAL      PiStr=SkSplitCSV(i)
051000130724     C                   EXSR      CHKNUM
051100130724     C                   IF        PiNum=*on
051200130724     C                   Z-ADD     PiVal         EFRIMP
051300130724     C                   ELSE
051400130724     C                   SETON                                        31
051500130724     C                   EVAL      EFRIMP = *zeros
051600130724     C                   EVAL      vinmsg = %trimr(vinmsg)
051700130724     C                             + ' ' + 'EFRIMP'
051800130724     C                   ENDIF
051900130724     C                   ENDIF
052000130724     C*** PES
052100130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
052200130724     C                   EVAL      PiStr=SkSplitCSV(i)
052300130724     C                   EXSR      CHKNUM
052400130724     C                   IF        PiNum=*on
052500130724     C                   Z-ADD     PiVal         EFRPES
052600130724     C                   ELSE
052700130724     C                   SETON                                        31
052800130724     C                   EVAL      EFRPES = *zeros
052900130724     C                   EVAL      vinmsg = %trimr(vinmsg)
053000130724     C                             + ' ' + 'EFRPES'
053100130724     C                   ENDIF
053200130724     C                   ENDIF
053300130724     C*** VOL
053400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
053500130724     C                   EVAL      PiStr=SkSplitCSV(i)
053600130724     C                   EXSR      CHKNUM
053700130724     C                   IF        PiNum=*on
053800130724     C                   Z-ADD     PiVal         EFRVOL
053900130724     C                   ELSE
054000130724     C                   SETON                                        31
054100130724     C                   EVAL      EFRVOL = *zeros
054200130724     C                   EVAL      vinmsg = %trimr(vinmsg)
054300130724     C                             + ' ' + 'EFRVOL'
054400130724     C                   ENDIF
054500130724     C                   ENDIF
054600110222     C*
054700110222     C***  ===>  Gestione campi "particolari" dentro ciclo
054800110222     C*
054900130813      *** W_DFT: la data è in formato ggmmaaaa o gmmaaaa alfanum. e va trasformata in 8 S
055000130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
055100130813     C                   EVAL      DATA8N = %dec(%trim(SkSplitCSV(i)) : 8 : 0)
055200130813     C                   EVAL      Data8A = %subst(%editc(Data8N:'X'):5:4) +
055300130813     C                                      %subst(%editc(Data8N:'X'):3:2) +
055400130813     C                                      %subst(%editc(Data8N:'X'):1:2)
055500130729     C                   MOVEL     Data8A        EFRDFT
055600130729     C                   ENDIF
055700130813
055800130813     C*
055900130813      *** W_RIF_1: dopo aver eliso il primo byte se è = ' controllo che il risultato sia un numerico
056000130813     C*          lungo 14
056100130813     C*          se non lo è prendo il _2
056200130813     C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF_1'
056300130813     C* controllo se il primo carattere è apice
056400130813     C                   EVAL      result = %trim(SkSplitCSV(i))
056500130813     C                   IF        %subst(result:1:1)=''''
056600130813     C* se lo è, lo elido
056700130813     C                   EVAL      result = %replace( '' : result : 1 : 1)
056800130813     C                   ENDIF
056900130813     C* controllo che sia lungo 14
057000130813     C                   IF        %len(%trim(result))=14
057100130813     C* controllo che sia numerico
057200130813     C                   EVAL      wX = %checkR(cifre : result : 14)
057300130813     C                   IF        wX = 0
057400130813     C* se numerico lungo 14 è questo EFRRIF
057500130813     C                   EVAL      EFRRIF = %trim(result)
057600130813     C                   ENDIF
057700130813     C                   ENDIF
057800130813     C                   ENDIF
057900130813
058000130813     C*
058100130813      *** W_RIF_2: lo salvo e fuori ciclo testo se EFRRIF è già valorizzato (con W_RIF_1) o no
058200130813     C*          se non lo è prendo il _2
058300130813     C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF_2'
058400130813     C* controllo se il primo carattere è apice
058500130813     C                   EVAL      result = %trim(SkSplitCSV(i))
058600130813     C                   IF        %subst(result:1:1)=''''
058700130813     C* se lo è, lo elido
058800130813     C                   EVAL      result = %replace( '' : result : 1 : 1)
058900130813     C                   ENDIF
059000130813     C                   EVAL      wEFRRIF2 = result
059100130813     C                   ENDIF
059200110222     C***  <===  -----------------------------------------
059300030715     C*
0594001308133-   C                   ENDIF
059500030715     C                   EVAL      i = i + 1
0596001308132-   C                   ENDDO
059700130724     C*
059800130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
059900130724     C
060000130813     C* se EFRRIF non è stato valorizzato da _1
060100130813     C                   IF        EFRRIF = *blank
060200130813     C* lo valorizzo col _2
060300130813     C                   EVAL      EFRRIF = wEFRRIF2
060400130813     C                   ENDIF
060500130813     C
060600130724     C***  <===  -----------------------------------------
060700020204     C*
0608001308131-   C                   ENDIF
060900020204     C*
061000010607     C                   ENDSR
061100010607     C*----------------------------------------------------*
061200040714
061300010330
061400010330
061500010330     C*----------------------------------------------------*
061600010330     C*  CONTROLLO NUMERICITA' CAMPI
061700010330     C*----------------------------------------------------*
061800010330     C     CHKNUM        BEGSR
061900010330     C*
062000010606     C                   IF        PiDecChr = *blanks
062100030715     C                   EVAL      PiDecChr = CharNUM
062200010606     C                   ENDIF
062300010606     C*
062400010606     C                   CALL(e)   'ISNUMERIC'
062500010330     C                   PARM                    PiStr            30
062600010606     C                   PARM                    PiDecChr          1
062700010330     C                   PARM      *ZEROS        PiVal            30 9
062800010330     C                   PARM      '0'           PiInt             1
062900010330     C                   PARM      '0'           PiNum             1
063000010330     C                   IF        %error
063100010606     C                   EVAL      PiNum=*off
063200010330     C                   ENDIF
063300010330     C*
063400010330     C                   ENDSR
063500010330     C***
063600010601
063700010601
063800010601
063900010601      /TITLE Invio dei dati al punto operativo.
064000010601     C     opeini        BEGSR
064100010601     C*
064200010601     C* Inizializzo flag e contatori operativi
064300010601     C                   movel     '0'           flgGiro           1
064400010615     C                   movel     '0'           flgOk             1
064500010601     C                   z-add     *zeros        cntNonEl         10 0
064600010601     C*
064700010601     C                   ENDSR
064800010601     C***
064900010601
065000050421
065100050421
065200050421     C     *pssr         BEGSR
065300050421     C*
065400050421     C                   eval      wrkesito = '2'
065500050421     C*
065600050421     C                   ENDSR     '*CANCL'
065700050421     C***
065800010601
065900010330
066000010330
066100000613     C     *inzsr        BEGSR
066200990910     C*
066300990910     C     *entry        plist
066400990920     C                   parm                    tivlrds
066500990921     C                   parm      wrkesito      esito
066600000724     C                   parm                    prmlit
066700000710     C                   parm                    prmfir
066800010330     C*
066900010330     C* CALCOLA LA DATA CORRENTE
067000130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
067100130724     C                   eval      datcor = %dec(%date() : *ISO)
067200000613     C*
067300000613     C                   ENDSR
067400000613     C***
067500040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
067600130813X;X;EFRNFT;W_DFT;X;W_RIF_2;W_RIF_1;EFRRFP;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;EFRPES;
067700131029X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
067800131029X;X;X;X;X;EFRIMP;
