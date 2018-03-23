000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500130724     FTNEFR00F  O    E             DISK
000600130731     FTITAS30C  IF   E           K DISK    usropn
000700000313     D*
000800040726     D*------------
000900040726     D* SCHIEDE A PROGRAMMA
001000040726     D*------------
001100040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002300030715     D*------------------
002400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002500030715     D*------------------
002600030715     D SkSplitFLD      S             10    DIM(100)
002700030715     D SkSplitCSV      S            256    DIM(100)
002800030715     D CharCSV         S              1
002900030715     D CharTXT         S              1
003000030715     D CharNUM         S              1
003100130724     D posDa           S              4  0 INZ(*zeros)
003200130724     D posA            S              4  0 INZ(*zeros)
003300130724     D i               s              4  0 INZ(1)
003400030715     D wGiro           s              1  0 INZ(*zeros)
003500130731     D Data8A          s              8
003600130731     D w14A            s             14
003700130731     D result          s             50
003800130731     D wK              s              3s 0
003900130731     D wX              s              3s 0
004000130731     D wY              s              3s 0
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
005400130731     D cifre           c                   const('0123456789')
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
010400130731     C                   if        not %open(titas30c)
010500130731     C                   open      titas30c
010600130731     C                   endif
010700010330     C*
010800010604     C                   clear                   §CTROK
010900010604     C                   clear                   §CTRMO
011000010604     C                   clear                   §CTRNO
011100040726     C*
011200040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011300040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011400130724 xxx C***                exsr      inzvar
011500130724     C***                exsr      impvat
011600010330     C*
011700030822     C                   do        *HIVAL
011800010330     C*
011900030822     C                   read      tivin00r                               70
012000010618     C*
012100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012200010618     C                   if        vinflg = '1'
012300010618     C                   eval      flgOk = '1'
012400010618     C                   endif
012500010618     C*
012600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012700040510     C                   z-add     *zeros        wLenVINDTA        4 0
012800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013200040510     C                   z-add     1             wFlgVINDTA
013300040510     C                   leave
013400040510     C                   endif
013500040510     C                   enddo
013600040510     C*
013700040218     C                   if        vindta > *blanks AND
013800040510     C                             wFlgVINDTA = 1
013900010330     C*
014000010601     C                   if        *in70 = *off and
014100010330     C                             (vinflg = *blanks
014200010330     C                              or vinflg = '0'
014300010330     C                              or vinflg = '2')
014400010330     C*
014500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014600010711     C                   if        vinflg = *blanks or vinflg = '0'
014700010711     C                   clear                   vinmsg
014800010711     C                   endif
014900010601     C*
015000010330     C                   exsr      inzvar
015100130724     C                   exsr      impfile
015200010601     C*
015300010601     C                   exsr      PREELA
015400010601     C*
015500010604     C* Ebbene...
015600010604     C*
015700030822     C  N31              add       1             §CTROK            7 0
015800030822     C   32              add       1             §CTRMO            7 0
015900030822     C   31              add       1             §CTRNO            7 0
016000030822     C                   if        wGiro = 2
016100130731     C* scrivo se il network è <> LNA
016200130731     C* 'LNA' non è un network, indica che mi è stato detto di reperire il network dalla bolla,
016300130731     C* cioè da TITAS (quindi non sono scritti né gli ORM né tutto ciò che non contiene un
016400130731     C* riferimento BRT associabile a una bolla)
016500130731     C                   IF        EFRNTW <> 'LNA'
016600130724     C  N31              write     TNEFR000
016700130731     C                   ENDIF
016800130731     C                   endif
016900010604     C*
017000010604     C                   if        *in31 = *off and
017100010604     C                             *in32 = *off
017200010604     C                   eval      vinflg = '1'
017300010604     C                   else
017400010604     C                   eval      vinflg = '2'
017500010604     C                   endif
017600130724     C*
017700130724     C                   endif
017800010604     C*
017900010330     C                   else
018000010330     C                   eval      vinflg = '1'
018100010330     C                   endif
018200010601     C*
018300010601     C  N70              update    tivin000
018400010330     C*
018500030822     C  N70              enddo
018600010601     C
018700010601     C*
018800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018900130724     C                   if        cntNonEl = *zeros
019000010330     C* Se non ci sono record con errori ...
019100010601     C                   if        §ctrno = 0 and
019200130724     C                             §ctrmo = 0
019300010330     C* ... restituisco esito OK.
019400010330     C                   eval      wrkesito = '0'
019500010330     C                   else
019600010330     C                   if        §ctrok > 0
019700010330     C                   eval      wrkesito = '1'
019800010330     C                   else
019900010615     C                   if        flgOk = '0'
020000010615     C                   eval      wrkesito = '2'
020100010615     C                   else
020200010615     C                   eval      wrkesito = '6'
020300010615     C                   endif
020400010330     C                   endif
020500010330     C                   endif
020600010601     C                   else
020700010601     C                   eval      wrkesito = '9'
020800010601     C                   endif
020900010330     C*
021000010330     C                   if        %open(tivin00r)
021100010330     C                   close     tivin00r
021200010330     C                   endif
021300130731     C                   if        %open(titas30c)
021400130731     C                   close     titas30c
021500130731     C                   endif
021600010601     C*
021700010618     C                   if        flgGiro = '1'
021800010601     C                   exsr      endela
021900010618     C                   endif
022000010330     C*
022100010330     C                   ENDSR
022200010330     C***
022300010601
022400010601
022500010601
022600010330     C*----------------------------------------------------*
022700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022800010330     C*----------------------------------------------------*
022900010330     C     INZVAR        BEGSR
023000010330     C*
023100030822     C* Inizializzo variabili di wrk
023200130724     C
023300030822     C*
023400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023500130724     C                   CLEAR                   TNEFR000
023600030822     C                   CLEAR                   SkSplitCSV
023700030822     C                   EVAL      i = 1
023800030822     C                   EVAL      posDa = *zeros
023900030822     C                   EVAL      posA  = *zeros
024000030822     C*
024100030822     C* Reimposto i valori di default
024200030822     C                   EXSR      DEFCAM
024300030822     C*
024400010330     C                   ENDSR
024500130724
024600130724
024700130724
024800010330     C*----------------------------------------------------*
024900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025000010330     C*----------------------------------------------------*
025100010330     C     DEFCAM        BEGSR
025200030822     C*
025300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025400030715     C* e delimitatore testo.
025500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
025600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
025700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
025800030822     C*
025900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026000130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
026100030715     C                   ENDIF
026200030822     C*
026300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026400030822     C                   EVAL      posDaDft = 1
026500030822     C                   EVAL      posADft  = 0
026600030822     C                   EVAL      wGiroDft = 0
026700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
026800030822     C                             posDaDft > 0
026900030822     C*
027000030822     C* Gestisco il 1° giro
027100030822     C                   IF        wGiroDft = 0
027200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
027300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027400030822     C* Incremento il contatore dei "giri"
027500030822     C                   EVAL      wGiroDft = 1
027600030822     C                   ELSE
027700030822     C                   EVAL      posDaDft = posADft
027800030822     C                   ENDIF
027900030822     C* Eseguo lo scan x trovare la fine del campo corrente
028000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028100030822     C*
028200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
028300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028400030822     C                   IF        posDaDft > 0 AND
028500030822     C                             posADft  > 0
028600130724     C* NTW
028700130724     C                   IF        %subst(
028800130724     C                             %subst(vlrppt:posDaDft+1:
028900130724     C                             posADft-posDaDft-1):1:3)
029000130724     C                             = 'NTW'
029100130731     C* se il valore del parametro è *LNA, lo memorizzo perché mi serve dopo (EFRNTW='LNA')
029200130731     C                   IF        %trim(%subst(
029300130724     C                             %subst(vlrppt:posDaDft+1:
029400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029500130731     C                             = '*LNA'
029600130731     C                   EVAL      EFRNTW='LNA'
029700130731     C                   ELSE
029800130731     C                   EVAL      EFRNTW=%trim(%subst(
029900130731     C                             %subst(vlrppt:posDaDft+1:
030000130731     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030100130724     C                   ENDIF
030200130731     C                   ENDIF
030300130724     C* TPD
030400130724     C                   IF        %subst(
030500130724     C                             %subst(vlrppt:posDaDft+1:
030600130724     C                             posADft-posDaDft-1):1:3)
030700130724     C                             = 'TPD'
030800130724     C                   EVAL      EFRTPD=%trim(%subst(
030900130724     C                             %subst(vlrppt:posDaDft+1:
031000130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031100130724     C                   ENDIF
031200130724     C* CAU
031300130724     C                   IF        %subst(
031400130724     C                             %subst(vlrppt:posDaDft+1:
031500130724     C                             posADft-posDaDft-1):1:3)
031600130724     C                             = 'CAU'
031700130724     C                   EVAL      EFRCAU=%trim(%subst(
031800130724     C                             %subst(vlrppt:posDaDft+1:
031900130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032000130724     C                   ENDIF
032100130724     C* GRP
032200130724     C                   IF        %subst(
032300130724     C                             %subst(vlrppt:posDaDft+1:
032400130724     C                             posADft-posDaDft-1):1:3)
032500130724     C                             = 'GRP'
032600130724     C                   EVAL      EFRGRP=%trim(%subst(
032700130724     C                             %subst(vlrppt:posDaDft+1:
032800130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032900130724     C                   ENDIF
033000030822     C* ...
033100030822     C                   ENDIF
033200030822     C                   ENDDO
033300020204     C*
033400010330     C                   ENDSR
033500130724
033600130724
033700130724
033800010607     C*----------------------------------------------------*
033900130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
034000010607     C*----------------------------------------------------*
034100130724     C     IMPFILE       BEGSR
034200010607     C*
034300130724     C                   SETOFF                                       3132
034400130724     C*
034500130724     C                   MOVEL     datcor        EFRDTR
034600010607     C*
034700030715     C**********
034800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
034900030715     C**********
035000030715     C                   IF        wGiro = *zeros
035100040726     C*
035200040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
035300040726     C                   EVAL      depvinDTA = vinDTA
035400040726     C*
035500040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
035600040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
035700040726     C                                      %trim(intcol(3))
035800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
035900031201     C     minu:maiu     XLATE     vindta        vindta
036000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
036100040324     C                   DOW       posDa <= %len(%trimr(vindta))
036200030715     C*
036300030715     C* Gestisco il 1° campo
036400030715     C                   IF        i = 1
036500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
036600030715     C                   EVAL      posDa = 1
036700030715     C* Eseguo lo scan x trovare la fine del primo campo
036800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
036900030715     C* A questo punto "estraggo" il campo corrente
037000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037100030715     C                                                    (posA-posDa))
037200030715     C* X i campi successivi al 1°
037300030715     C                   ELSE
037400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037500030715     C                   EVAL      posDa = posA + 1
037600030715     C* Eseguo lo scan x trovare la fine del campo corrente
037700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037800030715     C* Gestisco l'ultimo campo
037900030715     C                   IF        posA = *zeros
038000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
038100030715     C* A questo punto "estraggo" il campo corrente
038200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038300030715     C                                                    (posA-posDa))
038400030715     C                   LEAVE
038500030715     C                   ELSE
038600030715     C* A questo punto "estraggo" il campo corrente
038700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038800030715     C                                                    (posA-posDa))
038900030715     C                   ENDIF
039000030715     C                   ENDIF
039100030715     C* Incremento il contatore d campo
039200030715     C                   EVAL      i = i +1
039300030715     C                   ENDDO
039400030715     C*
039500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
039600030715     C* delimitazione testo
039700030715     C                   EVAL      i = 1
039800030715     C                   DOW       i < %elem(SkSplitFLD)
039900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
040000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
040100030715     C                   EVAL      i = i + 1
040200030715     C                   ENDDO
040300030715     C                   EVAL      wGiro = 1
040400040726     C* Ripristino la 1° riga originale
040500040726     C                   EVAL      vinDTA = depvinDTA
040600030715     C                   ELSE
040700030715     C                   EVAL      wGiro = 2
040800031204     C**********
040900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
041000031204     C* d separatore campo
041100031204     C**********
041200040726     C                   EVAL      vindta = %trim(vindta)
041300040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
041400040726     C                   EVAL      vindta = %subst(vindta:2)
041500040726     C                   ENDDO
041600031204     C*
041700031204     C                   Z-ADD     *zeros        lunghInput        4 0
041800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
041900040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
042000040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
042100040726     C                   EVAL      lunghInput = %len(%trim(vindta))
042200040726     C                   ENDDO
042300030715     C**********
042400030715     C* Effettuo lo split del campo dati d input
042500030715     C**********
042600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
042700040324     C                   DOW       posDa <= %len(%trimr(vindta))
042800030715     C*
042900030715     C* Gestisco il 1° campo
043000030715     C                   IF        i = 1
043100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
043200030715     C                   EVAL      posDa = 1
043300030715     C* Eseguo lo scan x trovare la fine del primo campo
043400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043500030715     C* A questo punto "estraggo" il campo corrente
043600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043700030715     C                                                    (posA-posDa))
043800030715     C* X i campi successivi al 1°
043900030715     C                   ELSE
044000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
044100030715     C                   EVAL      posDa = posA + 1
044200031204     C* Verifico che nn vi sia il campo nn valorizzato
044300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
044400031204     C* Se campo nn valorizzato skippo al prossimo
044500031204     C                   EVAL      SkSplitCSV(i) = *blanks
044600031204     C                   EVAL      posA  = posA + 1
044700031204     C                   ELSE
044800030715     C* Eseguo lo scan x trovare la fine del campo corrente
044900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
045000030715     C* Gestisco l'ultimo campo
045100030715     C                   IF        posA = *zeros
045200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
045300030715     C* A questo punto "estraggo" il campo corrente
045400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045500030715     C                                                    (posA-posDa))
045600030715     C                   LEAVE
045700030715     C                   ELSE
045800030715     C* A questo punto "estraggo" il campo corrente
045900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046000030715     C                                                    (posA-posDa))
046100030715     C                   ENDIF
046200031204     C                   ENDIF
046300030715     C                   ENDIF
046400030715     C* Incremento il contatore d campo
046500030715     C                   EVAL      i = i +1
046600030715     C                   ENDDO
046700030715     C*
046800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
046900030715     C* delimitazione testo
047000030715     C                   EVAL      i = 1
047100030715     C                   DOW       i < %elem(SkSplitCSV)
047200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
047300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
047400030715     C                   EVAL      i = i + 1
047500030715     C                   ENDDO
047600030715     C*
047700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
047800030715     C                   EVAL      i = 1
047900030715     C                   DOW       i < %elem(SkSplitFLD)
048000030715     C*
048100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
048200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
048300030715     C                             SkSplitCSV(i) <> *blanks
048400130724     C*** RIF
048500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
048600130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
048700130724     C                   ENDIF
048800130724     C*** RFP
048900130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
049000130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
049100130724     C                   ENDIF
049200130724     C*** NFT
049300130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
049400130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
049500130724     C                   ENDIF
049600130724     C*
049700130724     C* Reperisco quindi i campi numerici...
049800130724     C*** DFT
049900130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
050000130724     C                   EVAL      PiStr=SkSplitCSV(i)
050100130724     C                   EXSR      CHKNUM
050200130724     C                   IF        PiInt=*on
050300130724     C                   Z-ADD     PiVal         EFRDFT
050400130724     C                   ELSE
050500130724     C                   SETON                                        31
050600130724     C                   EVAL      EFRDFT = *zeros
050700130724     C                   EVAL      vinmsg = %trimr(vinmsg)
050800130724     C                             + ' ' + 'EFRDFT'
050900130724     C                   ENDIF
051000130724     C                   ENDIF
051100130724     C*** IMP
051200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
051300130724     C                   EVAL      PiStr=SkSplitCSV(i)
051400130724     C                   EXSR      CHKNUM
051500130724     C                   IF        PiNum=*on
051600130724     C                   Z-ADD     PiVal         EFRIMP
051700130724     C                   ELSE
051800130724     C                   SETON                                        31
051900130724     C                   EVAL      EFRIMP = *zeros
052000130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052100130724     C                             + ' ' + 'EFRIMP'
052200130724     C                   ENDIF
052300130724     C                   ENDIF
052400130724     C*** PES
052500130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
052600130724     C                   EVAL      PiStr=SkSplitCSV(i)
052700130724     C                   EXSR      CHKNUM
052800130724     C                   IF        PiNum=*on
052900130724     C                   Z-ADD     PiVal         EFRPES
053000130724     C                   ELSE
053100130724     C                   SETON                                        31
053200130724     C                   EVAL      EFRPES = *zeros
053300130724     C                   EVAL      vinmsg = %trimr(vinmsg)
053400130724     C                             + ' ' + 'EFRPES'
053500130724     C                   ENDIF
053600130724     C                   ENDIF
053700130724     C*** VOL
053800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
053900130724     C                   EVAL      PiStr=SkSplitCSV(i)
054000130724     C                   EXSR      CHKNUM
054100130724     C                   IF        PiNum=*on
054200130724     C                   Z-ADD     PiVal         EFRVOL
054300130724     C                   ELSE
054400130724     C                   SETON                                        31
054500130724     C                   EVAL      EFRVOL = *zeros
054600130724     C                   EVAL      vinmsg = %trimr(vinmsg)
054700130724     C                             + ' ' + 'EFRVOL'
054800130724     C                   ENDIF
054900130724     C                   ENDIF
055000110222     C*
055100110222     C***  ===>  Gestione campi "particolari" dentro ciclo
055200130731     C*
055300130731      *** W_DFT: la data è in formato gg.mm.aaaa alfanum. e va trasformata in 8 S
055400130731     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
055500130731     C* il partner può mandare la data con l'anno di 2 o di 4, bisogna gestirla
055600130731     C                   IF        %len(%trim(SkSplitCSV(i)))=10
055700130731     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
055800130731     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
055900130731     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
056000130731     C                   MOVEL     Data8A        EFRDFT
056100130731     C                   ELSE
056200130731     C* se la data non fosse né di 6 né di 8, valorizzo EFTDFT come se mi arrivasse di 6
056300130731     C* così se il valore è impossibile va in errore e ce ne accorgiamo
056400130731     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
056500130731     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
056600130731     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
056700130731     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
056800130731     C                   MOVEL     Data8A        EFRDFT
056900130731     C                   ENDIF
057000130731     C                   ENDIF
057100130731
057200130731     C*
057300130731      *** W_RFT: - se il valore indicato è un numerico lungo 14 e se il valore del parametro in
057400130731     C*          ingresso 'NTW' è = '*LNA => scomponendo i 14 bytes in:
057500130731     C*            YY (portare da 13 a 2013) => TASAAS
057600130731     C*            LNP     => TASLNP
057700130731     C*            NRS     => TASNRS
057800130731     C*            NSP     => TASNSP
057900130731     C*          chainare il TITAS30C e reperire TASLNA da utilizzare nella valorizzazione del campo
058000130731     C*          EFRNTW
058100130731     C*          - diversamente normalizzare eliminando tutti i caratteri non numerici, quindi
058200130731     C*          compattare il dato e tenere solamente i primi 10 bytes (occhio che potrebbero
058300130731     C*          esserci dei casi in cui il dato rimanente è lungo meno di 10 bytes) quindi
058400130731     C*          valorizzazione il campo EFRRIF preceduto da 'ORM-'
058500130731     C*          - diversamente o se vuoto considerare errore in importazione dati
0586001307311    C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF'
058700130731     C                   EVAL      w14A = *blank
058800130731     C* controllo che sia lungo 14
0589001307312    C                   IF        %len(%trim(SkSplitCSV(i)))=14
059000130731     C                             and EFRNTW = 'LNA'
059100130731     C                   EVAL      w14A = %trim(SkSplitCSV(i))
059200130731     C* controllo che sia numerico
059300130731     C                   EVAL      wX = %check(cifre : w14A)
0594001307313    C                   IF        wX = 0
059500130731     C                   EVAL      TASAAS = 2000 + %dec(%subst(w14A:1:2) :4:0)
059600130731     C                   EVAL      TASLNP = %dec(%subst(w14A:3:3) :3:0)
059700130731     C                   EVAL      TASNRS = %dec(%subst(w14A:6:2) :2:0)
059800130731     C                   EVAL      TASNSP = %dec(%subst(w14A:8:7) :7:0)
059900130731     C                   EVAL      EFRRIF = w14A
060000130731     C     K04TAS30      CHAIN     TITAS30C
0601001307314    C                   IF        %found
060200130731     C                   EVAL      EFRNTW = %editc(TASLNA:'X')
060300130731     C* se non trovo il rcd su TITAS do un warning
0604001307314e   C                   ELSE
060500130731     C                   SETON                                        32
060600130731     C                   EVAL      EFRNTW = *blank
060700130731     C                   EVAL      vinmsg = %trimr(vinmsg)
060800130731     C                             + ' ' + 'EFRNTW EFRRIF'
0609001307314-   C                   ENDIF
0610001307313e   C                   ELSE
061100130731     C                   EVAL      w14A = *blank
0612001307313-   C                   ENDIF
0613001307312-   C                   ENDIF
061400130731     C* se il dato in ingresso non era lungo 14 char o non era numerico
061500130731     C* o il network nel parametro non è *LNA
0616001307312    C                   IF        w14A = *blank
061700130731     C* tolgo i caratteri non numerici e compatto e tengo solo i primi 10 char
061800130731     C                   EVAL      wX = 1
061900130731     C                   EVAL      result = %trim(SkSplitCSV(i))
062000130731       // salvo il risultato perché la variabile result non è fissa e non mi fido
062100130731     C                   EVAL      wY = %checkR(' ' : result)
062200130731     C* trovo l'ultimo carattere non blank
062300130731     C                   FOR       wK = 1 TO wY
062400130731     C                   EVAL      wX = %check(cifre : result  : wX)
062500130731     C                   if        wX > 0
062600130731     C                   EVAL      result = %replace( '' : result : wX : 1)
062700130731     C                   ELSE
062800130731     C                   LEAVE
062900130731     C                   ENDIF
063000130731     C                   ENDFOR
063100130731     C* se il risultato del compattamento è blank, do un warning
063200130731     C                   IF        result = *blank
063300130731     C                   SETON                                        32
063400130731     C                   EVAL      EFRRIF = *blank
063500130731     C                   EVAL      vinmsg = %trimr(vinmsg)
063600130731     C                             + ' ' + 'EFRRIF'
063700130731     C                   ELSE
063800130731     C                   EVAL      EFRRIF = 'ORM-' + %subst(result : 1 : 10)
063900130731     C                   ENDIF
0640001307312-   C                   ENDIF
0641001307311-   C                   ENDIF
064200130731     C*
064300110222     C***  <===  -----------------------------------------
064400030715     C*
064500030715     C                   ENDIF
064600030715     C                   EVAL      i = i + 1
064700030715     C                   ENDDO
064800130724     C*
064900130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
065000130724     C
065100130724     C***  <===  -----------------------------------------
065200020204     C*
065300030715     C                   ENDIF
065400020204     C*
065500010607     C                   ENDSR
065600010607     C*----------------------------------------------------*
065700040714
065800010330
065900010330
066000010330     C*----------------------------------------------------*
066100010330     C*  CONTROLLO NUMERICITA' CAMPI
066200010330     C*----------------------------------------------------*
066300010330     C     CHKNUM        BEGSR
066400010330     C*
066500010606     C                   IF        PiDecChr = *blanks
066600030715     C                   EVAL      PiDecChr = CharNUM
066700010606     C                   ENDIF
066800010606     C*
066900010606     C                   CALL(e)   'ISNUMERIC'
067000010330     C                   PARM                    PiStr            30
067100010606     C                   PARM                    PiDecChr          1
067200010330     C                   PARM      *ZEROS        PiVal            30 9
067300010330     C                   PARM      '0'           PiInt             1
067400010330     C                   PARM      '0'           PiNum             1
067500010330     C                   IF        %error
067600010606     C                   EVAL      PiNum=*off
067700010330     C                   ENDIF
067800010330     C*
067900010330     C                   ENDSR
068000010330     C***
068100010601
068200010601
068300010601
068400010601      /TITLE Invio dei dati al punto operativo.
068500010601     C     opeini        BEGSR
068600010601     C*
068700010601     C* Inizializzo flag e contatori operativi
068800010601     C                   movel     '0'           flgGiro           1
068900010615     C                   movel     '0'           flgOk             1
069000010601     C                   z-add     *zeros        cntNonEl         10 0
069100010601     C*
069200010601     C                   ENDSR
069300010601     C***
069400010601
069500050421
069600050421
069700050421     C     *pssr         BEGSR
069800050421     C*
069900050421     C                   eval      wrkesito = '2'
070000050421     C*
070100050421     C                   ENDSR     '*CANCL'
070200050421     C***
070300010601
070400010330
070500010330
070600000613     C     *inzsr        BEGSR
070700990910     C*
070800990910     C     *entry        plist
070900990920     C                   parm                    tivlrds
071000990921     C                   parm      wrkesito      esito
071100000724     C                   parm                    prmlit
071200000710     C                   parm                    prmfir
071300130731     C*
071400130731     C* KLIST
071500130731     c     K04TAS30      klist
071600130731     c                   kfld                    TASAas
071700130731     c                   kfld                    TASLnp
071800130731     c                   kfld                    TASNrs
071900130731     c                   kfld                    TASNsp
072000010330     C*
072100010330     C* CALCOLA LA DATA CORRENTE
072200130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072300130724     C                   eval      datcor = %dec(%date() : *ISO)
072400000613     C*
072500000613     C                   ENDSR
072600000613     C***
072700040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
072800130731X;X;X;X;X;X;EFRNFT;W_DFT;X;X;X;X;X;W_RIF;EFRRFP;EFRPES;EFRVOL;
072900130731X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;EFRIMP;X;X;X;X;X;X;X;X;X;X;
073000130528
