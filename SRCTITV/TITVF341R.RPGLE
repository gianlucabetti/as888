000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200180205      *
000300180205      * PARTICOLARITA':
000400180205      *
000500180205      * - QUESTO TRADUTTORE NON VUOLE L'INTESTAZIONE DI COLONNA
000600180205      *
000700130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800000313     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000130724     FTNEFR00F  O    E             DISK
001100130731     FTITAS30C  IF   E           K DISK    usropn
001200000313     D*
001300040726     D*------------
001400040726     D* SCHIEDE A PROGRAMMA
001500040726     D*------------
001600040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002800030715     D*------------------
002900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003000030715     D*------------------
003100030715     D SkSplitFLD      S             10    DIM(100)
003200030715     D SkSplitCSV      S            256    DIM(100)
003300030715     D CharCSV         S              1
003400030715     D CharTXT         S              1
003500030715     D CharNUM         S              1
003600130724     D posDa           S              4  0 INZ(*zeros)
003700130724     D posA            S              4  0 INZ(*zeros)
003800130724     D i               s              4  0 INZ(1)
003900030715     D wGiro           s              1  0 INZ(*zeros)
004000130731     D Data8A          s              8
004100130731     D w14A            s             14
004200130731     D result          s             50
004300130731     D wK              s              3s 0
004400130731     D wX              s              3s 0
004500130731     D wY              s              3s 0
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900130724     D posDaDft        S              4  0 INZ(*zeros)
005000130724     D posADft         S              4  0 INZ(*zeros)
005100130724     D j               s              4  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300110222
005400031201     D*------------------
005500031201     D* Costanti
005600031201     D*------------------
005700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005900130731     D cifre           c                   const('0123456789')
006000010330
006100010330
006200010330
006300990921     C                   reset                   esito
006400990921     C                   reset                   wrkesito
006500010601     C*
006600010601     C                   exsr      opeini
006700130724     C                   exsr      rwfile
006800010601     C*
006900010601     C                   seton                                        lr
007000010601
007100010601
007200010601
007300010601
007400010601     C*--------------------------------------------------------
007500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007600010601     C*--------------------------------------------------------
007700010601     C     PREELA        BEGSR
007800010601     C*
007900130724     C                   if        flgGiro = '0'
008000010601     C*
008100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008200010601     C                   eval      flgGiro = '1'
008300010601     C*
008400010601     C                   endif
008500010601     C*
008600010601     C                   ENDSR
008700010601     C***
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009300010601     C*--------------------------------------------------------
009400010601     C     ENDELA        BEGSR
009500000616     C*
009600010601     C                   ENDSR
009700010601     C***
009800010601
009900130724
010000010601
010100010330     C*--------------------------------------------------------
010200130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
010300010330     C*--------------------------------------------------------
010400130724     C     RWFILE        BEGSR
010500010330     C*
010600010330     C                   if        not %open(tivin00r)
010700010330     C                   open      tivin00r
010800010330     C                   endif
010900130731     C                   if        not %open(titas30c)
011000130731     C                   open      titas30c
011100130731     C                   endif
011200010330     C*
011300010604     C                   clear                   §CTROK
011400010604     C                   clear                   §CTRMO
011500010604     C                   clear                   §CTRNO
011600040726     C*
011700040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011800040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011900180205     C                   exsr      inzvar
012000180205     C                   exsr      impfile
012100010330     C*
012200030822     C                   do        *HIVAL
012300010330     C*
012400030822     C                   read      tivin00r                               70
012500010618     C*
012600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012700010618     C                   if        vinflg = '1'
012800010618     C                   eval      flgOk = '1'
012900010618     C                   endif
013000010618     C*
013100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
013200040510     C                   z-add     *zeros        wLenVINDTA        4 0
013300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013700040510     C                   z-add     1             wFlgVINDTA
013800040510     C                   leave
013900040510     C                   endif
014000040510     C                   enddo
014100040510     C*
014200040218     C                   if        vindta > *blanks AND
014300040510     C                             wFlgVINDTA = 1
014400010330     C*
014500010601     C                   if        *in70 = *off and
014600010330     C                             (vinflg = *blanks
014700010330     C                              or vinflg = '0'
014800010330     C                              or vinflg = '2')
014900010330     C*
015000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015100010711     C                   if        vinflg = *blanks or vinflg = '0'
015200010711     C                   clear                   vinmsg
015300010711     C                   endif
015400010601     C*
015500010330     C                   exsr      inzvar
015600130724     C                   exsr      impfile
015700010601     C*
015800010601     C                   exsr      PREELA
015900010601     C*
016000010604     C* Ebbene...
016100010604     C*
016200030822     C  N31              add       1             §CTROK            7 0
016300030822     C   32              add       1             §CTRMO            7 0
016400030822     C   31              add       1             §CTRNO            7 0
016500030822     C                   if        wGiro = 2
016600130731     C* scrivo se il network è <> LNA
016700130731     C* 'LNA' non è un network, indica che mi è stato detto di reperire il network dalla bolla,
016800130731     C* cioè da TITAS (quindi non sono scritti né gli ORM né tutto ciò che non contiene un
016900130731     C* riferimento BRT associabile a una bolla)
017000130731     C                   IF        EFRNTW <> 'LNA'
017100130724     C  N31              write     TNEFR000
017200130731     C                   ENDIF
017300130731     C                   endif
017400010604     C*
017500010604     C                   if        *in31 = *off and
017600010604     C                             *in32 = *off
017700010604     C                   eval      vinflg = '1'
017800010604     C                   else
017900010604     C                   eval      vinflg = '2'
018000010604     C                   endif
018100130724     C*
018200130724     C                   endif
018300010604     C*
018400010330     C                   else
018500010330     C                   eval      vinflg = '1'
018600010330     C                   endif
018700010601     C*
018800010601     C  N70              update    tivin000
018900010330     C*
019000030822     C  N70              enddo
019100010601     C
019200010601     C*
019300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019400130724     C                   if        cntNonEl = *zeros
019500010330     C* Se non ci sono record con errori ...
019600010601     C                   if        §ctrno = 0 and
019700130724     C                             §ctrmo = 0
019800010330     C* ... restituisco esito OK.
019900010330     C                   eval      wrkesito = '0'
020000010330     C                   else
020100010330     C                   if        §ctrok > 0
020200010330     C                   eval      wrkesito = '1'
020300010330     C                   else
020400010615     C                   if        flgOk = '0'
020500010615     C                   eval      wrkesito = '2'
020600010615     C                   else
020700010615     C                   eval      wrkesito = '6'
020800010615     C                   endif
020900010330     C                   endif
021000010330     C                   endif
021100010601     C                   else
021200010601     C                   eval      wrkesito = '9'
021300010601     C                   endif
021400010330     C*
021500010330     C                   if        %open(tivin00r)
021600010330     C                   close     tivin00r
021700010330     C                   endif
021800130731     C                   if        %open(titas30c)
021900130731     C                   close     titas30c
022000130731     C                   endif
022100010601     C*
022200010618     C                   if        flgGiro = '1'
022300010601     C                   exsr      endela
022400010618     C                   endif
022500010330     C*
022600010330     C                   ENDSR
022700010330     C***
022800010601
022900010601
023000010601
023100010330     C*----------------------------------------------------*
023200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023300010330     C*----------------------------------------------------*
023400010330     C     INZVAR        BEGSR
023500010330     C*
023600030822     C* Inizializzo variabili di wrk
023700130724     C
023800030822     C*
023900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
024000130724     C                   CLEAR                   TNEFR000
024100030822     C                   CLEAR                   SkSplitCSV
024200030822     C                   EVAL      i = 1
024300030822     C                   EVAL      posDa = *zeros
024400030822     C                   EVAL      posA  = *zeros
024500030822     C*
024600030822     C* Reimposto i valori di default
024700030822     C                   EXSR      DEFCAM
024800030822     C*
024900010330     C                   ENDSR
025000130724
025100130724
025200130724
025300010330     C*----------------------------------------------------*
025400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025500010330     C*----------------------------------------------------*
025600010330     C     DEFCAM        BEGSR
025700030822     C*
025800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025900030715     C* e delimitatore testo.
026000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
026100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
026200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
026300030822     C*
026400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026500130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
026600030715     C                   ENDIF
026700030822     C*
026800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026900030822     C                   EVAL      posDaDft = 1
027000030822     C                   EVAL      posADft  = 0
027100030822     C                   EVAL      wGiroDft = 0
027200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
027300030822     C                             posDaDft > 0
027400030822     C*
027500030822     C* Gestisco il 1° giro
027600030822     C                   IF        wGiroDft = 0
027700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
027800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027900030822     C* Incremento il contatore dei "giri"
028000030822     C                   EVAL      wGiroDft = 1
028100030822     C                   ELSE
028200030822     C                   EVAL      posDaDft = posADft
028300030822     C                   ENDIF
028400030822     C* Eseguo lo scan x trovare la fine del campo corrente
028500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028600030822     C*
028700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
028800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028900030822     C                   IF        posDaDft > 0 AND
029000030822     C                             posADft  > 0
029100130724     C* NTW
029200130724     C                   IF        %subst(
029300130724     C                             %subst(vlrppt:posDaDft+1:
029400130724     C                             posADft-posDaDft-1):1:3)
029500130724     C                             = 'NTW'
029600130731     C* se il valore del parametro è *LNA, lo memorizzo perché mi serve dopo (EFRNTW='LNA')
029700130731     C                   IF        %trim(%subst(
029800130724     C                             %subst(vlrppt:posDaDft+1:
029900130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030000130731     C                             = '*LNA'
030100130731     C                   EVAL      EFRNTW='LNA'
030200130731     C                   ELSE
030300130731     C                   EVAL      EFRNTW=%trim(%subst(
030400130731     C                             %subst(vlrppt:posDaDft+1:
030500130731     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030600130724     C                   ENDIF
030700130731     C                   ENDIF
030800130724     C* TPD
030900130724     C                   IF        %subst(
031000130724     C                             %subst(vlrppt:posDaDft+1:
031100130724     C                             posADft-posDaDft-1):1:3)
031200130724     C                             = 'TPD'
031300130724     C                   EVAL      EFRTPD=%trim(%subst(
031400130724     C                             %subst(vlrppt:posDaDft+1:
031500130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031600130724     C                   ENDIF
031700130724     C* CAU
031800130724     C                   IF        %subst(
031900130724     C                             %subst(vlrppt:posDaDft+1:
032000130724     C                             posADft-posDaDft-1):1:3)
032100130724     C                             = 'CAU'
032200130724     C                   EVAL      EFRCAU=%trim(%subst(
032300130724     C                             %subst(vlrppt:posDaDft+1:
032400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032500130724     C                   ENDIF
032600130724     C* GRP
032700130724     C                   IF        %subst(
032800130724     C                             %subst(vlrppt:posDaDft+1:
032900130724     C                             posADft-posDaDft-1):1:3)
033000130724     C                             = 'GRP'
033100130724     C                   EVAL      EFRGRP=%trim(%subst(
033200130724     C                             %subst(vlrppt:posDaDft+1:
033300130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033400130724     C                   ENDIF
033500030822     C* ...
033600030822     C                   ENDIF
033700030822     C                   ENDDO
033800020204     C*
033900010330     C                   ENDSR
034000130724
034100130724
034200130724
034300010607     C*----------------------------------------------------*
034400130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
034500010607     C*----------------------------------------------------*
034600130724     C     IMPFILE       BEGSR
034700010607     C*
034800130724     C                   SETOFF                                       3132
034900130724     C*
035000130724     C                   MOVEL     datcor        EFRDTR
035100010607     C*
035200030715     C**********
035300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
035400030715     C**********
035500030715     C                   IF        wGiro = *zeros
035600040726     C*
035700040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
035800040726     C                   EVAL      depvinDTA = vinDTA
035900040726     C*
036000040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
036100040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
036200040726     C                                      %trim(intcol(3))
036300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
036400031201     C     minu:maiu     XLATE     vindta        vindta
036500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
036600040324     C                   DOW       posDa <= %len(%trimr(vindta))
036700030715     C*
036800030715     C* Gestisco il 1° campo
036900030715     C                   IF        i = 1
037000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
037100030715     C                   EVAL      posDa = 1
037200030715     C* Eseguo lo scan x trovare la fine del primo campo
037300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037400030715     C* A questo punto "estraggo" il campo corrente
037500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037600030715     C                                                    (posA-posDa))
037700030715     C* X i campi successivi al 1°
037800030715     C                   ELSE
037900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
038000030715     C                   EVAL      posDa = posA + 1
038100030715     C* Eseguo lo scan x trovare la fine del campo corrente
038200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
038300030715     C* Gestisco l'ultimo campo
038400030715     C                   IF        posA = *zeros
038500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
038600030715     C* A questo punto "estraggo" il campo corrente
038700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038800030715     C                                                    (posA-posDa))
038900030715     C                   LEAVE
039000030715     C                   ELSE
039100030715     C* A questo punto "estraggo" il campo corrente
039200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039300030715     C                                                    (posA-posDa))
039400030715     C                   ENDIF
039500030715     C                   ENDIF
039600030715     C* Incremento il contatore d campo
039700030715     C                   EVAL      i = i +1
039800030715     C                   ENDDO
039900030715     C*
040000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
040100030715     C* delimitazione testo
040200030715     C                   EVAL      i = 1
040300030715     C                   DOW       i < %elem(SkSplitFLD)
040400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
040500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
040600030715     C                   EVAL      i = i + 1
040700030715     C                   ENDDO
040800030715     C                   EVAL      wGiro = 1
040900040726     C* Ripristino la 1° riga originale
041000040726     C                   EVAL      vinDTA = depvinDTA
041100030715     C                   ELSE
041200030715     C                   EVAL      wGiro = 2
041300031204     C**********
041400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
041500031204     C* d separatore campo
041600031204     C**********
041700040726     C                   EVAL      vindta = %trim(vindta)
041800040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
041900040726     C                   EVAL      vindta = %subst(vindta:2)
042000040726     C                   ENDDO
042100031204     C*
042200031204     C                   Z-ADD     *zeros        lunghInput        4 0
042300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
042400040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
042500040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
042600040726     C                   EVAL      lunghInput = %len(%trim(vindta))
042700040726     C                   ENDDO
042800030715     C**********
042900030715     C* Effettuo lo split del campo dati d input
043000030715     C**********
043100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
043200040324     C                   DOW       posDa <= %len(%trimr(vindta))
043300030715     C*
043400030715     C* Gestisco il 1° campo
043500030715     C                   IF        i = 1
043600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
043700030715     C                   EVAL      posDa = 1
043800030715     C* Eseguo lo scan x trovare la fine del primo campo
043900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044000030715     C* A questo punto "estraggo" il campo corrente
044100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
044200030715     C                                                    (posA-posDa))
044300030715     C* X i campi successivi al 1°
044400030715     C                   ELSE
044500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
044600030715     C                   EVAL      posDa = posA + 1
044700031204     C* Verifico che nn vi sia il campo nn valorizzato
044800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
044900031204     C* Se campo nn valorizzato skippo al prossimo
045000031204     C                   EVAL      SkSplitCSV(i) = *blanks
045100031204     C                   EVAL      posA  = posA + 1
045200031204     C                   ELSE
045300030715     C* Eseguo lo scan x trovare la fine del campo corrente
045400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
045500030715     C* Gestisco l'ultimo campo
045600030715     C                   IF        posA = *zeros
045700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
045800030715     C* A questo punto "estraggo" il campo corrente
045900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046000030715     C                                                    (posA-posDa))
046100030715     C                   LEAVE
046200030715     C                   ELSE
046300030715     C* A questo punto "estraggo" il campo corrente
046400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046500030715     C                                                    (posA-posDa))
046600030715     C                   ENDIF
046700031204     C                   ENDIF
046800030715     C                   ENDIF
046900030715     C* Incremento il contatore d campo
047000030715     C                   EVAL      i = i +1
047100030715     C                   ENDDO
047200030715     C*
047300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
047400030715     C* delimitazione testo
047500030715     C                   EVAL      i = 1
047600030715     C                   DOW       i < %elem(SkSplitCSV)
047700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
047800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
047900030715     C                   EVAL      i = i + 1
048000030715     C                   ENDDO
048100030715     C*
048200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
048300030715     C                   EVAL      i = 1
048400030715     C                   DOW       i < %elem(SkSplitFLD)
048500030715     C*
048600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
048700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
048800030715     C                             SkSplitCSV(i) <> *blanks
048900130724     C*** RIF
049000130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
049100130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
049200130724     C                   ENDIF
049300130724     C*** RFP
049400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
049500130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
049600130724     C                   ENDIF
049700130724     C*** NFT
049800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
049900130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
050000130724     C                   ENDIF
050100130724     C*
050200130724     C* Reperisco quindi i campi numerici...
050300130724     C*** DFT
050400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
050500130724     C                   EVAL      PiStr=SkSplitCSV(i)
050600130724     C                   EXSR      CHKNUM
050700130724     C                   IF        PiInt=*on
050800130724     C                   Z-ADD     PiVal         EFRDFT
050900130724     C                   ELSE
051000130724     C                   SETON                                        31
051100130724     C                   EVAL      EFRDFT = *zeros
051200130724     C                   EVAL      vinmsg = %trimr(vinmsg)
051300130724     C                             + ' ' + 'EFRDFT'
051400130724     C                   ENDIF
051500130724     C                   ENDIF
051600130724     C*** IMP
051700130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
051800130724     C                   EVAL      PiStr=SkSplitCSV(i)
051900130724     C                   EXSR      CHKNUM
052000130724     C                   IF        PiNum=*on
052100130724     C                   Z-ADD     PiVal         EFRIMP
052200130724     C                   ELSE
052300130724     C                   SETON                                        31
052400130724     C                   EVAL      EFRIMP = *zeros
052500130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052600130724     C                             + ' ' + 'EFRIMP'
052700130724     C                   ENDIF
052800130724     C                   ENDIF
052900130724     C*** PES
053000130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
053100130724     C                   EVAL      PiStr=SkSplitCSV(i)
053200130724     C                   EXSR      CHKNUM
053300130724     C                   IF        PiNum=*on
053400130724     C                   Z-ADD     PiVal         EFRPES
053500130724     C                   ELSE
053600130724     C                   SETON                                        31
053700130724     C                   EVAL      EFRPES = *zeros
053800130724     C                   EVAL      vinmsg = %trimr(vinmsg)
053900130724     C                             + ' ' + 'EFRPES'
054000130724     C                   ENDIF
054100130724     C                   ENDIF
054200130724     C*** VOL
054300130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
054400130724     C                   EVAL      PiStr=SkSplitCSV(i)
054500130724     C                   EXSR      CHKNUM
054600130724     C                   IF        PiNum=*on
054700130724     C                   Z-ADD     PiVal         EFRVOL
054800130724     C                   ELSE
054900130724     C                   SETON                                        31
055000130724     C                   EVAL      EFRVOL = *zeros
055100130724     C                   EVAL      vinmsg = %trimr(vinmsg)
055200130724     C                             + ' ' + 'EFRVOL'
055300130724     C                   ENDIF
055400130724     C                   ENDIF
055500110222     C*
055600110222     C***  ===>  Gestione campi "particolari" dentro ciclo
055700130731     C*
055800130731      *** W_DFT: la data è in formato gg.mm.aaaa alfanum. e va trasformata in 8 S
055900130731     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
056000130731     C* il partner può mandare la data con l'anno di 2 o di 4, bisogna gestirla
056100130731     C                   IF        %len(%trim(SkSplitCSV(i)))=10
056200130731     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
056300130731     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
056400130731     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
056500130731     C                   MOVEL     Data8A        EFRDFT
056600130731     C                   ELSE
056700130731     C* se la data non fosse né di 6 né di 8, valorizzo EFTDFT come se mi arrivasse di 6
056800130731     C* così se il valore è impossibile va in errore e ce ne accorgiamo
056900130731     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
057000130731     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
057100130731     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
057200130731     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
057300130731     C                   MOVEL     Data8A        EFRDFT
057400130731     C                   ENDIF
057500130731     C                   ENDIF
057600130731
057700130731     C*
057800130731      *** W_RFT: - se il valore indicato è un numerico lungo 14 e se il valore del parametro in
057900130731     C*          ingresso 'NTW' è = '*LNA => scomponendo i 14 bytes in:
058000130731     C*            YY (portare da 13 a 2013) => TASAAS
058100130731     C*            LNP     => TASLNP
058200130731     C*            NRS     => TASNRS
058300130731     C*            NSP     => TASNSP
058400130731     C*          chainare il TITAS30C e reperire TASLNA da utilizzare nella valorizzazione del campo
058500130731     C*          EFRNTW
058600130731     C*          - diversamente normalizzare eliminando tutti i caratteri non numerici, quindi
058700130731     C*          compattare il dato e tenere solamente i primi 10 bytes (occhio che potrebbero
058800130731     C*          esserci dei casi in cui il dato rimanente è lungo meno di 10 bytes) quindi
058900130731     C*          valorizzazione il campo EFRRIF preceduto da 'ORM-'
059000130731     C*          - diversamente o se vuoto considerare errore in importazione dati
0591001307311    C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF'
059200130731     C                   EVAL      w14A = *blank
059300130731     C* controllo che sia lungo 14
0594001307312    C                   IF        %len(%trim(SkSplitCSV(i)))=14
059500130731     C                             and EFRNTW = 'LNA'
059600130731     C                   EVAL      w14A = %trim(SkSplitCSV(i))
059700130731     C* controllo che sia numerico
059800130731     C                   EVAL      wX = %check(cifre : w14A)
0599001307313    C                   IF        wX = 0
060000130731     C                   EVAL      TASAAS = 2000 + %dec(%subst(w14A:1:2) :4:0)
060100130731     C                   EVAL      TASLNP = %dec(%subst(w14A:3:3) :3:0)
060200130731     C                   EVAL      TASNRS = %dec(%subst(w14A:6:2) :2:0)
060300130731     C                   EVAL      TASNSP = %dec(%subst(w14A:8:7) :7:0)
060400130731     C                   EVAL      EFRRIF = w14A
060500130731     C     K04TAS30      CHAIN     TITAS30C
0606001307314    C                   IF        %found
060700130731     C                   EVAL      EFRNTW = %editc(TASLNA:'X')
060800130731     C* se non trovo il rcd su TITAS do un warning
0609001307314e   C                   ELSE
061000130731     C                   SETON                                        32
061100130731     C                   EVAL      EFRNTW = *blank
061200130731     C                   EVAL      vinmsg = %trimr(vinmsg)
061300130731     C                             + ' ' + 'EFRNTW EFRRIF'
0614001307314-   C                   ENDIF
0615001307313e   C                   ELSE
061600130731     C                   EVAL      w14A = *blank
0617001307313-   C                   ENDIF
0618001307312-   C                   ENDIF
061900130731     C* se il dato in ingresso non era lungo 14 char o non era numerico
062000130731     C* o il network nel parametro non è *LNA
0621001307312    C                   IF        w14A = *blank
062200130731     C* tolgo i caratteri non numerici e compatto e tengo solo i primi 10 char
062300130731     C                   EVAL      wX = 1
062400130731     C                   EVAL      result = %trim(SkSplitCSV(i))
062500130731       // salvo il risultato perché la variabile result non è fissa e non mi fido
062600130731     C                   EVAL      wY = %checkR(' ' : result)
062700130731     C* trovo l'ultimo carattere non blank
062800130731     C                   FOR       wK = 1 TO wY
062900130731     C                   EVAL      wX = %check(cifre : result  : wX)
063000130731     C                   if        wX > 0
063100130731     C                   EVAL      result = %replace( '' : result : wX : 1)
063200130731     C                   ELSE
063300130731     C                   LEAVE
063400130731     C                   ENDIF
063500130731     C                   ENDFOR
063600130731     C* se il risultato del compattamento è blank, do un warning
063700130731     C                   IF        result = *blank
063800130731     C                   SETON                                        32
063900130731     C                   EVAL      EFRRIF = *blank
064000130731     C                   EVAL      vinmsg = %trimr(vinmsg)
064100130731     C                             + ' ' + 'EFRRIF'
064200130731     C                   ELSE
064300130731     C                   EVAL      EFRRIF = 'ORM-' + %subst(result : 1 : 10)
064400130731     C                   ENDIF
0645001307312-   C                   ENDIF
0646001307311-   C                   ENDIF
064700130731     C*
064800110222     C***  <===  -----------------------------------------
064900030715     C*
065000030715     C                   ENDIF
065100030715     C                   EVAL      i = i + 1
065200030715     C                   ENDDO
065300130724     C*
065400130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
065500130724     C
065600130724     C***  <===  -----------------------------------------
065700020204     C*
065800030715     C                   ENDIF
065900020204     C*
066000010607     C                   ENDSR
066100010607     C*----------------------------------------------------*
066200040714
066300010330
066400010330
066500010330     C*----------------------------------------------------*
066600010330     C*  CONTROLLO NUMERICITA' CAMPI
066700010330     C*----------------------------------------------------*
066800010330     C     CHKNUM        BEGSR
066900010330     C*
067000010606     C                   IF        PiDecChr = *blanks
067100030715     C                   EVAL      PiDecChr = CharNUM
067200010606     C                   ENDIF
067300010606     C*
067400010606     C                   CALL(e)   'ISNUMERIC'
067500010330     C                   PARM                    PiStr            30
067600010606     C                   PARM                    PiDecChr          1
067700010330     C                   PARM      *ZEROS        PiVal            30 9
067800010330     C                   PARM      '0'           PiInt             1
067900010330     C                   PARM      '0'           PiNum             1
068000010330     C                   IF        %error
068100010606     C                   EVAL      PiNum=*off
068200010330     C                   ENDIF
068300010330     C*
068400010330     C                   ENDSR
068500010330     C***
068600010601
068700010601
068800010601
068900010601      /TITLE Invio dei dati al punto operativo.
069000010601     C     opeini        BEGSR
069100010601     C*
069200010601     C* Inizializzo flag e contatori operativi
069300010601     C                   movel     '0'           flgGiro           1
069400010615     C                   movel     '0'           flgOk             1
069500010601     C                   z-add     *zeros        cntNonEl         10 0
069600010601     C*
069700010601     C                   ENDSR
069800010601     C***
069900010601
070000050421
070100050421
070200050421     C     *pssr         BEGSR
070300050421     C*
070400050421     C                   eval      wrkesito = '2'
070500050421     C*
070600050421     C                   ENDSR     '*CANCL'
070700050421     C***
070800010601
070900010330
071000010330
071100000613     C     *inzsr        BEGSR
071200990910     C*
071300990910     C     *entry        plist
071400990920     C                   parm                    tivlrds
071500990921     C                   parm      wrkesito      esito
071600000724     C                   parm                    prmlit
071700000710     C                   parm                    prmfir
071800130731     C*
071900130731     C* KLIST
072000130731     c     K04TAS30      klist
072100130731     c                   kfld                    TASAas
072200130731     c                   kfld                    TASLnp
072300130731     c                   kfld                    TASNrs
072400130731     c                   kfld                    TASNsp
072500010330     C*
072600010330     C* CALCOLA LA DATA CORRENTE
072700130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072800130724     C                   eval      datcor = %dec(%date() : *ISO)
072900000613     C*
073000000613     C                   ENDSR
073100000613     C***
073200040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
073300180205X;X;EFRNFT;W_DFT;X;X;X;X;X;X;X;X;X;X;EFRRFP;X;X;X;X;X;
073400180202X;X;X;X;W_RIF;X;X;X;X;X;EFRPES;X;EFRIMP;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
073500130528
