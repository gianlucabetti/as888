000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200131015     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
003600131015     D wEFRDFT         s                   like(EFRDFT)
003700130730     D wEFRPES1        s                   like(EFRPES)
003800130730     D wEFRPES2        s                   like(EFRPES)
003900130729     D result          s             50
004000130729     D wK              s              3s 0
004100130729     D wX              s              3s 0
004200130729     D wY              s              3s 0
004300030822     D*------------------
004400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004500030822     D*------------------
004600130724     D posDaDft        S              4  0 INZ(*zeros)
004700130724     D posADft         S              4  0 INZ(*zeros)
004800130724     D j               s              4  0 INZ(1)
004900030822     D wGiroDft        s              1  0 INZ(*zeros)
005000110222
005100031201     D*------------------
005200031201     D* Costanti
005300031201     D*------------------
005400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005600130729     D cifre           c                   const('0123456789')
005700010330
005800010330
005900010330
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200010601     C*
006300010601     C                   exsr      opeini
006400130724     C                   exsr      rwfile
006500010601     C*
006600010601     C                   seton                                        lr
006700010601
006800010601
006900010601
007000010601
007100010601     C*--------------------------------------------------------
007200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007300010601     C*--------------------------------------------------------
007400010601     C     PREELA        BEGSR
007500010601     C*
007600130724     C                   if        flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000010601     C*
008100010601     C                   endif
008200010601     C*
008300010601     C                   ENDSR
008400010601     C***
008500010601
008600010601
008700010601
008800010601     C*--------------------------------------------------------
008900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009000010601     C*--------------------------------------------------------
009100010601     C     ENDELA        BEGSR
009200000616     C*
009300010601     C                   ENDSR
009400010601     C***
009500010601
009600130724
009700010601
009800010330     C*--------------------------------------------------------
009900130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
010000010330     C*--------------------------------------------------------
010100130724     C     RWFILE        BEGSR
010200010330     C*
010300010330     C                   if        not %open(tivin00r)
010400010330     C                   open      tivin00r
010500010330     C                   endif
010600010330     C*
010700010604     C                   clear                   §CTROK
010800010604     C                   clear                   §CTRMO
010900010604     C                   clear                   §CTRNO
011000130729     C                   EVAL      wEFRNFT=*blank
011100131015     C                   EVAL      wEFRDFT=0
011200040726     C*
011300040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011400040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011500130724 xxx C***                exsr      inzvar
011600130724     C***                exsr      impvat
011700010330     C*
011800030822     C                   do        *HIVAL
011900010330     C*
012000030822     C                   read      tivin00r                               70
012100010618     C*
012200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012300010618     C                   if        vinflg = '1'
012400010618     C                   eval      flgOk = '1'
012500010618     C                   endif
012600010618     C*
012700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012800040510     C                   z-add     *zeros        wLenVINDTA        4 0
012900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013300040510     C                   z-add     1             wFlgVINDTA
013400040510     C                   leave
013500040510     C                   endif
013600040510     C                   enddo
013700040510     C*
013800040218     C                   if        vindta > *blanks AND
013900040510     C                             wFlgVINDTA = 1
014000010330     C*
014100010601     C                   if        *in70 = *off and
014200010330     C                             (vinflg = *blanks
014300010330     C                              or vinflg = '0'
014400010330     C                              or vinflg = '2')
014500010330     C*
014600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014700010711     C                   if        vinflg = *blanks or vinflg = '0'
014800010711     C                   clear                   vinmsg
014900010711     C                   endif
015000130729     C*
015100130729     C* siccome sappiamo che la prima colonna sarà la maggior parte delle volte vuota,
015200130729     C* mettiamo un carattere non vuoto per evitare che vengano elise le colonne vuote
015300130729     C                   eval      vindta  = '*' + %trim(vindta)
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
016600130729     C*
016700130729     C* se il riferimento originale è blank (riga di continuazione), salto la riga
016800130730     C                   IF        EFRRFP <> *blank
016900130724     C  N31              write     TNEFR000
017000130729     C                   ENDIF
017100130729     C*
017200030822     C                   endif
017300010604     C*
017400010604     C                   if        *in31 = *off and
017500010604     C                             *in32 = *off
017600010604     C                   eval      vinflg = '1'
017700010604     C                   else
017800010604     C                   eval      vinflg = '2'
017900010604     C                   endif
018000130724     C*
018100130724     C                   endif
018200010604     C*
018300010330     C                   else
018400010330     C                   eval      vinflg = '1'
018500010330     C                   endif
018600010601     C*
018700010601     C  N70              update    tivin000
018800010330     C*
018900030822     C  N70              enddo
019000010601     C
019100010601     C*
019200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019300130724     C                   if        cntNonEl = *zeros
019400010330     C* Se non ci sono record con errori ...
019500010601     C                   if        §ctrno = 0 and
019600130724     C                             §ctrmo = 0
019700010330     C* ... restituisco esito OK.
019800010330     C                   eval      wrkesito = '0'
019900010330     C                   else
020000010330     C                   if        §ctrok > 0
020100010330     C                   eval      wrkesito = '1'
020200010330     C                   else
020300010615     C                   if        flgOk = '0'
020400010615     C                   eval      wrkesito = '2'
020500010615     C                   else
020600010615     C                   eval      wrkesito = '6'
020700010615     C                   endif
020800010330     C                   endif
020900010330     C                   endif
021000010601     C                   else
021100010601     C                   eval      wrkesito = '9'
021200010601     C                   endif
021300010330     C*
021400010330     C                   if        %open(tivin00r)
021500010330     C                   close     tivin00r
021600010330     C                   endif
021700010601     C*
021800010618     C                   if        flgGiro = '1'
021900010601     C                   exsr      endela
022000010618     C                   endif
022100010330     C*
022200010330     C                   ENDSR
022300010330     C***
022400010601
022500010601
022600010601
022700010330     C*----------------------------------------------------*
022800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022900010330     C*----------------------------------------------------*
023000010330     C     INZVAR        BEGSR
023100010330     C*
023200030822     C* Inizializzo variabili di wrk
023300130724     C
023400030822     C*
023500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023600130724     C                   CLEAR                   TNEFR000
023700030822     C                   CLEAR                   SkSplitCSV
023800030822     C                   EVAL      i = 1
023900030822     C                   EVAL      posDa = *zeros
024000030822     C                   EVAL      posA  = *zeros
024100130730     C                   EVAL      wEFRPES1=0
024200130730     C                   EVAL      wEFRPES2=0
024300030822     C*
024400030822     C* Reimposto i valori di default
024500030822     C                   EXSR      DEFCAM
024600030822     C*
024700010330     C                   ENDSR
024800130724
024900130724
025000130724
025100010330     C*----------------------------------------------------*
025200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025300010330     C*----------------------------------------------------*
025400010330     C     DEFCAM        BEGSR
025500030822     C*
025600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025700030715     C* e delimitatore testo.
025800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
025900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
026000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
026100030822     C*
026200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026300130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
026400030715     C                   ENDIF
026500030822     C*
026600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026700030822     C                   EVAL      posDaDft = 1
026800030822     C                   EVAL      posADft  = 0
026900030822     C                   EVAL      wGiroDft = 0
027000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
027100030822     C                             posDaDft > 0
027200030822     C*
027300030822     C* Gestisco il 1° giro
027400030822     C                   IF        wGiroDft = 0
027500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
027600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027700030822     C* Incremento il contatore dei "giri"
027800030822     C                   EVAL      wGiroDft = 1
027900030822     C                   ELSE
028000030822     C                   EVAL      posDaDft = posADft
028100030822     C                   ENDIF
028200030822     C* Eseguo lo scan x trovare la fine del campo corrente
028300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028400030822     C*
028500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
028600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028700030822     C                   IF        posDaDft > 0 AND
028800030822     C                             posADft  > 0
028900130724     C* NTW
029000130724     C                   IF        %subst(
029100130724     C                             %subst(vlrppt:posDaDft+1:
029200130724     C                             posADft-posDaDft-1):1:3)
029300130724     C                             = 'NTW'
029400130724     C                   EVAL      EFRNTW=%trim(%subst(
029500130724     C                             %subst(vlrppt:posDaDft+1:
029600130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029700130724     C                   ENDIF
029800130724     C* TPD
029900130724     C                   IF        %subst(
030000130724     C                             %subst(vlrppt:posDaDft+1:
030100130724     C                             posADft-posDaDft-1):1:3)
030200130724     C                             = 'TPD'
030300130724     C                   EVAL      EFRTPD=%trim(%subst(
030400130724     C                             %subst(vlrppt:posDaDft+1:
030500130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030600130724     C                   ENDIF
030700130724     C* CAU
030800130724     C                   IF        %subst(
030900130724     C                             %subst(vlrppt:posDaDft+1:
031000130724     C                             posADft-posDaDft-1):1:3)
031100130724     C                             = 'CAU'
031200130724     C                   EVAL      EFRCAU=%trim(%subst(
031300130724     C                             %subst(vlrppt:posDaDft+1:
031400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031500130724     C                   ENDIF
031600130724     C* GRP
031700130724     C                   IF        %subst(
031800130724     C                             %subst(vlrppt:posDaDft+1:
031900130724     C                             posADft-posDaDft-1):1:3)
032000130724     C                             = 'GRP'
032100130724     C                   EVAL      EFRGRP=%trim(%subst(
032200130724     C                             %subst(vlrppt:posDaDft+1:
032300130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032400130724     C                   ENDIF
032500030822     C* ...
032600030822     C                   ENDIF
032700030822     C                   ENDDO
032800020204     C*
032900010330     C                   ENDSR
033000130724
033100130724
033200130724
033300010607     C*----------------------------------------------------*
033400130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
033500010607     C*----------------------------------------------------*
033600130724     C     IMPFILE       BEGSR
033700010607     C*
033800130724     C                   SETOFF                                       3132
033900130724     C*
034000130724     C                   MOVEL     datcor        EFRDTR
034100010607     C*
034200030715     C**********
034300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
034400030715     C**********
034500030715     C                   IF        wGiro = *zeros
034600040726     C*
034700040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
034800040726     C                   EVAL      depvinDTA = vinDTA
034900040726     C*
035000040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
035100040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
035200040726     C                                      %trim(intcol(3))
035300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
035400031201     C     minu:maiu     XLATE     vindta        vindta
035500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
035600040324     C                   DOW       posDa <= %len(%trimr(vindta))
035700030715     C*
035800030715     C* Gestisco il 1° campo
035900030715     C                   IF        i = 1
036000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
036100030715     C                   EVAL      posDa = 1
036200030715     C* Eseguo lo scan x trovare la fine del primo campo
036300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
036400030715     C* A questo punto "estraggo" il campo corrente
036500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
036600030715     C                                                    (posA-posDa))
036700030715     C* X i campi successivi al 1°
036800030715     C                   ELSE
036900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037000030715     C                   EVAL      posDa = posA + 1
037100030715     C* Eseguo lo scan x trovare la fine del campo corrente
037200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037300030715     C* Gestisco l'ultimo campo
037400030715     C                   IF        posA = *zeros
037500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
037600030715     C* A questo punto "estraggo" il campo corrente
037700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037800030715     C                                                    (posA-posDa))
037900030715     C                   LEAVE
038000030715     C                   ELSE
038100030715     C* A questo punto "estraggo" il campo corrente
038200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038300030715     C                                                    (posA-posDa))
038400030715     C                   ENDIF
038500030715     C                   ENDIF
038600030715     C* Incremento il contatore d campo
038700030715     C                   EVAL      i = i +1
038800030715     C                   ENDDO
038900030715     C*
039000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
039100030715     C* delimitazione testo
039200030715     C                   EVAL      i = 1
039300030715     C                   DOW       i < %elem(SkSplitFLD)
039400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
039500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
039600030715     C                   EVAL      i = i + 1
039700030715     C                   ENDDO
039800030715     C                   EVAL      wGiro = 1
039900040726     C* Ripristino la 1° riga originale
040000040726     C                   EVAL      vinDTA = depvinDTA
040100030715     C                   ELSE
040200030715     C                   EVAL      wGiro = 2
040300031204     C**********
040400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
040500031204     C* d separatore campo
040600031204     C**********
040700040726     C                   EVAL      vindta = %trim(vindta)
040800040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
040900040726     C                   EVAL      vindta = %subst(vindta:2)
041000040726     C                   ENDDO
041100031204     C*
041200031204     C                   Z-ADD     *zeros        lunghInput        4 0
041300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
041400040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
041500040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
041600040726     C                   EVAL      lunghInput = %len(%trim(vindta))
041700040726     C                   ENDDO
041800030715     C**********
041900030715     C* Effettuo lo split del campo dati d input
042000030715     C**********
042100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
042200040324     C                   DOW       posDa <= %len(%trimr(vindta))
042300030715     C*
042400030715     C* Gestisco il 1° campo
042500030715     C                   IF        i = 1
042600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
042700030715     C                   EVAL      posDa = 1
042800030715     C* Eseguo lo scan x trovare la fine del primo campo
042900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043000030715     C* A questo punto "estraggo" il campo corrente
043100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043200030715     C                                                    (posA-posDa))
043300030715     C* X i campi successivi al 1°
043400030715     C                   ELSE
043500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
043600030715     C                   EVAL      posDa = posA + 1
043700031204     C* Verifico che nn vi sia il campo nn valorizzato
043800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
043900031204     C* Se campo nn valorizzato skippo al prossimo
044000031204     C                   EVAL      SkSplitCSV(i) = *blanks
044100031204     C                   EVAL      posA  = posA + 1
044200031204     C                   ELSE
044300030715     C* Eseguo lo scan x trovare la fine del campo corrente
044400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044500030715     C* Gestisco l'ultimo campo
044600030715     C                   IF        posA = *zeros
044700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
044800030715     C* A questo punto "estraggo" il campo corrente
044900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045000030715     C                                                    (posA-posDa))
045100030715     C                   LEAVE
045200030715     C                   ELSE
045300030715     C* A questo punto "estraggo" il campo corrente
045400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045500030715     C                                                    (posA-posDa))
045600030715     C                   ENDIF
045700031204     C                   ENDIF
045800030715     C                   ENDIF
045900030715     C* Incremento il contatore d campo
046000030715     C                   EVAL      i = i +1
046100030715     C                   ENDDO
046200030715     C*
046300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
046400030715     C* delimitazione testo
046500030715     C                   EVAL      i = 1
046600030715     C                   DOW       i < %elem(SkSplitCSV)
046700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
046800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
046900030715     C                   EVAL      i = i + 1
047000030715     C                   ENDDO
047100030715     C*
047200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
047300030715     C                   EVAL      i = 1
047400030715     C                   DOW       i < %elem(SkSplitFLD)
047500030715     C*
047600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
047700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
047800030715     C                             SkSplitCSV(i) <> *blanks
047900130724     C*** RIF
048000130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
048100130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
048200130724     C                   ENDIF
048300130724     C*** RFP
048400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
048500130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
048600130724     C                   ENDIF
048700130724     C*** NFT
048800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
048900130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
049000130724     C                   ENDIF
049100130724     C*
049200130724     C* Reperisco quindi i campi numerici...
049300130724     C*** DFT
049400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
049500130724     C                   EVAL      PiStr=SkSplitCSV(i)
049600130724     C                   EXSR      CHKNUM
049700130724     C                   IF        PiInt=*on
049800130724     C                   Z-ADD     PiVal         EFRDFT
049900130724     C                   ELSE
050000130724     C                   SETON                                        31
050100130724     C                   EVAL      EFRDFT = *zeros
050200130724     C                   EVAL      vinmsg = %trimr(vinmsg)
050300130724     C                             + ' ' + 'EFRDFT'
050400130724     C                   ENDIF
050500130724     C                   ENDIF
050600130724     C*** IMP
050700130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
050800130724     C                   EVAL      PiStr=SkSplitCSV(i)
050900130724     C                   EXSR      CHKNUM
051000130724     C                   IF        PiNum=*on
051100130724     C                   Z-ADD     PiVal         EFRIMP
051200130724     C                   ELSE
051300130724     C                   SETON                                        31
051400130724     C                   EVAL      EFRIMP = *zeros
051500130724     C                   EVAL      vinmsg = %trimr(vinmsg)
051600130724     C                             + ' ' + 'EFRIMP'
051700130724     C                   ENDIF
051800130724     C                   ENDIF
051900130724     C*** PES
052000130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
052100130724     C                   EVAL      PiStr=SkSplitCSV(i)
052200130724     C                   EXSR      CHKNUM
052300130724     C                   IF        PiNum=*on
052400130724     C                   Z-ADD     PiVal         EFRPES
052500130724     C                   ELSE
052600130724     C                   SETON                                        31
052700130724     C                   EVAL      EFRPES = *zeros
052800130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052900130724     C                             + ' ' + 'EFRPES'
053000130724     C                   ENDIF
053100130724     C                   ENDIF
053200130724     C*** VOL
053300130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
053400130724     C                   EVAL      PiStr=SkSplitCSV(i)
053500130724     C                   EXSR      CHKNUM
053600130724     C                   IF        PiNum=*on
053700130724     C                   Z-ADD     PiVal         EFRVOL
053800130724     C                   ELSE
053900130724     C                   SETON                                        31
054000130724     C                   EVAL      EFRVOL = *zeros
054100130724     C                   EVAL      vinmsg = %trimr(vinmsg)
054200130724     C                             + ' ' + 'EFRVOL'
054300130724     C                   ENDIF
054400130724     C                   ENDIF
054500110222     C*
054600130729     C***  ===>  Gestione campi "particolari" dentro ciclo
054700130729
054800110222     C*
054900131015      *** W_DFT: c'è solo nella prima riga della fattura
055000131015     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
055100131015     C                             and SkSplitCSV(i)<>*blank
055200131015     C                   EVAL      PiStr=SkSplitCSV(i)
055300131015     C                   EXSR      CHKNUM
055400131015     C                   IF        PiInt=*on
055500131015     C                   Z-ADD     PiVal         wEFRDFT
055600131015     C                   ELSE
055700131015     C                   EVAL      wEFRDFT = *zeros
055800131015     C* se il valore non è un intero, do un warning
055900131015     C                   EVAL      vinmsg = %trimr(vinmsg)
056000131015     C                             + ' ' + 'EFRDFT'
056100131015     C                   ENDIF
056200130729     C                   ENDIF
056300131015
056400131015     C*
056500131015      *** W_NFT: c'è solo nella prima riga della fattura
056600131015     C                   IF        %trim(SkSplitFLD(i)) = 'W_NFT'
056700131015     C* siccome metto * nel 1° carattere che fa parte del nr.fattura, non posso testare blank
056800131015     C                             and SkSplitCSV(i)<>'*'
056900131015     C                   EVAL      wEFRNFT=%trim(%subst(SkSplitCSV(i) : 2 : 15))
057000131015     C                   ENDIF
057100130729
057200130729     C*
057300130729      *** W_RIF: normalizzare eliminando tutti i caratteri non numerici, quindi compattare il dato e
057400130729     C*          tenere solamente i primi 14 bytes (occhio che ci sono dei casi in cui il dato
057500130729     C*          rimanente è lungo meno di 14 bytes)
057600130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF'
057700130729     C                   EVAL      wX = 1
057800130729     C                   EVAL      result = %trim(SkSplitCSV(i))
057900130729       // salvo il risultato perché la variabile result non è fissa e non mi fido
058000130729     C                   EVAL      wY = %checkR(' ' : result)
058100130729     C* trovo l'ultimo carattere non blank
058200130729     C                   FOR       wK = 1 TO wY
058300130729     C                   EVAL      wX = %check(cifre : result  : wX)
058400130729     C                   if        wX > 0
058500130729     C                   EVAL      result = %replace( '' : result : wX : 1)
058600130729     C                   ELSE
058700130729     C                   LEAVE
058800130729     C                   ENDIF
058900130729     C                   ENDFOR
059000130801     C* se il risultato del compattamento è blank, do un warning
059100130801     C                   IF        result = *blank
059200130801     C                   SETON                                        32
059300130801     C                   EVAL      EFRRIF = *blank
059400130801     C                   EVAL      vinmsg = %trimr(vinmsg)
059500130801     C                             + ' ' + 'EFRRIF'
059600130801     C                   ELSE
059700130729     C                   EVAL      EFRRIF = %subst(result : 1 : 14)
059800130801     C                   ENDIF
059900130729     C                   ENDIF
060000130730
060100130730     C*** W_PES1 e 2
060200130730     C                   IF        %trim(SkSplitFLD(i)) = 'W_PES1'
060300130730     C                   EVAL      PiStr=SkSplitCSV(i)
060400130730     C                   EXSR      CHKNUM
060500130730     C                   IF        PiNum=*on
060600130730     C                   Z-ADD     PiVal         wEFRPES1
060700130730     C                   ELSE
060800130730     C                   EVAL      wEFRPES1 = *zeros
060900130730     C                   ENDIF
061000130730     C                   ENDIF
061100130730     C                   IF        %trim(SkSplitFLD(i)) = 'W_PES2'
061200130730     C                   EVAL      PiStr=SkSplitCSV(i)
061300130730     C                   EXSR      CHKNUM
061400130730     C                   IF        PiNum=*on
061500130730     C                   Z-ADD     PiVal         wEFRPES2
061600130730     C                   ELSE
061700130730     C                   EVAL      wEFRPES2 = *zeros
061800130730     C                   ENDIF
061900130730     C                   ENDIF
062000130729
062100130729     C*
062200130729      *** W_RFP: escludere i primi 3 caratteri fissi 'SAL'
062300130729     C                   IF        %trim(SkSplitFLD(i)) = 'W_RFP'
062400130729     C                   EVAL      EFRRFP = %subst(SkSplitCSV(i) : 4 : 35)
062500130729     C                   ENDIF
062600130729
062700110222     C***  <===  -----------------------------------------
062800030715     C*
062900030715     C                   ENDIF
063000030715     C                   EVAL      i = i + 1
063100030715     C                   ENDDO
063200130724     C*
063300130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
063400130729
063500131015     C                   EVAL      EFRDFT=wEFRDFT
063600131015
063700131015     C                   EVAL      EFRNFT=wEFRNFT
063800130730
063900130730     C                   EVAL      EFRPES=wEFRPES1
064000130730     C                   IF        EFRPES = 0
064100130730     C                   EVAL      EFRPES=wEFRPES2
064200130730     C                   ENDIF
064300130724     C***  <===  -----------------------------------------
064400020204     C*
064500030715     C                   ENDIF
064600020204     C*
064700010607     C                   ENDSR
064800010607     C*----------------------------------------------------*
064900040714
065000010330
065100010330
065200010330     C*----------------------------------------------------*
065300010330     C*  CONTROLLO NUMERICITA' CAMPI
065400010330     C*----------------------------------------------------*
065500010330     C     CHKNUM        BEGSR
065600010330     C*
065700010606     C                   IF        PiDecChr = *blanks
065800030715     C                   EVAL      PiDecChr = CharNUM
065900010606     C                   ENDIF
066000010606     C*
066100010606     C                   CALL(e)   'ISNUMERIC'
066200010330     C                   PARM                    PiStr            30
066300010606     C                   PARM                    PiDecChr          1
066400010330     C                   PARM      *ZEROS        PiVal            30 9
066500010330     C                   PARM      '0'           PiInt             1
066600010330     C                   PARM      '0'           PiNum             1
066700010330     C                   IF        %error
066800010606     C                   EVAL      PiNum=*off
066900010330     C                   ENDIF
067000010330     C*
067100010330     C                   ENDSR
067200010330     C***
067300010601
067400010601
067500010601
067600010601      /TITLE Invio dei dati al punto operativo.
067700010601     C     opeini        BEGSR
067800010601     C*
067900010601     C* Inizializzo flag e contatori operativi
068000010601     C                   movel     '0'           flgGiro           1
068100010615     C                   movel     '0'           flgOk             1
068200010601     C                   z-add     *zeros        cntNonEl         10 0
068300010601     C*
068400010601     C                   ENDSR
068500010601     C***
068600010601
068700050421
068800050421
068900050421     C     *pssr         BEGSR
069000050421     C*
069100050421     C                   eval      wrkesito = '2'
069200050421     C*
069300050421     C                   ENDSR     '*CANCL'
069400050421     C***
069500010601
069600010330
069700010330
069800000613     C     *inzsr        BEGSR
069900990910     C*
070000990910     C     *entry        plist
070100990920     C                   parm                    tivlrds
070200990921     C                   parm      wrkesito      esito
070300000724     C                   parm                    prmlit
070400000710     C                   parm                    prmfir
070500010330     C*
070600010330     C* CALCOLA LA DATA CORRENTE
070700130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
070800130724     C                   eval      datcor = %dec(%date() : *ISO)
070900000613     C*
071000000613     C                   ENDSR
071100000613     C***
071200040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
071300131015W_NFT;X;W_DFT;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;W_RFP;W_RIF;X;X;X;W_PES2;EFRVOL;
071400130730X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;EFRIMP;X;X;X;X;X;X;X;X;W_PES1;X;X;X;
071500130528
