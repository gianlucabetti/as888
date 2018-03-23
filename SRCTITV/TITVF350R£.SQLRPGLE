000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500130724     FTNEFR00F  O    E             DISK
000600130806     FTITAT24C  IF   E           K DISK    usropn
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
004100130807     D wNrRcdLetti     s              3s 0
004200130806     D wAnno           s              4s 0
004300130807     D wSQL            s           5000
004400130807     D savAAS          s                   like(TATAAS)
004500130807     D savLNP          s                   like(TATLNP)
004600130807     D savNRS          s                   like(TATNRS)
004700130807     D savNSP          s                   like(TATNSP)
004800130806     D*------------------
004900130806     D* variabili per l'SQL
005000130806     D*------------------
005100130806     D resultSQL       ds                  occurs(10)
005200130806     D  AAS                           4s 0
005300130806     D  LNP                           3s 0
005400130806     D  NRS                           2s 0
005500130806     D  NSP                           7s 0
005600030822     D*------------------
005700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005800030822     D*------------------
005900130724     D posDaDft        S              4  0 INZ(*zeros)
006000130724     D posADft         S              4  0 INZ(*zeros)
006100130724     D j               s              4  0 INZ(1)
006200030822     D wGiroDft        s              1  0 INZ(*zeros)
006300110222
006400031201     D*------------------
006500031201     D* Costanti
006600031201     D*------------------
006700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006900130731     D cifre           c                   const('0123456789')
007000010330
007100010330
007200130807     C*
007300130807      /FREE
007400130807        exec sql  set option  dynusrprf = *owner,
007500130807                              closqlcsr = *endmod;
007600130807      /END-FREE
007700130807     C*
007800010330
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100010601     C*
008200010601     C                   exsr      opeini
008300130724     C                   exsr      rwfile
008400010601     C*
008500010601     C                   seton                                        lr
008600010601
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010601     C*--------------------------------------------------------
009300010601     C     PREELA        BEGSR
009400010601     C*
009500130724     C                   if        flgGiro = '0'
009600010601     C*
009700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009800010601     C                   eval      flgGiro = '1'
009900010601     C*
010000010601     C                   endif
010100010601     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010601     C*--------------------------------------------------------
011000010601     C     ENDELA        BEGSR
011100000616     C*
011200010601     C                   ENDSR
011300010601     C***
011400010601
011500130724
011600010601
011700010330     C*--------------------------------------------------------
011800130724     C* RWFILE  LEGGE TIVIN00R E SCRIVE
011900010330     C*--------------------------------------------------------
012000130724     C     RWFILE        BEGSR
012100010330     C*
012200010330     C                   if        not %open(tivin00r)
012300010330     C                   open      tivin00r
012400010330     C                   endif
012500130807     C                   if        not %open(titat24c)
012600130807     C                   open      titat24c
012700130731     C                   endif
012800010330     C*
012900010604     C                   clear                   §CTROK
013000010604     C                   clear                   §CTRMO
013100010604     C                   clear                   §CTRNO
013200040726     C*
013300040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
013400040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
013500130724 xxx C***                exsr      inzvar
013600130724     C***                exsr      impvat
013700010330     C*
013800030822     C                   do        *HIVAL
013900010330     C*
014000030822     C                   read      tivin00r                               70
014100010618     C*
014200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014300010618     C                   if        vinflg = '1'
014400010618     C                   eval      flgOk = '1'
014500010618     C                   endif
014600010618     C*
014700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014800040510     C                   z-add     *zeros        wLenVINDTA        4 0
014900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015300040510     C                   z-add     1             wFlgVINDTA
015400040510     C                   leave
015500040510     C                   endif
015600040510     C                   enddo
015700040510     C*
015800040218     C                   if        vindta > *blanks AND
015900040510     C                             wFlgVINDTA = 1
016000010330     C*
016100010601     C                   if        *in70 = *off and
016200010330     C                             (vinflg = *blanks
016300010330     C                              or vinflg = '0'
016400010330     C                              or vinflg = '2')
016500010330     C*
016600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016700010711     C                   if        vinflg = *blanks or vinflg = '0'
016800010711     C                   clear                   vinmsg
016900010711     C                   endif
017000010601     C*
017100010330     C                   exsr      inzvar
017200130724     C                   exsr      impfile
017300010601     C*
017400010601     C                   exsr      PREELA
017500010601     C*
017600010604     C* Ebbene...
017700010604     C*
017800030822     C  N31              add       1             §CTROK            7 0
017900030822     C   32              add       1             §CTRMO            7 0
018000030822     C   31              add       1             §CTRNO            7 0
018100030822     C                   if        wGiro = 2
018200130807     C* scrivo se il riferimento è stato valorizzato
018300130807     C                   IF        EFRRIF <> *blank
018400130724     C  N31              write     TNEFR000
018500130731     C                   ENDIF
018600130731     C                   endif
018700010604     C*
018800010604     C                   if        *in31 = *off and
018900010604     C                             *in32 = *off
019000010604     C                   eval      vinflg = '1'
019100010604     C                   else
019200010604     C                   eval      vinflg = '2'
019300010604     C                   endif
019400130724     C*
019500130724     C                   endif
019600010604     C*
019700010330     C                   else
019800010330     C                   eval      vinflg = '1'
019900010330     C                   endif
020000010601     C*
020100010601     C  N70              update    tivin000
020200010330     C*
020300030822     C  N70              enddo
020400010601     C
020500010601     C*
020600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
020700130724     C                   if        cntNonEl = *zeros
020800010330     C* Se non ci sono record con errori ...
020900010601     C                   if        §ctrno = 0 and
021000130724     C                             §ctrmo = 0
021100010330     C* ... restituisco esito OK.
021200010330     C                   eval      wrkesito = '0'
021300010330     C                   else
021400010330     C                   if        §ctrok > 0
021500010330     C                   eval      wrkesito = '1'
021600010330     C                   else
021700010615     C                   if        flgOk = '0'
021800010615     C                   eval      wrkesito = '2'
021900010615     C                   else
022000010615     C                   eval      wrkesito = '6'
022100010615     C                   endif
022200010330     C                   endif
022300010330     C                   endif
022400010601     C                   else
022500010601     C                   eval      wrkesito = '9'
022600010601     C                   endif
022700010330     C*
022800010330     C                   if        %open(tivin00r)
022900010330     C                   close     tivin00r
023000010330     C                   endif
023100130807     C                   if        %open(titat24c)
023200130807     C                   close     titat24c
023300130731     C                   endif
023400010601     C*
023500010618     C                   if        flgGiro = '1'
023600010601     C                   exsr      endela
023700010618     C                   endif
023800010330     C*
023900010330     C                   ENDSR
024000010330     C***
024100010601
024200010601
024300010601
024400010330     C*----------------------------------------------------*
024500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024600010330     C*----------------------------------------------------*
024700010330     C     INZVAR        BEGSR
024800010330     C*
024900030822     C* Inizializzo variabili di wrk
025000130724     C
025100030822     C*
025200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
025300130724     C                   CLEAR                   TNEFR000
025400030822     C                   CLEAR                   SkSplitCSV
025500030822     C                   EVAL      i = 1
025600030822     C                   EVAL      posDa = *zeros
025700030822     C                   EVAL      posA  = *zeros
025800030822     C*
025900030822     C* Reimposto i valori di default
026000030822     C                   EXSR      DEFCAM
026100030822     C*
026200010330     C                   ENDSR
026300130724
026400130724
026500130724
026600010330     C*----------------------------------------------------*
026700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
026800010330     C*----------------------------------------------------*
026900010330     C     DEFCAM        BEGSR
027000030822     C*
027100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
027200030715     C* e delimitatore testo.
027300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
027400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
027500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
027600030822     C*
027700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
027800130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
027900030715     C                   ENDIF
028000030822     C*
028100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
028200030822     C                   EVAL      posDaDft = 1
028300030822     C                   EVAL      posADft  = 0
028400030822     C                   EVAL      wGiroDft = 0
028500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
028600030822     C                             posDaDft > 0
028700030822     C*
028800030822     C* Gestisco il 1° giro
028900030822     C                   IF        wGiroDft = 0
029000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
029100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
029200030822     C* Incremento il contatore dei "giri"
029300030822     C                   EVAL      wGiroDft = 1
029400030822     C                   ELSE
029500030822     C                   EVAL      posDaDft = posADft
029600030822     C                   ENDIF
029700030822     C* Eseguo lo scan x trovare la fine del campo corrente
029800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
029900030822     C*
030000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
030100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
030200030822     C                   IF        posDaDft > 0 AND
030300030822     C                             posADft  > 0
030400130724     C* NTW
030500130724     C                   IF        %subst(
030600130724     C                             %subst(vlrppt:posDaDft+1:
030700130724     C                             posADft-posDaDft-1):1:3)
030800130724     C                             = 'NTW'
030900130731     C                   EVAL      EFRNTW=%trim(%subst(
031000130731     C                             %subst(vlrppt:posDaDft+1:
031100130731     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031200130731     C                   ENDIF
031300130724     C* TPD
031400130724     C                   IF        %subst(
031500130724     C                             %subst(vlrppt:posDaDft+1:
031600130724     C                             posADft-posDaDft-1):1:3)
031700130724     C                             = 'TPD'
031800130724     C                   EVAL      EFRTPD=%trim(%subst(
031900130724     C                             %subst(vlrppt:posDaDft+1:
032000130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032100130724     C                   ENDIF
032200130724     C* CAU
032300130724     C                   IF        %subst(
032400130724     C                             %subst(vlrppt:posDaDft+1:
032500130724     C                             posADft-posDaDft-1):1:3)
032600130724     C                             = 'CAU'
032700130724     C                   EVAL      EFRCAU=%trim(%subst(
032800130724     C                             %subst(vlrppt:posDaDft+1:
032900130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033000130724     C                   ENDIF
033100130724     C* GRP
033200130724     C                   IF        %subst(
033300130724     C                             %subst(vlrppt:posDaDft+1:
033400130724     C                             posADft-posDaDft-1):1:3)
033500130724     C                             = 'GRP'
033600130724     C                   EVAL      EFRGRP=%trim(%subst(
033700130724     C                             %subst(vlrppt:posDaDft+1:
033800130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033900130724     C                   ENDIF
034000130806     C* AAS
034100130806     C                   IF        %subst(
034200130806     C                             %subst(vlrppt:posDaDft+1:
034300130806     C                             posADft-posDaDft-1):1:3)
034400130806     C                             = 'AAS'
034500130807     C* il parametro può essere un valore o la parola chiave *AAA =  anno corrente
034600130807     C                   IF        %trim(%subst(
034700130806     C                             %subst(vlrppt:posDaDft+1:
034800130806     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034900130807     C                             = '*AAA'
035000130807     C                   EVAL      wAnno = %dec(%subst(%char(datcor) : 1 : 4)
035100130807     C                                           : 4 : 0)
035200130807     C                   ELSE
035300130807     C                   EVAL      wAnno = %dec(%trim(%subst(
035400130807     C                             %subst(vlrppt:posDaDft+1:
035500130807     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035600130807     C                                           : 4 : 0)
035700130806     C                   ENDIF
035800130807     C                   ENDIF
035900130806     C* ...
036000030822     C                   ENDIF
036100030822     C                   ENDDO
036200020204     C*
036300010330     C                   ENDSR
036400130724
036500130724
036600130724
036700010607     C*----------------------------------------------------*
036800130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
036900010607     C*----------------------------------------------------*
037000130724     C     IMPFILE       BEGSR
037100010607     C*
037200130724     C                   SETOFF                                       3132
037300130724     C*
037400130724     C                   MOVEL     datcor        EFRDTR
037500010607     C*
037600030715     C**********
037700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
037800030715     C**********
037900030715     C                   IF        wGiro = *zeros
038000040726     C*
038100040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
038200040726     C                   EVAL      depvinDTA = vinDTA
038300040726     C*
038400040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
038500040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
038600040726     C                                      %trim(intcol(3))
038700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
038800031201     C     minu:maiu     XLATE     vindta        vindta
038900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
039000040324     C                   DOW       posDa <= %len(%trimr(vindta))
039100030715     C*
039200030715     C* Gestisco il 1° campo
039300030715     C                   IF        i = 1
039400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
039500030715     C                   EVAL      posDa = 1
039600030715     C* Eseguo lo scan x trovare la fine del primo campo
039700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
039800030715     C* A questo punto "estraggo" il campo corrente
039900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040000030715     C                                                    (posA-posDa))
040100030715     C* X i campi successivi al 1°
040200030715     C                   ELSE
040300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
040400030715     C                   EVAL      posDa = posA + 1
040500030715     C* Eseguo lo scan x trovare la fine del campo corrente
040600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040700030715     C* Gestisco l'ultimo campo
040800030715     C                   IF        posA = *zeros
040900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
041000030715     C* A questo punto "estraggo" il campo corrente
041100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041200030715     C                                                    (posA-posDa))
041300030715     C                   LEAVE
041400030715     C                   ELSE
041500030715     C* A questo punto "estraggo" il campo corrente
041600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041700030715     C                                                    (posA-posDa))
041800030715     C                   ENDIF
041900030715     C                   ENDIF
042000030715     C* Incremento il contatore d campo
042100030715     C                   EVAL      i = i +1
042200030715     C                   ENDDO
042300030715     C*
042400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
042500030715     C* delimitazione testo
042600030715     C                   EVAL      i = 1
042700030715     C                   DOW       i < %elem(SkSplitFLD)
042800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
042900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
043000030715     C                   EVAL      i = i + 1
043100030715     C                   ENDDO
043200030715     C                   EVAL      wGiro = 1
043300040726     C* Ripristino la 1° riga originale
043400040726     C                   EVAL      vinDTA = depvinDTA
043500030715     C                   ELSE
043600030715     C                   EVAL      wGiro = 2
043700031204     C**********
043800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
043900031204     C* d separatore campo
044000031204     C**********
044100040726     C                   EVAL      vindta = %trim(vindta)
044200040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
044300040726     C                   EVAL      vindta = %subst(vindta:2)
044400040726     C                   ENDDO
044500031204     C*
044600031204     C                   Z-ADD     *zeros        lunghInput        4 0
044700040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
044800040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
044900040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045000040726     C                   EVAL      lunghInput = %len(%trim(vindta))
045100040726     C                   ENDDO
045200030715     C**********
045300030715     C* Effettuo lo split del campo dati d input
045400030715     C**********
045500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
045600040324     C                   DOW       posDa <= %len(%trimr(vindta))
045700030715     C*
045800030715     C* Gestisco il 1° campo
045900030715     C                   IF        i = 1
046000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046100030715     C                   EVAL      posDa = 1
046200030715     C* Eseguo lo scan x trovare la fine del primo campo
046300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046400030715     C* A questo punto "estraggo" il campo corrente
046500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046600030715     C                                                    (posA-posDa))
046700030715     C* X i campi successivi al 1°
046800030715     C                   ELSE
046900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047000030715     C                   EVAL      posDa = posA + 1
047100031204     C* Verifico che nn vi sia il campo nn valorizzato
047200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047300031204     C* Se campo nn valorizzato skippo al prossimo
047400031204     C                   EVAL      SkSplitCSV(i) = *blanks
047500031204     C                   EVAL      posA  = posA + 1
047600031204     C                   ELSE
047700030715     C* Eseguo lo scan x trovare la fine del campo corrente
047800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047900030715     C* Gestisco l'ultimo campo
048000030715     C                   IF        posA = *zeros
048100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048200030715     C* A questo punto "estraggo" il campo corrente
048300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048400030715     C                                                    (posA-posDa))
048500030715     C                   LEAVE
048600030715     C                   ELSE
048700030715     C* A questo punto "estraggo" il campo corrente
048800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048900030715     C                                                    (posA-posDa))
049000030715     C                   ENDIF
049100031204     C                   ENDIF
049200030715     C                   ENDIF
049300030715     C* Incremento il contatore d campo
049400030715     C                   EVAL      i = i +1
049500030715     C                   ENDDO
049600030715     C*
049700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049800030715     C* delimitazione testo
049900030715     C                   EVAL      i = 1
050000030715     C                   DOW       i < %elem(SkSplitCSV)
050100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
050200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050300030715     C                   EVAL      i = i + 1
050400030715     C                   ENDDO
050500030715     C*
050600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050700030715     C                   EVAL      i = 1
050800030715     C                   DOW       i < %elem(SkSplitFLD)
050900030715     C*
051000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
051100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
051200030715     C                             SkSplitCSV(i) <> *blanks
051300130724     C*** RIF
051400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
051500130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
051600130724     C                   ENDIF
051700130724     C*** RFP
051800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
051900130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
052000130724     C                   ENDIF
052100130724     C*** NFT
052200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
052300130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
052400130724     C                   ENDIF
052500130724     C*
052600130724     C* Reperisco quindi i campi numerici...
052700130724     C*** DFT
052800130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
052900130724     C                   EVAL      PiStr=SkSplitCSV(i)
053000130724     C                   EXSR      CHKNUM
053100130724     C                   IF        PiInt=*on
053200130724     C                   Z-ADD     PiVal         EFRDFT
053300130724     C                   ELSE
053400130724     C                   SETON                                        31
053500130724     C                   EVAL      EFRDFT = *zeros
053600130724     C                   EVAL      vinmsg = %trimr(vinmsg)
053700130724     C                             + ' ' + 'EFRDFT'
053800130724     C                   ENDIF
053900130724     C                   ENDIF
054000130724     C*** IMP
054100130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
054200130724     C                   EVAL      PiStr=SkSplitCSV(i)
054300130724     C                   EXSR      CHKNUM
054400130724     C                   IF        PiNum=*on
054500130724     C                   Z-ADD     PiVal         EFRIMP
054600130724     C                   ELSE
054700130724     C                   SETON                                        31
054800130724     C                   EVAL      EFRIMP = *zeros
054900130724     C                   EVAL      vinmsg = %trimr(vinmsg)
055000130724     C                             + ' ' + 'EFRIMP'
055100130724     C                   ENDIF
055200130724     C                   ENDIF
055300130724     C*** PES
055400130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
055500130724     C                   EVAL      PiStr=SkSplitCSV(i)
055600130724     C                   EXSR      CHKNUM
055700130724     C                   IF        PiNum=*on
055800130724     C                   Z-ADD     PiVal         EFRPES
055900130724     C                   ELSE
056000130724     C                   SETON                                        31
056100130724     C                   EVAL      EFRPES = *zeros
056200130724     C                   EVAL      vinmsg = %trimr(vinmsg)
056300130724     C                             + ' ' + 'EFRPES'
056400130724     C                   ENDIF
056500130724     C                   ENDIF
056600130724     C*** VOL
056700130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
056800130724     C                   EVAL      PiStr=SkSplitCSV(i)
056900130724     C                   EXSR      CHKNUM
057000130724     C                   IF        PiNum=*on
057100130724     C                   Z-ADD     PiVal         EFRVOL
057200130724     C                   ELSE
057300130724     C                   SETON                                        31
057400130724     C                   EVAL      EFRVOL = *zeros
057500130724     C                   EVAL      vinmsg = %trimr(vinmsg)
057600130724     C                             + ' ' + 'EFRVOL'
057700130724     C                   ENDIF
057800130724     C                   ENDIF
057900110222     C*
058000110222     C***  ===>  Gestione campi "particolari" dentro ciclo
058100130731     C*
058200130806      *** W_DFT: la data è in formato DD/MM/YYYY alfanum. e va trasformata in 8 S
058300130731     C                   IF        %trim(SkSplitFLD(i)) = 'W_DFT'
058400130801     C* il partner può mandare la data con l'anno di 2 o di 4, il giorno o il mese senza
058500130801     C* zeri non significativi, bisogna gestirla (se è 10 char siamo sicuri, altrimenti no)
058600130731     C                   IF        %len(%trim(SkSplitCSV(i)))=10
058700130731     C                   EVAL      Data8A = %subst(%trim(SkSplitCSV(i)):7:4) +
058800130806     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
058900130806     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
059000130731     C                   MOVEL     Data8A        EFRDFT
059100130731     C                   ELSE
059200130806     C* se la data non fosse né di 6 né di 8, valorizzo EFTDFT come se mi arrivasse di 6
059300130806     C* così se il valore è impossibile va in errore e ce ne accorgiamo
059400130806     C                   EVAL      Data8A = %subst(%char(datcor):1:2) +
059500130806     C                                      %subst(%trim(SkSplitCSV(i)):7:2) +
059600130806     C                                      %subst(%trim(SkSplitCSV(i)):4:2) +
059700130806     C                                      %subst(%trim(SkSplitCSV(i)):1:2)
059800130806     C                   MOVEL     Data8A        EFRDFT
059900130731     C                   ENDIF
060000130807     C                   ENDIF
060100130731
060200130731     C*
060300130806      *** W_RIF: il cliente ci passa come dato il numero segnacollo senza gli zeri non significativi
0604001307311    C                   IF        %trim(SkSplitFLD(i)) = 'W_RIF'
060500130806     C* controllo che sia un dato numerico
0606001308072    C                   IF        %check(cifre : %trim(SkSplitCSV(i))) = 0
060700130806     C* scompongo il dato in questo modo:
060800130806     C* da pos 1 a 3 = filiale segnacollo
060900130806     C                   EVAL      TATFLS = %dec(%subst(
061000130806     C                                    %editc(
061100130806     C                                     %dec(%trim(SkSplitCSV(i)) : 18 : 0)
061200130806     C                                    : 'X')
061300130806     C                                   : 1 : 3) : 3 : 0)
061400130806     C* da pos 4 a 6 = linea di arrivo
061500130806     C                   EVAL      TATLNA = %dec(%subst(
061600130806     C                                    %editc(
061700130806     C                                     %dec(%trim(SkSplitCSV(i)) : 18 : 0)
061800130806     C                                    : 'X')
061900130806     C                                   : 4 : 3) : 3 : 0)
062000130806     C* da pos 7 a 8 = serie
062100130806     C                   EVAL      TATNRS = %dec(%subst(
062200130806     C                                    %editc(
062300130806     C                                     %dec(%trim(SkSplitCSV(i)) : 18 : 0)
062400130806     C                                    : 'X')
062500130806     C                                   : 7 : 2) : 2 : 0)
062600130806     C* da pos 9 a 15 = nr. segnacollo
062700130806     C                   EVAL      TATNSC = %dec(%subst(
062800130806     C                                    %editc(
062900130806     C                                     %dec(%trim(SkSplitCSV(i)) : 18 : 0)
063000130806     C                                    : 'X')
063100130806     C                                   : 9 : 7) : 7 : 0)
063200130806     C*
063300130806     C                   CLEAR                   savAAS
063400130806     C                   CLEAR                   savLNP
063500130806     C                   CLEAR                   savNRS
063600130806     C                   CLEAR                   savNSP
063700130806     C* mi posiziono per le chiavi ottenute
063800130806     C                   EVAL      wNrRcdLetti = 0
063900130806     C     K04TAT24      SETLL     TITAT24C
0640001308073    C                   DOW       not %eof(TITAT24C)
064100130806     C     K04TAT24      READE     TITAT24C
0642001308074    C                   IF        not %eof(TITAT24C)
064300130806     C                   EVAL      wNrRcdLetti = wNrRcdLetti + 1
064400130806     C* filtro
0645001308065    C                   IF        TATAAS <> wAnno
064600130806     C* se il rcd è stato scartato, leggo il successivo
064700130806     C                   ITER
0648001308065-   C                   ENDIF
064900130806     C* se rcd non scartato, salvo i dati su costruire EFRRIF
065000130806     C                   EVAL      savAAS = TATAAS
065100130806     C                   EVAL      savLNP = TATLNP
065200130806     C                   EVAL      savNRS = TATNRS
065300130806     C                   EVAL      savNSP = TATNSP
0654001308064-   C                   ENDIF
0655001308063-   C                   ENDDO
065600130806     C*
0657001308063    C                   SELECT
065800130806     C* se non ho letto rcd, devo eseguire SQL su TITAS00F
065900130806     C                   WHEN      wNrRcdLetti = 0
066000130806     C                   EXSR      CercoSuTITAS
066100130806     C*
066200130806     C* se ho letto un solo rcd, uso comunque i suoi dati (anche se scartabile)
066300130806     C                   WHEN      wNrRcdLetti = 1
066400130806     C                   EVAL      savAAS = TATAAS
066500130806     C                   EVAL      savLNP = TATLNP
066600130806     C                   EVAL      savNRS = TATNRS
066700130806     C                   EVAL      savNSP = TATNSP
0668001308063-   C                   ENDSL
066900130806     C*
067000130807     C* costruisco EFRRIF se i dati ci sono
067100130807     C                   IF        savAAS > 0 and savLNP > 0 and savNSP > 0
067200130806     C                   EVAL      EFRRIF =
067300130806     C                              %subst(%char(savAAS) : 3 : 2) +
067400130806     C                              %editc(savLNP : 'X') +
067500130806     C                              %editc(savNRS : 'X') +
067600130806     C                              %editc(savNSP : 'X')
067700130807     C* altrimenti EFRRIF viene costruito come:
067800130807     C* 'SEGN-' + il valore inizialmente reperito di 15
0679001308072e   C                   ELSE
068000130807     C                   EVAL      EFRRIF = 'SEGN-' +
068100130807     C                                    %subst(
068200130807     C                                    %editc(
068300130807     C                                     %dec(%trim(SkSplitCSV(i)) : 18 : 0)
068400130807     C                                    : 'X')
068500130807     C                                   : 1 : 15)
0686001308072-   C                   ENDIF
068700130806
0688001308062e   C                   ELSE
068900130806     C                   SETON                                        32
069000130806     C                   EVAL      EFRRIF = *blank
069100130806     C                   EVAL      vinmsg = %trimr(vinmsg)
069200130806     C                             + ' ' + 'EFRRIF'
0693001308062-   C                   ENDIF
0694001308061-   C                   ENDIF
069500130731     C*
069600110222     C***  <===  -----------------------------------------
069700030715     C*
069800030715     C                   ENDIF
069900030715     C                   EVAL      i = i + 1
070000030715     C                   ENDDO
070100130724     C*
070200130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
070300130724     C
070400130724     C***  <===  -----------------------------------------
070500020204     C*
070600030715     C                   ENDIF
070700020204     C*
070800010607     C                   ENDSR
070900010607     C*----------------------------------------------------*
071000040714
071100010330
071200010330
071300010330     C*----------------------------------------------------*
071400130806     C*  Eseguo SQL su TITAS00F
071500010330     C*----------------------------------------------------*
071600130806     C     CercoSuTITAS  BEGSR
071700010330     C*
071800130806     C                   EVAL      wSQL =
071900130806     C                              'select tasAAS, tasLNP, tasNRS, tasNSP +
072000130806     C                              from titas00f where tasfls=' +
072100130806     C                              %editc(TATFLS : 'X') +
072200130806     C                              ' and tasnrs=' +
072300130806     C                              %editc(TATNRS : 'X') +
072400130806     C                              ' and taslna=' +
072500130806     C                              %editc(TATLNA : 'X') +
072600130807     C                              ' and (' + %char(TATNSC) +
072700130807     C                              ' between tasncd and tasnca or ' +
072800130807     C                              %char(TATNSC) +
072900130807     C                              ' = tasncd)' +
073000130806     C                              ' union all ' +
073100130806     C                              'select tasAAS, tasLNP, tasNRS, tasNSP +
073200130806     C                              from titas10f where tasfls=' +
073300130806     C                              %editc(TATFLS : 'X') +
073400130806     C                              ' and tasnrs=' +
073500130806     C                              %editc(TATNRS : 'X') +
073600130806     C                              ' and taslna=' +
073700130806     C                              %editc(TATLNA : 'X') +
073800130807     C                              ' and (' + %char(TATNSC) +
073900130807     C                              ' between tasncd and tasnca or ' +
074000130807     C                              %char(TATNSC) +
074100130807     C                              ' = tasncd)'
074200130806     C*
074300130806      /FREE
074400130806       exec sql  prepare S1  from :wSQL;
074500130806       exec sql  declare C1  cursor for S1;
074600130806       exec sql  open C1;
074700130806       clear  *all resultSQL;
074800130806       wX = 1;
074900130806       // punto all'occorrenza da valorizzare
075000130806       %occur(resultSQL) = wX;
075100130806       // ciclo finché non sono finiti i rcd da leggere
075200130806       DoU  SQLCode = 100
075300130806       // oppure non c'è stato un errore bloccante
075400130806        or SQLCode < 0
075500130806       // oppure siamo al limite delle occorrenze memorizzabili
075600130806        or wX > %elem(resultSQL);
075700130806         exec sql  fetch next  from C1  into :resultSQL;
075800130806         select;
075900130806           when SQLCODE < *zero;
076000130806             //errore
076100130807             leave;
076200130806           // nessuna lettura
076300130806           when SQLCode = 100 and wX = 1;
076400130806             // non faccio nulla
076500130806           // lettura
076600130806           when SQLCode = 0 or (SQLCode > 0 and SQLCode <> 100);
076700130806             wX = wX + 1;
076800130806             // punto all'occorrenza da valorizzare se non siamo già arrivati a fine occorrenza
076900130806             if wX <= %elem(resultSQL);
077000130806               // punto all'occorrenza da valorizzare
077100130806               %occur(resultSQL) = wX;
077200130806             endif;
077300130806           endsl;
077400130806         enddo;
077500130806         // - Chiusura cursore
077600130807         exec sql   close C1;
077700130806         // siccome all'inizio di ogni ciclo incremento di 1 il valore dell'occorrenza,
077800130806         // le occorrenze davvero valorizzate sono inferiori di 1
077900130806         wX = wX - 1;
078000130806
078100130806         select;
078200130806           //se non ho trovato neanche un rcd, azzero i risultati
078300130806           when wX = 0;
078400130806             clear savAAS;
078500130806             clear savLNP;
078600130806             clear savNRS;
078700130806             clear savNSP;
078800130806           //se ho trovato un solo rcd, prendo quel valore senza filtrare
078900130806           when wX = 1;
079000130807             // punto all'occorrenza da cui reperire i dati
079100130807             %occur(resultSQL) = wX;
079200130806             savAAS = AAS;
079300130806             savLNP = LNP;
079400130806             savNRS = NRS;
079500130806             savNSP = NSP;
079600130806           //se ho trovato più di un rcd, ciclo e filtro
079700130806           when wX > 1;
079800130806             for wY = 1 to wX;
079900130806               %occur(resultSQL) = wY;
080000130806               if AAS = wAnno;
080100130806                 savAAS = AAS;
080200130806                 savLNP = LNP;
080300130806                 savNRS = NRS;
080400130806                 savNSP = NSP;
080500130806                 leave;
080600130806               endif;
080700130806             endfor;
080800130806         endsl;
080900130806
081000130806      /END-FREE
081100130806     C*
081200130806     C                   ENDSR
081300130806     C*----------------------------------------------------*
081400130806
081500130806
081600130806
081700130806     C*----------------------------------------------------*
081800130806     C*  CONTROLLO NUMERICITA' CAMPI
081900130806     C*----------------------------------------------------*
082000130806     C     CHKNUM        BEGSR
082100130806     C*
082200010606     C                   IF        PiDecChr = *blanks
082300030715     C                   EVAL      PiDecChr = CharNUM
082400010606     C                   ENDIF
082500010606     C*
082600010606     C                   CALL(e)   'ISNUMERIC'
082700010330     C                   PARM                    PiStr            30
082800010606     C                   PARM                    PiDecChr          1
082900010330     C                   PARM      *ZEROS        PiVal            30 9
083000010330     C                   PARM      '0'           PiInt             1
083100010330     C                   PARM      '0'           PiNum             1
083200010330     C                   IF        %error
083300010606     C                   EVAL      PiNum=*off
083400010330     C                   ENDIF
083500010330     C*
083600010330     C                   ENDSR
083700010330     C***
083800010601
083900010601
084000010601
084100010601      /TITLE Invio dei dati al punto operativo.
084200010601     C     opeini        BEGSR
084300010601     C*
084400010601     C* Inizializzo flag e contatori operativi
084500010601     C                   movel     '0'           flgGiro           1
084600010615     C                   movel     '0'           flgOk             1
084700010601     C                   z-add     *zeros        cntNonEl         10 0
084800010601     C*
084900010601     C                   ENDSR
085000010601     C***
085100010601
085200050421
085300050421
085400130807     C     *pssr         BEGSR
085500050421     C*
085600130807     C                   eval      wrkesito = '2'
085700050421     C*
085800130807     C                   ENDSR     '*CANCL'
085900050421     C***
086000010601
086100010330
086200010330
086300000613     C     *inzsr        BEGSR
086400990910     C*
086500990910     C     *entry        plist
086600990920     C                   parm                    tivlrds
086700990921     C                   parm      wrkesito      esito
086800000724     C                   parm                    prmlit
086900000710     C                   parm                    prmfir
087000130731     C*
087100130731     C* KLIST
087200130806     c     K04TAT24      klist
087300130806     c                   kfld                    TATFls
087400130806     c                   kfld                    TATNrs
087500130806     c                   kfld                    TATNsc
087600130806     c                   kfld                    TATLna
087700010330     C*
087800010330     C* CALCOLA LA DATA CORRENTE
087900130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088000130724     C                   eval      datcor = %dec(%date() : *ISO)
088100000613     C*
088200000613     C                   ENDSR
088300000613     C***
088400040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
088500130806EFRNFT;W_DFT;X;X;X;X;X;X;EFRRFP;X;X;X;X;X;EFRPES;X;X;X;X;X;EFRIMP;X;X;X;W_RIF;
088600130806
088700130528
