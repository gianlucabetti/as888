000100130724      /TITLE Upload via Internet: traduzione file confronto fatturazione PARTNER
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500130724     FTNEFR00F  O    E             DISK
000600141209     FTITAS30C  IF   E           K DISK    usropn
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
003500130729     D Data8A          s              8
003600141210     D w14A            s             14
003700130813     D Data8N          s              8s 0
003800130813     D wEFRRIF2        s                   like(EFRRIF)
003900130813     D result          s             50
004000130813     D wK              s              3s 0
004100130813     D wX              s              3s 0
004200130813     D wY              s              3s 0
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
005600130813     D cifre           c                   const('0123456789')
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
010600141209     C                   if        not %open(titas30c)
010700141209     C                   open      titas30c
010800141209     C                   endif
010900010330     C*
011000010604     C                   clear                   §CTROK
011100010604     C                   clear                   §CTRMO
011200010604     C                   clear                   §CTRNO
011300040726     C*
011400040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
011500040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
011600130724 xxx C***                exsr      inzvar
011700130724     C***                exsr      impvat
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
014100010330     C*
014200010601     C                   if        *in70 = *off and
014300010330     C                             (vinflg = *blanks
014400010330     C                              or vinflg = '0'
014500010330     C                              or vinflg = '2')
014600010330     C*
014700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014800010711     C                   if        vinflg = *blanks or vinflg = '0'
014900010711     C                   clear                   vinmsg
015000010711     C                   endif
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
016500141209     C* e il network è <> LNA
016600141209     C* 'LNA' non è un network, indica che mi è stato detto di reperire il network dalla bolla,
016700141209     C* cioè da TITAS (quindi non sono scritti né gli ORM né tutto ciò che non contiene un
016800141209     C* riferimento BRT associabile a una bolla)
016900141209     C                             and EFRNTW <> 'LNA'
017000130724     C  N31              write     TNEFR000
017100130813     C                   ENDIF
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
021700141209     C                   if        %open(titas30c)
021800141209     C                   close     titas30c
021900141209     C                   endif
022000010601     C*
022100010618     C                   if        flgGiro = '1'
022200010601     C                   exsr      endela
022300010618     C                   endif
022400010330     C*
022500010330     C                   ENDSR
022600010330     C***
022700010601
022800010601
022900010601
023000010330     C*----------------------------------------------------*
023100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023200010330     C*----------------------------------------------------*
023300010330     C     INZVAR        BEGSR
023400010330     C*
023500030822     C* Inizializzo variabili di wrk
023600130724     C
023700030822     C*
023800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
023900130724     C                   CLEAR                   TNEFR000
024000030822     C                   CLEAR                   SkSplitCSV
024100030822     C                   EVAL      i = 1
024200030822     C                   EVAL      posDa = *zeros
024300030822     C                   EVAL      posA  = *zeros
024400030822     C*
024500030822     C* Reimposto i valori di default
024600030822     C                   EXSR      DEFCAM
024700030822     C*
024800010330     C                   ENDSR
024900130724
025000130724
025100130724
025200010330     C*----------------------------------------------------*
025300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
025400010330     C*----------------------------------------------------*
025500010330     C     DEFCAM        BEGSR
025600030822     C*
025700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
025800030715     C* e delimitatore testo.
025900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
026000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
026100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
026200030822     C*
026300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
026400130724 xxx C                   IF        %subst(vlrppt:1:1) = 'S'
026500030715     C                   ENDIF
026600030822     C*
026700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
026800030822     C                   EVAL      posDaDft = 1
026900030822     C                   EVAL      posADft  = 0
027000030822     C                   EVAL      wGiroDft = 0
027100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
027200030822     C                             posDaDft > 0
027300030822     C*
027400030822     C* Gestisco il 1° giro
027500030822     C                   IF        wGiroDft = 0
027600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
027700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
027800030822     C* Incremento il contatore dei "giri"
027900030822     C                   EVAL      wGiroDft = 1
028000030822     C                   ELSE
028100030822     C                   EVAL      posDaDft = posADft
028200030822     C                   ENDIF
028300030822     C* Eseguo lo scan x trovare la fine del campo corrente
028400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
028500030822     C*
028600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
028700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
028800030822     C                   IF        posDaDft > 0 AND
028900030822     C                             posADft  > 0
029000130724     C* NTW
029100130724     C                   IF        %subst(
029200130724     C                             %subst(vlrppt:posDaDft+1:
029300130724     C                             posADft-posDaDft-1):1:3)
029400130724     C                             = 'NTW'
029500141209     C* se il valore del parametro è *LNA, lo memorizzo perché mi serve dopo (EFRNTW='LNA')
029600141209     C                   IF        %trim(%subst(
029700141209     C                             %subst(vlrppt:posDaDft+1:
029800141209     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029900141209     C                             = '*LNA'
030000141209     C                   EVAL      EFRNTW='LNA'
030100141209     C                   ELSE
030200130724     C                   EVAL      EFRNTW=%trim(%subst(
030300130724     C                             %subst(vlrppt:posDaDft+1:
030400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030500141209     C                   ENDIF
030600130724     C                   ENDIF
030700130724     C* TPD
030800130724     C                   IF        %subst(
030900130724     C                             %subst(vlrppt:posDaDft+1:
031000130724     C                             posADft-posDaDft-1):1:3)
031100130724     C                             = 'TPD'
031200130724     C                   EVAL      EFRTPD=%trim(%subst(
031300130724     C                             %subst(vlrppt:posDaDft+1:
031400130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031500130724     C                   ENDIF
031600130724     C* CAU
031700130724     C                   IF        %subst(
031800130724     C                             %subst(vlrppt:posDaDft+1:
031900130724     C                             posADft-posDaDft-1):1:3)
032000130724     C                             = 'CAU'
032100130724     C                   EVAL      EFRCAU=%trim(%subst(
032200130724     C                             %subst(vlrppt:posDaDft+1:
032300130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
032400130724     C                   ENDIF
032500130724     C* GRP
032600130724     C                   IF        %subst(
032700130724     C                             %subst(vlrppt:posDaDft+1:
032800130724     C                             posADft-posDaDft-1):1:3)
032900130724     C                             = 'GRP'
033000130724     C                   EVAL      EFRGRP=%trim(%subst(
033100130724     C                             %subst(vlrppt:posDaDft+1:
033200130724     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033300130724     C                   ENDIF
033400030822     C* ...
033500030822     C                   ENDIF
033600030822     C                   ENDDO
033700020204     C*
033800010330     C                   ENDSR
033900130724
034000130724
034100130724
034200010607     C*----------------------------------------------------*
034300130724     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
034400010607     C*----------------------------------------------------*
034500130724     C     IMPFILE       BEGSR
034600010607     C*
034700130724     C                   SETOFF                                       3132
034800130724     C*
034900130724     C                   MOVEL     datcor        EFRDTR
035000010607     C*
035100030715     C**********
035200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
035300030715     C**********
0354001308131    C                   IF        wGiro = *zeros
035500040726     C*
035600040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
035700040726     C                   EVAL      depvinDTA = vinDTA
035800040726     C*
035900040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
036000040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
036100040726     C                                      %trim(intcol(3))
036200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
036300031201     C     minu:maiu     XLATE     vindta        vindta
036400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
0365001308132    C                   DOW       posDa <= %len(%trimr(vindta))
036600030715     C*
036700030715     C* Gestisco il 1° campo
0368001308133    C                   IF        i = 1
036900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
037000030715     C                   EVAL      posDa = 1
037100030715     C* Eseguo lo scan x trovare la fine del primo campo
037200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
037300030715     C* A questo punto "estraggo" il campo corrente
037400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037500030715     C                                                    (posA-posDa))
037600030715     C* X i campi successivi al 1°
0377001308133e   C                   ELSE
037800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
037900030715     C                   EVAL      posDa = posA + 1
038000030715     C* Eseguo lo scan x trovare la fine del campo corrente
038100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
038200030715     C* Gestisco l'ultimo campo
0383001308134    C                   IF        posA = *zeros
038400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
038500030715     C* A questo punto "estraggo" il campo corrente
038600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
038700030715     C                                                    (posA-posDa))
038800030715     C                   LEAVE
0389001308134e   C                   ELSE
039000030715     C* A questo punto "estraggo" il campo corrente
039100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039200030715     C                                                    (posA-posDa))
0393001308134-   C                   ENDIF
0394001308133-   C                   ENDIF
039500030715     C* Incremento il contatore d campo
039600030715     C                   EVAL      i = i +1
0397001308132-   C                   ENDDO
039800030715     C*
039900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
040000030715     C* delimitazione testo
040100030715     C                   EVAL      i = 1
0402001308132    C                   DOW       i < %elem(SkSplitFLD)
040300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
040400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
040500030715     C                   EVAL      i = i + 1
0406001308132-   C                   ENDDO
040700030715     C                   EVAL      wGiro = 1
040800040726     C* Ripristino la 1° riga originale
040900040726     C                   EVAL      vinDTA = depvinDTA
0410001308131e   C                   ELSE
041100030715     C                   EVAL      wGiro = 2
041200031204     C**********
041300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
041400031204     C* d separatore campo
041500031204     C**********
041600040726     C                   EVAL      vindta = %trim(vindta)
0417001308132    C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
041800040726     C                   EVAL      vindta = %subst(vindta:2)
0419001308132-   C                   ENDDO
042000031204     C*
042100031204     C                   Z-ADD     *zeros        lunghInput        4 0
042200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
0423001308132    C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
042400040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
042500040726     C                   EVAL      lunghInput = %len(%trim(vindta))
0426001308132-   C                   ENDDO
042700030715     C**********
042800030715     C* Effettuo lo split del campo dati d input
042900030715     C**********
043000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
0431001308132    C                   DOW       posDa <= %len(%trimr(vindta))
043200030715     C*
043300030715     C* Gestisco il 1° campo
0434001308133    C                   IF        i = 1
043500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
043600030715     C                   EVAL      posDa = 1
043700030715     C* Eseguo lo scan x trovare la fine del primo campo
043800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043900030715     C* A questo punto "estraggo" il campo corrente
044000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
044100030715     C                                                    (posA-posDa))
044200030715     C* X i campi successivi al 1°
0443001308133e   C                   ELSE
044400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
044500030715     C                   EVAL      posDa = posA + 1
044600031204     C* Verifico che nn vi sia il campo nn valorizzato
0447001308134    C                   IF        %subst(vindta:posDa:1) = CharCSV
044800031204     C* Se campo nn valorizzato skippo al prossimo
044900031204     C                   EVAL      SkSplitCSV(i) = *blanks
045000031204     C                   EVAL      posA  = posA + 1
0451001308134e   C                   ELSE
045200030715     C* Eseguo lo scan x trovare la fine del campo corrente
045300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
045400030715     C* Gestisco l'ultimo campo
0455001308135    C                   IF        posA = *zeros
045600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
045700030715     C* A questo punto "estraggo" il campo corrente
045800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
045900030715     C                                                    (posA-posDa))
046000030715     C                   LEAVE
0461001308135e   C                   ELSE
046200030715     C* A questo punto "estraggo" il campo corrente
046300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046400030715     C                                                    (posA-posDa))
0465001308135-   C                   ENDIF
0466001308134-   C                   ENDIF
0467001308133-   C                   ENDIF
046800030715     C* Incremento il contatore d campo
046900030715     C                   EVAL      i = i +1
0470001308132-   C                   ENDDO
047100030715     C*
047200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
047300030715     C* delimitazione testo
047400030715     C                   EVAL      i = 1
0475001308132    C                   DOW       i < %elem(SkSplitCSV)
047600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
047700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
047800030715     C                   EVAL      i = i + 1
0479001308132-   C                   ENDDO
048000030715     C*
048100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
048200030715     C                   EVAL      i = 1
0483001308132    C                   DOW       i < %elem(SkSplitFLD)
048400030715     C*
048500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
0486001308133    C                   IF        SkSplitFLD(i) <> *blanks AND
048700030715     C                             SkSplitCSV(i) <> *blanks
048800130724     C*** RIF
048900130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRIF'
049000130724     C                   EVAL      EFRRIF = SkSplitCSV(i)
049100130724     C                   ENDIF
049200130724     C*** RFP
049300130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRRFP'
049400130724     C                   EVAL      EFRRFP = SkSplitCSV(i)
049500130724     C                   ENDIF
049600130724     C*** NFT
049700130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRNFT'
049800130724     C                   EVAL      EFRNFT = SkSplitCSV(i)
049900130724     C                   ENDIF
050000130724     C*
050100130724     C* Reperisco quindi i campi numerici...
050200130724     C*** DFT
050300130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRDFT'
050400130724     C                   EVAL      PiStr=SkSplitCSV(i)
050500130724     C                   EXSR      CHKNUM
050600130724     C                   IF        PiInt=*on
050700130724     C                   Z-ADD     PiVal         EFRDFT
050800130724     C                   ELSE
050900130724     C                   SETON                                        31
051000130724     C                   EVAL      EFRDFT = *zeros
051100130724     C                   EVAL      vinmsg = %trimr(vinmsg)
051200130724     C                             + ' ' + 'EFRDFT'
051300130724     C                   ENDIF
051400130724     C                   ENDIF
051500130724     C*** IMP
051600130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRIMP'
051700130724     C                   EVAL      PiStr=SkSplitCSV(i)
051800130724     C                   EXSR      CHKNUM
051900130724     C                   IF        PiNum=*on
052000130724     C                   Z-ADD     PiVal         EFRIMP
052100130724     C                   ELSE
052200130724     C                   SETON                                        31
052300130724     C                   EVAL      EFRIMP = *zeros
052400130724     C                   EVAL      vinmsg = %trimr(vinmsg)
052500130724     C                             + ' ' + 'EFRIMP'
052600130724     C                   ENDIF
052700130724     C                   ENDIF
052800130724     C*** PES
052900130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRPES'
053000130724     C                   EVAL      PiStr=SkSplitCSV(i)
053100130724     C                   EXSR      CHKNUM
053200130724     C                   IF        PiNum=*on
053300130724     C                   Z-ADD     PiVal         EFRPES
053400130724     C                   ELSE
053500130724     C                   SETON                                        31
053600130724     C                   EVAL      EFRPES = *zeros
053700130724     C                   EVAL      vinmsg = %trimr(vinmsg)
053800130724     C                             + ' ' + 'EFRPES'
053900130724     C                   ENDIF
054000130724     C                   ENDIF
054100130724     C*** VOL
054200130724     C                   IF        %trim(SkSplitFLD(i)) = 'EFRVOL'
054300130724     C                   EVAL      PiStr=SkSplitCSV(i)
054400130724     C                   EXSR      CHKNUM
054500130724     C                   IF        PiNum=*on
054600130724     C                   Z-ADD     PiVal         EFRVOL
054700130724     C                   ELSE
054800130724     C                   SETON                                        31
054900130724     C                   EVAL      EFRVOL = *zeros
055000130724     C                   EVAL      vinmsg = %trimr(vinmsg)
055100130724     C                             + ' ' + 'EFRVOL'
055200130724     C                   ENDIF
055300130724     C                   ENDIF
055400110222     C*
055500110222     C***  ===>  Gestione campi "particolari" dentro ciclo
055600110222     C*
055700140923
055800140923     C*
055900110222     C***  <===  -----------------------------------------
056000030715     C*
0561001308133-   C                   ENDIF
056200030715     C                   EVAL      i = i + 1
0563001308132-   C                   ENDDO
056400130724     C*
056500130724     C***  ===>  Eseguo considerazioni "fuori ciclo"
056600130724     C
056700140923
056800141210      *** EFRRIF: - se il valore indicato è un numerico lungo 14 e se il valore del parametro in
056900141209     C*          ingresso 'NTW' è = '*LNA => scomponendo i 14 bytes in:
057000141209     C*            YY (portare da 13 a 2013) => TASAAS
057100141209     C*            LNP     => TASLNP
057200141209     C*            NRS     => TASNRS
057300141209     C*            NSP     => TASNSP
057400141209     C*          chainare il TITAS30C e reperire TASLNA da utilizzare nella valorizzazione del campo
057500141209     C*          EFRNTW
057600141209     C                   EVAL      w14A = *blank
057700141209     C* controllo che sia lungo 14
057800141210     C                   IF        %len(%trim(EFRRIF))=14
057900141209     C                             and EFRNTW = 'LNA'
058000141210     C                   EVAL      w14A = %trim(EFRRIF)
058100141209     C* controllo che sia numerico
058200141209     C                   EVAL      wX = %check(cifre : w14A)
0583001412093    C                   IF        wX = 0
058400141209     C                   EVAL      TASAAS = 2000 + %dec(%subst(w14A:1:2) :4:0)
058500141209     C                   EVAL      TASLNP = %dec(%subst(w14A:3:3) :3:0)
058600141209     C                   EVAL      TASNRS = %dec(%subst(w14A:6:2) :2:0)
058700141209     C                   EVAL      TASNSP = %dec(%subst(w14A:8:7) :7:0)
058800141209     C     K04TAS30      CHAIN     TITAS30C
0589001412094    C                   IF        %found
059000141209     C                   EVAL      EFRNTW = %editc(TASLNA:'X')
059100141209     C* se non trovo il rcd su TITAS do un warning
0592001412094e   C                   ELSE
059300141209     C                   SETON                                        32
059400141209     C                   EVAL      EFRNTW = *blank
059500141209     C                   EVAL      vinmsg = %trimr(vinmsg)
059600141210     C                             + ' ' + 'EFRNTW'
0597001412094-   C                   ENDIF
0598001412093e   C                   ELSE
059900141209     C                   EVAL      w14A = *blank
0600001412093-   C                   ENDIF
0601001412092-   C                   ENDIF
060200141209     C* se il dato in ingresso non era lungo 14 char o non era numerico
060300141209     C* o il network nel parametro non è *LNA
060400141210     C* oppure la LNA trovata non è né 333 né 334
0605001412092    C                   IF        w14A = *blank
0606001412102    C                             or (EFRNTW <> '333' and EFRNTW <> '334')
060700141210     C* imposto il network col default e cioè 333
0608001412102    C                   EVAL      EFRNTW = '333'
0609001412092-   C                   ENDIF
061000141209
061100130813     C
061200130724     C***  <===  -----------------------------------------
061300020204     C*
0614001308131-   C                   ENDIF
061500020204     C*
061600010607     C                   ENDSR
061700010607     C*----------------------------------------------------*
061800040714
061900010330
062000010330
062100010330     C*----------------------------------------------------*
062200010330     C*  CONTROLLO NUMERICITA' CAMPI
062300010330     C*----------------------------------------------------*
062400010330     C     CHKNUM        BEGSR
062500010330     C*
062600010606     C                   IF        PiDecChr = *blanks
062700030715     C                   EVAL      PiDecChr = CharNUM
062800010606     C                   ENDIF
062900010606     C*
063000010606     C                   CALL(e)   'ISNUMERIC'
063100010330     C                   PARM                    PiStr            30
063200010606     C                   PARM                    PiDecChr          1
063300010330     C                   PARM      *ZEROS        PiVal            30 9
063400010330     C                   PARM      '0'           PiInt             1
063500010330     C                   PARM      '0'           PiNum             1
063600010330     C                   IF        %error
063700010606     C                   EVAL      PiNum=*off
063800010330     C                   ENDIF
063900010330     C*
064000010330     C                   ENDSR
064100010330     C***
064200010601
064300010601
064400010601
064500010601      /TITLE Invio dei dati al punto operativo.
064600010601     C     opeini        BEGSR
064700010601     C*
064800010601     C* Inizializzo flag e contatori operativi
064900010601     C                   movel     '0'           flgGiro           1
065000010615     C                   movel     '0'           flgOk             1
065100010601     C                   z-add     *zeros        cntNonEl         10 0
065200010601     C*
065300010601     C                   ENDSR
065400010601     C***
065500010601
065600050421
065700050421
065800050421     C     *pssr         BEGSR
065900050421     C*
066000050421     C                   eval      wrkesito = '2'
066100050421     C*
066200050421     C                   ENDSR     '*CANCL'
066300050421     C***
066400010601
066500010330
066600010330
066700000613     C     *inzsr        BEGSR
066800990910     C*
066900990910     C     *entry        plist
067000990920     C                   parm                    tivlrds
067100990921     C                   parm      wrkesito      esito
067200000724     C                   parm                    prmlit
067300000710     C                   parm                    prmfir
067400141210     C*
067500141210     C* KLIST
067600141210     c     K04TAS30      klist
067700141210     c                   kfld                    TASAas
067800141210     c                   kfld                    TASLnp
067900141210     c                   kfld                    TASNrs
068000141210     c                   kfld                    TASNsp
068100010330     C*
068200010330     C* CALCOLA LA DATA CORRENTE
068300130724     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
068400130724     C                   eval      datcor = %dec(%date() : *ISO)
068500000613     C*
068600000613     C                   ENDSR
068700000613     C***
068800040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
068900140923EFRRFP;X;X;X;X;X;X;X;X;X;X;X;X;X;X;EFRPES;EFRVOL;X;X;X;X;X;X;X;X;X;X;X;X;EFRIMP;EFRRIF;
069000140923X;X;EFRNFT;EFRDFT;X;
069100140923
