000100110222      /TITLE Upload via Internet: traduzione in FIVATWWR. (Cliente CHEBANCA - Cod. 0895142)
000200130625     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040714     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700040726     D*------------
000800040726     D* SCHIEDE A PROGRAMMA
000900040726     D*------------
001000040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800010330     D esito           s              1
001900010330     D prmlit          s             10
002000010330     D prmfir          s             10
002100010330     D wrkesito        s                   like(esito)
002200010330     D rrnum           s              6  0 INZ(*zeros)
002300040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002400030715     D*------------------
002500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002600030715     D*------------------
002700030715     D SkSplitFLD      S             10    DIM(100)
002800030715     D SkSplitCSV      S            256    DIM(100)
002900030715     D CharCSV         S              1
003000030715     D CharTXT         S              1
003100030715     D CharNUM         S              1
003200030715     D posDa           S              3  0 INZ(*zeros)
003300030715     D posA            S              3  0 INZ(*zeros)
003400030715     D i               s              3  0 INZ(1)
003500030715     D wGiro           s              1  0 INZ(*zeros)
003600130625     D w50             s             50
003700130625     D wX              s              3  0
003800130625     D num             c                   const('1234567890 ')
003900130528     D wVATNOT_E       s                   LIKE(VATNOT)
004000030822     D*------------------
004100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004200030822     D*------------------
004300030822     D posDaDft        S              3  0 INZ(*zeros)
004400030822     D posADft         S              3  0 INZ(*zeros)
004500030822     D j               s              3  0 INZ(1)
004600030822     D wGiroDft        s              1  0 INZ(*zeros)
004700010330     D*------------------
004800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900010330     D*------------------
005000010330     D WLBDA8          DS                  INZ
005100010330     D  G08DAT                 1      8  0
005200010330     D  G08INV                 9     16  0
005300010330     D  G08ERR                17     17
005400010330     D  G08TGI                18     22  0
005500110222
005600031201     D*------------------
005700031201     D* Costanti
005800031201     D*------------------
005900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006100010330
006200010330
006300010330
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700010601     C*
006800010601     C                   exsr      opeini
006900040714     C                   exsr      rwvat
007000010601     C*
007100010601     C                   seton                                        lr
007200010601
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007800010601     C*--------------------------------------------------------
007900010601     C     PREELA        BEGSR
008000010601     C*
008100010601     C* SE OCCORRE SPEDIRE IN FILIALE
008200010601     C                   if        invfil <> *zeros and
008300010601     C                             flgGiro = '0'
008400010601     C*
008500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008600010601     C                   eval      flgGiro = '1'
008700010601     C*
008800010601     C                   endif
008900010601     C*
009000010601     C                   ENDSR
009100010601     C***
009200010601
009300010601
009400010601
009500010601
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010100010601     C*--------------------------------------------------------
010200010601     C     ENDELA        BEGSR
010300000616     C*
010400010601     C                   ENDSR
010500010601     C***
010600000613
010700010330
010800010601
010900010601
011000010601
011100010330     C*--------------------------------------------------------
011200040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
011300010330     C*--------------------------------------------------------
011400040714     C     RWVAT         BEGSR
011500010330     C*
011600010330     C                   if        not %open(tivin00r)
011700010330     C                   open      tivin00r
011800010330     C                   endif
011900040714     C                   if        not %open(fivatwwr)
012000040714     C                   open      fivatwwr
012100010330     C                   endif
012200010330     C*
012300010604     C                   clear                   §CTROK
012400010604     C                   clear                   §CTRMO
012500010604     C                   clear                   §CTRNO
012600040726     C*
012700040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
012800040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
012900040726     C                   exsr      inzvar
013000040726     C                   exsr      impvat
013100010330     C*
013200030822     C                   do        *HIVAL
013300010330     C*
013400030822     C                   read      tivin00r                               70
013500010618     C*
013600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013700010618     C                   if        vinflg = '1'
013800010618     C                   eval      flgOk = '1'
013900010618     C                   endif
014000010618     C*
014100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014200040510     C                   z-add     *zeros        wLenVINDTA        4 0
014300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
014400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
014500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
014600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014700040510     C                   z-add     1             wFlgVINDTA
014800040510     C                   leave
014900040510     C                   endif
015000040510     C                   enddo
015100040510     C*
015200040218     C                   if        vindta > *blanks AND
015300040510     C                             wFlgVINDTA = 1
015400010330     C                   add       1             rrnum
015500010330     C*
015600010601     C                   if        *in70 = *off and
015700010330     C                             (vinflg = *blanks
015800010330     C                              or vinflg = '0'
015900010330     C                              or vinflg = '2')
016000010330     C*
016100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016200010711     C                   if        vinflg = *blanks or vinflg = '0'
016300010711     C                   clear                   vinmsg
016400010711     C                   endif
016500010601     C*
016600010330     C                   exsr      inzvar
016700040714     C                   exsr      impvat
016800010601     C*
016900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017000040714     C                   eval      depfil = VATLNP
017100010601     C                   exsr      repfil
017200010601     C                   if        depfil = invfil
017300021025     C                   if        vlrpoi = 999
017400040714     C                   move(P)   invfil        VATFGS
017500021025     C                   else
017600040714     C                   move(P)   vlrpoi        VATFGS
017700021025     C                   endif
017800010601     C*
017900010601     C                   exsr      PREELA
018000010601     C*
018100010604     C* Ebbene...
018200010604     C*
018300030822     C  N31              add       1             §CTROK            7 0
018400030822     C   32              add       1             §CTRMO            7 0
018500030822     C   31              add       1             §CTRNO            7 0
018600030822     C                   if        wGiro = 2
018700110222     C*
018800110222     C                   if        not *in30
018900110222     C*
019000130528     C* inserimento tipo record 'E'
019100130528     C                   if        wVATNOT_E <> *blanks
019200130528     C                   eval      VATNOT = wVATNOT_E
019300130528     C                   eval      VATTRC = 'E'
019400110222     C                   if        vatnot <> *blanks
019500040714     C  N31              write     FIVAT000
019600110222     C                   endif
019700130528     C                   endif
019800110222     C                   endif
019900030822     C                   endif
020000010604     C*
020100010604     C                   if        *in31 = *off and
020200010604     C                             *in32 = *off
020300010604     C                   eval      vinflg = '1'
020400010604     C                   else
020500010604     C                   eval      vinflg = '2'
020600010604     C                   endif
020700010604     C                   endif
020800010601     C*
020900010604     C                   endif
021000010604     C*
021100010330     C                   else
021200010330     C                   eval      vinflg = '1'
021300010330     C                   endif
021400010601     C*
021500010601     C  N70              update    tivin000
021600010330     C*
021700030822     C  N70              enddo
021800010601     C
021900010601     C*
022000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022100010601     C                   if        cntNonEl = *zeros or
022200010601     C                             flgMulti = '0'
022300010330     C* Se non ci sono record con errori ...
022400010601     C                   if        §ctrno = 0 and
022500010604     C                             §ctrmo = 0 and
022600010601     C                             flgStato <> '2'
022700010330     C* ... restituisco esito OK.
022800010330     C                   eval      wrkesito = '0'
022900010330     C                   else
023000010330     C                   if        §ctrok > 0
023100010330     C                   eval      wrkesito = '1'
023200010330     C                   else
023300010615     C                   if        flgOk = '0'
023400010615     C                   eval      wrkesito = '2'
023500010615     C                   else
023600010615     C                   eval      wrkesito = '6'
023700010615     C                   endif
023800010330     C                   endif
023900010330     C                   endif
024000010601     C                   else
024100010601     C                   eval      wrkesito = '9'
024200010601     C                   endif
024300010330     C*
024400010330     C                   if        %open(tivin00r)
024500010330     C                   close     tivin00r
024600010330     C                   endif
024700040714     C                   if        %open(fivatwwr)
024800040714     C                   close     fivatwwr
024900010330     C                   endif
025000010601     C*
025100010601     C                   if        vlrpoi <> 999
025200010601     C                   eval      invfil = vlrpoi
025300010601     C                   endif
025400010330     C*
025500010330     C                   if        §ctrok > 0
025600010601     C                             and invfil > *zeros
025700010330     C                   exsr      invio
025800010330     C                   endif
025900010601     C*
026000010618     C                   if        flgGiro = '1'
026100010601     C                   exsr      endela
026200010618     C                   endif
026300010330     C*
026400010330     C                   ENDSR
026500010330     C***
026600010601
026700010601
026800010601
026900010330     C*----------------------------------------------------*
027000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100010330     C*----------------------------------------------------*
027200010330     C     INZVAR        BEGSR
027300010330     C*
027400030822     C* Inizializzo variabili di wrk
027500020204     C                   Z-ADD     *zeros        Num5_0            5 0
027600030822     C*
027700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027800040714     C                   CLEAR                   FIVAT000
027900030822     C                   CLEAR                   SkSplitCSV
028000030822     C                   EVAL      i = 1
028100030822     C                   EVAL      posDa = *zeros
028200030822     C                   EVAL      posA  = *zeros
028300110222     C*
028400130528     C                   MOVEL     *blanks       wVATNOT_E
028500030822     C*
028600030822     C* Reimposto i valori di default
028700030822     C                   EXSR      DEFCAM
028800030822     C*
028900010330     C                   ENDSR
029000010330     C*----------------------------------------------------*
029100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029200010330     C*----------------------------------------------------*
029300010330     C     DEFCAM        BEGSR
029400030822     C*
029500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029600030715     C* e delimitatore testo.
029700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030000030822     C*
030100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
030200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
030300030715     C                   SETON                                        50
030400030715     C                   ELSE
030500030715     C                   SETOFF                                       50
030600030715     C                   ENDIF
030700030822     C*
030800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030900030822     C                   EVAL      posDaDft = 1
031000030822     C                   EVAL      posADft  = 0
031100030822     C                   EVAL      wGiroDft = 0
031200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031300030822     C                             posDaDft > 0
031400030822     C*
031500030822     C* Gestisco il 1° giro
031600030822     C                   IF        wGiroDft = 0
031700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031900030822     C* Incremento il contatore dei "giri"
032000030822     C                   EVAL      wGiroDft = 1
032100030822     C                   ELSE
032200030822     C                   EVAL      posDaDft = posADft
032300030822     C                   ENDIF
032400030822     C* Eseguo lo scan x trovare la fine del campo corrente
032500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032600030822     C*
032700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032900030822     C                   IF        posDaDft > 0 AND
033000030822     C                             posADft  > 0
033100030822     C* CCM
033200030822     C                   IF        %subst(
033300030822     C                             %subst(vlrppt:posDaDft+1:
033400030822     C                             posADft-posDaDft-1):1:3)
033500030822     C                             = 'CCM'
033600030822     C                   EVAL      PiStr=%trim(%subst(
033700030822     C                             %subst(vlrppt:posDaDft+1:
033800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033900030822     C                   EXSR      CHKNUM
034000030822     C                   IF        PiInt=*on
034100040714     C                   Z-ADD     PiVal         VATCCM
034200030822     C                   ENDIF
034300030822     C                   ENDIF
034400030822     C* LNP
034500030822     C                   IF        %subst(
034600030822     C                             %subst(vlrppt:posDaDft+1:
034700030822     C                             posADft-posDaDft-1):1:3)
034800030822     C                             = 'LNP'
034900030822     C                   EVAL      PiStr=%trim(%subst(
035000030822     C                             %subst(vlrppt:posDaDft+1:
035100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035200030822     C                   EXSR      CHKNUM
035300030822     C                   IF        PiInt=*on
035400040714     C                   Z-ADD     PiVal         VATLNP
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700040714     C* TRC
035800030822     C                   IF        %subst(
035900030822     C                             %subst(vlrppt:posDaDft+1:
036000030822     C                             posADft-posDaDft-1):1:3)
036100040714     C                             = 'TRC'
036200040714     C                   EVAL      VATTRC=%trim(%subst(
036300030822     C                             %subst(vlrppt:posDaDft+1:
036400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500030822     C                   ENDIF
036600030822     C* ...
036700030822     C                   ENDIF
036800030822     C                   ENDDO
036900020204     C*
037000010330     C                   ENDSR
037100010607     C*----------------------------------------------------*
037200040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
037300010607     C*----------------------------------------------------*
037400040714     C     IMPVAT        BEGSR
037500010607     C*
037600110222     C                   SETOFF                                       313230
037700030515     C*
037800040714     C                   MOVEL     datcor        VATAAS
037900010607     C*
038000030715     C**********
038100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038200030715     C**********
038300030715     C                   IF        wGiro = *zeros
038400040726     C*
038500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
038600040726     C                   EVAL      depvinDTA = vinDTA
038700040726     C*
038800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
038900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
039000040726     C                                      %trim(intcol(3))
039100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
039200031201     C     minu:maiu     XLATE     vindta        vindta
039300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
039400040324     C                   DOW       posDa <= %len(%trimr(vindta))
039500030715     C*
039600030715     C* Gestisco il 1° campo
039700030715     C                   IF        i = 1
039800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
039900030715     C                   EVAL      posDa = 1
040000030715     C* Eseguo lo scan x trovare la fine del primo campo
040100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040200030715     C* A questo punto "estraggo" il campo corrente
040300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040400030715     C                                                    (posA-posDa))
040500030715     C* X i campi successivi al 1°
040600030715     C                   ELSE
040700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
040800030715     C                   EVAL      posDa = posA + 1
040900030715     C* Eseguo lo scan x trovare la fine del campo corrente
041000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041100030715     C* Gestisco l'ultimo campo
041200030715     C                   IF        posA = *zeros
041300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
041400030715     C* A questo punto "estraggo" il campo corrente
041500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041600030715     C                                                    (posA-posDa))
041700030715     C                   LEAVE
041800030715     C                   ELSE
041900030715     C* A questo punto "estraggo" il campo corrente
042000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042100030715     C                                                    (posA-posDa))
042200030715     C                   ENDIF
042300030715     C                   ENDIF
042400030715     C* Incremento il contatore d campo
042500030715     C                   EVAL      i = i +1
042600030715     C                   ENDDO
042700030715     C*
042800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
042900030715     C* delimitazione testo
043000030715     C                   EVAL      i = 1
043100030715     C                   DOW       i < %elem(SkSplitFLD)
043200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
043300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
043400030715     C                   EVAL      i = i + 1
043500030715     C                   ENDDO
043600030715     C                   EVAL      wGiro = 1
043700040726     C* Ripristino la 1° riga originale
043800040726     C                   EVAL      vinDTA = depvinDTA
043900030715     C                   ELSE
044000030715     C                   EVAL      wGiro = 2
044100031204     C**********
044200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
044300031204     C* d separatore campo
044400031204     C**********
044500040726     C                   EVAL      vindta = %trim(vindta)
044600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
044700040726     C                   EVAL      vindta = %subst(vindta:2)
044800040726     C                   ENDDO
044900031204     C*
045000031204     C                   Z-ADD     *zeros        lunghInput        4 0
045100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
045200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
045300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
045500040726     C                   ENDDO
045600030715     C**********
045700030715     C* Effettuo lo split del campo dati d input
045800030715     C**********
045900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
046000040324     C                   DOW       posDa <= %len(%trimr(vindta))
046100030715     C*
046200030715     C* Gestisco il 1° campo
046300030715     C                   IF        i = 1
046400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046500030715     C                   EVAL      posDa = 1
046600030715     C* Eseguo lo scan x trovare la fine del primo campo
046700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046800030715     C* A questo punto "estraggo" il campo corrente
046900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
047000030715     C                                                    (posA-posDa))
047100030715     C* X i campi successivi al 1°
047200030715     C                   ELSE
047300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047400030715     C                   EVAL      posDa = posA + 1
047500031204     C* Verifico che nn vi sia il campo nn valorizzato
047600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047700031204     C* Se campo nn valorizzato skippo al prossimo
047800031204     C                   EVAL      SkSplitCSV(i) = *blanks
047900031204     C                   EVAL      posA  = posA + 1
048000031204     C                   ELSE
048100030715     C* Eseguo lo scan x trovare la fine del campo corrente
048200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048300030715     C* Gestisco l'ultimo campo
048400030715     C                   IF        posA = *zeros
048500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048600030715     C* A questo punto "estraggo" il campo corrente
048700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048800030715     C                                                    (posA-posDa))
048900030715     C                   LEAVE
049000030715     C                   ELSE
049100030715     C* A questo punto "estraggo" il campo corrente
049200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049300030715     C                                                    (posA-posDa))
049400030715     C                   ENDIF
049500031204     C                   ENDIF
049600030715     C                   ENDIF
049700030715     C* Incremento il contatore d campo
049800030715     C                   EVAL      i = i +1
049900030715     C                   ENDDO
050000030715     C*
050100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
050200030715     C* delimitazione testo
050300030715     C                   EVAL      i = 1
050400030715     C                   DOW       i < %elem(SkSplitCSV)
050500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
050600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050700030715     C                   EVAL      i = i + 1
050800030715     C                   ENDDO
050900030715     C*
051000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
051100030715     C                   EVAL      i = 1
051200030715     C                   DOW       i < %elem(SkSplitFLD)
051300030715     C*
051400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
051500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
051600030715     C                             SkSplitCSV(i) <> *blanks
051700030721     C*** ATB
051800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
051900040714     C                   EVAL      VATATB = SkSplitCSV(i)
052000030721     C                   ENDIF
052100040714     C*** TRC
052200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
052300040714     C                   EVAL      VATTRC = SkSplitCSV(i)
052400030715     C                   ENDIF
052500040714     C*** NOT
052600040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
052700040714     C                   EVAL      VATNOT = SkSplitCSV(i)
052800030721     C                   ENDIF
052900030721     C*
053000030721     C* Reperisco quindi i campi numerici...
053100030721     C*** NSP
053200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
053300030721     C                   EVAL      PiStr=SkSplitCSV(i)
053400030721     C                   EXSR      CHKNUM
053500040726     C                   IF        PiInt=*on
053600040726     C                   Z-ADD     PiVal         VATNSP
053700040802     C*                  IF        PiVal>9999999
053800040802     C*                  SETON                                        32
053900040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
054000040802     C*                            + ' ' + 'VATNSP'
054100040802     C*                  ENDIF
054200030721     C                   ELSE
054300071002     C                   SETON                                        32
054400040714     C                   EVAL      VATNSP = *zeros
054500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
054600040714     C                             + ' ' + 'VATNSP'
054700030721     C                   ENDIF
054800030721     C                   ENDIF
054900030721     C*** CCM
055000040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
055100030721     C                   EVAL      PiStr=SkSplitCSV(i)
055200030721     C                   EXSR      CHKNUM
055300030721     C                   IF        PiInt=*on      AND
055400030721     C                             PiVal<=9999999 AND
055500030721     C                             PiVal>*zeros
055600040714     C                   Z-ADD     PiVal         VATCCM
055700030721     C                   ELSE
055800030721     C                   SETON                                        32
055900040714     C                   EVAL      VATCCM = *zeros
056000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056100040714     C                             + ' ' + 'VATCCM'
056200030721     C                   ENDIF
056300030721     C                   ENDIF
056400030721     C*** LNP
056500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
056600030721     C                   EVAL      PiStr=SkSplitCSV(i)
056700030721     C                   EXSR      CHKNUM
056800030721     C                   IF        PiInt=*on    AND
056900030721     C                             PiVal<=999   AND
057000030721     C                             PiVal>*zeros
057100040714     C                   Z-ADD     PiVal         VATLNP
057200030721     C                   ELSE
057300030721     C                   SETON                                        32
057400040714     C                   EVAL      VATLNP = *zeros
057500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057600040714     C                             + ' ' + 'VATLNP'
057700030721     C                   ENDIF
057800030721     C                   ENDIF
057900030721     C*** NRS
058000040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
058100030721     C                   EVAL      PiStr=SkSplitCSV(i)
058200030721     C                   EXSR      CHKNUM
058300030721     C                   IF        PiInt=*on AND
058400030721     C                             PiVal<=99
058500040714     C                   Z-ADD     PiVal         VATNRS
058600030721     C                   ELSE
058700030721     C                   SETON                                        32
058800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058900040714     C                             + ' ' + 'VATNRS'
059000030721     C                   ENDIF
059100030721     C                   ENDIF
059200130528     C*** VATNOT_E
059300130528     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
059400130528     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
059500130528     C                   ENDIF
059600110222     C*
059700110222     C***  ===>  Gestione campi "particolari" dentro ciclo
059800130625      *** W_VATNSP: è sia VABRMA che VABNSP ma solo gli ultimi char a dx numerici
059900130625     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNSP'
060000130625     C                   EVAL      w50 = %trim(SkSplitCSV(i))
060100130625     C* trovo l'ultimo carattere non numerico e non blank
060200130625     C                   EVAL      wX = %checkR(num : W50) + 1
060300130625     C                   EVAL      VATNSP = %dec(
060400130625     C                                   %subst(w50:wX:
060500130625     C                                    %scan(' ':w50)-wX)
060600130625     C                                                 :7:0)
060700130528     C                   ENDIF
060800110222     C*
060900110222     C***  <===  -----------------------------------------
061000030715     C*
061100030715     C                   ENDIF
061200030715     C                   EVAL      i = i + 1
061300030715     C                   ENDDO
061400040726     C*
061500040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
061600040726     C                   IF        VATNSP = *zeros
061700040726     C                   SETON                                        31
061800040726     C                   ENDIF
061900040726     C*
062000040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
062100040726     C                   IF        VATLNP = *zeros
062200040726     C                   SETON                                        31
062300040726     C                   ENDIF
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
065300010330
065400010601
065500010601
065600010601
065700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
065800010601     C     repfil        BEGSR
065900010601     C*
066000010601     C                   if        invfil = *zeros and
066100010601     C                             depfil > *zeros and
066200010629     C                             (vinflg = *blanks or
066300010629     C                              vinflg = *zeros)
066400010601     C
066500010601     C                   eval      invfil = depfil
066600010601     C                   endif
066700010601     C*
066800010601     C                   if        depfil <> invfil and
066900010601     C                             invfil > *zeros
067000010601     C                   eval      flgMulti = '1'
067100010601     C                   if        vinflg = *blanks
067200010601     C                   add       1             cntNonEl
067300010601     C                   endif
067400010601     C                   endif
067500010601     C*
067600010601     C                   if        vinflg = '2'
067700010601     C                   eval      flgStato = '2'
067800010601     C                   endif
067900010601     C*
068000010601     C                   ENDSR
068100010601     C***
068200010601
068300010601
068400010601
068500010330
068600010330
068700010330
068800990920      /TITLE Invio dei dati al punto operativo.
068900000613     C     invio         BEGSR
069000990920     C*
069100990920     C                   reset                   dscmz
069200010601     C                   move      invfil        cmzdst
069300990920     C                   eval      cmzfld = vlrfou
069400990920     C                   eval      cmzmbd = vlrhdl
069500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
069600000710     C                   if        prmfir = *blanks
069700040714     C                   eval      cmzfla = 'FIVAT00F'
069800040714     C                   eval      cmzmba = 'FIVAT00F'
069900000710     C                   else
070000000710     C                   eval      cmzfla = prmfir
070100000710     C                   eval      cmzmba = prmfir
070200000710     C                   endif
070300990920     C                   eval      cmznrr = *zeros
070400990920     C                   move      §ctrok        cmznrr
070500021018     C                   eval      cmzlba = vlrfl1
070600990920     C                   call(e)   'TIS711C'
070700990920     C                   parm                    dscmz
070800990921     C                   parm      *blanks       esito
070900990920     C                   if        %error
071000990920     C                             or cmzerr = '1'
071100990921     C                             or esito  = '1'
071200000710     C                   eval      wrkesito = '3'
071300990920     C                   endif
071400990920     C*
071500000613     C                   ENDSR
071600990910
071700010601
071800010601
071900010601
072000010601
072100010601      /TITLE Invio dei dati al punto operativo.
072200010601     C     opeini        BEGSR
072300010601     C*
072400010601     C* Inizializzo flag e contatori operativi
072500010601     C                   movel     '0'           flgGiro           1
072600010601     C                   movel     '0'           flgMulti          1
072700010601     C                   movel     '1'           flgStato          1
072800010615     C                   movel     '0'           flgOk             1
072900010601     C                   z-add     *zeros        cntNonEl         10 0
073000010601     C                   z-add     *zeros        depfil            3 0
073100010601     C                   z-add     *zeros        invfil            3 0
073200010601     C*
073300010601     C                   ENDSR
073400010601     C***
073500010601
073600010601
073700050421
073800050421
073900050421     C     *pssr         BEGSR
074000050421     C*
074100050421     C                   eval      wrkesito = '2'
074200050421     C*
074300050421     C                   ENDSR     '*CANCL'
074400050421     C***
074500050421
074600010601
074700010330
074800010330
074900000613     C     *inzsr        BEGSR
075000990910     C*
075100990910     C     *entry        plist
075200990920     C                   parm                    tivlrds
075300990921     C                   parm      wrkesito      esito
075400000724     C                   parm                    prmlit
075500000710     C                   parm                    prmfir
075600010330     C*
075700010330     C* CALCOLA LA DATA CORRENTE
075800010330     C                   time                    wn14             14 0
075900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
076000010330     C                   z-add     wn8           g08dat
076100010330     C                   z-add     *zeros        g08inv
076200010330     C                   movel     '0'           g08err
076300010330     C                   call      'XSRDA8'
076400010330     C                   parm                    wlbda8
076500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076600000613     C*
076700000613     C                   ENDSR
076800000613     C***
076900040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
077000130528X;X;X;W_VATNSP;X;X;X;X;X;X;X;X;X;X;X;X;X;X;VATNOT_E;X;X;X;X;X;
077100130528
077200130528
