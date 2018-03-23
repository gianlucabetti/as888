000100140821      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200140821      *
000300140821      * PARTICOLARITA':
000400140821      *
000500140821     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600140821     H decedit('0,')
000700000313     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900040714     FFIVATwwr  O    E             DISK    usropn
001000000313     D*
001100040726     D*------------
001200040726     D* SCHIEDE A PROGRAMMA
001300040726     D*------------
001400040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001500010330     D*----------------------------------------------------
001600010330     D* DICHIARAZIOINE VARIABILI DI WRK
001700010330     D*----------------------------------------------------
001800010330     D dscmz         e ds                  inz
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200010330     D esito           s              1
002300010330     D prmlit          s             10
002400010330     D prmfir          s             10
002500010330     D wrkesito        s                   like(esito)
002600010330     D rrnum           s              6  0 INZ(*zeros)
002700040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002800030715     D*------------------
002900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003000030715     D*------------------
003100030715     D SkSplitFLD      S             10    DIM(100)
003200030715     D SkSplitCSV      S            256    DIM(100)
003300030715     D CharCSV         S              1
003400030715     D CharTXT         S              1
003500030715     D CharNUM         S              1
003600030715     D posDa           S              3  0 INZ(*zeros)
003700030715     D posA            S              3  0 INZ(*zeros)
003800030715     D i               s              3  0 INZ(1)
003900030715     D wGiro           s              1  0 INZ(*zeros)
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
005500110222     D*
005600110222
005700031201     D*------------------
005800031201     D* Costanti
005900031201     D*------------------
006000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006200010330
006300010330
006400010330
006500120309     C*
006600120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006700120309     C
006800120309     C/EXEC SQL
006900120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007000120309     C/END-EXEC
007100000913     C                   reset                   rrnum
007200990921     C                   reset                   esito
007300990921     C                   reset                   wrkesito
007400010601     C*
007500010601     C                   exsr      opeini
007600040714     C                   exsr      rwvat
007700010601     C*
007800010601     C                   seton                                        lr
007900010601
008000010601
008100010601
008200010601
008300010601     C*--------------------------------------------------------
008400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010601     C*--------------------------------------------------------
008600010601     C     PREELA        BEGSR
008700010601     C*
008800010601     C* SE OCCORRE SPEDIRE IN FILIALE
008900010601     C                   if        invfil <> *zeros and
009000010601     C                             flgGiro = '0'
009100010601     C*
009200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010601     C                   eval      flgGiro = '1'
009400010601     C*
009500010601     C                   endif
009600010601     C*
009700010601     C                   ENDSR
009800010601     C***
009900010601
010000010601
010100010601
010200010601
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010601     C*--------------------------------------------------------
010900010601     C     ENDELA        BEGSR
011000000616     C*
011100010601     C                   ENDSR
011200010601     C***
011300000613
011400010330
011500010601
011600010601
011700010601
011800010330     C*--------------------------------------------------------
011900040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
012000010330     C*--------------------------------------------------------
012100040714     C     RWVAT         BEGSR
012200010330     C*
012300010330     C                   if        not %open(tivin00r)
012400010330     C                   open      tivin00r
012500010330     C                   endif
012600040714     C                   if        not %open(fivatwwr)
012700040714     C                   open      fivatwwr
012800010330     C                   endif
012900010330     C*
013000010604     C                   clear                   §CTROK
013100010604     C                   clear                   §CTRMO
013200010604     C                   clear                   §CTRNO
013300040726     C*
013400040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
013500040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
013600040726     C                   exsr      inzvar
013700040726     C                   exsr      impvat
013800010330     C*
013900030822     C                   do        *HIVAL
014000010330     C*
014100030822     C                   read      tivin00r                               70
014200010618     C*
014300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014400010618     C                   if        vinflg = '1'
014500010618     C                   eval      flgOk = '1'
014600010618     C                   endif
014700010618     C*
014800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014900040510     C                   z-add     *zeros        wLenVINDTA        4 0
015000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015400040510     C                   z-add     1             wFlgVINDTA
015500040510     C                   leave
015600040510     C                   endif
015700040510     C                   enddo
015800040510     C*
015900040218     C                   if        vindta > *blanks AND
016000040510     C                             wFlgVINDTA = 1
016100010330     C                   add       1             rrnum
016200010330     C*
016300010601     C                   if        *in70 = *off and
016400010330     C                             (vinflg = *blanks
016500010330     C                              or vinflg = '0'
016600010330     C                              or vinflg = '2')
016700010330     C*
016800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016900010711     C                   if        vinflg = *blanks or vinflg = '0'
017000010711     C                   clear                   vinmsg
017100010711     C                   endif
017200010601     C*
017300010330     C                   exsr      inzvar
017400040714     C                   exsr      impvat
017500010601     C*
017600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017700040714     C                   eval      depfil = VATLNP
017800010601     C                   exsr      repfil
017900010601     C                   if        depfil = invfil
018000021025     C                   if        vlrpoi = 999
018100040714     C                   move(P)   invfil        VATFGS
018200021025     C                   else
018300040714     C                   move(P)   vlrpoi        VATFGS
018400021025     C                   endif
018500010601     C*
018600010601     C                   exsr      PREELA
018700010601     C*
018800010604     C* Ebbene...
018900010604     C*
019000030822     C  N31              add       1             §CTROK            7 0
019100030822     C   32              add       1             §CTRMO            7 0
019200030822     C   31              add       1             §CTRNO            7 0
019300030822     C                   if        wGiro = 2
019400110222     C*
019500110222     C                   if        not *in30
019600110222     C*
019700140821     C* tipo record 'E'
019800140821     C                   eval      VATTRC = 'E'
019900140821     C                   eval      VATNOT = %trim(wNOT_E)
020000110222     C                   if        vatnot <> *blanks
020100110222     C  N31              write     FIVAT000
020200110222     C                   endif
020300110222     C                   endif
020400030822     C                   endif
020500010604     C*
020600010604     C                   if        *in31 = *off and
020700010604     C                             *in32 = *off
020800010604     C                   eval      vinflg = '1'
020900010604     C                   else
021000010604     C                   eval      vinflg = '2'
021100010604     C                   endif
021200010604     C                   endif
021300010601     C*
021400010604     C                   endif
021500010604     C*
021600010330     C                   else
021700010330     C                   eval      vinflg = '1'
021800010330     C                   endif
021900010601     C*
022000010601     C  N70              update    tivin000
022100010330     C*
022200030822     C  N70              enddo
022300010601     C
022400010601     C*
022500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022600010601     C                   if        cntNonEl = *zeros or
022700010601     C                             flgMulti = '0'
022800010330     C* Se non ci sono record con errori ...
022900010601     C                   if        §ctrno = 0 and
023000010604     C                             §ctrmo = 0 and
023100010601     C                             flgStato <> '2'
023200010330     C* ... restituisco esito OK.
023300010330     C                   eval      wrkesito = '0'
023400010330     C                   else
023500010330     C                   if        §ctrok > 0
023600010330     C                   eval      wrkesito = '1'
023700010330     C                   else
023800010615     C                   if        flgOk = '0'
023900010615     C                   eval      wrkesito = '2'
024000010615     C                   else
024100010615     C                   eval      wrkesito = '6'
024200010615     C                   endif
024300010330     C                   endif
024400010330     C                   endif
024500010601     C                   else
024600010601     C                   eval      wrkesito = '9'
024700010601     C                   endif
024800010330     C*
024900010330     C                   if        %open(tivin00r)
025000010330     C                   close     tivin00r
025100010330     C                   endif
025200040714     C                   if        %open(fivatwwr)
025300040714     C                   close     fivatwwr
025400010330     C                   endif
025500010601     C*
025600010601     C                   if        vlrpoi <> 999
025700010601     C                   eval      invfil = vlrpoi
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        §ctrok > 0
026100010601     C                             and invfil > *zeros
026200010330     C                   exsr      invio
026300010330     C                   endif
026400010601     C*
026500010618     C                   if        flgGiro = '1'
026600010601     C                   exsr      endela
026700010618     C                   endif
026800010330     C*
026900010330     C                   ENDSR
027000010330     C***
027100010601
027200010601
027300010601
027400010330     C*----------------------------------------------------*
027500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027600010330     C*----------------------------------------------------*
027700010330     C     INZVAR        BEGSR
027800010330     C*
027900030822     C* Inizializzo variabili di wrk
028000020204     C                   Z-ADD     *zeros        Num5_0            5 0
028100030822     C*
028200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
028300040714     C                   CLEAR                   FIVAT000
028400030822     C                   CLEAR                   SkSplitCSV
028500030822     C                   EVAL      i = 1
028600030822     C                   EVAL      posDa = *zeros
028700030822     C                   EVAL      posA  = *zeros
028800110222     C*
028900140821     C                   MOVEL     *blanks       wNOT_E           35
029000030822     C*
029100030822     C* Reimposto i valori di default
029200030822     C                   EXSR      DEFCAM
029300030822     C*
029400010330     C                   ENDSR
029500010330     C*----------------------------------------------------*
029600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029700010330     C*----------------------------------------------------*
029800010330     C     DEFCAM        BEGSR
029900030822     C*
030000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030100030715     C* e delimitatore testo.
030200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
030300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030500030822     C*
030600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
030700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
030800030715     C                   SETON                                        50
030900030715     C                   ELSE
031000030715     C                   SETOFF                                       50
031100030715     C                   ENDIF
031200030822     C*
031300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031400030822     C                   EVAL      posDaDft = 1
031500030822     C                   EVAL      posADft  = 0
031600030822     C                   EVAL      wGiroDft = 0
031700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031800030822     C                             posDaDft > 0
031900030822     C*
032000030822     C* Gestisco il 1° giro
032100030822     C                   IF        wGiroDft = 0
032200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
032300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032400030822     C* Incremento il contatore dei "giri"
032500030822     C                   EVAL      wGiroDft = 1
032600030822     C                   ELSE
032700030822     C                   EVAL      posDaDft = posADft
032800030822     C                   ENDIF
032900030822     C* Eseguo lo scan x trovare la fine del campo corrente
033000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033100030822     C*
033200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
033300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
033400030822     C                   IF        posDaDft > 0 AND
033500030822     C                             posADft  > 0
033600030822     C* CCM
033700030822     C                   IF        %subst(
033800030822     C                             %subst(vlrppt:posDaDft+1:
033900030822     C                             posADft-posDaDft-1):1:3)
034000030822     C                             = 'CCM'
034100030822     C                   EVAL      PiStr=%trim(%subst(
034200030822     C                             %subst(vlrppt:posDaDft+1:
034300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034400030822     C                   EXSR      CHKNUM
034500030822     C                   IF        PiInt=*on
034600040714     C                   Z-ADD     PiVal         VATCCM
034700030822     C                   ENDIF
034800030822     C                   ENDIF
034900030822     C* LNP
035000030822     C                   IF        %subst(
035100030822     C                             %subst(vlrppt:posDaDft+1:
035200030822     C                             posADft-posDaDft-1):1:3)
035300030822     C                             = 'LNP'
035400030822     C                   EVAL      PiStr=%trim(%subst(
035500030822     C                             %subst(vlrppt:posDaDft+1:
035600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035700030822     C                   EXSR      CHKNUM
035800030822     C                   IF        PiInt=*on
035900040714     C                   Z-ADD     PiVal         VATLNP
036000030822     C                   ENDIF
036100030822     C                   ENDIF
036200040714     C* TRC
036300030822     C                   IF        %subst(
036400030822     C                             %subst(vlrppt:posDaDft+1:
036500030822     C                             posADft-posDaDft-1):1:3)
036600040714     C                             = 'TRC'
036700040714     C                   EVAL      VATTRC=%trim(%subst(
036800030822     C                             %subst(vlrppt:posDaDft+1:
036900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037000030822     C                   ENDIF
037100030822     C* ...
037200030822     C                   ENDIF
037300030822     C                   ENDDO
037400020204     C*
037500010330     C                   ENDSR
037600010607     C*----------------------------------------------------*
037700040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
037800010607     C*----------------------------------------------------*
037900040714     C     IMPVAT        BEGSR
038000010607     C*
038100110222     C                   SETOFF                                       313230
038200030515     C*
038300040714     C                   MOVEL     datcor        VATAAS
038400010607     C*
038500030715     C**********
038600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038700030715     C**********
038800030715     C                   IF        wGiro = *zeros
038900040726     C*
039000040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
039100040726     C                   EVAL      depvinDTA = vinDTA
039200040726     C*
039300040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
039400040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
039500040726     C                                      %trim(intcol(3))
039600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
039700031201     C     minu:maiu     XLATE     vindta        vindta
039800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
039900040324     C                   DOW       posDa <= %len(%trimr(vindta))
040000030715     C*
040100030715     C* Gestisco il 1° campo
040200030715     C                   IF        i = 1
040300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
040400030715     C                   EVAL      posDa = 1
040500030715     C* Eseguo lo scan x trovare la fine del primo campo
040600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040700030715     C* A questo punto "estraggo" il campo corrente
040800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040900030715     C                                                    (posA-posDa))
041000030715     C* X i campi successivi al 1°
041100030715     C                   ELSE
041200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
041300030715     C                   EVAL      posDa = posA + 1
041400030715     C* Eseguo lo scan x trovare la fine del campo corrente
041500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041600030715     C* Gestisco l'ultimo campo
041700030715     C                   IF        posA = *zeros
041800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
041900030715     C* A questo punto "estraggo" il campo corrente
042000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042100030715     C                                                    (posA-posDa))
042200030715     C                   LEAVE
042300030715     C                   ELSE
042400030715     C* A questo punto "estraggo" il campo corrente
042500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042600030715     C                                                    (posA-posDa))
042700030715     C                   ENDIF
042800030715     C                   ENDIF
042900030715     C* Incremento il contatore d campo
043000030715     C                   EVAL      i = i +1
043100030715     C                   ENDDO
043200030715     C*
043300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
043400030715     C* delimitazione testo
043500030715     C                   EVAL      i = 1
043600030715     C                   DOW       i < %elem(SkSplitFLD)
043700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
043800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
043900030715     C                   EVAL      i = i + 1
044000030715     C                   ENDDO
044100030715     C                   EVAL      wGiro = 1
044200040726     C* Ripristino la 1° riga originale
044300040726     C                   EVAL      vinDTA = depvinDTA
044400030715     C                   ELSE
044500030715     C                   EVAL      wGiro = 2
044600031204     C**********
044700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
044800031204     C* d separatore campo
044900031204     C**********
045000040726     C                   EVAL      vindta = %trim(vindta)
045100040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
045200040726     C                   EVAL      vindta = %subst(vindta:2)
045300040726     C                   ENDDO
045400031204     C*
045500031204     C                   Z-ADD     *zeros        lunghInput        4 0
045600040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
045700040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
045800040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045900040726     C                   EVAL      lunghInput = %len(%trim(vindta))
046000040726     C                   ENDDO
046100030715     C**********
046200030715     C* Effettuo lo split del campo dati d input
046300030715     C**********
046400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
046500040324     C                   DOW       posDa <= %len(%trimr(vindta))
046600030715     C*
046700030715     C* Gestisco il 1° campo
046800030715     C                   IF        i = 1
046900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
047000030715     C                   EVAL      posDa = 1
047100030715     C* Eseguo lo scan x trovare la fine del primo campo
047200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047300030715     C* A questo punto "estraggo" il campo corrente
047400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
047500030715     C                                                    (posA-posDa))
047600030715     C* X i campi successivi al 1°
047700030715     C                   ELSE
047800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047900030715     C                   EVAL      posDa = posA + 1
048000031204     C* Verifico che nn vi sia il campo nn valorizzato
048100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
048200031204     C* Se campo nn valorizzato skippo al prossimo
048300031204     C                   EVAL      SkSplitCSV(i) = *blanks
048400031204     C                   EVAL      posA  = posA + 1
048500031204     C                   ELSE
048600030715     C* Eseguo lo scan x trovare la fine del campo corrente
048700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048800030715     C* Gestisco l'ultimo campo
048900030715     C                   IF        posA = *zeros
049000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
049100030715     C* A questo punto "estraggo" il campo corrente
049200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049300030715     C                                                    (posA-posDa))
049400030715     C                   LEAVE
049500030715     C                   ELSE
049600030715     C* A questo punto "estraggo" il campo corrente
049700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049800030715     C                                                    (posA-posDa))
049900030715     C                   ENDIF
050000031204     C                   ENDIF
050100030715     C                   ENDIF
050200030715     C* Incremento il contatore d campo
050300030715     C                   EVAL      i = i +1
050400030715     C                   ENDDO
050500030715     C*
050600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
050700030715     C* delimitazione testo
050800030715     C                   EVAL      i = 1
050900030715     C                   DOW       i < %elem(SkSplitCSV)
051000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
051100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
051200030715     C                   EVAL      i = i + 1
051300030715     C                   ENDDO
051400030715     C*
051500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
051600030715     C                   EVAL      i = 1
051700030715     C                   DOW       i < %elem(SkSplitFLD)
051800030715     C*
051900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
052000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
052100030715     C                             SkSplitCSV(i) <> *blanks
052200030721     C*** ATB
052300040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
052400040714     C                   EVAL      VATATB = SkSplitCSV(i)
052500030721     C                   ENDIF
052600040714     C*** TRC
052700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
052800040714     C                   EVAL      VATTRC = SkSplitCSV(i)
052900030715     C                   ENDIF
053000040714     C*** NOT
053100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
053200040714     C                   EVAL      VATNOT = SkSplitCSV(i)
053300030721     C                   ENDIF
053400030721     C*
053500030721     C* Reperisco quindi i campi numerici...
053600030721     C*** NSP
053700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
053800030721     C                   EVAL      PiStr=SkSplitCSV(i)
053900030721     C                   EXSR      CHKNUM
054000040726     C                   IF        PiInt=*on
054100040726     C                   Z-ADD     PiVal         VATNSP
054200040802     C*                  IF        PiVal>9999999
054300040802     C*                  SETON                                        32
054400040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
054500040802     C*                            + ' ' + 'VATNSP'
054600040802     C*                  ENDIF
054700030721     C                   ELSE
054800071002     C                   SETON                                        32
054900040714     C                   EVAL      VATNSP = *zeros
055000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
055100040714     C                             + ' ' + 'VATNSP'
055200030721     C                   ENDIF
055300030721     C                   ENDIF
055400030721     C*** CCM
055500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
055600030721     C                   EVAL      PiStr=SkSplitCSV(i)
055700030721     C                   EXSR      CHKNUM
055800030721     C                   IF        PiInt=*on      AND
055900030721     C                             PiVal<=9999999 AND
056000030721     C                             PiVal>*zeros
056100040714     C                   Z-ADD     PiVal         VATCCM
056200030721     C                   ELSE
056300030721     C                   SETON                                        32
056400040714     C                   EVAL      VATCCM = *zeros
056500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056600040714     C                             + ' ' + 'VATCCM'
056700030721     C                   ENDIF
056800030721     C                   ENDIF
056900030721     C*** LNP
057000040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
057100030721     C                   EVAL      PiStr=SkSplitCSV(i)
057200030721     C                   EXSR      CHKNUM
057300030721     C                   IF        PiInt=*on    AND
057400030721     C                             PiVal<=999   AND
057500030721     C                             PiVal>*zeros
057600040714     C                   Z-ADD     PiVal         VATLNP
057700030721     C                   ELSE
057800030721     C                   SETON                                        32
057900040714     C                   EVAL      VATLNP = *zeros
058000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058100040714     C                             + ' ' + 'VATLNP'
058200030721     C                   ENDIF
058300030721     C                   ENDIF
058400030721     C*** NRS
058500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
058600030721     C                   EVAL      PiStr=SkSplitCSV(i)
058700030721     C                   EXSR      CHKNUM
058800030721     C                   IF        PiInt=*on AND
058900030721     C                             PiVal<=99
059000040714     C                   Z-ADD     PiVal         VATNRS
059100030721     C                   ELSE
059200030721     C                   SETON                                        32
059300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
059400040714     C                             + ' ' + 'VATNRS'
059500030721     C                   ENDIF
059600030721     C                   ENDIF
059700110222     C*
059800110222     C***  ===>  Gestione campi "particolari" dentro ciclo
059900140821     C*** WNOT_E
060000140821     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
060100140821     C                   EVAL      wNOT_E = %trim(SkSplitCSV(i))
060200110222     C                   ENDIF
060300110222     C*
060400110222     C***  <===  -----------------------------------------
060500030715     C*
060600030715     C                   ENDIF
060700030715     C                   EVAL      i = i + 1
060800030715     C                   ENDDO
060900040726     C*
061000040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
061100040726     C                   IF        VATNSP = *zeros
061200040726     C                   SETON                                        31
061300040726     C                   ENDIF
061400040726     C*
061500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
061600040726     C                   IF        VATLNP = *zeros
061700040726     C                   SETON                                        31
061800040726     C                   ENDIF
061900020204     C*
062000030715     C                   ENDIF
062100020204     C*
062200010607     C                   ENDSR
062300010607     C*----------------------------------------------------*
062400040714
062500010330
062600010330
062700010330     C*----------------------------------------------------*
062800010330     C*  CONTROLLO NUMERICITA' CAMPI
062900010330     C*----------------------------------------------------*
063000010330     C     CHKNUM        BEGSR
063100010330     C*
063200010606     C                   IF        PiDecChr = *blanks
063300030715     C                   EVAL      PiDecChr = CharNUM
063400010606     C                   ENDIF
063500010606     C*
063600010606     C                   CALL(e)   'ISNUMERIC'
063700010330     C                   PARM                    PiStr            30
063800010606     C                   PARM                    PiDecChr          1
063900010330     C                   PARM      *ZEROS        PiVal            30 9
064000010330     C                   PARM      '0'           PiInt             1
064100010330     C                   PARM      '0'           PiNum             1
064200010330     C                   IF        %error
064300010606     C                   EVAL      PiNum=*off
064400010330     C                   ENDIF
064500010330     C*
064600010330     C                   ENDSR
064700010330     C***
064800010330
064900010601
065000010601
065100010601
065200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
065300010601     C     repfil        BEGSR
065400010601     C*
065500010601     C                   if        invfil = *zeros and
065600010601     C                             depfil > *zeros and
065700010629     C                             (vinflg = *blanks or
065800010629     C                              vinflg = *zeros)
065900010601     C
066000010601     C                   eval      invfil = depfil
066100010601     C                   endif
066200010601     C*
066300010601     C                   if        depfil <> invfil and
066400010601     C                             invfil > *zeros
066500010601     C                   eval      flgMulti = '1'
066600010601     C                   if        vinflg = *blanks
066700010601     C                   add       1             cntNonEl
066800010601     C                   endif
066900010601     C                   endif
067000010601     C*
067100010601     C                   if        vinflg = '2'
067200010601     C                   eval      flgStato = '2'
067300010601     C                   endif
067400010601     C*
067500010601     C                   ENDSR
067600010601     C***
067700010601
067800010601
067900010601
068000010330
068100010330
068200010330
068300990920      /TITLE Invio dei dati al punto operativo.
068400000613     C     invio         BEGSR
068500990920     C*
068600990920     C                   reset                   dscmz
068700010601     C                   move      invfil        cmzdst
068800990920     C                   eval      cmzfld = vlrfou
068900990920     C                   eval      cmzmbd = vlrhdl
069000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
069100000710     C                   if        prmfir = *blanks
069200040714     C                   eval      cmzfla = 'FIVAT00F'
069300040714     C                   eval      cmzmba = 'FIVAT00F'
069400000710     C                   else
069500000710     C                   eval      cmzfla = prmfir
069600000710     C                   eval      cmzmba = prmfir
069700000710     C                   endif
069800990920     C                   eval      cmznrr = *zeros
069900990920     C                   move      §ctrok        cmznrr
070000021018     C                   eval      cmzlba = vlrfl1
070100990920     C                   call(e)   'TIS711C'
070200990920     C                   parm                    dscmz
070300990921     C                   parm      *blanks       esito
070400990920     C                   if        %error
070500990920     C                             or cmzerr = '1'
070600990921     C                             or esito  = '1'
070700000710     C                   eval      wrkesito = '3'
070800990920     C                   endif
070900990920     C*
071000000613     C                   ENDSR
071100990910
071200010601
071300010601
071400010601
071500010601
071600010601      /TITLE Invio dei dati al punto operativo.
071700010601     C     opeini        BEGSR
071800010601     C*
071900010601     C* Inizializzo flag e contatori operativi
072000010601     C                   movel     '0'           flgGiro           1
072100010601     C                   movel     '0'           flgMulti          1
072200010601     C                   movel     '1'           flgStato          1
072300010615     C                   movel     '0'           flgOk             1
072400010601     C                   z-add     *zeros        cntNonEl         10 0
072500010601     C                   z-add     *zeros        depfil            3 0
072600010601     C                   z-add     *zeros        invfil            3 0
072700010601     C*
072800010601     C                   ENDSR
072900010601     C***
073000010601
073100010601
073200050421
073300050421
073400050421     C     *pssr         BEGSR
073500050421     C*
073600050421     C                   eval      wrkesito = '2'
073700050421     C*
073800050421     C                   ENDSR     '*CANCL'
073900050421     C***
074000050421
074100010601
074200010330
074300010330
074400000613     C     *inzsr        BEGSR
074500990910     C*
074600990910     C     *entry        plist
074700990920     C                   parm                    tivlrds
074800990921     C                   parm      wrkesito      esito
074900000724     C                   parm                    prmlit
075000000710     C                   parm                    prmfir
075100010330     C*
075200010330     C* CALCOLA LA DATA CORRENTE
075300010330     C                   time                    wn14             14 0
075400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075500010330     C                   z-add     wn8           g08dat
075600010330     C                   z-add     *zeros        g08inv
075700010330     C                   movel     '0'           g08err
075800010330     C                   call      'XSRDA8'
075900010330     C                   parm                    wlbda8
076000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076100000613     C*
076200000613     C                   ENDSR
076300000613     C***
076400040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
076500140821VATNSP;X;X;VATNOT_E;
076600140821
076700040726
