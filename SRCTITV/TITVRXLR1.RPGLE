000100051111      /TITLE Upload via Internet: traduzione in FNVAR00R.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500051111     FFNVAR00r  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000051111     D tibs55ds      e ds                  inz
001100051111     D kpjba         e ds                  inz
001200010330     D dscmz         e ds                  inz
001300010330     D psds           sds
001400010330     D  procname         *PROC
001500010330     D tivlrds       e ds                  extname(tivlr00f)
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100030715     D*------------------
002200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002300030715     D*------------------
002400030715     D SkSplitFLD      S             10    DIM(100)
002500030715     D SkSplitCSV      S            256    DIM(100)
002600030715     D CharCSV         S              1
002700030715     D CharTXT         S              1
002800030715     D CharNUM         S              1
002900090403     D CharSOS         S              1
003000030715     D posDa           S              3  0 INZ(*zeros)
003100030715     D posA            S              3  0 INZ(*zeros)
003200030715     D i               s              3  0 INZ(1)
003300030715     D wGiro           s              1  0 INZ(*zeros)
003400030822     D*------------------
003500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003600030822     D*------------------
003700030822     D posDaDft        S              3  0 INZ(*zeros)
003800030822     D posADft         S              3  0 INZ(*zeros)
003900030822     D j               s              3  0 INZ(1)
004000030822     D wGiroDft        s              1  0 INZ(*zeros)
004100010330     D*------------------
004200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300010330     D*------------------
004400010330     D WLBDA8          DS                  INZ
004500010330     D  G08DAT                 1      8  0
004600010330     D  G08INV                 9     16  0
004700010330     D  G08ERR                17     17
004800010330     D  G08TGI                18     22  0
004900031201     D*------------------
005000031201     D* Costanti
005100031201     D*------------------
005200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005400010330
005500010330
005600010330
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200050303     C                   exsr      rwvar
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
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
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
008800010601
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009400010601     C*--------------------------------------------------------
009500010601     C     ENDELA        BEGSR
009600990915     C*
009700000616     C*
009800010601     C                   ENDSR
009900010601     C***
010000010330     C*--------------------------------------------------------
010100051111     C* RWVAR   LEGGE TIVIN00R E SCRIVE FNVAR00R              *
010200010330     C*--------------------------------------------------------
010300050303     C     RWVAR         BEGSR
010400010330     C*
010500010330     C                   if        not %open(tivin00r)
010600010330     C                   open      tivin00r
010700010330     C                   endif
010800051111     C                   if        not %open(fnvar00r)
010900051111     C                   open      fnvar00r
011000010330     C                   endif
011100010330     C*
011200010604     C                   clear                   §CTROK
011300010604     C                   clear                   §CTRMO
011400010604     C                   clear                   §CTRNO
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
013800010330     C                   add       1             rrnum
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
015100090403     C*
015200090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015300090403     C* separatori campi
015400090403     C                   if        CharTXT <> *blanks
015500090403     C                   z-add     *zeros        wLenVINDTA        4 0
015600090403     C                   z-add     *zeros        wFlgVINDTA        1 0
015700090403     C                   setoff                                       80
015800090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
015900090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016000090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016100090403     C                   if        not *in80
016200090403     C                   seton                                        80
016300090403     C                   else
016400090403     C                   setoff                                       80
016500090403     C                   endif
016600090403     C                   endif
016700090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
016800090403     C                             and *in80 = *on
016900090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017000090403     C                   endif
017100090403     C                   enddo
017200090403     C                   endif
017300090403     C*
017400050303     C                   exsr      impvar
017500010601     C*
017600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017700050303     C                   eval      depfil = VARLNP
017800010601     C                   exsr      repfil
017900010601     C                   if        depfil = invfil
018000010601     C*
018100010601     C                   exsr      PREELA
018200010601     C*
018300010604     C* Ebbene...
018400010604     C*
018500030822     C  N31              add       1             §CTROK            7 0
018600030822     C   32              add       1             §CTRMO            7 0
018700030822     C   31              add       1             §CTRNO            7 0
018800030822     C                   if        wGiro = 2
018900050303     C  N31              write     FNVAR000
019000030822     C                   endif
019100050303     C                   endif
019200010604     C*
019300010604     C                   if        *in31 = *off and
019400010604     C                             *in32 = *off
019500010604     C                   eval      vinflg = '1'
019600010604     C                   else
019700010604     C                   eval      vinflg = '2'
019800010604     C                   endif
019900010601     C*
020000010604     C                   endif
020100010604     C*
020200010330     C                   else
020300010330     C                   eval      vinflg = '1'
020400010330     C                   endif
020500010601     C*
020600010601     C  N70              update    tivin000
020700010330     C*
020800030822     C  N70              enddo
020900010601     C
021000010601     C*
021100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021200010601     C                   if        cntNonEl = *zeros or
021300010601     C                             flgMulti = '0'
021400010330     C* Se non ci sono record con errori ...
021500010601     C                   if        §ctrno = 0 and
021600010604     C                             §ctrmo = 0 and
021700010601     C                             flgStato <> '2'
021800010330     C* ... restituisco esito OK.
021900010330     C                   eval      wrkesito = '0'
022000010330     C                   else
022100010330     C                   if        §ctrok > 0
022200010330     C                   eval      wrkesito = '1'
022300010330     C                   else
022400010615     C                   if        flgOk = '0'
022500010615     C                   eval      wrkesito = '2'
022600010615     C                   else
022700010615     C                   eval      wrkesito = '6'
022800010615     C                   endif
022900010330     C                   endif
023000010330     C                   endif
023100010601     C                   else
023200010601     C                   eval      wrkesito = '9'
023300010601     C                   endif
023400010330     C*
023500010330     C                   if        %open(tivin00r)
023600010330     C                   close     tivin00r
023700010330     C                   endif
023800051111     C                   if        %open(fnvar00r)
023900051111     C                   close     fnvar00r
024000010330     C                   endif
024100010601     C*
024200010601     C                   if        vlrpoi <> 999
024300010601     C                   eval      invfil = vlrpoi
024400010601     C                   endif
024500010330     C*
024600010330     C                   if        §ctrok > 0
024700010601     C                             and invfil > *zeros
024800051111     C                   COMMIT
024900010330     C                   exsr      invio
025000010330     C                   endif
025100010601     C*
025200010618     C                   if        flgGiro = '1'
025300010601     C                   exsr      endela
025400010618     C                   endif
025500010330     C*
025600010330     C                   ENDSR
025700010330     C***
025800010601
025900010601
026000010601
026100010330     C*----------------------------------------------------*
026200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300010330     C*----------------------------------------------------*
026400010330     C     INZVAR        BEGSR
026500010330     C*
026600030822     C* Inizializzo variabili di wrk
026700020204     C                   Z-ADD     *zeros        Num5_0            5 0
026800090403     C                   z-add     *zeros        invfil
026900030822     C*
027000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027100050303     C                   CLEAR                   FNVAR000
027200030822     C                   CLEAR                   SkSplitCSV
027300030822     C                   EVAL      i = 1
027400030822     C                   EVAL      posDa = *zeros
027500030822     C                   EVAL      posA  = *zeros
027600030822     C*
027700030822     C* Reimposto i valori di default
027800030822     C                   EXSR      DEFCAM
027900030822     C*
028000010330     C                   ENDSR
028100010330     C*----------------------------------------------------*
028200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028300010330     C*----------------------------------------------------*
028400010330     C     DEFCAM        BEGSR
028500030822     C*
028600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
028700030715     C* e delimitatore testo.
028800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
028900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029100090403     C*
029200090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
029300090403     C                   EVAL      CharSOS = CharNUM
029400030822     C*
029500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
029600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
029700030715     C                   SETON                                        50
029800030715     C                   ELSE
029900030715     C                   SETOFF                                       50
030000030715     C                   ENDIF
030100030822     C*
030200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030300030822     C                   EVAL      posDaDft = 1
030400030822     C                   EVAL      posADft  = 0
030500030822     C                   EVAL      wGiroDft = 0
030600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
030700030822     C                             posDaDft > 0
030800030822     C*
030900030822     C* Gestisco il 1° giro
031000030822     C                   IF        wGiroDft = 0
031100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031300030822     C* Incremento il contatore dei "giri"
031400030822     C                   EVAL      wGiroDft = 1
031500030822     C                   ELSE
031600030822     C                   EVAL      posDaDft = posADft
031700030822     C                   ENDIF
031800030822     C* Eseguo lo scan x trovare la fine del campo corrente
031900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032000030822     C*
032100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032300030822     C                   IF        posDaDft > 0 AND
032400030822     C                             posADft  > 0
032500050303     C* CCM
032600030822     C                   IF        %subst(
032700030822     C                             %subst(vlrppt:posDaDft+1:
032800030822     C                             posADft-posDaDft-1):1:3)
032900050303     C                             = 'CCM'
033000030822     C                   EVAL      PiStr=%trim(%subst(
033100030822     C                             %subst(vlrppt:posDaDft+1:
033200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033300030822     C                   EXSR      CHKNUM
033400030822     C                   IF        PiInt=*on
033500050303     C                   Z-ADD     PiVal         VARCCM
033600030822     C                   ENDIF
033700030822     C                   ENDIF
033800120905     C* NPR
033900120905     C                   IF        %subst(
034000120905     C                             %subst(vlrppt:posDaDft+1:
034100120905     C                             posADft-posDaDft-1):1:3)
034200120905     C                             = 'NPR'
034300120905     C                   EVAL      PiStr=%trim(%subst(
034400120905     C                             %subst(vlrppt:posDaDft+1:
034500120905     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034600120905     C                   EXSR      CHKNUM
034700120905     C                   IF        PiInt=*on
034800120905     C                   Z-ADD     PiVal         VARNPR
034900120905     C                   ENDIF
035000120905     C                   ENDIF
035100120905     C* DTR
035200120905     C                   IF        %subst(
035300120905     C                             %subst(vlrppt:posDaDft+1:
035400120905     C                             posADft-posDaDft-1):1:3)
035500120905     C                             = 'DTR'
035600120905     C                   EVAL      PiStr=%trim(%subst(
035700120905     C                             %subst(vlrppt:posDaDft+1:
035800120905     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035900120905     C                   IF        PiStr = '*DATE'
036000120905     C                   EVAL      VARDTR = %dec(%date() : *ISO)
036100120905     C                   ENDIF
036200120905     C                   ENDIF
036300120905     C* ...
036400030822     C                   ENDIF
036500030822     C                   ENDDO
036600020204     C*
036700010330     C                   ENDSR
036800010607     C*----------------------------------------------------*
036900050303     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNVAR)
037000010607     C*----------------------------------------------------*
037100050303     C     IMPVAR        BEGSR
037200010607     C*
037300010607     C                   SETOFF                                       3132
037400030515     C*
037500050303     C                   MOVEL     datcor        VARAAS
037600130108     C                   MOVEL     datcor        VARAGC
037700010607     C*
037800030715     C**********
037900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038000030715     C**********
038100030715     C                   IF        wGiro = *zeros
038200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
038300031201     C     minu:maiu     XLATE     vindta        vindta
038400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
038500040324     C                   DOW       posDa <= %len(%trimr(vindta))
038600030715     C*
038700030715     C* Gestisco il 1° campo
038800030715     C                   IF        i = 1
038900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
039000030715     C                   EVAL      posDa = 1
039100030715     C* Eseguo lo scan x trovare la fine del primo campo
039200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
039300030715     C* A questo punto "estraggo" il campo corrente
039400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039500030715     C                                                    (posA-posDa))
039600030715     C* X i campi successivi al 1°
039700030715     C                   ELSE
039800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
039900030715     C                   EVAL      posDa = posA + 1
040000030715     C* Eseguo lo scan x trovare la fine del campo corrente
040100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040200030715     C* Gestisco l'ultimo campo
040300030715     C                   IF        posA = *zeros
040400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
040500030715     C* A questo punto "estraggo" il campo corrente
040600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040700030715     C                                                    (posA-posDa))
040800030715     C                   LEAVE
040900030715     C                   ELSE
041000030715     C* A questo punto "estraggo" il campo corrente
041100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041200030715     C                                                    (posA-posDa))
041300030715     C                   ENDIF
041400030715     C                   ENDIF
041500030715     C* Incremento il contatore d campo
041600030715     C                   EVAL      i = i +1
041700030715     C                   ENDDO
041800030715     C*
041900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
042000030715     C* delimitazione testo
042100030715     C                   EVAL      i = 1
042200030715     C                   DOW       i < %elem(SkSplitFLD)
042300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
042400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
042500030715     C                   EVAL      i = i + 1
042600030715     C                   ENDDO
042700030715     C                   EVAL      wGiro = 1
042800030715     C                   ELSE
042900030715     C                   EVAL      wGiro = 2
043000031204     C**********
043100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
043200031204     C* d separatore campo
043300031204     C**********
043400040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
043500040304     C                   EVAL      vindta = ' ' + vindta
043600040304     C                   ENDIF
043700040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
043800040304     C***                EVAL      vindta = %subst(vindta:2)
043900040304     C***                ENDDO
044000031204     C*
044100031204     C                   Z-ADD     *zeros        lunghInput        4 0
044200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
044300040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
044400040304     C                   EVAL      vindta = vindta + ' '
044500040304     C                   ENDIF
044600040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
044700040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
044800040304     C***                EVAL      lunghInput = %len(%trim(vindta))
044900040304     C***                ENDDO
045000030715     C**********
045100030715     C* Effettuo lo split del campo dati d input
045200030715     C**********
045300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
045400040324     C                   DOW       posDa <= %len(%trimr(vindta))
045500030715     C*
045600030715     C* Gestisco il 1° campo
045700030715     C                   IF        i = 1
045800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
045900030715     C                   EVAL      posDa = 1
046000030715     C* Eseguo lo scan x trovare la fine del primo campo
046100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046200030715     C* A questo punto "estraggo" il campo corrente
046300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046400030715     C                                                    (posA-posDa))
046500030715     C* X i campi successivi al 1°
046600030715     C                   ELSE
046700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
046800030715     C                   EVAL      posDa = posA + 1
046900031204     C* Verifico che nn vi sia il campo nn valorizzato
047000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047100031204     C* Se campo nn valorizzato skippo al prossimo
047200031204     C                   EVAL      SkSplitCSV(i) = *blanks
047300031204     C                   EVAL      posA  = posA + 1
047400031204     C                   ELSE
047500030715     C* Eseguo lo scan x trovare la fine del campo corrente
047600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047700030715     C* Gestisco l'ultimo campo
047800030715     C                   IF        posA = *zeros
047900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048000030715     C* A questo punto "estraggo" il campo corrente
048100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048200030715     C                                                    (posA-posDa))
048300030715     C                   LEAVE
048400030715     C                   ELSE
048500030715     C* A questo punto "estraggo" il campo corrente
048600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048700030715     C                                                    (posA-posDa))
048800030715     C                   ENDIF
048900031204     C                   ENDIF
049000030715     C                   ENDIF
049100030715     C* Incremento il contatore d campo
049200030715     C                   EVAL      i = i +1
049300030715     C                   ENDDO
049400030715     C*
049500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049600030715     C* delimitazione testo
049700030715     C                   EVAL      i = 1
049800030715     C                   DOW       i < %elem(SkSplitCSV)
049900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
050000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050100030715     C                   EVAL      i = i + 1
050200030715     C                   ENDDO
050300030715     C*
050400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050500030715     C                   EVAL      i = 1
050600030715     C                   DOW       i < %elem(SkSplitFLD)
050700030715     C*
050800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
050900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
051000030715     C                             SkSplitCSV(i) <> *blanks
051100050303     C*** DMC
051200050303     C                   IF        %trim(SkSplitFLD(i)) = 'VARDMC'
051300050303     C                   EVAL      VARDMC = SkSplitCSV(i)
051400030721     C                   ENDIF
051500030721     C*
051600030721     C* Reperisco quindi i campi numerici...
051700130107     C*** AAS
051800130107     C                   IF        %trim(SkSplitFLD(i)) = 'VAGAAS'
051900130107     C                   EVAL      PiStr=SkSplitCSV(i)
052000130107     C                   EXSR      CHKNUM
052100130107     C                   IF        PiInt=*on AND
052200130107     C                             PiVal<=9999
052300130107     C                   Z-ADD     PiVal         VARAAS
052400130107     C                   ELSE
052500130107     C                   SETON                                        32
052600130107     C                   EVAL      vinmsg = %trimr(vinmsg)
052700130107     C                             + ' ' + 'VARAAS'
052800130107     C                   ENDIF
052900130107     C                   ENDIF
053000030721     C*** NSP
053100120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGNSP'
053200030721     C                   EVAL      PiStr=SkSplitCSV(i)
053300030721     C                   EXSR      CHKNUM
053400030721     C                   IF        PiInt=*on AND
053500030721     C                             PiVal<=9999999
053600050303     C                   Z-ADD     PiVal         VARNSP
053700030721     C                   ELSE
053800030721     C                   SETON                                        32
053900050303     C                   EVAL      VARNSP = *zeros
054000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
054100050303     C                             + ' ' + 'VARNSP'
054200030721     C                   ENDIF
054300030721     C                   ENDIF
054400030721     C*** CCM
054500120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGSCM'
054600030721     C                   EVAL      PiStr=SkSplitCSV(i)
054700030721     C                   EXSR      CHKNUM
054800030721     C                   IF        PiInt=*on      AND
054900030721     C                             PiVal<=9999999 AND
055000030721     C                             PiVal>*zeros
055100050303     C                   Z-ADD     PiVal         VARCCM
055200030721     C                   ELSE
055300030721     C                   SETON                                        32
055400050303     C                   EVAL      VARCCM = *zeros
055500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
055600050303     C                             + ' ' + 'VARCCM'
055700030721     C                   ENDIF
055800030721     C                   ENDIF
055900030721     C*** LNP
056000120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGLNP'
056100030721     C                   EVAL      PiStr=SkSplitCSV(i)
056200030721     C                   EXSR      CHKNUM
056300030721     C                   IF        PiInt=*on    AND
056400030721     C                             PiVal<=999   AND
056500030721     C                             PiVal>*zeros
056600050303     C                   Z-ADD     PiVal         VARLNP
056700030721     C                   ELSE
056800030721     C                   SETON                                        32
056900050303     C                   EVAL      VARLNP = *zeros
057000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057100050303     C                             + ' ' + 'VARLNP'
057200030721     C                   ENDIF
057300030721     C                   ENDIF
057400030721     C*** NRS
057500120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGNRS'
057600030721     C                   EVAL      PiStr=SkSplitCSV(i)
057700030721     C                   EXSR      CHKNUM
057800030721     C                   IF        PiInt=*on AND
057900030721     C                             PiVal<=99
058000050303     C                   Z-ADD     PiVal         VARNRS
058100030721     C                   ELSE
058200030721     C                   SETON                                        32
058300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058400050303     C                             + ' ' + 'VARNRS'
058500030721     C                   ENDIF
058600030721     C                   ENDIF
058700050303     C*** AGC
058800120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGAGC'
058900050303     C                   EVAL      PiStr=SkSplitCSV(i)
059000050303     C                   EXSR      CHKNUM
059100050303     C                   IF        PiInt=*on AND
059200050303     C                             PiVal<=9999
059300050303     C                   Z-ADD     PiVal         VARAGC
059400050303     C                   ELSE
059500050303     C                   SETON                                        32
059600050303     C                   EVAL      vinmsg = %trimr(vinmsg)
059700050303     C                             + ' ' + 'VARAGC'
059800050303     C                   ENDIF
059900050303     C                   ENDIF
060000050303     C*** FGC
060100120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGFGC'
060200050303     C                   EVAL      PiStr=SkSplitCSV(i)
060300050303     C                   EXSR      CHKNUM
060400050303     C                   IF        PiInt=*on AND
060500050303     C                             PiVal<=999
060600050303     C                   Z-ADD     PiVal         VARFGC
060700050303     C                   ELSE
060800050303     C                   SETON                                        32
060900050303     C                   EVAL      vinmsg = %trimr(vinmsg)
061000050303     C                             + ' ' + 'VARFGC'
061100050303     C                   ENDIF
061200050303     C                   ENDIF
061300050303     C*** NGC
061400120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGNGC'
061500050303     C                   EVAL      PiStr=SkSplitCSV(i)
061600050303     C                   EXSR      CHKNUM
061700050303     C                   IF        PiInt=*on AND
061800050303     C                             PiVal<=9999999
061900050303     C                   Z-ADD     PiVal         VARNGC
062000050303     C                   ELSE
062100050303     C                   SETON                                        32
062200050303     C                   EVAL      vinmsg = %trimr(vinmsg)
062300050303     C                             + ' ' + 'VARNGC'
062400050303     C                   ENDIF
062500050303     C                   ENDIF
062600050303     C*** NPR
062700120905     C                   IF        %trim(SkSplitFLD(i)) = 'VAGNPR'
062800050303     C                   EVAL      PiStr=SkSplitCSV(i)
062900050303     C                   EXSR      CHKNUM
063000050303     C                   IF        PiInt=*on AND
063100050303     C                             PiVal<=999
063200050303     C                   Z-ADD     PiVal         VARNPR
063300050303     C                   ELSE
063400050303     C                   SETON                                        32
063500050303     C                   EVAL      vinmsg = %trimr(vinmsg)
063600050303     C                             + ' ' + 'VARNPR'
063700050303     C                   ENDIF
063800050303     C                   ENDIF
063900050303     C*** DTR (forzo sempre a data corrente)
064000050303     C                   Z-ADD     datcor        VARDTR
064100050303     C*** DRD (forzo sempre a *zeros)
064200050303     C                   EVAL      VARDRD = *zeros
064300030715     C*
064400030715     C                   ENDIF
064500030715     C                   EVAL      i = i + 1
064600030715     C                   ENDDO
064700020204     C*
064800030715     C                   ENDIF
064900020204     C*
065000010607     C                   ENDSR
065100010607     C*----------------------------------------------------*
065200040714
065300010330
065400010330
065500010330     C*----------------------------------------------------*
065600010330     C*  CONTROLLO NUMERICITA' CAMPI
065700010330     C*----------------------------------------------------*
065800010330     C     CHKNUM        BEGSR
065900010330     C*
066000010606     C                   IF        PiDecChr = *blanks
066100030715     C                   EVAL      PiDecChr = CharNUM
066200010606     C                   ENDIF
066300010606     C*
066400010606     C                   CALL(e)   'ISNUMERIC'
066500010330     C                   PARM                    PiStr            30
066600010606     C                   PARM                    PiDecChr          1
066700010330     C                   PARM      *ZEROS        PiVal            30 9
066800010330     C                   PARM      '0'           PiInt             1
066900010330     C                   PARM      '0'           PiNum             1
067000010330     C                   IF        %error
067100010606     C                   EVAL      PiNum=*off
067200010330     C                   ENDIF
067300010330     C*
067400010330     C                   ENDSR
067500010330     C***
067600010330
067700010601
067800010601
067900010601
068000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
068100010601     C     repfil        BEGSR
068200010601     C*
068300010601     C                   if        invfil = *zeros and
068400010601     C                             depfil > *zeros and
068500010629     C                             (vinflg = *blanks or
068600010629     C                              vinflg = *zeros)
068700010601     C
068800010601     C                   eval      invfil = depfil
068900010601     C                   endif
069000010601     C*
069100010601     C                   if        depfil <> invfil and
069200010601     C                             invfil > *zeros
069300010601     C                   eval      flgMulti = '1'
069400010601     C                   if        vinflg = *blanks
069500010601     C                   add       1             cntNonEl
069600010601     C                   endif
069700010601     C                   endif
069800010601     C*
069900010601     C                   if        vinflg = '2'
070000010601     C                   eval      flgStato = '2'
070100010601     C                   endif
070200010601     C*
070300010601     C                   ENDSR
070400010601     C***
070500050303
070600050303
070700050303
070800050303
070900050303
071000050303      /TITLE Invio dei dati al punto operativo.
071100050303     C     invio         begsr
071200051111     C*
071300051111     C                   clear                   tibs55ds
071400051111     C                   move      invfil        i50apo
071500051111     C                   call(e)   'TIBS55R'
071600051111     C                   parm                    tibs55ds
071700051111     C*
071800051111     C                   if        %error or
071900051111     C                             o50err <> *blanks
072000051111     C                   eval      wrkesito = '3'
072100051111     C                   else
072200051111     C*
072300051111     C* Imposto i parametri fondamentali: azione e S.I. in cui sottomettere
072400051111     C                   clear                   KPJBA
072500051111     C                   movel     'LG95'        KCOAZ
072600051111     C                   movel     o50asi        KNSIF
072700051111     C*
072800051111     C* Imposto altri parametri affinchè nn si spacchi la sottomissinoe del job
072900051111     C                   movel     'EDPCEDXXX'   KNMUS
073000051111     C                   z-add     *zeros        KNRAZ
073100051111     C                   movel     'DSP01'       KNMTD
073200051111     C                   z-add     *zeros        KNRVJ
073300051111     C                   z-add     *zeros        KNRJO
073400051111     C                   movel     'N'           KSTJO
073500051111     C                   z-add     *zeros        KTMIN
073600051111     C                   movel     'B'           KTPAZ
073700051111     C                   movel     'RICEZ_VAR'   KNMEB
073800051111     C                   z-add     *zeros        KNMSB
073900051111     C                   z-add     *zeros        KKEYF
074000051111     C                   z-add     *zeros        KRCDS
074100051111     C                   movel     'J'           KEXCN
074200051111     C                   movel     'P5'          KCOJB
074300051111     C***                movel     'KEDI'        KQIEB
074400051111     C***                movel     'KEDI'        KQEBI
074500051111     C                   movel     'N'           KCANC
074600051111     C***                call      'BCH10'
074700051111     C***                parm                    KPJBA
074800051111     C                   call      'FNLG95C2'
074900051111     C                   parm                    KPJBA
075000051111     C                   endif
075100051111     C*
075200051111     C                   endsr
075300010601
075400010601
075500010601
075600010601
075700010601      /TITLE Invio dei dati al punto operativo.
075800010601     C     opeini        BEGSR
075900010601     C*
076000010601     C* Inizializzo flag e contatori operativi
076100010601     C                   movel     '0'           flgGiro           1
076200010601     C                   movel     '0'           flgMulti          1
076300010601     C                   movel     '1'           flgStato          1
076400010615     C                   movel     '0'           flgOk             1
076500010601     C                   z-add     *zeros        cntNonEl         10 0
076600010601     C                   z-add     *zeros        depfil            3 0
076700010601     C                   z-add     *zeros        invfil            3 0
076800010601     C*
076900010601     C                   ENDSR
077000010601     C***
077100050421
077200050421
077300050421
077400050421     C     *pssr         BEGSR
077500050421     C*
077600050421     C                   eval      wrkesito = '2'
077700051111     C*
077800051111     C* Se errore => emetto operazione d ROLLBACK
077900051111     C                   ROLBK
078000050421     C*
078100050421     C                   ENDSR     '*CANCL'
078200050421     C***
078300010601
078400010601
078500010601
078600010330
078700010330
078800000613     C     *inzsr        BEGSR
078900990910     C*
079000990910     C     *entry        plist
079100990920     C                   parm                    tivlrds
079200990921     C                   parm      wrkesito      esito
079300000724     C                   parm                    prmlit
079400000710     C                   parm                    prmfir
079500010330     C*
079600010330     C* CALCOLA LA DATA CORRENTE
079700010330     C                   time                    wn14             14 0
079800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
079900010330     C                   z-add     wn8           g08dat
080000010330     C                   z-add     *zeros        g08inv
080100010330     C                   movel     '0'           g08err
080200010330     C                   call      'XSRDA8'
080300010330     C                   parm                    wlbda8
080400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080500000613     C*
080600000613     C                   ENDSR
080700000613     C***
