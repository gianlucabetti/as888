000100040714      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040714     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900030715     D*------------------
002000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002100030715     D*------------------
002200030715     D SkSplitFLD      S             10    DIM(100)
002300030715     D SkSplitCSV      S            256    DIM(100)
002400030715     D CharCSV         S              1
002500030715     D CharTXT         S              1
002600030715     D CharNUM         S              1
002700090403     D CharSOS         S              1
002800030715     D posDa           S              3  0 INZ(*zeros)
002900030715     D posA            S              3  0 INZ(*zeros)
003000030715     D i               s              3  0 INZ(1)
003100030715     D wGiro           s              1  0 INZ(*zeros)
003200030822     D*------------------
003300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003400030822     D*------------------
003500030822     D posDaDft        S              3  0 INZ(*zeros)
003600030822     D posADft         S              3  0 INZ(*zeros)
003700030822     D j               s              3  0 INZ(1)
003800030822     D wGiroDft        s              1  0 INZ(*zeros)
003900010330     D*------------------
004000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004100010330     D*------------------
004200010330     D WLBDA8          DS                  INZ
004300010330     D  G08DAT                 1      8  0
004400010330     D  G08INV                 9     16  0
004500010330     D  G08ERR                17     17
004600010330     D  G08TGI                18     22  0
004700031201     D*------------------
004800031201     D* Costanti
004900031201     D*------------------
005000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005200010330
005300010330
005400010330
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010601     C*
005900010601     C                   exsr      opeini
006000040714     C                   exsr      rwvat
006100010601     C*
006200010601     C                   seton                                        lr
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006900010601     C*--------------------------------------------------------
007000010601     C     PREELA        BEGSR
007100010601     C*
007200010601     C* SE OCCORRE SPEDIRE IN FILIALE
007300010601     C                   if        invfil <> *zeros and
007400010601     C                             flgGiro = '0'
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
008600010601
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009200010601     C*--------------------------------------------------------
009300010601     C     ENDELA        BEGSR
009400990915     C*
009500000616     C*
009600010601     C                   ENDSR
009700010601     C***
009800000613
009900010330
010000010601
010100010601
010200010601
010300010330     C*--------------------------------------------------------
010400040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
010500010330     C*--------------------------------------------------------
010600040714     C     RWVAT         BEGSR
010700010330     C*
010800010330     C                   if        not %open(tivin00r)
010900010330     C                   open      tivin00r
011000010330     C                   endif
011100040714     C                   if        not %open(fivatwwr)
011200040714     C                   open      fivatwwr
011300010330     C                   endif
011400010330     C*
011500010604     C                   clear                   §CTROK
011600010604     C                   clear                   §CTRMO
011700010604     C                   clear                   §CTRNO
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
014100010330     C                   add       1             rrnum
014200010330     C*
014300010601     C                   if        *in70 = *off and
014400010330     C                             (vinflg = *blanks
014500010330     C                              or vinflg = '0'
014600010330     C                              or vinflg = '2')
014700010330     C*
014800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014900010711     C                   if        vinflg = *blanks or vinflg = '0'
015000010711     C                   clear                   vinmsg
015100010711     C                   endif
015200010601     C*
015300010330     C                   exsr      inzvar
015400090403     C*
015500090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015600090403     C* separatori campi
015700090403     C                   if        CharTXT <> *blanks
015800090403     C                   z-add     *zeros        wLenVINDTA        4 0
015900090403     C                   z-add     *zeros        wFlgVINDTA        1 0
016000090403     C                   setoff                                       80
016100090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016200090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016300090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016400090403     C                   if        not *in80
016500090403     C                   seton                                        80
016600090403     C                   else
016700090403     C                   setoff                                       80
016800090403     C                   endif
016900090403     C                   endif
017000090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017100090403     C                             and *in80 = *on
017200090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017300090403     C                   endif
017400090403     C                   enddo
017500090403     C                   endif
017600090403     C*
017700040714     C                   exsr      impvat
017800010601     C*
017900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018000040714     C                   eval      depfil = VATLNP
018100010601     C                   exsr      repfil
018200010601     C                   if        depfil = invfil
018300021025     C                   if        vlrpoi = 999
018400040714     C                   move(P)   invfil        VATFGS
018500021025     C                   else
018600040714     C                   move(P)   vlrpoi        VATFGS
018700021025     C                   endif
018800050113     C                   endif
018900010601     C*
019000010601     C                   exsr      PREELA
019100010601     C*
019200010604     C* Ebbene...
019300010604     C*
019400030822     C  N31              add       1             §CTROK            7 0
019500030822     C   32              add       1             §CTRMO            7 0
019600030822     C   31              add       1             §CTRNO            7 0
019700030822     C                   if        wGiro = 2
019800040714     C  N31              write     FIVAT000
019900030822     C                   endif
020000010604     C*
020100010604     C                   if        *in31 = *off and
020200010604     C                             *in32 = *off
020300010604     C                   eval      vinflg = '1'
020400010604     C                   else
020500010604     C                   eval      vinflg = '2'
020600010604     C                   endif
020700010601     C*
020800010604     C                   endif
020900010604     C*
021000010330     C                   else
021100010330     C                   eval      vinflg = '1'
021200010330     C                   endif
021300010601     C*
021400010601     C  N70              update    tivin000
021500010330     C*
021600030822     C  N70              enddo
021700010601     C
021800010601     C*
021900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022000010601     C                   if        cntNonEl = *zeros or
022100010601     C                             flgMulti = '0'
022200010330     C* Se non ci sono record con errori ...
022300010601     C                   if        §ctrno = 0 and
022400010604     C                             §ctrmo = 0 and
022500010601     C                             flgStato <> '2'
022600010330     C* ... restituisco esito OK.
022700010330     C                   eval      wrkesito = '0'
022800010330     C                   else
022900010330     C                   if        §ctrok > 0
023000010330     C                   eval      wrkesito = '1'
023100010330     C                   else
023200010615     C                   if        flgOk = '0'
023300010615     C                   eval      wrkesito = '2'
023400010615     C                   else
023500010615     C                   eval      wrkesito = '6'
023600010615     C                   endif
023700010330     C                   endif
023800010330     C                   endif
023900010601     C                   else
024000010601     C                   eval      wrkesito = '9'
024100010601     C                   endif
024200010330     C*
024300010330     C                   if        %open(tivin00r)
024400010330     C                   close     tivin00r
024500010330     C                   endif
024600040714     C                   if        %open(fivatwwr)
024700040714     C                   close     fivatwwr
024800010330     C                   endif
024900010601     C*
025000010601     C                   if        vlrpoi <> 999
025100010601     C                   eval      invfil = vlrpoi
025200010601     C                   endif
025300010330     C*
025400010330     C                   if        §ctrok > 0
025500010601     C                             and invfil > *zeros
025600010330     C                   exsr      invio
025700010330     C                   endif
025800010601     C*
025900010618     C                   if        flgGiro = '1'
026000010601     C                   exsr      endela
026100010618     C                   endif
026200010330     C*
026300010330     C                   ENDSR
026400010330     C***
026500010601
026600010601
026700010601
026800010330     C*----------------------------------------------------*
026900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027000010330     C*----------------------------------------------------*
027100010330     C     INZVAR        BEGSR
027200010330     C*
027300030822     C* Inizializzo variabili di wrk
027400020204     C                   Z-ADD     *zeros        Num5_0            5 0
027500090403     C                   z-add     *zeros        invfil
027600030822     C*
027700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027800040714     C                   CLEAR                   FIVAT000
027900030822     C                   CLEAR                   SkSplitCSV
028000030822     C                   EVAL      i = 1
028100030822     C                   EVAL      posDa = *zeros
028200030822     C                   EVAL      posA  = *zeros
028300030822     C*
028400030822     C* Reimposto i valori di default
028500030822     C                   EXSR      DEFCAM
028600030822     C*
028700010330     C                   ENDSR
028800010330     C*----------------------------------------------------*
028900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029000010330     C*----------------------------------------------------*
029100010330     C     DEFCAM        BEGSR
029200030822     C*
029300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029400030715     C* e delimitatore testo.
029500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029800090403     C*
029900090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030000090403     C                   EVAL      CharSOS = CharNUM
030100030822     C*
030200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
030300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
030400030715     C                   SETON                                        50
030500030715     C                   ELSE
030600030715     C                   SETOFF                                       50
030700030715     C                   ENDIF
030800030822     C*
030900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031000030822     C                   EVAL      posDaDft = 1
031100030822     C                   EVAL      posADft  = 0
031200030822     C                   EVAL      wGiroDft = 0
031300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031400030822     C                             posDaDft > 0
031500030822     C*
031600030822     C* Gestisco il 1° giro
031700030822     C                   IF        wGiroDft = 0
031800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032000030822     C* Incremento il contatore dei "giri"
032100030822     C                   EVAL      wGiroDft = 1
032200030822     C                   ELSE
032300030822     C                   EVAL      posDaDft = posADft
032400030822     C                   ENDIF
032500030822     C* Eseguo lo scan x trovare la fine del campo corrente
032600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032700030822     C*
032800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
033000030822     C                   IF        posDaDft > 0 AND
033100030822     C                             posADft  > 0
033200030822     C* CCM
033300030822     C                   IF        %subst(
033400030822     C                             %subst(vlrppt:posDaDft+1:
033500030822     C                             posADft-posDaDft-1):1:3)
033600030822     C                             = 'CCM'
033700030822     C                   EVAL      PiStr=%trim(%subst(
033800030822     C                             %subst(vlrppt:posDaDft+1:
033900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034000030822     C                   EXSR      CHKNUM
034100030822     C                   IF        PiInt=*on
034200040714     C                   Z-ADD     PiVal         VATCCM
034300030822     C                   ENDIF
034400030822     C                   ENDIF
034500030822     C* LNP
034600030822     C                   IF        %subst(
034700030822     C                             %subst(vlrppt:posDaDft+1:
034800030822     C                             posADft-posDaDft-1):1:3)
034900030822     C                             = 'LNP'
035000030822     C                   EVAL      PiStr=%trim(%subst(
035100030822     C                             %subst(vlrppt:posDaDft+1:
035200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035300030822     C                   EXSR      CHKNUM
035400030822     C                   IF        PiInt=*on
035500040714     C                   Z-ADD     PiVal         VATLNP
035600030822     C                   ENDIF
035700030822     C                   ENDIF
035800040714     C* TRC
035900030822     C                   IF        %subst(
036000030822     C                             %subst(vlrppt:posDaDft+1:
036100030822     C                             posADft-posDaDft-1):1:3)
036200040714     C                             = 'TRC'
036300040714     C                   EVAL      VATTRC=%trim(%subst(
036400030822     C                             %subst(vlrppt:posDaDft+1:
036500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036600030822     C                   ENDIF
036700030822     C* ...
036800030822     C                   ENDIF
036900030822     C                   ENDDO
037000020204     C*
037100010330     C                   ENDSR
037200010607     C*----------------------------------------------------*
037300040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
037400010607     C*----------------------------------------------------*
037500040714     C     IMPVAT        BEGSR
037600010607     C*
037700010607     C                   SETOFF                                       3132
037800030515     C*
037900040714     C                   MOVEL     datcor        VATAAS
038000010607     C*
038100030715     C**********
038200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038300030715     C**********
038400030715     C                   IF        wGiro = *zeros
038500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
038600031201     C     minu:maiu     XLATE     vindta        vindta
038700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
038800040324     C                   DOW       posDa <= %len(%trimr(vindta))
038900030715     C*
039000030715     C* Gestisco il 1° campo
039100030715     C                   IF        i = 1
039200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
039300030715     C                   EVAL      posDa = 1
039400030715     C* Eseguo lo scan x trovare la fine del primo campo
039500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
039600030715     C* A questo punto "estraggo" il campo corrente
039700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039800030715     C                                                    (posA-posDa))
039900030715     C* X i campi successivi al 1°
040000030715     C                   ELSE
040100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
040200030715     C                   EVAL      posDa = posA + 1
040300030715     C* Eseguo lo scan x trovare la fine del campo corrente
040400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040500030715     C* Gestisco l'ultimo campo
040600030715     C                   IF        posA = *zeros
040700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
040800030715     C* A questo punto "estraggo" il campo corrente
040900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041000030715     C                                                    (posA-posDa))
041100030715     C                   LEAVE
041200030715     C                   ELSE
041300030715     C* A questo punto "estraggo" il campo corrente
041400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041500030715     C                                                    (posA-posDa))
041600030715     C                   ENDIF
041700030715     C                   ENDIF
041800030715     C* Incremento il contatore d campo
041900030715     C                   EVAL      i = i +1
042000030715     C                   ENDDO
042100030715     C*
042200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
042300030715     C* delimitazione testo
042400030715     C                   EVAL      i = 1
042500030715     C                   DOW       i < %elem(SkSplitFLD)
042600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
042700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
042800030715     C                   EVAL      i = i + 1
042900030715     C                   ENDDO
043000030715     C                   EVAL      wGiro = 1
043100030715     C                   ELSE
043200030715     C                   EVAL      wGiro = 2
043300031204     C**********
043400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
043500031204     C* d separatore campo
043600031204     C**********
043700040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
043800040304     C                   EVAL      vindta = ' ' + vindta
043900040304     C                   ENDIF
044000040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
044100040304     C***                EVAL      vindta = %subst(vindta:2)
044200040304     C***                ENDDO
044300031204     C*
044400031204     C                   Z-ADD     *zeros        lunghInput        4 0
044500040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
044600040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
044700040304     C                   EVAL      vindta = vindta + ' '
044800040304     C                   ENDIF
044900040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
045000040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045100040304     C***                EVAL      lunghInput = %len(%trim(vindta))
045200040304     C***                ENDDO
045300030715     C**********
045400030715     C* Effettuo lo split del campo dati d input
045500030715     C**********
045600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
045700040324     C                   DOW       posDa <= %len(%trimr(vindta))
045800030715     C*
045900030715     C* Gestisco il 1° campo
046000030715     C                   IF        i = 1
046100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046200030715     C                   EVAL      posDa = 1
046300030715     C* Eseguo lo scan x trovare la fine del primo campo
046400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046500030715     C* A questo punto "estraggo" il campo corrente
046600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046700030715     C                                                    (posA-posDa))
046800030715     C* X i campi successivi al 1°
046900030715     C                   ELSE
047000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047100030715     C                   EVAL      posDa = posA + 1
047200031204     C* Verifico che nn vi sia il campo nn valorizzato
047300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047400031204     C* Se campo nn valorizzato skippo al prossimo
047500031204     C                   EVAL      SkSplitCSV(i) = *blanks
047600031204     C                   EVAL      posA  = posA + 1
047700031204     C                   ELSE
047800030715     C* Eseguo lo scan x trovare la fine del campo corrente
047900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048000030715     C* Gestisco l'ultimo campo
048100030715     C                   IF        posA = *zeros
048200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048300030715     C* A questo punto "estraggo" il campo corrente
048400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048500030715     C                                                    (posA-posDa))
048600030715     C                   LEAVE
048700030715     C                   ELSE
048800030715     C* A questo punto "estraggo" il campo corrente
048900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049000030715     C                                                    (posA-posDa))
049100030715     C                   ENDIF
049200031204     C                   ENDIF
049300030715     C                   ENDIF
049400030715     C* Incremento il contatore d campo
049500030715     C                   EVAL      i = i +1
049600030715     C                   ENDDO
049700030715     C*
049800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049900030715     C* delimitazione testo
050000030715     C                   EVAL      i = 1
050100030715     C                   DOW       i < %elem(SkSplitCSV)
050200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
050300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050400030715     C                   EVAL      i = i + 1
050500030715     C                   ENDDO
050600030715     C*
050700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050800030715     C                   EVAL      i = 1
050900030715     C                   DOW       i < %elem(SkSplitFLD)
051000030715     C*
051100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
051200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
051300030715     C                             SkSplitCSV(i) <> *blanks
051400030721     C*** ATB
051500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
051600040714     C                   EVAL      VATATB = SkSplitCSV(i)
051700030721     C                   ENDIF
051800040714     C*** TRC
051900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
052000040714     C                   EVAL      VATTRC = SkSplitCSV(i)
052100030715     C                   ENDIF
052200040714     C*** NOT
052300040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
052400040714     C                   EVAL      VATNOT = SkSplitCSV(i)
052500030721     C                   ENDIF
052600030721     C*
052700030721     C* Reperisco quindi i campi numerici...
052800030721     C*** NSP
052900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
053000030721     C                   EVAL      PiStr=SkSplitCSV(i)
053100030721     C                   EXSR      CHKNUM
053200100517     C                   IF        PiInt=*on
053300040714     C                   Z-ADD     PiVal         VATNSP
053400030721     C                   ELSE
053500030721     C                   SETON                                        32
053600040714     C                   EVAL      VATNSP = *zeros
053700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
053800040714     C                             + ' ' + 'VATNSP'
053900030721     C                   ENDIF
054000030721     C                   ENDIF
054100030721     C*** CCM
054200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
054300030721     C                   EVAL      PiStr=SkSplitCSV(i)
054400030721     C                   EXSR      CHKNUM
054500030721     C                   IF        PiInt=*on      AND
054600030721     C                             PiVal<=9999999 AND
054700030721     C                             PiVal>*zeros
054800040714     C                   Z-ADD     PiVal         VATCCM
054900030721     C                   ELSE
055000030721     C                   SETON                                        32
055100040714     C                   EVAL      VATCCM = *zeros
055200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
055300040714     C                             + ' ' + 'VATCCM'
055400030721     C                   ENDIF
055500030721     C                   ENDIF
055600030721     C*** LNP
055700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
055800030721     C                   EVAL      PiStr=SkSplitCSV(i)
055900030721     C                   EXSR      CHKNUM
056000030721     C                   IF        PiInt=*on    AND
056100030721     C                             PiVal<=999   AND
056200030721     C                             PiVal>*zeros
056300040714     C                   Z-ADD     PiVal         VATLNP
056400030721     C                   ELSE
056500030721     C                   SETON                                        32
056600040714     C                   EVAL      VATLNP = *zeros
056700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056800040714     C                             + ' ' + 'VATLNP'
056900030721     C                   ENDIF
057000030721     C                   ENDIF
057100030721     C*** NRS
057200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
057300030721     C                   EVAL      PiStr=SkSplitCSV(i)
057400030721     C                   EXSR      CHKNUM
057500030721     C                   IF        PiInt=*on AND
057600030721     C                             PiVal<=99
057700040714     C                   Z-ADD     PiVal         VATNRS
057800030721     C                   ELSE
057900030721     C                   SETON                                        32
058000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058100040714     C                             + ' ' + 'VATNRS'
058200030721     C                   ENDIF
058300030721     C                   ENDIF
058400030715     C*
058500030715     C                   ENDIF
058600030715     C                   EVAL      i = i + 1
058700030715     C                   ENDDO
058800020204     C*
058900030715     C                   ENDIF
059000020204     C*
059100010607     C                   ENDSR
059200010607     C*----------------------------------------------------*
059300040714
059400010330
059500010330
059600010330     C*----------------------------------------------------*
059700010330     C*  CONTROLLO NUMERICITA' CAMPI
059800010330     C*----------------------------------------------------*
059900010330     C     CHKNUM        BEGSR
060000010330     C*
060100010606     C                   IF        PiDecChr = *blanks
060200030715     C                   EVAL      PiDecChr = CharNUM
060300010606     C                   ENDIF
060400010606     C*
060500010606     C                   CALL(e)   'ISNUMERIC'
060600010330     C                   PARM                    PiStr            30
060700010606     C                   PARM                    PiDecChr          1
060800010330     C                   PARM      *ZEROS        PiVal            30 9
060900010330     C                   PARM      '0'           PiInt             1
061000010330     C                   PARM      '0'           PiNum             1
061100010330     C                   IF        %error
061200010606     C                   EVAL      PiNum=*off
061300010330     C                   ENDIF
061400010330     C*
061500010330     C                   ENDSR
061600010330     C***
061700010330
061800010601
061900010601
062000010601
062100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
062200010601     C     repfil        BEGSR
062300010601     C*
062400010601     C                   if        invfil = *zeros and
062500010601     C                             depfil > *zeros and
062600010629     C                             (vinflg = *blanks or
062700010629     C                              vinflg = *zeros)
062800010601     C
062900010601     C                   eval      invfil = depfil
063000010601     C                   endif
063100010601     C*
063200010601     C                   if        depfil <> invfil and
063300010601     C                             invfil > *zeros
063400010601     C                   eval      flgMulti = '1'
063500010601     C                   if        vinflg = *blanks
063600010601     C                   add       1             cntNonEl
063700010601     C                   endif
063800010601     C                   endif
063900010601     C*
064000010601     C                   if        vinflg = '2'
064100010601     C                   eval      flgStato = '2'
064200010601     C                   endif
064300010601     C*
064400010601     C                   ENDSR
064500010601     C***
064600010601
064700010601
064800010601
064900010330
065000010330
065100010330
065200990920      /TITLE Invio dei dati al punto operativo.
065300000613     C     invio         BEGSR
065400990920     C*
065500990920     C                   reset                   dscmz
065600010601     C                   move      invfil        cmzdst
065700990920     C                   eval      cmzfld = vlrfou
065800990920     C                   eval      cmzmbd = vlrhdl
065900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
066000000710     C                   if        prmfir = *blanks
066100040714     C                   eval      cmzfla = 'FIVAT00F'
066200040714     C                   eval      cmzmba = 'FIVAT00F'
066300000710     C                   else
066400000710     C                   eval      cmzfla = prmfir
066500000710     C                   eval      cmzmba = prmfir
066600000710     C                   endif
066700990920     C                   eval      cmznrr = *zeros
066800990920     C                   move      §ctrok        cmznrr
066900021018     C                   eval      cmzlba = vlrfl1
067000990920     C                   call(e)   'TIS711C'
067100990920     C                   parm                    dscmz
067200990921     C                   parm      *blanks       esito
067300990920     C                   if        %error
067400990920     C                             or cmzerr = '1'
067500990921     C                             or esito  = '1'
067600000710     C                   eval      wrkesito = '3'
067700990920     C                   endif
067800990920     C*
067900000613     C                   ENDSR
068000990910
068100010601
068200010601
068300010601
068400010601
068500010601      /TITLE Invio dei dati al punto operativo.
068600010601     C     opeini        BEGSR
068700010601     C*
068800010601     C* Inizializzo flag e contatori operativi
068900010601     C                   movel     '0'           flgGiro           1
069000010601     C                   movel     '0'           flgMulti          1
069100010601     C                   movel     '1'           flgStato          1
069200010615     C                   movel     '0'           flgOk             1
069300010601     C                   z-add     *zeros        cntNonEl         10 0
069400010601     C                   z-add     *zeros        depfil            3 0
069500010601     C                   z-add     *zeros        invfil            3 0
069600010601     C*
069700010601     C                   ENDSR
069800010601     C***
069900050421
070000050421
070100050421
070200050421     C     *pssr         BEGSR
070300050421     C*
070400050421     C                   eval      wrkesito = '2'
070500050421     C*
070600050421     C                   ENDSR     '*CANCL'
070700050421     C***
070800010601
070900010601
071000010601
071100010330
071200010330
071300000613     C     *inzsr        BEGSR
071400990910     C*
071500990910     C     *entry        plist
071600990920     C                   parm                    tivlrds
071700990921     C                   parm      wrkesito      esito
071800000724     C                   parm                    prmlit
071900000710     C                   parm                    prmfir
072000010330     C*
072100010330     C* CALCOLA LA DATA CORRENTE
072200010330     C                   time                    wn14             14 0
072300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
072400010330     C                   z-add     wn8           g08dat
072500010330     C                   z-add     *zeros        g08inv
072600010330     C                   movel     '0'           g08err
072700010330     C                   call      'XSRDA8'
072800010330     C                   parm                    wlbda8
072900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
073000000613     C*
073100000613     C                   ENDSR
073200000613     C***
