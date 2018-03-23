000100121109      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500121109     FEDIVATwr  O    E             DISK    usropn
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
003900031201     D*------------------
004000031201     D* Costanti
004100031201     D*------------------
004200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004400010330
004500010330
004600010330
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200040714     C                   exsr      rwvat
005300010601     C*
005400010601     C                   seton                                        lr
005500010601
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010601     C*--------------------------------------------------------
006200010601     C     PREELA        BEGSR
006300010601     C*
006400010601     C* SE OCCORRE SPEDIRE IN FILIALE
006500010601     C                   if        invfil <> *zeros and
006600010601     C                             flgGiro = '0'
006700010601     C*
006800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010601     C                   eval      flgGiro = '1'
007000010601     C*
007100010601     C                   endif
007200010601     C*
007300010601     C                   ENDSR
007400010601     C***
007500010601
007600010601
007700010601
007800010601
007900010601
008000010601
008100010601
008200010601     C*--------------------------------------------------------
008300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008400010601     C*--------------------------------------------------------
008500010601     C     ENDELA        BEGSR
008600990915     C*
008700000616     C*
008800010601     C                   ENDSR
008900010601     C***
009000000613
009100010330
009200010601
009300010601
009400010601
009500010330     C*--------------------------------------------------------
009600121109     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
009700010330     C*--------------------------------------------------------
009800040714     C     RWVAT         BEGSR
009900010330     C*
010000010330     C                   if        not %open(tivin00r)
010100010330     C                   open      tivin00r
010200010330     C                   endif
010300121109     C                   if        not %open(edivatwr)
010400121109     C                   open      edivatwr
010500010330     C                   endif
010600010330     C*
010700010604     C                   clear                   §CTROK
010800010604     C                   clear                   §CTRMO
010900010604     C                   clear                   §CTRNO
011000010330     C*
011100030822     C                   do        *HIVAL
011200010330     C*
011300030822     C                   read      tivin00r                               70
011400010618     C*
011500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011600010618     C                   if        vinflg = '1'
011700010618     C                   eval      flgOk = '1'
011800010618     C                   endif
011900010618     C*
012000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012100040510     C                   z-add     *zeros        wLenVINDTA        4 0
012200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
012500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
012600040510     C                   z-add     1             wFlgVINDTA
012700040510     C                   leave
012800040510     C                   endif
012900040510     C                   enddo
013000040510     C*
013100040218     C                   if        vindta > *blanks AND
013200040510     C                             wFlgVINDTA = 1
013300010330     C                   add       1             rrnum
013400010330     C*
013500010601     C                   if        *in70 = *off and
013600010330     C                             (vinflg = *blanks
013700010330     C                              or vinflg = '0'
013800010330     C                              or vinflg = '2')
013900010330     C*
014000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014100010711     C                   if        vinflg = *blanks or vinflg = '0'
014200010711     C                   clear                   vinmsg
014300010711     C                   endif
014400010601     C*
014500010330     C                   exsr      inzvar
014600090403     C*
014700090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
014800090403     C* separatori campi
014900090403     C                   if        CharTXT <> *blanks
015000090403     C                   z-add     *zeros        wLenVINDTA        4 0
015100090403     C                   z-add     *zeros        wFlgVINDTA        1 0
015200090403     C                   setoff                                       80
015300090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
015400090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
015500090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
015600090403     C                   if        not *in80
015700090403     C                   seton                                        80
015800090403     C                   else
015900090403     C                   setoff                                       80
016000090403     C                   endif
016100090403     C                   endif
016200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
016300090403     C                             and *in80 = *on
016400090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
016500090403     C                   endif
016600090403     C                   enddo
016700090403     C                   endif
016800090403     C*
016900040714     C                   exsr      impvat
017000010601     C*
017100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017200040714     C                   eval      depfil = VATLNP
017300010601     C                   exsr      repfil
017400010601     C                   if        depfil = invfil
017500021025     C                   if        vlrpoi = 999
017600040714     C                   move(P)   invfil        VATFGS
017700021025     C                   else
017800040714     C                   move(P)   vlrpoi        VATFGS
017900021025     C                   endif
018000050113     C                   endif
018100010601     C*
018200010601     C                   exsr      PREELA
018300010601     C*
018400010604     C* Ebbene...
018500010604     C*
018600030822     C  N31              add       1             §CTROK            7 0
018700030822     C   32              add       1             §CTRMO            7 0
018800030822     C   31              add       1             §CTRNO            7 0
018900030822     C                   if        wGiro = 2
019000121109     C* VALORIZZO CAMPI RELATIVI AL "CMR"
019100121109     C                   EVAL      VATCMR = wCMR
019200121109     C                   EVAL      VATCNT = 1
019300121109     C*
019400121109     C  N31              write     EDIVAT00
019500030822     C                   endif
019600010604     C*
019700010604     C                   if        *in31 = *off and
019800010604     C                             *in32 = *off
019900010604     C                   eval      vinflg = '1'
020000010604     C                   else
020100010604     C                   eval      vinflg = '2'
020200010604     C                   endif
020300010601     C*
020400010604     C                   endif
020500010604     C*
020600010330     C                   else
020700010330     C                   eval      vinflg = '1'
020800010330     C                   endif
020900010601     C*
021000010601     C  N70              update    tivin000
021100010330     C*
021200030822     C  N70              enddo
021300010601     C
021400010601     C*
021500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021600010601     C                   if        cntNonEl = *zeros or
021700010601     C                             flgMulti = '0'
021800010330     C* Se non ci sono record con errori ...
021900010601     C                   if        §ctrno = 0 and
022000010604     C                             §ctrmo = 0 and
022100010601     C                             flgStato <> '2'
022200010330     C* ... restituisco esito OK.
022300010330     C                   eval      wrkesito = '0'
022400010330     C                   else
022500010330     C                   if        §ctrok > 0
022600010330     C                   eval      wrkesito = '1'
022700010330     C                   else
022800010615     C                   if        flgOk = '0'
022900010615     C                   eval      wrkesito = '2'
023000010615     C                   else
023100010615     C                   eval      wrkesito = '6'
023200010615     C                   endif
023300010330     C                   endif
023400010330     C                   endif
023500010601     C                   else
023600010601     C                   eval      wrkesito = '9'
023700010601     C                   endif
023800010330     C*
023900010330     C                   if        %open(tivin00r)
024000010330     C                   close     tivin00r
024100010330     C                   endif
024200121109     C                   if        %open(edivatwr)
024300121109     C                   close     edivatwr
024400010330     C                   endif
024500010601     C*
024600010601     C                   if        vlrpoi <> 999
024700010601     C                   eval      invfil = vlrpoi
024800010601     C                   endif
024900010330     C*
025000010330     C                   if        §ctrok > 0
025100010601     C                             and invfil > *zeros
025200010330     C                   exsr      invio
025300010330     C                   endif
025400010601     C*
025500010618     C                   if        flgGiro = '1'
025600010601     C                   exsr      endela
025700010618     C                   endif
025800010330     C*
025900010330     C                   ENDSR
026000010330     C***
026100010601
026200010601
026300010601
026400010330     C*----------------------------------------------------*
026500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600010330     C*----------------------------------------------------*
026700010330     C     INZVAR        BEGSR
026800010330     C*
026900030822     C* Inizializzo variabili di wrk
027000020204     C                   Z-ADD     *zeros        Num5_0            5 0
027100090403     C                   z-add     *zeros        invfil
027200030822     C*
027300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027400121109     C                   CLEAR                   EDIVAT00
027500030822     C                   CLEAR                   SkSplitCSV
027600030822     C                   EVAL      i = 1
027700030822     C                   EVAL      posDa = *zeros
027800030822     C                   EVAL      posA  = *zeros
027900030822     C*
028000030822     C* Reimposto i valori di default
028100030822     C                   EXSR      DEFCAM
028200030822     C*
028300010330     C                   ENDSR
028400010330     C*----------------------------------------------------*
028500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028600010330     C*----------------------------------------------------*
028700010330     C     DEFCAM        BEGSR
028800030822     C*
028900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029000030715     C* e delimitatore testo.
029100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029400090403     C*
029500090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
029600090403     C                   EVAL      CharSOS = CharNUM
029700030822     C*
029800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
029900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
030000030715     C                   SETON                                        50
030100030715     C                   ELSE
030200030715     C                   SETOFF                                       50
030300030715     C                   ENDIF
030400030822     C*
030500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030600030822     C                   EVAL      posDaDft = 1
030700030822     C                   EVAL      posADft  = 0
030800030822     C                   EVAL      wGiroDft = 0
030900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031000030822     C                             posDaDft > 0
031100030822     C*
031200030822     C* Gestisco il 1° giro
031300030822     C                   IF        wGiroDft = 0
031400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031600030822     C* Incremento il contatore dei "giri"
031700030822     C                   EVAL      wGiroDft = 1
031800030822     C                   ELSE
031900030822     C                   EVAL      posDaDft = posADft
032000030822     C                   ENDIF
032100030822     C* Eseguo lo scan x trovare la fine del campo corrente
032200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032300030822     C*
032400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032600030822     C                   IF        posDaDft > 0 AND
032700030822     C                             posADft  > 0
032800030822     C* CCM
032900030822     C                   IF        %subst(
033000030822     C                             %subst(vlrppt:posDaDft+1:
033100030822     C                             posADft-posDaDft-1):1:3)
033200030822     C                             = 'CCM'
033300030822     C                   EVAL      PiStr=%trim(%subst(
033400030822     C                             %subst(vlrppt:posDaDft+1:
033500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033600030822     C                   EXSR      CHKNUM
033700030822     C                   IF        PiInt=*on
033800040714     C                   Z-ADD     PiVal         VATCCM
033900030822     C                   ENDIF
034000030822     C                   ENDIF
034100030822     C* LNP
034200030822     C                   IF        %subst(
034300030822     C                             %subst(vlrppt:posDaDft+1:
034400030822     C                             posADft-posDaDft-1):1:3)
034500030822     C                             = 'LNP'
034600030822     C                   EVAL      PiStr=%trim(%subst(
034700030822     C                             %subst(vlrppt:posDaDft+1:
034800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034900030822     C                   EXSR      CHKNUM
035000030822     C                   IF        PiInt=*on
035100040714     C                   Z-ADD     PiVal         VATLNP
035200030822     C                   ENDIF
035300030822     C                   ENDIF
035400040714     C* TRC
035500030822     C                   IF        %subst(
035600030822     C                             %subst(vlrppt:posDaDft+1:
035700030822     C                             posADft-posDaDft-1):1:3)
035800040714     C                             = 'TRC'
035900040714     C                   EVAL      VATTRC=%trim(%subst(
036000030822     C                             %subst(vlrppt:posDaDft+1:
036100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036200030822     C                   ENDIF
036300030822     C* ...
036400030822     C                   ENDIF
036500030822     C                   ENDDO
036600020204     C*
036700010330     C                   ENDSR
036800010607     C*----------------------------------------------------*
036900121109     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
037000010607     C*----------------------------------------------------*
037100040714     C     IMPVAT        BEGSR
037200010607     C*
037300010607     C                   SETOFF                                       3132
037400030515     C*
037500040714     C                   MOVEL     datcor        VATAAS
037600010607     C*
037700030715     C**********
037800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
037900030715     C**********
038000030715     C                   IF        wGiro = *zeros
038100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
038200031201     C     minu:maiu     XLATE     vindta        vindta
038300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
038400040324     C                   DOW       posDa <= %len(%trimr(vindta))
038500030715     C*
038600030715     C* Gestisco il 1° campo
038700030715     C                   IF        i = 1
038800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
038900030715     C                   EVAL      posDa = 1
039000030715     C* Eseguo lo scan x trovare la fine del primo campo
039100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
039200030715     C* A questo punto "estraggo" il campo corrente
039300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
039400030715     C                                                    (posA-posDa))
039500030715     C* X i campi successivi al 1°
039600030715     C                   ELSE
039700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
039800030715     C                   EVAL      posDa = posA + 1
039900030715     C* Eseguo lo scan x trovare la fine del campo corrente
040000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040100030715     C* Gestisco l'ultimo campo
040200030715     C                   IF        posA = *zeros
040300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
040400030715     C* A questo punto "estraggo" il campo corrente
040500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040600030715     C                                                    (posA-posDa))
040700030715     C                   LEAVE
040800030715     C                   ELSE
040900030715     C* A questo punto "estraggo" il campo corrente
041000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041100030715     C                                                    (posA-posDa))
041200030715     C                   ENDIF
041300030715     C                   ENDIF
041400030715     C* Incremento il contatore d campo
041500030715     C                   EVAL      i = i +1
041600030715     C                   ENDDO
041700030715     C*
041800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
041900030715     C* delimitazione testo
042000030715     C                   EVAL      i = 1
042100030715     C                   DOW       i < %elem(SkSplitFLD)
042200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
042300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
042400030715     C                   EVAL      i = i + 1
042500030715     C                   ENDDO
042600030715     C                   EVAL      wGiro = 1
042700030715     C                   ELSE
042800030715     C                   EVAL      wGiro = 2
042900031204     C**********
043000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
043100031204     C* d separatore campo
043200031204     C**********
043300040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
043400040304     C                   EVAL      vindta = ' ' + vindta
043500040304     C                   ENDIF
043600040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
043700040304     C***                EVAL      vindta = %subst(vindta:2)
043800040304     C***                ENDDO
043900031204     C*
044000031204     C                   Z-ADD     *zeros        lunghInput        4 0
044100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
044200040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
044300040304     C                   EVAL      vindta = vindta + ' '
044400040304     C                   ENDIF
044500040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
044600040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
044700040304     C***                EVAL      lunghInput = %len(%trim(vindta))
044800040304     C***                ENDDO
044900030715     C**********
045000030715     C* Effettuo lo split del campo dati d input
045100030715     C**********
045200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
045300040324     C                   DOW       posDa <= %len(%trimr(vindta))
045400030715     C*
045500030715     C* Gestisco il 1° campo
045600030715     C                   IF        i = 1
045700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
045800030715     C                   EVAL      posDa = 1
045900030715     C* Eseguo lo scan x trovare la fine del primo campo
046000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046100030715     C* A questo punto "estraggo" il campo corrente
046200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
046300030715     C                                                    (posA-posDa))
046400030715     C* X i campi successivi al 1°
046500030715     C                   ELSE
046600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
046700030715     C                   EVAL      posDa = posA + 1
046800031204     C* Verifico che nn vi sia il campo nn valorizzato
046900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047000031204     C* Se campo nn valorizzato skippo al prossimo
047100031204     C                   EVAL      SkSplitCSV(i) = *blanks
047200031204     C                   EVAL      posA  = posA + 1
047300031204     C                   ELSE
047400030715     C* Eseguo lo scan x trovare la fine del campo corrente
047500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047600030715     C* Gestisco l'ultimo campo
047700030715     C                   IF        posA = *zeros
047800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
047900030715     C* A questo punto "estraggo" il campo corrente
048000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048100030715     C                                                    (posA-posDa))
048200030715     C                   LEAVE
048300030715     C                   ELSE
048400030715     C* A questo punto "estraggo" il campo corrente
048500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048600030715     C                                                    (posA-posDa))
048700030715     C                   ENDIF
048800031204     C                   ENDIF
048900030715     C                   ENDIF
049000030715     C* Incremento il contatore d campo
049100030715     C                   EVAL      i = i +1
049200030715     C                   ENDDO
049300030715     C*
049400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049500030715     C* delimitazione testo
049600030715     C                   EVAL      i = 1
049700030715     C                   DOW       i < %elem(SkSplitCSV)
049800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
049900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050000030715     C                   EVAL      i = i + 1
050100030715     C                   ENDDO
050200030715     C*
050300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
050400030715     C                   EVAL      i = 1
050500030715     C                   DOW       i < %elem(SkSplitFLD)
050600030715     C*
050700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
050800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
050900030715     C                             SkSplitCSV(i) <> *blanks
051000030721     C*** ATB
051100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
051200040714     C                   EVAL      VATATB = SkSplitCSV(i)
051300030721     C                   ENDIF
051400040714     C*** TRC
051500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
051600040714     C                   EVAL      VATTRC = SkSplitCSV(i)
051700030715     C                   ENDIF
051800040714     C*** NOT
051900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
052000040714     C                   EVAL      VATNOT = SkSplitCSV(i)
052100030721     C                   ENDIF
052200030721     C*
052300030721     C* Reperisco quindi i campi numerici...
052400030721     C*** NSP
052500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
052600030721     C                   EVAL      PiStr=SkSplitCSV(i)
052700030721     C                   EXSR      CHKNUM
052800100517     C                   IF        PiInt=*on
052900040714     C                   Z-ADD     PiVal         VATNSP
053000030721     C                   ELSE
053100030721     C                   SETON                                        32
053200040714     C                   EVAL      VATNSP = *zeros
053300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
053400040714     C                             + ' ' + 'VATNSP'
053500030721     C                   ENDIF
053600030721     C                   ENDIF
053700030721     C*** CCM
053800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
053900030721     C                   EVAL      PiStr=SkSplitCSV(i)
054000030721     C                   EXSR      CHKNUM
054100030721     C                   IF        PiInt=*on      AND
054200030721     C                             PiVal<=9999999 AND
054300030721     C                             PiVal>*zeros
054400040714     C                   Z-ADD     PiVal         VATCCM
054500030721     C                   ELSE
054600030721     C                   SETON                                        32
054700040714     C                   EVAL      VATCCM = *zeros
054800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
054900040714     C                             + ' ' + 'VATCCM'
055000030721     C                   ENDIF
055100030721     C                   ENDIF
055200030721     C*** LNP
055300040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
055400030721     C                   EVAL      PiStr=SkSplitCSV(i)
055500030721     C                   EXSR      CHKNUM
055600030721     C                   IF        PiInt=*on    AND
055700030721     C                             PiVal<=999   AND
055800030721     C                             PiVal>*zeros
055900040714     C                   Z-ADD     PiVal         VATLNP
056000030721     C                   ELSE
056100030721     C                   SETON                                        32
056200040714     C                   EVAL      VATLNP = *zeros
056300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056400040714     C                             + ' ' + 'VATLNP'
056500030721     C                   ENDIF
056600030721     C                   ENDIF
056700030721     C*** NRS
056800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
056900030721     C                   EVAL      PiStr=SkSplitCSV(i)
057000030721     C                   EXSR      CHKNUM
057100030721     C                   IF        PiInt=*on AND
057200030721     C                             PiVal<=99
057300040714     C                   Z-ADD     PiVal         VATNRS
057400030721     C                   ELSE
057500030721     C                   SETON                                        32
057600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057700040714     C                             + ' ' + 'VATNRS'
057800030721     C                   ENDIF
057900030721     C                   ENDIF
058000030715     C*
058100030715     C                   ENDIF
058200030715     C                   EVAL      i = i + 1
058300030715     C                   ENDDO
058400020204     C*
058500030715     C                   ENDIF
058600020204     C*
058700010607     C                   ENDSR
058800010607     C*----------------------------------------------------*
058900040714
059000010330
059100010330
059200010330     C*----------------------------------------------------*
059300010330     C*  CONTROLLO NUMERICITA' CAMPI
059400010330     C*----------------------------------------------------*
059500010330     C     CHKNUM        BEGSR
059600010330     C*
059700010606     C                   IF        PiDecChr = *blanks
059800030715     C                   EVAL      PiDecChr = CharNUM
059900010606     C                   ENDIF
060000010606     C*
060100010606     C                   CALL(e)   'ISNUMERIC'
060200010330     C                   PARM                    PiStr            30
060300010606     C                   PARM                    PiDecChr          1
060400010330     C                   PARM      *ZEROS        PiVal            30 9
060500010330     C                   PARM      '0'           PiInt             1
060600010330     C                   PARM      '0'           PiNum             1
060700010330     C                   IF        %error
060800010606     C                   EVAL      PiNum=*off
060900010330     C                   ENDIF
061000010330     C*
061100010330     C                   ENDSR
061200010330     C***
061300010330
061400010601
061500010601
061600010601
061700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061800010601     C     repfil        BEGSR
061900010601     C*
062000010601     C                   if        invfil = *zeros and
062100010601     C                             depfil > *zeros and
062200010629     C                             (vinflg = *blanks or
062300010629     C                              vinflg = *zeros)
062400010601     C
062500010601     C                   eval      invfil = depfil
062600010601     C                   endif
062700010601     C*
062800010601     C                   if        depfil <> invfil and
062900010601     C                             invfil > *zeros
063000010601     C                   eval      flgMulti = '1'
063100010601     C                   if        vinflg = *blanks
063200010601     C                   add       1             cntNonEl
063300010601     C                   endif
063400010601     C                   endif
063500010601     C*
063600010601     C                   if        vinflg = '2'
063700010601     C                   eval      flgStato = '2'
063800010601     C                   endif
063900010601     C*
064000010601     C                   ENDSR
064100010601     C***
064200010601
064300010601
064400010601
064500010330
064600010330
064700010330
064800990920      /TITLE Invio dei dati al punto operativo.
064900000613     C     invio         BEGSR
065000990920     C*
065100990920     C                   reset                   dscmz
065200010601     C                   move      invfil        cmzdst
065300990920     C                   eval      cmzfld = vlrfou
065400990920     C                   eval      cmzmbd = vlrhdl
065500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
065600000710     C                   if        prmfir = *blanks
065700121109     C                   eval      cmzfla = 'EDIVAT0F'
065800121109     C                   eval      cmzmba = 'EDIVAT0F'
065900000710     C                   else
066000000710     C                   eval      cmzfla = prmfir
066100000710     C                   eval      cmzmba = prmfir
066200000710     C                   endif
066300990920     C                   eval      cmznrr = *zeros
066400990920     C                   move      §ctrok        cmznrr
066500021018     C                   eval      cmzlba = vlrfl1
066600990920     C                   call(e)   'TIS711C'
066700990920     C                   parm                    dscmz
066800990921     C                   parm      *blanks       esito
066900990920     C                   if        %error
067000990920     C                             or cmzerr = '1'
067100990921     C                             or esito  = '1'
067200000710     C                   eval      wrkesito = '3'
067300990920     C                   endif
067400990920     C*
067500000613     C                   ENDSR
067600990910
067700010601
067800010601
067900010601
068000010601
068100010601      /TITLE Invio dei dati al punto operativo.
068200010601     C     opeini        BEGSR
068300010601     C*
068400010601     C* Inizializzo flag e contatori operativi
068500010601     C                   movel     '0'           flgGiro           1
068600010601     C                   movel     '0'           flgMulti          1
068700010601     C                   movel     '1'           flgStato          1
068800010615     C                   movel     '0'           flgOk             1
068900010601     C                   z-add     *zeros        cntNonEl         10 0
069000010601     C                   z-add     *zeros        depfil            3 0
069100010601     C                   z-add     *zeros        invfil            3 0
069200010601     C*
069300010601     C                   ENDSR
069400010601     C***
069500050421
069600050421
069700050421
069800050421     C     *pssr         BEGSR
069900050421     C*
070000050421     C                   eval      wrkesito = '2'
070100050421     C*
070200050421     C                   ENDSR     '*CANCL'
070300050421     C***
070400010601
070500010601
070600010601
070700010330
070800010330
070900000613     C     *inzsr        BEGSR
071000990910     C*
071100990910     C     *entry        plist
071200990920     C                   parm                    tivlrds
071300990921     C                   parm      wrkesito      esito
071400000724     C                   parm                    prmlit
071500000710     C                   parm                    prmfir
071600010330     C*
071700010330     C* CALCOLA LA DATA CORRENTE
071800121109     C                   time                    wn14             14 0
071900121109     C                   movel     wn14          oracor            6 0          *ORA
072000121109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
072100121109     C                   eval      datcor = %dec(%date() : *ISO)
072200121109     C*
072300121109     C* Determino il CMR
072400121109     C                   movel     *blanks       wCMR             35
072500130220     C                   eval      wCMR = %char(datcor)
072600000613     C*
072700000613     C                   ENDSR
072800000613     C***
