000100160805      /TITLE Upload via Internet: traduzione in TIVMS00F.
000200160923     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300160923     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600160923     FTIVMS01I  UF A E           K DISK
000700160923     F                                     prefix(O_)
000800160808     FTIVMS02I  IF   E           K DISK    rename(tivms000:tivms002)
000900160808     F                                     prefix(K_)
001000000313     D*
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D psds           sds
001500010330     D  procname         *PROC
001600010330     D tivlrds       e ds                  extname(tivlr00f)
001700160923     D tivmsds       e ds                  extname(tivms00f)
001800160923     D tivmsds_O     e ds                  extname(tivms00f) prefix(O_)
001900010330     D esito           s              1
002000160805     D prmlit          s             10
002100160805     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300030715     D*------------------
002400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002500030715     D*------------------
002600030715     D SkSplitFLD      S             10    DIM(100)
002700030715     D SkSplitCSV      S            256    DIM(100)
002800030715     D CharCSV         S              1
002900030715     D CharTXT         S              1
003000030715     D CharNUM         S              1
003100090403     D CharSOS         S              1
003200030715     D posDa           S              3  0 INZ(*zeros)
003300030715     D posA            S              3  0 INZ(*zeros)
003400030715     D i               s              3  0 INZ(1)
003500030715     D wGiro           s              1  0 INZ(*zeros)
003600030822     D*------------------
003700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003800030822     D*------------------
003900030822     D posDaDft        S              3  0 INZ(*zeros)
004000030822     D posADft         S              3  0 INZ(*zeros)
004100030822     D j               s              3  0 INZ(1)
004200030822     D wGiroDft        s              1  0 INZ(*zeros)
004300031201     D*------------------
004400031201     D* Costanti
004500031201     D*------------------
004600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004800010330
004900010330
005000010330
005100990921     C                   reset                   esito
005200990921     C                   reset                   wrkesito
005300010601     C*
005400010601     C                   exsr      opeini
005500160805     C                   exsr      rwvms
005600010601     C*
005700010601     C                   seton                                        lr
005800010601
005900010601
006000010601     C*--------------------------------------------------------
006100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006200010601     C*--------------------------------------------------------
006300010601     C     PREELA        BEGSR
006400010601     C*
006500160805     C                   if        flgGiro = '0'
006600010601     C*
006700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006800010601     C                   eval      flgGiro = '1'
006900010601     C*
007000010601     C                   endif
007100010601     C*
007200010601     C                   ENDSR
007300010601     C***
007400010601
007500010601
007600010330     C*--------------------------------------------------------
007700160805     C* RWVMS   LEGGE TIVIN00R E SCRIVE TIMVS00F              *
007800010330     C*--------------------------------------------------------
007900160805     C     RWVMS         BEGSR
008000010330     C*
008100010330     C                   if        not %open(tivin00r)
008200010330     C                   open      tivin00r
008300010330     C                   endif
008400010330     C*
008500010604     C                   clear                   §CTROK
008600010604     C                   clear                   §CTRMO
008700010604     C                   clear                   §CTRNO
008800010330     C*
008900030822     C                   do        *HIVAL
009000010330     C*
009100030822     C                   read      tivin00r                               70
009200010618     C*
009300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009400010618     C                   if        vinflg = '1'
009500010618     C                   eval      flgOk = '1'
009600010618     C                   endif
009700010618     C*
009800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
009900040510     C                   z-add     *zeros        wLenVINDTA        4 0
010000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
010100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
010200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
010300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
010400040510     C                   z-add     1             wFlgVINDTA
010500040510     C                   leave
010600040510     C                   endif
010700040510     C                   enddo
010800040510     C*
010900040218     C                   if        vindta > *blanks AND
011000040510     C                             wFlgVINDTA = 1
011100010330     C*
011200010601     C                   if        *in70 = *off and
011300010330     C                             (vinflg = *blanks
011400010330     C                              or vinflg = '0'
011500010330     C                              or vinflg = '2')
011600010330     C*
011700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011800010711     C                   if        vinflg = *blanks or vinflg = '0'
011900010711     C                   clear                   vinmsg
012000010711     C                   endif
012100010601     C*
012200010330     C                   exsr      inzvar
012300090403     C*
012400090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
012500090403     C* separatori campi
012600090403     C                   if        CharTXT <> *blanks
012700090403     C                   z-add     *zeros        wLenVINDTA        4 0
012800090403     C                   z-add     *zeros        wFlgVINDTA        1 0
012900090403     C                   setoff                                       80
013000090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
013100090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
013200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
013300090403     C                   if        not *in80
013400090403     C                   seton                                        80
013500090403     C                   else
013600090403     C                   setoff                                       80
013700090403     C                   endif
013800090403     C                   endif
013900090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
014000090403     C                             and *in80 = *on
014100090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
014200090403     C                   endif
014300090403     C                   enddo
014400090403     C                   endif
014500090403     C*
014600160805     C                   exsr      impVMS
014700010601     C*
014800010601     C                   exsr      PREELA
014900010601     C*
015000010604     C* Ebbene...
015100010604     C*
015200030822     C  N31              add       1             §CTROK            7 0
015300030822     C   32              add       1             §CTRMO            7 0
015400030822     C   31              add       1             §CTRNO            7 0
015500030822     C                   if        wGiro = 2
015600160923     C  N31              exsr      WRIVMS
015700030822     C                   endif
015800010604     C*
015900010604     C                   if        *in31 = *off and
016000010604     C                             *in32 = *off
016100010604     C                   eval      vinflg = '1'
016200010604     C                   else
016300010604     C                   eval      vinflg = '2'
016400010604     C                   endif
016500010601     C*
016600010604     C                   endif
016700010604     C*
016800010330     C                   else
016900010330     C                   eval      vinflg = '1'
017000010330     C                   endif
017100010601     C*
017200010601     C  N70              update    tivin000
017300010330     C*
017400030822     C  N70              enddo
017500010601     C
017600010601     C*
017700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
017800010601     C                   if        cntNonEl = *zeros or
017900010601     C                             flgMulti = '0'
018000010330     C* Se non ci sono record con errori ...
018100010601     C                   if        §ctrno = 0 and
018200010604     C                             §ctrmo = 0 and
018300010601     C                             flgStato <> '2'
018400010330     C* ... restituisco esito OK.
018500010330     C                   eval      wrkesito = '0'
018600010330     C                   else
018700010330     C                   if        §ctrok > 0
018800010330     C                   eval      wrkesito = '1'
018900010330     C                   else
019000010615     C                   if        flgOk = '0'
019100010615     C                   eval      wrkesito = '2'
019200010615     C                   else
019300010615     C                   eval      wrkesito = '6'
019400010615     C                   endif
019500010330     C                   endif
019600010330     C                   endif
019700010601     C                   else
019800010601     C                   eval      wrkesito = '9'
019900010601     C                   endif
020000010330     C*
020100010330     C                   if        %open(tivin00r)
020200010330     C                   close     tivin00r
020300010330     C                   endif
020400010330     C*
020500010330     C                   ENDSR
020600010330     C***
020700010601
020800010601
020900010601
021000010330     C*----------------------------------------------------*
021100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021200010330     C*----------------------------------------------------*
021300010330     C     INZVAR        BEGSR
021400030822     C*
021500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
021600160805     C                   CLEAR                   TIVMS000
021700160923     C                   CLEAR                   tivmsds
021800160923     C                   CLEAR                   tivmsds_O
021900030822     C                   CLEAR                   SkSplitCSV
022000030822     C                   EVAL      i = 1
022100030822     C                   EVAL      posDa = *zeros
022200030822     C                   EVAL      posA  = *zeros
022300030822     C*
022400030822     C* Reimposto i valori di default
022500030822     C                   EXSR      DEFCAM
022600030822     C*
022700010330     C                   ENDSR
022800010330     C*----------------------------------------------------*
022900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
023000010330     C*----------------------------------------------------*
023100010330     C     DEFCAM        BEGSR
023200030822     C*
023300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
023400030715     C* e delimitatore testo.
023500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
023600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
023700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
023800090403     C*
023900090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
024000090403     C                   EVAL      CharSOS = CharNUM
024100030822     C*
024200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
024300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
024400030715     C                   SETON                                        50
024500030715     C                   ELSE
024600030715     C                   SETOFF                                       50
024700030715     C                   ENDIF
024800030822     C*
024900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
025000030822     C                   EVAL      posDaDft = 1
025100030822     C                   EVAL      posADft  = 0
025200030822     C                   EVAL      wGiroDft = 0
025300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
025400030822     C                             posDaDft > 0
025500030822     C*
025600030822     C* Gestisco il 1° giro
025700030822     C                   IF        wGiroDft = 0
025800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
025900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
026000030822     C* Incremento il contatore dei "giri"
026100030822     C                   EVAL      wGiroDft = 1
026200030822     C                   ELSE
026300030822     C                   EVAL      posDaDft = posADft
026400030822     C                   ENDIF
026500030822     C* Eseguo lo scan x trovare la fine del campo corrente
026600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
026700030822     C*
026800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
026900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
027000030822     C                   IF        posDaDft > 0 AND
027100030822     C                             posADft  > 0
027200160805     C* DDE
027300030822     C                   IF        %subst(
027400030822     C                             %subst(vlrppt:posDaDft+1:
027500030822     C                             posADft-posDaDft-1):1:3)
027600160805     C                             = 'DDE'
027700030822     C                   EVAL      PiStr=%trim(%subst(
027800030822     C                             %subst(vlrppt:posDaDft+1:
027900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
028000030822     C                   EXSR      CHKNUM
028100030822     C                   IF        PiInt=*on
028200160805     C                   Z-ADD     PiVal         VMSDDE
028300030822     C                   ENDIF
028400030822     C                   ENDIF
028500160805     C* DSC
028600160805     C                   IF        %subst(
028700160805     C                             %subst(vlrppt:posDaDft+1:
028800160805     C                             posADft-posDaDft-1):1:3)
028900160805     C                             = 'DSC'
029000160805     C                   EVAL      PiStr=%trim(%subst(
029100160805     C                             %subst(vlrppt:posDaDft+1:
029200160805     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
029300160805     C                   EXSR      CHKNUM
029400160805     C                   IF        PiInt=*on
029500160805     C                   Z-ADD     PiVal         VMSDSC
029600160805     C                   ENDIF
029700160805     C                   ENDIF
029800160805     C* KSU
029900030822     C                   IF        %subst(
030000030822     C                             %subst(vlrppt:posDaDft+1:
030100030822     C                             posADft-posDaDft-1):1:3)
030200160805     C                             = 'KSU'
030300160805     C                   EVAL      VMSKSU=%trim(%subst(
030400030822     C                             %subst(vlrppt:posDaDft+1:
030500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
030600030822     C                   ENDIF
030700160805     C* FLO
030800160805     C                   IF        %subst(
030900160805     C                             %subst(vlrppt:posDaDft+1:
031000160805     C                             posADft-posDaDft-1):1:3)
031100160805     C                             = 'FLO'
031200160805     C                   EVAL      VMSFLO=%trim(%subst(
031300160805     C                             %subst(vlrppt:posDaDft+1:
031400160805     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
031500160805     C                   ENDIF
031600160805     C* PROC
031700160805     C                   IF        %subst(
031800160805     C                             %subst(vlrppt:posDaDft+1:
031900160805     C                             posADft-posDaDft-1):1:4)
032000160805     C                             = 'PROC'
032100160805     C                   EVAL      VMSPROC=%trim(%subst(
032200160805     C                             %subst(vlrppt:posDaDft+1:
032300160805     C                             posADft-posDaDft-1):5:posADft-posDaDft-1-4))
032400160805     C                   ENDIF
032500030822     C* ...
032600030822     C                   ENDIF
032700030822     C                   ENDDO
032800020204     C*
032900010330     C                   ENDSR
033000010607     C*----------------------------------------------------*
033100160805     C*  IMPOSTAZIONE CAMPI DA FLAT FILE
033200010607     C*----------------------------------------------------*
033300160805     C     IMPVMS        BEGSR
033400010607     C*
033500010607     C                   SETOFF                                       3132
033600010607     C*
033700030715     C**********
033800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
033900030715     C**********
034000030715     C                   IF        wGiro = *zeros
034100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
034200031201     C     minu:maiu     XLATE     vindta        vindta
034300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
034400040324     C                   DOW       posDa <= %len(%trimr(vindta))
034500030715     C*
034600030715     C* Gestisco il 1° campo
034700030715     C                   IF        i = 1
034800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
034900030715     C                   EVAL      posDa = 1
035000030715     C* Eseguo lo scan x trovare la fine del primo campo
035100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
035200030715     C* A questo punto "estraggo" il campo corrente
035300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
035400030715     C                                                    (posA-posDa))
035500030715     C* X i campi successivi al 1°
035600030715     C                   ELSE
035700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
035800030715     C                   EVAL      posDa = posA + 1
035900030715     C* Eseguo lo scan x trovare la fine del campo corrente
036000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
036100030715     C* Gestisco l'ultimo campo
036200030715     C                   IF        posA = *zeros
036300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
036400030715     C* A questo punto "estraggo" il campo corrente
036500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
036600030715     C                                                    (posA-posDa))
036700030715     C                   LEAVE
036800030715     C                   ELSE
036900030715     C* A questo punto "estraggo" il campo corrente
037000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
037100030715     C                                                    (posA-posDa))
037200030715     C                   ENDIF
037300030715     C                   ENDIF
037400030715     C* Incremento il contatore d campo
037500030715     C                   EVAL      i = i +1
037600030715     C                   ENDDO
037700030715     C*
037800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
037900030715     C* delimitazione testo
038000030715     C                   EVAL      i = 1
038100030715     C                   DOW       i < %elem(SkSplitFLD)
038200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
038300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
038400030715     C                   EVAL      i = i + 1
038500030715     C                   ENDDO
038600030715     C                   EVAL      wGiro = 1
038700030715     C                   ELSE
038800030715     C                   EVAL      wGiro = 2
038900031204     C**********
039000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
039100031204     C* d separatore campo
039200031204     C**********
039300040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
039400040304     C                   EVAL      vindta = ' ' + vindta
039500040304     C                   ENDIF
039600031204     C*
039700031204     C                   Z-ADD     *zeros        lunghInput        4 0
039800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
039900040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
040000040304     C                   EVAL      vindta = vindta + ' '
040100040304     C                   ENDIF
040200030715     C**********
040300030715     C* Effettuo lo split del campo dati d input
040400030715     C**********
040500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
040600040324     C                   DOW       posDa <= %len(%trimr(vindta))
040700030715     C*
040800030715     C* Gestisco il 1° campo
040900030715     C                   IF        i = 1
041000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
041100030715     C                   EVAL      posDa = 1
041200030715     C* Eseguo lo scan x trovare la fine del primo campo
041300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041400030715     C* A questo punto "estraggo" il campo corrente
041500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
041600030715     C                                                    (posA-posDa))
041700030715     C* X i campi successivi al 1°
041800030715     C                   ELSE
041900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
042000030715     C                   EVAL      posDa = posA + 1
042100031204     C* Verifico che nn vi sia il campo nn valorizzato
042200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
042300031204     C* Se campo nn valorizzato skippo al prossimo
042400031204     C                   EVAL      SkSplitCSV(i) = *blanks
042500031204     C                   EVAL      posA  = posA + 1
042600031204     C                   ELSE
042700030715     C* Eseguo lo scan x trovare la fine del campo corrente
042800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042900030715     C* Gestisco l'ultimo campo
043000030715     C                   IF        posA = *zeros
043100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
043200030715     C* A questo punto "estraggo" il campo corrente
043300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043400030715     C                                                    (posA-posDa))
043500030715     C                   LEAVE
043600030715     C                   ELSE
043700030715     C* A questo punto "estraggo" il campo corrente
043800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
043900030715     C                                                    (posA-posDa))
044000030715     C                   ENDIF
044100031204     C                   ENDIF
044200030715     C                   ENDIF
044300030715     C* Incremento il contatore d campo
044400030715     C                   EVAL      i = i +1
044500030715     C                   ENDDO
044600030715     C*
044700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044800030715     C* delimitazione testo
044900030715     C                   EVAL      i = 1
045000030715     C                   DOW       i < %elem(SkSplitCSV)
045100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
045200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
045300030715     C                   EVAL      i = i + 1
045400030715     C                   ENDDO
045500030715     C*
045600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
045700030715     C                   EVAL      i = 1
045800030715     C                   DOW       i < %elem(SkSplitFLD)
045900030715     C*
046000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
046100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
046200030715     C                             SkSplitCSV(i) <> *blanks
046300030721     C*** ATB
046400160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSATB'
046500160805     C                   EVAL      VMSATB = SkSplitCSV(i)
046600030721     C                   ENDIF
046700160805     C*** PROC
046800160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSPROC'
046900160805     C                   EVAL      VMSPROC = SkSplitCSV(i)
047000160805     C                   ENDIF
047100160805     C*** KSU
047200160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSKSU'
047300160805     C                   EVAL      VMSKSU = SkSplitCSV(i)
047400160805     C                   ENDIF
047500160808     C*** CAUTIP
047600160808     C                   IF        %trim(SkSplitFLD(i)) = 'VMSCAUTIP'
047700160808     C                   EVAL      VMSCAUTIP = SkSplitCSV(i)
047800160808     C                   ENDIF
047900160805     C*** CAUBRT
048000160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSCAUBRT'
048100160805     C                   EVAL      VMSCAUBRT = SkSplitCSV(i)
048200160805     C                   ENDIF
048300160805     C*** CAUTT
048400160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSCAUTT'
048500160805     C                   EVAL      VMSCAUTT = SkSplitCSV(i)
048600160805     C                   ENDIF
048700160805     C*** STSCLI
048800160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSSTSCLI'
048900160805     C                   EVAL      VMSSTSCLI = SkSplitCSV(i)
049000160805     C                   ENDIF
049100160805     C*** STSOPZ
049200160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSSTSOPZ'
049300160805     C                   EVAL      VMSSTSOPZ = SkSplitCSV(i)
049400160805     C                   ENDIF
049500160805     C*** FASOP
049600160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSFASOP'
049700160805     C                   EVAL      VMSFASOP = SkSplitCSV(i)
049800160805     C                   ENDIF
049900160805     C*** FLO
050000160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSFLO'
050100160805     C                   EVAL      VMSFLO = SkSplitCSV(i)
050200030721     C                   ENDIF
050300160805     C*** DESCRIZ
050400160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCRIZ'
050500160805     C                   EVAL      VMSDESCRIZ = SkSplitCSV(i)
050600160805     C                   ENDIF
050700160805     C*** DESCIT
050800160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCIT'
050900160805     C                   EVAL      VMSDESCIT = SkSplitCSV(i)
051000160805     C                   ENDIF
051100160805     C*** DESCEN
051200160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCEN'
051300160805     C                   EVAL      VMSDESCEN = SkSplitCSV(i)
051400160805     C                   ENDIF
051500160805     C*** DESCFR
051600160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCFR'
051700160805     C                   EVAL      VMSDESCFR = SkSplitCSV(i)
051800160805     C                   ENDIF
051900160805     C*** DESCDE
052000160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCDE'
052100160805     C                   EVAL      VMSDESCDE = SkSplitCSV(i)
052200160805     C                   ENDIF
052300160805     C*** DESCES
052400160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDESCES'
052500160805     C                   EVAL      VMSDESCES = SkSplitCSV(i)
052600160805     C                   ENDIF
052700030721     C*
052800030721     C* Reperisco quindi i campi numerici...
052900160805     C*** DDE
053000160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDDE'              VMSDSC
053100030721     C                   EVAL      PiStr=SkSplitCSV(i)
053200030721     C                   EXSR      CHKNUM
053300100517     C                   IF        PiInt=*on
053400160805     C                   Z-ADD     PiVal         VMSDDE
053500030721     C                   ELSE
053600030721     C                   SETON                                        32
053700160805     C                   EVAL      VMSDDE = *zeros
053800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
053900160805     C                             + ' ' + 'VMSDDE'
054000030721     C                   ENDIF
054100030721     C                   ENDIF
054200160805     C*** DSC
054300160805     C                   IF        %trim(SkSplitFLD(i)) = 'VMSDSC'
054400160805     C                   EVAL      PiStr=SkSplitCSV(i)
054500160805     C                   EXSR      CHKNUM
054600160805     C                   IF        PiInt=*on
054700160805     C                   Z-ADD     PiVal         VMSDSC
054800160805     C                   ELSE
054900160805     C                   SETON                                        32
055000160805     C                   EVAL      VMSDSC = *zeros
055100160805     C                   EVAL      vinmsg = %trimr(vinmsg)
055200160805     C                             + ' ' + 'VMSDSC'
055300160805     C                   ENDIF
055400160805     C                   ENDIF
055500030715     C*
055600030715     C                   ENDIF
055700030715     C                   EVAL      i = i + 1
055800030715     C                   ENDDO
055900160808     C*
056000160808     C***  ===>  Gestione campi "particolari" fuori ciclo
056100160808     C*
056200160808     C* Completamento campi non ricevuti da mappatura standard
056300160808     C                   eval      K_VMSATB    = *blanks
056400160923     C                   eval      K_VMSPROC   = '*EDIFACT'
056500160808     C                   eval      K_VMSKSU    = *blanks
056600160808     C                   eval      K_VMSCAUTIP = VMSCAUTIP
056700160808     C                   eval      K_VMSCAUBRT = VMSCAUBRT
056800160808     C                   eval      K_VMSDDE    = 20010101
056900160808     C     KeyVMS_002C   chain     tivms02i
057000160923     C                   if        %found(tivms02i)
057100160808     C*
057200160808     C                   if        VMSCAUTT   = *blanks
057300160808     C                   eval      VMSCAUTT   = K_VMSCAUTT
057400160808     C                   endif
057500160923     C*
057600160923     C                   if        VMSSTSOPZ  = *blanks
057700160923     C                   eval      VMSSTSOPZ  = K_VMSSTSOPZ
057800160923     C                   endif
057900160923     C*
058000160923     C                   if        VMSFASOP   = *blanks
058100160923     C                   eval      VMSFASOP   = K_VMSFASOP
058200160923     C                   endif
058300160923     C*
058400160923     C                   if        VMSFLO     = *blanks
058500160923     C                   eval      VMSFLO     = K_VMSFLO
058600160923     C                   endif
058700160808     C*
058800160808     C                   if        VMSDESCRIZ = *blanks
058900160808     C                   eval      VMSDESCRIZ = K_VMSDESCRIZ
059000160808     C                   endif
059100160808     C*
059200160808     C                   if        VMSDESCIT  = *blanks
059300160808     C                   eval      VMSDESCIT  = K_VMSDESCIT
059400160808     C                   endif
059500160808     C*
059600160808     C                   if        VMSDESCEN  = *blanks
059700160808     C                   eval      VMSDESCEN  = K_VMSDESCEN
059800160808     C                   endif
059900160808     C*
060000160808     C                   if        VMSDESCFR  = *blanks
060100160808     C                   eval      VMSDESCFR  = K_VMSDESCFR
060200160808     C                   endif
060300160808     C*
060400160808     C                   if        VMSDESCDE  = *blanks
060500160808     C                   eval      VMSDESCDE  = K_VMSDESCDE
060600160808     C                   endif
060700160808     C*
060800160808     C                   if        VMSDESCES  = *blanks
060900160808     C                   eval      VMSDESCES  = K_VMSDESCES
061000160808     C                   endif
061100160808     C*
061200160808     C                   if        VMSDDE     = *zeros
061300160808     C                   eval      VMSDDE     = K_VMSDDE
061400160808     C                   endif
061500160808     C*
061600160808     C                   if        VMSDSC     = *zeros
061700160808     C                   eval      VMSDSC     = K_VMSDSC
061800160808     C                   endif
061900160808     C*
062000160808     C                   endif
062100160808     C*
062200160808     C* Verifiche finali ed attribuzione valori default
062300160808     C                   if        VMSDDE = *zeros
062400160808     C                   eval      VMSDDE = 20010101
062500160808     C                   endif
062600160808     C*
062700160808     C                   if        VMSDSC = *zeros
062800160808     C                   eval      VMSDSC = 20391231
062900160808     C                   endif
063000160808     C*
063100160808     C                   if        VMSCAUTIP = *blanks
063200160808     C                   seton                                        31
063300160808     C                   endif
063400160808     C*
063500160808     C                   if        VMSCAUBRT = *blanks
063600160808     C                   seton                                        31
063700160808     C                   endif
063800160808     C*
063900160808     C                   if        VMSSTSCLI  =  *blanks AND
064000160808     C                             VMSPROC   <> '*STD'
064100160808     C                   seton                                        31
064200160808     C                   endif
064300160808     C*
064400160808     C***  <===  -----------------------------------------
064500020204     C*
064600030715     C                   ENDIF
064700020204     C*
064800010607     C                   ENDSR
064900010607     C*----------------------------------------------------*
065000160923
065100160923
065200160923
065300160923     C*----------------------------------------------------*
065400160923     C*  SCRITTURA / AGGIORNAMENTO TIVMS
065500160923     C*----------------------------------------------------*
065600160923     C     WRIVMS        BEGSR
065700160923     C*
065800160923     C* Con il buffer di output corrente verifico se causale già esistente
065900160923     C     KeyVMS_001C   chain     tivms01i
066000160923     C                   if        not %found(tivms01i)
066100160923     C*
066200160923     C* Se inesistente => inserisco
066300160923     C                   eval      tivmsds_O = tivmsds
066400160923     C*
066500160923     C                   write     TIVMS000
066600160923     C*
066700160923     C* Se già esiste => aggiorno
066800160923     C                   else
066900160923     C*
067000160923     C                   if        VMSCAUTT     <> *blanks
067100160923     C                   eval      O_VMSCAUTT    = VMSCAUTT
067200160923     C                   endif
067300160923     C*
067400160923     C                   if        VMSSTSCLI    <> *blanks
067500160923     C                   eval      O_VMSSTSCLI   = VMSSTSCLI
067600160923     C                   endif
067700160923     C*
067800160923     C                   if        VMSSTSOPZ    <> *blanks
067900160923     C                   eval      O_VMSSTSOPZ   = VMSSTSOPZ
068000160923     C                   endif
068100160923     C*
068200160923     C                   if        VMSFASOP     <> *blanks
068300160923     C                   eval      O_VMSFASOP    = VMSFASOP
068400160923     C                   endif
068500160923     C*
068600160923     C                   if        VMSFLO       <> *blanks
068700160923     C                   eval      O_VMSFLO      = VMSFLO
068800160923     C                   endif
068900160923     C*
069000160923     C                   if        VMSDESCRIZ   <> *blanks
069100160923     C                   eval      O_VMSDESCRIZ  = VMSDESCRIZ
069200160923     C                   endif
069300160923     C*
069400160923     C                   if        VMSDESCIT    <> *blanks
069500160923     C                   eval      O_VMSDESCIT   = VMSDESCIT
069600160923     C                   endif
069700160923     C*
069800160923     C                   if        VMSDESCEN    <> *blanks
069900160923     C                   eval      O_VMSDESCEN   = VMSDESCEN
070000160923     C                   endif
070100160923     C*
070200160923     C                   if        VMSDESCFR    <> *blanks
070300160923     C                   eval      O_VMSDESCFR   = VMSDESCFR
070400160923     C                   endif
070500160923     C*
070600160923     C                   if        VMSDESCDE    <> *blanks
070700160923     C                   eval      O_VMSDESCDE   = VMSDESCDE
070800160923     C                   endif
070900160923     C*
071000160923     C                   if        VMSDESCES    <> *blanks
071100160923     C                   eval      O_VMSDESCES   = VMSDESCES
071200160923     C                   endif
071300160923     C*
071400160923     C                   update    TIVMS000
071500160923     C*
071600160923     C                   endif
071700160923     C*
071800160923     C                   ENDSR
071900040714
072000010330
072100010330
072200010330     C*----------------------------------------------------*
072300010330     C*  CONTROLLO NUMERICITA' CAMPI
072400010330     C*----------------------------------------------------*
072500010330     C     CHKNUM        BEGSR
072600010330     C*
072700010606     C                   IF        PiDecChr = *blanks
072800030715     C                   EVAL      PiDecChr = CharNUM
072900010606     C                   ENDIF
073000010606     C*
073100010606     C                   CALL(e)   'ISNUMERIC'
073200010330     C                   PARM                    PiStr            30
073300010606     C                   PARM                    PiDecChr          1
073400010330     C                   PARM      *ZEROS        PiVal            30 9
073500010330     C                   PARM      '0'           PiInt             1
073600010330     C                   PARM      '0'           PiNum             1
073700010330     C                   IF        %error
073800010606     C                   EVAL      PiNum=*off
073900010330     C                   ENDIF
074000010330     C*
074100010330     C                   ENDSR
074200010330     C***
074300010601
074400010601
074500010601      /TITLE Invio dei dati al punto operativo.
074600010601     C     opeini        BEGSR
074700010601     C*
074800010601     C* Inizializzo flag e contatori operativi
074900010601     C                   movel     '0'           flgGiro           1
075000010601     C                   movel     '0'           flgMulti          1
075100010601     C                   movel     '1'           flgStato          1
075200010615     C                   movel     '0'           flgOk             1
075300010601     C                   z-add     *zeros        cntNonEl         10 0
075400010601     C*
075500010601     C                   ENDSR
075600010601     C***
075700050421
075800050421
075900160923     C***     *pssr         BEGSR
076000050421     C*
076100160923     C***                eval      wrkesito = '2'
076200050421     C*
076300160923     C***                ENDSR     '*CANCL'
076400050421     C***
076500010330
076600010330
076700000613     C     *inzsr        BEGSR
076800990910     C*
076900990910     C     *entry        plist
077000990920     C                   parm                    tivlrds
077100990921     C                   parm      wrkesito      esito
077200000724     C                   parm                    prmlit
077300000710     C                   parm                    prmfir
077400160808     C*
077500160808     C     KeyVMS_002C   KLIST
077600160808     C                   KFLD                    K_VMSATB
077700160808     C                   KFLD                    K_VMSPROC
077800160808     C                   KFLD                    K_VMSKSU
077900160808     C                   KFLD                    K_VMSCAUTIP
078000160808     C                   KFLD                    K_VMSCAUBRT
078100160808     C                   KFLD                    K_VMSDDE
078200160923     C*
078300160923 xxx C     KeyVMS_001C   KLIST
078400160923     C                   KFLD                    VMSPROC
078500160923     C                   KFLD                    VMSKSU
078600160923     C                   KFLD                    VMSDDE
078700160923     C                   KFLD                    VMSCAUTIP
078800160923     C                   KFLD                    VMSCAUBRT
078900010330     C*
079000010330     C* CALCOLA LA DATA CORRENTE
079100160805     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079200160805     C                   eval      datcor = %dec(%date() : *ISO)
079300000613     C*
079400000613     C                   ENDSR
079500000613     C***
