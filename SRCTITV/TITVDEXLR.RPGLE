000100130220      /TITLE Upload via Internet: traduzione in EDIVADWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500130220     FEDIVADwr  O    E             DISK    usropn
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
005200130220     C                   exsr      rwvad
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
009600130220     C* RWVAD   LEGGE TIVIN00R E SCRIVE EDIVADWF              *
009700010330     C*--------------------------------------------------------
009800130220     C     RWVAD         BEGSR
009900010330     C*
010000010330     C                   if        not %open(tivin00r)
010100010330     C                   open      tivin00r
010200010330     C                   endif
010300130220     C                   if        not %open(edivadwr)
010400130220     C                   open      edivadwr
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
016900130220     C                   exsr      impvad
017000010601     C*
017100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017200130220     C                   eval      depfil = VADLNP
017300010601     C                   exsr      repfil
017400010601     C                   if        depfil = invfil
017500021025     C                   if        vlrpoi = 999
017600130220     C                   move(P)   invfil        VADFGS
017700021025     C                   else
017800130220     C                   move(P)   vlrpoi        VADFGS
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
019100130220     C                   EVAL      VADCMR = wCMR
019200130220     C                   EVAL      VADCNT = 1
019300121109     C*
019400130220     C  N31              write     EDIVAD00
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
024200130220     C                   if        %open(edivadwr)
024300130220     C                   close     edivadwr
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
027400130220     C                   CLEAR                   EDIVAD00
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
032800130220     C* CCM
032900130220     C                   IF        %subst(
033000130220     C                             %subst(vlrppt:posDaDft+1:
033100130220     C                             posADft-posDaDft-1):1:3)
033200130220     C                             = 'CCM'
033300130220     C                   EVAL      PiStr=%trim(%subst(
033400130220     C                             %subst(vlrppt:posDaDft+1:
033500130220     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033600130220     C                   EXSR      CHKNUM
033700130220     C                   IF        PiInt=*on
033800130220     C                   Z-ADD     PiVal         VADCCM
033900130220     C                   ENDIF
034000130220     C                   ENDIF
034100130220     C* LNP
034200130220     C                   IF        %subst(
034300130220     C                             %subst(vlrppt:posDaDft+1:
034400130220     C                             posADft-posDaDft-1):1:3)
034500130220     C                             = 'LNP'
034600130220     C                   EVAL      PiStr=%trim(%subst(
034700130220     C                             %subst(vlrppt:posDaDft+1:
034800130220     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034900130220     C                   EXSR      CHKNUM
035000130220     C                   IF        PiInt=*on
035100130220     C                   Z-ADD     PiVal         VADLNP
035200130220     C                   ENDIF
035300130220     C                   ENDIF
035400130220     C* NRS
035500130220     C                   IF        %subst(
035600130220     C                             %subst(vlrppt:posDaDft+1:
035700130220     C                             posADft-posDaDft-1):1:3)
035800130220     C                             = 'NRS'
035900130220     C                   EVAL      PiStr=%trim(%subst(
036000130220     C                             %subst(vlrppt:posDaDft+1:
036100130220     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036200130220     C                   EXSR      CHKNUM
036300130220     C                   IF        PiInt=*on
036400130220     C                   Z-ADD     PiVal         VADNRS
036500130220     C                   ENDIF
036600130220     C                   ENDIF
036700130220     C* NCL
036800130220     C                   IF        %subst(
036900130220     C                             %subst(vlrppt:posDaDft+1:
037000130220     C                             posADft-posDaDft-1):1:3)
037100130220     C                             = 'NCL'
037200130220     C                   EVAL      PiStr=%trim(%subst(
037300130220     C                             %subst(vlrppt:posDaDft+1:
037400130220     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037500130220     C                   EXSR      CHKNUM
037600130220     C                   IF        PiInt=*on
037700130220     C                   Z-ADD     PiVal         VADNCL
037800130220     C                   ENDIF
037900130220     C                   ENDIF
038000130220     C* CDP
038100130220     C                   IF        %subst(
038200130220     C                             %subst(vlrppt:posDaDft+1:
038300130220     C                             posADft-posDaDft-1):1:3)
038400130220     C                             = 'CDP'
038500130220     C                   EVAL      VADCDP=%trim(%subst(
038600130220     C                             %subst(vlrppt:posDaDft+1:
038700130220     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038800130220     C                   ENDIF
038900030822     C* ...
039000030822     C                   ENDIF
039100030822     C                   ENDDO
039200020204     C*
039300010330     C                   ENDSR
039400010607     C*----------------------------------------------------*
039500130220     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAD)
039600010607     C*----------------------------------------------------*
039700130220     C     IMPVAD        BEGSR
039800010607     C*
039900010607     C                   SETOFF                                       3132
040000030515     C*
040100130220     C                   MOVEL     datcor        VADAAS
040200010607     C*
040300030715     C**********
040400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
040500030715     C**********
040600030715     C                   IF        wGiro = *zeros
040700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
040800031201     C     minu:maiu     XLATE     vindta        vindta
040900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
041000040324     C                   DOW       posDa <= %len(%trimr(vindta))
041100030715     C*
041200030715     C* Gestisco il 1° campo
041300030715     C                   IF        i = 1
041400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
041500030715     C                   EVAL      posDa = 1
041600030715     C* Eseguo lo scan x trovare la fine del primo campo
041700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041800030715     C* A questo punto "estraggo" il campo corrente
041900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042000030715     C                                                    (posA-posDa))
042100030715     C* X i campi successivi al 1°
042200030715     C                   ELSE
042300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
042400030715     C                   EVAL      posDa = posA + 1
042500030715     C* Eseguo lo scan x trovare la fine del campo corrente
042600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042700030715     C* Gestisco l'ultimo campo
042800030715     C                   IF        posA = *zeros
042900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
043000030715     C* A questo punto "estraggo" il campo corrente
043100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043200030715     C                                                    (posA-posDa))
043300030715     C                   LEAVE
043400030715     C                   ELSE
043500030715     C* A questo punto "estraggo" il campo corrente
043600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043700030715     C                                                    (posA-posDa))
043800030715     C                   ENDIF
043900030715     C                   ENDIF
044000030715     C* Incremento il contatore d campo
044100030715     C                   EVAL      i = i +1
044200030715     C                   ENDDO
044300030715     C*
044400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044500030715     C* delimitazione testo
044600030715     C                   EVAL      i = 1
044700030715     C                   DOW       i < %elem(SkSplitFLD)
044800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
044900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
045000030715     C                   EVAL      i = i + 1
045100030715     C                   ENDDO
045200030715     C                   EVAL      wGiro = 1
045300030715     C                   ELSE
045400030715     C                   EVAL      wGiro = 2
045500031204     C**********
045600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
045700031204     C* d separatore campo
045800031204     C**********
045900040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
046000040304     C                   EVAL      vindta = ' ' + vindta
046100040304     C                   ENDIF
046200040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
046300040304     C***                EVAL      vindta = %subst(vindta:2)
046400040304     C***                ENDDO
046500031204     C*
046600031204     C                   Z-ADD     *zeros        lunghInput        4 0
046700040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
046800040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
046900040304     C                   EVAL      vindta = vindta + ' '
047000040304     C                   ENDIF
047100040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
047200040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
047300040304     C***                EVAL      lunghInput = %len(%trim(vindta))
047400040304     C***                ENDDO
047500030715     C**********
047600030715     C* Effettuo lo split del campo dati d input
047700030715     C**********
047800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047900040324     C                   DOW       posDa <= %len(%trimr(vindta))
048000030715     C*
048100030715     C* Gestisco il 1° campo
048200030715     C                   IF        i = 1
048300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
048400030715     C                   EVAL      posDa = 1
048500030715     C* Eseguo lo scan x trovare la fine del primo campo
048600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048700030715     C* A questo punto "estraggo" il campo corrente
048800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048900030715     C                                                    (posA-posDa))
049000030715     C* X i campi successivi al 1°
049100030715     C                   ELSE
049200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
049300030715     C                   EVAL      posDa = posA + 1
049400031204     C* Verifico che nn vi sia il campo nn valorizzato
049500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
049600031204     C* Se campo nn valorizzato skippo al prossimo
049700031204     C                   EVAL      SkSplitCSV(i) = *blanks
049800031204     C                   EVAL      posA  = posA + 1
049900031204     C                   ELSE
050000030715     C* Eseguo lo scan x trovare la fine del campo corrente
050100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
050200030715     C* Gestisco l'ultimo campo
050300030715     C                   IF        posA = *zeros
050400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
050500030715     C* A questo punto "estraggo" il campo corrente
050600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
050700030715     C                                                    (posA-posDa))
050800030715     C                   LEAVE
050900030715     C                   ELSE
051000030715     C* A questo punto "estraggo" il campo corrente
051100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
051200030715     C                                                    (posA-posDa))
051300030715     C                   ENDIF
051400031204     C                   ENDIF
051500030715     C                   ENDIF
051600030715     C* Incremento il contatore d campo
051700030715     C                   EVAL      i = i +1
051800030715     C                   ENDDO
051900030715     C*
052000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
052100030715     C* delimitazione testo
052200030715     C                   EVAL      i = 1
052300030715     C                   DOW       i < %elem(SkSplitCSV)
052400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
052500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
052600030715     C                   EVAL      i = i + 1
052700030715     C                   ENDDO
052800030715     C*
052900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
053000030715     C                   EVAL      i = 1
053100030715     C                   DOW       i < %elem(SkSplitFLD)
053200030715     C*
053300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
053400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
053500030715     C                             SkSplitCSV(i) <> *blanks
053600130220     C*** ATB
053700130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADATB'
053800130220     C                   EVAL      VADATB = SkSplitCSV(i)
053900130220     C                   ENDIF
054000130220     C*** CDP
054100130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP'
054200130220     C                   EVAL      VADCDP = SkSplitCSV(i)
054300130220     C                   ENDIF
054400130220     C*
054500130220     C* Reperisco quindi i campi numerici...
054600130220     C*** NSP
054700130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADNSP'
054800130220     C                   EVAL      PiStr=SkSplitCSV(i)
054900130220     C                   EXSR      CHKNUM
055000130220     C                   IF        PiInt=*on AND
055100130220     C                             PiVal<=9999999
055200130220     C                   Z-ADD     PiVal         VADNSP
055300130220     C                   ELSE
055400130220     C                   SETON                                        32
055500130220     C                   EVAL      VADNSP = *zeros
055600130220     C                   EVAL      vinmsg = %trimr(vinmsg)
055700130220     C                             + ' ' + 'VADNSP'
055800130220     C                   ENDIF
055900130220     C                   ENDIF
056000130220     C*** CCM
056100130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADCCM'
056200130220     C                   EVAL      PiStr=SkSplitCSV(i)
056300130220     C                   EXSR      CHKNUM
056400130220     C                   IF        PiInt=*on      AND
056500130220     C                             PiVal<=9999999 AND
056600130220     C                             PiVal>*zeros
056700130220     C                   Z-ADD     PiVal         VADCCM
056800130220     C                   ELSE
056900130220     C                   SETON                                        32
057000130220     C                   EVAL      VADCCM = *zeros
057100130220     C                   EVAL      vinmsg = %trimr(vinmsg)
057200130220     C                             + ' ' + 'VADCCM'
057300130220     C                   ENDIF
057400130220     C                   ENDIF
057500130220     C*** LNP
057600130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADLNP'
057700130220     C                   EVAL      PiStr=SkSplitCSV(i)
057800130220     C                   EXSR      CHKNUM
057900130220     C                   IF        PiInt=*on    AND
058000130220     C                             PiVal<=999   AND
058100130220     C                             PiVal>*zeros
058200130220     C                   Z-ADD     PiVal         VADLNP
058300130220     C                   ELSE
058400130220     C                   SETON                                        32
058500130220     C                   EVAL      VADLNP = *zeros
058600130220     C                   EVAL      vinmsg = %trimr(vinmsg)
058700130220     C                             + ' ' + 'VADLNP'
058800130220     C                   ENDIF
058900130220     C                   ENDIF
059000130220     C*** NRS
059100130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADNRS'
059200130220     C                   EVAL      PiStr=SkSplitCSV(i)
059300130220     C                   EXSR      CHKNUM
059400130220     C                   IF        PiInt=*on AND
059500130220     C                             PiVal<=99
059600130220     C                   Z-ADD     PiVal         VADNRS
059700130220     C                   ELSE
059800130220     C                   SETON                                        32
059900130220     C                   EVAL      vinmsg = %trimr(vinmsg)
060000130220     C                             + ' ' + 'VADNRS'
060100130220     C                   ENDIF
060200130220     C                   ENDIF
060300130220     C*** NCL
060400130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL'
060500130220     C                   EVAL      PiStr=SkSplitCSV(i)
060600130220     C                   EXSR      CHKNUM
060700130220     C                   IF        PiInt=*on
060800130220     C                   Z-ADD     PiVal         VADNCL
060900130220     C                   ELSE
061000130220     C                   SETON                                        32
061100130220     C                   EVAL      vinmsg = %trimr(vinmsg)
061200130220     C                             + ' ' + 'VADNCL'
061300130220     C                   ENDIF
061400130220     C                   ENDIF
061500130220     C*** NCD
061600130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCD'
061700130220     C                   EVAL      PiStr=SkSplitCSV(i)
061800130220     C                   EXSR      CHKNUM
061900130220     C                   IF        PiInt=*on
062000130220     C                   Z-ADD     PiVal         VADNCD
062100130220     C                   ELSE
062200130220     C                   SETON                                        32
062300130220     C                   EVAL      vinmsg = %trimr(vinmsg)
062400130220     C                             + ' ' + 'VADNCD'
062500130220     C                   ENDIF
062600130220     C                   ENDIF
062700130220     C*** NCA
062800130220     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCA'
062900130220     C                   EVAL      PiStr=SkSplitCSV(i)
063000130220     C                   EXSR      CHKNUM
063100130220     C                   IF        PiInt=*on
063200130220     C                   Z-ADD     PiVal         VADNCA
063300130220     C                   ELSE
063400130220     C                   SETON                                        32
063500130220     C                   EVAL      vinmsg = %trimr(vinmsg)
063600130220     C                             + ' ' + 'VADNCA'
063700130220     C                   ENDIF
063800130220     C                   ENDIF
063900030715     C*
064000030715     C                   ENDIF
064100030715     C                   EVAL      i = i + 1
064200030715     C                   ENDDO
064300020204     C*
064400030715     C                   ENDIF
064500020204     C*
064600010607     C                   ENDSR
064700010607     C*----------------------------------------------------*
064800040714
064900010330
065000010330
065100010330     C*----------------------------------------------------*
065200010330     C*  CONTROLLO NUMERICITA' CAMPI
065300010330     C*----------------------------------------------------*
065400010330     C     CHKNUM        BEGSR
065500010330     C*
065600010606     C                   IF        PiDecChr = *blanks
065700030715     C                   EVAL      PiDecChr = CharNUM
065800010606     C                   ENDIF
065900010606     C*
066000010606     C                   CALL(e)   'ISNUMERIC'
066100010330     C                   PARM                    PiStr            30
066200010606     C                   PARM                    PiDecChr          1
066300010330     C                   PARM      *ZEROS        PiVal            30 9
066400010330     C                   PARM      '0'           PiInt             1
066500010330     C                   PARM      '0'           PiNum             1
066600010330     C                   IF        %error
066700010606     C                   EVAL      PiNum=*off
066800010330     C                   ENDIF
066900010330     C*
067000010330     C                   ENDSR
067100010330     C***
067200010330
067300010601
067400010601
067500010601
067600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
067700010601     C     repfil        BEGSR
067800010601     C*
067900010601     C                   if        invfil = *zeros and
068000010601     C                             depfil > *zeros and
068100010629     C                             (vinflg = *blanks or
068200010629     C                              vinflg = *zeros)
068300010601     C
068400010601     C                   eval      invfil = depfil
068500010601     C                   endif
068600010601     C*
068700010601     C                   if        depfil <> invfil and
068800010601     C                             invfil > *zeros
068900010601     C                   eval      flgMulti = '1'
069000010601     C                   if        vinflg = *blanks
069100010601     C                   add       1             cntNonEl
069200010601     C                   endif
069300010601     C                   endif
069400010601     C*
069500010601     C                   if        vinflg = '2'
069600010601     C                   eval      flgStato = '2'
069700010601     C                   endif
069800010601     C*
069900010601     C                   ENDSR
070000010601     C***
070100010601
070200010601
070300010601
070400010330
070500010330
070600010330
070700990920      /TITLE Invio dei dati al punto operativo.
070800000613     C     invio         BEGSR
070900990920     C*
071000990920     C                   reset                   dscmz
071100010601     C                   move      invfil        cmzdst
071200990920     C                   eval      cmzfld = vlrfou
071300990920     C                   eval      cmzmbd = vlrhdl
071400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500000710     C                   if        prmfir = *blanks
071600130220     C                   eval      cmzfla = 'EDIVAD0F'
071700130220     C                   eval      cmzmba = 'EDIVAD0F'
071800000710     C                   else
071900000710     C                   eval      cmzfla = prmfir
072000000710     C                   eval      cmzmba = prmfir
072100000710     C                   endif
072200990920     C                   eval      cmznrr = *zeros
072300990920     C                   move      §ctrok        cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500990920     C                   call(e)   'TIS711C'
072600990920     C                   parm                    dscmz
072700990921     C                   parm      *blanks       esito
072800990920     C                   if        %error
072900990920     C                             or cmzerr = '1'
073000990921     C                             or esito  = '1'
073100000710     C                   eval      wrkesito = '3'
073200990920     C                   endif
073300990920     C*
073400000613     C                   ENDSR
073500990910
073600010601
073700010601
073800010601
073900010601
074000010601      /TITLE Invio dei dati al punto operativo.
074100010601     C     opeini        BEGSR
074200010601     C*
074300010601     C* Inizializzo flag e contatori operativi
074400010601     C                   movel     '0'           flgGiro           1
074500010601     C                   movel     '0'           flgMulti          1
074600010601     C                   movel     '1'           flgStato          1
074700010615     C                   movel     '0'           flgOk             1
074800010601     C                   z-add     *zeros        cntNonEl         10 0
074900010601     C                   z-add     *zeros        depfil            3 0
075000010601     C                   z-add     *zeros        invfil            3 0
075100010601     C*
075200010601     C                   ENDSR
075300010601     C***
075400050421
075500050421
075600050421
075700050421     C     *pssr         BEGSR
075800050421     C*
075900050421     C                   eval      wrkesito = '2'
076000050421     C*
076100050421     C                   ENDSR     '*CANCL'
076200050421     C***
076300010601
076400010601
076500010601
076600010330
076700010330
076800000613     C     *inzsr        BEGSR
076900990910     C*
077000990910     C     *entry        plist
077100990920     C                   parm                    tivlrds
077200990921     C                   parm      wrkesito      esito
077300000724     C                   parm                    prmlit
077400000710     C                   parm                    prmfir
077500010330     C*
077600010330     C* CALCOLA LA DATA CORRENTE
077700121109     C                   time                    wn14             14 0
077800121109     C                   movel     wn14          oracor            6 0          *ORA
077900121109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
078000121109     C                   eval      datcor = %dec(%date() : *ISO)
078100121109     C*
078200121109     C* Determino il CMR
078300121109     C                   movel     *blanks       wCMR             35
078400130220     C                   eval      wCMR = %char(datcor)
078500000613     C*
078600000613     C                   ENDSR
078700000613     C***
