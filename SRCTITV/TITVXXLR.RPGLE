000100080729      /TITLE Upload via Internet: traduzione in FIVAXWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500080729     FFIVAXwwr  O    E             DISK    usropn
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
006000080729     C                   exsr      rwvax
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
009400000616     C*
009500010601     C                   ENDSR
009600010601     C***
009700000613
009800010330
009900010601
010000010601
010100010601
010200010330     C*--------------------------------------------------------
010300080729     C* RWVAX   LEGGE TIVIN00R E SCRIVE FIVAXWWF              *
010400010330     C*--------------------------------------------------------
010500080729     C     RWVAX         BEGSR
010600010330     C*
010700010330     C                   if        not %open(tivin00r)
010800010330     C                   open      tivin00r
010900010330     C                   endif
011000080729     C                   if        not %open(fivaxwwr)
011100080729     C                   open      fivaxwwr
011200010330     C                   endif
011300010330     C*
011400010604     C                   clear                   §CTROK
011500010604     C                   clear                   §CTRMO
011600010604     C                   clear                   §CTRNO
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
014000010330     C                   add       1             rrnum
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
015300090403     C*
015400090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015500090403     C* separatori campi
015600090403     C                   if        CharTXT <> *blanks
015700090403     C                   z-add     *zeros        wLenVINDTA        4 0
015800090403     C                   z-add     *zeros        wFlgVINDTA        1 0
015900090403     C                   setoff                                       80
016000090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016100090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016200090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016300090403     C                   if        not *in80
016400090403     C                   seton                                        80
016500090403     C                   else
016600090403     C                   setoff                                       80
016700090403     C                   endif
016800090403     C                   endif
016900090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017000090403     C                             and *in80 = *on
017100090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017200090403     C                   endif
017300090403     C                   enddo
017400090403     C                   endif
017500090403     C*
017600080729     C                   exsr      impvax
017700010601     C*
017800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017900080729     C                   eval      depfil = VAXLNP
018000010601     C                   exsr      repfil
018100010601     C                   if        depfil = invfil
018200021025     C                   if        vlrpoi = 999
018300080729     C                   move(P)   invfil        VAXFGS
018400021025     C                   else
018500080729     C                   move(P)   vlrpoi        VAXFGS
018600021025     C                   endif
018700050113     C                   endif
018800010601     C*
018900010601     C                   exsr      PREELA
019000010601     C*
019100010604     C* Ebbene...
019200010604     C*
019300030822     C  N31              add       1             §CTROK            7 0
019400030822     C   32              add       1             §CTRMO            7 0
019500030822     C   31              add       1             §CTRNO            7 0
019600030822     C                   if        wGiro = 2
019700080729     C  N31              write     FIVAX000
019800030822     C                   endif
019900010604     C*
020000010604     C                   if        *in31 = *off and
020100010604     C                             *in32 = *off
020200010604     C                   eval      vinflg = '1'
020300010604     C                   else
020400010604     C                   eval      vinflg = '2'
020500010604     C                   endif
020600010601     C*
020700010604     C                   endif
020800010604     C*
020900010330     C                   else
021000010330     C                   eval      vinflg = '1'
021100010330     C                   endif
021200010601     C*
021300010601     C  N70              update    tivin000
021400010330     C*
021500030822     C  N70              enddo
021600010601     C
021700010601     C*
021800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021900010601     C                   if        cntNonEl = *zeros or
022000010601     C                             flgMulti = '0'
022100010330     C* Se non ci sono record con errori ...
022200010601     C                   if        §ctrno = 0 and
022300010604     C                             §ctrmo = 0 and
022400010601     C                             flgStato <> '2'
022500010330     C* ... restituisco esito OK.
022600010330     C                   eval      wrkesito = '0'
022700010330     C                   else
022800010330     C                   if        §ctrok > 0
022900010330     C                   eval      wrkesito = '1'
023000010330     C                   else
023100010615     C                   if        flgOk = '0'
023200010615     C                   eval      wrkesito = '2'
023300010615     C                   else
023400010615     C                   eval      wrkesito = '6'
023500010615     C                   endif
023600010330     C                   endif
023700010330     C                   endif
023800010601     C                   else
023900010601     C                   eval      wrkesito = '9'
024000010601     C                   endif
024100010330     C*
024200010330     C                   if        %open(tivin00r)
024300010330     C                   close     tivin00r
024400010330     C                   endif
024500080729     C                   if        %open(fivaxwwr)
024600080729     C                   close     fivaxwwr
024700010330     C                   endif
024800010601     C*
024900010601     C                   if        vlrpoi <> 999
025000010601     C                   eval      invfil = vlrpoi
025100010601     C                   endif
025200010330     C*
025300010330     C                   if        §ctrok > 0
025400010601     C                             and invfil > *zeros
025500010330     C                   exsr      invio
025600010330     C                   endif
025700010601     C*
025800010618     C                   if        flgGiro = '1'
025900010601     C                   exsr      endela
026000010618     C                   endif
026100010330     C*
026200010330     C                   ENDSR
026300010330     C***
026400010601
026500010601
026600010601
026700010330     C*----------------------------------------------------*
026800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026900010330     C*----------------------------------------------------*
027000010330     C     INZVAR        BEGSR
027100010330     C*
027200030822     C* Inizializzo variabili di wrk
027300020204     C                   Z-ADD     *zeros        Num5_0            5 0
027400090403     C                   z-add     *zeros        invfil
027500030822     C*
027600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027700080729     C                   CLEAR                   FIVAX000
027800030822     C                   CLEAR                   SkSplitCSV
027900030822     C                   EVAL      i = 1
028000030822     C                   EVAL      posDa = *zeros
028100030822     C                   EVAL      posA  = *zeros
028200030822     C*
028300030822     C* Reimposto i valori di default
028400030822     C                   EXSR      DEFCAM
028500030822     C*
028600010330     C                   ENDSR
028700010330     C*----------------------------------------------------*
028800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028900010330     C*----------------------------------------------------*
029000010330     C     DEFCAM        BEGSR
029100030822     C*
029200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029300030715     C* e delimitatore testo.
029400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029700090403     C*
029800090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
029900090403     C                   EVAL      CharSOS = CharNUM
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
034100080729     C                   Z-ADD     PiVal         VAXCCM
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
035400080729     C                   Z-ADD     PiVal         VAXLNP
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700080729     C* TMD
035800030822     C                   IF        %subst(
035900030822     C                             %subst(vlrppt:posDaDft+1:
036000030822     C                             posADft-posDaDft-1):1:3)
036100080729     C                             = 'TMD'
036200080729     C                   EVAL      VAXTMD=%trim(%subst(
036300030822     C                             %subst(vlrppt:posDaDft+1:
036400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500030822     C                   ENDIF
036600080729     C* TRK
036700080729     C                   IF        %subst(
036800080729     C                             %subst(vlrppt:posDaDft+1:
036900080729     C                             posADft-posDaDft-1):1:3)
037000080729     C                             = 'TRK'
037100080729     C                   EVAL      VAXTRK=%trim(%subst(
037200080729     C                             %subst(vlrppt:posDaDft+1:
037300080729     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037400080729     C                   ENDIF
037500030822     C* ...
037600030822     C                   ENDIF
037700030822     C                   ENDDO
037800020204     C*
037900010330     C                   ENDSR
038000010607     C*----------------------------------------------------*
038100080729     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAX)
038200010607     C*----------------------------------------------------*
038300080729     C     IMPVAX        BEGSR
038400010607     C*
038500010607     C                   SETOFF                                       3132
038600030515     C*
038700080729     C                   MOVEL     datcor        VAXAAS
038800010607     C*
038900030715     C**********
039000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
039100030715     C**********
039200030715     C                   IF        wGiro = *zeros
039300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
039400031201     C     minu:maiu     XLATE     vindta        vindta
039500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
039600040324     C                   DOW       posDa <= %len(%trimr(vindta))
039700030715     C*
039800030715     C* Gestisco il 1° campo
039900030715     C                   IF        i = 1
040000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
040100030715     C                   EVAL      posDa = 1
040200030715     C* Eseguo lo scan x trovare la fine del primo campo
040300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
040400030715     C* A questo punto "estraggo" il campo corrente
040500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
040600030715     C                                                    (posA-posDa))
040700030715     C* X i campi successivi al 1°
040800030715     C                   ELSE
040900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
041000030715     C                   EVAL      posDa = posA + 1
041100030715     C* Eseguo lo scan x trovare la fine del campo corrente
041200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041300030715     C* Gestisco l'ultimo campo
041400030715     C                   IF        posA = *zeros
041500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
041600030715     C* A questo punto "estraggo" il campo corrente
041700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041800030715     C                                                    (posA-posDa))
041900030715     C                   LEAVE
042000030715     C                   ELSE
042100030715     C* A questo punto "estraggo" il campo corrente
042200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042300030715     C                                                    (posA-posDa))
042400030715     C                   ENDIF
042500030715     C                   ENDIF
042600030715     C* Incremento il contatore d campo
042700030715     C                   EVAL      i = i +1
042800030715     C                   ENDDO
042900030715     C*
043000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
043100030715     C* delimitazione testo
043200030715     C                   EVAL      i = 1
043300030715     C                   DOW       i < %elem(SkSplitFLD)
043400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
043500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
043600030715     C                   EVAL      i = i + 1
043700030715     C                   ENDDO
043800030715     C                   EVAL      wGiro = 1
043900030715     C                   ELSE
044000030715     C                   EVAL      wGiro = 2
044100031204     C**********
044200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
044300031204     C* d separatore campo
044400031204     C**********
044500040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
044600040304     C                   EVAL      vindta = ' ' + vindta
044700040304     C                   ENDIF
044800040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
044900040304     C***                EVAL      vindta = %subst(vindta:2)
045000040304     C***                ENDDO
045100031204     C*
045200031204     C                   Z-ADD     *zeros        lunghInput        4 0
045300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
045400040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
045500040304     C                   EVAL      vindta = vindta + ' '
045600040304     C                   ENDIF
045700040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
045800040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045900040304     C***                EVAL      lunghInput = %len(%trim(vindta))
046000040304     C***                ENDDO
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
051100110527     C***                EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
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
052300080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXATB'
052400080729     C                   EVAL      VAXATB = SkSplitCSV(i)
052500030721     C                   ENDIF
052600080729     C*** TMD
052700080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXTMD'
052800080729     C                   EVAL      VAXTMD = SkSplitCSV(i)
052900030715     C                   ENDIF
053000080729     C*** TRK
053100080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXTRK'
053200080729     C                   EVAL      VAXTRK = SkSplitCSV(i)
053300030721     C                   ENDIF
053400080729     C*** DTT
053500080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXDTT'
053600080729     C                   EVAL      VAXDTT = SkSplitCSV(i)
053700080729     C                   ENDIF
053800030721     C*
053900030721     C* Reperisco quindi i campi numerici...
054000030721     C*** NSP
054100080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXNSP'
054200030721     C                   EVAL      PiStr=SkSplitCSV(i)
054300030721     C                   EXSR      CHKNUM
054400030721     C                   IF        PiInt=*on AND
054500030721     C                             PiVal<=9999999
054600080729     C                   Z-ADD     PiVal         VAXNSP
054700030721     C                   ELSE
054800030721     C                   SETON                                        32
054900080729     C                   EVAL      VAXNSP = *zeros
055000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
055100080729     C                             + ' ' + 'VAXNSP'
055200030721     C                   ENDIF
055300030721     C                   ENDIF
055400030721     C*** CCM
055500080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXCCM'
055600030721     C                   EVAL      PiStr=SkSplitCSV(i)
055700030721     C                   EXSR      CHKNUM
055800030721     C                   IF        PiInt=*on      AND
055900030721     C                             PiVal<=9999999 AND
056000030721     C                             PiVal>*zeros
056100080729     C                   Z-ADD     PiVal         VAXCCM
056200030721     C                   ELSE
056300030721     C                   SETON                                        32
056400080729     C                   EVAL      VAXCCM = *zeros
056500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056600080729     C                             + ' ' + 'VAXCCM'
056700030721     C                   ENDIF
056800030721     C                   ENDIF
056900030721     C*** LNP
057000080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXLNP'
057100030721     C                   EVAL      PiStr=SkSplitCSV(i)
057200030721     C                   EXSR      CHKNUM
057300030721     C                   IF        PiInt=*on    AND
057400030721     C                             PiVal<=999   AND
057500030721     C                             PiVal>*zeros
057600080729     C                   Z-ADD     PiVal         VAXLNP
057700030721     C                   ELSE
057800030721     C                   SETON                                        32
057900080729     C                   EVAL      VAXLNP = *zeros
058000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058100080729     C                             + ' ' + 'VAXLNP'
058200030721     C                   ENDIF
058300030721     C                   ENDIF
058400030721     C*** NRS
058500080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXNRS'
058600030721     C                   EVAL      PiStr=SkSplitCSV(i)
058700030721     C                   EXSR      CHKNUM
058800030721     C                   IF        PiInt=*on AND
058900030721     C                             PiVal<=99
059000080729     C                   Z-ADD     PiVal         VAXNRS
059100030721     C                   ELSE
059200030721     C                   SETON                                        32
059300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
059400080729     C                             + ' ' + 'VAXNRS'
059500030721     C                   ENDIF
059600030721     C                   ENDIF
059700080729     C*** PRR
059800080729     C                   IF        %trim(SkSplitFLD(i)) = 'VAXPRR'
059900080729     C                   EVAL      PiStr=SkSplitCSV(i)
060000080729     C                   EXSR      CHKNUM
060100080729     C                   IF        PiInt=*on AND
060200080729     C                             PiVal<=9999
060300080729     C                   Z-ADD     PiVal         VAXPRR
060400080729     C                   ELSE
060500080729     C                   SETON                                        32
060600080729     C                   EVAL      vinmsg = %trimr(vinmsg)
060700080729     C                             + ' ' + 'VAXPRR'
060800080729     C                   ENDIF
060900080729     C                   ENDIF
061000030715     C*
061100030715     C                   ENDIF
061200030715     C                   EVAL      i = i + 1
061300030715     C                   ENDDO
061400020204     C*
061500030715     C                   ENDIF
061600020204     C*
061700010607     C                   ENDSR
061800010607     C*----------------------------------------------------*
061900040714
062000010330
062100010330
062200010330     C*----------------------------------------------------*
062300010330     C*  CONTROLLO NUMERICITA' CAMPI
062400010330     C*----------------------------------------------------*
062500010330     C     CHKNUM        BEGSR
062600010330     C*
062700010606     C                   IF        PiDecChr = *blanks
062800030715     C                   EVAL      PiDecChr = CharNUM
062900010606     C                   ENDIF
063000010606     C*
063100010606     C                   CALL(e)   'ISNUMERIC'
063200010330     C                   PARM                    PiStr            30
063300010606     C                   PARM                    PiDecChr          1
063400010330     C                   PARM      *ZEROS        PiVal            30 9
063500010330     C                   PARM      '0'           PiInt             1
063600010330     C                   PARM      '0'           PiNum             1
063700010330     C                   IF        %error
063800010606     C                   EVAL      PiNum=*off
063900010330     C                   ENDIF
064000010330     C*
064100010330     C                   ENDSR
064200010330     C***
064300010330
064400010601
064500010601
064600010601
064700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064800010601     C     repfil        BEGSR
064900010601     C*
065000010601     C                   if        invfil = *zeros and
065100010601     C                             depfil > *zeros and
065200010629     C                             (vinflg = *blanks or
065300010629     C                              vinflg = *zeros)
065400010601     C
065500010601     C                   eval      invfil = depfil
065600010601     C                   endif
065700010601     C*
065800010601     C                   if        depfil <> invfil and
065900010601     C                             invfil > *zeros
066000010601     C                   eval      flgMulti = '1'
066100010601     C                   if        vinflg = *blanks
066200010601     C                   add       1             cntNonEl
066300010601     C                   endif
066400010601     C                   endif
066500010601     C*
066600010601     C                   if        vinflg = '2'
066700010601     C                   eval      flgStato = '2'
066800010601     C                   endif
066900010601     C*
067000010601     C                   ENDSR
067100010601     C***
067200010601
067300010601
067400010601
067500010330
067600010330
067700010330
067800990920      /TITLE Invio dei dati al punto operativo.
067900000613     C     invio         BEGSR
068000990920     C*
068100990920     C                   reset                   dscmz
068200010601     C                   move      invfil        cmzdst
068300990920     C                   eval      cmzfld = vlrfou
068400990920     C                   eval      cmzmbd = vlrhdl
068500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
068600000710     C                   if        prmfir = *blanks
068700080729     C                   eval      cmzfla = 'FIVAX00F'
068800080729     C                   eval      cmzmba = 'FIVAX00F'
068900000710     C                   else
069000000710     C                   eval      cmzfla = prmfir
069100000710     C                   eval      cmzmba = prmfir
069200000710     C                   endif
069300990920     C                   eval      cmznrr = *zeros
069400990920     C                   move      §ctrok        cmznrr
069500021018     C                   eval      cmzlba = vlrfl1
069600990920     C                   call(e)   'TIS711C'
069700990920     C                   parm                    dscmz
069800990921     C                   parm      *blanks       esito
069900990920     C                   if        %error
070000990920     C                             or cmzerr = '1'
070100990921     C                             or esito  = '1'
070200000710     C                   eval      wrkesito = '3'
070300990920     C                   endif
070400990920     C*
070500000613     C                   ENDSR
070600990910
070700010601
070800010601
070900010601
071000010601
071100010601      /TITLE Invio dei dati al punto operativo.
071200010601     C     opeini        BEGSR
071300010601     C*
071400010601     C* Inizializzo flag e contatori operativi
071500010601     C                   movel     '0'           flgGiro           1
071600010601     C                   movel     '0'           flgMulti          1
071700010601     C                   movel     '1'           flgStato          1
071800010615     C                   movel     '0'           flgOk             1
071900010601     C                   z-add     *zeros        cntNonEl         10 0
072000010601     C                   z-add     *zeros        depfil            3 0
072100010601     C                   z-add     *zeros        invfil            3 0
072200010601     C*
072300010601     C                   ENDSR
072400010601     C***
072500050421
072600050421
072700050421
072800050421     C     *pssr         BEGSR
072900050421     C*
073000050421     C                   eval      wrkesito = '2'
073100050421     C*
073200050421     C                   ENDSR     '*CANCL'
073300050421     C***
073400010601
073500010601
073600010601
073700010330
073800010330
073900000613     C     *inzsr        BEGSR
074000990910     C*
074100990910     C     *entry        plist
074200990920     C                   parm                    tivlrds
074300990921     C                   parm      wrkesito      esito
074400000724     C                   parm                    prmlit
074500000710     C                   parm                    prmfir
074600010330     C*
074700010330     C* CALCOLA LA DATA CORRENTE
074800010330     C                   time                    wn14             14 0
074900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075000010330     C                   z-add     wn8           g08dat
075100010330     C                   z-add     *zeros        g08inv
075200010330     C                   movel     '0'           g08err
075300010330     C                   call      'XSRDA8'
075400010330     C                   parm                    wlbda8
075500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075600000613     C*
075700000613     C                   ENDSR
075800000613     C***
