000100060622      /TITLE Upload via Internet: traduzione in FIVADWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500060622     FFIVADwwr  O    E             DISK    usropn
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
006000060622     C                   exsr      rwvad
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
010300060622     C* RWVAD   LEGGE TIVIN00R E SCRIVE FIVADWWF              *
010400010330     C*--------------------------------------------------------
010500060622     C     RWVAD         BEGSR
010600010330     C*
010700010330     C                   if        not %open(tivin00r)
010800010330     C                   open      tivin00r
010900010330     C                   endif
011000060622     C                   if        not %open(fivadwwr)
011100060622     C                   open      fivadwwr
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
017600060622     C                   exsr      impvad
017700010601     C*
017800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017900060622     C                   eval      depfil = VADLNP
018000010601     C                   exsr      repfil
018100010601     C                   if        depfil = invfil
018200021025     C                   if        vlrpoi = 999
018300060622     C                   move(P)   invfil        VADFGS
018400021025     C                   else
018500060622     C                   move(P)   vlrpoi        VADFGS
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
019700060622     C  N31              write     FIVAD000
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
024500060622     C                   if        %open(fivadwwr)
024600060622     C                   close     fivadwwr
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
027700060622     C                   CLEAR                   FIVAD000
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
034100060622     C                   Z-ADD     PiVal         VADCCM
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
035400060622     C                   Z-ADD     PiVal         VADLNP
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700060622     C* NRS
035800060622     C                   IF        %subst(
035900060622     C                             %subst(vlrppt:posDaDft+1:
036000060622     C                             posADft-posDaDft-1):1:3)
036100060622     C                             = 'NRS'
036200060622     C                   EVAL      PiStr=%trim(%subst(
036300060622     C                             %subst(vlrppt:posDaDft+1:
036400060622     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500060622     C                   EXSR      CHKNUM
036600060622     C                   IF        PiInt=*on
036700060622     C                   Z-ADD     PiVal         VADNRS
036800060622     C                   ENDIF
036900060622     C                   ENDIF
037000060622     C* NCL
037100060622     C                   IF        %subst(
037200060622     C                             %subst(vlrppt:posDaDft+1:
037300060622     C                             posADft-posDaDft-1):1:3)
037400060622     C                             = 'NCL'
037500060622     C                   EVAL      PiStr=%trim(%subst(
037600060622     C                             %subst(vlrppt:posDaDft+1:
037700060622     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800060622     C                   EXSR      CHKNUM
037900060622     C                   IF        PiInt=*on
038000060622     C                   Z-ADD     PiVal         VADNCL
038100060622     C                   ENDIF
038200060622     C                   ENDIF
038300060622     C* CDP
038400030822     C                   IF        %subst(
038500030822     C                             %subst(vlrppt:posDaDft+1:
038600030822     C                             posADft-posDaDft-1):1:3)
038700060622     C                             = 'CDP'
038800060622     C                   EVAL      VADCDP=%trim(%subst(
038900030822     C                             %subst(vlrppt:posDaDft+1:
039000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039100030822     C                   ENDIF
039200030822     C* ...
039300030822     C                   ENDIF
039400030822     C                   ENDDO
039500020204     C*
039600010330     C                   ENDSR
039700010607     C*----------------------------------------------------*
039800060622     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAD)
039900010607     C*----------------------------------------------------*
040000060622     C     IMPVAD        BEGSR
040100010607     C*
040200010607     C                   SETOFF                                       3132
040300030515     C*
040400060622     C                   MOVEL     datcor        VADAAS
040500010607     C*
040600030715     C**********
040700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
040800030715     C**********
040900030715     C                   IF        wGiro = *zeros
041000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
041100031201     C     minu:maiu     XLATE     vindta        vindta
041200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
041300040324     C                   DOW       posDa <= %len(%trimr(vindta))
041400030715     C*
041500030715     C* Gestisco il 1° campo
041600030715     C                   IF        i = 1
041700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
041800030715     C                   EVAL      posDa = 1
041900030715     C* Eseguo lo scan x trovare la fine del primo campo
042000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042100030715     C* A questo punto "estraggo" il campo corrente
042200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042300030715     C                                                    (posA-posDa))
042400030715     C* X i campi successivi al 1°
042500030715     C                   ELSE
042600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
042700030715     C                   EVAL      posDa = posA + 1
042800030715     C* Eseguo lo scan x trovare la fine del campo corrente
042900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043000030715     C* Gestisco l'ultimo campo
043100030715     C                   IF        posA = *zeros
043200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
043300030715     C* A questo punto "estraggo" il campo corrente
043400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043500030715     C                                                    (posA-posDa))
043600030715     C                   LEAVE
043700030715     C                   ELSE
043800030715     C* A questo punto "estraggo" il campo corrente
043900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
044000030715     C                                                    (posA-posDa))
044100030715     C                   ENDIF
044200030715     C                   ENDIF
044300030715     C* Incremento il contatore d campo
044400030715     C                   EVAL      i = i +1
044500030715     C                   ENDDO
044600030715     C*
044700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044800030715     C* delimitazione testo
044900030715     C                   EVAL      i = 1
045000030715     C                   DOW       i < %elem(SkSplitFLD)
045100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
045200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
045300030715     C                   EVAL      i = i + 1
045400030715     C                   ENDDO
045500030715     C                   EVAL      wGiro = 1
045600030715     C                   ELSE
045700030715     C                   EVAL      wGiro = 2
045800031204     C**********
045900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
046000031204     C* d separatore campo
046100031204     C**********
046200040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
046300040304     C                   EVAL      vindta = ' ' + vindta
046400040304     C                   ENDIF
046500040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
046600040304     C***                EVAL      vindta = %subst(vindta:2)
046700040304     C***                ENDDO
046800031204     C*
046900031204     C                   Z-ADD     *zeros        lunghInput        4 0
047000040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
047100040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
047200040304     C                   EVAL      vindta = vindta + ' '
047300040304     C                   ENDIF
047400040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
047500040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
047600040304     C***                EVAL      lunghInput = %len(%trim(vindta))
047700040304     C***                ENDDO
047800030715     C**********
047900030715     C* Effettuo lo split del campo dati d input
048000030715     C**********
048100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
048200040324     C                   DOW       posDa <= %len(%trimr(vindta))
048300030715     C*
048400030715     C* Gestisco il 1° campo
048500030715     C                   IF        i = 1
048600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
048700030715     C                   EVAL      posDa = 1
048800030715     C* Eseguo lo scan x trovare la fine del primo campo
048900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049000030715     C* A questo punto "estraggo" il campo corrente
049100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049200030715     C                                                    (posA-posDa))
049300030715     C* X i campi successivi al 1°
049400030715     C                   ELSE
049500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
049600030715     C                   EVAL      posDa = posA + 1
049700031204     C* Verifico che nn vi sia il campo nn valorizzato
049800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
049900031204     C* Se campo nn valorizzato skippo al prossimo
050000031204     C                   EVAL      SkSplitCSV(i) = *blanks
050100031204     C                   EVAL      posA  = posA + 1
050200031204     C                   ELSE
050300030715     C* Eseguo lo scan x trovare la fine del campo corrente
050400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
050500030715     C* Gestisco l'ultimo campo
050600030715     C                   IF        posA = *zeros
050700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
050800030715     C* A questo punto "estraggo" il campo corrente
050900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
051000030715     C                                                    (posA-posDa))
051100030715     C                   LEAVE
051200030715     C                   ELSE
051300030715     C* A questo punto "estraggo" il campo corrente
051400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
051500030715     C                                                    (posA-posDa))
051600030715     C                   ENDIF
051700031204     C                   ENDIF
051800030715     C                   ENDIF
051900030715     C* Incremento il contatore d campo
052000030715     C                   EVAL      i = i +1
052100030715     C                   ENDDO
052200030715     C*
052300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
052400030715     C* delimitazione testo
052500030715     C                   EVAL      i = 1
052600030715     C                   DOW       i < %elem(SkSplitCSV)
052700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
052800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
052900030715     C                   EVAL      i = i + 1
053000030715     C                   ENDDO
053100030715     C*
053200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
053300030715     C                   EVAL      i = 1
053400030715     C                   DOW       i < %elem(SkSplitFLD)
053500030715     C*
053600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
053700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
053800030715     C                             SkSplitCSV(i) <> *blanks
053900030721     C*** ATB
054000060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADATB'
054100060622     C                   EVAL      VADATB = SkSplitCSV(i)
054200030721     C                   ENDIF
054300060622     C*** CDP
054400060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADCDP'
054500060622     C                   EVAL      VADCDP = SkSplitCSV(i)
054600030715     C                   ENDIF
054700030721     C*
054800030721     C* Reperisco quindi i campi numerici...
054900030721     C*** NSP
055000060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNSP'
055100030721     C                   EVAL      PiStr=SkSplitCSV(i)
055200030721     C                   EXSR      CHKNUM
055300030721     C                   IF        PiInt=*on AND
055400030721     C                             PiVal<=9999999
055500060622     C                   Z-ADD     PiVal         VADNSP
055600030721     C                   ELSE
055700030721     C                   SETON                                        32
055800060622     C                   EVAL      VADNSP = *zeros
055900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056000060622     C                             + ' ' + 'VADNSP'
056100030721     C                   ENDIF
056200030721     C                   ENDIF
056300030721     C*** CCM
056400060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADCCM'
056500030721     C                   EVAL      PiStr=SkSplitCSV(i)
056600030721     C                   EXSR      CHKNUM
056700030721     C                   IF        PiInt=*on      AND
056800030721     C                             PiVal<=9999999 AND
056900030721     C                             PiVal>*zeros
057000060622     C                   Z-ADD     PiVal         VADCCM
057100030721     C                   ELSE
057200030721     C                   SETON                                        32
057300060622     C                   EVAL      VADCCM = *zeros
057400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057500060622     C                             + ' ' + 'VADCCM'
057600030721     C                   ENDIF
057700030721     C                   ENDIF
057800030721     C*** LNP
057900060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADLNP'
058000030721     C                   EVAL      PiStr=SkSplitCSV(i)
058100030721     C                   EXSR      CHKNUM
058200030721     C                   IF        PiInt=*on    AND
058300030721     C                             PiVal<=999   AND
058400030721     C                             PiVal>*zeros
058500060622     C                   Z-ADD     PiVal         VADLNP
058600030721     C                   ELSE
058700030721     C                   SETON                                        32
058800060622     C                   EVAL      VADLNP = *zeros
058900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
059000060622     C                             + ' ' + 'VADLNP'
059100030721     C                   ENDIF
059200030721     C                   ENDIF
059300030721     C*** NRS
059400060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNRS'
059500030721     C                   EVAL      PiStr=SkSplitCSV(i)
059600030721     C                   EXSR      CHKNUM
059700030721     C                   IF        PiInt=*on AND
059800030721     C                             PiVal<=99
059900060622     C                   Z-ADD     PiVal         VADNRS
060000030721     C                   ELSE
060100030721     C                   SETON                                        32
060200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
060300060622     C                             + ' ' + 'VADNRS'
060400030721     C                   ENDIF
060500030721     C                   ENDIF
060600060622     C*** NCL
060700060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCL'
060800060622     C                   EVAL      PiStr=SkSplitCSV(i)
060900060622     C                   EXSR      CHKNUM
061000060622     C                   IF        PiInt=*on
061100060622     C                   Z-ADD     PiVal         VADNCL
061200060622     C                   ELSE
061300060622     C                   SETON                                        32
061400060622     C                   EVAL      vinmsg = %trimr(vinmsg)
061500060622     C                             + ' ' + 'VADNCL'
061600060622     C                   ENDIF
061700060622     C                   ENDIF
061800060622     C*** NCD
061900060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCD'
062000060622     C                   EVAL      PiStr=SkSplitCSV(i)
062100060622     C                   EXSR      CHKNUM
062200060622     C                   IF        PiInt=*on
062300060622     C                   Z-ADD     PiVal         VADNCD
062400060622     C                   ELSE
062500060622     C                   SETON                                        32
062600060622     C                   EVAL      vinmsg = %trimr(vinmsg)
062700060622     C                             + ' ' + 'VADNCD'
062800060622     C                   ENDIF
062900060622     C                   ENDIF
063000060622     C*** NCA
063100060622     C                   IF        %trim(SkSplitFLD(i)) = 'VADNCA'
063200060622     C                   EVAL      PiStr=SkSplitCSV(i)
063300060622     C                   EXSR      CHKNUM
063400060622     C                   IF        PiInt=*on
063500060622     C                   Z-ADD     PiVal         VADNCA
063600060622     C                   ELSE
063700060622     C                   SETON                                        32
063800060622     C                   EVAL      vinmsg = %trimr(vinmsg)
063900060622     C                             + ' ' + 'VADNCA'
064000060622     C                   ENDIF
064100060622     C                   ENDIF
064200030715     C*
064300030715     C                   ENDIF
064400030715     C                   EVAL      i = i + 1
064500030715     C                   ENDDO
064600020204     C*
064700030715     C                   ENDIF
064800020204     C*
064900010607     C                   ENDSR
065000010607     C*----------------------------------------------------*
065100040714
065200010330
065300010330
065400010330     C*----------------------------------------------------*
065500010330     C*  CONTROLLO NUMERICITA' CAMPI
065600010330     C*----------------------------------------------------*
065700010330     C     CHKNUM        BEGSR
065800010330     C*
065900010606     C                   IF        PiDecChr = *blanks
066000030715     C                   EVAL      PiDecChr = CharNUM
066100010606     C                   ENDIF
066200010606     C*
066300010606     C                   CALL(e)   'ISNUMERIC'
066400010330     C                   PARM                    PiStr            30
066500010606     C                   PARM                    PiDecChr          1
066600010330     C                   PARM      *ZEROS        PiVal            30 9
066700010330     C                   PARM      '0'           PiInt             1
066800010330     C                   PARM      '0'           PiNum             1
066900010330     C                   IF        %error
067000010606     C                   EVAL      PiNum=*off
067100010330     C                   ENDIF
067200010330     C*
067300010330     C                   ENDSR
067400010330     C***
067500010330
067600010601
067700010601
067800010601
067900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
068000010601     C     repfil        BEGSR
068100010601     C*
068200010601     C                   if        invfil = *zeros and
068300010601     C                             depfil > *zeros and
068400010629     C                             (vinflg = *blanks or
068500010629     C                              vinflg = *zeros)
068600010601     C
068700010601     C                   eval      invfil = depfil
068800010601     C                   endif
068900010601     C*
069000010601     C                   if        depfil <> invfil and
069100010601     C                             invfil > *zeros
069200010601     C                   eval      flgMulti = '1'
069300010601     C                   if        vinflg = *blanks
069400010601     C                   add       1             cntNonEl
069500010601     C                   endif
069600010601     C                   endif
069700010601     C*
069800010601     C                   if        vinflg = '2'
069900010601     C                   eval      flgStato = '2'
070000010601     C                   endif
070100010601     C*
070200010601     C                   ENDSR
070300010601     C***
070400010601
070500010601
070600010601
070700010330
070800010330
070900010330
071000990920      /TITLE Invio dei dati al punto operativo.
071100000613     C     invio         BEGSR
071200990920     C*
071300990920     C                   reset                   dscmz
071400010601     C                   move      invfil        cmzdst
071500990920     C                   eval      cmzfld = vlrfou
071600990920     C                   eval      cmzmbd = vlrhdl
071700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
071800000710     C                   if        prmfir = *blanks
071900060622     C                   eval      cmzfla = 'FIVAD00F'
072000060622     C                   eval      cmzmba = 'FIVAD00F'
072100000710     C                   else
072200000710     C                   eval      cmzfla = prmfir
072300000710     C                   eval      cmzmba = prmfir
072400000710     C                   endif
072500990920     C                   eval      cmznrr = *zeros
072600990920     C                   move      §ctrok        cmznrr
072700021018     C                   eval      cmzlba = vlrfl1
072800990920     C                   call(e)   'TIS711C'
072900990920     C                   parm                    dscmz
073000990921     C                   parm      *blanks       esito
073100990920     C                   if        %error
073200990920     C                             or cmzerr = '1'
073300990921     C                             or esito  = '1'
073400000710     C                   eval      wrkesito = '3'
073500990920     C                   endif
073600990920     C*
073700000613     C                   ENDSR
073800990910
073900010601
074000010601
074100010601
074200010601
074300010601      /TITLE Invio dei dati al punto operativo.
074400010601     C     opeini        BEGSR
074500010601     C*
074600010601     C* Inizializzo flag e contatori operativi
074700010601     C                   movel     '0'           flgGiro           1
074800010601     C                   movel     '0'           flgMulti          1
074900010601     C                   movel     '1'           flgStato          1
075000010615     C                   movel     '0'           flgOk             1
075100010601     C                   z-add     *zeros        cntNonEl         10 0
075200010601     C                   z-add     *zeros        depfil            3 0
075300010601     C                   z-add     *zeros        invfil            3 0
075400010601     C*
075500010601     C                   ENDSR
075600010601     C***
075700050421
075800050421
075900050421
076000050421     C     *pssr         BEGSR
076100050421     C*
076200050421     C                   eval      wrkesito = '2'
076300050421     C*
076400050421     C                   ENDSR     '*CANCL'
076500050421     C***
076600010601
076700010601
076800010601
076900010330
077000010330
077100000613     C     *inzsr        BEGSR
077200990910     C*
077300990910     C     *entry        plist
077400990920     C                   parm                    tivlrds
077500990921     C                   parm      wrkesito      esito
077600000724     C                   parm                    prmlit
077700000710     C                   parm                    prmfir
077800010330     C*
077900010330     C* CALCOLA LA DATA CORRENTE
078000010330     C                   time                    wn14             14 0
078100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
078200010330     C                   z-add     wn8           g08dat
078300010330     C                   z-add     *zeros        g08inv
078400010330     C                   movel     '0'           g08err
078500010330     C                   call      'XSRDA8'
078600010330     C                   parm                    wlbda8
078700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
078800000613     C*
078900000613     C                   ENDSR
079000000613     C***
