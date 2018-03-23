000100081028      /TITLE Upload via Internet: traduzione in EDIVATWR.
000200081028     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500081028     FEDIVATwr  O    E             DISK    usropn
000600000313     D*
000700040726     D*------------
000800081028     D* SCHIERE A PROGRAMMA
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
003600030822     D*------------------
003700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003800030822     D*------------------
003900030822     D posDaDft        S              3  0 INZ(*zeros)
004000030822     D posADft         S              3  0 INZ(*zeros)
004100030822     D j               s              3  0 INZ(1)
004200030822     D wGiroDft        s              1  0 INZ(*zeros)
004300010330     D*------------------
004400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500010330     D*------------------
004600010330     D WLBDA8          DS                  INZ
004700010330     D  G08DAT                 1      8  0
004800010330     D  G08INV                 9     16  0
004900010330     D  G08ERR                17     17
005000010330     D  G08TGI                18     22  0
005100031201     D*------------------
005200031201     D* Costanti
005300031201     D*------------------
005400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005600081028
005700081028     D*------------------
005800081028     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
005900081028     D*------------------
006000081028     D PiStr           S            100A   INZ
006100081028     D PiDecChr        S              1A   INZ
006200081028     D PiVal           S             63S30 INZ
006300081028     D PiInt           S               N   INZ
006400081028     D PiNum           S               N   INZ
006500081028
006600081028     D*------------------
006700081028     D* LINKING A DEFINIZIONI ESTERNE
006800081028     D*------------------
006900081028     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000081028
007100081028
007200010330
007300010330
007400010330
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800010601     C*
007900010601     C                   exsr      opeini
008000040714     C                   exsr      rwvat
008100010601     C*
008200010601     C                   seton                                        lr
008300010601
008400010601
008500010601
008600010601
008700010601     C*--------------------------------------------------------
008800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008900010601     C*--------------------------------------------------------
009000010601     C     PREELA        BEGSR
009100010601     C*
009200010601     C* SE OCCORRE SPEDIRE IN FILIALE
009300010601     C                   if        invfil <> *zeros and
009400010601     C                             flgGiro = '0'
009500010601     C*
009600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009700010601     C                   eval      flgGiro = '1'
009800010601     C*
009900010601     C                   endif
010000010601     C*
010100010601     C                   ENDSR
010200010601     C***
010300010601
010400010601
010500010601
010600010601
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400000616     C*
011500010601     C                   ENDSR
011600010601     C***
011700000613
011800010330
011900010601
012000010601
012100010601
012200010330     C*--------------------------------------------------------
012300081028     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
012400010330     C*--------------------------------------------------------
012500040714     C     RWVAT         BEGSR
012600010330     C*
012700010330     C                   if        not %open(tivin00r)
012800010330     C                   open      tivin00r
012900010330     C                   endif
013000081028     C                   if        not %open(edivatwr)
013100081028     C                   open      edivatwr
013200010330     C                   endif
013300010330     C*
013400010604     C                   clear                   §CTROK
013500010604     C                   clear                   §CTRMO
013600010604     C                   clear                   §CTRNO
013700040726     C*
013800040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
013900040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014000100906     C***                exsr      inzvar
014100100906     C***                exsr      impvat
014200010330     C*
014300030822     C                   do        *HIVAL
014400010330     C*
014500030822     C                   read      tivin00r                               70
014600010618     C*
014700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014800010618     C                   if        vinflg = '1'
014900010618     C                   eval      flgOk = '1'
015000010618     C                   endif
015100010618     C*
015200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015300040510     C                   z-add     *zeros        wLenVINDTA        4 0
015400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015800040510     C                   z-add     1             wFlgVINDTA
015900040510     C                   leave
016000040510     C                   endif
016100040510     C                   enddo
016200040510     C*
016300040218     C                   if        vindta > *blanks AND
016400040510     C                             wFlgVINDTA = 1
016500010330     C                   add       1             rrnum
016600010330     C*
016700010601     C                   if        *in70 = *off and
016800010330     C                             (vinflg = *blanks
016900010330     C                              or vinflg = '0'
017000010330     C                              or vinflg = '2')
017100010330     C*
017200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017300010711     C                   if        vinflg = *blanks or vinflg = '0'
017400010711     C                   clear                   vinmsg
017500010711     C                   endif
017600010601     C*
017700010330     C                   exsr      inzvar
017800040714     C                   exsr      impvat
017900010601     C*
018000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018100040714     C                   eval      depfil = VATLNP
018200010601     C                   exsr      repfil
018300010601     C                   if        depfil = invfil
018400021025     C                   if        vlrpoi = 999
018500040714     C                   move(P)   invfil        VATFGS
018600021025     C                   else
018700040714     C                   move(P)   vlrpoi        VATFGS
018800021025     C                   endif
018900010601     C*
019000010601     C                   exsr      PREELA
019100081031     C*
019200081031     C* VALORIZZO CAMPI RELATIVI AL "CMR"
019300081031     C                   eval      VATCMR = 'AKU ' + %char(datcor)
019400081031     C                   eval      VATCNT = 1
019500010601     C*
019600010604     C* Ebbene...
019700010604     C*
019800030822     C  N31              add       1             §CTROK            7 0
019900030822     C   32              add       1             §CTRMO            7 0
020000030822     C   31              add       1             §CTRNO            7 0
020100030822     C                   if        wGiro = 2
020200081028     C  N31              write     EDIVAT00
020300030822     C                   endif
020400010604     C*
020500010604     C                   if        *in31 = *off and
020600010604     C                             *in32 = *off
020700010604     C                   eval      vinflg = '1'
020800010604     C                   else
020900010604     C                   eval      vinflg = '2'
021000010604     C                   endif
021100010604     C                   endif
021200010601     C*
021300010604     C                   endif
021400010604     C*
021500010330     C                   else
021600010330     C                   eval      vinflg = '1'
021700010330     C                   endif
021800010601     C*
021900010601     C  N70              update    tivin000
022000010330     C*
022100030822     C  N70              enddo
022200010601     C
022300010601     C*
022400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022500010601     C                   if        cntNonEl = *zeros or
022600010601     C                             flgMulti = '0'
022700010330     C* Se non ci sono record con errori ...
022800010601     C                   if        §ctrno = 0 and
022900010604     C                             §ctrmo = 0 and
023000010601     C                             flgStato <> '2'
023100010330     C* ... restituisco esito OK.
023200010330     C                   eval      wrkesito = '0'
023300010330     C                   else
023400010330     C                   if        §ctrok > 0
023500010330     C                   eval      wrkesito = '1'
023600010330     C                   else
023700010615     C                   if        flgOk = '0'
023800010615     C                   eval      wrkesito = '2'
023900010615     C                   else
024000010615     C                   eval      wrkesito = '6'
024100010615     C                   endif
024200010330     C                   endif
024300010330     C                   endif
024400010601     C                   else
024500010601     C                   eval      wrkesito = '9'
024600010601     C                   endif
024700010330     C*
024800010330     C                   if        %open(tivin00r)
024900010330     C                   close     tivin00r
025000010330     C                   endif
025100081028     C                   if        %open(edivatwr)
025200081028     C                   close     edivatwr
025300010330     C                   endif
025400010601     C*
025500010601     C                   if        vlrpoi <> 999
025600010601     C                   eval      invfil = vlrpoi
025700010601     C                   endif
025800010330     C*
025900010330     C                   if        §ctrok > 0
026000010601     C                             and invfil > *zeros
026100010330     C                   exsr      invio
026200010330     C                   endif
026300010601     C*
026400010618     C                   if        flgGiro = '1'
026500010601     C                   exsr      endela
026600010618     C                   endif
026700010330     C*
026800010330     C                   ENDSR
026900010330     C***
027000010601
027100010601
027200010601
027300010330     C*----------------------------------------------------*
027400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500010330     C*----------------------------------------------------*
027600010330     C     INZVAR        BEGSR
027700010330     C*
027800030822     C* Inizializzo variabili di wrk
027900020204     C                   Z-ADD     *zeros        Num5_0            5 0
028000030822     C*
028100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
028200081028     C                   CLEAR                   EDIVAT00
028300030822     C                   CLEAR                   SkSplitCSV
028400030822     C                   EVAL      i = 1
028500030822     C                   EVAL      posDa = *zeros
028600030822     C                   EVAL      posA  = *zeros
028700030822     C*
028800030822     C* Reimposto i valori di default
028900030822     C                   EXSR      DEFCAM
029000030822     C*
029100010330     C                   ENDSR
029200010330     C*----------------------------------------------------*
029300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029400010330     C*----------------------------------------------------*
029500010330     C     DEFCAM        BEGSR
029600030822     C*
029700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029800030715     C* e delimitatore testo.
029900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
030000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030200030822     C*
030300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
030400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
030500030715     C                   SETON                                        50
030600030715     C                   ELSE
030700030715     C                   SETOFF                                       50
030800030715     C                   ENDIF
030900030822     C*
031000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031100030822     C                   EVAL      posDaDft = 1
031200030822     C                   EVAL      posADft  = 0
031300030822     C                   EVAL      wGiroDft = 0
031400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
031500030822     C                             posDaDft > 0
031600030822     C*
031700030822     C* Gestisco il 1° giro
031800030822     C                   IF        wGiroDft = 0
031900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
032000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032100030822     C* Incremento il contatore dei "giri"
032200030822     C                   EVAL      wGiroDft = 1
032300030822     C                   ELSE
032400030822     C                   EVAL      posDaDft = posADft
032500030822     C                   ENDIF
032600030822     C* Eseguo lo scan x trovare la fine del campo corrente
032700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
032800030822     C*
032900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
033000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
033100030822     C                   IF        posDaDft > 0 AND
033200030822     C                             posADft  > 0
033300030822     C* CCM
033400030822     C                   IF        %subst(
033500030822     C                             %subst(vlrppt:posDaDft+1:
033600030822     C                             posADft-posDaDft-1):1:3)
033700030822     C                             = 'CCM'
033800030822     C                   EVAL      PiStr=%trim(%subst(
033900030822     C                             %subst(vlrppt:posDaDft+1:
034000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034100030822     C                   EXSR      CHKNUM
034200030822     C                   IF        PiInt=*on
034300040714     C                   Z-ADD     PiVal         VATCCM
034400030822     C                   ENDIF
034500030822     C                   ENDIF
034600030822     C* LNP
034700030822     C                   IF        %subst(
034800030822     C                             %subst(vlrppt:posDaDft+1:
034900030822     C                             posADft-posDaDft-1):1:3)
035000030822     C                             = 'LNP'
035100030822     C                   EVAL      PiStr=%trim(%subst(
035200030822     C                             %subst(vlrppt:posDaDft+1:
035300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035400030822     C                   EXSR      CHKNUM
035500030822     C                   IF        PiInt=*on
035600040714     C                   Z-ADD     PiVal         VATLNP
035700030822     C                   ENDIF
035800030822     C                   ENDIF
035900040714     C* TRC
036000030822     C                   IF        %subst(
036100030822     C                             %subst(vlrppt:posDaDft+1:
036200030822     C                             posADft-posDaDft-1):1:3)
036300040714     C                             = 'TRC'
036400040714     C                   EVAL      VATTRC=%trim(%subst(
036500030822     C                             %subst(vlrppt:posDaDft+1:
036600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036700030822     C                   ENDIF
036800030822     C* ...
036900030822     C                   ENDIF
037000030822     C                   ENDDO
037100020204     C*
037200010330     C                   ENDSR
037300010607     C*----------------------------------------------------*
037400081028     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
037500010607     C*----------------------------------------------------*
037600040714     C     IMPVAT        BEGSR
037700010607     C*
037800010607     C                   SETOFF                                       3132
037900030515     C*
038000040714     C                   MOVEL     datcor        VATAAS
038100010607     C*
038200030715     C**********
038300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
038400030715     C**********
038500030715     C                   IF        wGiro = *zeros
038600040726     C*
038700040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
038800040726     C                   EVAL      depvinDTA = vinDTA
038900040726     C*
039000040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
039100040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
039200040726     C                                      %trim(intcol(3))
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
043900040726     C* Ripristino la 1° riga originale
044000040726     C                   EVAL      vinDTA = depvinDTA
044100030715     C                   ELSE
044200030715     C                   EVAL      wGiro = 2
044300031204     C**********
044400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
044500031204     C* d separatore campo
044600031204     C**********
044700040726     C                   EVAL      vindta = %trim(vindta)
044800040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
044900040726     C                   EVAL      vindta = %subst(vindta:2)
045000040726     C                   ENDDO
045100031204     C*
045200031204     C                   Z-ADD     *zeros        lunghInput        4 0
045300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
045400040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
045500040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
045600040726     C                   EVAL      lunghInput = %len(%trim(vindta))
045700040726     C                   ENDDO
045800030715     C**********
045900030715     C* Effettuo lo split del campo dati d input
046000030715     C**********
046100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
046200040324     C                   DOW       posDa <= %len(%trimr(vindta))
046300030715     C*
046400030715     C* Gestisco il 1° campo
046500030715     C                   IF        i = 1
046600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046700030715     C                   EVAL      posDa = 1
046800030715     C* Eseguo lo scan x trovare la fine del primo campo
046900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047000030715     C* A questo punto "estraggo" il campo corrente
047100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
047200030715     C                                                    (posA-posDa))
047300030715     C* X i campi successivi al 1°
047400030715     C                   ELSE
047500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047600030715     C                   EVAL      posDa = posA + 1
047700031204     C* Verifico che nn vi sia il campo nn valorizzato
047800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
047900031204     C* Se campo nn valorizzato skippo al prossimo
048000031204     C                   EVAL      SkSplitCSV(i) = *blanks
048100031204     C                   EVAL      posA  = posA + 1
048200031204     C                   ELSE
048300030715     C* Eseguo lo scan x trovare la fine del campo corrente
048400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048500030715     C* Gestisco l'ultimo campo
048600030715     C                   IF        posA = *zeros
048700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048800030715     C* A questo punto "estraggo" il campo corrente
048900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049000030715     C                                                    (posA-posDa))
049100030715     C                   LEAVE
049200030715     C                   ELSE
049300030715     C* A questo punto "estraggo" il campo corrente
049400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049500030715     C                                                    (posA-posDa))
049600030715     C                   ENDIF
049700031204     C                   ENDIF
049800030715     C                   ENDIF
049900030715     C* Incremento il contatore d campo
050000030715     C                   EVAL      i = i +1
050100030715     C                   ENDDO
050200030715     C*
050300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
050400030715     C* delimitazione testo
050500030715     C                   EVAL      i = 1
050600030715     C                   DOW       i < %elem(SkSplitCSV)
050700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
050800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
050900030715     C                   EVAL      i = i + 1
051000030715     C                   ENDDO
051100030715     C*
051200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
051300030715     C                   EVAL      i = 1
051400030715     C                   DOW       i < %elem(SkSplitFLD)
051500030715     C*
051600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
051700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
051800030715     C                             SkSplitCSV(i) <> *blanks
051900030721     C*** ATB
052000040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
052100040714     C                   EVAL      VATATB = SkSplitCSV(i)
052200030721     C                   ENDIF
052300040714     C*** TRC
052400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
052500040714     C                   EVAL      VATTRC = SkSplitCSV(i)
052600030715     C                   ENDIF
052700040714     C*** NOT
052800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
052900040714     C                   EVAL      VATNOT = SkSplitCSV(i)
053000030721     C                   ENDIF
053100030721     C*
053200030721     C* Reperisco quindi i campi numerici...
053300030721     C*** NSP
053400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
053500030721     C                   EVAL      PiStr=SkSplitCSV(i)
053600030721     C                   EXSR      CHKNUM
053700040726     C                   IF        PiInt=*on
053800040726     C                   Z-ADD     PiVal         VATNSP
053900040802     C*                  IF        PiVal>9999999
054000040802     C*                  SETON                                        32
054100040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
054200040802     C*                            + ' ' + 'VATNSP'
054300040802     C*                  ENDIF
054400030721     C                   ELSE
054500071002     C                   SETON                                        32
054600040714     C                   EVAL      VATNSP = *zeros
054700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
054800040714     C                             + ' ' + 'VATNSP'
054900030721     C                   ENDIF
055000030721     C                   ENDIF
055100030721     C*** CCM
055200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
055300030721     C                   EVAL      PiStr=SkSplitCSV(i)
055400030721     C                   EXSR      CHKNUM
055500030721     C                   IF        PiInt=*on      AND
055600030721     C                             PiVal<=9999999 AND
055700030721     C                             PiVal>*zeros
055800040714     C                   Z-ADD     PiVal         VATCCM
055900030721     C                   ELSE
056000030721     C                   SETON                                        32
056100040714     C                   EVAL      VATCCM = *zeros
056200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
056300040714     C                             + ' ' + 'VATCCM'
056400030721     C                   ENDIF
056500030721     C                   ENDIF
056600030721     C*** LNP
056700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
056800030721     C                   EVAL      PiStr=SkSplitCSV(i)
056900030721     C                   EXSR      CHKNUM
057000030721     C                   IF        PiInt=*on    AND
057100030721     C                             PiVal<=999   AND
057200030721     C                             PiVal>*zeros
057300040714     C                   Z-ADD     PiVal         VATLNP
057400030721     C                   ELSE
057500030721     C                   SETON                                        32
057600040714     C                   EVAL      VATLNP = *zeros
057700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057800040714     C                             + ' ' + 'VATLNP'
057900030721     C                   ENDIF
058000030721     C                   ENDIF
058100030721     C*** NRS
058200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
058300030721     C                   EVAL      PiStr=SkSplitCSV(i)
058400030721     C                   EXSR      CHKNUM
058500030721     C                   IF        PiInt=*on AND
058600030721     C                             PiVal<=99
058700040714     C                   Z-ADD     PiVal         VATNRS
058800030721     C                   ELSE
058900030721     C                   SETON                                        32
059000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
059100040714     C                             + ' ' + 'VATNRS'
059200030721     C                   ENDIF
059300030721     C                   ENDIF
059400030715     C*
059500081107     C*********************
059600081107     C* Gestioni particolari
059700081107     C*
059800081107     C* NOT_E
059900081107     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT'
060000100204     C***                EVAL      VATNOT = %subst(SkSplitCSV(i):2)
060100100204     C                   EVAL      VATNOT = SkSplitCSV(i)
060200081107     C                   ENDIF
060300081107     C*
060400081107     C*********************
060500081107     C*
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
063500081028     C*
063600081028     C                   callp     UBISNUM_Check(PiStr
063700081028     C                                          :PiDecChr
063800081028     C                                          :PiVal
063900081028     C                                          :PiNum
064000081028     C                                          :PiInt)
064100010330     C*
064200010330     C                   ENDSR
064300010330     C***
064400010330
064500010601
064600010601
064700010601
064800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
064900010601     C     repfil        BEGSR
065000010601     C*
065100010601     C                   if        invfil = *zeros and
065200010601     C                             depfil > *zeros and
065300010629     C                             (vinflg = *blanks or
065400010629     C                              vinflg = *zeros)
065500010601     C
065600010601     C                   eval      invfil = depfil
065700010601     C                   endif
065800010601     C*
065900010601     C                   if        depfil <> invfil and
066000010601     C                             invfil > *zeros
066100010601     C                   eval      flgMulti = '1'
066200010601     C                   if        vinflg = *blanks
066300010601     C                   add       1             cntNonEl
066400010601     C                   endif
066500010601     C                   endif
066600010601     C*
066700010601     C                   if        vinflg = '2'
066800010601     C                   eval      flgStato = '2'
066900010601     C                   endif
067000010601     C*
067100010601     C                   ENDSR
067200010601     C***
067300010601
067400010601
067500010601
067600010330
067700010330
067800010330
067900990920      /TITLE Invio dei dati al punto operativo.
068000000613     C     invio         BEGSR
068100990920     C*
068200990920     C                   reset                   dscmz
068300010601     C                   move      invfil        cmzdst
068400990920     C                   eval      cmzfld = vlrfou
068500990920     C                   eval      cmzmbd = vlrhdl
068600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
068700000710     C                   if        prmfir = *blanks
068800081028     C                   eval      cmzfla = 'EDIVAT0F'
068900081028     C                   eval      cmzmba = 'EDIVAT0F'
069000000710     C                   else
069100000710     C                   eval      cmzfla = prmfir
069200000710     C                   eval      cmzmba = prmfir
069300000710     C                   endif
069400990920     C                   eval      cmznrr = *zeros
069500990920     C                   move      §ctrok        cmznrr
069600021018     C                   eval      cmzlba = vlrfl1
069700990920     C                   call(e)   'TIS711C'
069800990920     C                   parm                    dscmz
069900990921     C                   parm      *blanks       esito
070000990920     C                   if        %error
070100990920     C                             or cmzerr = '1'
070200990921     C                             or esito  = '1'
070300000710     C                   eval      wrkesito = '3'
070400990920     C                   endif
070500990920     C*
070600000613     C                   ENDSR
070700990910
070800010601
070900010601
071000010601
071100010601
071200010601      /TITLE Invio dei dati al punto operativo.
071300010601     C     opeini        BEGSR
071400010601     C*
071500010601     C* Inizializzo flag e contatori operativi
071600010601     C                   movel     '0'           flgGiro           1
071700010601     C                   movel     '0'           flgMulti          1
071800010601     C                   movel     '1'           flgStato          1
071900010615     C                   movel     '0'           flgOk             1
072000010601     C                   z-add     *zeros        cntNonEl         10 0
072100010601     C                   z-add     *zeros        depfil            3 0
072200010601     C                   z-add     *zeros        invfil            3 0
072300010601     C*
072400010601     C                   ENDSR
072500010601     C***
072600010601
072700010601
072800050421
072900050421
073000050421     C     *pssr         BEGSR
073100050421     C*
073200050421     C                   eval      wrkesito = '2'
073300050421     C*
073400050421     C                   ENDSR     '*CANCL'
073500050421     C***
073600050421
073700010601
073800010330
073900010330
074000000613     C     *inzsr        BEGSR
074100990910     C*
074200990910     C     *entry        plist
074300990920     C                   parm                    tivlrds
074400990921     C                   parm      wrkesito      esito
074500000724     C                   parm                    prmlit
074600000710     C                   parm                    prmfir
074700010330     C*
074800010330     C* CALCOLA LA DATA CORRENTE
074900010330     C                   time                    wn14             14 0
075000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
075100010330     C                   z-add     wn8           g08dat
075200010330     C                   z-add     *zeros        g08inv
075300010330     C                   movel     '0'           g08err
075400010330     C                   call      'XSRDA8'
075500010330     C                   parm                    wlbda8
075600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
075700000613     C*
075800000613     C                   ENDSR
075900000613     C***
076000040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
076100081107FILL001;FILL002;FILL003;VATNSP;FILL004;W_VATNOT
076200040726
076300040726
