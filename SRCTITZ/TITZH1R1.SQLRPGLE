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
004700151105     D w8              s              8
004800010330     D*------------------
004900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005000010330     D*------------------
005100010330     D WLBDA8          DS                  INZ
005200010330     D  G08DAT                 1      8  0
005300010330     D  G08INV                 9     16  0
005400010330     D  G08ERR                17     17
005500010330     D  G08TGI                18     22  0
005600151019     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
005700151019     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005800151019     D Intestazioni    s               n   inz(*on)
005900110222     D*
006000110222
006100031201     D*------------------
006200031201     D* Costanti
006300031201     D*------------------
006400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006600010330
006700010330
006800010330
006900120309     C*
007000120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100120309     C
007200120309     C/EXEC SQL
007300120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400120309     C/END-EXEC
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
012300040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
012400010330     C*--------------------------------------------------------
012500040714     C     RWVAT         BEGSR
012600010330     C*
012700010330     C                   if        not %open(tivin00r)
012800010330     C                   open      tivin00r
012900010330     C                   endif
013000040714     C                   if        not %open(fivatwwr)
013100040714     C                   open      fivatwwr
013200010330     C                   endif
013300010330     C*
013400010604     C                   clear                   §CTROK
013500010604     C                   clear                   §CTRMO
013600010604     C                   clear                   §CTRNO
013700040726     C*
013800151019     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
013900151019     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
014000151019     C                   IF        Intestazioni = *off
014100040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014200040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014300040726     C                   exsr      inzvar
014400040726     C                   exsr      impvat
014500151019     C                   ENDIF
014600010330     C*
014700030822     C                   do        *HIVAL
014800010330     C*
014900030822     C                   read      tivin00r                               70
015000010618     C*
015100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015200010618     C                   if        vinflg = '1'
015300010618     C                   eval      flgOk = '1'
015400010618     C                   endif
015500010618     C*
015600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015700040510     C                   z-add     *zeros        wLenVINDTA        4 0
015800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016200040510     C                   z-add     1             wFlgVINDTA
016300040510     C                   leave
016400040510     C                   endif
016500040510     C                   enddo
016600040510     C*
016700040218     C                   if        vindta > *blanks AND
016800040510     C                             wFlgVINDTA = 1
016900010330     C                   add       1             rrnum
017000010330     C*
017100010601     C                   if        *in70 = *off and
017200010330     C                             (vinflg = *blanks
017300010330     C                              or vinflg = '0'
017400010330     C                              or vinflg = '2')
017500010330     C*
017600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017700010711     C                   if        vinflg = *blanks or vinflg = '0'
017800010711     C                   clear                   vinmsg
017900010711     C                   endif
018000010601     C*
018100010330     C                   exsr      inzvar
018200040714     C                   exsr      impvat
018300010601     C*
018400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018500040714     C                   eval      depfil = VATLNP
018600010601     C                   exsr      repfil
018700010601     C                   if        depfil = invfil
018800021025     C                   if        vlrpoi = 999
018900040714     C                   move(P)   invfil        VATFGS
019000021025     C                   else
019100040714     C                   move(P)   vlrpoi        VATFGS
019200021025     C                   endif
019300010601     C*
019400010601     C                   exsr      PREELA
019500010601     C*
019600010604     C* Ebbene...
019700010604     C*
019800030822     C  N31              add       1             §CTROK            7 0
019900030822     C   32              add       1             §CTRMO            7 0
020000030822     C   31              add       1             §CTRNO            7 0
020100030822     C                   if        wGiro = 2
020200110222     C*
020300110222     C                   if        not *in30
020400110222     C*
020500140821     C* tipo record 'E'
020600140821     C                   eval      VATTRC = 'E'
020700140821     C                   eval      VATNOT = %trim(wNOT_E)
020800110222     C                   if        vatnot <> *blanks
020900110222     C  N31              write     FIVAT000
021000110222     C                   endif
021100110222     C                   endif
021200030822     C                   endif
021300010604     C*
021400010604     C                   if        *in31 = *off and
021500010604     C                             *in32 = *off
021600010604     C                   eval      vinflg = '1'
021700010604     C                   else
021800010604     C                   eval      vinflg = '2'
021900010604     C                   endif
022000010604     C                   endif
022100010601     C*
022200010604     C                   endif
022300010604     C*
022400010330     C                   else
022500010330     C                   eval      vinflg = '1'
022600010330     C                   endif
022700010601     C*
022800010601     C  N70              update    tivin000
022900010330     C*
023000030822     C  N70              enddo
023100010601     C
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300010330     C                   if        §ctrok > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000040714     C                   if        %open(fivatwwr)
026100040714     C                   close     fivatwwr
026200010330     C                   endif
026300010601     C*
026400010601     C                   if        vlrpoi <> 999
026500010601     C                   eval      invfil = vlrpoi
026600010601     C                   endif
026700010330     C*
026800010330     C                   if        §ctrok > 0
026900010601     C                             and invfil > *zeros
027000010330     C                   exsr      invio
027100010330     C                   endif
027200010601     C*
027300010618     C                   if        flgGiro = '1'
027400010601     C                   exsr      endela
027500010618     C                   endif
027600010330     C*
027700010330     C                   ENDSR
027800010330     C***
027900010601
028000010601
028100010601
028200010330     C*----------------------------------------------------*
028300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028400010330     C*----------------------------------------------------*
028500010330     C     INZVAR        BEGSR
028600010330     C*
028700030822     C* Inizializzo variabili di wrk
028800020204     C                   Z-ADD     *zeros        Num5_0            5 0
028900030822     C*
029000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
029100040714     C                   CLEAR                   FIVAT000
029200030822     C                   CLEAR                   SkSplitCSV
029300030822     C                   EVAL      i = 1
029400030822     C                   EVAL      posDa = *zeros
029500030822     C                   EVAL      posA  = *zeros
029600110222     C*
029700140821     C                   MOVEL     *blanks       wNOT_E           35
029800030822     C*
029900030822     C* Reimposto i valori di default
030000030822     C                   EXSR      DEFCAM
030100030822     C*
030200010330     C                   ENDSR
030300010330     C*----------------------------------------------------*
030400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
030500010330     C*----------------------------------------------------*
030600010330     C     DEFCAM        BEGSR
030700030822     C*
030800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030900030715     C* e delimitatore testo.
031000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
031100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
031200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
031300030822     C*
031400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
031500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
031600030715     C                   SETON                                        50
031700030715     C                   ELSE
031800030715     C                   SETOFF                                       50
031900030715     C                   ENDIF
032000030822     C*
032100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032200030822     C                   EVAL      posDaDft = 1
032300030822     C                   EVAL      posADft  = 0
032400030822     C                   EVAL      wGiroDft = 0
032500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032600030822     C                             posDaDft > 0
032700030822     C*
032800030822     C* Gestisco il 1° giro
032900030822     C                   IF        wGiroDft = 0
033000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
033100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033200030822     C* Incremento il contatore dei "giri"
033300030822     C                   EVAL      wGiroDft = 1
033400030822     C                   ELSE
033500030822     C                   EVAL      posDaDft = posADft
033600030822     C                   ENDIF
033700030822     C* Eseguo lo scan x trovare la fine del campo corrente
033800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033900030822     C*
034000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
034100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
034200030822     C                   IF        posDaDft > 0 AND
034300030822     C                             posADft  > 0
034400030822     C* CCM
034500030822     C                   IF        %subst(
034600030822     C                             %subst(vlrppt:posDaDft+1:
034700030822     C                             posADft-posDaDft-1):1:3)
034800030822     C                             = 'CCM'
034900030822     C                   EVAL      PiStr=%trim(%subst(
035000030822     C                             %subst(vlrppt:posDaDft+1:
035100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035200030822     C                   EXSR      CHKNUM
035300030822     C                   IF        PiInt=*on
035400040714     C                   Z-ADD     PiVal         VATCCM
035500030822     C                   ENDIF
035600030822     C                   ENDIF
035700030822     C* LNP
035800030822     C                   IF        %subst(
035900030822     C                             %subst(vlrppt:posDaDft+1:
036000030822     C                             posADft-posDaDft-1):1:3)
036100030822     C                             = 'LNP'
036200030822     C                   EVAL      PiStr=%trim(%subst(
036300030822     C                             %subst(vlrppt:posDaDft+1:
036400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036500030822     C                   EXSR      CHKNUM
036600030822     C                   IF        PiInt=*on
036700040714     C                   Z-ADD     PiVal         VATLNP
036800030822     C                   ENDIF
036900030822     C                   ENDIF
037000040714     C* TRC
037100030822     C                   IF        %subst(
037200030822     C                             %subst(vlrppt:posDaDft+1:
037300030822     C                             posADft-posDaDft-1):1:3)
037400040714     C                             = 'TRC'
037500040714     C                   EVAL      VATTRC=%trim(%subst(
037600030822     C                             %subst(vlrppt:posDaDft+1:
037700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037800030822     C                   ENDIF
037900030822     C* ...
038000030822     C                   ENDIF
038100030822     C                   ENDDO
038200020204     C*
038300010330     C                   ENDSR
038400010607     C*----------------------------------------------------*
038500040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
038600010607     C*----------------------------------------------------*
038700040714     C     IMPVAT        BEGSR
038800010607     C*
038900110222     C                   SETOFF                                       313230
039000030515     C*
039100040714     C                   MOVEL     datcor        VATAAS
039200010607     C*
039300030715     C**********
039400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
039500030715     C**********
039600030715     C                   IF        wGiro = *zeros
039700040726     C*
039800040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
039900040726     C                   EVAL      depvinDTA = vinDTA
040000040726     C*
040100040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
040200040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
040300040726     C                                      %trim(intcol(3))
040400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
040500031201     C     minu:maiu     XLATE     vindta        vindta
040600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
040700040324     C                   DOW       posDa <= %len(%trimr(vindta))
040800030715     C*
040900030715     C* Gestisco il 1° campo
041000030715     C                   IF        i = 1
041100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
041200030715     C                   EVAL      posDa = 1
041300030715     C* Eseguo lo scan x trovare la fine del primo campo
041400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
041500030715     C* A questo punto "estraggo" il campo corrente
041600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
041700030715     C                                                    (posA-posDa))
041800030715     C* X i campi successivi al 1°
041900030715     C                   ELSE
042000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
042100030715     C                   EVAL      posDa = posA + 1
042200030715     C* Eseguo lo scan x trovare la fine del campo corrente
042300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
042400030715     C* Gestisco l'ultimo campo
042500030715     C                   IF        posA = *zeros
042600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
042700030715     C* A questo punto "estraggo" il campo corrente
042800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
042900030715     C                                                    (posA-posDa))
043000030715     C                   LEAVE
043100030715     C                   ELSE
043200030715     C* A questo punto "estraggo" il campo corrente
043300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043400030715     C                                                    (posA-posDa))
043500030715     C                   ENDIF
043600030715     C                   ENDIF
043700030715     C* Incremento il contatore d campo
043800030715     C                   EVAL      i = i +1
043900030715     C                   ENDDO
044000030715     C*
044100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
044200030715     C* delimitazione testo
044300030715     C                   EVAL      i = 1
044400030715     C                   DOW       i < %elem(SkSplitFLD)
044500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
044600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
044700030715     C                   EVAL      i = i + 1
044800030715     C                   ENDDO
044900030715     C                   EVAL      wGiro = 1
045000040726     C* Ripristino la 1° riga originale
045100040726     C                   EVAL      vinDTA = depvinDTA
045200030715     C                   ELSE
045300030715     C                   EVAL      wGiro = 2
045400031204     C**********
045500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
045600031204     C* d separatore campo
045700031204     C**********
045800040726     C                   EVAL      vindta = %trim(vindta)
045900040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
046000040726     C                   EVAL      vindta = %subst(vindta:2)
046100040726     C                   ENDDO
046200031204     C*
046300031204     C                   Z-ADD     *zeros        lunghInput        4 0
046400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
046500040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
046600040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
046700040726     C                   EVAL      lunghInput = %len(%trim(vindta))
046800040726     C                   ENDDO
046900030715     C**********
047000030715     C* Effettuo lo split del campo dati d input
047100030715     C**********
047200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
047300040324     C                   DOW       posDa <= %len(%trimr(vindta))
047400030715     C*
047500030715     C* Gestisco il 1° campo
047600030715     C                   IF        i = 1
047700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
047800030715     C                   EVAL      posDa = 1
047900030715     C* Eseguo lo scan x trovare la fine del primo campo
048000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
048100030715     C* A questo punto "estraggo" il campo corrente
048200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
048300030715     C                                                    (posA-posDa))
048400030715     C* X i campi successivi al 1°
048500030715     C                   ELSE
048600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
048700030715     C                   EVAL      posDa = posA + 1
048800031204     C* Verifico che nn vi sia il campo nn valorizzato
048900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
049000031204     C* Se campo nn valorizzato skippo al prossimo
049100031204     C                   EVAL      SkSplitCSV(i) = *blanks
049200031204     C                   EVAL      posA  = posA + 1
049300031204     C                   ELSE
049400030715     C* Eseguo lo scan x trovare la fine del campo corrente
049500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049600030715     C* Gestisco l'ultimo campo
049700030715     C                   IF        posA = *zeros
049800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
049900030715     C* A questo punto "estraggo" il campo corrente
050000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
050100030715     C                                                    (posA-posDa))
050200030715     C                   LEAVE
050300030715     C                   ELSE
050400030715     C* A questo punto "estraggo" il campo corrente
050500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
050600030715     C                                                    (posA-posDa))
050700030715     C                   ENDIF
050800031204     C                   ENDIF
050900030715     C                   ENDIF
051000030715     C* Incremento il contatore d campo
051100030715     C                   EVAL      i = i +1
051200030715     C                   ENDDO
051300030715     C*
051400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
051500030715     C* delimitazione testo
051600030715     C                   EVAL      i = 1
051700030715     C                   DOW       i < %elem(SkSplitCSV)
051800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
051900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
052000030715     C                   EVAL      i = i + 1
052100030715     C                   ENDDO
052200030715     C*
052300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
052400030715     C                   EVAL      i = 1
052500030715     C                   DOW       i < %elem(SkSplitFLD)
052600030715     C*
052700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
052800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
052900030715     C                             SkSplitCSV(i) <> *blanks
053000030721     C*** ATB
053100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
053200040714     C                   EVAL      VATATB = SkSplitCSV(i)
053300030721     C                   ENDIF
053400040714     C*** TRC
053500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
053600040714     C                   EVAL      VATTRC = SkSplitCSV(i)
053700030715     C                   ENDIF
053800040714     C*** NOT
053900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
054000040714     C                   EVAL      VATNOT = SkSplitCSV(i)
054100030721     C                   ENDIF
054200030721     C*
054300030721     C* Reperisco quindi i campi numerici...
054400030721     C*** NSP
054500151105     C* "normalizzare" le casistiche ad una forma numerica lunga 8, ovvero (le x sono numeri):
054600160401     C* xxxxxxx (es.: 3814425) = quando è un numerico lungo 7 lo trasformiamo in 99xxxxxx => NO !!
054700151105     C* MASxxxxx (es.: MAS58523) = quando inizia con 'MAS' lo trasformiamo in 111xxxxx
054800151105     C* TRAxxxxx (es.: TRA77848) = quando inizia con 'TRA' lo trasformiamo in 222xxxxx
054900160401     C* TKAMxxxx (es.: TKAM4402) = quando inizia con 'TKAM' lo trasformiamo in 8888xxxx
055000160401     C* TAxxxx (es.: TA2176) = quando inizia con 'TA' lo trasformiamo in 9999xxxx
055100151105     C* Quindi del valore risultante teniamo gli ultimi 7 digits a dx per NSP
055200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
055300151105     C                   EVAL      w8 = *blank
055400151105     C                   SELECT
055500151105     C                   WHEN      %len(%trim(SkSplitCSV(i))) = 7
055600160401     C                   EVAL      w8 = %trim(SkSplitCSV(i))
055700160401     C***                EVAL      w8 = '99' +
055800160401     C***                           %subst(%trim(SkSplitCSV(i)) : 2 : 6)
055900151105     C                   WHEN      %subst(%trim(SkSplitCSV(i)) : 1 : 3)
056000151105     C                             = 'MAS'
056100151105     C                   EVAL      w8 =
056200151105     C                              %replace('111':%trim(SkSplitCSV(i)))
056300151105     C                   WHEN      %subst(%trim(SkSplitCSV(i)) : 1 : 3)
056400151105     C                             = 'TRA'
056500151105     C                   EVAL      w8 =
056600151105     C                              %replace('222':%trim(SkSplitCSV(i)))
056700151105     C                   WHEN      %subst(%trim(SkSplitCSV(i)) : 1 : 4)
056800151105     C                             = 'TKAM'
056900151105     C                   EVAL      w8 =
057000160401     C                             %replace('8888':%trim(SkSplitCSV(i)))
057100151105     C                   WHEN      %subst(%trim(SkSplitCSV(i)) : 1 : 2)
057200151105     C                             = 'TA'
057300151105     C                   EVAL      w8 =
057400160401     C                             %replace('9999':%trim(SkSplitCSV(i))
057500151105     C                             : 1 : 2)
057600151105     C                   ENDSL
057700151105     C                   EVAL      PiStr=w8
057800030721     C                   EXSR      CHKNUM
057900040726     C                   IF        PiInt=*on
058000040726     C                   Z-ADD     PiVal         VATNSP
058100030721     C                   ELSE
058200071002     C                   SETON                                        32
058300040714     C                   EVAL      VATNSP = *zeros
058400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058500040714     C                             + ' ' + 'VATNSP'
058600030721     C                   ENDIF
058700030721     C                   ENDIF
058800030721     C*** CCM
058900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
059000030721     C                   EVAL      PiStr=SkSplitCSV(i)
059100030721     C                   EXSR      CHKNUM
059200030721     C                   IF        PiInt=*on      AND
059300030721     C                             PiVal<=9999999 AND
059400030721     C                             PiVal>*zeros
059500040714     C                   Z-ADD     PiVal         VATCCM
059600030721     C                   ELSE
059700030721     C                   SETON                                        32
059800040714     C                   EVAL      VATCCM = *zeros
059900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
060000040714     C                             + ' ' + 'VATCCM'
060100030721     C                   ENDIF
060200030721     C                   ENDIF
060300030721     C*** LNP
060400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
060500030721     C                   EVAL      PiStr=SkSplitCSV(i)
060600030721     C                   EXSR      CHKNUM
060700030721     C                   IF        PiInt=*on    AND
060800030721     C                             PiVal<=999   AND
060900030721     C                             PiVal>*zeros
061000040714     C                   Z-ADD     PiVal         VATLNP
061100030721     C                   ELSE
061200030721     C                   SETON                                        32
061300040714     C                   EVAL      VATLNP = *zeros
061400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
061500040714     C                             + ' ' + 'VATLNP'
061600030721     C                   ENDIF
061700030721     C                   ENDIF
061800030721     C*** NRS
061900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
062000030721     C                   EVAL      PiStr=SkSplitCSV(i)
062100030721     C                   EXSR      CHKNUM
062200030721     C                   IF        PiInt=*on AND
062300030721     C                             PiVal<=99
062400040714     C                   Z-ADD     PiVal         VATNRS
062500030721     C                   ELSE
062600030721     C                   SETON                                        32
062700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
062800040714     C                             + ' ' + 'VATNRS'
062900030721     C                   ENDIF
063000030721     C                   ENDIF
063100110222     C*
063200110222     C***  ===>  Gestione campi "particolari" dentro ciclo
063300140821     C*** WNOT_E
063400140821     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
063500140821     C                   EVAL      wNOT_E = %trim(SkSplitCSV(i))
063600110222     C                   ENDIF
063700110222     C*
063800110222     C***  <===  -----------------------------------------
063900030715     C*
064000030715     C                   ENDIF
064100030715     C                   EVAL      i = i + 1
064200030715     C                   ENDDO
064300040726     C*
064400040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
064500040726     C                   IF        VATNSP = *zeros
064600040726     C                   SETON                                        31
064700040726     C                   ENDIF
064800040726     C*
064900040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
065000040726     C                   IF        VATLNP = *zeros
065100040726     C                   SETON                                        31
065200040726     C                   ENDIF
065300020204     C*
065400030715     C                   ENDIF
065500020204     C*
065600010607     C                   ENDSR
065700010607     C*----------------------------------------------------*
065800040714
065900010330
066000010330
066100010330     C*----------------------------------------------------*
066200010330     C*  CONTROLLO NUMERICITA' CAMPI
066300010330     C*----------------------------------------------------*
066400010330     C     CHKNUM        BEGSR
066500010330     C*
066600010606     C                   IF        PiDecChr = *blanks
066700030715     C                   EVAL      PiDecChr = CharNUM
066800010606     C                   ENDIF
066900010606     C*
067000010606     C                   CALL(e)   'ISNUMERIC'
067100010330     C                   PARM                    PiStr            30
067200010606     C                   PARM                    PiDecChr          1
067300010330     C                   PARM      *ZEROS        PiVal            30 9
067400010330     C                   PARM      '0'           PiInt             1
067500010330     C                   PARM      '0'           PiNum             1
067600010330     C                   IF        %error
067700010606     C                   EVAL      PiNum=*off
067800010330     C                   ENDIF
067900010330     C*
068000010330     C                   ENDSR
068100010330     C***
068200010330
068300010601
068400010601
068500010601
068600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
068700010601     C     repfil        BEGSR
068800010601     C*
068900010601     C                   if        invfil = *zeros and
069000010601     C                             depfil > *zeros and
069100010629     C                             (vinflg = *blanks or
069200010629     C                              vinflg = *zeros)
069300010601     C
069400010601     C                   eval      invfil = depfil
069500010601     C                   endif
069600010601     C*
069700010601     C                   if        depfil <> invfil and
069800010601     C                             invfil > *zeros
069900010601     C                   eval      flgMulti = '1'
070000010601     C                   if        vinflg = *blanks
070100010601     C                   add       1             cntNonEl
070200010601     C                   endif
070300010601     C                   endif
070400010601     C*
070500010601     C                   if        vinflg = '2'
070600010601     C                   eval      flgStato = '2'
070700010601     C                   endif
070800010601     C*
070900010601     C                   ENDSR
071000010601     C***
071100010601
071200010601
071300010601
071400010330
071500010330
071600010330
071700990920      /TITLE Invio dei dati al punto operativo.
071800000613     C     invio         BEGSR
071900990920     C*
072000990920     C                   reset                   dscmz
072100010601     C                   move      invfil        cmzdst
072200990920     C                   eval      cmzfld = vlrfou
072300990920     C                   eval      cmzmbd = vlrhdl
072400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
072500000710     C                   if        prmfir = *blanks
072600040714     C                   eval      cmzfla = 'FIVAT00F'
072700040714     C                   eval      cmzmba = 'FIVAT00F'
072800000710     C                   else
072900000710     C                   eval      cmzfla = prmfir
073000000710     C                   eval      cmzmba = prmfir
073100000710     C                   endif
073200990920     C                   eval      cmznrr = *zeros
073300990920     C                   move      §ctrok        cmznrr
073400021018     C                   eval      cmzlba = vlrfl1
073500990920     C                   call(e)   'TIS711C'
073600990920     C                   parm                    dscmz
073700990921     C                   parm      *blanks       esito
073800990920     C                   if        %error
073900990920     C                             or cmzerr = '1'
074000990921     C                             or esito  = '1'
074100000710     C                   eval      wrkesito = '3'
074200990920     C                   endif
074300990920     C*
074400000613     C                   ENDSR
074500990910
074600010601
074700010601
074800010601
074900010601
075000010601      /TITLE Invio dei dati al punto operativo.
075100010601     C     opeini        BEGSR
075200010601     C*
075300010601     C* Inizializzo flag e contatori operativi
075400010601     C                   movel     '0'           flgGiro           1
075500010601     C                   movel     '0'           flgMulti          1
075600010601     C                   movel     '1'           flgStato          1
075700010615     C                   movel     '0'           flgOk             1
075800010601     C                   z-add     *zeros        cntNonEl         10 0
075900010601     C                   z-add     *zeros        depfil            3 0
076000010601     C                   z-add     *zeros        invfil            3 0
076100010601     C*
076200010601     C                   ENDSR
076300010601     C***
076400010601
076500010601
076600050421
076700050421
076800050421     C     *pssr         BEGSR
076900050421     C*
077000050421     C                   eval      wrkesito = '2'
077100050421     C*
077200050421     C                   ENDSR     '*CANCL'
077300050421     C***
077400050421
077500010601
077600010330
077700010330
077800000613     C     *inzsr        BEGSR
077900990910     C*
078000990910     C     *entry        plist
078100990920     C                   parm                    tivlrds
078200990921     C                   parm      wrkesito      esito
078300000724     C                   parm                    prmlit
078400000710     C                   parm                    prmfir
078500010330     C*
078600010330     C* CALCOLA LA DATA CORRENTE
078700010330     C                   time                    wn14             14 0
078800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
078900010330     C                   z-add     wn8           g08dat
079000010330     C                   z-add     *zeros        g08inv
079100010330     C                   movel     '0'           g08err
079200010330     C                   call      'XSRDA8'
079300010330     C                   parm                    wlbda8
079400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
079500000613     C*
079600000613     C                   ENDSR
079700000613     C***
079800040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
079900151019X;VATNSP;VATNOT_E;
080000140821
080100040726
