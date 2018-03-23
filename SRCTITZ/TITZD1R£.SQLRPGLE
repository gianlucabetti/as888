000100110222      /TITLE Upload via Internet: traduzione in FIVATWWR. (Cliente CHEBANCA - Cod. 0895142)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500040714     FFIVATwwr  O    E             DISK    usropn
000600000313     D*
000700040726     D*------------
000800040726     D* SCHIEDE A PROGRAMMA
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
005100110222
005200110222     D*------------------
005300110222     D* DEFINIZIONE DS DI LETTURA VIA SWL ARCHIVIO FNVAB
005400110222     D*------------------
005500110222     D FNVAB_DS        DS                  INZ
005600110222     D  VABCCM                        7  0
005700110222     D  VABAAS                        4  0
005800110222     D  VABLNP                        3  0
005900110222     D  VABNRS                        2  0
006000110222     D  VABNSP                        7  0
006100110222     D  VABIND                       35
006200110222     D  VABLOD                       35
006300110222     D  CONTEGGIO                     9  0
006400110222     D*
006500110222
006600031201     D*------------------
006700031201     D* Costanti
006800031201     D*------------------
006900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100010330
007200010330
007300010330
007400120309     C*
007500120309     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600120309     C
007700120309     C/EXEC SQL
007800120309     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900120309     C/END-EXEC
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300010601     C*
008400010601     C                   exsr      opeini
008500040714     C                   exsr      rwvat
008600010601     C*
008700010601     C                   seton                                        lr
008800010601
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010601     C*--------------------------------------------------------
009500010601     C     PREELA        BEGSR
009600010601     C*
009700010601     C* SE OCCORRE SPEDIRE IN FILIALE
009800010601     C                   if        invfil <> *zeros and
009900010601     C                             flgGiro = '0'
010000010601     C*
010100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010601     C                   eval      flgGiro = '1'
010300010601     C*
010400010601     C                   endif
010500010601     C*
010600010601     C                   ENDSR
010700010601     C***
010800010601
010900010601
011000010601
011100010601
011200010601
011300010601
011400010601
011500010601     C*--------------------------------------------------------
011600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011700010601     C*--------------------------------------------------------
011800010601     C     ENDELA        BEGSR
011900000616     C*
012000010601     C                   ENDSR
012100010601     C***
012200000613
012300010330
012400010601
012500010601
012600010601
012700010330     C*--------------------------------------------------------
012800040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
012900010330     C*--------------------------------------------------------
013000040714     C     RWVAT         BEGSR
013100010330     C*
013200010330     C                   if        not %open(tivin00r)
013300010330     C                   open      tivin00r
013400010330     C                   endif
013500040714     C                   if        not %open(fivatwwr)
013600040714     C                   open      fivatwwr
013700010330     C                   endif
013800010330     C*
013900010604     C                   clear                   §CTROK
014000010604     C                   clear                   §CTRMO
014100010604     C                   clear                   §CTRNO
014200040726     C*
014300040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014400040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
014500040726     C                   exsr      inzvar
014600040726     C                   exsr      impvat
014700010330     C*
014800030822     C                   do        *HIVAL
014900010330     C*
015000030822     C                   read      tivin00r                               70
015100010618     C*
015200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015300010618     C                   if        vinflg = '1'
015400010618     C                   eval      flgOk = '1'
015500010618     C                   endif
015600010618     C*
015700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015800040510     C                   z-add     *zeros        wLenVINDTA        4 0
015900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016300040510     C                   z-add     1             wFlgVINDTA
016400040510     C                   leave
016500040510     C                   endif
016600040510     C                   enddo
016700040510     C*
016800040218     C                   if        vindta > *blanks AND
016900040510     C                             wFlgVINDTA = 1
017000010330     C                   add       1             rrnum
017100010330     C*
017200010601     C                   if        *in70 = *off and
017300010330     C                             (vinflg = *blanks
017400010330     C                              or vinflg = '0'
017500010330     C                              or vinflg = '2')
017600010330     C*
017700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017800010711     C                   if        vinflg = *blanks or vinflg = '0'
017900010711     C                   clear                   vinmsg
018000010711     C                   endif
018100010601     C*
018200010330     C                   exsr      inzvar
018300040714     C                   exsr      impvat
018400010601     C*
018500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018600040714     C                   eval      depfil = VATLNP
018700010601     C                   exsr      repfil
018800010601     C                   if        depfil = invfil
018900021025     C                   if        vlrpoi = 999
019000040714     C                   move(P)   invfil        VATFGS
019100021025     C                   else
019200040714     C                   move(P)   vlrpoi        VATFGS
019300021025     C                   endif
019400010601     C*
019500010601     C                   exsr      PREELA
019600010601     C*
019700010604     C* Ebbene...
019800010604     C*
019900030822     C  N31              add       1             §CTROK            7 0
020000030822     C   32              add       1             §CTRMO            7 0
020100030822     C   31              add       1             §CTRNO            7 0
020200030822     C                   if        wGiro = 2
020300110222     C*
020400110222     C                   if        not *in30
020500110222     C*
020600110222     C* 1° inserimento tipo record 'A'
020700110222     C                   eval      VATTRC = 'A'
020800110222     C                   eval      VATNOT = %trim(wNOT_A_1) + ' ' +
020900110222     C                                      %trim(wNOT_A_2)
021000110222     C                   if        vatnot <> *blanks
021100040714     C  N31              write     FIVAT000
021200110222     C                   endif
021300110222     C*
021400110222     C* 2° inserimento tipo record 'B'
021500110222     C                   eval      VATTRC = 'B'
021600110222     C                   eval      VATNOT = %trim(wNOT_B_1) + ' ' +
021700110222     C                                      %trim(wNOT_B_2)
021800110222     C                   if        vatnot <> *blanks
021900110222     C  N31              write     FIVAT000
022000110222     C                   endif
022100110222     C                   endif
022200030822     C                   endif
022300010604     C*
022400010604     C                   if        *in31 = *off and
022500010604     C                             *in32 = *off
022600010604     C                   eval      vinflg = '1'
022700010604     C                   else
022800010604     C                   eval      vinflg = '2'
022900010604     C                   endif
023000010604     C                   endif
023100010601     C*
023200010604     C                   endif
023300010604     C*
023400010330     C                   else
023500010330     C                   eval      vinflg = '1'
023600010330     C                   endif
023700010601     C*
023800010601     C  N70              update    tivin000
023900010330     C*
024000030822     C  N70              enddo
024100010601     C
024200010601     C*
024300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024400010601     C                   if        cntNonEl = *zeros or
024500010601     C                             flgMulti = '0'
024600010330     C* Se non ci sono record con errori ...
024700010601     C                   if        §ctrno = 0 and
024800010604     C                             §ctrmo = 0 and
024900010601     C                             flgStato <> '2'
025000010330     C* ... restituisco esito OK.
025100010330     C                   eval      wrkesito = '0'
025200010330     C                   else
025300010330     C                   if        §ctrok > 0
025400010330     C                   eval      wrkesito = '1'
025500010330     C                   else
025600010615     C                   if        flgOk = '0'
025700010615     C                   eval      wrkesito = '2'
025800010615     C                   else
025900010615     C                   eval      wrkesito = '6'
026000010615     C                   endif
026100010330     C                   endif
026200010330     C                   endif
026300010601     C                   else
026400010601     C                   eval      wrkesito = '9'
026500010601     C                   endif
026600010330     C*
026700010330     C                   if        %open(tivin00r)
026800010330     C                   close     tivin00r
026900010330     C                   endif
027000040714     C                   if        %open(fivatwwr)
027100040714     C                   close     fivatwwr
027200010330     C                   endif
027300010601     C*
027400010601     C                   if        vlrpoi <> 999
027500010601     C                   eval      invfil = vlrpoi
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        §ctrok > 0
027900010601     C                             and invfil > *zeros
028000010330     C                   exsr      invio
028100010330     C                   endif
028200010601     C*
028300010618     C                   if        flgGiro = '1'
028400010601     C                   exsr      endela
028500010618     C                   endif
028600010330     C*
028700010330     C                   ENDSR
028800010330     C***
028900010601
029000010601
029100010601
029200010330     C*----------------------------------------------------*
029300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029400010330     C*----------------------------------------------------*
029500010330     C     INZVAR        BEGSR
029600010330     C*
029700030822     C* Inizializzo variabili di wrk
029800020204     C                   Z-ADD     *zeros        Num5_0            5 0
029900030822     C*
030000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
030100040714     C                   CLEAR                   FIVAT000
030200030822     C                   CLEAR                   SkSplitCSV
030300030822     C                   EVAL      i = 1
030400030822     C                   EVAL      posDa = *zeros
030500030822     C                   EVAL      posA  = *zeros
030600110222     C*
030700110222     C                   MOVEL     *blanks       wRSD             35
030800110222     C                   MOVEL     *blanks       wRSD_1           35
030900110222     C                   MOVEL     *blanks       wRSD_2           35
031000110222     C                   MOVEL     *blanks       wNOT_A_1         35
031100110222     C                   MOVEL     *blanks       wNOT_A_2         35
031200110222     C                   MOVEL     *blanks       wNOT_B_1         35
031300110222     C                   MOVEL     *blanks       wNOT_B_2         35
031400030822     C*
031500030822     C* Reimposto i valori di default
031600030822     C                   EXSR      DEFCAM
031700030822     C*
031800010330     C                   ENDSR
031900010330     C*----------------------------------------------------*
032000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
032100010330     C*----------------------------------------------------*
032200010330     C     DEFCAM        BEGSR
032300030822     C*
032400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032500030715     C* e delimitatore testo.
032600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
032700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
032800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032900030822     C*
033000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
033100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
033200030715     C                   SETON                                        50
033300030715     C                   ELSE
033400030715     C                   SETOFF                                       50
033500030715     C                   ENDIF
033600030822     C*
033700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
033800030822     C                   EVAL      posDaDft = 1
033900030822     C                   EVAL      posADft  = 0
034000030822     C                   EVAL      wGiroDft = 0
034100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034200030822     C                             posDaDft > 0
034300030822     C*
034400030822     C* Gestisco il 1° giro
034500030822     C                   IF        wGiroDft = 0
034600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
034700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
034800030822     C* Incremento il contatore dei "giri"
034900030822     C                   EVAL      wGiroDft = 1
035000030822     C                   ELSE
035100030822     C                   EVAL      posDaDft = posADft
035200030822     C                   ENDIF
035300030822     C* Eseguo lo scan x trovare la fine del campo corrente
035400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035500030822     C*
035600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
035700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
035800030822     C                   IF        posDaDft > 0 AND
035900030822     C                             posADft  > 0
036000030822     C* CCM
036100030822     C                   IF        %subst(
036200030822     C                             %subst(vlrppt:posDaDft+1:
036300030822     C                             posADft-posDaDft-1):1:3)
036400030822     C                             = 'CCM'
036500030822     C                   EVAL      PiStr=%trim(%subst(
036600030822     C                             %subst(vlrppt:posDaDft+1:
036700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036800030822     C                   EXSR      CHKNUM
036900030822     C                   IF        PiInt=*on
037000040714     C                   Z-ADD     PiVal         VATCCM
037100030822     C                   ENDIF
037200030822     C                   ENDIF
037300030822     C* LNP
037400030822     C                   IF        %subst(
037500030822     C                             %subst(vlrppt:posDaDft+1:
037600030822     C                             posADft-posDaDft-1):1:3)
037700030822     C                             = 'LNP'
037800030822     C                   EVAL      PiStr=%trim(%subst(
037900030822     C                             %subst(vlrppt:posDaDft+1:
038000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038100030822     C                   EXSR      CHKNUM
038200030822     C                   IF        PiInt=*on
038300040714     C                   Z-ADD     PiVal         VATLNP
038400030822     C                   ENDIF
038500030822     C                   ENDIF
038600040714     C* TRC
038700030822     C                   IF        %subst(
038800030822     C                             %subst(vlrppt:posDaDft+1:
038900030822     C                             posADft-posDaDft-1):1:3)
039000040714     C                             = 'TRC'
039100040714     C                   EVAL      VATTRC=%trim(%subst(
039200030822     C                             %subst(vlrppt:posDaDft+1:
039300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039400030822     C                   ENDIF
039500030822     C* ...
039600030822     C                   ENDIF
039700030822     C                   ENDDO
039800020204     C*
039900010330     C                   ENDSR
040000010607     C*----------------------------------------------------*
040100040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
040200010607     C*----------------------------------------------------*
040300040714     C     IMPVAT        BEGSR
040400010607     C*
040500110222     C                   SETOFF                                       313230
040600030515     C*
040700040714     C                   MOVEL     datcor        VATAAS
040800010607     C*
040900030715     C**********
041000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
041100030715     C**********
041200030715     C                   IF        wGiro = *zeros
041300040726     C*
041400040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
041500040726     C                   EVAL      depvinDTA = vinDTA
041600040726     C*
041700040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
041800040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
041900040726     C                                      %trim(intcol(3))
042000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
042100031201     C     minu:maiu     XLATE     vindta        vindta
042200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
042300040324     C                   DOW       posDa <= %len(%trimr(vindta))
042400030715     C*
042500030715     C* Gestisco il 1° campo
042600030715     C                   IF        i = 1
042700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
042800030715     C                   EVAL      posDa = 1
042900030715     C* Eseguo lo scan x trovare la fine del primo campo
043000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
043100030715     C* A questo punto "estraggo" il campo corrente
043200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
043300030715     C                                                    (posA-posDa))
043400030715     C* X i campi successivi al 1°
043500030715     C                   ELSE
043600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
043700030715     C                   EVAL      posDa = posA + 1
043800030715     C* Eseguo lo scan x trovare la fine del campo corrente
043900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044000030715     C* Gestisco l'ultimo campo
044100030715     C                   IF        posA = *zeros
044200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
044300030715     C* A questo punto "estraggo" il campo corrente
044400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
044500030715     C                                                    (posA-posDa))
044600030715     C                   LEAVE
044700030715     C                   ELSE
044800030715     C* A questo punto "estraggo" il campo corrente
044900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
045000030715     C                                                    (posA-posDa))
045100030715     C                   ENDIF
045200030715     C                   ENDIF
045300030715     C* Incremento il contatore d campo
045400030715     C                   EVAL      i = i +1
045500030715     C                   ENDDO
045600030715     C*
045700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
045800030715     C* delimitazione testo
045900030715     C                   EVAL      i = 1
046000030715     C                   DOW       i < %elem(SkSplitFLD)
046100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
046200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
046300030715     C                   EVAL      i = i + 1
046400030715     C                   ENDDO
046500030715     C                   EVAL      wGiro = 1
046600040726     C* Ripristino la 1° riga originale
046700040726     C                   EVAL      vinDTA = depvinDTA
046800030715     C                   ELSE
046900030715     C                   EVAL      wGiro = 2
047000031204     C**********
047100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
047200031204     C* d separatore campo
047300031204     C**********
047400040726     C                   EVAL      vindta = %trim(vindta)
047500040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
047600040726     C                   EVAL      vindta = %subst(vindta:2)
047700040726     C                   ENDDO
047800031204     C*
047900031204     C                   Z-ADD     *zeros        lunghInput        4 0
048000040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
048100040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
048200040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
048300040726     C                   EVAL      lunghInput = %len(%trim(vindta))
048400040726     C                   ENDDO
048500030715     C**********
048600030715     C* Effettuo lo split del campo dati d input
048700030715     C**********
048800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
048900040324     C                   DOW       posDa <= %len(%trimr(vindta))
049000030715     C*
049100030715     C* Gestisco il 1° campo
049200030715     C                   IF        i = 1
049300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
049400030715     C                   EVAL      posDa = 1
049500030715     C* Eseguo lo scan x trovare la fine del primo campo
049600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
049700030715     C* A questo punto "estraggo" il campo corrente
049800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
049900030715     C                                                    (posA-posDa))
050000030715     C* X i campi successivi al 1°
050100030715     C                   ELSE
050200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
050300030715     C                   EVAL      posDa = posA + 1
050400031204     C* Verifico che nn vi sia il campo nn valorizzato
050500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
050600031204     C* Se campo nn valorizzato skippo al prossimo
050700031204     C                   EVAL      SkSplitCSV(i) = *blanks
050800031204     C                   EVAL      posA  = posA + 1
050900031204     C                   ELSE
051000030715     C* Eseguo lo scan x trovare la fine del campo corrente
051100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
051200030715     C* Gestisco l'ultimo campo
051300030715     C                   IF        posA = *zeros
051400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
051500030715     C* A questo punto "estraggo" il campo corrente
051600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
051700030715     C                                                    (posA-posDa))
051800030715     C                   LEAVE
051900030715     C                   ELSE
052000030715     C* A questo punto "estraggo" il campo corrente
052100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
052200030715     C                                                    (posA-posDa))
052300030715     C                   ENDIF
052400031204     C                   ENDIF
052500030715     C                   ENDIF
052600030715     C* Incremento il contatore d campo
052700030715     C                   EVAL      i = i +1
052800030715     C                   ENDDO
052900030715     C*
053000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
053100030715     C* delimitazione testo
053200030715     C                   EVAL      i = 1
053300030715     C                   DOW       i < %elem(SkSplitCSV)
053400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
053500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
053600030715     C                   EVAL      i = i + 1
053700030715     C                   ENDDO
053800030715     C*
053900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
054000030715     C                   EVAL      i = 1
054100030715     C                   DOW       i < %elem(SkSplitFLD)
054200030715     C*
054300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
054400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
054500030715     C                             SkSplitCSV(i) <> *blanks
054600030721     C*** ATB
054700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
054800040714     C                   EVAL      VATATB = SkSplitCSV(i)
054900030721     C                   ENDIF
055000040714     C*** TRC
055100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
055200040714     C                   EVAL      VATTRC = SkSplitCSV(i)
055300030715     C                   ENDIF
055400040714     C*** NOT
055500040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
055600040714     C                   EVAL      VATNOT = SkSplitCSV(i)
055700030721     C                   ENDIF
055800030721     C*
055900030721     C* Reperisco quindi i campi numerici...
056000030721     C*** NSP
056100040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
056200030721     C                   EVAL      PiStr=SkSplitCSV(i)
056300030721     C                   EXSR      CHKNUM
056400040726     C                   IF        PiInt=*on
056500040726     C                   Z-ADD     PiVal         VATNSP
056600040802     C*                  IF        PiVal>9999999
056700040802     C*                  SETON                                        32
056800040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
056900040802     C*                            + ' ' + 'VATNSP'
057000040802     C*                  ENDIF
057100030721     C                   ELSE
057200071002     C                   SETON                                        32
057300040714     C                   EVAL      VATNSP = *zeros
057400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
057500040714     C                             + ' ' + 'VATNSP'
057600030721     C                   ENDIF
057700030721     C                   ENDIF
057800030721     C*** CCM
057900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
058000030721     C                   EVAL      PiStr=SkSplitCSV(i)
058100030721     C                   EXSR      CHKNUM
058200030721     C                   IF        PiInt=*on      AND
058300030721     C                             PiVal<=9999999 AND
058400030721     C                             PiVal>*zeros
058500040714     C                   Z-ADD     PiVal         VATCCM
058600030721     C                   ELSE
058700030721     C                   SETON                                        32
058800040714     C                   EVAL      VATCCM = *zeros
058900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
059000040714     C                             + ' ' + 'VATCCM'
059100030721     C                   ENDIF
059200030721     C                   ENDIF
059300030721     C*** LNP
059400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
059500030721     C                   EVAL      PiStr=SkSplitCSV(i)
059600030721     C                   EXSR      CHKNUM
059700030721     C                   IF        PiInt=*on    AND
059800030721     C                             PiVal<=999   AND
059900030721     C                             PiVal>*zeros
060000040714     C                   Z-ADD     PiVal         VATLNP
060100030721     C                   ELSE
060200030721     C                   SETON                                        32
060300040714     C                   EVAL      VATLNP = *zeros
060400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
060500040714     C                             + ' ' + 'VATLNP'
060600030721     C                   ENDIF
060700030721     C                   ENDIF
060800030721     C*** NRS
060900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
061000030721     C                   EVAL      PiStr=SkSplitCSV(i)
061100030721     C                   EXSR      CHKNUM
061200030721     C                   IF        PiInt=*on AND
061300030721     C                             PiVal<=99
061400040714     C                   Z-ADD     PiVal         VATNRS
061500030721     C                   ELSE
061600030721     C                   SETON                                        32
061700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
061800040714     C                             + ' ' + 'VATNRS'
061900030721     C                   ENDIF
062000030721     C                   ENDIF
062100110222     C*
062200110222     C***  ===>  Gestione campi "particolari" dentro ciclo
062300110222     C*** W_RSD_1
062400110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSD_1'
062500110222     C                   EVAL      wRSD_1 = %trim(SkSplitCSV(i))
062600110222     C                   ENDIF
062700110222     C*** W_RSD_2
062800110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_RSD_2'
062900110222     C                   EVAL      wRSD_2 = %trim(SkSplitCSV(i))
063000110222     C                   ENDIF
063100110222     C*** W_NOT_A_1
063200110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOT_A_1'
063300110222     C                   EVAL      wNOT_A_1 = %trim(SkSplitCSV(i))
063400110222     C                   ENDIF
063500110222     C*** W_NOT_A_2
063600110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOT_A_2'
063700110222     C                   EVAL      wNOT_A_2 = %trim(SkSplitCSV(i))
063800110222     C                   ENDIF
063900110222     C*** W_NOT_B_1
064000110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOT_B_1'
064100110222     C                   EVAL      wNOT_B_1 = %trim(SkSplitCSV(i))
064200110222     C                   ENDIF
064300110222     C*** W_NOT_B_2
064400110222     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOT_B_2'
064500110222     C                   EVAL      wNOT_B_2 = %trim(SkSplitCSV(i))
064600110222     C                   ENDIF
064700110222     C*
064800110222     C***  <===  -----------------------------------------
064900030715     C*
065000030715     C                   ENDIF
065100030715     C                   EVAL      i = i + 1
065200030715     C                   ENDDO
065300110222     C*
065400110222     C* Gestione partcolare x tentativo match su FNVAB
065500110222     C                   eval      wRSD = %trim(wRSD_1) + ' ' +
065600110222     C                                    %trim(wRSD_2)
065700110222     C*
065800110222     C/EXEC SQL
065900110222     C+ declare C1 cursor for select
066000110222     C+ vabccm, vabaas, vablnp, vabnrs, vabnsp, vabind, vablod,
066100110222     C+ count(*)
066200110222     C+ from filtragru/fivab00f
066300110222     C+ where vabccm = :VATCCM and vabrsd = :wRSD
066400110222     C+ group by
066500110222     C+ vabccm, vabaas, vablnp, vabnrs, vabnsp, vabind, vablod
066600110222     C+ for read only
066700110222     C/END-EXEC
066800110222     C
066900110222     C/EXEC SQL
067000110222     C+ open C1
067100110222     C/END-EXEC
067200110222     C
067300110222     C/EXEC SQL
067400110222     C+ Fetch C1 into :FNVAB_DS
067500110222     C/END-EXEC
067600110222     C
067700110222     C/EXEC SQL
067800110222     C+ close C1
067900110222     C/END-EXEC
068000110222     C*
068100110222     C                   if        conteggio = 1
068200110222     C                   eval      VATAAS = VABAAS
068300110222     C                   eval      VATLNP = VABLNP
068400110222     C                   eval      VATNRS = VABNRS
068500110222     C                   eval      VATNSP = VABNSP
068600110222     C                   else
068700110223     C                   seton                                        3031
068800110222     C                   endif
068900040726     C*
069000040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
069100040726     C                   IF        VATNSP = *zeros
069200040726     C                   SETON                                        31
069300040726     C                   ENDIF
069400040726     C*
069500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
069600040726     C                   IF        VATLNP = *zeros
069700040726     C                   SETON                                        31
069800040726     C                   ENDIF
069900020204     C*
070000030715     C                   ENDIF
070100020204     C*
070200010607     C                   ENDSR
070300010607     C*----------------------------------------------------*
070400040714
070500010330
070600010330
070700010330     C*----------------------------------------------------*
070800010330     C*  CONTROLLO NUMERICITA' CAMPI
070900010330     C*----------------------------------------------------*
071000010330     C     CHKNUM        BEGSR
071100010330     C*
071200010606     C                   IF        PiDecChr = *blanks
071300030715     C                   EVAL      PiDecChr = CharNUM
071400010606     C                   ENDIF
071500010606     C*
071600010606     C                   CALL(e)   'ISNUMERIC'
071700010330     C                   PARM                    PiStr            30
071800010606     C                   PARM                    PiDecChr          1
071900010330     C                   PARM      *ZEROS        PiVal            30 9
072000010330     C                   PARM      '0'           PiInt             1
072100010330     C                   PARM      '0'           PiNum             1
072200010330     C                   IF        %error
072300010606     C                   EVAL      PiNum=*off
072400010330     C                   ENDIF
072500010330     C*
072600010330     C                   ENDSR
072700010330     C***
072800010330
072900010601
073000010601
073100010601
073200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
073300010601     C     repfil        BEGSR
073400010601     C*
073500010601     C                   if        invfil = *zeros and
073600010601     C                             depfil > *zeros and
073700010629     C                             (vinflg = *blanks or
073800010629     C                              vinflg = *zeros)
073900010601     C
074000010601     C                   eval      invfil = depfil
074100010601     C                   endif
074200010601     C*
074300010601     C                   if        depfil <> invfil and
074400010601     C                             invfil > *zeros
074500010601     C                   eval      flgMulti = '1'
074600010601     C                   if        vinflg = *blanks
074700010601     C                   add       1             cntNonEl
074800010601     C                   endif
074900010601     C                   endif
075000010601     C*
075100010601     C                   if        vinflg = '2'
075200010601     C                   eval      flgStato = '2'
075300010601     C                   endif
075400010601     C*
075500010601     C                   ENDSR
075600010601     C***
075700010601
075800010601
075900010601
076000010330
076100010330
076200010330
076300990920      /TITLE Invio dei dati al punto operativo.
076400000613     C     invio         BEGSR
076500990920     C*
076600990920     C                   reset                   dscmz
076700010601     C                   move      invfil        cmzdst
076800990920     C                   eval      cmzfld = vlrfou
076900990920     C                   eval      cmzmbd = vlrhdl
077000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
077100000710     C                   if        prmfir = *blanks
077200040714     C                   eval      cmzfla = 'FIVAT00F'
077300040714     C                   eval      cmzmba = 'FIVAT00F'
077400000710     C                   else
077500000710     C                   eval      cmzfla = prmfir
077600000710     C                   eval      cmzmba = prmfir
077700000710     C                   endif
077800990920     C                   eval      cmznrr = *zeros
077900990920     C                   move      §ctrok        cmznrr
078000021018     C                   eval      cmzlba = vlrfl1
078100990920     C                   call(e)   'TIS711C'
078200990920     C                   parm                    dscmz
078300990921     C                   parm      *blanks       esito
078400990920     C                   if        %error
078500990920     C                             or cmzerr = '1'
078600990921     C                             or esito  = '1'
078700000710     C                   eval      wrkesito = '3'
078800990920     C                   endif
078900990920     C*
079000000613     C                   ENDSR
079100990910
079200010601
079300010601
079400010601
079500010601
079600010601      /TITLE Invio dei dati al punto operativo.
079700010601     C     opeini        BEGSR
079800010601     C*
079900010601     C* Inizializzo flag e contatori operativi
080000010601     C                   movel     '0'           flgGiro           1
080100010601     C                   movel     '0'           flgMulti          1
080200010601     C                   movel     '1'           flgStato          1
080300010615     C                   movel     '0'           flgOk             1
080400010601     C                   z-add     *zeros        cntNonEl         10 0
080500010601     C                   z-add     *zeros        depfil            3 0
080600010601     C                   z-add     *zeros        invfil            3 0
080700010601     C*
080800010601     C                   ENDSR
080900010601     C***
081000010601
081100010601
081200050421
081300050421
081400050421     C     *pssr         BEGSR
081500050421     C*
081600050421     C                   eval      wrkesito = '2'
081700050421     C*
081800050421     C                   ENDSR     '*CANCL'
081900050421     C***
082000050421
082100010601
082200010330
082300010330
082400000613     C     *inzsr        BEGSR
082500990910     C*
082600990910     C     *entry        plist
082700990920     C                   parm                    tivlrds
082800990921     C                   parm      wrkesito      esito
082900000724     C                   parm                    prmlit
083000000710     C                   parm                    prmfir
083100010330     C*
083200010330     C* CALCOLA LA DATA CORRENTE
083300010330     C                   time                    wn14             14 0
083400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
083500010330     C                   z-add     wn8           g08dat
083600010330     C                   z-add     *zeros        g08inv
083700010330     C                   movel     '0'           g08err
083800010330     C                   call      'XSRDA8'
083900010330     C                   parm                    wlbda8
084000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
084100000613     C*
084200000613     C                   ENDSR
084300000613     C***
084400040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
084500110222FILL001;FIL002;FIL003;W_RSD_1;W_RSD_2;W_NOT_A_1;W_NOT_A_2;
084600110222W_NOT_B_1;W_NOT_B_2;
084700040726
