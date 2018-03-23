000100040714      /TITLE Upload via Internet: traduzione in FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600040714     FFIVATwwr  O    E             DISK    usropn
000700030715     Ftitvxlp   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030715     Ftitvxlps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D wrkdata         s               d
002800010330     D wrkora          s               t
002900010330     D rrnum           s              6  0 INZ(*zeros)
003000010330     D recko           s            150    INZ(*blanks)
003100011113     D depcmd          s            150    INZ(*blanks)
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000030715     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400030822     D*------------------
004500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004600030822     D*------------------
004700030822     D posDaDft        S              3  0 INZ(*zeros)
004800030822     D posADft         S              3  0 INZ(*zeros)
004900030822     D j               s              3  0 INZ(1)
005000030822     D wGiroDft        s              1  0 INZ(*zeros)
005100010330     D*------------------
005200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300010330     D*------------------
005400010330     D WLBDA8          DS                  INZ
005500010330     D  G08DAT                 1      8  0
005600010330     D  G08INV                 9     16  0
005700010330     D  G08ERR                17     17
005800010330     D  G08TGI                18     22  0
005900031201     D*------------------
006000031201     D* Costanti
006100031201     D*------------------
006200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006400010330
006500010330
006600010330
006700990915     C                   time                    wrkdata
006800990915     C                   time                    wrkora
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200010601     C*
007300010601     C                   exsr      opeini
007400040714     C                   exsr      rwvat
007500010601     C*
007600010601     C                   seton                                        lr
007700010601
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010601     C*--------------------------------------------------------
008400010601     C     PREELA        BEGSR
008500010601     C*
008600010601     C* SE OCCORRE SPEDIRE IN FILIALE
008700010601     C                   if        invfil <> *zeros and
008800010601     C                             flgGiro = '0'
008900010601     C*
009000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010601     C                   eval      flgGiro = '1'
009200000724     C*
009300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009500010601     C     invfil        chain     azorg01l
009600000724     C                   if        %found
009700000724     C                   movel(p)  CMD(1)        depcmd
009800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009900000724     C*
010000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010100011113     C                   Z-ADD     150           LENGH            15 5
010200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010300000724     C                   PARM                    depcmd
010400000724     C                   PARM                    LENGH
010500000724     C*
010600000724     C                   endif
010700000616     C*
010800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010900000616     C                   movel(p)  CMD(2)        depcmd
011000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011100000616     C*
011200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011300011113     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    depcmd
011600000616     C                   PARM                    LENGH
011700000613     C*
011800030715     C                   if        not %open(titvxlps)
011900030715     C                   open      titvxlps
012000030715     C                   except    testdett
012100000613     C                   endif
012200010601     C*
012300010601     C                   endif
012400010601     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601
013400010601     C*--------------------------------------------------------
013500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013600010601     C*--------------------------------------------------------
013700010601     C     ENDELA        BEGSR
013800990915     C*
013900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014000000613     C*
014100030715     C                   if        %open(titvxlps)
014200000616     C                   except    findett
014300030715     C                   close     titvxlps
014400000613     C                   endif
014500000616     C*
014600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014700011113     C                   Z-ADD     150           LENGH            15 5
014800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
014900000616     C                   PARM                    CMD(3)
015000000616     C                   PARM                    LENGH
015100000616     C*
015200010601     C                   ENDSR
015300010601     C***
015400010601
015500010601
015600010601
015700010601
015800000613
015900000613     C*--------------------------------------------------------
016000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016100000613     C*--------------------------------------------------------
016200000621     C     STPR          BEGSR
016300000613     C*
016400030715     C                   if        not %open(titvxlp)
016500030715     C   50              open      titvxlp
016600990915     C                   endif
016700990915     C*
016800030715     C   50              except    riepilogo
016900990915     C*
017000030715     C                   if        %open(titvxlp)
017100030715     C   50              close     titvxlp
017200990914     C                   endif
017300000613     C*
017400000613     C                   ENDSR
017500000613     C***
017600000613
017700010330
017800010601
017900010601
018000010601
018100010330     C*--------------------------------------------------------
018200040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
018300010330     C*--------------------------------------------------------
018400040714     C     RWVAT         BEGSR
018500010330     C*
018600010330     C                   if        not %open(tivin00r)
018700010330     C                   open      tivin00r
018800010330     C                   endif
018900040714     C                   if        not %open(fivatwwr)
019000040714     C                   open      fivatwwr
019100010330     C                   endif
019200010330     C*
019300010604     C                   clear                   §CTROK
019400010604     C                   clear                   §CTRMO
019500010604     C                   clear                   §CTRNO
019600010330     C*
019700030822     C                   do        *HIVAL
019800010330     C*
019900030822     C                   read      tivin00r                               70
020000010618     C*
020100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020200010618     C                   if        vinflg = '1'
020300010618     C                   eval      flgOk = '1'
020400010618     C                   endif
020500010618     C*
020600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020700040510     C                   z-add     *zeros        wLenVINDTA        4 0
020800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021200040510     C                   z-add     1             wFlgVINDTA
021300040510     C                   leave
021400040510     C                   endif
021500040510     C                   enddo
021600040510     C*
021700040218     C                   if        vindta > *blanks AND
021800040510     C                             wFlgVINDTA = 1
021900010330     C                   add       1             rrnum
022000010330     C*
022100010601     C                   if        *in70 = *off and
022200010330     C                             (vinflg = *blanks
022300010330     C                              or vinflg = '0'
022400010330     C                              or vinflg = '2')
022500010330     C*
022600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022700010711     C                   if        vinflg = *blanks or vinflg = '0'
022800010711     C                   clear                   vinmsg
022900010711     C                   endif
023000010601     C*
023100010330     C                   exsr      inzvar
023200040714     C                   exsr      impvat
023300010601     C*
023400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500040714     C                   eval      depfil = VATLNP
023600010601     C                   exsr      repfil
023700090225     C                   if        VATFGS = *zeros
023800090225     C                   eval      VATFGS = VATLNP
023900090225     C                   endif
024000010601     C*
024100010601     C                   exsr      PREELA
024200010601     C*
024300010604     C* Ebbene...
024400010604     C*
024500030822     C  N31              add       1             §CTROK            7 0
024600030822     C   32              add       1             §CTRMO            7 0
024700030822     C   31              add       1             §CTRNO            7 0
024800030822     C                   if        wGiro = 2
024900040714     C  N31              write     FIVAT000
025000030822     C                   endif
025100010604     C*
025200010604     C                   if        *in31 = *off and
025300010604     C                             *in32 = *off
025400010604     C                   eval      vinflg = '1'
025500010604     C                   else
025600010604     C                   eval      recko = vindta
025700010604     C                   except    rigadett
025800010604     C                   eval      vinflg = '2'
025900010604     C                   endif
026000010601     C*
026100010604     C                   endif
026200010604     C*
026300010330     C                   else
026400010330     C                   eval      vinflg = '1'
026500010330     C                   endif
026600010601     C*
026700010601     C  N70              update    tivin000
026800010330     C*
026900030822     C  N70              enddo
027000010601     C
027100010601     C*
027200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027300010601     C                   if        cntNonEl = *zeros or
027400010601     C                             flgMulti = '0'
027500010330     C* Se non ci sono record con errori ...
027600010601     C                   if        §ctrno = 0 and
027700010604     C                             §ctrmo = 0 and
027800010601     C                             flgStato <> '2'
027900010330     C* ... restituisco esito OK.
028000010330     C                   eval      wrkesito = '0'
028100010330     C                   else
028200010330     C                   if        §ctrok > 0
028300010330     C                   eval      wrkesito = '1'
028400010330     C                   else
028500010615     C                   if        flgOk = '0'
028600010615     C                   eval      wrkesito = '2'
028700010615     C                   else
028800010615     C                   eval      wrkesito = '6'
028900010615     C                   endif
029000010330     C                   endif
029100010330     C                   endif
029200010601     C                   else
029300010601     C                   eval      wrkesito = '9'
029400010601     C                   endif
029500010330     C*
029600010330     C                   if        %open(tivin00r)
029700010330     C                   close     tivin00r
029800010330     C                   endif
029900040714     C                   if        %open(fivatwwr)
030000040714     C                   close     fivatwwr
030100010330     C                   endif
030200010601     C*
030300010601     C                   if        vlrpoi <> 999
030400010601     C                   eval      invfil = vlrpoi
030500010601     C                   endif
030600010330     C*
030700010330     C                   if        §ctrok > 0
030800010601     C                             and invfil > *zeros
030900010330     C                   exsr      invio
031000010330     C                   endif
031100010601     C*
031200010618     C                   if        flgGiro = '1'
031300010601     C                   exsr      endela
031400010618     C                   endif
031500010330     C*
031600010330     C                   ENDSR
031700010330     C***
031800010601
031900010601
032000010601
032100010330     C*----------------------------------------------------*
032200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032300010330     C*----------------------------------------------------*
032400010330     C     INZVAR        BEGSR
032500010330     C*
032600030822     C* Inizializzo variabili di wrk
032700020204     C                   Z-ADD     *zeros        Num5_0            5 0
032800030822     C*
032900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
033000040714     C                   CLEAR                   FIVAT000
033100030822     C                   CLEAR                   SkSplitCSV
033200030822     C                   EVAL      i = 1
033300030822     C                   EVAL      posDa = *zeros
033400030822     C                   EVAL      posA  = *zeros
033500030822     C*
033600030822     C* Reimposto i valori di default
033700030822     C                   EXSR      DEFCAM
033800030822     C*
033900010330     C                   ENDSR
034000010330     C*----------------------------------------------------*
034100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
034200010330     C*----------------------------------------------------*
034300010330     C     DEFCAM        BEGSR
034400030822     C*
034500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
034600030715     C* e delimitatore testo.
034700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
034800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
034900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035000030822     C*
035100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
035200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
035300030715     C                   SETON                                        50
035400030715     C                   ELSE
035500030715     C                   SETOFF                                       50
035600030715     C                   ENDIF
035700030822     C*
035800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
035900030822     C                   EVAL      posDaDft = 1
036000030822     C                   EVAL      posADft  = 0
036100030822     C                   EVAL      wGiroDft = 0
036200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
036300030822     C                             posDaDft > 0
036400030822     C*
036500030822     C* Gestisco il 1° giro
036600030822     C                   IF        wGiroDft = 0
036700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
036800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
036900030822     C* Incremento il contatore dei "giri"
037000030822     C                   EVAL      wGiroDft = 1
037100030822     C                   ELSE
037200030822     C                   EVAL      posDaDft = posADft
037300030822     C                   ENDIF
037400030822     C* Eseguo lo scan x trovare la fine del campo corrente
037500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
037600030822     C*
037700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
037800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
037900030822     C                   IF        posDaDft > 0 AND
038000030822     C                             posADft  > 0
038100030822     C* CCM
038200030822     C                   IF        %subst(
038300030822     C                             %subst(vlrppt:posDaDft+1:
038400030822     C                             posADft-posDaDft-1):1:3)
038500030822     C                             = 'CCM'
038600030822     C                   EVAL      PiStr=%trim(%subst(
038700030822     C                             %subst(vlrppt:posDaDft+1:
038800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038900030822     C                   EXSR      CHKNUM
039000030822     C                   IF        PiInt=*on
039100040714     C                   Z-ADD     PiVal         VATCCM
039200030822     C                   ENDIF
039300030822     C                   ENDIF
039400030822     C* LNP
039500030822     C                   IF        %subst(
039600030822     C                             %subst(vlrppt:posDaDft+1:
039700030822     C                             posADft-posDaDft-1):1:3)
039800030822     C                             = 'LNP'
039900030822     C                   EVAL      PiStr=%trim(%subst(
040000030822     C                             %subst(vlrppt:posDaDft+1:
040100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040200030822     C                   EXSR      CHKNUM
040300030822     C                   IF        PiInt=*on
040400040714     C                   Z-ADD     PiVal         VATLNP
040500030822     C                   ENDIF
040600030822     C                   ENDIF
040700040714     C* TRC
040800030822     C                   IF        %subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):1:3)
041100040714     C                             = 'TRC'
041200040714     C                   EVAL      VATTRC=%trim(%subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500030822     C                   ENDIF
041600030822     C* ...
041700030822     C                   ENDIF
041800030822     C                   ENDDO
041900020204     C*
042000010330     C                   ENDSR
042100010607     C*----------------------------------------------------*
042200040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
042300010607     C*----------------------------------------------------*
042400040714     C     IMPVAT        BEGSR
042500010607     C*
042600010607     C                   SETOFF                                       3132
042700030515     C*
042800040714     C                   MOVEL     datcor        VATAAS
042900010607     C*
043000030715     C**********
043100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
043200030715     C**********
043300030715     C                   IF        wGiro = *zeros
043400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
043500031201     C     minu:maiu     XLATE     vindta        vindta
043600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
043700040324     C                   DOW       posDa <= %len(%trimr(vindta))
043800030715     C*
043900030715     C* Gestisco il 1° campo
044000030715     C                   IF        i = 1
044100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
044200030715     C                   EVAL      posDa = 1
044300030715     C* Eseguo lo scan x trovare la fine del primo campo
044400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
044500030715     C* A questo punto "estraggo" il campo corrente
044600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
044700030715     C                                                    (posA-posDa))
044800030715     C* X i campi successivi al 1°
044900030715     C                   ELSE
045000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
045100030715     C                   EVAL      posDa = posA + 1
045200030715     C* Eseguo lo scan x trovare la fine del campo corrente
045300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
045400030715     C* Gestisco l'ultimo campo
045500030715     C                   IF        posA = *zeros
045600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
045700030715     C* A questo punto "estraggo" il campo corrente
045800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
045900030715     C                                                    (posA-posDa))
046000030715     C                   LEAVE
046100030715     C                   ELSE
046200030715     C* A questo punto "estraggo" il campo corrente
046300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
046400030715     C                                                    (posA-posDa))
046500030715     C                   ENDIF
046600030715     C                   ENDIF
046700030715     C* Incremento il contatore d campo
046800030715     C                   EVAL      i = i +1
046900030715     C                   ENDDO
047000030715     C*
047100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
047200030715     C* delimitazione testo
047300030715     C                   EVAL      i = 1
047400030715     C                   DOW       i < %elem(SkSplitFLD)
047500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
047600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
047700030715     C                   EVAL      i = i + 1
047800030715     C                   ENDDO
047900030715     C                   EVAL      wGiro = 1
048000030715     C                   ELSE
048100030715     C                   EVAL      wGiro = 2
048200031204     C**********
048300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
048400031204     C* d separatore campo
048500031204     C**********
048600040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
048700040304     C                   EVAL      vindta = ' ' + vindta
048800040304     C                   ENDIF
048900040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
049000040304     C***                EVAL      vindta = %subst(vindta:2)
049100040304     C***                ENDDO
049200031204     C*
049300031204     C                   Z-ADD     *zeros        lunghInput        4 0
049400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
049500040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
049600040304     C                   EVAL      vindta = vindta + ' '
049700040304     C                   ENDIF
049800040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
049900040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
050000040304     C***                EVAL      lunghInput = %len(%trim(vindta))
050100040304     C***                ENDDO
050200030715     C**********
050300030715     C* Effettuo lo split del campo dati d input
050400030715     C**********
050500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
050600040324     C                   DOW       posDa <= %len(%trimr(vindta))
050700030715     C*
050800030715     C* Gestisco il 1° campo
050900030715     C                   IF        i = 1
051000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
051100030715     C                   EVAL      posDa = 1
051200030715     C* Eseguo lo scan x trovare la fine del primo campo
051300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
051400030715     C* A questo punto "estraggo" il campo corrente
051500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
051600030715     C                                                    (posA-posDa))
051700030715     C* X i campi successivi al 1°
051800030715     C                   ELSE
051900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
052000030715     C                   EVAL      posDa = posA + 1
052100031204     C* Verifico che nn vi sia il campo nn valorizzato
052200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
052300031204     C* Se campo nn valorizzato skippo al prossimo
052400031204     C                   EVAL      SkSplitCSV(i) = *blanks
052500031204     C                   EVAL      posA  = posA + 1
052600031204     C                   ELSE
052700030715     C* Eseguo lo scan x trovare la fine del campo corrente
052800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
052900030715     C* Gestisco l'ultimo campo
053000030715     C                   IF        posA = *zeros
053100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
053200030715     C* A questo punto "estraggo" il campo corrente
053300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
053400030715     C                                                    (posA-posDa))
053500030715     C                   LEAVE
053600030715     C                   ELSE
053700030715     C* A questo punto "estraggo" il campo corrente
053800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
053900030715     C                                                    (posA-posDa))
054000030715     C                   ENDIF
054100031204     C                   ENDIF
054200030715     C                   ENDIF
054300030715     C* Incremento il contatore d campo
054400030715     C                   EVAL      i = i +1
054500030715     C                   ENDDO
054600030715     C*
054700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
054800030715     C* delimitazione testo
054900030715     C                   EVAL      i = 1
055000030715     C                   DOW       i < %elem(SkSplitCSV)
055100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
055200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
055300030715     C                   EVAL      i = i + 1
055400030715     C                   ENDDO
055500030715     C*
055600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
055700030715     C                   EVAL      i = 1
055800030715     C                   DOW       i < %elem(SkSplitFLD)
055900030715     C*
056000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
056100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
056200030715     C                             SkSplitCSV(i) <> *blanks
056300030721     C*** ATB
056400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
056500040714     C                   EVAL      VATATB = SkSplitCSV(i)
056600030721     C                   ENDIF
056700040714     C*** TRC
056800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
056900040714     C                   EVAL      VATTRC = SkSplitCSV(i)
057000030715     C                   ENDIF
057100040714     C*** NOT
057200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
057300040714     C                   EVAL      VATNOT = SkSplitCSV(i)
057400030721     C                   ENDIF
057500030721     C*
057600030721     C* Reperisco quindi i campi numerici...
057700030721     C*** NSP
057800040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
057900030721     C                   EVAL      PiStr=SkSplitCSV(i)
058000030721     C                   EXSR      CHKNUM
058100030721     C                   IF        PiInt=*on AND
058200030721     C                             PiVal<=9999999
058300040714     C                   Z-ADD     PiVal         VATNSP
058400030721     C                   ELSE
058500030721     C                   SETON                                        32
058600040714     C                   EVAL      VATNSP = *zeros
058700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
058800040714     C                             + ' ' + 'VATNSP'
058900030721     C                   ENDIF
059000030721     C                   ENDIF
059100030721     C*** CCM
059200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
059300030721     C                   EVAL      PiStr=SkSplitCSV(i)
059400030721     C                   EXSR      CHKNUM
059500030721     C                   IF        PiInt=*on      AND
059600030721     C                             PiVal<=9999999 AND
059700030721     C                             PiVal>*zeros
059800040714     C                   Z-ADD     PiVal         VATCCM
059900030721     C                   ELSE
060000030721     C                   SETON                                        32
060100040714     C                   EVAL      VATCCM = *zeros
060200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
060300040714     C                             + ' ' + 'VATCCM'
060400030721     C                   ENDIF
060500030721     C                   ENDIF
060600030721     C*** LNP
060700040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
060800030721     C                   EVAL      PiStr=SkSplitCSV(i)
060900030721     C                   EXSR      CHKNUM
061000030721     C                   IF        PiInt=*on    AND
061100030721     C                             PiVal<=999   AND
061200030721     C                             PiVal>*zeros
061300040714     C                   Z-ADD     PiVal         VATLNP
061400030721     C                   ELSE
061500030721     C                   SETON                                        32
061600040714     C                   EVAL      VATLNP = *zeros
061700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
061800040714     C                             + ' ' + 'VATLNP'
061900030721     C                   ENDIF
062000030721     C                   ENDIF
062100030721     C*** NRS
062200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
062300030721     C                   EVAL      PiStr=SkSplitCSV(i)
062400030721     C                   EXSR      CHKNUM
062500030721     C                   IF        PiInt=*on AND
062600030721     C                             PiVal<=99
062700040714     C                   Z-ADD     PiVal         VATNRS
062800030721     C                   ELSE
062900030721     C                   SETON                                        32
063000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
063100040714     C                             + ' ' + 'VATNRS'
063200030721     C                   ENDIF
063300030721     C                   ENDIF
063400030715     C*
063500030715     C                   ENDIF
063600030715     C                   EVAL      i = i + 1
063700030715     C                   ENDDO
063800020204     C*
063900030715     C                   ENDIF
064000020204     C*
064100010607     C                   ENDSR
064200010607     C*----------------------------------------------------*
064300040714
064400010330
064500010330
064600010330     C*----------------------------------------------------*
064700010330     C*  CONTROLLO NUMERICITA' CAMPI
064800010330     C*----------------------------------------------------*
064900010330     C     CHKNUM        BEGSR
065000010330     C*
065100010606     C                   IF        PiDecChr = *blanks
065200030715     C                   EVAL      PiDecChr = CharNUM
065300010606     C                   ENDIF
065400010606     C*
065500010606     C                   CALL(e)   'ISNUMERIC'
065600010330     C                   PARM                    PiStr            30
065700010606     C                   PARM                    PiDecChr          1
065800010330     C                   PARM      *ZEROS        PiVal            30 9
065900010330     C                   PARM      '0'           PiInt             1
066000010330     C                   PARM      '0'           PiNum             1
066100010330     C                   IF        %error
066200010606     C                   EVAL      PiNum=*off
066300010330     C                   ENDIF
066400010330     C*
066500010330     C                   ENDSR
066600010330     C***
066700010330
066800010601
066900010601
067000010601
067100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
067200010601     C     repfil        BEGSR
067300010601     C*
067400010601     C                   if        invfil = *zeros and
067500010601     C                             depfil > *zeros and
067600010629     C                             (vinflg = *blanks or
067700010629     C                              vinflg = *zeros)
067800010601     C
067900010601     C                   eval      invfil = depfil
068000010601     C                   endif
068100010601     C*
068200010601     C                   if        depfil <> invfil and
068300010601     C                             invfil > *zeros
068400010601     C                   eval      flgMulti = '1'
068500010601     C                   if        vinflg = *blanks
068600010601     C                   add       1             cntNonEl
068700010601     C                   endif
068800010601     C                   endif
068900010601     C*
069000010601     C                   if        vinflg = '2'
069100010601     C                   eval      flgStato = '2'
069200010601     C                   endif
069300010601     C*
069400010601     C                   ENDSR
069500010601     C***
069600010601
069700010601
069800010601
069900010330
070000010330
070100010330
070200990920      /TITLE Invio dei dati al punto operativo.
070300000613     C     invio         BEGSR
070400990920     C*
070500990920     C                   reset                   dscmz
070600010601     C                   move      invfil        cmzdst
070700990920     C                   eval      cmzfld = vlrfou
070800990920     C                   eval      cmzmbd = vlrhdl
070900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
071000000710     C                   if        prmfir = *blanks
071100040714     C                   eval      cmzfla = 'FIVAT00F'
071200040714     C                   eval      cmzmba = 'FIVAT00F'
071300000710     C                   else
071400000710     C                   eval      cmzfla = prmfir
071500000710     C                   eval      cmzmba = prmfir
071600000710     C                   endif
071700990920     C                   eval      cmznrr = *zeros
071800990920     C                   move      §ctrok        cmznrr
071900021018     C                   eval      cmzlba = vlrfl1
072000990920     C                   call(e)   'TIS711C'
072100990920     C                   parm                    dscmz
072200990921     C                   parm      *blanks       esito
072300990920     C                   if        %error
072400990920     C                             or cmzerr = '1'
072500990921     C                             or esito  = '1'
072600000710     C                   eval      wrkesito = '3'
072700990920     C                   endif
072800990920     C*
072900000613     C                   ENDSR
073000990910
073100010601
073200010601
073300010601
073400010601
073500010601      /TITLE Invio dei dati al punto operativo.
073600010601     C     opeini        BEGSR
073700010601     C*
073800010601     C* Inizializzo flag e contatori operativi
073900010601     C                   movel     '0'           flgGiro           1
074000010601     C                   movel     '0'           flgMulti          1
074100010601     C                   movel     '1'           flgStato          1
074200010615     C                   movel     '0'           flgOk             1
074300010601     C                   z-add     *zeros        cntNonEl         10 0
074400010601     C                   z-add     *zeros        depfil            3 0
074500010601     C                   z-add     *zeros        invfil            3 0
074600010601     C*
074700010601     C                   ENDSR
074800010601     C***
074900050421
075000050421
075100050421
075200050421     C     *pssr         BEGSR
075300050421     C*
075400050421     C                   eval      wrkesito = '2'
075500050421     C*
075600050421     C                   ENDSR     '*CANCL'
075700050421     C***
075800010601
075900010601
076000010601
076100010330
076200010330
076300000613     C     *inzsr        BEGSR
076400990910     C*
076500990910     C     *entry        plist
076600990920     C                   parm                    tivlrds
076700990921     C                   parm      wrkesito      esito
076800000724     C                   parm                    prmlit
076900000710     C                   parm                    prmfir
077000010330     C*
077100010330     C* CALCOLA LA DATA CORRENTE
077200010330     C                   time                    wn14             14 0
077300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
077400010330     C                   z-add     wn8           g08dat
077500010330     C                   z-add     *zeros        g08inv
077600010330     C                   movel     '0'           g08err
077700010330     C                   call      'XSRDA8'
077800010330     C                   parm                    wlbda8
077900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
078000000613     C*
078100000613     C                   ENDSR
078200000613     C***
078300990908
078400030715     Otitvxlp   E            riepilogo         2
078500990915     O                                              'Upload via Internet'
078600990915     O                                           +1 'Traduzione TIVIN00R -'
078700040714     O                                           +1 'FIVATWWR'
078800990915     O                                           +1 'Testate bolle'
078900990915     O          E            riepilogo   2
079000990915     O                       wrkdata
079100990915     O                       wrkora              +1
079200990915     O                       procname            +1
079300990915     O          E            riepilogo   2
079400990915     O                                              'Cliente..................:'
079500040714     O                       VATCCM        z     +1
079600990915     O          E            riepilogo   2
079700990920     O                                              'Riferimento Strategi.....:'
079800990920     O                       vlrhdl              +1
079900990915     O          E            riepilogo   2
080000990915     O                                              'Giusti...................:'
080100971022     O                       §CTROK        2   +  1
080200990915     O          E            riepilogo   2
080300990915     O                                              'Sbagliati e corretti.....:'
080400971022     O                       §CTRMO        2   +  1
080500990915     O          E            riepilogo   2
080600990915     O                                              'Sbagliati e scartati.....:'
080700971022     O                       §CTRNO        2   +  1
080800000613
080900030715     Otitvxlps  E            testdett          2
081000000613     O                                              'Upload via Internet'
081100000613     O                                           +1 'Traduzione TIVIN00R -'
081200040714     O                                           +1 'FIVATWWR'
081300000613     O                                           +1 'Testate bolle'
081400000616     O          E            testdett    3
081500000613     O                                           +2 'N° rec'
081600000613     O                                           +3 'Anteprima contenuto'
081700000616     O          E            rigadett    2
081800000613     O                       rrnum               +2
081900000621     O                       recko               +3
082000000616     O          E            findett     2
082100000613     O                       wrkdata
082200000613     O                       wrkora              +1
082300000613     O                       procname            +1
082400000616     O          E            findett     2
082500000613     O                                              'Cliente..................:'
082600040714     O                       VATCCM        z     +1
082700000616     O          E            findett     2
082800000613     O                                              'Riferimento Strategi.....:'
082900000613     O                       vlrhdl              +1
083000000616     O          E            findett     2
083100000613     O                                              'Giusti...................:'
083200000613     O                       §CTROK        2   +  1
083300000616     O          E            findett     2
083400000613     O                                              'Sbagliati e corretti.....:'
083500000613     O                       §CTRMO        2   +  1
083600000616     O          E            findett     2
083700000613     O                                              'Sbagliati e scartati.....:'
083800000613     O                       §CTRNO        2   +  1
083900000613** CMD - COMANDI CL
084000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
084100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
084200030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
084300010606
084400000613
