000100070629      /TITLE Upload via Internet: traduzione in FIVATWWR. (Cliente Esprit (DPD) - Cod. 1161104)
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
001600040726     D*------------
001700040726     D* SCHIEDE A PROGRAMMA
001800040726     D*------------
001900040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D wrkdata         s               d
003200010330     D wrkora          s               t
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400010330     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700030715     D*------------------
003800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003900030715     D*------------------
004000030715     D SkSplitFLD      S             10    DIM(100)
004100030715     D SkSplitCSV      S            256    DIM(100)
004200030715     D CharCSV         S              1
004300030715     D CharTXT         S              1
004400030715     D CharNUM         S              1
004500030715     D posDa           S              3  0 INZ(*zeros)
004600030715     D posA            S              3  0 INZ(*zeros)
004700030715     D i               s              3  0 INZ(1)
004800030715     D wGiro           s              1  0 INZ(*zeros)
004900030822     D*------------------
005000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005100030822     D*------------------
005200030822     D posDaDft        S              3  0 INZ(*zeros)
005300030822     D posADft         S              3  0 INZ(*zeros)
005400030822     D j               s              3  0 INZ(1)
005500030822     D wGiroDft        s              1  0 INZ(*zeros)
005600010330     D*------------------
005700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800010330     D*------------------
005900010330     D WLBDA8          DS                  INZ
006000010330     D  G08DAT                 1      8  0
006100010330     D  G08INV                 9     16  0
006200010330     D  G08ERR                17     17
006300010330     D  G08TGI                18     22  0
006400031201     D*------------------
006500031201     D* Costanti
006600031201     D*------------------
006700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006900010330
007000010330
007100010330
007200990915     C                   time                    wrkdata
007300990915     C                   time                    wrkora
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700010601     C*
007800010601     C                   exsr      opeini
007900040714     C                   exsr      rwvat
008000010601     C*
008100010601     C                   seton                                        lr
008200010601
008300010601
008400010601
008500010601
008600010601     C*--------------------------------------------------------
008700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008800010601     C*--------------------------------------------------------
008900010601     C     PREELA        BEGSR
009000010601     C*
009100010601     C* SE OCCORRE SPEDIRE IN FILIALE
009200010601     C                   if        invfil <> *zeros and
009300010601     C                             flgGiro = '0'
009400010601     C*
009500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009600010601     C                   eval      flgGiro = '1'
009700000724     C*
009800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000010601     C     invfil        chain     azorg01l
010100000724     C                   if        %found
010200000724     C                   movel(p)  CMD(1)        depcmd
010300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010400000724     C*
010500000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010600011113     C                   Z-ADD     150           LENGH            15 5
010700000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000724     C                   PARM                    depcmd
010900000724     C                   PARM                    LENGH
011000000724     C*
011100000724     C                   endif
011200000616     C*
011300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011400000616     C                   movel(p)  CMD(2)        depcmd
011500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011600000616     C*
011700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    depcmd
012100000616     C                   PARM                    LENGH
012200000613     C*
012300030715     C                   if        not %open(titvxlps)
012400030715     C                   open      titvxlps
012500030715     C                   except    testdett
012600000613     C                   endif
012700010601     C*
012800010601     C                   endif
012900010601     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600010601
013700010601
013800010601
013900010601     C*--------------------------------------------------------
014000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014100010601     C*--------------------------------------------------------
014200010601     C     ENDELA        BEGSR
014300990915     C*
014400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014500000613     C*
014600030715     C                   if        %open(titvxlps)
014700000616     C                   except    findett
014800030715     C                   close     titvxlps
014900000613     C                   endif
015000000616     C*
015100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015200011113     C                   Z-ADD     150           LENGH            15 5
015300010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015400000616     C                   PARM                    CMD(3)
015500000616     C                   PARM                    LENGH
015600000616     C*
015700010601     C                   ENDSR
015800010601     C***
015900010601
016000010601
016100010601
016200010601
016300000613
016400000613     C*--------------------------------------------------------
016500000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016600000613     C*--------------------------------------------------------
016700000621     C     STPR          BEGSR
016800000613     C*
016900030715     C                   if        not %open(titvxlp)
017000030715     C   50              open      titvxlp
017100990915     C                   endif
017200990915     C*
017300030715     C   50              except    riepilogo
017400990915     C*
017500030715     C                   if        %open(titvxlp)
017600030715     C   50              close     titvxlp
017700990914     C                   endif
017800000613     C*
017900000613     C                   ENDSR
018000000613     C***
018100000613
018200010330
018300010601
018400010601
018500010601
018600010330     C*--------------------------------------------------------
018700040714     C* RWVAT   LEGGE TIVIN00R E SCRIVE FIVATWWF              *
018800010330     C*--------------------------------------------------------
018900040714     C     RWVAT         BEGSR
019000010330     C*
019100010330     C                   if        not %open(tivin00r)
019200010330     C                   open      tivin00r
019300010330     C                   endif
019400040714     C                   if        not %open(fivatwwr)
019500040714     C                   open      fivatwwr
019600010330     C                   endif
019700010330     C*
019800010604     C                   clear                   §CTROK
019900010604     C                   clear                   §CTRMO
020000010604     C                   clear                   §CTRNO
020100040726     C*
020200040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020300040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020400040726     C                   exsr      inzvar
020500040726     C                   exsr      impvat
020600010330     C*
020700030822     C                   do        *HIVAL
020800010330     C*
020900030822     C                   read      tivin00r                               70
021000010618     C*
021100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021200010618     C                   if        vinflg = '1'
021300010618     C                   eval      flgOk = '1'
021400010618     C                   endif
021500010618     C*
021600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021700040510     C                   z-add     *zeros        wLenVINDTA        4 0
021800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022200040510     C                   z-add     1             wFlgVINDTA
022300040510     C                   leave
022400040510     C                   endif
022500040510     C                   enddo
022600040510     C*
022700040218     C                   if        vindta > *blanks AND
022800040510     C                             wFlgVINDTA = 1
022900010330     C                   add       1             rrnum
023000010330     C*
023100010601     C                   if        *in70 = *off and
023200010330     C                             (vinflg = *blanks
023300010330     C                              or vinflg = '0'
023400010330     C                              or vinflg = '2')
023500010330     C*
023600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023700010711     C                   if        vinflg = *blanks or vinflg = '0'
023800010711     C                   clear                   vinmsg
023900010711     C                   endif
024000010601     C*
024100010330     C                   exsr      inzvar
024200040714     C                   exsr      impvat
024300010601     C*
024400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024500040714     C                   eval      depfil = VATLNP
024600010601     C                   exsr      repfil
024700010601     C                   if        depfil = invfil
024800021025     C                   if        vlrpoi = 999
024900040714     C                   move(P)   invfil        VATFGS
025000021025     C                   else
025100040714     C                   move(P)   vlrpoi        VATFGS
025200021025     C                   endif
025300010601     C*
025400010601     C                   exsr      PREELA
025500010601     C*
025600010604     C* Ebbene...
025700010604     C*
025800030822     C  N31              add       1             §CTROK            7 0
025900030822     C   32              add       1             §CTRMO            7 0
026000030822     C   31              add       1             §CTRNO            7 0
026100030822     C                   if        wGiro = 2
026200040714     C  N31              write     FIVAT000
026300030822     C                   endif
026400010604     C*
026500010604     C                   if        *in31 = *off and
026600010604     C                             *in32 = *off
026700010604     C                   eval      vinflg = '1'
026800010604     C                   else
026900010604     C                   eval      recko = vindta
027000010604     C                   except    rigadett
027100010604     C                   eval      vinflg = '2'
027200010604     C                   endif
027300010604     C                   endif
027400010601     C*
027500010604     C                   endif
027600010604     C*
027700010330     C                   else
027800010330     C                   eval      vinflg = '1'
027900010330     C                   endif
028000010601     C*
028100010601     C  N70              update    tivin000
028200010330     C*
028300030822     C  N70              enddo
028400010601     C
028500010601     C*
028600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028700010601     C                   if        cntNonEl = *zeros or
028800010601     C                             flgMulti = '0'
028900010330     C* Se non ci sono record con errori ...
029000010601     C                   if        §ctrno = 0 and
029100010604     C                             §ctrmo = 0 and
029200010601     C                             flgStato <> '2'
029300010330     C* ... restituisco esito OK.
029400010330     C                   eval      wrkesito = '0'
029500010330     C                   else
029600010330     C                   if        §ctrok > 0
029700010330     C                   eval      wrkesito = '1'
029800010330     C                   else
029900010615     C                   if        flgOk = '0'
030000010615     C                   eval      wrkesito = '2'
030100010615     C                   else
030200010615     C                   eval      wrkesito = '6'
030300010615     C                   endif
030400010330     C                   endif
030500010330     C                   endif
030600010601     C                   else
030700010601     C                   eval      wrkesito = '9'
030800010601     C                   endif
030900010330     C*
031000010330     C                   if        %open(tivin00r)
031100010330     C                   close     tivin00r
031200010330     C                   endif
031300040714     C                   if        %open(fivatwwr)
031400040714     C                   close     fivatwwr
031500010330     C                   endif
031600010601     C*
031700010601     C                   if        vlrpoi <> 999
031800010601     C                   eval      invfil = vlrpoi
031900010601     C                   endif
032000010330     C*
032100010330     C                   if        §ctrok > 0
032200010601     C                             and invfil > *zeros
032300010330     C                   exsr      invio
032400010330     C                   endif
032500010601     C*
032600010618     C                   if        flgGiro = '1'
032700010601     C                   exsr      endela
032800010618     C                   endif
032900010330     C*
033000010330     C                   ENDSR
033100010330     C***
033200010601
033300010601
033400010601
033500010330     C*----------------------------------------------------*
033600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033700010330     C*----------------------------------------------------*
033800010330     C     INZVAR        BEGSR
033900010330     C*
034000030822     C* Inizializzo variabili di wrk
034100020204     C                   Z-ADD     *zeros        Num5_0            5 0
034200030822     C*
034300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034400040714     C                   CLEAR                   FIVAT000
034500030822     C                   CLEAR                   SkSplitCSV
034600030822     C                   EVAL      i = 1
034700030822     C                   EVAL      posDa = *zeros
034800030822     C                   EVAL      posA  = *zeros
034900030822     C*
035000030822     C* Reimposto i valori di default
035100030822     C                   EXSR      DEFCAM
035200030822     C*
035300010330     C                   ENDSR
035400010330     C*----------------------------------------------------*
035500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035600010330     C*----------------------------------------------------*
035700010330     C     DEFCAM        BEGSR
035800030822     C*
035900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
036000030715     C* e delimitatore testo.
036100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
036400030822     C*
036500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
036600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
036700030715     C                   SETON                                        50
036800030715     C                   ELSE
036900030715     C                   SETOFF                                       50
037000030715     C                   ENDIF
037100030822     C*
037200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
037300030822     C                   EVAL      posDaDft = 1
037400030822     C                   EVAL      posADft  = 0
037500030822     C                   EVAL      wGiroDft = 0
037600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
037700030822     C                             posDaDft > 0
037800030822     C*
037900030822     C* Gestisco il 1° giro
038000030822     C                   IF        wGiroDft = 0
038100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
038200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
038300030822     C* Incremento il contatore dei "giri"
038400030822     C                   EVAL      wGiroDft = 1
038500030822     C                   ELSE
038600030822     C                   EVAL      posDaDft = posADft
038700030822     C                   ENDIF
038800030822     C* Eseguo lo scan x trovare la fine del campo corrente
038900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
039000030822     C*
039100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
039200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
039300030822     C                   IF        posDaDft > 0 AND
039400030822     C                             posADft  > 0
039500030822     C* CCM
039600030822     C                   IF        %subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):1:3)
039900030822     C                             = 'CCM'
040000030822     C                   EVAL      PiStr=%trim(%subst(
040100030822     C                             %subst(vlrppt:posDaDft+1:
040200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040300030822     C                   EXSR      CHKNUM
040400030822     C                   IF        PiInt=*on
040500040714     C                   Z-ADD     PiVal         VATCCM
040600030822     C                   ENDIF
040700030822     C                   ENDIF
040800030822     C* LNP
040900030822     C                   IF        %subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):1:3)
041200030822     C                             = 'LNP'
041300030822     C                   EVAL      PiStr=%trim(%subst(
041400030822     C                             %subst(vlrppt:posDaDft+1:
041500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041600030822     C                   EXSR      CHKNUM
041700030822     C                   IF        PiInt=*on
041800040714     C                   Z-ADD     PiVal         VATLNP
041900030822     C                   ENDIF
042000030822     C                   ENDIF
042100040714     C* TRC
042200030822     C                   IF        %subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):1:3)
042500040714     C                             = 'TRC'
042600040714     C                   EVAL      VATTRC=%trim(%subst(
042700030822     C                             %subst(vlrppt:posDaDft+1:
042800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042900030822     C                   ENDIF
043000030822     C* ...
043100030822     C                   ENDIF
043200030822     C                   ENDDO
043300020204     C*
043400010330     C                   ENDSR
043500010607     C*----------------------------------------------------*
043600040714     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
043700010607     C*----------------------------------------------------*
043800040714     C     IMPVAT        BEGSR
043900010607     C*
044000010607     C                   SETOFF                                       3132
044100030515     C*
044200040714     C                   MOVEL     datcor        VATAAS
044300010607     C*
044400030715     C**********
044500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
044600030715     C**********
044700030715     C                   IF        wGiro = *zeros
044800040726     C*
044900040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
045000040726     C                   EVAL      depvinDTA = vinDTA
045100040726     C*
045200040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
045300040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
045400040726     C                                      %trim(intcol(3))
045500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
045600031201     C     minu:maiu     XLATE     vindta        vindta
045700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
045800040324     C                   DOW       posDa <= %len(%trimr(vindta))
045900030715     C*
046000030715     C* Gestisco il 1° campo
046100030715     C                   IF        i = 1
046200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046300030715     C                   EVAL      posDa = 1
046400030715     C* Eseguo lo scan x trovare la fine del primo campo
046500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046600030715     C* A questo punto "estraggo" il campo corrente
046700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
046800030715     C                                                    (posA-posDa))
046900030715     C* X i campi successivi al 1°
047000030715     C                   ELSE
047100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047200030715     C                   EVAL      posDa = posA + 1
047300030715     C* Eseguo lo scan x trovare la fine del campo corrente
047400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047500030715     C* Gestisco l'ultimo campo
047600030715     C                   IF        posA = *zeros
047700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
047800030715     C* A questo punto "estraggo" il campo corrente
047900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048000030715     C                                                    (posA-posDa))
048100030715     C                   LEAVE
048200030715     C                   ELSE
048300030715     C* A questo punto "estraggo" il campo corrente
048400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048500030715     C                                                    (posA-posDa))
048600030715     C                   ENDIF
048700030715     C                   ENDIF
048800030715     C* Incremento il contatore d campo
048900030715     C                   EVAL      i = i +1
049000030715     C                   ENDDO
049100030715     C*
049200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049300030715     C* delimitazione testo
049400030715     C                   EVAL      i = 1
049500030715     C                   DOW       i < %elem(SkSplitFLD)
049600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
049700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
049800030715     C                   EVAL      i = i + 1
049900030715     C                   ENDDO
050000030715     C                   EVAL      wGiro = 1
050100040726     C* Ripristino la 1° riga originale
050200040726     C                   EVAL      vinDTA = depvinDTA
050300030715     C                   ELSE
050400030715     C                   EVAL      wGiro = 2
050500031204     C**********
050600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
050700031204     C* d separatore campo
050800031204     C**********
050900040726     C                   EVAL      vindta = %trim(vindta)
051000040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
051100040726     C                   EVAL      vindta = %subst(vindta:2)
051200040726     C                   ENDDO
051300031204     C*
051400031204     C                   Z-ADD     *zeros        lunghInput        4 0
051500040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
051600040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
051700040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
051800040726     C                   EVAL      lunghInput = %len(%trim(vindta))
051900040726     C                   ENDDO
052000030715     C**********
052100030715     C* Effettuo lo split del campo dati d input
052200030715     C**********
052300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
052400040324     C                   DOW       posDa <= %len(%trimr(vindta))
052500030715     C*
052600030715     C* Gestisco il 1° campo
052700030715     C                   IF        i = 1
052800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
052900030715     C                   EVAL      posDa = 1
053000030715     C* Eseguo lo scan x trovare la fine del primo campo
053100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
053200030715     C* A questo punto "estraggo" il campo corrente
053300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
053400030715     C                                                    (posA-posDa))
053500030715     C* X i campi successivi al 1°
053600030715     C                   ELSE
053700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
053800030715     C                   EVAL      posDa = posA + 1
053900031204     C* Verifico che nn vi sia il campo nn valorizzato
054000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
054100031204     C* Se campo nn valorizzato skippo al prossimo
054200031204     C                   EVAL      SkSplitCSV(i) = *blanks
054300031204     C                   EVAL      posA  = posA + 1
054400031204     C                   ELSE
054500030715     C* Eseguo lo scan x trovare la fine del campo corrente
054600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
054700030715     C* Gestisco l'ultimo campo
054800030715     C                   IF        posA = *zeros
054900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
055000030715     C* A questo punto "estraggo" il campo corrente
055100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055200030715     C                                                    (posA-posDa))
055300030715     C                   LEAVE
055400030715     C                   ELSE
055500030715     C* A questo punto "estraggo" il campo corrente
055600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055700030715     C                                                    (posA-posDa))
055800030715     C                   ENDIF
055900031204     C                   ENDIF
056000030715     C                   ENDIF
056100030715     C* Incremento il contatore d campo
056200030715     C                   EVAL      i = i +1
056300030715     C                   ENDDO
056400030715     C*
056500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
056600030715     C* delimitazione testo
056700030715     C                   EVAL      i = 1
056800030715     C                   DOW       i < %elem(SkSplitCSV)
056900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
057000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
057100030715     C                   EVAL      i = i + 1
057200030715     C                   ENDDO
057300030715     C*
057400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
057500030715     C                   EVAL      i = 1
057600030715     C                   DOW       i < %elem(SkSplitFLD)
057700030715     C*
057800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
057900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
058000030715     C                             SkSplitCSV(i) <> *blanks
058100030721     C*** ATB
058200040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
058300040714     C                   EVAL      VATATB = SkSplitCSV(i)
058400030721     C                   ENDIF
058500040714     C*** TRC
058600040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
058700040714     C                   EVAL      VATTRC = SkSplitCSV(i)
058800030715     C                   ENDIF
058900040714     C*** NOT
059000040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
059100040714     C                   EVAL      VATNOT = SkSplitCSV(i)
059200030721     C                   ENDIF
059300030721     C*
059400030721     C* Reperisco quindi i campi numerici...
059500030721     C*** NSP
059600040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNSP'
059700030721     C                   EVAL      PiStr=SkSplitCSV(i)
059800030721     C                   EXSR      CHKNUM
059900040726     C                   IF        PiInt=*on
060000040726     C                   Z-ADD     PiVal         VATNSP
060600030721     C                   ELSE
060700030721     C                   SETON                                        32
060800040714     C                   EVAL      VATNSP = *zeros
060900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
061000040714     C                             + ' ' + 'VATNSP'
061100030721     C                   ENDIF
061200030721     C                   ENDIF
061300030721     C*** CCM
061400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
061500030721     C                   EVAL      PiStr=SkSplitCSV(i)
061600030721     C                   EXSR      CHKNUM
061700030721     C                   IF        PiInt=*on      AND
061800030721     C                             PiVal<=9999999 AND
061900030721     C                             PiVal>*zeros
062000040714     C                   Z-ADD     PiVal         VATCCM
062100030721     C                   ELSE
062200030721     C                   SETON                                        32
062300040714     C                   EVAL      VATCCM = *zeros
062400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
062500040714     C                             + ' ' + 'VATCCM'
062600030721     C                   ENDIF
062700030721     C                   ENDIF
062800030721     C*** LNP
062900040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
063000030721     C                   EVAL      PiStr=SkSplitCSV(i)
063100030721     C                   EXSR      CHKNUM
063200030721     C                   IF        PiInt=*on    AND
063300030721     C                             PiVal<=999   AND
063400030721     C                             PiVal>*zeros
063500040714     C                   Z-ADD     PiVal         VATLNP
063600030721     C                   ELSE
063700030721     C                   SETON                                        32
063800040714     C                   EVAL      VATLNP = *zeros
063900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
064000040714     C                             + ' ' + 'VATLNP'
064100030721     C                   ENDIF
064200030721     C                   ENDIF
064300030721     C*** NRS
064400040714     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
064500030721     C                   EVAL      PiStr=SkSplitCSV(i)
064600030721     C                   EXSR      CHKNUM
064700030721     C                   IF        PiInt=*on AND
064800030721     C                             PiVal<=99
064900040714     C                   Z-ADD     PiVal         VATNRS
065000030721     C                   ELSE
065100030721     C                   SETON                                        32
065200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
065300040714     C                             + ' ' + 'VATNRS'
065400030721     C                   ENDIF
065500030721     C                   ENDIF
065600030715     C*
065700030715     C                   ENDIF
065800030715     C                   EVAL      i = i + 1
065900030715     C                   ENDDO
066000040726     C*
066100040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
066200040726     C                   IF        VATNSP = *zeros
066300040726     C                   SETON                                        31
066400040726     C                   ENDIF
066500040726     C*
066600040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
066700040726     C                   IF        VATLNP = *zeros
066800040726     C                   SETON                                        31
066900040726     C                   ENDIF
067000020204     C*
067100030715     C                   ENDIF
067200020204     C*
067300010607     C                   ENDSR
067400010607     C*----------------------------------------------------*
067500040714
067600010330
067700010330
067800010330     C*----------------------------------------------------*
067900010330     C*  CONTROLLO NUMERICITA' CAMPI
068000010330     C*----------------------------------------------------*
068100010330     C     CHKNUM        BEGSR
068200010330     C*
068300010606     C                   IF        PiDecChr = *blanks
068400030715     C                   EVAL      PiDecChr = CharNUM
068500010606     C                   ENDIF
068600010606     C*
068700010606     C                   CALL(e)   'ISNUMERIC'
068800010330     C                   PARM                    PiStr            30
068900010606     C                   PARM                    PiDecChr          1
069000010330     C                   PARM      *ZEROS        PiVal            30 9
069100010330     C                   PARM      '0'           PiInt             1
069200010330     C                   PARM      '0'           PiNum             1
069300010330     C                   IF        %error
069400010606     C                   EVAL      PiNum=*off
069500010330     C                   ENDIF
069600010330     C*
069700010330     C                   ENDSR
069800010330     C***
069900010330
070000010601
070100010601
070200010601
070300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
070400010601     C     repfil        BEGSR
070500010601     C*
070600010601     C                   if        invfil = *zeros and
070700010601     C                             depfil > *zeros and
070800010629     C                             (vinflg = *blanks or
070900010629     C                              vinflg = *zeros)
071000010601     C
071100010601     C                   eval      invfil = depfil
071200010601     C                   endif
071300010601     C*
071400010601     C                   if        depfil <> invfil and
071500010601     C                             invfil > *zeros
071600010601     C                   eval      flgMulti = '1'
071700010601     C                   if        vinflg = *blanks
071800010601     C                   add       1             cntNonEl
071900010601     C                   endif
072000010601     C                   endif
072100010601     C*
072200010601     C                   if        vinflg = '2'
072300010601     C                   eval      flgStato = '2'
072400010601     C                   endif
072500010601     C*
072600010601     C                   ENDSR
072700010601     C***
072800010601
072900010601
073000010601
073100010330
073200010330
073300010330
073400990920      /TITLE Invio dei dati al punto operativo.
073500000613     C     invio         BEGSR
073600990920     C*
073700990920     C                   reset                   dscmz
073800010601     C                   move      invfil        cmzdst
073900990920     C                   eval      cmzfld = vlrfou
074000990920     C                   eval      cmzmbd = vlrhdl
074100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
074200000710     C                   if        prmfir = *blanks
074300040714     C                   eval      cmzfla = 'FIVAT00F'
074400040714     C                   eval      cmzmba = 'FIVAT00F'
074500000710     C                   else
074600000710     C                   eval      cmzfla = prmfir
074700000710     C                   eval      cmzmba = prmfir
074800000710     C                   endif
074900990920     C                   eval      cmznrr = *zeros
075000990920     C                   move      §ctrok        cmznrr
075100021018     C                   eval      cmzlba = vlrfl1
075200990920     C                   call(e)   'TIS711C'
075300990920     C                   parm                    dscmz
075400990921     C                   parm      *blanks       esito
075500990920     C                   if        %error
075600990920     C                             or cmzerr = '1'
075700990921     C                             or esito  = '1'
075800000710     C                   eval      wrkesito = '3'
075900990920     C                   endif
076000990920     C*
076100000613     C                   ENDSR
076200990910
076300010601
076400010601
076500010601
076600010601
076700010601      /TITLE Invio dei dati al punto operativo.
076800010601     C     opeini        BEGSR
076900010601     C*
077000010601     C* Inizializzo flag e contatori operativi
077100010601     C                   movel     '0'           flgGiro           1
077200010601     C                   movel     '0'           flgMulti          1
077300010601     C                   movel     '1'           flgStato          1
077400010615     C                   movel     '0'           flgOk             1
077500010601     C                   z-add     *zeros        cntNonEl         10 0
077600010601     C                   z-add     *zeros        depfil            3 0
077700010601     C                   z-add     *zeros        invfil            3 0
077800010601     C*
077900010601     C                   ENDSR
078000010601     C***
078100010601
078200010601
078300050421
078400050421
078500050421     C     *pssr         BEGSR
078600050421     C*
078700050421     C                   eval      wrkesito = '2'
078800050421     C*
078900050421     C                   ENDSR     '*CANCL'
079000050421     C***
079100050421
079200010601
079300010330
079400010330
079500000613     C     *inzsr        BEGSR
079600990910     C*
079700990910     C     *entry        plist
079800990920     C                   parm                    tivlrds
079900990921     C                   parm      wrkesito      esito
080000000724     C                   parm                    prmlit
080100000710     C                   parm                    prmfir
080200010330     C*
080300010330     C* CALCOLA LA DATA CORRENTE
080400010330     C                   time                    wn14             14 0
080500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080600010330     C                   z-add     wn8           g08dat
080700010330     C                   z-add     *zeros        g08inv
080800010330     C                   movel     '0'           g08err
080900010330     C                   call      'XSRDA8'
081000010330     C                   parm                    wlbda8
081100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081200000613     C*
081300000613     C                   ENDSR
081400000613     C***
081500990908
081600030715     Otitvxlp   E            riepilogo         2
081700990915     O                                              'Upload via Internet'
081800990915     O                                           +1 'Traduzione TIVIN00R -'
081900040714     O                                           +1 'FIVATWWR'
082000990915     O                                           +1 'Testate bolle'
082100990915     O          E            riepilogo   2
082200990915     O                       wrkdata
082300990915     O                       wrkora              +1
082400990915     O                       procname            +1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Cliente..................:'
082700040714     O                       VATCCM        z     +1
082800990915     O          E            riepilogo   2
082900990920     O                                              'Riferimento Strategi.....:'
083000990920     O                       vlrhdl              +1
083100990915     O          E            riepilogo   2
083200990915     O                                              'Giusti...................:'
083300971022     O                       §CTROK        2   +  1
083400990915     O          E            riepilogo   2
083500990915     O                                              'Sbagliati e corretti.....:'
083600971022     O                       §CTRMO        2   +  1
083700990915     O          E            riepilogo   2
083800990915     O                                              'Sbagliati e scartati.....:'
083900971022     O                       §CTRNO        2   +  1
084000000613
084100030715     Otitvxlps  E            testdett          2
084200000613     O                                              'Upload via Internet'
084300000613     O                                           +1 'Traduzione TIVIN00R -'
084400040714     O                                           +1 'FIVATWWR'
084500000613     O                                           +1 'Testate bolle'
084600000616     O          E            testdett    3
084700000613     O                                           +2 'N° rec'
084800000613     O                                           +3 'Anteprima contenuto'
084900000616     O          E            rigadett    2
085000000613     O                       rrnum               +2
085100000621     O                       recko               +3
085200000616     O          E            findett     2
085300000613     O                       wrkdata
085400000613     O                       wrkora              +1
085500000613     O                       procname            +1
085600000616     O          E            findett     2
085700000613     O                                              'Cliente..................:'
085800040714     O                       VATCCM        z     +1
085900000616     O          E            findett     2
086000000613     O                                              'Riferimento Strategi.....:'
086100000613     O                       vlrhdl              +1
086200000616     O          E            findett     2
086300000613     O                                              'Giusti...................:'
086400000613     O                       §CTROK        2   +  1
086500000616     O          E            findett     2
086600000613     O                                              'Sbagliati e corretti.....:'
086700000613     O                       §CTRMO        2   +  1
086800000616     O          E            findett     2
086900000613     O                                              'Sbagliati e scartati.....:'
087000000613     O                       §CTRNO        2   +  1
087100000613** CMD - COMANDI CL
087200030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
087300030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
087400030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
087500010606
087600000613
087700040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
087800070629VATCCM;VATLNP;FILL1;VATNSP;VATTRC;VATNOT
087900040726
088000040726
