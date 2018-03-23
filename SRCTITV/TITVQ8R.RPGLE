000100060123      /TITLE Upload via Internet: traduzione in EDIVATWR. (Cliente TNT - Fila Olanda Cod. 2100787)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060123     FEDIVATwr  O    E             DISK    usropn
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
003500060123     D SkSplitFLD      S             10    DIM(500)
003600060123     D SkSplitCSV      S            256    DIM(500)
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
007400060123     C                   exsr      rwvat
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
018200060123     C* RWVAT   LEGGE TIVIN00R E SCRIVE EDIVATWF              *
018300010330     C*--------------------------------------------------------
018400060123     C     RWVAT         BEGSR
018500010330     C*
018600010330     C                   if        not %open(tivin00r)
018700010330     C                   open      tivin00r
018800010330     C                   endif
018900060123     C                   if        not %open(edivatwr)
019000060123     C                   open      edivatwr
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
023200060123     C                   exsr      impvat
023300010601     C*
023400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500060123     C                   eval      depfil = VATLNP
023600010601     C                   exsr      repfil
023700010601     C                   if        depfil = invfil
023800021025     C                   if        vlrpoi = 999
023900060123     C                   move(P)   invfil        VATFGS
024000021025     C                   else
024100060123     C                   move(P)   vlrpoi        VATFGS
024200021025     C                   endif
024300010601     C*
024400010601     C                   exsr      PREELA
024500010601     C*
024600010604     C* Ebbene...
024700010604     C*
024800030822     C  N31              add       1             §CTROK            7 0
024900030822     C   32              add       1             §CTRMO            7 0
025000030822     C   31              add       1             §CTRNO            7 0
025100030822     C                   if        wGiro = 2
025200060123     C*
025300060123     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
025400060123     C                   EVAL      VATCMR = 'TNT FASHION'
025500060123     C                   EVAL      VATCNT = 1
025600060123     C*
025700060123     C  N31              write     EDIVAT00
025800030822     C                   endif
025900010604     C*
026000010604     C                   if        *in31 = *off and
026100010604     C                             *in32 = *off
026200010604     C                   eval      vinflg = '1'
026300010604     C                   else
026400010604     C                   eval      recko = vindta
026500010604     C                   except    rigadett
026600010604     C                   eval      vinflg = '2'
026700010604     C                   endif
026800010604     C                   endif
026900010601     C*
027000010604     C                   endif
027100010604     C*
027200010330     C                   else
027300010330     C                   eval      vinflg = '1'
027400010330     C                   endif
027500010601     C*
027600010601     C  N70              update    tivin000
027700010330     C*
027800030822     C  N70              enddo
027900010601     C
028000010601     C*
028100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028200010601     C                   if        cntNonEl = *zeros or
028300010601     C                             flgMulti = '0'
028400010330     C* Se non ci sono record con errori ...
028500010601     C                   if        §ctrno = 0 and
028600010604     C                             §ctrmo = 0 and
028700010601     C                             flgStato <> '2'
028800010330     C* ... restituisco esito OK.
028900010330     C                   eval      wrkesito = '0'
029000010330     C                   else
029100010330     C                   if        §ctrok > 0
029200010330     C                   eval      wrkesito = '1'
029300010330     C                   else
029400010615     C                   if        flgOk = '0'
029500010615     C                   eval      wrkesito = '2'
029600010615     C                   else
029700010615     C                   eval      wrkesito = '6'
029800010615     C                   endif
029900010330     C                   endif
030000010330     C                   endif
030100010601     C                   else
030200010601     C                   eval      wrkesito = '9'
030300010601     C                   endif
030400010330     C*
030500010330     C                   if        %open(tivin00r)
030600010330     C                   close     tivin00r
030700010330     C                   endif
030800060123     C                   if        %open(edivatwr)
030900060123     C                   close     edivatwr
031000010330     C                   endif
031100010601     C*
031200010601     C                   if        vlrpoi <> 999
031300010601     C                   eval      invfil = vlrpoi
031400010601     C                   endif
031500010330     C*
031600010330     C                   if        §ctrok > 0
031700010601     C                             and invfil > *zeros
031800010330     C                   exsr      invio
031900010330     C                   endif
032000010601     C*
032100010618     C                   if        flgGiro = '1'
032200010601     C                   exsr      endela
032300010618     C                   endif
032400010330     C*
032500010330     C                   ENDSR
032600010330     C***
032700010601
032800010601
032900010601
033000010330     C*----------------------------------------------------*
033100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033200010330     C*----------------------------------------------------*
033300010330     C     INZVAR        BEGSR
033400010330     C*
033500030822     C* Inizializzo variabili di wrk
033600020204     C                   Z-ADD     *zeros        Num5_0            5 0
033700030822     C*
033800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
033900060123     C                   CLEAR                   EDIVAT00
034000030822     C                   CLEAR                   SkSplitCSV
034100030822     C                   EVAL      i = 1
034200030822     C                   EVAL      posDa = *zeros
034300030822     C                   EVAL      posA  = *zeros
034400030822     C*
034500030822     C* Reimposto i valori di default
034600030822     C                   EXSR      DEFCAM
034700030822     C*
034800010330     C                   ENDSR
034900010330     C*----------------------------------------------------*
035000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035100010330     C*----------------------------------------------------*
035200010330     C     DEFCAM        BEGSR
035300060123     C*
035400060123     C* Imposto alcuni valori d default relativi al clientre e al traffico realtivo
035500060123     C                   EVAL      VATCCM = 2100787
035600060123     C                   EVAL      VATLNP = 210
035700030822     C*
035800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035900030715     C* e delimitatore testo.
036000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
036300060123     C*
036400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
036500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
036600030715     C                   SETON                                        50
036700030715     C                   ELSE
036800030715     C                   SETOFF                                       50
036900030715     C                   ENDIF
037000030822     C*
037100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
037200030822     C                   EVAL      posDaDft = 1
037300030822     C                   EVAL      posADft  = 0
037400030822     C                   EVAL      wGiroDft = 0
037500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
037600030822     C                             posDaDft > 0
037700030822     C*
037800030822     C* Gestisco il 1° giro
037900030822     C                   IF        wGiroDft = 0
038000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
038100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
038200030822     C* Incremento il contatore dei "giri"
038300030822     C                   EVAL      wGiroDft = 1
038400030822     C                   ELSE
038500030822     C                   EVAL      posDaDft = posADft
038600030822     C                   ENDIF
038700030822     C* Eseguo lo scan x trovare la fine del campo corrente
038800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
038900030822     C*
039000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
039100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
039200030822     C                   IF        posDaDft > 0 AND
039300030822     C                             posADft  > 0
039400060123     C* CCM
039500060123     C                   IF        %subst(
039600060123     C                             %subst(vlrppt:posDaDft+1:
039700060123     C                             posADft-posDaDft-1):1:3)
039800060123     C                             = 'CCM'
039900060123     C                   EVAL      PiStr=%trim(%subst(
040000060123     C                             %subst(vlrppt:posDaDft+1:
040100060123     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040200060123     C                   EXSR      CHKNUM
040300060123     C                   IF        PiInt=*on
040400060123     C                   Z-ADD     PiVal         VATCCM
040500060123     C                   ENDIF
040600060123     C                   ENDIF
040700060123     C* LNP
040800060123     C                   IF        %subst(
040900060123     C                             %subst(vlrppt:posDaDft+1:
041000060123     C                             posADft-posDaDft-1):1:3)
041100060123     C                             = 'LNP'
041200060123     C                   EVAL      PiStr=%trim(%subst(
041300060123     C                             %subst(vlrppt:posDaDft+1:
041400060123     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500060123     C                   EXSR      CHKNUM
041600060123     C                   IF        PiInt=*on
041700060123     C                   Z-ADD     PiVal         VATLNP
041800060123     C                   ENDIF
041900060123     C                   ENDIF
042000060123     C* TRC
042100060123     C                   IF        %subst(
042200060123     C                             %subst(vlrppt:posDaDft+1:
042300060123     C                             posADft-posDaDft-1):1:3)
042400060123     C                             = 'TRC'
042500060123     C                   EVAL      VATTRC=%trim(%subst(
042600060123     C                             %subst(vlrppt:posDaDft+1:
042700060123     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800060123     C                   ENDIF
042900030822     C* ...
043000030822     C                   ENDIF
043100030822     C                   ENDDO
043200020204     C*
043300010330     C                   ENDSR
043400010607     C*----------------------------------------------------*
043500060123     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
043600010607     C*----------------------------------------------------*
043700060123     C     IMPVAT        BEGSR
043800010607     C*
043900010607     C                   SETOFF                                       3132
044000030515     C*
044100060123     C                   MOVEL     datcor        VATAAS
044200050321     C*
044300050321     C* 3° Giro x eliminare il separatore testo
044400050321     C                   Z-ADD     1             wj                5 0
044500050321     C                   DOW       wj <= %len(vindta) - 1
044600050321     C                   IF        %subst(vindta:wj:1) = CharTXT
044700050321     C                   EVAL      %subst(vindta:wj:1) = *blanks
044800050321     C                   ENDIF
044900050321     C                   ADD       1             wj
045000050321     C                   ENDDO
045100010607     C*
045200030715     C**********
045300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
045400030715     C**********
045500030715     C                   IF        wGiro = *zeros
045600060123     C*
045700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
045800031201     C     minu:maiu     XLATE     vindta        vindta
045900060123     C*
046000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
046100040324     C                   DOW       posDa <= %len(%trimr(vindta))
046200030715     C*
046300030715     C* Gestisco il 1° campo
046400030715     C                   IF        i = 1
046500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
046600030715     C                   EVAL      posDa = 1
046700030715     C* Eseguo lo scan x trovare la fine del primo campo
046800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
046900030715     C* A questo punto "estraggo" il campo corrente
047000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
047100030715     C                                                    (posA-posDa))
047200030715     C* X i campi successivi al 1°
047300030715     C                   ELSE
047400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
047500030715     C                   EVAL      posDa = posA + 1
047600030715     C* Eseguo lo scan x trovare la fine del campo corrente
047700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
047800030715     C* Gestisco l'ultimo campo
047900030715     C                   IF        posA = *zeros
048000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
048100030715     C* A questo punto "estraggo" il campo corrente
048200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048300030715     C                                                    (posA-posDa))
048400030715     C                   LEAVE
048500030715     C                   ELSE
048600030715     C* A questo punto "estraggo" il campo corrente
048700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
048800030715     C                                                    (posA-posDa))
048900030715     C                   ENDIF
049000030715     C                   ENDIF
049100030715     C* Incremento il contatore d campo
049200030715     C                   EVAL      i = i +1
049300030715     C                   ENDDO
049400030715     C*
049500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
049600030715     C* delimitazione testo
049700030715     C                   EVAL      i = 1
049800030715     C                   DOW       i < %elem(SkSplitFLD)
049900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
050000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
050100030715     C                   EVAL      i = i + 1
050200030715     C                   ENDDO
050300030715     C                   EVAL      wGiro = 1
050400030715     C                   ELSE
050500030715     C                   EVAL      wGiro = 2
050600031204     C**********
050700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
050800031204     C* d separatore campo
050900031204     C**********
051000040726     C                   EVAL      vindta = %trim(vindta)
051100040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
051200040726     C                   EVAL      vindta = %subst(vindta:2)
051300040726     C                   ENDDO
051400031204     C*
051500031204     C                   Z-ADD     *zeros        lunghInput        4 0
051600040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
051700040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
051800040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
051900040726     C                   EVAL      lunghInput = %len(%trim(vindta))
052000040726     C                   ENDDO
052100030715     C**********
052200030715     C* Effettuo lo split del campo dati d input
052300030715     C**********
052400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
052500040324     C                   DOW       posDa <= %len(%trimr(vindta))
052600030715     C*
052700030715     C* Gestisco il 1° campo
052800030715     C                   IF        i = 1
052900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
053000030715     C                   EVAL      posDa = 1
053100030715     C* Eseguo lo scan x trovare la fine del primo campo
053200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
053300030715     C* A questo punto "estraggo" il campo corrente
053400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
053500030715     C                                                    (posA-posDa))
053600030715     C* X i campi successivi al 1°
053700030715     C                   ELSE
053800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
053900030715     C                   EVAL      posDa = posA + 1
054000031204     C* Verifico che nn vi sia il campo nn valorizzato
054100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
054200031204     C* Se campo nn valorizzato skippo al prossimo
054300031204     C                   EVAL      SkSplitCSV(i) = *blanks
054400031204     C                   EVAL      posA  = posA + 1
054500031204     C                   ELSE
054600030715     C* Eseguo lo scan x trovare la fine del campo corrente
054700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
054800030715     C* Gestisco l'ultimo campo
054900030715     C                   IF        posA = *zeros
055000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
055100030715     C* A questo punto "estraggo" il campo corrente
055200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055300030715     C                                                    (posA-posDa))
055400030715     C                   LEAVE
055500030715     C                   ELSE
055600030715     C* A questo punto "estraggo" il campo corrente
055700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
055800030715     C                                                    (posA-posDa))
055900030715     C                   ENDIF
056000031204     C                   ENDIF
056100030715     C                   ENDIF
056200030715     C* Incremento il contatore d campo
056300030715     C                   EVAL      i = i +1
056400030715     C                   ENDDO
056500030715     C*
056600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
056700030715     C* delimitazione testo
056800030715     C                   EVAL      i = 1
056900030715     C                   DOW       i < %elem(SkSplitCSV)
057000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
057100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
057200030715     C                   EVAL      i = i + 1
057300030715     C                   ENDDO
057400030715     C*
057500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
057600030715     C                   EVAL      i = 1
057700030715     C                   DOW       i < %elem(SkSplitFLD)
057800030715     C*
057900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
058000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
058100060123     C                             SkSplitCSV(i) <> *blanks
058200060123     C*** ATB
058300060123     C                   IF        %trim(SkSplitFLD(i)) = 'VATATB'
058400060123     C                   EVAL      VATATB = SkSplitCSV(i)
058500060123     C                   ENDIF
058600060123     C*** TRC
058700060123     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
058800060123     C                   EVAL      VATTRC = SkSplitCSV(i)
058900060123     C                   ENDIF
059000060123     C*** NOT
059100060123     C                   IF        %trim(SkSplitFLD(i)) = 'O3TRKN'
059200060123     C                   EVAL      VATNOT = SkSplitCSV(i)
059300060123     C                   ENDIF
059400060123     C*
059500060123     C* Reperisco quindi i campi numerici...
059600060123     C*** NSP
059700060123     C                   IF        %trim(SkSplitFLD(i)) = 'O3PCTL'
059800060123     C                   EVAL      PiStr=SkSplitCSV(i)
059900060123     C                   EXSR      CHKNUM
060000060123     C                   IF        PiInt=*on
060100060123     C                   Z-ADD     PiVal         VATNSP
060200060123     C*                  IF        PiVal>9999999
060300060123     C*                  SETON                                        32
060400060123     C*                  EVAL      vinmsg = %trimr(vinmsg)
060500060123     C*                            + ' ' + 'VATNSP'
060600060123     C*                  ENDIF
060700060123     C                   ELSE
060800060123     C                   SETON                                        32
060900060123     C                   EVAL      VATNSP = *zeros
061000060123     C                   EVAL      vinmsg = %trimr(vinmsg)
061100060123     C                             + ' ' + 'VATNSP'
061200060123     C                   ENDIF
061300060123     C                   ENDIF
061400060123     C*** CCM
061500060123     C                   IF        %trim(SkSplitFLD(i)) = 'VATCCM'
061600060123     C                   EVAL      PiStr=SkSplitCSV(i)
061700060123     C                   EXSR      CHKNUM
061800060123     C                   IF        PiInt=*on      AND
061900060123     C                             PiVal<=9999999 AND
062000060123     C                             PiVal>*zeros
062100060123     C                   Z-ADD     PiVal         VATCCM
062200060123     C                   ELSE
062300060123     C                   SETON                                        32
062400060123     C                   EVAL      VATCCM = *zeros
062500060123     C                   EVAL      vinmsg = %trimr(vinmsg)
062600060123     C                             + ' ' + 'VATCCM'
062700060123     C                   ENDIF
062800060123     C                   ENDIF
062900060123     C*** LNP
063000060123     C                   IF        %trim(SkSplitFLD(i)) = 'VATLNP'
063100060123     C                   EVAL      PiStr=SkSplitCSV(i)
063200060123     C                   EXSR      CHKNUM
063300060123     C                   IF        PiInt=*on    AND
063400060123     C                             PiVal<=999   AND
063500060123     C                             PiVal>*zeros
063600060123     C                   Z-ADD     PiVal         VATLNP
063700060123     C                   ELSE
063800060123     C                   SETON                                        32
063900060123     C                   EVAL      VATLNP = *zeros
064000060123     C                   EVAL      vinmsg = %trimr(vinmsg)
064100060123     C                             + ' ' + 'VATLNP'
064200060123     C                   ENDIF
064300060123     C                   ENDIF
064400060123     C*** NRS
064500060123     C                   IF        %trim(SkSplitFLD(i)) = 'VATNRS'
064600060123     C                   EVAL      PiStr=SkSplitCSV(i)
064700060123     C                   EXSR      CHKNUM
064800060123     C                   IF        PiInt=*on AND
064900060123     C                             PiVal<=99
065000060123     C                   Z-ADD     PiVal         VATNRS
065100060123     C                   ELSE
065200060123     C                   SETON                                        32
065300060123     C                   EVAL      vinmsg = %trimr(vinmsg)
065400060123     C                             + ' ' + 'VATNRS'
065500060123     C                   ENDIF
065600060123     C                   ENDIF
065700030715     C*
065800030715     C                   ENDIF
065900030715     C                   EVAL      i = i + 1
066000030715     C                   ENDDO
066100040726     C*
066200040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
066300060123     C                   IF        VATNSP = *zeros
066400040726     C                   SETON                                        31
066500040726     C                   ENDIF
066600040726     C*
066700040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
066800060123     C                   IF        VATLNP = *zeros
066900040726     C                   SETON                                        31
067000040726     C                   ENDIF
067100060123     C*
067200030715     C                   ENDIF
067300020204     C*
067400010607     C                   ENDSR
067500010607     C*----------------------------------------------------*
067600020204
067700010330
067800010330
067900010330     C*----------------------------------------------------*
068000010330     C*  CONTROLLO NUMERICITA' CAMPI
068100010330     C*----------------------------------------------------*
068200010330     C     CHKNUM        BEGSR
068300010330     C*
068400010606     C                   IF        PiDecChr = *blanks
068500030715     C                   EVAL      PiDecChr = CharNUM
068600010606     C                   ENDIF
068700010606     C*
068800010606     C                   CALL(e)   'ISNUMERIC'
068900010330     C                   PARM                    PiStr            30
069000010606     C                   PARM                    PiDecChr          1
069100010330     C                   PARM      *ZEROS        PiVal            30 9
069200010330     C                   PARM      '0'           PiInt             1
069300010330     C                   PARM      '0'           PiNum             1
069400010330     C                   IF        %error
069500010606     C                   EVAL      PiNum=*off
069600010330     C                   ENDIF
069700010330     C*
069800010330     C                   ENDSR
069900010330     C***
070000010330
070100010601
070200010601
070300010601
070400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
070500010601     C     repfil        BEGSR
070600010601     C*
070700010601     C                   if        invfil = *zeros and
070800010601     C                             depfil > *zeros and
070900010629     C                             (vinflg = *blanks or
071000010629     C                              vinflg = *zeros)
071100010601     C
071200010601     C                   eval      invfil = depfil
071300010601     C                   endif
071400010601     C*
071500010601     C                   if        depfil <> invfil and
071600010601     C                             invfil > *zeros
071700010601     C                   eval      flgMulti = '1'
071800010601     C                   if        vinflg = *blanks
071900010601     C                   add       1             cntNonEl
072000010601     C                   endif
072100010601     C                   endif
072200010601     C*
072300010601     C                   if        vinflg = '2'
072400010601     C                   eval      flgStato = '2'
072500010601     C                   endif
072600010601     C*
072700010601     C                   ENDSR
072800010601     C***
072900010601
073000010601
073100010601
073200010330
073300010330
073400010330
073500990920      /TITLE Invio dei dati al punto operativo.
073600000613     C     invio         BEGSR
073700990920     C*
073800990920     C                   reset                   dscmz
073900010601     C                   move      invfil        cmzdst
074000990920     C                   eval      cmzfld = vlrfou
074100990920     C                   eval      cmzmbd = vlrhdl
074200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
074300000710     C                   if        prmfir = *blanks
074400060123     C                   eval      cmzfla = 'EDIVAT0F'
074500060123     C                   eval      cmzmba = 'EDIVAT0F'
074600000710     C                   else
074700000710     C                   eval      cmzfla = prmfir
074800000710     C                   eval      cmzmba = prmfir
074900000710     C                   endif
075000990920     C                   eval      cmznrr = *zeros
075100990920     C                   move      §ctrok        cmznrr
075200021018     C                   eval      cmzlba = vlrfl1
075300990920     C                   call(e)   'TIS711C'
075400990920     C                   parm                    dscmz
075500990921     C                   parm      *blanks       esito
075600990920     C                   if        %error
075700990920     C                             or cmzerr = '1'
075800990921     C                             or esito  = '1'
075900000710     C                   eval      wrkesito = '3'
076000990920     C                   endif
076100990920     C*
076200000613     C                   ENDSR
076300990910
076400010601
076500010601
076600010601
076700010601
076800010601      /TITLE Invio dei dati al punto operativo.
076900010601     C     opeini        BEGSR
077000010601     C*
077100010601     C* Inizializzo flag e contatori operativi
077200010601     C                   movel     '0'           flgGiro           1
077300010601     C                   movel     '0'           flgMulti          1
077400010601     C                   movel     '1'           flgStato          1
077500010615     C                   movel     '0'           flgOk             1
077600010601     C                   z-add     *zeros        cntNonEl         10 0
077700010601     C                   z-add     *zeros        depfil            3 0
077800010601     C                   z-add     *zeros        invfil            3 0
077900010601     C*
078000010601     C                   ENDSR
078100010601     C***
078200010601
078300010601
078400050421
078500050421
078600060123     C     *pssr         BEGSR
078700050421     C*
078800060123     C                   eval      wrkesito = '2'
078900050421     C*
079000060123     C                   ENDSR     '*CANCL'
079100050421     C***
079200050421
079300010601
079400010330
079500010330
079600000613     C     *inzsr        BEGSR
079700990910     C*
079800990910     C     *entry        plist
079900990920     C                   parm                    tivlrds
080000990921     C                   parm      wrkesito      esito
080100000724     C                   parm                    prmlit
080200000710     C                   parm                    prmfir
080300050505     C*
080400050505     C* CALCOLA LA DATA CORRENTE
080500050505     C                   time                    wn14             14 0
080600050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080700060123     C                   movel     wn14          oracor            6 0          *ORA
080800050505     C                   z-add     wn8           g08dat
080900050505     C                   z-add     *zeros        g08inv
081000050505     C                   movel     '0'           g08err
081100050505     C                   call      'XSRDA8'
081200050505     C                   parm                    wlbda8
081300050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
081400000613     C*
081500000613     C                   ENDSR
081600000613     C***
081700990908
081800030715     Otitvxlp   E            riepilogo         2
081900990915     O                                              'Upload via Internet'
082000990915     O                                           +1 'Traduzione TIVIN00R -'
082100060123     O                                           +1 'EDIVATWR'
082200990915     O                                           +1 'Testate bolle'
082300990915     O          E            riepilogo   2
082400990915     O                       wrkdata
082500990915     O                       wrkora              +1
082600990915     O                       procname            +1
082700990915     O          E            riepilogo   2
082800990915     O                                              'Cliente..................:'
082900060123     O                       VATCCM        z     +1
083000990915     O          E            riepilogo   2
083100990920     O                                              'Riferimento Strategi.....:'
083200990920     O                       vlrhdl              +1
083300990915     O          E            riepilogo   2
083400990915     O                                              'Giusti...................:'
083500971022     O                       §CTROK        2   +  1
083600990915     O          E            riepilogo   2
083700990915     O                                              'Sbagliati e corretti.....:'
083800971022     O                       §CTRMO        2   +  1
083900990915     O          E            riepilogo   2
084000990915     O                                              'Sbagliati e scartati.....:'
084100971022     O                       §CTRNO        2   +  1
084200000613
084300030715     Otitvxlps  E            testdett          2
084400000613     O                                              'Upload via Internet'
084500000613     O                                           +1 'Traduzione TIVIN00R -'
084600060123     O                                           +1 'EDIVATWR'
084700000613     O                                           +1 'Testate bolle'
084800000616     O          E            testdett    3
084900000613     O                                           +2 'N° rec'
085000000613     O                                           +3 'Anteprima contenuto'
085100000616     O          E            rigadett    2
085200000613     O                       rrnum               +2
085300000621     O                       recko               +3
085400000616     O          E            findett     2
085500000613     O                       wrkdata
085600000613     O                       wrkora              +1
085700000613     O                       procname            +1
085800000616     O          E            findett     2
085900000613     O                                              'Cliente..................:'
086000060123     O                       VATCCM        z     +1
086100000616     O          E            findett     2
086200000613     O                                              'Riferimento Strategi.....:'
086300000613     O                       vlrhdl              +1
086400000616     O          E            findett     2
086500000613     O                                              'Giusti...................:'
086600000613     O                       §CTROK        2   +  1
086700000616     O          E            findett     2
086800000613     O                                              'Sbagliati e corretti.....:'
086900000613     O                       §CTRMO        2   +  1
087000000616     O          E            findett     2
087100000613     O                                              'Sbagliati e scartati.....:'
087200000613     O                       §CTRNO        2   +  1
087300000613** CMD - COMANDI CL
087400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
087500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
087600030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
087700010606
087800000613
