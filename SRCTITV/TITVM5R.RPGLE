000100050505      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente BUCCI - Cod. 0570152)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
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
001900050321     D intcol          S            100    DIM(10) CTDATA PERRCD(1)
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
003700050505     D wProgRcd        s              3  0 INZ(*zeros)
003800030715     D*------------------
003900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000030715     D*------------------
004100030715     D SkSplitFLD      S             10    DIM(100)
004200030715     D SkSplitCSV      S            256    DIM(100)
004300030715     D CharCSV         S              1
004400030715     D CharTXT         S              1
004500030715     D CharNUM         S              1
004600030715     D posDa           S              3  0 INZ(*zeros)
004700030715     D posA            S              3  0 INZ(*zeros)
004800030715     D i               s              3  0 INZ(1)
004900030715     D wGiro           s              1  0 INZ(*zeros)
005000050321     D wOldSep1        s              2    INZ(*blanks)
005100050321     D wOldSep2        s              2    INZ(*blanks)
005200050321     D wNewSep1        s              2    INZ(*blanks)
005300050321     D wNewSep2        s              2    INZ(*blanks)
005400030822     D*------------------
005500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005600030822     D*------------------
005700030822     D posDaDft        S              3  0 INZ(*zeros)
005800030822     D posADft         S              3  0 INZ(*zeros)
005900030822     D j               s              3  0 INZ(1)
006000030822     D wGiroDft        s              1  0 INZ(*zeros)
006100010330     D*------------------
006200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300010330     D*------------------
006400010330     D WLBDA8          DS                  INZ
006500010330     D  G08DAT                 1      8  0
006600010330     D  G08INV                 9     16  0
006700010330     D  G08ERR                17     17
006800010330     D  G08TGI                18     22  0
006900031201     D*------------------
007000031201     D* Costanti
007100031201     D*------------------
007200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007400010330
007500010330
007600010330
007700990915     C                   time                    wrkdata
007800990915     C                   time                    wrkora
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010601     C*
008300010601     C                   exsr      opeini
008400010605     C                   exsr      rwvab
008500010601     C*
008600010601     C                   seton                                        lr
008700010601
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009300010601     C*--------------------------------------------------------
009400010601     C     PREELA        BEGSR
009500010601     C*
009600010601     C* SE OCCORRE SPEDIRE IN FILIALE
009700010601     C                   if        invfil <> *zeros and
009800010601     C                             flgGiro = '0'
009900010601     C*
010000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010100010601     C                   eval      flgGiro = '1'
010200000724     C*
010300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010500010601     C     invfil        chain     azorg01l
010600000724     C                   if        %found
010700000724     C                   movel(p)  CMD(1)        depcmd
010800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010900000724     C*
011000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000724     C                   PARM                    depcmd
011400000724     C                   PARM                    LENGH
011500000724     C*
011600000724     C                   endif
011700000616     C*
011800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011900000616     C                   movel(p)  CMD(2)        depcmd
012000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012100000616     C*
012200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012300011113     C                   Z-ADD     150           LENGH            15 5
012400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012500000616     C                   PARM                    depcmd
012600000616     C                   PARM                    LENGH
012700000613     C*
012800030715     C                   if        not %open(titvxlps)
012900030715     C                   open      titvxlps
013000030715     C                   except    testdett
013100000613     C                   endif
013200010601     C*
013300010601     C                   endif
013400010601     C*
013500010601     C                   ENDSR
013600010601     C***
013700010601
013800010601
013900010601
014000010601
014100010601
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800990915     C*
014900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015000000613     C*
015100030715     C                   if        %open(titvxlps)
015200000616     C                   except    findett
015300030715     C                   close     titvxlps
015400000613     C                   endif
015500000616     C*
015600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015700011113     C                   Z-ADD     150           LENGH            15 5
015800010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015900000616     C                   PARM                    CMD(3)
016000000616     C                   PARM                    LENGH
016100000616     C*
016200010601     C                   ENDSR
016300010601     C***
016400010601
016500010601
016600010601
016700010601
016800000613
016900000613     C*--------------------------------------------------------
017000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017100000613     C*--------------------------------------------------------
017200000621     C     STPR          BEGSR
017300000613     C*
017400030715     C                   if        not %open(titvxlp)
017500030715     C   50              open      titvxlp
017600990915     C                   endif
017700990915     C*
017800030715     C   50              except    riepilogo
017900990915     C*
018000030715     C                   if        %open(titvxlp)
018100030715     C   50              close     titvxlp
018200990914     C                   endif
018300000613     C*
018400000613     C                   ENDSR
018500000613     C***
018600000613
018700010330
018800010601
018900010601
019000010601
019100010330     C*--------------------------------------------------------
019200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019300010330     C*--------------------------------------------------------
019400010605     C     RWVAB         BEGSR
019500010330     C*
019600010330     C                   if        not %open(tivin00r)
019700010330     C                   open      tivin00r
019800010330     C                   endif
019900021025     C                   if        not %open(fivabwwr)
020000021025     C                   open      fivabwwr
020100010330     C                   endif
020200010330     C*
020300010604     C                   clear                   §CTROK
020400010604     C                   clear                   §CTRMO
020500010604     C                   clear                   §CTRNO
020600010330     C*
020700030822     C                   do        *HIVAL
020800010330     C*
020900030822     C                   read      tivin00r                               70
021000050505     C*
021100050505     C* CALCOLA LA DATA CORRENTE
021200050505     C                   time                    wn14             14 0
021300050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
021400050505     C                   movel     wn14          wn6               6 0          *ORA  (6) IN H/M/SS
021500050506     C                   movel     wn6           oracor            4 0          *ORA  (4)
021600050505     C                   z-add     wn8           g08dat
021700050505     C                   z-add     *zeros        g08inv
021800050505     C                   movel     '0'           g08err
021900050505     C                   call      'XSRDA8'
022000050505     C                   parm                    wlbda8
022100050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
022200050505     C                   movel     datcor        datoracor        12 0          *DATA E ORA CORRENTE
022300050505     C                   move      oracor        datoracor
022400050505     C*
022500050505     C* Costruisco il progressivo valido come riferimenti cliente
022600050505     C                   eval      wProgRcd = wProgRcd + 1
022700050505     C                   movel     datoracor     wRiferim         15 0
022800050505     C                   move      wProgRcd      wRiferim
022900010618     C*
023000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023100010618     C                   if        vinflg = '1'
023200010618     C                   eval      flgOk = '1'
023300010618     C                   endif
023400010618     C*
023500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023600040510     C                   z-add     *zeros        wLenVINDTA        4 0
023700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
023800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024100040510     C                   z-add     1             wFlgVINDTA
024200040510     C                   leave
024300040510     C                   endif
024400040510     C                   enddo
024500040510     C*
024600050506     C                   if        vindta <> *blanks AND
024700040510     C                             wFlgVINDTA = 1
024800010330     C                   add       1             rrnum
024900010330     C*
025000010601     C                   if        *in70 = *off and
025100010330     C                             (vinflg = *blanks
025200010330     C                              or vinflg = '0'
025300010330     C                              or vinflg = '2')
025400010330     C*
025500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025600010711     C                   if        vinflg = *blanks or vinflg = '0'
025700010711     C                   clear                   vinmsg
025800010711     C                   endif
025900010601     C*
026000010330     C                   exsr      inzvar
026100010605     C                   exsr      impvab
026200010601     C*
026300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026400010605     C                   eval      depfil = VABLNP
026500010601     C                   exsr      repfil
026600010601     C                   if        depfil = invfil
026700021025     C                   if        vlrpoi = 999
026800030822     C                   move(P)   invfil        VABFGS
026900021025     C                   else
027000030822     C                   move(P)   vlrpoi        VABFGS
027100021025     C                   endif
027200010601     C*
027300010601     C                   exsr      PREELA
027400010601     C*
027500010604     C* Ebbene...
027600010604     C*
027700030822     C  N31              add       1             §CTROK            7 0
027800030822     C   32              add       1             §CTRMO            7 0
027900030822     C   31              add       1             §CTRNO            7 0
028000030822     C                   if        wGiro = 2
028100030822     C  N31              write     FIVAB000
028200030822     C                   endif
028300010604     C*
028400010604     C                   if        *in31 = *off and
028500010604     C                             *in32 = *off
028600010604     C                   eval      vinflg = '1'
028700010604     C                   else
028800010604     C                   eval      recko = vindta
028900010604     C                   except    rigadett
029000010604     C                   eval      vinflg = '2'
029100010604     C                   endif
029200010604     C                   endif
029300010601     C*
029400010604     C                   endif
029500010604     C*
029600010330     C                   else
029700010330     C                   eval      vinflg = '1'
029800010330     C                   endif
029900010601     C*
030000010601     C  N70              update    tivin000
030100010330     C*
030200030822     C  N70              enddo
030300010601     C
030400010601     C*
030500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030600010601     C                   if        cntNonEl = *zeros or
030700010601     C                             flgMulti = '0'
030800010330     C* Se non ci sono record con errori ...
030900010601     C                   if        §ctrno = 0 and
031000010604     C                             §ctrmo = 0 and
031100010601     C                             flgStato <> '2'
031200010330     C* ... restituisco esito OK.
031300010330     C                   eval      wrkesito = '0'
031400010330     C                   else
031500010330     C                   if        §ctrok > 0
031600010330     C                   eval      wrkesito = '1'
031700010330     C                   else
031800010615     C                   if        flgOk = '0'
031900010615     C                   eval      wrkesito = '2'
032000010615     C                   else
032100010615     C                   eval      wrkesito = '6'
032200010615     C                   endif
032300010330     C                   endif
032400010330     C                   endif
032500010601     C                   else
032600010601     C                   eval      wrkesito = '9'
032700010601     C                   endif
032800010330     C*
032900010330     C                   if        %open(tivin00r)
033000010330     C                   close     tivin00r
033100010330     C                   endif
033200021025     C                   if        %open(fivabwwr)
033300021025     C                   close     fivabwwr
033400010330     C                   endif
033500010601     C*
033600010601     C                   if        vlrpoi <> 999
033700010601     C                   eval      invfil = vlrpoi
033800010601     C                   endif
033900010330     C*
034000010330     C                   if        §ctrok > 0
034100010601     C                             and invfil > *zeros
034200010330     C                   exsr      invio
034300010330     C                   endif
034400010601     C*
034500010618     C                   if        flgGiro = '1'
034600010601     C                   exsr      endela
034700010618     C                   endif
034800010330     C*
034900010330     C                   ENDSR
035000010330     C***
035100010601
035200010601
035300010601
035400010330     C*----------------------------------------------------*
035500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035600010330     C*----------------------------------------------------*
035700010330     C     INZVAR        BEGSR
035800010330     C*
035900030822     C* Inizializzo variabili di wrk
036000020204     C                   Z-ADD     *zeros        Num5_0            5 0
036100030822     C*
036200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036300030822     C                   CLEAR                   FIVAB000
036400030822     C                   CLEAR                   SkSplitCSV
036500030822     C                   EVAL      i = 1
036600030822     C                   EVAL      posDa = *zeros
036700030822     C                   EVAL      posA  = *zeros
036800030822     C*
036900030822     C* Reimposto i valori di default
037000030822     C                   EXSR      DEFCAM
037100030822     C*
037200010330     C                   ENDSR
037300010330     C*----------------------------------------------------*
037400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037500010330     C*----------------------------------------------------*
037600010330     C     DEFCAM        BEGSR
037700030822     C*
037800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037900030715     C* e delimitatore testo.
038000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038300050321     C*
038400050321     C* Compongo la sequenza separatore testo + separatore campo
038500050321     C                   EVAL      wOldSep1 = CharTXT + CharCSV
038600050321     C                   EVAL      wOldSep2 = CharCSV + CharTXT
038700050321     C                   EVAL      wNewSep1 = CharTXT + ';'
038800050321     C                   EVAL      wNewSep2 = ';' + CharTXT
038900030822     C*
039000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
039100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
039200030715     C                   SETON                                        50
039300030715     C                   ELSE
039400030715     C                   SETOFF                                       50
039500030715     C                   ENDIF
039600030822     C*
039700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039800030822     C                   EVAL      posDaDft = 1
039900030822     C                   EVAL      posADft  = 0
040000030822     C                   EVAL      wGiroDft = 0
040100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
040200030822     C                             posDaDft > 0
040300030822     C*
040400030822     C* Gestisco il 1° giro
040500030822     C                   IF        wGiroDft = 0
040600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040800030822     C* Incremento il contatore dei "giri"
040900030822     C                   EVAL      wGiroDft = 1
041000030822     C                   ELSE
041100030822     C                   EVAL      posDaDft = posADft
041200030822     C                   ENDIF
041300030822     C* Eseguo lo scan x trovare la fine del campo corrente
041400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
041500030822     C*
041600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041800030822     C                   IF        posDaDft > 0 AND
041900030822     C                             posADft  > 0
042000030822     C* NCL
042100030822     C                   IF        %subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):1:3)
042400030822     C                             = 'NCL'
042500030822     C                   EVAL      PiStr=%trim(%subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800030822     C                   EXSR      CHKNUM
042900030822     C                   IF        PiInt=*on
043000030822     C                   Z-ADD     PiVal         VABNCL
043100030822     C                   ENDIF
043200030822     C                   ENDIF
043300030822     C* CCM
043400030822     C                   IF        %subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):1:3)
043700030822     C                             = 'CCM'
043800030822     C                   EVAL      PiStr=%trim(%subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044100030822     C                   EXSR      CHKNUM
044200030822     C                   IF        PiInt=*on
044300030822     C                   Z-ADD     PiVal         VABCCM
044400030822     C                   ENDIF
044500030822     C                   ENDIF
044600030822     C* LNP
044700030822     C                   IF        %subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):1:3)
045000030822     C                             = 'LNP'
045100030822     C                   EVAL      PiStr=%trim(%subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400030822     C                   EXSR      CHKNUM
045500030822     C                   IF        PiInt=*on
045600030822     C                   Z-ADD     PiVal         VABLNP
045700030822     C                   ENDIF
045800030822     C                   ENDIF
045900030822     C* NRS
046000030822     C                   IF        %subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):1:3)
046300030822     C                             = 'NRS'
046400030822     C                   EVAL      PiStr=%trim(%subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700030822     C                   EXSR      CHKNUM
046800030822     C                   IF        PiInt=*on
046900030822     C                   Z-ADD     PiVal         VABNRS
047000030822     C                   ENDIF
047100030822     C                   ENDIF
047200030822     C* CTR
047300030822     C                   IF        %subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):1:3)
047600030822     C                             = 'CTR'
047700030822     C                   EVAL      PiStr=%trim(%subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048000030822     C                   EXSR      CHKNUM
048100030822     C                   IF        PiInt=*on
048200030822     C                   Z-ADD     PiVal         VABCTR
048300030822     C                   ENDIF
048400030822     C                   ENDIF
048500030822     C* PKB
048600030822     C                   IF        %subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):1:3)
048900030822     C                             = 'PKB'
049000030822     C                   EVAL      PiStr=%trim(%subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300030822     C                   EXSR      CHKNUM
049400030822     C                   IF        PiNum=*on
049500030822     C                   Z-ADD     PiVal         VABPKB
049600030822     C                   ENDIF
049700030822     C                   ENDIF
049800030822     C* VLB
049900030822     C                   IF        %subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):1:3)
050200030822     C                             = 'VLB'
050300030822     C                   EVAL      PiStr=%trim(%subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600030822     C                   EXSR      CHKNUM
050700030822     C                   IF        PiNum=*on
050800030822     C                   Z-ADD     PiVal         VABVLB
050900030822     C                   ENDIF
051000030822     C                   ENDIF
051100030822     C* QFT
051200030822     C                   IF        %subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):1:3)
051500030822     C                             = 'QFT'
051600030822     C                   EVAL      PiStr=%trim(%subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051900030822     C                   EXSR      CHKNUM
052000030822     C                   IF        PiNum=*on
052100030822     C                   Z-ADD     PiVal         VABQFT
052200030822     C                   ENDIF
052300030822     C                   ENDIF
052400030822     C* CBO
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'CBO'
052900030822     C                   EVAL      VABCBO=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   ENDIF
053300030822     C* TSP
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'TSP'
053800030822     C                   EVAL      VABTSP=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   ENDIF
054200030822     C* VAS
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'VAS'
054700030822     C                   EVAL      VABVAS=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   ENDIF
055100030822     C* VCA
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'VCA'
055600030822     C                   EVAL      VABVCA=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   ENDIF
056000030822     C* GCA
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'GCA'
056500030822     C                   EVAL      VABGCA=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   ENDIF
056900030822     C* CTM
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'CTM'
057400030822     C                   EVAL      VABCTM=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   ENDIF
057800030822     C* FFD
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'FFD'
058300030822     C                   EVAL      VABFFD=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* VAD
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'VAD'
059200030822     C                   EVAL      VABVAD=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600030822     C* GMA
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'GMA'
060100030822     C                   EVAL      VABGMA=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* GGA
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'GGA'
061000030822     C                   EVAL      VABGGA=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* GVA
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'GVA'
061900030822     C                   EVAL      VABGVA=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* TC1
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'TC1'
062800030822     C                   EVAL      VABTC1=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* TC2
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'TC2'
063700030822     C                   EVAL      VABTC2=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   ENDIF
064100030822     C* ...
064200030822     C                   ENDIF
064300030822     C                   ENDDO
064400020204     C*
064500010330     C                   ENDSR
064600010607     C*----------------------------------------------------*
064700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064800010607     C*----------------------------------------------------*
064900010607     C     IMPVAB        BEGSR
065000010607     C*
065100010607     C                   SETOFF                                       3132
065200030515     C*
065300010607     C                   MOVEL     datcor        VABAAS
065400010607     C                   MOVE      datcor        VABMGS
065500050321     C*
065600050321     C* Dato che c'è 1 conflitto tra separatore campo e carattere ',' nell'indirizzo dest.
065700050321     C* => sostituisco quello campi
065800050321     C                   EVAL      CharCSV = ';'
065900050321     C*
066000050321     C* 1° Giro x fine separatore testo + separatore campo (",)
066100050321     C                   Z-ADD     1             wj                5 0
066200050321     C                   DOW       wj <= %len(vindta) - %len(wOldSep1)
066300050321     C                   IF        %subst(vindta:wj:2) = wOldSep1
066400050321     C                   EVAL      %subst(vindta:wj:2) = wNewSep1
066500050321     C                   ENDIF
066600050321     C                   ADD       1             wj
066700050321     C                   ENDDO
066800050321     C*
066900050321     C* 2° Giro x separatore campo + inizio separatore testo (,")
067000050321     C                   Z-ADD     1             wj                5 0
067100050321     C                   DOW       wj <= %len(vindta) - %len(wOldSep2)
067200050321     C                   IF        %subst(vindta:wj:2) = wOldSep2
067300050321     C                   EVAL      %subst(vindta:wj:2) = wNewSep2
067400050321     C                   ENDIF
067500050321     C                   ADD       1             wj
067600050321     C                   ENDDO
067700050321     C*
067800050321     C* 3° Giro x eliminare il separatore testo
067900050321     C                   Z-ADD     1             wj                5 0
068000050321     C                   DOW       wj <= %len(vindta) - 1
068100050321     C                   IF        %subst(vindta:wj:1) = CharTXT
068200050321     C                   EVAL      %subst(vindta:wj:1) = *blanks
068300050321     C                   ENDIF
068400050321     C                   ADD       1             wj
068500050321     C                   ENDDO
068600010607     C*
068700030715     C**********
068800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
068900030715     C**********
069000030715     C                   IF        wGiro = *zeros
069100040726     C*
069200040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
069300040726     C                   EVAL      depvinDTA = vinDTA
069400040726     C*
069500040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
069600040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
069700050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
069800050321     C                                      %trim(intcol(5))+%trim(intcol(6))+
069900050321     C                                      %trim(intcol(7))+%trim(intcol(8))+
070000050321     C                                      %trim(intcol(9))+%trim(intcol(10))
070100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070200031201     C     minu:maiu     XLATE     vindta        vindta
070300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070400040324     C                   DOW       posDa <= %len(%trimr(vindta))
070500030715     C*
070600030715     C* Gestisco il 1° campo
070700030715     C                   IF        i = 1
070800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070900030715     C                   EVAL      posDa = 1
071000030715     C* Eseguo lo scan x trovare la fine del primo campo
071100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071200030715     C* A questo punto "estraggo" il campo corrente
071300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071400030715     C                                                    (posA-posDa))
071500030715     C* X i campi successivi al 1°
071600030715     C                   ELSE
071700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071800030715     C                   EVAL      posDa = posA + 1
071900030715     C* Eseguo lo scan x trovare la fine del campo corrente
072000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072100030715     C* Gestisco l'ultimo campo
072200030715     C                   IF        posA = *zeros
072300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
072400030715     C* A questo punto "estraggo" il campo corrente
072500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072600030715     C                                                    (posA-posDa))
072700030715     C                   LEAVE
072800030715     C                   ELSE
072900030715     C* A questo punto "estraggo" il campo corrente
073000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073100030715     C                                                    (posA-posDa))
073200030715     C                   ENDIF
073300030715     C                   ENDIF
073400030715     C* Incremento il contatore d campo
073500030715     C                   EVAL      i = i +1
073600030715     C                   ENDDO
073700030715     C*
073800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073900030715     C* delimitazione testo
074000030715     C                   EVAL      i = 1
074100030715     C                   DOW       i < %elem(SkSplitFLD)
074200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
074300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
074400030715     C                   EVAL      i = i + 1
074500030715     C                   ENDDO
074600030715     C                   EVAL      wGiro = 1
074700040726     C* Ripristino la 1° riga originale
074800040726     C                   EVAL      vinDTA = depvinDTA
074900030715     C                   ELSE
075000030715     C                   EVAL      wGiro = 2
075100031204     C**********
075200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
075300031204     C* d separatore campo
075400031204     C**********
075500040726     C                   EVAL      vindta = %trim(vindta)
075600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
075700040726     C                   EVAL      vindta = %subst(vindta:2)
075800040726     C                   ENDDO
075900031204     C*
076000031204     C                   Z-ADD     *zeros        lunghInput        4 0
076100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
076200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
076300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
076400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
076500040726     C                   ENDDO
076600030715     C**********
076700030715     C* Effettuo lo split del campo dati d input
076800030715     C**********
076900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077000040324     C                   DOW       posDa <= %len(%trimr(vindta))
077100030715     C*
077200030715     C* Gestisco il 1° campo
077300030715     C                   IF        i = 1
077400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077500030715     C                   EVAL      posDa = 1
077600030715     C* Eseguo lo scan x trovare la fine del primo campo
077700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077800030715     C* A questo punto "estraggo" il campo corrente
077900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078000030715     C                                                    (posA-posDa))
078100030715     C* X i campi successivi al 1°
078200030715     C                   ELSE
078300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078400030715     C                   EVAL      posDa = posA + 1
078500031204     C* Verifico che nn vi sia il campo nn valorizzato
078600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078700031204     C* Se campo nn valorizzato skippo al prossimo
078800031204     C                   EVAL      SkSplitCSV(i) = *blanks
078900031204     C                   EVAL      posA  = posA + 1
079000031204     C                   ELSE
079100030715     C* Eseguo lo scan x trovare la fine del campo corrente
079200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079300030715     C* Gestisco l'ultimo campo
079400030715     C                   IF        posA = *zeros
079500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C                   LEAVE
080000030715     C                   ELSE
080100030715     C* A questo punto "estraggo" il campo corrente
080200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080300030715     C                                                    (posA-posDa))
080400030715     C                   ENDIF
080500031204     C                   ENDIF
080600030715     C                   ENDIF
080700030715     C* Incremento il contatore d campo
080800030715     C                   EVAL      i = i +1
080900030715     C                   ENDDO
081000030715     C*
081100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081200030715     C* delimitazione testo
081300030715     C                   EVAL      i = 1
081400030715     C                   DOW       i < %elem(SkSplitCSV)
081500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081700030715     C                   EVAL      i = i + 1
081800030715     C                   ENDDO
081900030715     C*
082000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082100030715     C                   EVAL      i = 1
082200030715     C                   DOW       i < %elem(SkSplitFLD)
082300030715     C*
082400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082600030715     C                             SkSplitCSV(i) <> *blanks
082700030721     C*** ATB
082800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
082900030721     C                   EVAL      VABATB = SkSplitCSV(i)
083000030721     C                   ENDIF
083100030715     C*** RSD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
083300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
083400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083600030715     C* ==
083700030715     C                   ENDIF
083800030721     C*** RD2
083900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084100030721     C                   ENDIF
084200030715     C*** IND
084300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
084400030715     C                   EVAL      VABIND = SkSplitCSV(i)
084500030715     C                   ENDIF
084600030715     C*** LOD
084700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
084900030715     C                   ENDIF
085000030715     C*** PRD
085100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
085300030715     C                   ENDIF
085400030721     C*** NZD
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085700030721     C                   ENDIF
085800030715     C*** RMA
085900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086100030715     C                   ENDIF
086200030715     C*** NOT
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
086400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** NT2
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030715     C*** VCA
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030715     C*** VAS
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030715     C*** FFD
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030715     C*** CBO
088300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
088400030715     C                   EVAL      VABCBO = SkSplitCSV(i)
088500030715     C                   ENDIF
088600030715     C
088700030721     C*** GC1
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GC2
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TSP
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** NAS
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** TIC
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** GCA
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
090900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** XCO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** CTM
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** TCR
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** CTS
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** FTM
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
092900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** VAD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** GMA
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GGA
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** GVA
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** TC1
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
094900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TC2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** SCL
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** RMO
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** NMO
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*
096800030721     C* Reperisco quindi i campi numerici...
096900030721     C*** CAD
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
097100030721     C                   EVAL      PiStr=SkSplitCSV(i)
097200030721     C                   EXSR      CHKNUM
097300030721     C                   IF        PiInt=*on
097400030721     C                   Z-ADD     PiVal         Num5_0
097500030721     C                   MOVEL(p)  Num5_0        VABCAD
097600030721     C                   ELSE
097700030721     C                   SETON                                        32
097800030721     C                   EVAL      VABCAD = *zeros
097900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098000030721     C                             + ' ' + 'VABCAD'
098100030721     C                   ENDIF
098200030721     C                   ENDIF
098300030721     C*** RMN
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
098500030721     C                   EVAL      PiStr=SkSplitCSV(i)
098600030721     C                   EXSR      CHKNUM
098700030721     C                   IF        PiInt=*on
098800030721     C                   Z-ADD     PiVal         VABRMN
098900030721     C                   ELSE
099000030721     C                   SETON                                        32
099100030721     C                   EVAL      VABRMN = 1
099200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099300030721     C                             + ' ' + 'VABRMN'
099400030721     C                   ENDIF
099500030721     C                   ENDIF
099600030721     C*** NSP
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
099800030721     C                   EVAL      PiStr=SkSplitCSV(i)
099900030721     C                   EXSR      CHKNUM
100000040726     C                   IF        PiInt=*on
100100040726     C                   Z-ADD     PiVal         VABNSP
100200040802     C*                  IF        PiVal>9999999
100300040802     C*                  SETON                                        32
100400040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
100500040802     C*                            + ' ' + 'VABNSP'
100600040802     C*                  ENDIF
100700030721     C                   ELSE
100800030721     C                   SETON                                        32
100900030721     C                   EVAL      VABNSP = *zeros
101000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101100030721     C                             + ' ' + 'VABNSP'
101200030721     C                   ENDIF
101300030721     C                   ENDIF
101400030721     C*** NCL
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
101600030721     C                   EVAL      PiStr=SkSplitCSV(i)
101700030721     C                   EXSR      CHKNUM
101800030721     C                   IF        PiInt=*on
101900030721     C                   Z-ADD     PiVal         VABNCL
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      VABNCL = *zeros
102300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102400030721     C                             + ' ' + 'VABNCL'
102500030721     C                   ENDIF
102600030721     C                   ENDIF
102700030721     C*** PKB
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
102900030721     C                   EVAL      PiStr=SkSplitCSV(i)
103000030721     C                   EXSR      CHKNUM
103100030721     C                   IF        PiNum=*on
103200030721     C                   Z-ADD     PiVal         VABPKB
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      VABPKB = *zeros
103600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103700030721     C                             + ' ' + 'VABPKB'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000050505     C*** CAS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104200050506     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
104300050617     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiNum=*on
104600030721     C                   Z-ADD     PiVal         VABCAS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      VABCAS = *zeros
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100030721     C                             + ' ' + 'VABCAS'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** CCM
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiInt=*on      AND
105900030721     C                             PiVal<=9999999 AND
106000030721     C                             PiVal>*zeros
106100030721     C                   Z-ADD     PiVal         VABCCM
106200030721     C                   ELSE
106300030721     C                   SETON                                        32
106400030721     C                   EVAL      VABCCM = *zeros
106500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106600030721     C                             + ' ' + 'VABCCM'
106700030721     C                   ENDIF
106800030721     C                   ENDIF
106900030721     C*** LNP
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107100030721     C                   EVAL      PiStr=SkSplitCSV(i)
107200030721     C                   EXSR      CHKNUM
107300030721     C                   IF        PiInt=*on    AND
107400030721     C                             PiVal<=999   AND
107500030721     C                             PiVal>*zeros
107600030721     C                   Z-ADD     PiVal         VABLNP
107700030721     C                   ELSE
107800030721     C                   SETON                                        32
107900030721     C                   EVAL      VABLNP = *zeros
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100030721     C                             + ' ' + 'VABLNP'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** NRS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiInt=*on AND
108900030721     C                             PiVal<=99
109000030721     C                   Z-ADD     PiVal         VABNRS
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400030721     C                             + ' ' + 'VABNRS'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** LNA
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on    AND
110200030721     C                             PiVal<=999   AND
110300030721     C                             PiVal>*zeros
110400030721     C                   Z-ADD     PiVal         VABLNA
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABLNA = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABLNA'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** CTR
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on  AND
111700030721     C                             PiVal<=999
111800030721     C                   Z-ADD     PiVal         VABCTR
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABCTR'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** IAS
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiNum=*on
113000030721     C                   Z-ADD     PiVal         VABIAS
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABIAS'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** VLB
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABVLB
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABVLB'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** QFT
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115100030721     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiNum=*on
115400030721     C                   Z-ADD     PiVal         VABQFT
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABQFT'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** NCD
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on      AND
116600030721     C                             PiVal<=9999999
116700030721     C                   Z-ADD     PiVal         VABNCD
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABNCD'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** NCA
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on      AND
117900030721     C                             PiVal<=9999999
118000030721     C                   Z-ADD     PiVal         VABNCA
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABNCA'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** ZNC
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on AND
119200030721     C                             PiVal<=99
119300030721     C                   Z-ADD     PiVal         VABZNC
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABZNC'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** DCR
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on       AND
120500030721     C                             PiVal<=99999999
120600030721     C                   Z-ADD     PiVal         VABDCR
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABDCR'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** HCR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on   AND
121800030721     C                             PiVal<=9999
121900030721     C                   Z-ADD     PiVal         VABHCR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABHCR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** VMD
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABVMD
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABVMD'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** ANT
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on        AND
124300030721     C                             PiVal<=999999999
124400030721     C                   Z-ADD     PiVal         VABANT
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABANT'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** CMO
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on
125600030721     C                   Z-ADD     PiVal         Num5_0
125700030721     C                   MOVEL(p)  Num5_0        VABCMO
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      VABCMO = *zeros
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABCMO'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030715     C*
126600030715     C                   ENDIF
126700030715     C                   EVAL      i = i + 1
126800030715     C                   ENDDO
126900040726     C*
127000040726     C* Considerazioni personalizzate x il traduttore corrente
127100050505     C                   MOVEL     wRiferim      VABRMA
127200050506     C                   MOVE(P)   wRiferim      VABNSP
127300050505     C                   EVAL      VABRMN = wRiferim
127400040726     C*
127500040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127600040726     C                   IF        VABNSP = *zeros
127700040726     C                   SETON                                        31
127800040726     C                   ENDIF
127900040726     C*
128000040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
128100040726     C                   IF        VABLNP = *zeros
128200040726     C                   SETON                                        31
128300040726     C                   ENDIF
128400040726     C*
128500040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
128600040726     C* d valorizzarli sempre entrambi
128700040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
128800040726     C                   Z-ADD     VABNSP        VABRMN
128900040726     C                   ENDIF
129000040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
129100040726     C                   MOVEL     VABRMN        VABRMA
129200040726     C                   ENDIF
129300040726     C*
129400040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
129500040726     C                   IF        VABCAS > *zeros
129600040726     C                   IF        VABVCA = *blanks
129700040726     C                   EVAL      VABVCA = 'EUR'
129800040726     C                   ENDIF
129900040726     C                   IF        VABCBO = '1'
130000040726     C                   EVAL      VABCBO = '4'
130100040726     C                   ENDIF
130200040726     C                   IF        VABCBO = '2'
130300040726     C                   EVAL      VABCBO = '6'
130400040726     C                   ENDIF
130500040726     C                   ENDIF
130600020204     C*
130700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
130800020204     C                   EXSR      CHKIMPDIV
130900030715     C                   ENDIF
131000020204     C*
131100010607     C                   ENDSR
131200010607     C*----------------------------------------------------*
131300010601
131400020204
131500020204     C*----------------------------------------------------*
131600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
131700020204     C*----------------------------------------------------*
131800020204     C     CHKIMPDIV     BEGSR
131900020204     C*
132000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
132100020204     C                   Z-ADD     *zeros        wrkDec            9 9
132200020204     C*
132300020204     C* Come prima cosa effettuo considerazioni sulla divisa
132400020204     C                   IF        vabIAS > *zeros
132500020204     C                   IF        vabVAS <> 'EUR'
132600020204     C                   EVAL      vabVAS =  'ITL'
132700020204     C                   ENDIF
132800020204     C                   ENDIF
132900020204     C*
133000020204     C                   IF        vabCAS > *zeros
133100020204     C                   IF        vabVCA <> 'EUR'
133200020204     C                   EVAL      vabVCA =  'ITL'
133300020204     C                   ENDIF
133400020204     C                   ENDIF
133500020204     C*
133600020204     C                   IF        vabVMD > *zeros
133700020204     C                   IF        vabVAD <> 'EUR'
133800020204     C                   EVAL      vabVAD =  'ITL'
133900020204     C                   ENDIF
134000020204     C                   ENDIF
134100020204     C*
134200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
134300020204     C                   Z-ADD     vabIAS        wrkDec
134400020204     C                   IF        wrkDec > *zeros
134500020204     C                   IF        vabVAS = 'ITL'
134600020204     C                   EVAL      vabIAS = *zeros
134700020204     C                   ENDIF
134800020204     C                   ENDIF
134900020204     C*
135000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
135100020204     C                   Z-ADD     vabCAS        wrkDec
135200020204     C                   IF        wrkDec > *zeros
135300020204     C                   IF        vabVCA = 'ITL'
135400020204     C                   EVAL      vabCAS = *zeros
135500020204     C                   ENDIF
135600020204     C                   ENDIF
135700020204     C*
135800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
135900020204     C                   Z-ADD     vabVMD        wrkDec
136000020204     C                   IF        wrkDec > *zeros
136100020204     C                   IF        vabVAD = 'ITL'
136200020204     C                   EVAL      vabVMD = *zeros
136300020204     C                   ENDIF
136400020204     C                   ENDIF
136500020204     C*
136600020204     C                   ENDSR
136700020204     C***
136800020204
136900010330
137000010330
137100010330     C*----------------------------------------------------*
137200010330     C*  CONTROLLO NUMERICITA' CAMPI
137300010330     C*----------------------------------------------------*
137400010330     C     CHKNUM        BEGSR
137500010330     C*
137600010606     C                   IF        PiDecChr = *blanks
137700030715     C                   EVAL      PiDecChr = CharNUM
137800010606     C                   ENDIF
137900010606     C*
138000010606     C                   CALL(e)   'ISNUMERIC'
138100010330     C                   PARM                    PiStr            30
138200010606     C                   PARM                    PiDecChr          1
138300010330     C                   PARM      *ZEROS        PiVal            30 9
138400010330     C                   PARM      '0'           PiInt             1
138500010330     C                   PARM      '0'           PiNum             1
138600010330     C                   IF        %error
138700010606     C                   EVAL      PiNum=*off
138800010330     C                   ENDIF
138900010330     C*
139000010330     C                   ENDSR
139100010330     C***
139200010330
139300010601
139400010601
139500010601
139600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
139700010601     C     repfil        BEGSR
139800010601     C*
139900010601     C                   if        invfil = *zeros and
140000010601     C                             depfil > *zeros and
140100010629     C                             (vinflg = *blanks or
140200010629     C                              vinflg = *zeros)
140300010601     C
140400010601     C                   eval      invfil = depfil
140500010601     C                   endif
140600010601     C*
140700010601     C                   if        depfil <> invfil and
140800010601     C                             invfil > *zeros
140900010601     C                   eval      flgMulti = '1'
141000010601     C                   if        vinflg = *blanks
141100010601     C                   add       1             cntNonEl
141200010601     C                   endif
141300010601     C                   endif
141400010601     C*
141500010601     C                   if        vinflg = '2'
141600010601     C                   eval      flgStato = '2'
141700010601     C                   endif
141800010601     C*
141900010601     C                   ENDSR
142000010601     C***
142100010601
142200010601
142300010601
142400010330
142500010330
142600010330
142700990920      /TITLE Invio dei dati al punto operativo.
142800000613     C     invio         BEGSR
142900990920     C*
143000990920     C                   reset                   dscmz
143100010601     C                   move      invfil        cmzdst
143200990920     C                   eval      cmzfld = vlrfou
143300990920     C                   eval      cmzmbd = vlrhdl
143400990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
143500000710     C                   if        prmfir = *blanks
143600021025     C                   eval      cmzfla = 'FIVAB00F'
143700021025     C                   eval      cmzmba = 'FIVAB00F'
143800000710     C                   else
143900000710     C                   eval      cmzfla = prmfir
144000000710     C                   eval      cmzmba = prmfir
144100000710     C                   endif
144200990920     C                   eval      cmznrr = *zeros
144300990920     C                   move      §ctrok        cmznrr
144400021018     C                   eval      cmzlba = vlrfl1
144500990920     C                   call(e)   'TIS711C'
144600990920     C                   parm                    dscmz
144700990921     C                   parm      *blanks       esito
144800990920     C                   if        %error
144900990920     C                             or cmzerr = '1'
145000990921     C                             or esito  = '1'
145100000710     C                   eval      wrkesito = '3'
145200990920     C                   endif
145300990920     C*
145400000613     C                   ENDSR
145500990910
145600010601
145700010601
145800010601
145900010601
146000010601      /TITLE Invio dei dati al punto operativo.
146100010601     C     opeini        BEGSR
146200010601     C*
146300010601     C* Inizializzo flag e contatori operativi
146400010601     C                   movel     '0'           flgGiro           1
146500010601     C                   movel     '0'           flgMulti          1
146600010601     C                   movel     '1'           flgStato          1
146700010615     C                   movel     '0'           flgOk             1
146800010601     C                   z-add     *zeros        cntNonEl         10 0
146900010601     C                   z-add     *zeros        depfil            3 0
147000010601     C                   z-add     *zeros        invfil            3 0
147100010601     C*
147200010601     C                   ENDSR
147300010601     C***
147400010601
147500010601
147600050421
147700050421
147800050421     C     *pssr         BEGSR
147900050421     C*
148000050421     C                   eval      wrkesito = '2'
148100050421     C*
148200050421     C                   ENDSR     '*CANCL'
148300050421     C***
148400050421
148500010601
148600010330
148700010330
148800000613     C     *inzsr        BEGSR
148900990910     C*
149000990910     C     *entry        plist
149100990920     C                   parm                    tivlrds
149200990921     C                   parm      wrkesito      esito
149300000724     C                   parm                    prmlit
149400000710     C                   parm                    prmfir
149500000613     C*
149600000613     C                   ENDSR
149700000613     C***
149800990908
149900030715     Otitvxlp   E            riepilogo         2
150000990915     O                                              'Upload via Internet'
150100990915     O                                           +1 'Traduzione TIVIN00R -'
150200021025     O                                           +1 'FIVABWWR'
150300990915     O                                           +1 'Testate bolle'
150400990915     O          E            riepilogo   2
150500990915     O                       wrkdata
150600990915     O                       wrkora              +1
150700990915     O                       procname            +1
150800990915     O          E            riepilogo   2
150900990915     O                                              'Cliente..................:'
151000010605     O                       VABCCM        z     +1
151100990915     O          E            riepilogo   2
151200990920     O                                              'Riferimento Strategi.....:'
151300990920     O                       vlrhdl              +1
151400990915     O          E            riepilogo   2
151500990915     O                                              'Giusti...................:'
151600971022     O                       §CTROK        2   +  1
151700990915     O          E            riepilogo   2
151800990915     O                                              'Sbagliati e corretti.....:'
151900971022     O                       §CTRMO        2   +  1
152000990915     O          E            riepilogo   2
152100990915     O                                              'Sbagliati e scartati.....:'
152200971022     O                       §CTRNO        2   +  1
152300000613
152400030715     Otitvxlps  E            testdett          2
152500000613     O                                              'Upload via Internet'
152600000613     O                                           +1 'Traduzione TIVIN00R -'
152700021025     O                                           +1 'FIVABWWR'
152800000613     O                                           +1 'Testate bolle'
152900000616     O          E            testdett    3
153000000613     O                                           +2 'N° rec'
153100000613     O                                           +3 'Anteprima contenuto'
153200000616     O          E            rigadett    2
153300000613     O                       rrnum               +2
153400000621     O                       recko               +3
153500000616     O          E            findett     2
153600000613     O                       wrkdata
153700000613     O                       wrkora              +1
153800000613     O                       procname            +1
153900000616     O          E            findett     2
154000000613     O                                              'Cliente..................:'
154100010605     O                       VABCCM        z     +1
154200000616     O          E            findett     2
154300000613     O                                              'Riferimento Strategi.....:'
154400000613     O                       vlrhdl              +1
154500000616     O          E            findett     2
154600000613     O                                              'Giusti...................:'
154700000613     O                       §CTROK        2   +  1
154800000616     O          E            findett     2
154900000613     O                                              'Sbagliati e corretti.....:'
155000000613     O                       §CTRMO        2   +  1
155100000616     O          E            findett     2
155200000613     O                                              'Sbagliati e scartati.....:'
155300000613     O                       §CTRNO        2   +  1
155400000613** CMD - COMANDI CL
155500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
155600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
155700030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
155800010606
155900000613
156000050505** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
156100050505VABCAS;VABRSD;VABNOT;VABIND;VABLOD;VABPRD;VABCAD;VABRD2;VABNT2
156200050505
156300050505
156400050505
156500050505
156600050505
156700050505
156800050505
156900050505
157000050505
