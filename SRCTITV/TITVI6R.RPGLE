000100050307      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente TEUCO GUZZINI - Cod. 0661217)
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
007900010605     C                   exsr      rwvab
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
018700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018800010330     C*--------------------------------------------------------
018900010605     C     RWVAB         BEGSR
019000010330     C*
019100010330     C                   if        not %open(tivin00r)
019200010330     C                   open      tivin00r
019300010330     C                   endif
019400021025     C                   if        not %open(fivabwwr)
019500021025     C                   open      fivabwwr
019600010330     C                   endif
019700010330     C*
019800010604     C                   clear                   §CTROK
019900010604     C                   clear                   §CTRMO
020000010604     C                   clear                   §CTRNO
020100040726     C*
020200040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020300040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020400040726     C                   exsr      inzvar
020500040726     C                   exsr      impvab
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
024200010605     C                   exsr      impvab
024300010601     C*
024400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024500010605     C                   eval      depfil = VABLNP
024600010601     C                   exsr      repfil
024700010601     C                   if        depfil = invfil
024800021025     C                   if        vlrpoi = 999
024900030822     C                   move(P)   invfil        VABFGS
025000021025     C                   else
025100030822     C                   move(P)   vlrpoi        VABFGS
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
026200030822     C  N31              write     FIVAB000
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
031300021025     C                   if        %open(fivabwwr)
031400021025     C                   close     fivabwwr
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
034400030822     C                   CLEAR                   FIVAB000
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
039500030822     C* NCL
039600030822     C                   IF        %subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):1:3)
039900030822     C                             = 'NCL'
040000030822     C                   EVAL      PiStr=%trim(%subst(
040100030822     C                             %subst(vlrppt:posDaDft+1:
040200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040300030822     C                   EXSR      CHKNUM
040400030822     C                   IF        PiInt=*on
040500030822     C                   Z-ADD     PiVal         VABNCL
040600030822     C                   ENDIF
040700030822     C                   ENDIF
040800030822     C* CCM
040900030822     C                   IF        %subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):1:3)
041200030822     C                             = 'CCM'
041300030822     C                   EVAL      PiStr=%trim(%subst(
041400030822     C                             %subst(vlrppt:posDaDft+1:
041500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041600030822     C                   EXSR      CHKNUM
041700030822     C                   IF        PiInt=*on
041800030822     C                   Z-ADD     PiVal         VABCCM
041900030822     C                   ENDIF
042000030822     C                   ENDIF
042100030822     C* LNP
042200030822     C                   IF        %subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):1:3)
042500030822     C                             = 'LNP'
042600030822     C                   EVAL      PiStr=%trim(%subst(
042700030822     C                             %subst(vlrppt:posDaDft+1:
042800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042900030822     C                   EXSR      CHKNUM
043000030822     C                   IF        PiInt=*on
043100030822     C                   Z-ADD     PiVal         VABLNP
043200030822     C                   ENDIF
043300030822     C                   ENDIF
043400030822     C* NRS
043500030822     C                   IF        %subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):1:3)
043800030822     C                             = 'NRS'
043900030822     C                   EVAL      PiStr=%trim(%subst(
044000030822     C                             %subst(vlrppt:posDaDft+1:
044100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044200030822     C                   EXSR      CHKNUM
044300030822     C                   IF        PiInt=*on
044400030822     C                   Z-ADD     PiVal         VABNRS
044500030822     C                   ENDIF
044600030822     C                   ENDIF
044700030822     C* CTR
044800030822     C                   IF        %subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):1:3)
045100030822     C                             = 'CTR'
045200030822     C                   EVAL      PiStr=%trim(%subst(
045300030822     C                             %subst(vlrppt:posDaDft+1:
045400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045500030822     C                   EXSR      CHKNUM
045600030822     C                   IF        PiInt=*on
045700030822     C                   Z-ADD     PiVal         VABCTR
045800030822     C                   ENDIF
045900030822     C                   ENDIF
046000030822     C* PKB
046100030822     C                   IF        %subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):1:3)
046400030822     C                             = 'PKB'
046500030822     C                   EVAL      PiStr=%trim(%subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046800030822     C                   EXSR      CHKNUM
046900030822     C                   IF        PiNum=*on
047000030822     C                   Z-ADD     PiVal         VABPKB
047100030822     C                   ENDIF
047200030822     C                   ENDIF
047300030822     C* VLB
047400030822     C                   IF        %subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):1:3)
047700030822     C                             = 'VLB'
047800030822     C                   EVAL      PiStr=%trim(%subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100030822     C                   EXSR      CHKNUM
048200030822     C                   IF        PiNum=*on
048300030822     C                   Z-ADD     PiVal         VABVLB
048400030822     C                   ENDIF
048500030822     C                   ENDIF
048600030822     C* QFT
048700030822     C                   IF        %subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):1:3)
049000030822     C                             = 'QFT'
049100030822     C                   EVAL      PiStr=%trim(%subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400030822     C                   EXSR      CHKNUM
049500030822     C                   IF        PiNum=*on
049600030822     C                   Z-ADD     PiVal         VABQFT
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* CBO
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'CBO'
050400030822     C                   EVAL      VABCBO=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   ENDIF
050800030822     C* TSP
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'TSP'
051300030822     C                   EVAL      VABTSP=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   ENDIF
051700030822     C* VAS
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'VAS'
052200030822     C                   EVAL      VABVAS=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   ENDIF
052600030822     C* VCA
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'VCA'
053100030822     C                   EVAL      VABVCA=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   ENDIF
053500030822     C* GCA
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'GCA'
054000030822     C                   EVAL      VABGCA=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   ENDIF
054400030822     C* CTM
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'CTM'
054900030822     C                   EVAL      VABCTM=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   ENDIF
055300030822     C* FFD
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'FFD'
055800030822     C                   EVAL      VABFFD=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   ENDIF
056200030822     C* VAD
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'VAD'
056700030822     C                   EVAL      VABVAD=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   ENDIF
057100030822     C* GMA
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'GMA'
057600030822     C                   EVAL      VABGMA=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* GGA
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'GGA'
058500030822     C                   EVAL      VABGGA=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* GVA
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'GVA'
059400030822     C                   EVAL      VABGVA=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* TC1
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'TC1'
060300030822     C                   EVAL      VABTC1=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* TC2
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'TC2'
061200030822     C                   EVAL      VABTC2=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600030822     C* ...
061700030822     C                   ENDIF
061800030822     C                   ENDDO
061900020204     C*
062000010330     C                   ENDSR
062100010607     C*----------------------------------------------------*
062200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
062300010607     C*----------------------------------------------------*
062400010607     C     IMPVAB        BEGSR
062500010607     C*
062600010607     C                   SETOFF                                       3132
062700030515     C*
062800010607     C                   MOVEL     datcor        VABAAS
062900010607     C                   MOVE      datcor        VABMGS
063000010607     C*
063100030715     C**********
063200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
063300030715     C**********
063400030715     C                   IF        wGiro = *zeros
063500040726     C*
063600040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
063700040726     C                   EVAL      depvinDTA = vinDTA
063800040726     C*
063900040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
064000040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
064100040726     C                                      %trim(intcol(3))
064200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
064300031201     C     minu:maiu     XLATE     vindta        vindta
064400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
064500040324     C                   DOW       posDa <= %len(%trimr(vindta))
064600030715     C*
064700030715     C* Gestisco il 1° campo
064800030715     C                   IF        i = 1
064900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
065000030715     C                   EVAL      posDa = 1
065100030715     C* Eseguo lo scan x trovare la fine del primo campo
065200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065300030715     C* A questo punto "estraggo" il campo corrente
065400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065500030715     C                                                    (posA-posDa))
065600030715     C* X i campi successivi al 1°
065700030715     C                   ELSE
065800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
065900030715     C                   EVAL      posDa = posA + 1
066000030715     C* Eseguo lo scan x trovare la fine del campo corrente
066100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066200030715     C* Gestisco l'ultimo campo
066300030715     C                   IF        posA = *zeros
066400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
066500030715     C* A questo punto "estraggo" il campo corrente
066600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066700030715     C                                                    (posA-posDa))
066800030715     C                   LEAVE
066900030715     C                   ELSE
067000030715     C* A questo punto "estraggo" il campo corrente
067100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067200030715     C                                                    (posA-posDa))
067300030715     C                   ENDIF
067400030715     C                   ENDIF
067500030715     C* Incremento il contatore d campo
067600030715     C                   EVAL      i = i +1
067700030715     C                   ENDDO
067800030715     C*
067900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
068000030715     C* delimitazione testo
068100030715     C                   EVAL      i = 1
068200030715     C                   DOW       i < %elem(SkSplitFLD)
068300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
068400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
068500030715     C                   EVAL      i = i + 1
068600030715     C                   ENDDO
068700030715     C                   EVAL      wGiro = 1
068800040726     C* Ripristino la 1° riga originale
068900040726     C                   EVAL      vinDTA = depvinDTA
069000030715     C                   ELSE
069100030715     C                   EVAL      wGiro = 2
069200031204     C**********
069300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
069400031204     C* d separatore campo
069500031204     C**********
069600040726     C                   EVAL      vindta = %trim(vindta)
069700040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
069800040726     C                   EVAL      vindta = %subst(vindta:2)
069900040726     C                   ENDDO
070000031204     C*
070100031204     C                   Z-ADD     *zeros        lunghInput        4 0
070200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
070300040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
070400040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
070500040726     C                   EVAL      lunghInput = %len(%trim(vindta))
070600040726     C                   ENDDO
070700030715     C**********
070800030715     C* Effettuo lo split del campo dati d input
070900030715     C**********
071000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071100040324     C                   DOW       posDa <= %len(%trimr(vindta))
071200030715     C*
071300030715     C* Gestisco il 1° campo
071400030715     C                   IF        i = 1
071500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071600030715     C                   EVAL      posDa = 1
071700030715     C* Eseguo lo scan x trovare la fine del primo campo
071800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071900030715     C* A questo punto "estraggo" il campo corrente
072000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072100030715     C                                                    (posA-posDa))
072200030715     C* X i campi successivi al 1°
072300030715     C                   ELSE
072400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072500030715     C                   EVAL      posDa = posA + 1
072600031204     C* Verifico che nn vi sia il campo nn valorizzato
072700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
072800031204     C* Se campo nn valorizzato skippo al prossimo
072900031204     C                   EVAL      SkSplitCSV(i) = *blanks
073000031204     C                   EVAL      posA  = posA + 1
073100031204     C                   ELSE
073200030715     C* Eseguo lo scan x trovare la fine del campo corrente
073300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073400030715     C* Gestisco l'ultimo campo
073500030715     C                   IF        posA = *zeros
073600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
073700030715     C* A questo punto "estraggo" il campo corrente
073800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073900030715     C                                                    (posA-posDa))
074000030715     C                   LEAVE
074100030715     C                   ELSE
074200030715     C* A questo punto "estraggo" il campo corrente
074300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074400030715     C                                                    (posA-posDa))
074500030715     C                   ENDIF
074600031204     C                   ENDIF
074700030715     C                   ENDIF
074800030715     C* Incremento il contatore d campo
074900030715     C                   EVAL      i = i +1
075000030715     C                   ENDDO
075100030715     C*
075200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075300030715     C* delimitazione testo
075400030715     C                   EVAL      i = 1
075500030715     C                   DOW       i < %elem(SkSplitCSV)
075600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
075700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
075800030715     C                   EVAL      i = i + 1
075900030715     C                   ENDDO
076000030715     C*
076100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
076200030715     C                   EVAL      i = 1
076300030715     C                   DOW       i < %elem(SkSplitFLD)
076400030715     C*
076500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
076600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
076700030715     C                             SkSplitCSV(i) <> *blanks
076800030721     C*** ATB
076900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
077000030721     C                   EVAL      VABATB = SkSplitCSV(i)
077100030721     C                   ENDIF
077200030715     C*** RSD
077300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
077400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
077500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
077600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
077700030715     C* ==
077800030715     C                   ENDIF
077900030721     C*** RD2
078000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
078100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
078200030721     C                   ENDIF
078300030715     C*** IND
078400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
078500030715     C                   EVAL      VABIND = SkSplitCSV(i)
078600030715     C                   ENDIF
078700030715     C*** LOD
078800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
078900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
079000030715     C                   ENDIF
079100030715     C*** PRD
079200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
079300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
079400030715     C                   ENDIF
079500030721     C*** NZD
079600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
079700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
079800030721     C                   ENDIF
079900030715     C*** RMA
080000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
080100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
080200030715     C                   ENDIF
080300030715     C*** NOT
080400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
080500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
080600030715     C                   ENDIF
080700030715     C*** NT2
080800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
080900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
081000030715     C                   ENDIF
081100030715     C*** VCA
081200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
081300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
081400030715     C                   ENDIF
081500030715     C*** VAS
081600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
081700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
081800030715     C                   ENDIF
081900030715     C*** FFD
082000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
082100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
082200030715     C                   ENDIF
082300030715     C*** CBO
082400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
082500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
082600030715     C                   ENDIF
082700030715     C
082800030721     C*** GC1
082900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
083000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
083100030721     C                   ENDIF
083200030721     C*** GC2
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
083400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030721     C*** TSP
083700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
083800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
083900030721     C                   ENDIF
084000030721     C*** NAS
084100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
084200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
084300030721     C                   ENDIF
084400030721     C*** TIC
084500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
084600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
084700030721     C                   ENDIF
084800030721     C*** GCA
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030721     C*** XCO
085300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
085400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
085500030721     C                   ENDIF
085600030721     C*** CTM
085700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
085800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
085900030721     C                   ENDIF
086000030721     C*** TCR
086100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
086200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
086300030721     C                   ENDIF
086400030721     C*** CTS
086500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
086600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
086700030721     C                   ENDIF
086800030721     C*** FTM
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030721     C*** VAD
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
087400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030721     C*** GMA
087700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
087800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
087900030721     C                   ENDIF
088000030721     C*** GGA
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
088200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** GVA
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
088600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** TC1
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** TC2
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
089400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** SCL
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
089800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** RMO
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
090200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** NMO
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
090600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*
090900030721     C* Reperisco quindi i campi numerici...
091000030721     C*** CAD
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
091200030721     C                   EVAL      PiStr=SkSplitCSV(i)
091300030721     C                   EXSR      CHKNUM
091400030721     C                   IF        PiInt=*on
091500030721     C                   Z-ADD     PiVal         Num5_0
091600030721     C                   MOVEL(p)  Num5_0        VABCAD
091700030721     C                   ELSE
091800030721     C                   SETON                                        32
091900030721     C                   EVAL      VABCAD = *zeros
092000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092100030721     C                             + ' ' + 'VABCAD'
092200030721     C                   ENDIF
092300030721     C                   ENDIF
092400030721     C*** RMN
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
092600030721     C                   EVAL      PiStr=SkSplitCSV(i)
092700030721     C                   EXSR      CHKNUM
092800030721     C                   IF        PiInt=*on
092900030721     C                   Z-ADD     PiVal         VABRMN
093000030721     C                   ELSE
093100030721     C                   SETON                                        32
093200030721     C                   EVAL      VABRMN = 1
093300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093400030721     C                             + ' ' + 'VABRMN'
093500030721     C                   ENDIF
093600030721     C                   ENDIF
093700030721     C*** NSP
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093900030721     C                   EVAL      PiStr=SkSplitCSV(i)
094000030721     C                   EXSR      CHKNUM
094100040726     C                   IF        PiInt=*on
094200040726     C                   Z-ADD     PiVal         VABNSP
094300040802     C*                  IF        PiVal>9999999
094400040802     C*                  SETON                                        32
094500040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
094600040802     C*                            + ' ' + 'VABNSP'
094700040802     C*                  ENDIF
094800030721     C                   ELSE
094900030721     C                   SETON                                        32
095000030721     C                   EVAL      VABNSP = *zeros
095100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095200030721     C                             + ' ' + 'VABNSP'
095300030721     C                   ENDIF
095400030721     C                   ENDIF
095500030721     C*** NCL
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
095700030721     C                   EVAL      PiStr=SkSplitCSV(i)
095800030721     C                   EXSR      CHKNUM
095900030721     C                   IF        PiInt=*on
096000030721     C                   Z-ADD     PiVal         VABNCL
096100030721     C                   ELSE
096200030721     C                   SETON                                        32
096300030721     C                   EVAL      VABNCL = *zeros
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABNCL'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** PKB
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiNum=*on
097300030721     C                   Z-ADD     PiVal         VABPKB
097400030721     C                   ELSE
097500030721     C                   SETON                                        32
097600030721     C                   EVAL      VABPKB = *zeros
097700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097800030721     C                             + ' ' + 'VABPKB'
097900030721     C                   ENDIF
098000030721     C                   ENDIF
098100030721     C*** CAS
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
098300030721     C                   EVAL      PiStr=SkSplitCSV(i)
098400030721     C                   EXSR      CHKNUM
098500030721     C                   IF        PiNum=*on
098600030721     C                   Z-ADD     PiVal         VABCAS
098700030721     C                   ELSE
098800030721     C                   SETON                                        32
098900030721     C                   EVAL      VABCAS = *zeros
099000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099100030721     C                             + ' ' + 'VABCAS'
099200030721     C                   ENDIF
099300030721     C                   ENDIF
099400030721     C*** CCM
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
099600030721     C                   EVAL      PiStr=SkSplitCSV(i)
099700030721     C                   EXSR      CHKNUM
099800030721     C                   IF        PiInt=*on      AND
099900030721     C                             PiVal<=9999999 AND
100000030721     C                             PiVal>*zeros
100100030721     C                   Z-ADD     PiVal         VABCCM
100200030721     C                   ELSE
100300030721     C                   SETON                                        32
100400030721     C                   EVAL      VABCCM = *zeros
100500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100600030721     C                             + ' ' + 'VABCCM'
100700030721     C                   ENDIF
100800030721     C                   ENDIF
100900030721     C*** LNP
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
101100030721     C                   EVAL      PiStr=SkSplitCSV(i)
101200030721     C                   EXSR      CHKNUM
101300030721     C                   IF        PiInt=*on    AND
101400030721     C                             PiVal<=999   AND
101500030721     C                             PiVal>*zeros
101600030721     C                   Z-ADD     PiVal         VABLNP
101700030721     C                   ELSE
101800030721     C                   SETON                                        32
101900030721     C                   EVAL      VABLNP = *zeros
102000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102100030721     C                             + ' ' + 'VABLNP'
102200030721     C                   ENDIF
102300030721     C                   ENDIF
102400030721     C*** NRS
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
102600030721     C                   EVAL      PiStr=SkSplitCSV(i)
102700030721     C                   EXSR      CHKNUM
102800030721     C                   IF        PiInt=*on AND
102900030721     C                             PiVal<=99
103000030721     C                   Z-ADD     PiVal         VABNRS
103100030721     C                   ELSE
103200030721     C                   SETON                                        32
103300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103400030721     C                             + ' ' + 'VABNRS'
103500030721     C                   ENDIF
103600030721     C                   ENDIF
103700030721     C*** LNA
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
103900030721     C                   EVAL      PiStr=SkSplitCSV(i)
104000030721     C                   EXSR      CHKNUM
104100030721     C                   IF        PiInt=*on    AND
104200030721     C                             PiVal<=999   AND
104300030721     C                             PiVal>*zeros
104400030721     C                   Z-ADD     PiVal         VABLNA
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      VABLNA = *zeros
104800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104900030721     C                             + ' ' + 'VABLNA'
105000030721     C                   ENDIF
105100030721     C                   ENDIF
105200030721     C*** CTR
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
105400030721     C                   EVAL      PiStr=SkSplitCSV(i)
105500030721     C                   EXSR      CHKNUM
105600030721     C                   IF        PiInt=*on  AND
105700030721     C                             PiVal<=999
105800030721     C                   Z-ADD     PiVal         VABCTR
105900030721     C                   ELSE
106000030721     C                   SETON                                        32
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200030721     C                             + ' ' + 'VABCTR'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** IAS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiNum=*on
107000030721     C                   Z-ADD     PiVal         VABIAS
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107400030721     C                             + ' ' + 'VABIAS'
107500030721     C                   ENDIF
107600030721     C                   ENDIF
107700030721     C*** VLB
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
107900030721     C                   EVAL      PiStr=SkSplitCSV(i)
108000030721     C                   EXSR      CHKNUM
108100030721     C                   IF        PiNum=*on
108200030721     C                   Z-ADD     PiVal         VABVLB
108300030721     C                   ELSE
108400030721     C                   SETON                                        32
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABVLB'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** QFT
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiNum=*on
109400030721     C                   Z-ADD     PiVal         VABQFT
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800030721     C                             + ' ' + 'VABQFT'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** NCD
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiInt=*on      AND
110600030721     C                             PiVal<=9999999
110700030721     C                   Z-ADD     PiVal         VABNCD
110800030721     C                   ELSE
110900030721     C                   SETON                                        32
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100030721     C                             + ' ' + 'VABNCD'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** NCA
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on      AND
111900030721     C                             PiVal<=9999999
112000030721     C                   Z-ADD     PiVal         VABNCA
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABNCA'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** ZNC
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiInt=*on AND
113200030721     C                             PiVal<=99
113300030721     C                   Z-ADD     PiVal         VABZNC
113400030721     C                   ELSE
113500030721     C                   SETON                                        32
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABZNC'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** DCR
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on       AND
114500030721     C                             PiVal<=99999999
114600030721     C                   Z-ADD     PiVal         VABDCR
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABDCR'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** HCR
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on   AND
115800030721     C                             PiVal<=9999
115900030721     C                   Z-ADD     PiVal         VABHCR
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABHCR'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** VMD
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiNum=*on
117100030721     C                   Z-ADD     PiVal         VABVMD
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABVMD'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** ANT
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on        AND
118300030721     C                             PiVal<=999999999
118400030721     C                   Z-ADD     PiVal         VABANT
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABANT'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** CMO
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on
119600030721     C                   Z-ADD     PiVal         Num5_0
119700030721     C                   MOVEL(p)  Num5_0        VABCMO
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABCMO = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABCMO'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030715     C*
120600030715     C                   ENDIF
120700030715     C                   EVAL      i = i + 1
120800030715     C                   ENDDO
120900040726     C*
121000040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
121100040726     C                   IF        VABNSP = *zeros
121200040726     C                   SETON                                        31
121300040726     C                   ENDIF
121400040726     C*
121500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
121600040726     C                   IF        VABLNP = *zeros
121700040726     C                   SETON                                        31
121800040726     C                   ENDIF
121900040726     C*
122000040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
122100040726     C* d valorizzarli sempre entrambi
122200040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
122300040726     C                   Z-ADD     VABNSP        VABRMN
122400040726     C                   ENDIF
122500040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
122600040726     C                   MOVEL     VABRMN        VABRMA
122700040726     C                   ENDIF
122800040726     C*
122900040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
123000040726     C                   IF        VABCAS > *zeros
123100040726     C                   IF        VABVCA = *blanks
123200040726     C                   EVAL      VABVCA = 'EUR'
123300040726     C                   ENDIF
123400040726     C                   IF        VABCBO = '1'
123500040726     C                   EVAL      VABCBO = '4'
123600040726     C                   ENDIF
123700040726     C                   IF        VABCBO = '2'
123800040726     C                   EVAL      VABCBO = '6'
123900040726     C                   ENDIF
124000040726     C                   ENDIF
124100020204     C*
124200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
124300020204     C                   EXSR      CHKIMPDIV
124400030715     C                   ENDIF
124500020204     C*
124600010607     C                   ENDSR
124700010607     C*----------------------------------------------------*
124800010601
124900020204
125000020204     C*----------------------------------------------------*
125100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
125200020204     C*----------------------------------------------------*
125300020204     C     CHKIMPDIV     BEGSR
125400020204     C*
125500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
125600020204     C                   Z-ADD     *zeros        wrkDec            9 9
125700020204     C*
125800020204     C* Come prima cosa effettuo considerazioni sulla divisa
125900020204     C                   IF        vabIAS > *zeros
126000020204     C                   IF        vabVAS <> 'EUR'
126100020204     C                   EVAL      vabVAS =  'ITL'
126200020204     C                   ENDIF
126300020204     C                   ENDIF
126400020204     C*
126500020204     C                   IF        vabCAS > *zeros
126600020204     C                   IF        vabVCA <> 'EUR'
126700020204     C                   EVAL      vabVCA =  'ITL'
126800020204     C                   ENDIF
126900020204     C                   ENDIF
127000020204     C*
127100020204     C                   IF        vabVMD > *zeros
127200020204     C                   IF        vabVAD <> 'EUR'
127300020204     C                   EVAL      vabVAD =  'ITL'
127400020204     C                   ENDIF
127500020204     C                   ENDIF
127600020204     C*
127700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
127800020204     C                   Z-ADD     vabIAS        wrkDec
127900020204     C                   IF        wrkDec > *zeros
128000020204     C                   IF        vabVAS = 'ITL'
128100020204     C                   EVAL      vabIAS = *zeros
128200020204     C                   ENDIF
128300020204     C                   ENDIF
128400020204     C*
128500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
128600020204     C                   Z-ADD     vabCAS        wrkDec
128700020204     C                   IF        wrkDec > *zeros
128800020204     C                   IF        vabVCA = 'ITL'
128900020204     C                   EVAL      vabCAS = *zeros
129000020204     C                   ENDIF
129100020204     C                   ENDIF
129200020204     C*
129300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
129400020204     C                   Z-ADD     vabVMD        wrkDec
129500020204     C                   IF        wrkDec > *zeros
129600020204     C                   IF        vabVAD = 'ITL'
129700020204     C                   EVAL      vabVMD = *zeros
129800020204     C                   ENDIF
129900020204     C                   ENDIF
130000020204     C*
130100020204     C                   ENDSR
130200020204     C***
130300020204
130400010330
130500010330
130600010330     C*----------------------------------------------------*
130700010330     C*  CONTROLLO NUMERICITA' CAMPI
130800010330     C*----------------------------------------------------*
130900010330     C     CHKNUM        BEGSR
131000010330     C*
131100010606     C                   IF        PiDecChr = *blanks
131200030715     C                   EVAL      PiDecChr = CharNUM
131300010606     C                   ENDIF
131400010606     C*
131500010606     C                   CALL(e)   'ISNUMERIC'
131600010330     C                   PARM                    PiStr            30
131700010606     C                   PARM                    PiDecChr          1
131800010330     C                   PARM      *ZEROS        PiVal            30 9
131900010330     C                   PARM      '0'           PiInt             1
132000010330     C                   PARM      '0'           PiNum             1
132100010330     C                   IF        %error
132200010606     C                   EVAL      PiNum=*off
132300010330     C                   ENDIF
132400010330     C*
132500010330     C                   ENDSR
132600010330     C***
132700010330
132800010601
132900010601
133000010601
133100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
133200010601     C     repfil        BEGSR
133300010601     C*
133400010601     C                   if        invfil = *zeros and
133500010601     C                             depfil > *zeros and
133600010629     C                             (vinflg = *blanks or
133700010629     C                              vinflg = *zeros)
133800010601     C
133900010601     C                   eval      invfil = depfil
134000010601     C                   endif
134100010601     C*
134200010601     C                   if        depfil <> invfil and
134300010601     C                             invfil > *zeros
134400010601     C                   eval      flgMulti = '1'
134500010601     C                   if        vinflg = *blanks
134600010601     C                   add       1             cntNonEl
134700010601     C                   endif
134800010601     C                   endif
134900010601     C*
135000010601     C                   if        vinflg = '2'
135100010601     C                   eval      flgStato = '2'
135200010601     C                   endif
135300010601     C*
135400010601     C                   ENDSR
135500010601     C***
135600010601
135700010601
135800010601
135900010330
136000010330
136100010330
136200990920      /TITLE Invio dei dati al punto operativo.
136300000613     C     invio         BEGSR
136400990920     C*
136500990920     C                   reset                   dscmz
136600010601     C                   move      invfil        cmzdst
136700990920     C                   eval      cmzfld = vlrfou
136800990920     C                   eval      cmzmbd = vlrhdl
136900990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
137000000710     C                   if        prmfir = *blanks
137100021025     C                   eval      cmzfla = 'FIVAB00F'
137200021025     C                   eval      cmzmba = 'FIVAB00F'
137300000710     C                   else
137400000710     C                   eval      cmzfla = prmfir
137500000710     C                   eval      cmzmba = prmfir
137600000710     C                   endif
137700990920     C                   eval      cmznrr = *zeros
137800990920     C                   move      §ctrok        cmznrr
137900021018     C                   eval      cmzlba = vlrfl1
138000990920     C                   call(e)   'TIS711C'
138100990920     C                   parm                    dscmz
138200990921     C                   parm      *blanks       esito
138300990920     C                   if        %error
138400990920     C                             or cmzerr = '1'
138500990921     C                             or esito  = '1'
138600000710     C                   eval      wrkesito = '3'
138700990920     C                   endif
138800990920     C*
138900000613     C                   ENDSR
139000990910
139100010601
139200010601
139300010601
139400010601
139500010601      /TITLE Invio dei dati al punto operativo.
139600010601     C     opeini        BEGSR
139700010601     C*
139800010601     C* Inizializzo flag e contatori operativi
139900010601     C                   movel     '0'           flgGiro           1
140000010601     C                   movel     '0'           flgMulti          1
140100010601     C                   movel     '1'           flgStato          1
140200010615     C                   movel     '0'           flgOk             1
140300010601     C                   z-add     *zeros        cntNonEl         10 0
140400010601     C                   z-add     *zeros        depfil            3 0
140500010601     C                   z-add     *zeros        invfil            3 0
140600010601     C*
140700010601     C                   ENDSR
140800010601     C***
140900010601
141000010601
141100050421
141200050421
141300050421     C     *pssr         BEGSR
141400050421     C*
141500050421     C                   eval      wrkesito = '2'
141600050421     C*
141700050421     C                   ENDSR     '*CANCL'
141800050421     C***
141900050421
142000010601
142100010330
142200010330
142300000613     C     *inzsr        BEGSR
142400990910     C*
142500990910     C     *entry        plist
142600990920     C                   parm                    tivlrds
142700990921     C                   parm      wrkesito      esito
142800000724     C                   parm                    prmlit
142900000710     C                   parm                    prmfir
143000010330     C*
143100010330     C* CALCOLA LA DATA CORRENTE
143200010330     C                   time                    wn14             14 0
143300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
143400010330     C                   z-add     wn8           g08dat
143500010330     C                   z-add     *zeros        g08inv
143600010330     C                   movel     '0'           g08err
143700010330     C                   call      'XSRDA8'
143800010330     C                   parm                    wlbda8
143900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
144000000613     C*
144100000613     C                   ENDSR
144200000613     C***
144300990908
144400030715     Otitvxlp   E            riepilogo         2
144500990915     O                                              'Upload via Internet'
144600990915     O                                           +1 'Traduzione TIVIN00R -'
144700021025     O                                           +1 'FIVABWWR'
144800990915     O                                           +1 'Testate bolle'
144900990915     O          E            riepilogo   2
145000990915     O                       wrkdata
145100990915     O                       wrkora              +1
145200990915     O                       procname            +1
145300990915     O          E            riepilogo   2
145400990915     O                                              'Cliente..................:'
145500010605     O                       VABCCM        z     +1
145600990915     O          E            riepilogo   2
145700990920     O                                              'Riferimento Strategi.....:'
145800990920     O                       vlrhdl              +1
145900990915     O          E            riepilogo   2
146000990915     O                                              'Giusti...................:'
146100971022     O                       §CTROK        2   +  1
146200990915     O          E            riepilogo   2
146300990915     O                                              'Sbagliati e corretti.....:'
146400971022     O                       §CTRMO        2   +  1
146500990915     O          E            riepilogo   2
146600990915     O                                              'Sbagliati e scartati.....:'
146700971022     O                       §CTRNO        2   +  1
146800000613
146900030715     Otitvxlps  E            testdett          2
147000000613     O                                              'Upload via Internet'
147100000613     O                                           +1 'Traduzione TIVIN00R -'
147200021025     O                                           +1 'FIVABWWR'
147300000613     O                                           +1 'Testate bolle'
147400000616     O          E            testdett    3
147500000613     O                                           +2 'N° rec'
147600000613     O                                           +3 'Anteprima contenuto'
147700000616     O          E            rigadett    2
147800000613     O                       rrnum               +2
147900000621     O                       recko               +3
148000000616     O          E            findett     2
148100000613     O                       wrkdata
148200000613     O                       wrkora              +1
148300000613     O                       procname            +1
148400000616     O          E            findett     2
148500000613     O                                              'Cliente..................:'
148600010605     O                       VABCCM        z     +1
148700000616     O          E            findett     2
148800000613     O                                              'Riferimento Strategi.....:'
148900000613     O                       vlrhdl              +1
149000000616     O          E            findett     2
149100000613     O                                              'Giusti...................:'
149200000613     O                       §CTROK        2   +  1
149300000616     O          E            findett     2
149400000613     O                                              'Sbagliati e corretti.....:'
149500000613     O                       §CTRMO        2   +  1
149600000616     O          E            findett     2
149700000613     O                                              'Sbagliati e scartati.....:'
149800000613     O                       §CTRNO        2   +  1
149900000613** CMD - COMANDI CL
150000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
150100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
150200030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
150300010606
150400000613
150500040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
150600050307X|VABRMA|X|X|X|X|X|X|X|X|VABPKB|X|X|X|X|VABNCL|X|X|X|VABRSD|
150700050307VABIND|VABLOD|VABCAD|VABPRD|VABNZD|X|X|X|X|X|X|X|X
150800040726
