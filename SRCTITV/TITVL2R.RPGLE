000100050321      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente ITELCOCLIMA - Cod. 2492992)
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
063000050321     C*
063100050512     C* 1° Giro x eliminare il separatore testo
063200050321     C                   Z-ADD     1             wj                5 0
063300050321     C                   DOW       wj <= %len(vindta) - 1
063400050321     C                   IF        %subst(vindta:wj:1) = CharTXT
063500050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
063600050512     C                                      %subst(vindta:wj+1)
063700050512     C                   EVAL      wj = wj - 1
063800050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
063900050321     C                   ENDIF
064000050321     C                   ADD       1             wj
064100050321     C                   ENDDO
064200050512     C*
064300050512     C* Se la stringa dati da splittare inizia conil carattere separatore campo aggiungo in test
064400050512     C* 2 byte a blank
064500050512     C                   IF        %subst(vindta:1:1) = CharCSV
064600050512     C                   EVAL      vindta = '  ' + vindta
064700050512     C                   ENDIF
064800010607     C*
064900030715     C**********
065000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
065100030715     C**********
065200030715     C                   IF        wGiro = *zeros
065300040726     C*
065400040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
065500040726     C                   EVAL      depvinDTA = vinDTA
065600040726     C*
065700040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
065800040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
065900050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
066000050321     C                                      %trim(intcol(5))+%trim(intcol(6))+
066100050321     C                                      %trim(intcol(7))+%trim(intcol(8))+
066200050321     C                                      %trim(intcol(9))+%trim(intcol(10))
066300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
066400031201     C     minu:maiu     XLATE     vindta        vindta
066500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066600040324     C                   DOW       posDa <= %len(%trimr(vindta))
066700030715     C*
066800030715     C* Gestisco il 1° campo
066900030715     C                   IF        i = 1
067000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
067100030715     C                   EVAL      posDa = 1
067200030715     C* Eseguo lo scan x trovare la fine del primo campo
067300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067400030715     C* A questo punto "estraggo" il campo corrente
067500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067600030715     C                                                    (posA-posDa))
067700030715     C* X i campi successivi al 1°
067800030715     C                   ELSE
067900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
068000030715     C                   EVAL      posDa = posA + 1
068100030715     C* Eseguo lo scan x trovare la fine del campo corrente
068200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068300030715     C* Gestisco l'ultimo campo
068400030715     C                   IF        posA = *zeros
068500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
068600030715     C* A questo punto "estraggo" il campo corrente
068700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068800030715     C                                                    (posA-posDa))
068900030715     C                   LEAVE
069000030715     C                   ELSE
069100030715     C* A questo punto "estraggo" il campo corrente
069200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069300030715     C                                                    (posA-posDa))
069400030715     C                   ENDIF
069500030715     C                   ENDIF
069600030715     C* Incremento il contatore d campo
069700030715     C                   EVAL      i = i +1
069800030715     C                   ENDDO
069900030715     C*
070000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
070100030715     C* delimitazione testo
070200030715     C                   EVAL      i = 1
070300030715     C                   DOW       i < %elem(SkSplitFLD)
070400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
070500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
070600030715     C                   EVAL      i = i + 1
070700030715     C                   ENDDO
070800030715     C                   EVAL      wGiro = 1
070900040726     C* Ripristino la 1° riga originale
071000040726     C                   EVAL      vinDTA = depvinDTA
071100030715     C                   ELSE
071200030715     C                   EVAL      wGiro = 2
071300031204     C**********
071400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
071500031204     C* d separatore campo
071600031204     C**********
071700050512     C*                  EVAL      vindta = %trim(vindta)
071800050512     C*                  DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
071900050512     C*                  EVAL      vindta = %subst(vindta:2)
072000050512     C*                  ENDDO
072100050512     C*
072200050512     C*                  Z-ADD     *zeros        lunghInput        4 0
072300050512     C*                  EVAL      lunghInput = %len(%trimr(vindta))
072400050512     C*                  DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
072500050512     C*                  EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
072600050512     C*                  EVAL      lunghInput = %len(%trim(vindta))
072700050512     C*                  ENDDO
072800030715     C**********
072900030715     C* Effettuo lo split del campo dati d input
073000030715     C**********
073100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073200040324     C                   DOW       posDa <= %len(%trimr(vindta))
073300030715     C*
073400030715     C* Gestisco il 1° campo
073500030715     C                   IF        i = 1
073600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073700030715     C                   EVAL      posDa = 1
073800030715     C* Eseguo lo scan x trovare la fine del primo campo
073900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074000030715     C* A questo punto "estraggo" il campo corrente
074100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074200030715     C                                                    (posA-posDa))
074300030715     C* X i campi successivi al 1°
074400030715     C                   ELSE
074500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074600030715     C                   EVAL      posDa = posA + 1
074700031204     C* Verifico che nn vi sia il campo nn valorizzato
074800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
074900031204     C* Se campo nn valorizzato skippo al prossimo
075000031204     C                   EVAL      SkSplitCSV(i) = *blanks
075100031204     C                   EVAL      posA  = posA + 1
075200031204     C                   ELSE
075300030715     C* Eseguo lo scan x trovare la fine del campo corrente
075400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075500030715     C* Gestisco l'ultimo campo
075600030715     C                   IF        posA = *zeros
075700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
075800030715     C* A questo punto "estraggo" il campo corrente
075900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076000030715     C                                                    (posA-posDa))
076100030715     C                   LEAVE
076200030715     C                   ELSE
076300030715     C* A questo punto "estraggo" il campo corrente
076400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076500030715     C                                                    (posA-posDa))
076600030715     C                   ENDIF
076700031204     C                   ENDIF
076800030715     C                   ENDIF
076900030715     C* Incremento il contatore d campo
077000030715     C                   EVAL      i = i +1
077100030715     C                   ENDDO
077200030715     C*
077300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077400030715     C* delimitazione testo
077500030715     C                   EVAL      i = 1
077600030715     C                   DOW       i < %elem(SkSplitCSV)
077700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
077800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
077900030715     C                   EVAL      i = i + 1
078000030715     C                   ENDDO
078100030715     C*
078200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
078300030715     C                   EVAL      i = 1
078400030715     C                   DOW       i < %elem(SkSplitFLD)
078500030715     C*
078600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
078800030715     C                             SkSplitCSV(i) <> *blanks
078900030721     C*** ATB
079000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
079100030721     C                   EVAL      VABATB = SkSplitCSV(i)
079200030721     C                   ENDIF
079300030715     C*** RSD
079400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
079500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
079600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
079800030715     C* ==
079900030715     C                   ENDIF
080000030721     C*** RD2
080100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
080200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
080300030721     C                   ENDIF
080400030715     C*** IND
080500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
080600030715     C                   EVAL      VABIND = SkSplitCSV(i)
080700030715     C                   ENDIF
080800030715     C*** LOD
080900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
081000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
081100030715     C                   ENDIF
081200030715     C*** PRD
081300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
081400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
081500030715     C                   ENDIF
081600030721     C*** NZD
081700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
081800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
081900030721     C                   ENDIF
082000030715     C*** RMA
082100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
082200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
082300030715     C                   ENDIF
082400030715     C*** NOT
082500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
082600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
082700030715     C                   ENDIF
082800030715     C*** NT2
082900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
083000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
083100030715     C                   ENDIF
083200030715     C*** VCA
083300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
083400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
083500030715     C                   ENDIF
083600030715     C*** VAS
083700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
083800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
083900030715     C                   ENDIF
084000030715     C*** FFD
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
084200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
084300030715     C                   ENDIF
084400030715     C*** CBO
084500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
084600030715     C                   EVAL      VABCBO = SkSplitCSV(i)
084700030715     C                   ENDIF
084800030715     C
084900030721     C*** GC1
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
085100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** GC2
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
085500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** TSP
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
085900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** NAS
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
086300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** TIC
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
086700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** GCA
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
087100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** XCO
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
087500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** CTM
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
087900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** TCR
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
088300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** CTS
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
088700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** FTM
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
089100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** VAD
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
089500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** GMA
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
089900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** GGA
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
090300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** GVA
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
090700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** TC1
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
091100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** TC2
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
091500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** SCL
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
091900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** RMO
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
092300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** NMO
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
092700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*
093000030721     C* Reperisco quindi i campi numerici...
093100030721     C*** CAD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
093300030721     C                   EVAL      PiStr=SkSplitCSV(i)
093400030721     C                   EXSR      CHKNUM
093500030721     C                   IF        PiInt=*on
093600030721     C                   Z-ADD     PiVal         Num5_0
093700030721     C                   MOVEL(p)  Num5_0        VABCAD
093800030721     C                   ELSE
093900030721     C                   SETON                                        32
094000030721     C                   EVAL      VABCAD = *zeros
094100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094200030721     C                             + ' ' + 'VABCAD'
094300030721     C                   ENDIF
094400030721     C                   ENDIF
094500030721     C*** RMN
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
094700030721     C                   EVAL      PiStr=SkSplitCSV(i)
094800030721     C                   EXSR      CHKNUM
094900030721     C                   IF        PiInt=*on
095000030721     C                   Z-ADD     PiVal         VABRMN
095100030721     C                   ELSE
095200030721     C                   SETON                                        32
095300030721     C                   EVAL      VABRMN = 1
095400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095500030721     C                             + ' ' + 'VABRMN'
095600030721     C                   ENDIF
095700030721     C                   ENDIF
095800030721     C*** NSP
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096000030721     C                   EVAL      PiStr=SkSplitCSV(i)
096100030721     C                   EXSR      CHKNUM
096200040726     C                   IF        PiInt=*on
096300040726     C                   Z-ADD     PiVal         VABNSP
096400040802     C*                  IF        PiVal>9999999
096500040802     C*                  SETON                                        32
096600040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
096700040802     C*                            + ' ' + 'VABNSP'
096800040802     C*                  ENDIF
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABNSP = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABNSP'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** NCL
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiInt=*on
098100030721     C                   Z-ADD     PiVal         VABNCL
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABNCL = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABNCL'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** PKB
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiNum=*on
099400030721     C                   Z-ADD     PiVal         VABPKB
099500030721     C                   ELSE
099600030721     C                   SETON                                        32
099700030721     C                   EVAL      VABPKB = *zeros
099800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099900030721     C                             + ' ' + 'VABPKB'
100000030721     C                   ENDIF
100100030721     C                   ENDIF
100200030721     C*** CAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
100400030721     C                   EVAL      PiStr=SkSplitCSV(i)
100500030721     C                   EXSR      CHKNUM
100600030721     C                   IF        PiNum=*on
100700030721     C                   Z-ADD     PiVal         VABCAS
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAS = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAS'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** CCM
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on      AND
102000030721     C                             PiVal<=9999999 AND
102100030721     C                             PiVal>*zeros
102200030721     C                   Z-ADD     PiVal         VABCCM
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABCCM = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700030721     C                             + ' ' + 'VABCCM'
102800030721     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C*** LNP
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
103200030721     C                   EVAL      PiStr=SkSplitCSV(i)
103300030721     C                   EXSR      CHKNUM
103400030721     C                   IF        PiInt=*on    AND
103500030721     C                             PiVal<=999   AND
103600030721     C                             PiVal>*zeros
103700030721     C                   Z-ADD     PiVal         VABLNP
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      VABLNP = *zeros
104100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104200030721     C                             + ' ' + 'VABLNP'
104300030721     C                   ENDIF
104400030721     C                   ENDIF
104500030721     C*** NRS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104700030721     C                   EVAL      PiStr=SkSplitCSV(i)
104800030721     C                   EXSR      CHKNUM
104900030721     C                   IF        PiInt=*on AND
105000030721     C                             PiVal<=99
105100030721     C                   Z-ADD     PiVal         VABNRS
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105500030721     C                             + ' ' + 'VABNRS'
105600030721     C                   ENDIF
105700030721     C                   ENDIF
105800030721     C*** LNA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
106000030721     C                   EVAL      PiStr=SkSplitCSV(i)
106100030721     C                   EXSR      CHKNUM
106200030721     C                   IF        PiInt=*on    AND
106300030721     C                             PiVal<=999   AND
106400030721     C                             PiVal>*zeros
106500030721     C                   Z-ADD     PiVal         VABLNA
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      VABLNA = *zeros
106900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107000030721     C                             + ' ' + 'VABLNA'
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** CTR
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on  AND
107800030721     C                             PiVal<=999
107900030721     C                   Z-ADD     PiVal         VABCTR
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABCTR'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** IAS
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABIAS
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABIAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** VLB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABVLB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABVLB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** QFT
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABQFT
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABQFT'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCD
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on      AND
112700030721     C                             PiVal<=9999999
112800030721     C                   Z-ADD     PiVal         VABNCD
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNCD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on      AND
114000030721     C                             PiVal<=9999999
114100030721     C                   Z-ADD     PiVal         VABNCA
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABNCA'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** ZNC
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on AND
115300030721     C                             PiVal<=99
115400030721     C                   Z-ADD     PiVal         VABZNC
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABZNC'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** DCR
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on       AND
116600030721     C                             PiVal<=99999999
116700030721     C                   Z-ADD     PiVal         VABDCR
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABDCR'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** HCR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on   AND
117900030721     C                             PiVal<=9999
118000030721     C                   Z-ADD     PiVal         VABHCR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABHCR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** VMD
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABVMD
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABVMD'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** ANT
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on        AND
120400030721     C                             PiVal<=999999999
120500030721     C                   Z-ADD     PiVal         VABANT
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABANT'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CMO
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700030721     C                   Z-ADD     PiVal         Num5_0
121800030721     C                   MOVEL(p)  Num5_0        VABCMO
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCMO = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCMO'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030715     C*
122700030715     C                   ENDIF
122800030715     C                   EVAL      i = i + 1
122900030715     C                   ENDDO
123000040726     C*
123100040726     C* Considerazioni personalizzate x il traduttore corrente
123200040726     C                   MOVEL     *blanks       wRMNAlfa          6
123300040726     C                   EVALR     wRMNALfa = %trim(VABRMA)
123400040726     C                   MOVE(P)   wRMNAlfa      VABRMN
123500040726     C*
123600040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
123700040726     C                   IF        VABNSP = *zeros
123800040726     C                   SETON                                        31
123900040726     C                   ENDIF
124000040726     C*
124100040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
124200040726     C                   IF        VABLNP = *zeros
124300040726     C                   SETON                                        31
124400040726     C                   ENDIF
124500040726     C*
124600040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
124700040726     C* d valorizzarli sempre entrambi
124800040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
124900040726     C                   Z-ADD     VABNSP        VABRMN
125000040726     C                   ENDIF
125100040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
125200040726     C                   MOVEL     VABRMN        VABRMA
125300040726     C                   ENDIF
125400040726     C*
125500040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
125600040726     C                   IF        VABCAS > *zeros
125700040726     C                   IF        VABVCA = *blanks
125800040726     C                   EVAL      VABVCA = 'EUR'
125900040726     C                   ENDIF
126000040726     C                   IF        VABCBO = '1'
126100040726     C                   EVAL      VABCBO = '4'
126200040726     C                   ENDIF
126300040726     C                   IF        VABCBO = '2'
126400040726     C                   EVAL      VABCBO = '6'
126500040726     C                   ENDIF
126600040726     C                   ENDIF
126700020204     C*
126800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
126900020204     C                   EXSR      CHKIMPDIV
127000030715     C                   ENDIF
127100020204     C*
127200010607     C                   ENDSR
127300010607     C*----------------------------------------------------*
127400010601
127500020204
127600020204     C*----------------------------------------------------*
127700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
127800020204     C*----------------------------------------------------*
127900020204     C     CHKIMPDIV     BEGSR
128000020204     C*
128100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
128200020204     C                   Z-ADD     *zeros        wrkDec            9 9
128300020204     C*
128400020204     C* Come prima cosa effettuo considerazioni sulla divisa
128500020204     C                   IF        vabIAS > *zeros
128600020204     C                   IF        vabVAS <> 'EUR'
128700020204     C                   EVAL      vabVAS =  'ITL'
128800020204     C                   ENDIF
128900020204     C                   ENDIF
129000020204     C*
129100020204     C                   IF        vabCAS > *zeros
129200020204     C                   IF        vabVCA <> 'EUR'
129300020204     C                   EVAL      vabVCA =  'ITL'
129400020204     C                   ENDIF
129500020204     C                   ENDIF
129600020204     C*
129700020204     C                   IF        vabVMD > *zeros
129800020204     C                   IF        vabVAD <> 'EUR'
129900020204     C                   EVAL      vabVAD =  'ITL'
130000020204     C                   ENDIF
130100020204     C                   ENDIF
130200020204     C*
130300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
130400020204     C                   Z-ADD     vabIAS        wrkDec
130500020204     C                   IF        wrkDec > *zeros
130600020204     C                   IF        vabVAS = 'ITL'
130700020204     C                   EVAL      vabIAS = *zeros
130800020204     C                   ENDIF
130900020204     C                   ENDIF
131000020204     C*
131100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
131200020204     C                   Z-ADD     vabCAS        wrkDec
131300020204     C                   IF        wrkDec > *zeros
131400020204     C                   IF        vabVCA = 'ITL'
131500020204     C                   EVAL      vabCAS = *zeros
131600020204     C                   ENDIF
131700020204     C                   ENDIF
131800020204     C*
131900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
132000020204     C                   Z-ADD     vabVMD        wrkDec
132100020204     C                   IF        wrkDec > *zeros
132200020204     C                   IF        vabVAD = 'ITL'
132300020204     C                   EVAL      vabVMD = *zeros
132400020204     C                   ENDIF
132500020204     C                   ENDIF
132600020204     C*
132700020204     C                   ENDSR
132800020204     C***
132900020204
133000010330
133100010330
133200010330     C*----------------------------------------------------*
133300010330     C*  CONTROLLO NUMERICITA' CAMPI
133400010330     C*----------------------------------------------------*
133500010330     C     CHKNUM        BEGSR
133600010330     C*
133700010606     C                   IF        PiDecChr = *blanks
133800030715     C                   EVAL      PiDecChr = CharNUM
133900010606     C                   ENDIF
134000010606     C*
134100010606     C                   CALL(e)   'ISNUMERIC'
134200010330     C                   PARM                    PiStr            30
134300010606     C                   PARM                    PiDecChr          1
134400010330     C                   PARM      *ZEROS        PiVal            30 9
134500010330     C                   PARM      '0'           PiInt             1
134600010330     C                   PARM      '0'           PiNum             1
134700010330     C                   IF        %error
134800010606     C                   EVAL      PiNum=*off
134900010330     C                   ENDIF
135000010330     C*
135100010330     C                   ENDSR
135200010330     C***
135300010330
135400010601
135500010601
135600010601
135700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
135800010601     C     repfil        BEGSR
135900010601     C*
136000010601     C                   if        invfil = *zeros and
136100010601     C                             depfil > *zeros and
136200010629     C                             (vinflg = *blanks or
136300010629     C                              vinflg = *zeros)
136400010601     C
136500010601     C                   eval      invfil = depfil
136600010601     C                   endif
136700010601     C*
136800010601     C                   if        depfil <> invfil and
136900010601     C                             invfil > *zeros
137000010601     C                   eval      flgMulti = '1'
137100010601     C                   if        vinflg = *blanks
137200010601     C                   add       1             cntNonEl
137300010601     C                   endif
137400010601     C                   endif
137500010601     C*
137600010601     C                   if        vinflg = '2'
137700010601     C                   eval      flgStato = '2'
137800010601     C                   endif
137900010601     C*
138000010601     C                   ENDSR
138100010601     C***
138200010601
138300010601
138400010601
138500010330
138600010330
138700010330
138800990920      /TITLE Invio dei dati al punto operativo.
138900000613     C     invio         BEGSR
139000990920     C*
139100990920     C                   reset                   dscmz
139200010601     C                   move      invfil        cmzdst
139300990920     C                   eval      cmzfld = vlrfou
139400990920     C                   eval      cmzmbd = vlrhdl
139500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
139600000710     C                   if        prmfir = *blanks
139700021025     C                   eval      cmzfla = 'FIVAB00F'
139800021025     C                   eval      cmzmba = 'FIVAB00F'
139900000710     C                   else
140000000710     C                   eval      cmzfla = prmfir
140100000710     C                   eval      cmzmba = prmfir
140200000710     C                   endif
140300990920     C                   eval      cmznrr = *zeros
140400990920     C                   move      §ctrok        cmznrr
140500021018     C                   eval      cmzlba = vlrfl1
140600990920     C                   call(e)   'TIS711C'
140700990920     C                   parm                    dscmz
140800990921     C                   parm      *blanks       esito
140900990920     C                   if        %error
141000990920     C                             or cmzerr = '1'
141100990921     C                             or esito  = '1'
141200000710     C                   eval      wrkesito = '3'
141300990920     C                   endif
141400990920     C*
141500000613     C                   ENDSR
141600990910
141700010601
141800010601
141900010601
142000010601
142100010601      /TITLE Invio dei dati al punto operativo.
142200010601     C     opeini        BEGSR
142300010601     C*
142400010601     C* Inizializzo flag e contatori operativi
142500010601     C                   movel     '0'           flgGiro           1
142600010601     C                   movel     '0'           flgMulti          1
142700010601     C                   movel     '1'           flgStato          1
142800010615     C                   movel     '0'           flgOk             1
142900010601     C                   z-add     *zeros        cntNonEl         10 0
143000010601     C                   z-add     *zeros        depfil            3 0
143100010601     C                   z-add     *zeros        invfil            3 0
143200010601     C*
143300010601     C                   ENDSR
143400010601     C***
143500010601
143600010601
143700050421
143800050421
143900050421     C     *pssr         BEGSR
144000050421     C*
144100050421     C                   eval      wrkesito = '2'
144200050421     C*
144300050421     C                   ENDSR     '*CANCL'
144400050421     C***
144500050421
144600010601
144700010330
144800010330
144900000613     C     *inzsr        BEGSR
145000990910     C*
145100990910     C     *entry        plist
145200990920     C                   parm                    tivlrds
145300990921     C                   parm      wrkesito      esito
145400000724     C                   parm                    prmlit
145500000710     C                   parm                    prmfir
145600010330     C*
145700010330     C* CALCOLA LA DATA CORRENTE
145800010330     C                   time                    wn14             14 0
145900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
146000010330     C                   z-add     wn8           g08dat
146100010330     C                   z-add     *zeros        g08inv
146200010330     C                   movel     '0'           g08err
146300010330     C                   call      'XSRDA8'
146400010330     C                   parm                    wlbda8
146500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146600000613     C*
146700000613     C                   ENDSR
146800000613     C***
146900990908
147000030715     Otitvxlp   E            riepilogo         2
147100990915     O                                              'Upload via Internet'
147200990915     O                                           +1 'Traduzione TIVIN00R -'
147300021025     O                                           +1 'FIVABWWR'
147400990915     O                                           +1 'Testate bolle'
147500990915     O          E            riepilogo   2
147600990915     O                       wrkdata
147700990915     O                       wrkora              +1
147800990915     O                       procname            +1
147900990915     O          E            riepilogo   2
148000990915     O                                              'Cliente..................:'
148100010605     O                       VABCCM        z     +1
148200990915     O          E            riepilogo   2
148300990920     O                                              'Riferimento Strategi.....:'
148400990920     O                       vlrhdl              +1
148500990915     O          E            riepilogo   2
148600990915     O                                              'Giusti...................:'
148700971022     O                       §CTROK        2   +  1
148800990915     O          E            riepilogo   2
148900990915     O                                              'Sbagliati e corretti.....:'
149000971022     O                       §CTRMO        2   +  1
149100990915     O          E            riepilogo   2
149200990915     O                                              'Sbagliati e scartati.....:'
149300971022     O                       §CTRNO        2   +  1
149400000613
149500030715     Otitvxlps  E            testdett          2
149600000613     O                                              'Upload via Internet'
149700000613     O                                           +1 'Traduzione TIVIN00R -'
149800021025     O                                           +1 'FIVABWWR'
149900000613     O                                           +1 'Testate bolle'
150000000616     O          E            testdett    3
150100000613     O                                           +2 'N° rec'
150200000613     O                                           +3 'Anteprima contenuto'
150300000616     O          E            rigadett    2
150400000613     O                       rrnum               +2
150500000621     O                       recko               +3
150600000616     O          E            findett     2
150700000613     O                       wrkdata
150800000613     O                       wrkora              +1
150900000613     O                       procname            +1
151000000616     O          E            findett     2
151100000613     O                                              'Cliente..................:'
151200010605     O                       VABCCM        z     +1
151300000616     O          E            findett     2
151400000613     O                                              'Riferimento Strategi.....:'
151500000613     O                       vlrhdl              +1
151600000616     O          E            findett     2
151700000613     O                                              'Giusti...................:'
151800000613     O                       §CTROK        2   +  1
151900000616     O          E            findett     2
152000000613     O                                              'Sbagliati e corretti.....:'
152100000613     O                       §CTRMO        2   +  1
152200000616     O          E            findett     2
152300000613     O                                              'Sbagliati e scartati.....:'
152400000613     O                       §CTRNO        2   +  1
152500000613** CMD - COMANDI CL
152600030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
152700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
152800030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
152900010606
153000000613
153100040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
153200050517VABATB;VABCCM;VABLNP;VABAAS;VABMGS;VABNRS;VABNSP;VABCBO;XXXLNA;
153300050512VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VABGC1;VABGC2;
153400050513VABCTR;VABTSP;VABIAS;VABVAS;VABNAS;VABNCL;VABPKB;XXXFIL1;VABVLB;VABQFT;
153500050513VABCAS;XXXTIC;VABVCA;VABGCA;VABRMN;VABRMA;VABNCD;VABNCA;VABXCO;
153600050512VABNOT;VABNT2;VABZNC;VABCTM;VABFFD;VABDCR;VABTCR;VABHCR;VABCTS;
153700050512VABFTM;VABVMD;VABVAD;VABGMA;VABGGA;VABGVA;VABTC1;VABTC2;VABSCL;
153800050512VABANT;VABRMO;VABCMO;VABNMO
153900050321
154000050321
154100050321
