000100050505      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente DGM S.p.A. - Cod. 0661039)
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
004900050321     D wOldSep1        s              2    INZ(*blanks)
005000050321     D wOldSep2        s              2    INZ(*blanks)
005100050321     D wNewSep1        s              2    INZ(*blanks)
005200050321     D wNewSep2        s              2    INZ(*blanks)
005300030822     D*------------------
005400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005500030822     D*------------------
005600030822     D posDaDft        S              3  0 INZ(*zeros)
005700030822     D posADft         S              3  0 INZ(*zeros)
005800030822     D j               s              3  0 INZ(1)
005900030822     D wGiroDft        s              1  0 INZ(*zeros)
006000010330     D*------------------
006100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200010330     D*------------------
006300010330     D WLBDA8          DS                  INZ
006400010330     D  G08DAT                 1      8  0
006500010330     D  G08INV                 9     16  0
006600010330     D  G08ERR                17     17
006700010330     D  G08TGI                18     22  0
006800031201     D*------------------
006900031201     D* Costanti
007000031201     D*------------------
007100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007300010330
007400010330
007500010330
007600990915     C                   time                    wrkdata
007700990915     C                   time                    wrkora
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100010601     C*
008200010601     C                   exsr      opeini
008300010605     C                   exsr      rwvab
008400010601     C*
008500010601     C                   seton                                        lr
008600010601
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010601     C*--------------------------------------------------------
009300010601     C     PREELA        BEGSR
009400010601     C*
009500010601     C* SE OCCORRE SPEDIRE IN FILIALE
009600010601     C                   if        invfil <> *zeros and
009700010601     C                             flgGiro = '0'
009800010601     C*
009900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010000010601     C                   eval      flgGiro = '1'
010100000724     C*
010200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400010601     C     invfil        chain     azorg01l
010500000724     C                   if        %found
010600000724     C                   movel(p)  CMD(1)        depcmd
010700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010800000724     C*
010900000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011000011113     C                   Z-ADD     150           LENGH            15 5
011100000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000724     C                   PARM                    depcmd
011300000724     C                   PARM                    LENGH
011400000724     C*
011500000724     C                   endif
011600000616     C*
011700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011800000616     C                   movel(p)  CMD(2)        depcmd
011900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012000000616     C*
012100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012200011113     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    depcmd
012500000616     C                   PARM                    LENGH
012600000613     C*
012700030715     C                   if        not %open(titvxlps)
012800030715     C                   open      titvxlps
012900030715     C                   except    testdett
013000000613     C                   endif
013100010601     C*
013200010601     C                   endif
013300010601     C*
013400010601     C                   ENDSR
013500010601     C***
013600010601
013700010601
013800010601
013900010601
014000010601
014100010601
014200010601
014300010601     C*--------------------------------------------------------
014400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014500010601     C*--------------------------------------------------------
014600010601     C     ENDELA        BEGSR
014700990915     C*
014800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014900000613     C*
015000030715     C                   if        %open(titvxlps)
015100000616     C                   except    findett
015200030715     C                   close     titvxlps
015300000613     C                   endif
015400000616     C*
015500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015600011113     C                   Z-ADD     150           LENGH            15 5
015700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015800000616     C                   PARM                    CMD(3)
015900000616     C                   PARM                    LENGH
016000000616     C*
016100010601     C                   ENDSR
016200010601     C***
016300010601
016400010601
016500010601
016600010601
016700000613
016800000613     C*--------------------------------------------------------
016900000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017000000613     C*--------------------------------------------------------
017100000621     C     STPR          BEGSR
017200000613     C*
017300030715     C                   if        not %open(titvxlp)
017400030715     C   50              open      titvxlp
017500990915     C                   endif
017600990915     C*
017700030715     C   50              except    riepilogo
017800990915     C*
017900030715     C                   if        %open(titvxlp)
018000030715     C   50              close     titvxlp
018100990914     C                   endif
018200000613     C*
018300000613     C                   ENDSR
018400000613     C***
018500000613
018600010330
018700010601
018800010601
018900010601
019000010330     C*--------------------------------------------------------
019100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019200010330     C*--------------------------------------------------------
019300010605     C     RWVAB         BEGSR
019400010330     C*
019500010330     C                   if        not %open(tivin00r)
019600010330     C                   open      tivin00r
019700010330     C                   endif
019800021025     C                   if        not %open(fivabwwr)
019900021025     C                   open      fivabwwr
020000010330     C                   endif
020100010330     C*
020200010604     C                   clear                   §CTROK
020300010604     C                   clear                   §CTRMO
020400010604     C                   clear                   §CTRNO
020500010330     C*
020600030822     C                   do        *HIVAL
020700010330     C*
020800030822     C                   read      tivin00r                               70
020900010618     C*
021000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021100010618     C                   if        vinflg = '1'
021200010618     C                   eval      flgOk = '1'
021300010618     C                   endif
021400010618     C*
021500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021600040510     C                   z-add     *zeros        wLenVINDTA        4 0
021700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022100040510     C                   z-add     1             wFlgVINDTA
022200040510     C                   leave
022300040510     C                   endif
022400040510     C                   enddo
022500040510     C*
022600040218     C                   if        vindta > *blanks AND
022700040510     C                             wFlgVINDTA = 1
022800010330     C                   add       1             rrnum
022900010330     C*
023000010601     C                   if        *in70 = *off and
023100010330     C                             (vinflg = *blanks
023200010330     C                              or vinflg = '0'
023300010330     C                              or vinflg = '2')
023400010330     C*
023500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023600010711     C                   if        vinflg = *blanks or vinflg = '0'
023700010711     C                   clear                   vinmsg
023800010711     C                   endif
023900010601     C*
024000010330     C                   exsr      inzvar
024100010605     C                   exsr      impvab
024200010601     C*
024300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024400010605     C                   eval      depfil = VABLNP
024500010601     C                   exsr      repfil
024600010601     C                   if        depfil = invfil
024700021025     C                   if        vlrpoi = 999
024800030822     C                   move(P)   invfil        VABFGS
024900021025     C                   else
025000030822     C                   move(P)   vlrpoi        VABFGS
025100021025     C                   endif
025200010601     C*
025300010601     C                   exsr      PREELA
025400010601     C*
025500010604     C* Ebbene...
025600010604     C*
025700030822     C  N31              add       1             §CTROK            7 0
025800030822     C   32              add       1             §CTRMO            7 0
025900030822     C   31              add       1             §CTRNO            7 0
026000030822     C                   if        wGiro = 2
026100030822     C  N31              write     FIVAB000
026200030822     C                   endif
026300010604     C*
026400010604     C                   if        *in31 = *off and
026500010604     C                             *in32 = *off
026600010604     C                   eval      vinflg = '1'
026700010604     C                   else
026800010604     C                   eval      recko = vindta
026900010604     C                   except    rigadett
027000010604     C                   eval      vinflg = '2'
027100010604     C                   endif
027200010604     C                   endif
027300010601     C*
027400010604     C                   endif
027500010604     C*
027600010330     C                   else
027700010330     C                   eval      vinflg = '1'
027800010330     C                   endif
027900010601     C*
028000010601     C  N70              update    tivin000
028100010330     C*
028200030822     C  N70              enddo
028300010601     C
028400010601     C*
028500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028600010601     C                   if        cntNonEl = *zeros or
028700010601     C                             flgMulti = '0'
028800010330     C* Se non ci sono record con errori ...
028900010601     C                   if        §ctrno = 0 and
029000010604     C                             §ctrmo = 0 and
029100010601     C                             flgStato <> '2'
029200010330     C* ... restituisco esito OK.
029300010330     C                   eval      wrkesito = '0'
029400010330     C                   else
029500010330     C                   if        §ctrok > 0
029600010330     C                   eval      wrkesito = '1'
029700010330     C                   else
029800010615     C                   if        flgOk = '0'
029900010615     C                   eval      wrkesito = '2'
030000010615     C                   else
030100010615     C                   eval      wrkesito = '6'
030200010615     C                   endif
030300010330     C                   endif
030400010330     C                   endif
030500010601     C                   else
030600010601     C                   eval      wrkesito = '9'
030700010601     C                   endif
030800010330     C*
030900010330     C                   if        %open(tivin00r)
031000010330     C                   close     tivin00r
031100010330     C                   endif
031200021025     C                   if        %open(fivabwwr)
031300021025     C                   close     fivabwwr
031400010330     C                   endif
031500010601     C*
031600010601     C                   if        vlrpoi <> 999
031700010601     C                   eval      invfil = vlrpoi
031800010601     C                   endif
031900010330     C*
032000010330     C                   if        §ctrok > 0
032100010601     C                             and invfil > *zeros
032200010330     C                   exsr      invio
032300010330     C                   endif
032400010601     C*
032500010618     C                   if        flgGiro = '1'
032600010601     C                   exsr      endela
032700010618     C                   endif
032800010330     C*
032900010330     C                   ENDSR
033000010330     C***
033100010601
033200010601
033300010601
033400010330     C*----------------------------------------------------*
033500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033600010330     C*----------------------------------------------------*
033700010330     C     INZVAR        BEGSR
033800010330     C*
033900030822     C* Inizializzo variabili di wrk
034000020204     C                   Z-ADD     *zeros        Num5_0            5 0
034100030822     C*
034200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034300030822     C                   CLEAR                   FIVAB000
034400030822     C                   CLEAR                   SkSplitCSV
034500030822     C                   EVAL      i = 1
034600030822     C                   EVAL      posDa = *zeros
034700030822     C                   EVAL      posA  = *zeros
034800030822     C*
034900030822     C* Reimposto i valori di default
035000030822     C                   EXSR      DEFCAM
035100030822     C*
035200010330     C                   ENDSR
035300010330     C*----------------------------------------------------*
035400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035500010330     C*----------------------------------------------------*
035600010330     C     DEFCAM        BEGSR
035700030822     C*
035800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035900030715     C* e delimitatore testo.
036000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
036300050321     C*
036400050321     C* Compongo la sequenza separatore testo + separatore campo
036500050321     C                   EVAL      wOldSep1 = CharTXT + CharCSV
036600050321     C                   EVAL      wOldSep2 = CharCSV + CharTXT
036700050321     C                   EVAL      wNewSep1 = CharTXT + ';'
036800050321     C                   EVAL      wNewSep2 = ';' + CharTXT
036900030822     C*
037000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
037100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
037200030715     C                   SETON                                        50
037300030715     C                   ELSE
037400030715     C                   SETOFF                                       50
037500030715     C                   ENDIF
037600030822     C*
037700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
037800030822     C                   EVAL      posDaDft = 1
037900030822     C                   EVAL      posADft  = 0
038000030822     C                   EVAL      wGiroDft = 0
038100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
038200030822     C                             posDaDft > 0
038300030822     C*
038400030822     C* Gestisco il 1° giro
038500030822     C                   IF        wGiroDft = 0
038600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
038700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
038800030822     C* Incremento il contatore dei "giri"
038900030822     C                   EVAL      wGiroDft = 1
039000030822     C                   ELSE
039100030822     C                   EVAL      posDaDft = posADft
039200030822     C                   ENDIF
039300030822     C* Eseguo lo scan x trovare la fine del campo corrente
039400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
039500030822     C*
039600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
039700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
039800030822     C                   IF        posDaDft > 0 AND
039900030822     C                             posADft  > 0
040000030822     C* NCL
040100030822     C                   IF        %subst(
040200030822     C                             %subst(vlrppt:posDaDft+1:
040300030822     C                             posADft-posDaDft-1):1:3)
040400030822     C                             = 'NCL'
040500030822     C                   EVAL      PiStr=%trim(%subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040800030822     C                   EXSR      CHKNUM
040900030822     C                   IF        PiInt=*on
041000030822     C                   Z-ADD     PiVal         VABNCL
041100030822     C                   ENDIF
041200030822     C                   ENDIF
041300030822     C* CCM
041400030822     C                   IF        %subst(
041500030822     C                             %subst(vlrppt:posDaDft+1:
041600030822     C                             posADft-posDaDft-1):1:3)
041700030822     C                             = 'CCM'
041800030822     C                   EVAL      PiStr=%trim(%subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042100030822     C                   EXSR      CHKNUM
042200030822     C                   IF        PiInt=*on
042300030822     C                   Z-ADD     PiVal         VABCCM
042400030822     C                   ENDIF
042500030822     C                   ENDIF
042600030822     C* LNP
042700030822     C                   IF        %subst(
042800030822     C                             %subst(vlrppt:posDaDft+1:
042900030822     C                             posADft-posDaDft-1):1:3)
043000030822     C                             = 'LNP'
043100030822     C                   EVAL      PiStr=%trim(%subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043400030822     C                   EXSR      CHKNUM
043500030822     C                   IF        PiInt=*on
043600030822     C                   Z-ADD     PiVal         VABLNP
043700030822     C                   ENDIF
043800030822     C                   ENDIF
043900030822     C* NRS
044000030822     C                   IF        %subst(
044100030822     C                             %subst(vlrppt:posDaDft+1:
044200030822     C                             posADft-posDaDft-1):1:3)
044300030822     C                             = 'NRS'
044400030822     C                   EVAL      PiStr=%trim(%subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044700030822     C                   EXSR      CHKNUM
044800030822     C                   IF        PiInt=*on
044900030822     C                   Z-ADD     PiVal         VABNRS
045000030822     C                   ENDIF
045100030822     C                   ENDIF
045200030822     C* CTR
045300030822     C                   IF        %subst(
045400030822     C                             %subst(vlrppt:posDaDft+1:
045500030822     C                             posADft-posDaDft-1):1:3)
045600030822     C                             = 'CTR'
045700030822     C                   EVAL      PiStr=%trim(%subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046000030822     C                   EXSR      CHKNUM
046100030822     C                   IF        PiInt=*on
046200030822     C                   Z-ADD     PiVal         VABCTR
046300030822     C                   ENDIF
046400030822     C                   ENDIF
046500030822     C* PKB
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900030822     C                             = 'PKB'
047000030822     C                   EVAL      PiStr=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   EXSR      CHKNUM
047400030822     C                   IF        PiNum=*on
047500030822     C                   Z-ADD     PiVal         VABPKB
047600030822     C                   ENDIF
047700030822     C                   ENDIF
047800030822     C* VLB
047900030822     C                   IF        %subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):1:3)
048200030822     C                             = 'VLB'
048300030822     C                   EVAL      PiStr=%trim(%subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048600030822     C                   EXSR      CHKNUM
048700030822     C                   IF        PiNum=*on
048800030822     C                   Z-ADD     PiVal         VABVLB
048900030822     C                   ENDIF
049000030822     C                   ENDIF
049100030822     C* QFT
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'QFT'
049600030822     C                   EVAL      PiStr=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   EXSR      CHKNUM
050000030822     C                   IF        PiNum=*on
050100030822     C                   Z-ADD     PiVal         VABQFT
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C* CBO
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'CBO'
050900030822     C                   EVAL      VABCBO=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   ENDIF
051300030822     C* TSP
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'TSP'
051800030822     C                   EVAL      VABTSP=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   ENDIF
052200030822     C* VAS
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'VAS'
052700030822     C                   EVAL      VABVAS=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   ENDIF
053100030822     C* VCA
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'VCA'
053600030822     C                   EVAL      VABVCA=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   ENDIF
054000030822     C* GCA
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'GCA'
054500030822     C                   EVAL      VABGCA=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   ENDIF
054900030822     C* CTM
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'CTM'
055400030822     C                   EVAL      VABCTM=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   ENDIF
055800030822     C* FFD
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'FFD'
056300030822     C                   EVAL      VABFFD=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* VAD
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'VAD'
057200030822     C                   EVAL      VABVAD=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* GMA
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'GMA'
058100030822     C                   EVAL      VABGMA=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* GGA
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'GGA'
059000030822     C                   EVAL      VABGGA=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* GVA
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'GVA'
059900030822     C                   EVAL      VABGVA=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* TC1
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'TC1'
060800030822     C                   EVAL      VABTC1=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* TC2
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'TC2'
061700030822     C                   EVAL      VABTC2=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* ...
062200030822     C                   ENDIF
062300030822     C                   ENDDO
062400020204     C*
062500010330     C                   ENDSR
062600010607     C*----------------------------------------------------*
062700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
062800010607     C*----------------------------------------------------*
062900010607     C     IMPVAB        BEGSR
063000010607     C*
063100010607     C                   SETOFF                                       3132
063200030515     C*
063300010607     C                   MOVEL     datcor        VABAAS
063400010607     C                   MOVE      datcor        VABMGS
063500050321     C*
063600050321     C* Dato che c'è 1 conflitto tra separatore campo e carattere ',' nell'indirizzo dest.
063700050321     C* => sostituisco quello campi
063800050321     C                   EVAL      CharCSV = ';'
063900050321     C*
064000050321     C* 1° Giro x fine separatore testo + separatore campo (",)
064100050321     C                   Z-ADD     1             wj                5 0
064200050321     C                   DOW       wj <= %len(vindta) - %len(wOldSep1)
064300050321     C                   IF        %subst(vindta:wj:2) = wOldSep1
064400050321     C                   EVAL      %subst(vindta:wj:2) = wNewSep1
064500050321     C                   ENDIF
064600050321     C                   ADD       1             wj
064700050321     C                   ENDDO
064800050321     C*
064900050321     C* 2° Giro x separatore campo + inizio separatore testo (,")
065000050321     C                   Z-ADD     1             wj                5 0
065100050321     C                   DOW       wj <= %len(vindta) - %len(wOldSep2)
065200050321     C                   IF        %subst(vindta:wj:2) = wOldSep2
065300050321     C                   EVAL      %subst(vindta:wj:2) = wNewSep2
065400050321     C                   ENDIF
065500050321     C                   ADD       1             wj
065600050321     C                   ENDDO
065700050321     C*
065800050321     C* 3° Giro x eliminare il separatore testo
065900050321     C                   Z-ADD     1             wj                5 0
066000050321     C                   DOW       wj <= %len(vindta) - 1
066100050321     C                   IF        %subst(vindta:wj:1) = CharTXT
066200050321     C                   EVAL      %subst(vindta:wj:1) = *blanks
066300050321     C                   ENDIF
066400050321     C                   ADD       1             wj
066500050321     C                   ENDDO
066600010607     C*
066700030715     C**********
066800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
066900030715     C**********
067000030715     C                   IF        wGiro = *zeros
067100040726     C*
067200040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
067300040726     C                   EVAL      depvinDTA = vinDTA
067400040726     C*
067500040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
067600040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
067700050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
067800050321     C                                      %trim(intcol(5))+%trim(intcol(6))+
067900050321     C                                      %trim(intcol(7))+%trim(intcol(8))+
068000050321     C                                      %trim(intcol(9))+%trim(intcol(10))
068100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
068200031201     C     minu:maiu     XLATE     vindta        vindta
068300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068400040324     C                   DOW       posDa <= %len(%trimr(vindta))
068500030715     C*
068600030715     C* Gestisco il 1° campo
068700030715     C                   IF        i = 1
068800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068900030715     C                   EVAL      posDa = 1
069000030715     C* Eseguo lo scan x trovare la fine del primo campo
069100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069200030715     C* A questo punto "estraggo" il campo corrente
069300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069400030715     C                                                    (posA-posDa))
069500030715     C* X i campi successivi al 1°
069600030715     C                   ELSE
069700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069800030715     C                   EVAL      posDa = posA + 1
069900030715     C* Eseguo lo scan x trovare la fine del campo corrente
070000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070100030715     C* Gestisco l'ultimo campo
070200030715     C                   IF        posA = *zeros
070300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070400030715     C* A questo punto "estraggo" il campo corrente
070500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070600030715     C                                                    (posA-posDa))
070700030715     C                   LEAVE
070800030715     C                   ELSE
070900030715     C* A questo punto "estraggo" il campo corrente
071000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071100030715     C                                                    (posA-posDa))
071200030715     C                   ENDIF
071300030715     C                   ENDIF
071400030715     C* Incremento il contatore d campo
071500030715     C                   EVAL      i = i +1
071600030715     C                   ENDDO
071700030715     C*
071800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071900030715     C* delimitazione testo
072000030715     C                   EVAL      i = 1
072100030715     C                   DOW       i < %elem(SkSplitFLD)
072200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
072300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072400030715     C                   EVAL      i = i + 1
072500030715     C                   ENDDO
072600030715     C                   EVAL      wGiro = 1
072700040726     C* Ripristino la 1° riga originale
072800040726     C                   EVAL      vinDTA = depvinDTA
072900030715     C                   ELSE
073000030715     C                   EVAL      wGiro = 2
073100031204     C**********
073200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
073300031204     C* d separatore campo
073400031204     C**********
073500040726     C                   EVAL      vindta = %trim(vindta)
073600040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073700040726     C                   EVAL      vindta = %subst(vindta:2)
073800040726     C                   ENDDO
073900031204     C*
074000031204     C                   Z-ADD     *zeros        lunghInput        4 0
074100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
074200040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074300040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074400040726     C                   EVAL      lunghInput = %len(%trim(vindta))
074500040726     C                   ENDDO
074600030715     C**********
074700030715     C* Effettuo lo split del campo dati d input
074800030715     C**********
074900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075000040324     C                   DOW       posDa <= %len(%trimr(vindta))
075100030715     C*
075200030715     C* Gestisco il 1° campo
075300030715     C                   IF        i = 1
075400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075500030715     C                   EVAL      posDa = 1
075600030715     C* Eseguo lo scan x trovare la fine del primo campo
075700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075800030715     C* A questo punto "estraggo" il campo corrente
075900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076000030715     C                                                    (posA-posDa))
076100030715     C* X i campi successivi al 1°
076200030715     C                   ELSE
076300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076400030715     C                   EVAL      posDa = posA + 1
076500031204     C* Verifico che nn vi sia il campo nn valorizzato
076600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076700031204     C* Se campo nn valorizzato skippo al prossimo
076800031204     C                   EVAL      SkSplitCSV(i) = *blanks
076900031204     C                   EVAL      posA  = posA + 1
077000031204     C                   ELSE
077100030715     C* Eseguo lo scan x trovare la fine del campo corrente
077200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077300030715     C* Gestisco l'ultimo campo
077400030715     C                   IF        posA = *zeros
077500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077600030715     C* A questo punto "estraggo" il campo corrente
077700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077800030715     C                                                    (posA-posDa))
077900030715     C                   LEAVE
078000030715     C                   ELSE
078100030715     C* A questo punto "estraggo" il campo corrente
078200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078300030715     C                                                    (posA-posDa))
078400030715     C                   ENDIF
078500031204     C                   ENDIF
078600030715     C                   ENDIF
078700030715     C* Incremento il contatore d campo
078800030715     C                   EVAL      i = i +1
078900030715     C                   ENDDO
079000030715     C*
079100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079200030715     C* delimitazione testo
079300030715     C                   EVAL      i = 1
079400030715     C                   DOW       i < %elem(SkSplitCSV)
079500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079700030715     C                   EVAL      i = i + 1
079800030715     C                   ENDDO
079900030715     C*
080000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080100030715     C                   EVAL      i = 1
080200030715     C                   DOW       i < %elem(SkSplitFLD)
080300030715     C*
080400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080600030715     C                             SkSplitCSV(i) <> *blanks
080700030721     C*** ATB
080800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
080900030721     C                   EVAL      VABATB = SkSplitCSV(i)
081000030721     C                   ENDIF
081100030715     C*** RSD
081200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081600030715     C* ==
081700030715     C                   ENDIF
081800030721     C*** RD2
081900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
082000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
082100030721     C                   ENDIF
082200030715     C*** IND
082300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082400030715     C                   EVAL      VABIND = SkSplitCSV(i)
082500030715     C                   ENDIF
082600030715     C*** LOD
082700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
082900030715     C                   ENDIF
083000030715     C*** PRD
083100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083300030715     C                   ENDIF
083400030721     C*** NZD
083500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083700030721     C                   ENDIF
083800030715     C*** RMA
083900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
084000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
084100030715     C                   ENDIF
084200030715     C*** NOT
084300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
084500030715     C                   ENDIF
084600030715     C*** NT2
084700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
084900030715     C                   ENDIF
085000030715     C*** VCA
085100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085300030715     C                   ENDIF
085400030715     C*** VAS
085500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085700030715     C                   ENDIF
085800030715     C*** FFD
085900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
086000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
086100030715     C                   ENDIF
086200030715     C*** CBO
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086400030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C
086700030721     C*** GC1
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
086900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** GC2
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
087300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** TSP
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** NAS
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
088100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** TIC
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GCA
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** XCO
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
089300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** CTM
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** TCR
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
090100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** CTS
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** FTM
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** VAD
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
091300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** GMA
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** GGA
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
092100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** GVA
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** TC1
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** TC2
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
093300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** SCL
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** RMO
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
094100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** NMO
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*
094800030721     C* Reperisco quindi i campi numerici...
094900030721     C*** CAD
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
095100030721     C                   EVAL      PiStr=SkSplitCSV(i)
095200030721     C                   EXSR      CHKNUM
095300030721     C                   IF        PiInt=*on
095400030721     C                   Z-ADD     PiVal         Num5_0
095500030721     C                   MOVEL(p)  Num5_0        VABCAD
095600030721     C                   ELSE
095700030721     C                   SETON                                        32
095800030721     C                   EVAL      VABCAD = *zeros
095900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096000030721     C                             + ' ' + 'VABCAD'
096100030721     C                   ENDIF
096200030721     C                   ENDIF
096300030721     C*** RMN
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096500030721     C                   EVAL      PiStr=SkSplitCSV(i)
096600030721     C                   EXSR      CHKNUM
096700030721     C                   IF        PiInt=*on
096800030721     C                   Z-ADD     PiVal         VABRMN
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABRMN = 1
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABRMN'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** NSP
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000040726     C                   IF        PiInt=*on
098100040726     C                   Z-ADD     PiVal         VABNSP
098200040802     C*                  IF        PiVal>9999999
098300040802     C*                  SETON                                        32
098400040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
098500040802     C*                            + ' ' + 'VABNSP'
098600040802     C*                  ENDIF
098700030721     C                   ELSE
098800030721     C                   SETON                                        32
098900030721     C                   EVAL      VABNSP = *zeros
099000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099100030721     C                             + ' ' + 'VABNSP'
099200030721     C                   ENDIF
099300030721     C                   ENDIF
099400030721     C*** NCL
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099600030721     C                   EVAL      PiStr=SkSplitCSV(i)
099700030721     C                   EXSR      CHKNUM
099800030721     C                   IF        PiInt=*on
099900030721     C                   Z-ADD     PiVal         VABNCL
100000030721     C                   ELSE
100100030721     C                   SETON                                        32
100200030721     C                   EVAL      VABNCL = *zeros
100300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100400030721     C                             + ' ' + 'VABNCL'
100500030721     C                   ENDIF
100600030721     C                   ENDIF
100700030721     C*** PKB
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100900030721     C                   EVAL      PiStr=SkSplitCSV(i)
101000030721     C                   EXSR      CHKNUM
101100030721     C                   IF        PiNum=*on
101200030721     C                   Z-ADD     PiVal         VABPKB
101300030721     C                   ELSE
101400030721     C                   SETON                                        32
101500030721     C                   EVAL      VABPKB = *zeros
101600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101700030721     C                             + ' ' + 'VABPKB'
101800030721     C                   ENDIF
101900030721     C                   ENDIF
102000050505     C*** CAS
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102200030721     C                   EVAL      PiStr=SkSplitCSV(i)
102300030721     C                   EXSR      CHKNUM
102400030721     C                   IF        PiNum=*on
102500030721     C                   Z-ADD     PiVal         VABCAS
102600030721     C                   ELSE
102700030721     C                   SETON                                        32
102800030721     C                   EVAL      VABCAS = *zeros
102900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103000030721     C                             + ' ' + 'VABCAS'
103100030721     C                   ENDIF
103200030721     C                   ENDIF
103300030721     C*** CCM
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103500030721     C                   EVAL      PiStr=SkSplitCSV(i)
103600030721     C                   EXSR      CHKNUM
103700030721     C                   IF        PiInt=*on      AND
103800030721     C                             PiVal<=9999999 AND
103900030721     C                             PiVal>*zeros
104000030721     C                   Z-ADD     PiVal         VABCCM
104100030721     C                   ELSE
104200030721     C                   SETON                                        32
104300030721     C                   EVAL      VABCCM = *zeros
104400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104500030721     C                             + ' ' + 'VABCCM'
104600030721     C                   ENDIF
104700030721     C                   ENDIF
104800030721     C*** LNP
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
105000030721     C                   EVAL      PiStr=SkSplitCSV(i)
105100030721     C                   EXSR      CHKNUM
105200030721     C                   IF        PiInt=*on    AND
105300030721     C                             PiVal<=999   AND
105400030721     C                             PiVal>*zeros
105500030721     C                   Z-ADD     PiVal         VABLNP
105600030721     C                   ELSE
105700030721     C                   SETON                                        32
105800030721     C                   EVAL      VABLNP = *zeros
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000030721     C                             + ' ' + 'VABLNP'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** NRS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiInt=*on AND
106800030721     C                             PiVal<=99
106900030721     C                   Z-ADD     PiVal         VABNRS
107000030721     C                   ELSE
107100030721     C                   SETON                                        32
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABNRS'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** LNA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiInt=*on    AND
108100030721     C                             PiVal<=999   AND
108200030721     C                             PiVal>*zeros
108300030721     C                   Z-ADD     PiVal         VABLNA
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      VABLNA = *zeros
108700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108800030721     C                             + ' ' + 'VABLNA'
108900030721     C                   ENDIF
109000030721     C                   ENDIF
109100030721     C*** CTR
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiInt=*on  AND
109600030721     C                             PiVal<=999
109700030721     C                   Z-ADD     PiVal         VABCTR
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100030721     C                             + ' ' + 'VABCTR'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** IAS
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiNum=*on
110900030721     C                   Z-ADD     PiVal         VABIAS
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABIAS'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** VLB
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiNum=*on
112100030721     C                   Z-ADD     PiVal         VABVLB
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500030721     C                             + ' ' + 'VABVLB'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** QFT
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiNum=*on
113300030721     C                   Z-ADD     PiVal         VABQFT
113400030721     C                   ELSE
113500030721     C                   SETON                                        32
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABQFT'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** NCD
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on      AND
114500030721     C                             PiVal<=9999999
114600030721     C                   Z-ADD     PiVal         VABNCD
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABNCD'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** NCA
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on      AND
115800030721     C                             PiVal<=9999999
115900030721     C                   Z-ADD     PiVal         VABNCA
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABNCA'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** ZNC
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on AND
117100030721     C                             PiVal<=99
117200030721     C                   Z-ADD     PiVal         VABZNC
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABZNC'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** DCR
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on       AND
118400030721     C                             PiVal<=99999999
118500030721     C                   Z-ADD     PiVal         VABDCR
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABDCR'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** HCR
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on   AND
119700030721     C                             PiVal<=9999
119800030721     C                   Z-ADD     PiVal         VABHCR
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABHCR'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** VMD
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiNum=*on
121000030721     C                   Z-ADD     PiVal         VABVMD
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABVMD'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** ANT
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiInt=*on        AND
122200030721     C                             PiVal<=999999999
122300030721     C                   Z-ADD     PiVal         VABANT
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABANT'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CMO
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on
123500030721     C                   Z-ADD     PiVal         Num5_0
123600030721     C                   MOVEL(p)  Num5_0        VABCMO
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      VABCMO = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABCMO'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030715     C*
124500030715     C                   ENDIF
124600030715     C                   EVAL      i = i + 1
124700030715     C                   ENDDO
124800040726     C*
124900040726     C* Considerazioni personalizzate x il traduttore corrente
125000050505     C                   MOVEL     VABRMN        VABRMA
125100050506     C                   MOVE(P)   VABRMN        VABNSP
125200050505     C                   EVAL      VABPKB = VABPKB * VABNCL
125300040726     C*
125400040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
125500040726     C                   IF        VABNSP = *zeros
125600040726     C                   SETON                                        31
125700040726     C                   ENDIF
125800040726     C*
125900040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
126000040726     C                   IF        VABLNP = *zeros
126100040726     C                   SETON                                        31
126200040726     C                   ENDIF
126300040726     C*
126400040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
126500040726     C* d valorizzarli sempre entrambi
126600040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
126700040726     C                   Z-ADD     VABNSP        VABRMN
126800040726     C                   ENDIF
126900040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
127000040726     C                   MOVEL     VABRMN        VABRMA
127100040726     C                   ENDIF
127200040726     C*
127300040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
127400040726     C                   IF        VABCAS > *zeros
127500040726     C                   IF        VABVCA = *blanks
127600040726     C                   EVAL      VABVCA = 'EUR'
127700040726     C                   ENDIF
127800040726     C                   IF        VABCBO = '1'
127900040726     C                   EVAL      VABCBO = '4'
128000040726     C                   ENDIF
128100040726     C                   IF        VABCBO = '2'
128200040726     C                   EVAL      VABCBO = '6'
128300040726     C                   ENDIF
128400040726     C                   ENDIF
128500020204     C*
128600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
128700020204     C                   EXSR      CHKIMPDIV
128800030715     C                   ENDIF
128900020204     C*
129000010607     C                   ENDSR
129100010607     C*----------------------------------------------------*
129200010601
129300020204
129400020204     C*----------------------------------------------------*
129500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
129600020204     C*----------------------------------------------------*
129700020204     C     CHKIMPDIV     BEGSR
129800020204     C*
129900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130000020204     C                   Z-ADD     *zeros        wrkDec            9 9
130100020204     C*
130200020204     C* Come prima cosa effettuo considerazioni sulla divisa
130300020204     C                   IF        vabIAS > *zeros
130400020204     C                   IF        vabVAS <> 'EUR'
130500020204     C                   EVAL      vabVAS =  'ITL'
130600020204     C                   ENDIF
130700020204     C                   ENDIF
130800020204     C*
130900020204     C                   IF        vabCAS > *zeros
131000020204     C                   IF        vabVCA <> 'EUR'
131100020204     C                   EVAL      vabVCA =  'ITL'
131200020204     C                   ENDIF
131300020204     C                   ENDIF
131400020204     C*
131500020204     C                   IF        vabVMD > *zeros
131600020204     C                   IF        vabVAD <> 'EUR'
131700020204     C                   EVAL      vabVAD =  'ITL'
131800020204     C                   ENDIF
131900020204     C                   ENDIF
132000020204     C*
132100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132200020204     C                   Z-ADD     vabIAS        wrkDec
132300020204     C                   IF        wrkDec > *zeros
132400020204     C                   IF        vabVAS = 'ITL'
132500020204     C                   EVAL      vabIAS = *zeros
132600020204     C                   ENDIF
132700020204     C                   ENDIF
132800020204     C*
132900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133000020204     C                   Z-ADD     vabCAS        wrkDec
133100020204     C                   IF        wrkDec > *zeros
133200020204     C                   IF        vabVCA = 'ITL'
133300020204     C                   EVAL      vabCAS = *zeros
133400020204     C                   ENDIF
133500020204     C                   ENDIF
133600020204     C*
133700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
133800020204     C                   Z-ADD     vabVMD        wrkDec
133900020204     C                   IF        wrkDec > *zeros
134000020204     C                   IF        vabVAD = 'ITL'
134100020204     C                   EVAL      vabVMD = *zeros
134200020204     C                   ENDIF
134300020204     C                   ENDIF
134400020204     C*
134500020204     C                   ENDSR
134600020204     C***
134700020204
134800010330
134900010330
135000010330     C*----------------------------------------------------*
135100010330     C*  CONTROLLO NUMERICITA' CAMPI
135200010330     C*----------------------------------------------------*
135300010330     C     CHKNUM        BEGSR
135400010330     C*
135500010606     C                   IF        PiDecChr = *blanks
135600030715     C                   EVAL      PiDecChr = CharNUM
135700010606     C                   ENDIF
135800010606     C*
135900010606     C                   CALL(e)   'ISNUMERIC'
136000010330     C                   PARM                    PiStr            30
136100010606     C                   PARM                    PiDecChr          1
136200010330     C                   PARM      *ZEROS        PiVal            30 9
136300010330     C                   PARM      '0'           PiInt             1
136400010330     C                   PARM      '0'           PiNum             1
136500010330     C                   IF        %error
136600010606     C                   EVAL      PiNum=*off
136700010330     C                   ENDIF
136800010330     C*
136900010330     C                   ENDSR
137000010330     C***
137100010330
137200010601
137300010601
137400010601
137500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
137600010601     C     repfil        BEGSR
137700010601     C*
137800010601     C                   if        invfil = *zeros and
137900010601     C                             depfil > *zeros and
138000010629     C                             (vinflg = *blanks or
138100010629     C                              vinflg = *zeros)
138200010601     C
138300010601     C                   eval      invfil = depfil
138400010601     C                   endif
138500010601     C*
138600010601     C                   if        depfil <> invfil and
138700010601     C                             invfil > *zeros
138800010601     C                   eval      flgMulti = '1'
138900010601     C                   if        vinflg = *blanks
139000010601     C                   add       1             cntNonEl
139100010601     C                   endif
139200010601     C                   endif
139300010601     C*
139400010601     C                   if        vinflg = '2'
139500010601     C                   eval      flgStato = '2'
139600010601     C                   endif
139700010601     C*
139800010601     C                   ENDSR
139900010601     C***
140000010601
140100010601
140200010601
140300010330
140400010330
140500010330
140600990920      /TITLE Invio dei dati al punto operativo.
140700000613     C     invio         BEGSR
140800990920     C*
140900990920     C                   reset                   dscmz
141000010601     C                   move      invfil        cmzdst
141100990920     C                   eval      cmzfld = vlrfou
141200990920     C                   eval      cmzmbd = vlrhdl
141300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
141400000710     C                   if        prmfir = *blanks
141500021025     C                   eval      cmzfla = 'FIVAB00F'
141600021025     C                   eval      cmzmba = 'FIVAB00F'
141700000710     C                   else
141800000710     C                   eval      cmzfla = prmfir
141900000710     C                   eval      cmzmba = prmfir
142000000710     C                   endif
142100990920     C                   eval      cmznrr = *zeros
142200990920     C                   move      §ctrok        cmznrr
142300021018     C                   eval      cmzlba = vlrfl1
142400990920     C                   call(e)   'TIS711C'
142500990920     C                   parm                    dscmz
142600990921     C                   parm      *blanks       esito
142700990920     C                   if        %error
142800990920     C                             or cmzerr = '1'
142900990921     C                             or esito  = '1'
143000000710     C                   eval      wrkesito = '3'
143100990920     C                   endif
143200990920     C*
143300000613     C                   ENDSR
143400990910
143500010601
143600010601
143700010601
143800010601
143900010601      /TITLE Invio dei dati al punto operativo.
144000010601     C     opeini        BEGSR
144100010601     C*
144200010601     C* Inizializzo flag e contatori operativi
144300010601     C                   movel     '0'           flgGiro           1
144400010601     C                   movel     '0'           flgMulti          1
144500010601     C                   movel     '1'           flgStato          1
144600010615     C                   movel     '0'           flgOk             1
144700010601     C                   z-add     *zeros        cntNonEl         10 0
144800010601     C                   z-add     *zeros        depfil            3 0
144900010601     C                   z-add     *zeros        invfil            3 0
145000010601     C*
145100010601     C                   ENDSR
145200010601     C***
145300010601
145400010601
145500050421
145600050421
145700050421     C     *pssr         BEGSR
145800050421     C*
145900050421     C                   eval      wrkesito = '2'
146000050421     C*
146100050421     C                   ENDSR     '*CANCL'
146200050421     C***
146300050421
146400010601
146500010330
146600010330
146700000613     C     *inzsr        BEGSR
146800990910     C*
146900990910     C     *entry        plist
147000990920     C                   parm                    tivlrds
147100990921     C                   parm      wrkesito      esito
147200000724     C                   parm                    prmlit
147300000710     C                   parm                    prmfir
147400050505     C*
147500050505     C* CALCOLA LA DATA CORRENTE
147600050505     C                   time                    wn14             14 0
147700050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
147800050505     C                   z-add     wn8           g08dat
147900050505     C                   z-add     *zeros        g08inv
148000050505     C                   movel     '0'           g08err
148100050505     C                   call      'XSRDA8'
148200050505     C                   parm                    wlbda8
148300050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
148400000613     C*
148500000613     C                   ENDSR
148600000613     C***
148700990908
148800030715     Otitvxlp   E            riepilogo         2
148900990915     O                                              'Upload via Internet'
149000990915     O                                           +1 'Traduzione TIVIN00R -'
149100021025     O                                           +1 'FIVABWWR'
149200990915     O                                           +1 'Testate bolle'
149300990915     O          E            riepilogo   2
149400990915     O                       wrkdata
149500990915     O                       wrkora              +1
149600990915     O                       procname            +1
149700990915     O          E            riepilogo   2
149800990915     O                                              'Cliente..................:'
149900010605     O                       VABCCM        z     +1
150000990915     O          E            riepilogo   2
150100990920     O                                              'Riferimento Strategi.....:'
150200990920     O                       vlrhdl              +1
150300990915     O          E            riepilogo   2
150400990915     O                                              'Giusti...................:'
150500971022     O                       §CTROK        2   +  1
150600990915     O          E            riepilogo   2
150700990915     O                                              'Sbagliati e corretti.....:'
150800971022     O                       §CTRMO        2   +  1
150900990915     O          E            riepilogo   2
151000990915     O                                              'Sbagliati e scartati.....:'
151100971022     O                       §CTRNO        2   +  1
151200000613
151300030715     Otitvxlps  E            testdett          2
151400000613     O                                              'Upload via Internet'
151500000613     O                                           +1 'Traduzione TIVIN00R -'
151600021025     O                                           +1 'FIVABWWR'
151700000613     O                                           +1 'Testate bolle'
151800000616     O          E            testdett    3
151900000613     O                                           +2 'N° rec'
152000000613     O                                           +3 'Anteprima contenuto'
152100000616     O          E            rigadett    2
152200000613     O                       rrnum               +2
152300000621     O                       recko               +3
152400000616     O          E            findett     2
152500000613     O                       wrkdata
152600000613     O                       wrkora              +1
152700000613     O                       procname            +1
152800000616     O          E            findett     2
152900000613     O                                              'Cliente..................:'
153000010605     O                       VABCCM        z     +1
153100000616     O          E            findett     2
153200000613     O                                              'Riferimento Strategi.....:'
153300000613     O                       vlrhdl              +1
153400000616     O          E            findett     2
153500000613     O                                              'Giusti...................:'
153600000613     O                       §CTROK        2   +  1
153700000616     O          E            findett     2
153800000613     O                                              'Sbagliati e corretti.....:'
153900000613     O                       §CTRMO        2   +  1
154000000616     O          E            findett     2
154100000613     O                                              'Sbagliati e scartati.....:'
154200000613     O                       §CTRNO        2   +  1
154300000613** CMD - COMANDI CL
154400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
154500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
154600030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
154700010606
154800000613
154900050505** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155000050505VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABRMN
155100050505
155200050505
155300050505
155400050505
155500050505
155600050505
155700050505
155800050505
155900050505
