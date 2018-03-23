000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
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
007400010605     C                   exsr      rwvab
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
018200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018300010330     C*--------------------------------------------------------
018400010605     C     RWVAB         BEGSR
018500010330     C*
018600010330     C                   if        not %open(tivin00r)
018700010330     C                   open      tivin00r
018800010330     C                   endif
018900021025     C                   if        not %open(fivabwwr)
019000021025     C                   open      fivabwwr
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
023200010605     C                   exsr      impvab
023300010601     C*
023400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500010605     C                   eval      depfil = VABLNP
023600010601     C                   exsr      repfil
023700060331     C                   if        VABFGS = *zeros
023800010601     C                   if        depfil = invfil
023900021025     C                   if        vlrpoi = 999
024000030822     C                   move(P)   invfil        VABFGS
024100021025     C                   else
024200030822     C                   move(P)   vlrpoi        VABFGS
024300021025     C                   endif
024400050113     C                   endif
024500060331     C                   endif
024600010601     C*
024700010601     C                   exsr      PREELA
024800010601     C*
024900010604     C* Ebbene...
025000010604     C*
025100030822     C  N31              add       1             §CTROK            7 0
025200030822     C   32              add       1             §CTRMO            7 0
025300030822     C   31              add       1             §CTRNO            7 0
025400030822     C                   if        wGiro = 2
025500030822     C  N31              write     FIVAB000
025600030822     C                   endif
025700010604     C*
025800010604     C                   if        *in31 = *off and
025900010604     C                             *in32 = *off
026000010604     C                   eval      vinflg = '1'
026100010604     C                   else
026200010604     C                   eval      recko = vindta
026300070102     C                   if        %open(titvxlps)
026400070102     C                   except    rigadett
026500070102     C                   endif
026600010604     C                   eval      vinflg = '2'
026700010604     C                   endif
026800010601     C*
026900010604     C                   endif
027000010604     C*
027100010330     C                   else
027200010330     C                   eval      vinflg = '1'
027300010330     C                   endif
027400010601     C*
027500010601     C  N70              update    tivin000
027600010330     C*
027700030822     C  N70              enddo
027800010601     C
027900010601     C*
028000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028100010601     C                   if        cntNonEl = *zeros or
028200010601     C                             flgMulti = '0'
028300010330     C* Se non ci sono record con errori ...
028400010601     C                   if        §ctrno = 0 and
028500010604     C                             §ctrmo = 0 and
028600010601     C                             flgStato <> '2'
028700010330     C* ... restituisco esito OK.
028800010330     C                   eval      wrkesito = '0'
028900010330     C                   else
029000010330     C                   if        §ctrok > 0
029100010330     C                   eval      wrkesito = '1'
029200010330     C                   else
029300010615     C                   if        flgOk = '0'
029400010615     C                   eval      wrkesito = '2'
029500010615     C                   else
029600010615     C                   eval      wrkesito = '6'
029700010615     C                   endif
029800010330     C                   endif
029900010330     C                   endif
030000010601     C                   else
030100010601     C                   eval      wrkesito = '9'
030200010601     C                   endif
030300010330     C*
030400010330     C                   if        %open(tivin00r)
030500010330     C                   close     tivin00r
030600010330     C                   endif
030700021025     C                   if        %open(fivabwwr)
030800021025     C                   close     fivabwwr
030900010330     C                   endif
031000010601     C*
031100010601     C                   if        vlrpoi <> 999
031200010601     C                   eval      invfil = vlrpoi
031300010601     C                   endif
031400010330     C*
031500010330     C                   if        §ctrok > 0
031600010601     C                             and invfil > *zeros
031700010330     C                   exsr      invio
031800010330     C                   endif
031900010601     C*
032000010618     C                   if        flgGiro = '1'
032100010601     C                   exsr      endela
032200010618     C                   endif
032300010330     C*
032400010330     C                   ENDSR
032500010330     C***
032600010601
032700010601
032800010601
032900010330     C*----------------------------------------------------*
033000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033100010330     C*----------------------------------------------------*
033200010330     C     INZVAR        BEGSR
033300010330     C*
033400030822     C* Inizializzo variabili di wrk
033500020204     C                   Z-ADD     *zeros        Num5_0            5 0
033600030822     C*
033700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
033800030822     C                   CLEAR                   FIVAB000
033900030822     C                   CLEAR                   SkSplitCSV
034000030822     C                   EVAL      i = 1
034100030822     C                   EVAL      posDa = *zeros
034200030822     C                   EVAL      posA  = *zeros
034300030822     C*
034400030822     C* Reimposto i valori di default
034500030822     C                   EXSR      DEFCAM
034600030822     C*
034700010330     C                   ENDSR
034800010330     C*----------------------------------------------------*
034900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035000010330     C*----------------------------------------------------*
035100010330     C     DEFCAM        BEGSR
035200030822     C*
035300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035400030715     C* e delimitatore testo.
035500080215     C***                EVAL      CharCSV = %subst(vlrppt:2:1)
035600080215     C                   EVAL      CharCSV = X'05'
035700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
035800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
035900030822     C*
036000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
036100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
036200030715     C                   SETON                                        50
036300030715     C                   ELSE
036400030715     C                   SETOFF                                       50
036500030715     C                   ENDIF
036600030822     C*
036700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
036800030822     C                   EVAL      posDaDft = 1
036900030822     C                   EVAL      posADft  = 0
037000030822     C                   EVAL      wGiroDft = 0
037100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
037200030822     C                             posDaDft > 0
037300030822     C*
037400030822     C* Gestisco il 1° giro
037500030822     C                   IF        wGiroDft = 0
037600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
037700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
037800030822     C* Incremento il contatore dei "giri"
037900030822     C                   EVAL      wGiroDft = 1
038000030822     C                   ELSE
038100030822     C                   EVAL      posDaDft = posADft
038200030822     C                   ENDIF
038300030822     C* Eseguo lo scan x trovare la fine del campo corrente
038400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
038500030822     C*
038600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
038700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
038800030822     C                   IF        posDaDft > 0 AND
038900030822     C                             posADft  > 0
039000060331     C* FGS
039100060331     C                   IF        %subst(
039200060331     C                             %subst(vlrppt:posDaDft+1:
039300060331     C                             posADft-posDaDft-1):1:3)
039400060331     C                             = 'FGS'
039500060331     C                   EVAL      PiStr=%trim(%subst(
039600060331     C                             %subst(vlrppt:posDaDft+1:
039700060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039800060331     C                   EXSR      CHKNUM
039900060331     C                   IF        PiInt=*on
040000060331     C                   Z-ADD     PiVal         VABFGS
040100060331     C                   ENDIF
040200060331     C                   ENDIF
040300030822     C* NCL
040400030822     C                   IF        %subst(
040500030822     C                             %subst(vlrppt:posDaDft+1:
040600030822     C                             posADft-posDaDft-1):1:3)
040700030822     C                             = 'NCL'
040800030822     C                   EVAL      PiStr=%trim(%subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041100030822     C                   EXSR      CHKNUM
041200030822     C                   IF        PiInt=*on
041300030822     C                   Z-ADD     PiVal         VABNCL
041400030822     C                   ENDIF
041500030822     C                   ENDIF
041600030822     C* CCM
041700030822     C                   IF        %subst(
041800030822     C                             %subst(vlrppt:posDaDft+1:
041900030822     C                             posADft-posDaDft-1):1:3)
042000030822     C                             = 'CCM'
042100030822     C                   EVAL      PiStr=%trim(%subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400030822     C                   EXSR      CHKNUM
042500030822     C                   IF        PiInt=*on
042600030822     C                   Z-ADD     PiVal         VABCCM
042700030822     C                   ENDIF
042800030822     C                   ENDIF
042900030822     C* LNP
043000030822     C                   IF        %subst(
043100030822     C                             %subst(vlrppt:posDaDft+1:
043200030822     C                             posADft-posDaDft-1):1:3)
043300030822     C                             = 'LNP'
043400030822     C                   EVAL      PiStr=%trim(%subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700030822     C                   EXSR      CHKNUM
043800030822     C                   IF        PiInt=*on
043900030822     C                   Z-ADD     PiVal         VABLNP
044000030822     C                   ENDIF
044100030822     C                   ENDIF
044200030822     C* NRS
044300030822     C                   IF        %subst(
044400030822     C                             %subst(vlrppt:posDaDft+1:
044500030822     C                             posADft-posDaDft-1):1:3)
044600030822     C                             = 'NRS'
044700030822     C                   EVAL      PiStr=%trim(%subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045000030822     C                   EXSR      CHKNUM
045100030822     C                   IF        PiInt=*on
045200030822     C                   Z-ADD     PiVal         VABNRS
045300030822     C                   ENDIF
045400030822     C                   ENDIF
045500030822     C* CTR
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'CTR'
046000030822     C                   EVAL      PiStr=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   EXSR      CHKNUM
046400030822     C                   IF        PiInt=*on
046500030822     C                   Z-ADD     PiVal         VABCTR
046600030822     C                   ENDIF
046700030822     C                   ENDIF
046800030822     C* PKB
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'PKB'
047300030822     C                   EVAL      PiStr=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   EXSR      CHKNUM
047700030822     C                   IF        PiNum=*on
047800030822     C                   Z-ADD     PiVal         VABPKB
047900030822     C                   ENDIF
048000030822     C                   ENDIF
048100030822     C* VLB
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'VLB'
048600030822     C                   EVAL      PiStr=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   EXSR      CHKNUM
049000030822     C                   IF        PiNum=*on
049100030822     C                   Z-ADD     PiVal         VABVLB
049200030822     C                   ENDIF
049300030822     C                   ENDIF
049400030822     C* QFT
049500030822     C                   IF        %subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):1:3)
049800030822     C                             = 'QFT'
049900030822     C                   EVAL      PiStr=%trim(%subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200030822     C                   EXSR      CHKNUM
050300030822     C                   IF        PiNum=*on
050400030822     C                   Z-ADD     PiVal         VABQFT
050500030822     C                   ENDIF
050600030822     C                   ENDIF
050700030822     C* CBO
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'CBO'
051200030822     C                   EVAL      VABCBO=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   ENDIF
051600030822     C* TSP
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'TSP'
052100030822     C                   EVAL      VABTSP=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   ENDIF
052500030822     C* VAS
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'VAS'
053000030822     C                   EVAL      VABVAS=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   ENDIF
053400030822     C* VCA
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'VCA'
053900030822     C                   EVAL      VABVCA=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   ENDIF
054300030822     C* GCA
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'GCA'
054800030822     C                   EVAL      VABGCA=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   ENDIF
055200030822     C* CTM
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'CTM'
055700030822     C                   EVAL      VABCTM=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   ENDIF
056100030822     C* FFD
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'FFD'
056600030822     C                   EVAL      VABFFD=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   ENDIF
057000030822     C* VAD
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'VAD'
057500030822     C                   EVAL      VABVAD=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   ENDIF
057900030822     C* GMA
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'GMA'
058400030822     C                   EVAL      VABGMA=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   ENDIF
058800030822     C* GGA
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'GGA'
059300030822     C                   EVAL      VABGGA=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   ENDIF
059700030822     C* GVA
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'GVA'
060200030822     C                   EVAL      VABGVA=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   ENDIF
060600030822     C* TC1
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'TC1'
061100030822     C                   EVAL      VABTC1=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   ENDIF
061500030822     C* TC2
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'TC2'
062000030822     C                   EVAL      VABTC2=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   ENDIF
062400030822     C* ...
062500030822     C                   ENDIF
062600030822     C                   ENDDO
062700020204     C*
062800010330     C                   ENDSR
062900010607     C*----------------------------------------------------*
063000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063100010607     C*----------------------------------------------------*
063200010607     C     IMPVAB        BEGSR
063300010607     C*
063400010607     C                   SETOFF                                       3132
063500030515     C*
063600010607     C                   MOVEL     datcor        VABAAS
063700010607     C                   MOVE      datcor        VABMGS
063800010607     C*
063900030715     C**********
064000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
064100030715     C**********
064200030715     C                   IF        wGiro = *zeros
064300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
064400031201     C     minu:maiu     XLATE     vindta        vindta
064500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
064600040324     C                   DOW       posDa <= %len(%trimr(vindta))
064700030715     C*
064800030715     C* Gestisco il 1° campo
064900030715     C                   IF        i = 1
065000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
065100030715     C                   EVAL      posDa = 1
065200030715     C* Eseguo lo scan x trovare la fine del primo campo
065300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065400030715     C* A questo punto "estraggo" il campo corrente
065500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065600030715     C                                                    (posA-posDa))
065700030715     C* X i campi successivi al 1°
065800030715     C                   ELSE
065900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
066000030715     C                   EVAL      posDa = posA + 1
066100030715     C* Eseguo lo scan x trovare la fine del campo corrente
066200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066300030715     C* Gestisco l'ultimo campo
066400030715     C                   IF        posA = *zeros
066500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
066600030715     C* A questo punto "estraggo" il campo corrente
066700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066800030715     C                                                    (posA-posDa))
066900030715     C                   LEAVE
067000030715     C                   ELSE
067100030715     C* A questo punto "estraggo" il campo corrente
067200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067300030715     C                                                    (posA-posDa))
067400030715     C                   ENDIF
067500030715     C                   ENDIF
067600030715     C* Incremento il contatore d campo
067700030715     C                   EVAL      i = i +1
067800030715     C                   ENDDO
067900030715     C*
068000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
068100030715     C* delimitazione testo
068200030715     C                   EVAL      i = 1
068300030715     C                   DOW       i < %elem(SkSplitFLD)
068400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
068500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
068600030715     C                   EVAL      i = i + 1
068700030715     C                   ENDDO
068800030715     C                   EVAL      wGiro = 1
068900030715     C                   ELSE
069000030715     C                   EVAL      wGiro = 2
069100031204     C**********
069200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
069300031204     C* d separatore campo
069400031204     C**********
069500040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
069600040304     C                   EVAL      vindta = ' ' + vindta
069700040304     C                   ENDIF
069800040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
069900040304     C***                EVAL      vindta = %subst(vindta:2)
070000040304     C***                ENDDO
070100031204     C*
070200031204     C                   Z-ADD     *zeros        lunghInput        4 0
070300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
070400040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
070500040304     C                   EVAL      vindta = vindta + ' '
070600040304     C                   ENDIF
070700040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
070800040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
070900040304     C***                EVAL      lunghInput = %len(%trim(vindta))
071000040304     C***                ENDDO
071100030715     C**********
071200030715     C* Effettuo lo split del campo dati d input
071300030715     C**********
071400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071500040324     C                   DOW       posDa <= %len(%trimr(vindta))
071600030715     C*
071700030715     C* Gestisco il 1° campo
071800030715     C                   IF        i = 1
071900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072000030715     C                   EVAL      posDa = 1
072100030715     C* Eseguo lo scan x trovare la fine del primo campo
072200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072300030715     C* A questo punto "estraggo" il campo corrente
072400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072500030715     C                                                    (posA-posDa))
072600030715     C* X i campi successivi al 1°
072700030715     C                   ELSE
072800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072900030715     C                   EVAL      posDa = posA + 1
073000031204     C* Verifico che nn vi sia il campo nn valorizzato
073100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
073200031204     C* Se campo nn valorizzato skippo al prossimo
073300031204     C                   EVAL      SkSplitCSV(i) = *blanks
073400031204     C                   EVAL      posA  = posA + 1
073500031204     C                   ELSE
073600030715     C* Eseguo lo scan x trovare la fine del campo corrente
073700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073800030715     C* Gestisco l'ultimo campo
073900030715     C                   IF        posA = *zeros
074000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
074100030715     C* A questo punto "estraggo" il campo corrente
074200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074300030715     C                                                    (posA-posDa))
074400030715     C                   LEAVE
074500030715     C                   ELSE
074600030715     C* A questo punto "estraggo" il campo corrente
074700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074800030715     C                                                    (posA-posDa))
074900030715     C                   ENDIF
075000031204     C                   ENDIF
075100030715     C                   ENDIF
075200030715     C* Incremento il contatore d campo
075300030715     C                   EVAL      i = i +1
075400030715     C                   ENDDO
075500030715     C*
075600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075700030715     C* delimitazione testo
075800030715     C                   EVAL      i = 1
075900030715     C                   DOW       i < %elem(SkSplitCSV)
076000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
076100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
076200030715     C                   EVAL      i = i + 1
076300030715     C                   ENDDO
076400030715     C*
076500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
076600030715     C                   EVAL      i = 1
076700030715     C                   DOW       i < %elem(SkSplitFLD)
076800030715     C*
076900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
077000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
077100030715     C                             SkSplitCSV(i) <> *blanks
077200030721     C*** ATB
077300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
077400030721     C                   EVAL      VABATB = SkSplitCSV(i)
077500030721     C                   ENDIF
077600030715     C*** RSD
077700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
077800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
077900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
078100030715     C* ==
078200030715     C                   ENDIF
078300030721     C*** RD2
078400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
078500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
078600030721     C                   ENDIF
078700030715     C*** IND
078800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
078900030715     C                   EVAL      VABIND = SkSplitCSV(i)
079000030715     C                   ENDIF
079100030715     C*** LOD
079200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
079300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
079400030715     C                   ENDIF
079500030715     C*** PRD
079600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
079700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
079800030715     C                   ENDIF
079900030721     C*** NZD
080000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
080100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
080200030721     C                   ENDIF
080300030715     C*** RMA
080400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
080500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
080600030715     C                   ENDIF
080700030715     C*** NOT
080800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
080900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
081000030715     C                   ENDIF
081100030715     C*** NT2
081200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
081300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
081400030715     C                   ENDIF
081500030715     C*** VCA
081600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
081700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
081800030715     C                   ENDIF
081900030715     C*** VAS
082000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
082100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
082200030715     C                   ENDIF
082300030715     C*** FFD
082400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
082500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
082600030715     C                   ENDIF
082700030715     C*** CBO
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
082900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
083000030715     C                   ENDIF
083100030721     C*** GC1
083200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
083300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
083400030721     C                   ENDIF
083500030721     C*** GC2
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
083700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
083800030721     C                   ENDIF
083900030721     C*** TSP
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030721     C*** NAS
084400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
084500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
084600030721     C                   ENDIF
084700030721     C*** TIC
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
084900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030721     C*** GCA
085200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
085400030721     C                   ENDIF
085500030721     C*** XCO
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
085700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030721     C*** CTM
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
086200030721     C                   ENDIF
086300030721     C*** TCR
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
086500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030721     C*** CTS
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
086900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** FTM
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** VAD
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
087700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** GMA
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** GGA
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
088500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GVA
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
088900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** TC1
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TC2
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
089700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** SCL
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** RMO
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
090500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** NMO
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
090900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*
091200070705     C* Reperisco quindi i campi numerici...
091300030721     C*** CAD
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
091500070705     C                   EVAL      VABCAD = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** RMN
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
091900030721     C                   EVAL      PiStr=SkSplitCSV(i)
092000030721     C                   EXSR      CHKNUM
092100030721     C                   IF        PiInt=*on
092200030721     C                   Z-ADD     PiVal         VABRMN
092300030721     C                   ELSE
092400030721     C                   SETON                                        32
092500030721     C                   EVAL      VABRMN = 1
092600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092700030721     C                             + ' ' + 'VABRMN'
092800030721     C                   ENDIF
092900030721     C                   ENDIF
093000030721     C*** NSP
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093200030721     C                   EVAL      PiStr=SkSplitCSV(i)
093300030721     C                   EXSR      CHKNUM
093400030721     C                   IF        PiInt=*on AND
093500030721     C                             PiVal<=9999999
093600030721     C                   Z-ADD     PiVal         VABNSP
093700030721     C                   ELSE
093800030721     C                   SETON                                        32
093900030721     C                   EVAL      VABNSP = *zeros
094000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094100030721     C                             + ' ' + 'VABNSP'
094200030721     C                   ENDIF
094300030721     C                   ENDIF
094400030721     C*** NCL
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
094600030721     C                   EVAL      PiStr=SkSplitCSV(i)
094700030721     C                   EXSR      CHKNUM
094800030721     C                   IF        PiInt=*on
094900030721     C                   Z-ADD     PiVal         VABNCL
095000030721     C                   ELSE
095100030721     C                   SETON                                        32
095200030721     C                   EVAL      VABNCL = *zeros
095300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095400030721     C                             + ' ' + 'VABNCL'
095500030721     C                   ENDIF
095600030721     C                   ENDIF
095700030721     C*** PKB
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
095900030721     C                   EVAL      PiStr=SkSplitCSV(i)
096000030721     C                   EXSR      CHKNUM
096100030721     C                   IF        PiNum=*on
096200030721     C                   Z-ADD     PiVal         VABPKB
096300030721     C                   ELSE
096400030721     C                   SETON                                        32
096500030721     C                   EVAL      VABPKB = *zeros
096600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096700030721     C                             + ' ' + 'VABPKB'
096800030721     C                   ENDIF
096900030721     C                   ENDIF
097000030721     C*** CAS
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
097200030721     C                   EVAL      PiStr=SkSplitCSV(i)
097300030721     C                   EXSR      CHKNUM
097400030721     C                   IF        PiNum=*on
097500030721     C                   Z-ADD     PiVal         VABCAS
097600030721     C                   ELSE
097700030721     C                   SETON                                        32
097800030721     C                   EVAL      VABCAS = *zeros
097900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098000030721     C                             + ' ' + 'VABCAS'
098100030721     C                   ENDIF
098200030721     C                   ENDIF
098300030721     C*** CCM
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
098500030721     C                   EVAL      PiStr=SkSplitCSV(i)
098600030721     C                   EXSR      CHKNUM
098700030721     C                   IF        PiInt=*on      AND
098800030721     C                             PiVal<=9999999 AND
098900030721     C                             PiVal>*zeros
099000030721     C                   Z-ADD     PiVal         VABCCM
099100030721     C                   ELSE
099200030721     C                   SETON                                        32
099300030721     C                   EVAL      VABCCM = *zeros
099400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099500030721     C                             + ' ' + 'VABCCM'
099600030721     C                   ENDIF
099700030721     C                   ENDIF
099800030721     C*** LNP
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
100000030721     C                   EVAL      PiStr=SkSplitCSV(i)
100100030721     C                   EXSR      CHKNUM
100200030721     C                   IF        PiInt=*on    AND
100300030721     C                             PiVal<=999   AND
100400030721     C                             PiVal>*zeros
100500030721     C                   Z-ADD     PiVal         VABLNP
100600030721     C                   ELSE
100700030721     C                   SETON                                        32
100800030721     C                   EVAL      VABLNP = *zeros
100900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101000030721     C                             + ' ' + 'VABLNP'
101100030721     C                   ENDIF
101200030721     C                   ENDIF
101300030721     C*** NRS
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
101500030721     C                   EVAL      PiStr=SkSplitCSV(i)
101600030721     C                   EXSR      CHKNUM
101700030721     C                   IF        PiInt=*on AND
101800030721     C                             PiVal<=99
101900030721     C                   Z-ADD     PiVal         VABNRS
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102300030721     C                             + ' ' + 'VABNRS'
102400030721     C                   ENDIF
102500030721     C                   ENDIF
102600030721     C*** LNA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
102800030721     C                   EVAL      PiStr=SkSplitCSV(i)
102900030721     C                   EXSR      CHKNUM
103000030721     C                   IF        PiInt=*on    AND
103100030721     C                             PiVal<=999   AND
103200030721     C                             PiVal>*zeros
103300030721     C                   Z-ADD     PiVal         VABLNA
103400030721     C                   ELSE
103500030721     C                   SETON                                        32
103600030721     C                   EVAL      VABLNA = *zeros
103700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103800030721     C                             + ' ' + 'VABLNA'
103900030721     C                   ENDIF
104000030721     C                   ENDIF
104100030721     C*** CTR
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
104300030721     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiInt=*on  AND
104600030721     C                             PiVal<=999
104700030721     C                   Z-ADD     PiVal         VABCTR
104800030721     C                   ELSE
104900030721     C                   SETON                                        32
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100030721     C                             + ' ' + 'VABCTR'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** IAS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiNum=*on
105900030721     C                   Z-ADD     PiVal         VABIAS
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106300030721     C                             + ' ' + 'VABIAS'
106400030721     C                   ENDIF
106500030721     C                   ENDIF
106600030721     C*** VLB
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
106800030721     C                   EVAL      PiStr=SkSplitCSV(i)
106900030721     C                   EXSR      CHKNUM
107000030721     C                   IF        PiNum=*on
107100030721     C                   Z-ADD     PiVal         VABVLB
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABVLB'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** QFT
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiNum=*on
108300030721     C                   Z-ADD     PiVal         VABQFT
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700030721     C                             + ' ' + 'VABQFT'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** NCD
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on      AND
109500030721     C                             PiVal<=9999999
109600030721     C                   Z-ADD     PiVal         VABNCD
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABNCD'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** NCA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on      AND
110800030721     C                             PiVal<=9999999
110900030721     C                   Z-ADD     PiVal         VABNCA
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABNCA'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** ZNC
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on AND
112100030721     C                             PiVal<=99
112200030721     C                   Z-ADD     PiVal         VABZNC
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABZNC'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** DCR
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on       AND
113400030721     C                             PiVal<=99999999
113500030721     C                   Z-ADD     PiVal         VABDCR
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABDCR'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** HCR
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on   AND
114700030721     C                             PiVal<=9999
114800030721     C                   Z-ADD     PiVal         VABHCR
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABHCR'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** VMD
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000030721     C                   Z-ADD     PiVal         VABVMD
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABVMD'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** ANT
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on        AND
117200030721     C                             PiVal<=999999999
117300030721     C                   Z-ADD     PiVal         VABANT
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABANT'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** CMO
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on
118500030721     C                   Z-ADD     PiVal         Num5_0
118600030721     C                   MOVEL(p)  Num5_0        VABCMO
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABCMO = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABCMO'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030715     C*
119500030715     C                   ENDIF
119600030715     C                   EVAL      i = i + 1
119700030715     C                   ENDDO
119800070705     C*
119900070705     C* Considerazioni particolari
120000070705     C*** NZD
120100070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
120200070705     C                   EVAL      VABNZD = *blanks
120300070705     C                   ENDIF
120400070705     C*** CAD
120500070705     C                   IF        VABNZD = *blanks
120600070705     C                   EVAL      PiStr=VABCAD
120700070705     C                   EXSR      CHKNUM
120800070705     C                   IF        PiInt=*on
120900070705     C                   Z-ADD     PiVal         Num5_0
121000070705     C                   MOVEL(p)  Num5_0        VABCAD
121100070705     C                   ELSE
121200070705     C                   SETON                                        32
121300070705     C                   EVAL      VABCAD = *zeros
121400070705     C                   EVAL      vinmsg = %trimr(vinmsg)
121500070705     C                             + ' ' + 'VABCAD'
121600070705     C                   ENDIF
121700070705     C                   ENDIF
121800020204     C*
121900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
122000020204     C                   EXSR      CHKIMPDIV
122100030715     C                   ENDIF
122200020204     C*
122300010607     C                   ENDSR
122400010607     C*----------------------------------------------------*
122500010601
122600020204
122700020204     C*----------------------------------------------------*
122800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
122900020204     C*----------------------------------------------------*
123000020204     C     CHKIMPDIV     BEGSR
123100060524     C*
123200060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
123300060524     C                   IF        vabCAS > *zeros
123400060524     C                   IF        vabCBO = '1'
123500060524     C                   EVAL      vabCBO = '4'
123600060524     C                   ENDIF
123700060524     C                   IF        vabCBO = '2'
123800060524     C                   EVAL      vabCBO = '6'
123900060524     C                   ENDIF
124000060524     C                   ENDIF
124100020204     C*
124200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
124300020204     C                   Z-ADD     *zeros        wrkDec            9 9
124400020204     C*
124500020204     C* Come prima cosa effettuo considerazioni sulla divisa
124600020204     C                   IF        vabIAS > *zeros
124700020204     C                   IF        vabVAS <> 'EUR'
124800020204     C                   EVAL      vabVAS =  'ITL'
124900020204     C                   ENDIF
125000020204     C                   ENDIF
125100020204     C*
125200020204     C                   IF        vabCAS > *zeros
125300020204     C                   IF        vabVCA <> 'EUR'
125400020204     C                   EVAL      vabVCA =  'ITL'
125500020204     C                   ENDIF
125600020204     C                   ENDIF
125700020204     C*
125800020204     C                   IF        vabVMD > *zeros
125900020204     C                   IF        vabVAD <> 'EUR'
126000020204     C                   EVAL      vabVAD =  'ITL'
126100020204     C                   ENDIF
126200020204     C                   ENDIF
126300020204     C*
126400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
126500020204     C                   Z-ADD     vabIAS        wrkDec
126600020204     C                   IF        wrkDec > *zeros
126700020204     C                   IF        vabVAS = 'ITL'
126800020204     C                   EVAL      vabIAS = *zeros
126900020204     C                   ENDIF
127000020204     C                   ENDIF
127100020204     C*
127200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
127300020204     C                   Z-ADD     vabCAS        wrkDec
127400020204     C                   IF        wrkDec > *zeros
127500020204     C                   IF        vabVCA = 'ITL'
127600020204     C                   EVAL      vabCAS = *zeros
127700020204     C                   ENDIF
127800020204     C                   ENDIF
127900020204     C*
128000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
128100020204     C                   Z-ADD     vabVMD        wrkDec
128200020204     C                   IF        wrkDec > *zeros
128300020204     C                   IF        vabVAD = 'ITL'
128400020204     C                   EVAL      vabVMD = *zeros
128500020204     C                   ENDIF
128600020204     C                   ENDIF
128700020204     C*
128800020204     C                   ENDSR
128900020204     C***
129000020204
129100010330
129200010330
129300010330     C*----------------------------------------------------*
129400010330     C*  CONTROLLO NUMERICITA' CAMPI
129500010330     C*----------------------------------------------------*
129600010330     C     CHKNUM        BEGSR
129700010330     C*
129800010606     C                   IF        PiDecChr = *blanks
129900030715     C                   EVAL      PiDecChr = CharNUM
130000010606     C                   ENDIF
130100010606     C*
130200010606     C                   CALL(e)   'ISNUMERIC'
130300010330     C                   PARM                    PiStr            30
130400010606     C                   PARM                    PiDecChr          1
130500010330     C                   PARM      *ZEROS        PiVal            30 9
130600010330     C                   PARM      '0'           PiInt             1
130700010330     C                   PARM      '0'           PiNum             1
130800010330     C                   IF        %error
130900010606     C                   EVAL      PiNum=*off
131000010330     C                   ENDIF
131100010330     C*
131200010330     C                   ENDSR
131300010330     C***
131400010330
131500010601
131600010601
131700010601
131800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
131900010601     C     repfil        BEGSR
132000010601     C*
132100010601     C                   if        invfil = *zeros and
132200010601     C                             depfil > *zeros and
132300010629     C                             (vinflg = *blanks or
132400010629     C                              vinflg = *zeros)
132500010601     C
132600010601     C                   eval      invfil = depfil
132700010601     C                   endif
132800010601     C*
132900010601     C                   if        depfil <> invfil and
133000010601     C                             invfil > *zeros
133100010601     C                   eval      flgMulti = '1'
133200010601     C                   if        vinflg = *blanks
133300010601     C                   add       1             cntNonEl
133400010601     C                   endif
133500010601     C                   endif
133600010601     C*
133700010601     C                   if        vinflg = '2'
133800010601     C                   eval      flgStato = '2'
133900010601     C                   endif
134000010601     C*
134100010601     C                   ENDSR
134200010601     C***
134300010601
134400010601
134500010601
134600010330
134700010330
134800010330
134900990920      /TITLE Invio dei dati al punto operativo.
135000000613     C     invio         BEGSR
135100990920     C*
135200990920     C                   reset                   dscmz
135300010601     C                   move      invfil        cmzdst
135400990920     C                   eval      cmzfld = vlrfou
135500990920     C                   eval      cmzmbd = vlrhdl
135600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
135700000710     C                   if        prmfir = *blanks
135800021025     C                   eval      cmzfla = 'FIVAB00F'
135900021025     C                   eval      cmzmba = 'FIVAB00F'
136000000710     C                   else
136100000710     C                   eval      cmzfla = prmfir
136200000710     C                   eval      cmzmba = prmfir
136300000710     C                   endif
136400990920     C                   eval      cmznrr = *zeros
136500990920     C                   move      §ctrok        cmznrr
136600021018     C                   eval      cmzlba = vlrfl1
136700990920     C                   call(e)   'TIS711C'
136800990920     C                   parm                    dscmz
136900990921     C                   parm      *blanks       esito
137000990920     C                   if        %error
137100990920     C                             or cmzerr = '1'
137200990921     C                             or esito  = '1'
137300000710     C                   eval      wrkesito = '3'
137400990920     C                   endif
137500990920     C*
137600000613     C                   ENDSR
137700990910
137800010601
137900010601
138000010601
138100010601
138200010601      /TITLE Invio dei dati al punto operativo.
138300010601     C     opeini        BEGSR
138400010601     C*
138500010601     C* Inizializzo flag e contatori operativi
138600010601     C                   movel     '0'           flgGiro           1
138700010601     C                   movel     '0'           flgMulti          1
138800010601     C                   movel     '1'           flgStato          1
138900010615     C                   movel     '0'           flgOk             1
139000010601     C                   z-add     *zeros        cntNonEl         10 0
139100010601     C                   z-add     *zeros        depfil            3 0
139200010601     C                   z-add     *zeros        invfil            3 0
139300010601     C*
139400010601     C                   ENDSR
139500010601     C***
139600050421
139700050421
139800050421
139900050421     C     *pssr         BEGSR
140000050421     C*
140100050421     C                   eval      wrkesito = '2'
140200050421     C*
140300050421     C                   ENDSR     '*CANCL'
140400050421     C***
140500010601
140600010601
140700010601
140800010330
140900010330
141000000613     C     *inzsr        BEGSR
141100990910     C*
141200990910     C     *entry        plist
141300990920     C                   parm                    tivlrds
141400990921     C                   parm      wrkesito      esito
141500000724     C                   parm                    prmlit
141600000710     C                   parm                    prmfir
141700010330     C*
141800010330     C* CALCOLA LA DATA CORRENTE
141900010330     C                   time                    wn14             14 0
142000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
142100010330     C                   z-add     wn8           g08dat
142200010330     C                   z-add     *zeros        g08inv
142300010330     C                   movel     '0'           g08err
142400010330     C                   call      'XSRDA8'
142500010330     C                   parm                    wlbda8
142600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
142700000613     C*
142800000613     C                   ENDSR
142900000613     C***
143000990908
143100030715     Otitvxlp   E            riepilogo         2
143200990915     O                                              'Upload via Internet'
143300990915     O                                           +1 'Traduzione TIVIN00R -'
143400021025     O                                           +1 'FIVABWWR'
143500990915     O                                           +1 'Testate bolle'
143600990915     O          E            riepilogo   2
143700990915     O                       wrkdata
143800990915     O                       wrkora              +1
143900990915     O                       procname            +1
144000990915     O          E            riepilogo   2
144100990915     O                                              'Cliente..................:'
144200010605     O                       VABCCM        z     +1
144300990915     O          E            riepilogo   2
144400990920     O                                              'Riferimento Strategi.....:'
144500990920     O                       vlrhdl              +1
144600990915     O          E            riepilogo   2
144700990915     O                                              'Giusti...................:'
144800971022     O                       §CTROK        2   +  1
144900990915     O          E            riepilogo   2
145000990915     O                                              'Sbagliati e corretti.....:'
145100971022     O                       §CTRMO        2   +  1
145200990915     O          E            riepilogo   2
145300990915     O                                              'Sbagliati e scartati.....:'
145400971022     O                       §CTRNO        2   +  1
145500000613
145600030715     Otitvxlps  E            testdett          2
145700000613     O                                              'Upload via Internet'
145800000613     O                                           +1 'Traduzione TIVIN00R -'
145900021025     O                                           +1 'FIVABWWR'
146000000613     O                                           +1 'Testate bolle'
146100000616     O          E            testdett    3
146200000613     O                                           +2 'N° rec'
146300000613     O                                           +3 'Anteprima contenuto'
146400000616     O          E            rigadett    2
146500000613     O                       rrnum               +2
146600000621     O                       recko               +3
146700000616     O          E            findett     2
146800000613     O                       wrkdata
146900000613     O                       wrkora              +1
147000000613     O                       procname            +1
147100000616     O          E            findett     2
147200000613     O                                              'Cliente..................:'
147300010605     O                       VABCCM        z     +1
147400000616     O          E            findett     2
147500000613     O                                              'Riferimento Strategi.....:'
147600000613     O                       vlrhdl              +1
147700000616     O          E            findett     2
147800000613     O                                              'Giusti...................:'
147900000613     O                       §CTROK        2   +  1
148000000616     O          E            findett     2
148100000613     O                                              'Sbagliati e corretti.....:'
148200000613     O                       §CTRMO        2   +  1
148300000616     O          E            findett     2
148400000613     O                                              'Sbagliati e scartati.....:'
148500000613     O                       §CTRNO        2   +  1
148600000613** CMD - COMANDI CL
148700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
148800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
148900030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
149000010606
149100000613
