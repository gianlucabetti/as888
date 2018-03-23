000100060123      /TITLE Upload via Internet: traduzione in EDIVABWR. (Cliente TNT - Fila Olanda Cod. 2100787)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060123     FEDIVABwr  O    E             DISK    usropn
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
018200060123     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
018300010330     C*--------------------------------------------------------
018400010605     C     RWVAB         BEGSR
018500010330     C*
018600010330     C                   if        not %open(tivin00r)
018700010330     C                   open      tivin00r
018800010330     C                   endif
018900060123     C                   if        not %open(edivabwr)
019000060123     C                   open      edivabwr
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
023700010601     C                   if        depfil = invfil
023800021025     C                   if        vlrpoi = 999
023900030822     C                   move(P)   invfil        VABFGS
024000021025     C                   else
024100030822     C                   move(P)   vlrpoi        VABFGS
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
025300060123     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
025400060123     C                   EVAL      VABCMR = 'TNT FASHION'
025500060123     C                   EVAL      VABDCM = DATCOR
025600060123     C                   EVAL      VABDTS = DATCOR
025700060123     C                   EVAL      VABHMS = ORACOR
025800060123     C                   EVAL      VABCNT = 1
025900060123     C*
026000060123     C  N31              write     EDIVAB00
026100030822     C                   endif
026200010604     C*
026300010604     C                   if        *in31 = *off and
026400010604     C                             *in32 = *off
026500010604     C                   eval      vinflg = '1'
026600010604     C                   else
026700010604     C                   eval      recko = vindta
026800010604     C                   except    rigadett
026900010604     C                   eval      vinflg = '2'
027000010604     C                   endif
027100010604     C                   endif
027200010601     C*
027300010604     C                   endif
027400010604     C*
027500010330     C                   else
027600010330     C                   eval      vinflg = '1'
027700010330     C                   endif
027800010601     C*
027900010601     C  N70              update    tivin000
028000010330     C*
028100030822     C  N70              enddo
028200010601     C
028300010601     C*
028400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028500010601     C                   if        cntNonEl = *zeros or
028600010601     C                             flgMulti = '0'
028700010330     C* Se non ci sono record con errori ...
028800010601     C                   if        §ctrno = 0 and
028900010604     C                             §ctrmo = 0 and
029000010601     C                             flgStato <> '2'
029100010330     C* ... restituisco esito OK.
029200010330     C                   eval      wrkesito = '0'
029300010330     C                   else
029400010330     C                   if        §ctrok > 0
029500010330     C                   eval      wrkesito = '1'
029600010330     C                   else
029700010615     C                   if        flgOk = '0'
029800010615     C                   eval      wrkesito = '2'
029900010615     C                   else
030000010615     C                   eval      wrkesito = '6'
030100010615     C                   endif
030200010330     C                   endif
030300010330     C                   endif
030400010601     C                   else
030500010601     C                   eval      wrkesito = '9'
030600010601     C                   endif
030700010330     C*
030800010330     C                   if        %open(tivin00r)
030900010330     C                   close     tivin00r
031000010330     C                   endif
031100060123     C                   if        %open(edivabwr)
031200060123     C                   close     edivabwr
031300010330     C                   endif
031400010601     C*
031500010601     C                   if        vlrpoi <> 999
031600010601     C                   eval      invfil = vlrpoi
031700010601     C                   endif
031800010330     C*
031900010330     C                   if        §ctrok > 0
032000010601     C                             and invfil > *zeros
032100010330     C                   exsr      invio
032200010330     C                   endif
032300010601     C*
032400010618     C                   if        flgGiro = '1'
032500010601     C                   exsr      endela
032600010618     C                   endif
032700010330     C*
032800010330     C                   ENDSR
032900010330     C***
033000010601
033100010601
033200010601
033300010330     C*----------------------------------------------------*
033400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033500010330     C*----------------------------------------------------*
033600010330     C     INZVAR        BEGSR
033700010330     C*
033800030822     C* Inizializzo variabili di wrk
033900020204     C                   Z-ADD     *zeros        Num5_0            5 0
034000030822     C*
034100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034200060123     C                   CLEAR                   EDIVAB00
034300030822     C                   CLEAR                   SkSplitCSV
034400030822     C                   EVAL      i = 1
034500030822     C                   EVAL      posDa = *zeros
034600030822     C                   EVAL      posA  = *zeros
034700030822     C*
034800030822     C* Reimposto i valori di default
034900030822     C                   EXSR      DEFCAM
035000030822     C*
035100010330     C                   ENDSR
035200010330     C*----------------------------------------------------*
035300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035400010330     C*----------------------------------------------------*
035500010330     C     DEFCAM        BEGSR
035600060123     C*
035700060123     C* Imposto alcuni valori d default relativi al clientre e al traffico realtivo
035800060123     C                   EVAL      VABCCM = 2100787
035900060123     C                   EVAL      VABLNP = 210
036000060123     C                   EVAL      VABCTR = 101
036100060123     C                   EVAL      VABCTM = '7Q'
036200060123     C                   EVAL      VABCBO = '1'
036300030822     C*
036400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
036500030715     C* e delimitatore testo.
036600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
036900060123     C*
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
062700060123     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
062800010607     C*----------------------------------------------------*
062900010607     C     IMPVAB        BEGSR
063000010607     C*
063100010607     C                   SETOFF                                       3132
063200030515     C*
063300010607     C                   MOVEL     datcor        VABAAS
063400010607     C                   MOVE      datcor        VABMGS
063500050321     C*
063600050321     C* 3° Giro x eliminare il separatore testo
063700050321     C                   Z-ADD     1             wj                5 0
063800050321     C                   DOW       wj <= %len(vindta) - 1
063900050321     C                   IF        %subst(vindta:wj:1) = CharTXT
064000050321     C                   EVAL      %subst(vindta:wj:1) = *blanks
064100050321     C                   ENDIF
064200050321     C                   ADD       1             wj
064300050321     C                   ENDDO
064400010607     C*
064500030715     C**********
064600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
064700030715     C**********
064800030715     C                   IF        wGiro = *zeros
064900060123     C*
065000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
065100031201     C     minu:maiu     XLATE     vindta        vindta
065200060123     C*
065300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
065400040324     C                   DOW       posDa <= %len(%trimr(vindta))
065500030715     C*
065600030715     C* Gestisco il 1° campo
065700030715     C                   IF        i = 1
065800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
065900030715     C                   EVAL      posDa = 1
066000030715     C* Eseguo lo scan x trovare la fine del primo campo
066100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066200030715     C* A questo punto "estraggo" il campo corrente
066300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066400030715     C                                                    (posA-posDa))
066500030715     C* X i campi successivi al 1°
066600030715     C                   ELSE
066700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
066800030715     C                   EVAL      posDa = posA + 1
066900030715     C* Eseguo lo scan x trovare la fine del campo corrente
067000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067100030715     C* Gestisco l'ultimo campo
067200030715     C                   IF        posA = *zeros
067300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
067400030715     C* A questo punto "estraggo" il campo corrente
067500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067600030715     C                                                    (posA-posDa))
067700030715     C                   LEAVE
067800030715     C                   ELSE
067900030715     C* A questo punto "estraggo" il campo corrente
068000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068100030715     C                                                    (posA-posDa))
068200030715     C                   ENDIF
068300030715     C                   ENDIF
068400030715     C* Incremento il contatore d campo
068500030715     C                   EVAL      i = i +1
068600030715     C                   ENDDO
068700030715     C*
068800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
068900030715     C* delimitazione testo
069000030715     C                   EVAL      i = 1
069100030715     C                   DOW       i < %elem(SkSplitFLD)
069200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
069300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
069400030715     C                   EVAL      i = i + 1
069500030715     C                   ENDDO
069600030715     C                   EVAL      wGiro = 1
069700030715     C                   ELSE
069800030715     C                   EVAL      wGiro = 2
069900031204     C**********
070000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
070100031204     C* d separatore campo
070200031204     C**********
070300040726     C                   EVAL      vindta = %trim(vindta)
070400040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
070500040726     C                   EVAL      vindta = %subst(vindta:2)
070600040726     C                   ENDDO
070700031204     C*
070800031204     C                   Z-ADD     *zeros        lunghInput        4 0
070900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
071000040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
071100040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
071200040726     C                   EVAL      lunghInput = %len(%trim(vindta))
071300040726     C                   ENDDO
071400030715     C**********
071500030715     C* Effettuo lo split del campo dati d input
071600030715     C**********
071700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071800040324     C                   DOW       posDa <= %len(%trimr(vindta))
071900030715     C*
072000030715     C* Gestisco il 1° campo
072100030715     C                   IF        i = 1
072200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072300030715     C                   EVAL      posDa = 1
072400030715     C* Eseguo lo scan x trovare la fine del primo campo
072500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072600030715     C* A questo punto "estraggo" il campo corrente
072700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072800030715     C                                                    (posA-posDa))
072900030715     C* X i campi successivi al 1°
073000030715     C                   ELSE
073100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073200030715     C                   EVAL      posDa = posA + 1
073300031204     C* Verifico che nn vi sia il campo nn valorizzato
073400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
073500031204     C* Se campo nn valorizzato skippo al prossimo
073600031204     C                   EVAL      SkSplitCSV(i) = *blanks
073700031204     C                   EVAL      posA  = posA + 1
073800031204     C                   ELSE
073900030715     C* Eseguo lo scan x trovare la fine del campo corrente
074000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074100030715     C* Gestisco l'ultimo campo
074200030715     C                   IF        posA = *zeros
074300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
074400030715     C* A questo punto "estraggo" il campo corrente
074500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074600030715     C                                                    (posA-posDa))
074700030715     C                   LEAVE
074800030715     C                   ELSE
074900030715     C* A questo punto "estraggo" il campo corrente
075000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075100030715     C                                                    (posA-posDa))
075200030715     C                   ENDIF
075300031204     C                   ENDIF
075400030715     C                   ENDIF
075500030715     C* Incremento il contatore d campo
075600030715     C                   EVAL      i = i +1
075700030715     C                   ENDDO
075800030715     C*
075900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076000030715     C* delimitazione testo
076100030715     C                   EVAL      i = 1
076200030715     C                   DOW       i < %elem(SkSplitCSV)
076300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
076400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
076500030715     C                   EVAL      i = i + 1
076600030715     C                   ENDDO
076700030715     C*
076800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
076900030715     C                   EVAL      i = 1
077000030715     C                   DOW       i < %elem(SkSplitFLD)
077100030715     C*
077200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
077300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
077400030715     C                             SkSplitCSV(i) <> *blanks
077500030721     C*** ATB
077600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
077700030721     C                   EVAL      VABATB = SkSplitCSV(i)
077800030721     C                   ENDIF
077900030715     C*** RSD
078000060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1SHNM'
078100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
078200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
078400030715     C* ==
078500030715     C                   ENDIF
078600030721     C*** RD2
078700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
078800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
078900030721     C                   ENDIF
079000030715     C*** IND
079100060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1SHA1'
079200030715     C                   EVAL      VABIND = SkSplitCSV(i)
079300030715     C                   ENDIF
079400030715     C*** LOD
079500060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1SHCT'
079600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
079700030715     C                   ENDIF
079800030715     C*** PRD
079900060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1SHST'
080000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
080100030715     C                   ENDIF
080200030721     C*** NZD
080300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
080400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
080500030721     C                   ENDIF
080600030715     C*** RMA
080700060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1PCTL'
080800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
080900030715     C                   ENDIF
081000030715     C*** NOT
081100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
081200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
081300030715     C                   ENDIF
081400030715     C*** NT2
081500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
081600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
081700030715     C                   ENDIF
081800030715     C*** VCA
081900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
082000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
082100030715     C                   ENDIF
082200030715     C*** VAS
082300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
082400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
082500030715     C                   ENDIF
082600030715     C*** FFD
082700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
082800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
082900030715     C                   ENDIF
083000030715     C*** CBO
083100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
083200030715     C                   EVAL      VABCBO = SkSplitCSV(i)
083300030715     C                   ENDIF
083400030715     C
083500030721     C*** GC1
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
083700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
083800030721     C                   ENDIF
083900030721     C*** GC2
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
084100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030721     C*** TSP
084400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
084600030721     C                   ENDIF
084700030721     C*** NAS
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
084900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030721     C*** TIC
085200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085400030721     C                   ENDIF
085500030721     C*** GCA
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030721     C*** XCO
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086200030721     C                   ENDIF
086300030721     C*** CTM
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030721     C*** TCR
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
086900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** CTS
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** FTM
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** VAD
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** GMA
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GGA
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
088900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GVA
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** TC1
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** TC2
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** SCL
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** RMO
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
090900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** NMO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*
091600030721     C* Reperisco quindi i campi numerici...
091700030721     C*** CAD
091800060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1SHZP'
091900030721     C                   EVAL      PiStr=SkSplitCSV(i)
092000030721     C                   EXSR      CHKNUM
092100030721     C                   IF        PiInt=*on
092200030721     C                   Z-ADD     PiVal         Num5_0
092300030721     C                   MOVEL(p)  Num5_0        VABCAD
092400030721     C                   ELSE
092500030721     C                   SETON                                        32
092600030721     C                   EVAL      VABCAD = *zeros
092700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092800030721     C                             + ' ' + 'VABCAD'
092900030721     C                   ENDIF
093000030721     C                   ENDIF
093100030721     C*** RMN
093200060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1PCTL'
093300030721     C                   EVAL      PiStr=SkSplitCSV(i)
093400030721     C                   EXSR      CHKNUM
093500030721     C                   IF        PiInt=*on
093600030721     C                   Z-ADD     PiVal         VABRMN
093700030721     C                   ELSE
093800030721     C                   SETON                                        32
093900030721     C                   EVAL      VABRMN = 1
094000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094100030721     C                             + ' ' + 'VABRMN'
094200030721     C                   ENDIF
094300030721     C                   ENDIF
094400030721     C*** NSP
094500060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1PCTL'
094600030721     C                   EVAL      PiStr=SkSplitCSV(i)
094700030721     C                   EXSR      CHKNUM
094800040726     C                   IF        PiInt=*on
094900040726     C                   Z-ADD     PiVal         VABNSP
095000040802     C*                  IF        PiVal>9999999
095100040802     C*                  SETON                                        32
095200040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
095300040802     C*                            + ' ' + 'VABNSP'
095400040802     C*                  ENDIF
095500030721     C                   ELSE
095600030721     C                   SETON                                        32
095700030721     C                   EVAL      VABNSP = *zeros
095800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095900030721     C                             + ' ' + 'VABNSP'
096000030721     C                   ENDIF
096100030721     C                   ENDIF
096200030721     C*** NCL
096300060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1TCT'
096400030721     C                   EVAL      PiStr=SkSplitCSV(i)
096500030721     C                   EXSR      CHKNUM
096600030721     C                   IF        PiInt=*on
096700030721     C                   Z-ADD     PiVal         VABNCL
096800030721     C                   ELSE
096900030721     C                   SETON                                        32
097000030721     C                   EVAL      VABNCL = *zeros
097100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097200030721     C                             + ' ' + 'VABNCL'
097300030721     C                   ENDIF
097400030721     C                   ENDIF
097500030721     C*** PKB
097600060123     C                   IF        %trim(SkSplitFLD(i)) = 'O1WGHT'
097700030721     C                   EVAL      PiStr=SkSplitCSV(i)
097800030721     C                   EXSR      CHKNUM
097900030721     C                   IF        PiNum=*on
098000030721     C                   Z-ADD     PiVal         VABPKB
098100030721     C                   ELSE
098200030721     C                   SETON                                        32
098300030721     C                   EVAL      VABPKB = *zeros
098400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098500030721     C                             + ' ' + 'VABPKB'
098600030721     C                   ENDIF
098700030721     C                   ENDIF
098800050505     C*** CAS
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
099000030721     C                   EVAL      PiStr=SkSplitCSV(i)
099100030721     C                   EXSR      CHKNUM
099200030721     C                   IF        PiNum=*on
099300030721     C                   Z-ADD     PiVal         VABCAS
099400030721     C                   ELSE
099500030721     C                   SETON                                        32
099600030721     C                   EVAL      VABCAS = *zeros
099700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099800030721     C                             + ' ' + 'VABCAS'
099900030721     C                   ENDIF
100000030721     C                   ENDIF
100100030721     C*** CCM
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
100300030721     C                   EVAL      PiStr=SkSplitCSV(i)
100400030721     C                   EXSR      CHKNUM
100500030721     C                   IF        PiInt=*on      AND
100600030721     C                             PiVal<=9999999 AND
100700030721     C                             PiVal>*zeros
100800030721     C                   Z-ADD     PiVal         VABCCM
100900030721     C                   ELSE
101000030721     C                   SETON                                        32
101100030721     C                   EVAL      VABCCM = *zeros
101200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101300030721     C                             + ' ' + 'VABCCM'
101400030721     C                   ENDIF
101500030721     C                   ENDIF
101600030721     C*** LNP
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
101800030721     C                   EVAL      PiStr=SkSplitCSV(i)
101900030721     C                   EXSR      CHKNUM
102000030721     C                   IF        PiInt=*on    AND
102100030721     C                             PiVal<=999   AND
102200030721     C                             PiVal>*zeros
102300030721     C                   Z-ADD     PiVal         VABLNP
102400030721     C                   ELSE
102500030721     C                   SETON                                        32
102600030721     C                   EVAL      VABLNP = *zeros
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800030721     C                             + ' ' + 'VABLNP'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** NRS
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiInt=*on AND
103600030721     C                             PiVal<=99
103700030721     C                   Z-ADD     PiVal         VABNRS
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104100030721     C                             + ' ' + 'VABNRS'
104200030721     C                   ENDIF
104300030721     C                   ENDIF
104400030721     C*** LNA
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
104600030721     C                   EVAL      PiStr=SkSplitCSV(i)
104700030721     C                   EXSR      CHKNUM
104800030721     C                   IF        PiInt=*on    AND
104900030721     C                             PiVal<=999   AND
105000030721     C                             PiVal>*zeros
105100030721     C                   Z-ADD     PiVal         VABLNA
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      VABLNA = *zeros
105500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105600030721     C                             + ' ' + 'VABLNA'
105700030721     C                   ENDIF
105800030721     C                   ENDIF
105900030721     C*** CTR
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
106100030721     C                   EVAL      PiStr=SkSplitCSV(i)
106200030721     C                   EXSR      CHKNUM
106300030721     C                   IF        PiInt=*on  AND
106400030721     C                             PiVal<=999
106500030721     C                   Z-ADD     PiVal         VABCTR
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106900030721     C                             + ' ' + 'VABCTR'
107000030721     C                   ENDIF
107100030721     C                   ENDIF
107200030721     C*** IAS
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
107400030721     C                   EVAL      PiStr=SkSplitCSV(i)
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiNum=*on
107700030721     C                   Z-ADD     PiVal         VABIAS
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100030721     C                             + ' ' + 'VABIAS'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** VLB
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiNum=*on
108900030721     C                   Z-ADD     PiVal         VABVLB
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABVLB'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** QFT
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiNum=*on
110100030721     C                   Z-ADD     PiVal         VABQFT
110200030721     C                   ELSE
110300030721     C                   SETON                                        32
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABQFT'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** NCD
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on      AND
111300030721     C                             PiVal<=9999999
111400030721     C                   Z-ADD     PiVal         VABNCD
111500030721     C                   ELSE
111600030721     C                   SETON                                        32
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABNCD'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NCA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on      AND
112600030721     C                             PiVal<=9999999
112700030721     C                   Z-ADD     PiVal         VABNCA
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABNCA'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** ZNC
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on AND
113900030721     C                             PiVal<=99
114000030721     C                   Z-ADD     PiVal         VABZNC
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABZNC'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** DCR
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on       AND
115200030721     C                             PiVal<=99999999
115300030721     C                   Z-ADD     PiVal         VABDCR
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABDCR'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** HCR
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on   AND
116500030721     C                             PiVal<=9999
116600030721     C                   Z-ADD     PiVal         VABHCR
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABHCR'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** VMD
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABVMD
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABVMD'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** ANT
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on        AND
119000030721     C                             PiVal<=999999999
119100030721     C                   Z-ADD     PiVal         VABANT
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABANT'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** CMO
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on
120300030721     C                   Z-ADD     PiVal         Num5_0
120400030721     C                   MOVEL(p)  Num5_0        VABCMO
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCMO = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABCMO'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030715     C*
121300030715     C                   ENDIF
121400030715     C                   EVAL      i = i + 1
121500030715     C                   ENDDO
121600040726     C*
121700040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
121800040726     C                   IF        VABNSP = *zeros
121900040726     C                   SETON                                        31
122000040726     C                   ENDIF
122100040726     C*
122200040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
122300040726     C                   IF        VABLNP = *zeros
122400040726     C                   SETON                                        31
122500040726     C                   ENDIF
122600040726     C*
122700040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
122800040726     C* d valorizzarli sempre entrambi
122900040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
123000040726     C                   Z-ADD     VABNSP        VABRMN
123100040726     C                   ENDIF
123200040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
123300040726     C                   MOVEL     VABRMN        VABRMA
123400040726     C                   ENDIF
123500040726     C*
123600040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
123700040726     C                   IF        VABCAS > *zeros
123800040726     C                   IF        VABVCA = *blanks
123900040726     C                   EVAL      VABVCA = 'EUR'
124000040726     C                   ENDIF
124100040726     C                   IF        VABCBO = '1'
124200040726     C                   EVAL      VABCBO = '4'
124300040726     C                   ENDIF
124400040726     C                   IF        VABCBO = '2'
124500040726     C                   EVAL      VABCBO = '6'
124600040726     C                   ENDIF
124700040726     C                   ENDIF
124800020204     C*
124900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
125000020204     C                   EXSR      CHKIMPDIV
125100030715     C                   ENDIF
125200020204     C*
125300010607     C                   ENDSR
125400010607     C*----------------------------------------------------*
125500010601
125600020204
125700020204     C*----------------------------------------------------*
125800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
125900020204     C*----------------------------------------------------*
126000020204     C     CHKIMPDIV     BEGSR
126100020204     C*
126200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
126300020204     C                   Z-ADD     *zeros        wrkDec            9 9
126400020204     C*
126500020204     C* Come prima cosa effettuo considerazioni sulla divisa
126600020204     C                   IF        vabIAS > *zeros
126700020204     C                   IF        vabVAS <> 'EUR'
126800020204     C                   EVAL      vabVAS =  'ITL'
126900020204     C                   ENDIF
127000020204     C                   ENDIF
127100020204     C*
127200020204     C                   IF        vabCAS > *zeros
127300020204     C                   IF        vabVCA <> 'EUR'
127400020204     C                   EVAL      vabVCA =  'ITL'
127500020204     C                   ENDIF
127600020204     C                   ENDIF
127700020204     C*
127800020204     C                   IF        vabVMD > *zeros
127900020204     C                   IF        vabVAD <> 'EUR'
128000020204     C                   EVAL      vabVAD =  'ITL'
128100020204     C                   ENDIF
128200020204     C                   ENDIF
128300020204     C*
128400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
128500020204     C                   Z-ADD     vabIAS        wrkDec
128600020204     C                   IF        wrkDec > *zeros
128700020204     C                   IF        vabVAS = 'ITL'
128800020204     C                   EVAL      vabIAS = *zeros
128900020204     C                   ENDIF
129000020204     C                   ENDIF
129100020204     C*
129200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
129300020204     C                   Z-ADD     vabCAS        wrkDec
129400020204     C                   IF        wrkDec > *zeros
129500020204     C                   IF        vabVCA = 'ITL'
129600020204     C                   EVAL      vabCAS = *zeros
129700020204     C                   ENDIF
129800020204     C                   ENDIF
129900020204     C*
130000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
130100020204     C                   Z-ADD     vabVMD        wrkDec
130200020204     C                   IF        wrkDec > *zeros
130300020204     C                   IF        vabVAD = 'ITL'
130400020204     C                   EVAL      vabVMD = *zeros
130500020204     C                   ENDIF
130600020204     C                   ENDIF
130700020204     C*
130800020204     C                   ENDSR
130900020204     C***
131000020204
131100010330
131200010330
131300010330     C*----------------------------------------------------*
131400010330     C*  CONTROLLO NUMERICITA' CAMPI
131500010330     C*----------------------------------------------------*
131600010330     C     CHKNUM        BEGSR
131700010330     C*
131800010606     C                   IF        PiDecChr = *blanks
131900030715     C                   EVAL      PiDecChr = CharNUM
132000010606     C                   ENDIF
132100010606     C*
132200010606     C                   CALL(e)   'ISNUMERIC'
132300010330     C                   PARM                    PiStr            30
132400010606     C                   PARM                    PiDecChr          1
132500010330     C                   PARM      *ZEROS        PiVal            30 9
132600010330     C                   PARM      '0'           PiInt             1
132700010330     C                   PARM      '0'           PiNum             1
132800010330     C                   IF        %error
132900010606     C                   EVAL      PiNum=*off
133000010330     C                   ENDIF
133100010330     C*
133200010330     C                   ENDSR
133300010330     C***
133400010330
133500010601
133600010601
133700010601
133800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
133900010601     C     repfil        BEGSR
134000010601     C*
134100010601     C                   if        invfil = *zeros and
134200010601     C                             depfil > *zeros and
134300010629     C                             (vinflg = *blanks or
134400010629     C                              vinflg = *zeros)
134500010601     C
134600010601     C                   eval      invfil = depfil
134700010601     C                   endif
134800010601     C*
134900010601     C                   if        depfil <> invfil and
135000010601     C                             invfil > *zeros
135100010601     C                   eval      flgMulti = '1'
135200010601     C                   if        vinflg = *blanks
135300010601     C                   add       1             cntNonEl
135400010601     C                   endif
135500010601     C                   endif
135600010601     C*
135700010601     C                   if        vinflg = '2'
135800010601     C                   eval      flgStato = '2'
135900010601     C                   endif
136000010601     C*
136100010601     C                   ENDSR
136200010601     C***
136300010601
136400010601
136500010601
136600010330
136700010330
136800010330
136900990920      /TITLE Invio dei dati al punto operativo.
137000000613     C     invio         BEGSR
137100990920     C*
137200990920     C                   reset                   dscmz
137300010601     C                   move      invfil        cmzdst
137400990920     C                   eval      cmzfld = vlrfou
137500990920     C                   eval      cmzmbd = vlrhdl
137600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
137700000710     C                   if        prmfir = *blanks
137800060123     C                   eval      cmzfla = 'EDIVAB0F'
137900060123     C                   eval      cmzmba = 'EDIVAB0F'
138000000710     C                   else
138100000710     C                   eval      cmzfla = prmfir
138200000710     C                   eval      cmzmba = prmfir
138300000710     C                   endif
138400990920     C                   eval      cmznrr = *zeros
138500990920     C                   move      §ctrok        cmznrr
138600021018     C                   eval      cmzlba = vlrfl1
138700990920     C                   call(e)   'TIS711C'
138800990920     C                   parm                    dscmz
138900990921     C                   parm      *blanks       esito
139000990920     C                   if        %error
139100990920     C                             or cmzerr = '1'
139200990921     C                             or esito  = '1'
139300000710     C                   eval      wrkesito = '3'
139400990920     C                   endif
139500990920     C*
139600000613     C                   ENDSR
139700990910
139800010601
139900010601
140000010601
140100010601
140200010601      /TITLE Invio dei dati al punto operativo.
140300010601     C     opeini        BEGSR
140400010601     C*
140500010601     C* Inizializzo flag e contatori operativi
140600010601     C                   movel     '0'           flgGiro           1
140700010601     C                   movel     '0'           flgMulti          1
140800010601     C                   movel     '1'           flgStato          1
140900010615     C                   movel     '0'           flgOk             1
141000010601     C                   z-add     *zeros        cntNonEl         10 0
141100010601     C                   z-add     *zeros        depfil            3 0
141200010601     C                   z-add     *zeros        invfil            3 0
141300010601     C*
141400010601     C                   ENDSR
141500010601     C***
141600010601
141700010601
141800050421
141900050421
142000060123     C     *pssr         BEGSR
142100050421     C*
142200060123     C                   eval      wrkesito = '2'
142300050421     C*
142400060123     C                   ENDSR     '*CANCL'
142500050421     C***
142600050421
142700010601
142800010330
142900010330
143000000613     C     *inzsr        BEGSR
143100990910     C*
143200990910     C     *entry        plist
143300990920     C                   parm                    tivlrds
143400990921     C                   parm      wrkesito      esito
143500000724     C                   parm                    prmlit
143600000710     C                   parm                    prmfir
143700050505     C*
143800050505     C* CALCOLA LA DATA CORRENTE
143900050505     C                   time                    wn14             14 0
144000050505     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
144100060123     C                   movel     wn14          oracor            6 0          *ORA
144200050505     C                   z-add     wn8           g08dat
144300050505     C                   z-add     *zeros        g08inv
144400050505     C                   movel     '0'           g08err
144500050505     C                   call      'XSRDA8'
144600050505     C                   parm                    wlbda8
144700050505     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
144800000613     C*
144900000613     C                   ENDSR
145000000613     C***
145100990908
145200030715     Otitvxlp   E            riepilogo         2
145300990915     O                                              'Upload via Internet'
145400990915     O                                           +1 'Traduzione TIVIN00R -'
145500060123     O                                           +1 'EDIVABWR'
145600990915     O                                           +1 'Testate bolle'
145700990915     O          E            riepilogo   2
145800990915     O                       wrkdata
145900990915     O                       wrkora              +1
146000990915     O                       procname            +1
146100990915     O          E            riepilogo   2
146200990915     O                                              'Cliente..................:'
146300010605     O                       VABCCM        z     +1
146400990915     O          E            riepilogo   2
146500990920     O                                              'Riferimento Strategi.....:'
146600990920     O                       vlrhdl              +1
146700990915     O          E            riepilogo   2
146800990915     O                                              'Giusti...................:'
146900971022     O                       §CTROK        2   +  1
147000990915     O          E            riepilogo   2
147100990915     O                                              'Sbagliati e corretti.....:'
147200971022     O                       §CTRMO        2   +  1
147300990915     O          E            riepilogo   2
147400990915     O                                              'Sbagliati e scartati.....:'
147500971022     O                       §CTRNO        2   +  1
147600000613
147700030715     Otitvxlps  E            testdett          2
147800000613     O                                              'Upload via Internet'
147900000613     O                                           +1 'Traduzione TIVIN00R -'
148000060123     O                                           +1 'EDIVABWR'
148100000613     O                                           +1 'Testate bolle'
148200000616     O          E            testdett    3
148300000613     O                                           +2 'N° rec'
148400000613     O                                           +3 'Anteprima contenuto'
148500000616     O          E            rigadett    2
148600000613     O                       rrnum               +2
148700000621     O                       recko               +3
148800000616     O          E            findett     2
148900000613     O                       wrkdata
149000000613     O                       wrkora              +1
149100000613     O                       procname            +1
149200000616     O          E            findett     2
149300000613     O                                              'Cliente..................:'
149400010605     O                       VABCCM        z     +1
149500000616     O          E            findett     2
149600000613     O                                              'Riferimento Strategi.....:'
149700000613     O                       vlrhdl              +1
149800000616     O          E            findett     2
149900000613     O                                              'Giusti...................:'
150000000613     O                       §CTROK        2   +  1
150100000616     O          E            findett     2
150200000613     O                                              'Sbagliati e corretti.....:'
150300000613     O                       §CTRMO        2   +  1
150400000616     O          E            findett     2
150500000613     O                                              'Sbagliati e scartati.....:'
150600000613     O                       §CTRNO        2   +  1
150700000613** CMD - COMANDI CL
150800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
150900030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
151000030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
151100010606
151200000613
