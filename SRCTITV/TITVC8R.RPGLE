000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021025     FFIVABwwr  O    E             DISK    usropn
000700040616     Ftitvc8p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900040616     Ftitvc8ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100000313     D*
001200000613     D*------------
001300000613     D* COMANDI
001400000613     D*------------
001500011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600040616     D*------------
001700040616     D* SCHIEDE A PROGRAMMA
001800040616     D*------------
001900040616     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
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
003600030715     D*------------------
003700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003800030715     D*------------------
003900030715     D SkSplitFLD      S             10    DIM(100)
004000030715     D SkSplitCSV      S            256    DIM(100)
004100030715     D CharCSV         S              1
004200030715     D CharTXT         S              1
004300030715     D CharNUM         S              1
004400030715     D posDa           S              3  0 INZ(*zeros)
004500030715     D posA            S              3  0 INZ(*zeros)
004600030715     D i               s              3  0 INZ(1)
004700030715     D wGiro           s              1  0 INZ(*zeros)
004800030822     D*------------------
004900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005000030822     D*------------------
005100030822     D posDaDft        S              3  0 INZ(*zeros)
005200030822     D posADft         S              3  0 INZ(*zeros)
005300030822     D j               s              3  0 INZ(1)
005400030822     D wGiroDft        s              1  0 INZ(*zeros)
005500010330     D*------------------
005600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700010330     D*------------------
005800010330     D WLBDA8          DS                  INZ
005900010330     D  G08DAT                 1      8  0
006000010330     D  G08INV                 9     16  0
006100010330     D  G08ERR                17     17
006200010330     D  G08TGI                18     22  0
006300031201     D*------------------
006400031201     D* Costanti
006500031201     D*------------------
006600031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006700031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006800010330
006900010330
007000010330
007100990915     C                   time                    wrkdata
007200990915     C                   time                    wrkora
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600010601     C*
007700010601     C                   exsr      opeini
007800010605     C                   exsr      rwvab
007900010601     C*
008000010601     C                   seton                                        lr
008100010601
008200010601
008300010601
008400010601
008500010601     C*--------------------------------------------------------
008600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008700010601     C*--------------------------------------------------------
008800010601     C     PREELA        BEGSR
008900010601     C*
009000010601     C* SE OCCORRE SPEDIRE IN FILIALE
009100010601     C                   if        invfil <> *zeros and
009200010601     C                             flgGiro = '0'
009300010601     C*
009400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009500010601     C                   eval      flgGiro = '1'
009600000724     C*
009700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009900010601     C     invfil        chain     azorg01l
010000000724     C                   if        %found
010100000724     C                   movel(p)  CMD(1)        depcmd
010200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010300000724     C*
010400000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010500011113     C                   Z-ADD     150           LENGH            15 5
010600000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000724     C                   PARM                    depcmd
010800000724     C                   PARM                    LENGH
010900000724     C*
011000000724     C                   endif
011100000616     C*
011200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300000616     C                   movel(p)  CMD(2)        depcmd
011400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011500000616     C*
011600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011700011113     C                   Z-ADD     150           LENGH            15 5
011800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    depcmd
012000000616     C                   PARM                    LENGH
012100000613     C*
012200040616     C                   if        not %open(titvc8ps)
012300040616     C                   open      titvc8ps
012400030715     C                   except    testdett
012500000613     C                   endif
012600010601     C*
012700010601     C                   endif
012800010601     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500010601
013600010601
013700010601
013800010601     C*--------------------------------------------------------
013900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014000010601     C*--------------------------------------------------------
014100010601     C     ENDELA        BEGSR
014200990915     C*
014300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014400000613     C*
014500040616     C                   if        %open(titvc8ps)
014600000616     C                   except    findett
014700040616     C                   close     titvc8ps
014800000613     C                   endif
014900000616     C*
015000000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015100011113     C                   Z-ADD     150           LENGH            15 5
015200010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015300000616     C                   PARM                    CMD(3)
015400000616     C                   PARM                    LENGH
015500000616     C*
015600010601     C                   ENDSR
015700010601     C***
015800010601
015900010601
016000010601
016100010601
016200000613
016300000613     C*--------------------------------------------------------
016400000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016500000613     C*--------------------------------------------------------
016600000621     C     STPR          BEGSR
016700000613     C*
016800040616     C                   if        not %open(titvc8p)
016900040616     C   50              open      titvc8p
017000990915     C                   endif
017100990915     C*
017200030715     C   50              except    riepilogo
017300990915     C*
017400040616     C                   if        %open(titvc8p)
017500040616     C   50              close     titvc8p
017600990914     C                   endif
017700000613     C*
017800000613     C                   ENDSR
017900000613     C***
018000000613
018100010330
018200010601
018300010601
018400010601
018500010330     C*--------------------------------------------------------
018600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018700010330     C*--------------------------------------------------------
018800010605     C     RWVAB         BEGSR
018900010330     C*
019000010330     C                   if        not %open(tivin00r)
019100010330     C                   open      tivin00r
019200010330     C                   endif
019300021025     C                   if        not %open(fivabwwr)
019400021025     C                   open      fivabwwr
019500010330     C                   endif
019600010330     C*
019700010604     C                   clear                   §CTROK
019800010604     C                   clear                   §CTRMO
019900010604     C                   clear                   §CTRNO
020000010330     C*
020100030822     C                   do        *HIVAL
020200010330     C*
020300030822     C                   read      tivin00r                               70
020400010618     C*
020500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020600010618     C                   if        vinflg = '1'
020700010618     C                   eval      flgOk = '1'
020800010618     C                   endif
020900010618     C*
021000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021100040510     C                   z-add     *zeros        wLenVINDTA        4 0
021200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021600040510     C                   z-add     1             wFlgVINDTA
021700040510     C                   leave
021800040510     C                   endif
021900040510     C                   enddo
022000040510     C*
022100040218     C                   if        vindta > *blanks AND
022200040510     C                             wFlgVINDTA = 1
022300010330     C                   add       1             rrnum
022400010330     C*
022500010601     C                   if        *in70 = *off and
022600010330     C                             (vinflg = *blanks
022700010330     C                              or vinflg = '0'
022800010330     C                              or vinflg = '2')
022900010330     C*
023000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023100010711     C                   if        vinflg = *blanks or vinflg = '0'
023200010711     C                   clear                   vinmsg
023300010711     C                   endif
023400010601     C*
023500010330     C                   exsr      inzvar
023600010605     C                   exsr      impvab
023700010601     C*
023800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023900010605     C                   eval      depfil = VABLNP
024000010601     C                   exsr      repfil
024100010601     C                   if        depfil = invfil
024200021025     C                   if        vlrpoi = 999
024300030822     C                   move(P)   invfil        VABFGS
024400021025     C                   else
024500030822     C                   move(P)   vlrpoi        VABFGS
024600021025     C                   endif
024700010601     C*
024800010601     C                   exsr      PREELA
024900010601     C*
025000010604     C* Ebbene...
025100010604     C*
025200030822     C  N31              add       1             §CTROK            7 0
025300030822     C   32              add       1             §CTRMO            7 0
025400030822     C   31              add       1             §CTRNO            7 0
025500030822     C                   if        wGiro = 2
025600030822     C  N31              write     FIVAB000
025700030822     C                   endif
025800010604     C*
025900010604     C                   if        *in31 = *off and
026000010604     C                             *in32 = *off
026100010604     C                   eval      vinflg = '1'
026200010604     C                   else
026300010604     C                   eval      recko = vindta
026400010604     C                   except    rigadett
026500010604     C                   eval      vinflg = '2'
026600010604     C                   endif
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
033600040716     C                   Z-ADD     *zeros        wFlgCAS           3 0
033700030822     C*
033800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
033900030822     C                   CLEAR                   FIVAB000
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
035300030822     C*
035400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
035500030715     C* e delimitatore testo.
035600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
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
039000030822     C* NCL
039100030822     C                   IF        %subst(
039200030822     C                             %subst(vlrppt:posDaDft+1:
039300030822     C                             posADft-posDaDft-1):1:3)
039400030822     C                             = 'NCL'
039500030822     C                   EVAL      PiStr=%trim(%subst(
039600030822     C                             %subst(vlrppt:posDaDft+1:
039700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039800030822     C                   EXSR      CHKNUM
039900030822     C                   IF        PiInt=*on
040000030822     C                   Z-ADD     PiVal         VABNCL
040100030822     C                   ENDIF
040200030822     C                   ENDIF
040300030822     C* CCM
040400030822     C                   IF        %subst(
040500030822     C                             %subst(vlrppt:posDaDft+1:
040600030822     C                             posADft-posDaDft-1):1:3)
040700030822     C                             = 'CCM'
040800030822     C                   EVAL      PiStr=%trim(%subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041100030822     C                   EXSR      CHKNUM
041200030822     C                   IF        PiInt=*on
041300030822     C                   Z-ADD     PiVal         VABCCM
041400030822     C                   ENDIF
041500030822     C                   ENDIF
041600030822     C* LNP
041700030822     C                   IF        %subst(
041800030822     C                             %subst(vlrppt:posDaDft+1:
041900030822     C                             posADft-posDaDft-1):1:3)
042000030822     C                             = 'LNP'
042100030822     C                   EVAL      PiStr=%trim(%subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400030822     C                   EXSR      CHKNUM
042500030822     C                   IF        PiInt=*on
042600030822     C                   Z-ADD     PiVal         VABLNP
042700030822     C                   ENDIF
042800030822     C                   ENDIF
042900030822     C* NRS
043000030822     C                   IF        %subst(
043100030822     C                             %subst(vlrppt:posDaDft+1:
043200030822     C                             posADft-posDaDft-1):1:3)
043300030822     C                             = 'NRS'
043400030822     C                   EVAL      PiStr=%trim(%subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700030822     C                   EXSR      CHKNUM
043800030822     C                   IF        PiInt=*on
043900030822     C                   Z-ADD     PiVal         VABNRS
044000030822     C                   ENDIF
044100030822     C                   ENDIF
044200030822     C* CTR
044300030822     C                   IF        %subst(
044400030822     C                             %subst(vlrppt:posDaDft+1:
044500030822     C                             posADft-posDaDft-1):1:3)
044600030822     C                             = 'CTR'
044700030822     C                   EVAL      PiStr=%trim(%subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045000030822     C                   EXSR      CHKNUM
045100030822     C                   IF        PiInt=*on
045200030822     C                   Z-ADD     PiVal         VABCTR
045300030822     C                   ENDIF
045400030822     C                   ENDIF
045500030822     C* PKB
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'PKB'
046000030822     C                   EVAL      PiStr=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   EXSR      CHKNUM
046400030822     C                   IF        PiNum=*on
046500030822     C                   Z-ADD     PiVal         VABPKB
046600030822     C                   ENDIF
046700030822     C                   ENDIF
046800030822     C* VLB
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'VLB'
047300030822     C                   EVAL      PiStr=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   EXSR      CHKNUM
047700030822     C                   IF        PiNum=*on
047800030822     C                   Z-ADD     PiVal         VABVLB
047900030822     C                   ENDIF
048000030822     C                   ENDIF
048100030822     C* QFT
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'QFT'
048600030822     C                   EVAL      PiStr=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   EXSR      CHKNUM
049000030822     C                   IF        PiNum=*on
049100030822     C                   Z-ADD     PiVal         VABQFT
049200030822     C                   ENDIF
049300030822     C                   ENDIF
049400030822     C* CBO
049500030822     C                   IF        %subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):1:3)
049800030822     C                             = 'CBO'
049900030822     C                   EVAL      VABCBO=%trim(%subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200030822     C                   ENDIF
050300030822     C* TSP
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'TSP'
050800030822     C                   EVAL      VABTSP=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   ENDIF
051200030822     C* VAS
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'VAS'
051700030822     C                   EVAL      VABVAS=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   ENDIF
052100030822     C* VCA
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'VCA'
052600030822     C                   EVAL      VABVCA=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   ENDIF
053000030822     C* GCA
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'GCA'
053500030822     C                   EVAL      VABGCA=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   ENDIF
053900030822     C* CTM
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'CTM'
054400030822     C                   EVAL      VABCTM=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   ENDIF
054800030822     C* FFD
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'FFD'
055300030822     C                   EVAL      VABFFD=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   ENDIF
055700030822     C* VAD
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'VAD'
056200030822     C                   EVAL      VABVAD=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   ENDIF
056600030822     C* GMA
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'GMA'
057100030822     C                   EVAL      VABGMA=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   ENDIF
057500030822     C* GGA
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'GGA'
058000030822     C                   EVAL      VABGGA=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   ENDIF
058400030822     C* GVA
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'GVA'
058900030822     C                   EVAL      VABGVA=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   ENDIF
059300030822     C* TC1
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'TC1'
059800030822     C                   EVAL      VABTC1=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   ENDIF
060200030822     C* TC2
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'TC2'
060700030822     C                   EVAL      VABTC2=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   ENDIF
061100030822     C* ...
061200030822     C                   ENDIF
061300030822     C                   ENDDO
061400020204     C*
061500010330     C                   ENDSR
061600010607     C*----------------------------------------------------*
061700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
061800010607     C*----------------------------------------------------*
061900010607     C     IMPVAB        BEGSR
062000010607     C*
062100010607     C                   SETOFF                                       3132
062200030515     C*
062300010607     C                   MOVEL     datcor        VABAAS
062400010607     C                   MOVE      datcor        VABMGS
062500010607     C*
062600030715     C**********
062700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
062800030715     C**********
062900030715     C                   IF        wGiro = *zeros
063000040616     C*
063100040616     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
063200040616     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
063300040616     C                                      %trim(intcol(3))
063400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
063500031201     C     minu:maiu     XLATE     vindta        vindta
063600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
063700040324     C                   DOW       posDa <= %len(%trimr(vindta))
063800030715     C*
063900030715     C* Gestisco il 1° campo
064000030715     C                   IF        i = 1
064100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
064200030715     C                   EVAL      posDa = 1
064300030715     C* Eseguo lo scan x trovare la fine del primo campo
064400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064500030715     C* A questo punto "estraggo" il campo corrente
064600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064700030715     C                                                    (posA-posDa))
064800030715     C* X i campi successivi al 1°
064900030715     C                   ELSE
065000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
065100030715     C                   EVAL      posDa = posA + 1
065200030715     C* Eseguo lo scan x trovare la fine del campo corrente
065300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065400030715     C* Gestisco l'ultimo campo
065500030715     C                   IF        posA = *zeros
065600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
065700030715     C* A questo punto "estraggo" il campo corrente
065800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065900030715     C                                                    (posA-posDa))
066000030715     C                   LEAVE
066100030715     C                   ELSE
066200030715     C* A questo punto "estraggo" il campo corrente
066300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066400030715     C                                                    (posA-posDa))
066500030715     C                   ENDIF
066600030715     C                   ENDIF
066700030715     C* Incremento il contatore d campo
066800030715     C                   EVAL      i = i +1
066900030715     C                   ENDDO
067000030715     C*
067100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
067200030715     C* delimitazione testo
067300030715     C                   EVAL      i = 1
067400030715     C                   DOW       i < %elem(SkSplitFLD)
067500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
067600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
067700030715     C                   EVAL      i = i + 1
067800030715     C                   ENDDO
067900030715     C                   EVAL      wGiro = 1
068000030715     C                   ELSE
068100030715     C                   EVAL      wGiro = 2
068200031204     C**********
068300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
068400031204     C* d separatore campo
068500031204     C**********
068600040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
068700040304     C                   EVAL      vindta = ' ' + vindta
068800040304     C                   ENDIF
068900040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
069000040304     C***                EVAL      vindta = %subst(vindta:2)
069100040304     C***                ENDDO
069200031204     C*
069300031204     C                   Z-ADD     *zeros        lunghInput        4 0
069400040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
069500040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
069600040304     C                   EVAL      vindta = vindta + ' '
069700040304     C                   ENDIF
069800040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
069900040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
070000040304     C***                EVAL      lunghInput = %len(%trim(vindta))
070100040304     C***                ENDDO
070200030715     C**********
070300030715     C* Effettuo lo split del campo dati d input
070400030715     C**********
070500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070600040324     C                   DOW       posDa <= %len(%trimr(vindta))
070700030715     C*
070800030715     C* Gestisco il 1° campo
070900030715     C                   IF        i = 1
071000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071100030715     C                   EVAL      posDa = 1
071200030715     C* Eseguo lo scan x trovare la fine del primo campo
071300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071400030715     C* A questo punto "estraggo" il campo corrente
071500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071600030715     C                                                    (posA-posDa))
071700030715     C* X i campi successivi al 1°
071800030715     C                   ELSE
071900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072000030715     C                   EVAL      posDa = posA + 1
072100031204     C* Verifico che nn vi sia il campo nn valorizzato
072200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
072300031204     C* Se campo nn valorizzato skippo al prossimo
072400031204     C                   EVAL      SkSplitCSV(i) = *blanks
072500031204     C                   EVAL      posA  = posA + 1
072600031204     C                   ELSE
072700030715     C* Eseguo lo scan x trovare la fine del campo corrente
072800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072900030715     C* Gestisco l'ultimo campo
073000030715     C                   IF        posA = *zeros
073100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C                   LEAVE
073600030715     C                   ELSE
073700030715     C* A questo punto "estraggo" il campo corrente
073800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073900030715     C                                                    (posA-posDa))
074000030715     C                   ENDIF
074100031204     C                   ENDIF
074200030715     C                   ENDIF
074300030715     C* Incremento il contatore d campo
074400030715     C                   EVAL      i = i +1
074500030715     C                   ENDDO
074600030715     C*
074700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074800030715     C* delimitazione testo
074900030715     C                   EVAL      i = 1
075000030715     C                   DOW       i < %elem(SkSplitCSV)
075100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
075200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
075300030715     C                   EVAL      i = i + 1
075400030715     C                   ENDDO
075500030715     C*
075600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
075700030715     C                   EVAL      i = 1
075800030715     C                   DOW       i < %elem(SkSplitFLD)
075900030715     C*
076000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
076100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
076200030715     C                             SkSplitCSV(i) <> *blanks
076300030721     C*** ATB
076400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
076500030721     C                   EVAL      VABATB = SkSplitCSV(i)
076600030721     C                   ENDIF
076700030715     C*** RSD
076800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
076900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
077000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
077100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
077200030715     C* ==
077300030715     C                   ENDIF
077400030721     C*** RD2
077500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
077600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
077700030721     C                   ENDIF
077800030715     C*** IND
077900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
078000030715     C                   EVAL      VABIND = SkSplitCSV(i)
078100030715     C                   ENDIF
078200040616     C*** PRD
078300040616     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
078400040616     C                   EVAL      VABPRD = SkSplitCSV(i)
078500040616     C                   ENDIF
078600030715     C*** LOD
078700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
078800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
078900040616     C                   IF        %subst(%trim(VABLOD):%len(%trim(VABLOD))-1:2)
079000040621     C                             = VABPRD AND %len(%trim(VABLOD)) > 2
079100040616     C                   EVAL      VABLOD =
079200040616     C                             %subst(%trim(VABLOD):1:%len(%trim(VABLOD))-2)
079300040621     C                   ELSE
079400040616     C                   IF        %subst(%trim(VABLOD):%len(%trim(VABLOD))-3:4)
079500040621     C                             = '('+VABPRD+')' AND %len(%trim(VABLOD)) > 3
079600040616     C                   EVAL      VABLOD =
079700040616     C                             %subst(%trim(VABLOD):1:%len(%trim(VABLOD))-4)
079800040621     C                   ENDIF
079900040616     C                   ENDIF
080000040616     C                   ENDIF
080100030721     C*** NZD
080200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
080300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
080400030721     C                   ENDIF
080500030715     C*** RMA
080600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
080700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
080800030715     C                   ENDIF
080900030715     C*** NOT
081000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
081100040621     C                   IF        SkSplitCSV(i) <> *blanks
081200040621     C                   EVAL      VABNOT = 'CONSEGNA RICHIESTA IL: ' +
081300040621     C                                      SkSplitCSV(i)
081400030715     C                   ENDIF
081500040621     C                   ENDIF
081600030715     C*** NT2
081700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
081800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
081900030715     C                   ENDIF
082000030715     C*** VCA
082100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
082200040616     C                   IF        SkSplitCSV(i) = '€'
082300040616     C                   EVAL      VABVCA = 'EUR'
082400030715     C                   ENDIF
082500040616     C                   ENDIF
082600040616     C*** VAS
082700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
082800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
082900030715     C                   ENDIF
083000030715     C*** FFD
083100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
083200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
083300030715     C                   ENDIF
083400030715     C*** CBO
083500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
083600030715     C                   EVAL      VABCBO = SkSplitCSV(i)
083700030715     C                   ENDIF
083800030715     C
083900030721     C*** GC1
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
084100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030721     C*** GC2
084400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
084500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
084600030721     C                   ENDIF
084700030721     C*** TSP
084800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
085000030721     C                   ENDIF
085100030721     C*** NAS
085200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085400030721     C                   ENDIF
085500030721     C*** TIC
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030721     C*** GCA
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
086100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086200030721     C                   ENDIF
086300030721     C*** XCO
086400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086600030721     C                   ENDIF
086700030721     C*** CTM
086800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
087000030721     C                   ENDIF
087100030721     C*** TCR
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** CTS
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** FTM
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
088100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** VAD
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** GMA
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GGA
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** GVA
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** TC1
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
090100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** TC2
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** SCL
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** RMO
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** NMO
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*
092000030721     C* Reperisco quindi i campi numerici...
092100030721     C*** CAD
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
092300030721     C                   EVAL      PiStr=SkSplitCSV(i)
092400030721     C                   EXSR      CHKNUM
092500030721     C                   IF        PiInt=*on
092600030721     C                   Z-ADD     PiVal         Num5_0
092700030721     C                   MOVEL(p)  Num5_0        VABCAD
092800030721     C                   ELSE
092900030721     C                   SETON                                        32
093000030721     C                   EVAL      VABCAD = *zeros
093100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093200030721     C                             + ' ' + 'VABCAD'
093300030721     C                   ENDIF
093400030721     C                   ENDIF
093500030721     C*** RMN
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
093700030721     C                   EVAL      PiStr=SkSplitCSV(i)
093800030721     C                   EXSR      CHKNUM
093900030721     C                   IF        PiInt=*on
094000030721     C                   Z-ADD     PiVal         VABRMN
094100040616     C                   MOVEL     VABRMN        VABRMA
094200030721     C                   ELSE
094300030721     C                   SETON                                        32
094400030721     C                   EVAL      VABRMN = 1
094500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094600030721     C                             + ' ' + 'VABRMN'
094700030721     C                   ENDIF
094800030721     C                   ENDIF
094900030721     C*** NSP
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
095100030721     C                   EVAL      PiStr=SkSplitCSV(i)
095200030721     C                   EXSR      CHKNUM
095300030721     C                   IF        PiInt=*on AND
095400030721     C                             PiVal<=9999999
095500030721     C                   Z-ADD     PiVal         VABNSP
095600030721     C                   ELSE
095700030721     C                   SETON                                        32
095800030721     C                   EVAL      VABNSP = *zeros
095900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096000030721     C                             + ' ' + 'VABNSP'
096100030721     C                   ENDIF
096200030721     C                   ENDIF
096300030721     C*** NCL
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
096500030721     C                   EVAL      PiStr=SkSplitCSV(i)
096600030721     C                   EXSR      CHKNUM
096700030721     C                   IF        PiInt=*on
096800030721     C                   Z-ADD     PiVal         VABNCL
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABNCL = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABNCL'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** PKB
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiNum=*on
098100030721     C                   Z-ADD     PiVal         VABPKB
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABPKB = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABPKB'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** CAS
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiNum=*on
099400040716     C                   Z-ADD     PiVal         wFlgCAS
099500030721     C                   ELSE
099600040716     C                   Z-ADD     *zeros        wFlgCAS
099700030721     C                   SETON                                        32
099800030721     C                   EVAL      VABCAS = *zeros
099900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100000030721     C                             + ' ' + 'VABCAS'
100100030721     C                   ENDIF
100200030721     C                   ENDIF
100300030721     C*** CCM
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
100500030721     C                   EVAL      PiStr=SkSplitCSV(i)
100600030721     C                   EXSR      CHKNUM
100700030721     C                   IF        PiInt=*on      AND
100800030721     C                             PiVal<=9999999 AND
100900030721     C                             PiVal>*zeros
101000030721     C                   Z-ADD     PiVal         VABCCM
101100030721     C                   ELSE
101200030721     C                   SETON                                        32
101300030721     C                   EVAL      VABCCM = *zeros
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABCCM'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** LNP
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on    AND
102300030721     C                             PiVal<=999   AND
102400030721     C                             PiVal>*zeros
102500030721     C                   Z-ADD     PiVal         VABLNP
102600030721     C                   ELSE
102700030721     C                   SETON                                        32
102800030721     C                   EVAL      VABLNP = *zeros
102900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103000030721     C                             + ' ' + 'VABLNP'
103100030721     C                   ENDIF
103200030721     C                   ENDIF
103300030721     C*** NRS
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
103500030721     C                   EVAL      PiStr=SkSplitCSV(i)
103600030721     C                   EXSR      CHKNUM
103700030721     C                   IF        PiInt=*on AND
103800030721     C                             PiVal<=99
103900030721     C                   Z-ADD     PiVal         VABNRS
104000030721     C                   ELSE
104100030721     C                   SETON                                        32
104200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104300030721     C                             + ' ' + 'VABNRS'
104400030721     C                   ENDIF
104500030721     C                   ENDIF
104600030721     C*** LNA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
104800030721     C                   EVAL      PiStr=SkSplitCSV(i)
104900030721     C                   EXSR      CHKNUM
105000030721     C                   IF        PiInt=*on    AND
105100030721     C                             PiVal<=999   AND
105200030721     C                             PiVal>*zeros
105300030721     C                   Z-ADD     PiVal         VABLNA
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      VABLNA = *zeros
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800030721     C                             + ' ' + 'VABLNA'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** CTR
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on  AND
106600030721     C                             PiVal<=999
106700030721     C                   Z-ADD     PiVal         VABCTR
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107100030721     C                             + ' ' + 'VABCTR'
107200030721     C                   ENDIF
107300030721     C                   ENDIF
107400030721     C*** IAS
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
107600030721     C                   EVAL      PiStr=SkSplitCSV(i)
107700030721     C                   EXSR      CHKNUM
107800030721     C                   IF        PiNum=*on
107900030721     C                   Z-ADD     PiVal         VABIAS
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABIAS'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** VLB
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABVLB
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABVLB'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** QFT
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABQFT
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABQFT'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** NCD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on      AND
111500030721     C                             PiVal<=9999999
111600030721     C                   Z-ADD     PiVal         VABNCD
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000030721     C                             + ' ' + 'VABNCD'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** NCA
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiInt=*on      AND
112800030721     C                             PiVal<=9999999
112900030721     C                   Z-ADD     PiVal         VABNCA
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300030721     C                             + ' ' + 'VABNCA'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** ZNC
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on AND
114100030721     C                             PiVal<=99
114200030721     C                   Z-ADD     PiVal         VABZNC
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABZNC'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030721     C*** DCR
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
115100040621     C                   EVAL      PiStr=SkSplitCSV(i)
115200030721     C                   EXSR      CHKNUM
115300030721     C                   IF        PiInt=*on       AND
115400030721     C                             PiVal<=99999999
115500030721     C                   Z-ADD     PiVal         VABDCR
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABDCR'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** HCR
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on   AND
116700030721     C                             PiVal<=9999
116800030721     C                   Z-ADD     PiVal         VABHCR
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABHCR'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500040716     C*** VMD (in realtà x il cliente è il contrassegno...  nn ho parole!!!)
117600040716     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD' AND
117700040716     C                             wFlgCAS = 101
117800040616     C                   IF        SkSplitCSV(i) <> *blanks
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200040716     C                   Z-ADD     PiVal         VABCAS
118300040716     C                   EVAL      VABVCA = 'EUR'
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700040716     C                             + ' ' + 'VABVMD=>VABCAS'
118800030721     C                   ENDIF
118900040616     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** ANT
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on        AND
119600030721     C                             PiVal<=999999999
119700030721     C                   Z-ADD     PiVal         VABANT
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABANT'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CMO
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on
120900030721     C                   Z-ADD     PiVal         Num5_0
121000030721     C                   MOVEL(p)  Num5_0        VABCMO
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABCMO = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABCMO'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800090908     C*** W_VABTIC
121900090908     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
122000090908     C                   IF        %trim(SkSplitCSV(i)) = '100'
122100090908     C                   EVAL      VABTIC = 'BM'
122200090908     C                   ENDIF
122300090908     C                   ENDIF
122400030715     C*
122500030715     C                   ENDIF
122600030715     C                   EVAL      i = i + 1
122700030715     C                   ENDDO
122800020204     C*
122900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
123000020204     C                   EXSR      CHKIMPDIV
123100030715     C                   ENDIF
123200020204     C*
123300010607     C                   ENDSR
123400010607     C*----------------------------------------------------*
123500010601
123600020204
123700020204     C*----------------------------------------------------*
123800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
123900020204     C*----------------------------------------------------*
124000020204     C     CHKIMPDIV     BEGSR
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
139600010601
139700010601
139800050421
139900050421     C     *pssr         BEGSR
140000050421     C*
140100050421     C                   eval      wrkesito = '2'
140200050421     C*
140300050421     C                   ENDSR     '*CANCL'
140400050421     C***
140500050421
140600010601
140700010330
140800010330
140900000613     C     *inzsr        BEGSR
141000990910     C*
141100990910     C     *entry        plist
141200990920     C                   parm                    tivlrds
141300990921     C                   parm      wrkesito      esito
141400000724     C                   parm                    prmlit
141500000710     C                   parm                    prmfir
141600010330     C*
141700010330     C* CALCOLA LA DATA CORRENTE
141800010330     C                   time                    wn14             14 0
141900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
142000010330     C                   z-add     wn8           g08dat
142100010330     C                   z-add     *zeros        g08inv
142200010330     C                   movel     '0'           g08err
142300010330     C                   call      'XSRDA8'
142400010330     C                   parm                    wlbda8
142500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
142600000613     C*
142700000613     C                   ENDSR
142800000613     C***
142900990908
143000040616     Otitvc8p   E            riepilogo         2
143100990915     O                                              'Upload via Internet'
143200990915     O                                           +1 'Traduzione TIVIN00R -'
143300021025     O                                           +1 'FIVABWWR'
143400990915     O                                           +1 'Testate bolle'
143500990915     O          E            riepilogo   2
143600990915     O                       wrkdata
143700990915     O                       wrkora              +1
143800990915     O                       procname            +1
143900990915     O          E            riepilogo   2
144000990915     O                                              'Cliente..................:'
144100010605     O                       VABCCM        z     +1
144200990915     O          E            riepilogo   2
144300990920     O                                              'Riferimento Strategi.....:'
144400990920     O                       vlrhdl              +1
144500990915     O          E            riepilogo   2
144600990915     O                                              'Giusti...................:'
144700971022     O                       §CTROK        2   +  1
144800990915     O          E            riepilogo   2
144900990915     O                                              'Sbagliati e corretti.....:'
145000971022     O                       §CTRMO        2   +  1
145100990915     O          E            riepilogo   2
145200990915     O                                              'Sbagliati e scartati.....:'
145300971022     O                       §CTRNO        2   +  1
145400000613
145500040616     Otitvc8ps  E            testdett          2
145600000613     O                                              'Upload via Internet'
145700000613     O                                           +1 'Traduzione TIVIN00R -'
145800021025     O                                           +1 'FIVABWWR'
145900000613     O                                           +1 'Testate bolle'
146000000616     O          E            testdett    3
146100000613     O                                           +2 'N° rec'
146200000613     O                                           +3 'Anteprima contenuto'
146300000616     O          E            rigadett    2
146400000613     O                       rrnum               +2
146500000621     O                       recko               +3
146600000616     O          E            findett     2
146700000613     O                       wrkdata
146800000613     O                       wrkora              +1
146900000613     O                       procname            +1
147000000616     O          E            findett     2
147100000613     O                                              'Cliente..................:'
147200010605     O                       VABCCM        z     +1
147300000616     O          E            findett     2
147400000613     O                                              'Riferimento Strategi.....:'
147500000613     O                       vlrhdl              +1
147600000616     O          E            findett     2
147700000613     O                                              'Giusti...................:'
147800000613     O                       §CTROK        2   +  1
147900000616     O          E            findett     2
148000000613     O                                              'Sbagliati e corretti.....:'
148100000613     O                       §CTRMO        2   +  1
148200000616     O          E            findett     2
148300000613     O                                              'Sbagliati e scartati.....:'
148400000613     O                       §CTRNO        2   +  1
148500000613** CMD - COMANDI CL
148600040616OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC8P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
148700040616OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVC8PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
148800040616DLTOVR FILE(TITVC8P TITVC8PS) LVL(*)
148900010606
149000000613
149100040616** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
149200040621VABRMN;VABCBO;VABRSD;VABRD2;VABIND;VABCAD;VABPRD;VABLOD;VABNCL;VABPKB;
149300110527VABAAS;W_VABTIC;VABCAS;VABVMD;VABNOT;VABVCA;VABIAS;
149400040616
