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
002300080222     D tisi95ds      e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D wrkdata         s               d
002900010330     D wrkora          s               t
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100010330     D recko           s            150    INZ(*blanks)
003200011113     D depcmd          s            150    INZ(*blanks)
003300030715     D*------------------
003400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003500030715     D*------------------
003600030715     D SkSplitFLD      S             10    DIM(100)
003700030715     D SkSplitCSV      S            256    DIM(100)
003800030715     D CharCSV         S              1
003900030715     D CharTXT         S              1
004000030715     D CharNUM         S              1
004100030715     D posDa           S              3  0 INZ(*zeros)
004200030715     D posA            S              3  0 INZ(*zeros)
004300030715     D i               s              3  0 INZ(1)
004400030715     D wGiro           s              1  0 INZ(*zeros)
004500030822     D*------------------
004600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004700030822     D*------------------
004800030822     D posDaDft        S              3  0 INZ(*zeros)
004900030822     D posADft         S              3  0 INZ(*zeros)
005000030822     D j               s              3  0 INZ(1)
005100030822     D wGiroDft        s              1  0 INZ(*zeros)
005200010330     D*------------------
005300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400010330     D*------------------
005500010330     D WLBDA8          DS                  INZ
005600010330     D  G08DAT                 1      8  0
005700010330     D  G08INV                 9     16  0
005800010330     D  G08ERR                17     17
005900010330     D  G08TGI                18     22  0
006000031201     D*------------------
006100031201     D* Costanti
006200031201     D*------------------
006300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006500010330
006600010330
006700010330
006800990915     C                   time                    wrkdata
006900990915     C                   time                    wrkora
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300010601     C*
007400010601     C                   exsr      opeini
007500010605     C                   exsr      rwvab
007600010601     C*
007700010601     C                   seton                                        lr
007800010601
007900010601
008000010601
008100010601
008200010601     C*--------------------------------------------------------
008300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010601     C*--------------------------------------------------------
008500010601     C     PREELA        BEGSR
008600010601     C*
008700010601     C* SE OCCORRE SPEDIRE IN FILIALE
008800010601     C                   if        invfil <> *zeros and
008900010601     C                             flgGiro = '0'
009000010601     C*
009100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010601     C                   eval      flgGiro = '1'
009300000724     C*
009400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009600010601     C     invfil        chain     azorg01l
009700000724     C                   if        %found
009800000724     C                   movel(p)  CMD(1)        depcmd
009900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010000000724     C*
010100000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010200011113     C                   Z-ADD     150           LENGH            15 5
010300000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010400000724     C                   PARM                    depcmd
010500000724     C                   PARM                    LENGH
010600000724     C*
010700000724     C                   endif
010800000616     C*
010900000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011000000616     C                   movel(p)  CMD(2)        depcmd
011100000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011200000616     C*
011300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011400011113     C                   Z-ADD     150           LENGH            15 5
011500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011600000616     C                   PARM                    depcmd
011700000616     C                   PARM                    LENGH
011800000613     C*
011900030715     C                   if        not %open(titvxlps)
012000030715     C                   open      titvxlps
012100030715     C                   except    testdett
012200000613     C                   endif
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900990915     C*
014000000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014100080222     C*
014200080222     C* Effettuo lancio TISI95 solo x chiusura
014300080222     C                   CLEAR                   TISI95DS
014400080222     C                   EVAL      I95TLA = 'C'
014500080222     C                   CALL      'TISI95R'
014600080222     C                   PARM                    TISI95DS
014700000613     C*
014800030715     C                   if        %open(titvxlps)
014900000616     C                   except    findett
015000030715     C                   close     titvxlps
015100000613     C                   endif
015200000616     C*
015300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015400011113     C                   Z-ADD     150           LENGH            15 5
015500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015600000616     C                   PARM                    CMD(3)
015700000616     C                   PARM                    LENGH
015800000616     C*
015900010601     C                   ENDSR
016000010601     C***
016100010601
016200010601
016300010601
016400010601
016500000613
016600000613     C*--------------------------------------------------------
016700000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016800000613     C*--------------------------------------------------------
016900000621     C     STPR          BEGSR
017000000613     C*
017100030715     C                   if        not %open(titvxlp)
017200030715     C   50              open      titvxlp
017300990915     C                   endif
017400990915     C*
017500030715     C   50              except    riepilogo
017600990915     C*
017700030715     C                   if        %open(titvxlp)
017800030715     C   50              close     titvxlp
017900990914     C                   endif
018000000613     C*
018100000613     C                   ENDSR
018200000613     C***
018300000613
018400010330
018500010601
018600010601
018700010601
018800010330     C*--------------------------------------------------------
018900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
019000010330     C*--------------------------------------------------------
019100010605     C     RWVAB         BEGSR
019200010330     C*
019300010330     C                   if        not %open(tivin00r)
019400010330     C                   open      tivin00r
019500010330     C                   endif
019600021025     C                   if        not %open(fivabwwr)
019700021025     C                   open      fivabwwr
019800010330     C                   endif
019900010330     C*
020000010604     C                   clear                   §CTROK
020100010604     C                   clear                   §CTRMO
020200010604     C                   clear                   §CTRNO
020300010330     C*
020400030822     C                   do        *HIVAL
020500010330     C*
020600030822     C                   read      tivin00r                               70
020700010618     C*
020800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020900010618     C                   if        vinflg = '1'
021000010618     C                   eval      flgOk = '1'
021100010618     C                   endif
021200010618     C*
021300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021400040510     C                   z-add     *zeros        wLenVINDTA        4 0
021500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021900040510     C                   z-add     1             wFlgVINDTA
022000040510     C                   leave
022100040510     C                   endif
022200040510     C                   enddo
022300040510     C*
022400040218     C                   if        vindta > *blanks AND
022500040510     C                             wFlgVINDTA = 1
022600010330     C                   add       1             rrnum
022700010330     C*
022800010601     C                   if        *in70 = *off and
022900010330     C                             (vinflg = *blanks
023000010330     C                              or vinflg = '0'
023100010330     C                              or vinflg = '2')
023200010330     C*
023300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023400010711     C                   if        vinflg = *blanks or vinflg = '0'
023500010711     C                   clear                   vinmsg
023600010711     C                   endif
023700010601     C*
023800010330     C                   exsr      inzvar
023900010605     C                   exsr      impvab
024000010601     C*
024100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024200010605     C                   eval      depfil = VABLNP
024300010601     C                   exsr      repfil
024400060331     C                   if        VABFGS = *zeros
024401080616     C                   eval      VABFGS = VABLNP
025200060331     C                   endif
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
027000070102     C                   if        %open(titvxlps)
027100070102     C                   except    rigadett
027200070102     C                   endif
027300010604     C                   eval      vinflg = '2'
027400010604     C                   endif
027500010601     C*
027600010604     C                   endif
027700010604     C*
027800010330     C                   else
027900010330     C                   eval      vinflg = '1'
028000010330     C                   endif
028100010601     C*
028200010601     C  N70              update    tivin000
028300010330     C*
028400030822     C  N70              enddo
028500010601     C
028600010601     C*
028700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028800010601     C                   if        cntNonEl = *zeros or
028900010601     C                             flgMulti = '0'
029000010330     C* Se non ci sono record con errori ...
029100010601     C                   if        §ctrno = 0 and
029200010604     C                             §ctrmo = 0 and
029300010601     C                             flgStato <> '2'
029400010330     C* ... restituisco esito OK.
029500010330     C                   eval      wrkesito = '0'
029600010330     C                   else
029700010330     C                   if        §ctrok > 0
029800010330     C                   eval      wrkesito = '1'
029900010330     C                   else
030000010615     C                   if        flgOk = '0'
030100010615     C                   eval      wrkesito = '2'
030200010615     C                   else
030300010615     C                   eval      wrkesito = '6'
030400010615     C                   endif
030500010330     C                   endif
030600010330     C                   endif
030700010601     C                   else
030800010601     C                   eval      wrkesito = '9'
030900010601     C                   endif
031000010330     C*
031100010330     C                   if        %open(tivin00r)
031200010330     C                   close     tivin00r
031300010330     C                   endif
031400021025     C                   if        %open(fivabwwr)
031500021025     C                   close     fivabwwr
031600010330     C                   endif
031700010601     C*
031800010601     C                   if        vlrpoi <> 999
031900010601     C                   eval      invfil = vlrpoi
032000010601     C                   endif
032100010330     C*
032200010330     C                   if        §ctrok > 0
032300010601     C                             and invfil > *zeros
032400010330     C                   exsr      invio
032500010330     C                   endif
032600010601     C*
032700010618     C                   if        flgGiro = '1'
032800010601     C                   exsr      endela
032900010618     C                   endif
033000010330     C*
033100010330     C                   ENDSR
033200010330     C***
033300010601
033400010601
033500010601
033600010330     C*----------------------------------------------------*
033700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033800010330     C*----------------------------------------------------*
033900010330     C     INZVAR        BEGSR
034000010330     C*
034100030822     C* Inizializzo variabili di wrk
034200020204     C                   Z-ADD     *zeros        Num5_0            5 0
034300030822     C*
034400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
034500030822     C                   CLEAR                   FIVAB000
034600030822     C                   CLEAR                   SkSplitCSV
034700030822     C                   EVAL      i = 1
034800030822     C                   EVAL      posDa = *zeros
034900030822     C                   EVAL      posA  = *zeros
035000030822     C*
035100030822     C* Reimposto i valori di default
035200030822     C                   EXSR      DEFCAM
035300030822     C*
035400010330     C                   ENDSR
035500010330     C*----------------------------------------------------*
035600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
035700010330     C*----------------------------------------------------*
035800010330     C     DEFCAM        BEGSR
035900030822     C*
036000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
036100030715     C* e delimitatore testo.
036200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
036500030822     C*
036600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
036700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
036800030715     C                   SETON                                        50
036900030715     C                   ELSE
037000030715     C                   SETOFF                                       50
037100030715     C                   ENDIF
037200030822     C*
037300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
037400030822     C                   EVAL      posDaDft = 1
037500030822     C                   EVAL      posADft  = 0
037600030822     C                   EVAL      wGiroDft = 0
037700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
037800030822     C                             posDaDft > 0
037900030822     C*
038000030822     C* Gestisco il 1° giro
038100030822     C                   IF        wGiroDft = 0
038200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
038300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
038400030822     C* Incremento il contatore dei "giri"
038500030822     C                   EVAL      wGiroDft = 1
038600030822     C                   ELSE
038700030822     C                   EVAL      posDaDft = posADft
038800030822     C                   ENDIF
038900030822     C* Eseguo lo scan x trovare la fine del campo corrente
039000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
039100030822     C*
039200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
039300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
039400030822     C                   IF        posDaDft > 0 AND
039500030822     C                             posADft  > 0
039600060331     C* FGS
039700060331     C                   IF        %subst(
039800060331     C                             %subst(vlrppt:posDaDft+1:
039900060331     C                             posADft-posDaDft-1):1:3)
040000060331     C                             = 'FGS'
040100060331     C                   EVAL      PiStr=%trim(%subst(
040200060331     C                             %subst(vlrppt:posDaDft+1:
040300060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040400060331     C                   EXSR      CHKNUM
040500060331     C                   IF        PiInt=*on
040600060331     C                   Z-ADD     PiVal         VABFGS
040700060331     C                   ENDIF
040800060331     C                   ENDIF
040900030822     C* NCL
041000030822     C                   IF        %subst(
041100030822     C                             %subst(vlrppt:posDaDft+1:
041200030822     C                             posADft-posDaDft-1):1:3)
041300030822     C                             = 'NCL'
041400030822     C                   EVAL      PiStr=%trim(%subst(
041500030822     C                             %subst(vlrppt:posDaDft+1:
041600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041700030822     C                   EXSR      CHKNUM
041800030822     C                   IF        PiInt=*on
041900030822     C                   Z-ADD     PiVal         VABNCL
042000030822     C                   ENDIF
042100030822     C                   ENDIF
042200030822     C* CCM
042300030822     C                   IF        %subst(
042400030822     C                             %subst(vlrppt:posDaDft+1:
042500030822     C                             posADft-posDaDft-1):1:3)
042600030822     C                             = 'CCM'
042700030822     C                   EVAL      PiStr=%trim(%subst(
042800030822     C                             %subst(vlrppt:posDaDft+1:
042900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043000030822     C                   EXSR      CHKNUM
043100030822     C                   IF        PiInt=*on
043200030822     C                   Z-ADD     PiVal         VABCCM
043300030822     C                   ENDIF
043400030822     C                   ENDIF
043500030822     C* LNP
043600030822     C                   IF        %subst(
043700030822     C                             %subst(vlrppt:posDaDft+1:
043800030822     C                             posADft-posDaDft-1):1:3)
043900030822     C                             = 'LNP'
044000030822     C                   EVAL      PiStr=%trim(%subst(
044100030822     C                             %subst(vlrppt:posDaDft+1:
044200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044300030822     C                   EXSR      CHKNUM
044400030822     C                   IF        PiInt=*on
044500030822     C                   Z-ADD     PiVal         VABLNP
044600030822     C                   ENDIF
044700030822     C                   ENDIF
044800030822     C* NRS
044900030822     C                   IF        %subst(
045000030822     C                             %subst(vlrppt:posDaDft+1:
045100030822     C                             posADft-posDaDft-1):1:3)
045200030822     C                             = 'NRS'
045300030822     C                   EVAL      PiStr=%trim(%subst(
045400030822     C                             %subst(vlrppt:posDaDft+1:
045500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045600030822     C                   EXSR      CHKNUM
045700030822     C                   IF        PiInt=*on
045800030822     C                   Z-ADD     PiVal         VABNRS
045900030822     C                   ENDIF
046000030822     C                   ENDIF
046100030822     C* CTR
046200030822     C                   IF        %subst(
046300030822     C                             %subst(vlrppt:posDaDft+1:
046400030822     C                             posADft-posDaDft-1):1:3)
046500030822     C                             = 'CTR'
046600030822     C                   EVAL      PiStr=%trim(%subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046900030822     C                   EXSR      CHKNUM
047000030822     C                   IF        PiInt=*on
047100030822     C                   Z-ADD     PiVal         VABCTR
047200030822     C                   ENDIF
047300030822     C                   ENDIF
047400030822     C* PKB
047500030822     C                   IF        %subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):1:3)
047800030822     C                             = 'PKB'
047900030822     C                   EVAL      PiStr=%trim(%subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048200030822     C                   EXSR      CHKNUM
048300030822     C                   IF        PiNum=*on
048400030822     C                   Z-ADD     PiVal         VABPKB
048500030822     C                   ENDIF
048600030822     C                   ENDIF
048700030822     C* VLB
048800030822     C                   IF        %subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):1:3)
049100030822     C                             = 'VLB'
049200030822     C                   EVAL      PiStr=%trim(%subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049500030822     C                   EXSR      CHKNUM
049600030822     C                   IF        PiNum=*on
049700030822     C                   Z-ADD     PiVal         VABVLB
049800030822     C                   ENDIF
049900030822     C                   ENDIF
050000030822     C* QFT
050100030822     C                   IF        %subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):1:3)
050400030822     C                             = 'QFT'
050500030822     C                   EVAL      PiStr=%trim(%subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800030822     C                   EXSR      CHKNUM
050900030822     C                   IF        PiNum=*on
051000030822     C                   Z-ADD     PiVal         VABQFT
051100030822     C                   ENDIF
051200030822     C                   ENDIF
051300030822     C* CBO
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'CBO'
051800030822     C                   EVAL      VABCBO=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   ENDIF
052200030822     C* TSP
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'TSP'
052700030822     C                   EVAL      VABTSP=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   ENDIF
053100030822     C* VAS
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'VAS'
053600030822     C                   EVAL      VABVAS=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   ENDIF
054000030822     C* VCA
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'VCA'
054500030822     C                   EVAL      VABVCA=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   ENDIF
054900030822     C* GCA
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'GCA'
055400030822     C                   EVAL      VABGCA=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   ENDIF
055800030822     C* CTM
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'CTM'
056300030822     C                   EVAL      VABCTM=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* FFD
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'FFD'
057200030822     C                   EVAL      VABFFD=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* VAD
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'VAD'
058100030822     C                   EVAL      VABVAD=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* GMA
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'GMA'
059000030822     C                   EVAL      VABGMA=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* GGA
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'GGA'
059900030822     C                   EVAL      VABGGA=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* GVA
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'GVA'
060800030822     C                   EVAL      VABGVA=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* TC1
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'TC1'
061700030822     C                   EVAL      VABTC1=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* TC2
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'TC2'
062600030822     C                   EVAL      VABTC2=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* ...
063100030822     C                   ENDIF
063200030822     C                   ENDDO
063300020204     C*
063400010330     C                   ENDSR
063500010607     C*----------------------------------------------------*
063600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063700010607     C*----------------------------------------------------*
063800010607     C     IMPVAB        BEGSR
063900010607     C*
064000010607     C                   SETOFF                                       3132
064100030515     C*
064200010607     C                   MOVEL     datcor        VABAAS
064300010607     C                   MOVE      datcor        VABMGS
064400010607     C*
064500030715     C**********
064600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
064700030715     C**********
064800030715     C                   IF        wGiro = *zeros
064900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
065000031201     C     minu:maiu     XLATE     vindta        vindta
065100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
065200040324     C                   DOW       posDa <= %len(%trimr(vindta))
065300030715     C*
065400030715     C* Gestisco il 1° campo
065500030715     C                   IF        i = 1
065600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
065700030715     C                   EVAL      posDa = 1
065800030715     C* Eseguo lo scan x trovare la fine del primo campo
065900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066000030715     C* A questo punto "estraggo" il campo corrente
066100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066200030715     C                                                    (posA-posDa))
066300030715     C* X i campi successivi al 1°
066400030715     C                   ELSE
066500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
066600030715     C                   EVAL      posDa = posA + 1
066700030715     C* Eseguo lo scan x trovare la fine del campo corrente
066800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066900030715     C* Gestisco l'ultimo campo
067000030715     C                   IF        posA = *zeros
067100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
067200030715     C* A questo punto "estraggo" il campo corrente
067300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067400030715     C                                                    (posA-posDa))
067500030715     C                   LEAVE
067600030715     C                   ELSE
067700030715     C* A questo punto "estraggo" il campo corrente
067800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067900030715     C                                                    (posA-posDa))
068000030715     C                   ENDIF
068100030715     C                   ENDIF
068200030715     C* Incremento il contatore d campo
068300030715     C                   EVAL      i = i +1
068400030715     C                   ENDDO
068500030715     C*
068600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
068700030715     C* delimitazione testo
068800030715     C                   EVAL      i = 1
068900030715     C                   DOW       i < %elem(SkSplitFLD)
069000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
069100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
069200030715     C                   EVAL      i = i + 1
069300030715     C                   ENDDO
069400030715     C                   EVAL      wGiro = 1
069500030715     C                   ELSE
069600030715     C                   EVAL      wGiro = 2
069700031204     C**********
069800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
069900031204     C* d separatore campo
070000031204     C**********
070100040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
070200040304     C                   EVAL      vindta = ' ' + vindta
070300040304     C                   ENDIF
070400040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
070500040304     C***                EVAL      vindta = %subst(vindta:2)
070600040304     C***                ENDDO
070700031204     C*
070800031204     C                   Z-ADD     *zeros        lunghInput        4 0
070900040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
071000040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
071100040304     C                   EVAL      vindta = vindta + ' '
071200040304     C                   ENDIF
071300040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
071400040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
071500040304     C***                EVAL      lunghInput = %len(%trim(vindta))
071600040304     C***                ENDDO
071700030715     C**********
071800030715     C* Effettuo lo split del campo dati d input
071900030715     C**********
072000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072100040324     C                   DOW       posDa <= %len(%trimr(vindta))
072200030715     C*
072300030715     C* Gestisco il 1° campo
072400030715     C                   IF        i = 1
072500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072600030715     C                   EVAL      posDa = 1
072700030715     C* Eseguo lo scan x trovare la fine del primo campo
072800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072900030715     C* A questo punto "estraggo" il campo corrente
073000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073100030715     C                                                    (posA-posDa))
073200030715     C* X i campi successivi al 1°
073300030715     C                   ELSE
073400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073500030715     C                   EVAL      posDa = posA + 1
073600031204     C* Verifico che nn vi sia il campo nn valorizzato
073700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
073800031204     C* Se campo nn valorizzato skippo al prossimo
073900031204     C                   EVAL      SkSplitCSV(i) = *blanks
074000031204     C                   EVAL      posA  = posA + 1
074100031204     C                   ELSE
074200030715     C* Eseguo lo scan x trovare la fine del campo corrente
074300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074400030715     C* Gestisco l'ultimo campo
074500030715     C                   IF        posA = *zeros
074600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
074700030715     C* A questo punto "estraggo" il campo corrente
074800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074900030715     C                                                    (posA-posDa))
075000030715     C                   LEAVE
075100030715     C                   ELSE
075200030715     C* A questo punto "estraggo" il campo corrente
075300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075400030715     C                                                    (posA-posDa))
075500030715     C                   ENDIF
075600031204     C                   ENDIF
075700030715     C                   ENDIF
075800030715     C* Incremento il contatore d campo
075900030715     C                   EVAL      i = i +1
076000030715     C                   ENDDO
076100030715     C*
076200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076300030715     C* delimitazione testo
076400030715     C                   EVAL      i = 1
076500030715     C                   DOW       i < %elem(SkSplitCSV)
076600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
076700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
076800030715     C                   EVAL      i = i + 1
076900030715     C                   ENDDO
077000030715     C*
077100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
077200030715     C                   EVAL      i = 1
077300030715     C                   DOW       i < %elem(SkSplitFLD)
077400030715     C*
077500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
077600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
077700030715     C                             SkSplitCSV(i) <> *blanks
077800030721     C*** ATB
077900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
078000030721     C                   EVAL      VABATB = SkSplitCSV(i)
078100030721     C                   ENDIF
078200030715     C*** RSD
078300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
078400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
078500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
078600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
078700030715     C* ==
078800030715     C                   ENDIF
078900030721     C*** RD2
079000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
079100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
079200030721     C                   ENDIF
079300030715     C*** IND
079400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
079500030715     C                   EVAL      VABIND = SkSplitCSV(i)
079600030715     C                   ENDIF
079700030715     C*** LOD
079800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
079900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
080000030715     C                   ENDIF
080100030715     C*** PRD
080200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
080300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
080400030715     C                   ENDIF
080500030721     C*** NZD
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
080700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
080800030721     C                   ENDIF
080900030715     C*** RMA
081000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
081100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
081200030715     C                   ENDIF
081300030715     C*** NOT
081400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
081500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
081600030715     C                   ENDIF
081700030715     C*** NT2
081800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
081900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
082000030715     C                   ENDIF
082100030715     C*** VCA
082200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
082300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
082400030715     C                   ENDIF
082500030715     C*** VAS
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
082700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
082800030715     C                   ENDIF
082900030715     C*** FFD
083000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
083100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
083200030715     C                   ENDIF
083300030715     C*** CBO
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
083500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030721     C*** GC1
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
083900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030721     C*** GC2
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
084300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030721     C*** TSP
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** NAS
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** TIC
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** GCA
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** XCO
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** CTM
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** TCR
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** CTS
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** FTM
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** VAD
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** GMA
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** GGA
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** GVA
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** TC1
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** TC2
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** SCL
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** RMO
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** NMO
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*
091800070705     C* Reperisco quindi i campi numerici...
091900030721     C*** CAD
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
092100070705     C                   EVAL      VABCAD = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** RMN
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
092500030721     C                   EVAL      PiStr=SkSplitCSV(i)
092600030721     C                   EXSR      CHKNUM
092700030721     C                   IF        PiInt=*on
092800030721     C                   Z-ADD     PiVal         VABRMN
092900030721     C                   ELSE
093000030721     C                   SETON                                        32
093100030721     C                   EVAL      VABRMN = 1
093200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093300030721     C                             + ' ' + 'VABRMN'
093400030721     C                   ENDIF
093500030721     C                   ENDIF
093600030721     C*** NSP
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093800030721     C                   EVAL      PiStr=SkSplitCSV(i)
093900030721     C                   EXSR      CHKNUM
094000030721     C                   IF        PiInt=*on AND
094100030721     C                             PiVal<=9999999
094200030721     C                   Z-ADD     PiVal         VABNSP
094300030721     C                   ELSE
094400030721     C                   SETON                                        32
094500030721     C                   EVAL      VABNSP = *zeros
094600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094700030721     C                             + ' ' + 'VABNSP'
094800030721     C                   ENDIF
094900030721     C                   ENDIF
095000030721     C*** NCL
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
095200030721     C                   EVAL      PiStr=SkSplitCSV(i)
095300030721     C                   EXSR      CHKNUM
095400030721     C                   IF        PiInt=*on
095500030721     C                   Z-ADD     PiVal         VABNCL
095600030721     C                   ELSE
095700030721     C                   SETON                                        32
095800030721     C                   EVAL      VABNCL = *zeros
095900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096000030721     C                             + ' ' + 'VABNCL'
096100030721     C                   ENDIF
096200030721     C                   ENDIF
096300030721     C*** PKB
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
096500030721     C                   EVAL      PiStr=SkSplitCSV(i)
096600030721     C                   EXSR      CHKNUM
096700030721     C                   IF        PiNum=*on
096800030721     C                   Z-ADD     PiVal         VABPKB
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABPKB = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABPKB'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** CAS
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiNum=*on
098100030721     C                   Z-ADD     PiVal         VABCAS
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABCAS = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABCAS'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** CCM
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiInt=*on      AND
099400030721     C                             PiVal<=9999999 AND
099500030721     C                             PiVal>*zeros
099600030721     C                   Z-ADD     PiVal         VABCCM
099700030721     C                   ELSE
099800030721     C                   SETON                                        32
099900030721     C                   EVAL      VABCCM = *zeros
100000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100100030721     C                             + ' ' + 'VABCCM'
100200030721     C                   ENDIF
100300030721     C                   ENDIF
100400030721     C*** LNP
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
100600030721     C                   EVAL      PiStr=SkSplitCSV(i)
100700030721     C                   EXSR      CHKNUM
100800030721     C                   IF        PiInt=*on    AND
100900030721     C                             PiVal<=999   AND
101000030721     C                             PiVal>*zeros
101100030721     C                   Z-ADD     PiVal         VABLNP
101200030721     C                   ELSE
101300030721     C                   SETON                                        32
101400030721     C                   EVAL      VABLNP = *zeros
101500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101600030721     C                             + ' ' + 'VABLNP'
101700030721     C                   ENDIF
101800030721     C                   ENDIF
101900030721     C*** NRS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
102100030721     C                   EVAL      PiStr=SkSplitCSV(i)
102200030721     C                   EXSR      CHKNUM
102300030721     C                   IF        PiInt=*on AND
102400030721     C                             PiVal<=99
102500030721     C                   Z-ADD     PiVal         VABNRS
102600030721     C                   ELSE
102700030721     C                   SETON                                        32
102800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102900030721     C                             + ' ' + 'VABNRS'
103000030721     C                   ENDIF
103100030721     C                   ENDIF
103200030721     C*** LNA
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
103400030721     C                   EVAL      PiStr=SkSplitCSV(i)
103500030721     C                   EXSR      CHKNUM
103600030721     C                   IF        PiInt=*on    AND
103700030721     C                             PiVal<=999   AND
103800030721     C                             PiVal>*zeros
103900030721     C                   Z-ADD     PiVal         VABLNA
104000030721     C                   ELSE
104100030721     C                   SETON                                        32
104200030721     C                   EVAL      VABLNA = *zeros
104300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104400030721     C                             + ' ' + 'VABLNA'
104500030721     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** CTR
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100030721     C                   IF        PiInt=*on  AND
105200030721     C                             PiVal<=999
105300030721     C                   Z-ADD     PiVal         VABCTR
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105700030721     C                             + ' ' + 'VABCTR'
105800030721     C                   ENDIF
105900030721     C                   ENDIF
106000030721     C*** IAS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
106200030721     C                   EVAL      PiStr=SkSplitCSV(i)
106300030721     C                   EXSR      CHKNUM
106400030721     C                   IF        PiNum=*on
106500030721     C                   Z-ADD     PiVal         VABIAS
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106900030721     C                             + ' ' + 'VABIAS'
107000030721     C                   ENDIF
107100030721     C                   ENDIF
107200030721     C*** VLB
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
107400030721     C                   EVAL      PiStr=SkSplitCSV(i)
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiNum=*on
107700030721     C                   Z-ADD     PiVal         VABVLB
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100030721     C                             + ' ' + 'VABVLB'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** QFT
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiNum=*on
108900030721     C                   Z-ADD     PiVal         VABQFT
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABQFT'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** NCD
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on      AND
110100030721     C                             PiVal<=9999999
110200030721     C                   Z-ADD     PiVal         VABNCD
110300030721     C                   ELSE
110400030721     C                   SETON                                        32
110500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110600030721     C                             + ' ' + 'VABNCD'
110700030721     C                   ENDIF
110800030721     C                   ENDIF
110900030721     C*** NCA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
111100030721     C                   EVAL      PiStr=SkSplitCSV(i)
111200030721     C                   EXSR      CHKNUM
111300030721     C                   IF        PiInt=*on      AND
111400030721     C                             PiVal<=9999999
111500030721     C                   Z-ADD     PiVal         VABNCA
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABNCA'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** ZNC
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on AND
112700030721     C                             PiVal<=99
112800030721     C                   Z-ADD     PiVal         VABZNC
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABZNC'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** DCR
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on       AND
114000030721     C                             PiVal<=99999999
114100030721     C                   Z-ADD     PiVal         VABDCR
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABDCR'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** HCR
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on   AND
115300030721     C                             PiVal<=9999
115400030721     C                   Z-ADD     PiVal         VABHCR
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABHCR'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** VMD
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiNum=*on
116600030721     C                   Z-ADD     PiVal         VABVMD
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABVMD'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** ANT
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on        AND
117800030721     C                             PiVal<=999999999
117900030721     C                   Z-ADD     PiVal         VABANT
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABANT'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** CMO
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on
119100030721     C                   Z-ADD     PiVal         Num5_0
119200030721     C                   MOVEL(p)  Num5_0        VABCMO
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABCMO = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABCMO'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030715     C*
120100030715     C                   ENDIF
120200030715     C                   EVAL      i = i + 1
120300030715     C                   ENDDO
120400070705     C*
120500070705     C* Considerazioni particolari
120600070705     C*** NZD
120700070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
120800070705     C                   EVAL      VABNZD = *blanks
120900070705     C                   ENDIF
121000070705     C*** CAD
121100070705     C                   IF        VABNZD = *blanks
121200070705     C                   EVAL      PiStr=VABCAD
121300070705     C                   EXSR      CHKNUM
121400070705     C                   IF        PiInt=*on
121500070705     C                   Z-ADD     PiVal         Num5_0
121600070705     C                   MOVEL(p)  Num5_0        VABCAD
121700070705     C                   ELSE
121800070705     C                   SETON                                        32
121900070705     C                   EVAL      VABCAD = *zeros
122000070705     C                   EVAL      vinmsg = %trimr(vinmsg)
122100070705     C                             + ' ' + 'VABCAD'
122200070705     C                   ENDIF
122300070705     C                   ENDIF
122400080222     C*
122500080222     C* Se provincia nn valorizzata la reperisco
122600080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
122700080222     C                   IF        VABPRD = *blanks
122800080222     C                   CLEAR                   TISI95DS
122900080222     C                   EVAL      I95TCN = '3'
123000080222     C                   Z-ADD     datcor        I95DAT
123100080222     C                   EVAL      I95NAR = VABNZD
123200080222     C                   EVAL      I95CAP = VABCAD
123300080222     C                   EVAL      I95LOC = VABLOD
123400080222     C                   CALL      'TISI95R'
123500080222     C                   PARM                    TISI95DS
123600080222     C                   EVAL      VABPRD = O95PRV
123700080222     C                   ENDIF
123800020204     C*
123900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
124000020204     C                   EXSR      CHKIMPDIV
124100030715     C                   ENDIF
124200020204     C*
124300010607     C                   ENDSR
124400010607     C*----------------------------------------------------*
124500010601
124600020204
124700020204     C*----------------------------------------------------*
124800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
124900020204     C*----------------------------------------------------*
125000020204     C     CHKIMPDIV     BEGSR
125100060524     C*
125200060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
125300060524     C                   IF        vabCAS > *zeros
125400060524     C                   IF        vabCBO = '1'
125500060524     C                   EVAL      vabCBO = '4'
125600060524     C                   ENDIF
125700060524     C                   IF        vabCBO = '2'
125800060524     C                   EVAL      vabCBO = '6'
125900060524     C                   ENDIF
126000060524     C                   ENDIF
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
137800021025     C                   eval      cmzfla = 'FIVAB00F'
137900021025     C                   eval      cmzmba = 'FIVAB00F'
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
141600050421
141700050421
141800050421
141900050421     C     *pssr         BEGSR
142000050421     C*
142100050421     C                   eval      wrkesito = '2'
142200050421     C*
142300050421     C                   ENDSR     '*CANCL'
142400050421     C***
142500010601
142600010601
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
143700010330     C*
143800010330     C* CALCOLA LA DATA CORRENTE
143900010330     C                   time                    wn14             14 0
144000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
144100010330     C                   z-add     wn8           g08dat
144200010330     C                   z-add     *zeros        g08inv
144300010330     C                   movel     '0'           g08err
144400010330     C                   call      'XSRDA8'
144500010330     C                   parm                    wlbda8
144600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
144700000613     C*
144800000613     C                   ENDSR
144900000613     C***
145000990908
145100030715     Otitvxlp   E            riepilogo         2
145200990915     O                                              'Upload via Internet'
145300990915     O                                           +1 'Traduzione TIVIN00R -'
145400021025     O                                           +1 'FIVABWWR'
145500990915     O                                           +1 'Testate bolle'
145600990915     O          E            riepilogo   2
145700990915     O                       wrkdata
145800990915     O                       wrkora              +1
145900990915     O                       procname            +1
146000990915     O          E            riepilogo   2
146100990915     O                                              'Cliente..................:'
146200010605     O                       VABCCM        z     +1
146300990915     O          E            riepilogo   2
146400990920     O                                              'Riferimento Strategi.....:'
146500990920     O                       vlrhdl              +1
146600990915     O          E            riepilogo   2
146700990915     O                                              'Giusti...................:'
146800971022     O                       §CTROK        2   +  1
146900990915     O          E            riepilogo   2
147000990915     O                                              'Sbagliati e corretti.....:'
147100971022     O                       §CTRMO        2   +  1
147200990915     O          E            riepilogo   2
147300990915     O                                              'Sbagliati e scartati.....:'
147400971022     O                       §CTRNO        2   +  1
147500000613
147600030715     Otitvxlps  E            testdett          2
147700000613     O                                              'Upload via Internet'
147800000613     O                                           +1 'Traduzione TIVIN00R -'
147900021025     O                                           +1 'FIVABWWR'
148000000613     O                                           +1 'Testate bolle'
148100000616     O          E            testdett    3
148200000613     O                                           +2 'N° rec'
148300000613     O                                           +3 'Anteprima contenuto'
148400000616     O          E            rigadett    2
148500000613     O                       rrnum               +2
148600000621     O                       recko               +3
148700000616     O          E            findett     2
148800000613     O                       wrkdata
148900000613     O                       wrkora              +1
149000000613     O                       procname            +1
149100000616     O          E            findett     2
149200000613     O                                              'Cliente..................:'
149300010605     O                       VABCCM        z     +1
149400000616     O          E            findett     2
149500000613     O                                              'Riferimento Strategi.....:'
149600000613     O                       vlrhdl              +1
149700000616     O          E            findett     2
149800000613     O                                              'Giusti...................:'
149900000613     O                       §CTROK        2   +  1
150000000616     O          E            findett     2
150100000613     O                                              'Sbagliati e corretti.....:'
150200000613     O                       §CTRMO        2   +  1
150300000616     O          E            findett     2
150400000613     O                                              'Sbagliati e scartati.....:'
150500000613     O                       §CTRNO        2   +  1
150600000613** CMD - COMANDI CL
150700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
150800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
150900030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
151000010606
151100000613
