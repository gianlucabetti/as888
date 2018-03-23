000100070629      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente Esprit (DPD) Cod. 1161104)
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
001900060215     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700060306     D tisi95ds      e ds
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D wrkdata         s               d
003300010330     D wrkora          s               t
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500010330     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700060215     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003900030715     D*------------------
004000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004100030715     D*------------------
004200030715     D SkSplitFLD      S             10    DIM(100)
004300030715     D SkSplitCSV      S            256    DIM(100)
004400030715     D CharCSV         S              1
004500030715     D CharTXT         S              1
004600030715     D CharNUM         S              1
004700030715     D posDa           S              3  0 INZ(*zeros)
004800030715     D posA            S              3  0 INZ(*zeros)
004900030715     D i               s              3  0 INZ(1)
005000030715     D wGiro           s              1  0 INZ(*zeros)
005100030822     D*------------------
005200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005300030822     D*------------------
005400030822     D posDaDft        S              3  0 INZ(*zeros)
005500030822     D posADft         S              3  0 INZ(*zeros)
005600030822     D j               s              3  0 INZ(1)
005700030822     D wGiroDft        s              1  0 INZ(*zeros)
005800010330     D*------------------
005900010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006000010330     D*------------------
006100010330     D WLBDA8          DS                  INZ
006200010330     D  G08DAT                 1      8  0
006300010330     D  G08INV                 9     16  0
006400010330     D  G08ERR                17     17
006500010330     D  G08TGI                18     22  0
006600031201     D*------------------
006700031201     D* Costanti
006800031201     D*------------------
006900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100010330
007200010330
007300010330
007400990915     C                   time                    wrkdata
007500990915     C                   time                    wrkora
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900010601     C*
008000010601     C                   exsr      opeini
008100010605     C                   exsr      rwvab
008200010601     C*
008300010601     C                   seton                                        lr
008400010601
008500010601
008600010601
008700010601
008800010601     C*--------------------------------------------------------
008900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009000010601     C*--------------------------------------------------------
009100010601     C     PREELA        BEGSR
009200010601     C*
009300010601     C* SE OCCORRE SPEDIRE IN FILIALE
009400010601     C                   if        invfil <> *zeros and
009500010601     C                             flgGiro = '0'
009600010601     C*
009700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009800010601     C                   eval      flgGiro = '1'
009900000724     C*
010000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010200010601     C     invfil        chain     azorg01l
010300000724     C                   if        %found
010400000724     C                   movel(p)  CMD(1)        depcmd
010500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010600000724     C*
010700000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010800011113     C                   Z-ADD     150           LENGH            15 5
010900000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011000000724     C                   PARM                    depcmd
011100000724     C                   PARM                    LENGH
011200000724     C*
011300000724     C                   endif
011400000616     C*
011500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011600000616     C                   movel(p)  CMD(2)        depcmd
011700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011800000616     C*
011900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    depcmd
012300000616     C                   PARM                    LENGH
012400000613     C*
012500030715     C                   if        not %open(titvxlps)
012600030715     C                   open      titvxlps
012700030715     C                   except    testdett
012800000613     C                   endif
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601     C*--------------------------------------------------------
013800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010601     C*--------------------------------------------------------
014000010601     C     ENDELA        BEGSR
014100990915     C*
014200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014300060306     C*
014400060306     C* Effettuo lancio TISI95 solo x chiusura
014500060306     C                   CLEAR                   TISI95DS
014600060306     C                   EVAL      I95TLA = 'C'
014700060306     C                   CALL      'TISI95R'
014800060306     C                   PARM                    TISI95DS
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
021600010618     C*
021700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021800010618     C                   if        vinflg = '1'
021900010618     C                   eval      flgOk = '1'
022000010618     C                   endif
022100060216     C*
022200060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
022300060216     C* => gestisco sempre
022400060216     C                   z-add     *zeros        wLenVINDTA        4 0
022500060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
022600060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
022700060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
022800060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
022900060216     C                   endif
023000060216     C                   enddo
023100010618     C*
023200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023300040510     C                   z-add     *zeros        wLenVINDTA        4 0
023400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
023500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
023800040510     C                   z-add     1             wFlgVINDTA
023900040510     C                   leave
024000040510     C                   endif
024100040510     C                   enddo
024200040510     C*
024300040218     C                   if        vindta > *blanks AND
024400040510     C                             wFlgVINDTA = 1
024500010330     C                   add       1             rrnum
024600010330     C*
024700010601     C                   if        *in70 = *off and
024800010330     C                             (vinflg = *blanks
024900010330     C                              or vinflg = '0'
025000010330     C                              or vinflg = '2')
025100010330     C*
025200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025300010711     C                   if        vinflg = *blanks or vinflg = '0'
025400010711     C                   clear                   vinmsg
025500010711     C                   endif
025600010601     C*
025700010330     C                   exsr      inzvar
025800010605     C                   exsr      impvab
025900010601     C*
026000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026100010605     C                   eval      depfil = VABLNP
026200010601     C                   exsr      repfil
026300010601     C                   if        depfil = invfil
026400021025     C                   if        vlrpoi = 999
026500030822     C                   move(P)   invfil        VABFGS
026600021025     C                   else
026700030822     C                   move(P)   vlrpoi        VABFGS
026800021025     C                   endif
026900010601     C*
027000010601     C                   exsr      PREELA
027100010601     C*
027200010604     C* Ebbene...
027300010604     C*
027400030822     C  N31              add       1             §CTROK            7 0
027500030822     C   32              add       1             §CTRMO            7 0
027600030822     C   31              add       1             §CTRNO            7 0
027700030822     C                   if        wGiro = 2
027800030822     C  N31              write     FIVAB000
027900030822     C                   endif
028000010604     C*
028100010604     C                   if        *in31 = *off and
028200010604     C                             *in32 = *off
028300010604     C                   eval      vinflg = '1'
028400010604     C                   else
028500010604     C                   eval      recko = vindta
028600010604     C                   except    rigadett
028700010604     C                   eval      vinflg = '2'
028800010604     C                   endif
028900010604     C                   endif
029000010601     C*
029100010604     C                   endif
029200010604     C*
029300010330     C                   else
029400010330     C                   eval      vinflg = '1'
029500010330     C                   endif
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
038300030822     C*
038400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038600030715     C                   SETON                                        50
038700030715     C                   ELSE
038800030715     C                   SETOFF                                       50
038900030715     C                   ENDIF
039000030822     C*
039100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039200030822     C                   EVAL      posDaDft = 1
039300030822     C                   EVAL      posADft  = 0
039400030822     C                   EVAL      wGiroDft = 0
039500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039600030822     C                             posDaDft > 0
039700030822     C*
039800030822     C* Gestisco il 1° giro
039900030822     C                   IF        wGiroDft = 0
040000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040200030822     C* Incremento il contatore dei "giri"
040300030822     C                   EVAL      wGiroDft = 1
040400030822     C                   ELSE
040500030822     C                   EVAL      posDaDft = posADft
040600030822     C                   ENDIF
040700030822     C* Eseguo lo scan x trovare la fine del campo corrente
040800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040900030822     C*
041000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041200030822     C                   IF        posDaDft > 0 AND
041300030822     C                             posADft  > 0
041400030822     C* NCL
041500030822     C                   IF        %subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):1:3)
041800030822     C                             = 'NCL'
041900030822     C                   EVAL      PiStr=%trim(%subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200030822     C                   EXSR      CHKNUM
042300030822     C                   IF        PiInt=*on
042400030822     C                   Z-ADD     PiVal         VABNCL
042500030822     C                   ENDIF
042600030822     C                   ENDIF
042700030822     C* CCM
042800030822     C                   IF        %subst(
042900030822     C                             %subst(vlrppt:posDaDft+1:
043000030822     C                             posADft-posDaDft-1):1:3)
043100030822     C                             = 'CCM'
043200030822     C                   EVAL      PiStr=%trim(%subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500030822     C                   EXSR      CHKNUM
043600030822     C                   IF        PiInt=*on
043700030822     C                   Z-ADD     PiVal         VABCCM
043800030822     C                   ENDIF
043900030822     C                   ENDIF
044000030822     C* LNP
044100030822     C                   IF        %subst(
044200030822     C                             %subst(vlrppt:posDaDft+1:
044300030822     C                             posADft-posDaDft-1):1:3)
044400030822     C                             = 'LNP'
044500030822     C                   EVAL      PiStr=%trim(%subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800030822     C                   EXSR      CHKNUM
044900030822     C                   IF        PiInt=*on
045000030822     C                   Z-ADD     PiVal         VABLNP
045100030822     C                   ENDIF
045200030822     C                   ENDIF
045300030822     C* NRS
045400030822     C                   IF        %subst(
045500030822     C                             %subst(vlrppt:posDaDft+1:
045600030822     C                             posADft-posDaDft-1):1:3)
045700030822     C                             = 'NRS'
045800030822     C                   EVAL      PiStr=%trim(%subst(
045900030822     C                             %subst(vlrppt:posDaDft+1:
046000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100030822     C                   EXSR      CHKNUM
046200030822     C                   IF        PiInt=*on
046300030822     C                   Z-ADD     PiVal         VABNRS
046400030822     C                   ENDIF
046500030822     C                   ENDIF
046600030822     C* CTR
046700030822     C                   IF        %subst(
046800030822     C                             %subst(vlrppt:posDaDft+1:
046900030822     C                             posADft-posDaDft-1):1:3)
047000030822     C                             = 'CTR'
047100030822     C                   EVAL      PiStr=%trim(%subst(
047200030822     C                             %subst(vlrppt:posDaDft+1:
047300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047400030822     C                   EXSR      CHKNUM
047500030822     C                   IF        PiInt=*on
047600030822     C                   Z-ADD     PiVal         VABCTR
047700030822     C                   ENDIF
047800030822     C                   ENDIF
047900030822     C* PKB
048000030822     C                   IF        %subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):1:3)
048300030822     C                             = 'PKB'
048400030822     C                   EVAL      PiStr=%trim(%subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048700030822     C                   EXSR      CHKNUM
048800030822     C                   IF        PiNum=*on
048900030822     C                   Z-ADD     PiVal         VABPKB
049000030822     C                   ENDIF
049100030822     C                   ENDIF
049200030822     C* VLB
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'VLB'
049700030822     C                   EVAL      PiStr=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   EXSR      CHKNUM
050100030822     C                   IF        PiNum=*on
050200030822     C                   Z-ADD     PiVal         VABVLB
050300030822     C                   ENDIF
050400030822     C                   ENDIF
050500030822     C* QFT
050600030822     C                   IF        %subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):1:3)
050900030822     C                             = 'QFT'
051000030822     C                   EVAL      PiStr=%trim(%subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051300030822     C                   EXSR      CHKNUM
051400030822     C                   IF        PiNum=*on
051500030822     C                   Z-ADD     PiVal         VABQFT
051600030822     C                   ENDIF
051700030822     C                   ENDIF
051800030822     C* CBO
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'CBO'
052300030822     C                   EVAL      VABCBO=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   ENDIF
052700030822     C* TSP
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'TSP'
053200030822     C                   EVAL      VABTSP=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   ENDIF
053600030822     C* VAS
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'VAS'
054100030822     C                   EVAL      VABVAS=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   ENDIF
054500030822     C* VCA
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'VCA'
055000030822     C                   EVAL      VABVCA=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   ENDIF
055400030822     C* GCA
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'GCA'
055900030822     C                   EVAL      VABGCA=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   ENDIF
056300030822     C* CTM
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'CTM'
056800030822     C                   EVAL      VABCTM=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   ENDIF
057200030822     C* FFD
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'FFD'
057700030822     C                   EVAL      VABFFD=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   ENDIF
058100030822     C* VAD
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'VAD'
058600030822     C                   EVAL      VABVAD=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   ENDIF
059000030822     C* GMA
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'GMA'
059500030822     C                   EVAL      VABGMA=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   ENDIF
059900030822     C* GGA
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'GGA'
060400030822     C                   EVAL      VABGGA=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   ENDIF
060800030822     C* GVA
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'GVA'
061300030822     C                   EVAL      VABGVA=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   ENDIF
061700030822     C* TC1
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'TC1'
062200030822     C                   EVAL      VABTC1=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   ENDIF
062600030822     C* TC2
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'TC2'
063100030822     C                   EVAL      VABTC2=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   ENDIF
063500030822     C* ...
063600030822     C                   ENDIF
063700030822     C                   ENDDO
063800020204     C*
063900010330     C                   ENDSR
064000010607     C*----------------------------------------------------*
064100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064200010607     C*----------------------------------------------------*
064300010607     C     IMPVAB        BEGSR
064400010607     C*
064500010607     C                   SETOFF                                       3132
064600030515     C*
064700010607     C                   MOVEL     datcor        VABAAS
064800010607     C                   MOVE      datcor        VABMGS
064900050321     C*
065000050512     C* 1° Giro x eliminare il separatore testo
065100060215     C                   IF        CharTXT <> *blanks
065200050321     C                   Z-ADD     1             wj                5 0
065300050321     C                   DOW       wj <= %len(vindta) - 1
065400050321     C                   IF        %subst(vindta:wj:1) = CharTXT
065500050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
065600050512     C                                      %subst(vindta:wj+1)
065700050512     C                   EVAL      wj = wj - 1
065800050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
065900050321     C                   ENDIF
066000050321     C                   ADD       1             wj
066100050321     C                   ENDDO
066200060215     C                   ENDIF
066300050512     C*
066400060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
066500050512     C* 2 byte a blank
066600050512     C                   IF        %subst(vindta:1:1) = CharCSV
066700050512     C                   EVAL      vindta = '  ' + vindta
066800050512     C                   ENDIF
066900010607     C*
067000030715     C**********
067100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
067200030715     C**********
067300030715     C                   IF        wGiro = *zeros
067400040726     C*
067500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
067600040726     C                   EVAL      depvinDTA = vinDTA
067700040726     C*
067800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
067900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
068000050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
068100060215     C                                      %trim(intcol(5))
068200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
068300031201     C     minu:maiu     XLATE     vindta        vindta
068400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068500040324     C                   DOW       posDa <= %len(%trimr(vindta))
068600030715     C*
068700030715     C* Gestisco il 1° campo
068800030715     C                   IF        i = 1
068900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
069000030715     C                   EVAL      posDa = 1
069100030715     C* Eseguo lo scan x trovare la fine del primo campo
069200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069300030715     C* A questo punto "estraggo" il campo corrente
069400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069500030715     C                                                    (posA-posDa))
069600030715     C* X i campi successivi al 1°
069700030715     C                   ELSE
069800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069900030715     C                   EVAL      posDa = posA + 1
070000030715     C* Eseguo lo scan x trovare la fine del campo corrente
070100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070200030715     C* Gestisco l'ultimo campo
070300030715     C                   IF        posA = *zeros
070400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070500030715     C* A questo punto "estraggo" il campo corrente
070600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070700030715     C                                                    (posA-posDa))
070800030715     C                   LEAVE
070900030715     C                   ELSE
071000030715     C* A questo punto "estraggo" il campo corrente
071100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071200030715     C                                                    (posA-posDa))
071300030715     C                   ENDIF
071400030715     C                   ENDIF
071500030715     C* Incremento il contatore d campo
071600030715     C                   EVAL      i = i +1
071700030715     C                   ENDDO
071800030715     C*
071900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
072000030715     C* delimitazione testo
072100030715     C                   EVAL      i = 1
072200030715     C                   DOW       i < %elem(SkSplitFLD)
072300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
072400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072500030715     C                   EVAL      i = i + 1
072600030715     C                   ENDDO
072700030715     C                   EVAL      wGiro = 1
072800040726     C* Ripristino la 1° riga originale
072900040726     C                   EVAL      vinDTA = depvinDTA
073000030715     C                   ELSE
073100030715     C                   EVAL      wGiro = 2
073200031204     C**********
073300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
073400031204     C* d separatore campo
073500031204     C**********
073600060215     C                   EVAL      vindta = %trim(vindta)
073700060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073800060215     C                   EVAL      vindta = %subst(vindta:2)
073900060215     C                   ENDDO
074000060215     C
074100060215     C                   Z-ADD     *zeros        lunghInput        4 0
074200060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
074300060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074400060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074500060215     C                   EVAL      lunghInput = %len(%trim(vindta))
074600060215     C                   ENDDO
074700030715     C**********
074800030715     C* Effettuo lo split del campo dati d input
074900030715     C**********
075000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075100040324     C                   DOW       posDa <= %len(%trimr(vindta))
075200030715     C*
075300030715     C* Gestisco il 1° campo
075400030715     C                   IF        i = 1
075500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075600030715     C                   EVAL      posDa = 1
075700030715     C* Eseguo lo scan x trovare la fine del primo campo
075800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075900030715     C* A questo punto "estraggo" il campo corrente
076000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076100030715     C                                                    (posA-posDa))
076200030715     C* X i campi successivi al 1°
076300030715     C                   ELSE
076400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076500030715     C                   EVAL      posDa = posA + 1
076600031204     C* Verifico che nn vi sia il campo nn valorizzato
076700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076800031204     C* Se campo nn valorizzato skippo al prossimo
076900031204     C                   EVAL      SkSplitCSV(i) = *blanks
077000031204     C                   EVAL      posA  = posA + 1
077100031204     C                   ELSE
077200030715     C* Eseguo lo scan x trovare la fine del campo corrente
077300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* Gestisco l'ultimo campo
077500030715     C                   IF        posA = *zeros
077600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077700030715     C* A questo punto "estraggo" il campo corrente
077800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077900030715     C                                                    (posA-posDa))
078000030715     C                   LEAVE
078100030715     C                   ELSE
078200030715     C* A questo punto "estraggo" il campo corrente
078300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078400030715     C                                                    (posA-posDa))
078500030715     C                   ENDIF
078600031204     C                   ENDIF
078700030715     C                   ENDIF
078800030715     C* Incremento il contatore d campo
078900030715     C                   EVAL      i = i +1
079000030715     C                   ENDDO
079100030715     C*
079200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079300030715     C* delimitazione testo
079400030715     C                   EVAL      i = 1
079500030715     C                   DOW       i < %elem(SkSplitCSV)
079600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079800030715     C                   EVAL      i = i + 1
079900030715     C                   ENDDO
080000030715     C*
080100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080200030715     C                   EVAL      i = 1
080300030715     C                   DOW       i < %elem(SkSplitFLD)
080400030715     C*
080500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080700030715     C                             SkSplitCSV(i) <> *blanks
080800030721     C*** ATB
080900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
081000030721     C                   EVAL      VABATB = SkSplitCSV(i)
081100030721     C                   ENDIF
081200030715     C*** RSD
081300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081700030715     C* ==
081800030715     C                   ENDIF
081900030721     C*** RD2
082000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
082100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
082200030721     C                   ENDIF
082300030715     C*** IND
082400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082500030715     C                   EVAL      VABIND = SkSplitCSV(i)
082600030715     C                   ENDIF
082700030715     C*** LOD
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
083000030715     C                   ENDIF
083100030715     C*** PRD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083400030715     C                   ENDIF
083500030721     C*** NZD
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083800030721     C                   ENDIF
083900030715     C*** RMA
084000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
084100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
084200030715     C                   ENDIF
084300030715     C*** NOT
084400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084500060306     C                   EVAL      VABNOT = SkSplitCSV(i)
084600030715     C                   ENDIF
084700030715     C*** NT2
084800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
085000030715     C                   ENDIF
085100030715     C*** VCA
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085400030715     C                   ENDIF
085500030715     C*** VAS
085600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085800030715     C                   ENDIF
085900030715     C*** FFD
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
086100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030715     C*** CBO
086400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086600030715     C                   ENDIF
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
102000030721     C*** CAS
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
124701070629     C****
124702070629     C* Consierazioni particolari
124703070629     C*
124704070629     C* Imposto il peso fisso ad 1 Kg x collo
124705070629     C                   IF        VABPKB = *zeros
124706070629     C                   EVAL      VABPKB = 1 * VABNCL
124707070629     C                   ENDIF
124708070629     C****
124800040726     C*
124900040726     C* Considerazioni personalizzate x il traduttore corrente
125000060306     C* Reperisco la provincia dal CAP e dalla località
125100060306     C                   IF        VABPRD = *blanks AND
125200060306     C                             VABCAD <> *blanks
125300060306     C                   CLEAR                   TISI95DS
125400060306     C                   EVAL      I95TCN = '3'
125500060306     C                   Z-ADD     datcor        I95DAT
125600060306     C                   EVAL      I95CAP = VABCAD
125700060306     C                   EVAL      I95LOC = VABLOD
125800060306     C                   CALL      'TISI95R'
125900060306     C                   PARM                    TISI95DS
126000060306     C                   EVAL      VABPRD = O95PRV
126100060306     C                   ENDIF
126200040726     C*
126300040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
126400040726     C                   IF        VABNSP = *zeros
126500040726     C                   SETON                                        31
126600040726     C                   ENDIF
126700040726     C*
126800040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
126900040726     C                   IF        VABLNP = *zeros
127000040726     C                   SETON                                        31
127100040726     C                   ENDIF
127200040726     C*
127300040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
127400040726     C* d valorizzarli sempre entrambi
127500040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
127600040726     C                   Z-ADD     VABNSP        VABRMN
127700040726     C                   ENDIF
127800040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
127900040726     C                   MOVEL     VABRMN        VABRMA
128000040726     C                   ENDIF
128100040726     C*
128200040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
128300040726     C                   IF        VABCAS > *zeros
128400040726     C                   IF        VABVCA = *blanks
128500040726     C                   EVAL      VABVCA = 'EUR'
128600070629     C                   EVAL      VABTIC = *blanks
128700040726     C                   ENDIF
128800040726     C                   IF        VABCBO = '1'
128900040726     C                   EVAL      VABCBO = '4'
129000040726     C                   ENDIF
129100040726     C                   IF        VABCBO = '2'
129200040726     C                   EVAL      VABCBO = '6'
129300040726     C                   ENDIF
129400040726     C                   ENDIF
129500020204     C*
129600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129700020204     C                   EXSR      CHKIMPDIV
129800030715     C                   ENDIF
129900020204     C*
130000010607     C                   ENDSR
130100010607     C*----------------------------------------------------*
130200010601
130300020204
130400020204     C*----------------------------------------------------*
130500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
130600020204     C*----------------------------------------------------*
130700020204     C     CHKIMPDIV     BEGSR
130800020204     C*
130900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
131000020204     C                   Z-ADD     *zeros        wrkDec            9 9
131100020204     C*
131200020204     C* Come prima cosa effettuo considerazioni sulla divisa
131300020204     C                   IF        vabIAS > *zeros
131400020204     C                   IF        vabVAS <> 'EUR'
131500020204     C                   EVAL      vabVAS =  'ITL'
131600020204     C                   ENDIF
131700020204     C                   ENDIF
131800020204     C*
131900020204     C                   IF        vabCAS > *zeros
132000020204     C                   IF        vabVCA <> 'EUR'
132100020204     C                   EVAL      vabVCA =  'ITL'
132200020204     C                   ENDIF
132300020204     C                   ENDIF
132400020204     C*
132500020204     C                   IF        vabVMD > *zeros
132600020204     C                   IF        vabVAD <> 'EUR'
132700020204     C                   EVAL      vabVAD =  'ITL'
132800020204     C                   ENDIF
132900020204     C                   ENDIF
133000020204     C*
133100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
133200020204     C                   Z-ADD     vabIAS        wrkDec
133300020204     C                   IF        wrkDec > *zeros
133400020204     C                   IF        vabVAS = 'ITL'
133500020204     C                   EVAL      vabIAS = *zeros
133600020204     C                   ENDIF
133700020204     C                   ENDIF
133800020204     C*
133900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
134000020204     C                   Z-ADD     vabCAS        wrkDec
134100020204     C                   IF        wrkDec > *zeros
134200020204     C                   IF        vabVCA = 'ITL'
134300020204     C                   EVAL      vabCAS = *zeros
134400020204     C                   ENDIF
134500020204     C                   ENDIF
134600020204     C*
134700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134800020204     C                   Z-ADD     vabVMD        wrkDec
134900020204     C                   IF        wrkDec > *zeros
135000020204     C                   IF        vabVAD = 'ITL'
135100020204     C                   EVAL      vabVMD = *zeros
135200020204     C                   ENDIF
135300020204     C                   ENDIF
135400020204     C*
135500020204     C                   ENDSR
135600020204     C***
135700020204
135800010330
135900010330
136000010330     C*----------------------------------------------------*
136100010330     C*  CONTROLLO NUMERICITA' CAMPI
136200010330     C*----------------------------------------------------*
136300010330     C     CHKNUM        BEGSR
136400010330     C*
136500010606     C                   IF        PiDecChr = *blanks
136600030715     C                   EVAL      PiDecChr = CharNUM
136700010606     C                   ENDIF
136800010606     C*
136900010606     C                   CALL(e)   'ISNUMERIC'
137000010330     C                   PARM                    PiStr            30
137100010606     C                   PARM                    PiDecChr          1
137200010330     C                   PARM      *ZEROS        PiVal            30 9
137300010330     C                   PARM      '0'           PiInt             1
137400010330     C                   PARM      '0'           PiNum             1
137500010330     C                   IF        %error
137600010606     C                   EVAL      PiNum=*off
137700010330     C                   ENDIF
137800010330     C*
137900010330     C                   ENDSR
138000010330     C***
138100010330
138200010601
138300010601
138400010601
138500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
138600010601     C     repfil        BEGSR
138700010601     C*
138800010601     C                   if        invfil = *zeros and
138900010601     C                             depfil > *zeros and
139000010629     C                             (vinflg = *blanks or
139100010629     C                              vinflg = *zeros)
139200010601     C
139300010601     C                   eval      invfil = depfil
139400010601     C                   endif
139500010601     C*
139600010601     C                   if        depfil <> invfil and
139700010601     C                             invfil > *zeros
139800010601     C                   eval      flgMulti = '1'
139900010601     C                   if        vinflg = *blanks
140000010601     C                   add       1             cntNonEl
140100010601     C                   endif
140200010601     C                   endif
140300010601     C*
140400010601     C                   if        vinflg = '2'
140500010601     C                   eval      flgStato = '2'
140600010601     C                   endif
140700010601     C*
140800010601     C                   ENDSR
140900010601     C***
141000010601
141100010601
141200010601
141300010330
141400010330
141500010330
141600990920      /TITLE Invio dei dati al punto operativo.
141700000613     C     invio         BEGSR
141800990920     C*
141900990920     C                   reset                   dscmz
142000010601     C                   move      invfil        cmzdst
142100990920     C                   eval      cmzfld = vlrfou
142200990920     C                   eval      cmzmbd = vlrhdl
142300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
142400000710     C                   if        prmfir = *blanks
142500021025     C                   eval      cmzfla = 'FIVAB00F'
142600021025     C                   eval      cmzmba = 'FIVAB00F'
142700000710     C                   else
142800000710     C                   eval      cmzfla = prmfir
142900000710     C                   eval      cmzmba = prmfir
143000000710     C                   endif
143100990920     C                   eval      cmznrr = *zeros
143200990920     C                   move      §ctrok        cmznrr
143300021018     C                   eval      cmzlba = vlrfl1
143400990920     C                   call(e)   'TIS711C'
143500990920     C                   parm                    dscmz
143600990921     C                   parm      *blanks       esito
143700990920     C                   if        %error
143800990920     C                             or cmzerr = '1'
143900990921     C                             or esito  = '1'
144000000710     C                   eval      wrkesito = '3'
144100990920     C                   endif
144200990920     C*
144300000613     C                   ENDSR
144400990910
144500010601
144600010601
144700010601
144800010601
144900010601      /TITLE Invio dei dati al punto operativo.
145000010601     C     opeini        BEGSR
145100010601     C*
145200010601     C* Inizializzo flag e contatori operativi
145300010601     C                   movel     '0'           flgGiro           1
145400010601     C                   movel     '0'           flgMulti          1
145500010601     C                   movel     '1'           flgStato          1
145600010615     C                   movel     '0'           flgOk             1
145700010601     C                   z-add     *zeros        cntNonEl         10 0
145800010601     C                   z-add     *zeros        depfil            3 0
145900010601     C                   z-add     *zeros        invfil            3 0
146000010601     C*
146100010601     C                   ENDSR
146200010601     C***
146300010601
146400010601
146500050421
146600050421
146700050421     C     *pssr         BEGSR
146800050421     C*
146900050421     C                   eval      wrkesito = '2'
147000050421     C*
147100050421     C                   ENDSR     '*CANCL'
147200050421     C***
147300050421
147400010601
147500010330
147600010330
147700000613     C     *inzsr        BEGSR
147800990910     C*
147900990910     C     *entry        plist
148000990920     C                   parm                    tivlrds
148100990921     C                   parm      wrkesito      esito
148200000724     C                   parm                    prmlit
148300000710     C                   parm                    prmfir
148400010330     C*
148500010330     C* CALCOLA LA DATA CORRENTE
148600010330     C                   time                    wn14             14 0
148700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
148800010330     C                   z-add     wn8           g08dat
148900010330     C                   z-add     *zeros        g08inv
149000010330     C                   movel     '0'           g08err
149100010330     C                   call      'XSRDA8'
149200010330     C                   parm                    wlbda8
149300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
149400000613     C*
149500000613     C                   ENDSR
149600000613     C***
149700990908
149800030715     Otitvxlp   E            riepilogo         2
149900990915     O                                              'Upload via Internet'
150000990915     O                                           +1 'Traduzione TIVIN00R -'
150100021025     O                                           +1 'FIVABWWR'
150200990915     O                                           +1 'Testate bolle'
150300990915     O          E            riepilogo   2
150400990915     O                       wrkdata
150500990915     O                       wrkora              +1
150600990915     O                       procname            +1
150700990915     O          E            riepilogo   2
150800990915     O                                              'Cliente..................:'
150900010605     O                       VABCCM        z     +1
151000990915     O          E            riepilogo   2
151100990920     O                                              'Riferimento Strategi.....:'
151200990920     O                       vlrhdl              +1
151300990915     O          E            riepilogo   2
151400990915     O                                              'Giusti...................:'
151500971022     O                       §CTROK        2   +  1
151600990915     O          E            riepilogo   2
151700990915     O                                              'Sbagliati e corretti.....:'
151800971022     O                       §CTRMO        2   +  1
151900990915     O          E            riepilogo   2
152000990915     O                                              'Sbagliati e scartati.....:'
152100971022     O                       §CTRNO        2   +  1
152200000613
152300030715     Otitvxlps  E            testdett          2
152400000613     O                                              'Upload via Internet'
152500000613     O                                           +1 'Traduzione TIVIN00R -'
152600021025     O                                           +1 'FIVABWWR'
152700000613     O                                           +1 'Testate bolle'
152800000616     O          E            testdett    3
152900000613     O                                           +2 'N° rec'
153000000613     O                                           +3 'Anteprima contenuto'
153100000616     O          E            rigadett    2
153200000613     O                       rrnum               +2
153300000621     O                       recko               +3
153400000616     O          E            findett     2
153500000613     O                       wrkdata
153600000613     O                       wrkora              +1
153700000613     O                       procname            +1
153800000616     O          E            findett     2
153900000613     O                                              'Cliente..................:'
154000010605     O                       VABCCM        z     +1
154100000616     O          E            findett     2
154200000613     O                                              'Riferimento Strategi.....:'
154300000613     O                       vlrhdl              +1
154400000616     O          E            findett     2
154500000613     O                                              'Giusti...................:'
154600000613     O                       §CTROK        2   +  1
154700000616     O          E            findett     2
154800000613     O                                              'Sbagliati e corretti.....:'
154900000613     O                       §CTRMO        2   +  1
155000000616     O          E            findett     2
155100000613     O                                              'Sbagliati e scartati.....:'
155200000613     O                       §CTRNO        2   +  1
155300000613** CMD - COMANDI CL
155400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
155500030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
155600030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
155700010606
155800000613
155900040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
156000070629VABCCM;VABLNP;FILL1;FILL2;VABNSP;VABCBO;VABRSD;VABRD2;VABIND;VABCAD;
156100070629VABLOD;VABPRD;VABNZD;VABGC1;VABGC2;VABTSP;VABNCL;VABPKB;VABVLB;VABRMN;
156200070629VABRMA;VABNCD;VABNCA;VABXCO;VABNOT;VABNT2;VABDCR;VABCTR
156300050321
156400060215
