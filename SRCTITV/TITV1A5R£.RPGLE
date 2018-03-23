000100070925      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente FRO    Cod. 0432774)
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
005000030822     D*------------------
005100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005200030822     D*------------------
005300030822     D posDaDft        S              3  0 INZ(*zeros)
005400030822     D posADft         S              3  0 INZ(*zeros)
005500030822     D j               s              3  0 INZ(1)
005600030822     D wGiroDft        s              1  0 INZ(*zeros)
005700010330     D*------------------
005800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900010330     D*------------------
006000010330     D WLBDA8          DS                  INZ
006100010330     D  G08DAT                 1      8  0
006200010330     D  G08INV                 9     16  0
006300010330     D  G08ERR                17     17
006400010330     D  G08TGI                18     22  0
006500031201     D*------------------
006600031201     D* Costanti
006700031201     D*------------------
006800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000010330
007100010330
007200010330
007300990915     C                   time                    wrkdata
007400990915     C                   time                    wrkora
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800010601     C*
007900010601     C                   exsr      opeini
008000010605     C                   exsr      rwvab
008100010601     C*
008200010601     C                   seton                                        lr
008300010601
008400010601
008500010601
008600010601
008700010601     C*--------------------------------------------------------
008800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008900010601     C*--------------------------------------------------------
009000010601     C     PREELA        BEGSR
009100010601     C*
009200010601     C* SE OCCORRE SPEDIRE IN FILIALE
009300010601     C                   if        invfil <> *zeros and
009400010601     C                             flgGiro = '0'
009500010601     C*
009600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009700010601     C                   eval      flgGiro = '1'
009800000724     C*
009900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010100010601     C     invfil        chain     azorg01l
010200000724     C                   if        %found
010300000724     C                   movel(p)  CMD(1)        depcmd
010400020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010500000724     C*
010600000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000724     C                   PARM                    depcmd
011000000724     C                   PARM                    LENGH
011100000724     C*
011200000724     C                   endif
011300000616     C*
011400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500000616     C                   movel(p)  CMD(2)        depcmd
011600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011700000616     C*
011800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011900011113     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000613     C*
012400030715     C                   if        not %open(titvxlps)
012500030715     C                   open      titvxlps
012600030715     C                   except    testdett
012700000613     C                   endif
012800010601     C*
012900010601     C                   endif
013000010601     C*
013100010601     C                   ENDSR
013200010601     C***
013300010601
013400010601
013500010601
013600010601     C*--------------------------------------------------------
013700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013800010601     C*--------------------------------------------------------
013900010601     C     ENDELA        BEGSR
014000990915     C*
014100000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014200060306     C*
014300060306     C* Effettuo lancio TISI95 solo x chiusura
014400060306     C                   CLEAR                   TISI95DS
014500060306     C                   EVAL      I95TLA = 'C'
014600060306     C                   CALL      'TISI95R'
014700060306     C                   PARM                    TISI95DS
014800000613     C*
014900030715     C                   if        %open(titvxlps)
015000000616     C                   except    findett
015100030715     C                   close     titvxlps
015200000613     C                   endif
015300000616     C*
015400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015500011113     C                   Z-ADD     150           LENGH            15 5
015600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    CMD(3)
015800000616     C                   PARM                    LENGH
015900000616     C*
016000010601     C                   ENDSR
016100010601     C***
016200010601
016300010601
016400000613
016500000613     C*--------------------------------------------------------
016600000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016700000613     C*--------------------------------------------------------
016800000621     C     STPR          BEGSR
016900000613     C*
017000030715     C                   if        not %open(titvxlp)
017100030715     C   50              open      titvxlp
017200990915     C                   endif
017300990915     C*
017400030715     C   50              except    riepilogo
017500990915     C*
017600030715     C                   if        %open(titvxlp)
017700030715     C   50              close     titvxlp
017800990914     C                   endif
017900000613     C*
018000000613     C                   ENDSR
018100000613     C***
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
020000040726     C*
020100040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020200040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020300040726     C                   exsr      inzvar
020400040726     C                   exsr      impvab
020500010330     C*
020600030822     C                   do        *HIVAL
020700010330     C*
020800030822     C                   read      tivin00r                               70
020900010618     C*
021000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021100010618     C                   if        vinflg = '1'
021200010618     C                   eval      flgOk = '1'
021300010618     C                   endif
021400060216     C*
021500060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
021600060216     C* => gestisco sempre
021700060216     C                   z-add     *zeros        wLenVINDTA        4 0
021800060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
021900060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
022000060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
022100060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
022200060216     C                   endif
022300060216     C                   enddo
022400010618     C*
022500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
022600040510     C                   z-add     *zeros        wLenVINDTA        4 0
022700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
023100040510     C                   z-add     1             wFlgVINDTA
023200040510     C                   leave
023300040510     C                   endif
023400040510     C                   enddo
023500040510     C*
023600040218     C                   if        vindta > *blanks AND
023700040510     C                             wFlgVINDTA = 1
023800010330     C                   add       1             rrnum
023900010330     C*
024000010601     C                   if        *in70 = *off and
024100010330     C                             (vinflg = *blanks
024200010330     C                              or vinflg = '0'
024300010330     C                              or vinflg = '2')
024400010330     C*
024500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024600010711     C                   if        vinflg = *blanks or vinflg = '0'
024700010711     C                   clear                   vinmsg
024800010711     C                   endif
024900010601     C*
025000010330     C                   exsr      inzvar
025100010605     C                   exsr      impvab
025200010601     C*
025300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025400010605     C                   eval      depfil = VABLNP
025500010601     C                   exsr      repfil
025600010601     C                   if        depfil = invfil
025700021025     C                   if        vlrpoi = 999
025800030822     C                   move(P)   invfil        VABFGS
025900021025     C                   else
026000030822     C                   move(P)   vlrpoi        VABFGS
026100021025     C                   endif
026200010601     C*
026300010601     C                   exsr      PREELA
026400010601     C*
026500010604     C* Ebbene...
026600010604     C*
026700030822     C  N31              add       1             §CTROK            7 0
026800030822     C   32              add       1             §CTRMO            7 0
026900030822     C   31              add       1             §CTRNO            7 0
027000030822     C                   if        wGiro = 2
027100030822     C  N31              write     FIVAB000
027200030822     C                   endif
027300010604     C*
027400010604     C                   if        *in31 = *off and
027500010604     C                             *in32 = *off
027600010604     C                   eval      vinflg = '1'
027700010604     C                   else
027800010604     C                   eval      recko = vindta
027900010604     C                   except    rigadett
028000010604     C                   eval      vinflg = '2'
028100010604     C                   endif
028200010604     C                   endif
028300010601     C*
028400010604     C                   endif
028500010604     C*
028600010330     C                   else
028700010330     C                   eval      vinflg = '1'
028800010330     C                   endif
028900010601     C*
029000010601     C  N70              update    tivin000
029100010330     C*
029200030822     C  N70              enddo
029300010601     C
029400010601     C*
029500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600010601     C                   if        cntNonEl = *zeros or
029700010601     C                             flgMulti = '0'
029800010330     C* Se non ci sono record con errori ...
029900010601     C                   if        §ctrno = 0 and
030000010604     C                             §ctrmo = 0 and
030100010601     C                             flgStato <> '2'
030200010330     C* ... restituisco esito OK.
030300010330     C                   eval      wrkesito = '0'
030400010330     C                   else
030500010330     C                   if        §ctrok > 0
030600010330     C                   eval      wrkesito = '1'
030700010330     C                   else
030800010615     C                   if        flgOk = '0'
030900010615     C                   eval      wrkesito = '2'
031000010615     C                   else
031100010615     C                   eval      wrkesito = '6'
031200010615     C                   endif
031300010330     C                   endif
031400010330     C                   endif
031500010601     C                   else
031600010601     C                   eval      wrkesito = '9'
031700010601     C                   endif
031800010330     C*
031900010330     C                   if        %open(tivin00r)
032000010330     C                   close     tivin00r
032100010330     C                   endif
032200021025     C                   if        %open(fivabwwr)
032300021025     C                   close     fivabwwr
032400010330     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000010330     C                   if        §ctrok > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100010601
034200010601
034300010601
034400010330     C*----------------------------------------------------*
034500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034600010330     C*----------------------------------------------------*
034700010330     C     INZVAR        BEGSR
034800010330     C*
034900030822     C* Inizializzo variabili di wrk
035000020204     C                   Z-ADD     *zeros        Num5_0            5 0
035100030822     C*
035200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035300030822     C                   CLEAR                   FIVAB000
035400030822     C                   CLEAR                   SkSplitCSV
035500030822     C                   EVAL      i = 1
035600030822     C                   EVAL      posDa = *zeros
035700030822     C                   EVAL      posA  = *zeros
035800030822     C*
035900030822     C* Reimposto i valori di default
036000030822     C                   EXSR      DEFCAM
036100030822     C*
036200010330     C                   ENDSR
036300010330     C*----------------------------------------------------*
036400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036500010330     C*----------------------------------------------------*
036600010330     C     DEFCAM        BEGSR
036700030822     C*
036800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
036900030715     C* e delimitatore testo.
037000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037300030822     C*
037400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
037500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
037600030715     C                   SETON                                        50
037700030715     C                   ELSE
037800030715     C                   SETOFF                                       50
037900030715     C                   ENDIF
038000030822     C*
038100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038200030822     C                   EVAL      posDaDft = 1
038300030822     C                   EVAL      posADft  = 0
038400030822     C                   EVAL      wGiroDft = 0
038500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
038600030822     C                             posDaDft > 0
038700030822     C*
038800030822     C* Gestisco il 1° giro
038900030822     C                   IF        wGiroDft = 0
039000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039200030822     C* Incremento il contatore dei "giri"
039300030822     C                   EVAL      wGiroDft = 1
039400030822     C                   ELSE
039500030822     C                   EVAL      posDaDft = posADft
039600030822     C                   ENDIF
039700030822     C* Eseguo lo scan x trovare la fine del campo corrente
039800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
039900030822     C*
040000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040200030822     C                   IF        posDaDft > 0 AND
040300030822     C                             posADft  > 0
040400030822     C* NCL
040500030822     C                   IF        %subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):1:3)
040800030822     C                             = 'NCL'
040900030822     C                   EVAL      PiStr=%trim(%subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200030822     C                   EXSR      CHKNUM
041300030822     C                   IF        PiInt=*on
041400030822     C                   Z-ADD     PiVal         VABNCL
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030822     C* CCM
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'CCM'
042200030822     C                   EVAL      PiStr=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   EXSR      CHKNUM
042600030822     C                   IF        PiInt=*on
042700030822     C                   Z-ADD     PiVal         VABCCM
042800030822     C                   ENDIF
042900030822     C                   ENDIF
043000030822     C* LNP
043100030822     C                   IF        %subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):1:3)
043400030822     C                             = 'LNP'
043500030822     C                   EVAL      PiStr=%trim(%subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800030822     C                   EXSR      CHKNUM
043900030822     C                   IF        PiInt=*on
044000030822     C                   Z-ADD     PiVal         VABLNP
044100030822     C                   ENDIF
044200030822     C                   ENDIF
044300030822     C* NRS
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'NRS'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiInt=*on
045300030822     C                   Z-ADD     PiVal         VABNRS
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* CTR
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'CTR'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiInt=*on
046600030822     C                   Z-ADD     PiVal         VABCTR
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* PKB
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'PKB'
047400030822     C                   EVAL      PiStr=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   EXSR      CHKNUM
047800030822     C                   IF        PiNum=*on
047900030822     C                   Z-ADD     PiVal         VABPKB
048000030822     C                   ENDIF
048100030822     C                   ENDIF
048200030822     C* VLB
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'VLB'
048700030822     C                   EVAL      PiStr=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   EXSR      CHKNUM
049100030822     C                   IF        PiNum=*on
049200030822     C                   Z-ADD     PiVal         VABVLB
049300030822     C                   ENDIF
049400030822     C                   ENDIF
049500030822     C* QFT
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'QFT'
050000030822     C                   EVAL      PiStr=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   EXSR      CHKNUM
050400030822     C                   IF        PiNum=*on
050500030822     C                   Z-ADD     PiVal         VABQFT
050600030822     C                   ENDIF
050700030822     C                   ENDIF
050800030822     C* CBO
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'CBO'
051300030822     C                   EVAL      VABCBO=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   ENDIF
051700030822     C* TSP
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'TSP'
052200030822     C                   EVAL      VABTSP=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   ENDIF
052600030822     C* VAS
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'VAS'
053100030822     C                   EVAL      VABVAS=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   ENDIF
053500030822     C* VCA
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'VCA'
054000030822     C                   EVAL      VABVCA=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   ENDIF
054400030822     C* GCA
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'GCA'
054900030822     C                   EVAL      VABGCA=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   ENDIF
055300030822     C* CTM
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'CTM'
055800030822     C                   EVAL      VABCTM=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   ENDIF
056200030822     C* FFD
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'FFD'
056700030822     C                   EVAL      VABFFD=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   ENDIF
057100030822     C* VAD
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'VAD'
057600030822     C                   EVAL      VABVAD=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* GMA
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'GMA'
058500030822     C                   EVAL      VABGMA=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* GGA
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'GGA'
059400030822     C                   EVAL      VABGGA=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* GVA
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'GVA'
060300030822     C                   EVAL      VABGVA=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* TC1
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'TC1'
061200030822     C                   EVAL      VABTC1=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600030822     C* TC2
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'TC2'
062100030822     C                   EVAL      VABTC2=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   ENDIF
062500030822     C* ...
062600030822     C                   ENDIF
062700030822     C                   ENDDO
062800020204     C*
062900010330     C                   ENDSR
063000010607     C*----------------------------------------------------*
063100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063200010607     C*----------------------------------------------------*
063300010607     C     IMPVAB        BEGSR
063400010607     C*
063500010607     C                   SETOFF                                       3132
063600030515     C*
063700010607     C                   MOVEL     datcor        VABAAS
063800010607     C                   MOVE      datcor        VABMGS
063900050321     C*
064000050512     C* 1° Giro x eliminare il separatore testo
064100060215     C                   IF        CharTXT <> *blanks
064200050321     C                   Z-ADD     1             wj                5 0
064300050321     C                   DOW       wj <= %len(vindta) - 1
064400050321     C                   IF        %subst(vindta:wj:1) = CharTXT
064500050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
064600050512     C                                      %subst(vindta:wj+1)
064700050512     C                   EVAL      wj = wj - 1
064800050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
064900050321     C                   ENDIF
065000050321     C                   ADD       1             wj
065100050321     C                   ENDDO
065200060215     C                   ENDIF
065300050512     C*
065400060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
065500050512     C* 2 byte a blank
065600050512     C                   IF        %subst(vindta:1:1) = CharCSV
065700050512     C                   EVAL      vindta = '  ' + vindta
065800050512     C                   ENDIF
065900010607     C*
066000030715     C**********
066100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
066200030715     C**********
066300030715     C                   IF        wGiro = *zeros
066400040726     C*
066500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
066600040726     C                   EVAL      depvinDTA = vinDTA
066700040726     C*
066800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
066900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
067000050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
067100060215     C                                      %trim(intcol(5))
067200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
067300031201     C     minu:maiu     XLATE     vindta        vindta
067400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
067500040324     C                   DOW       posDa <= %len(%trimr(vindta))
067600030715     C*
067700030715     C* Gestisco il 1° campo
067800030715     C                   IF        i = 1
067900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068000030715     C                   EVAL      posDa = 1
068100030715     C* Eseguo lo scan x trovare la fine del primo campo
068200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068300030715     C* A questo punto "estraggo" il campo corrente
068400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068500030715     C                                                    (posA-posDa))
068600030715     C* X i campi successivi al 1°
068700030715     C                   ELSE
068800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
068900030715     C                   EVAL      posDa = posA + 1
069000030715     C* Eseguo lo scan x trovare la fine del campo corrente
069100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069200030715     C* Gestisco l'ultimo campo
069300030715     C                   IF        posA = *zeros
069400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
069500030715     C* A questo punto "estraggo" il campo corrente
069600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069700030715     C                                                    (posA-posDa))
069800030715     C                   LEAVE
069900030715     C                   ELSE
070000030715     C* A questo punto "estraggo" il campo corrente
070100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070200030715     C                                                    (posA-posDa))
070300030715     C                   ENDIF
070400030715     C                   ENDIF
070500030715     C* Incremento il contatore d campo
070600030715     C                   EVAL      i = i +1
070700030715     C                   ENDDO
070800030715     C*
070900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071000030715     C* delimitazione testo
071100030715     C                   EVAL      i = 1
071200030715     C                   DOW       i < %elem(SkSplitFLD)
071300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
071400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
071500030715     C                   EVAL      i = i + 1
071600030715     C                   ENDDO
071700030715     C                   EVAL      wGiro = 1
071800040726     C* Ripristino la 1° riga originale
071900040726     C                   EVAL      vinDTA = depvinDTA
072000030715     C                   ELSE
072100030715     C                   EVAL      wGiro = 2
072200031204     C**********
072300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
072400031204     C* d separatore campo
072500031204     C**********
072600060215     C                   EVAL      vindta = %trim(vindta)
072700060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
072800060215     C                   EVAL      vindta = %subst(vindta:2)
072900060215     C                   ENDDO
073000060215     C
073100060215     C                   Z-ADD     *zeros        lunghInput        4 0
073200060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
073300060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
073400060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
073500060215     C                   EVAL      lunghInput = %len(%trim(vindta))
073600060215     C                   ENDDO
073700030715     C**********
073800030715     C* Effettuo lo split del campo dati d input
073900030715     C**********
074000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074100040324     C                   DOW       posDa <= %len(%trimr(vindta))
074200030715     C*
074300030715     C* Gestisco il 1° campo
074400030715     C                   IF        i = 1
074500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074600030715     C                   EVAL      posDa = 1
074700030715     C* Eseguo lo scan x trovare la fine del primo campo
074800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074900030715     C* A questo punto "estraggo" il campo corrente
075000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075100030715     C                                                    (posA-posDa))
075200030715     C* X i campi successivi al 1°
075300030715     C                   ELSE
075400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075500030715     C                   EVAL      posDa = posA + 1
075600031204     C* Verifico che nn vi sia il campo nn valorizzato
075700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
075800031204     C* Se campo nn valorizzato skippo al prossimo
075900031204     C                   EVAL      SkSplitCSV(i) = *blanks
076000031204     C                   EVAL      posA  = posA + 1
076100031204     C                   ELSE
076200030715     C* Eseguo lo scan x trovare la fine del campo corrente
076300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076400030715     C* Gestisco l'ultimo campo
076500030715     C                   IF        posA = *zeros
076600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
076700030715     C* A questo punto "estraggo" il campo corrente
076800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076900030715     C                                                    (posA-posDa))
077000030715     C                   LEAVE
077100030715     C                   ELSE
077200030715     C* A questo punto "estraggo" il campo corrente
077300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077400030715     C                                                    (posA-posDa))
077500030715     C                   ENDIF
077600031204     C                   ENDIF
077700030715     C                   ENDIF
077800030715     C* Incremento il contatore d campo
077900030715     C                   EVAL      i = i +1
078000030715     C                   ENDDO
078100030715     C*
078200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078300030715     C* delimitazione testo
078400030715     C                   EVAL      i = 1
078500030715     C                   DOW       i < %elem(SkSplitCSV)
078600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
078700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
078800030715     C                   EVAL      i = i + 1
078900030715     C                   ENDDO
079000030715     C*
079100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
079200030715     C                   EVAL      i = 1
079300030715     C                   DOW       i < %elem(SkSplitFLD)
079400030715     C*
079500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
079600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
079700030715     C                             SkSplitCSV(i) <> *blanks
079800030721     C*** ATB
079900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
080000030721     C                   EVAL      VABATB = SkSplitCSV(i)
080100030721     C                   ENDIF
080200030715     C*** RSD
080300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
080400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
080500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
080600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
080700030715     C* ==
080800030715     C                   ENDIF
080900030721     C*** RD2
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
081100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
081200030721     C                   ENDIF
081300030715     C*** IND
081400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
081500030715     C                   EVAL      VABIND = SkSplitCSV(i)
081600030715     C                   ENDIF
081700030715     C*** LOD
081800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
081900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
082000030715     C                   ENDIF
082100030715     C*** PRD
082200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
082300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
082400030715     C                   ENDIF
082500030721     C*** NZD
082600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
082700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
082800030721     C                   ENDIF
082900030715     C*** RMA
083000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
083100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
083200030715     C                   ENDIF
083300030715     C*** NOT
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
083500060306     C                   EVAL      VABNOT = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030715     C*** NT2
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
083900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
084000030715     C                   ENDIF
084100030715     C*** VCA
084200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
084300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
084400030715     C                   ENDIF
084500030715     C*** VAS
084600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
084700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
084800030715     C                   ENDIF
084900030715     C*** FFD
085000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
085100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
085200030715     C                   ENDIF
085300030715     C*** CBO
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
085500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030721     C*** GC1
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
085900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** GC2
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
086300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** TSP
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
086700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** NAS
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** TIC
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
087500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** GCA
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
087900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** XCO
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
088300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** CTM
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
088700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** TCR
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** CTS
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
089500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** FTM
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
089900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** VAD
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
090300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** GMA
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
090700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** GGA
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** GVA
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
091500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** TC1
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
091900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** TC2
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
092300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** SCL
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
092700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** RMO
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** NMO
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
093500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*
093800030721     C* Reperisco quindi i campi numerici...
093900030721     C*** CAD
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
094100030721     C                   EVAL      PiStr=SkSplitCSV(i)
094200030721     C                   EXSR      CHKNUM
094300030721     C                   IF        PiInt=*on
094400030721     C                   Z-ADD     PiVal         Num5_0
094500030721     C                   MOVEL(p)  Num5_0        VABCAD
094600030721     C                   ELSE
094700030721     C                   SETON                                        32
094800030721     C                   EVAL      VABCAD = *zeros
094900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095000030721     C                             + ' ' + 'VABCAD'
095100030721     C                   ENDIF
095200030721     C                   ENDIF
095300030721     C*** RMN
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
095500030721     C                   EVAL      PiStr=SkSplitCSV(i)
095600030721     C                   EXSR      CHKNUM
095700030721     C                   IF        PiInt=*on
095800030721     C                   Z-ADD     PiVal         VABRMN
095900030721     C                   ELSE
096000030721     C                   SETON                                        32
096100030721     C                   EVAL      VABRMN = 1
096200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096300030721     C                             + ' ' + 'VABRMN'
096400030721     C                   ENDIF
096500030721     C                   ENDIF
096600030721     C*** NSP
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096800030721     C                   EVAL      PiStr=SkSplitCSV(i)
096900030721     C                   EXSR      CHKNUM
097000040726     C                   IF        PiInt=*on
097100040726     C                   Z-ADD     PiVal         VABNSP
097200030721     C                   ELSE
097300030721     C                   SETON                                        32
097400030721     C                   EVAL      VABNSP = *zeros
097500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097600030721     C                             + ' ' + 'VABNSP'
097700030721     C                   ENDIF
097800030721     C                   ENDIF
097900030721     C*** NCL
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
098100030721     C                   EVAL      PiStr=SkSplitCSV(i)
098200030721     C                   EXSR      CHKNUM
098300030721     C                   IF        PiInt=*on
098400030721     C                   Z-ADD     PiVal         VABNCL
098500030721     C                   ELSE
098600030721     C                   SETON                                        32
098700030721     C                   EVAL      VABNCL = *zeros
098800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098900030721     C                             + ' ' + 'VABNCL'
099000030721     C                   ENDIF
099100030721     C                   ENDIF
099200030721     C*** PKB
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
099400030721     C                   EVAL      PiStr=SkSplitCSV(i)
099500030721     C                   EXSR      CHKNUM
099600030721     C                   IF        PiNum=*on
099700030721     C                   Z-ADD     PiVal         VABPKB
099800030721     C                   ELSE
099900030721     C                   SETON                                        32
100000030721     C                   EVAL      VABPKB = *zeros
100100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100200030721     C                             + ' ' + 'VABPKB'
100300030721     C                   ENDIF
100400030721     C                   ENDIF
100500030721     C*** CAS
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
100700030721     C                   EVAL      PiStr=SkSplitCSV(i)
100800030721     C                   EXSR      CHKNUM
100900030721     C                   IF        PiNum=*on
101000030721     C                   Z-ADD     PiVal         VABCAS
101100030721     C                   ELSE
101200030721     C                   SETON                                        32
101300030721     C                   EVAL      VABCAS = *zeros
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABCAS'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** CCM
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on      AND
102300030721     C                             PiVal<=9999999 AND
102400030721     C                             PiVal>*zeros
102500030721     C                   Z-ADD     PiVal         VABCCM
102600030721     C                   ELSE
102700030721     C                   SETON                                        32
102800030721     C                   EVAL      VABCCM = *zeros
102900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103000030721     C                             + ' ' + 'VABCCM'
103100030721     C                   ENDIF
103200030721     C                   ENDIF
103300030721     C*** LNP
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
103500030721     C                   EVAL      PiStr=SkSplitCSV(i)
103600030721     C                   EXSR      CHKNUM
103700030721     C                   IF        PiInt=*on    AND
103800030721     C                             PiVal<=999   AND
103900030721     C                             PiVal>*zeros
104000030721     C                   Z-ADD     PiVal         VABLNP
104100030721     C                   ELSE
104200030721     C                   SETON                                        32
104300030721     C                   EVAL      VABLNP = *zeros
104400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104500030721     C                             + ' ' + 'VABLNP'
104600030721     C                   ENDIF
104700030721     C                   ENDIF
104800030721     C*** NRS
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
105000030721     C                   EVAL      PiStr=SkSplitCSV(i)
105100030721     C                   EXSR      CHKNUM
105200030721     C                   IF        PiInt=*on AND
105300030721     C                             PiVal<=99
105400030721     C                   Z-ADD     PiVal         VABNRS
105500030721     C                   ELSE
105600030721     C                   SETON                                        32
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800030721     C                             + ' ' + 'VABNRS'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** LNA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on    AND
106600030721     C                             PiVal<=999   AND
106700030721     C                             PiVal>*zeros
106800030721     C                   Z-ADD     PiVal         VABLNA
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      VABLNA = *zeros
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABLNA'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** CTR
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiInt=*on  AND
108100030721     C                             PiVal<=999
108200030721     C                   Z-ADD     PiVal         VABCTR
108300030721     C                   ELSE
108400030721     C                   SETON                                        32
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABCTR'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** IAS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiNum=*on
109400030721     C                   Z-ADD     PiVal         VABIAS
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800030721     C                             + ' ' + 'VABIAS'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** VLB
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiNum=*on
110600030721     C                   Z-ADD     PiVal         VABVLB
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABVLB'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** QFT
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiNum=*on
111800030721     C                   Z-ADD     PiVal         VABQFT
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABQFT'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** NCD
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on      AND
113000030721     C                             PiVal<=9999999
113100030721     C                   Z-ADD     PiVal         VABNCD
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABNCD'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** NCA
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiInt=*on      AND
114300030721     C                             PiVal<=9999999
114400030721     C                   Z-ADD     PiVal         VABNCA
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABNCA'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** ZNC
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on AND
115600030721     C                             PiVal<=99
115700030721     C                   Z-ADD     PiVal         VABZNC
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABZNC'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** DCR
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on       AND
116900030721     C                             PiVal<=99999999
117000030721     C                   Z-ADD     PiVal         VABDCR
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABDCR'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** HCR
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on   AND
118200030721     C                             PiVal<=9999
118300030721     C                   Z-ADD     PiVal         VABHCR
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABHCR'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** VMD
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500030721     C                   Z-ADD     PiVal         VABVMD
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABVMD'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** ANT
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on        AND
120700030721     C                             PiVal<=999999999
120800030721     C                   Z-ADD     PiVal         VABANT
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABANT'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** CMO
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on
122000030721     C                   Z-ADD     PiVal         Num5_0
122100030721     C                   MOVEL(p)  Num5_0        VABCMO
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABCMO = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABCMO'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030715     C*
123000030715     C                   ENDIF
123100030715     C                   EVAL      i = i + 1
123200030715     C                   ENDDO
123300070713     C*
123400070718     C*******                           ********
123500070718     C* CONSIDERAZIONI PARTOCOLARI FUORI CICLO *
123600070718     C*******                           ********
123700070713     C*
123800070925     C                   Z-ADD     VABRMN        VABNSP
123900040726     C*
124000070713     C*******************************
124100070713     C*
124200040726     C* Considerazioni personalizzate x il traduttore corrente
124300060306     C* Reperisco la provincia dal CAP e dalla località
124400060306     C                   IF        VABPRD = *blanks AND
124500060306     C                             VABCAD <> *blanks
124600060306     C                   CLEAR                   TISI95DS
124700060306     C                   EVAL      I95TCN = '3'
124800060306     C                   Z-ADD     datcor        I95DAT
124900060306     C                   EVAL      I95CAP = VABCAD
125000060306     C                   EVAL      I95LOC = VABLOD
125100060306     C                   CALL      'TISI95R'
125200060306     C                   PARM                    TISI95DS
125300060306     C                   EVAL      VABPRD = O95PRV
125400060306     C                   ENDIF
125500040726     C*
125600040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
125700040726     C                   IF        VABNSP = *zeros
125800040726     C                   SETON                                        31
125900040726     C                   ENDIF
126000040726     C*
126100040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
126200040726     C                   IF        VABLNP = *zeros
126300040726     C                   SETON                                        31
126400040726     C                   ENDIF
126500040726     C*
126600040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
126700040726     C* d valorizzarli sempre entrambi
126800040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
126900040726     C                   Z-ADD     VABNSP        VABRMN
127000040726     C                   ENDIF
127100040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
127200040726     C                   MOVEL     VABRMN        VABRMA
127300040726     C                   ENDIF
127400040726     C*
127500040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
127600040726     C                   IF        VABCAS > *zeros
127700040726     C                   IF        VABVCA = *blanks
127800040726     C                   EVAL      VABVCA = 'EUR'
127900070629     C                   EVAL      VABTIC = *blanks
128000040726     C                   ENDIF
128100040726     C                   IF        VABCBO = '1'
128200040726     C                   EVAL      VABCBO = '4'
128300040726     C                   ENDIF
128400040726     C                   IF        VABCBO = '2'
128500040726     C                   EVAL      VABCBO = '6'
128600040726     C                   ENDIF
128700040726     C                   ENDIF
128800020204     C*
128900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
129000020204     C                   EXSR      CHKIMPDIV
129100030715     C                   ENDIF
129200020204     C*
129300010607     C                   ENDSR
129400010607     C*----------------------------------------------------*
129500010601
129600020204
129700020204     C*----------------------------------------------------*
129800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
129900020204     C*----------------------------------------------------*
130000020204     C     CHKIMPDIV     BEGSR
130100020204     C*
130200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
130300020204     C                   Z-ADD     *zeros        wrkDec            9 9
130400020204     C*
130500020204     C* Come prima cosa effettuo considerazioni sulla divisa
130600020204     C                   IF        vabIAS > *zeros
130700020204     C                   IF        vabVAS <> 'EUR'
130800020204     C                   EVAL      vabVAS =  'ITL'
130900020204     C                   ENDIF
131000020204     C                   ENDIF
131100020204     C*
131200020204     C                   IF        vabCAS > *zeros
131300020204     C                   IF        vabVCA <> 'EUR'
131400020204     C                   EVAL      vabVCA =  'ITL'
131500020204     C                   ENDIF
131600020204     C                   ENDIF
131700020204     C*
131800020204     C                   IF        vabVMD > *zeros
131900020204     C                   IF        vabVAD <> 'EUR'
132000020204     C                   EVAL      vabVAD =  'ITL'
132100020204     C                   ENDIF
132200020204     C                   ENDIF
132300020204     C*
132400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
132500020204     C                   Z-ADD     vabIAS        wrkDec
132600020204     C                   IF        wrkDec > *zeros
132700020204     C                   IF        vabVAS = 'ITL'
132800020204     C                   EVAL      vabIAS = *zeros
132900020204     C                   ENDIF
133000020204     C                   ENDIF
133100020204     C*
133200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
133300020204     C                   Z-ADD     vabCAS        wrkDec
133400020204     C                   IF        wrkDec > *zeros
133500020204     C                   IF        vabVCA = 'ITL'
133600020204     C                   EVAL      vabCAS = *zeros
133700020204     C                   ENDIF
133800020204     C                   ENDIF
133900020204     C*
134000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
134100020204     C                   Z-ADD     vabVMD        wrkDec
134200020204     C                   IF        wrkDec > *zeros
134300020204     C                   IF        vabVAD = 'ITL'
134400020204     C                   EVAL      vabVMD = *zeros
134500020204     C                   ENDIF
134600020204     C                   ENDIF
134700020204     C*
134800020204     C                   ENDSR
134900020204     C***
135000020204
135100010330
135200010330
135300010330     C*----------------------------------------------------*
135400010330     C*  CONTROLLO NUMERICITA' CAMPI
135500010330     C*----------------------------------------------------*
135600010330     C     CHKNUM        BEGSR
135700010330     C*
135800010606     C                   IF        PiDecChr = *blanks
135900030715     C                   EVAL      PiDecChr = CharNUM
136000010606     C                   ENDIF
136100010606     C*
136200010606     C                   CALL(e)   'ISNUMERIC'
136300010330     C                   PARM                    PiStr            30
136400010606     C                   PARM                    PiDecChr          1
136500010330     C                   PARM      *ZEROS        PiVal            30 9
136600010330     C                   PARM      '0'           PiInt             1
136700010330     C                   PARM      '0'           PiNum             1
136800010330     C                   IF        %error
136900010606     C                   EVAL      PiNum=*off
137000010330     C                   ENDIF
137100010330     C*
137200010330     C                   ENDSR
137300010330     C***
137400010330
137500010601
137600010601
137700010601
137800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
137900010601     C     repfil        BEGSR
138000010601     C*
138100010601     C                   if        invfil = *zeros and
138200010601     C                             depfil > *zeros and
138300010629     C                             (vinflg = *blanks or
138400010629     C                              vinflg = *zeros)
138500010601     C
138600010601     C                   eval      invfil = depfil
138700010601     C                   endif
138800010601     C*
138900010601     C                   if        depfil <> invfil and
139000010601     C                             invfil > *zeros
139100010601     C                   eval      flgMulti = '1'
139200010601     C                   if        vinflg = *blanks
139300010601     C                   add       1             cntNonEl
139400010601     C                   endif
139500010601     C                   endif
139600010601     C*
139700010601     C                   if        vinflg = '2'
139800010601     C                   eval      flgStato = '2'
139900010601     C                   endif
140000010601     C*
140100010601     C                   ENDSR
140200010601     C***
140300010601
140400010601
140500010601
140600010330
140700010330
140800010330
140900990920      /TITLE Invio dei dati al punto operativo.
141000000613     C     invio         BEGSR
141100990920     C*
141200990920     C                   reset                   dscmz
141300010601     C                   move      invfil        cmzdst
141400990920     C                   eval      cmzfld = vlrfou
141500990920     C                   eval      cmzmbd = vlrhdl
141600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
141700000710     C                   if        prmfir = *blanks
141800021025     C                   eval      cmzfla = 'FIVAB00F'
141900021025     C                   eval      cmzmba = 'FIVAB00F'
142000000710     C                   else
142100000710     C                   eval      cmzfla = prmfir
142200000710     C                   eval      cmzmba = prmfir
142300000710     C                   endif
142400990920     C                   eval      cmznrr = *zeros
142500990920     C                   move      §ctrok        cmznrr
142600021018     C                   eval      cmzlba = vlrfl1
142700990920     C                   call(e)   'TIS711C'
142800990920     C                   parm                    dscmz
142900990921     C                   parm      *blanks       esito
143000990920     C                   if        %error
143100990920     C                             or cmzerr = '1'
143200990921     C                             or esito  = '1'
143300000710     C                   eval      wrkesito = '3'
143400990920     C                   endif
143500990920     C*
143600000613     C                   ENDSR
143700990910
143800010601
143900010601
144000010601
144100010601
144200010601      /TITLE Invio dei dati al punto operativo.
144300010601     C     opeini        BEGSR
144400010601     C*
144500010601     C* Inizializzo flag e contatori operativi
144600010601     C                   movel     '0'           flgGiro           1
144700010601     C                   movel     '0'           flgMulti          1
144800010601     C                   movel     '1'           flgStato          1
144900010615     C                   movel     '0'           flgOk             1
145000010601     C                   z-add     *zeros        cntNonEl         10 0
145100010601     C                   z-add     *zeros        depfil            3 0
145200010601     C                   z-add     *zeros        invfil            3 0
145300010601     C*
145400010601     C                   ENDSR
145500010601     C***
145600010601
145700010601
145800050421
145900050421
146000050421     C     *pssr         BEGSR
146100050421     C*
146200050421     C                   eval      wrkesito = '2'
146300050421     C*
146400050421     C                   ENDSR     '*CANCL'
146500050421     C***
146600050421
146700010601
146800010330
146900010330
147000000613     C     *inzsr        BEGSR
147100990910     C*
147200990910     C     *entry        plist
147300990920     C                   parm                    tivlrds
147400990921     C                   parm      wrkesito      esito
147500000724     C                   parm                    prmlit
147600000710     C                   parm                    prmfir
147700010330     C*
147800010330     C* CALCOLA LA DATA CORRENTE
147900010330     C                   time                    wn14             14 0
148000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
148100010330     C                   z-add     wn8           g08dat
148200010330     C                   z-add     *zeros        g08inv
148300010330     C                   movel     '0'           g08err
148400010330     C                   call      'XSRDA8'
148500010330     C                   parm                    wlbda8
148600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
148700000613     C*
148800000613     C                   ENDSR
148900000613     C***
149000990908
149100030715     Otitvxlp   E            riepilogo         2
149200990915     O                                              'Upload via Internet'
149300990915     O                                           +1 'Traduzione TIVIN00R -'
149400021025     O                                           +1 'FIVABWWR'
149500990915     O                                           +1 'Testate bolle'
149600990915     O          E            riepilogo   2
149700990915     O                       wrkdata
149800990915     O                       wrkora              +1
149900990915     O                       procname            +1
150000990915     O          E            riepilogo   2
150100990915     O                                              'Cliente..................:'
150200010605     O                       VABCCM        z     +1
150300990915     O          E            riepilogo   2
150400990920     O                                              'Riferimento Strategi.....:'
150500990920     O                       vlrhdl              +1
150600990915     O          E            riepilogo   2
150700990915     O                                              'Giusti...................:'
150800971022     O                       §CTROK        2   +  1
150900990915     O          E            riepilogo   2
151000990915     O                                              'Sbagliati e corretti.....:'
151100971022     O                       §CTRMO        2   +  1
151200990915     O          E            riepilogo   2
151300990915     O                                              'Sbagliati e scartati.....:'
151400971022     O                       §CTRNO        2   +  1
151500000613
151600030715     Otitvxlps  E            testdett          2
151700000613     O                                              'Upload via Internet'
151800000613     O                                           +1 'Traduzione TIVIN00R -'
151900021025     O                                           +1 'FIVABWWR'
152000000613     O                                           +1 'Testate bolle'
152100000616     O          E            testdett    3
152200000613     O                                           +2 'N° rec'
152300000613     O                                           +3 'Anteprima contenuto'
152400000616     O          E            rigadett    2
152500000613     O                       rrnum               +2
152600000621     O                       recko               +3
152700000616     O          E            findett     2
152800000613     O                       wrkdata
152900000613     O                       wrkora              +1
153000000613     O                       procname            +1
153100000616     O          E            findett     2
153200000613     O                                              'Cliente..................:'
153300010605     O                       VABCCM        z     +1
153400000616     O          E            findett     2
153500000613     O                                              'Riferimento Strategi.....:'
153600000613     O                       vlrhdl              +1
153700000616     O          E            findett     2
153800000613     O                                              'Giusti...................:'
153900000613     O                       §CTROK        2   +  1
154000000616     O          E            findett     2
154100000613     O                                              'Sbagliati e corretti.....:'
154200000613     O                       §CTRMO        2   +  1
154300000616     O          E            findett     2
154400000613     O                                              'Sbagliati e scartati.....:'
154500000613     O                       §CTRNO        2   +  1
154600000613** CMD - COMANDI CL
154700030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
154800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
154900030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
155000010606
155100000613
155200040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155300070925FILL1;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABNCL;VABPKB;VABRMN;FILL2;
155400070925FILL3
155500070713
155600050321
155700060215
