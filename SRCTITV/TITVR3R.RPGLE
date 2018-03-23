000100060215      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente GAMMA PIU' - Cod. 0930641)
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
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D wrkdata         s               d
003200010330     D wrkora          s               t
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400010330     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600060215     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700060215     D wPrimoRecord    s              1    INZ(*zeros)
003800060215     D wNote           s            100    INZ(*blanks)
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
014300000613     C*
014400030715     C                   if        %open(titvxlps)
014500000616     C                   except    findett
014600030715     C                   close     titvxlps
014700000613     C                   endif
014800000616     C*
014900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015000011113     C                   Z-ADD     150           LENGH            15 5
015100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015200000616     C                   PARM                    CMD(3)
015300000616     C                   PARM                    LENGH
015400000616     C*
015500010601     C                   ENDSR
015600010601     C***
015700010601
015800010601
015900000613
016000000613     C*--------------------------------------------------------
016100000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016200000613     C*--------------------------------------------------------
016300000621     C     STPR          BEGSR
016400000613     C*
016500030715     C                   if        not %open(titvxlp)
016600030715     C   50              open      titvxlp
016700990915     C                   endif
016800990915     C*
016900030715     C   50              except    riepilogo
017000990915     C*
017100030715     C                   if        %open(titvxlp)
017200030715     C   50              close     titvxlp
017300990914     C                   endif
017400000613     C*
017500000613     C                   ENDSR
017600000613     C***
017700010601
017800010601
017900010601
018000010330     C*--------------------------------------------------------
018100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
018200010330     C*--------------------------------------------------------
018300010605     C     RWVAB         BEGSR
018400010330     C*
018500010330     C                   if        not %open(tivin00r)
018600010330     C                   open      tivin00r
018700010330     C                   endif
018800021025     C                   if        not %open(fivabwwr)
018900021025     C                   open      fivabwwr
019000010330     C                   endif
019100010330     C*
019200010604     C                   clear                   §CTROK
019300010604     C                   clear                   §CTRMO
019400010604     C                   clear                   §CTRNO
019500040726     C*
019600040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019700040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019800040726     C                   exsr      inzvar
019900040726     C                   exsr      impvab
020000010330     C*
020100030822     C                   do        *HIVAL
020200010330     C*
020300030822     C                   read      tivin00r                               70
020400060215     C*
020500060215     C* Gestisco se trattasi del primo record => che occorre ignorare
020600060215     C                   if        wPrimoRecord = *zeros
020700060215     C                   eval      wPrimoRecord = '1'
020800060215     C                   eval      vinflg = '1'
020900060215     C                   else
021000010618     C*
021100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021200010618     C                   if        vinflg = '1'
021300010618     C                   eval      flgOk = '1'
021400010618     C                   endif
021500060216     C*
021600060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
021700060216     C* => gestisco sempre
021800060216     C                   z-add     *zeros        wLenVINDTA        4 0
021900060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
022000060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
022100060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
022200060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
022300060216     C                   endif
022400060216     C                   enddo
022500010618     C*
022600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
022700040510     C                   z-add     *zeros        wLenVINDTA        4 0
022800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
023200040510     C                   z-add     1             wFlgVINDTA
023300040510     C                   leave
023400040510     C                   endif
023500040510     C                   enddo
023600040510     C*
023700040218     C                   if        vindta > *blanks AND
023800040510     C                             wFlgVINDTA = 1
023900010330     C                   add       1             rrnum
024000010330     C*
024100010601     C                   if        *in70 = *off and
024200010330     C                             (vinflg = *blanks
024300010330     C                              or vinflg = '0'
024400010330     C                              or vinflg = '2')
024500010330     C*
024600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
024700010711     C                   if        vinflg = *blanks or vinflg = '0'
024800010711     C                   clear                   vinmsg
024900010711     C                   endif
025000010601     C*
025100010330     C                   exsr      inzvar
025200010605     C                   exsr      impvab
025300010601     C*
025400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025500010605     C                   eval      depfil = VABLNP
025600010601     C                   exsr      repfil
025700010601     C                   if        depfil = invfil
025800021025     C                   if        vlrpoi = 999
025900030822     C                   move(P)   invfil        VABFGS
026000021025     C                   else
026100030822     C                   move(P)   vlrpoi        VABFGS
026200021025     C                   endif
026300010601     C*
026400010601     C                   exsr      PREELA
026500010601     C*
026600010604     C* Ebbene...
026700010604     C*
026800030822     C  N31              add       1             §CTROK            7 0
026900030822     C   32              add       1             §CTRMO            7 0
027000030822     C   31              add       1             §CTRNO            7 0
027100030822     C                   if        wGiro = 2
027200030822     C  N31              write     FIVAB000
027300030822     C                   endif
027400010604     C*
027500010604     C                   if        *in31 = *off and
027600010604     C                             *in32 = *off
027700010604     C                   eval      vinflg = '1'
027800010604     C                   else
027900010604     C                   eval      recko = vindta
028000010604     C                   except    rigadett
028100010604     C                   eval      vinflg = '2'
028200010604     C                   endif
028300010604     C                   endif
028400010601     C*
028500010604     C                   endif
028600010604     C*
028700010330     C                   else
028800010330     C                   eval      vinflg = '1'
028900010330     C                   endif
029000060215     C*
029100060215     C* Se era il primo record (wPrimoRecord) => lo salto e aggiorno lo stato
029200060215     C                   endif
029300010601     C*
029400010601     C  N70              update    tivin000
029500010330     C*
029600030822     C  N70              enddo
029700010601     C
029800010601     C*
029900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030000010601     C                   if        cntNonEl = *zeros or
030100010601     C                             flgMulti = '0'
030200010330     C* Se non ci sono record con errori ...
030300010601     C                   if        §ctrno = 0 and
030400010604     C                             §ctrmo = 0 and
030500010601     C                             flgStato <> '2'
030600010330     C* ... restituisco esito OK.
030700010330     C                   eval      wrkesito = '0'
030800010330     C                   else
030900010330     C                   if        §ctrok > 0
031000010330     C                   eval      wrkesito = '1'
031100010330     C                   else
031200010615     C                   if        flgOk = '0'
031300010615     C                   eval      wrkesito = '2'
031400010615     C                   else
031500010615     C                   eval      wrkesito = '6'
031600010615     C                   endif
031700010330     C                   endif
031800010330     C                   endif
031900010601     C                   else
032000010601     C                   eval      wrkesito = '9'
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        %open(tivin00r)
032400010330     C                   close     tivin00r
032500010330     C                   endif
032600021025     C                   if        %open(fivabwwr)
032700021025     C                   close     fivabwwr
032800010330     C                   endif
032900010601     C*
033000010601     C                   if        vlrpoi <> 999
033100010601     C                   eval      invfil = vlrpoi
033200010601     C                   endif
033300010330     C*
033400010330     C                   if        §ctrok > 0
033500010601     C                             and invfil > *zeros
033600010330     C                   exsr      invio
033700010330     C                   endif
033800010601     C*
033900010618     C                   if        flgGiro = '1'
034000010601     C                   exsr      endela
034100010618     C                   endif
034200010330     C*
034300010330     C                   ENDSR
034400010330     C***
034500010601
034600010601
034700010601
034800010330     C*----------------------------------------------------*
034900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035000010330     C*----------------------------------------------------*
035100010330     C     INZVAR        BEGSR
035200010330     C*
035300030822     C* Inizializzo variabili di wrk
035400020204     C                   Z-ADD     *zeros        Num5_0            5 0
035500030822     C*
035600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035700030822     C                   CLEAR                   FIVAB000
035800030822     C                   CLEAR                   SkSplitCSV
035900030822     C                   EVAL      i = 1
036000030822     C                   EVAL      posDa = *zeros
036100030822     C                   EVAL      posA  = *zeros
036200030822     C*
036300030822     C* Reimposto i valori di default
036400030822     C                   EXSR      DEFCAM
036500030822     C*
036600010330     C                   ENDSR
036700010330     C*----------------------------------------------------*
036800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036900010330     C*----------------------------------------------------*
037000010330     C     DEFCAM        BEGSR
037100030822     C*
037200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037300030715     C* e delimitatore testo.
037400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037700030822     C*
037800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
037900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038000030715     C                   SETON                                        50
038100030715     C                   ELSE
038200030715     C                   SETOFF                                       50
038300030715     C                   ENDIF
038400030822     C*
038500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038600030822     C                   EVAL      posDaDft = 1
038700030822     C                   EVAL      posADft  = 0
038800030822     C                   EVAL      wGiroDft = 0
038900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039000030822     C                             posDaDft > 0
039100030822     C*
039200030822     C* Gestisco il 1° giro
039300030822     C                   IF        wGiroDft = 0
039400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039600030822     C* Incremento il contatore dei "giri"
039700030822     C                   EVAL      wGiroDft = 1
039800030822     C                   ELSE
039900030822     C                   EVAL      posDaDft = posADft
040000030822     C                   ENDIF
040100030822     C* Eseguo lo scan x trovare la fine del campo corrente
040200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040300030822     C*
040400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040600030822     C                   IF        posDaDft > 0 AND
040700030822     C                             posADft  > 0
040800030822     C* NCL
040900030822     C                   IF        %subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):1:3)
041200030822     C                             = 'NCL'
041300030822     C                   EVAL      PiStr=%trim(%subst(
041400030822     C                             %subst(vlrppt:posDaDft+1:
041500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041600030822     C                   EXSR      CHKNUM
041700030822     C                   IF        PiInt=*on
041800030822     C                   Z-ADD     PiVal         VABNCL
041900030822     C                   ENDIF
042000030822     C                   ENDIF
042100030822     C* CCM
042200030822     C                   IF        %subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):1:3)
042500030822     C                             = 'CCM'
042600030822     C                   EVAL      PiStr=%trim(%subst(
042700030822     C                             %subst(vlrppt:posDaDft+1:
042800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042900030822     C                   EXSR      CHKNUM
043000030822     C                   IF        PiInt=*on
043100030822     C                   Z-ADD     PiVal         VABCCM
043200030822     C                   ENDIF
043300030822     C                   ENDIF
043400030822     C* LNP
043500030822     C                   IF        %subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):1:3)
043800030822     C                             = 'LNP'
043900030822     C                   EVAL      PiStr=%trim(%subst(
044000030822     C                             %subst(vlrppt:posDaDft+1:
044100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044200030822     C                   EXSR      CHKNUM
044300030822     C                   IF        PiInt=*on
044400030822     C                   Z-ADD     PiVal         VABLNP
044500030822     C                   ENDIF
044600030822     C                   ENDIF
044700030822     C* NRS
044800030822     C                   IF        %subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):1:3)
045100030822     C                             = 'NRS'
045200030822     C                   EVAL      PiStr=%trim(%subst(
045300030822     C                             %subst(vlrppt:posDaDft+1:
045400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045500030822     C                   EXSR      CHKNUM
045600030822     C                   IF        PiInt=*on
045700030822     C                   Z-ADD     PiVal         VABNRS
045800030822     C                   ENDIF
045900030822     C                   ENDIF
046000030822     C* CTR
046100030822     C                   IF        %subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):1:3)
046400030822     C                             = 'CTR'
046500030822     C                   EVAL      PiStr=%trim(%subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046800030822     C                   EXSR      CHKNUM
046900030822     C                   IF        PiInt=*on
047000030822     C                   Z-ADD     PiVal         VABCTR
047100030822     C                   ENDIF
047200030822     C                   ENDIF
047300030822     C* PKB
047400030822     C                   IF        %subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):1:3)
047700030822     C                             = 'PKB'
047800030822     C                   EVAL      PiStr=%trim(%subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100030822     C                   EXSR      CHKNUM
048200030822     C                   IF        PiNum=*on
048300030822     C                   Z-ADD     PiVal         VABPKB
048400030822     C                   ENDIF
048500030822     C                   ENDIF
048600030822     C* VLB
048700030822     C                   IF        %subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):1:3)
049000030822     C                             = 'VLB'
049100030822     C                   EVAL      PiStr=%trim(%subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400030822     C                   EXSR      CHKNUM
049500030822     C                   IF        PiNum=*on
049600030822     C                   Z-ADD     PiVal         VABVLB
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* QFT
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'QFT'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiNum=*on
050900030822     C                   Z-ADD     PiVal         VABQFT
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C* CBO
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'CBO'
051700030822     C                   EVAL      VABCBO=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   ENDIF
052100030822     C* TSP
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'TSP'
052600030822     C                   EVAL      VABTSP=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   ENDIF
053000030822     C* VAS
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'VAS'
053500030822     C                   EVAL      VABVAS=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   ENDIF
053900030822     C* VCA
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'VCA'
054400030822     C                   EVAL      VABVCA=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   ENDIF
054800030822     C* GCA
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'GCA'
055300030822     C                   EVAL      VABGCA=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   ENDIF
055700030822     C* CTM
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'CTM'
056200030822     C                   EVAL      VABCTM=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   ENDIF
056600030822     C* FFD
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'FFD'
057100030822     C                   EVAL      VABFFD=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   ENDIF
057500030822     C* VAD
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'VAD'
058000030822     C                   EVAL      VABVAD=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   ENDIF
058400030822     C* GMA
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'GMA'
058900030822     C                   EVAL      VABGMA=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   ENDIF
059300030822     C* GGA
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'GGA'
059800030822     C                   EVAL      VABGGA=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   ENDIF
060200030822     C* GVA
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'GVA'
060700030822     C                   EVAL      VABGVA=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   ENDIF
061100030822     C* TC1
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'TC1'
061600030822     C                   EVAL      VABTC1=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   ENDIF
062000030822     C* TC2
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'TC2'
062500030822     C                   EVAL      VABTC2=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   ENDIF
062900030822     C* ...
063000030822     C                   ENDIF
063100030822     C                   ENDDO
063200020204     C*
063300010330     C                   ENDSR
063400010607     C*----------------------------------------------------*
063500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063600010607     C*----------------------------------------------------*
063700010607     C     IMPVAB        BEGSR
063800010607     C*
063900010607     C                   SETOFF                                       3132
064000030515     C*
064100010607     C                   MOVEL     datcor        VABAAS
064200010607     C                   MOVE      datcor        VABMGS
064300050321     C*
064400050512     C* 1° Giro x eliminare il separatore testo
064500060215     C                   IF        CharTXT <> *blanks
064600050321     C                   Z-ADD     1             wj                5 0
064700050321     C                   DOW       wj <= %len(vindta) - 1
064800050321     C                   IF        %subst(vindta:wj:1) = CharTXT
064900050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
065000050512     C                                      %subst(vindta:wj+1)
065100050512     C                   EVAL      wj = wj - 1
065200050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
065300050321     C                   ENDIF
065400050321     C                   ADD       1             wj
065500050321     C                   ENDDO
065600060215     C                   ENDIF
065700050512     C*
065800060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
065900050512     C* 2 byte a blank
066000050512     C                   IF        %subst(vindta:1:1) = CharCSV
066100050512     C                   EVAL      vindta = '  ' + vindta
066200050512     C                   ENDIF
066300010607     C*
066400030715     C**********
066500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
066600030715     C**********
066700030715     C                   IF        wGiro = *zeros
066800040726     C*
066900040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
067000040726     C                   EVAL      depvinDTA = vinDTA
067100040726     C*
067200040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
067300040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
067400050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
067500060215     C                                      %trim(intcol(5))
067600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
067700031201     C     minu:maiu     XLATE     vindta        vindta
067800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
067900040324     C                   DOW       posDa <= %len(%trimr(vindta))
068000030715     C*
068100030715     C* Gestisco il 1° campo
068200030715     C                   IF        i = 1
068300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068400030715     C                   EVAL      posDa = 1
068500030715     C* Eseguo lo scan x trovare la fine del primo campo
068600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068700030715     C* A questo punto "estraggo" il campo corrente
068800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068900030715     C                                                    (posA-posDa))
069000030715     C* X i campi successivi al 1°
069100030715     C                   ELSE
069200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069300030715     C                   EVAL      posDa = posA + 1
069400030715     C* Eseguo lo scan x trovare la fine del campo corrente
069500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069600030715     C* Gestisco l'ultimo campo
069700030715     C                   IF        posA = *zeros
069800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
069900030715     C* A questo punto "estraggo" il campo corrente
070000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070100030715     C                                                    (posA-posDa))
070200030715     C                   LEAVE
070300030715     C                   ELSE
070400030715     C* A questo punto "estraggo" il campo corrente
070500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070600030715     C                                                    (posA-posDa))
070700030715     C                   ENDIF
070800030715     C                   ENDIF
070900030715     C* Incremento il contatore d campo
071000030715     C                   EVAL      i = i +1
071100030715     C                   ENDDO
071200030715     C*
071300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071400030715     C* delimitazione testo
071500030715     C                   EVAL      i = 1
071600030715     C                   DOW       i < %elem(SkSplitFLD)
071700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
071800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
071900030715     C                   EVAL      i = i + 1
072000030715     C                   ENDDO
072100030715     C                   EVAL      wGiro = 1
072200040726     C* Ripristino la 1° riga originale
072300040726     C                   EVAL      vinDTA = depvinDTA
072400030715     C                   ELSE
072500030715     C                   EVAL      wGiro = 2
072600031204     C**********
072700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
072800031204     C* d separatore campo
072900031204     C**********
073000060215     C                   EVAL      vindta = %trim(vindta)
073100060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073200060215     C                   EVAL      vindta = %subst(vindta:2)
073300060215     C                   ENDDO
073400060215     C
073500060215     C                   Z-ADD     *zeros        lunghInput        4 0
073600060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
073700060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
073800060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
073900060215     C                   EVAL      lunghInput = %len(%trim(vindta))
074000060215     C                   ENDDO
074100030715     C**********
074200030715     C* Effettuo lo split del campo dati d input
074300030715     C**********
074400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074500040324     C                   DOW       posDa <= %len(%trimr(vindta))
074600030715     C*
074700030715     C* Gestisco il 1° campo
074800030715     C                   IF        i = 1
074900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075000030715     C                   EVAL      posDa = 1
075100030715     C* Eseguo lo scan x trovare la fine del primo campo
075200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075300030715     C* A questo punto "estraggo" il campo corrente
075400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075500030715     C                                                    (posA-posDa))
075600030715     C* X i campi successivi al 1°
075700030715     C                   ELSE
075800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075900030715     C                   EVAL      posDa = posA + 1
076000031204     C* Verifico che nn vi sia il campo nn valorizzato
076100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076200031204     C* Se campo nn valorizzato skippo al prossimo
076300031204     C                   EVAL      SkSplitCSV(i) = *blanks
076400031204     C                   EVAL      posA  = posA + 1
076500031204     C                   ELSE
076600030715     C* Eseguo lo scan x trovare la fine del campo corrente
076700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076800030715     C* Gestisco l'ultimo campo
076900030715     C                   IF        posA = *zeros
077000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077100030715     C* A questo punto "estraggo" il campo corrente
077200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077300030715     C                                                    (posA-posDa))
077400030715     C                   LEAVE
077500030715     C                   ELSE
077600030715     C* A questo punto "estraggo" il campo corrente
077700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077800030715     C                                                    (posA-posDa))
077900030715     C                   ENDIF
078000031204     C                   ENDIF
078100030715     C                   ENDIF
078200030715     C* Incremento il contatore d campo
078300030715     C                   EVAL      i = i +1
078400030715     C                   ENDDO
078500030715     C*
078600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078700030715     C* delimitazione testo
078800030715     C                   EVAL      i = 1
078900030715     C                   DOW       i < %elem(SkSplitCSV)
079000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079200030715     C                   EVAL      i = i + 1
079300030715     C                   ENDDO
079400030715     C*
079500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
079600030715     C                   EVAL      i = 1
079700030715     C                   DOW       i < %elem(SkSplitFLD)
079800030715     C*
079900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080100030715     C                             SkSplitCSV(i) <> *blanks
080200030721     C*** ATB
080300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
080400030721     C                   EVAL      VABATB = SkSplitCSV(i)
080500030721     C                   ENDIF
080600030715     C*** RSD
080700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
080800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
080900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081100030715     C* ==
081200030715     C                   ENDIF
081300030721     C*** RD2
081400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
081500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
081600030721     C                   ENDIF
081700030715     C*** IND
081800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
081900030715     C                   EVAL      VABIND = SkSplitCSV(i)
082000030715     C                   ENDIF
082100030715     C*** LOD
082200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
082400030715     C                   ENDIF
082500030715     C*** PRD
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
082700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
082800030715     C                   ENDIF
082900030721     C*** NZD
083000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083200030721     C                   ENDIF
083300030715     C*** RMA
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
083500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030715     C*** NOT
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
083900060215     C                   EVAL      wNote = SkSplitCSV(i)
084000030715     C                   ENDIF
084100030715     C*** NT2
084200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
084400030715     C                   ENDIF
084500030715     C*** VCA
084600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
084700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
084800030715     C                   ENDIF
084900030715     C*** VAS
085000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085200030715     C                   ENDIF
085300030715     C*** FFD
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
085500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030715     C*** CBO
085800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
085900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086000030715     C                   ENDIF
086100030721     C*** GC1
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
086300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** GC2
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
086700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** TSP
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** NAS
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** TIC
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
087900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** GCA
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** XCO
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
088700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** CTM
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** TCR
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** CTS
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
089900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** FTM
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** VAD
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
090700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** GMA
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** GGA
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** GVA
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
091900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** TC1
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** TC2
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
092700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** SCL
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** RMO
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** NMO
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
093900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*
094200030721     C* Reperisco quindi i campi numerici...
094300030721     C*** CAD
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
094500030721     C                   EVAL      PiStr=SkSplitCSV(i)
094600030721     C                   EXSR      CHKNUM
094700030721     C                   IF        PiInt=*on
094800030721     C                   Z-ADD     PiVal         Num5_0
094900030721     C                   MOVEL(p)  Num5_0        VABCAD
095000030721     C                   ELSE
095100030721     C                   SETON                                        32
095200030721     C                   EVAL      VABCAD = *zeros
095300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095400030721     C                             + ' ' + 'VABCAD'
095500030721     C                   ENDIF
095600030721     C                   ENDIF
095700030721     C*** RMN
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
095900030721     C                   EVAL      PiStr=SkSplitCSV(i)
096000030721     C                   EXSR      CHKNUM
096100030721     C                   IF        PiInt=*on
096200030721     C                   Z-ADD     PiVal         VABRMN
096300030721     C                   ELSE
096400030721     C                   SETON                                        32
096500030721     C                   EVAL      VABRMN = 1
096600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096700030721     C                             + ' ' + 'VABRMN'
096800030721     C                   ENDIF
096900030721     C                   ENDIF
097000030721     C*** NSP
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097200030721     C                   EVAL      PiStr=SkSplitCSV(i)
097300030721     C                   EXSR      CHKNUM
097400040726     C                   IF        PiInt=*on
097500040726     C                   Z-ADD     PiVal         VABNSP
097600040802     C*                  IF        PiVal>9999999
097700040802     C*                  SETON                                        32
097800040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
097900040802     C*                            + ' ' + 'VABNSP'
098000040802     C*                  ENDIF
098100030721     C                   ELSE
098200030721     C                   SETON                                        32
098300030721     C                   EVAL      VABNSP = *zeros
098400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098500030721     C                             + ' ' + 'VABNSP'
098600030721     C                   ENDIF
098700030721     C                   ENDIF
098800030721     C*** NCL
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099000030721     C                   EVAL      PiStr=SkSplitCSV(i)
099100030721     C                   EXSR      CHKNUM
099200030721     C                   IF        PiInt=*on
099300030721     C                   Z-ADD     PiVal         VABNCL
099400030721     C                   ELSE
099500030721     C                   SETON                                        32
099600030721     C                   EVAL      VABNCL = *zeros
099700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099800030721     C                             + ' ' + 'VABNCL'
099900030721     C                   ENDIF
100000030721     C                   ENDIF
100100030721     C*** PKB
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100300030721     C                   EVAL      PiStr=SkSplitCSV(i)
100400030721     C                   EXSR      CHKNUM
100500030721     C                   IF        PiNum=*on
100600030721     C                   Z-ADD     PiVal         VABPKB
100700030721     C                   ELSE
100800030721     C                   SETON                                        32
100900030721     C                   EVAL      VABPKB = *zeros
101000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101100030721     C                             + ' ' + 'VABPKB'
101200030721     C                   ENDIF
101300030721     C                   ENDIF
101400030721     C*** CAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
101600030721     C                   EVAL      PiStr=SkSplitCSV(i)
101700030721     C                   EXSR      CHKNUM
101800030721     C                   IF        PiNum=*on
101900030721     C                   Z-ADD     PiVal         VABCAS
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      VABCAS = *zeros
102300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102400030721     C                             + ' ' + 'VABCAS'
102500030721     C                   ENDIF
102600030721     C                   ENDIF
102700030721     C*** CCM
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
102900030721     C                   EVAL      PiStr=SkSplitCSV(i)
103000030721     C                   EXSR      CHKNUM
103100030721     C                   IF        PiInt=*on      AND
103200030721     C                             PiVal<=9999999 AND
103300030721     C                             PiVal>*zeros
103400030721     C                   Z-ADD     PiVal         VABCCM
103500030721     C                   ELSE
103600030721     C                   SETON                                        32
103700030721     C                   EVAL      VABCCM = *zeros
103800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103900030721     C                             + ' ' + 'VABCCM'
104000030721     C                   ENDIF
104100030721     C                   ENDIF
104200030721     C*** LNP
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
104400030721     C                   EVAL      PiStr=SkSplitCSV(i)
104500030721     C                   EXSR      CHKNUM
104600030721     C                   IF        PiInt=*on    AND
104700030721     C                             PiVal<=999   AND
104800030721     C                             PiVal>*zeros
104900030721     C                   Z-ADD     PiVal         VABLNP
105000030721     C                   ELSE
105100030721     C                   SETON                                        32
105200030721     C                   EVAL      VABLNP = *zeros
105300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105400030721     C                             + ' ' + 'VABLNP'
105500030721     C                   ENDIF
105600030721     C                   ENDIF
105700030721     C*** NRS
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
105900030721     C                   EVAL      PiStr=SkSplitCSV(i)
106000030721     C                   EXSR      CHKNUM
106100030721     C                   IF        PiInt=*on AND
106200030721     C                             PiVal<=99
106300030721     C                   Z-ADD     PiVal         VABNRS
106400030721     C                   ELSE
106500030721     C                   SETON                                        32
106600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106700030721     C                             + ' ' + 'VABNRS'
106800030721     C                   ENDIF
106900030721     C                   ENDIF
107000030721     C*** LNA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
107200030721     C                   EVAL      PiStr=SkSplitCSV(i)
107300030721     C                   EXSR      CHKNUM
107400030721     C                   IF        PiInt=*on    AND
107500030721     C                             PiVal<=999   AND
107600030721     C                             PiVal>*zeros
107700030721     C                   Z-ADD     PiVal         VABLNA
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABLNA = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABLNA'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** CTR
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiInt=*on  AND
109000030721     C                             PiVal<=999
109100030721     C                   Z-ADD     PiVal         VABCTR
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABCTR'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** IAS
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABIAS
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABIAS'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** VLB
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABVLB
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABVLB'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** QFT
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiNum=*on
112700030721     C                   Z-ADD     PiVal         VABQFT
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABQFT'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** NCD
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiInt=*on      AND
113900030721     C                             PiVal<=9999999
114000030721     C                   Z-ADD     PiVal         VABNCD
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABNCD'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** NCA
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on      AND
115200030721     C                             PiVal<=9999999
115300030721     C                   Z-ADD     PiVal         VABNCA
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABNCA'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** ZNC
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on AND
116500030721     C                             PiVal<=99
116600030721     C                   Z-ADD     PiVal         VABZNC
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABZNC'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** DCR
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on       AND
117800030721     C                             PiVal<=99999999
117900030721     C                   Z-ADD     PiVal         VABDCR
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABDCR'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** HCR
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on   AND
119100030721     C                             PiVal<=9999
119200030721     C                   Z-ADD     PiVal         VABHCR
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABHCR'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** VMD
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABVMD
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABVMD'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** ANT
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on        AND
121600030721     C                             PiVal<=999999999
121700030721     C                   Z-ADD     PiVal         VABANT
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABANT'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** CMO
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on
122900030721     C                   Z-ADD     PiVal         Num5_0
123000030721     C                   MOVEL(p)  Num5_0        VABCMO
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABCMO = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCMO'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030715     C*
123900030715     C                   ENDIF
124000030715     C                   EVAL      i = i + 1
124100030715     C                   ENDDO
124200040726     C*
124300040726     C* Considerazioni personalizzate x il traduttore corrente
124400060215     C                   EVAL      wNote = %trim(wNote)
124500060215     C                   EVAL      VABNOT = %subst(wNote:1:35)
124600060215     C                   EVAL      VABNT2 = %subst(wNote:36:35)
124700040726     C*
124800040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
124900040726     C                   IF        VABNSP = *zeros
125000040726     C                   SETON                                        31
125100040726     C                   ENDIF
125200040726     C*
125300040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
125400040726     C                   IF        VABLNP = *zeros
125500040726     C                   SETON                                        31
125600040726     C                   ENDIF
125700040726     C*
125800040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
125900040726     C* d valorizzarli sempre entrambi
126000040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
126100040726     C                   Z-ADD     VABNSP        VABRMN
126200040726     C                   ENDIF
126300040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
126400040726     C                   MOVEL     VABRMN        VABRMA
126500040726     C                   ENDIF
126600040726     C*
126700040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
126800040726     C                   IF        VABCAS > *zeros
126900040726     C                   IF        VABVCA = *blanks
127000040726     C                   EVAL      VABVCA = 'EUR'
127100060215     C                   EVAL      VABTIC = 'BM'
127200040726     C                   ENDIF
127300040726     C                   IF        VABCBO = '1'
127400040726     C                   EVAL      VABCBO = '4'
127500040726     C                   ENDIF
127600040726     C                   IF        VABCBO = '2'
127700040726     C                   EVAL      VABCBO = '6'
127800040726     C                   ENDIF
127900040726     C                   ENDIF
128000020204     C*
128100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
128200020204     C                   EXSR      CHKIMPDIV
128300030715     C                   ENDIF
128400020204     C*
128500010607     C                   ENDSR
128600010607     C*----------------------------------------------------*
128700010601
128800020204
128900020204     C*----------------------------------------------------*
129000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
129100020204     C*----------------------------------------------------*
129200020204     C     CHKIMPDIV     BEGSR
129300020204     C*
129400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
129500020204     C                   Z-ADD     *zeros        wrkDec            9 9
129600020204     C*
129700020204     C* Come prima cosa effettuo considerazioni sulla divisa
129800020204     C                   IF        vabIAS > *zeros
129900020204     C                   IF        vabVAS <> 'EUR'
130000020204     C                   EVAL      vabVAS =  'ITL'
130100020204     C                   ENDIF
130200020204     C                   ENDIF
130300020204     C*
130400020204     C                   IF        vabCAS > *zeros
130500020204     C                   IF        vabVCA <> 'EUR'
130600020204     C                   EVAL      vabVCA =  'ITL'
130700020204     C                   ENDIF
130800020204     C                   ENDIF
130900020204     C*
131000020204     C                   IF        vabVMD > *zeros
131100020204     C                   IF        vabVAD <> 'EUR'
131200020204     C                   EVAL      vabVAD =  'ITL'
131300020204     C                   ENDIF
131400020204     C                   ENDIF
131500020204     C*
131600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
131700020204     C                   Z-ADD     vabIAS        wrkDec
131800020204     C                   IF        wrkDec > *zeros
131900020204     C                   IF        vabVAS = 'ITL'
132000020204     C                   EVAL      vabIAS = *zeros
132100020204     C                   ENDIF
132200020204     C                   ENDIF
132300020204     C*
132400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
132500020204     C                   Z-ADD     vabCAS        wrkDec
132600020204     C                   IF        wrkDec > *zeros
132700020204     C                   IF        vabVCA = 'ITL'
132800020204     C                   EVAL      vabCAS = *zeros
132900020204     C                   ENDIF
133000020204     C                   ENDIF
133100020204     C*
133200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
133300020204     C                   Z-ADD     vabVMD        wrkDec
133400020204     C                   IF        wrkDec > *zeros
133500020204     C                   IF        vabVAD = 'ITL'
133600020204     C                   EVAL      vabVMD = *zeros
133700020204     C                   ENDIF
133800020204     C                   ENDIF
133900020204     C*
134000020204     C                   ENDSR
134100020204     C***
134200020204
134300010330
134400010330
134500010330     C*----------------------------------------------------*
134600010330     C*  CONTROLLO NUMERICITA' CAMPI
134700010330     C*----------------------------------------------------*
134800010330     C     CHKNUM        BEGSR
134900010330     C*
135000010606     C                   IF        PiDecChr = *blanks
135100030715     C                   EVAL      PiDecChr = CharNUM
135200010606     C                   ENDIF
135300010606     C*
135400010606     C                   CALL(e)   'ISNUMERIC'
135500010330     C                   PARM                    PiStr            30
135600010606     C                   PARM                    PiDecChr          1
135700010330     C                   PARM      *ZEROS        PiVal            30 9
135800010330     C                   PARM      '0'           PiInt             1
135900010330     C                   PARM      '0'           PiNum             1
136000010330     C                   IF        %error
136100010606     C                   EVAL      PiNum=*off
136200010330     C                   ENDIF
136300010330     C*
136400010330     C                   ENDSR
136500010330     C***
136600010330
136700010601
136800010601
136900010601
137000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
137100010601     C     repfil        BEGSR
137200010601     C*
137300010601     C                   if        invfil = *zeros and
137400010601     C                             depfil > *zeros and
137500010629     C                             (vinflg = *blanks or
137600010629     C                              vinflg = *zeros)
137700010601     C
137800010601     C                   eval      invfil = depfil
137900010601     C                   endif
138000010601     C*
138100010601     C                   if        depfil <> invfil and
138200010601     C                             invfil > *zeros
138300010601     C                   eval      flgMulti = '1'
138400010601     C                   if        vinflg = *blanks
138500010601     C                   add       1             cntNonEl
138600010601     C                   endif
138700010601     C                   endif
138800010601     C*
138900010601     C                   if        vinflg = '2'
139000010601     C                   eval      flgStato = '2'
139100010601     C                   endif
139200010601     C*
139300010601     C                   ENDSR
139400010601     C***
139500010601
139600010601
139700010601
139800010330
139900010330
140000010330
140100990920      /TITLE Invio dei dati al punto operativo.
140200000613     C     invio         BEGSR
140300990920     C*
140400990920     C                   reset                   dscmz
140500010601     C                   move      invfil        cmzdst
140600990920     C                   eval      cmzfld = vlrfou
140700990920     C                   eval      cmzmbd = vlrhdl
140800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
140900000710     C                   if        prmfir = *blanks
141000021025     C                   eval      cmzfla = 'FIVAB00F'
141100021025     C                   eval      cmzmba = 'FIVAB00F'
141200000710     C                   else
141300000710     C                   eval      cmzfla = prmfir
141400000710     C                   eval      cmzmba = prmfir
141500000710     C                   endif
141600990920     C                   eval      cmznrr = *zeros
141700990920     C                   move      §ctrok        cmznrr
141800021018     C                   eval      cmzlba = vlrfl1
141900990920     C                   call(e)   'TIS711C'
142000990920     C                   parm                    dscmz
142100990921     C                   parm      *blanks       esito
142200990920     C                   if        %error
142300990920     C                             or cmzerr = '1'
142400990921     C                             or esito  = '1'
142500000710     C                   eval      wrkesito = '3'
142600990920     C                   endif
142700990920     C*
142800000613     C                   ENDSR
142900990910
143000010601
143100010601
143200010601
143300010601
143400010601      /TITLE Invio dei dati al punto operativo.
143500010601     C     opeini        BEGSR
143600010601     C*
143700010601     C* Inizializzo flag e contatori operativi
143800010601     C                   movel     '0'           flgGiro           1
143900010601     C                   movel     '0'           flgMulti          1
144000010601     C                   movel     '1'           flgStato          1
144100010615     C                   movel     '0'           flgOk             1
144200010601     C                   z-add     *zeros        cntNonEl         10 0
144300010601     C                   z-add     *zeros        depfil            3 0
144400010601     C                   z-add     *zeros        invfil            3 0
144500010601     C*
144600010601     C                   ENDSR
144700010601     C***
144800010601
144900010601
145000050421
145100050421
145200050421     C     *pssr         BEGSR
145300050421     C*
145400050421     C                   eval      wrkesito = '2'
145500050421     C*
145600050421     C                   ENDSR     '*CANCL'
145700050421     C***
145800050421
145900010601
146000010330
146100010330
146200000613     C     *inzsr        BEGSR
146300990910     C*
146400990910     C     *entry        plist
146500990920     C                   parm                    tivlrds
146600990921     C                   parm      wrkesito      esito
146700000724     C                   parm                    prmlit
146800000710     C                   parm                    prmfir
146900010330     C*
147000010330     C* CALCOLA LA DATA CORRENTE
147100010330     C                   time                    wn14             14 0
147200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
147300010330     C                   z-add     wn8           g08dat
147400010330     C                   z-add     *zeros        g08inv
147500010330     C                   movel     '0'           g08err
147600010330     C                   call      'XSRDA8'
147700010330     C                   parm                    wlbda8
147800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
147900000613     C*
148000000613     C                   ENDSR
148100000613     C***
148200990908
148300030715     Otitvxlp   E            riepilogo         2
148400990915     O                                              'Upload via Internet'
148500990915     O                                           +1 'Traduzione TIVIN00R -'
148600021025     O                                           +1 'FIVABWWR'
148700990915     O                                           +1 'Testate bolle'
148800990915     O          E            riepilogo   2
148900990915     O                       wrkdata
149000990915     O                       wrkora              +1
149100990915     O                       procname            +1
149200990915     O          E            riepilogo   2
149300990915     O                                              'Cliente..................:'
149400010605     O                       VABCCM        z     +1
149500990915     O          E            riepilogo   2
149600990920     O                                              'Riferimento Strategi.....:'
149700990920     O                       vlrhdl              +1
149800990915     O          E            riepilogo   2
149900990915     O                                              'Giusti...................:'
150000971022     O                       §CTROK        2   +  1
150100990915     O          E            riepilogo   2
150200990915     O                                              'Sbagliati e corretti.....:'
150300971022     O                       §CTRMO        2   +  1
150400990915     O          E            riepilogo   2
150500990915     O                                              'Sbagliati e scartati.....:'
150600971022     O                       §CTRNO        2   +  1
150700000613
150800030715     Otitvxlps  E            testdett          2
150900000613     O                                              'Upload via Internet'
151000000613     O                                           +1 'Traduzione TIVIN00R -'
151100021025     O                                           +1 'FIVABWWR'
151200000613     O                                           +1 'Testate bolle'
151300000616     O          E            testdett    3
151400000613     O                                           +2 'N° rec'
151500000613     O                                           +3 'Anteprima contenuto'
151600000616     O          E            rigadett    2
151700000613     O                       rrnum               +2
151800000621     O                       recko               +3
151900000616     O          E            findett     2
152000000613     O                       wrkdata
152100000613     O                       wrkora              +1
152200000613     O                       procname            +1
152300000616     O          E            findett     2
152400000613     O                                              'Cliente..................:'
152500010605     O                       VABCCM        z     +1
152600000616     O          E            findett     2
152700000613     O                                              'Riferimento Strategi.....:'
152800000613     O                       vlrhdl              +1
152900000616     O          E            findett     2
153000000613     O                                              'Giusti...................:'
153100000613     O                       §CTROK        2   +  1
153200000616     O          E            findett     2
153300000613     O                                              'Sbagliati e corretti.....:'
153400000613     O                       §CTRMO        2   +  1
153500000616     O          E            findett     2
153600000613     O                                              'Sbagliati e scartati.....:'
153700000613     O                       §CTRNO        2   +  1
153800000613** CMD - COMANDI CL
153900030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
154000030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
154100030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
154200010606
154300000613
154400040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
154500060215FILLER1|VABNSP|FILLER2|VABRSD|VABIND|VABCAD|VABLOD|VABPRD|VABNCL|
154600060215VABPKB|VABCAS|FILLER3|VABRD2|FILLER4|VABNOT
154700050321
154800050321
154900060215
