000100060306      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente Editrice Periodici Cod. 0893534)
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
003800060306     D wPrimoRecord    s              1    INZ(*zeros)
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
020400080121     C                   exsr      inzvar
020500080121     C                   exsr      impvab
020600010330     C*
020700030822     C                   do        *HIVAL
020800010330     C*
020900030822     C                   read      tivin00r                               70
021000060215     C*
021100060215     C* Gestisco se trattasi del primo record => che occorre ignorare
021200060215     C                   if        wPrimoRecord = *zeros
021300060215     C                   eval      wPrimoRecord = '1'
021400060215     C                   eval      vinflg = '1'
021500060215     C                   else
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
029600060215     C*
029700060215     C* Se era il primo record (wPrimoRecord) => lo salto e aggiorno lo stato
029800060215     C                   endif
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
036100080121     C                   MOVEL     *blanks       wNOTE_1          50
036200080121     C                   MOVEL     *blanks       wNOTE_2          50
036300030822     C*
036400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036500030822     C                   CLEAR                   FIVAB000
036600030822     C                   CLEAR                   SkSplitCSV
036700030822     C                   EVAL      i = 1
036800030822     C                   EVAL      posDa = *zeros
036900030822     C                   EVAL      posA  = *zeros
037000030822     C*
037100030822     C* Reimposto i valori di default
037200030822     C                   EXSR      DEFCAM
037300030822     C*
037400010330     C                   ENDSR
037500010330     C*----------------------------------------------------*
037600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037700010330     C*----------------------------------------------------*
037800010330     C     DEFCAM        BEGSR
037900030822     C*
038000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
038100030715     C* e delimitatore testo.
038200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038500030822     C*
038600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
038800030715     C                   SETON                                        50
038900030715     C                   ELSE
039000030715     C                   SETOFF                                       50
039100030715     C                   ENDIF
039200030822     C*
039300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039400030822     C                   EVAL      posDaDft = 1
039500030822     C                   EVAL      posADft  = 0
039600030822     C                   EVAL      wGiroDft = 0
039700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039800030822     C                             posDaDft > 0
039900030822     C*
040000030822     C* Gestisco il 1° giro
040100030822     C                   IF        wGiroDft = 0
040200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040400030822     C* Incremento il contatore dei "giri"
040500030822     C                   EVAL      wGiroDft = 1
040600030822     C                   ELSE
040700030822     C                   EVAL      posDaDft = posADft
040800030822     C                   ENDIF
040900030822     C* Eseguo lo scan x trovare la fine del campo corrente
041000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
041100030822     C*
041200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041400030822     C                   IF        posDaDft > 0 AND
041500030822     C                             posADft  > 0
041600030822     C* NCL
041700030822     C                   IF        %subst(
041800030822     C                             %subst(vlrppt:posDaDft+1:
041900030822     C                             posADft-posDaDft-1):1:3)
042000030822     C                             = 'NCL'
042100030822     C                   EVAL      PiStr=%trim(%subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042400030822     C                   EXSR      CHKNUM
042500030822     C                   IF        PiInt=*on
042600030822     C                   Z-ADD     PiVal         VABNCL
042700030822     C                   ENDIF
042800030822     C                   ENDIF
042900030822     C* CCM
043000030822     C                   IF        %subst(
043100030822     C                             %subst(vlrppt:posDaDft+1:
043200030822     C                             posADft-posDaDft-1):1:3)
043300030822     C                             = 'CCM'
043400030822     C                   EVAL      PiStr=%trim(%subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043700030822     C                   EXSR      CHKNUM
043800030822     C                   IF        PiInt=*on
043900030822     C                   Z-ADD     PiVal         VABCCM
044000030822     C                   ENDIF
044100030822     C                   ENDIF
044200030822     C* LNP
044300030822     C                   IF        %subst(
044400030822     C                             %subst(vlrppt:posDaDft+1:
044500030822     C                             posADft-posDaDft-1):1:3)
044600030822     C                             = 'LNP'
044700030822     C                   EVAL      PiStr=%trim(%subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045000030822     C                   EXSR      CHKNUM
045100030822     C                   IF        PiInt=*on
045200030822     C                   Z-ADD     PiVal         VABLNP
045300030822     C                   ENDIF
045400030822     C                   ENDIF
045500030822     C* NRS
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'NRS'
046000030822     C                   EVAL      PiStr=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   EXSR      CHKNUM
046400030822     C                   IF        PiInt=*on
046500030822     C                   Z-ADD     PiVal         VABNRS
046600030822     C                   ENDIF
046700030822     C                   ENDIF
046800030822     C* CTR
046900030822     C                   IF        %subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):1:3)
047200030822     C                             = 'CTR'
047300030822     C                   EVAL      PiStr=%trim(%subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047600030822     C                   EXSR      CHKNUM
047700030822     C                   IF        PiInt=*on
047800030822     C                   Z-ADD     PiVal         VABCTR
047900030822     C                   ENDIF
048000030822     C                   ENDIF
048100030822     C* PKB
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'PKB'
048600030822     C                   EVAL      PiStr=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   EXSR      CHKNUM
049000030822     C                   IF        PiNum=*on
049100030822     C                   Z-ADD     PiVal         VABPKB
049200030822     C                   ENDIF
049300030822     C                   ENDIF
049400030822     C* VLB
049500030822     C                   IF        %subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):1:3)
049800030822     C                             = 'VLB'
049900030822     C                   EVAL      PiStr=%trim(%subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050200030822     C                   EXSR      CHKNUM
050300030822     C                   IF        PiNum=*on
050400030822     C                   Z-ADD     PiVal         VABVLB
050500030822     C                   ENDIF
050600030822     C                   ENDIF
050700030822     C* QFT
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'QFT'
051200030822     C                   EVAL      PiStr=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   EXSR      CHKNUM
051600030822     C                   IF        PiNum=*on
051700030822     C                   Z-ADD     PiVal         VABQFT
051800030822     C                   ENDIF
051900030822     C                   ENDIF
052000030822     C* CBO
052100030822     C                   IF        %subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):1:3)
052400030822     C                             = 'CBO'
052500030822     C                   EVAL      VABCBO=%trim(%subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800030822     C                   ENDIF
052900030822     C* TSP
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'TSP'
053400030822     C                   EVAL      VABTSP=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   ENDIF
053800030822     C* VAS
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'VAS'
054300030822     C                   EVAL      VABVAS=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   ENDIF
054700030822     C* VCA
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'VCA'
055200030822     C                   EVAL      VABVCA=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   ENDIF
055600030822     C* GCA
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'GCA'
056100030822     C                   EVAL      VABGCA=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   ENDIF
056500030822     C* CTM
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CTM'
057000030822     C                   EVAL      VABCTM=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   ENDIF
057400030822     C* FFD
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'FFD'
057900030822     C                   EVAL      VABFFD=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* VAD
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'VAD'
058800030822     C                   EVAL      VABVAD=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* GMA
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'GMA'
059700030822     C                   EVAL      VABGMA=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* GGA
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'GGA'
060600030822     C                   EVAL      VABGGA=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* GVA
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'GVA'
061500030822     C                   EVAL      VABGVA=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* TC1
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'TC1'
062400030822     C                   EVAL      VABTC1=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* TC2
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'TC2'
063300030822     C                   EVAL      VABTC2=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* ...
063800030822     C                   ENDIF
063900030822     C                   ENDDO
064000020204     C*
064100010330     C                   ENDSR
064200010607     C*----------------------------------------------------*
064300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064400010607     C*----------------------------------------------------*
064500010607     C     IMPVAB        BEGSR
064600010607     C*
064700010607     C                   SETOFF                                       3132
064800030515     C*
064900010607     C                   MOVEL     datcor        VABAAS
065000010607     C                   MOVE      datcor        VABMGS
065100050321     C*
065200050512     C* 1° Giro x eliminare il separatore testo
065300060215     C                   IF        CharTXT <> *blanks
065400050321     C                   Z-ADD     1             wj                5 0
065500050321     C                   DOW       wj <= %len(vindta) - 1
065600050321     C                   IF        %subst(vindta:wj:1) = CharTXT
065700050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
065800050512     C                                      %subst(vindta:wj+1)
065900050512     C                   EVAL      wj = wj - 1
066000050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
066100050321     C                   ENDIF
066200050321     C                   ADD       1             wj
066300050321     C                   ENDDO
066400060215     C                   ENDIF
066500050512     C*
066600060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
066700050512     C* 2 byte a blank
066800050512     C                   IF        %subst(vindta:1:1) = CharCSV
066900050512     C                   EVAL      vindta = '  ' + vindta
067000050512     C                   ENDIF
067100010607     C*
067200030715     C**********
067300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
067400030715     C**********
067500030715     C                   IF        wGiro = *zeros
067600040726     C*
067700040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
067800040726     C                   EVAL      depvinDTA = vinDTA
067900040726     C*
068000040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
068100040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
068200050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
068300060215     C                                      %trim(intcol(5))
068400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
068500031201     C     minu:maiu     XLATE     vindta        vindta
068600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068700040324     C                   DOW       posDa <= %len(%trimr(vindta))
068800030715     C*
068900030715     C* Gestisco il 1° campo
069000030715     C                   IF        i = 1
069100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
069200030715     C                   EVAL      posDa = 1
069300030715     C* Eseguo lo scan x trovare la fine del primo campo
069400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069500030715     C* A questo punto "estraggo" il campo corrente
069600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069700030715     C                                                    (posA-posDa))
069800030715     C* X i campi successivi al 1°
069900030715     C                   ELSE
070000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
070100030715     C                   EVAL      posDa = posA + 1
070200030715     C* Eseguo lo scan x trovare la fine del campo corrente
070300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070400030715     C* Gestisco l'ultimo campo
070500030715     C                   IF        posA = *zeros
070600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070700030715     C* A questo punto "estraggo" il campo corrente
070800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070900030715     C                                                    (posA-posDa))
071000030715     C                   LEAVE
071100030715     C                   ELSE
071200030715     C* A questo punto "estraggo" il campo corrente
071300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071400030715     C                                                    (posA-posDa))
071500030715     C                   ENDIF
071600030715     C                   ENDIF
071700030715     C* Incremento il contatore d campo
071800030715     C                   EVAL      i = i +1
071900030715     C                   ENDDO
072000030715     C*
072100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
072200030715     C* delimitazione testo
072300030715     C                   EVAL      i = 1
072400030715     C                   DOW       i < %elem(SkSplitFLD)
072500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
072600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072700030715     C                   EVAL      i = i + 1
072800030715     C                   ENDDO
072900030715     C                   EVAL      wGiro = 1
073000040726     C* Ripristino la 1° riga originale
073100040726     C                   EVAL      vinDTA = depvinDTA
073200030715     C                   ELSE
073300030715     C                   EVAL      wGiro = 2
073400031204     C**********
073500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
073600031204     C* d separatore campo
073700031204     C**********
073800060215     C                   EVAL      vindta = %trim(vindta)
073900060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
074000060215     C                   EVAL      vindta = %subst(vindta:2)
074100060215     C                   ENDDO
074200060215     C
074300060215     C                   Z-ADD     *zeros        lunghInput        4 0
074400060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
074500060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074600060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074700060215     C                   EVAL      lunghInput = %len(%trim(vindta))
074800060215     C                   ENDDO
074900030715     C**********
075000030715     C* Effettuo lo split del campo dati d input
075100030715     C**********
075200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075300040324     C                   DOW       posDa <= %len(%trimr(vindta))
075400030715     C*
075500030715     C* Gestisco il 1° campo
075600030715     C                   IF        i = 1
075700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075800030715     C                   EVAL      posDa = 1
075900030715     C* Eseguo lo scan x trovare la fine del primo campo
076000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076100030715     C* A questo punto "estraggo" il campo corrente
076200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076300030715     C                                                    (posA-posDa))
076400030715     C* X i campi successivi al 1°
076500030715     C                   ELSE
076600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076700030715     C                   EVAL      posDa = posA + 1
076800031204     C* Verifico che nn vi sia il campo nn valorizzato
076900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
077000031204     C* Se campo nn valorizzato skippo al prossimo
077100031204     C                   EVAL      SkSplitCSV(i) = *blanks
077200031204     C                   EVAL      posA  = posA + 1
077300031204     C                   ELSE
077400030715     C* Eseguo lo scan x trovare la fine del campo corrente
077500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077600030715     C* Gestisco l'ultimo campo
077700030715     C                   IF        posA = *zeros
077800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C                   LEAVE
078300030715     C                   ELSE
078400030715     C* A questo punto "estraggo" il campo corrente
078500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078600030715     C                                                    (posA-posDa))
078700030715     C                   ENDIF
078800031204     C                   ENDIF
078900030715     C                   ENDIF
079000030715     C* Incremento il contatore d campo
079100030715     C                   EVAL      i = i +1
079200030715     C                   ENDDO
079300030715     C*
079400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079500030715     C* delimitazione testo
079600030715     C                   EVAL      i = 1
079700030715     C                   DOW       i < %elem(SkSplitCSV)
079800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
080000030715     C                   EVAL      i = i + 1
080100030715     C                   ENDDO
080200030715     C*
080300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080400030715     C                   EVAL      i = 1
080500030715     C                   DOW       i < %elem(SkSplitFLD)
080600030715     C*
080700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080900030715     C                             SkSplitCSV(i) <> *blanks
081000030721     C*** ATB
081100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
081200030721     C                   EVAL      VABATB = SkSplitCSV(i)
081300030721     C                   ENDIF
081400030715     C*** RSD
081500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081900030715     C* ==
082000030715     C                   ENDIF
082100030721     C*** RD2
082200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
082300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
082400030721     C                   ENDIF
082500030715     C*** IND
082600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082700030715     C                   EVAL      VABIND = SkSplitCSV(i)
082800030715     C                   ENDIF
082900030715     C*** LOD
083000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
083100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
083200030715     C                   ENDIF
083300030715     C*** PRD
083400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083600030715     C                   ENDIF
083700030721     C*** NZD
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030715     C*** RMA
084200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
084300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
084400030715     C                   ENDIF
084500030715     C*** NOT
084600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084700060306     C                   EVAL      VABNOT = SkSplitCSV(i)
084800030715     C                   ENDIF
084900030715     C*** NT2
085000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
085100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
085200030715     C                   ENDIF
085300030715     C*** VCA
085400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085600030715     C                   ENDIF
085700030715     C*** VAS
085800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
086000030715     C                   ENDIF
086100030715     C*** FFD
086200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
086300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
086400030715     C                   ENDIF
086500030715     C*** CBO
086600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086700030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086800030715     C                   ENDIF
086900030721     C*** GC1
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
087100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** GC2
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
087500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** TSP
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** NAS
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
088300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** TIC
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** GCA
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
089100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** XCO
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
089500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** CTM
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** TCR
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
090300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** CTS
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** FTM
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
091100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** VAD
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
091500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** GMA
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** GGA
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
092300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** GVA
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*** TC1
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
093100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** TC2
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
093500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** SCL
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** RMO
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
094300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** NMO
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*
095000030721     C* Reperisco quindi i campi numerici...
095100030721     C*** CAD
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
095300030721     C                   EVAL      PiStr=SkSplitCSV(i)
095400030721     C                   EXSR      CHKNUM
095500030721     C                   IF        PiInt=*on
095600030721     C                   Z-ADD     PiVal         Num5_0
095700030721     C                   MOVEL(p)  Num5_0        VABCAD
095800030721     C                   ELSE
095900030721     C                   SETON                                        32
096000030721     C                   EVAL      VABCAD = *zeros
096100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096200030721     C                             + ' ' + 'VABCAD'
096300030721     C                   ENDIF
096400030721     C                   ENDIF
096500030721     C*** RMN
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096700030721     C                   EVAL      PiStr=SkSplitCSV(i)
096800030721     C                   EXSR      CHKNUM
096900030721     C                   IF        PiInt=*on
097000030721     C                   Z-ADD     PiVal         VABRMN
097100030721     C                   ELSE
097200030721     C                   SETON                                        32
097300030721     C                   EVAL      VABRMN = 1
097400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097500030721     C                             + ' ' + 'VABRMN'
097600030721     C                   ENDIF
097700030721     C                   ENDIF
097800030721     C*** NSP
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098000030721     C                   EVAL      PiStr=SkSplitCSV(i)
098100030721     C                   EXSR      CHKNUM
098200040726     C                   IF        PiInt=*on
098300040726     C                   Z-ADD     PiVal         VABNSP
098400040802     C*                  IF        PiVal>9999999
098500040802     C*                  SETON                                        32
098600040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
098700040802     C*                            + ' ' + 'VABNSP'
098800040802     C*                  ENDIF
098900030721     C                   ELSE
099000030721     C                   SETON                                        32
099100030721     C                   EVAL      VABNSP = *zeros
099200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099300030721     C                             + ' ' + 'VABNSP'
099400030721     C                   ENDIF
099500030721     C                   ENDIF
099600030721     C*** NCL
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099800030721     C                   EVAL      PiStr=SkSplitCSV(i)
099900030721     C                   EXSR      CHKNUM
100000030721     C                   IF        PiInt=*on
100100030721     C                   Z-ADD     PiVal         VABNCL
100200030721     C                   ELSE
100300030721     C                   SETON                                        32
100400030721     C                   EVAL      VABNCL = *zeros
100500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100600030721     C                             + ' ' + 'VABNCL'
100700030721     C                   ENDIF
100800030721     C                   ENDIF
100900030721     C*** PKB
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
101100030721     C                   EVAL      PiStr=SkSplitCSV(i)
101200030721     C                   EXSR      CHKNUM
101300030721     C                   IF        PiNum=*on
101400030721     C                   Z-ADD     PiVal         VABPKB
101500030721     C                   ELSE
101600030721     C                   SETON                                        32
101700030721     C                   EVAL      VABPKB = *zeros
101800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101900030721     C                             + ' ' + 'VABPKB'
102000030721     C                   ENDIF
102100030721     C                   ENDIF
102200080121     C*** CAS
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102400030721     C                   EVAL      PiStr=SkSplitCSV(i)
102500030721     C                   EXSR      CHKNUM
102600030721     C                   IF        PiNum=*on
102700030721     C                   Z-ADD     PiVal         VABCAS
102800030721     C                   ELSE
102900030721     C                   SETON                                        32
103000030721     C                   EVAL      VABCAS = *zeros
103100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103200030721     C                             + ' ' + 'VABCAS'
103300030721     C                   ENDIF
103400030721     C                   ENDIF
103500030721     C*** CCM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiInt=*on      AND
104000030721     C                             PiVal<=9999999 AND
104100030721     C                             PiVal>*zeros
104200030721     C                   Z-ADD     PiVal         VABCCM
104300030721     C                   ELSE
104400030721     C                   SETON                                        32
104500030721     C                   EVAL      VABCCM = *zeros
104600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104700030721     C                             + ' ' + 'VABCCM'
104800030721     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** LNP
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
105200030721     C                   EVAL      PiStr=SkSplitCSV(i)
105300030721     C                   EXSR      CHKNUM
105400030721     C                   IF        PiInt=*on    AND
105500030721     C                             PiVal<=999   AND
105600030721     C                             PiVal>*zeros
105700030721     C                   Z-ADD     PiVal         VABLNP
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      VABLNP = *zeros
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200030721     C                             + ' ' + 'VABLNP'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** NRS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiInt=*on AND
107000030721     C                             PiVal<=99
107100030721     C                   Z-ADD     PiVal         VABNRS
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107500030721     C                             + ' ' + 'VABNRS'
107600030721     C                   ENDIF
107700030721     C                   ENDIF
107800030721     C*** LNA
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
108000030721     C                   EVAL      PiStr=SkSplitCSV(i)
108100030721     C                   EXSR      CHKNUM
108200030721     C                   IF        PiInt=*on    AND
108300030721     C                             PiVal<=999   AND
108400030721     C                             PiVal>*zeros
108500030721     C                   Z-ADD     PiVal         VABLNA
108600030721     C                   ELSE
108700030721     C                   SETON                                        32
108800030721     C                   EVAL      VABLNA = *zeros
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABLNA'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** CTR
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on  AND
109800030721     C                             PiVal<=999
109900030721     C                   Z-ADD     PiVal         VABCTR
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABCTR'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** IAS
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiNum=*on
111100030721     C                   Z-ADD     PiVal         VABIAS
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABIAS'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** VLB
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiNum=*on
112300030721     C                   Z-ADD     PiVal         VABVLB
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABVLB'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** QFT
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiNum=*on
113500030721     C                   Z-ADD     PiVal         VABQFT
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABQFT'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** NCD
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on      AND
114700030721     C                             PiVal<=9999999
114800030721     C                   Z-ADD     PiVal         VABNCD
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABNCD'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** NCA
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on      AND
116000030721     C                             PiVal<=9999999
116100030721     C                   Z-ADD     PiVal         VABNCA
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABNCA'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** ZNC
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on AND
117300030721     C                             PiVal<=99
117400030721     C                   Z-ADD     PiVal         VABZNC
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABZNC'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** DCR
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on       AND
118600030721     C                             PiVal<=99999999
118700030721     C                   Z-ADD     PiVal         VABDCR
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABDCR'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** HCR
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on   AND
119900030721     C                             PiVal<=9999
120000030721     C                   Z-ADD     PiVal         VABHCR
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABHCR'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** VMD
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABVMD
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABVMD'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** ANT
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on        AND
122400030721     C                             PiVal<=999999999
122500030721     C                   Z-ADD     PiVal         VABANT
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABANT'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** CMO
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on
123700030721     C                   Z-ADD     PiVal         Num5_0
123800030721     C                   MOVEL(p)  Num5_0        VABCMO
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      VABCMO = *zeros
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABCMO'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600080121     C*
124700080121     C*******  CONSIDERAZIONI PARTICOLARI DENTRO CICLO ********
124800080121     C*** RMN - W
124900080121     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN_W'
125000080121     C                   EVAL      PiStr=SkSplitCSV(i)
125100080121     C                   EXSR      CHKNUM
125200080121     C                   IF        PiInt=*on
125300080121     C                   Z-ADD     PiVal         VABRMN
125400080121     C                   Z-ADD     PiVal         VABNSP
125500080121     C                   ELSE
125600080121     C                   SETON                                        32
125700080121     C                   EVAL      VABRMN = 1
125800080121     C                   EVAL      vinmsg = %trimr(vinmsg)
125900080121     C                             + ' ' + 'VABNSP VABRMN'
126000080121     C                   ENDIF
126100080121     C                   ENDIF
126200080121     C*** NOT - 1
126300080121     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT_1'
126400080121     C                   EVAL      wNOTE_1 = SkSplitCSV(i)
126500080121     C                   ENDIF
126600080121     C*** NOT - 2
126700080121     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT_2'
126800080121     C                   EVAL      wNOTE_1 = SkSplitCSV(i)
126900080121     C                   ENDIF
127000080121     C*** CAS - W
127100080121     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS_W'
127200080121     C                   EVAL      PiStr=SkSplitCSV(i)
127300080121     C     '€':' '       XLATE     PiStr         PiStr
127400080121     C                   EXSR      CHKNUM
127500080121     C                   IF        PiNum=*on
127600080121     C                   Z-ADD     PiVal         VABCAS
127700080121     C                   ELSE
127800080121     C                   SETON                                        32
127900080121     C                   EVAL      VABCAS = *zeros
128000080121     C                   EVAL      vinmsg = %trimr(vinmsg)
128100080121     C                             + ' ' + 'VABCAS'
128200080121     C                   ENDIF
128300080121     C                   ENDIF
128400080121     C*
128500080121     C*******  END PERSONALIZZAZIONI ********
128600030715     C*
128700030715     C                   ENDIF
128800030715     C                   EVAL      i = i + 1
128900030715     C                   ENDDO
129000080121     C*
129100080121     C*******  CONSIDERAZIONI PARTICOLARI FUORI CICLO ********
129200080121     C                   EVAL      VABNOT = %trim(wNOTE_1) + %trim(wNOTE_2)
129300080121     C*
129400080121     C*******  END PERSONALIZZAZIONI ********
129500040726     C*
129600040726     C* Considerazioni personalizzate x il traduttore corrente
129700060306     C* Reperisco la provincia dal CAP e dalla località
129800060306     C                   IF        VABPRD = *blanks AND
129900060306     C                             VABCAD <> *blanks
130000060306     C                   CLEAR                   TISI95DS
130100060306     C                   EVAL      I95TCN = '3'
130200060306     C                   Z-ADD     datcor        I95DAT
130300060306     C                   EVAL      I95CAP = VABCAD
130400060306     C                   EVAL      I95LOC = VABLOD
130500060306     C                   CALL      'TISI95R'
130600060306     C                   PARM                    TISI95DS
130700060306     C                   EVAL      VABPRD = O95PRV
130800060306     C                   ENDIF
130900040726     C*
131000040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
131100040726     C                   IF        VABNSP = *zeros
131200040726     C                   SETON                                        31
131300040726     C                   ENDIF
131400040726     C*
131500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
131600040726     C                   IF        VABLNP = *zeros
131700040726     C                   SETON                                        31
131800040726     C                   ENDIF
131900040726     C*
132000040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
132100040726     C* d valorizzarli sempre entrambi
132200040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
132300040726     C                   Z-ADD     VABNSP        VABRMN
132400040726     C                   ENDIF
132500040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
132600040726     C                   MOVEL     VABRMN        VABRMA
132700040726     C                   ENDIF
132800040726     C*
132900040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
133000040726     C                   IF        VABCAS > *zeros
133100040726     C                   IF        VABVCA = *blanks
133200040726     C                   EVAL      VABVCA = 'EUR'
133300060215     C                   EVAL      VABTIC = 'BM'
133400040726     C                   ENDIF
133500040726     C                   IF        VABCBO = '1'
133600040726     C                   EVAL      VABCBO = '4'
133700040726     C                   ENDIF
133800040726     C                   IF        VABCBO = '2'
133900040726     C                   EVAL      VABCBO = '6'
134000040726     C                   ENDIF
134100040726     C                   ENDIF
134200020204     C*
134300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
134400020204     C                   EXSR      CHKIMPDIV
134500030715     C                   ENDIF
134600020204     C*
134700010607     C                   ENDSR
134800010607     C*----------------------------------------------------*
134900010601
135000020204
135100020204     C*----------------------------------------------------*
135200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
135300020204     C*----------------------------------------------------*
135400020204     C     CHKIMPDIV     BEGSR
135500020204     C*
135600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
135700020204     C                   Z-ADD     *zeros        wrkDec            9 9
135800020204     C*
135900020204     C* Come prima cosa effettuo considerazioni sulla divisa
136000020204     C                   IF        vabIAS > *zeros
136100020204     C                   IF        vabVAS <> 'EUR'
136200020204     C                   EVAL      vabVAS =  'ITL'
136300020204     C                   ENDIF
136400020204     C                   ENDIF
136500020204     C*
136600020204     C                   IF        vabCAS > *zeros
136700020204     C                   IF        vabVCA <> 'EUR'
136800020204     C                   EVAL      vabVCA =  'ITL'
136900020204     C                   ENDIF
137000020204     C                   ENDIF
137100020204     C*
137200020204     C                   IF        vabVMD > *zeros
137300020204     C                   IF        vabVAD <> 'EUR'
137400020204     C                   EVAL      vabVAD =  'ITL'
137500020204     C                   ENDIF
137600020204     C                   ENDIF
137700020204     C*
137800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
137900020204     C                   Z-ADD     vabIAS        wrkDec
138000020204     C                   IF        wrkDec > *zeros
138100020204     C                   IF        vabVAS = 'ITL'
138200020204     C                   EVAL      vabIAS = *zeros
138300020204     C                   ENDIF
138400020204     C                   ENDIF
138500020204     C*
138600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
138700020204     C                   Z-ADD     vabCAS        wrkDec
138800020204     C                   IF        wrkDec > *zeros
138900020204     C                   IF        vabVCA = 'ITL'
139000020204     C                   EVAL      vabCAS = *zeros
139100020204     C                   ENDIF
139200020204     C                   ENDIF
139300020204     C*
139400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
139500020204     C                   Z-ADD     vabVMD        wrkDec
139600020204     C                   IF        wrkDec > *zeros
139700020204     C                   IF        vabVAD = 'ITL'
139800020204     C                   EVAL      vabVMD = *zeros
139900020204     C                   ENDIF
140000020204     C                   ENDIF
140100020204     C*
140200020204     C                   ENDSR
140300020204     C***
140400020204
140500010330
140600010330
140700010330     C*----------------------------------------------------*
140800010330     C*  CONTROLLO NUMERICITA' CAMPI
140900010330     C*----------------------------------------------------*
141000010330     C     CHKNUM        BEGSR
141100010330     C*
141200010606     C                   IF        PiDecChr = *blanks
141300030715     C                   EVAL      PiDecChr = CharNUM
141400010606     C                   ENDIF
141500010606     C*
141600010606     C                   CALL(e)   'ISNUMERIC'
141700010330     C                   PARM                    PiStr            30
141800010606     C                   PARM                    PiDecChr          1
141900010330     C                   PARM      *ZEROS        PiVal            30 9
142000010330     C                   PARM      '0'           PiInt             1
142100010330     C                   PARM      '0'           PiNum             1
142200010330     C                   IF        %error
142300010606     C                   EVAL      PiNum=*off
142400010330     C                   ENDIF
142500010330     C*
142600010330     C                   ENDSR
142700010330     C***
142800010330
142900010601
143000010601
143100010601
143200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
143300010601     C     repfil        BEGSR
143400010601     C*
143500010601     C                   if        invfil = *zeros and
143600010601     C                             depfil > *zeros and
143700010629     C                             (vinflg = *blanks or
143800010629     C                              vinflg = *zeros)
143900010601     C
144000010601     C                   eval      invfil = depfil
144100010601     C                   endif
144200010601     C*
144300010601     C                   if        depfil <> invfil and
144400010601     C                             invfil > *zeros
144500010601     C                   eval      flgMulti = '1'
144600010601     C                   if        vinflg = *blanks
144700010601     C                   add       1             cntNonEl
144800010601     C                   endif
144900010601     C                   endif
145000010601     C*
145100010601     C                   if        vinflg = '2'
145200010601     C                   eval      flgStato = '2'
145300010601     C                   endif
145400010601     C*
145500010601     C                   ENDSR
145600010601     C***
145700010601
145800010601
145900010601
146000010330
146100010330
146200010330
146300990920      /TITLE Invio dei dati al punto operativo.
146400000613     C     invio         BEGSR
146500990920     C*
146600990920     C                   reset                   dscmz
146700010601     C                   move      invfil        cmzdst
146800990920     C                   eval      cmzfld = vlrfou
146900990920     C                   eval      cmzmbd = vlrhdl
147000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
147100000710     C                   if        prmfir = *blanks
147200021025     C                   eval      cmzfla = 'FIVAB00F'
147300021025     C                   eval      cmzmba = 'FIVAB00F'
147400000710     C                   else
147500000710     C                   eval      cmzfla = prmfir
147600000710     C                   eval      cmzmba = prmfir
147700000710     C                   endif
147800990920     C                   eval      cmznrr = *zeros
147900990920     C                   move      §ctrok        cmznrr
148000021018     C                   eval      cmzlba = vlrfl1
148100990920     C                   call(e)   'TIS711C'
148200990920     C                   parm                    dscmz
148300990921     C                   parm      *blanks       esito
148400990920     C                   if        %error
148500990920     C                             or cmzerr = '1'
148600990921     C                             or esito  = '1'
148700000710     C                   eval      wrkesito = '3'
148800990920     C                   endif
148900990920     C*
149000000613     C                   ENDSR
149100990910
149200010601
149300010601
149400010601
149500010601
149600010601      /TITLE Invio dei dati al punto operativo.
149700010601     C     opeini        BEGSR
149800010601     C*
149900010601     C* Inizializzo flag e contatori operativi
150000010601     C                   movel     '0'           flgGiro           1
150100010601     C                   movel     '0'           flgMulti          1
150200010601     C                   movel     '1'           flgStato          1
150300010615     C                   movel     '0'           flgOk             1
150400010601     C                   z-add     *zeros        cntNonEl         10 0
150500010601     C                   z-add     *zeros        depfil            3 0
150600010601     C                   z-add     *zeros        invfil            3 0
150700010601     C*
150800010601     C                   ENDSR
150900010601     C***
151000010601
151100010601
151200050421
151300050421
151400050421     C     *pssr         BEGSR
151500050421     C*
151600050421     C                   eval      wrkesito = '2'
151700050421     C*
151800050421     C                   ENDSR     '*CANCL'
151900050421     C***
152000050421
152100010601
152200010330
152300010330
152400000613     C     *inzsr        BEGSR
152500990910     C*
152600990910     C     *entry        plist
152700990920     C                   parm                    tivlrds
152800990921     C                   parm      wrkesito      esito
152900000724     C                   parm                    prmlit
153000000710     C                   parm                    prmfir
153100010330     C*
153200010330     C* CALCOLA LA DATA CORRENTE
153300010330     C                   time                    wn14             14 0
153400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
153500010330     C                   z-add     wn8           g08dat
153600010330     C                   z-add     *zeros        g08inv
153700010330     C                   movel     '0'           g08err
153800010330     C                   call      'XSRDA8'
153900010330     C                   parm                    wlbda8
154000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
154100000613     C*
154200000613     C                   ENDSR
154300000613     C***
154400990908
154500030715     Otitvxlp   E            riepilogo         2
154600990915     O                                              'Upload via Internet'
154700990915     O                                           +1 'Traduzione TIVIN00R -'
154800021025     O                                           +1 'FIVABWWR'
154900990915     O                                           +1 'Testate bolle'
155000990915     O          E            riepilogo   2
155100990915     O                       wrkdata
155200990915     O                       wrkora              +1
155300990915     O                       procname            +1
155400990915     O          E            riepilogo   2
155500990915     O                                              'Cliente..................:'
155600010605     O                       VABCCM        z     +1
155700990915     O          E            riepilogo   2
155800990920     O                                              'Riferimento Strategi.....:'
155900990920     O                       vlrhdl              +1
156000990915     O          E            riepilogo   2
156100990915     O                                              'Giusti...................:'
156200971022     O                       §CTROK        2   +  1
156300990915     O          E            riepilogo   2
156400990915     O                                              'Sbagliati e corretti.....:'
156500971022     O                       §CTRMO        2   +  1
156600990915     O          E            riepilogo   2
156700990915     O                                              'Sbagliati e scartati.....:'
156800971022     O                       §CTRNO        2   +  1
156900000613
157000030715     Otitvxlps  E            testdett          2
157100000613     O                                              'Upload via Internet'
157200000613     O                                           +1 'Traduzione TIVIN00R -'
157300021025     O                                           +1 'FIVABWWR'
157400000613     O                                           +1 'Testate bolle'
157500000616     O          E            testdett    3
157600000613     O                                           +2 'N° rec'
157700000613     O                                           +3 'Anteprima contenuto'
157800000616     O          E            rigadett    2
157900000613     O                       rrnum               +2
158000000621     O                       recko               +3
158100000616     O          E            findett     2
158200000613     O                       wrkdata
158300000613     O                       wrkora              +1
158400000613     O                       procname            +1
158500000616     O          E            findett     2
158600000613     O                                              'Cliente..................:'
158700010605     O                       VABCCM        z     +1
158800000616     O          E            findett     2
158900000613     O                                              'Riferimento Strategi.....:'
159000000613     O                       vlrhdl              +1
159100000616     O          E            findett     2
159200000613     O                                              'Giusti...................:'
159300000613     O                       §CTROK        2   +  1
159400000616     O          E            findett     2
159500000613     O                                              'Sbagliati e corretti.....:'
159600000613     O                       §CTRMO        2   +  1
159700000616     O          E            findett     2
159800000613     O                                              'Sbagliati e scartati.....:'
159900000613     O                       §CTRNO        2   +  1
160000000613** CMD - COMANDI CL
160100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
160200030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
160300030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
160400010606
160500000613
160600040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
160700080121FILL1;VABRMN_W;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;
160800080121VABNOT_1;VABRD2;VABNOT_2;VABNT2;VABCAS_W
160900050321
161000050321
161100060215
