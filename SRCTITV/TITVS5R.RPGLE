000100060328      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente NEWSTONE Cod. 0661067)
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
003700060328     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
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
020000060328     C*
020100060328     C* Eseguo subito inizializzazioni
020200060328     C                   exsr      inzvar
020300060328     C*
020400060328     C* Se necessario e quindi richiesto nei parametri d ingresso =>
020500060328     C* eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020600040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020700060328     C                   if        *in51 = *on
020800040726     C                   exsr      impvab
020900060328     C                   endif
021000010330     C*
021100030822     C                   do        *HIVAL
021200010330     C*
021300030822     C                   read      tivin00r                               70
021400010618     C*
021500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021600010618     C                   if        vinflg = '1'
021700010618     C                   eval      flgOk = '1'
021800010618     C                   endif
021900060216     C*
022000060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
022100060216     C* => gestisco sempre
022200060216     C                   z-add     *zeros        wLenVINDTA        4 0
022300060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
022400060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
022500060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
022600060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
022700060216     C                   endif
022800060216     C                   enddo
022900010618     C*
023000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
023100040510     C                   z-add     *zeros        wLenVINDTA        4 0
023200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
023300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
023400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
023500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
023600040510     C                   z-add     1             wFlgVINDTA
023700040510     C                   leave
023800040510     C                   endif
023900040510     C                   enddo
024000040510     C*
024100040218     C                   if        vindta > *blanks AND
024200040510     C                             wFlgVINDTA = 1
024300010330     C                   add       1             rrnum
024400010330     C*
024500010601     C                   if        *in70 = *off and
024600010330     C                             (vinflg = *blanks
024700010330     C                              or vinflg = '0'
024800010330     C                              or vinflg = '2')
024900010330     C*
025000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025100010711     C                   if        vinflg = *blanks or vinflg = '0'
025200010711     C                   clear                   vinmsg
025300010711     C                   endif
025400010601     C*
025500010330     C                   exsr      inzvar
025600010605     C                   exsr      impvab
025700010601     C*
025800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025900010605     C                   eval      depfil = VABLNP
026000010601     C                   exsr      repfil
026100010601     C                   if        depfil = invfil
026200021025     C                   if        vlrpoi = 999
026300030822     C                   move(P)   invfil        VABFGS
026400021025     C                   else
026500030822     C                   move(P)   vlrpoi        VABFGS
026600021025     C                   endif
026700010601     C*
026800010601     C                   exsr      PREELA
026900010601     C*
027000010604     C* Ebbene...
027100010604     C*
027200030822     C  N31              add       1             §CTROK            7 0
027300030822     C   32              add       1             §CTRMO            7 0
027400030822     C   31              add       1             §CTRNO            7 0
027500030822     C                   if        wGiro = 2
027600030822     C  N31              write     FIVAB000
027700030822     C                   endif
027800010604     C*
027900010604     C                   if        *in31 = *off and
028000010604     C                             *in32 = *off
028100010604     C                   eval      vinflg = '1'
028200010604     C                   else
028300010604     C                   eval      recko = vindta
028400010604     C                   except    rigadett
028500010604     C                   eval      vinflg = '2'
028600010604     C                   endif
028700010604     C                   endif
028800010601     C*
028900010604     C                   endif
029000010604     C*
029100010330     C                   else
029200010330     C                   eval      vinflg = '1'
029300010330     C                   endif
029400010601     C*
029500010601     C  N70              update    tivin000
029600010330     C*
029700030822     C  N70              enddo
029800010601     C
029900010601     C*
030000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030100010601     C                   if        cntNonEl = *zeros or
030200010601     C                             flgMulti = '0'
030300010330     C* Se non ci sono record con errori ...
030400010601     C                   if        §ctrno = 0 and
030500010604     C                             §ctrmo = 0 and
030600010601     C                             flgStato <> '2'
030700010330     C* ... restituisco esito OK.
030800010330     C                   eval      wrkesito = '0'
030900010330     C                   else
031000010330     C                   if        §ctrok > 0
031100010330     C                   eval      wrkesito = '1'
031200010330     C                   else
031300010615     C                   if        flgOk = '0'
031400010615     C                   eval      wrkesito = '2'
031500010615     C                   else
031600010615     C                   eval      wrkesito = '6'
031700010615     C                   endif
031800010330     C                   endif
031900010330     C                   endif
032000010601     C                   else
032100010601     C                   eval      wrkesito = '9'
032200010601     C                   endif
032300010330     C*
032400010330     C                   if        %open(tivin00r)
032500010330     C                   close     tivin00r
032600010330     C                   endif
032700021025     C                   if        %open(fivabwwr)
032800021025     C                   close     fivabwwr
032900010330     C                   endif
033000010601     C*
033100010601     C                   if        vlrpoi <> 999
033200010601     C                   eval      invfil = vlrpoi
033300010601     C                   endif
033400010330     C*
033500010330     C                   if        §ctrok > 0
033600010601     C                             and invfil > *zeros
033700010330     C                   exsr      invio
033800010330     C                   endif
033900010601     C*
034000010618     C                   if        flgGiro = '1'
034100010601     C                   exsr      endela
034200010618     C                   endif
034300010330     C*
034400010330     C                   ENDSR
034500010330     C***
034600010601
034700010601
034800010601
034900010330     C*----------------------------------------------------*
035000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035100010330     C*----------------------------------------------------*
035200010330     C     INZVAR        BEGSR
035300010330     C*
035400030822     C* Inizializzo variabili di wrk
035500020204     C                   Z-ADD     *zeros        Num5_0            5 0
035600030822     C*
035700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035800030822     C                   CLEAR                   FIVAB000
035900030822     C                   CLEAR                   SkSplitCSV
036000030822     C                   EVAL      i = 1
036100030822     C                   EVAL      posDa = *zeros
036200030822     C                   EVAL      posA  = *zeros
036300030822     C*
036400030822     C* Reimposto i valori di default
036500030822     C                   EXSR      DEFCAM
036600030822     C*
036700010330     C                   ENDSR
036800010330     C*----------------------------------------------------*
036900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037000010330     C*----------------------------------------------------*
037100010330     C     DEFCAM        BEGSR
037200030822     C*
037300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
037400030715     C* e delimitatore testo.
037500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
037600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
037700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037800030822     C*
037900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038000060328     C                   SETOFF                                       50
038100060328     C                   IF        %subst(vlrppt:1:1) = 'S'
038200030715     C                   SETON                                        50
038300030715     C                   ENDIF
038400060328     C                   IF        %subst(vlrppt:1:1) = 'I'
038500060328     C                   SETON                                        51
038600060328     C                   ENDIF
038700030822     C*
038800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038900030822     C                   EVAL      posDaDft = 1
039000030822     C                   EVAL      posADft  = 0
039100030822     C                   EVAL      wGiroDft = 0
039200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
039300030822     C                             posDaDft > 0
039400030822     C*
039500030822     C* Gestisco il 1° giro
039600030822     C                   IF        wGiroDft = 0
039700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039900030822     C* Incremento il contatore dei "giri"
040000030822     C                   EVAL      wGiroDft = 1
040100030822     C                   ELSE
040200030822     C                   EVAL      posDaDft = posADft
040300030822     C                   ENDIF
040400030822     C* Eseguo lo scan x trovare la fine del campo corrente
040500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040600030822     C*
040700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040900030822     C                   IF        posDaDft > 0 AND
041000030822     C                             posADft  > 0
041100030822     C* NCL
041200030822     C                   IF        %subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):1:3)
041500030822     C                             = 'NCL'
041600030822     C                   EVAL      PiStr=%trim(%subst(
041700030822     C                             %subst(vlrppt:posDaDft+1:
041800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041900030822     C                   EXSR      CHKNUM
042000030822     C                   IF        PiInt=*on
042100030822     C                   Z-ADD     PiVal         VABNCL
042200030822     C                   ENDIF
042300030822     C                   ENDIF
042400030822     C* CCM
042500030822     C                   IF        %subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):1:3)
042800030822     C                             = 'CCM'
042900030822     C                   EVAL      PiStr=%trim(%subst(
043000030822     C                             %subst(vlrppt:posDaDft+1:
043100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043200030822     C                   EXSR      CHKNUM
043300030822     C                   IF        PiInt=*on
043400030822     C                   Z-ADD     PiVal         VABCCM
043500030822     C                   ENDIF
043600030822     C                   ENDIF
043700030822     C* LNP
043800030822     C                   IF        %subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):1:3)
044100030822     C                             = 'LNP'
044200030822     C                   EVAL      PiStr=%trim(%subst(
044300030822     C                             %subst(vlrppt:posDaDft+1:
044400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500030822     C                   EXSR      CHKNUM
044600030822     C                   IF        PiInt=*on
044700030822     C                   Z-ADD     PiVal         VABLNP
044800030822     C                   ENDIF
044900030822     C                   ENDIF
045000030822     C* NRS
045100030822     C                   IF        %subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):1:3)
045400030822     C                             = 'NRS'
045500030822     C                   EVAL      PiStr=%trim(%subst(
045600030822     C                             %subst(vlrppt:posDaDft+1:
045700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045800030822     C                   EXSR      CHKNUM
045900030822     C                   IF        PiInt=*on
046000030822     C                   Z-ADD     PiVal         VABNRS
046100030822     C                   ENDIF
046200030822     C                   ENDIF
046300030822     C* CTR
046400030822     C                   IF        %subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):1:3)
046700030822     C                             = 'CTR'
046800030822     C                   EVAL      PiStr=%trim(%subst(
046900030822     C                             %subst(vlrppt:posDaDft+1:
047000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047100030822     C                   EXSR      CHKNUM
047200030822     C                   IF        PiInt=*on
047300030822     C                   Z-ADD     PiVal         VABCTR
047400030822     C                   ENDIF
047500030822     C                   ENDIF
047600030822     C* PKB
047700030822     C                   IF        %subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):1:3)
048000030822     C                             = 'PKB'
048100030822     C                   EVAL      PiStr=%trim(%subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400030822     C                   EXSR      CHKNUM
048500030822     C                   IF        PiNum=*on
048600030822     C                   Z-ADD     PiVal         VABPKB
048700030822     C                   ENDIF
048800030822     C                   ENDIF
048900030822     C* VLB
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'VLB'
049400030822     C                   EVAL      PiStr=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   EXSR      CHKNUM
049800030822     C                   IF        PiNum=*on
049900030822     C                   Z-ADD     PiVal         VABVLB
050000030822     C                   ENDIF
050100030822     C                   ENDIF
050200030822     C* QFT
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'QFT'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiNum=*on
051200030822     C                   Z-ADD     PiVal         VABQFT
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* CBO
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'CBO'
052000030822     C                   EVAL      VABCBO=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   ENDIF
052400030822     C* TSP
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'TSP'
052900030822     C                   EVAL      VABTSP=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   ENDIF
053300030822     C* VAS
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'VAS'
053800030822     C                   EVAL      VABVAS=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   ENDIF
054200030822     C* VCA
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'VCA'
054700030822     C                   EVAL      VABVCA=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   ENDIF
055100030822     C* GCA
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'GCA'
055600030822     C                   EVAL      VABGCA=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   ENDIF
056000030822     C* CTM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CTM'
056500030822     C                   EVAL      VABCTM=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   ENDIF
056900030822     C* FFD
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'FFD'
057400030822     C                   EVAL      VABFFD=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   ENDIF
057800030822     C* VAD
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'VAD'
058300030822     C                   EVAL      VABVAD=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   ENDIF
058700030822     C* GMA
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'GMA'
059200030822     C                   EVAL      VABGMA=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   ENDIF
059600030822     C* GGA
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'GGA'
060100030822     C                   EVAL      VABGGA=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   ENDIF
060500030822     C* GVA
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'GVA'
061000030822     C                   EVAL      VABGVA=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   ENDIF
061400030822     C* TC1
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'TC1'
061900030822     C                   EVAL      VABTC1=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   ENDIF
062300030822     C* TC2
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'TC2'
062800030822     C                   EVAL      VABTC2=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   ENDIF
063200030822     C* ...
063300030822     C                   ENDIF
063400030822     C                   ENDDO
063500020204     C*
063600010330     C                   ENDSR
063700010607     C*----------------------------------------------------*
063800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063900010607     C*----------------------------------------------------*
064000010607     C     IMPVAB        BEGSR
064100010607     C*
064200010607     C                   SETOFF                                       3132
064300030515     C*
064400010607     C                   MOVEL     datcor        VABAAS
064500010607     C                   MOVE      datcor        VABMGS
064600050321     C*
064700050512     C* 1° Giro x eliminare il separatore testo
064800060215     C                   IF        CharTXT <> *blanks
064900050321     C                   Z-ADD     1             wj                5 0
065000050321     C                   DOW       wj <= %len(vindta) - 1
065100050321     C                   IF        %subst(vindta:wj:1) = CharTXT
065200050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
065300050512     C                                      %subst(vindta:wj+1)
065400050512     C                   EVAL      wj = wj - 1
065500050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
065600050321     C                   ENDIF
065700050321     C                   ADD       1             wj
065800050321     C                   ENDDO
065900060215     C                   ENDIF
066000050512     C*
066100060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
066200050512     C* 2 byte a blank
066300050512     C                   IF        %subst(vindta:1:1) = CharCSV
066400050512     C                   EVAL      vindta = '  ' + vindta
066500050512     C                   ENDIF
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
067800060215     C                                      %trim(intcol(5))
067900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
068000031201     C     minu:maiu     XLATE     vindta        vindta
068100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068200040324     C                   DOW       posDa <= %len(%trimr(vindta))
068300030715     C*
068400030715     C* Gestisco il 1° campo
068500030715     C                   IF        i = 1
068600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068700030715     C                   EVAL      posDa = 1
068800030715     C* Eseguo lo scan x trovare la fine del primo campo
068900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069000030715     C* A questo punto "estraggo" il campo corrente
069100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069200030715     C                                                    (posA-posDa))
069300030715     C* X i campi successivi al 1°
069400030715     C                   ELSE
069500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069600030715     C                   EVAL      posDa = posA + 1
069700030715     C* Eseguo lo scan x trovare la fine del campo corrente
069800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069900030715     C* Gestisco l'ultimo campo
070000030715     C                   IF        posA = *zeros
070100040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070200030715     C* A questo punto "estraggo" il campo corrente
070300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070400030715     C                                                    (posA-posDa))
070500030715     C                   LEAVE
070600030715     C                   ELSE
070700030715     C* A questo punto "estraggo" il campo corrente
070800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
070900030715     C                                                    (posA-posDa))
071000030715     C                   ENDIF
071100030715     C                   ENDIF
071200030715     C* Incremento il contatore d campo
071300030715     C                   EVAL      i = i +1
071400030715     C                   ENDDO
071500030715     C*
071600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071700030715     C* delimitazione testo
071800030715     C                   EVAL      i = 1
071900030715     C                   DOW       i < %elem(SkSplitFLD)
072000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
072100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072200030715     C                   EVAL      i = i + 1
072300030715     C                   ENDDO
072400030715     C                   EVAL      wGiro = 1
072500040726     C* Ripristino la 1° riga originale
072600040726     C                   EVAL      vinDTA = depvinDTA
072700030715     C                   ELSE
072800030715     C                   EVAL      wGiro = 2
072900031204     C**********
073000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
073100031204     C* d separatore campo
073200031204     C**********
073300060215     C                   EVAL      vindta = %trim(vindta)
073400060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
073500060215     C                   EVAL      vindta = %subst(vindta:2)
073600060215     C                   ENDDO
073700060215     C
073800060215     C                   Z-ADD     *zeros        lunghInput        4 0
073900060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
074000060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074100060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074200060215     C                   EVAL      lunghInput = %len(%trim(vindta))
074300060215     C                   ENDDO
074400030715     C**********
074500030715     C* Effettuo lo split del campo dati d input
074600030715     C**********
074700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074800040324     C                   DOW       posDa <= %len(%trimr(vindta))
074900030715     C*
075000030715     C* Gestisco il 1° campo
075100030715     C                   IF        i = 1
075200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075300030715     C                   EVAL      posDa = 1
075400030715     C* Eseguo lo scan x trovare la fine del primo campo
075500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075600030715     C* A questo punto "estraggo" il campo corrente
075700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
075800030715     C                                                    (posA-posDa))
075900030715     C* X i campi successivi al 1°
076000030715     C                   ELSE
076100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076200030715     C                   EVAL      posDa = posA + 1
076300031204     C* Verifico che nn vi sia il campo nn valorizzato
076400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
076500031204     C* Se campo nn valorizzato skippo al prossimo
076600031204     C                   EVAL      SkSplitCSV(i) = *blanks
076700031204     C                   EVAL      posA  = posA + 1
076800031204     C                   ELSE
076900030715     C* Eseguo lo scan x trovare la fine del campo corrente
077000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077100030715     C* Gestisco l'ultimo campo
077200030715     C                   IF        posA = *zeros
077300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C                   LEAVE
077800030715     C                   ELSE
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C                   ENDIF
078300031204     C                   ENDIF
078400030715     C                   ENDIF
078500030715     C* Incremento il contatore d campo
078600030715     C                   EVAL      i = i +1
078700030715     C                   ENDDO
078800030715     C*
078900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079000030715     C* delimitazione testo
079100030715     C                   EVAL      i = 1
079200030715     C                   DOW       i < %elem(SkSplitCSV)
079300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
079400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
079500030715     C                   EVAL      i = i + 1
079600030715     C                   ENDDO
079700030715     C*
079800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
079900030715     C                   EVAL      i = 1
080000030715     C                   DOW       i < %elem(SkSplitFLD)
080100030715     C*
080200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
080300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
080400030715     C                             SkSplitCSV(i) <> *blanks
080500030721     C*** ATB
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
080700030721     C                   EVAL      VABATB = SkSplitCSV(i)
080800030721     C                   ENDIF
080900030715     C*** RSD
081000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
081300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
081400030715     C* ==
081500030715     C                   ENDIF
081600030721     C*** RD2
081700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
081800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
081900030721     C                   ENDIF
082000030715     C*** IND
082100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082200030715     C                   EVAL      VABIND = SkSplitCSV(i)
082300030715     C                   ENDIF
082400030715     C*** LOD
082500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
082600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
082700030715     C                   ENDIF
082800030715     C*** PRD
082900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083100030715     C                   ENDIF
083200030721     C*** NZD
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
083400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030715     C*** RMA
083700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
083800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
083900030715     C                   ENDIF
084000030715     C*** NOT
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084200060306     C                   EVAL      VABNOT = SkSplitCSV(i)
084300030715     C                   ENDIF
084400030715     C*** NT2
084500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
084600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
084700030715     C                   ENDIF
084800030715     C*** VCA
084900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085100030715     C                   ENDIF
085200030715     C*** VAS
085300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
085400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
085500030715     C                   ENDIF
085600030715     C*** FFD
085700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
085800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
085900030715     C                   ENDIF
086000030715     C*** CBO
086100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086200030715     C                   EVAL      VABCBO = SkSplitCSV(i)
086300030715     C                   ENDIF
086400030721     C*** GC1
086500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
086600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
086700030721     C                   ENDIF
086800030721     C*** GC2
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
087000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030721     C*** TSP
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
087400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030721     C*** NAS
087700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
087800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
087900030721     C                   ENDIF
088000030721     C*** TIC
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** GCA
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
088600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** XCO
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
089000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** CTM
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
089400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** TCR
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
089800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** CTS
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** FTM
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
090600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*** VAD
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
091000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030721     C*** GMA
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
091400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** GGA
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
091800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** GVA
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** TC1
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
092600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** TC2
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
093000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** SCL
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
093400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** RMO
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
093800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** NMO
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*
094500030721     C* Reperisco quindi i campi numerici...
094600030721     C*** CAD
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
094800030721     C                   EVAL      PiStr=SkSplitCSV(i)
094900030721     C                   EXSR      CHKNUM
095000030721     C                   IF        PiInt=*on
095100030721     C                   Z-ADD     PiVal         Num5_0
095200030721     C                   MOVEL(p)  Num5_0        VABCAD
095300030721     C                   ELSE
095400030721     C                   SETON                                        32
095500030721     C                   EVAL      VABCAD = *zeros
095600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095700030721     C                             + ' ' + 'VABCAD'
095800030721     C                   ENDIF
095900030721     C                   ENDIF
096000030721     C*** RMN
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096200030721     C                   EVAL      PiStr=SkSplitCSV(i)
096300030721     C                   EXSR      CHKNUM
096400030721     C                   IF        PiInt=*on
096500030721     C                   Z-ADD     PiVal         VABRMN
096600030721     C                   ELSE
096700030721     C                   SETON                                        32
096800030721     C                   EVAL      VABRMN = 1
096900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097000030721     C                             + ' ' + 'VABRMN'
097100030721     C                   ENDIF
097200030721     C                   ENDIF
097300030721     C*** NSP
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097500030721     C                   EVAL      PiStr=SkSplitCSV(i)
097600030721     C                   EXSR      CHKNUM
097700040726     C                   IF        PiInt=*on
097800040726     C                   Z-ADD     PiVal         VABNSP
097900040802     C*                  IF        PiVal>9999999
098000040802     C*                  SETON                                        32
098100040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
098200040802     C*                            + ' ' + 'VABNSP'
098300040802     C*                  ENDIF
098400030721     C                   ELSE
098500030721     C                   SETON                                        32
098600030721     C                   EVAL      VABNSP = *zeros
098700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098800030721     C                             + ' ' + 'VABNSP'
098900030721     C                   ENDIF
099000030721     C                   ENDIF
099100030721     C*** NCL
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099300030721     C                   EVAL      PiStr=SkSplitCSV(i)
099400030721     C                   EXSR      CHKNUM
099500030721     C                   IF        PiInt=*on
099600030721     C                   Z-ADD     PiVal         VABNCL
099700030721     C                   ELSE
099800030721     C                   SETON                                        32
099900030721     C                   EVAL      VABNCL = *zeros
100000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100100030721     C                             + ' ' + 'VABNCL'
100200030721     C                   ENDIF
100300030721     C                   ENDIF
100400030721     C*** PKB
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100600030721     C                   EVAL      PiStr=SkSplitCSV(i)
100700030721     C                   EXSR      CHKNUM
100800030721     C                   IF        PiNum=*on
100900030721     C                   Z-ADD     PiVal         VABPKB
101000030721     C                   ELSE
101100030721     C                   SETON                                        32
101200030721     C                   EVAL      VABPKB = *zeros
101300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101400030721     C                             + ' ' + 'VABPKB'
101500030721     C                   ENDIF
101600030721     C                   ENDIF
101700030721     C*** CAS
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
101900030721     C                   EVAL      PiStr=SkSplitCSV(i)
102000030721     C                   EXSR      CHKNUM
102100030721     C                   IF        PiNum=*on
102200030721     C                   Z-ADD     PiVal         VABCAS
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABCAS = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700030721     C                             + ' ' + 'VABCAS'
102800030721     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C*** CCM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103200030721     C                   EVAL      PiStr=SkSplitCSV(i)
103300030721     C                   EXSR      CHKNUM
103400030721     C                   IF        PiInt=*on      AND
103500030721     C                             PiVal<=9999999 AND
103600030721     C                             PiVal>*zeros
103700030721     C                   Z-ADD     PiVal         VABCCM
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      VABCCM = *zeros
104100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104200030721     C                             + ' ' + 'VABCCM'
104300030721     C                   ENDIF
104400030721     C                   ENDIF
104500030721     C*** LNP
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
104700030721     C                   EVAL      PiStr=SkSplitCSV(i)
104800030721     C                   EXSR      CHKNUM
104900030721     C                   IF        PiInt=*on    AND
105000030721     C                             PiVal<=999   AND
105100030721     C                             PiVal>*zeros
105200030721     C                   Z-ADD     PiVal         VABLNP
105300030721     C                   ELSE
105400030721     C                   SETON                                        32
105500030721     C                   EVAL      VABLNP = *zeros
105600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105700030721     C                             + ' ' + 'VABLNP'
105800030721     C                   ENDIF
105900030721     C                   ENDIF
106000030721     C*** NRS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106200030721     C                   EVAL      PiStr=SkSplitCSV(i)
106300030721     C                   EXSR      CHKNUM
106400030721     C                   IF        PiInt=*on AND
106500030721     C                             PiVal<=99
106600030721     C                   Z-ADD     PiVal         VABNRS
106700030721     C                   ELSE
106800030721     C                   SETON                                        32
106900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107000030721     C                             + ' ' + 'VABNRS'
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** LNA
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on    AND
107800030721     C                             PiVal<=999   AND
107900030721     C                             PiVal>*zeros
108000030721     C                   Z-ADD     PiVal         VABLNA
108100030721     C                   ELSE
108200030721     C                   SETON                                        32
108300030721     C                   EVAL      VABLNA = *zeros
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500030721     C                             + ' ' + 'VABLNA'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** CTR
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiInt=*on  AND
109300030721     C                             PiVal<=999
109400030721     C                   Z-ADD     PiVal         VABCTR
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800030721     C                             + ' ' + 'VABCTR'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** IAS
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiNum=*on
110600030721     C                   Z-ADD     PiVal         VABIAS
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABIAS'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** VLB
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiNum=*on
111800030721     C                   Z-ADD     PiVal         VABVLB
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABVLB'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** QFT
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiNum=*on
113000030721     C                   Z-ADD     PiVal         VABQFT
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABQFT'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** NCD
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiInt=*on      AND
114200030721     C                             PiVal<=9999999
114300030721     C                   Z-ADD     PiVal         VABNCD
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABNCD'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** NCA
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on      AND
115500030721     C                             PiVal<=9999999
115600030721     C                   Z-ADD     PiVal         VABNCA
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABNCA'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** ZNC
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiInt=*on AND
116800030721     C                             PiVal<=99
116900030721     C                   Z-ADD     PiVal         VABZNC
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABZNC'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** DCR
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on       AND
118100030721     C                             PiVal<=99999999
118200030721     C                   Z-ADD     PiVal         VABDCR
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABDCR'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** HCR
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on   AND
119400030721     C                             PiVal<=9999
119500030721     C                   Z-ADD     PiVal         VABHCR
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABHCR'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** VMD
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiNum=*on
120700030721     C                   Z-ADD     PiVal         VABVMD
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABVMD'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** ANT
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on        AND
121900030721     C                             PiVal<=999999999
122000030721     C                   Z-ADD     PiVal         VABANT
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABANT'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** CMO
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on
123200030721     C                   Z-ADD     PiVal         Num5_0
123300030721     C                   MOVEL(p)  Num5_0        VABCMO
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABCMO = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABCMO'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100060328     C*** DEST1
124200060328     C                   IF        %trim(SkSplitFLD(i)) = 'DEST1'
124300060328     C                   EVAL      VABRSD = SkSplitCSV(i)
124400060328     C                   ENDIF
124500060328     C*** DEST2
124600060328     C                   IF        %trim(SkSplitFLD(i)) = 'DEST2'
124700060328     C                   EVAL      VABIND = SkSplitCSV(i)
124800060328     C                   ENDIF
124900060328     C*** DEST3
125000060328     C                   IF        %trim(SkSplitFLD(i)) = 'DEST3'
125100060328     C                   EVAL      VABLOD = SkSplitCSV(i)
125200060328     C                   ENDIF
125300030715     C*
125400030715     C                   ENDIF
125500030715     C                   EVAL      i = i + 1
125600030715     C                   ENDDO
125700040726     C*
125800040726     C* Considerazioni personalizzate x il traduttore corrente
125900060328     C                   IF        VABNOT <> *blanks
126000060328     C                   EVAL      VABCBO = *blanks
126100060328     C                   ENDIF
126200060328     C*
126300060306     C* Reperisco la provincia dal CAP e dalla località
126400060306     C                   IF        VABPRD = *blanks AND
126500060306     C                             VABCAD <> *blanks
126600060306     C                   CLEAR                   TISI95DS
126700060306     C                   EVAL      I95TCN = '3'
126800060306     C                   Z-ADD     datcor        I95DAT
126900060306     C                   EVAL      I95CAP = VABCAD
127000060306     C                   EVAL      I95LOC = VABLOD
127100060306     C                   CALL      'TISI95R'
127200060306     C                   PARM                    TISI95DS
127300060306     C                   EVAL      VABPRD = O95PRV
127400060306     C                   ENDIF
127500040726     C*
127600040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127700060328     C                   IF        VABNSP =  *zeros
127800060328     C                   IF        VABRMN <> *zeros
127900060328     C                   Z-ADD     VABRMN        VABNSP
128000060328     C                   ELSE
128100040726     C                   SETON                                        31
128200060328     C                   ENDIF
128300040726     C                   ENDIF
128400040726     C*
128500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
128600040726     C                   IF        VABLNP = *zeros
128700040726     C                   SETON                                        31
128800040726     C                   ENDIF
128900040726     C*
129000040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
129100040726     C* d valorizzarli sempre entrambi
129200040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
129300040726     C                   Z-ADD     VABNSP        VABRMN
129400040726     C                   ENDIF
129500040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
129600040726     C                   MOVEL     VABRMN        VABRMA
129700040726     C                   ENDIF
129800040726     C*
129900040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
130000040726     C                   IF        VABCAS > *zeros
130100040726     C                   IF        VABVCA = *blanks
130200060328     C                   EVAL      VABVCA = 'EUR'
130300040726     C                   ENDIF
130400040726     C                   IF        VABCBO = '1'
130500040726     C                   EVAL      VABCBO = '4'
130600040726     C                   ENDIF
130700040726     C                   IF        VABCBO = '2'
130800040726     C                   EVAL      VABCBO = '6'
130900040726     C                   ENDIF
131000040726     C                   ENDIF
131100020204     C*
131200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
131300020204     C                   EXSR      CHKIMPDIV
131400030715     C                   ENDIF
131500020204     C*
131600010607     C                   ENDSR
131700010607     C*----------------------------------------------------*
131800010601
131900020204
132000020204     C*----------------------------------------------------*
132100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
132200020204     C*----------------------------------------------------*
132300020204     C     CHKIMPDIV     BEGSR
132400020204     C*
132500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
132600020204     C                   Z-ADD     *zeros        wrkDec            9 9
132700020204     C*
132800020204     C* Come prima cosa effettuo considerazioni sulla divisa
132900020204     C                   IF        vabIAS > *zeros
133000020204     C                   IF        vabVAS <> 'EUR'
133100020204     C                   EVAL      vabVAS =  'ITL'
133200020204     C                   ENDIF
133300020204     C                   ENDIF
133400020204     C*
133500020204     C                   IF        vabCAS > *zeros
133600020204     C                   IF        vabVCA <> 'EUR'
133700020204     C                   EVAL      vabVCA =  'ITL'
133800020204     C                   ENDIF
133900020204     C                   ENDIF
134000020204     C*
134100020204     C                   IF        vabVMD > *zeros
134200020204     C                   IF        vabVAD <> 'EUR'
134300020204     C                   EVAL      vabVAD =  'ITL'
134400020204     C                   ENDIF
134500020204     C                   ENDIF
134600020204     C*
134700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
134800020204     C                   Z-ADD     vabIAS        wrkDec
134900020204     C                   IF        wrkDec > *zeros
135000020204     C                   IF        vabVAS = 'ITL'
135100020204     C                   EVAL      vabIAS = *zeros
135200020204     C                   ENDIF
135300020204     C                   ENDIF
135400020204     C*
135500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
135600020204     C                   Z-ADD     vabCAS        wrkDec
135700020204     C                   IF        wrkDec > *zeros
135800020204     C                   IF        vabVCA = 'ITL'
135900020204     C                   EVAL      vabCAS = *zeros
136000020204     C                   ENDIF
136100020204     C                   ENDIF
136200020204     C*
136300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
136400020204     C                   Z-ADD     vabVMD        wrkDec
136500020204     C                   IF        wrkDec > *zeros
136600020204     C                   IF        vabVAD = 'ITL'
136700020204     C                   EVAL      vabVMD = *zeros
136800020204     C                   ENDIF
136900020204     C                   ENDIF
137000020204     C*
137100020204     C                   ENDSR
137200020204     C***
137300020204
137400010330
137500010330
137600010330     C*----------------------------------------------------*
137700010330     C*  CONTROLLO NUMERICITA' CAMPI
137800010330     C*----------------------------------------------------*
137900010330     C     CHKNUM        BEGSR
138000010330     C*
138100010606     C                   IF        PiDecChr = *blanks
138200030715     C                   EVAL      PiDecChr = CharNUM
138300010606     C                   ENDIF
138400010606     C*
138500010606     C                   CALL(e)   'ISNUMERIC'
138600010330     C                   PARM                    PiStr            30
138700010606     C                   PARM                    PiDecChr          1
138800010330     C                   PARM      *ZEROS        PiVal            30 9
138900010330     C                   PARM      '0'           PiInt             1
139000010330     C                   PARM      '0'           PiNum             1
139100010330     C                   IF        %error
139200010606     C                   EVAL      PiNum=*off
139300010330     C                   ENDIF
139400010330     C*
139500010330     C                   ENDSR
139600010330     C***
139700010330
139800010601
139900010601
140000010601
140100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
140200010601     C     repfil        BEGSR
140300010601     C*
140400010601     C                   if        invfil = *zeros and
140500010601     C                             depfil > *zeros and
140600010629     C                             (vinflg = *blanks or
140700010629     C                              vinflg = *zeros)
140800010601     C
140900010601     C                   eval      invfil = depfil
141000010601     C                   endif
141100010601     C*
141200010601     C                   if        depfil <> invfil and
141300010601     C                             invfil > *zeros
141400010601     C                   eval      flgMulti = '1'
141500010601     C                   if        vinflg = *blanks
141600010601     C                   add       1             cntNonEl
141700010601     C                   endif
141800010601     C                   endif
141900010601     C*
142000010601     C                   if        vinflg = '2'
142100010601     C                   eval      flgStato = '2'
142200010601     C                   endif
142300010601     C*
142400010601     C                   ENDSR
142500010601     C***
142600010601
142700010330
142800010330
142900010330
143000990920      /TITLE Invio dei dati al punto operativo.
143100000613     C     invio         BEGSR
143200990920     C*
143300990920     C                   reset                   dscmz
143400010601     C                   move      invfil        cmzdst
143500990920     C                   eval      cmzfld = vlrfou
143600990920     C                   eval      cmzmbd = vlrhdl
143700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
143800000710     C                   if        prmfir = *blanks
143900021025     C                   eval      cmzfla = 'FIVAB00F'
144000021025     C                   eval      cmzmba = 'FIVAB00F'
144100000710     C                   else
144200000710     C                   eval      cmzfla = prmfir
144300000710     C                   eval      cmzmba = prmfir
144400000710     C                   endif
144500990920     C                   eval      cmznrr = *zeros
144600990920     C                   move      §ctrok        cmznrr
144700021018     C                   eval      cmzlba = vlrfl1
144800990920     C                   call(e)   'TIS711C'
144900990920     C                   parm                    dscmz
145000990921     C                   parm      *blanks       esito
145100990920     C                   if        %error
145200990920     C                             or cmzerr = '1'
145300990921     C                             or esito  = '1'
145400000710     C                   eval      wrkesito = '3'
145500990920     C                   endif
145600990920     C*
145700000613     C                   ENDSR
145800990910
145900010601
146000010601
146100010601
146200010601
146300010601      /TITLE Invio dei dati al punto operativo.
146400010601     C     opeini        BEGSR
146500010601     C*
146600010601     C* Inizializzo flag e contatori operativi
146700010601     C                   movel     '0'           flgGiro           1
146800010601     C                   movel     '0'           flgMulti          1
146900010601     C                   movel     '1'           flgStato          1
147000010615     C                   movel     '0'           flgOk             1
147100010601     C                   z-add     *zeros        cntNonEl         10 0
147200010601     C                   z-add     *zeros        depfil            3 0
147300010601     C                   z-add     *zeros        invfil            3 0
147400010601     C*
147500010601     C                   ENDSR
147600010601     C***
147700010601
147800010601
147900050421
148000050421
148100050421     C     *pssr         BEGSR
148200050421     C*
148300050421     C                   eval      wrkesito = '2'
148400050421     C*
148500050421     C                   ENDSR     '*CANCL'
148600050421     C***
148700050421
148800010601
148900010330
149000010330
149100000613     C     *inzsr        BEGSR
149200990910     C*
149300990910     C     *entry        plist
149400990920     C                   parm                    tivlrds
149500990921     C                   parm      wrkesito      esito
149600000724     C                   parm                    prmlit
149700000710     C                   parm                    prmfir
149800010330     C*
149900010330     C* CALCOLA LA DATA CORRENTE
150000010330     C                   time                    wn14             14 0
150100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
150200010330     C                   z-add     wn8           g08dat
150300010330     C                   z-add     *zeros        g08inv
150400010330     C                   movel     '0'           g08err
150500010330     C                   call      'XSRDA8'
150600010330     C                   parm                    wlbda8
150700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
150800000613     C*
150900000613     C                   ENDSR
151000000613     C***
151100990908
151200030715     Otitvxlp   E            riepilogo         2
151300990915     O                                              'Upload via Internet'
151400990915     O                                           +1 'Traduzione TIVIN00R -'
151500021025     O                                           +1 'FIVABWWR'
151600990915     O                                           +1 'Testate bolle'
151700990915     O          E            riepilogo   2
151800990915     O                       wrkdata
151900990915     O                       wrkora              +1
152000990915     O                       procname            +1
152100990915     O          E            riepilogo   2
152200990915     O                                              'Cliente..................:'
152300010605     O                       VABCCM        z     +1
152400990915     O          E            riepilogo   2
152500990920     O                                              'Riferimento Strategi.....:'
152600990920     O                       vlrhdl              +1
152700990915     O          E            riepilogo   2
152800990915     O                                              'Giusti...................:'
152900971022     O                       §CTROK        2   +  1
153000990915     O          E            riepilogo   2
153100990915     O                                              'Sbagliati e corretti.....:'
153200971022     O                       §CTRMO        2   +  1
153300990915     O          E            riepilogo   2
153400990915     O                                              'Sbagliati e scartati.....:'
153500971022     O                       §CTRNO        2   +  1
153600000613
153700030715     Otitvxlps  E            testdett          2
153800000613     O                                              'Upload via Internet'
153900000613     O                                           +1 'Traduzione TIVIN00R -'
154000021025     O                                           +1 'FIVABWWR'
154100000613     O                                           +1 'Testate bolle'
154200000616     O          E            testdett    3
154300000613     O                                           +2 'N° rec'
154400000613     O                                           +3 'Anteprima contenuto'
154500000616     O          E            rigadett    2
154600000613     O                       rrnum               +2
154700000621     O                       recko               +3
154800000616     O          E            findett     2
154900000613     O                       wrkdata
155000000613     O                       wrkora              +1
155100000613     O                       procname            +1
155200000616     O          E            findett     2
155300000613     O                                              'Cliente..................:'
155400010605     O                       VABCCM        z     +1
155500000616     O          E            findett     2
155600000613     O                                              'Riferimento Strategi.....:'
155700000613     O                       vlrhdl              +1
155800000616     O          E            findett     2
155900000613     O                                              'Giusti...................:'
156000000613     O                       §CTROK        2   +  1
156100000616     O          E            findett     2
156200000613     O                                              'Sbagliati e corretti.....:'
156300000613     O                       §CTRMO        2   +  1
156400000616     O          E            findett     2
156500000613     O                                              'Sbagliati e scartati.....:'
156600000613     O                       §CTRNO        2   +  1
156700000613** CMD - COMANDI CL
156800030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
156900030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
157000030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
157100010606
157200000613
157300040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
157400060328VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABCTR;VABRMA;
157500060328VABRMN;VABNOT;DEST1;DEST2;DEST3;DEST4
157600050321
157700050321
157800060215
