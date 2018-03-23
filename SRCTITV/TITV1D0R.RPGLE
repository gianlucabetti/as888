000100071211      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente ARIANNA 2001 Cod. 0634308)
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
006500071212     D*------------------
006600071212     D* DS REPERIMENTO NUMERATORE
006700071212     D*------------------
006800071212     D trul33ds      e ds                  inz
006900071212     D*------------------
007000071212     D* DS ARCHITETTURA
007100071212     D*------------------
007200071212     D kpjba         e ds                  inz
007300071211     D*------------------
007400071211     D*
007500071211     D wDS_RMO         ds
007600071211     D  wDS_RMO_RICEV                10    inz(*blanks)
007700071211     D  wDS_RMO_FILE                 15    inz(*blanks)
007800071211     D*
007900031201     D*------------------
008000031201     D* Costanti
008100031201     D*------------------
008200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008400010330
008500010330
008600010330
008700990915     C                   time                    wrkdata
008800990915     C                   time                    wrkora
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200000724     C*
011300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500010601     C     invfil        chain     azorg01l
011600000724     C                   if        %found
011700000724     C                   movel(p)  CMD(1)        depcmd
011800020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011900000724     C*
012000000724     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012100011113     C                   Z-ADD     150           LENGH            15 5
012200000724     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000724     C                   PARM                    depcmd
012400000724     C                   PARM                    LENGH
012500000724     C*
012600000724     C                   endif
012700000616     C*
012800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900000616     C                   movel(p)  CMD(2)        depcmd
013000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013100000616     C*
013200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013300011113     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    depcmd
013600000616     C                   PARM                    LENGH
013700000613     C*
013800030715     C                   if        not %open(titvxlps)
013900030715     C                   open      titvxlps
014000030715     C                   except    testdett
014100000613     C                   endif
014200010601     C*
014300010601     C                   endif
014400010601     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601     C*--------------------------------------------------------
015100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015200010601     C*--------------------------------------------------------
015300010601     C     ENDELA        BEGSR
015400990915     C*
015500000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015600060306     C*
015700060306     C* Effettuo lancio TISI95 solo x chiusura
015800060306     C                   CLEAR                   TISI95DS
015900060306     C                   EVAL      I95TLA = 'C'
016000060306     C                   CALL      'TISI95R'
016100060306     C                   PARM                    TISI95DS
016200000613     C*
016300030715     C                   if        %open(titvxlps)
016400000616     C                   except    findett
016500030715     C                   close     titvxlps
016600000613     C                   endif
016700000616     C*
016800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016900011113     C                   Z-ADD     150           LENGH            15 5
017000010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017100000616     C                   PARM                    CMD(3)
017200000616     C                   PARM                    LENGH
017300000616     C*
017400010601     C                   ENDSR
017500010601     C***
017600010601
017700010601
017800000613
017900000613     C*--------------------------------------------------------
018000000621     C* STPR  -  STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018100000613     C*--------------------------------------------------------
018200000621     C     STPR          BEGSR
018300000613     C*
018400030715     C                   if        not %open(titvxlp)
018500030715     C   50              open      titvxlp
018600990915     C                   endif
018700990915     C*
018800030715     C   50              except    riepilogo
018900990915     C*
019000030715     C                   if        %open(titvxlp)
019100030715     C   50              close     titvxlp
019200990914     C                   endif
019300000613     C*
019400000613     C                   ENDSR
019500000613     C***
019600010601
019700010601
019800010601
019900010330     C*--------------------------------------------------------
020000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
020100010330     C*--------------------------------------------------------
020200010605     C     RWVAB         BEGSR
020300010330     C*
020400010330     C                   if        not %open(tivin00r)
020500010330     C                   open      tivin00r
020600010330     C                   endif
020700021025     C                   if        not %open(fivabwwr)
020800021025     C                   open      fivabwwr
020900010330     C                   endif
021000010330     C*
021100010604     C                   clear                   §CTROK
021200010604     C                   clear                   §CTRMO
021300010604     C                   clear                   §CTRNO
021400040726     C*
021500040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
021600040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
021700071212     C****               exsr      inzvar
021800071212     C****               exsr      impvab
021900010330     C*
022000030822     C                   do        *HIVAL
022100010330     C*
022200030822     C                   read      tivin00r                               70
022300010618     C*
022400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022500010618     C                   if        vinflg = '1'
022600010618     C                   eval      flgOk = '1'
022700010618     C                   endif
022800060216     C*
022900060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
023000060216     C* => gestisco sempre
023100060216     C                   z-add     *zeros        wLenVINDTA        4 0
023200060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
023300060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
023400060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
023500060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
023600060216     C                   endif
023700060216     C                   enddo
023800010618     C*
023900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
024000040510     C                   z-add     *zeros        wLenVINDTA        4 0
024100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
024200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
024300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
024400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
024500040510     C                   z-add     1             wFlgVINDTA
024600040510     C                   leave
024700040510     C                   endif
024800040510     C                   enddo
024900040510     C*
025000040218     C                   if        vindta > *blanks AND
025100040510     C                             wFlgVINDTA = 1
025200010330     C                   add       1             rrnum
025300010330     C*
025400010601     C                   if        *in70 = *off and
025500010330     C                             (vinflg = *blanks
025600010330     C                              or vinflg = '0'
025700010330     C                              or vinflg = '2')
025800010330     C*
025900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
026000010711     C                   if        vinflg = *blanks or vinflg = '0'
026100010711     C                   clear                   vinmsg
026200010711     C                   endif
026300010601     C*
026400010330     C                   exsr      inzvar
026500010605     C                   exsr      impvab
026600010601     C*
026700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026800010605     C                   eval      depfil = VABLNP
026900010601     C                   exsr      repfil
027000010601     C                   if        depfil = invfil
027100021025     C                   if        vlrpoi = 999
027200030822     C                   move(P)   invfil        VABFGS
027300021025     C                   else
027400030822     C                   move(P)   vlrpoi        VABFGS
027500021025     C                   endif
027600010601     C*
027700010601     C                   exsr      PREELA
027800010601     C*
027900010604     C* Ebbene...
028000010604     C*
028100030822     C  N31              add       1             §CTROK            7 0
028200030822     C   32              add       1             §CTRMO            7 0
028300030822     C   31              add       1             §CTRNO            7 0
028400030822     C                   if        wGiro = 2
028500030822     C  N31              write     FIVAB000
028600030822     C                   endif
028700010604     C*
028800010604     C                   if        *in31 = *off and
028900010604     C                             *in32 = *off
029000010604     C                   eval      vinflg = '1'
029100010604     C                   else
029200010604     C                   eval      recko = vindta
029300010604     C                   except    rigadett
029400010604     C                   eval      vinflg = '2'
029500010604     C                   endif
029600010604     C                   endif
029700010601     C*
029800010604     C                   endif
029900010604     C*
030000010330     C                   else
030100010330     C                   eval      vinflg = '1'
030200010330     C                   endif
030300010601     C*
030400010601     C  N70              update    tivin000
030500010330     C*
030600030822     C  N70              enddo
030700010601     C
030800010601     C*
030900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
031000010601     C                   if        cntNonEl = *zeros or
031100010601     C                             flgMulti = '0'
031200010330     C* Se non ci sono record con errori ...
031300010601     C                   if        §ctrno = 0 and
031400010604     C                             §ctrmo = 0 and
031500010601     C                             flgStato <> '2'
031600010330     C* ... restituisco esito OK.
031700010330     C                   eval      wrkesito = '0'
031800010330     C                   else
031900010330     C                   if        §ctrok > 0
032000010330     C                   eval      wrkesito = '1'
032100010330     C                   else
032200010615     C                   if        flgOk = '0'
032300010615     C                   eval      wrkesito = '2'
032400010615     C                   else
032500010615     C                   eval      wrkesito = '6'
032600010615     C                   endif
032700010330     C                   endif
032800010330     C                   endif
032900010601     C                   else
033000010601     C                   eval      wrkesito = '9'
033100010601     C                   endif
033200010330     C*
033300010330     C                   if        %open(tivin00r)
033400010330     C                   close     tivin00r
033500010330     C                   endif
033600021025     C                   if        %open(fivabwwr)
033700021025     C                   close     fivabwwr
033800010330     C                   endif
033900010601     C*
034000010601     C                   if        vlrpoi <> 999
034100010601     C                   eval      invfil = vlrpoi
034200010601     C                   endif
034300010330     C*
034400010330     C                   if        §ctrok > 0
034500010601     C                             and invfil > *zeros
034600010330     C                   exsr      invio
034700010330     C                   endif
034800010601     C*
034900010618     C                   if        flgGiro = '1'
035000010601     C                   exsr      endela
035100010618     C                   endif
035200010330     C*
035300010330     C                   ENDSR
035400010330     C***
035500010601
035600010601
035700010601
035800010330     C*----------------------------------------------------*
035900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036000010330     C*----------------------------------------------------*
036100010330     C     INZVAR        BEGSR
036200010330     C*
036300030822     C* Inizializzo variabili di wrk
036400020204     C                   Z-ADD     *zeros        Num5_0            5 0
036500030822     C*
036600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
036700030822     C                   CLEAR                   FIVAB000
036800030822     C                   CLEAR                   SkSplitCSV
036900030822     C                   EVAL      i = 1
037000030822     C                   EVAL      posDa = *zeros
037100030822     C                   EVAL      posA  = *zeros
037200030822     C*
037300030822     C* Reimposto i valori di default
037400030822     C                   EXSR      DEFCAM
037500030822     C*
037600010330     C                   ENDSR
037700010330     C*----------------------------------------------------*
037800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
037900010330     C*----------------------------------------------------*
038000010330     C     DEFCAM        BEGSR
038100030822     C*
038200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
038300030715     C* e delimitatore testo.
038400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
038500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
038600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
038700030822     C*
038800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
038900030715     C                   IF        %subst(vlrppt:1:1) = 'S'
039000030715     C                   SETON                                        50
039100030715     C                   ELSE
039200030715     C                   SETOFF                                       50
039300030715     C                   ENDIF
039400030822     C*
039500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
039600030822     C                   EVAL      posDaDft = 1
039700030822     C                   EVAL      posADft  = 0
039800030822     C                   EVAL      wGiroDft = 0
039900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
040000030822     C                             posDaDft > 0
040100030822     C*
040200030822     C* Gestisco il 1° giro
040300030822     C                   IF        wGiroDft = 0
040400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
040500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
040600030822     C* Incremento il contatore dei "giri"
040700030822     C                   EVAL      wGiroDft = 1
040800030822     C                   ELSE
040900030822     C                   EVAL      posDaDft = posADft
041000030822     C                   ENDIF
041100030822     C* Eseguo lo scan x trovare la fine del campo corrente
041200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
041300030822     C*
041400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
041500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
041600030822     C                   IF        posDaDft > 0 AND
041700030822     C                             posADft  > 0
041800030822     C* NCL
041900030822     C                   IF        %subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):1:3)
042200030822     C                             = 'NCL'
042300030822     C                   EVAL      PiStr=%trim(%subst(
042400030822     C                             %subst(vlrppt:posDaDft+1:
042500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042600030822     C                   EXSR      CHKNUM
042700030822     C                   IF        PiInt=*on
042800030822     C                   Z-ADD     PiVal         VABNCL
042900030822     C                   ENDIF
043000030822     C                   ENDIF
043100030822     C* CCM
043200030822     C                   IF        %subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):1:3)
043500030822     C                             = 'CCM'
043600030822     C                   EVAL      PiStr=%trim(%subst(
043700030822     C                             %subst(vlrppt:posDaDft+1:
043800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043900030822     C                   EXSR      CHKNUM
044000030822     C                   IF        PiInt=*on
044100030822     C                   Z-ADD     PiVal         VABCCM
044200030822     C                   ENDIF
044300030822     C                   ENDIF
044400030822     C* LNP
044500030822     C                   IF        %subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):1:3)
044800030822     C                             = 'LNP'
044900030822     C                   EVAL      PiStr=%trim(%subst(
045000030822     C                             %subst(vlrppt:posDaDft+1:
045100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045200030822     C                   EXSR      CHKNUM
045300030822     C                   IF        PiInt=*on
045400030822     C                   Z-ADD     PiVal         VABLNP
045500030822     C                   ENDIF
045600030822     C                   ENDIF
045700030822     C* NRS
045800030822     C                   IF        %subst(
045900030822     C                             %subst(vlrppt:posDaDft+1:
046000030822     C                             posADft-posDaDft-1):1:3)
046100030822     C                             = 'NRS'
046200030822     C                   EVAL      PiStr=%trim(%subst(
046300030822     C                             %subst(vlrppt:posDaDft+1:
046400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046500030822     C                   EXSR      CHKNUM
046600030822     C                   IF        PiInt=*on
046700030822     C                   Z-ADD     PiVal         VABNRS
046800030822     C                   ENDIF
046900030822     C                   ENDIF
047000030822     C* CTR
047100030822     C                   IF        %subst(
047200030822     C                             %subst(vlrppt:posDaDft+1:
047300030822     C                             posADft-posDaDft-1):1:3)
047400030822     C                             = 'CTR'
047500030822     C                   EVAL      PiStr=%trim(%subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047800030822     C                   EXSR      CHKNUM
047900030822     C                   IF        PiInt=*on
048000030822     C                   Z-ADD     PiVal         VABCTR
048100030822     C                   ENDIF
048200030822     C                   ENDIF
048300030822     C* PKB
048400030822     C                   IF        %subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):1:3)
048700030822     C                             = 'PKB'
048800030822     C                   EVAL      PiStr=%trim(%subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049100030822     C                   EXSR      CHKNUM
049200030822     C                   IF        PiNum=*on
049300030822     C                   Z-ADD     PiVal         VABPKB
049400030822     C                   ENDIF
049500030822     C                   ENDIF
049600030822     C* VLB
049700030822     C                   IF        %subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):1:3)
050000030822     C                             = 'VLB'
050100030822     C                   EVAL      PiStr=%trim(%subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050400030822     C                   EXSR      CHKNUM
050500030822     C                   IF        PiNum=*on
050600030822     C                   Z-ADD     PiVal         VABVLB
050700030822     C                   ENDIF
050800030822     C                   ENDIF
050900030822     C* QFT
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'QFT'
051400030822     C                   EVAL      PiStr=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   EXSR      CHKNUM
051800030822     C                   IF        PiNum=*on
051900030822     C                   Z-ADD     PiVal         VABQFT
052000030822     C                   ENDIF
052100030822     C                   ENDIF
052200030822     C* CBO
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'CBO'
052700030822     C                   EVAL      VABCBO=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   ENDIF
053100030822     C* TSP
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'TSP'
053600030822     C                   EVAL      VABTSP=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   ENDIF
054000030822     C* VAS
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'VAS'
054500030822     C                   EVAL      VABVAS=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   ENDIF
054900030822     C* VCA
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'VCA'
055400030822     C                   EVAL      VABVCA=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   ENDIF
055800030822     C* GCA
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'GCA'
056300030822     C                   EVAL      VABGCA=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* CTM
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'CTM'
057200030822     C                   EVAL      VABCTM=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* FFD
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'FFD'
058100030822     C                   EVAL      VABFFD=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* VAD
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'VAD'
059000030822     C                   EVAL      VABVAD=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* GMA
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'GMA'
059900030822     C                   EVAL      VABGMA=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* GGA
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'GGA'
060800030822     C                   EVAL      VABGGA=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* GVA
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'GVA'
061700030822     C                   EVAL      VABGVA=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* TC1
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'TC1'
062600030822     C                   EVAL      VABTC1=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* TC2
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'TC2'
063500030822     C                   EVAL      VABTC2=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* ...
064000030822     C                   ENDIF
064100030822     C                   ENDDO
064200020204     C*
064300010330     C                   ENDSR
064400010607     C*----------------------------------------------------*
064500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
064600010607     C*----------------------------------------------------*
064700010607     C     IMPVAB        BEGSR
064800010607     C*
064900010607     C                   SETOFF                                       3132
065000030515     C*
065100010607     C                   MOVEL     datcor        VABAAS
065200010607     C                   MOVE      datcor        VABMGS
065300050321     C*
065400050512     C* 1° Giro x eliminare il separatore testo
065500060215     C                   IF        CharTXT <> *blanks
065600050321     C                   Z-ADD     1             wj                5 0
065700050321     C                   DOW       wj <= %len(vindta) - 1
065800050321     C                   IF        %subst(vindta:wj:1) = CharTXT
065900050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
066000050512     C                                      %subst(vindta:wj+1)
066100050512     C                   EVAL      wj = wj - 1
066200050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
066300050321     C                   ENDIF
066400050321     C                   ADD       1             wj
066500050321     C                   ENDDO
066600060215     C                   ENDIF
066700050512     C*
066800060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
066900050512     C* 2 byte a blank
067000050512     C                   IF        %subst(vindta:1:1) = CharCSV
067100050512     C                   EVAL      vindta = '  ' + vindta
067200050512     C                   ENDIF
067300010607     C*
067400030715     C**********
067500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
067600030715     C**********
067700030715     C                   IF        wGiro = *zeros
067800040726     C*
067900040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
068000040726     C                   EVAL      depvinDTA = vinDTA
068100040726     C*
068200040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
068300040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
068400050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
068500060215     C                                      %trim(intcol(5))
068600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
068700031201     C     minu:maiu     XLATE     vindta        vindta
068800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068900040324     C                   DOW       posDa <= %len(%trimr(vindta))
069000030715     C*
069100030715     C* Gestisco il 1° campo
069200030715     C                   IF        i = 1
069300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
069400030715     C                   EVAL      posDa = 1
069500030715     C* Eseguo lo scan x trovare la fine del primo campo
069600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
069700030715     C* A questo punto "estraggo" il campo corrente
069800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069900030715     C                                                    (posA-posDa))
070000030715     C* X i campi successivi al 1°
070100030715     C                   ELSE
070200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
070300030715     C                   EVAL      posDa = posA + 1
070400030715     C* Eseguo lo scan x trovare la fine del campo corrente
070500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070600030715     C* Gestisco l'ultimo campo
070700030715     C                   IF        posA = *zeros
070800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070900030715     C* A questo punto "estraggo" il campo corrente
071000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071100030715     C                                                    (posA-posDa))
071200030715     C                   LEAVE
071300030715     C                   ELSE
071400030715     C* A questo punto "estraggo" il campo corrente
071500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071600030715     C                                                    (posA-posDa))
071700030715     C                   ENDIF
071800030715     C                   ENDIF
071900030715     C* Incremento il contatore d campo
072000030715     C                   EVAL      i = i +1
072100030715     C                   ENDDO
072200030715     C*
072300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
072400030715     C* delimitazione testo
072500030715     C                   EVAL      i = 1
072600030715     C                   DOW       i < %elem(SkSplitFLD)
072700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
072800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
072900030715     C                   EVAL      i = i + 1
073000030715     C                   ENDDO
073100030715     C                   EVAL      wGiro = 1
073200040726     C* Ripristino la 1° riga originale
073300040726     C                   EVAL      vinDTA = depvinDTA
073400030715     C                   ELSE
073500030715     C                   EVAL      wGiro = 2
073600031204     C**********
073700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
073800031204     C* d separatore campo
073900031204     C**********
074000060215     C                   EVAL      vindta = %trim(vindta)
074100060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
074200060215     C                   EVAL      vindta = %subst(vindta:2)
074300060215     C                   ENDDO
074400060215     C
074500060215     C                   Z-ADD     *zeros        lunghInput        4 0
074600060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
074700060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
074800060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
074900060215     C                   EVAL      lunghInput = %len(%trim(vindta))
075000060215     C                   ENDDO
075100030715     C**********
075200030715     C* Effettuo lo split del campo dati d input
075300030715     C**********
075400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075500040324     C                   DOW       posDa <= %len(%trimr(vindta))
075600030715     C*
075700030715     C* Gestisco il 1° campo
075800030715     C                   IF        i = 1
075900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
076000030715     C                   EVAL      posDa = 1
076100030715     C* Eseguo lo scan x trovare la fine del primo campo
076200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076300030715     C* A questo punto "estraggo" il campo corrente
076400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076500030715     C                                                    (posA-posDa))
076600030715     C* X i campi successivi al 1°
076700030715     C                   ELSE
076800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076900030715     C                   EVAL      posDa = posA + 1
077000031204     C* Verifico che nn vi sia il campo nn valorizzato
077100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
077200031204     C* Se campo nn valorizzato skippo al prossimo
077300031204     C                   EVAL      SkSplitCSV(i) = *blanks
077400031204     C                   EVAL      posA  = posA + 1
077500031204     C                   ELSE
077600030715     C* Eseguo lo scan x trovare la fine del campo corrente
077700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077800030715     C* Gestisco l'ultimo campo
077900030715     C                   IF        posA = *zeros
078000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
078100030715     C* A questo punto "estraggo" il campo corrente
078200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078300030715     C                                                    (posA-posDa))
078400030715     C                   LEAVE
078500030715     C                   ELSE
078600030715     C* A questo punto "estraggo" il campo corrente
078700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078800030715     C                                                    (posA-posDa))
078900030715     C                   ENDIF
079000031204     C                   ENDIF
079100030715     C                   ENDIF
079200030715     C* Incremento il contatore d campo
079300030715     C                   EVAL      i = i +1
079400030715     C                   ENDDO
079500030715     C*
079600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
079700030715     C* delimitazione testo
079800030715     C                   EVAL      i = 1
079900030715     C                   DOW       i < %elem(SkSplitCSV)
080000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
080100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
080200030715     C                   EVAL      i = i + 1
080300030715     C                   ENDDO
080400030715     C*
080500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
080600030715     C                   EVAL      i = 1
080700030715     C                   DOW       i < %elem(SkSplitFLD)
080800030715     C*
080900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
081000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
081100030715     C                             SkSplitCSV(i) <> *blanks
081200030721     C*** ATB
081300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
081400030721     C                   EVAL      VABATB = SkSplitCSV(i)
081500030721     C                   ENDIF
081600030715     C*** RSD
081700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
081800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
081900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
082000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
082100030715     C* ==
082200030715     C                   ENDIF
082300030721     C*** RD2
082400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
082500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
082600030721     C                   ENDIF
082700030715     C*** IND
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
082900030715     C                   EVAL      VABIND = SkSplitCSV(i)
083000030715     C                   ENDIF
083100030715     C*** LOD
083200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
083300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
083400030715     C                   ENDIF
083500030715     C*** PRD
083600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
083700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
083800030715     C                   ENDIF
083900030721     C*** NZD
084000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
084100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
084200030721     C                   ENDIF
084300030715     C*** RMA
084400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
084500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
084600030715     C                   ENDIF
084700030715     C*** NOT
084800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
084900060306     C                   EVAL      VABNOT = SkSplitCSV(i)
085000030715     C                   ENDIF
085100030715     C*** NT2
085200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
085300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
085400030715     C                   ENDIF
085500030715     C*** VCA
085600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
085700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
085800030715     C                   ENDIF
085900030715     C*** VAS
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
086100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
086200030715     C                   ENDIF
086300030715     C*** FFD
086400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
086500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
086600030715     C                   ENDIF
086700030715     C*** CBO
086800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
086900030715     C                   EVAL      VABCBO = SkSplitCSV(i)
087000030715     C                   ENDIF
087100030721     C*** GC1
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
087300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030721     C*** GC2
087600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
087700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
087800030721     C                   ENDIF
087900030721     C*** TSP
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
088100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030721     C*** NAS
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
088500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030721     C*** TIC
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
088900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030721     C*** GCA
089200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
089300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
089400030721     C                   ENDIF
089500030721     C*** XCO
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
089700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030721     C*** CTM
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
090100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** TCR
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
090500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030721     C*** CTS
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
090900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030721     C*** FTM
091200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
091300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
091400030721     C                   ENDIF
091500030721     C*** VAD
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
091700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** GMA
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
092100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** GGA
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
092500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** GVA
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
092900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** TC1
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
093300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** TC2
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
093700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** SCL
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
094100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** RMO
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
094500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** NMO
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
094900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*
095200030721     C* Reperisco quindi i campi numerici...
095300030721     C*** CAD
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
095500030721     C                   EVAL      PiStr=SkSplitCSV(i)
095600030721     C                   EXSR      CHKNUM
095700030721     C                   IF        PiInt=*on
095800030721     C                   Z-ADD     PiVal         Num5_0
095900030721     C                   MOVEL(p)  Num5_0        VABCAD
096000030721     C                   ELSE
096100030721     C                   SETON                                        32
096200030721     C                   EVAL      VABCAD = *zeros
096300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096400030721     C                             + ' ' + 'VABCAD'
096500030721     C                   ENDIF
096600030721     C                   ENDIF
096700030721     C*** RMN
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
096900030721     C                   EVAL      PiStr=SkSplitCSV(i)
097000030721     C                   EXSR      CHKNUM
097100030721     C                   IF        PiInt=*on
097200030721     C                   Z-ADD     PiVal         VABRMN
097300030721     C                   ELSE
097400030721     C                   SETON                                        32
097500030721     C                   EVAL      VABRMN = 1
097600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097700030721     C                             + ' ' + 'VABRMN'
097800030721     C                   ENDIF
097900030721     C                   ENDIF
098000030721     C*** NSP
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098200030721     C                   EVAL      PiStr=SkSplitCSV(i)
098300030721     C                   EXSR      CHKNUM
098400040726     C                   IF        PiInt=*on
098500040726     C                   Z-ADD     PiVal         VABNSP
098600030721     C                   ELSE
098700030721     C                   SETON                                        32
098800030721     C                   EVAL      VABNSP = *zeros
098900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099000030721     C                             + ' ' + 'VABNSP'
099100030721     C                   ENDIF
099200030721     C                   ENDIF
099300030721     C*** NCL
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
099500030721     C                   EVAL      PiStr=SkSplitCSV(i)
099600030721     C                   EXSR      CHKNUM
099700030721     C                   IF        PiInt=*on
099800030721     C                   Z-ADD     PiVal         VABNCL
099900030721     C                   ELSE
100000030721     C                   SETON                                        32
100100030721     C                   EVAL      VABNCL = *zeros
100200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100300030721     C                             + ' ' + 'VABNCL'
100400030721     C                   ENDIF
100500030721     C                   ENDIF
100600030721     C*** PKB
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
100800030721     C                   EVAL      PiStr=SkSplitCSV(i)
100900030721     C                   EXSR      CHKNUM
101000030721     C                   IF        PiNum=*on
101100030721     C                   Z-ADD     PiVal         VABPKB
101200030721     C                   ELSE
101300030721     C                   SETON                                        32
101400030721     C                   EVAL      VABPKB = *zeros
101500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101600030721     C                             + ' ' + 'VABPKB'
101700030721     C                   ENDIF
101800030721     C                   ENDIF
101900030721     C*** CAS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
102100030721     C                   EVAL      PiStr=SkSplitCSV(i)
102200030721     C                   EXSR      CHKNUM
102300030721     C                   IF        PiNum=*on
102400030721     C                   Z-ADD     PiVal         VABCAS
102500030721     C                   ELSE
102600030721     C                   SETON                                        32
102700030721     C                   EVAL      VABCAS = *zeros
102800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102900030721     C                             + ' ' + 'VABCAS'
103000030721     C                   ENDIF
103100030721     C                   ENDIF
103200030721     C*** CCM
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
103400030721     C                   EVAL      PiStr=SkSplitCSV(i)
103500030721     C                   EXSR      CHKNUM
103600030721     C                   IF        PiInt=*on      AND
103700030721     C                             PiVal<=9999999 AND
103800030721     C                             PiVal>*zeros
103900030721     C                   Z-ADD     PiVal         VABCCM
104000030721     C                   ELSE
104100030721     C                   SETON                                        32
104200030721     C                   EVAL      VABCCM = *zeros
104300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104400030721     C                             + ' ' + 'VABCCM'
104500030721     C                   ENDIF
104600030721     C                   ENDIF
104700030721     C*** LNP
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100030721     C                   IF        PiInt=*on    AND
105200030721     C                             PiVal<=999   AND
105300030721     C                             PiVal>*zeros
105400030721     C                   Z-ADD     PiVal         VABLNP
105500030721     C                   ELSE
105600030721     C                   SETON                                        32
105700030721     C                   EVAL      VABLNP = *zeros
105800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105900030721     C                             + ' ' + 'VABLNP'
106000030721     C                   ENDIF
106100030721     C                   ENDIF
106200030721     C*** NRS
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
106400030721     C                   EVAL      PiStr=SkSplitCSV(i)
106500030721     C                   EXSR      CHKNUM
106600030721     C                   IF        PiInt=*on AND
106700030721     C                             PiVal<=99
106800030721     C                   Z-ADD     PiVal         VABNRS
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABNRS'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** LNA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiInt=*on    AND
108000030721     C                             PiVal<=999   AND
108100030721     C                             PiVal>*zeros
108200030721     C                   Z-ADD     PiVal         VABLNA
108300030721     C                   ELSE
108400030721     C                   SETON                                        32
108500030721     C                   EVAL      VABLNA = *zeros
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700030721     C                             + ' ' + 'VABLNA'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** CTR
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on  AND
109500030721     C                             PiVal<=999
109600030721     C                   Z-ADD     PiVal         VABCTR
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABCTR'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** IAS
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiNum=*on
110800030721     C                   Z-ADD     PiVal         VABIAS
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111200030721     C                             + ' ' + 'VABIAS'
111300030721     C                   ENDIF
111400030721     C                   ENDIF
111500030721     C*** VLB
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
111700030721     C                   EVAL      PiStr=SkSplitCSV(i)
111800030721     C                   EXSR      CHKNUM
111900030721     C                   IF        PiNum=*on
112000030721     C                   Z-ADD     PiVal         VABVLB
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABVLB'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** QFT
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiNum=*on
113200030721     C                   Z-ADD     PiVal         VABQFT
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABQFT'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** NCD
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on      AND
114400030721     C                             PiVal<=9999999
114500030721     C                   Z-ADD     PiVal         VABNCD
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABNCD'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** NCA
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on      AND
115700030721     C                             PiVal<=9999999
115800030721     C                   Z-ADD     PiVal         VABNCA
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200030721     C                             + ' ' + 'VABNCA'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** ZNC
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on AND
117000030721     C                             PiVal<=99
117100030721     C                   Z-ADD     PiVal         VABZNC
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABZNC'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** DCR
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on       AND
118300030721     C                             PiVal<=99999999
118400030721     C                   Z-ADD     PiVal         VABDCR
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABDCR'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** HCR
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on   AND
119600030721     C                             PiVal<=9999
119700030721     C                   Z-ADD     PiVal         VABHCR
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABHCR'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** VMD
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABVMD
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABVMD'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** ANT
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on        AND
122100030721     C                             PiVal<=999999999
122200030721     C                   Z-ADD     PiVal         VABANT
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABANT'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** CMO
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on
123400030721     C                   Z-ADD     PiVal         Num5_0
123500030721     C                   MOVEL(p)  Num5_0        VABCMO
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      VABCMO = *zeros
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCMO'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300071211     C*
124400071211     C*******                            ********
124500071211     C* CONSIDERAZIONI PARTICOLARI DENTRO CICLO *
124600071211     C*******                            ********
124700071211     C*** RMO
124800071211     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO_W'
124900071211     C                   EVAL      PiStr=SkSplitCSV(i)
125000071211     C                   EVAL      wDS_RMO_RICEV=%trim(PiStr)
125100071211     C*
125200071211     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
125300071211     C                   MOVE(p)   *blanks       wNomeFile        30
125400071211     C                   MOVE(p)   vlrMSG        wNomeFile
125500071211     C     minu:maiu     XLATE     wNomeFile     wNomeFile
125600071211     C                   IF        %scan('NORD1'  :wNomeFile) > 0 OR
125700071211     C                             %scan('NORD 1' :wNomeFile) > 0 OR
125800071211     C                             %scan('NORD01' :wNomeFile) > 0 OR
125900071211     C                             %scan('NORD 01':wNomeFile) > 0
126000071211     C                   EVAL      wDS_RMO_FILE ='NORD 1'
126100071211     C                   ENDIF
126200071211     C                   IF        %scan('NORD2'  :wNomeFile) > 0 OR
126300071211     C                             %scan('NORD 2' :wNomeFile) > 0 OR
126400071211     C                             %scan('NORD02' :wNomeFile) > 0 OR
126500071211     C                             %scan('NORD 02':wNomeFile) > 0
126600071211     C                   EVAL      wDS_RMO_FILE ='NORD 2'
126700071211     C                   ENDIF
126800071211     C                   IF        %scan('SUD':wNomeFile) > 0
126900071211     C                   EVAL      wDS_RMO_FILE ='SUD'
127000071211     C                   ENDIF
127100071211     C                   IF        %scan('CENTRO':wNomeFile) > 0
127200071211     C                   EVAL      wDS_RMO_FILE ='CENTRO'
127300071211     C                   ENDIF
127400071212     C                   EVAL      VABRMO = wDS_RMO
127500071211     C                   ENDIF
127600071211     C*
127700071211     C*******************************
127800030715     C*
127900030715     C                   ENDIF
128000030715     C                   EVAL      i = i + 1
128100030715     C                   ENDDO
128200070713     C*
128300071211     C*******                           ********
128400071211     C* CONSIDERAZIONI PARTICOLARI FUORI CICLO *
128500070718     C*******                           ********
128600070713     C*
128700071212     C* NSP => Stacco un numeratore da AZNUM
128800071212     C                   clear                   TRUL33DS
128900071212     C                   eval      I33OPE = *zeros
129000071212     C                   eval      I33CNU = 302
129100071212     C                   eval      I33NUM = 1
129200071212     C                   movel     TRUL33DS      KPJBU
129300071212     C                   call      'TRUL33R'
129400071212     C                   parm                    KPJBA
129500071212     C                   movel     KPJBU         TRUL33DS
129600071212     C                   if        O33ERR = *zeros
129700071212     C                   z-add     O33NRF        VABNSP
129800071212     C                   else
129900071212     C                   SETON                                        32
130000071212     C                   EVAL      vinmsg = %trimr(vinmsg)
130100071212     C                             + ' ' + 'VABNSP'
130200071212     C                   endif
130300040726     C*
130400070713     C*******************************
130500070713     C*
130600040726     C* Considerazioni personalizzate x il traduttore corrente
130700060306     C* Reperisco la provincia dal CAP e dalla località
130800060306     C                   IF        VABPRD = *blanks AND
130900060306     C                             VABCAD <> *blanks
131000060306     C                   CLEAR                   TISI95DS
131100060306     C                   EVAL      I95TCN = '3'
131200060306     C                   Z-ADD     datcor        I95DAT
131300060306     C                   EVAL      I95CAP = VABCAD
131400060306     C                   EVAL      I95LOC = VABLOD
131500060306     C                   CALL      'TISI95R'
131600060306     C                   PARM                    TISI95DS
131700060306     C                   EVAL      VABPRD = O95PRV
131800060306     C                   ENDIF
131900040726     C*
132000040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
132100040726     C                   IF        VABNSP = *zeros
132200040726     C                   SETON                                        31
132300040726     C                   ENDIF
132400040726     C*
132500040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
132600040726     C                   IF        VABLNP = *zeros
132700040726     C                   SETON                                        31
132800040726     C                   ENDIF
132900040726     C*
133000040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
133100040726     C* d valorizzarli sempre entrambi
133200040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
133300040726     C                   Z-ADD     VABNSP        VABRMN
133400040726     C                   ENDIF
133500040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
133600040726     C                   MOVEL     VABRMN        VABRMA
133700040726     C                   ENDIF
133800040726     C*
133900040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
134000040726     C                   IF        VABCAS > *zeros
134100040726     C                   IF        VABVCA = *blanks
134200040726     C                   EVAL      VABVCA = 'EUR'
134300070629     C                   EVAL      VABTIC = *blanks
134400040726     C                   ENDIF
134500040726     C                   IF        VABCBO = '1'
134600040726     C                   EVAL      VABCBO = '4'
134700040726     C                   ENDIF
134800040726     C                   IF        VABCBO = '2'
134900040726     C                   EVAL      VABCBO = '6'
135000040726     C                   ENDIF
135100040726     C                   ENDIF
135200020204     C*
135300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
135400020204     C                   EXSR      CHKIMPDIV
135500030715     C                   ENDIF
135600020204     C*
135700010607     C                   ENDSR
135800010607     C*----------------------------------------------------*
135900010601
136000020204
136100020204     C*----------------------------------------------------*
136200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
136300020204     C*----------------------------------------------------*
136400020204     C     CHKIMPDIV     BEGSR
136500020204     C*
136600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
136700020204     C                   Z-ADD     *zeros        wrkDec            9 9
136800020204     C*
136900020204     C* Come prima cosa effettuo considerazioni sulla divisa
137000020204     C                   IF        vabIAS > *zeros
137100020204     C                   IF        vabVAS <> 'EUR'
137200020204     C                   EVAL      vabVAS =  'ITL'
137300020204     C                   ENDIF
137400020204     C                   ENDIF
137500020204     C*
137600020204     C                   IF        vabCAS > *zeros
137700020204     C                   IF        vabVCA <> 'EUR'
137800020204     C                   EVAL      vabVCA =  'ITL'
137900020204     C                   ENDIF
138000020204     C                   ENDIF
138100020204     C*
138200020204     C                   IF        vabVMD > *zeros
138300020204     C                   IF        vabVAD <> 'EUR'
138400020204     C                   EVAL      vabVAD =  'ITL'
138500020204     C                   ENDIF
138600020204     C                   ENDIF
138700020204     C*
138800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
138900020204     C                   Z-ADD     vabIAS        wrkDec
139000020204     C                   IF        wrkDec > *zeros
139100020204     C                   IF        vabVAS = 'ITL'
139200020204     C                   EVAL      vabIAS = *zeros
139300020204     C                   ENDIF
139400020204     C                   ENDIF
139500020204     C*
139600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
139700020204     C                   Z-ADD     vabCAS        wrkDec
139800020204     C                   IF        wrkDec > *zeros
139900020204     C                   IF        vabVCA = 'ITL'
140000020204     C                   EVAL      vabCAS = *zeros
140100020204     C                   ENDIF
140200020204     C                   ENDIF
140300020204     C*
140400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
140500020204     C                   Z-ADD     vabVMD        wrkDec
140600020204     C                   IF        wrkDec > *zeros
140700020204     C                   IF        vabVAD = 'ITL'
140800020204     C                   EVAL      vabVMD = *zeros
140900020204     C                   ENDIF
141000020204     C                   ENDIF
141100020204     C*
141200020204     C                   ENDSR
141300020204     C***
141400020204
141500010330
141600010330
141700010330     C*----------------------------------------------------*
141800010330     C*  CONTROLLO NUMERICITA' CAMPI
141900010330     C*----------------------------------------------------*
142000010330     C     CHKNUM        BEGSR
142100010330     C*
142200010606     C                   IF        PiDecChr = *blanks
142300030715     C                   EVAL      PiDecChr = CharNUM
142400010606     C                   ENDIF
142500010606     C*
142600010606     C                   CALL(e)   'ISNUMERIC'
142700010330     C                   PARM                    PiStr            30
142800010606     C                   PARM                    PiDecChr          1
142900010330     C                   PARM      *ZEROS        PiVal            30 9
143000010330     C                   PARM      '0'           PiInt             1
143100010330     C                   PARM      '0'           PiNum             1
143200010330     C                   IF        %error
143300010606     C                   EVAL      PiNum=*off
143400010330     C                   ENDIF
143500010330     C*
143600010330     C                   ENDSR
143700010330     C***
143800010330
143900010601
144000010601
144100010601
144200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
144300010601     C     repfil        BEGSR
144400010601     C*
144500010601     C                   if        invfil = *zeros and
144600010601     C                             depfil > *zeros and
144700010629     C                             (vinflg = *blanks or
144800010629     C                              vinflg = *zeros)
144900010601     C
145000010601     C                   eval      invfil = depfil
145100010601     C                   endif
145200010601     C*
145300010601     C                   if        depfil <> invfil and
145400010601     C                             invfil > *zeros
145500010601     C                   eval      flgMulti = '1'
145600010601     C                   if        vinflg = *blanks
145700010601     C                   add       1             cntNonEl
145800010601     C                   endif
145900010601     C                   endif
146000010601     C*
146100010601     C                   if        vinflg = '2'
146200010601     C                   eval      flgStato = '2'
146300010601     C                   endif
146400010601     C*
146500010601     C                   ENDSR
146600010601     C***
146700010601
146800010601
146900010601
147000010330
147100010330
147200010330
147300990920      /TITLE Invio dei dati al punto operativo.
147400000613     C     invio         BEGSR
147500990920     C*
147600990920     C                   reset                   dscmz
147700010601     C                   move      invfil        cmzdst
147800990920     C                   eval      cmzfld = vlrfou
147900990920     C                   eval      cmzmbd = vlrhdl
148000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
148100000710     C                   if        prmfir = *blanks
148200021025     C                   eval      cmzfla = 'FIVAB00F'
148300021025     C                   eval      cmzmba = 'FIVAB00F'
148400000710     C                   else
148500000710     C                   eval      cmzfla = prmfir
148600000710     C                   eval      cmzmba = prmfir
148700000710     C                   endif
148800990920     C                   eval      cmznrr = *zeros
148900990920     C                   move      §ctrok        cmznrr
149000021018     C                   eval      cmzlba = vlrfl1
149100990920     C                   call(e)   'TIS711C'
149200990920     C                   parm                    dscmz
149300990921     C                   parm      *blanks       esito
149400990920     C                   if        %error
149500990920     C                             or cmzerr = '1'
149600990921     C                             or esito  = '1'
149700000710     C                   eval      wrkesito = '3'
149800990920     C                   endif
149900990920     C*
150000000613     C                   ENDSR
150100990910
150200010601
150300010601
150400010601
150500010601
150600010601      /TITLE Invio dei dati al punto operativo.
150700010601     C     opeini        BEGSR
150800010601     C*
150900010601     C* Inizializzo flag e contatori operativi
151000010601     C                   movel     '0'           flgGiro           1
151100010601     C                   movel     '0'           flgMulti          1
151200010601     C                   movel     '1'           flgStato          1
151300010615     C                   movel     '0'           flgOk             1
151400010601     C                   z-add     *zeros        cntNonEl         10 0
151500010601     C                   z-add     *zeros        depfil            3 0
151600010601     C                   z-add     *zeros        invfil            3 0
151700010601     C*
151800010601     C                   ENDSR
151900010601     C***
152000010601
152100010601
152200050421
152300050421
152400050421     C     *pssr         BEGSR
152500050421     C*
152600050421     C                   eval      wrkesito = '2'
152700050421     C*
152800050421     C                   ENDSR     '*CANCL'
152900050421     C***
153000050421
153100010601
153200010330
153300010330
153400000613     C     *inzsr        BEGSR
153500990910     C*
153600990910     C     *entry        plist
153700990920     C                   parm                    tivlrds
153800990921     C                   parm      wrkesito      esito
153900000724     C                   parm                    prmlit
154000000710     C                   parm                    prmfir
154100010330     C*
154200010330     C* CALCOLA LA DATA CORRENTE
154300010330     C                   time                    wn14             14 0
154400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
154500010330     C                   z-add     wn8           g08dat
154600010330     C                   z-add     *zeros        g08inv
154700010330     C                   movel     '0'           g08err
154800010330     C                   call      'XSRDA8'
154900010330     C                   parm                    wlbda8
155000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
155100000613     C*
155200000613     C                   ENDSR
155300000613     C***
155400990908
155500030715     Otitvxlp   E            riepilogo         2
155600990915     O                                              'Upload via Internet'
155700990915     O                                           +1 'Traduzione TIVIN00R -'
155800021025     O                                           +1 'FIVABWWR'
155900990915     O                                           +1 'Testate bolle'
156000990915     O          E            riepilogo   2
156100990915     O                       wrkdata
156200990915     O                       wrkora              +1
156300990915     O                       procname            +1
156400990915     O          E            riepilogo   2
156500990915     O                                              'Cliente..................:'
156600010605     O                       VABCCM        z     +1
156700990915     O          E            riepilogo   2
156800990920     O                                              'Riferimento Strategi.....:'
156900990920     O                       vlrhdl              +1
157000990915     O          E            riepilogo   2
157100990915     O                                              'Giusti...................:'
157200971022     O                       §CTROK        2   +  1
157300990915     O          E            riepilogo   2
157400990915     O                                              'Sbagliati e corretti.....:'
157500971022     O                       §CTRMO        2   +  1
157600990915     O          E            riepilogo   2
157700990915     O                                              'Sbagliati e scartati.....:'
157800971022     O                       §CTRNO        2   +  1
157900000613
158000030715     Otitvxlps  E            testdett          2
158100000613     O                                              'Upload via Internet'
158200000613     O                                           +1 'Traduzione TIVIN00R -'
158300021025     O                                           +1 'FIVABWWR'
158400000613     O                                           +1 'Testate bolle'
158500000616     O          E            testdett    3
158600000613     O                                           +2 'N° rec'
158700000613     O                                           +3 'Anteprima contenuto'
158800000616     O          E            rigadett    2
158900000613     O                       rrnum               +2
159000000621     O                       recko               +3
159100000616     O          E            findett     2
159200000613     O                       wrkdata
159300000613     O                       wrkora              +1
159400000613     O                       procname            +1
159500000616     O          E            findett     2
159600000613     O                                              'Cliente..................:'
159700010605     O                       VABCCM        z     +1
159800000616     O          E            findett     2
159900000613     O                                              'Riferimento Strategi.....:'
160000000613     O                       vlrhdl              +1
160100000616     O          E            findett     2
160200000613     O                                              'Giusti...................:'
160300000613     O                       §CTROK        2   +  1
160400000616     O          E            findett     2
160500000613     O                                              'Sbagliati e corretti.....:'
160600000613     O                       §CTRMO        2   +  1
160700000616     O          E            findett     2
160800000613     O                                              'Sbagliati e scartati.....:'
160900000613     O                       §CTRNO        2   +  1
161000000613** CMD - COMANDI CL
161100030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
161200030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
161300030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
161400010606
161500000613
161600040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
161700071212VABRMO_W;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABRMA;VABRMN;
161800071212FILL01;VABNCL;VABPKB;
161900070713
162000050321
162100060215
