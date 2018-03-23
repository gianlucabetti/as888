000100050308      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente WIRESHOPI - Cod. 1300449)
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
001900040726     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
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
003600040726     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700030715     D*------------------
003800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003900030715     D*------------------
004000030715     D SkSplitFLD      S             10    DIM(100)
004100030715     D SkSplitCSV      S            256    DIM(100)
004200030715     D CharCSV         S              1
004300030715     D CharTXT         S              1
004400030715     D CharNUM         S              1
004500030715     D posDa           S              3  0 INZ(*zeros)
004600030715     D posA            S              3  0 INZ(*zeros)
004700030715     D i               s              3  0 INZ(1)
004800030715     D wGiro           s              1  0 INZ(*zeros)
004900050307     D wOldSep         s              3    INZ(*blanks)
005000050307     D wNewSep         s              3    INZ(*blanks)
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
013700010601
013800010601
013900010601
014000010601
014100010601     C*--------------------------------------------------------
014200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014300010601     C*--------------------------------------------------------
014400010601     C     ENDELA        BEGSR
014500990915     C*
014600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
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
020300040726     C*
020400040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020500040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020600040726     C                   exsr      inzvar
020700040726     C                   exsr      impvab
020800010330     C*
020900030822     C                   do        *HIVAL
021000010330     C*
021100030822     C                   read      tivin00r                               70
021200010618     C*
021300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021400010618     C                   if        vinflg = '1'
021500010618     C                   eval      flgOk = '1'
021600010618     C                   endif
021700010618     C*
021800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021900040510     C                   z-add     *zeros        wLenVINDTA        4 0
022000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
022100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022400040510     C                   z-add     1             wFlgVINDTA
022500040510     C                   leave
022600040510     C                   endif
022700040510     C                   enddo
022800040510     C*
022900040218     C                   if        vindta > *blanks AND
023000040510     C                             wFlgVINDTA = 1
023100010330     C                   add       1             rrnum
023200010330     C*
023300010601     C                   if        *in70 = *off and
023400010330     C                             (vinflg = *blanks
023500010330     C                              or vinflg = '0'
023600010330     C                              or vinflg = '2')
023700010330     C*
023800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023900010711     C                   if        vinflg = *blanks or vinflg = '0'
024000010711     C                   clear                   vinmsg
024100010711     C                   endif
024200010601     C*
024300010330     C                   exsr      inzvar
024400010605     C                   exsr      impvab
024500010601     C*
024600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024700010605     C                   eval      depfil = VABLNP
024800010601     C                   exsr      repfil
024900010601     C                   if        depfil = invfil
025000021025     C                   if        vlrpoi = 999
025100030822     C                   move(P)   invfil        VABFGS
025200021025     C                   else
025300030822     C                   move(P)   vlrpoi        VABFGS
025400021025     C                   endif
025500010601     C*
025600010601     C                   exsr      PREELA
025700010601     C*
025800010604     C* Ebbene...
025900010604     C*
026000030822     C  N31              add       1             §CTROK            7 0
026100030822     C   32              add       1             §CTRMO            7 0
026200030822     C   31              add       1             §CTRNO            7 0
026300030822     C                   if        wGiro = 2
026400030822     C  N31              write     FIVAB000
026500030822     C                   endif
026600010604     C*
026700010604     C                   if        *in31 = *off and
026800010604     C                             *in32 = *off
026900010604     C                   eval      vinflg = '1'
027000010604     C                   else
027100010604     C                   eval      recko = vindta
027200010604     C                   except    rigadett
027300010604     C                   eval      vinflg = '2'
027400010604     C                   endif
027500010604     C                   endif
027600010601     C*
027700010604     C                   endif
027800010604     C*
027900010330     C                   else
028000010330     C                   eval      vinflg = '1'
028100010330     C                   endif
028200010601     C*
028300010601     C  N70              update    tivin000
028400010330     C*
028500030822     C  N70              enddo
028600010601     C
028700010601     C*
028800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028900010601     C                   if        cntNonEl = *zeros or
029000010601     C                             flgMulti = '0'
029100010330     C* Se non ci sono record con errori ...
029200010601     C                   if        §ctrno = 0 and
029300010604     C                             §ctrmo = 0 and
029400010601     C                             flgStato <> '2'
029500010330     C* ... restituisco esito OK.
029600010330     C                   eval      wrkesito = '0'
029700010330     C                   else
029800010330     C                   if        §ctrok > 0
029900010330     C                   eval      wrkesito = '1'
030000010330     C                   else
030100010615     C                   if        flgOk = '0'
030200010615     C                   eval      wrkesito = '2'
030300010615     C                   else
030400010615     C                   eval      wrkesito = '6'
030500010615     C                   endif
030600010330     C                   endif
030700010330     C                   endif
030800010601     C                   else
030900010601     C                   eval      wrkesito = '9'
031000010601     C                   endif
031100010330     C*
031200010330     C                   if        %open(tivin00r)
031300010330     C                   close     tivin00r
031400010330     C                   endif
031500021025     C                   if        %open(fivabwwr)
031600021025     C                   close     fivabwwr
031700010330     C                   endif
031800010601     C*
031900010601     C                   if        vlrpoi <> 999
032000010601     C                   eval      invfil = vlrpoi
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        §ctrok > 0
032400010601     C                             and invfil > *zeros
032500010330     C                   exsr      invio
032600010330     C                   endif
032700010601     C*
032800010618     C                   if        flgGiro = '1'
032900010601     C                   exsr      endela
033000010618     C                   endif
033100010330     C*
033200010330     C                   ENDSR
033300010330     C***
033400010601
033500010601
033600010601
033700010330     C*----------------------------------------------------*
033800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
033900010330     C*----------------------------------------------------*
034000010330     C     INZVAR        BEGSR
034100010330     C*
034200030822     C* Inizializzo variabili di wrk
034300020204     C                   Z-ADD     *zeros        Num5_0            5 0
034400050307     C                   MOVEL     *blanks       TIPOVIA          15
034500050307     C                   MOVEL     *blanks       PREFTEL          15
034600050307     C                   MOVEL     *blanks       FLAGCAS          15
034700050307     C                   MOVEL     *blanks       FLAGIAS          15
034800030822     C*
034900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
035000030822     C                   CLEAR                   FIVAB000
035100030822     C                   CLEAR                   SkSplitCSV
035200030822     C                   EVAL      i = 1
035300030822     C                   EVAL      posDa = *zeros
035400030822     C                   EVAL      posA  = *zeros
035500030822     C*
035600030822     C* Reimposto i valori di default
035700030822     C                   EXSR      DEFCAM
035800030822     C*
035900010330     C                   ENDSR
036000010330     C*----------------------------------------------------*
036100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
036200010330     C*----------------------------------------------------*
036300010330     C     DEFCAM        BEGSR
036400030822     C*
036500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
036600030715     C* e delimitatore testo.
036700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
036800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
036900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
037000050307     C*
037100050307     C* Compongo la sequenza separatore testo + separatore campo
037200050307     C                   EVAL      wOldSep = CharTXT + CharCSV + CharTXT
037300050307     C                   EVAL      wNewSep = CharTXT + ';' + CharTXT
037400030822     C*
037500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
037600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
037700030715     C                   SETON                                        50
037800030715     C                   ELSE
037900030715     C                   SETOFF                                       50
038000030715     C                   ENDIF
038100030822     C*
038200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
038300030822     C                   EVAL      posDaDft = 1
038400030822     C                   EVAL      posADft  = 0
038500030822     C                   EVAL      wGiroDft = 0
038600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
038700030822     C                             posDaDft > 0
038800030822     C*
038900030822     C* Gestisco il 1° giro
039000030822     C                   IF        wGiroDft = 0
039100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
039200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
039300030822     C* Incremento il contatore dei "giri"
039400030822     C                   EVAL      wGiroDft = 1
039500030822     C                   ELSE
039600030822     C                   EVAL      posDaDft = posADft
039700030822     C                   ENDIF
039800030822     C* Eseguo lo scan x trovare la fine del campo corrente
039900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
040000030822     C*
040100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
040200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
040300030822     C                   IF        posDaDft > 0 AND
040400030822     C                             posADft  > 0
040500030822     C* NCL
040600030822     C                   IF        %subst(
040700030822     C                             %subst(vlrppt:posDaDft+1:
040800030822     C                             posADft-posDaDft-1):1:3)
040900030822     C                             = 'NCL'
041000030822     C                   EVAL      PiStr=%trim(%subst(
041100030822     C                             %subst(vlrppt:posDaDft+1:
041200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041300030822     C                   EXSR      CHKNUM
041400030822     C                   IF        PiInt=*on
041500030822     C                   Z-ADD     PiVal         VABNCL
041600030822     C                   ENDIF
041700030822     C                   ENDIF
041800030822     C* CCM
041900030822     C                   IF        %subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):1:3)
042200030822     C                             = 'CCM'
042300030822     C                   EVAL      PiStr=%trim(%subst(
042400030822     C                             %subst(vlrppt:posDaDft+1:
042500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042600030822     C                   EXSR      CHKNUM
042700030822     C                   IF        PiInt=*on
042800030822     C                   Z-ADD     PiVal         VABCCM
042900030822     C                   ENDIF
043000030822     C                   ENDIF
043100030822     C* LNP
043200030822     C                   IF        %subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):1:3)
043500030822     C                             = 'LNP'
043600030822     C                   EVAL      PiStr=%trim(%subst(
043700030822     C                             %subst(vlrppt:posDaDft+1:
043800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043900030822     C                   EXSR      CHKNUM
044000030822     C                   IF        PiInt=*on
044100030822     C                   Z-ADD     PiVal         VABLNP
044200030822     C                   ENDIF
044300030822     C                   ENDIF
044400030822     C* NRS
044500030822     C                   IF        %subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):1:3)
044800030822     C                             = 'NRS'
044900030822     C                   EVAL      PiStr=%trim(%subst(
045000030822     C                             %subst(vlrppt:posDaDft+1:
045100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045200030822     C                   EXSR      CHKNUM
045300030822     C                   IF        PiInt=*on
045400030822     C                   Z-ADD     PiVal         VABNRS
045500030822     C                   ENDIF
045600030822     C                   ENDIF
045700030822     C* CTR
045800030822     C                   IF        %subst(
045900030822     C                             %subst(vlrppt:posDaDft+1:
046000030822     C                             posADft-posDaDft-1):1:3)
046100030822     C                             = 'CTR'
046200030822     C                   EVAL      PiStr=%trim(%subst(
046300030822     C                             %subst(vlrppt:posDaDft+1:
046400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046500030822     C                   EXSR      CHKNUM
046600030822     C                   IF        PiInt=*on
046700030822     C                   Z-ADD     PiVal         VABCTR
046800030822     C                   ENDIF
046900030822     C                   ENDIF
047000030822     C* PKB
047100030822     C                   IF        %subst(
047200030822     C                             %subst(vlrppt:posDaDft+1:
047300030822     C                             posADft-posDaDft-1):1:3)
047400030822     C                             = 'PKB'
047500030822     C                   EVAL      PiStr=%trim(%subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047800030822     C                   EXSR      CHKNUM
047900030822     C                   IF        PiNum=*on
048000030822     C                   Z-ADD     PiVal         VABPKB
048100030822     C                   ENDIF
048200030822     C                   ENDIF
048300030822     C* VLB
048400030822     C                   IF        %subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):1:3)
048700030822     C                             = 'VLB'
048800030822     C                   EVAL      PiStr=%trim(%subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049100030822     C                   EXSR      CHKNUM
049200030822     C                   IF        PiNum=*on
049300030822     C                   Z-ADD     PiVal         VABVLB
049400030822     C                   ENDIF
049500030822     C                   ENDIF
049600030822     C* QFT
049700030822     C                   IF        %subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):1:3)
050000030822     C                             = 'QFT'
050100030822     C                   EVAL      PiStr=%trim(%subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050400030822     C                   EXSR      CHKNUM
050500030822     C                   IF        PiNum=*on
050600030822     C                   Z-ADD     PiVal         VABQFT
050700030822     C                   ENDIF
050800030822     C                   ENDIF
050900030822     C* CBO
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'CBO'
051400030822     C                   EVAL      VABCBO=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   ENDIF
051800030822     C* TSP
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'TSP'
052300030822     C                   EVAL      VABTSP=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   ENDIF
052700030822     C* VAS
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'VAS'
053200030822     C                   EVAL      VABVAS=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   ENDIF
053600030822     C* VCA
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'VCA'
054100030822     C                   EVAL      VABVCA=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   ENDIF
054500030822     C* GCA
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'GCA'
055000030822     C                   EVAL      VABGCA=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   ENDIF
055400030822     C* CTM
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'CTM'
055900030822     C                   EVAL      VABCTM=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   ENDIF
056300030822     C* FFD
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'FFD'
056800030822     C                   EVAL      VABFFD=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   ENDIF
057200030822     C* VAD
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'VAD'
057700030822     C                   EVAL      VABVAD=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   ENDIF
058100030822     C* GMA
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'GMA'
058600030822     C                   EVAL      VABGMA=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   ENDIF
059000030822     C* GGA
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'GGA'
059500030822     C                   EVAL      VABGGA=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   ENDIF
059900030822     C* GVA
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'GVA'
060400030822     C                   EVAL      VABGVA=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   ENDIF
060800030822     C* TC1
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'TC1'
061300030822     C                   EVAL      VABTC1=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   ENDIF
061700030822     C* TC2
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'TC2'
062200030822     C                   EVAL      VABTC2=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   ENDIF
062600030822     C* ...
062700030822     C                   ENDIF
062800030822     C                   ENDDO
062900020204     C*
063000010330     C                   ENDSR
063100010607     C*----------------------------------------------------*
063200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
063300010607     C*----------------------------------------------------*
063400010607     C     IMPVAB        BEGSR
063500010607     C*
063600010607     C                   SETOFF                                       3132
063700030515     C*
063800010607     C                   MOVEL     datcor        VABAAS
063900010607     C                   MOVE      datcor        VABMGS
064000050307     C*
064100050307     C* Dato che c'è 1 conflitto d separatori campi/barcodes => sostituisco quello campi
064200050307     C                   EVAL      CharCSV = ';'
064300050307     C*
064400050307     C                   Z-ADD     1             wj                5 0
064500050307     C                   DOW       wj <= %len(vindta) - %len(wOldSep)
064600050307     C                   IF        %subst(vindta:wj:3) = wOldSep
064700050307     C                   EVAL      %subst(vindta:wj:3) = wNewSep
064800050307     C                   ENDIF
064900050307     C                   ADD       1             wj
065000050307     C                   ENDDO
065100010607     C*
065200030715     C**********
065300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
065400030715     C**********
065500030715     C                   IF        wGiro = *zeros
065600040726     C*
065700040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
065800040726     C                   EVAL      depvinDTA = vinDTA
065900040726     C*
066000040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
066100040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
066200040726     C                                      %trim(intcol(3))
066300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
066400031201     C     minu:maiu     XLATE     vindta        vindta
066500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066600040324     C                   DOW       posDa <= %len(%trimr(vindta))
066700030715     C*
066800030715     C* Gestisco il 1° campo
066900030715     C                   IF        i = 1
067000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
067100030715     C                   EVAL      posDa = 1
067200030715     C* Eseguo lo scan x trovare la fine del primo campo
067300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067400030715     C* A questo punto "estraggo" il campo corrente
067500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
067600030715     C                                                    (posA-posDa))
067700030715     C* X i campi successivi al 1°
067800030715     C                   ELSE
067900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
068000030715     C                   EVAL      posDa = posA + 1
068100030715     C* Eseguo lo scan x trovare la fine del campo corrente
068200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068300030715     C* Gestisco l'ultimo campo
068400030715     C                   IF        posA = *zeros
068500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
068600030715     C* A questo punto "estraggo" il campo corrente
068700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
068800030715     C                                                    (posA-posDa))
068900030715     C                   LEAVE
069000030715     C                   ELSE
069100030715     C* A questo punto "estraggo" il campo corrente
069200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
069300030715     C                                                    (posA-posDa))
069400030715     C                   ENDIF
069500030715     C                   ENDIF
069600030715     C* Incremento il contatore d campo
069700030715     C                   EVAL      i = i +1
069800030715     C                   ENDDO
069900030715     C*
070000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
070100030715     C* delimitazione testo
070200030715     C                   EVAL      i = 1
070300030715     C                   DOW       i < %elem(SkSplitFLD)
070400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
070500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
070600030715     C                   EVAL      i = i + 1
070700030715     C                   ENDDO
070800030715     C                   EVAL      wGiro = 1
070900040726     C* Ripristino la 1° riga originale
071000040726     C                   EVAL      vinDTA = depvinDTA
071100030715     C                   ELSE
071200030715     C                   EVAL      wGiro = 2
071300031204     C**********
071400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
071500031204     C* d separatore campo
071600031204     C**********
071700040726     C                   EVAL      vindta = %trim(vindta)
071800040726     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
071900040726     C                   EVAL      vindta = %subst(vindta:2)
072000040726     C                   ENDDO
072100031204     C*
072200031204     C                   Z-ADD     *zeros        lunghInput        4 0
072300040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
072400040726     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
072500040726     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
072600040726     C                   EVAL      lunghInput = %len(%trim(vindta))
072700040726     C                   ENDDO
072800030715     C**********
072900030715     C* Effettuo lo split del campo dati d input
073000030715     C**********
073100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073200040324     C                   DOW       posDa <= %len(%trimr(vindta))
073300030715     C*
073400030715     C* Gestisco il 1° campo
073500030715     C                   IF        i = 1
073600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073700030715     C                   EVAL      posDa = 1
073800030715     C* Eseguo lo scan x trovare la fine del primo campo
073900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074000030715     C* A questo punto "estraggo" il campo corrente
074100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
074200030715     C                                                    (posA-posDa))
074300030715     C* X i campi successivi al 1°
074400030715     C                   ELSE
074500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074600030715     C                   EVAL      posDa = posA + 1
074700031204     C* Verifico che nn vi sia il campo nn valorizzato
074800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
074900031204     C* Se campo nn valorizzato skippo al prossimo
075000031204     C                   EVAL      SkSplitCSV(i) = *blanks
075100031204     C                   EVAL      posA  = posA + 1
075200031204     C                   ELSE
075300030715     C* Eseguo lo scan x trovare la fine del campo corrente
075400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075500030715     C* Gestisco l'ultimo campo
075600030715     C                   IF        posA = *zeros
075700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
075800030715     C* A questo punto "estraggo" il campo corrente
075900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076000030715     C                                                    (posA-posDa))
076100030715     C                   LEAVE
076200030715     C                   ELSE
076300030715     C* A questo punto "estraggo" il campo corrente
076400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
076500030715     C                                                    (posA-posDa))
076600030715     C                   ENDIF
076700031204     C                   ENDIF
076800030715     C                   ENDIF
076900030715     C* Incremento il contatore d campo
077000030715     C                   EVAL      i = i +1
077100030715     C                   ENDDO
077200030715     C*
077300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077400030715     C* delimitazione testo
077500030715     C                   EVAL      i = 1
077600030715     C                   DOW       i < %elem(SkSplitCSV)
077700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
077800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
077900030715     C                   EVAL      i = i + 1
078000030715     C                   ENDDO
078100030715     C*
078200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
078300030715     C                   EVAL      i = 1
078400030715     C                   DOW       i < %elem(SkSplitFLD)
078500030715     C*
078600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
078700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
078800030715     C                             SkSplitCSV(i) <> *blanks
078900030721     C*** ATB
079000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
079100030721     C                   EVAL      VABATB = SkSplitCSV(i)
079200030721     C                   ENDIF
079300030715     C*** RSD
079400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
079500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
079600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
079800030715     C* ==
079900030715     C                   ENDIF
080000030721     C*** RD2
080100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
080200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
080300030721     C                   ENDIF
080400030715     C*** IND
080500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
080600030715     C                   EVAL      VABIND = SkSplitCSV(i)
080700030715     C                   ENDIF
080800030715     C*** LOD
080900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
081000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
081100030715     C                   ENDIF
081200030715     C*** PRD
081300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
081400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
081500030715     C                   ENDIF
081600030721     C*** NZD
081700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
081800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
081900030721     C                   ENDIF
082000030715     C*** RMA
082100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
082200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
082300030715     C                   ENDIF
082400030715     C*** NOT
082500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
082600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
082700030715     C                   ENDIF
082800030715     C*** NT2
082900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
083000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
083100030715     C                   ENDIF
083200030715     C*** VCA
083300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
083400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
083500030715     C                   ENDIF
083600030715     C*** VAS
083700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
083800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
083900030715     C                   ENDIF
084000030715     C*** FFD
084100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
084200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
084300030715     C                   ENDIF
084400030715     C*** CBO
084500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
084600030715     C                   EVAL      VABCBO = SkSplitCSV(i)
084700030715     C                   ENDIF
084800030715     C
084900030721     C*** GC1
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
085100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** GC2
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
085500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** TSP
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
085900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** NAS
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
086300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** TIC
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
086700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** GCA
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
087100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** XCO
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
087500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** CTM
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
087900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** TCR
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
088300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** CTS
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
088700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** FTM
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
089100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** VAD
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
089500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** GMA
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
089900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** GGA
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
090300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** GVA
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
090700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** TC1
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
091100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** TC2
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
091500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** SCL
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
091900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** RMO
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
092300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030721     C*** NMO
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
092700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030721     C*
093000030721     C* Reperisco quindi i campi numerici...
093100030721     C*** CAD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
093300030721     C                   EVAL      PiStr=SkSplitCSV(i)
093400030721     C                   EXSR      CHKNUM
093500030721     C                   IF        PiInt=*on
093600030721     C                   Z-ADD     PiVal         Num5_0
093700030721     C                   MOVEL(p)  Num5_0        VABCAD
093800030721     C                   ELSE
093900030721     C                   SETON                                        32
094000030721     C                   EVAL      VABCAD = *zeros
094100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094200030721     C                             + ' ' + 'VABCAD'
094300030721     C                   ENDIF
094400030721     C                   ENDIF
094500030721     C*** RMN
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
094700030721     C                   EVAL      PiStr=SkSplitCSV(i)
094800030721     C                   EXSR      CHKNUM
094900030721     C                   IF        PiInt=*on
095000030721     C                   Z-ADD     PiVal         VABRMN
095100030721     C                   ELSE
095200030721     C                   SETON                                        32
095300030721     C                   EVAL      VABRMN = 1
095400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095500030721     C                             + ' ' + 'VABRMN'
095600030721     C                   ENDIF
095700030721     C                   ENDIF
095800030721     C*** NSP
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096000030721     C                   EVAL      PiStr=SkSplitCSV(i)
096100030721     C                   EXSR      CHKNUM
096200040726     C                   IF        PiInt=*on
096300040726     C                   Z-ADD     PiVal         VABNSP
096400040802     C*                  IF        PiVal>9999999
096500040802     C*                  SETON                                        32
096600040802     C*                  EVAL      vinmsg = %trimr(vinmsg)
096700040802     C*                            + ' ' + 'VABNSP'
096800040802     C*                  ENDIF
096900030721     C                   ELSE
097000030721     C                   SETON                                        32
097100030721     C                   EVAL      VABNSP = *zeros
097200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097300030721     C                             + ' ' + 'VABNSP'
097400030721     C                   ENDIF
097500030721     C                   ENDIF
097600030721     C*** NCL
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
097800030721     C                   EVAL      PiStr=SkSplitCSV(i)
097900030721     C                   EXSR      CHKNUM
098000030721     C                   IF        PiInt=*on
098100030721     C                   Z-ADD     PiVal         VABNCL
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABNCL = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABNCL'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** PKB
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiNum=*on
099400030721     C                   Z-ADD     PiVal         VABPKB
099500030721     C                   ELSE
099600030721     C                   SETON                                        32
099700030721     C                   EVAL      VABPKB = *zeros
099800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099900030721     C                             + ' ' + 'VABPKB'
100000030721     C                   ENDIF
100100030721     C                   ENDIF
100200030721     C*** CAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
100400030721     C                   EVAL      PiStr=SkSplitCSV(i)
100500030721     C                   EXSR      CHKNUM
100600030721     C                   IF        PiNum=*on
100700030721     C                   Z-ADD     PiVal         VABCAS
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      VABCAS = *zeros
101100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101200030721     C                             + ' ' + 'VABCAS'
101300030721     C                   ENDIF
101400030721     C                   ENDIF
101500030721     C*** CCM
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on      AND
102000030721     C                             PiVal<=9999999 AND
102100030721     C                             PiVal>*zeros
102200030721     C                   Z-ADD     PiVal         VABCCM
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABCCM = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700030721     C                             + ' ' + 'VABCCM'
102800030721     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C*** LNP
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
103200030721     C                   EVAL      PiStr=SkSplitCSV(i)
103300030721     C                   EXSR      CHKNUM
103400030721     C                   IF        PiInt=*on    AND
103500030721     C                             PiVal<=999   AND
103600030721     C                             PiVal>*zeros
103700030721     C                   Z-ADD     PiVal         VABLNP
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      VABLNP = *zeros
104100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104200030721     C                             + ' ' + 'VABLNP'
104300030721     C                   ENDIF
104400030721     C                   ENDIF
104500030721     C*** NRS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
104700030721     C                   EVAL      PiStr=SkSplitCSV(i)
104800030721     C                   EXSR      CHKNUM
104900030721     C                   IF        PiInt=*on AND
105000030721     C                             PiVal<=99
105100030721     C                   Z-ADD     PiVal         VABNRS
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105500030721     C                             + ' ' + 'VABNRS'
105600030721     C                   ENDIF
105700030721     C                   ENDIF
105800030721     C*** LNA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
106000030721     C                   EVAL      PiStr=SkSplitCSV(i)
106100030721     C                   EXSR      CHKNUM
106200030721     C                   IF        PiInt=*on    AND
106300030721     C                             PiVal<=999   AND
106400030721     C                             PiVal>*zeros
106500030721     C                   Z-ADD     PiVal         VABLNA
106600030721     C                   ELSE
106700030721     C                   SETON                                        32
106800030721     C                   EVAL      VABLNA = *zeros
106900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107000030721     C                             + ' ' + 'VABLNA'
107100030721     C                   ENDIF
107200030721     C                   ENDIF
107300030721     C*** CTR
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on  AND
107800030721     C                             PiVal<=999
107900030721     C                   Z-ADD     PiVal         VABCTR
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABCTR'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** IAS
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABIAS
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABIAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** VLB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABVLB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABVLB'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** QFT
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABQFT
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABQFT'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NCD
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on      AND
112700030721     C                             PiVal<=9999999
112800030721     C                   Z-ADD     PiVal         VABNCD
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNCD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on      AND
114000030721     C                             PiVal<=9999999
114100030721     C                   Z-ADD     PiVal         VABNCA
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABNCA'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** ZNC
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on AND
115300030721     C                             PiVal<=99
115400030721     C                   Z-ADD     PiVal         VABZNC
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABZNC'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** DCR
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on       AND
116600030721     C                             PiVal<=99999999
116700030721     C                   Z-ADD     PiVal         VABDCR
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABDCR'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** HCR
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on   AND
117900030721     C                             PiVal<=9999
118000030721     C                   Z-ADD     PiVal         VABHCR
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABHCR'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** VMD
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABVMD
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABVMD'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** ANT
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on        AND
120400030721     C                             PiVal<=999999999
120500030721     C                   Z-ADD     PiVal         VABANT
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABANT'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CMO
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on
121700030721     C                   Z-ADD     PiVal         Num5_0
121800030721     C                   MOVEL(p)  Num5_0        VABCMO
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCMO = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCMO'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600050307     C*
122700050307     C* TEST SPECIFICI
122800050307     C*
122900050307     C*** TIPOVIA
123000050307     C                   IF        %trim(SkSplitFLD(i)) = 'TIPOVIA'
123100050307     C                   EVAL      TIPOVIA = SkSplitCSV(i)
123200050307     C                   ENDIF
123300050307     C*** PREFTEL
123400050307     C                   IF        %trim(SkSplitFLD(i)) = 'PREFTEL'
123500050307     C                   EVAL      PREFTEL = SkSplitCSV(i)
123600050307     C                   ENDIF
123700050307     C*** FLAGCAS
123800050307     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGCAS'
123900050307     C                   EVAL      FLAGCAS = SkSplitCSV(i)
124000050307     C                   ENDIF
124100050307     C*** FLAGIAS
124200050307     C                   IF        %trim(SkSplitFLD(i)) = 'FLAGIAS'
124300050307     C                   EVAL      FLAGIAS = SkSplitCSV(i)
124400050307     C                   ENDIF
124500030715     C*
124600030715     C                   ENDIF
124700030715     C                   EVAL      i = i + 1
124800030715     C                   ENDDO
124900040726     C*
125000040726     C* Considerazioni personalizzate x il traduttore corrente
125100050307     C                   IF        TIPOVIA <> *blanks
125200050307     C                   EVAL      VABIND = %trim(TIPOVIA) + ' ' + %trim(VABIND)
125300050307     C                   ENDIF
125400050307     C*
125500050307     C                   IF        PREFTEL <> *blanks
125600050307     C                   EVAL      VABNT2 = %trim(PREFTEL) + ' ' + %trim(VABNT2)
125700050307     C                   ENDIF
125800050307     C*
125900050307     C                   IF        FLAGCAS = 'CON'
126000050307     C                   EVAL      VABTIC = *blanks
126100050307     C                   EVAL      VABVCA = 'EUR'
126200050307     C                   ELSE
126300050307     C                   EVAL      VABCAS = *zeros
126400050307     C                   ENDIF
126500050307     C*
126600050307     C                   IF        FLAGIAS = 'SI'
126700050307     C                   EVAL      VABVAS = 'EUR'
126800050307     C                   ELSE
126900050307     C                   EVAL      VABIAS = *zeros
127000050307     C                   ENDIF
127100040726     C*
127200040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127300040726     C                   IF        VABNSP = *zeros
127400040726     C                   SETON                                        31
127500040726     C                   ENDIF
127600040726     C*
127700040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127800040726     C                   IF        VABLNP = *zeros
127900040726     C                   SETON                                        31
128000040726     C                   ENDIF
128100040726     C*
128200040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
128300040726     C* d valorizzarli sempre entrambi
128400040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
128500040726     C                   Z-ADD     VABNSP        VABRMN
128600040726     C                   ENDIF
128700040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
128800040726     C                   MOVEL     VABRMN        VABRMA
128900040726     C                   ENDIF
129000040726     C*
129100040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
129200050307     C                   IF        FLAGCAS <> *blanks
129300040726     C                   IF        VABCBO = '1'
129400040726     C                   EVAL      VABCBO = '4'
129500040726     C                   ENDIF
129600040726     C                   IF        VABCBO = '2'
129700040726     C                   EVAL      VABCBO = '6'
129800040726     C                   ENDIF
129900040726     C                   ENDIF
130000050307     C*
130100050307     C* Eseguo considerazioni sulla valuta in relazione al contrassegno
130200050307     C                   IF        VABCAS > *zeros
130300050307     C                   IF        VABVCA = *blanks
130400050307     C                   EVAL      VABVCA = 'EUR'
130500050307     C                   ENDIF
130600050307     C                   ENDIF
130700050307     C*
130800050307     C* Eseguo considerazioni sulla valuta in relazione all'importo da assicurare
130900050307     C                   IF        VABIAS > *zeros
131000050307     C                   IF        VABVAS = *blanks
131100050307     C                   EVAL      VABVAS = 'EUR'
131200050307     C                   ENDIF
131300050307     C                   ENDIF
131400020204     C*
131500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
131600020204     C                   EXSR      CHKIMPDIV
131700030715     C                   ENDIF
131800020204     C*
131900010607     C                   ENDSR
132000010607     C*----------------------------------------------------*
132100010601
132200020204
132300020204     C*----------------------------------------------------*
132400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
132500020204     C*----------------------------------------------------*
132600020204     C     CHKIMPDIV     BEGSR
132700020204     C*
132800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
132900020204     C                   Z-ADD     *zeros        wrkDec            9 9
133000020204     C*
133100020204     C* Come prima cosa effettuo considerazioni sulla divisa
133200020204     C                   IF        vabIAS > *zeros
133300020204     C                   IF        vabVAS <> 'EUR'
133400020204     C                   EVAL      vabVAS =  'ITL'
133500020204     C                   ENDIF
133600020204     C                   ENDIF
133700020204     C*
133800020204     C                   IF        vabCAS > *zeros
133900020204     C                   IF        vabVCA <> 'EUR'
134000020204     C                   EVAL      vabVCA =  'ITL'
134100020204     C                   ENDIF
134200020204     C                   ENDIF
134300020204     C*
134400020204     C                   IF        vabVMD > *zeros
134500020204     C                   IF        vabVAD <> 'EUR'
134600020204     C                   EVAL      vabVAD =  'ITL'
134700020204     C                   ENDIF
134800020204     C                   ENDIF
134900020204     C*
135000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
135100020204     C                   Z-ADD     vabIAS        wrkDec
135200020204     C                   IF        wrkDec > *zeros
135300020204     C                   IF        vabVAS = 'ITL'
135400020204     C                   EVAL      vabIAS = *zeros
135500020204     C                   ENDIF
135600020204     C                   ENDIF
135700020204     C*
135800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
135900020204     C                   Z-ADD     vabCAS        wrkDec
136000020204     C                   IF        wrkDec > *zeros
136100020204     C                   IF        vabVCA = 'ITL'
136200020204     C                   EVAL      vabCAS = *zeros
136300020204     C                   ENDIF
136400020204     C                   ENDIF
136500020204     C*
136600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
136700020204     C                   Z-ADD     vabVMD        wrkDec
136800020204     C                   IF        wrkDec > *zeros
136900020204     C                   IF        vabVAD = 'ITL'
137000020204     C                   EVAL      vabVMD = *zeros
137100020204     C                   ENDIF
137200020204     C                   ENDIF
137300020204     C*
137400020204     C                   ENDSR
137500020204     C***
137600020204
137700010330
137800010330
137900010330     C*----------------------------------------------------*
138000010330     C*  CONTROLLO NUMERICITA' CAMPI
138100010330     C*----------------------------------------------------*
138200010330     C     CHKNUM        BEGSR
138300010330     C*
138400010606     C                   IF        PiDecChr = *blanks
138500030715     C                   EVAL      PiDecChr = CharNUM
138600010606     C                   ENDIF
138700010606     C*
138800010606     C                   CALL(e)   'ISNUMERIC'
138900010330     C                   PARM                    PiStr            30
139000010606     C                   PARM                    PiDecChr          1
139100010330     C                   PARM      *ZEROS        PiVal            30 9
139200010330     C                   PARM      '0'           PiInt             1
139300010330     C                   PARM      '0'           PiNum             1
139400010330     C                   IF        %error
139500010606     C                   EVAL      PiNum=*off
139600010330     C                   ENDIF
139700010330     C*
139800010330     C                   ENDSR
139900010330     C***
140000010330
140100010601
140200010601
140300010601
140400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
140500010601     C     repfil        BEGSR
140600010601     C*
140700010601     C                   if        invfil = *zeros and
140800010601     C                             depfil > *zeros and
140900010629     C                             (vinflg = *blanks or
141000010629     C                              vinflg = *zeros)
141100010601     C
141200010601     C                   eval      invfil = depfil
141300010601     C                   endif
141400010601     C*
141500010601     C                   if        depfil <> invfil and
141600010601     C                             invfil > *zeros
141700010601     C                   eval      flgMulti = '1'
141800010601     C                   if        vinflg = *blanks
141900010601     C                   add       1             cntNonEl
142000010601     C                   endif
142100010601     C                   endif
142200010601     C*
142300010601     C                   if        vinflg = '2'
142400010601     C                   eval      flgStato = '2'
142500010601     C                   endif
142600010601     C*
142700010601     C                   ENDSR
142800010601     C***
142900010601
143000010601
143100010601
143200010330
143300010330
143400010330
143500990920      /TITLE Invio dei dati al punto operativo.
143600000613     C     invio         BEGSR
143700990920     C*
143800990920     C                   reset                   dscmz
143900010601     C                   move      invfil        cmzdst
144000990920     C                   eval      cmzfld = vlrfou
144100990920     C                   eval      cmzmbd = vlrhdl
144200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
144300000710     C                   if        prmfir = *blanks
144400021025     C                   eval      cmzfla = 'FIVAB00F'
144500021025     C                   eval      cmzmba = 'FIVAB00F'
144600000710     C                   else
144700000710     C                   eval      cmzfla = prmfir
144800000710     C                   eval      cmzmba = prmfir
144900000710     C                   endif
145000990920     C                   eval      cmznrr = *zeros
145100990920     C                   move      §ctrok        cmznrr
145200021018     C                   eval      cmzlba = vlrfl1
145300990920     C                   call(e)   'TIS711C'
145400990920     C                   parm                    dscmz
145500990921     C                   parm      *blanks       esito
145600990920     C                   if        %error
145700990920     C                             or cmzerr = '1'
145800990921     C                             or esito  = '1'
145900000710     C                   eval      wrkesito = '3'
146000990920     C                   endif
146100990920     C*
146200000613     C                   ENDSR
146300990910
146400010601
146500010601
146600010601
146700010601
146800010601      /TITLE Invio dei dati al punto operativo.
146900010601     C     opeini        BEGSR
147000010601     C*
147100010601     C* Inizializzo flag e contatori operativi
147200010601     C                   movel     '0'           flgGiro           1
147300010601     C                   movel     '0'           flgMulti          1
147400010601     C                   movel     '1'           flgStato          1
147500010615     C                   movel     '0'           flgOk             1
147600010601     C                   z-add     *zeros        cntNonEl         10 0
147700010601     C                   z-add     *zeros        depfil            3 0
147800010601     C                   z-add     *zeros        invfil            3 0
147900010601     C*
148000010601     C                   ENDSR
148100010601     C***
148200010601
148300010601
148400050421
148500050421
148600050421     C     *pssr         BEGSR
148700050421     C*
148800050421     C                   eval      wrkesito = '2'
148900050421     C*
149000050421     C                   ENDSR     '*CANCL'
149100050421     C***
149200050421
149300010601
149400010330
149500010330
149600000613     C     *inzsr        BEGSR
149700990910     C*
149800990910     C     *entry        plist
149900990920     C                   parm                    tivlrds
150000990921     C                   parm      wrkesito      esito
150100000724     C                   parm                    prmlit
150200000710     C                   parm                    prmfir
150300010330     C*
150400010330     C* CALCOLA LA DATA CORRENTE
150500010330     C                   time                    wn14             14 0
150600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
150700010330     C                   z-add     wn8           g08dat
150800010330     C                   z-add     *zeros        g08inv
150900010330     C                   movel     '0'           g08err
151000010330     C                   call      'XSRDA8'
151100010330     C                   parm                    wlbda8
151200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
151300000613     C*
151400000613     C                   ENDSR
151500000613     C***
151600990908
151700030715     Otitvxlp   E            riepilogo         2
151800990915     O                                              'Upload via Internet'
151900990915     O                                           +1 'Traduzione TIVIN00R -'
152000021025     O                                           +1 'FIVABWWR'
152100990915     O                                           +1 'Testate bolle'
152200990915     O          E            riepilogo   2
152300990915     O                       wrkdata
152400990915     O                       wrkora              +1
152500990915     O                       procname            +1
152600990915     O          E            riepilogo   2
152700990915     O                                              'Cliente..................:'
152800010605     O                       VABCCM        z     +1
152900990915     O          E            riepilogo   2
153000990920     O                                              'Riferimento Strategi.....:'
153100990920     O                       vlrhdl              +1
153200990915     O          E            riepilogo   2
153300990915     O                                              'Giusti...................:'
153400971022     O                       §CTROK        2   +  1
153500990915     O          E            riepilogo   2
153600990915     O                                              'Sbagliati e corretti.....:'
153700971022     O                       §CTRMO        2   +  1
153800990915     O          E            riepilogo   2
153900990915     O                                              'Sbagliati e scartati.....:'
154000971022     O                       §CTRNO        2   +  1
154100000613
154200030715     Otitvxlps  E            testdett          2
154300000613     O                                              'Upload via Internet'
154400000613     O                                           +1 'Traduzione TIVIN00R -'
154500021025     O                                           +1 'FIVABWWR'
154600000613     O                                           +1 'Testate bolle'
154700000616     O          E            testdett    3
154800000613     O                                           +2 'N° rec'
154900000613     O                                           +3 'Anteprima contenuto'
155000000616     O          E            rigadett    2
155100000613     O                       rrnum               +2
155200000621     O                       recko               +3
155300000616     O          E            findett     2
155400000613     O                       wrkdata
155500000613     O                       wrkora              +1
155600000613     O                       procname            +1
155700000616     O          E            findett     2
155800000613     O                                              'Cliente..................:'
155900010605     O                       VABCCM        z     +1
156000000616     O          E            findett     2
156100000613     O                                              'Riferimento Strategi.....:'
156200000613     O                       vlrhdl              +1
156300000616     O          E            findett     2
156400000613     O                                              'Giusti...................:'
156500000613     O                       §CTROK        2   +  1
156600000616     O          E            findett     2
156700000613     O                                              'Sbagliati e corretti.....:'
156800000613     O                       §CTRMO        2   +  1
156900000616     O          E            findett     2
157000000613     O                                              'Sbagliati e scartati.....:'
157100000613     O                       §CTRNO        2   +  1
157200000613** CMD - COMANDI CL
157300030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
157400030715OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVXLPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
157500030715DLTOVR FILE(TITVXLP TITVXLPS) LVL(*)
157600010606
157700000613
157800040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
157900050307VABRSD;TIPOVIA;VABIND;VABLOD;VABPRD;VABCAD;X;PREFTEL;VABNT2;X;X;X;X;X;X;VABNSP;
158000050307VABNCL;VABPKB;VABNAS;VABNOT;FLAGCAS;VABCAS;X;FLAGIAS;VABIAS;X;X;X;X;X;X;X;X;X;X;X;X
158100040726
