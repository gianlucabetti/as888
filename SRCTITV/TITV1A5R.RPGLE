000100070925      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente FRO    Cod. 0432774)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700040726     D*------------
000800040726     D* SCHIEDE A PROGRAMMA
000900040726     D*------------
001000060215     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800060306     D tisi95ds      e ds
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D rrnum           s              6  0 INZ(*zeros)
002400060215     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
002500030715     D*------------------
002600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002700030715     D*------------------
002800030715     D SkSplitFLD      S             10    DIM(100)
002900030715     D SkSplitCSV      S            256    DIM(100)
003000030715     D CharCSV         S              1
003100030715     D CharTXT         S              1
003200030715     D CharNUM         S              1
003300030715     D posDa           S              3  0 INZ(*zeros)
003400030715     D posA            S              3  0 INZ(*zeros)
003500030715     D i               s              3  0 INZ(1)
003600030715     D wGiro           s              1  0 INZ(*zeros)
003700030822     D*------------------
003800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003900030822     D*------------------
004000030822     D posDaDft        S              3  0 INZ(*zeros)
004100030822     D posADft         S              3  0 INZ(*zeros)
004200030822     D j               s              3  0 INZ(1)
004300030822     D wGiroDft        s              1  0 INZ(*zeros)
004400010330     D*------------------
004500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004600010330     D*------------------
004700010330     D WLBDA8          DS                  INZ
004800010330     D  G08DAT                 1      8  0
004900010330     D  G08INV                 9     16  0
005000010330     D  G08ERR                17     17
005100010330     D  G08TGI                18     22  0
005200031201     D*------------------
005300031201     D* Costanti
005400031201     D*------------------
005500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005700010330
005800010330
005900010330
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300010601     C*
006400010601     C                   exsr      opeini
006500010605     C                   exsr      rwvab
006600010601     C*
006700010601     C                   seton                                        lr
006800010601
006900010601
007000010601
007100010601
007200010601     C*--------------------------------------------------------
007300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007400010601     C*--------------------------------------------------------
007500010601     C     PREELA        BEGSR
007600010601     C*
007700010601     C* SE OCCORRE SPEDIRE IN FILIALE
007800010601     C                   if        invfil <> *zeros and
007900010601     C                             flgGiro = '0'
008000010601     C*
008100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008200010601     C                   eval      flgGiro = '1'
008300010601     C*
008400010601     C                   endif
008500010601     C*
008600010601     C                   ENDSR
008700010601     C***
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009300010601     C*--------------------------------------------------------
009400010601     C     ENDELA        BEGSR
009500060306     C*
009600060306     C* Effettuo lancio TISI95 solo x chiusura
009700060306     C                   CLEAR                   TISI95DS
009800060306     C                   EVAL      I95TLA = 'C'
009900060306     C                   CALL      'TISI95R'
010000060306     C                   PARM                    TISI95DS
010100000616     C*
010200010601     C                   ENDSR
010300010601     C***
010400010601
010500010601
010600010601
010700010330     C*--------------------------------------------------------
010800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010900010330     C*--------------------------------------------------------
011000010605     C     RWVAB         BEGSR
011100010330     C*
011200010330     C                   if        not %open(tivin00r)
011300010330     C                   open      tivin00r
011400010330     C                   endif
011500021025     C                   if        not %open(fivabwwr)
011600021025     C                   open      fivabwwr
011700010330     C                   endif
011800010330     C*
011900010604     C                   clear                   §CTROK
012000010604     C                   clear                   §CTRMO
012100010604     C                   clear                   §CTRNO
012200040726     C*
012300040726     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
012400040726     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
012500040726     C                   exsr      inzvar
012600040726     C                   exsr      impvab
012700010330     C*
012800030822     C                   do        *HIVAL
012900010330     C*
013000030822     C                   read      tivin00r                               70
013100010618     C*
013200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013300010618     C                   if        vinflg = '1'
013400010618     C                   eval      flgOk = '1'
013500010618     C                   endif
013600060216     C*
013700060216     C* Se byte inferiore a HEX40 è potenzialmente probabile ricorra 1 errore nella visualizzazione
013800060216     C* => gestisco sempre
013900060216     C                   z-add     *zeros        wLenVINDTA        4 0
014000060216     C                   dow       wLenVINDTA < %len(%trim(vindta))
014100060216     C                   eval      wLenVINDTA = wLenVINDTA + 1
014200060216     C                   if        %subst(vindta:wLenVINDTA:1) < X'40'
014300060216     C                   eval      %subst(vindta:wLenVINDTA:1) = *blanks
014400060216     C                   endif
014500060216     C                   enddo
014600010618     C*
014700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
014800040510     C                   z-add     *zeros        wLenVINDTA        4 0
014900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
015100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
015200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
015300040510     C                   z-add     1             wFlgVINDTA
015400040510     C                   leave
015500040510     C                   endif
015600040510     C                   enddo
015700040510     C*
015800040218     C                   if        vindta > *blanks AND
015900040510     C                             wFlgVINDTA = 1
016000010330     C                   add       1             rrnum
016100010330     C*
016200010601     C                   if        *in70 = *off and
016300010330     C                             (vinflg = *blanks
016400010330     C                              or vinflg = '0'
016500010330     C                              or vinflg = '2')
016600010330     C*
016700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016800010711     C                   if        vinflg = *blanks or vinflg = '0'
016900010711     C                   clear                   vinmsg
017000010711     C                   endif
017100010601     C*
017200010330     C                   exsr      inzvar
017300010605     C                   exsr      impvab
017400010601     C*
017500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017600010605     C                   eval      depfil = VABLNP
017700010601     C                   exsr      repfil
017800010601     C                   if        depfil = invfil
017900021025     C                   if        vlrpoi = 999
018000030822     C                   move(P)   invfil        VABFGS
018100021025     C                   else
018200030822     C                   move(P)   vlrpoi        VABFGS
018300021025     C                   endif
018400010601     C*
018500010601     C                   exsr      PREELA
018600010601     C*
018700010604     C* Ebbene...
018800010604     C*
018900030822     C  N31              add       1             §CTROK            7 0
019000030822     C   32              add       1             §CTRMO            7 0
019100030822     C   31              add       1             §CTRNO            7 0
019200030822     C                   if        wGiro = 2
019300030822     C  N31              write     FIVAB000
019400030822     C                   endif
019500010604     C*
019600010604     C                   if        *in31 = *off and
019700010604     C                             *in32 = *off
019800010604     C                   eval      vinflg = '1'
019900010604     C                   else
020000010604     C                   eval      vinflg = '2'
020100010604     C                   endif
020200010604     C                   endif
020300010601     C*
020400010604     C                   endif
020500010604     C*
020600010330     C                   else
020700010330     C                   eval      vinflg = '1'
020800010330     C                   endif
020900010601     C*
021000010601     C  N70              update    tivin000
021100010330     C*
021200030822     C  N70              enddo
021300010601     C
021400010601     C*
021500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
021600010601     C                   if        cntNonEl = *zeros or
021700010601     C                             flgMulti = '0'
021800010330     C* Se non ci sono record con errori ...
021900010601     C                   if        §ctrno = 0 and
022000010604     C                             §ctrmo = 0 and
022100010601     C                             flgStato <> '2'
022200010330     C* ... restituisco esito OK.
022300010330     C                   eval      wrkesito = '0'
022400010330     C                   else
022500010330     C                   if        §ctrok > 0
022600010330     C                   eval      wrkesito = '1'
022700010330     C                   else
022800010615     C                   if        flgOk = '0'
022900010615     C                   eval      wrkesito = '2'
023000010615     C                   else
023100010615     C                   eval      wrkesito = '6'
023200010615     C                   endif
023300010330     C                   endif
023400010330     C                   endif
023500010601     C                   else
023600010601     C                   eval      wrkesito = '9'
023700010601     C                   endif
023800010330     C*
023900010330     C                   if        %open(tivin00r)
024000010330     C                   close     tivin00r
024100010330     C                   endif
024200021025     C                   if        %open(fivabwwr)
024300021025     C                   close     fivabwwr
024400010330     C                   endif
024500010601     C*
024600010601     C                   if        vlrpoi <> 999
024700010601     C                   eval      invfil = vlrpoi
024800010601     C                   endif
024900010330     C*
025000010330     C                   if        §ctrok > 0
025100010601     C                             and invfil > *zeros
025200010330     C                   exsr      invio
025300010330     C                   endif
025400010601     C*
025500010618     C                   if        flgGiro = '1'
025600010601     C                   exsr      endela
025700010618     C                   endif
025800010330     C*
025900010330     C                   ENDSR
026000010330     C***
026100010601
026200010601
026300010601
026400010330     C*----------------------------------------------------*
026500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026600010330     C*----------------------------------------------------*
026700010330     C     INZVAR        BEGSR
026800010330     C*
026900030822     C* Inizializzo variabili di wrk
027000020204     C                   Z-ADD     *zeros        Num5_0            5 0
027100030822     C*
027200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027300030822     C                   CLEAR                   FIVAB000
027400030822     C                   CLEAR                   SkSplitCSV
027500030822     C                   EVAL      i = 1
027600030822     C                   EVAL      posDa = *zeros
027700030822     C                   EVAL      posA  = *zeros
027800030822     C*
027900030822     C* Reimposto i valori di default
028000030822     C                   EXSR      DEFCAM
028100030822     C*
028200010330     C                   ENDSR
028300010330     C*----------------------------------------------------*
028400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028500010330     C*----------------------------------------------------*
028600010330     C     DEFCAM        BEGSR
028700030822     C*
028800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
028900030715     C* e delimitatore testo.
029000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029300030822     C*
029400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
029500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
029600030715     C                   SETON                                        50
029700030715     C                   ELSE
029800030715     C                   SETOFF                                       50
029900030715     C                   ENDIF
030000030822     C*
030100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
030200030822     C                   EVAL      posDaDft = 1
030300030822     C                   EVAL      posADft  = 0
030400030822     C                   EVAL      wGiroDft = 0
030500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
030600030822     C                             posDaDft > 0
030700030822     C*
030800030822     C* Gestisco il 1° giro
030900030822     C                   IF        wGiroDft = 0
031000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
031100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
031200030822     C* Incremento il contatore dei "giri"
031300030822     C                   EVAL      wGiroDft = 1
031400030822     C                   ELSE
031500030822     C                   EVAL      posDaDft = posADft
031600030822     C                   ENDIF
031700030822     C* Eseguo lo scan x trovare la fine del campo corrente
031800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
031900030822     C*
032000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
032100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
032200030822     C                   IF        posDaDft > 0 AND
032300030822     C                             posADft  > 0
032400030822     C* NCL
032500030822     C                   IF        %subst(
032600030822     C                             %subst(vlrppt:posDaDft+1:
032700030822     C                             posADft-posDaDft-1):1:3)
032800030822     C                             = 'NCL'
032900030822     C                   EVAL      PiStr=%trim(%subst(
033000030822     C                             %subst(vlrppt:posDaDft+1:
033100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
033200030822     C                   EXSR      CHKNUM
033300030822     C                   IF        PiInt=*on
033400030822     C                   Z-ADD     PiVal         VABNCL
033500030822     C                   ENDIF
033600030822     C                   ENDIF
033700030822     C* CCM
033800030822     C                   IF        %subst(
033900030822     C                             %subst(vlrppt:posDaDft+1:
034000030822     C                             posADft-posDaDft-1):1:3)
034100030822     C                             = 'CCM'
034200030822     C                   EVAL      PiStr=%trim(%subst(
034300030822     C                             %subst(vlrppt:posDaDft+1:
034400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034500030822     C                   EXSR      CHKNUM
034600030822     C                   IF        PiInt=*on
034700030822     C                   Z-ADD     PiVal         VABCCM
034800030822     C                   ENDIF
034900030822     C                   ENDIF
035000030822     C* LNP
035100030822     C                   IF        %subst(
035200030822     C                             %subst(vlrppt:posDaDft+1:
035300030822     C                             posADft-posDaDft-1):1:3)
035400030822     C                             = 'LNP'
035500030822     C                   EVAL      PiStr=%trim(%subst(
035600030822     C                             %subst(vlrppt:posDaDft+1:
035700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035800030822     C                   EXSR      CHKNUM
035900030822     C                   IF        PiInt=*on
036000030822     C                   Z-ADD     PiVal         VABLNP
036100030822     C                   ENDIF
036200030822     C                   ENDIF
036300030822     C* NRS
036400030822     C                   IF        %subst(
036500030822     C                             %subst(vlrppt:posDaDft+1:
036600030822     C                             posADft-posDaDft-1):1:3)
036700030822     C                             = 'NRS'
036800030822     C                   EVAL      PiStr=%trim(%subst(
036900030822     C                             %subst(vlrppt:posDaDft+1:
037000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037100030822     C                   EXSR      CHKNUM
037200030822     C                   IF        PiInt=*on
037300030822     C                   Z-ADD     PiVal         VABNRS
037400030822     C                   ENDIF
037500030822     C                   ENDIF
037600030822     C* CTR
037700030822     C                   IF        %subst(
037800030822     C                             %subst(vlrppt:posDaDft+1:
037900030822     C                             posADft-posDaDft-1):1:3)
038000030822     C                             = 'CTR'
038100030822     C                   EVAL      PiStr=%trim(%subst(
038200030822     C                             %subst(vlrppt:posDaDft+1:
038300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038400030822     C                   EXSR      CHKNUM
038500030822     C                   IF        PiInt=*on
038600030822     C                   Z-ADD     PiVal         VABCTR
038700030822     C                   ENDIF
038800030822     C                   ENDIF
038900030822     C* PKB
039000030822     C                   IF        %subst(
039100030822     C                             %subst(vlrppt:posDaDft+1:
039200030822     C                             posADft-posDaDft-1):1:3)
039300030822     C                             = 'PKB'
039400030822     C                   EVAL      PiStr=%trim(%subst(
039500030822     C                             %subst(vlrppt:posDaDft+1:
039600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039700030822     C                   EXSR      CHKNUM
039800030822     C                   IF        PiNum=*on
039900030822     C                   Z-ADD     PiVal         VABPKB
040000030822     C                   ENDIF
040100030822     C                   ENDIF
040200030822     C* VLB
040300030822     C                   IF        %subst(
040400030822     C                             %subst(vlrppt:posDaDft+1:
040500030822     C                             posADft-posDaDft-1):1:3)
040600030822     C                             = 'VLB'
040700030822     C                   EVAL      PiStr=%trim(%subst(
040800030822     C                             %subst(vlrppt:posDaDft+1:
040900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041000030822     C                   EXSR      CHKNUM
041100030822     C                   IF        PiNum=*on
041200030822     C                   Z-ADD     PiVal         VABVLB
041300030822     C                   ENDIF
041400030822     C                   ENDIF
041500030822     C* QFT
041600030822     C                   IF        %subst(
041700030822     C                             %subst(vlrppt:posDaDft+1:
041800030822     C                             posADft-posDaDft-1):1:3)
041900030822     C                             = 'QFT'
042000030822     C                   EVAL      PiStr=%trim(%subst(
042100030822     C                             %subst(vlrppt:posDaDft+1:
042200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042300030822     C                   EXSR      CHKNUM
042400030822     C                   IF        PiNum=*on
042500030822     C                   Z-ADD     PiVal         VABQFT
042600030822     C                   ENDIF
042700030822     C                   ENDIF
042800030822     C* CBO
042900030822     C                   IF        %subst(
043000030822     C                             %subst(vlrppt:posDaDft+1:
043100030822     C                             posADft-posDaDft-1):1:3)
043200030822     C                             = 'CBO'
043300030822     C                   EVAL      VABCBO=%trim(%subst(
043400030822     C                             %subst(vlrppt:posDaDft+1:
043500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043600030822     C                   ENDIF
043700030822     C* TSP
043800030822     C                   IF        %subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):1:3)
044100030822     C                             = 'TSP'
044200030822     C                   EVAL      VABTSP=%trim(%subst(
044300030822     C                             %subst(vlrppt:posDaDft+1:
044400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044500030822     C                   ENDIF
044600030822     C* VAS
044700030822     C                   IF        %subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):1:3)
045000030822     C                             = 'VAS'
045100030822     C                   EVAL      VABVAS=%trim(%subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400030822     C                   ENDIF
045500030822     C* VCA
045600030822     C                   IF        %subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):1:3)
045900030822     C                             = 'VCA'
046000030822     C                   EVAL      VABVCA=%trim(%subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046300030822     C                   ENDIF
046400030822     C* GCA
046500030822     C                   IF        %subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):1:3)
046800030822     C                             = 'GCA'
046900030822     C                   EVAL      VABGCA=%trim(%subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200030822     C                   ENDIF
047300030822     C* CTM
047400030822     C                   IF        %subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):1:3)
047700030822     C                             = 'CTM'
047800030822     C                   EVAL      VABCTM=%trim(%subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100030822     C                   ENDIF
048200030822     C* FFD
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'FFD'
048700030822     C                   EVAL      VABFFD=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   ENDIF
049100030822     C* VAD
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'VAD'
049600030822     C                   EVAL      VABVAD=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   ENDIF
050000030822     C* GMA
050100030822     C                   IF        %subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):1:3)
050400030822     C                             = 'GMA'
050500030822     C                   EVAL      VABGMA=%trim(%subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800030822     C                   ENDIF
050900030822     C* GGA
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'GGA'
051400030822     C                   EVAL      VABGGA=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   ENDIF
051800030822     C* GVA
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'GVA'
052300030822     C                   EVAL      VABGVA=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   ENDIF
052700030822     C* TC1
052800030822     C                   IF        %subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):1:3)
053100030822     C                             = 'TC1'
053200030822     C                   EVAL      VABTC1=%trim(%subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053500030822     C                   ENDIF
053600030822     C* TC2
053700030822     C                   IF        %subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):1:3)
054000030822     C                             = 'TC2'
054100030822     C                   EVAL      VABTC2=%trim(%subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054400030822     C                   ENDIF
054500030822     C* ...
054600030822     C                   ENDIF
054700030822     C                   ENDDO
054800020204     C*
054900010330     C                   ENDSR
055000010607     C*----------------------------------------------------*
055100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
055200010607     C*----------------------------------------------------*
055300010607     C     IMPVAB        BEGSR
055400010607     C*
055500010607     C                   SETOFF                                       3132
055600030515     C*
055700010607     C                   MOVEL     datcor        VABAAS
055800010607     C                   MOVE      datcor        VABMGS
055900050321     C*
056000050512     C* 1° Giro x eliminare il separatore testo
056100060215     C                   IF        CharTXT <> *blanks
056200050321     C                   Z-ADD     1             wj                5 0
056300050321     C                   DOW       wj <= %len(vindta) - 1
056400050321     C                   IF        %subst(vindta:wj:1) = CharTXT
056500050512     C                   EVAL      vindta = %subst(vindta:1:wj-1) +
056600050512     C                                      %subst(vindta:wj+1)
056700050512     C                   EVAL      wj = wj - 1
056800050512     C*                  EVAL      %subst(vindta:wj:1) = *blanks
056900050321     C                   ENDIF
057000050321     C                   ADD       1             wj
057100050321     C                   ENDDO
057200060215     C                   ENDIF
057300050512     C*
057400060215     C* Se la stringa dati da splittare inizia con il carattere separatore campo aggiungo in testa
057500050512     C* 2 byte a blank
057600050512     C                   IF        %subst(vindta:1:1) = CharCSV
057700050512     C                   EVAL      vindta = '  ' + vindta
057800050512     C                   ENDIF
057900010607     C*
058000030715     C**********
058100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
058200030715     C**********
058300030715     C                   IF        wGiro = *zeros
058400040726     C*
058500040726     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
058600040726     C                   EVAL      depvinDTA = vinDTA
058700040726     C*
058800040726     C* Inserisco qui la forzatura della intesatazione colonne personalizzata
058900040726     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
059000050321     C                                      %trim(intcol(3))+%trim(intcol(4))+
059100060215     C                                      %trim(intcol(5))
059200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
059300031201     C     minu:maiu     XLATE     vindta        vindta
059400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
059500040324     C                   DOW       posDa <= %len(%trimr(vindta))
059600030715     C*
059700030715     C* Gestisco il 1° campo
059800030715     C                   IF        i = 1
059900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
060000030715     C                   EVAL      posDa = 1
060100030715     C* Eseguo lo scan x trovare la fine del primo campo
060200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
060300030715     C* A questo punto "estraggo" il campo corrente
060400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
060500030715     C                                                    (posA-posDa))
060600030715     C* X i campi successivi al 1°
060700030715     C                   ELSE
060800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
060900030715     C                   EVAL      posDa = posA + 1
061000030715     C* Eseguo lo scan x trovare la fine del campo corrente
061100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
061200030715     C* Gestisco l'ultimo campo
061300030715     C                   IF        posA = *zeros
061400040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
061500030715     C* A questo punto "estraggo" il campo corrente
061600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
061700030715     C                                                    (posA-posDa))
061800030715     C                   LEAVE
061900030715     C                   ELSE
062000030715     C* A questo punto "estraggo" il campo corrente
062100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
062200030715     C                                                    (posA-posDa))
062300030715     C                   ENDIF
062400030715     C                   ENDIF
062500030715     C* Incremento il contatore d campo
062600030715     C                   EVAL      i = i +1
062700030715     C                   ENDDO
062800030715     C*
062900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
063000030715     C* delimitazione testo
063100030715     C                   EVAL      i = 1
063200030715     C                   DOW       i < %elem(SkSplitFLD)
063300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
063400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
063500030715     C                   EVAL      i = i + 1
063600030715     C                   ENDDO
063700030715     C                   EVAL      wGiro = 1
063800040726     C* Ripristino la 1° riga originale
063900040726     C                   EVAL      vinDTA = depvinDTA
064000030715     C                   ELSE
064100030715     C                   EVAL      wGiro = 2
064200031204     C**********
064300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
064400031204     C* d separatore campo
064500031204     C**********
064600060215     C                   EVAL      vindta = %trim(vindta)
064700060215     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
064800060215     C                   EVAL      vindta = %subst(vindta:2)
064900060215     C                   ENDDO
065000060215     C
065100060215     C                   Z-ADD     *zeros        lunghInput        4 0
065200060215     C                   EVAL      lunghInput = %len(%trimr(vindta))
065300060215     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
065400060215     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
065500060215     C                   EVAL      lunghInput = %len(%trim(vindta))
065600060215     C                   ENDDO
065700030715     C**********
065800030715     C* Effettuo lo split del campo dati d input
065900030715     C**********
066000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066100040324     C                   DOW       posDa <= %len(%trimr(vindta))
066200030715     C*
066300030715     C* Gestisco il 1° campo
066400030715     C                   IF        i = 1
066500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
066600030715     C                   EVAL      posDa = 1
066700030715     C* Eseguo lo scan x trovare la fine del primo campo
066800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
066900030715     C* A questo punto "estraggo" il campo corrente
067000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
067100030715     C                                                    (posA-posDa))
067200030715     C* X i campi successivi al 1°
067300030715     C                   ELSE
067400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
067500030715     C                   EVAL      posDa = posA + 1
067600031204     C* Verifico che nn vi sia il campo nn valorizzato
067700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
067800031204     C* Se campo nn valorizzato skippo al prossimo
067900031204     C                   EVAL      SkSplitCSV(i) = *blanks
068000031204     C                   EVAL      posA  = posA + 1
068100031204     C                   ELSE
068200030715     C* Eseguo lo scan x trovare la fine del campo corrente
068300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068400030715     C* Gestisco l'ultimo campo
068500030715     C                   IF        posA = *zeros
068600040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
068700030715     C* A questo punto "estraggo" il campo corrente
068800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
068900030715     C                                                    (posA-posDa))
069000030715     C                   LEAVE
069100030715     C                   ELSE
069200030715     C* A questo punto "estraggo" il campo corrente
069300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069400030715     C                                                    (posA-posDa))
069500030715     C                   ENDIF
069600031204     C                   ENDIF
069700030715     C                   ENDIF
069800030715     C* Incremento il contatore d campo
069900030715     C                   EVAL      i = i +1
070000030715     C                   ENDDO
070100030715     C*
070200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
070300030715     C* delimitazione testo
070400030715     C                   EVAL      i = 1
070500030715     C                   DOW       i < %elem(SkSplitCSV)
070600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
070700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
070800030715     C                   EVAL      i = i + 1
070900030715     C                   ENDDO
071000030715     C*
071100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
071200030715     C                   EVAL      i = 1
071300030715     C                   DOW       i < %elem(SkSplitFLD)
071400030715     C*
071500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
071600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
071700030715     C                             SkSplitCSV(i) <> *blanks
071800030721     C*** ATB
071900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
072000030721     C                   EVAL      VABATB = SkSplitCSV(i)
072100030721     C                   ENDIF
072200030715     C*** RSD
072300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
072400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
072500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
072700030715     C* ==
072800030715     C                   ENDIF
072900030721     C*** RD2
073000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
073100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
073200030721     C                   ENDIF
073300030715     C*** IND
073400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
073500030715     C                   EVAL      VABIND = SkSplitCSV(i)
073600030715     C                   ENDIF
073700030715     C*** LOD
073800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
073900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
074000030715     C                   ENDIF
074100030715     C*** PRD
074200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
074300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
074400030715     C                   ENDIF
074500030721     C*** NZD
074600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
074700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
074800030721     C                   ENDIF
074900030715     C*** RMA
075000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
075100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
075200030715     C                   ENDIF
075300030715     C*** NOT
075400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
075500060306     C                   EVAL      VABNOT = SkSplitCSV(i)
075600030715     C                   ENDIF
075700030715     C*** NT2
075800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
075900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
076000030715     C                   ENDIF
076100030715     C*** VCA
076200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
076300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
076400030715     C                   ENDIF
076500030715     C*** VAS
076600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
076700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
076800030715     C                   ENDIF
076900030715     C*** FFD
077000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
077100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
077200030715     C                   ENDIF
077300030715     C*** CBO
077400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
077500030715     C                   EVAL      VABCBO = SkSplitCSV(i)
077600030715     C                   ENDIF
077700030721     C*** GC1
077800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
077900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
078000030721     C                   ENDIF
078100030721     C*** GC2
078200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
078300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
078400030721     C                   ENDIF
078500030721     C*** TSP
078600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
078700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
078800030721     C                   ENDIF
078900030721     C*** NAS
079000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
079100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
079200030721     C                   ENDIF
079300030721     C*** TIC
079400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
079500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
079600030721     C                   ENDIF
079700030721     C*** GCA
079800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
079900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
080000030721     C                   ENDIF
080100030721     C*** XCO
080200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
080300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
080400030721     C                   ENDIF
080500030721     C*** CTM
080600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
080700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
080800030721     C                   ENDIF
080900030721     C*** TCR
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
081100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
081200030721     C                   ENDIF
081300030721     C*** CTS
081400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
081500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
081600030721     C                   ENDIF
081700030721     C*** FTM
081800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
081900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
082000030721     C                   ENDIF
082100030721     C*** VAD
082200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
082300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
082400030721     C                   ENDIF
082500030721     C*** GMA
082600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
082700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
082800030721     C                   ENDIF
082900030721     C*** GGA
083000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
083100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
083200030721     C                   ENDIF
083300030721     C*** GVA
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
083500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030721     C*** TC1
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
083900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030721     C*** TC2
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
084300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030721     C*** SCL
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
084700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** RMO
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
085100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** NMO
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
085500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*
085800030721     C* Reperisco quindi i campi numerici...
085900030721     C*** CAD
086000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
086100030721     C                   EVAL      PiStr=SkSplitCSV(i)
086200030721     C                   EXSR      CHKNUM
086300030721     C                   IF        PiInt=*on
086400030721     C                   Z-ADD     PiVal         Num5_0
086500030721     C                   MOVEL(p)  Num5_0        VABCAD
086600030721     C                   ELSE
086700030721     C                   SETON                                        32
086800030721     C                   EVAL      VABCAD = *zeros
086900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
087000030721     C                             + ' ' + 'VABCAD'
087100030721     C                   ENDIF
087200030721     C                   ENDIF
087300030721     C*** RMN
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
087500030721     C                   EVAL      PiStr=SkSplitCSV(i)
087600030721     C                   EXSR      CHKNUM
087700030721     C                   IF        PiInt=*on
087800030721     C                   Z-ADD     PiVal         VABRMN
087900030721     C                   ELSE
088000030721     C                   SETON                                        32
088100030721     C                   EVAL      VABRMN = 1
088200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
088300030721     C                             + ' ' + 'VABRMN'
088400030721     C                   ENDIF
088500030721     C                   ENDIF
088600030721     C*** NSP
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
088800030721     C                   EVAL      PiStr=SkSplitCSV(i)
088900030721     C                   EXSR      CHKNUM
089000040726     C                   IF        PiInt=*on
089100040726     C                   Z-ADD     PiVal         VABNSP
089200030721     C                   ELSE
089300030721     C                   SETON                                        32
089400030721     C                   EVAL      VABNSP = *zeros
089500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
089600030721     C                             + ' ' + 'VABNSP'
089700030721     C                   ENDIF
089800030721     C                   ENDIF
089900030721     C*** NCL
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
090100030721     C                   EVAL      PiStr=SkSplitCSV(i)
090200030721     C                   EXSR      CHKNUM
090300030721     C                   IF        PiInt=*on
090400030721     C                   Z-ADD     PiVal         VABNCL
090500030721     C                   ELSE
090600030721     C                   SETON                                        32
090700030721     C                   EVAL      VABNCL = *zeros
090800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
090900030721     C                             + ' ' + 'VABNCL'
091000030721     C                   ENDIF
091100030721     C                   ENDIF
091200030721     C*** PKB
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
091400030721     C                   EVAL      PiStr=SkSplitCSV(i)
091500030721     C                   EXSR      CHKNUM
091600030721     C                   IF        PiNum=*on
091700030721     C                   Z-ADD     PiVal         VABPKB
091800030721     C                   ELSE
091900030721     C                   SETON                                        32
092000030721     C                   EVAL      VABPKB = *zeros
092100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092200030721     C                             + ' ' + 'VABPKB'
092300030721     C                   ENDIF
092400030721     C                   ENDIF
092500030721     C*** CAS
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
092700030721     C                   EVAL      PiStr=SkSplitCSV(i)
092800030721     C                   EXSR      CHKNUM
092900030721     C                   IF        PiNum=*on
093000030721     C                   Z-ADD     PiVal         VABCAS
093100030721     C                   ELSE
093200030721     C                   SETON                                        32
093300030721     C                   EVAL      VABCAS = *zeros
093400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093500030721     C                             + ' ' + 'VABCAS'
093600030721     C                   ENDIF
093700030721     C                   ENDIF
093800030721     C*** CCM
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
094000030721     C                   EVAL      PiStr=SkSplitCSV(i)
094100030721     C                   EXSR      CHKNUM
094200030721     C                   IF        PiInt=*on      AND
094300030721     C                             PiVal<=9999999 AND
094400030721     C                             PiVal>*zeros
094500030721     C                   Z-ADD     PiVal         VABCCM
094600030721     C                   ELSE
094700030721     C                   SETON                                        32
094800030721     C                   EVAL      VABCCM = *zeros
094900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095000030721     C                             + ' ' + 'VABCCM'
095100030721     C                   ENDIF
095200030721     C                   ENDIF
095300030721     C*** LNP
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
095500030721     C                   EVAL      PiStr=SkSplitCSV(i)
095600030721     C                   EXSR      CHKNUM
095700030721     C                   IF        PiInt=*on    AND
095800030721     C                             PiVal<=999   AND
095900030721     C                             PiVal>*zeros
096000030721     C                   Z-ADD     PiVal         VABLNP
096100030721     C                   ELSE
096200030721     C                   SETON                                        32
096300030721     C                   EVAL      VABLNP = *zeros
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABLNP'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** NRS
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiInt=*on AND
097300030721     C                             PiVal<=99
097400030721     C                   Z-ADD     PiVal         VABNRS
097500030721     C                   ELSE
097600030721     C                   SETON                                        32
097700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097800030721     C                             + ' ' + 'VABNRS'
097900030721     C                   ENDIF
098000030721     C                   ENDIF
098100030721     C*** LNA
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
098300030721     C                   EVAL      PiStr=SkSplitCSV(i)
098400030721     C                   EXSR      CHKNUM
098500030721     C                   IF        PiInt=*on    AND
098600030721     C                             PiVal<=999   AND
098700030721     C                             PiVal>*zeros
098800030721     C                   Z-ADD     PiVal         VABLNA
098900030721     C                   ELSE
099000030721     C                   SETON                                        32
099100030721     C                   EVAL      VABLNA = *zeros
099200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099300030721     C                             + ' ' + 'VABLNA'
099400030721     C                   ENDIF
099500030721     C                   ENDIF
099600030721     C*** CTR
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
099800030721     C                   EVAL      PiStr=SkSplitCSV(i)
099900030721     C                   EXSR      CHKNUM
100000030721     C                   IF        PiInt=*on  AND
100100030721     C                             PiVal<=999
100200030721     C                   Z-ADD     PiVal         VABCTR
100300030721     C                   ELSE
100400030721     C                   SETON                                        32
100500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100600030721     C                             + ' ' + 'VABCTR'
100700030721     C                   ENDIF
100800030721     C                   ENDIF
100900030721     C*** IAS
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
101100030721     C                   EVAL      PiStr=SkSplitCSV(i)
101200030721     C                   EXSR      CHKNUM
101300030721     C                   IF        PiNum=*on
101400030721     C                   Z-ADD     PiVal         VABIAS
101500030721     C                   ELSE
101600030721     C                   SETON                                        32
101700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101800030721     C                             + ' ' + 'VABIAS'
101900030721     C                   ENDIF
102000030721     C                   ENDIF
102100030721     C*** VLB
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
102300030721     C                   EVAL      PiStr=SkSplitCSV(i)
102400030721     C                   EXSR      CHKNUM
102500030721     C                   IF        PiNum=*on
102600030721     C                   Z-ADD     PiVal         VABVLB
102700030721     C                   ELSE
102800030721     C                   SETON                                        32
102900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103000030721     C                             + ' ' + 'VABVLB'
103100030721     C                   ENDIF
103200030721     C                   ENDIF
103300030721     C*** QFT
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
103500030721     C                   EVAL      PiStr=SkSplitCSV(i)
103600030721     C                   EXSR      CHKNUM
103700030721     C                   IF        PiNum=*on
103800030721     C                   Z-ADD     PiVal         VABQFT
103900030721     C                   ELSE
104000030721     C                   SETON                                        32
104100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104200030721     C                             + ' ' + 'VABQFT'
104300030721     C                   ENDIF
104400030721     C                   ENDIF
104500030721     C*** NCD
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
104700030721     C                   EVAL      PiStr=SkSplitCSV(i)
104800030721     C                   EXSR      CHKNUM
104900030721     C                   IF        PiInt=*on      AND
105000030721     C                             PiVal<=9999999
105100030721     C                   Z-ADD     PiVal         VABNCD
105200030721     C                   ELSE
105300030721     C                   SETON                                        32
105400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105500030721     C                             + ' ' + 'VABNCD'
105600030721     C                   ENDIF
105700030721     C                   ENDIF
105800030721     C*** NCA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
106000030721     C                   EVAL      PiStr=SkSplitCSV(i)
106100030721     C                   EXSR      CHKNUM
106200030721     C                   IF        PiInt=*on      AND
106300030721     C                             PiVal<=9999999
106400030721     C                   Z-ADD     PiVal         VABNCA
106500030721     C                   ELSE
106600030721     C                   SETON                                        32
106700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106800030721     C                             + ' ' + 'VABNCA'
106900030721     C                   ENDIF
107000030721     C                   ENDIF
107100030721     C*** ZNC
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on AND
107600030721     C                             PiVal<=99
107700030721     C                   Z-ADD     PiVal         VABZNC
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108100030721     C                             + ' ' + 'VABZNC'
108200030721     C                   ENDIF
108300030721     C                   ENDIF
108400030721     C*** DCR
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiInt=*on       AND
108900030721     C                             PiVal<=99999999
109000030721     C                   Z-ADD     PiVal         VABDCR
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400030721     C                             + ' ' + 'VABDCR'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** HCR
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on   AND
110200030721     C                             PiVal<=9999
110300030721     C                   Z-ADD     PiVal         VABHCR
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110700030721     C                             + ' ' + 'VABHCR'
110800030721     C                   ENDIF
110900030721     C                   ENDIF
111000030721     C*** VMD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiNum=*on
111500030721     C                   Z-ADD     PiVal         VABVMD
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900030721     C                             + ' ' + 'VABVMD'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** ANT
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on        AND
112700030721     C                             PiVal<=999999999
112800030721     C                   Z-ADD     PiVal         VABANT
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABANT'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** CMO
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on
114000030721     C                   Z-ADD     PiVal         Num5_0
114100030721     C                   MOVEL(p)  Num5_0        VABCMO
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      VABCMO = *zeros
114500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114600030721     C                             + ' ' + 'VABCMO'
114700030721     C                   ENDIF
114800030721     C                   ENDIF
114900030715     C*
115000030715     C                   ENDIF
115100030715     C                   EVAL      i = i + 1
115200030715     C                   ENDDO
115300070713     C*
115400070718     C*******                           ********
115500070718     C* CONSIDERAZIONI PARTOCOLARI FUORI CICLO *
115600070718     C*******                           ********
115700070713     C*
115800070925     C                   Z-ADD     VABRMN        VABNSP
115900040726     C*
116000070713     C*******************************
116100070713     C*
116200040726     C* Considerazioni personalizzate x il traduttore corrente
116300060306     C* Reperisco la provincia dal CAP e dalla località
116400060306     C                   IF        VABPRD = *blanks AND
116500060306     C                             VABCAD <> *blanks
116600060306     C                   CLEAR                   TISI95DS
116700060306     C                   EVAL      I95TCN = '3'
116800060306     C                   Z-ADD     datcor        I95DAT
116900060306     C                   EVAL      I95CAP = VABCAD
117000060306     C                   EVAL      I95LOC = VABLOD
117100060306     C                   CALL      'TISI95R'
117200060306     C                   PARM                    TISI95DS
117300060306     C                   EVAL      VABPRD = O95PRV
117400060306     C                   ENDIF
117500040726     C*
117600040726     C* Se NSP nn valorizzato considero errore bloccante in traduzione
117700040726     C                   IF        VABNSP = *zeros
117800040726     C                   SETON                                        31
117900040726     C                   ENDIF
118000040726     C*
118100040726     C* Se LNP nn valorizzato considero errore bloccante in traduzione
118200040726     C                   IF        VABLNP = *zeros
118300040726     C                   SETON                                        31
118400040726     C                   ENDIF
118500040726     C*
118600040726     C* Eseguo particolari considerazioni in quanto RMA e RMN cerco x quanto possibile
118700040726     C* d valorizzarli sempre entrambi
118800040726     C                   IF        VABNSP <> *zeros  AND VABRMN = *zeros
118900040726     C                   Z-ADD     VABNSP        VABRMN
119000040726     C                   ENDIF
119100040726     C                   IF        VABRMN <> *zeros  AND VABRMA = *blanks
119200040726     C                   MOVEL     VABRMN        VABRMA
119300040726     C                   ENDIF
119400040726     C*
119500040726     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
119600040726     C                   IF        VABCAS > *zeros
119700040726     C                   IF        VABVCA = *blanks
119800040726     C                   EVAL      VABVCA = 'EUR'
119900070629     C                   EVAL      VABTIC = *blanks
120000040726     C                   ENDIF
120100040726     C                   IF        VABCBO = '1'
120200040726     C                   EVAL      VABCBO = '4'
120300040726     C                   ENDIF
120400040726     C                   IF        VABCBO = '2'
120500040726     C                   EVAL      VABCBO = '6'
120600040726     C                   ENDIF
120700040726     C                   ENDIF
120800020204     C*
120900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
121000020204     C                   EXSR      CHKIMPDIV
121100030715     C                   ENDIF
121200020204     C*
121300010607     C                   ENDSR
121400010607     C*----------------------------------------------------*
121500010601
121600020204
121700020204     C*----------------------------------------------------*
121800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
121900020204     C*----------------------------------------------------*
122000020204     C     CHKIMPDIV     BEGSR
122100020204     C*
122200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
122300020204     C                   Z-ADD     *zeros        wrkDec            9 9
122400020204     C*
122500020204     C* Come prima cosa effettuo considerazioni sulla divisa
122600020204     C                   IF        vabIAS > *zeros
122700020204     C                   IF        vabVAS <> 'EUR'
122800020204     C                   EVAL      vabVAS =  'ITL'
122900020204     C                   ENDIF
123000020204     C                   ENDIF
123100020204     C*
123200020204     C                   IF        vabCAS > *zeros
123300020204     C                   IF        vabVCA <> 'EUR'
123400020204     C                   EVAL      vabVCA =  'ITL'
123500020204     C                   ENDIF
123600020204     C                   ENDIF
123700020204     C*
123800020204     C                   IF        vabVMD > *zeros
123900020204     C                   IF        vabVAD <> 'EUR'
124000020204     C                   EVAL      vabVAD =  'ITL'
124100020204     C                   ENDIF
124200020204     C                   ENDIF
124300020204     C*
124400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
124500020204     C                   Z-ADD     vabIAS        wrkDec
124600020204     C                   IF        wrkDec > *zeros
124700020204     C                   IF        vabVAS = 'ITL'
124800020204     C                   EVAL      vabIAS = *zeros
124900020204     C                   ENDIF
125000020204     C                   ENDIF
125100020204     C*
125200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
125300020204     C                   Z-ADD     vabCAS        wrkDec
125400020204     C                   IF        wrkDec > *zeros
125500020204     C                   IF        vabVCA = 'ITL'
125600020204     C                   EVAL      vabCAS = *zeros
125700020204     C                   ENDIF
125800020204     C                   ENDIF
125900020204     C*
126000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
126100020204     C                   Z-ADD     vabVMD        wrkDec
126200020204     C                   IF        wrkDec > *zeros
126300020204     C                   IF        vabVAD = 'ITL'
126400020204     C                   EVAL      vabVMD = *zeros
126500020204     C                   ENDIF
126600020204     C                   ENDIF
126700020204     C*
126800020204     C                   ENDSR
126900020204     C***
127000020204
127100010330
127200010330
127300010330     C*----------------------------------------------------*
127400010330     C*  CONTROLLO NUMERICITA' CAMPI
127500010330     C*----------------------------------------------------*
127600010330     C     CHKNUM        BEGSR
127700010330     C*
127800010606     C                   IF        PiDecChr = *blanks
127900030715     C                   EVAL      PiDecChr = CharNUM
128000010606     C                   ENDIF
128100010606     C*
128200010606     C                   CALL(e)   'ISNUMERIC'
128300010330     C                   PARM                    PiStr            30
128400010606     C                   PARM                    PiDecChr          1
128500010330     C                   PARM      *ZEROS        PiVal            30 9
128600010330     C                   PARM      '0'           PiInt             1
128700010330     C                   PARM      '0'           PiNum             1
128800010330     C                   IF        %error
128900010606     C                   EVAL      PiNum=*off
129000010330     C                   ENDIF
129100010330     C*
129200010330     C                   ENDSR
129300010330     C***
129400010330
129500010601
129600010601
129700010601
129800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
129900010601     C     repfil        BEGSR
130000010601     C*
130100010601     C                   if        invfil = *zeros and
130200010601     C                             depfil > *zeros and
130300010629     C                             (vinflg = *blanks or
130400010629     C                              vinflg = *zeros)
130500010601     C
130600010601     C                   eval      invfil = depfil
130700010601     C                   endif
130800010601     C*
130900010601     C                   if        depfil <> invfil and
131000010601     C                             invfil > *zeros
131100010601     C                   eval      flgMulti = '1'
131200010601     C                   if        vinflg = *blanks
131300010601     C                   add       1             cntNonEl
131400010601     C                   endif
131500010601     C                   endif
131600010601     C*
131700010601     C                   if        vinflg = '2'
131800010601     C                   eval      flgStato = '2'
131900010601     C                   endif
132000010601     C*
132100010601     C                   ENDSR
132200010601     C***
132300010601
132400010601
132500010601
132600010330
132700010330
132800010330
132900990920      /TITLE Invio dei dati al punto operativo.
133000000613     C     invio         BEGSR
133100990920     C*
133200990920     C                   reset                   dscmz
133300010601     C                   move      invfil        cmzdst
133400990920     C                   eval      cmzfld = vlrfou
133500990920     C                   eval      cmzmbd = vlrhdl
133600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
133700000710     C                   if        prmfir = *blanks
133800021025     C                   eval      cmzfla = 'FIVAB00F'
133900021025     C                   eval      cmzmba = 'FIVAB00F'
134000000710     C                   else
134100000710     C                   eval      cmzfla = prmfir
134200000710     C                   eval      cmzmba = prmfir
134300000710     C                   endif
134400990920     C                   eval      cmznrr = *zeros
134500990920     C                   move      §ctrok        cmznrr
134600021018     C                   eval      cmzlba = vlrfl1
134700990920     C                   call(e)   'TIS711C'
134800990920     C                   parm                    dscmz
134900990921     C                   parm      *blanks       esito
135000990920     C                   if        %error
135100990920     C                             or cmzerr = '1'
135200990921     C                             or esito  = '1'
135300000710     C                   eval      wrkesito = '3'
135400990920     C                   endif
135500990920     C*
135600000613     C                   ENDSR
135700990910
135800010601
135900010601
136000010601
136100010601
136200010601      /TITLE Invio dei dati al punto operativo.
136300010601     C     opeini        BEGSR
136400010601     C*
136500010601     C* Inizializzo flag e contatori operativi
136600010601     C                   movel     '0'           flgGiro           1
136700010601     C                   movel     '0'           flgMulti          1
136800010601     C                   movel     '1'           flgStato          1
136900010615     C                   movel     '0'           flgOk             1
137000010601     C                   z-add     *zeros        cntNonEl         10 0
137100010601     C                   z-add     *zeros        depfil            3 0
137200010601     C                   z-add     *zeros        invfil            3 0
137300010601     C*
137400010601     C                   ENDSR
137500010601     C***
137600010601
137700010601
137800050421
137900050421
138000050421     C     *pssr         BEGSR
138100050421     C*
138200050421     C                   eval      wrkesito = '2'
138300050421     C*
138400050421     C                   ENDSR     '*CANCL'
138500050421     C***
138600050421
138700010601
138800010330
138900010330
139000000613     C     *inzsr        BEGSR
139100990910     C*
139200990910     C     *entry        plist
139300990920     C                   parm                    tivlrds
139400990921     C                   parm      wrkesito      esito
139500000724     C                   parm                    prmlit
139600000710     C                   parm                    prmfir
139700010330     C*
139800010330     C* CALCOLA LA DATA CORRENTE
139900010330     C                   time                    wn14             14 0
140000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
140100010330     C                   z-add     wn8           g08dat
140200010330     C                   z-add     *zeros        g08inv
140300010330     C                   movel     '0'           g08err
140400010330     C                   call      'XSRDA8'
140500010330     C                   parm                    wlbda8
140600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
140700000613     C*
140800000613     C                   ENDSR
140900000613     C***
141000040726** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
141100070925FILL1;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABNCL;VABPKB;VABRMN;FILL2;
141200070925FILL3
141300070713
141400050321
141500060215
