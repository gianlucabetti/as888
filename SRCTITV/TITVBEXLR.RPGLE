000100101018      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200130220     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500101018     FEDIVABwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400080222     D tisi95ds      e ds
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000030715     D*------------------
002100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002200030715     D*------------------
002300030715     D SkSplitFLD      S             10    DIM(100)
002400030715     D SkSplitCSV      S            256    DIM(100)
002500030715     D CharCSV         S              1
002600030715     D CharTXT         S              1
002700030715     D CharNUM         S              1
002800090403     D CharSOS         S              1
002900030715     D posDa           S              3  0 INZ(*zeros)
003000030715     D posA            S              3  0 INZ(*zeros)
003100030715     D i               s              3  0 INZ(1)
003200030715     D wGiro           s              1  0 INZ(*zeros)
003300030822     D*------------------
003400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003500030822     D*------------------
003600030822     D posDaDft        S              3  0 INZ(*zeros)
003700030822     D posADft         S              3  0 INZ(*zeros)
003800030822     D j               s              3  0 INZ(1)
003900030822     D wGiroDft        s              1  0 INZ(*zeros)
004000031201     D*------------------
004100031201     D* Costanti
004200031201     D*------------------
004300031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
004400031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
004500010330
004600010330
004700000913     C                   reset                   rrnum
004800990921     C                   reset                   esito
004900990921     C                   reset                   wrkesito
005000010601     C*
005100010601     C                   exsr      opeini
005200010605     C                   exsr      rwvab
005300010601     C*
005400010601     C                   seton                                        lr
005500010601
005600010601
005700010601
005800010601
005900010601     C*--------------------------------------------------------
006000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
006100010601     C*--------------------------------------------------------
006200010601     C     PREELA        BEGSR
006300010601     C*
006400010601     C* SE OCCORRE SPEDIRE IN FILIALE
006500010601     C                   if        invfil <> *zeros and
006600010601     C                             flgGiro = '0'
006700010601     C*
006800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006900010601     C                   eval      flgGiro = '1'
007000010601     C*
007100010601     C                   endif
007200010601     C*
007300010601     C                   ENDSR
007400010601     C***
007500010601
007600010601
007700010601
007800010601
007900010601
008000010601
008100010601
008200010601     C*--------------------------------------------------------
008300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008400010601     C*--------------------------------------------------------
008500010601     C     ENDELA        BEGSR
008600080222     C*
008700080222     C* Effettuo lancio TISI95 solo x chiusura
008800080222     C                   CLEAR                   TISI95DS
008900080222     C                   EVAL      I95TLA = 'C'
009000080222     C                   CALL      'TISI95R'
009100080222     C                   PARM                    TISI95DS
009200000616     C*
009300010601     C                   ENDSR
009400010601     C***
009500000613
009600010330
009700010601
009800010601
009900010601
010000010330     C*--------------------------------------------------------
010100101018     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
010200010330     C*--------------------------------------------------------
010300010605     C     RWVAB         BEGSR
010400010330     C*
010500010330     C                   if        not %open(tivin00r)
010600010330     C                   open      tivin00r
010700010330     C                   endif
010800101018     C                   if        not %open(edivabwr)
010900101018     C                   open      edivabwr
011000010330     C                   endif
011100010330     C*
011200010604     C                   clear                   §CTROK
011300010604     C                   clear                   §CTRMO
011400010604     C                   clear                   §CTRNO
011500010330     C*
011600030822     C                   do        *HIVAL
011700010330     C*
011800030822     C                   read      tivin00r                               70
011900010618     C*
012000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012100010618     C                   if        vinflg = '1'
012200010618     C                   eval      flgOk = '1'
012300010618     C                   endif
012400010618     C*
012500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012600040510     C                   z-add     *zeros        wLenVINDTA        4 0
012700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
012800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
012900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013100040510     C                   z-add     1             wFlgVINDTA
013200040510     C                   leave
013300040510     C                   endif
013400040510     C                   enddo
013500040510     C*
013600040218     C                   if        vindta > *blanks AND
013700040510     C                             wFlgVINDTA = 1
013800010330     C                   add       1             rrnum
013900010330     C*
014000010601     C                   if        *in70 = *off and
014100010330     C                             (vinflg = *blanks
014200010330     C                              or vinflg = '0'
014300010330     C                              or vinflg = '2')
014400010330     C*
014500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014600010711     C                   if        vinflg = *blanks or vinflg = '0'
014700010711     C                   clear                   vinmsg
014800010711     C                   endif
014900010601     C*
015000010330     C                   exsr      inzvar
015100100419     C                   exsr      normCSV
015200090403     C*
015300090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015400090403     C* separatori campi
015500090403     C                   if        CharTXT <> *blanks
015600090403     C                   z-add     *zeros        wLenVINDTA        4 0
015700090403     C                   z-add     *zeros        wFlgVINDTA        1 0
015800090403     C                   setoff                                       80
015900090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016000090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016200090403     C                   if        not *in80
016300090403     C                   seton                                        80
016400090403     C                   else
016500090403     C                   setoff                                       80
016600090403     C                   endif
016700090403     C                   endif
016800090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
016900090403     C                             and *in80 = *on
017000090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017100090403     C                   endif
017200090403     C                   enddo
017300090403     C                   endif
017400090403     C*
017500010605     C                   exsr      impvab
017600010601     C*
017700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
017800010605     C                   eval      depfil = VABLNP
017900010601     C                   exsr      repfil
018000060331     C                   if        VABFGS = *zeros
018100010601     C                   if        depfil = invfil
018200021025     C                   if        vlrpoi = 999
018300030822     C                   move(P)   invfil        VABFGS
018400021025     C                   else
018500030822     C                   move(P)   vlrpoi        VABFGS
018600021025     C                   endif
018700050113     C                   endif
018800060331     C                   endif
018900010601     C*
019000010601     C                   exsr      PREELA
019100010601     C*
019200010604     C* Ebbene...
019300010604     C*
019400030822     C  N31              add       1             §CTROK            7 0
019500030822     C   32              add       1             §CTRMO            7 0
019600030822     C   31              add       1             §CTRNO            7 0
019700030822     C                   if        wGiro = 2
019800101018     C* VALORIZZO CAMPI RELATIVI AL "CMR"
019900101018     C                   EVAL      VABCMR = wCMR
020000101018     C                   EVAL      VABDCM = DATCOR
020100101018     C                   EVAL      VABDTS = DATCOR
020200101018     C                   EVAL      VABHMS = ORACOR
020300101018     C                   EVAL      VABCNT = 1
020400101018     C*
020500101018     C  N31              write     EDIVAB00
020600030822     C                   endif
020700010604     C*
020800010604     C                   if        *in31 = *off and
020900010604     C                             *in32 = *off
021000010604     C                   eval      vinflg = '1'
021100010604     C                   else
021200010604     C                   eval      vinflg = '2'
021300010604     C                   endif
021400010601     C*
021500010604     C                   endif
021600010604     C*
021700010330     C                   else
021800010330     C                   eval      vinflg = '1'
021900010330     C                   endif
022000010601     C*
022100010601     C  N70              update    tivin000
022200010330     C*
022300030822     C  N70              enddo
022400010601     C
022500010601     C*
022600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022700010601     C                   if        cntNonEl = *zeros or
022800010601     C                             flgMulti = '0'
022900010330     C* Se non ci sono record con errori ...
023000010601     C                   if        §ctrno = 0 and
023100010604     C                             §ctrmo = 0 and
023200010601     C                             flgStato <> '2'
023300010330     C* ... restituisco esito OK.
023400010330     C                   eval      wrkesito = '0'
023500010330     C                   else
023600010330     C                   if        §ctrok > 0
023700010330     C                   eval      wrkesito = '1'
023800010330     C                   else
023900010615     C                   if        flgOk = '0'
024000010615     C                   eval      wrkesito = '2'
024100010615     C                   else
024200010615     C                   eval      wrkesito = '6'
024300010615     C                   endif
024400010330     C                   endif
024500010330     C                   endif
024600010601     C                   else
024700010601     C                   eval      wrkesito = '9'
024800010601     C                   endif
024900010330     C*
025000010330     C                   if        %open(tivin00r)
025100010330     C                   close     tivin00r
025200010330     C                   endif
025300101018     C                   if        %open(edivabwr)
025400101018     C                   close     edivabwr
025500010330     C                   endif
025600010601     C*
025700010601     C                   if        vlrpoi <> 999
025800010601     C                   eval      invfil = vlrpoi
025900010601     C                   endif
026000010330     C*
026100010330     C                   if        §ctrok > 0
026200010601     C                             and invfil > *zeros
026300010330     C                   exsr      invio
026400010330     C                   endif
026500010601     C*
026600010618     C                   if        flgGiro = '1'
026700010601     C                   exsr      endela
026800010618     C                   endif
026900010330     C*
027000010330     C                   ENDSR
027100010330     C***
027200010601
027300010601
027400010601
027500010330     C*----------------------------------------------------*
027600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027700010330     C*----------------------------------------------------*
027800010330     C     INZVAR        BEGSR
027900010330     C*
028000030822     C* Inizializzo variabili di wrk
028100020204     C                   Z-ADD     *zeros        Num5_0            5 0
028200090403     C                   z-add     *zeros        invfil
028300030822     C*
028400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
028500101018     C                   CLEAR                   EDIVAB00
028600030822     C                   CLEAR                   SkSplitCSV
028700030822     C                   EVAL      i = 1
028800030822     C                   EVAL      posDa = *zeros
028900030822     C                   EVAL      posA  = *zeros
029000030822     C*
029100030822     C* Reimposto i valori di default
029200030822     C                   EXSR      DEFCAM
029300030822     C*
029400010330     C                   ENDSR
029500010330     C*----------------------------------------------------*
029600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029700010330     C*----------------------------------------------------*
029800010330     C     DEFCAM        BEGSR
029900030822     C*
030000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030100030715     C* e delimitatore testo.
030200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
030300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030500090403     C*
030600090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030700090403     C                   EVAL      CharSOS = CharNUM
030800030822     C*
030900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
031000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
031100030715     C                   SETON                                        50
031200030715     C                   ELSE
031300030715     C                   SETOFF                                       50
031400030715     C                   ENDIF
031500030822     C*
031600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031700030822     C                   EVAL      posDaDft = 1
031800030822     C                   EVAL      posADft  = 0
031900030822     C                   EVAL      wGiroDft = 0
032000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032100030822     C                             posDaDft > 0
032200030822     C*
032300030822     C* Gestisco il 1° giro
032400030822     C                   IF        wGiroDft = 0
032500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
032600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032700030822     C* Incremento il contatore dei "giri"
032800030822     C                   EVAL      wGiroDft = 1
032900030822     C                   ELSE
033000030822     C                   EVAL      posDaDft = posADft
033100030822     C                   ENDIF
033200030822     C* Eseguo lo scan x trovare la fine del campo corrente
033300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033400030822     C*
033500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
033600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
033700030822     C                   IF        posDaDft > 0 AND
033800030822     C                             posADft  > 0
033900060331     C* FGS
034000060331     C                   IF        %subst(
034100060331     C                             %subst(vlrppt:posDaDft+1:
034200060331     C                             posADft-posDaDft-1):1:3)
034300060331     C                             = 'FGS'
034400060331     C                   EVAL      PiStr=%trim(%subst(
034500060331     C                             %subst(vlrppt:posDaDft+1:
034600060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034700060331     C                   EXSR      CHKNUM
034800060331     C                   IF        PiInt=*on
034900060331     C                   Z-ADD     PiVal         VABFGS
035000060331     C                   ENDIF
035100060331     C                   ENDIF
035200030822     C* NCL
035300030822     C                   IF        %subst(
035400030822     C                             %subst(vlrppt:posDaDft+1:
035500030822     C                             posADft-posDaDft-1):1:3)
035600030822     C                             = 'NCL'
035700030822     C                   EVAL      PiStr=%trim(%subst(
035800030822     C                             %subst(vlrppt:posDaDft+1:
035900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036000030822     C                   EXSR      CHKNUM
036100030822     C                   IF        PiInt=*on
036200030822     C                   Z-ADD     PiVal         VABNCL
036300030822     C                   ENDIF
036400030822     C                   ENDIF
036500030822     C* CCM
036600030822     C                   IF        %subst(
036700030822     C                             %subst(vlrppt:posDaDft+1:
036800030822     C                             posADft-posDaDft-1):1:3)
036900030822     C                             = 'CCM'
037000030822     C                   EVAL      PiStr=%trim(%subst(
037100030822     C                             %subst(vlrppt:posDaDft+1:
037200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037300030822     C                   EXSR      CHKNUM
037400030822     C                   IF        PiInt=*on
037500030822     C                   Z-ADD     PiVal         VABCCM
037600030822     C                   ENDIF
037700030822     C                   ENDIF
037800030822     C* LNP
037900030822     C                   IF        %subst(
038000030822     C                             %subst(vlrppt:posDaDft+1:
038100030822     C                             posADft-posDaDft-1):1:3)
038200030822     C                             = 'LNP'
038300030822     C                   EVAL      PiStr=%trim(%subst(
038400030822     C                             %subst(vlrppt:posDaDft+1:
038500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038600030822     C                   EXSR      CHKNUM
038700030822     C                   IF        PiInt=*on
038800030822     C                   Z-ADD     PiVal         VABLNP
038900030822     C                   ENDIF
039000030822     C                   ENDIF
039100030822     C* NRS
039200030822     C                   IF        %subst(
039300030822     C                             %subst(vlrppt:posDaDft+1:
039400030822     C                             posADft-posDaDft-1):1:3)
039500030822     C                             = 'NRS'
039600030822     C                   EVAL      PiStr=%trim(%subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039900030822     C                   EXSR      CHKNUM
040000030822     C                   IF        PiInt=*on
040100030822     C                   Z-ADD     PiVal         VABNRS
040200030822     C                   ENDIF
040300030822     C                   ENDIF
040400030822     C* CTR
040500030822     C                   IF        %subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):1:3)
040800030822     C                             = 'CTR'
040900030822     C                   EVAL      PiStr=%trim(%subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200030822     C                   EXSR      CHKNUM
041300030822     C                   IF        PiInt=*on
041400030822     C                   Z-ADD     PiVal         VABCTR
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030822     C* PKB
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'PKB'
042200030822     C                   EVAL      PiStr=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   EXSR      CHKNUM
042600030822     C                   IF        PiNum=*on
042700030822     C                   Z-ADD     PiVal         VABPKB
042800030822     C                   ENDIF
042900030822     C                   ENDIF
043000030822     C* VLB
043100030822     C                   IF        %subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):1:3)
043400030822     C                             = 'VLB'
043500030822     C                   EVAL      PiStr=%trim(%subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800030822     C                   EXSR      CHKNUM
043900030822     C                   IF        PiNum=*on
044000030822     C                   Z-ADD     PiVal         VABVLB
044100030822     C                   ENDIF
044200030822     C                   ENDIF
044300030822     C* QFT
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'QFT'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiNum=*on
045300030822     C                   Z-ADD     PiVal         VABQFT
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* CBO
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'CBO'
046100030822     C                   EVAL      VABCBO=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   ENDIF
046500030822     C* TSP
046600030822     C                   IF        %subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):1:3)
046900030822     C                             = 'TSP'
047000030822     C                   EVAL      VABTSP=%trim(%subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047300030822     C                   ENDIF
047400030822     C* VAS
047500030822     C                   IF        %subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):1:3)
047800030822     C                             = 'VAS'
047900030822     C                   EVAL      VABVAS=%trim(%subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048200030822     C                   ENDIF
048300030822     C* VCA
048400030822     C                   IF        %subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):1:3)
048700030822     C                             = 'VCA'
048800030822     C                   EVAL      VABVCA=%trim(%subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049100030822     C                   ENDIF
049200030822     C* GCA
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'GCA'
049700030822     C                   EVAL      VABGCA=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   ENDIF
050100030822     C* CTM
050200030822     C                   IF        %subst(
050300030822     C                             %subst(vlrppt:posDaDft+1:
050400030822     C                             posADft-posDaDft-1):1:3)
050500030822     C                             = 'CTM'
050600030822     C                   EVAL      VABCTM=%trim(%subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050900030822     C                   ENDIF
051000030822     C* FFD
051100030822     C                   IF        %subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):1:3)
051400030822     C                             = 'FFD'
051500030822     C                   EVAL      VABFFD=%trim(%subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800030822     C                   ENDIF
051900030822     C* VAD
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'VAD'
052400030822     C                   EVAL      VABVAD=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   ENDIF
052800030822     C* GMA
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'GMA'
053300030822     C                   EVAL      VABGMA=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   ENDIF
053700030822     C* GGA
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'GGA'
054200030822     C                   EVAL      VABGGA=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   ENDIF
054600030822     C* GVA
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'GVA'
055100030822     C                   EVAL      VABGVA=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   ENDIF
055500030822     C* TC1
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'TC1'
056000030822     C                   EVAL      VABTC1=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   ENDIF
056400030822     C* TC2
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'TC2'
056900030822     C                   EVAL      VABTC2=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* ...
057400030822     C                   ENDIF
057500030822     C                   ENDDO
057600020204     C*
057700010330     C                   ENDSR
057800010607     C*----------------------------------------------------*
057900101018     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
058000010607     C*----------------------------------------------------*
058100010607     C     IMPVAB        BEGSR
058200010607     C*
058300010607     C                   SETOFF                                       3132
058400030515     C*
058500010607     C                   MOVEL     datcor        VABAAS
058600010607     C                   MOVE      datcor        VABMGS
058700010607     C*
058800030715     C**********
058900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
059000030715     C**********
059100030715     C                   IF        wGiro = *zeros
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
063800030715     C                   ELSE
063900030715     C                   EVAL      wGiro = 2
064000031204     C**********
064100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
064200031204     C* d separatore campo
064300031204     C**********
064400040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
064500040304     C                   EVAL      vindta = ' ' + vindta
064600040304     C                   ENDIF
064700040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
064800040304     C***                EVAL      vindta = %subst(vindta:2)
064900040304     C***                ENDDO
065000031204     C*
065100031204     C                   Z-ADD     *zeros        lunghInput        4 0
065200040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
065300040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
065400040304     C                   EVAL      vindta = vindta + ' '
065500040304     C                   ENDIF
065600040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
065700040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
065800040304     C***                EVAL      lunghInput = %len(%trim(vindta))
065900040304     C***                ENDDO
066000030715     C**********
066100030715     C* Effettuo lo split del campo dati d input
066200030715     C**********
066300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
066400040324     C                   DOW       posDa <= %len(%trimr(vindta))
066500030715     C*
066600030715     C* Gestisco il 1° campo
066700030715     C                   IF        i = 1
066800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
066900030715     C                   EVAL      posDa = 1
067000030715     C* Eseguo lo scan x trovare la fine del primo campo
067100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
067200030715     C* A questo punto "estraggo" il campo corrente
067300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
067400030715     C                                                    (posA-posDa))
067500030715     C* X i campi successivi al 1°
067600030715     C                   ELSE
067700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
067800030715     C                   EVAL      posDa = posA + 1
067900031204     C* Verifico che nn vi sia il campo nn valorizzato
068000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
068100031204     C* Se campo nn valorizzato skippo al prossimo
068200031204     C                   EVAL      SkSplitCSV(i) = *blanks
068300031204     C                   EVAL      posA  = posA + 1
068400031204     C                   ELSE
068500030715     C* Eseguo lo scan x trovare la fine del campo corrente
068600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068700030715     C* Gestisco l'ultimo campo
068800030715     C                   IF        posA = *zeros
068900040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
069000030715     C* A questo punto "estraggo" il campo corrente
069100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069200030715     C                                                    (posA-posDa))
069300030715     C                   LEAVE
069400030715     C                   ELSE
069500030715     C* A questo punto "estraggo" il campo corrente
069600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069700030715     C                                                    (posA-posDa))
069800030715     C                   ENDIF
069900031204     C                   ENDIF
070000030715     C                   ENDIF
070100030715     C* Incremento il contatore d campo
070200030715     C                   EVAL      i = i +1
070300030715     C                   ENDDO
070400030715     C*
070500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
070600030715     C* delimitazione testo
070700030715     C                   EVAL      i = 1
070800030715     C                   DOW       i < %elem(SkSplitCSV)
070900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
071000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
071100030715     C                   EVAL      i = i + 1
071200030715     C                   ENDDO
071300030715     C*
071400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
071500030715     C                   EVAL      i = 1
071600030715     C                   DOW       i < %elem(SkSplitFLD)
071700030715     C*
071800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
071900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
072000030715     C                             SkSplitCSV(i) <> *blanks
072100030721     C*** ATB
072200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
072300030721     C                   EVAL      VABATB = SkSplitCSV(i)
072400030721     C                   ENDIF
072500030715     C*** RSD
072600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
072700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
072800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
073000030715     C* ==
073100030715     C                   ENDIF
073200030721     C*** RD2
073300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
073400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
073500030721     C                   ENDIF
073600030715     C*** IND
073700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
073800030715     C                   EVAL      VABIND = SkSplitCSV(i)
073900030715     C                   ENDIF
074000030715     C*** LOD
074100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
074200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
074300030715     C                   ENDIF
074400030715     C*** PRD
074500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
074600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
074700030715     C                   ENDIF
074800030721     C*** NZD
074900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
075000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
075100030721     C                   ENDIF
075200030715     C*** RMA
075300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
075400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
075500030715     C                   ENDIF
075600030715     C*** NOT
075700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
075800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
075900030715     C                   ENDIF
076000030715     C*** NT2
076100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
076200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
076300030715     C                   ENDIF
076400030715     C*** VCA
076500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
076600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
076700030715     C                   ENDIF
076800030715     C*** VAS
076900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
077000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
077100030715     C                   ENDIF
077200030715     C*** FFD
077300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
077400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
077500030715     C                   ENDIF
077600030715     C*** CBO
077700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
077800030715     C                   EVAL      VABCBO = SkSplitCSV(i)
077900030715     C                   ENDIF
078000030721     C*** GC1
078100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
078200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
078300030721     C                   ENDIF
078400030721     C*** GC2
078500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
078600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
078700030721     C                   ENDIF
078800030721     C*** TSP
078900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
079000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
079100030721     C                   ENDIF
079200030721     C*** NAS
079300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
079400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
079500030721     C                   ENDIF
079600030721     C*** TIC
079700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
079800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
079900030721     C                   ENDIF
080000030721     C*** GCA
080100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
080200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
080300030721     C                   ENDIF
080400030721     C*** XCO
080500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
080600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
080700030721     C                   ENDIF
080800030721     C*** CTM
080900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
081000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
081100030721     C                   ENDIF
081200030721     C*** TCR
081300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
081400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
081500030721     C                   ENDIF
081600030721     C*** CTS
081700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
081800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
081900030721     C                   ENDIF
082000030721     C*** FTM
082100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
082200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
082300030721     C                   ENDIF
082400030721     C*** VAD
082500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
082600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
082700030721     C                   ENDIF
082800030721     C*** GMA
082900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
083000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
083100030721     C                   ENDIF
083200030721     C*** GGA
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
083400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030721     C*** GVA
083700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
083800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
083900030721     C                   ENDIF
084000030721     C*** TC1
084100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
084200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
084300030721     C                   ENDIF
084400030721     C*** TC2
084500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
084600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
084700030721     C                   ENDIF
084800030721     C*** SCL
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
085000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030721     C*** RMO
085300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
085400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
085500030721     C                   ENDIF
085600030721     C*** NMO
085700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
085800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
085900030721     C                   ENDIF
086000030721     C*
086100070705     C* Reperisco quindi i campi numerici...
086200030721     C*** CAD
086300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
086400070705     C                   EVAL      VABCAD = SkSplitCSV(i)
086500030721     C                   ENDIF
086600030721     C*** RMN
086700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
086800030721     C                   EVAL      PiStr=SkSplitCSV(i)
086900030721     C                   EXSR      CHKNUM
087000030721     C                   IF        PiInt=*on
087100030721     C                   Z-ADD     PiVal         VABRMN
087200030721     C                   ELSE
087300030721     C                   SETON                                        32
087400030721     C                   EVAL      VABRMN = 1
087500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
087600030721     C                             + ' ' + 'VABRMN'
087700030721     C                   ENDIF
087800030721     C                   ENDIF
087900030721     C*** NSP
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
088100030721     C                   EVAL      PiStr=SkSplitCSV(i)
088200030721     C                   EXSR      CHKNUM
088300100517     C                   IF        PiInt=*on
088400030721     C                   Z-ADD     PiVal         VABNSP
088500030721     C                   ELSE
088600030721     C                   SETON                                        32
088700030721     C                   EVAL      VABNSP = *zeros
088800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
088900030721     C                             + ' ' + 'VABNSP'
089000030721     C                   ENDIF
089100030721     C                   ENDIF
089200030721     C*** NCL
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
089400030721     C                   EVAL      PiStr=SkSplitCSV(i)
089500030721     C                   EXSR      CHKNUM
089600030721     C                   IF        PiInt=*on
089700030721     C                   Z-ADD     PiVal         VABNCL
089800030721     C                   ELSE
089900030721     C                   SETON                                        32
090000030721     C                   EVAL      VABNCL = *zeros
090100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
090200030721     C                             + ' ' + 'VABNCL'
090300030721     C                   ENDIF
090400030721     C                   ENDIF
090500030721     C*** PKB
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
090700030721     C                   EVAL      PiStr=SkSplitCSV(i)
090800030721     C                   EXSR      CHKNUM
090900030721     C                   IF        PiNum=*on
091000030721     C                   Z-ADD     PiVal         VABPKB
091100030721     C                   ELSE
091200030721     C                   SETON                                        32
091300030721     C                   EVAL      VABPKB = *zeros
091400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091500030721     C                             + ' ' + 'VABPKB'
091600030721     C                   ENDIF
091700030721     C                   ENDIF
091800030721     C*** CAS
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
092000030721     C                   EVAL      PiStr=SkSplitCSV(i)
092100030721     C                   EXSR      CHKNUM
092200030721     C                   IF        PiNum=*on
092300030721     C                   Z-ADD     PiVal         VABCAS
092400030721     C                   ELSE
092500030721     C                   SETON                                        32
092600030721     C                   EVAL      VABCAS = *zeros
092700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092800030721     C                             + ' ' + 'VABCAS'
092900030721     C                   ENDIF
093000030721     C                   ENDIF
093100030721     C*** CCM
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
093300030721     C                   EVAL      PiStr=SkSplitCSV(i)
093400030721     C                   EXSR      CHKNUM
093500030721     C                   IF        PiInt=*on      AND
093600030721     C                             PiVal<=9999999 AND
093700030721     C                             PiVal>*zeros
093800030721     C                   Z-ADD     PiVal         VABCCM
093900030721     C                   ELSE
094000030721     C                   SETON                                        32
094100030721     C                   EVAL      VABCCM = *zeros
094200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094300030721     C                             + ' ' + 'VABCCM'
094400030721     C                   ENDIF
094500030721     C                   ENDIF
094600030721     C*** LNP
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
094800030721     C                   EVAL      PiStr=SkSplitCSV(i)
094900030721     C                   EXSR      CHKNUM
095000030721     C                   IF        PiInt=*on    AND
095100030721     C                             PiVal<=999   AND
095200030721     C                             PiVal>*zeros
095300030721     C                   Z-ADD     PiVal         VABLNP
095400030721     C                   ELSE
095500030721     C                   SETON                                        32
095600030721     C                   EVAL      VABLNP = *zeros
095700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095800030721     C                             + ' ' + 'VABLNP'
095900030721     C                   ENDIF
096000030721     C                   ENDIF
096100030721     C*** NRS
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
096300030721     C                   EVAL      PiStr=SkSplitCSV(i)
096400030721     C                   EXSR      CHKNUM
096500030721     C                   IF        PiInt=*on AND
096600030721     C                             PiVal<=99
096700030721     C                   Z-ADD     PiVal         VABNRS
096800030721     C                   ELSE
096900030721     C                   SETON                                        32
097000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097100030721     C                             + ' ' + 'VABNRS'
097200030721     C                   ENDIF
097300030721     C                   ENDIF
097400030721     C*** LNA
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
097600030721     C                   EVAL      PiStr=SkSplitCSV(i)
097700030721     C                   EXSR      CHKNUM
097800030721     C                   IF        PiInt=*on    AND
097900030721     C                             PiVal<=999   AND
098000030721     C                             PiVal>*zeros
098100030721     C                   Z-ADD     PiVal         VABLNA
098200030721     C                   ELSE
098300030721     C                   SETON                                        32
098400030721     C                   EVAL      VABLNA = *zeros
098500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098600030721     C                             + ' ' + 'VABLNA'
098700030721     C                   ENDIF
098800030721     C                   ENDIF
098900030721     C*** CTR
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
099100030721     C                   EVAL      PiStr=SkSplitCSV(i)
099200030721     C                   EXSR      CHKNUM
099300030721     C                   IF        PiInt=*on  AND
099400030721     C                             PiVal<=999
099500030721     C                   Z-ADD     PiVal         VABCTR
099600030721     C                   ELSE
099700030721     C                   SETON                                        32
099800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099900030721     C                             + ' ' + 'VABCTR'
100000030721     C                   ENDIF
100100030721     C                   ENDIF
100200030721     C*** IAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
100400030721     C                   EVAL      PiStr=SkSplitCSV(i)
100500030721     C                   EXSR      CHKNUM
100600030721     C                   IF        PiNum=*on
100700030721     C                   Z-ADD     PiVal         VABIAS
100800030721     C                   ELSE
100900030721     C                   SETON                                        32
101000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101100030721     C                             + ' ' + 'VABIAS'
101200030721     C                   ENDIF
101300030721     C                   ENDIF
101400030721     C*** VLB
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
101600030721     C                   EVAL      PiStr=SkSplitCSV(i)
101700030721     C                   EXSR      CHKNUM
101800030721     C                   IF        PiNum=*on
101900030721     C                   Z-ADD     PiVal         VABVLB
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102300030721     C                             + ' ' + 'VABVLB'
102400030721     C                   ENDIF
102500030721     C                   ENDIF
102600030721     C*** QFT
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
102800030721     C                   EVAL      PiStr=SkSplitCSV(i)
102900030721     C                   EXSR      CHKNUM
103000030721     C                   IF        PiNum=*on
103100030721     C                   Z-ADD     PiVal         VABQFT
103200030721     C                   ELSE
103300030721     C                   SETON                                        32
103400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103500030721     C                             + ' ' + 'VABQFT'
103600030721     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** NCD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on      AND
104300030721     C                             PiVal<=9999999
104400030721     C                   Z-ADD     PiVal         VABNCD
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABNCD'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** NCA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiInt=*on      AND
105600030721     C                             PiVal<=9999999
105700030721     C                   Z-ADD     PiVal         VABNCA
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106100030721     C                             + ' ' + 'VABNCA'
106200030721     C                   ENDIF
106300030721     C                   ENDIF
106400030721     C*** ZNC
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
106600030721     C                   EVAL      PiStr=SkSplitCSV(i)
106700030721     C                   EXSR      CHKNUM
106800030721     C                   IF        PiInt=*on AND
106900030721     C                             PiVal<=99
107000030721     C                   Z-ADD     PiVal         VABZNC
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107400030721     C                             + ' ' + 'VABZNC'
107500030721     C                   ENDIF
107600030721     C                   ENDIF
107700030721     C*** DCR
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
107900030721     C                   EVAL      PiStr=SkSplitCSV(i)
108000030721     C                   EXSR      CHKNUM
108100030721     C                   IF        PiInt=*on       AND
108200030721     C                             PiVal<=99999999
108300030721     C                   Z-ADD     PiVal         VABDCR
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700030721     C                             + ' ' + 'VABDCR'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** HCR
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on   AND
109500030721     C                             PiVal<=9999
109600030721     C                   Z-ADD     PiVal         VABHCR
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABHCR'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** VMD
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiNum=*on
110800030721     C                   Z-ADD     PiVal         VABVMD
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111200030721     C                             + ' ' + 'VABVMD'
111300030721     C                   ENDIF
111400030721     C                   ENDIF
111500030721     C*** ANT
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
111700030721     C                   EVAL      PiStr=SkSplitCSV(i)
111800030721     C                   EXSR      CHKNUM
111900030721     C                   IF        PiInt=*on        AND
112000030721     C                             PiVal<=999999999
112100030721     C                   Z-ADD     PiVal         VABANT
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500030721     C                             + ' ' + 'VABANT'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** CMO
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on
113300030721     C                   Z-ADD     PiVal         Num5_0
113400030721     C                   MOVEL(p)  Num5_0        VABCMO
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABCMO = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABCMO'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030715     C*
114300030715     C                   ENDIF
114400030715     C                   EVAL      i = i + 1
114500030715     C                   ENDDO
114600070705     C*
114700070705     C* Considerazioni particolari
114800070705     C*** NZD
114900070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
115000070705     C                   EVAL      VABNZD = *blanks
115100070705     C                   ENDIF
115200070705     C*** CAD
115300070705     C                   IF        VABNZD = *blanks
115400070705     C                   EVAL      PiStr=VABCAD
115500070705     C                   EXSR      CHKNUM
115600070705     C                   IF        PiInt=*on
115700070705     C                   Z-ADD     PiVal         Num5_0
115800070705     C                   MOVEL(p)  Num5_0        VABCAD
115900070705     C                   ELSE
116000070705     C                   SETON                                        32
116100070705     C                   EVAL      VABCAD = *zeros
116200070705     C                   EVAL      vinmsg = %trimr(vinmsg)
116300070705     C                             + ' ' + 'VABCAD'
116400070705     C                   ENDIF
116500070705     C                   ENDIF
116600080222     C*
116700080222     C* Se provincia nn valorizzata la reperisco
116800080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
116900080222     C                   IF        VABPRD = *blanks
117000080222     C                   CLEAR                   TISI95DS
117100080222     C                   EVAL      I95TCN = '3'
117200080222     C                   Z-ADD     datcor        I95DAT
117300080222     C                   EVAL      I95NAR = VABNZD
117400080222     C                   EVAL      I95CAP = VABCAD
117500080222     C                   EVAL      I95LOC = VABLOD
117600080222     C                   CALL      'TISI95R'
117700080222     C                   PARM                    TISI95DS
117800080222     C                   EVAL      VABPRD = O95PRV
117900080222     C                   ENDIF
118000020204     C*
118100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
118200020204     C                   EXSR      CHKIMPDIV
118300030715     C                   ENDIF
118400020204     C*
118500010607     C                   ENDSR
118600010607     C*----------------------------------------------------*
118700010601
118800020204
118900020204     C*----------------------------------------------------*
119000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
119100020204     C*----------------------------------------------------*
119200020204     C     CHKIMPDIV     BEGSR
119300060524     C*
119400060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
119500060524     C                   IF        vabCAS > *zeros
119600060524     C                   IF        vabCBO = '1'
119700060524     C                   EVAL      vabCBO = '4'
119800060524     C                   ENDIF
119900060524     C                   IF        vabCBO = '2'
120000060524     C                   EVAL      vabCBO = '6'
120100060524     C                   ENDIF
120200060524     C                   ENDIF
120300020204     C*
120400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
120500020204     C                   Z-ADD     *zeros        wrkDec            9 9
120600020204     C*
120700020204     C* Come prima cosa effettuo considerazioni sulla divisa
120800020204     C                   IF        vabIAS > *zeros
120900020204     C                   IF        vabVAS <> 'EUR'
121000020204     C                   EVAL      vabVAS =  'ITL'
121100020204     C                   ENDIF
121200020204     C                   ENDIF
121300020204     C*
121400020204     C                   IF        vabCAS > *zeros
121500020204     C                   IF        vabVCA <> 'EUR'
121600020204     C                   EVAL      vabVCA =  'ITL'
121700020204     C                   ENDIF
121800020204     C                   ENDIF
121900020204     C*
122000020204     C                   IF        vabVMD > *zeros
122100020204     C                   IF        vabVAD <> 'EUR'
122200020204     C                   EVAL      vabVAD =  'ITL'
122300020204     C                   ENDIF
122400020204     C                   ENDIF
122500020204     C*
122600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
122700020204     C                   Z-ADD     vabIAS        wrkDec
122800020204     C                   IF        wrkDec > *zeros
122900020204     C                   IF        vabVAS = 'ITL'
123000020204     C                   EVAL      vabIAS = *zeros
123100020204     C                   ENDIF
123200020204     C                   ENDIF
123300020204     C*
123400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
123500020204     C                   Z-ADD     vabCAS        wrkDec
123600020204     C                   IF        wrkDec > *zeros
123700020204     C                   IF        vabVCA = 'ITL'
123800020204     C                   EVAL      vabCAS = *zeros
123900020204     C                   ENDIF
124000020204     C                   ENDIF
124100020204     C*
124200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
124300020204     C                   Z-ADD     vabVMD        wrkDec
124400020204     C                   IF        wrkDec > *zeros
124500020204     C                   IF        vabVAD = 'ITL'
124600020204     C                   EVAL      vabVMD = *zeros
124700020204     C                   ENDIF
124800020204     C                   ENDIF
124900020204     C*
125000020204     C                   ENDSR
125100020204     C***
125200020204
125300010330
125400010330
125500010330     C*----------------------------------------------------*
125600010330     C*  CONTROLLO NUMERICITA' CAMPI
125700010330     C*----------------------------------------------------*
125800010330     C     CHKNUM        BEGSR
125900010330     C*
126000010606     C                   IF        PiDecChr = *blanks
126100030715     C                   EVAL      PiDecChr = CharNUM
126200010606     C                   ENDIF
126300010606     C*
126400010606     C                   CALL(e)   'ISNUMERIC'
126500010330     C                   PARM                    PiStr            30
126600010606     C                   PARM                    PiDecChr          1
126700010330     C                   PARM      *ZEROS        PiVal            30 9
126800010330     C                   PARM      '0'           PiInt             1
126900010330     C                   PARM      '0'           PiNum             1
127000010330     C                   IF        %error
127100010606     C                   EVAL      PiNum=*off
127200010330     C                   ENDIF
127300010330     C*
127400010330     C                   ENDSR
127500010330     C***
127600010330
127700010601
127800010601
127900010601
128000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
128100010601     C     repfil        BEGSR
128200010601     C*
128300010601     C                   if        invfil = *zeros and
128400010601     C                             depfil > *zeros and
128500010629     C                             (vinflg = *blanks or
128600010629     C                              vinflg = *zeros)
128700010601     C
128800010601     C                   eval      invfil = depfil
128900010601     C                   endif
129000010601     C*
129100010601     C                   if        depfil <> invfil and
129200010601     C                             invfil > *zeros
129300010601     C                   eval      flgMulti = '1'
129400010601     C                   if        vinflg = *blanks
129500010601     C                   add       1             cntNonEl
129600010601     C                   endif
129700010601     C                   endif
129800010601     C*
129900010601     C                   if        vinflg = '2'
130000010601     C                   eval      flgStato = '2'
130100010601     C                   endif
130200010601     C*
130300010601     C                   ENDSR
130400010601     C***
130500010601
130600010601
130700010601
130800010330
130900010330
131000010330
131100990920      /TITLE Invio dei dati al punto operativo.
131200000613     C     invio         BEGSR
131300990920     C*
131400990920     C                   reset                   dscmz
131500010601     C                   move      invfil        cmzdst
131600990920     C                   eval      cmzfld = vlrfou
131700990920     C                   eval      cmzmbd = vlrhdl
131800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
131900000710     C                   if        prmfir = *blanks
132000101018     C                   eval      cmzfla = 'EDIVAB0F'
132100101018     C                   eval      cmzmba = 'EDIVAB0F'
132200000710     C                   else
132300000710     C                   eval      cmzfla = prmfir
132400000710     C                   eval      cmzmba = prmfir
132500000710     C                   endif
132600990920     C                   eval      cmznrr = *zeros
132700990920     C                   move      §ctrok        cmznrr
132800021018     C                   eval      cmzlba = vlrfl1
132900990920     C                   call(e)   'TIS711C'
133000990920     C                   parm                    dscmz
133100990921     C                   parm      *blanks       esito
133200990920     C                   if        %error
133300990920     C                             or cmzerr = '1'
133400990921     C                             or esito  = '1'
133500000710     C                   eval      wrkesito = '3'
133600990920     C                   endif
133700990920     C*
133800000613     C                   ENDSR
133900990910
134000010601
134100010601
134200010601
134300010601
134400010601      /TITLE Invio dei dati al punto operativo.
134500010601     C     opeini        BEGSR
134600010601     C*
134700010601     C* Inizializzo flag e contatori operativi
134800010601     C                   movel     '0'           flgGiro           1
134900010601     C                   movel     '0'           flgMulti          1
135000010601     C                   movel     '1'           flgStato          1
135100010615     C                   movel     '0'           flgOk             1
135200010601     C                   z-add     *zeros        cntNonEl         10 0
135300010601     C                   z-add     *zeros        depfil            3 0
135400010601     C                   z-add     *zeros        invfil            3 0
135500010601     C*
135600010601     C                   ENDSR
135700010601     C***
135800050421
135900050421
136000050421
136100050421     C     *pssr         BEGSR
136200050421     C*
136300050421     C                   eval      wrkesito = '2'
136400050421     C*
136500050421     C                   ENDSR     '*CANCL'
136600050421     C***
136700100419
136800100419
136900100419
137000100419     C     normCSV       BEGSR
137100100419     C*
137200100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
137300100419     C* separatori campi
137400100419     C                   if        CharTXT <> *blanks
137500100419     C                   z-add     *zeros        wLenVINDTA        4 0
137600100419     C                   z-add     *zeros        wFlgVINDTA        1 0
137700100419     C                   setoff                                       80
137800100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
137900100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
138000100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
138100100419     C                   if        not *in80
138200100419     C                   seton                                        80
138300100419     C                   else
138400100419     C                   setoff                                       80
138500100419     C                   endif
138600100419     C                   endif
138700100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
138800100419     C                             and *in80 = *on
138900100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
139000100419     C                   endif
139100100419     C                   enddo
139200100419     C                   endif
139300100419     C*
139400100419     C                   ENDSR
139500100419     C***
139600010601
139700010330
139800010330
139900000613     C     *inzsr        BEGSR
140000990910     C*
140100990910     C     *entry        plist
140200990920     C                   parm                    tivlrds
140300990921     C                   parm      wrkesito      esito
140400000724     C                   parm                    prmlit
140500000710     C                   parm                    prmfir
140600010330     C*
140700010330     C* CALCOLA LA DATA CORRENTE
140800010330     C                   time                    wn14             14 0
140900101018     C                   movel     wn14          oracor            6 0          *ORA
141000121109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
141100121109     C                   eval      datcor = %dec(%date() : *ISO)
141200121109     C*
141300121109     C* Determino il CMR
141400121109     C                   movel     *blanks       wCMR             35
141500130220     C                   eval      wCMR = %char(datcor)
141600000613     C*
141700000613     C                   ENDSR
141800000613     C***
