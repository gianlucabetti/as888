000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200090323     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
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
004000010330     D*------------------
004100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004200010330     D*------------------
004300010330     D WLBDA8          DS                  INZ
004400010330     D  G08DAT                 1      8  0
004500010330     D  G08INV                 9     16  0
004600010330     D  G08ERR                17     17
004700010330     D  G08TGI                18     22  0
004800031201     D*------------------
004900031201     D* Costanti
005000031201     D*------------------
005100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005300010330
005400010330
005500010330
005600000913     C                   reset                   rrnum
005700990921     C                   reset                   esito
005800990921     C                   reset                   wrkesito
005900010601     C*
006000010601     C                   exsr      opeini
006100010605     C                   exsr      rwvab
006200010601     C*
006300010601     C                   seton                                        lr
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010601     C*--------------------------------------------------------
007100010601     C     PREELA        BEGSR
007200010601     C*
007300010601     C* SE OCCORRE SPEDIRE IN FILIALE
007400010601     C                   if        invfil <> *zeros and
007500010601     C                             flgGiro = '0'
007600010601     C*
007700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010601     C                   eval      flgGiro = '1'
007900010601     C*
008000010601     C                   endif
008100010601     C*
008200010601     C                   ENDSR
008300010601     C***
008400010601
008500010601
008600010601
008700010601
008800010601
008900010601
009000010601
009100010601     C*--------------------------------------------------------
009200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009300010601     C*--------------------------------------------------------
009400010601     C     ENDELA        BEGSR
009500080222     C*
009600080222     C* Effettuo lancio TISI95 solo x chiusura
009700080222     C                   CLEAR                   TISI95DS
009800080222     C                   EVAL      I95TLA = 'C'
009900080222     C                   CALL      'TISI95R'
010000080222     C                   PARM                    TISI95DS
010100000613     C*
010200010601     C                   ENDSR
010300010601     C***
010400000613
010500010330
010600010601
010700010601
010800010601
010900010330     C*--------------------------------------------------------
011000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
011100010330     C*--------------------------------------------------------
011200010605     C     RWVAB         BEGSR
011300010330     C*
011400010330     C                   if        not %open(tivin00r)
011500010330     C                   open      tivin00r
011600010330     C                   endif
011700021025     C                   if        not %open(fivabwwr)
011800021025     C                   open      fivabwwr
011900010330     C                   endif
012000010330     C*
012100010604     C                   clear                   §CTROK
012200010604     C                   clear                   §CTRMO
012300010604     C                   clear                   §CTRNO
012400010330     C*
012500030822     C                   do        *HIVAL
012600010330     C*
012700030822     C                   read      tivin00r                               70
012800010618     C*
012900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013000010618     C                   if        vinflg = '1'
013100010618     C                   eval      flgOk = '1'
013200010618     C                   endif
013300010618     C*
013400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
013500040510     C                   z-add     *zeros        wLenVINDTA        4 0
013600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
014000040510     C                   z-add     1             wFlgVINDTA
014100040510     C                   leave
014200040510     C                   endif
014300040510     C                   enddo
014400040510     C*
014500040218     C                   if        vindta > *blanks AND
014600040510     C                             wFlgVINDTA = 1
014700010330     C                   add       1             rrnum
014800010330     C*
014900010601     C                   if        *in70 = *off and
015000010330     C                             (vinflg = *blanks
015100010330     C                              or vinflg = '0'
015200010330     C                              or vinflg = '2')
015300010330     C*
015400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015500010711     C                   if        vinflg = *blanks or vinflg = '0'
015600010711     C                   clear                   vinmsg
015700010711     C                   endif
015800010601     C*
015900010330     C                   exsr      inzvar
016000100419     C                   exsr      normCSV
016100090403     C*
016200090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
016300090403     C* separatori campi
016400090403     C                   if        CharTXT <> *blanks
016500090403     C                   z-add     *zeros        wLenVINDTA        4 0
016600090403     C                   z-add     *zeros        wFlgVINDTA        1 0
016700090403     C                   setoff                                       80
016800090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016900090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
017000090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
017100090403     C                   if        not *in80
017200090403     C                   seton                                        80
017300090403     C                   else
017400090403     C                   setoff                                       80
017500090403     C                   endif
017600090403     C                   endif
017700090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017800090403     C                             and *in80 = *on
017900090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
018000090403     C                   endif
018100090403     C                   enddo
018200090403     C                   endif
018300090403     C*
018400010605     C                   exsr      impvab
018500010601     C*
018600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018700010605     C                   eval      depfil = VABLNP
018800010601     C                   exsr      repfil
018900060331     C                   if        VABFGS = *zeros
019000010601     C                   if        depfil = invfil
019100021025     C                   if        vlrpoi = 999
019200030822     C                   move(P)   invfil        VABFGS
019300021025     C                   else
019400030822     C                   move(P)   vlrpoi        VABFGS
019500021025     C                   endif
019600050113     C                   endif
019700060331     C                   endif
019800010601     C*
019900010601     C                   exsr      PREELA
020000010601     C*
020100010604     C* Ebbene...
020200010604     C*
020300030822     C  N31              add       1             §CTROK            7 0
020400030822     C   32              add       1             §CTRMO            7 0
020500030822     C   31              add       1             §CTRNO            7 0
020600030822     C                   if        wGiro = 2
020700030822     C  N31              write     FIVAB000
020800030822     C                   endif
020900010604     C*
021000010604     C                   if        *in31 = *off and
021100010604     C                             *in32 = *off
021200010604     C                   eval      vinflg = '1'
021300010604     C                   else
021400010604     C                   eval      vinflg = '2'
021500010604     C                   endif
021600010601     C*
021700010604     C                   endif
021800010604     C*
021900010330     C                   else
022000010330     C                   eval      vinflg = '1'
022100010330     C                   endif
022200010601     C*
022300010601     C  N70              update    tivin000
022400010330     C*
022500030822     C  N70              enddo
022600010601     C
022700010601     C*
022800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022900010601     C                   if        cntNonEl = *zeros or
023000010601     C                             flgMulti = '0'
023100010330     C* Se non ci sono record con errori ...
023200010601     C                   if        §ctrno = 0 and
023300010604     C                             §ctrmo = 0 and
023400010601     C                             flgStato <> '2'
023500010330     C* ... restituisco esito OK.
023600010330     C                   eval      wrkesito = '0'
023700010330     C                   else
023800010330     C                   if        §ctrok > 0
023900010330     C                   eval      wrkesito = '1'
024000010330     C                   else
024100010615     C                   if        flgOk = '0'
024200010615     C                   eval      wrkesito = '2'
024300010615     C                   else
024400010615     C                   eval      wrkesito = '6'
024500010615     C                   endif
024600010330     C                   endif
024700010330     C                   endif
024800010601     C                   else
024900010601     C                   eval      wrkesito = '9'
025000010601     C                   endif
025100010330     C*
025200010330     C                   if        %open(tivin00r)
025300010330     C                   close     tivin00r
025400010330     C                   endif
025500021025     C                   if        %open(fivabwwr)
025600021025     C                   close     fivabwwr
025700010330     C                   endif
025800010601     C*
025900010601     C                   if        vlrpoi <> 999
026000010601     C                   eval      invfil = vlrpoi
026100010601     C                   endif
026200010330     C*
026300010330     C                   if        §ctrok > 0
026400010601     C                             and invfil > *zeros
026500010330     C                   exsr      invio
026600010330     C                   endif
026700010601     C*
026800010618     C                   if        flgGiro = '1'
026900010601     C                   exsr      endela
027000010618     C                   endif
027100010330     C*
027200010330     C                   ENDSR
027300010330     C***
027400010601
027500010601
027600010601
027700010330     C*----------------------------------------------------*
027800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027900010330     C*----------------------------------------------------*
028000010330     C     INZVAR        BEGSR
028100010330     C*
028200030822     C* Inizializzo variabili di wrk
028300020204     C                   Z-ADD     *zeros        Num5_0            5 0
028400090403     C                   z-add     *zeros        invfil
028500030822     C*
028600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
028700030822     C                   CLEAR                   FIVAB000
028800030822     C                   CLEAR                   SkSplitCSV
028900030822     C                   EVAL      i = 1
029000030822     C                   EVAL      posDa = *zeros
029100030822     C                   EVAL      posA  = *zeros
029200030822     C*
029300030822     C* Reimposto i valori di default
029400030822     C                   EXSR      DEFCAM
029500030822     C*
029600010330     C                   ENDSR
029700010330     C*----------------------------------------------------*
029800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
029900010330     C*----------------------------------------------------*
030000010330     C     DEFCAM        BEGSR
030100110706     C*
030200110706     C                   SETOFF                                       70
030300030822     C*
030400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
030500030715     C* e delimitatore testo.
030600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
030700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
030800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
030900090403     C*
031000090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
031100090403     C                   EVAL      CharSOS = CharNUM
031200030822     C*
031300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
031400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
031500030715     C                   SETON                                        50
031600030715     C                   ELSE
031700030715     C                   SETOFF                                       50
031800030715     C                   ENDIF
031900030822     C*
032000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
032100030822     C                   EVAL      posDaDft = 1
032200030822     C                   EVAL      posADft  = 0
032300030822     C                   EVAL      wGiroDft = 0
032400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032500030822     C                             posDaDft > 0
032600030822     C*
032700030822     C* Gestisco il 1° giro
032800030822     C                   IF        wGiroDft = 0
032900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
033000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
033100030822     C* Incremento il contatore dei "giri"
033200030822     C                   EVAL      wGiroDft = 1
033300030822     C                   ELSE
033400030822     C                   EVAL      posDaDft = posADft
033500030822     C                   ENDIF
033600030822     C* Eseguo lo scan x trovare la fine del campo corrente
033700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033800030822     C*
033900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
034000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
034100030822     C                   IF        posDaDft > 0 AND
034200030822     C                             posADft  > 0
034300110706     C* RMN
034400110706     C                   IF        %subst(
034500110706     C                             %subst(vlrppt:posDaDft+1:
034600110706     C                             posADft-posDaDft-1):1:3)
034700110706     C                             = 'RMN'
034800110706     C                   IF        %trim(%subst(
034900110706     C                             %subst(vlrppt:posDaDft+1:
035000110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
035100110706     C                             = '*NSP'
035200110706     C                   SETON                                        70
035300110706     C                   ENDIF
035400110706     C                   ENDIF
035500060331     C* FGS
035600060331     C                   IF        %subst(
035700060331     C                             %subst(vlrppt:posDaDft+1:
035800060331     C                             posADft-posDaDft-1):1:3)
035900060331     C                             = 'FGS'
036000060331     C                   EVAL      PiStr=%trim(%subst(
036100060331     C                             %subst(vlrppt:posDaDft+1:
036200060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036300060331     C                   EXSR      CHKNUM
036400060331     C                   IF        PiInt=*on
036500060331     C                   Z-ADD     PiVal         VABFGS
036600060331     C                   ENDIF
036700060331     C                   ENDIF
036800030822     C* NCL
036900030822     C                   IF        %subst(
037000030822     C                             %subst(vlrppt:posDaDft+1:
037100030822     C                             posADft-posDaDft-1):1:3)
037200030822     C                             = 'NCL'
037300030822     C                   EVAL      PiStr=%trim(%subst(
037400030822     C                             %subst(vlrppt:posDaDft+1:
037500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037600030822     C                   EXSR      CHKNUM
037700030822     C                   IF        PiInt=*on
037800030822     C                   Z-ADD     PiVal         VABNCL
037900030822     C                   ENDIF
038000030822     C                   ENDIF
038100030822     C* CCM
038200030822     C                   IF        %subst(
038300030822     C                             %subst(vlrppt:posDaDft+1:
038400030822     C                             posADft-posDaDft-1):1:3)
038500030822     C                             = 'CCM'
038600030822     C                   EVAL      PiStr=%trim(%subst(
038700030822     C                             %subst(vlrppt:posDaDft+1:
038800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038900030822     C                   EXSR      CHKNUM
039000030822     C                   IF        PiInt=*on
039100030822     C                   Z-ADD     PiVal         VABCCM
039200030822     C                   ENDIF
039300030822     C                   ENDIF
039400030822     C* LNP
039500030822     C                   IF        %subst(
039600030822     C                             %subst(vlrppt:posDaDft+1:
039700030822     C                             posADft-posDaDft-1):1:3)
039800030822     C                             = 'LNP'
039900030822     C                   EVAL      PiStr=%trim(%subst(
040000030822     C                             %subst(vlrppt:posDaDft+1:
040100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040200030822     C                   EXSR      CHKNUM
040300030822     C                   IF        PiInt=*on
040400030822     C                   Z-ADD     PiVal         VABLNP
040500030822     C                   ENDIF
040600030822     C                   ENDIF
040700030822     C* NRS
040800030822     C                   IF        %subst(
040900030822     C                             %subst(vlrppt:posDaDft+1:
041000030822     C                             posADft-posDaDft-1):1:3)
041100030822     C                             = 'NRS'
041200030822     C                   EVAL      PiStr=%trim(%subst(
041300030822     C                             %subst(vlrppt:posDaDft+1:
041400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041500030822     C                   EXSR      CHKNUM
041600030822     C                   IF        PiInt=*on
041700030822     C                   Z-ADD     PiVal         VABNRS
041800030822     C                   ENDIF
041900030822     C                   ENDIF
042000030822     C* CTR
042100030822     C                   IF        %subst(
042200030822     C                             %subst(vlrppt:posDaDft+1:
042300030822     C                             posADft-posDaDft-1):1:3)
042400030822     C                             = 'CTR'
042500030822     C                   EVAL      PiStr=%trim(%subst(
042600030822     C                             %subst(vlrppt:posDaDft+1:
042700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042800030822     C                   EXSR      CHKNUM
042900030822     C                   IF        PiInt=*on
043000030822     C                   Z-ADD     PiVal         VABCTR
043100030822     C                   ENDIF
043200030822     C                   ENDIF
043300030822     C* PKB
043400030822     C                   IF        %subst(
043500030822     C                             %subst(vlrppt:posDaDft+1:
043600030822     C                             posADft-posDaDft-1):1:3)
043700030822     C                             = 'PKB'
043800030822     C                   EVAL      PiStr=%trim(%subst(
043900030822     C                             %subst(vlrppt:posDaDft+1:
044000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044100030822     C                   EXSR      CHKNUM
044200030822     C                   IF        PiNum=*on
044300030822     C                   Z-ADD     PiVal         VABPKB
044400030822     C                   ENDIF
044500030822     C                   ENDIF
044600030822     C* VLB
044700030822     C                   IF        %subst(
044800030822     C                             %subst(vlrppt:posDaDft+1:
044900030822     C                             posADft-posDaDft-1):1:3)
045000030822     C                             = 'VLB'
045100030822     C                   EVAL      PiStr=%trim(%subst(
045200030822     C                             %subst(vlrppt:posDaDft+1:
045300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045400030822     C                   EXSR      CHKNUM
045500030822     C                   IF        PiNum=*on
045600030822     C                   Z-ADD     PiVal         VABVLB
045700030822     C                   ENDIF
045800030822     C                   ENDIF
045900030822     C* QFT
046000030822     C                   IF        %subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):1:3)
046300030822     C                             = 'QFT'
046400030822     C                   EVAL      PiStr=%trim(%subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700030822     C                   EXSR      CHKNUM
046800030822     C                   IF        PiNum=*on
046900030822     C                   Z-ADD     PiVal         VABQFT
047000030822     C                   ENDIF
047100030822     C                   ENDIF
047200030822     C* CBO
047300030822     C                   IF        %subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):1:3)
047600030822     C                             = 'CBO'
047700030822     C                   EVAL      VABCBO=%trim(%subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048000030822     C                   ENDIF
048100030822     C* TSP
048200030822     C                   IF        %subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):1:3)
048500030822     C                             = 'TSP'
048600030822     C                   EVAL      VABTSP=%trim(%subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048900030822     C                   ENDIF
049000030822     C* VAS
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'VAS'
049500030822     C                   EVAL      VABVAS=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   ENDIF
049900030822     C* VCA
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'VCA'
050400030822     C                   EVAL      VABVCA=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   ENDIF
050800030822     C* GCA
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'GCA'
051300030822     C                   EVAL      VABGCA=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   ENDIF
051700030822     C* CTM
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'CTM'
052200030822     C                   EVAL      VABCTM=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   ENDIF
052600030822     C* FFD
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'FFD'
053100030822     C                   EVAL      VABFFD=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   ENDIF
053500030822     C* VAD
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'VAD'
054000030822     C                   EVAL      VABVAD=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   ENDIF
054400030822     C* GMA
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'GMA'
054900030822     C                   EVAL      VABGMA=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   ENDIF
055300030822     C* GGA
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'GGA'
055800030822     C                   EVAL      VABGGA=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   ENDIF
056200030822     C* GVA
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'GVA'
056700030822     C                   EVAL      VABGVA=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   ENDIF
057100030822     C* TC1
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'TC1'
057600030822     C                   EVAL      VABTC1=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   ENDIF
058000030822     C* TC2
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'TC2'
058500030822     C                   EVAL      VABTC2=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* ...
059000030822     C                   ENDIF
059100030822     C                   ENDDO
059200020204     C*
059300010330     C                   ENDSR
059400010607     C*----------------------------------------------------*
059500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
059600010607     C*----------------------------------------------------*
059700010607     C     IMPVAB        BEGSR
059800010607     C*
059900010607     C                   SETOFF                                       3132
060000030515     C*
060100010607     C                   MOVEL     datcor        VABAAS
060200010607     C                   MOVE      datcor        VABMGS
060300010607     C*
060400030715     C**********
060500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
060600030715     C**********
060700030715     C                   IF        wGiro = *zeros
060800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
060900031201     C     minu:maiu     XLATE     vindta        vindta
061000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
061100040324     C                   DOW       posDa <= %len(%trimr(vindta))
061200030715     C*
061300030715     C* Gestisco il 1° campo
061400030715     C                   IF        i = 1
061500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
061600030715     C                   EVAL      posDa = 1
061700030715     C* Eseguo lo scan x trovare la fine del primo campo
061800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
061900030715     C* A questo punto "estraggo" il campo corrente
062000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
062100030715     C                                                    (posA-posDa))
062200030715     C* X i campi successivi al 1°
062300030715     C                   ELSE
062400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
062500030715     C                   EVAL      posDa = posA + 1
062600030715     C* Eseguo lo scan x trovare la fine del campo corrente
062700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
062800030715     C* Gestisco l'ultimo campo
062900030715     C                   IF        posA = *zeros
063000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
063100030715     C* A questo punto "estraggo" il campo corrente
063200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063300030715     C                                                    (posA-posDa))
063400030715     C                   LEAVE
063500030715     C                   ELSE
063600030715     C* A questo punto "estraggo" il campo corrente
063700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063800030715     C                                                    (posA-posDa))
063900030715     C                   ENDIF
064000030715     C                   ENDIF
064100030715     C* Incremento il contatore d campo
064200030715     C                   EVAL      i = i +1
064300030715     C                   ENDDO
064400030715     C*
064500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
064600030715     C* delimitazione testo
064700030715     C                   EVAL      i = 1
064800030715     C                   DOW       i < %elem(SkSplitFLD)
064900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
065000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
065100030715     C                   EVAL      i = i + 1
065200030715     C                   ENDDO
065300030715     C                   EVAL      wGiro = 1
065400030715     C                   ELSE
065500030715     C                   EVAL      wGiro = 2
065600031204     C**********
065700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
065800031204     C* d separatore campo
065900031204     C**********
066000040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
066100040304     C                   EVAL      vindta = ' ' + vindta
066200040304     C                   ENDIF
066300040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
066400040304     C***                EVAL      vindta = %subst(vindta:2)
066500040304     C***                ENDDO
066600031204     C*
066700031204     C                   Z-ADD     *zeros        lunghInput        4 0
066800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
066900040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
067000040304     C                   EVAL      vindta = vindta + ' '
067100040304     C                   ENDIF
067200040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
067300040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
067400040304     C***                EVAL      lunghInput = %len(%trim(vindta))
067500040304     C***                ENDDO
067600030715     C**********
067700030715     C* Effettuo lo split del campo dati d input
067800030715     C**********
067900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
068000040324     C                   DOW       posDa <= %len(%trimr(vindta))
068100030715     C*
068200030715     C* Gestisco il 1° campo
068300030715     C                   IF        i = 1
068400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068500030715     C                   EVAL      posDa = 1
068600030715     C* Eseguo lo scan x trovare la fine del primo campo
068700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068800030715     C* A questo punto "estraggo" il campo corrente
068900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
069000030715     C                                                    (posA-posDa))
069100030715     C* X i campi successivi al 1°
069200030715     C                   ELSE
069300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069400030715     C                   EVAL      posDa = posA + 1
069500031204     C* Verifico che nn vi sia il campo nn valorizzato
069600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
069700031204     C* Se campo nn valorizzato skippo al prossimo
069800031204     C                   EVAL      SkSplitCSV(i) = *blanks
069900031204     C                   EVAL      posA  = posA + 1
070000031204     C                   ELSE
070100030715     C* Eseguo lo scan x trovare la fine del campo corrente
070200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070300030715     C* Gestisco l'ultimo campo
070400030715     C                   IF        posA = *zeros
070500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070600030715     C* A questo punto "estraggo" il campo corrente
070700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
070800030715     C                                                    (posA-posDa))
070900030715     C                   LEAVE
071000030715     C                   ELSE
071100030715     C* A questo punto "estraggo" il campo corrente
071200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071300030715     C                                                    (posA-posDa))
071400030715     C                   ENDIF
071500031204     C                   ENDIF
071600030715     C                   ENDIF
071700030715     C* Incremento il contatore d campo
071800030715     C                   EVAL      i = i +1
071900030715     C                   ENDDO
072000030715     C*
072100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
072200030715     C* delimitazione testo
072300030715     C                   EVAL      i = 1
072400030715     C                   DOW       i < %elem(SkSplitCSV)
072500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
072600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
072700030715     C                   EVAL      i = i + 1
072800030715     C                   ENDDO
072900030715     C*
073000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
073100030715     C                   EVAL      i = 1
073200030715     C                   DOW       i < %elem(SkSplitFLD)
073300030715     C*
073400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
073500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
073600030715     C                             SkSplitCSV(i) <> *blanks
073700130624     C*** ATB
073800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
073900130624     C                   IF        SkSplitCSV(i) = 'D'
074000130624     C                   EVAL      VABCTR = 390
074100130624     C                   EVAL      VABCTM = '7R'
074200130624     C                   ENDIF
074300030721     C                   ENDIF
074400030715     C*** RSD
074500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
074600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
074700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
074800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
074900030715     C* ==
075000030715     C                   ENDIF
075100030721     C*** RD2
075200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
075300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
075400030721     C                   ENDIF
075500030715     C*** IND
075600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
075700030715     C                   EVAL      VABIND = SkSplitCSV(i)
075800030715     C                   ENDIF
075900030715     C*** LOD
076000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
076100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
076200030715     C                   ENDIF
076300030715     C*** PRD
076400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
076500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
076600030715     C                   ENDIF
076700030721     C*** NZD
076800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
076900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
077000030721     C                   ENDIF
077100030715     C*** RMA
077200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
077300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
077400030715     C                   ENDIF
077500030715     C*** NOT
077600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
077700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
077800030715     C                   ENDIF
077900030715     C*** NT2
078000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
078100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
078200030715     C                   ENDIF
078300030715     C*** VCA
078400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
078500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
078600030715     C                   ENDIF
078700030715     C*** VAS
078800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
078900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
079000030715     C                   ENDIF
079100030715     C*** FFD
079200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
079300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
079400030715     C                   ENDIF
079500030715     C*** CBO
079600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
079700030715     C                   EVAL      VABCBO = SkSplitCSV(i)
079800030715     C                   ENDIF
079900030721     C*** GC1
080000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
080100120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
080200120314     C                   MOVEL     *blanks       wGC               1
080300120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
080400120314     C                   IF        SkSplitCSV(i) = 'M' OR
080500120314     C                             SkSplitCSV(i) = 'P'
080600120314     C                   MOVE(P)   wGC           VABGC1
080700120314     C                   ELSE
080800120314     C                   MOVEL(P)  wGC           VABGC1
080900120314     C                   ENDIF
081000120314     C                   ENDIF
081100030721     C                   ENDIF
081200030721     C*** GC2
081300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
081400120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
081500120314     C                   MOVEL     *blanks       wGC               1
081600120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
081700120314     C                   IF        SkSplitCSV(i) = 'M' OR
081800120314     C                             SkSplitCSV(i) = 'P'
081900120314     C                   MOVE(P)   wGC           VABGC2
082000120314     C                   ELSE
082100120314     C                   MOVEL(P)  wGC           VABGC2
082200120314     C                   ENDIF
082300120314     C                   ENDIF
082400030721     C                   ENDIF
082500030721     C*** TSP
082600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
082700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
082800030721     C                   ENDIF
082900030721     C*** NAS
083000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
083100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
083200030721     C                   ENDIF
083300030721     C*** TIC
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
083500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030721     C*** GCA
083800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
083900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
084000030721     C                   ENDIF
084100030721     C*** XCO
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
084300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030721     C*** CTM
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
084700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** TCR
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
085100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** CTS
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
085500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** FTM
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
085900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** VAD
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
086300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** GMA
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
086700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** GGA
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
087100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** GVA
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
087500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** TC1
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
087900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** TC2
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
088300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** SCL
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
088700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** RMO
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
089100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** NMO
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
089500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*
089800070705     C* Reperisco quindi i campi numerici...
089900030721     C*** CAD
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
090100070705     C                   EVAL      VABCAD = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030721     C*** RMN
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
090500030721     C                   EVAL      PiStr=SkSplitCSV(i)
090600030721     C                   EXSR      CHKNUM
090700030721     C                   IF        PiInt=*on
090800030721     C                   Z-ADD     PiVal         VABRMN
090900030721     C                   ELSE
091000030721     C                   SETON                                        32
091100030721     C                   EVAL      VABRMN = 1
091200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091300030721     C                             + ' ' + 'VABRMN'
091400030721     C                   ENDIF
091500030721     C                   ENDIF
091600030721     C*** NSP
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
091800030721     C                   EVAL      PiStr=SkSplitCSV(i)
091900030721     C                   EXSR      CHKNUM
092000100517     C                   IF        PiInt=*on
092100030721     C                   Z-ADD     PiVal         VABNSP
092200030721     C                   ELSE
092300030721     C                   SETON                                        32
092400030721     C                   EVAL      VABNSP = *zeros
092500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092600030721     C                             + ' ' + 'VABNSP'
092700030721     C                   ENDIF
092800030721     C                   ENDIF
092900030721     C*** NCL
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
093100030721     C                   EVAL      PiStr=SkSplitCSV(i)
093200030721     C                   EXSR      CHKNUM
093300030721     C                   IF        PiInt=*on
093400030721     C                   Z-ADD     PiVal         VABNCL
093500030721     C                   ELSE
093600030721     C                   SETON                                        32
093700030721     C                   EVAL      VABNCL = *zeros
093800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093900030721     C                             + ' ' + 'VABNCL'
094000030721     C                   ENDIF
094100030721     C                   ENDIF
094200030721     C*** PKB
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
094400030721     C                   EVAL      PiStr=SkSplitCSV(i)
094500030721     C                   EXSR      CHKNUM
094600030721     C                   IF        PiNum=*on
094700030721     C                   Z-ADD     PiVal         VABPKB
094800030721     C                   ELSE
094900030721     C                   SETON                                        32
095000030721     C                   EVAL      VABPKB = *zeros
095100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095200030721     C                             + ' ' + 'VABPKB'
095300030721     C                   ENDIF
095400030721     C                   ENDIF
095500030721     C*** CAS
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
095700030721     C                   EVAL      PiStr=SkSplitCSV(i)
095800030721     C                   EXSR      CHKNUM
095900030721     C                   IF        PiNum=*on
096000030721     C                   Z-ADD     PiVal         VABCAS
096100030721     C                   ELSE
096200030721     C                   SETON                                        32
096300030721     C                   EVAL      VABCAS = *zeros
096400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096500030721     C                             + ' ' + 'VABCAS'
096600030721     C                   ENDIF
096700030721     C                   ENDIF
096800030721     C*** CCM
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
097000030721     C                   EVAL      PiStr=SkSplitCSV(i)
097100030721     C                   EXSR      CHKNUM
097200030721     C                   IF        PiInt=*on      AND
097300030721     C                             PiVal<=9999999 AND
097400030721     C                             PiVal>*zeros
097500030721     C                   Z-ADD     PiVal         VABCCM
097600030721     C                   ELSE
097700030721     C                   SETON                                        32
097800030721     C                   EVAL      VABCCM = *zeros
097900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098000030721     C                             + ' ' + 'VABCCM'
098100030721     C                   ENDIF
098200030721     C                   ENDIF
098300030721     C*** LNP
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
098500030721     C                   EVAL      PiStr=SkSplitCSV(i)
098600030721     C                   EXSR      CHKNUM
098700030721     C                   IF        PiInt=*on    AND
098800030721     C                             PiVal<=999   AND
098900030721     C                             PiVal>*zeros
099000030721     C                   Z-ADD     PiVal         VABLNP
099100030721     C                   ELSE
099200030721     C                   SETON                                        32
099300030721     C                   EVAL      VABLNP = *zeros
099400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099500030721     C                             + ' ' + 'VABLNP'
099600030721     C                   ENDIF
099700030721     C                   ENDIF
099800030721     C*** NRS
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
100000030721     C                   EVAL      PiStr=SkSplitCSV(i)
100100030721     C                   EXSR      CHKNUM
100200030721     C                   IF        PiInt=*on AND
100300030721     C                             PiVal<=99
100400030721     C                   Z-ADD     PiVal         VABNRS
100500030721     C                   ELSE
100600030721     C                   SETON                                        32
100700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100800030721     C                             + ' ' + 'VABNRS'
100900030721     C                   ENDIF
101000030721     C                   ENDIF
101100030721     C*** LNA
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
101300120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
101400120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
101500120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
101600120314     C                             %trim(SkSplitCSV(i)) <> *blanks
101700030721     C                   EVAL      PiStr=SkSplitCSV(i)
101800030721     C                   EXSR      CHKNUM
101900030721     C                   IF        PiInt=*on    AND
102000030721     C                             PiVal<=999   AND
102100030721     C                             PiVal>*zeros
102200030721     C                   Z-ADD     PiVal         VABLNA
102300030721     C                   ELSE
102400030721     C                   SETON                                        32
102500030721     C                   EVAL      VABLNA = *zeros
102600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102700030721     C                             + ' ' + 'VABLNA'
102800120314     C                   ENDIF
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** CTR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
103300030721     C                   EVAL      PiStr=SkSplitCSV(i)
103400030721     C                   EXSR      CHKNUM
103500030721     C                   IF        PiInt=*on  AND
103600030721     C                             PiVal<=999
103700030721     C                   Z-ADD     PiVal         VABCTR
103800030721     C                   ELSE
103900030721     C                   SETON                                        32
104000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104100030721     C                             + ' ' + 'VABCTR'
104200030721     C                   ENDIF
104300030721     C                   ENDIF
104400030721     C*** IAS
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
104600030721     C                   EVAL      PiStr=SkSplitCSV(i)
104700030721     C                   EXSR      CHKNUM
104800030721     C                   IF        PiNum=*on
104900030721     C                   Z-ADD     PiVal         VABIAS
105000030721     C                   ELSE
105100030721     C                   SETON                                        32
105200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105300030721     C                             + ' ' + 'VABIAS'
105400030721     C                   ENDIF
105500030721     C                   ENDIF
105600030721     C*** VLB
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
105800030721     C                   EVAL      PiStr=SkSplitCSV(i)
105900030721     C                   EXSR      CHKNUM
106000030721     C                   IF        PiNum=*on
106100030721     C                   Z-ADD     PiVal         VABVLB
106200030721     C                   ELSE
106300030721     C                   SETON                                        32
106400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106500030721     C                             + ' ' + 'VABVLB'
106600030721     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** QFT
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiNum=*on
107300030721     C                   Z-ADD     PiVal         VABQFT
107400030721     C                   ELSE
107500030721     C                   SETON                                        32
107600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107700030721     C                             + ' ' + 'VABQFT'
107800030721     C                   ENDIF
107900030721     C                   ENDIF
108000030721     C*** NCD
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
108200030721     C                   EVAL      PiStr=SkSplitCSV(i)
108300030721     C                   EXSR      CHKNUM
108400030721     C                   IF        PiInt=*on      AND
108500030721     C                             PiVal<=9999999
108600030721     C                   Z-ADD     PiVal         VABNCD
108700030721     C                   ELSE
108800030721     C                   SETON                                        32
108900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109000030721     C                             + ' ' + 'VABNCD'
109100030721     C                   ENDIF
109200030721     C                   ENDIF
109300030721     C*** NCA
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on      AND
109800030721     C                             PiVal<=9999999
109900030721     C                   Z-ADD     PiVal         VABNCA
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABNCA'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** ZNC
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiInt=*on AND
111100030721     C                             PiVal<=99
111200030721     C                   Z-ADD     PiVal         VABZNC
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABZNC'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** DCR
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on       AND
112400030721     C                             PiVal<=99999999
112500030721     C                   Z-ADD     PiVal         VABDCR
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABDCR'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** HCR
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on   AND
113700030721     C                             PiVal<=9999
113800030721     C                   Z-ADD     PiVal         VABHCR
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABHCR'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** VMD
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiNum=*on
115000030721     C                   Z-ADD     PiVal         VABVMD
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABVMD'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** ANT
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiInt=*on        AND
116200030721     C                             PiVal<=999999999
116300030721     C                   Z-ADD     PiVal         VABANT
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABANT'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** CMO
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on
117500030721     C                   Z-ADD     PiVal         Num5_0
117600030721     C                   MOVEL(p)  Num5_0        VABCMO
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCMO = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABCMO'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030715     C*
118500030715     C                   ENDIF
118600030715     C                   EVAL      i = i + 1
118700030715     C                   ENDDO
118800070705     C*
118900070705     C* Considerazioni particolari
119000110706     C*
119100110706     C*** RMN
119200110706     C                   IF        *in70
119300110706     C                   EVAL      VABRMN = VABNSP
119400110706     C                   ENDIF
119500070705     C*** NZD
119600070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
119700070705     C                   EVAL      VABNZD = *blanks
119800070705     C                   ENDIF
119900070705     C*** CAD
120000070705     C                   IF        VABNZD = *blanks
120100070705     C                   EVAL      PiStr=VABCAD
120200070705     C                   EXSR      CHKNUM
120300070705     C                   IF        PiInt=*on
120400070705     C                   Z-ADD     PiVal         Num5_0
120500070705     C                   MOVEL(p)  Num5_0        VABCAD
120600070705     C                   ELSE
120700070705     C                   SETON                                        32
120800070705     C                   EVAL      VABCAD = *zeros
120900070705     C                   EVAL      vinmsg = %trimr(vinmsg)
121000070705     C                             + ' ' + 'VABCAD'
121100070705     C                   ENDIF
121200070705     C                   ENDIF
121300080222     C*
121400080222     C* Se provincia nn valorizzata la reperisco
121500080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
121600080222     C                   IF        VABPRD = *blanks
121700080222     C                   CLEAR                   TISI95DS
121800080222     C                   EVAL      I95TCN = '3'
121900080222     C                   Z-ADD     datcor        I95DAT
122000080222     C                   EVAL      I95NAR = VABNZD
122100080222     C                   EVAL      I95CAP = VABCAD
122200080222     C                   EVAL      I95LOC = VABLOD
122300080222     C                   CALL      'TISI95R'
122400080222     C                   PARM                    TISI95DS
122500080222     C                   EVAL      VABPRD = O95PRV
122600080222     C                   ENDIF
122700020204     C*
122800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
122900020204     C                   EXSR      CHKIMPDIV
123000030715     C                   ENDIF
123100020204     C*
123200010607     C                   ENDSR
123300010607     C*----------------------------------------------------*
123400010601
123500020204
123600020204     C*----------------------------------------------------*
123700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
123800020204     C*----------------------------------------------------*
123900020204     C     CHKIMPDIV     BEGSR
124000060524     C*
124100060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
124200060524     C                   IF        vabCAS > *zeros
124300060524     C                   IF        vabCBO = '1'
124400060524     C                   EVAL      vabCBO = '4'
124500060524     C                   ENDIF
124600060524     C                   IF        vabCBO = '2'
124700060524     C                   EVAL      vabCBO = '6'
124800060524     C                   ENDIF
124900060524     C                   ENDIF
125000020204     C*
125100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
125200020204     C                   Z-ADD     *zeros        wrkDec            9 9
125300020204     C*
125400020204     C* Come prima cosa effettuo considerazioni sulla divisa
125500020204     C                   IF        vabIAS > *zeros
125600020204     C                   IF        vabVAS <> 'EUR'
125700020204     C                   EVAL      vabVAS =  'ITL'
125800020204     C                   ENDIF
125900020204     C                   ENDIF
126000020204     C*
126100020204     C                   IF        vabCAS > *zeros
126200020204     C                   IF        vabVCA <> 'EUR'
126300020204     C                   EVAL      vabVCA =  'ITL'
126400020204     C                   ENDIF
126500020204     C                   ENDIF
126600020204     C*
126700020204     C                   IF        vabVMD > *zeros
126800020204     C                   IF        vabVAD <> 'EUR'
126900020204     C                   EVAL      vabVAD =  'ITL'
127000020204     C                   ENDIF
127100020204     C                   ENDIF
127200020204     C*
127300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
127400020204     C                   Z-ADD     vabIAS        wrkDec
127500020204     C                   IF        wrkDec > *zeros
127600020204     C                   IF        vabVAS = 'ITL'
127700020204     C                   EVAL      vabIAS = *zeros
127800020204     C                   ENDIF
127900020204     C                   ENDIF
128000020204     C*
128100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
128200020204     C                   Z-ADD     vabCAS        wrkDec
128300020204     C                   IF        wrkDec > *zeros
128400020204     C                   IF        vabVCA = 'ITL'
128500020204     C                   EVAL      vabCAS = *zeros
128600020204     C                   ENDIF
128700020204     C                   ENDIF
128800020204     C*
128900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
129000020204     C                   Z-ADD     vabVMD        wrkDec
129100020204     C                   IF        wrkDec > *zeros
129200020204     C                   IF        vabVAD = 'ITL'
129300020204     C                   EVAL      vabVMD = *zeros
129400020204     C                   ENDIF
129500020204     C                   ENDIF
129600020204     C*
129700020204     C                   ENDSR
129800020204     C***
129900020204
130000010330
130100010330
130200010330     C*----------------------------------------------------*
130300010330     C*  CONTROLLO NUMERICITA' CAMPI
130400010330     C*----------------------------------------------------*
130500010330     C     CHKNUM        BEGSR
130600010330     C*
130700010606     C                   IF        PiDecChr = *blanks
130800030715     C                   EVAL      PiDecChr = CharNUM
130900010606     C                   ENDIF
131000010606     C*
131100010606     C                   CALL(e)   'ISNUMERIC'
131200010330     C                   PARM                    PiStr            30
131300010606     C                   PARM                    PiDecChr          1
131400010330     C                   PARM      *ZEROS        PiVal            30 9
131500010330     C                   PARM      '0'           PiInt             1
131600010330     C                   PARM      '0'           PiNum             1
131700010330     C                   IF        %error
131800010606     C                   EVAL      PiNum=*off
131900010330     C                   ENDIF
132000010330     C*
132100010330     C                   ENDSR
132200010330     C***
132300010330
132400010601
132500010601
132600010601
132700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
132800010601     C     repfil        BEGSR
132900010601     C*
133000010601     C                   if        invfil = *zeros and
133100010601     C                             depfil > *zeros and
133200010629     C                             (vinflg = *blanks or
133300010629     C                              vinflg = *zeros)
133400010601     C
133500010601     C                   eval      invfil = depfil
133600010601     C                   endif
133700010601     C*
133800010601     C                   if        depfil <> invfil and
133900010601     C                             invfil > *zeros
134000010601     C                   eval      flgMulti = '1'
134100010601     C                   if        vinflg = *blanks
134200010601     C                   add       1             cntNonEl
134300010601     C                   endif
134400010601     C                   endif
134500010601     C*
134600010601     C                   if        vinflg = '2'
134700010601     C                   eval      flgStato = '2'
134800010601     C                   endif
134900010601     C*
135000010601     C                   ENDSR
135100010601     C***
135200010601
135300010601
135400010601
135500010330
135600010330
135700010330
135800990920      /TITLE Invio dei dati al punto operativo.
135900000613     C     invio         BEGSR
136000990920     C*
136100990920     C                   reset                   dscmz
136200010601     C                   move      invfil        cmzdst
136300990920     C                   eval      cmzfld = vlrfou
136400990920     C                   eval      cmzmbd = vlrhdl
136500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
136600000710     C                   if        prmfir = *blanks
136700021025     C                   eval      cmzfla = 'FIVAB00F'
136800021025     C                   eval      cmzmba = 'FIVAB00F'
136900000710     C                   else
137000000710     C                   eval      cmzfla = prmfir
137100000710     C                   eval      cmzmba = prmfir
137200000710     C                   endif
137300990920     C                   eval      cmznrr = *zeros
137400990920     C                   move      §ctrok        cmznrr
137500021018     C                   eval      cmzlba = vlrfl1
137600990920     C                   call(e)   'TIS711C'
137700990920     C                   parm                    dscmz
137800990921     C                   parm      *blanks       esito
137900990920     C                   if        %error
138000990920     C                             or cmzerr = '1'
138100990921     C                             or esito  = '1'
138200000710     C                   eval      wrkesito = '3'
138300990920     C                   endif
138400990920     C*
138500000613     C                   ENDSR
138600990910
138700010601
138800010601
138900010601
139000010601
139100010601      /TITLE Invio dei dati al punto operativo.
139200010601     C     opeini        BEGSR
139300010601     C*
139400010601     C* Inizializzo flag e contatori operativi
139500010601     C                   movel     '0'           flgGiro           1
139600010601     C                   movel     '0'           flgMulti          1
139700010601     C                   movel     '1'           flgStato          1
139800010615     C                   movel     '0'           flgOk             1
139900010601     C                   z-add     *zeros        cntNonEl         10 0
140000010601     C                   z-add     *zeros        depfil            3 0
140100010601     C                   z-add     *zeros        invfil            3 0
140200010601     C*
140300010601     C                   ENDSR
140400010601     C***
140500050421
140600050421
140700050421
140800050421     C     *pssr         BEGSR
140900050421     C*
141000050421     C                   eval      wrkesito = '2'
141100050421     C*
141200050421     C                   ENDSR     '*CANCL'
141300050421     C***
141400100419
141500100419
141600100419
141700100419     C     normCSV       BEGSR
141800100419     C*
141900100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
142000100419     C* separatori campi
142100100419     C                   if        CharTXT <> *blanks
142200100419     C                   z-add     *zeros        wLenVINDTA        4 0
142300100419     C                   z-add     *zeros        wFlgVINDTA        1 0
142400100419     C                   setoff                                       80
142500100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
142600100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
142700100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
142800100419     C                   if        not *in80
142900100419     C                   seton                                        80
143000100419     C                   else
143100100419     C                   setoff                                       80
143200100419     C                   endif
143300100419     C                   endif
143400100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
143500100419     C                             and *in80 = *on
143600100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
143700100419     C                   endif
143800100419     C                   enddo
143900100419     C                   endif
144000100419     C*
144100100419     C                   ENDSR
144200100419     C***
144300010601
144400010330
144500010330
144600000613     C     *inzsr        BEGSR
144700990910     C*
144800990910     C     *entry        plist
144900990920     C                   parm                    tivlrds
145000990921     C                   parm      wrkesito      esito
145100000724     C                   parm                    prmlit
145200000710     C                   parm                    prmfir
145300010330     C*
145400010330     C* CALCOLA LA DATA CORRENTE
145500010330     C                   time                    wn14             14 0
145600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
145700010330     C                   z-add     wn8           g08dat
145800010330     C                   z-add     *zeros        g08inv
145900010330     C                   movel     '0'           g08err
146000010330     C                   call      'XSRDA8'
146100010330     C                   parm                    wlbda8
146200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146300000613     C*
146400000613     C                   ENDSR
146500000613     C***
