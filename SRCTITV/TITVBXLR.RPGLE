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
073700030721     C*** ATB
073800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
073900030721     C                   EVAL      VABATB = SkSplitCSV(i)
074000030721     C                   ENDIF
074100030715     C*** RSD
074200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
074300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
074400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
074500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
074600030715     C* ==
074700030715     C                   ENDIF
074800030721     C*** RD2
074900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
075000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
075100030721     C                   ENDIF
075200030715     C*** IND
075300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
075400030715     C                   EVAL      VABIND = SkSplitCSV(i)
075500030715     C                   ENDIF
075600030715     C*** LOD
075700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
075800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
075900030715     C                   ENDIF
076000030715     C*** PRD
076100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
076200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
076300030715     C                   ENDIF
076400030721     C*** NZD
076500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
076600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
076700030721     C                   ENDIF
076800030715     C*** RMA
076900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
077000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
077100030715     C                   ENDIF
077200030715     C*** NOT
077300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
077400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
077500030715     C                   ENDIF
077600030715     C*** NT2
077700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
077800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
077900030715     C                   ENDIF
078000030715     C*** VCA
078100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
078200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
078300030715     C                   ENDIF
078400030715     C*** VAS
078500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
078600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
078700030715     C                   ENDIF
078800030715     C*** FFD
078900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
079000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
079100030715     C                   ENDIF
079200030715     C*** CBO
079300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
079400030715     C                   EVAL      VABCBO = SkSplitCSV(i)
079500030715     C                   ENDIF
079600030721     C*** GC1
079700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
079800120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
079900120314     C                   MOVEL     *blanks       wGC               1
080000120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
080100120314     C                   IF        SkSplitCSV(i) = 'M' OR
080200120314     C                             SkSplitCSV(i) = 'P'
080300120314     C                   MOVE(P)   wGC           VABGC1
080400120314     C                   ELSE
080500120314     C                   MOVEL(P)  wGC           VABGC1
080600120314     C                   ENDIF
080700120314     C                   ENDIF
080800030721     C                   ENDIF
080900030721     C*** GC2
081000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
081100120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
081200120314     C                   MOVEL     *blanks       wGC               1
081300120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
081400120314     C                   IF        SkSplitCSV(i) = 'M' OR
081500120314     C                             SkSplitCSV(i) = 'P'
081600120314     C                   MOVE(P)   wGC           VABGC2
081700120314     C                   ELSE
081800120314     C                   MOVEL(P)  wGC           VABGC2
081900120314     C                   ENDIF
082000120314     C                   ENDIF
082100030721     C                   ENDIF
082200030721     C*** TSP
082300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
082400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
082500030721     C                   ENDIF
082600030721     C*** NAS
082700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
082800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
082900030721     C                   ENDIF
083000030721     C*** TIC
083100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
083200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
083300030721     C                   ENDIF
083400030721     C*** GCA
083500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
083600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
083700030721     C                   ENDIF
083800030721     C*** XCO
083900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
084000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
084100030721     C                   ENDIF
084200030721     C*** CTM
084300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
084400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
084500030721     C                   ENDIF
084600030721     C*** TCR
084700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
084800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
084900030721     C                   ENDIF
085000030721     C*** CTS
085100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
085200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
085300030721     C                   ENDIF
085400030721     C*** FTM
085500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
085600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
085700030721     C                   ENDIF
085800030721     C*** VAD
085900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
086000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
086100030721     C                   ENDIF
086200030721     C*** GMA
086300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
086400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
086500030721     C                   ENDIF
086600030721     C*** GGA
086700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
086800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
086900030721     C                   ENDIF
087000030721     C*** GVA
087100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
087200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
087300030721     C                   ENDIF
087400030721     C*** TC1
087500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
087600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
087700030721     C                   ENDIF
087800030721     C*** TC2
087900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
088000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
088100030721     C                   ENDIF
088200030721     C*** SCL
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
088400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030721     C*** RMO
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
088800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
088900030721     C                   ENDIF
089000030721     C*** NMO
089100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
089200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
089300030721     C                   ENDIF
089400030721     C*
089500070705     C* Reperisco quindi i campi numerici...
089600030721     C*** CAD
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
089800070705     C                   EVAL      VABCAD = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** RMN
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
090200030721     C                   EVAL      PiStr=SkSplitCSV(i)
090300030721     C                   EXSR      CHKNUM
090400030721     C                   IF        PiInt=*on
090500030721     C                   Z-ADD     PiVal         VABRMN
090600030721     C                   ELSE
090700030721     C                   SETON                                        32
090800030721     C                   EVAL      VABRMN = 1
090900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
091000030721     C                             + ' ' + 'VABRMN'
091100030721     C                   ENDIF
091200030721     C                   ENDIF
091300030721     C*** NSP
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
091500030721     C                   EVAL      PiStr=SkSplitCSV(i)
091600030721     C                   EXSR      CHKNUM
091700100517     C                   IF        PiInt=*on
091800030721     C                   Z-ADD     PiVal         VABNSP
091900030721     C                   ELSE
092000030721     C                   SETON                                        32
092100030721     C                   EVAL      VABNSP = *zeros
092200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092300030721     C                             + ' ' + 'VABNSP'
092400030721     C                   ENDIF
092500030721     C                   ENDIF
092600030721     C*** NCL
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
092800030721     C                   EVAL      PiStr=SkSplitCSV(i)
092900030721     C                   EXSR      CHKNUM
093000030721     C                   IF        PiInt=*on
093100030721     C                   Z-ADD     PiVal         VABNCL
093200030721     C                   ELSE
093300030721     C                   SETON                                        32
093400030721     C                   EVAL      VABNCL = *zeros
093500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093600030721     C                             + ' ' + 'VABNCL'
093700030721     C                   ENDIF
093800030721     C                   ENDIF
093900030721     C*** PKB
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
094100030721     C                   EVAL      PiStr=SkSplitCSV(i)
094200030721     C                   EXSR      CHKNUM
094300030721     C                   IF        PiNum=*on
094400030721     C                   Z-ADD     PiVal         VABPKB
094500030721     C                   ELSE
094600030721     C                   SETON                                        32
094700030721     C                   EVAL      VABPKB = *zeros
094800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094900030721     C                             + ' ' + 'VABPKB'
095000030721     C                   ENDIF
095100030721     C                   ENDIF
095200030721     C*** CAS
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
095400030721     C                   EVAL      PiStr=SkSplitCSV(i)
095500030721     C                   EXSR      CHKNUM
095600030721     C                   IF        PiNum=*on
095700030721     C                   Z-ADD     PiVal         VABCAS
095800030721     C                   ELSE
095900030721     C                   SETON                                        32
096000030721     C                   EVAL      VABCAS = *zeros
096100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096200030721     C                             + ' ' + 'VABCAS'
096300030721     C                   ENDIF
096400030721     C                   ENDIF
096500030721     C*** CCM
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
096700030721     C                   EVAL      PiStr=SkSplitCSV(i)
096800030721     C                   EXSR      CHKNUM
096900030721     C                   IF        PiInt=*on      AND
097000030721     C                             PiVal<=9999999 AND
097100030721     C                             PiVal>*zeros
097200030721     C                   Z-ADD     PiVal         VABCCM
097300030721     C                   ELSE
097400030721     C                   SETON                                        32
097500030721     C                   EVAL      VABCCM = *zeros
097600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097700030721     C                             + ' ' + 'VABCCM'
097800030721     C                   ENDIF
097900030721     C                   ENDIF
098000030721     C*** LNP
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
098200030721     C                   EVAL      PiStr=SkSplitCSV(i)
098300030721     C                   EXSR      CHKNUM
098400030721     C                   IF        PiInt=*on    AND
098500030721     C                             PiVal<=999   AND
098600030721     C                             PiVal>*zeros
098700030721     C                   Z-ADD     PiVal         VABLNP
098800030721     C                   ELSE
098900030721     C                   SETON                                        32
099000030721     C                   EVAL      VABLNP = *zeros
099100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
099200030721     C                             + ' ' + 'VABLNP'
099300030721     C                   ENDIF
099400030721     C                   ENDIF
099500030721     C*** NRS
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
099700030721     C                   EVAL      PiStr=SkSplitCSV(i)
099800030721     C                   EXSR      CHKNUM
099900030721     C                   IF        PiInt=*on AND
100000030721     C                             PiVal<=99
100100030721     C                   Z-ADD     PiVal         VABNRS
100200030721     C                   ELSE
100300030721     C                   SETON                                        32
100400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100500030721     C                             + ' ' + 'VABNRS'
100600030721     C                   ENDIF
100700030721     C                   ENDIF
100800030721     C*** LNA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
101000120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
101100120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
101200120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
101300120314     C                             %trim(SkSplitCSV(i)) <> *blanks
101400030721     C                   EVAL      PiStr=SkSplitCSV(i)
101500030721     C                   EXSR      CHKNUM
101600030721     C                   IF        PiInt=*on    AND
101700030721     C                             PiVal<=999   AND
101800030721     C                             PiVal>*zeros
101900030721     C                   Z-ADD     PiVal         VABLNA
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      VABLNA = *zeros
102300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102400030721     C                             + ' ' + 'VABLNA'
102500120314     C                   ENDIF
102600030721     C                   ENDIF
102700030721     C                   ENDIF
102800030721     C*** CTR
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
103000030721     C                   EVAL      PiStr=SkSplitCSV(i)
103100030721     C                   EXSR      CHKNUM
103200030721     C                   IF        PiInt=*on  AND
103300030721     C                             PiVal<=999
103400030721     C                   Z-ADD     PiVal         VABCTR
103500030721     C                   ELSE
103600030721     C                   SETON                                        32
103700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103800030721     C                             + ' ' + 'VABCTR'
103900030721     C                   ENDIF
104000030721     C                   ENDIF
104100030721     C*** IAS
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
104300030721     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiNum=*on
104600030721     C                   Z-ADD     PiVal         VABIAS
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABIAS'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** VLB
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiNum=*on
105800030721     C                   Z-ADD     PiVal         VABVLB
105900030721     C                   ELSE
106000030721     C                   SETON                                        32
106100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106200030721     C                             + ' ' + 'VABVLB'
106300030721     C                   ENDIF
106400030721     C                   ENDIF
106500030721     C*** QFT
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
106700030721     C                   EVAL      PiStr=SkSplitCSV(i)
106800030721     C                   EXSR      CHKNUM
106900030721     C                   IF        PiNum=*on
107000030721     C                   Z-ADD     PiVal         VABQFT
107100030721     C                   ELSE
107200030721     C                   SETON                                        32
107300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107400030721     C                             + ' ' + 'VABQFT'
107500030721     C                   ENDIF
107600030721     C                   ENDIF
107700030721     C*** NCD
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
107900030721     C                   EVAL      PiStr=SkSplitCSV(i)
108000030721     C                   EXSR      CHKNUM
108100030721     C                   IF        PiInt=*on      AND
108200030721     C                             PiVal<=9999999
108300030721     C                   Z-ADD     PiVal         VABNCD
108400030721     C                   ELSE
108500030721     C                   SETON                                        32
108600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108700030721     C                             + ' ' + 'VABNCD'
108800030721     C                   ENDIF
108900030721     C                   ENDIF
109000030721     C*** NCA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
109200030721     C                   EVAL      PiStr=SkSplitCSV(i)
109300030721     C                   EXSR      CHKNUM
109400030721     C                   IF        PiInt=*on      AND
109500030721     C                             PiVal<=9999999
109600030721     C                   Z-ADD     PiVal         VABNCA
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABNCA'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** ZNC
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on AND
110800030721     C                             PiVal<=99
110900030721     C                   Z-ADD     PiVal         VABZNC
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABZNC'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** DCR
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on       AND
112100030721     C                             PiVal<=99999999
112200030721     C                   Z-ADD     PiVal         VABDCR
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABDCR'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** HCR
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on   AND
113400030721     C                             PiVal<=9999
113500030721     C                   Z-ADD     PiVal         VABHCR
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABHCR'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** VMD
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABVMD
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABVMD'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** ANT
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on        AND
115900030721     C                             PiVal<=999999999
116000030721     C                   Z-ADD     PiVal         VABANT
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABANT'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** CMO
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on
117200030721     C                   Z-ADD     PiVal         Num5_0
117300030721     C                   MOVEL(p)  Num5_0        VABCMO
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCMO = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABCMO'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030715     C*
118200030715     C                   ENDIF
118300030715     C                   EVAL      i = i + 1
118400030715     C                   ENDDO
118500070705     C*
118600070705     C* Considerazioni particolari
118700110706     C*
118800110706     C*** RMN
118900110706     C                   IF        *in70
119000110706     C                   EVAL      VABRMN = VABNSP
119100110706     C                   ENDIF
119200070705     C*** NZD
119300070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
119400070705     C                   EVAL      VABNZD = *blanks
119500070705     C                   ENDIF
119600070705     C*** CAD
119700070705     C                   IF        VABNZD = *blanks
119800070705     C                   EVAL      PiStr=VABCAD
119900070705     C                   EXSR      CHKNUM
120000070705     C                   IF        PiInt=*on
120100070705     C                   Z-ADD     PiVal         Num5_0
120200070705     C                   MOVEL(p)  Num5_0        VABCAD
120300070705     C                   ELSE
120400070705     C                   SETON                                        32
120500070705     C                   EVAL      VABCAD = *zeros
120600070705     C                   EVAL      vinmsg = %trimr(vinmsg)
120700070705     C                             + ' ' + 'VABCAD'
120800070705     C                   ENDIF
120900070705     C                   ENDIF
121000080222     C*
121100080222     C* Se provincia nn valorizzata la reperisco
121200080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
121300080222     C                   IF        VABPRD = *blanks
121400080222     C                   CLEAR                   TISI95DS
121500080222     C                   EVAL      I95TCN = '3'
121600080222     C                   Z-ADD     datcor        I95DAT
121700080222     C                   EVAL      I95NAR = VABNZD
121800080222     C                   EVAL      I95CAP = VABCAD
121900080222     C                   EVAL      I95LOC = VABLOD
122000080222     C                   CALL      'TISI95R'
122100080222     C                   PARM                    TISI95DS
122200080222     C                   EVAL      VABPRD = O95PRV
122300080222     C                   ENDIF
122400020204     C*
122500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
122600020204     C                   EXSR      CHKIMPDIV
122700030715     C                   ENDIF
122800020204     C*
122900010607     C                   ENDSR
123000010607     C*----------------------------------------------------*
123100010601
123200020204
123300020204     C*----------------------------------------------------*
123400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
123500020204     C*----------------------------------------------------*
123600020204     C     CHKIMPDIV     BEGSR
123700060524     C*
123800060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
123900060524     C                   IF        vabCAS > *zeros
124000060524     C                   IF        vabCBO = '1'
124100060524     C                   EVAL      vabCBO = '4'
124200060524     C                   ENDIF
124300060524     C                   IF        vabCBO = '2'
124400060524     C                   EVAL      vabCBO = '6'
124500060524     C                   ENDIF
124600060524     C                   ENDIF
124700020204     C*
124800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
124900020204     C                   Z-ADD     *zeros        wrkDec            9 9
125000020204     C*
125100020204     C* Come prima cosa effettuo considerazioni sulla divisa
125200020204     C                   IF        vabIAS > *zeros
125300020204     C                   IF        vabVAS <> 'EUR'
125400020204     C                   EVAL      vabVAS =  'ITL'
125500020204     C                   ENDIF
125600020204     C                   ENDIF
125700020204     C*
125800020204     C                   IF        vabCAS > *zeros
125900020204     C                   IF        vabVCA <> 'EUR'
126000020204     C                   EVAL      vabVCA =  'ITL'
126100020204     C                   ENDIF
126200020204     C                   ENDIF
126300020204     C*
126400020204     C                   IF        vabVMD > *zeros
126500020204     C                   IF        vabVAD <> 'EUR'
126600020204     C                   EVAL      vabVAD =  'ITL'
126700020204     C                   ENDIF
126800020204     C                   ENDIF
126900020204     C*
127000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
127100020204     C                   Z-ADD     vabIAS        wrkDec
127200020204     C                   IF        wrkDec > *zeros
127300020204     C                   IF        vabVAS = 'ITL'
127400020204     C                   EVAL      vabIAS = *zeros
127500020204     C                   ENDIF
127600020204     C                   ENDIF
127700020204     C*
127800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
127900020204     C                   Z-ADD     vabCAS        wrkDec
128000020204     C                   IF        wrkDec > *zeros
128100020204     C                   IF        vabVCA = 'ITL'
128200020204     C                   EVAL      vabCAS = *zeros
128300020204     C                   ENDIF
128400020204     C                   ENDIF
128500020204     C*
128600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
128700020204     C                   Z-ADD     vabVMD        wrkDec
128800020204     C                   IF        wrkDec > *zeros
128900020204     C                   IF        vabVAD = 'ITL'
129000020204     C                   EVAL      vabVMD = *zeros
129100020204     C                   ENDIF
129200020204     C                   ENDIF
129300020204     C*
129400020204     C                   ENDSR
129500020204     C***
129600020204
129700010330
129800010330
129900010330     C*----------------------------------------------------*
130000010330     C*  CONTROLLO NUMERICITA' CAMPI
130100010330     C*----------------------------------------------------*
130200010330     C     CHKNUM        BEGSR
130300010330     C*
130400010606     C                   IF        PiDecChr = *blanks
130500030715     C                   EVAL      PiDecChr = CharNUM
130600010606     C                   ENDIF
130700010606     C*
130800010606     C                   CALL(e)   'ISNUMERIC'
130900010330     C                   PARM                    PiStr            30
131000010606     C                   PARM                    PiDecChr          1
131100010330     C                   PARM      *ZEROS        PiVal            30 9
131200010330     C                   PARM      '0'           PiInt             1
131300010330     C                   PARM      '0'           PiNum             1
131400010330     C                   IF        %error
131500010606     C                   EVAL      PiNum=*off
131600010330     C                   ENDIF
131700010330     C*
131800010330     C                   ENDSR
131900010330     C***
132000010330
132100010601
132200010601
132300010601
132400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
132500010601     C     repfil        BEGSR
132600010601     C*
132700010601     C                   if        invfil = *zeros and
132800010601     C                             depfil > *zeros and
132900010629     C                             (vinflg = *blanks or
133000010629     C                              vinflg = *zeros)
133100010601     C
133200010601     C                   eval      invfil = depfil
133300010601     C                   endif
133400010601     C*
133500010601     C                   if        depfil <> invfil and
133600010601     C                             invfil > *zeros
133700010601     C                   eval      flgMulti = '1'
133800010601     C                   if        vinflg = *blanks
133900010601     C                   add       1             cntNonEl
134000010601     C                   endif
134100010601     C                   endif
134200010601     C*
134300010601     C                   if        vinflg = '2'
134400010601     C                   eval      flgStato = '2'
134500010601     C                   endif
134600010601     C*
134700010601     C                   ENDSR
134800010601     C***
134900010601
135000010601
135100010601
135200010330
135300010330
135400010330
135500990920      /TITLE Invio dei dati al punto operativo.
135600000613     C     invio         BEGSR
135700990920     C*
135800990920     C                   reset                   dscmz
135900010601     C                   move      invfil        cmzdst
136000990920     C                   eval      cmzfld = vlrfou
136100990920     C                   eval      cmzmbd = vlrhdl
136200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
136300000710     C                   if        prmfir = *blanks
136400021025     C                   eval      cmzfla = 'FIVAB00F'
136500021025     C                   eval      cmzmba = 'FIVAB00F'
136600000710     C                   else
136700000710     C                   eval      cmzfla = prmfir
136800000710     C                   eval      cmzmba = prmfir
136900000710     C                   endif
137000990920     C                   eval      cmznrr = *zeros
137100990920     C                   move      §ctrok        cmznrr
137200021018     C                   eval      cmzlba = vlrfl1
137300990920     C                   call(e)   'TIS711C'
137400990920     C                   parm                    dscmz
137500990921     C                   parm      *blanks       esito
137600990920     C                   if        %error
137700990920     C                             or cmzerr = '1'
137800990921     C                             or esito  = '1'
137900000710     C                   eval      wrkesito = '3'
138000990920     C                   endif
138100990920     C*
138200000613     C                   ENDSR
138300990910
138400010601
138500010601
138600010601
138700010601
138800010601      /TITLE Invio dei dati al punto operativo.
138900010601     C     opeini        BEGSR
139000010601     C*
139100010601     C* Inizializzo flag e contatori operativi
139200010601     C                   movel     '0'           flgGiro           1
139300010601     C                   movel     '0'           flgMulti          1
139400010601     C                   movel     '1'           flgStato          1
139500010615     C                   movel     '0'           flgOk             1
139600010601     C                   z-add     *zeros        cntNonEl         10 0
139700010601     C                   z-add     *zeros        depfil            3 0
139800010601     C                   z-add     *zeros        invfil            3 0
139900010601     C*
140000010601     C                   ENDSR
140100010601     C***
140200050421
140300050421
140400050421
140500050421     C     *pssr         BEGSR
140600050421     C*
140700050421     C                   eval      wrkesito = '2'
140800050421     C*
140900050421     C                   ENDSR     '*CANCL'
141000050421     C***
141100100419
141200100419
141300100419
141400100419     C     normCSV       BEGSR
141500100419     C*
141600100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
141700100419     C* separatori campi
141800100419     C                   if        CharTXT <> *blanks
141900100419     C                   z-add     *zeros        wLenVINDTA        4 0
142000100419     C                   z-add     *zeros        wFlgVINDTA        1 0
142100100419     C                   setoff                                       80
142200100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
142300100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
142400100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
142500100419     C                   if        not *in80
142600100419     C                   seton                                        80
142700100419     C                   else
142800100419     C                   setoff                                       80
142900100419     C                   endif
143000100419     C                   endif
143100100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
143200100419     C                             and *in80 = *on
143300100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
143400100419     C                   endif
143500100419     C                   enddo
143600100419     C                   endif
143700100419     C*
143800100419     C                   ENDSR
143900100419     C***
144000010601
144100010330
144200010330
144300000613     C     *inzsr        BEGSR
144400990910     C*
144500990910     C     *entry        plist
144600990920     C                   parm                    tivlrds
144700990921     C                   parm      wrkesito      esito
144800000724     C                   parm                    prmlit
144900000710     C                   parm                    prmfir
145000010330     C*
145100010330     C* CALCOLA LA DATA CORRENTE
145200010330     C                   time                    wn14             14 0
145300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
145400010330     C                   z-add     wn8           g08dat
145500010330     C                   z-add     *zeros        g08inv
145600010330     C                   movel     '0'           g08err
145700010330     C                   call      'XSRDA8'
145800010330     C                   parm                    wlbda8
145900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
146000000613     C*
146100000613     C                   ENDSR
146200000613     C***
