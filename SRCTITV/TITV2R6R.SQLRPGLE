000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200140120     H dftactgrp(*no) actgrp(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500140114     FTIVGD00F  O    E             DISK
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400140114     D fivabds       e ds                  extname(fivab00f)
001500140114     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900010330     D wrkesito        s                   like(esito)
002000010330     D rrnum           s              6  0 INZ(*zeros)
002100030715     D*------------------
002200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002300030715     D*------------------
002400030715     D SkSplitFLD      S             10    DIM(100)
002500030715     D SkSplitCSV      S            256    DIM(100)
002600030715     D CharCSV         S              1
002700030715     D CharTXT         S              1
002800030715     D CharNUM         S              1
002900090403     D CharSOS         S              1
003000030715     D posDa           S              3  0 INZ(*zeros)
003100030715     D posA            S              3  0 INZ(*zeros)
003200030715     D i               s              3  0 INZ(1)
003300030715     D wGiro           s              1  0 INZ(*zeros)
003400030822     D*------------------
003500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
003600030822     D*------------------
003700030822     D posDaDft        S              3  0 INZ(*zeros)
003800030822     D posADft         S              3  0 INZ(*zeros)
003900030822     D j               s              3  0 INZ(1)
004000030822     D wGiroDft        s              1  0 INZ(*zeros)
004100010330     D*------------------
004200010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004300010330     D*------------------
004400010330     D WLBDA8          DS                  INZ
004500010330     D  G08DAT                 1      8  0
004600010330     D  G08INV                 9     16  0
004700010330     D  G08ERR                17     17
004800010330     D  G08TGI                18     22  0
004900031201     D*------------------
005000031201     D* Costanti
005100031201     D*------------------
005200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005400010330
005500010330
005600010330
005700000913     C                   reset                   rrnum
005800990921     C                   reset                   esito
005900990921     C                   reset                   wrkesito
006000010601     C*
006100010601     C                   exsr      opeini
006200010605     C                   exsr      rwvab
006300010601     C*
006400010601     C                   seton                                        lr
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007100010601     C*--------------------------------------------------------
007200010601     C     PREELA        BEGSR
007300010601     C*
007400010601     C* SE OCCORRE SPEDIRE IN FILIALE
007500010601     C                   if        invfil <> *zeros and
007600010601     C                             flgGiro = '0'
007700010601     C*
007800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007900010601     C                   eval      flgGiro = '1'
008000010601     C*
008100010601     C                   endif
008200010601     C*
008300010601     C                   ENDSR
008400010601     C***
008500010601
008600010601
008700010601
008800010601
008900010601     C*--------------------------------------------------------
009000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009100010601     C*--------------------------------------------------------
009200010601     C     ENDELA        BEGSR
009300080222     C*
009400080222     C* Effettuo lancio TISI95 solo x chiusura
009500080222     C                   CLEAR                   TISI95DS
009600080222     C                   EVAL      I95TLA = 'C'
009700080222     C                   CALL      'TISI95R'
009800080222     C                   PARM                    TISI95DS
009900000613     C*
010000010601     C                   ENDSR
010100010601     C***
010200010330
010300010601
010400010601
010500010601
010600010330     C*--------------------------------------------------------
010700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010800010330     C*--------------------------------------------------------
010900010605     C     RWVAB         BEGSR
011000010330     C*
011100010330     C                   if        not %open(tivin00r)
011200010330     C                   open      tivin00r
011300010330     C                   endif
011400010330     C*
011500010604     C                   clear                   §CTROK
011600010604     C                   clear                   §CTRMO
011700010604     C                   clear                   §CTRNO
011800010330     C*
011900030822     C                   do        *HIVAL
012000010330     C*
012100030822     C                   read      tivin00r                               70
012200010618     C*
012300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012400010618     C                   if        vinflg = '1'
012500010618     C                   eval      flgOk = '1'
012600010618     C                   endif
012700010618     C*
012800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012900040510     C                   z-add     *zeros        wLenVINDTA        4 0
013000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013400040510     C                   z-add     1             wFlgVINDTA
013500040510     C                   leave
013600040510     C                   endif
013700040510     C                   enddo
013800040510     C*
013900040218     C                   if        vindta > *blanks AND
014000040510     C                             wFlgVINDTA = 1
014100010330     C                   add       1             rrnum
014200010330     C*
014300010601     C                   if        *in70 = *off and
014400010330     C                             (vinflg = *blanks
014500010330     C                              or vinflg = '0'
014600010330     C                              or vinflg = '2')
014700010330     C*
014800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014900010711     C                   if        vinflg = *blanks or vinflg = '0'
015000010711     C                   clear                   vinmsg
015100010711     C                   endif
015200010601     C*
015300010330     C                   exsr      inzvar
015400100419     C                   exsr      normCSV
015500090403     C*
015600090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015700090403     C* separatori campi
015800090403     C                   if        CharTXT <> *blanks
015900090403     C                   z-add     *zeros        wLenVINDTA        4 0
016000090403     C                   z-add     *zeros        wFlgVINDTA        1 0
016100090403     C                   setoff                                       80
016200090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016300090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016400090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016500090403     C                   if        not *in80
016600090403     C                   seton                                        80
016700090403     C                   else
016800090403     C                   setoff                                       80
016900090403     C                   endif
017000090403     C                   endif
017100090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017200090403     C                             and *in80 = *on
017300090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017400090403     C                   endif
017500090403     C                   enddo
017600090403     C                   endif
017700090403     C*
017800010605     C                   exsr      impvab
017900010601     C*
018000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018100010605     C                   eval      depfil = VABLNP
018200010601     C                   exsr      repfil
018300060331     C                   if        VABFGS = *zeros
018400010601     C                   if        depfil = invfil
018500021025     C                   if        vlrpoi = 999
018600030822     C                   move(P)   invfil        VABFGS
018700021025     C                   else
018800030822     C                   move(P)   vlrpoi        VABFGS
018900021025     C                   endif
019000050113     C                   endif
019100060331     C                   endif
019200010601     C*
019300010601     C                   exsr      PREELA
019400010601     C*
019500010604     C* Ebbene...
019600010604     C*
019700030822     C  N31              add       1             §CTROK            7 0
019800030822     C   32              add       1             §CTRMO            7 0
019900030822     C   31              add       1             §CTRNO            7 0
020000140114     C                   exsr      wrivgd
020100010604     C*
020200010604     C                   if        *in31 = *off and
020300010604     C                             *in32 = *off
020400010604     C                   eval      vinflg = '1'
020500010604     C                   else
020600010604     C                   eval      vinflg = '2'
020700010604     C                   endif
020800010601     C*
020900010604     C                   endif
021000010604     C*
021100010330     C                   else
021200010330     C                   eval      vinflg = '1'
021300010330     C                   endif
021400010601     C*
021500010601     C  N70              update    tivin000
021600010330     C*
021700030822     C  N70              enddo
021800010601     C
021900010601     C*
022000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022100010601     C                   if        cntNonEl = *zeros or
022200010601     C                             flgMulti = '0'
022300010330     C* Se non ci sono record con errori ...
022400010601     C                   if        §ctrno = 0 and
022500010604     C                             §ctrmo = 0 and
022600010601     C                             flgStato <> '2'
022700010330     C* ... restituisco esito OK.
022800010330     C                   eval      wrkesito = '0'
022900010330     C                   else
023000010330     C                   if        §ctrok > 0
023100010330     C                   eval      wrkesito = '1'
023200010330     C                   else
023300010615     C                   if        flgOk = '0'
023400010615     C                   eval      wrkesito = '2'
023500010615     C                   else
023600010615     C                   eval      wrkesito = '6'
023700010615     C                   endif
023800010330     C                   endif
023900010330     C                   endif
024000010601     C                   else
024100010601     C                   eval      wrkesito = '9'
024200010601     C                   endif
024300010330     C*
024400010330     C                   if        %open(tivin00r)
024500010330     C                   close     tivin00r
024600010330     C                   endif
024700010601     C*
024800010601     C                   if        vlrpoi <> 999
024900010601     C                   eval      invfil = vlrpoi
025000010601     C                   endif
025100010330     C*
025200010330     C                   if        §ctrok > 0
025300010601     C                             and invfil > *zeros
025400010330     C                   exsr      invio
025500010330     C                   endif
025600010601     C*
025700010618     C                   if        flgGiro = '1'
025800010601     C                   exsr      endela
025900010618     C                   endif
026000010330     C*
026100010330     C                   ENDSR
026200010330     C***
026300140114
026400140114
026500140114
026600140114      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
026700140114     C     wriVGD        BEGSR
026800140114     C*
026900140114     C                   clear                   tivgd000
027000140114     C*
027100140114     C                   if        wGiro = 1
027200140116     C
027300140116      /Free
027400140203         Exec SQL   set :vinDTA = replace(:vinDTA, 'VABPRD', 'VOID');
027500140116      /End-Free
027600140116     C
027700140116     C                   eval      vgdDTA = 'VABPRD' + CharCSV + vindta
027800140114     C                   else
027900140116     C                   eval      vgdDTA = VABPRD + CharCSV + vindta
028000140114     C                   endif
028100140114     C*
028200140114     C                   eval      vgdTIP = 'FP'
028300140114     C                   eval      vgdKSU = vlrKSC
028400140114     C                   eval      vgdTSC = 'WW'
028500140114     C                   eval      vgdDAT = datcor
028600140116     C                   eval      vgdPGM = 'TITV2R6R'
028700140114     C                   write     tivgd000
028800140114     C*
028900140114     C                   ENDSR
029000140114     C*------------------------------------------------------------------------*
029100140114
029200140114
029300140114
029400010601
029500010601
029600010601
029700010330     C*----------------------------------------------------*
029800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029900010330     C*----------------------------------------------------*
030000010330     C     INZVAR        BEGSR
030100010330     C*
030200030822     C* Inizializzo variabili di wrk
030300020204     C                   Z-ADD     *zeros        Num5_0            5 0
030400090403     C                   z-add     *zeros        invfil
030500030822     C*
030600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
030700140114     C                   CLEAR                   FIVABDS
030800030822     C                   CLEAR                   SkSplitCSV
030900030822     C                   EVAL      i = 1
031000030822     C                   EVAL      posDa = *zeros
031100030822     C                   EVAL      posA  = *zeros
031200030822     C*
031300030822     C* Reimposto i valori di default
031400030822     C                   EXSR      DEFCAM
031500030822     C*
031600010330     C                   ENDSR
031700010330     C*----------------------------------------------------*
031800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
031900010330     C*----------------------------------------------------*
032000010330     C     DEFCAM        BEGSR
032100110706     C*
032200110706     C                   SETOFF                                       70
032300030822     C*
032400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032500030715     C* e delimitatore testo.
032600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
032700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
032800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
032900090403     C*
033000090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
033100090403     C                   EVAL      CharSOS = CharNUM
033200030822     C*
033300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
033400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
033500030715     C                   SETON                                        50
033600030715     C                   ELSE
033700030715     C                   SETOFF                                       50
033800030715     C                   ENDIF
033900030822     C*
034000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
034100030822     C                   EVAL      posDaDft = 1
034200030822     C                   EVAL      posADft  = 0
034300030822     C                   EVAL      wGiroDft = 0
034400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034500030822     C                             posDaDft > 0
034600030822     C*
034700030822     C* Gestisco il 1° giro
034800030822     C                   IF        wGiroDft = 0
034900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
035000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
035100030822     C* Incremento il contatore dei "giri"
035200030822     C                   EVAL      wGiroDft = 1
035300030822     C                   ELSE
035400030822     C                   EVAL      posDaDft = posADft
035500030822     C                   ENDIF
035600030822     C* Eseguo lo scan x trovare la fine del campo corrente
035700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
035800030822     C*
035900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
036000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
036100030822     C                   IF        posDaDft > 0 AND
036200030822     C                             posADft  > 0
036300110706     C* RMN
036400110706     C                   IF        %subst(
036500110706     C                             %subst(vlrppt:posDaDft+1:
036600110706     C                             posADft-posDaDft-1):1:3)
036700110706     C                             = 'RMN'
036800110706     C                   IF        %trim(%subst(
036900110706     C                             %subst(vlrppt:posDaDft+1:
037000110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037100110706     C                             = '*NSP'
037200110706     C                   SETON                                        70
037300110706     C                   ENDIF
037400110706     C                   ENDIF
037500060331     C* FGS
037600060331     C                   IF        %subst(
037700060331     C                             %subst(vlrppt:posDaDft+1:
037800060331     C                             posADft-posDaDft-1):1:3)
037900060331     C                             = 'FGS'
038000060331     C                   EVAL      PiStr=%trim(%subst(
038100060331     C                             %subst(vlrppt:posDaDft+1:
038200060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038300060331     C                   EXSR      CHKNUM
038400060331     C                   IF        PiInt=*on
038500060331     C                   Z-ADD     PiVal         VABFGS
038600060331     C                   ENDIF
038700060331     C                   ENDIF
038800030822     C* NCL
038900030822     C                   IF        %subst(
039000030822     C                             %subst(vlrppt:posDaDft+1:
039100030822     C                             posADft-posDaDft-1):1:3)
039200030822     C                             = 'NCL'
039300030822     C                   EVAL      PiStr=%trim(%subst(
039400030822     C                             %subst(vlrppt:posDaDft+1:
039500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039600030822     C                   EXSR      CHKNUM
039700030822     C                   IF        PiInt=*on
039800030822     C                   Z-ADD     PiVal         VABNCL
039900030822     C                   ENDIF
040000030822     C                   ENDIF
040100030822     C* CCM
040200030822     C                   IF        %subst(
040300030822     C                             %subst(vlrppt:posDaDft+1:
040400030822     C                             posADft-posDaDft-1):1:3)
040500030822     C                             = 'CCM'
040600030822     C                   EVAL      PiStr=%trim(%subst(
040700030822     C                             %subst(vlrppt:posDaDft+1:
040800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
040900030822     C                   EXSR      CHKNUM
041000030822     C                   IF        PiInt=*on
041100030822     C                   Z-ADD     PiVal         VABCCM
041200030822     C                   ENDIF
041300030822     C                   ENDIF
041400030822     C* LNP
041500030822     C                   IF        %subst(
041600030822     C                             %subst(vlrppt:posDaDft+1:
041700030822     C                             posADft-posDaDft-1):1:3)
041800030822     C                             = 'LNP'
041900030822     C                   EVAL      PiStr=%trim(%subst(
042000030822     C                             %subst(vlrppt:posDaDft+1:
042100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042200030822     C                   EXSR      CHKNUM
042300030822     C                   IF        PiInt=*on
042400030822     C                   Z-ADD     PiVal         VABLNP
042500030822     C                   ENDIF
042600030822     C                   ENDIF
042700030822     C* NRS
042800030822     C                   IF        %subst(
042900030822     C                             %subst(vlrppt:posDaDft+1:
043000030822     C                             posADft-posDaDft-1):1:3)
043100030822     C                             = 'NRS'
043200030822     C                   EVAL      PiStr=%trim(%subst(
043300030822     C                             %subst(vlrppt:posDaDft+1:
043400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043500030822     C                   EXSR      CHKNUM
043600030822     C                   IF        PiInt=*on
043700030822     C                   Z-ADD     PiVal         VABNRS
043800030822     C                   ENDIF
043900030822     C                   ENDIF
044000030822     C* CTR
044100030822     C                   IF        %subst(
044200030822     C                             %subst(vlrppt:posDaDft+1:
044300030822     C                             posADft-posDaDft-1):1:3)
044400030822     C                             = 'CTR'
044500030822     C                   EVAL      PiStr=%trim(%subst(
044600030822     C                             %subst(vlrppt:posDaDft+1:
044700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
044800030822     C                   EXSR      CHKNUM
044900030822     C                   IF        PiInt=*on
045000030822     C                   Z-ADD     PiVal         VABCTR
045100030822     C                   ENDIF
045200030822     C                   ENDIF
045300030822     C* PKB
045400030822     C                   IF        %subst(
045500030822     C                             %subst(vlrppt:posDaDft+1:
045600030822     C                             posADft-posDaDft-1):1:3)
045700030822     C                             = 'PKB'
045800030822     C                   EVAL      PiStr=%trim(%subst(
045900030822     C                             %subst(vlrppt:posDaDft+1:
046000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046100030822     C                   EXSR      CHKNUM
046200030822     C                   IF        PiNum=*on
046300030822     C                   Z-ADD     PiVal         VABPKB
046400030822     C                   ENDIF
046500030822     C                   ENDIF
046600030822     C* VLB
046700030822     C                   IF        %subst(
046800030822     C                             %subst(vlrppt:posDaDft+1:
046900030822     C                             posADft-posDaDft-1):1:3)
047000030822     C                             = 'VLB'
047100030822     C                   EVAL      PiStr=%trim(%subst(
047200030822     C                             %subst(vlrppt:posDaDft+1:
047300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047400030822     C                   EXSR      CHKNUM
047500030822     C                   IF        PiNum=*on
047600030822     C                   Z-ADD     PiVal         VABVLB
047700030822     C                   ENDIF
047800030822     C                   ENDIF
047900030822     C* QFT
048000030822     C                   IF        %subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):1:3)
048300030822     C                             = 'QFT'
048400030822     C                   EVAL      PiStr=%trim(%subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048700030822     C                   EXSR      CHKNUM
048800030822     C                   IF        PiNum=*on
048900030822     C                   Z-ADD     PiVal         VABQFT
049000030822     C                   ENDIF
049100030822     C                   ENDIF
049200030822     C* CBO
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'CBO'
049700030822     C                   EVAL      VABCBO=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   ENDIF
050100030822     C* TSP
050200030822     C                   IF        %subst(
050300030822     C                             %subst(vlrppt:posDaDft+1:
050400030822     C                             posADft-posDaDft-1):1:3)
050500030822     C                             = 'TSP'
050600030822     C                   EVAL      VABTSP=%trim(%subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050900030822     C                   ENDIF
051000030822     C* VAS
051100030822     C                   IF        %subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):1:3)
051400030822     C                             = 'VAS'
051500030822     C                   EVAL      VABVAS=%trim(%subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051800030822     C                   ENDIF
051900030822     C* VCA
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'VCA'
052400030822     C                   EVAL      VABVCA=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   ENDIF
052800030822     C* GCA
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'GCA'
053300030822     C                   EVAL      VABGCA=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   ENDIF
053700030822     C* CTM
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'CTM'
054200030822     C                   EVAL      VABCTM=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   ENDIF
054600030822     C* FFD
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'FFD'
055100030822     C                   EVAL      VABFFD=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   ENDIF
055500030822     C* VAD
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'VAD'
056000030822     C                   EVAL      VABVAD=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   ENDIF
056400030822     C* GMA
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'GMA'
056900030822     C                   EVAL      VABGMA=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* GGA
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'GGA'
057800030822     C                   EVAL      VABGGA=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   ENDIF
058200030822     C* GVA
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'GVA'
058700030822     C                   EVAL      VABGVA=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   ENDIF
059100030822     C* TC1
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'TC1'
059600030822     C                   EVAL      VABTC1=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   ENDIF
060000030822     C* TC2
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'TC2'
060500030822     C                   EVAL      VABTC2=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   ENDIF
060900030822     C* ...
061000030822     C                   ENDIF
061100030822     C                   ENDDO
061200020204     C*
061300010330     C                   ENDSR
061400010607     C*----------------------------------------------------*
061500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
061600010607     C*----------------------------------------------------*
061700010607     C     IMPVAB        BEGSR
061800010607     C*
061900010607     C                   SETOFF                                       3132
062000030515     C*
062100010607     C                   MOVEL     datcor        VABAAS
062200010607     C                   MOVE      datcor        VABMGS
062300010607     C*
062400030715     C**********
062500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
062600030715     C**********
062700030715     C                   IF        wGiro = *zeros
062800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
062900031201     C     minu:maiu     XLATE     vindta        vindta
063000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
063100040324     C                   DOW       posDa <= %len(%trimr(vindta))
063200030715     C*
063300030715     C* Gestisco il 1° campo
063400030715     C                   IF        i = 1
063500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
063600030715     C                   EVAL      posDa = 1
063700030715     C* Eseguo lo scan x trovare la fine del primo campo
063800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
063900030715     C* A questo punto "estraggo" il campo corrente
064000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064100030715     C                                                    (posA-posDa))
064200030715     C* X i campi successivi al 1°
064300030715     C                   ELSE
064400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
064500030715     C                   EVAL      posDa = posA + 1
064600030715     C* Eseguo lo scan x trovare la fine del campo corrente
064700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064800030715     C* Gestisco l'ultimo campo
064900030715     C                   IF        posA = *zeros
065000040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
065100030715     C* A questo punto "estraggo" il campo corrente
065200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065300030715     C                                                    (posA-posDa))
065400030715     C                   LEAVE
065500030715     C                   ELSE
065600030715     C* A questo punto "estraggo" il campo corrente
065700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065800030715     C                                                    (posA-posDa))
065900030715     C                   ENDIF
066000030715     C                   ENDIF
066100030715     C* Incremento il contatore d campo
066200030715     C                   EVAL      i = i +1
066300030715     C                   ENDDO
066400030715     C*
066500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
066600030715     C* delimitazione testo
066700030715     C                   EVAL      i = 1
066800030715     C                   DOW       i < %elem(SkSplitFLD)
066900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
067000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
067100030715     C                   EVAL      i = i + 1
067200030715     C                   ENDDO
067300030715     C                   EVAL      wGiro = 1
067400030715     C                   ELSE
067500030715     C                   EVAL      wGiro = 2
067600031204     C**********
067700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
067800031204     C* d separatore campo
067900031204     C**********
068000040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
068100040304     C                   EVAL      vindta = ' ' + vindta
068200040304     C                   ENDIF
068300040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
068400040304     C***                EVAL      vindta = %subst(vindta:2)
068500040304     C***                ENDDO
068600031204     C*
068700031204     C                   Z-ADD     *zeros        lunghInput        4 0
068800040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
068900040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
069000040304     C                   EVAL      vindta = vindta + ' '
069100040304     C                   ENDIF
069200040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
069300040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
069400040304     C***                EVAL      lunghInput = %len(%trim(vindta))
069500040304     C***                ENDDO
069600030715     C**********
069700030715     C* Effettuo lo split del campo dati d input
069800030715     C**********
069900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070000040324     C                   DOW       posDa <= %len(%trimr(vindta))
070100030715     C*
070200030715     C* Gestisco il 1° campo
070300030715     C                   IF        i = 1
070400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070500030715     C                   EVAL      posDa = 1
070600030715     C* Eseguo lo scan x trovare la fine del primo campo
070700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070800030715     C* A questo punto "estraggo" il campo corrente
070900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071000030715     C                                                    (posA-posDa))
071100030715     C* X i campi successivi al 1°
071200030715     C                   ELSE
071300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071400030715     C                   EVAL      posDa = posA + 1
071500031204     C* Verifico che nn vi sia il campo nn valorizzato
071600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
071700031204     C* Se campo nn valorizzato skippo al prossimo
071800031204     C                   EVAL      SkSplitCSV(i) = *blanks
071900031204     C                   EVAL      posA  = posA + 1
072000031204     C                   ELSE
072100030715     C* Eseguo lo scan x trovare la fine del campo corrente
072200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072300030715     C* Gestisco l'ultimo campo
072400030715     C                   IF        posA = *zeros
072500040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
072600030715     C* A questo punto "estraggo" il campo corrente
072700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
072800030715     C                                                    (posA-posDa))
072900030715     C                   LEAVE
073000030715     C                   ELSE
073100030715     C* A questo punto "estraggo" il campo corrente
073200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073300030715     C                                                    (posA-posDa))
073400030715     C                   ENDIF
073500031204     C                   ENDIF
073600030715     C                   ENDIF
073700030715     C* Incremento il contatore d campo
073800030715     C                   EVAL      i = i +1
073900030715     C                   ENDDO
074000030715     C*
074100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074200030715     C* delimitazione testo
074300030715     C                   EVAL      i = 1
074400030715     C                   DOW       i < %elem(SkSplitCSV)
074500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
074600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
074700030715     C                   EVAL      i = i + 1
074800030715     C                   ENDDO
074900030715     C*
075000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
075100030715     C                   EVAL      i = 1
075200030715     C                   DOW       i < %elem(SkSplitFLD)
075300030715     C*
075400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
075500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
075600030715     C                             SkSplitCSV(i) <> *blanks
075700130624     C*** ATB
075800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
075900130624     C                   IF        SkSplitCSV(i) = 'D'
076000130624     C                   EVAL      VABCTR = 390
076100130624     C                   EVAL      VABCTM = '7R'
076200130624     C                   ENDIF
076300030721     C                   ENDIF
076400030715     C*** RSD
076500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
076600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
076700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
076800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
076900030715     C* ==
077000030715     C                   ENDIF
077100030721     C*** RD2
077200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
077300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
077400030721     C                   ENDIF
077500030715     C*** IND
077600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
077700030715     C                   EVAL      VABIND = SkSplitCSV(i)
077800030715     C                   ENDIF
077900030715     C*** LOD
078000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
078100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
078200030715     C                   ENDIF
078300030715     C*** PRD
078400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
078500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
078600030715     C                   ENDIF
078700030721     C*** NZD
078800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
078900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
079000030721     C                   ENDIF
079100030715     C*** RMA
079200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
079300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
079400030715     C                   ENDIF
079500030715     C*** NOT
079600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
079700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
079800030715     C                   ENDIF
079900030715     C*** NT2
080000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
080100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
080200030715     C                   ENDIF
080300030715     C*** VCA
080400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
080500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
080600030715     C                   ENDIF
080700030715     C*** VAS
080800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
080900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
081000030715     C                   ENDIF
081100030715     C*** FFD
081200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
081300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
081400030715     C                   ENDIF
081500030715     C*** CBO
081600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
081700030715     C                   EVAL      VABCBO = SkSplitCSV(i)
081800030715     C                   ENDIF
081900030721     C*** GC1
082000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
082100120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
082200120314     C                   MOVEL     *blanks       wGC               1
082300120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
082400120314     C                   IF        SkSplitCSV(i) = 'M' OR
082500120314     C                             SkSplitCSV(i) = 'P'
082600120314     C                   MOVE(P)   wGC           VABGC1
082700120314     C                   ELSE
082800120314     C                   MOVEL(P)  wGC           VABGC1
082900120314     C                   ENDIF
083000120314     C                   ENDIF
083100030721     C                   ENDIF
083200030721     C*** GC2
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
083400120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
083500120314     C                   MOVEL     *blanks       wGC               1
083600120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
083700120314     C                   IF        SkSplitCSV(i) = 'M' OR
083800120314     C                             SkSplitCSV(i) = 'P'
083900120314     C                   MOVE(P)   wGC           VABGC2
084000120314     C                   ELSE
084100120314     C                   MOVEL(P)  wGC           VABGC2
084200120314     C                   ENDIF
084300120314     C                   ENDIF
084400030721     C                   ENDIF
084500030721     C*** TSP
084600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
084700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
084800030721     C                   ENDIF
084900030721     C*** NAS
085000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085200030721     C                   ENDIF
085300030721     C*** TIC
085400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085600030721     C                   ENDIF
085700030721     C*** GCA
085800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
085900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086000030721     C                   ENDIF
086100030721     C*** XCO
086200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086400030721     C                   ENDIF
086500030721     C*** CTM
086600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
086700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
086800030721     C                   ENDIF
086900030721     C*** TCR
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030721     C*** CTS
087400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087600030721     C                   ENDIF
087700030721     C*** FTM
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
087900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030721     C*** VAD
088200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088400030721     C                   ENDIF
088500030721     C*** GMA
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
088700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030721     C*** GGA
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030721     C*** GVA
089400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089600030721     C                   ENDIF
089700030721     C*** TC1
089800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
089900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090000030721     C                   ENDIF
090100030721     C*** TC2
090200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090400030721     C                   ENDIF
090500030721     C*** SCL
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
090700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030721     C*** RMO
091000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091200030721     C                   ENDIF
091300030721     C*** NMO
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*
091800070705     C* Reperisco quindi i campi numerici...
091900030721     C*** CAD
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
092100070705     C                   EVAL      VABCAD = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** RMN
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
092500030721     C                   EVAL      PiStr=SkSplitCSV(i)
092600030721     C                   EXSR      CHKNUM
092700030721     C                   IF        PiInt=*on
092800030721     C                   Z-ADD     PiVal         VABRMN
092900030721     C                   ELSE
093000030721     C                   SETON                                        32
093100030721     C                   EVAL      VABRMN = 1
093200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093300030721     C                             + ' ' + 'VABRMN'
093400030721     C                   ENDIF
093500030721     C                   ENDIF
093600030721     C*** NSP
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
093800030721     C                   EVAL      PiStr=SkSplitCSV(i)
093900030721     C                   EXSR      CHKNUM
094000100517     C                   IF        PiInt=*on
094100030721     C                   Z-ADD     PiVal         VABNSP
094200030721     C                   ELSE
094300030721     C                   SETON                                        32
094400030721     C                   EVAL      VABNSP = *zeros
094500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094600030721     C                             + ' ' + 'VABNSP'
094700030721     C                   ENDIF
094800030721     C                   ENDIF
094900030721     C*** NCL
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
095100030721     C                   EVAL      PiStr=SkSplitCSV(i)
095200030721     C                   EXSR      CHKNUM
095300030721     C                   IF        PiInt=*on
095400030721     C                   Z-ADD     PiVal         VABNCL
095500030721     C                   ELSE
095600030721     C                   SETON                                        32
095700030721     C                   EVAL      VABNCL = *zeros
095800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095900030721     C                             + ' ' + 'VABNCL'
096000030721     C                   ENDIF
096100030721     C                   ENDIF
096200030721     C*** PKB
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
096400030721     C                   EVAL      PiStr=SkSplitCSV(i)
096500030721     C                   EXSR      CHKNUM
096600030721     C                   IF        PiNum=*on
096700030721     C                   Z-ADD     PiVal         VABPKB
096800030721     C                   ELSE
096900030721     C                   SETON                                        32
097000030721     C                   EVAL      VABPKB = *zeros
097100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097200030721     C                             + ' ' + 'VABPKB'
097300030721     C                   ENDIF
097400030721     C                   ENDIF
097500030721     C*** CAS
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
097700030721     C                   EVAL      PiStr=SkSplitCSV(i)
097800030721     C                   EXSR      CHKNUM
097900030721     C                   IF        PiNum=*on
098000030721     C                   Z-ADD     PiVal         VABCAS
098100030721     C                   ELSE
098200030721     C                   SETON                                        32
098300030721     C                   EVAL      VABCAS = *zeros
098400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098500030721     C                             + ' ' + 'VABCAS'
098600030721     C                   ENDIF
098700030721     C                   ENDIF
098800030721     C*** CCM
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
099000030721     C                   EVAL      PiStr=SkSplitCSV(i)
099100030721     C                   EXSR      CHKNUM
099200030721     C                   IF        PiInt=*on      AND
099300030721     C                             PiVal<=9999999 AND
099400030721     C                             PiVal>*zeros
099500030721     C                   Z-ADD     PiVal         VABCCM
099600030721     C                   ELSE
099700030721     C                   SETON                                        32
099800030721     C                   EVAL      VABCCM = *zeros
099900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100000030721     C                             + ' ' + 'VABCCM'
100100030721     C                   ENDIF
100200030721     C                   ENDIF
100300030721     C*** LNP
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
100500030721     C                   EVAL      PiStr=SkSplitCSV(i)
100600030721     C                   EXSR      CHKNUM
100700030721     C                   IF        PiInt=*on    AND
100800030721     C                             PiVal<=999   AND
100900030721     C                             PiVal>*zeros
101000030721     C                   Z-ADD     PiVal         VABLNP
101100030721     C                   ELSE
101200030721     C                   SETON                                        32
101300030721     C                   EVAL      VABLNP = *zeros
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABLNP'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** NRS
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
102000030721     C                   EVAL      PiStr=SkSplitCSV(i)
102100030721     C                   EXSR      CHKNUM
102200030721     C                   IF        PiInt=*on AND
102300030721     C                             PiVal<=99
102400030721     C                   Z-ADD     PiVal         VABNRS
102500030721     C                   ELSE
102600030721     C                   SETON                                        32
102700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102800030721     C                             + ' ' + 'VABNRS'
102900030721     C                   ENDIF
103000030721     C                   ENDIF
103100030721     C*** LNA
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
103300120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
103400120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
103500120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
103600120314     C                             %trim(SkSplitCSV(i)) <> *blanks
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiInt=*on    AND
104000030721     C                             PiVal<=999   AND
104100030721     C                             PiVal>*zeros
104200030721     C                   Z-ADD     PiVal         VABLNA
104300030721     C                   ELSE
104400030721     C                   SETON                                        32
104500030721     C                   EVAL      VABLNA = *zeros
104600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104700030721     C                             + ' ' + 'VABLNA'
104800120314     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** CTR
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiInt=*on  AND
105600030721     C                             PiVal<=999
105700030721     C                   Z-ADD     PiVal         VABCTR
105800030721     C                   ELSE
105900030721     C                   SETON                                        32
106000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106100030721     C                             + ' ' + 'VABCTR'
106200030721     C                   ENDIF
106300030721     C                   ENDIF
106400030721     C*** IAS
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
106600030721     C                   EVAL      PiStr=SkSplitCSV(i)
106700030721     C                   EXSR      CHKNUM
106800030721     C                   IF        PiNum=*on
106900030721     C                   Z-ADD     PiVal         VABIAS
107000030721     C                   ELSE
107100030721     C                   SETON                                        32
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABIAS'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** VLB
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiNum=*on
108100030721     C                   Z-ADD     PiVal         VABVLB
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500030721     C                             + ' ' + 'VABVLB'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** QFT
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiNum=*on
109300030721     C                   Z-ADD     PiVal         VABQFT
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109700030721     C                             + ' ' + 'VABQFT'
109800030721     C                   ENDIF
109900030721     C                   ENDIF
110000030721     C*** NCD
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiInt=*on      AND
110500030721     C                             PiVal<=9999999
110600030721     C                   Z-ADD     PiVal         VABNCD
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABNCD'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** NCA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on      AND
111800030721     C                             PiVal<=9999999
111900030721     C                   Z-ADD     PiVal         VABNCA
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABNCA'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** ZNC
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on AND
113100030721     C                             PiVal<=99
113200030721     C                   Z-ADD     PiVal         VABZNC
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABZNC'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** DCR
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on       AND
114400030721     C                             PiVal<=99999999
114500030721     C                   Z-ADD     PiVal         VABDCR
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABDCR'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** HCR
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on   AND
115700030721     C                             PiVal<=9999
115800030721     C                   Z-ADD     PiVal         VABHCR
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200030721     C                             + ' ' + 'VABHCR'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** VMD
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiNum=*on
117000030721     C                   Z-ADD     PiVal         VABVMD
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABVMD'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** ANT
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on        AND
118200030721     C                             PiVal<=999999999
118300030721     C                   Z-ADD     PiVal         VABANT
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABANT'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CMO
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on
119500030721     C                   Z-ADD     PiVal         Num5_0
119600030721     C                   MOVEL(p)  Num5_0        VABCMO
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCMO = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABCMO'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030715     C*
120500030715     C                   ENDIF
120600030715     C                   EVAL      i = i + 1
120700030715     C                   ENDDO
120800070705     C*
120900070705     C* Considerazioni particolari
121000110706     C*
121100110706     C*** RMN
121200110706     C                   IF        *in70
121300110706     C                   EVAL      VABRMN = VABNSP
121400110706     C                   ENDIF
121500070705     C*** NZD
121600070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
121700070705     C                   EVAL      VABNZD = *blanks
121800070705     C                   ENDIF
121900070705     C*** CAD
122000070705     C                   IF        VABNZD = *blanks
122100070705     C                   EVAL      PiStr=VABCAD
122200070705     C                   EXSR      CHKNUM
122300070705     C                   IF        PiInt=*on
122400070705     C                   Z-ADD     PiVal         Num5_0
122500070705     C                   MOVEL(p)  Num5_0        VABCAD
122600070705     C                   ELSE
122700070705     C                   SETON                                        32
122800070705     C                   EVAL      VABCAD = *zeros
122900070705     C                   EVAL      vinmsg = %trimr(vinmsg)
123000070705     C                             + ' ' + 'VABCAD'
123100070705     C                   ENDIF
123200070705     C                   ENDIF
123300080222     C*
123400080222     C* Se provincia nn valorizzata la reperisco
123500080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
123600140116     C                   IF        VABPRD  = *blanks    AND
123700140116     C                             VABCAD <> *blanks    AND
123800140116     C                             VABLOD <> *blanks    AND
123900140116     C                             VABNZD  = *blanks
124000080222     C                   CLEAR                   TISI95DS
124100080222     C                   EVAL      I95TCN = '3'
124200080222     C                   Z-ADD     datcor        I95DAT
124300080222     C                   EVAL      I95NAR = VABNZD
124400080222     C                   EVAL      I95CAP = VABCAD
124500080222     C                   EVAL      I95LOC = VABLOD
124600080222     C                   CALL      'TISI95R'
124700080222     C                   PARM                    TISI95DS
124800080222     C                   EVAL      VABPRD = O95PRV
124900080222     C                   ENDIF
125000020204     C*
125100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
125200020204     C                   EXSR      CHKIMPDIV
125300030715     C                   ENDIF
125400020204     C*
125500010607     C                   ENDSR
125600010607     C*----------------------------------------------------*
125700010601
125800020204
125900020204     C*----------------------------------------------------*
126000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
126100020204     C*----------------------------------------------------*
126200020204     C     CHKIMPDIV     BEGSR
126300060524     C*
126400060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
126500060524     C                   IF        vabCAS > *zeros
126600060524     C                   IF        vabCBO = '1'
126700060524     C                   EVAL      vabCBO = '4'
126800060524     C                   ENDIF
126900060524     C                   IF        vabCBO = '2'
127000060524     C                   EVAL      vabCBO = '6'
127100060524     C                   ENDIF
127200060524     C                   ENDIF
127300020204     C*
127400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
127500020204     C                   Z-ADD     *zeros        wrkDec            9 9
127600020204     C*
127700020204     C* Come prima cosa effettuo considerazioni sulla divisa
127800020204     C                   IF        vabIAS > *zeros
127900020204     C                   IF        vabVAS <> 'EUR'
128000020204     C                   EVAL      vabVAS =  'ITL'
128100020204     C                   ENDIF
128200020204     C                   ENDIF
128300020204     C*
128400020204     C                   IF        vabCAS > *zeros
128500020204     C                   IF        vabVCA <> 'EUR'
128600020204     C                   EVAL      vabVCA =  'ITL'
128700020204     C                   ENDIF
128800020204     C                   ENDIF
128900020204     C*
129000020204     C                   IF        vabVMD > *zeros
129100020204     C                   IF        vabVAD <> 'EUR'
129200020204     C                   EVAL      vabVAD =  'ITL'
129300020204     C                   ENDIF
129400020204     C                   ENDIF
129500020204     C*
129600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
129700020204     C                   Z-ADD     vabIAS        wrkDec
129800020204     C                   IF        wrkDec > *zeros
129900020204     C                   IF        vabVAS = 'ITL'
130000020204     C                   EVAL      vabIAS = *zeros
130100020204     C                   ENDIF
130200020204     C                   ENDIF
130300020204     C*
130400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
130500020204     C                   Z-ADD     vabCAS        wrkDec
130600020204     C                   IF        wrkDec > *zeros
130700020204     C                   IF        vabVCA = 'ITL'
130800020204     C                   EVAL      vabCAS = *zeros
130900020204     C                   ENDIF
131000020204     C                   ENDIF
131100020204     C*
131200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
131300020204     C                   Z-ADD     vabVMD        wrkDec
131400020204     C                   IF        wrkDec > *zeros
131500020204     C                   IF        vabVAD = 'ITL'
131600020204     C                   EVAL      vabVMD = *zeros
131700020204     C                   ENDIF
131800020204     C                   ENDIF
131900020204     C*
132000020204     C                   ENDSR
132100020204     C***
132200020204
132300010330
132400010330
132500010330     C*----------------------------------------------------*
132600010330     C*  CONTROLLO NUMERICITA' CAMPI
132700010330     C*----------------------------------------------------*
132800010330     C     CHKNUM        BEGSR
132900010330     C*
133000010606     C                   IF        PiDecChr = *blanks
133100030715     C                   EVAL      PiDecChr = CharNUM
133200010606     C                   ENDIF
133300010606     C*
133400010606     C                   CALL(e)   'ISNUMERIC'
133500010330     C                   PARM                    PiStr            30
133600010606     C                   PARM                    PiDecChr          1
133700010330     C                   PARM      *ZEROS        PiVal            30 9
133800010330     C                   PARM      '0'           PiInt             1
133900010330     C                   PARM      '0'           PiNum             1
134000010330     C                   IF        %error
134100010606     C                   EVAL      PiNum=*off
134200010330     C                   ENDIF
134300010330     C*
134400010330     C                   ENDSR
134500010330     C***
134600010330
134700010601
134800010601
134900010601
135000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
135100010601     C     repfil        BEGSR
135200010601     C*
135300010601     C                   if        invfil = *zeros and
135400010601     C                             depfil > *zeros and
135500010629     C                             (vinflg = *blanks or
135600010629     C                              vinflg = *zeros)
135700010601     C
135800010601     C                   eval      invfil = depfil
135900010601     C                   endif
136000010601     C*
136100010601     C                   if        depfil <> invfil and
136200010601     C                             invfil > *zeros
136300010601     C                   eval      flgMulti = '1'
136400010601     C                   if        vinflg = *blanks
136500010601     C                   add       1             cntNonEl
136600010601     C                   endif
136700010601     C                   endif
136800010601     C*
136900010601     C                   if        vinflg = '2'
137000010601     C                   eval      flgStato = '2'
137100010601     C                   endif
137200010601     C*
137300010601     C                   ENDSR
137400010601     C***
137500010601
137600010601
137700010601
137800010330
137900010330
138000010330
138100990920      /TITLE Invio dei dati al punto operativo.
138200000613     C     invio         BEGSR
138300990920     C*
138400990920     C                   reset                   dscmz
138500010601     C                   move      invfil        cmzdst
138600990920     C                   eval      cmzfld = vlrfou
138700990920     C                   eval      cmzmbd = vlrhdl
138800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
138900000710     C                   if        prmfir = *blanks
139000021025     C                   eval      cmzfla = 'FIVAB00F'
139100021025     C                   eval      cmzmba = 'FIVAB00F'
139200000710     C                   else
139300000710     C                   eval      cmzfla = prmfir
139400000710     C                   eval      cmzmba = prmfir
139500000710     C                   endif
139600990920     C                   eval      cmznrr = *zeros
139700990920     C                   move      §ctrok        cmznrr
139800021018     C                   eval      cmzlba = vlrfl1
139900990920     C                   call(e)   'TIS711C'
140000990920     C                   parm                    dscmz
140100990921     C                   parm      *blanks       esito
140200990920     C                   if        %error
140300990920     C                             or cmzerr = '1'
140400990921     C                             or esito  = '1'
140500000710     C                   eval      wrkesito = '3'
140600990920     C                   endif
140700990920     C*
140800000613     C                   ENDSR
140900990910
141000010601
141100010601
141200010601
141300010601
141400010601      /TITLE Invio dei dati al punto operativo.
141500010601     C     opeini        BEGSR
141600010601     C*
141700010601     C* Inizializzo flag e contatori operativi
141800010601     C                   movel     '0'           flgGiro           1
141900010601     C                   movel     '0'           flgMulti          1
142000010601     C                   movel     '1'           flgStato          1
142100010615     C                   movel     '0'           flgOk             1
142200010601     C                   z-add     *zeros        cntNonEl         10 0
142300010601     C                   z-add     *zeros        depfil            3 0
142400010601     C                   z-add     *zeros        invfil            3 0
142500010601     C*
142600010601     C                   ENDSR
142700010601     C***
142800050421
142900050421
143000050421
143100050421     C     *pssr         BEGSR
143200050421     C*
143300050421     C                   eval      wrkesito = '2'
143400050421     C*
143500050421     C                   ENDSR     '*CANCL'
143600050421     C***
143700100419
143800100419
143900100419
144000100419     C     normCSV       BEGSR
144100100419     C*
144200100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
144300100419     C* separatori campi
144400100419     C                   if        CharTXT <> *blanks
144500100419     C                   z-add     *zeros        wLenVINDTA        4 0
144600100419     C                   z-add     *zeros        wFlgVINDTA        1 0
144700100419     C                   setoff                                       80
144800100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
144900100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
145000100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
145100100419     C                   if        not *in80
145200100419     C                   seton                                        80
145300100419     C                   else
145400100419     C                   setoff                                       80
145500100419     C                   endif
145600100419     C                   endif
145700100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
145800100419     C                             and *in80 = *on
145900100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
146000100419     C                   endif
146100100419     C                   enddo
146200100419     C                   endif
146300100419     C*
146400100419     C                   ENDSR
146500100419     C***
146600010601
146700010330
146800010330
146900000613     C     *inzsr        BEGSR
147000990910     C*
147100990910     C     *entry        plist
147200990920     C                   parm                    tivlrds
147300990921     C                   parm      wrkesito      esito
147400000724     C                   parm                    prmlit
147500000710     C                   parm                    prmfir
147600010330     C*
147700010330     C* CALCOLA LA DATA CORRENTE
147800010330     C                   time                    wn14             14 0
147900010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
148000010330     C                   z-add     wn8           g08dat
148100010330     C                   z-add     *zeros        g08inv
148200010330     C                   movel     '0'           g08err
148300010330     C                   call      'XSRDA8'
148400010330     C                   parm                    wlbda8
148500010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
148600000613     C*
148700000613     C                   ENDSR
148800000613     C***
