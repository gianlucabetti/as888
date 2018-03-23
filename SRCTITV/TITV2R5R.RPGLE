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
027200140114     C                   eval      vgdDTA = 'VABATB' + CharCSV + vindta
027300140114     C                   else
027400140114     C                   select
027500140114     C                   when      vabCCM = 0436208
027600140114     C                   movel     'D'           wNTW              1
027700140114     C                   when      vabCCM = 0436209
027800140114     C                   movel     'E'           wNTW
027900140114     C                   other
028000140114     C                   movel     *blanks       wNTW
028100140114     C                   endsl
028200140114     C                   eval      vgdDTA = wNTW + CharCSV + vindta
028300140114     C                   endif
028400140114     C*
028500140114     C                   eval      vgdTIP = 'FP'
028600140114     C                   eval      vgdKSU = vlrKSC
028700140114     C                   eval      vgdTSC = 'WW'
028800140114     C                   eval      vgdDAT = datcor
028900140114     C                   eval      vgdPGM = 'TITV2R5R'
029000140114     C                   write     tivgd000
029100140114     C*
029200140114     C                   ENDSR
029300140114     C*------------------------------------------------------------------------*
029400140114
029500140114
029600140114
029700010601
029800010601
029900010601
030000010330     C*----------------------------------------------------*
030100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200010330     C*----------------------------------------------------*
030300010330     C     INZVAR        BEGSR
030400010330     C*
030500030822     C* Inizializzo variabili di wrk
030600020204     C                   Z-ADD     *zeros        Num5_0            5 0
030700090403     C                   z-add     *zeros        invfil
030800030822     C*
030900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
031000140114     C                   CLEAR                   FIVABDS
031100030822     C                   CLEAR                   SkSplitCSV
031200030822     C                   EVAL      i = 1
031300030822     C                   EVAL      posDa = *zeros
031400030822     C                   EVAL      posA  = *zeros
031500030822     C*
031600030822     C* Reimposto i valori di default
031700030822     C                   EXSR      DEFCAM
031800030822     C*
031900010330     C                   ENDSR
032000010330     C*----------------------------------------------------*
032100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
032200010330     C*----------------------------------------------------*
032300010330     C     DEFCAM        BEGSR
032400110706     C*
032500110706     C                   SETOFF                                       70
032600030822     C*
032700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
032800030715     C* e delimitatore testo.
032900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
033000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
033100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
033200090403     C*
033300090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
033400090403     C                   EVAL      CharSOS = CharNUM
033500030822     C*
033600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
033700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
033800030715     C                   SETON                                        50
033900030715     C                   ELSE
034000030715     C                   SETOFF                                       50
034100030715     C                   ENDIF
034200030822     C*
034300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
034400030822     C                   EVAL      posDaDft = 1
034500030822     C                   EVAL      posADft  = 0
034600030822     C                   EVAL      wGiroDft = 0
034700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
034800030822     C                             posDaDft > 0
034900030822     C*
035000030822     C* Gestisco il 1° giro
035100030822     C                   IF        wGiroDft = 0
035200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
035300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
035400030822     C* Incremento il contatore dei "giri"
035500030822     C                   EVAL      wGiroDft = 1
035600030822     C                   ELSE
035700030822     C                   EVAL      posDaDft = posADft
035800030822     C                   ENDIF
035900030822     C* Eseguo lo scan x trovare la fine del campo corrente
036000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
036100030822     C*
036200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
036300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
036400030822     C                   IF        posDaDft > 0 AND
036500030822     C                             posADft  > 0
036600110706     C* RMN
036700110706     C                   IF        %subst(
036800110706     C                             %subst(vlrppt:posDaDft+1:
036900110706     C                             posADft-posDaDft-1):1:3)
037000110706     C                             = 'RMN'
037100110706     C                   IF        %trim(%subst(
037200110706     C                             %subst(vlrppt:posDaDft+1:
037300110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037400110706     C                             = '*NSP'
037500110706     C                   SETON                                        70
037600110706     C                   ENDIF
037700110706     C                   ENDIF
037800060331     C* FGS
037900060331     C                   IF        %subst(
038000060331     C                             %subst(vlrppt:posDaDft+1:
038100060331     C                             posADft-posDaDft-1):1:3)
038200060331     C                             = 'FGS'
038300060331     C                   EVAL      PiStr=%trim(%subst(
038400060331     C                             %subst(vlrppt:posDaDft+1:
038500060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038600060331     C                   EXSR      CHKNUM
038700060331     C                   IF        PiInt=*on
038800060331     C                   Z-ADD     PiVal         VABFGS
038900060331     C                   ENDIF
039000060331     C                   ENDIF
039100030822     C* NCL
039200030822     C                   IF        %subst(
039300030822     C                             %subst(vlrppt:posDaDft+1:
039400030822     C                             posADft-posDaDft-1):1:3)
039500030822     C                             = 'NCL'
039600030822     C                   EVAL      PiStr=%trim(%subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039900030822     C                   EXSR      CHKNUM
040000030822     C                   IF        PiInt=*on
040100030822     C                   Z-ADD     PiVal         VABNCL
040200030822     C                   ENDIF
040300030822     C                   ENDIF
040400030822     C* CCM
040500030822     C                   IF        %subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):1:3)
040800030822     C                             = 'CCM'
040900030822     C                   EVAL      PiStr=%trim(%subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200030822     C                   EXSR      CHKNUM
041300030822     C                   IF        PiInt=*on
041400030822     C                   Z-ADD     PiVal         VABCCM
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030822     C* LNP
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'LNP'
042200030822     C                   EVAL      PiStr=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   EXSR      CHKNUM
042600030822     C                   IF        PiInt=*on
042700030822     C                   Z-ADD     PiVal         VABLNP
042800030822     C                   ENDIF
042900030822     C                   ENDIF
043000030822     C* NRS
043100030822     C                   IF        %subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):1:3)
043400030822     C                             = 'NRS'
043500030822     C                   EVAL      PiStr=%trim(%subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800030822     C                   EXSR      CHKNUM
043900030822     C                   IF        PiInt=*on
044000030822     C                   Z-ADD     PiVal         VABNRS
044100030822     C                   ENDIF
044200030822     C                   ENDIF
044300030822     C* CTR
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'CTR'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiInt=*on
045300030822     C                   Z-ADD     PiVal         VABCTR
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* PKB
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'PKB'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiNum=*on
046600030822     C                   Z-ADD     PiVal         VABPKB
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* VLB
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'VLB'
047400030822     C                   EVAL      PiStr=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   EXSR      CHKNUM
047800030822     C                   IF        PiNum=*on
047900030822     C                   Z-ADD     PiVal         VABVLB
048000030822     C                   ENDIF
048100030822     C                   ENDIF
048200030822     C* QFT
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'QFT'
048700030822     C                   EVAL      PiStr=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   EXSR      CHKNUM
049100030822     C                   IF        PiNum=*on
049200030822     C                   Z-ADD     PiVal         VABQFT
049300030822     C                   ENDIF
049400030822     C                   ENDIF
049500030822     C* CBO
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'CBO'
050000030822     C                   EVAL      VABCBO=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   ENDIF
050400030822     C* TSP
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'TSP'
050900030822     C                   EVAL      VABTSP=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   ENDIF
051300030822     C* VAS
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'VAS'
051800030822     C                   EVAL      VABVAS=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   ENDIF
052200030822     C* VCA
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'VCA'
052700030822     C                   EVAL      VABVCA=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   ENDIF
053100030822     C* GCA
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'GCA'
053600030822     C                   EVAL      VABGCA=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   ENDIF
054000030822     C* CTM
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'CTM'
054500030822     C                   EVAL      VABCTM=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   ENDIF
054900030822     C* FFD
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'FFD'
055400030822     C                   EVAL      VABFFD=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   ENDIF
055800030822     C* VAD
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'VAD'
056300030822     C                   EVAL      VABVAD=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   ENDIF
056700030822     C* GMA
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'GMA'
057200030822     C                   EVAL      VABGMA=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   ENDIF
057600030822     C* GGA
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'GGA'
058100030822     C                   EVAL      VABGGA=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   ENDIF
058500030822     C* GVA
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'GVA'
059000030822     C                   EVAL      VABGVA=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   ENDIF
059400030822     C* TC1
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'TC1'
059900030822     C                   EVAL      VABTC1=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   ENDIF
060300030822     C* TC2
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'TC2'
060800030822     C                   EVAL      VABTC2=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* ...
061300030822     C                   ENDIF
061400030822     C                   ENDDO
061500020204     C*
061600010330     C                   ENDSR
061700010607     C*----------------------------------------------------*
061800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
061900010607     C*----------------------------------------------------*
062000010607     C     IMPVAB        BEGSR
062100010607     C*
062200010607     C                   SETOFF                                       3132
062300030515     C*
062400010607     C                   MOVEL     datcor        VABAAS
062500010607     C                   MOVE      datcor        VABMGS
062600010607     C*
062700030715     C**********
062800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
062900030715     C**********
063000030715     C                   IF        wGiro = *zeros
063100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
063200031201     C     minu:maiu     XLATE     vindta        vindta
063300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
063400040324     C                   DOW       posDa <= %len(%trimr(vindta))
063500030715     C*
063600030715     C* Gestisco il 1° campo
063700030715     C                   IF        i = 1
063800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
063900030715     C                   EVAL      posDa = 1
064000030715     C* Eseguo lo scan x trovare la fine del primo campo
064100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
064200030715     C* A questo punto "estraggo" il campo corrente
064300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
064400030715     C                                                    (posA-posDa))
064500030715     C* X i campi successivi al 1°
064600030715     C                   ELSE
064700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
064800030715     C                   EVAL      posDa = posA + 1
064900030715     C* Eseguo lo scan x trovare la fine del campo corrente
065000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
065100030715     C* Gestisco l'ultimo campo
065200030715     C                   IF        posA = *zeros
065300040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
065400030715     C* A questo punto "estraggo" il campo corrente
065500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
065600030715     C                                                    (posA-posDa))
065700030715     C                   LEAVE
065800030715     C                   ELSE
065900030715     C* A questo punto "estraggo" il campo corrente
066000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
066100030715     C                                                    (posA-posDa))
066200030715     C                   ENDIF
066300030715     C                   ENDIF
066400030715     C* Incremento il contatore d campo
066500030715     C                   EVAL      i = i +1
066600030715     C                   ENDDO
066700030715     C*
066800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
066900030715     C* delimitazione testo
067000030715     C                   EVAL      i = 1
067100030715     C                   DOW       i < %elem(SkSplitFLD)
067200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
067300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
067400030715     C                   EVAL      i = i + 1
067500030715     C                   ENDDO
067600030715     C                   EVAL      wGiro = 1
067700030715     C                   ELSE
067800030715     C                   EVAL      wGiro = 2
067900031204     C**********
068000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
068100031204     C* d separatore campo
068200031204     C**********
068300040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
068400040304     C                   EVAL      vindta = ' ' + vindta
068500040304     C                   ENDIF
068600040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
068700040304     C***                EVAL      vindta = %subst(vindta:2)
068800040304     C***                ENDDO
068900031204     C*
069000031204     C                   Z-ADD     *zeros        lunghInput        4 0
069100040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
069200040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
069300040304     C                   EVAL      vindta = vindta + ' '
069400040304     C                   ENDIF
069500040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
069600040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
069700040304     C***                EVAL      lunghInput = %len(%trim(vindta))
069800040304     C***                ENDDO
069900030715     C**********
070000030715     C* Effettuo lo split del campo dati d input
070100030715     C**********
070200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070300040324     C                   DOW       posDa <= %len(%trimr(vindta))
070400030715     C*
070500030715     C* Gestisco il 1° campo
070600030715     C                   IF        i = 1
070700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070800030715     C                   EVAL      posDa = 1
070900030715     C* Eseguo lo scan x trovare la fine del primo campo
071000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071100030715     C* A questo punto "estraggo" il campo corrente
071200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071300030715     C                                                    (posA-posDa))
071400030715     C* X i campi successivi al 1°
071500030715     C                   ELSE
071600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071700030715     C                   EVAL      posDa = posA + 1
071800031204     C* Verifico che nn vi sia il campo nn valorizzato
071900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
072000031204     C* Se campo nn valorizzato skippo al prossimo
072100031204     C                   EVAL      SkSplitCSV(i) = *blanks
072200031204     C                   EVAL      posA  = posA + 1
072300031204     C                   ELSE
072400030715     C* Eseguo lo scan x trovare la fine del campo corrente
072500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072600030715     C* Gestisco l'ultimo campo
072700030715     C                   IF        posA = *zeros
072800040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
072900030715     C* A questo punto "estraggo" il campo corrente
073000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073100030715     C                                                    (posA-posDa))
073200030715     C                   LEAVE
073300030715     C                   ELSE
073400030715     C* A questo punto "estraggo" il campo corrente
073500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
073600030715     C                                                    (posA-posDa))
073700030715     C                   ENDIF
073800031204     C                   ENDIF
073900030715     C                   ENDIF
074000030715     C* Incremento il contatore d campo
074100030715     C                   EVAL      i = i +1
074200030715     C                   ENDDO
074300030715     C*
074400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074500030715     C* delimitazione testo
074600030715     C                   EVAL      i = 1
074700030715     C                   DOW       i < %elem(SkSplitCSV)
074800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
074900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
075000030715     C                   EVAL      i = i + 1
075100030715     C                   ENDDO
075200030715     C*
075300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
075400030715     C                   EVAL      i = 1
075500030715     C                   DOW       i < %elem(SkSplitFLD)
075600030715     C*
075700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
075800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
075900030715     C                             SkSplitCSV(i) <> *blanks
076000130624     C*** ATB
076100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
076200130624     C                   IF        SkSplitCSV(i) = 'D'
076300130624     C                   EVAL      VABCTR = 390
076400130624     C                   EVAL      VABCTM = '7R'
076500130624     C                   ENDIF
076600030721     C                   ENDIF
076700030715     C*** RSD
076800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
076900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
077000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
077100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
077200030715     C* ==
077300030715     C                   ENDIF
077400030721     C*** RD2
077500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
077600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
077700030721     C                   ENDIF
077800030715     C*** IND
077900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
078000030715     C                   EVAL      VABIND = SkSplitCSV(i)
078100030715     C                   ENDIF
078200030715     C*** LOD
078300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
078400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
078500030715     C                   ENDIF
078600030715     C*** PRD
078700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
078800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
078900030715     C                   ENDIF
079000030721     C*** NZD
079100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
079200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
079300030721     C                   ENDIF
079400030715     C*** RMA
079500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
079600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
079700030715     C                   ENDIF
079800030715     C*** NOT
079900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
080000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
080100030715     C                   ENDIF
080200030715     C*** NT2
080300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
080400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
080500030715     C                   ENDIF
080600030715     C*** VCA
080700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
080800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
080900030715     C                   ENDIF
081000030715     C*** VAS
081100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
081200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
081300030715     C                   ENDIF
081400030715     C*** FFD
081500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
081600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
081700030715     C                   ENDIF
081800030715     C*** CBO
081900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
082000030715     C                   EVAL      VABCBO = SkSplitCSV(i)
082100030715     C                   ENDIF
082200030721     C*** GC1
082300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
082400120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
082500120314     C                   MOVEL     *blanks       wGC               1
082600120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
082700120314     C                   IF        SkSplitCSV(i) = 'M' OR
082800120314     C                             SkSplitCSV(i) = 'P'
082900120314     C                   MOVE(P)   wGC           VABGC1
083000120314     C                   ELSE
083100120314     C                   MOVEL(P)  wGC           VABGC1
083200120314     C                   ENDIF
083300120314     C                   ENDIF
083400030721     C                   ENDIF
083500030721     C*** GC2
083600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
083700120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
083800120314     C                   MOVEL     *blanks       wGC               1
083900120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
084000120314     C                   IF        SkSplitCSV(i) = 'M' OR
084100120314     C                             SkSplitCSV(i) = 'P'
084200120314     C                   MOVE(P)   wGC           VABGC2
084300120314     C                   ELSE
084400120314     C                   MOVEL(P)  wGC           VABGC2
084500120314     C                   ENDIF
084600120314     C                   ENDIF
084700030721     C                   ENDIF
084800030721     C*** TSP
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
085000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030721     C*** NAS
085300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
085400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
085500030721     C                   ENDIF
085600030721     C*** TIC
085700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
085800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
085900030721     C                   ENDIF
086000030721     C*** GCA
086100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
086200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
086300030721     C                   ENDIF
086400030721     C*** XCO
086500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
086600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
086700030721     C                   ENDIF
086800030721     C*** CTM
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
087000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030721     C*** TCR
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
087400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030721     C*** CTS
087700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
087800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
087900030721     C                   ENDIF
088000030721     C*** FTM
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
088200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** VAD
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
088600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** GMA
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
089000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** GGA
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
089400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** GVA
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
089800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** TC1
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
090200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** TC2
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
090600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*** SCL
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
091000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030721     C*** RMO
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
091400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** NMO
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
091800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*
092100070705     C* Reperisco quindi i campi numerici...
092200030721     C*** CAD
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
092400070705     C                   EVAL      VABCAD = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030721     C*** RMN
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
092800030721     C                   EVAL      PiStr=SkSplitCSV(i)
092900030721     C                   EXSR      CHKNUM
093000030721     C                   IF        PiInt=*on
093100030721     C                   Z-ADD     PiVal         VABRMN
093200030721     C                   ELSE
093300030721     C                   SETON                                        32
093400030721     C                   EVAL      VABRMN = 1
093500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093600030721     C                             + ' ' + 'VABRMN'
093700030721     C                   ENDIF
093800030721     C                   ENDIF
093900030721     C*** NSP
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
094100030721     C                   EVAL      PiStr=SkSplitCSV(i)
094200030721     C                   EXSR      CHKNUM
094300100517     C                   IF        PiInt=*on
094400030721     C                   Z-ADD     PiVal         VABNSP
094500030721     C                   ELSE
094600030721     C                   SETON                                        32
094700030721     C                   EVAL      VABNSP = *zeros
094800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094900030721     C                             + ' ' + 'VABNSP'
095000030721     C                   ENDIF
095100030721     C                   ENDIF
095200030721     C*** NCL
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
095400030721     C                   EVAL      PiStr=SkSplitCSV(i)
095500030721     C                   EXSR      CHKNUM
095600030721     C                   IF        PiInt=*on
095700030721     C                   Z-ADD     PiVal         VABNCL
095800030721     C                   ELSE
095900030721     C                   SETON                                        32
096000030721     C                   EVAL      VABNCL = *zeros
096100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
096200030721     C                             + ' ' + 'VABNCL'
096300030721     C                   ENDIF
096400030721     C                   ENDIF
096500030721     C*** PKB
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
096700030721     C                   EVAL      PiStr=SkSplitCSV(i)
096800030721     C                   EXSR      CHKNUM
096900030721     C                   IF        PiNum=*on
097000030721     C                   Z-ADD     PiVal         VABPKB
097100030721     C                   ELSE
097200030721     C                   SETON                                        32
097300030721     C                   EVAL      VABPKB = *zeros
097400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097500030721     C                             + ' ' + 'VABPKB'
097600030721     C                   ENDIF
097700030721     C                   ENDIF
097800030721     C*** CAS
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
098000030721     C                   EVAL      PiStr=SkSplitCSV(i)
098100030721     C                   EXSR      CHKNUM
098200030721     C                   IF        PiNum=*on
098300030721     C                   Z-ADD     PiVal         VABCAS
098400030721     C                   ELSE
098500030721     C                   SETON                                        32
098600030721     C                   EVAL      VABCAS = *zeros
098700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098800030721     C                             + ' ' + 'VABCAS'
098900030721     C                   ENDIF
099000030721     C                   ENDIF
099100030721     C*** CCM
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
099300030721     C                   EVAL      PiStr=SkSplitCSV(i)
099400030721     C                   EXSR      CHKNUM
099500030721     C                   IF        PiInt=*on      AND
099600030721     C                             PiVal<=9999999 AND
099700030721     C                             PiVal>*zeros
099800030721     C                   Z-ADD     PiVal         VABCCM
099900030721     C                   ELSE
100000030721     C                   SETON                                        32
100100030721     C                   EVAL      VABCCM = *zeros
100200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100300030721     C                             + ' ' + 'VABCCM'
100400030721     C                   ENDIF
100500030721     C                   ENDIF
100600030721     C*** LNP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
100800030721     C                   EVAL      PiStr=SkSplitCSV(i)
100900030721     C                   EXSR      CHKNUM
101000030721     C                   IF        PiInt=*on    AND
101100030721     C                             PiVal<=999   AND
101200030721     C                             PiVal>*zeros
101300030721     C                   Z-ADD     PiVal         VABLNP
101400030721     C                   ELSE
101500030721     C                   SETON                                        32
101600030721     C                   EVAL      VABLNP = *zeros
101700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101800030721     C                             + ' ' + 'VABLNP'
101900030721     C                   ENDIF
102000030721     C                   ENDIF
102100030721     C*** NRS
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
102300030721     C                   EVAL      PiStr=SkSplitCSV(i)
102400030721     C                   EXSR      CHKNUM
102500030721     C                   IF        PiInt=*on AND
102600030721     C                             PiVal<=99
102700030721     C                   Z-ADD     PiVal         VABNRS
102800030721     C                   ELSE
102900030721     C                   SETON                                        32
103000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103100030721     C                             + ' ' + 'VABNRS'
103200030721     C                   ENDIF
103300030721     C                   ENDIF
103400030721     C*** LNA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
103600120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
103700120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
103800120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
103900120314     C                             %trim(SkSplitCSV(i)) <> *blanks
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on    AND
104300030721     C                             PiVal<=999   AND
104400030721     C                             PiVal>*zeros
104500030721     C                   Z-ADD     PiVal         VABLNA
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABLNA = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABLNA'
105100120314     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** CTR
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiInt=*on  AND
105900030721     C                             PiVal<=999
106000030721     C                   Z-ADD     PiVal         VABCTR
106100030721     C                   ELSE
106200030721     C                   SETON                                        32
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400030721     C                             + ' ' + 'VABCTR'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** IAS
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
106900030721     C                   EVAL      PiStr=SkSplitCSV(i)
107000030721     C                   EXSR      CHKNUM
107100030721     C                   IF        PiNum=*on
107200030721     C                   Z-ADD     PiVal         VABIAS
107300030721     C                   ELSE
107400030721     C                   SETON                                        32
107500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107600030721     C                             + ' ' + 'VABIAS'
107700030721     C                   ENDIF
107800030721     C                   ENDIF
107900030721     C*** VLB
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
108100030721     C                   EVAL      PiStr=SkSplitCSV(i)
108200030721     C                   EXSR      CHKNUM
108300030721     C                   IF        PiNum=*on
108400030721     C                   Z-ADD     PiVal         VABVLB
108500030721     C                   ELSE
108600030721     C                   SETON                                        32
108700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108800030721     C                             + ' ' + 'VABVLB'
108900030721     C                   ENDIF
109000030721     C                   ENDIF
109100030721     C*** QFT
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiNum=*on
109600030721     C                   Z-ADD     PiVal         VABQFT
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110000030721     C                             + ' ' + 'VABQFT'
110100030721     C                   ENDIF
110200030721     C                   ENDIF
110300030721     C*** NCD
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on      AND
110800030721     C                             PiVal<=9999999
110900030721     C                   Z-ADD     PiVal         VABNCD
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABNCD'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NCA
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on      AND
112100030721     C                             PiVal<=9999999
112200030721     C                   Z-ADD     PiVal         VABNCA
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABNCA'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** ZNC
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on AND
113400030721     C                             PiVal<=99
113500030721     C                   Z-ADD     PiVal         VABZNC
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABZNC'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** DCR
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on       AND
114700030721     C                             PiVal<=99999999
114800030721     C                   Z-ADD     PiVal         VABDCR
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABDCR'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** HCR
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on   AND
116000030721     C                             PiVal<=9999
116100030721     C                   Z-ADD     PiVal         VABHCR
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABHCR'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** VMD
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABVMD
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700030721     C                             + ' ' + 'VABVMD'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** ANT
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiInt=*on        AND
118500030721     C                             PiVal<=999999999
118600030721     C                   Z-ADD     PiVal         VABANT
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABANT'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** CMO
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on
119800030721     C                   Z-ADD     PiVal         Num5_0
119900030721     C                   MOVEL(p)  Num5_0        VABCMO
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABCMO = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABCMO'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030715     C*
120800030715     C                   ENDIF
120900030715     C                   EVAL      i = i + 1
121000030715     C                   ENDDO
121100070705     C*
121200070705     C* Considerazioni particolari
121300110706     C*
121400110706     C*** RMN
121500110706     C                   IF        *in70
121600110706     C                   EVAL      VABRMN = VABNSP
121700110706     C                   ENDIF
121800070705     C*** NZD
121900070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
122000070705     C                   EVAL      VABNZD = *blanks
122100070705     C                   ENDIF
122200070705     C*** CAD
122300070705     C                   IF        VABNZD = *blanks
122400070705     C                   EVAL      PiStr=VABCAD
122500070705     C                   EXSR      CHKNUM
122600070705     C                   IF        PiInt=*on
122700070705     C                   Z-ADD     PiVal         Num5_0
122800070705     C                   MOVEL(p)  Num5_0        VABCAD
122900070705     C                   ELSE
123000070705     C                   SETON                                        32
123100070705     C                   EVAL      VABCAD = *zeros
123200070705     C                   EVAL      vinmsg = %trimr(vinmsg)
123300070705     C                             + ' ' + 'VABCAD'
123400070705     C                   ENDIF
123500070705     C                   ENDIF
123600080222     C*
123700080222     C* Se provincia nn valorizzata la reperisco
123800080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
123900080222     C                   IF        VABPRD = *blanks
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
