000100030715      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200150420     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300150420     H DECEDIT('0,') DATEDIT(*DMY.)
000400000313     F*
000500990910     Ftivin00r  uF   E             DISK    usropn
000600140114     FTIVGD00F  O    E             DISK
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400140114     D fivabds       e ds                  extname(fivab00f)
001500140114     D tisi95ds      e ds
001600010330     D esito           s              1
001700010330     D prmlit          s             10
001800010330     D prmfir          s             10
001900150420     D pptCHR          s              1    inz
002000150420     D pptCSV          s              1    inz
002100150420     D wBufferOut      s           2048    inz
002200150420     D wVATNOT_I       s             70    inz
002300150421     D wVATNOT_A       s             35    inz
002400150420     D wrkesito        s                   like(esito)
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600030715     D*------------------
002700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
002800030715     D*------------------
002900150420     D SkSplitFLD      S            128    DIM(100)
003000030715     D SkSplitCSV      S            256    DIM(100)
003100030715     D CharCSV         S              1
003200030715     D CharTXT         S              1
003300030715     D CharNUM         S              1
003400090403     D CharSOS         S              1
003500030715     D posDa           S              3  0 INZ(*zeros)
003600030715     D posA            S              3  0 INZ(*zeros)
003700030715     D i               s              3  0 INZ(1)
003800030715     D wGiro           s              1  0 INZ(*zeros)
003900030822     D*------------------
004000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004100030822     D*------------------
004200030822     D posDaDft        S              3  0 INZ(*zeros)
004300030822     D posADft         S              3  0 INZ(*zeros)
004400030822     D j               s              3  0 INZ(1)
004500030822     D wGiroDft        s              1  0 INZ(*zeros)
004600031201     D*------------------
004700031201     D* Costanti
004800031201     D*------------------
004900031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005000031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
005100150420
005200010330
005300010330
005400000913     C                   reset                   rrnum
005500990921     C                   reset                   esito
005600990921     C                   reset                   wrkesito
005700010601     C*
005800010601     C                   exsr      opeini
005900150420     C                   exsr      DEFCAM
006000150420     C                   exsr      wriVGDHEAD
006100150420     C                   exsr      rwfile
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
008800010601     C*--------------------------------------------------------
008900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009000010601     C*--------------------------------------------------------
009100010601     C     ENDELA        BEGSR
009200080222     C*
009300080222     C* Effettuo lancio TISI95 solo x chiusura
009400080222     C                   CLEAR                   TISI95DS
009500080222     C                   EVAL      I95TLA = 'C'
009600080222     C                   CALL      'TISI95R'
009700080222     C                   PARM                    TISI95DS
009800000613     C*
009900010601     C                   ENDSR
010000010601     C***
010100010330
010200010601
010300010601
010400010601
010500010330     C*--------------------------------------------------------
010600150420     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIVABWWF              *
010700010330     C*--------------------------------------------------------
010800150420     C     RWFILE        BEGSR
010900010330     C*
011000010330     C                   if        not %open(tivin00r)
011100010330     C                   open      tivin00r
011200010330     C                   endif
011300010330     C*
011400010604     C                   clear                   §CTROK
011500010604     C                   clear                   §CTRMO
011600010604     C                   clear                   §CTRNO
011700010330     C*
011800030822     C                   do        *HIVAL
011900010330     C*
012000030822     C                   read      tivin00r                               70
012100010618     C*
012200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012300010618     C                   if        vinflg = '1'
012400010618     C                   eval      flgOk = '1'
012500010618     C                   endif
012600010618     C*
012700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
012800040510     C                   z-add     *zeros        wLenVINDTA        4 0
012900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
013000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
013100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
013200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
013300040510     C                   z-add     1             wFlgVINDTA
013400040510     C                   leave
013500040510     C                   endif
013600040510     C                   enddo
013700040510     C*
013800040218     C                   if        vindta > *blanks AND
013900040510     C                             wFlgVINDTA = 1
014000010330     C                   add       1             rrnum
014100010330     C*
014200010601     C                   if        *in70 = *off and
014300010330     C                             (vinflg = *blanks
014400010330     C                              or vinflg = '0'
014500010330     C                              or vinflg = '2')
014600010330     C*
014700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014800010711     C                   if        vinflg = *blanks or vinflg = '0'
014900010711     C                   clear                   vinmsg
015000010711     C                   endif
015100010601     C*
015200010330     C                   exsr      inzvar
015300100419     C                   exsr      normCSV
015400090403     C*
015500090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
015600090403     C* separatori campi
015700090403     C                   if        CharTXT <> *blanks
015800090403     C                   z-add     *zeros        wLenVINDTA        4 0
015900090403     C                   z-add     *zeros        wFlgVINDTA        1 0
016000090403     C                   setoff                                       80
016100090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
016200090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
016300090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
016400090403     C                   if        not *in80
016500090403     C                   seton                                        80
016600090403     C                   else
016700090403     C                   setoff                                       80
016800090403     C                   endif
016900090403     C                   endif
017000090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
017100090403     C                             and *in80 = *on
017200090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
017300090403     C                   endif
017400090403     C                   enddo
017500090403     C                   endif
017600090403     C*
017700010605     C                   exsr      impvab
017800010601     C*
017900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018000010605     C                   eval      depfil = VABLNP
018100010601     C                   exsr      repfil
018200060331     C                   if        VABFGS = *zeros
018300010601     C                   if        depfil = invfil
018400021025     C                   if        vlrpoi = 999
018500030822     C                   move(P)   invfil        VABFGS
018600021025     C                   else
018700030822     C                   move(P)   vlrpoi        VABFGS
018800021025     C                   endif
018900050113     C                   endif
019000060331     C                   endif
019100010601     C*
019200010601     C                   exsr      PREELA
019300010601     C*
019400010604     C* Ebbene...
019500010604     C*
019600030822     C  N31              add       1             §CTROK            7 0
019700030822     C   32              add       1             §CTRMO            7 0
019800030822     C   31              add       1             §CTRNO            7 0
019900150420     C                   if        wGiro > 1
020000150420     C                   EXSR      wriVGDSHPM
020100150420     C                   endif
020200010604     C*
020300010604     C                   if        *in31 = *off and
020400010604     C                             *in32 = *off
020500010604     C                   eval      vinflg = '1'
020600010604     C                   else
020700010604     C                   eval      vinflg = '2'
020800010604     C                   endif
020900010601     C*
021000010604     C                   endif
021100010604     C*
021200010330     C                   else
021300010330     C                   eval      vinflg = '1'
021400010330     C                   endif
021500010601     C*
021600010601     C  N70              update    tivin000
021700010330     C*
021800030822     C  N70              enddo
021900010601     C
022000010601     C*
022100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
022200010601     C                   if        cntNonEl = *zeros or
022300010601     C                             flgMulti = '0'
022400010330     C* Se non ci sono record con errori ...
022500010601     C                   if        §ctrno = 0 and
022600010604     C                             §ctrmo = 0 and
022700010601     C                             flgStato <> '2'
022800010330     C* ... restituisco esito OK.
022900010330     C                   eval      wrkesito = '0'
023000010330     C                   else
023100010330     C                   if        §ctrok > 0
023200010330     C                   eval      wrkesito = '1'
023300010330     C                   else
023400010615     C                   if        flgOk = '0'
023500010615     C                   eval      wrkesito = '2'
023600010615     C                   else
023700010615     C                   eval      wrkesito = '6'
023800010615     C                   endif
023900010330     C                   endif
024000010330     C                   endif
024100010601     C                   else
024200010601     C                   eval      wrkesito = '9'
024300010601     C                   endif
024400010330     C*
024500010330     C                   if        %open(tivin00r)
024600010330     C                   close     tivin00r
024700010330     C                   endif
024800010601     C*
024900010601     C                   if        vlrpoi <> 999
025000010601     C                   eval      invfil = vlrpoi
025100010601     C                   endif
025200010601     C*
025300010618     C                   if        flgGiro = '1'
025400010601     C                   exsr      endela
025500010618     C                   endif
025600010330     C*
025700010330     C                   ENDSR
025800010330     C***
025900010601
026000010601
026100010601
026200010330     C*----------------------------------------------------*
026300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026400010330     C*----------------------------------------------------*
026500010330     C     INZVAR        BEGSR
026600010330     C*
026700030822     C* Inizializzo variabili di wrk
026800020204     C                   Z-ADD     *zeros        Num5_0            5 0
026900090403     C                   z-add     *zeros        invfil
027000030822     C*
027100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
027200140114     C                   CLEAR                   FIVABDS
027300150420     C                   CLEAR                   wVATNOT_I
027400150421     C                   CLEAR                   wVATNOT_A
027500030822     C                   CLEAR                   SkSplitCSV
027600030822     C                   EVAL      i = 1
027700030822     C                   EVAL      posDa = *zeros
027800030822     C                   EVAL      posA  = *zeros
027900030822     C*
028000030822     C* Reimposto i valori di default
028100030822     C                   EXSR      DEFCAM
028200030822     C*
028300010330     C                   ENDSR
028400010330     C*----------------------------------------------------*
028500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
028600010330     C*----------------------------------------------------*
028700010330     C     DEFCAM        BEGSR
028800110706     C*
028900110706     C                   SETOFF                                       70
029000030822     C*
029100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
029200030715     C* e delimitatore testo.
029300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
029400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
029500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
029600150420     C                   IF        %subst(vlrppt:5:1) <> *blanks
029700150420     C                   EVAL      pptCSV = %subst(vlrppt:5:1)
029800150420     C                   ELSE
029900150420     C                   EVAL      pptCSV = ';'
030000150420     C                   ENDIF
030100150420     C                   IF        %subst(vlrppt:6:1) <> *blanks
030200150420     C                   EVAL      pptCHR = %subst(vlrppt:6:1)
030300150420     C                   ELSE
030400150420     C                   EVAL      pptCHR = ' '
030500150420     C                   ENDIF
030600090403     C*
030700090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
030800090403     C                   EVAL      CharSOS = CharNUM
030900030822     C*
031000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
031100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
031200030715     C                   SETON                                        50
031300030715     C                   ELSE
031400030715     C                   SETOFF                                       50
031500030715     C                   ENDIF
031600030822     C*
031700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
031800030822     C                   EVAL      posDaDft = 1
031900030822     C                   EVAL      posADft  = 0
032000030822     C                   EVAL      wGiroDft = 0
032100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
032200030822     C                             posDaDft > 0
032300030822     C*
032400030822     C* Gestisco il 1° giro
032500030822     C                   IF        wGiroDft = 0
032600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
032700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
032800030822     C* Incremento il contatore dei "giri"
032900030822     C                   EVAL      wGiroDft = 1
033000030822     C                   ELSE
033100030822     C                   EVAL      posDaDft = posADft
033200030822     C                   ENDIF
033300030822     C* Eseguo lo scan x trovare la fine del campo corrente
033400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
033500030822     C*
033600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
033700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
033800030822     C                   IF        posDaDft > 0 AND
033900030822     C                             posADft  > 0
034000110706     C* RMN
034100110706     C                   IF        %subst(
034200110706     C                             %subst(vlrppt:posDaDft+1:
034300110706     C                             posADft-posDaDft-1):1:3)
034400110706     C                             = 'RMN'
034500110706     C                   IF        %trim(%subst(
034600110706     C                             %subst(vlrppt:posDaDft+1:
034700110706     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
034800110706     C                             = '*NSP'
034900110706     C                   SETON                                        70
035000110706     C                   ENDIF
035100110706     C                   ENDIF
035200060331     C* FGS
035300060331     C                   IF        %subst(
035400060331     C                             %subst(vlrppt:posDaDft+1:
035500060331     C                             posADft-posDaDft-1):1:3)
035600060331     C                             = 'FGS'
035700060331     C                   EVAL      PiStr=%trim(%subst(
035800060331     C                             %subst(vlrppt:posDaDft+1:
035900060331     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
036000060331     C                   EXSR      CHKNUM
036100060331     C                   IF        PiInt=*on
036200060331     C                   Z-ADD     PiVal         VABFGS
036300060331     C                   ENDIF
036400060331     C                   ENDIF
036500030822     C* NCL
036600030822     C                   IF        %subst(
036700030822     C                             %subst(vlrppt:posDaDft+1:
036800030822     C                             posADft-posDaDft-1):1:3)
036900030822     C                             = 'NCL'
037000030822     C                   EVAL      PiStr=%trim(%subst(
037100030822     C                             %subst(vlrppt:posDaDft+1:
037200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
037300030822     C                   EXSR      CHKNUM
037400030822     C                   IF        PiInt=*on
037500030822     C                   Z-ADD     PiVal         VABNCL
037600030822     C                   ENDIF
037700030822     C                   ENDIF
037800030822     C* CCM
037900030822     C                   IF        %subst(
038000030822     C                             %subst(vlrppt:posDaDft+1:
038100030822     C                             posADft-posDaDft-1):1:3)
038200030822     C                             = 'CCM'
038300030822     C                   EVAL      PiStr=%trim(%subst(
038400030822     C                             %subst(vlrppt:posDaDft+1:
038500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
038600030822     C                   EXSR      CHKNUM
038700030822     C                   IF        PiInt=*on
038800030822     C                   Z-ADD     PiVal         VABCCM
038900030822     C                   ENDIF
039000030822     C                   ENDIF
039100030822     C* LNP
039200030822     C                   IF        %subst(
039300030822     C                             %subst(vlrppt:posDaDft+1:
039400030822     C                             posADft-posDaDft-1):1:3)
039500030822     C                             = 'LNP'
039600030822     C                   EVAL      PiStr=%trim(%subst(
039700030822     C                             %subst(vlrppt:posDaDft+1:
039800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
039900030822     C                   EXSR      CHKNUM
040000030822     C                   IF        PiInt=*on
040100030822     C                   Z-ADD     PiVal         VABLNP
040200030822     C                   ENDIF
040300030822     C                   ENDIF
040400030822     C* NRS
040500030822     C                   IF        %subst(
040600030822     C                             %subst(vlrppt:posDaDft+1:
040700030822     C                             posADft-posDaDft-1):1:3)
040800030822     C                             = 'NRS'
040900030822     C                   EVAL      PiStr=%trim(%subst(
041000030822     C                             %subst(vlrppt:posDaDft+1:
041100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
041200030822     C                   EXSR      CHKNUM
041300030822     C                   IF        PiInt=*on
041400030822     C                   Z-ADD     PiVal         VABNRS
041500030822     C                   ENDIF
041600030822     C                   ENDIF
041700030822     C* CTR
041800030822     C                   IF        %subst(
041900030822     C                             %subst(vlrppt:posDaDft+1:
042000030822     C                             posADft-posDaDft-1):1:3)
042100030822     C                             = 'CTR'
042200030822     C                   EVAL      PiStr=%trim(%subst(
042300030822     C                             %subst(vlrppt:posDaDft+1:
042400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
042500030822     C                   EXSR      CHKNUM
042600030822     C                   IF        PiInt=*on
042700030822     C                   Z-ADD     PiVal         VABCTR
042800030822     C                   ENDIF
042900030822     C                   ENDIF
043000030822     C* PKB
043100030822     C                   IF        %subst(
043200030822     C                             %subst(vlrppt:posDaDft+1:
043300030822     C                             posADft-posDaDft-1):1:3)
043400030822     C                             = 'PKB'
043500030822     C                   EVAL      PiStr=%trim(%subst(
043600030822     C                             %subst(vlrppt:posDaDft+1:
043700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
043800030822     C                   EXSR      CHKNUM
043900030822     C                   IF        PiNum=*on
044000030822     C                   Z-ADD     PiVal         VABPKB
044100030822     C                   ENDIF
044200030822     C                   ENDIF
044300030822     C* VLB
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'VLB'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiNum=*on
045300030822     C                   Z-ADD     PiVal         VABVLB
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* QFT
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'QFT'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiNum=*on
046600030822     C                   Z-ADD     PiVal         VABQFT
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* CBO
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'CBO'
047400030822     C                   EVAL      VABCBO=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   ENDIF
047800030822     C* TSP
047900030822     C                   IF        %subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):1:3)
048200030822     C                             = 'TSP'
048300030822     C                   EVAL      VABTSP=%trim(%subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048600030822     C                   ENDIF
048700030822     C* VAS
048800030822     C                   IF        %subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):1:3)
049100030822     C                             = 'VAS'
049200030822     C                   EVAL      VABVAS=%trim(%subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049500030822     C                   ENDIF
049600030822     C* VCA
049700030822     C                   IF        %subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):1:3)
050000030822     C                             = 'VCA'
050100030822     C                   EVAL      VABVCA=%trim(%subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050400030822     C                   ENDIF
050500030822     C* GCA
050600030822     C                   IF        %subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):1:3)
050900030822     C                             = 'GCA'
051000030822     C                   EVAL      VABGCA=%trim(%subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051300030822     C                   ENDIF
051400030822     C* CTM
051500030822     C                   IF        %subst(
051600030822     C                             %subst(vlrppt:posDaDft+1:
051700030822     C                             posADft-posDaDft-1):1:3)
051800030822     C                             = 'CTM'
051900030822     C                   EVAL      VABCTM=%trim(%subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052200030822     C                   ENDIF
052300030822     C* FFD
052400030822     C                   IF        %subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):1:3)
052700030822     C                             = 'FFD'
052800030822     C                   EVAL      VABFFD=%trim(%subst(
052900030822     C                             %subst(vlrppt:posDaDft+1:
053000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053100030822     C                   ENDIF
053200030822     C* VAD
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'VAD'
053700030822     C                   EVAL      VABVAD=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   ENDIF
054100030822     C* GMA
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'GMA'
054600030822     C                   EVAL      VABGMA=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   ENDIF
055000030822     C* GGA
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'GGA'
055500030822     C                   EVAL      VABGGA=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   ENDIF
055900030822     C* GVA
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'GVA'
056400030822     C                   EVAL      VABGVA=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   ENDIF
056800030822     C* TC1
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'TC1'
057300030822     C                   EVAL      VABTC1=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   ENDIF
057700030822     C* TC2
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'TC2'
058200030822     C                   EVAL      VABTC2=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   ENDIF
058600030822     C* ...
058700030822     C                   ENDIF
058800030822     C                   ENDDO
058900020204     C*
059000010330     C                   ENDSR
059100010607     C*----------------------------------------------------*
059200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
059300010607     C*----------------------------------------------------*
059400010607     C     IMPVAB        BEGSR
059500010607     C*
059600010607     C                   SETOFF                                       3132
059700030515     C*
059800010607     C                   MOVEL     datcor        VABAAS
059900010607     C                   MOVE      datcor        VABMGS
060000010607     C*
060100030715     C**********
060200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
060300030715     C**********
060400030715     C                   IF        wGiro = *zeros
060500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
060600031201     C     minu:maiu     XLATE     vindta        vindta
060700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
060800040324     C                   DOW       posDa <= %len(%trimr(vindta))
060900030715     C*
061000030715     C* Gestisco il 1° campo
061100030715     C                   IF        i = 1
061200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
061300030715     C                   EVAL      posDa = 1
061400030715     C* Eseguo lo scan x trovare la fine del primo campo
061500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
061600030715     C* A questo punto "estraggo" il campo corrente
061700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
061800030715     C                                                    (posA-posDa))
061900030715     C* X i campi successivi al 1°
062000030715     C                   ELSE
062100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
062200030715     C                   EVAL      posDa = posA + 1
062300030715     C* Eseguo lo scan x trovare la fine del campo corrente
062400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
062500030715     C* Gestisco l'ultimo campo
062600030715     C                   IF        posA = *zeros
062700040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
062800030715     C* A questo punto "estraggo" il campo corrente
062900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063000030715     C                                                    (posA-posDa))
063100030715     C                   LEAVE
063200030715     C                   ELSE
063300030715     C* A questo punto "estraggo" il campo corrente
063400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
063500030715     C                                                    (posA-posDa))
063600030715     C                   ENDIF
063700030715     C                   ENDIF
063800030715     C* Incremento il contatore d campo
063900030715     C                   EVAL      i = i +1
064000030715     C                   ENDDO
064100030715     C*
064200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
064300030715     C* delimitazione testo
064400030715     C                   EVAL      i = 1
064500030715     C                   DOW       i < %elem(SkSplitFLD)
064600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
064700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
064800030715     C                   EVAL      i = i + 1
064900030715     C                   ENDDO
065000030715     C                   EVAL      wGiro = 1
065100030715     C                   ELSE
065200030715     C                   EVAL      wGiro = 2
065300031204     C**********
065400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
065500031204     C* d separatore campo
065600031204     C**********
065700040304     C                   IF        %subst(vindta:1:1) = CharCSV                 * all'inizio
065800040304     C                   EVAL      vindta = ' ' + vindta
065900040304     C                   ENDIF
066000040304     C***                DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
066100040304     C***                EVAL      vindta = %subst(vindta:2)
066200040304     C***                ENDDO
066300031204     C*
066400031204     C                   Z-ADD     *zeros        lunghInput        4 0
066500040324     C                   EVAL      lunghInput = %len(%trimr(vindta))
066600040324     C                   IF        %subst(%trimr(vindta):lunghInput:1) = CharCSV
066700040304     C                   EVAL      vindta = vindta + ' '
066800040304     C                   ENDIF
066900040324     C***                DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
067000040324     C***                EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
067100040304     C***                EVAL      lunghInput = %len(%trim(vindta))
067200040304     C***                ENDDO
067300030715     C**********
067400030715     C* Effettuo lo split del campo dati d input
067500030715     C**********
067600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
067700040324     C                   DOW       posDa <= %len(%trimr(vindta))
067800030715     C*
067900030715     C* Gestisco il 1° campo
068000030715     C                   IF        i = 1
068100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
068200030715     C                   EVAL      posDa = 1
068300030715     C* Eseguo lo scan x trovare la fine del primo campo
068400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
068500030715     C* A questo punto "estraggo" il campo corrente
068600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
068700030715     C                                                    (posA-posDa))
068800030715     C* X i campi successivi al 1°
068900030715     C                   ELSE
069000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
069100030715     C                   EVAL      posDa = posA + 1
069200031204     C* Verifico che nn vi sia il campo nn valorizzato
069300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
069400031204     C* Se campo nn valorizzato skippo al prossimo
069500031204     C                   EVAL      SkSplitCSV(i) = *blanks
069600031204     C                   EVAL      posA  = posA + 1
069700031204     C                   ELSE
069800030715     C* Eseguo lo scan x trovare la fine del campo corrente
069900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
070000030715     C* Gestisco l'ultimo campo
070100030715     C                   IF        posA = *zeros
070200040324     C                   EVAL      posA = %len(%trimr(vindta)) + 1
070300030715     C* A questo punto "estraggo" il campo corrente
070400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
070500030715     C                                                    (posA-posDa))
070600030715     C                   LEAVE
070700030715     C                   ELSE
070800030715     C* A questo punto "estraggo" il campo corrente
070900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
071000030715     C                                                    (posA-posDa))
071100030715     C                   ENDIF
071200031204     C                   ENDIF
071300030715     C                   ENDIF
071400030715     C* Incremento il contatore d campo
071500030715     C                   EVAL      i = i +1
071600030715     C                   ENDDO
071700030715     C*
071800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
071900030715     C* delimitazione testo
072000030715     C                   EVAL      i = 1
072100030715     C                   DOW       i < %elem(SkSplitCSV)
072200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
072300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
072400030715     C                   EVAL      i = i + 1
072500030715     C                   ENDDO
072600030715     C*
072700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
072800030715     C                   EVAL      i = 1
072900030715     C                   DOW       i < %elem(SkSplitFLD)
073000030715     C*
073100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
073200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
073300030715     C                             SkSplitCSV(i) <> *blanks
073400130624     C*** ATB
073500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
073600150420     C                   EVAL      VABATB = SkSplitCSV(i)
073700030721     C                   ENDIF
073800030715     C*** RSD
073900150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
074000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
074100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
074200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
074300030715     C* ==
074400030715     C                   ENDIF
074500030721     C*** RD2
074600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
074700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
074800030721     C                   ENDIF
074900030715     C*** IND
075000150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
075100030715     C                   EVAL      VABIND = SkSplitCSV(i)
075200030715     C                   ENDIF
075300030715     C*** LOD
075400150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
075500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
075600030715     C                   ENDIF
075700030715     C*** PRD
075800150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
075900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
076000030715     C                   ENDIF
076100030721     C*** NZD
076200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
076300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
076400030721     C                   ENDIF
076500030715     C*** RMA
076600150515     C***                IF        %trim(SkSplitFLD(i)) = 'MVSERIAL'
076700150515     C***                EVAL      VABRMA = SkSplitCSV(i)
076800150515     C***                ENDIF
076900150515     C                   IF        %trim(SkSplitFLD(i)) = 'DATA_BOLLA'
077000150515     C                   EVAL      VABRMA = SkSplitCSV(i)
077100150515     C                   ENDIF
077200030715     C*** NOT
077300150421     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC_A'
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
078900150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
079000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
079100030715     C                   ENDIF
079200030715     C*** CBO
079300150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
079400150420     C                   IF        SkSplitCSV(i) <> 'FRANCO'
079500150420     C                   EVAL      VABCBO = '2 '
079600150420     C                   ENDIF
079700030715     C                   ENDIF
079800030721     C*** GC1
079900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
080000150420     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
080100150420     C                   MOVEL     *blanks       wGC               1
080200150420     C                   EVAL      wGC = %trim(SkSplitCSV(i))
080300150420     C                   IF        SkSplitCSV(i) = 'M' OR
080400150420     C                             SkSplitCSV(i) = 'P'
080500150420     C                   MOVE(P)   wGC           VABGC1
080600150420     C                   ELSE
080700150420     C                   MOVEL(P)  wGC           VABGC1
080800150420     C                   ENDIF
080900150420     C                   ENDIF
081000030721     C                   ENDIF
081100030721     C*** GC2
081200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
081300120314     C                   IF        %len(%trim(SkSplitCSV(i))) = 1
081400120314     C                   MOVEL     *blanks       wGC               1
081500120314     C                   EVAL      wGC = %trim(SkSplitCSV(i))
081600120314     C                   IF        SkSplitCSV(i) = 'M' OR
081700120314     C                             SkSplitCSV(i) = 'P'
081800120314     C                   MOVE(P)   wGC           VABGC2
081900120314     C                   ELSE
082000120314     C                   MOVEL(P)  wGC           VABGC2
082100120314     C                   ENDIF
082200120314     C                   ENDIF
082300030721     C                   ENDIF
082400030721     C*** TSP
082500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
082600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
082700030721     C                   ENDIF
082800030721     C*** NAS
082900150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
083000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
083100030721     C                   ENDIF
083200030721     C*** TIC
083300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
083400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
083500030721     C                   ENDIF
083600030721     C*** GCA
083700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
083800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
083900030721     C                   ENDIF
084000030721     C*** XCO
084100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
084200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
084300030721     C                   ENDIF
084400030721     C*** CTM
084500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
084600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
084700030721     C                   ENDIF
084800030721     C*** TCR
084900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
085000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
085100030721     C                   ENDIF
085200030721     C*** CTS
085300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
085400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
085500030721     C                   ENDIF
085600030721     C*** FTM
085700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
085800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
085900030721     C                   ENDIF
086000030721     C*** VAD
086100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
086200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
086300030721     C                   ENDIF
086400030721     C*** GMA
086500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
086600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
086700030721     C                   ENDIF
086800030721     C*** GGA
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
087000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030721     C*** GVA
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
087400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030721     C*** TC1
087700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
087800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
087900030721     C                   ENDIF
088000030721     C*** TC2
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
088200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030721     C*** SCL
088500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
088600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
088700030721     C                   ENDIF
088800030721     C*** RMO
088900150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
089000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** NMO
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
089400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
089500030721     C                   ENDIF
089600150420     C*** NOT_I
089700150421     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC_I'
089800150420     C                   EVAL      wVATNOT_I = SkSplitCSV(i)
089900150420     C                   ENDIF
090000150421     C*** NOT_A
090100150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090200150421     C                   EVAL      wVATNOT_A = SkSplitCSV(i)
090300150420     C                   ENDIF
090400030721     C*
090500070705     C* Reperisco quindi i campi numerici...
090600030721     C*** CAD
090700150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
090800070705     C                   EVAL      VABCAD = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030721     C*** RMN
091100150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
091200030721     C                   EVAL      PiStr=SkSplitCSV(i)
091300030721     C                   EXSR      CHKNUM
091400030721     C                   IF        PiInt=*on
091500030721     C                   Z-ADD     PiVal         VABRMN
091600030721     C                   ELSE
091700030721     C                   SETON                                        32
091800030721     C                   EVAL      VABRMN = 1
091900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
092000030721     C                             + ' ' + 'VABRMN'
092100030721     C                   ENDIF
092200030721     C                   ENDIF
092300030721     C*** NSP
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
092500030721     C                   EVAL      PiStr=SkSplitCSV(i)
092600030721     C                   EXSR      CHKNUM
092700100517     C                   IF        PiInt=*on
092800030721     C                   Z-ADD     PiVal         VABNSP
092900030721     C                   ELSE
093000030721     C                   SETON                                        32
093100030721     C                   EVAL      VABNSP = *zeros
093200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
093300030721     C                             + ' ' + 'VABNSP'
093400030721     C                   ENDIF
093500030721     C                   ENDIF
093600030721     C*** NCL
093700150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
093800030721     C                   EVAL      PiStr=SkSplitCSV(i)
093900030721     C                   EXSR      CHKNUM
094000030721     C                   IF        PiInt=*on
094100030721     C                   Z-ADD     PiVal         VABNCL
094200030721     C                   ELSE
094300030721     C                   SETON                                        32
094400030721     C                   EVAL      VABNCL = *zeros
094500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
094600030721     C                             + ' ' + 'VABNCL'
094700030721     C                   ENDIF
094800030721     C                   ENDIF
094900030721     C*** PKB
095000150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
095100030721     C                   EVAL      PiStr=SkSplitCSV(i)
095200030721     C                   EXSR      CHKNUM
095300030721     C                   IF        PiNum=*on
095400030721     C                   Z-ADD     PiVal         VABPKB
095500030721     C                   ELSE
095600030721     C                   SETON                                        32
095700030721     C                   EVAL      VABPKB = *zeros
095800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
095900030721     C                             + ' ' + 'VABPKB'
096000030721     C                   ENDIF
096100030721     C                   ENDIF
096200030721     C*** CAS
096300150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
096400030721     C                   EVAL      PiStr=SkSplitCSV(i)
096500030721     C                   EXSR      CHKNUM
096600030721     C                   IF        PiNum=*on
096700030721     C                   Z-ADD     PiVal         VABCAS
096800030721     C                   ELSE
096900030721     C                   SETON                                        32
097000030721     C                   EVAL      VABCAS = *zeros
097100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
097200030721     C                             + ' ' + 'VABCAS'
097300030721     C                   ENDIF
097400030721     C                   ENDIF
097500030721     C*** CCM
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
097700030721     C                   EVAL      PiStr=SkSplitCSV(i)
097800030721     C                   EXSR      CHKNUM
097900030721     C                   IF        PiInt=*on      AND
098000030721     C                             PiVal<=9999999 AND
098100030721     C                             PiVal>*zeros
098200030721     C                   Z-ADD     PiVal         VABCCM
098300030721     C                   ELSE
098400030721     C                   SETON                                        32
098500030721     C                   EVAL      VABCCM = *zeros
098600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098700030721     C                             + ' ' + 'VABCCM'
098800030721     C                   ENDIF
098900030721     C                   ENDIF
099000030721     C*** LNP
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
099200030721     C                   EVAL      PiStr=SkSplitCSV(i)
099300030721     C                   EXSR      CHKNUM
099400030721     C                   IF        PiInt=*on    AND
099500030721     C                             PiVal<=999   AND
099600030721     C                             PiVal>*zeros
099700030721     C                   Z-ADD     PiVal         VABLNP
099800030721     C                   ELSE
099900030721     C                   SETON                                        32
100000030721     C                   EVAL      VABLNP = *zeros
100100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
100200030721     C                             + ' ' + 'VABLNP'
100300030721     C                   ENDIF
100400030721     C                   ENDIF
100500030721     C*** NRS
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
100700030721     C                   EVAL      PiStr=SkSplitCSV(i)
100800030721     C                   EXSR      CHKNUM
100900030721     C                   IF        PiInt=*on AND
101000030721     C                             PiVal<=99
101100030721     C                   Z-ADD     PiVal         VABNRS
101200030721     C                   ELSE
101300030721     C                   SETON                                        32
101400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101500030721     C                             + ' ' + 'VABNRS'
101600030721     C                   ENDIF
101700030721     C                   ENDIF
101800030721     C*** LNA
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
102000120314     C                   IF        %trim(SkSplitCSV(i)) <> *zeros  AND
102100120314     C                             %trim(SkSplitCSV(i)) <> '0'     AND
102200120314     C                             %trim(SkSplitCSV(i)) <> '000'   AND
102300120314     C                             %trim(SkSplitCSV(i)) <> *blanks
102400030721     C                   EVAL      PiStr=SkSplitCSV(i)
102500030721     C                   EXSR      CHKNUM
102600030721     C                   IF        PiInt=*on    AND
102700030721     C                             PiVal<=999   AND
102800030721     C                             PiVal>*zeros
102900030721     C                   Z-ADD     PiVal         VABLNA
103000030721     C                   ELSE
103100030721     C                   SETON                                        32
103200030721     C                   EVAL      VABLNA = *zeros
103300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103400030721     C                             + ' ' + 'VABLNA'
103500120314     C                   ENDIF
103600030721     C                   ENDIF
103700030721     C                   ENDIF
103800030721     C*** CTR
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
104000030721     C                   EVAL      PiStr=SkSplitCSV(i)
104100030721     C                   EXSR      CHKNUM
104200030721     C                   IF        PiInt=*on  AND
104300030721     C                             PiVal<=999
104400030721     C                   Z-ADD     PiVal         VABCTR
104500030721     C                   ELSE
104600030721     C                   SETON                                        32
104700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104800030721     C                             + ' ' + 'VABCTR'
104900030721     C                   ENDIF
105000030721     C                   ENDIF
105100030721     C*** IAS
105200150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
105300030721     C                   EVAL      PiStr=SkSplitCSV(i)
105400030721     C                   EXSR      CHKNUM
105500030721     C                   IF        PiNum=*on
105600030721     C                   Z-ADD     PiVal         VABIAS
105700030721     C                   ELSE
105800030721     C                   SETON                                        32
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000030721     C                             + ' ' + 'VABIAS'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** VLB
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiNum=*on
106800030721     C                   Z-ADD     PiVal         VABVLB
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABVLB'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** QFT
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiNum=*on
108000030721     C                   Z-ADD     PiVal         VABQFT
108100030721     C                   ELSE
108200030721     C                   SETON                                        32
108300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108400030721     C                             + ' ' + 'VABQFT'
108500030721     C                   ENDIF
108600030721     C                   ENDIF
108700030721     C*** NCD
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
108900030721     C                   EVAL      PiStr=SkSplitCSV(i)
109000030721     C                   EXSR      CHKNUM
109100030721     C                   IF        PiInt=*on      AND
109200030721     C                             PiVal<=9999999
109300030721     C                   Z-ADD     PiVal         VABNCD
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109700030721     C                             + ' ' + 'VABNCD'
109800030721     C                   ENDIF
109900030721     C                   ENDIF
110000030721     C*** NCA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiInt=*on      AND
110500030721     C                             PiVal<=9999999
110600030721     C                   Z-ADD     PiVal         VABNCA
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABNCA'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** ZNC
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on AND
111800030721     C                             PiVal<=99
111900030721     C                   Z-ADD     PiVal         VABZNC
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABZNC'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** DCR
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on       AND
113100030721     C                             PiVal<=99999999
113200030721     C                   Z-ADD     PiVal         VABDCR
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600030721     C                             + ' ' + 'VABDCR'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** HCR
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on   AND
114400030721     C                             PiVal<=9999
114500030721     C                   Z-ADD     PiVal         VABHCR
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABHCR'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** VMD
115300150421     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiNum=*on
115700030721     C                   Z-ADD     PiVal         VABVMD
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABVMD'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** ANT
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on        AND
116900030721     C                             PiVal<=999999999
117000030721     C                   Z-ADD     PiVal         VABANT
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABANT'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CMO
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         Num5_0
118300030721     C                   MOVEL(p)  Num5_0        VABCMO
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCMO = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCMO'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030715     C*
119200030715     C                   ENDIF
119300030715     C                   EVAL      i = i + 1
119400030715     C                   ENDDO
119500070705     C*
119600070705     C* Considerazioni particolari
119700110706     C*
119800110706     C*** RMN
119900110706     C                   IF        *in70
120000110706     C                   EVAL      VABRMN = VABNSP
120100110706     C                   ENDIF
120200070705     C*** NZD
120300070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
120400070705     C                   EVAL      VABNZD = *blanks
120500070705     C                   ENDIF
120600080222     C*
120700080222     C* Se provincia nn valorizzata la reperisco
120800080222     C* tramite TISI95R a seconda dei dati d instradamento presenti
120900080222     C                   IF        VABPRD = *blanks
121000080222     C                   CLEAR                   TISI95DS
121100080222     C                   EVAL      I95TCN = '3'
121200080222     C                   Z-ADD     datcor        I95DAT
121300080222     C                   EVAL      I95NAR = VABNZD
121400080222     C                   EVAL      I95CAP = VABCAD
121500080222     C                   EVAL      I95LOC = VABLOD
121600080222     C                   CALL      'TISI95R'
121700080222     C                   PARM                    TISI95DS
121800080222     C                   EVAL      VABPRD = O95PRV
121900080222     C                   ENDIF
122000020204     C*
122100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
122200020204     C                   EXSR      CHKIMPDIV
122300030715     C                   ENDIF
122400020204     C*
122500010607     C                   ENDSR
122600010607     C*----------------------------------------------------*
122700010601
122800020204
122900020204     C*----------------------------------------------------*
123000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
123100020204     C*----------------------------------------------------*
123200020204     C     CHKIMPDIV     BEGSR
123300060524     C*
123400060524     C* Verifico il codice bolla in base al tipo porto e alla presenza o meno del contrassegno
123500060524     C                   IF        vabCAS > *zeros
123600060524     C                   IF        vabCBO = '1'
123700060524     C                   EVAL      vabCBO = '4'
123800060524     C                   ENDIF
123900060524     C                   IF        vabCBO = '2'
124000060524     C                   EVAL      vabCBO = '6'
124100060524     C                   ENDIF
124200060524     C                   ENDIF
124300020204     C*
124400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
124500020204     C                   Z-ADD     *zeros        wrkDec            9 9
124600020204     C*
124700020204     C* Come prima cosa effettuo considerazioni sulla divisa
124800020204     C                   IF        vabIAS > *zeros
124900150420     C                   IF        vabVAS =  *blanks
125000150420     C                   EVAL      vabVAS =  'EUR'
125100020204     C                   ENDIF
125200020204     C                   ENDIF
125300020204     C*
125400020204     C                   IF        vabCAS > *zeros
125500150420     C                   IF        vabVCA <> *blanks
125600150420     C                   EVAL      vabVCA =  'EUR'
125700020204     C                   ENDIF
125800020204     C                   ENDIF
125900020204     C*
126000020204     C                   IF        vabVMD > *zeros
126100150420     C                   IF        vabVAD <> *blanks
126200150420     C                   EVAL      vabVAD =  'EUR'
126300020204     C                   ENDIF
126400020204     C                   ENDIF
126500020204     C*
126600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
126700020204     C                   Z-ADD     vabIAS        wrkDec
126800020204     C                   IF        wrkDec > *zeros
126900020204     C                   IF        vabVAS = 'ITL'
127000020204     C                   EVAL      vabIAS = *zeros
127100020204     C                   ENDIF
127200020204     C                   ENDIF
127300020204     C*
127400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
127500020204     C                   Z-ADD     vabCAS        wrkDec
127600020204     C                   IF        wrkDec > *zeros
127700020204     C                   IF        vabVCA = 'ITL'
127800020204     C                   EVAL      vabCAS = *zeros
127900020204     C                   ENDIF
128000020204     C                   ENDIF
128100020204     C*
128200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
128300020204     C                   Z-ADD     vabVMD        wrkDec
128400020204     C                   IF        wrkDec > *zeros
128500020204     C                   IF        vabVAD = 'ITL'
128600020204     C                   EVAL      vabVMD = *zeros
128700020204     C                   ENDIF
128800020204     C                   ENDIF
128900020204     C*
129000020204     C                   ENDSR
129100020204     C***
129200020204
129300010330
129400010330
129500010330     C*----------------------------------------------------*
129600010330     C*  CONTROLLO NUMERICITA' CAMPI
129700010330     C*----------------------------------------------------*
129800010330     C     CHKNUM        BEGSR
129900010330     C*
130000010606     C                   IF        PiDecChr = *blanks
130100030715     C                   EVAL      PiDecChr = CharNUM
130200010606     C                   ENDIF
130300010606     C*
130400010606     C                   CALL(e)   'ISNUMERIC'
130500010330     C                   PARM                    PiStr            30
130600010606     C                   PARM                    PiDecChr          1
130700010330     C                   PARM      *ZEROS        PiVal            30 9
130800010330     C                   PARM      '0'           PiInt             1
130900010330     C                   PARM      '0'           PiNum             1
131000010330     C                   IF        %error
131100010606     C                   EVAL      PiNum=*off
131200010330     C                   ENDIF
131300010330     C*
131400010330     C                   ENDSR
131500010330     C***
131600010330
131700010601
131800010601
131900010601
132000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
132100010601     C     repfil        BEGSR
132200010601     C*
132300010601     C                   if        invfil = *zeros and
132400010601     C                             depfil > *zeros and
132500010629     C                             (vinflg = *blanks or
132600010629     C                              vinflg = *zeros)
132700010601     C
132800010601     C                   eval      invfil = depfil
132900010601     C                   endif
133000010601     C*
133100010601     C                   if        depfil <> invfil and
133200010601     C                             invfil > *zeros
133300010601     C                   eval      flgMulti = '1'
133400010601     C                   if        vinflg = *blanks
133500010601     C                   add       1             cntNonEl
133600010601     C                   endif
133700010601     C                   endif
133800010601     C*
133900010601     C                   if        vinflg = '2'
134000010601     C                   eval      flgStato = '2'
134100010601     C                   endif
134200010601     C*
134300010601     C                   ENDSR
134400010601     C***
134500010601
134600150420
134700010601
134800010601      /TITLE Invio dei dati al punto operativo.
134900010601     C     opeini        BEGSR
135000010601     C*
135100010601     C* Inizializzo flag e contatori operativi
135200010601     C                   movel     '0'           flgGiro           1
135300010601     C                   movel     '0'           flgMulti          1
135400010601     C                   movel     '1'           flgStato          1
135500010615     C                   movel     '0'           flgOk             1
135600010601     C                   z-add     *zeros        cntNonEl         10 0
135700010601     C                   z-add     *zeros        depfil            3 0
135800010601     C                   z-add     *zeros        invfil            3 0
135900010601     C*
136000010601     C                   ENDSR
136100010601     C***
136200050421
136300050421
136400050421
136500050421     C     *pssr         BEGSR
136600050421     C*
136700050421     C                   eval      wrkesito = '2'
136800050421     C*
136900050421     C                   ENDSR     '*CANCL'
137000050421     C***
137100100419
137200100419
137300100419
137400100419     C     normCSV       BEGSR
137500100419     C*
137600100419     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
137700100419     C* separatori campi
137800100419     C                   if        CharTXT <> *blanks
137900100419     C                   z-add     *zeros        wLenVINDTA        4 0
138000100419     C                   z-add     *zeros        wFlgVINDTA        1 0
138100100419     C                   setoff                                       80
138200100419     C                   dow       wLenVINDTA < %len(%trim(vindta))
138300100419     C                   eval      wLenVINDTA = wLenVINDTA + 1
138400100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
138500100419     C                   if        not *in80
138600100419     C                   seton                                        80
138700100419     C                   else
138800100419     C                   setoff                                       80
138900100419     C                   endif
139000100419     C                   endif
139100100419     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
139200100419     C                             and *in80 = *on
139300100419     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
139400100419     C                   endif
139500100419     C                   enddo
139600100419     C                   endif
139700100419     C*
139800100419     C                   ENDSR
139900100419     C***
140000150420
140100150420
140200150420
140300150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
140400150420     C     wriVGDHEAD    BEGSR
140500150420     C*
140600150420     C                   eval      wBufferOut =
140700150420     C                             %trim(pptCHR)+'VABCCM' +%trim(pptCHR)+pptCSV+
140800150420     C                             %trim(pptCHR)+'VABLNP' +%trim(pptCHR)+pptCSV+
140900150420     C                             %trim(pptCHR)+'VABCTR' +%trim(pptCHR)+pptCSV+
141000150420     C                             %trim(pptCHR)+'VABRSD' +%trim(pptCHR)+pptCSV+
141100150420     C                             %trim(pptCHR)+'VABIND' +%trim(pptCHR)+pptCSV+
141200150420     C                             %trim(pptCHR)+'VABLOD' +%trim(pptCHR)+pptCSV+
141300150420     C                             %trim(pptCHR)+'VABCAD' +%trim(pptCHR)+pptCSV+
141400150420     C                             %trim(pptCHR)+'VABPRD' +%trim(pptCHR)+pptCSV+
141500150420     C                             %trim(pptCHR)+'VABCBO' +%trim(pptCHR)+pptCSV+
141600150420     C                             %trim(pptCHR)+'VABRMO' +%trim(pptCHR)+pptCSV+
141700150420     C                             %trim(pptCHR)+'VABRMN' +%trim(pptCHR)+pptCSV+
141800150420     C                             %trim(pptCHR)+'VABRMA' +%trim(pptCHR)+pptCSV+
141900150420     C                             %trim(pptCHR)+'VABNAS' +%trim(pptCHR)+pptCSV+
142000150420     C                             %trim(pptCHR)+'VABNCL' +%trim(pptCHR)+pptCSV+
142100150420     C                             %trim(pptCHR)+'VABPKB' +%trim(pptCHR)+pptCSV+
142200150420     C                             %trim(pptCHR)+'VABCAS' +%trim(pptCHR)+pptCSV+
142300150420     C                             %trim(pptCHR)+'VABVCA' +%trim(pptCHR)+pptCSV+
142400150420     C                             %trim(pptCHR)+'VABFFD' +%trim(pptCHR)+pptCSV+
142500150420     C                             %trim(pptCHR)+'VABIAS' +%trim(pptCHR)+pptCSV+
142600150420     C                             %trim(pptCHR)+'VABVAS' +%trim(pptCHR)+pptCSV+
142700150421     C                             %trim(pptCHR)+'VABNOT' +%trim(pptCHR)+pptCSV+
142800150420     C                             %trim(pptCHR)+'VABEMD' +%trim(pptCHR)+pptCSV+
142900150421     C                             %trim(pptCHR)+'VABTRC' +%trim(pptCHR)
143000150420     C                   exsr      wriVGD
143100150420     C*
143200150420     C                   ENDSR
143300150420     C*------------------------------------------------------------------------*
143400150420
143500150420
143600150420      /TITLE Scrittura record in file TIVGD00F (file VAS generico download)
143700150420     C     wriVGDSHPM    BEGSR
143800150420     C*
143900150420     C                   eval      wBufferOut =
144000150420     C                             %trim(pptCHR)+%trim(%editc(VABCCM:'X'))
144100150420     C                                                    +%trim(pptCHR)+pptCSV+
144200150420     C                             %trim(pptCHR)+%trim(%editc(VABLNP:'X'))
144300150420     C                                                    +%trim(pptCHR)+pptCSV+
144400150420     C                             %trim(pptCHR)+%trim(%editc(VABCTR:'X'))
144500150420     C                                                    +%trim(pptCHR)+pptCSV+
144600150420     C                             %trim(pptCHR)+%trim(VABRSD)
144700150420     C                                                    +%trim(pptCHR)+pptCSV+
144800150420     C                             %trim(pptCHR)+%trim(VABIND)
144900150420     C                                                    +%trim(pptCHR)+pptCSV+
145000150420     C                             %trim(pptCHR)+%trim(VABLOD)
145100150420     C                                                    +%trim(pptCHR)+pptCSV+
145200150420     C                             %trim(pptCHR)+%trim(VABCAD)
145300150420     C                                                    +%trim(pptCHR)+pptCSV+
145400150420     C                             %trim(pptCHR)+%trim(VABPRD)
145500150420     C                                                    +%trim(pptCHR)+pptCSV+
145600150420     C                             %trim(pptCHR)+%trim(VABCBO)
145700150420     C                                                    +%trim(pptCHR)+pptCSV+
145800150420     C                             %trim(pptCHR)+%trim(VABRMO)
145900150420     C                                                    +%trim(pptCHR)+pptCSV+
146000150420     C                             %trim(pptCHR)+%trim(%editc(VABRMN:'4'))
146100150420     C                                                    +%trim(pptCHR)+pptCSV+
146200150420     C                             %trim(pptCHR)+%trim(VABRMA)
146300150420     C                                                    +%trim(pptCHR)+pptCSV+
146400150420     C                             %trim(pptCHR)+%trim(VABNAS)
146500150420     C                                                    +%trim(pptCHR)+pptCSV+
146600150420     C                             %trim(pptCHR)+%trim(%editc(VABNCL:'4'))
146700150420     C                                                    +%trim(pptCHR)+pptCSV+
146800150420     C                             %trim(pptCHR)+%trim(%editc(VABPKB:'4'))
146900150420     C                                                    +%trim(pptCHR)+pptCSV+
147000150420     C                             %trim(pptCHR)+%trim(%editc(VABCAS:'4'))
147100150420     C                                                    +%trim(pptCHR)+pptCSV+
147200150420     C                             %trim(pptCHR)+%trim(VABVCA)
147300150420     C                                                    +%trim(pptCHR)+pptCSV+
147400150420     C                             %trim(pptCHR)+%trim(VABFFD)
147500150420     C                                                    +%trim(pptCHR)+pptCSV+
147600150420     C                             %trim(pptCHR)+%trim(%editc(VABIAS:'4'))
147700150420     C                                                    +%trim(pptCHR)+pptCSV+
147800150420     C                             %trim(pptCHR)+%trim(VABVAS)
147900150420     C                                                    +%trim(pptCHR)+pptCSV+
148000150421     C                             %trim(pptCHR)+%trim(VABNOT)
148100150421     C                                                    +%trim(pptCHR)+pptCSV+
148200150420     C                             %trim(pptCHR)+%trim(wVATNOT_I)
148300150420     C                                                    +%trim(pptCHR)+pptCSV+
148400150421     C                             %trim(pptCHR)+%trim(wVATNOT_A)
148500150420     C                                                    +%trim(pptCHR)
148600150420     C                   exsr      wriVGD
148700150420     C*
148800150420     C                   ENDSR
148900150420     C*------------------------------------------------------------------------*
149000150420
149100150420
149200150420
149300150420      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
149400150420     C     wriVGD        BEGSR
149500150420     C*
149600150420     C                   clear                   tivgd000
149700150420     C                   eval      vgdDTA = wBufferOut
149800150420     C                   eval      vgdTIP = 'EB'
149900150420     C                   eval      vgdKSU = vlrKSC
150000150420     C                   eval      vgdTSC = 'WW'
150100150420     C                   eval      vgdDAT = datcor
150200150421     C                   eval      vgdPGM = 'TITV2T8R1'
150300150420     C                   write     tivgd000
150400150420     C*
150500150420     C                   ENDSR
150600150420     C*------------------------------------------------------------------------*
150700150420
150800150420
150900150420
151000000613     C     *inzsr        BEGSR
151100990910     C*
151200990910     C     *entry        plist
151300990920     C                   parm                    tivlrds
151400990921     C                   parm      wrkesito      esito
151500000724     C                   parm                    prmlit
151600000710     C                   parm                    prmfir
151700010330     C*
151800010330     C* CALCOLA LA DATA CORRENTE
151900150420     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
152000150420     C                   eval      datcor = %dec(%date() : *ISO)
152100000613     C*
152200000613     C                   ENDSR
152300000613     C***
