000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002600040510     D parccm          s              8    INZ(*blanks)
002700040119     D parmbr          s             10    INZ(*blanks)
002800040119     D paropz          s              1    INZ(*blanks)
002900040119     D chkcall         s              1    INZ(*blanks)
003000030715     D*------------------
003100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003200030715     D*------------------
003300030715     D SkSplitFLD      S             10    DIM(100)
003400030715     D SkSplitCSV      S            256    DIM(100)
003500030715     D CharCSV         S              1
003600030715     D CharTXT         S              1
003700030715     D CharNUM         S              1
003800030715     D posDa           S              3  0 INZ(*zeros)
003900030715     D posA            S              3  0 INZ(*zeros)
004000030715     D i               s              3  0 INZ(1)
004100030715     D wGiro           s              1  0 INZ(*zeros)
004200060331     D wVATNOT_A       s                   LIKE(VATNOT)
004300060331     D wVATNOT_B       s                   LIKE(VATNOT)
004400060331     D wVATNOT_E       s                   LIKE(VATNOT)
004500060331     D wVABNSP         s                   LIKE(VABNSP) INZ
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300010330     D*------------------
005400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010330     D*------------------
005600010330     D WLBDA8          DS                  INZ
005700010330     D  G08DAT                 1      8  0
005800010330     D  G08INV                 9     16  0
005900010330     D  G08ERR                17     17
006000010330     D  G08TGI                18     22  0
006100031201     D*------------------
006200031201     D* Costanti
006300031201     D*------------------
006400031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006500031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006600010330
006700010330
006800010330
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200010601     C*
007300010601     C                   exsr      opeini
007400010605     C                   exsr      rwvab
007500010601     C*
007600010601     C                   seton                                        lr
007700010601
007800010601
007900010601
008000010601     C*--------------------------------------------------------
008100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008200010601     C*--------------------------------------------------------
008300010601     C     PREELA        BEGSR
008400010601     C*
008500010601     C* SE OCCORRE SPEDIRE IN FILIALE
008600010601     C                   if        invfil <> *zeros and
008700010601     C                             flgGiro = '0'
008800010601     C*
008900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009000010601     C                   eval      flgGiro = '1'
009100010601     C*
009200010601     C                   endif
009300010601     C*
009400010601     C                   ENDSR
009500010601     C***
009600010601
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010601     C*--------------------------------------------------------
010300010601     C     ENDELA        BEGSR
010400040119     C*
010500040119     C* Effettuo la chiamata al CLLE preposto
010600040119     C                   call(e)   'TITVVTC'
010700040510     C                   parm                    parccm
010800040119     C                   parm                    parmbr
010900040119     C                   parm      '2'           paropz
011000050201     C*
011100050201     C* Effettuo lancio TISI95 solo x chiusura
011200050201     C                   CLEAR                   TISI95DS
011300050201     C                   EVAL      I95TLA = 'C'
011400050201     C                   CALL      'TISI95R'
011500050201     C                   PARM                    TISI95DS
011600000616     C*
011700010601     C                   ENDSR
011800010601     C***
011900000613
012000010601
012100010601
012200010330     C*--------------------------------------------------------
012300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012400010330     C*--------------------------------------------------------
012500010605     C     RWVAB         BEGSR
012600010330     C*
012700010330     C                   if        not %open(tivin00r)
012800010330     C                   open      tivin00r
012900010330     C                   endif
013000021025     C                   if        not %open(fivabwwr)
013100021025     C                   open      fivabwwr
013200010330     C                   endif
013300040119     C*
013400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013500040119     C                   exsr      prevat
013600040119     C*
013700040119     C                   if        chkcall = '0'
013800040119     C*
013900040119     C                   if        not %open(fivatwwr)
014000040119     C                   open      fivatwwr
014100040119     C                   endif
014200010330     C*
014300040119     C                   clear                   §CTROKVB          7 0
014400040119     C                   clear                   §CTROKVT          7 0
014500040119     C                   clear                   §CTRMO            7 0
014600040119     C                   clear                   §CTRNO            7 0
014700010330     C*
014800030822     C                   do        *HIVAL
014900010330     C*
015000030822     C                   read      tivin00r                               70
015100010618     C*
015200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015300010618     C                   if        vinflg = '1'
015400010618     C                   eval      flgOk = '1'
015500010618     C                   endif
015600040510     C*
015700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015800040510     C                   z-add     *zeros        wLenVINDTA        4 0
015900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016300040510     C                   z-add     1             wFlgVINDTA
016400040510     C                   leave
016500040510     C                   endif
016600040510     C                   enddo
016700010618     C*
016800040510     C                   if        vindta > *blanks AND
016900040510     C                             wFlgVINDTA = 1
017000010330     C                   add       1             rrnum
017100010330     C*
017200010601     C                   if        *in70 = *off and
017300010330     C                             (vinflg = *blanks
017400010330     C                              or vinflg = '0'
017500010330     C                              or vinflg = '2')
017600010330     C*
017700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017800010711     C                   if        vinflg = *blanks or vinflg = '0'
017900010711     C                   clear                   vinmsg
018000010711     C                   endif
018100010601     C*
018200010330     C                   exsr      inzvar
018300010605     C                   exsr      impvab
018400010601     C*
018500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018600010605     C                   eval      depfil = VABLNP
018700010601     C                   exsr      repfil
018800010601     C                   if        depfil = invfil
018900021025     C                   if        vlrpoi = 999
019000030822     C                   move(P)   invfil        VABFGS
019100021025     C                   else
019200030822     C                   move(P)   vlrpoi        VABFGS
019300021025     C                   endif
019400010601     C*
019500010601     C                   exsr      PREELA
019600010601     C*
019700010604     C* Ebbene...
019800010604     C*
019900040119     C  N31              add       1             §CTROKVB
020000040119     C   32              add       1             §CTRMO
020100040119     C   31              add       1             §CTRNO
020200030822     C                   if        wGiro = 2
020300040510     C* Scrivo sempre il VAT
020400040510     C  N31              exsr      wrivat
020500040510     C* Scrivo il VAB solo a rottura d spedizione
020600040119     C                   if        depspe =  *zeros OR
020700040119     C                             depspe <> vabNSP
020800040119     C  N31              exsr      wrivab
020900040119     C  N31              eval      depspe = vabNSP
021000040119     C                   endif
021100040119     C                   endif
021200010604     C*
021300010604     C                   if        *in31 = *off and
021400010604     C                             *in32 = *off
021500010604     C                   eval      vinflg = '1'
021600010604     C                   else
021700010604     C                   eval      vinflg = '2'
021800010604     C                   endif
021900010604     C                   endif
022000010601     C*
022100010604     C                   endif
022200010604     C*
022300010330     C                   else
022400010330     C                   eval      vinflg = '1'
022500010330     C                   endif
022600010601     C*
022700010601     C  N70              update    tivin000
022800010330     C*
022900030822     C  N70              enddo
023000040119     C*
023100040119     C                   endif                                                  (endif - chkcall)
023200010601     C*
023300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023400010601     C                   if        cntNonEl = *zeros or
023500010601     C                             flgMulti = '0'
023600010330     C* Se non ci sono record con errori ...
023700010601     C                   if        §ctrno = 0 and
023800010604     C                             §ctrmo = 0 and
023900010601     C                             flgStato <> '2'
024000010330     C* ... restituisco esito OK.
024100010330     C                   eval      wrkesito = '0'
024200010330     C                   else
024300040119     C                   if        §ctrokvb > 0
024400010330     C                   eval      wrkesito = '1'
024500010330     C                   else
024600010615     C                   if        flgOk = '0'
024700010615     C                   eval      wrkesito = '2'
024800010615     C                   else
024900010615     C                   eval      wrkesito = '6'
025000010615     C                   endif
025100010330     C                   endif
025200010330     C                   endif
025300010601     C                   else
025400010601     C                   eval      wrkesito = '9'
025500010601     C                   endif
025600010330     C*
025700010330     C                   if        %open(tivin00r)
025800010330     C                   close     tivin00r
025900010330     C                   endif
026000021025     C                   if        %open(fivabwwr)
026100021025     C                   close     fivabwwr
026200010330     C                   endif
026300040512     C                   if        %open(fivatwwr)
026400040512     C                   close     fivatwwr
026500040512     C                   endif
026600010601     C*
026700010601     C                   if        vlrpoi <> 999
026800010601     C                   eval      invfil = vlrpoi
026900010601     C                   endif
027000010330     C*
027100040119     C                   if        §ctrokvb > 0
027200010601     C                             and invfil > *zeros
027300010330     C                   exsr      invio
027400010330     C                   endif
027500010601     C*
027600010618     C                   if        flgGiro = '1'
027700010601     C                   exsr      endela
027800010618     C                   endif
027900010330     C*
028000010330     C                   ENDSR
028100010330     C***
028200040119
028300040119
028400040119
028500040119     C*----------------------------------------------------*
028600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
028700040119     C*----------------------------------------------------*
028800040510     C     WRIVAB        BEGSR
028900040119     C*
029000060331     C* Gestisco l'eventuale rottura x numero spedizione
029100060331     C                   if        VABNSP <> wVABNSP
029200070301     C                   if        wVATNOT_E <> *blanks
029300070301     C                   eval      VABCTM = '7Q'
029400070301     C                   endif
029500040119     C                   write     fivab000                                     => scarico il VAB
029600060331     C                   eval      wVABNSP = VABNSP
029700060331     C                   endif
029800040119     C*
029900040119     C                   ENDSR
030000040119
030100040119
030200040119
030300040119     C*----------------------------------------------------*
030400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
030500040119     C*----------------------------------------------------*
030600040510     C     WRIVAT        BEGSR
030700060512     C*
030800060512     C* Inizializzo i campi chiave
030900060512     C                   eval      VATFGS = VABFGS
031000060512     C                   eval      VATAAS = VABAAS
031100060512     C                   eval      VATCCM = VABCCM
031200060512     C                   eval      VATNSP = VABNSP
031300060512     C                   eval      VATLNP = VABLNP
031400040714     C*
031500040714     C* Solo se VATNOT valorizzato scarico il buffer
031600040714     C                   if        VATNOT <> *blanks
031700040119     C*
031800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
031900040510     C                   if        VATTRC = *blanks
032000040510     C                   eval      VATTRC = 'E'
032100040510     C                   endif
032200040510     C                   write     FIVAT000
032300040119     C                   add       1             §CTROKVT
032400040714     C*
032500040714     C                   endif
032600060331     C*
032700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
032800060331     C                   if        wVATNOT_A <> *blanks
032900060331     C                   eval      VATNOT = wVATNOT_A
033000060331     C                   eval      VATTRC = 'A'
033100060331     C                   write     FIVAT000
033200060331     C                   add       1             §CTROKVT
033300060331     C                   endif
033400060331     C*
033500060331     C                   if        wVATNOT_B <> *blanks
033600060331     C                   eval      VATNOT = wVATNOT_B
033700060331     C                   eval      VATTRC = 'B'
033800060331     C                   write     FIVAT000
033900060331     C                   add       1             §CTROKVT
034000060331     C                   endif
034100060331     C*
034200060331     C                   if        wVATNOT_E <> *blanks
034300060331     C                   eval      VATNOT = wVATNOT_E
034400060331     C                   eval      VATTRC = 'E'
034500060331     C                   write     FIVAT000
034600060331     C                   add       1             §CTROKVT
034700060331     C                   endif
034800040119     C*
034900040119     C                   ENDSR
035000040119
035100040119
035200040119
035300040119     C*----------------------------------------------------*
035400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
035500040119     C*----------------------------------------------------*
035600040119     C     PREVAT        BEGSR
035700040119     C*
035800040119     C* Compongo il nome del membro da dare al FIVATWWR
035900040119     C                   eval      parmbr = vlrhdl
036000040119     C                   movel     'M'           parmbr
036100040510     C                   eval      parccm = vlrksc
036200040119     C                   eval      paropz = '1'
036300040119     C* Effettuo la chiamata al CLLE preposto
036400040119     C                   call(e)   'TITVVTC'
036500040119     C                   parm                    parccm
036600040119     C                   parm                    parmbr
036700040119     C                   parm                    paropz
036800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
036900040119     C                   if        %error
037000040119     C                   movel     '1'           chkcall
037100040119     C                   else
037200040119     C                   movel     '0'           chkcall
037300040119     C                   endif
037400040119     C*
037500040119     C                   ENDSR
037600010601
037700010601
037800010601
037900010330     C*----------------------------------------------------*
038000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038100010330     C*----------------------------------------------------*
038200040119     C     INZVAR        BEGSR
038300010330     C*
038400030822     C* Inizializzo variabili di wrk
038500020204     C                   Z-ADD     *zeros        Num5_0            5 0
038600060331     C                   MOVEL     *blanks       wVATNOT_A
038700060331     C                   MOVEL     *blanks       wVATNOT_B
038800060331     C                   MOVEL     *blanks       wVATNOT_E
038900030822     C*
039000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
039100030822     C                   CLEAR                   FIVAB000
039200040119     C                   CLEAR                   FIVAT000
039300030822     C                   CLEAR                   SkSplitCSV
039400030822     C                   EVAL      i = 1
039500030822     C                   EVAL      posDa = *zeros
039600030822     C                   EVAL      posA  = *zeros
039700030822     C*
039800030822     C* Reimposto i valori di default
039900030822     C                   EXSR      DEFCAM
040000030822     C*
040100010330     C                   ENDSR
040200010330     C*----------------------------------------------------*
040300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
040400010330     C*----------------------------------------------------*
040500010330     C     DEFCAM        BEGSR
040600030822     C*
040700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
040800030715     C* e delimitatore testo.
040900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
041000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
041100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
041200030822     C*
041300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
041400030715     C                   IF        %subst(vlrppt:1:1) = 'S'
041500030715     C                   SETON                                        50
041600030715     C                   ELSE
041700030715     C                   SETOFF                                       50
041800030715     C                   ENDIF
041900030822     C*
042000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
042100030822     C                   EVAL      posDaDft = 1
042200030822     C                   EVAL      posADft  = 0
042300030822     C                   EVAL      wGiroDft = 0
042400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
042500030822     C                             posDaDft > 0
042600030822     C*
042700030822     C* Gestisco il 1° giro
042800030822     C                   IF        wGiroDft = 0
042900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
043000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
043100030822     C* Incremento il contatore dei "giri"
043200030822     C                   EVAL      wGiroDft = 1
043300030822     C                   ELSE
043400030822     C                   EVAL      posDaDft = posADft
043500030822     C                   ENDIF
043600030822     C* Eseguo lo scan x trovare la fine del campo corrente
043700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
043800030822     C*
043900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
044000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
044100030822     C                   IF        posDaDft > 0 AND
044200030822     C                             posADft  > 0
044300030822     C* NCL
044400030822     C                   IF        %subst(
044500030822     C                             %subst(vlrppt:posDaDft+1:
044600030822     C                             posADft-posDaDft-1):1:3)
044700030822     C                             = 'NCL'
044800030822     C                   EVAL      PiStr=%trim(%subst(
044900030822     C                             %subst(vlrppt:posDaDft+1:
045000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045100030822     C                   EXSR      CHKNUM
045200030822     C                   IF        PiInt=*on
045300030822     C                   Z-ADD     PiVal         VABNCL
045400030822     C                   ENDIF
045500030822     C                   ENDIF
045600030822     C* CCM
045700030822     C                   IF        %subst(
045800030822     C                             %subst(vlrppt:posDaDft+1:
045900030822     C                             posADft-posDaDft-1):1:3)
046000030822     C                             = 'CCM'
046100030822     C                   EVAL      PiStr=%trim(%subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046400030822     C                   EXSR      CHKNUM
046500030822     C                   IF        PiInt=*on
046600030822     C                   Z-ADD     PiVal         VABCCM
046700030822     C                   ENDIF
046800030822     C                   ENDIF
046900030822     C* LNP
047000030822     C                   IF        %subst(
047100030822     C                             %subst(vlrppt:posDaDft+1:
047200030822     C                             posADft-posDaDft-1):1:3)
047300030822     C                             = 'LNP'
047400030822     C                   EVAL      PiStr=%trim(%subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047700030822     C                   EXSR      CHKNUM
047800030822     C                   IF        PiInt=*on
047900030822     C                   Z-ADD     PiVal         VABLNP
048000030822     C                   ENDIF
048100030822     C                   ENDIF
048200030822     C* NRS
048300030822     C                   IF        %subst(
048400030822     C                             %subst(vlrppt:posDaDft+1:
048500030822     C                             posADft-posDaDft-1):1:3)
048600030822     C                             = 'NRS'
048700030822     C                   EVAL      PiStr=%trim(%subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049000030822     C                   EXSR      CHKNUM
049100030822     C                   IF        PiInt=*on
049200030822     C                   Z-ADD     PiVal         VABNRS
049300030822     C                   ENDIF
049400030822     C                   ENDIF
049500030822     C* CTR
049600030822     C                   IF        %subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):1:3)
049900030822     C                             = 'CTR'
050000030822     C                   EVAL      PiStr=%trim(%subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050300030822     C                   EXSR      CHKNUM
050400030822     C                   IF        PiInt=*on
050500030822     C                   Z-ADD     PiVal         VABCTR
050600030822     C                   ENDIF
050700030822     C                   ENDIF
050800030822     C* PKB
050900030822     C                   IF        %subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):1:3)
051200030822     C                             = 'PKB'
051300030822     C                   EVAL      PiStr=%trim(%subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051600030822     C                   EXSR      CHKNUM
051700030822     C                   IF        PiNum=*on
051800030822     C                   Z-ADD     PiVal         VABPKB
051900030822     C                   ENDIF
052000030822     C                   ENDIF
052100030822     C* VLB
052200030822     C                   IF        %subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):1:3)
052500030822     C                             = 'VLB'
052600030822     C                   EVAL      PiStr=%trim(%subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052900030822     C                   EXSR      CHKNUM
053000030822     C                   IF        PiNum=*on
053100030822     C                   Z-ADD     PiVal         VABVLB
053200030822     C                   ENDIF
053300030822     C                   ENDIF
053400030822     C* QFT
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'QFT'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiNum=*on
054400030822     C                   Z-ADD     PiVal         VABQFT
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* CBO
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'CBO'
055200030822     C                   EVAL      VABCBO=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   ENDIF
055600030822     C* TSP
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'TSP'
056100030822     C                   EVAL      VABTSP=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   ENDIF
056500030822     C* VAS
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'VAS'
057000030822     C                   EVAL      VABVAS=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   ENDIF
057400030822     C* VCA
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'VCA'
057900030822     C                   EVAL      VABVCA=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* GCA
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'GCA'
058800030822     C                   EVAL      VABGCA=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* CTM
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'CTM'
059700030822     C                   EVAL      VABCTM=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* FFD
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'FFD'
060600030822     C                   EVAL      VABFFD=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* VAD
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'VAD'
061500030822     C                   EVAL      VABVAD=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* GMA
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'GMA'
062400030822     C                   EVAL      VABGMA=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* GGA
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'GGA'
063300030822     C                   EVAL      VABGGA=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* GVA
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'GVA'
064200030822     C                   EVAL      VABGVA=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* TC1
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'TC1'
065100030822     C                   EVAL      VABTC1=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* TC2
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'TC2'
066000030822     C                   EVAL      VABTC2=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400040714     C* VATTRC
066500040714     C                   IF        %subst(
066600040714     C                             %subst(vlrppt:posDaDft+1:
066700040714     C                             posADft-posDaDft-1):1:3)
066800040714     C                             = 'TRC'
066900040714     C                   EVAL      VATTRC=%trim(%subst(
067000040714     C                             %subst(vlrppt:posDaDft+1:
067100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200040714     C                   ENDIF
067300030822     C* ...
067400030822     C                   ENDIF
067500030822     C                   ENDDO
067600020204     C*
067700010330     C                   ENDSR
067800010607     C*----------------------------------------------------*
067900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
068000010607     C*----------------------------------------------------*
068100010607     C     IMPVAB        BEGSR
068200010607     C*
068300010607     C                   SETOFF                                       3132
068400030515     C*
068500010607     C                   MOVEL     datcor        VABAAS
068600010607     C                   MOVE      datcor        VABMGS
068700010607     C*
068800030715     C**********
068900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069000030715     C**********
069100030715     C                   IF        wGiro = *zeros
069200040113     C*
069300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
069400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
069500040510     C                             %trim(intcol(3))<>*blanks
069600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
069700040113     C                                      %trim(intcol(3))
069800040510     C                   ENDIF
069900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
070000031201     C     minu:maiu     XLATE     vindta        vindta
070100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
070200030715     C                   DOW       posDa <= %len(%trim(vindta))
070300030715     C*
070400030715     C* Gestisco il 1° campo
070500030715     C                   IF        i = 1
070600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
070700030715     C                   EVAL      posDa = 1
070800030715     C* Eseguo lo scan x trovare la fine del primo campo
070900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071000030715     C* A questo punto "estraggo" il campo corrente
071100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
071200030715     C                                                    (posA-posDa))
071300030715     C* X i campi successivi al 1°
071400030715     C                   ELSE
071500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
071600030715     C                   EVAL      posDa = posA + 1
071700030715     C* Eseguo lo scan x trovare la fine del campo corrente
071800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
071900030715     C* Gestisco l'ultimo campo
072000030715     C                   IF        posA = *zeros
072100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
072200030715     C* A questo punto "estraggo" il campo corrente
072300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072400030715     C                                                    (posA-posDa))
072500030715     C                   LEAVE
072600030715     C                   ELSE
072700030715     C* A questo punto "estraggo" il campo corrente
072800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072900030715     C                                                    (posA-posDa))
073000030715     C                   ENDIF
073100030715     C                   ENDIF
073200030715     C* Incremento il contatore d campo
073300030715     C                   EVAL      i = i +1
073400030715     C                   ENDDO
073500030715     C*
073600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
073700030715     C* delimitazione testo
073800030715     C                   EVAL      i = 1
073900030715     C                   DOW       i < %elem(SkSplitFLD)
074000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
074100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
074200030715     C                   EVAL      i = i + 1
074300030715     C                   ENDDO
074400030715     C                   EVAL      wGiro = 1
074500030715     C                   ELSE
074600030715     C                   EVAL      wGiro = 2
074700031204     C**********
074800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
074900031204     C* d separatore campo
075000031204     C**********
075100031204     C                   EVAL      vindta = %trim(vindta)
075200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
075300031204     C                   EVAL      vindta = %subst(vindta:2)
075400031204     C                   ENDDO
075500031204     C*
075600031204     C                   Z-ADD     *zeros        lunghInput        4 0
075700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
075800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
075900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
076000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076100031204     C                   ENDDO
076200030715     C**********
076300030715     C* Effettuo lo split del campo dati d input
076400030715     C**********
076500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076600030715     C                   DOW       posDa <= %len(%trim(vindta))
076700030715     C*
076800030715     C* Gestisco il 1° campo
076900030715     C                   IF        i = 1
077000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077100030715     C                   EVAL      posDa = 1
077200030715     C* Eseguo lo scan x trovare la fine del primo campo
077300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C* X i campi successivi al 1°
077800030715     C                   ELSE
077900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078000030715     C                   EVAL      posDa = posA + 1
078100031204     C* Verifico che nn vi sia il campo nn valorizzato
078200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
078300031204     C* Se campo nn valorizzato skippo al prossimo
078400031204     C                   EVAL      SkSplitCSV(i) = *blanks
078500031204     C                   EVAL      posA  = posA + 1
078600031204     C                   ELSE
078700030715     C* Eseguo lo scan x trovare la fine del campo corrente
078800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078900030715     C* Gestisco l'ultimo campo
079000030715     C                   IF        posA = *zeros
079100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079200030715     C* A questo punto "estraggo" il campo corrente
079300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079400030715     C                                                    (posA-posDa))
079500030715     C                   LEAVE
079600030715     C                   ELSE
079700030715     C* A questo punto "estraggo" il campo corrente
079800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079900030715     C                                                    (posA-posDa))
080000030715     C                   ENDIF
080100031204     C                   ENDIF
080200030715     C                   ENDIF
080300030715     C* Incremento il contatore d campo
080400030715     C                   EVAL      i = i +1
080500030715     C                   ENDDO
080600030715     C*
080700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080800030715     C* delimitazione testo
080900030715     C                   EVAL      i = 1
081000030715     C                   DOW       i < %elem(SkSplitCSV)
081100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
081200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
081300030715     C                   EVAL      i = i + 1
081400030715     C                   ENDDO
081500030715     C*
081600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
081700030715     C                   EVAL      i = 1
081800030715     C                   DOW       i < %elem(SkSplitFLD)
081900030715     C*
082000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
082100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
082200030715     C                             SkSplitCSV(i) <> *blanks
082300030721     C*** ATB
082400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
082500030721     C                   EVAL      VABATB = SkSplitCSV(i)
082600030721     C                   ENDIF
082700030715     C*** RSD
082800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
082900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
083000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
083100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
083200030715     C* ==
083300030715     C                   ENDIF
083400030721     C*** RD2
083500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
083600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
083700030721     C                   ENDIF
083800030715     C*** IND
083900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
084000030715     C                   EVAL      VABIND = SkSplitCSV(i)
084100030715     C                   ENDIF
084200030715     C*** LOD
084300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
084400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
084500030715     C                   ENDIF
084600030715     C*** PRD
084700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
084800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
084900030715     C                   ENDIF
085000030721     C*** NZD
085100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
085200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
085300030721     C                   ENDIF
085400030715     C*** RMA
085500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
085600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
085700030715     C                   ENDIF
085800030715     C*** NOT
085900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
086000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
086100030715     C                   ENDIF
086200030715     C*** NT2
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
086400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
086500030715     C                   ENDIF
086600030715     C*** VCA
086700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
086800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
086900030715     C                   ENDIF
087000030715     C*** VAS
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
087200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030715     C*** FFD
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
087600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030721     C*** GC1
087900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
088000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
088100030721     C                   ENDIF
088200030721     C*** GC2
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
088400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030721     C*** TSP
088700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
088800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
088900030721     C                   ENDIF
089000040510     C*** CBO
089100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
089200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
089300040510     C                   ENDIF
089400030721     C*** NAS
089500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
089600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
089700030721     C                   ENDIF
089800030721     C*** TIC
089900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
090000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
090100030721     C                   ENDIF
090200030721     C*** GCA
090300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
090400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
090500030721     C                   ENDIF
090600030721     C*** XCO
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
090800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030721     C*** CTM
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
091200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030721     C*** TCR
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
091600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030721     C*** CTS
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
092000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
092100030721     C                   ENDIF
092200030721     C*** FTM
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
092400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030721     C*** VAD
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
092800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030721     C*** GMA
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
093200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030721     C*** GGA
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
093600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030721     C*** GVA
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
094000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030721     C*** TC1
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
094400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** TC2
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
094800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** SCL
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
095200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** RMO
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
095600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** NMO
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
096000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*
096300030721     C* Reperisco quindi i campi numerici...
096400040510     C*** NSP
096500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
096600040510     C                   EVAL      PiStr=SkSplitCSV(i)
096700040510     C                   EXSR      CHKNUM
096800040510     C                   IF        PiInt=*on
096900040510     C                   Z-ADD     PiVal         VABNSP
097000040510     C                   ELSE
097100040510     C                   SETON                                        31
097200040510     C                   EVAL      VABNSP = *zeros
097300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
097400040510     C                             + ' ' + 'VABNSP'
097500040510     C                   ENDIF
097600040510     C                   ENDIF
097700030721     C*** CAD
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
097900030721     C                   EVAL      PiStr=SkSplitCSV(i)
098000030721     C                   EXSR      CHKNUM
098100030721     C                   IF        PiInt=*on
098200030721     C                   Z-ADD     PiVal         Num5_0
098300030721     C                   MOVEL(p)  Num5_0        VABCAD
098400030721     C                   ELSE
098500030721     C                   SETON                                        32
098600030721     C                   EVAL      VABCAD = *zeros
098700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
098800030721     C                             + ' ' + 'VABCAD'
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
100100040122     C                             + ' ' + 'VABNCL'
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
104200040510     C                             + ' ' + 'VABCCM'
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
105400040715     C                   SETON                                        32
105500030721     C                   EVAL      VABLNP = *zeros
105600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105700040510     C                             + ' ' + 'VABLNP'
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
124100040510     C*
124200040510     C* Reperisco i campi del FIVAT
124300040510     C*** VATNOT
124400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
124500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
124600040510     C                   ENDIF
124700040510     C*** VATTRC
124800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
124900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
125000040510     C                   ENDIF
125100060331     C*** VATNOT_A
125200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
125300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
125400060331     C                   ENDIF
125500060331     C*** VATNOT_B
125600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
125700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
125800060331     C                   ENDIF
125900060331     C*** VATNOT_E
126000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
126100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
126200060331     C                   ENDIF
126300040510     C*
126400030715     C                   ENDIF
126500030715     C                   EVAL      i = i + 1
126600030715     C                   ENDDO
126700040510     C*
126800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
126900040510     C                   IF        VABNSP = *zeros
127000040510     C                   SETON                                        31
127100040510     C                   ENDIF
127200040715     C*
127300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127400040715     C                   IF        VABLNP = *zeros
127500040715     C                   SETON                                        31
127600040715     C                   ENDIF
127700040510     C*
127800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
127900040510     C                   IF        VABRMN = *zeros
128000040510     C                   Z-ADD     VABNSP        VABRMN
128100040510     C                   ENDIF
128200040510     C*
128300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
128400040510     C                   IF        VABRMA = *blanks
128500040714     C                   MOVEL     VABRMN        VABRMA
128600040510     C                   ENDIF
128700040128     C*
128800040510     C* Se provincia nn valorizzata la reperisco
128900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
129000040510     C                   IF        VABPRD = *blanks
129100040128     C                   CLEAR                   TISI95DS
129200040128     C                   EVAL      I95TCN = '3'
129300040128     C                   Z-ADD     datcor        I95DAT
129400040128     C                   EVAL      I95NAR = VABNZD
129500040128     C                   EVAL      I95CAP = VABCAD
129600040128     C                   EVAL      I95LOC = VABLOD
129700040128     C                   CALL      'TISI95R'
129800040128     C                   PARM                    TISI95DS
129900040128     C                   EVAL      VABPRD = O95PRV
130000040510     C                   ENDIF
130100040113     C*
130200040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
130300040113     C                   IF        VABCAS > *zeros
130400040119     C                   EVAL      VABVCA = 'EUR'
130500040119     C                   IF        VABCBO = '1'
130600040113     C                   EVAL      VABCBO = '4'
130700040119     C                   ENDIF
130800040119     C                   IF        VABCBO = '2'
130900040119     C                   EVAL      VABCBO = '6'
131000040119     C                   ENDIF
131100040113     C                   ENDIF
131200020204     C*
131300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
131400020204     C                   EXSR      CHKIMPDIV
131500030715     C                   ENDIF
131600020204     C*
131700010607     C                   ENDSR
131800010607     C*----------------------------------------------------*
131900010601
132000020204
132100020204     C*----------------------------------------------------*
132200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
132300020204     C*----------------------------------------------------*
132400020204     C     CHKIMPDIV     BEGSR
132500020204     C*
132600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
132700020204     C                   Z-ADD     *zeros        wrkDec            9 9
132800020204     C*
132900020204     C* Come prima cosa effettuo considerazioni sulla divisa
133000020204     C                   IF        vabIAS > *zeros
133100020204     C                   IF        vabVAS <> 'EUR'
133200020204     C                   EVAL      vabVAS =  'ITL'
133300020204     C                   ENDIF
133400020204     C                   ENDIF
133500020204     C*
133600020204     C                   IF        vabCAS > *zeros
133700020204     C                   IF        vabVCA <> 'EUR'
133800020204     C                   EVAL      vabVCA =  'ITL'
133900020204     C                   ENDIF
134000020204     C                   ENDIF
134100020204     C*
134200020204     C                   IF        vabVMD > *zeros
134300020204     C                   IF        vabVAD <> 'EUR'
134400020204     C                   EVAL      vabVAD =  'ITL'
134500020204     C                   ENDIF
134600020204     C                   ENDIF
134700020204     C*
134800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
134900020204     C                   Z-ADD     vabIAS        wrkDec
135000020204     C                   IF        wrkDec > *zeros
135100020204     C                   IF        vabVAS = 'ITL'
135200020204     C                   EVAL      vabIAS = *zeros
135300020204     C                   ENDIF
135400020204     C                   ENDIF
135500020204     C*
135600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
135700020204     C                   Z-ADD     vabCAS        wrkDec
135800020204     C                   IF        wrkDec > *zeros
135900020204     C                   IF        vabVCA = 'ITL'
136000020204     C                   EVAL      vabCAS = *zeros
136100020204     C                   ENDIF
136200020204     C                   ENDIF
136300020204     C*
136400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
136500020204     C                   Z-ADD     vabVMD        wrkDec
136600020204     C                   IF        wrkDec > *zeros
136700020204     C                   IF        vabVAD = 'ITL'
136800020204     C                   EVAL      vabVMD = *zeros
136900020204     C                   ENDIF
137000020204     C                   ENDIF
137100020204     C*
137200020204     C                   ENDSR
137300020204     C***
137400020204
137500010330
137600010330
137700010330     C*----------------------------------------------------*
137800010330     C*  CONTROLLO NUMERICITA' CAMPI
137900010330     C*----------------------------------------------------*
138000010330     C     CHKNUM        BEGSR
138100010330     C*
138200010606     C                   IF        PiDecChr = *blanks
138300030715     C                   EVAL      PiDecChr = CharNUM
138400010606     C                   ENDIF
138500010606     C*
138600010606     C                   CALL(e)   'ISNUMERIC'
138700010330     C                   PARM                    PiStr            30
138800010606     C                   PARM                    PiDecChr          1
138900010330     C                   PARM      *ZEROS        PiVal            30 9
139000010330     C                   PARM      '0'           PiInt             1
139100010330     C                   PARM      '0'           PiNum             1
139200010330     C                   IF        %error
139300010606     C                   EVAL      PiNum=*off
139400010330     C                   ENDIF
139500010330     C*
139600010330     C                   ENDSR
139700010330     C***
139800010330
139900010601
140000010601
140100010601
140200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
140300010601     C     repfil        BEGSR
140400010601     C*
140500010601     C                   if        invfil = *zeros and
140600010601     C                             depfil > *zeros and
140700010629     C                             (vinflg = *blanks or
140800010629     C                              vinflg = *zeros)
140900010601     C
141000010601     C                   eval      invfil = depfil
141100010601     C                   endif
141200010601     C*
141300010601     C                   if        depfil <> invfil and
141400010601     C                             invfil > *zeros
141500010601     C                   eval      flgMulti = '1'
141600010601     C                   if        vinflg = *blanks
141700010601     C                   add       1             cntNonEl
141800010601     C                   endif
141900010601     C                   endif
142000010601     C*
142100010601     C                   if        vinflg = '2'
142200010601     C                   eval      flgStato = '2'
142300010601     C                   endif
142400010601     C*
142500010601     C                   ENDSR
142600010601     C***
142700010601
142800010601
142900010601
143000010330
143100040119      /TITLE Invio dei dati al punto operativo.
143200040119     C     invio         BEGSR
143300040119     C*
143400040119     C* 1° invio FIVAT
143500040119     C                   reset                   dscmz
143600040119     C                   move      vlrpoi        cmzdst
143700040119     C                   eval      cmzfld = 'FIVATWWR'
143800040119     C                   eval      cmzmbd = vlrhdl
143900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
144000040119     C***                if        prmfir = *blanks
144100040119     C                   eval      cmzfla = 'FIVAT00F'
144200040119     C                   eval      cmzmba = 'FIVAT00F'
144300040119     C***                else
144400040119     C***                eval      cmzfla = prmfir
144500040119     C***                eval      cmzmba = prmfir
144600040119     C***                endif
144700040119     C                   eval      cmznrr = *zeros
144800040119     C                   move      §ctrokvt      cmznrr
144900040119     C                   eval      cmzlba = vlrfl1
145000040119     C                   call(e)   'TIS711C'
145100040119     C                   parm                    dscmz
145200040119     C                   parm      *blanks       esito
145300040119     C                   if        %error
145400040119     C                             or cmzerr = '1'
145500040119     C                             or esito  = '1'
145600040119     C                   eval      wrkesito = '3'
145700040119     C                   else
145800040119     C*
145900040119     C* 2° invio FIVAB
146000040119     C                   reset                   dscmz
146100040119     C                   move      vlrpoi        cmzdst
146200040119     C                   eval      cmzfld = vlrfou
146300040119     C                   eval      cmzmbd = vlrhdl
146400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
146500040119     C***                if        prmfir = *blanks
146600040119     C                   eval      cmzfla = 'FIVAB00F'
146700040119     C                   eval      cmzmba = 'FIVAB00F'
146800040119     C***                else
146900040119     C***                eval      cmzfla = prmfir
147000040119     C***                eval      cmzmba = prmfir
147100040119     C***                endif
147200040119     C                   eval      cmznrr = *zeros
147300040119     C                   move      §ctrokvb      cmznrr
147400040119     C                   eval      cmzlba = vlrfl1
147500040119     C                   call(e)   'TIS711C'
147600040119     C                   parm                    dscmz
147700040119     C                   parm      *blanks       esito
147800040119     C                   if        %error
147900040119     C                             or cmzerr = '1'
148000040119     C                             or esito  = '1'
148100040119     C                   eval      wrkesito = '3'
148200040119     C                   endif
148300040119     C                   endif
148400040119     C*
148500040119     C                   ENDSR
148600040119     C***
148700010601
148800010601
148900010601
149000010601
149100010601      /TITLE Invio dei dati al punto operativo.
149200010601     C     opeini        BEGSR
149300010601     C*
149400010601     C* Inizializzo flag e contatori operativi
149500010601     C                   movel     '0'           flgGiro           1
149600010601     C                   movel     '0'           flgMulti          1
149700010601     C                   movel     '1'           flgStato          1
149800010615     C                   movel     '0'           flgOk             1
149900010601     C                   z-add     *zeros        cntNonEl         10 0
150000010601     C                   z-add     *zeros        depfil            3 0
150100010601     C                   z-add     *zeros        invfil            3 0
150200010601     C*
150300010601     C                   ENDSR
150400010601     C***
150500070326
150600070326
150700070326
150800070326
150900070326     C     *pssr         BEGSR
151000070329     C*
151100070329     C                   if        %open(tivin00r)
151200070329     C                   close     tivin00r
151300070329     C                   endif
151400070329     C                   if        %open(fivabwwr)
151500070329     C                   close     fivabwwr
151600070329     C                   endif
151700070329     C                   if        %open(fivatwwr)
151800070329     C                   close     fivatwwr
151900070329     C                   endif
152000070326     C*
152100070326     C* Effettuo la chiamata al CLLE preposto
152200070326     C                   call(e)   'TITVVTC'
152300070326     C                   parm                    parccm
152400070326     C                   parm                    parmbr
152500070326     C                   parm      '2'           paropz
152600070326     C*
152700070326     C                   eval      wrkesito = '2'
152800070404     C*
152900070404     C                   seton                                        LR
153000070326     C*
153100070326     C                   ENDSR     '*CANCL'
153200070326     C***
153300070326
153400070326
153500010330
153600010330
153700000613     C     *inzsr        BEGSR
153800990910     C*
153900990910     C     *entry        plist
154000990920     C                   parm                    tivlrds
154100990921     C                   parm      wrkesito      esito
154200000724     C                   parm                    prmlit
154300000710     C                   parm                    prmfir
154400010330     C*
154500010330     C* CALCOLA LA DATA CORRENTE
154600010330     C                   time                    wn14             14 0
154700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
154800010330     C                   z-add     wn8           g08dat
154900010330     C                   z-add     *zeros        g08inv
155000010330     C                   movel     '0'           g08err
155100010330     C                   call      'XSRDA8'
155200010330     C                   parm                    wlbda8
155300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
155400000613     C*
155500000613     C                   ENDSR
155600000613     C***
155700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
155800070405VABCCM;VABNSP;VABCBO;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;VABNCL;
155900070405VABPKB;VABCAS;VABIAS;VABVAS;VATNOT_E;VABTIC;VABTC1;VABFFD;VABNOT
156000040113
