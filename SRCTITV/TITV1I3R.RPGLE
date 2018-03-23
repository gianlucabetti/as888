000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080418     H dftactgrp(*yes)
000300080804     F*
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
002500040510     D parccm          s              8    INZ(*blanks)
002600040119     D parmbr          s             10    INZ(*blanks)
002700040119     D paropz          s              1    INZ(*blanks)
002800040119     D chkcall         s              1    INZ(*blanks)
002900080418     D wFlgCAS         s              1    INZ(*blanks)
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
004200070823     D wVATNOT_A       s                   LIKE(VATNOT)
004300060331     D wVATNOT_B       s                   LIKE(VATNOT)
004400060331     D wVATNOT_E       s                   LIKE(VATNOT)
004500030822     D*------------------
004600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004700030822     D*------------------
004800030822     D posDaDft        S              3  0 INZ(*zeros)
004900030822     D posADft         S              3  0 INZ(*zeros)
005000030822     D j               s              3  0 INZ(1)
005100030822     D wGiroDft        s              1  0 INZ(*zeros)
005200010330     D*------------------
005300010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005400010330     D*------------------
005500010330     D WLBDA8          DS                  INZ
005600010330     D  G08DAT                 1      8  0
005700010330     D  G08INV                 9     16  0
005800010330     D  G08ERR                17     17
005900010330     D  G08TGI                18     22  0
006000071029     D*------------------
006100080418     D* Costanti
006200080418     D*------------------
006300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006500080418     D*------------------
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
014700070823     C*
014800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015000110902     C                   exsr      inzvar
015100110902     C                   exsr      impvab
015200010330     C*
015300030822     C                   do        *HIVAL
015400010330     C*
015500030822     C                   read      tivin00r                               70
015600010618     C*
015700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015800010618     C                   if        vinflg = '1'
015900010618     C                   eval      flgOk = '1'
016000010618     C                   endif
016100040510     C*
016200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016300040510     C                   z-add     *zeros        wLenVINDTA        4 0
016400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
016600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016800040510     C                   z-add     1             wFlgVINDTA
016900040510     C                   leave
017000040510     C                   endif
017100040510     C                   enddo
017200010618     C*
017300040510     C                   if        vindta > *blanks AND
017400040510     C                             wFlgVINDTA = 1
017500010330     C                   add       1             rrnum
017600010330     C*
017700010601     C                   if        *in70 = *off and
017800010330     C                             (vinflg = *blanks
017900010330     C                              or vinflg = '0'
018000010330     C                              or vinflg = '2')
018100010330     C*
018200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018300010711     C                   if        vinflg = *blanks or vinflg = '0'
018400010711     C                   clear                   vinmsg
018500010711     C                   endif
018600010601     C*
018700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
018800070925     C                   eval      vindta  = ' ' + %trim(vindta)
018900070925     C*
019000010330     C                   exsr      inzvar
019100010605     C                   exsr      impvab
019200010601     C*
019300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019400010605     C                   eval      depfil = VABLNP
019500010601     C                   exsr      repfil
019600010601     C                   if        depfil = invfil
019700021025     C                   if        vlrpoi = 999
019800030822     C                   move(P)   invfil        VABFGS
019900021025     C                   else
020000030822     C                   move(P)   vlrpoi        VABFGS
020100021025     C                   endif
020200010601     C*
020300010601     C                   exsr      PREELA
020400010601     C*
020500010604     C* Ebbene...
020600010604     C*
020700040119     C  N31              add       1             §CTROKVB
020800040119     C   32              add       1             §CTRMO
020900040119     C   31              add       1             §CTRNO
021000080418     C*
021100030822     C                   if        wGiro = 2
021200040510     C  N31              exsr      wrivat
021300040119     C  N31              exsr      wrivab
021400040119     C                   endif
021500010604     C*
021600010604     C                   if        *in31 = *off and
021700010604     C                             *in32 = *off
021800010604     C                   eval      vinflg = '1'
021900010604     C                   else
022000010604     C                   eval      vinflg = '2'
022100010604     C                   endif
022200010604     C                   endif
022300010601     C*
022400010604     C                   endif
022500010604     C*
022600010330     C                   else
022700010330     C                   eval      vinflg = '1'
022800010330     C                   endif
022900010601     C*
023000010601     C  N70              update    tivin000
023100010330     C*
023200030822     C  N70              enddo
023300040119     C*
023400040119     C                   endif                                                  (endif - chkcall)
023500010601     C*
023600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
023700010601     C                   if        cntNonEl = *zeros or
023800010601     C                             flgMulti = '0'
023900010330     C* Se non ci sono record con errori ...
024000010601     C                   if        §ctrno = 0 and
024100010604     C                             §ctrmo = 0 and
024200010601     C                             flgStato <> '2'
024300010330     C* ... restituisco esito OK.
024400010330     C                   eval      wrkesito = '0'
024500010330     C                   else
024600040119     C                   if        §ctrokvb > 0
024700010330     C                   eval      wrkesito = '1'
024800010330     C                   else
024900010615     C                   if        flgOk = '0'
025000010615     C                   eval      wrkesito = '2'
025100010615     C                   else
025200010615     C                   eval      wrkesito = '6'
025300010615     C                   endif
025400010330     C                   endif
025500010330     C                   endif
025600010601     C                   else
025700010601     C                   eval      wrkesito = '9'
025800010601     C                   endif
025900010330     C*
026000010330     C                   if        %open(tivin00r)
026100010330     C                   close     tivin00r
026200010330     C                   endif
026300021025     C                   if        %open(fivabwwr)
026400021025     C                   close     fivabwwr
026500010330     C                   endif
026600040512     C                   if        %open(fivatwwr)
026700040512     C                   close     fivatwwr
026800040512     C                   endif
026900010601     C*
027000010601     C                   if        vlrpoi <> 999
027100010601     C                   eval      invfil = vlrpoi
027200010601     C                   endif
027300010330     C*
027400040119     C                   if        §ctrokvb > 0
027500010601     C                             and invfil > *zeros
027600010330     C                   exsr      invio
027700010330     C                   endif
027800010601     C*
027900010618     C                   if        flgGiro = '1'
028000010601     C                   exsr      endela
028100010618     C                   endif
028200010330     C*
028300010330     C                   ENDSR
028400010330     C***
028500040119
028600040119
028700040119
028800040119     C*----------------------------------------------------*
028900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
029000040119     C*----------------------------------------------------*
029100040510     C     WRIVAB        BEGSR
029200040119     C*
029300070301     C                   if        wVATNOT_E <> *blanks
029400070301     C                   eval      VABCTM = '7Q'
029500070301     C                   endif
029600040119     C                   write     fivab000                                     => scarico il VAB
029700040119     C*
029800040119     C                   ENDSR
029900040119
030000040119
030100040119
030200040119     C*----------------------------------------------------*
030300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
030400040119     C*----------------------------------------------------*
030500040510     C     WRIVAT        BEGSR
030600060512     C*
030700060512     C* Inizializzo i campi chiave
030800060512     C                   eval      VATFGS = VABFGS
030900060512     C                   eval      VATAAS = VABAAS
031000060512     C                   eval      VATCCM = VABCCM
031100070823     C                   eval      VATNRS = VABNRS
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
058300070925     C* TIC
058400070925     C                   IF        %subst(
058500070925     C                             %subst(vlrppt:posDaDft+1:
058600070925     C                             posADft-posDaDft-1):1:3)
058700070925     C                             = 'TIC'
058800070925     C                   EVAL      VABTIC=%trim(%subst(
058900070925     C                             %subst(vlrppt:posDaDft+1:
059000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100070925     C                   ENDIF
059200030822     C* GCA
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'GCA'
059700030822     C                   EVAL      VABGCA=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* CTM
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CTM'
060600030822     C                   EVAL      VABCTM=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* FFD
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'FFD'
061500030822     C                   EVAL      VABFFD=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* VAD
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'VAD'
062400030822     C                   EVAL      VABVAD=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* GMA
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'GMA'
063300030822     C                   EVAL      VABGMA=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* GGA
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'GGA'
064200030822     C                   EVAL      VABGGA=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* GVA
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'GVA'
065100030822     C                   EVAL      VABGVA=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* TC1
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'TC1'
066000030822     C                   EVAL      VABTC1=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* TC2
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'TC2'
066900030822     C                   EVAL      VABTC2=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300040714     C* VATTRC
067400040714     C                   IF        %subst(
067500040714     C                             %subst(vlrppt:posDaDft+1:
067600040714     C                             posADft-posDaDft-1):1:3)
067700040714     C                             = 'TRC'
067800040714     C                   EVAL      VATTRC=%trim(%subst(
067900040714     C                             %subst(vlrppt:posDaDft+1:
068000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100040714     C                   ENDIF
068200030822     C* ...
068300030822     C                   ENDIF
068400030822     C                   ENDDO
068500020204     C*
068600010330     C                   ENDSR
068700010607     C*----------------------------------------------------*
068800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
068900010607     C*----------------------------------------------------*
069000010607     C     IMPVAB        BEGSR
069100010607     C*
069200010607     C                   SETOFF                                       3132
069300030515     C*
069400010607     C                   MOVEL     datcor        VABAAS
069500010607     C                   MOVE      datcor        VABMGS
069600010607     C*
069700030715     C**********
069800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
069900030715     C**********
070000030715     C                   IF        wGiro = *zeros
070100040113     C*
070200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
070300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
070400040510     C                             %trim(intcol(3))<>*blanks
070500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
070600040113     C                                      %trim(intcol(3))
070700070823     C     ';':CharCSV   XLATE     vindta        vindta
070800040510     C                   ENDIF
070900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
071000031201     C     minu:maiu     XLATE     vindta        vindta
071100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
071200030715     C                   DOW       posDa <= %len(%trim(vindta))
071300030715     C*
071400030715     C* Gestisco il 1° campo
071500030715     C                   IF        i = 1
071600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
071700030715     C                   EVAL      posDa = 1
071800030715     C* Eseguo lo scan x trovare la fine del primo campo
071900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072000030715     C* A questo punto "estraggo" il campo corrente
072100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
072200030715     C                                                    (posA-posDa))
072300030715     C* X i campi successivi al 1°
072400030715     C                   ELSE
072500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
072600030715     C                   EVAL      posDa = posA + 1
072700030715     C* Eseguo lo scan x trovare la fine del campo corrente
072800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072900030715     C* Gestisco l'ultimo campo
073000030715     C                   IF        posA = *zeros
073100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
073200030715     C* A questo punto "estraggo" il campo corrente
073300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073400030715     C                                                    (posA-posDa))
073500030715     C                   LEAVE
073600030715     C                   ELSE
073700030715     C* A questo punto "estraggo" il campo corrente
073800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073900030715     C                                                    (posA-posDa))
074000030715     C                   ENDIF
074100030715     C                   ENDIF
074200030715     C* Incremento il contatore d campo
074300030715     C                   EVAL      i = i +1
074400030715     C                   ENDDO
074500030715     C*
074600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
074700030715     C* delimitazione testo
074800030715     C                   EVAL      i = 1
074900030715     C                   DOW       i < %elem(SkSplitFLD)
075000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
075100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
075200030715     C                   EVAL      i = i + 1
075300030715     C                   ENDDO
075400030715     C                   EVAL      wGiro = 1
075500030715     C                   ELSE
075600030715     C                   EVAL      wGiro = 2
075700031204     C**********
075800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
075900031204     C* d separatore campo
076000031204     C**********
076100031204     C                   EVAL      vindta = %trim(vindta)
076200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
076300031204     C                   EVAL      vindta = %subst(vindta:2)
076400031204     C                   ENDDO
076500031204     C*
076600031204     C                   Z-ADD     *zeros        lunghInput        4 0
076700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
076800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
076900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
077000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077100031204     C                   ENDDO
077200030715     C**********
077300030715     C* Effettuo lo split del campo dati d input
077400030715     C**********
077500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077600030715     C                   DOW       posDa <= %len(%trim(vindta))
077700030715     C*
077800030715     C* Gestisco il 1° campo
077900030715     C                   IF        i = 1
078000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078100030715     C                   EVAL      posDa = 1
078200030715     C* Eseguo lo scan x trovare la fine del primo campo
078300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078400030715     C* A questo punto "estraggo" il campo corrente
078500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
078600030715     C                                                    (posA-posDa))
078700030715     C* X i campi successivi al 1°
078800030715     C                   ELSE
078900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079000030715     C                   EVAL      posDa = posA + 1
079100031204     C* Verifico che nn vi sia il campo nn valorizzato
079200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
079300031204     C* Se campo nn valorizzato skippo al prossimo
079400031204     C                   EVAL      SkSplitCSV(i) = *blanks
079500031204     C                   EVAL      posA  = posA + 1
079600031204     C                   ELSE
079700030715     C* Eseguo lo scan x trovare la fine del campo corrente
079800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079900030715     C* Gestisco l'ultimo campo
080000030715     C                   IF        posA = *zeros
080100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080200030715     C* A questo punto "estraggo" il campo corrente
080300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080400030715     C                                                    (posA-posDa))
080500030715     C                   LEAVE
080600030715     C                   ELSE
080700030715     C* A questo punto "estraggo" il campo corrente
080800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080900030715     C                                                    (posA-posDa))
081000030715     C                   ENDIF
081100031204     C                   ENDIF
081200030715     C                   ENDIF
081300030715     C* Incremento il contatore d campo
081400030715     C                   EVAL      i = i +1
081500030715     C                   ENDDO
081600030715     C*
081700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081800030715     C* delimitazione testo
081900030715     C                   EVAL      i = 1
082000030715     C                   DOW       i < %elem(SkSplitCSV)
082100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
082200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
082300030715     C                   EVAL      i = i + 1
082400030715     C                   ENDDO
082500030715     C*
082600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
082700030715     C                   EVAL      i = 1
082800030715     C                   DOW       i < %elem(SkSplitFLD)
082900030715     C*
083000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
083100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
083200030715     C                             SkSplitCSV(i) <> *blanks
083300030721     C*** ATB
083400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
083500030721     C                   EVAL      VABATB = SkSplitCSV(i)
083600030721     C                   ENDIF
083700030715     C*** RSD
083800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
083900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
084000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
084100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
084200030715     C* ==
084300030715     C                   ENDIF
084400030721     C*** RD2
084500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
084600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
084700030721     C                   ENDIF
084800030715     C*** IND
084900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
085000030715     C                   EVAL      VABIND = SkSplitCSV(i)
085100030715     C                   ENDIF
085200030715     C*** LOD
085300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
085400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
085500030715     C                   ENDIF
085600030715     C*** PRD
085700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
085800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
085900030715     C                   ENDIF
086000030721     C*** NZD
086100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
086200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
086300030721     C                   ENDIF
086400030715     C*** RMA
086500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
086600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
086700030715     C                   ENDIF
086800030715     C*** NOT
086900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
087000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
087100030715     C                   ENDIF
087200030715     C*** NT2
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
087400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
087500030715     C                   ENDIF
087600030715     C*** VCA
087700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
087800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
087900030715     C                   ENDIF
088000030715     C*** VAS
088100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
088200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
088300030715     C                   ENDIF
088400030715     C*** FFD
088500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
088600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
088700030715     C                   ENDIF
088800030721     C*** GC1
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
089000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030721     C*** GC2
089300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
089400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
089500030721     C                   ENDIF
089600030721     C*** TSP
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
089800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
089900030721     C                   ENDIF
090000040510     C*** CBO
090100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
090200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
090300040510     C                   ENDIF
090400030721     C*** NAS
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
090600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030721     C*** TIC
090900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
091000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
091100030721     C                   ENDIF
091200030721     C*** GCA
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
091400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** XCO
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
091800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** CTM
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
092200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** TCR
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
092600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** CTS
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
093000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** FTM
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
093400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** VAD
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
093800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** GMA
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
094200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** GGA
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
094600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** GVA
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
095000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** TC1
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
095400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** TC2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
095800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** SCL
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
096200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** RMO
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
096600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** NMO
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
097000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*
097300030721     C* Reperisco quindi i campi numerici...
097400040510     C*** NSP
097500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
097600040510     C                   EVAL      PiStr=SkSplitCSV(i)
097700040510     C                   EXSR      CHKNUM
097800040510     C                   IF        PiInt=*on
097900040510     C                   Z-ADD     PiVal         VABNSP
098000040510     C                   ELSE
098100040510     C                   SETON                                        31
098200040510     C                   EVAL      VABNSP = *zeros
098300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
098400040510     C                             + ' ' + 'VABNSP'
098500040510     C                   ENDIF
098600040510     C                   ENDIF
098700071106     C*** RMN
098800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
098900071106     C                   EVAL      PiStr=SkSplitCSV(i)
099000071106     C                   EXSR      CHKNUM
099100071106     C                   IF        PiInt=*on
099200071106     C                   Z-ADD     PiVal         VABRMN
099300071106     C                   ELSE
099400071106     C                   SETON                                        32
099500071106     C                   EVAL      VABRMN = *zeros
099600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
099700071106     C                             + ' ' + 'VABRMN'
099800071106     C                   ENDIF
099900071106     C                   ENDIF
100000030721     C*** CAD
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
100200030721     C                   EVAL      PiStr=SkSplitCSV(i)
100300030721     C                   EXSR      CHKNUM
100400030721     C                   IF        PiInt=*on
100500030721     C                   Z-ADD     PiVal         Num5_0
100600030721     C                   MOVEL(p)  Num5_0        VABCAD
100700030721     C                   ELSE
100800030721     C                   SETON                                        32
100900030721     C                   EVAL      VABCAD = *zeros
101000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101100030721     C                             + ' ' + 'VABCAD'
101200030721     C                   ENDIF
101300030721     C                   ENDIF
101400030721     C*** NCL
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
101600030721     C                   EVAL      PiStr=SkSplitCSV(i)
101700030721     C                   EXSR      CHKNUM
101800030721     C                   IF        PiInt=*on
101900030721     C                   Z-ADD     PiVal         VABNCL
102000030721     C                   ELSE
102100030721     C                   SETON                                        32
102200030721     C                   EVAL      VABNCL = *zeros
102300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
102400040122     C                             + ' ' + 'VABNCL'
102500030721     C                   ENDIF
102600030721     C                   ENDIF
102700030721     C*** PKB
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
102900030721     C                   EVAL      PiStr=SkSplitCSV(i)
103000030721     C                   EXSR      CHKNUM
103100030721     C                   IF        PiNum=*on
103200030721     C                   Z-ADD     PiVal         VABPKB
103300030721     C                   ELSE
103400030721     C                   SETON                                        32
103500030721     C                   EVAL      VABPKB = *zeros
103600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103700030721     C                             + ' ' + 'VABPKB'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000030721     C*** CAS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiNum=*on
104500030721     C                   Z-ADD     PiVal         VABCAS
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABCAS = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABCAS'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** CCM
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiInt=*on      AND
105800030721     C                             PiVal<=9999999 AND
105900030721     C                             PiVal>*zeros
106000030721     C                   Z-ADD     PiVal         VABCCM
106100030721     C                   ELSE
106200030721     C                   SETON                                        32
106300030721     C                   EVAL      VABCCM = *zeros
106400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040510     C                             + ' ' + 'VABCCM'
106600030721     C                   ENDIF
106700030721     C                   ENDIF
106800030721     C*** LNP
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107000030721     C                   EVAL      PiStr=SkSplitCSV(i)
107100030721     C                   EXSR      CHKNUM
107200030721     C                   IF        PiInt=*on    AND
107300030721     C                             PiVal<=999   AND
107400030721     C                             PiVal>*zeros
107500030721     C                   Z-ADD     PiVal         VABLNP
107600030721     C                   ELSE
107700040715     C                   SETON                                        32
107800030721     C                   EVAL      VABLNP = *zeros
107900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108000040510     C                             + ' ' + 'VABLNP'
108100030721     C                   ENDIF
108200030721     C                   ENDIF
108300030721     C*** NRS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
108500030721     C                   EVAL      PiStr=SkSplitCSV(i)
108600030721     C                   EXSR      CHKNUM
108700030721     C                   IF        PiInt=*on AND
108800030721     C                             PiVal<=99
108900030721     C                   Z-ADD     PiVal         VABNRS
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109300030721     C                             + ' ' + 'VABNRS'
109400030721     C                   ENDIF
109500030721     C                   ENDIF
109600030721     C*** LNA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
109800030721     C                   EVAL      PiStr=SkSplitCSV(i)
109900030721     C                   EXSR      CHKNUM
110000030721     C                   IF        PiInt=*on    AND
110100030721     C                             PiVal<=999   AND
110200030721     C                             PiVal>*zeros
110300030721     C                   Z-ADD     PiVal         VABLNA
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      VABLNA = *zeros
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800030721     C                             + ' ' + 'VABLNA'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** CTR
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiInt=*on  AND
111600030721     C                             PiVal<=999
111700030721     C                   Z-ADD     PiVal         VABCTR
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCTR'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** IAS
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiNum=*on
112900030721     C                   Z-ADD     PiVal         VABIAS
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300030721     C                             + ' ' + 'VABIAS'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** VLB
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiNum=*on
114100030721     C                   Z-ADD     PiVal         VABVLB
114200030721     C                   ELSE
114300030721     C                   SETON                                        32
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500030721     C                             + ' ' + 'VABVLB'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** QFT
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiNum=*on
115300030721     C                   Z-ADD     PiVal         VABQFT
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABQFT'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** NCD
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on      AND
116500030721     C                             PiVal<=9999999
116600030721     C                   Z-ADD     PiVal         VABNCD
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABNCD'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** NCA
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on      AND
117800030721     C                             PiVal<=9999999
117900030721     C                   Z-ADD     PiVal         VABNCA
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABNCA'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** ZNC
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on AND
119100030721     C                             PiVal<=99
119200030721     C                   Z-ADD     PiVal         VABZNC
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABZNC'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** DCR
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on       AND
120400030721     C                             PiVal<=99999999
120500030721     C                   Z-ADD     PiVal         VABDCR
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABDCR'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** HCR
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on   AND
121700030721     C                             PiVal<=9999
121800030721     C                   Z-ADD     PiVal         VABHCR
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABHCR'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** VMD
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiNum=*on
123000030721     C                   Z-ADD     PiVal         VABVMD
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABVMD'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** ANT
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on        AND
124200030721     C                             PiVal<=999999999
124300030721     C                   Z-ADD     PiVal         VABANT
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABANT'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** CMO
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on
125500030721     C                   Z-ADD     PiVal         Num5_0
125600030721     C                   MOVEL(p)  Num5_0        VABCMO
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      VABCMO = *zeros
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABCMO'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400040510     C*
126500040510     C* Reperisco i campi del FIVAT
126600040510     C*** VATNOT
126700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
126800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
126900040510     C                   ENDIF
127000040510     C*** VATTRC
127100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
127200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
127300040510     C                   ENDIF
127400060331     C*** VATNOT_A
127500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
127600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
127700060331     C                   ENDIF
127800060331     C*** VATNOT_B
127900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
128000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
128100060331     C                   ENDIF
128200060331     C*** VATNOT_E
128300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
128400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
128500060331     C                   ENDIF
128600080804     C*** RMN/NSP
128700080804     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
128800091020     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4)
128900080804     C                   EXSR      CHKNUM
129000080804     C                   IF        PiInt=*on
129100080804     C                   Z-ADD     PiVal         VABRMN
129200080804     C                   Z-ADD     PiVal         VABNSP
129300080804     C                   ELSE
129400080804     C                   SETON                                        31
129500080804     C                   EVAL      VABNSP = *zeros
129600080804     C                   EVAL      VABRMN = *zeros
129700080804     C                   EVAL      vinmsg = %trimr(vinmsg)
129800080804     C                             + ' ' + 'VABNSP VABRMN'
129900080804     C                   ENDIF
130000080804     C                   ENDIF
130100070925     C*
130200071029     C***  <===  -----------------------------------------
130300040510     C*
130400030715     C                   ENDIF
130500030715     C                   EVAL      i = i + 1
130600030715     C                   ENDDO
130700070925     C*
130800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
130900070925     C*
131000071029     C***  <===  -----------------------------------------
131100070925     C*
131200070925     C* Considerazioni finali su CBO/CAS
131300080418     C                   IF        wFlgCAS = '1'
131400070925     C                   IF        VABCBO = '1'
131500070925     C                   EVAL      VABCBO = '4'
131600070925     C                   ENDIF
131700070925     C                   IF        VABCBO = '2'
131800070925     C                   EVAL      VABCBO = '6'
131900070925     C                   ENDIF
132000070925     C                   ENDIF
132100040510     C*
132200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
132300040510     C                   IF        VABNSP = *zeros
132400040510     C                   SETON                                        31
132500040510     C                   ENDIF
132600040715     C*
132700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
132800040715     C                   IF        VABLNP = *zeros
132900040715     C                   SETON                                        31
133000040715     C                   ENDIF
133100040510     C*
133200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
133300040510     C                   IF        VABRMN = *zeros
133400040510     C                   Z-ADD     VABNSP        VABRMN
133500040510     C                   ENDIF
133600040510     C*
133700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
133800040510     C                   IF        VABRMA = *blanks
133900040714     C                   MOVEL     VABRMN        VABRMA
134000040510     C                   ENDIF
134100040128     C*
134200040510     C* Se provincia nn valorizzata la reperisco
134300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
134400040510     C                   IF        VABPRD = *blanks
134500040128     C                   CLEAR                   TISI95DS
134600040128     C                   EVAL      I95TCN = '3'
134700040128     C                   Z-ADD     datcor        I95DAT
134800040128     C                   EVAL      I95NAR = VABNZD
134900040128     C                   EVAL      I95CAP = VABCAD
135000040128     C                   EVAL      I95LOC = VABLOD
135100040128     C                   CALL      'TISI95R'
135200040128     C                   PARM                    TISI95DS
135300040128     C                   EVAL      VABPRD = O95PRV
135400040510     C                   ENDIF
135500040113     C*
135600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
135700040113     C                   IF        VABCAS > *zeros
135800040119     C                   EVAL      VABVCA = 'EUR'
135900040119     C                   IF        VABCBO = '1'
136000040113     C                   EVAL      VABCBO = '4'
136100040119     C                   ENDIF
136200040119     C                   IF        VABCBO = '2'
136300040119     C                   EVAL      VABCBO = '6'
136400040119     C                   ENDIF
136500040113     C                   ENDIF
136600020204     C*
136700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
136800020204     C                   EXSR      CHKIMPDIV
136900030715     C                   ENDIF
137000020204     C*
137100010607     C                   ENDSR
137200010607     C*----------------------------------------------------*
137300010601
137400020204
137500020204     C*----------------------------------------------------*
137600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
137700020204     C*----------------------------------------------------*
137800020204     C     CHKIMPDIV     BEGSR
137900020204     C*
138000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138100020204     C                   Z-ADD     *zeros        wrkDec            9 9
138200020204     C*
138300020204     C* Come prima cosa effettuo considerazioni sulla divisa
138400020204     C                   IF        vabIAS > *zeros
138500020204     C                   IF        vabVAS <> 'EUR'
138600020204     C                   EVAL      vabVAS =  'ITL'
138700020204     C                   ENDIF
138800020204     C                   ENDIF
138900020204     C*
139000020204     C                   IF        vabCAS > *zeros
139100020204     C                   IF        vabVCA <> 'EUR'
139200020204     C                   EVAL      vabVCA =  'ITL'
139300020204     C                   ENDIF
139400020204     C                   ENDIF
139500020204     C*
139600020204     C                   IF        vabVMD > *zeros
139700020204     C                   IF        vabVAD <> 'EUR'
139800020204     C                   EVAL      vabVAD =  'ITL'
139900020204     C                   ENDIF
140000020204     C                   ENDIF
140100020204     C*
140200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
140300020204     C                   Z-ADD     vabIAS        wrkDec
140400020204     C                   IF        wrkDec > *zeros
140500020204     C                   IF        vabVAS = 'ITL'
140600020204     C                   EVAL      vabIAS = *zeros
140700020204     C                   ENDIF
140800020204     C                   ENDIF
140900020204     C*
141000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141100020204     C                   Z-ADD     vabCAS        wrkDec
141200020204     C                   IF        wrkDec > *zeros
141300020204     C                   IF        vabVCA = 'ITL'
141400020204     C                   EVAL      vabCAS = *zeros
141500020204     C                   ENDIF
141600020204     C                   ENDIF
141700020204     C*
141800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
141900020204     C                   Z-ADD     vabVMD        wrkDec
142000020204     C                   IF        wrkDec > *zeros
142100020204     C                   IF        vabVAD = 'ITL'
142200020204     C                   EVAL      vabVMD = *zeros
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C                   ENDSR
142700020204     C***
142800020204
142900010330
143000010330
143100010330     C*----------------------------------------------------*
143200010330     C*  CONTROLLO NUMERICITA' CAMPI
143300010330     C*----------------------------------------------------*
143400010330     C     CHKNUM        BEGSR
143500010330     C*
143600010606     C                   IF        PiDecChr = *blanks
143700030715     C                   EVAL      PiDecChr = CharNUM
143800010606     C                   ENDIF
143900010606     C*
144000010606     C                   CALL(e)   'ISNUMERIC'
144100010330     C                   PARM                    PiStr            30
144200010606     C                   PARM                    PiDecChr          1
144300010330     C                   PARM      *ZEROS        PiVal            30 9
144400010330     C                   PARM      '0'           PiInt             1
144500010330     C                   PARM      '0'           PiNum             1
144600010330     C                   IF        %error
144700010606     C                   EVAL      PiNum=*off
144800010330     C                   ENDIF
144900010330     C*
145000010330     C                   ENDSR
145100010330     C***
145200010330
145300010601
145400010601
145500010601
145600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
145700010601     C     repfil        BEGSR
145800010601     C*
145900010601     C                   if        invfil = *zeros and
146000010601     C                             depfil > *zeros and
146100010629     C                             (vinflg = *blanks or
146200010629     C                              vinflg = *zeros)
146300010601     C
146400010601     C                   eval      invfil = depfil
146500010601     C                   endif
146600010601     C*
146700010601     C                   if        depfil <> invfil and
146800010601     C                             invfil > *zeros
146900010601     C                   eval      flgMulti = '1'
147000010601     C                   if        vinflg = *blanks
147100010601     C                   add       1             cntNonEl
147200010601     C                   endif
147300010601     C                   endif
147400010601     C*
147500010601     C                   if        vinflg = '2'
147600010601     C                   eval      flgStato = '2'
147700010601     C                   endif
147800010601     C*
147900010601     C                   ENDSR
148000010601     C***
148100010601
148200010601
148300010601
148400010330
148500040119      /TITLE Invio dei dati al punto operativo.
148600040119     C     invio         BEGSR
148700040119     C*
148800040119     C* 1° invio FIVAT
148900040119     C                   reset                   dscmz
149000040119     C                   move      vlrpoi        cmzdst
149100040119     C                   eval      cmzfld = 'FIVATWWR'
149200040119     C                   eval      cmzmbd = vlrhdl
149300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
149400040119     C***                if        prmfir = *blanks
149500040119     C                   eval      cmzfla = 'FIVAT00F'
149600040119     C                   eval      cmzmba = 'FIVAT00F'
149700040119     C***                else
149800040119     C***                eval      cmzfla = prmfir
149900040119     C***                eval      cmzmba = prmfir
150000040119     C***                endif
150100040119     C                   eval      cmznrr = *zeros
150200040119     C                   move      §ctrokvt      cmznrr
150300040119     C                   eval      cmzlba = vlrfl1
150400040119     C                   call(e)   'TIS711C'
150500040119     C                   parm                    dscmz
150600040119     C                   parm      *blanks       esito
150700040119     C                   if        %error
150800040119     C                             or cmzerr = '1'
150900040119     C                             or esito  = '1'
151000040119     C                   eval      wrkesito = '3'
151100040119     C                   else
151200040119     C*
151300040119     C* 2° invio FIVAB
151400040119     C                   reset                   dscmz
151500040119     C                   move      vlrpoi        cmzdst
151600040119     C                   eval      cmzfld = vlrfou
151700040119     C                   eval      cmzmbd = vlrhdl
151800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
151900040119     C***                if        prmfir = *blanks
152000040119     C                   eval      cmzfla = 'FIVAB00F'
152100040119     C                   eval      cmzmba = 'FIVAB00F'
152200040119     C***                else
152300040119     C***                eval      cmzfla = prmfir
152400040119     C***                eval      cmzmba = prmfir
152500040119     C***                endif
152600040119     C                   eval      cmznrr = *zeros
152700040119     C                   move      §ctrokvb      cmznrr
152800040119     C                   eval      cmzlba = vlrfl1
152900040119     C                   call(e)   'TIS711C'
153000040119     C                   parm                    dscmz
153100040119     C                   parm      *blanks       esito
153200040119     C                   if        %error
153300040119     C                             or cmzerr = '1'
153400040119     C                             or esito  = '1'
153500040119     C                   eval      wrkesito = '3'
153600040119     C                   endif
153700040119     C                   endif
153800040119     C*
153900040119     C                   ENDSR
154000040119     C***
154100010601
154200010601
154300010601
154400010601
154500010601      /TITLE Invio dei dati al punto operativo.
154600010601     C     opeini        BEGSR
154700010601     C*
154800010601     C* Inizializzo flag e contatori operativi
154900010601     C                   movel     '0'           flgGiro           1
155000010601     C                   movel     '0'           flgMulti          1
155100010601     C                   movel     '1'           flgStato          1
155200010615     C                   movel     '0'           flgOk             1
155300010601     C                   z-add     *zeros        cntNonEl         10 0
155400010601     C                   z-add     *zeros        depfil            3 0
155500010601     C                   z-add     *zeros        invfil            3 0
155600010601     C*
155700010601     C                   ENDSR
155800010601     C***
155900070326
156000070326
156100070326
156200070326
156300070326     C     *pssr         BEGSR
156400070329     C*
156500070329     C                   if        %open(tivin00r)
156600070329     C                   close     tivin00r
156700070329     C                   endif
156800070329     C                   if        %open(fivabwwr)
156900070329     C                   close     fivabwwr
157000070329     C                   endif
157100070329     C                   if        %open(fivatwwr)
157200070329     C                   close     fivatwwr
157300070329     C                   endif
157400070326     C*
157500070326     C* Effettuo la chiamata al CLLE preposto
157600070326     C                   call(e)   'TITVVTC'
157700070326     C                   parm                    parccm
157800070326     C                   parm                    parmbr
157900070326     C                   parm      '2'           paropz
158000070326     C*
158100070326     C                   eval      wrkesito = '2'
158200070404     C*
158300070404     C                   seton                                        LR
158400070326     C*
158500070326     C                   ENDSR     '*CANCL'
158600070326     C***
158700070326
158800070326
158900010330
159000010330
159100000613     C     *inzsr        BEGSR
159200990910     C*
159300990910     C     *entry        plist
159400990920     C                   parm                    tivlrds
159500990921     C                   parm      wrkesito      esito
159600000724     C                   parm                    prmlit
159700000710     C                   parm                    prmfir
159800010330     C*
159900010330     C* CALCOLA LA DATA CORRENTE
160000010330     C                   time                    wn14             14 0
160100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160200010330     C                   z-add     wn8           g08dat
160300010330     C                   z-add     *zeros        g08inv
160400010330     C                   movel     '0'           g08err
160500010330     C                   call      'XSRDA8'
160600010330     C                   parm                    wlbda8
160700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
160800000613     C*
160900000613     C                   ENDSR
161000000613     C***
161100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
161200080804W_VABRMN;VABRMA;FILL01;FILL02;VABRSD;VABIND;VABCAD;VABLOD;
161300080804VABPRD;VABPKB;VABVLB;VABNCL;FILL03;VABNOT
161400080418
