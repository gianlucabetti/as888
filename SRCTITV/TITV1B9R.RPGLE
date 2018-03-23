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
003000071029     D wFlgCAS         s              1    INZ(*blanks)
003100030715     D*------------------
003200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003300030715     D*------------------
003400030715     D SkSplitFLD      S             10    DIM(100)
003500030715     D SkSplitCSV      S            256    DIM(100)
003600030715     D CharCSV         S              1
003700030715     D CharTXT         S              1
003800030715     D CharNUM         S              1
003900030715     D posDa           S              3  0 INZ(*zeros)
004000030715     D posA            S              3  0 INZ(*zeros)
004100030715     D i               s              3  0 INZ(1)
004200030715     D wGiro           s              1  0 INZ(*zeros)
004300070823     D wVATNOT_A       s                   LIKE(VATNOT)
004400060331     D wVATNOT_B       s                   LIKE(VATNOT)
004500060331     D wVATNOT_E       s                   LIKE(VATNOT)
004600030822     D*------------------
004700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004800030822     D*------------------
004900030822     D posDaDft        S              3  0 INZ(*zeros)
005000030822     D posADft         S              3  0 INZ(*zeros)
005100030822     D j               s              3  0 INZ(1)
005200030822     D wGiroDft        s              1  0 INZ(*zeros)
005300071029     D*------------------
005400071029     D* DS REPERIMENTO NUMERATORE
005500071029     D*------------------
005600071029     D trul33ds      e ds                  inz
005700071029     D*------------------
005800071029     D* DS ARCHITETTURA
005900071029     D*------------------
006000071029     D kpjba         e ds                  inz
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
014700070823     C*
014800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
014900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
015000070823     C                   exsr      inzvar
015100070823     C                   exsr      impvab
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
021000030822     C                   if        wGiro = 2
021100040510     C* Scrivo sempre il VAT
021200040510     C  N31              exsr      wrivat
021300040510     C* Scrivo il VAB solo a rottura d spedizione
021400040119     C                   if        depspe =  *zeros OR
021500040119     C                             depspe <> vabNSP
021600040119     C  N31              exsr      wrivab
021700040119     C  N31              eval      depspe = vabNSP
021800040119     C                   endif
021900040119     C                   endif
022000010604     C*
022100010604     C                   if        *in31 = *off and
022200010604     C                             *in32 = *off
022300010604     C                   eval      vinflg = '1'
022400010604     C                   else
022500010604     C                   eval      vinflg = '2'
022600010604     C                   endif
022700010604     C                   endif
022800010601     C*
022900010604     C                   endif
023000010604     C*
023100010330     C                   else
023200010330     C                   eval      vinflg = '1'
023300010330     C                   endif
023400010601     C*
023500010601     C  N70              update    tivin000
023600010330     C*
023700030822     C  N70              enddo
023800040119     C*
023900040119     C                   endif                                                  (endif - chkcall)
024000010601     C*
024100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
024200010601     C                   if        cntNonEl = *zeros or
024300010601     C                             flgMulti = '0'
024400010330     C* Se non ci sono record con errori ...
024500010601     C                   if        §ctrno = 0 and
024600010604     C                             §ctrmo = 0 and
024700010601     C                             flgStato <> '2'
024800010330     C* ... restituisco esito OK.
024900010330     C                   eval      wrkesito = '0'
025000010330     C                   else
025100040119     C                   if        §ctrokvb > 0
025200010330     C                   eval      wrkesito = '1'
025300010330     C                   else
025400010615     C                   if        flgOk = '0'
025500010615     C                   eval      wrkesito = '2'
025600010615     C                   else
025700010615     C                   eval      wrkesito = '6'
025800010615     C                   endif
025900010330     C                   endif
026000010330     C                   endif
026100010601     C                   else
026200010601     C                   eval      wrkesito = '9'
026300010601     C                   endif
026400010330     C*
026500010330     C                   if        %open(tivin00r)
026600010330     C                   close     tivin00r
026700010330     C                   endif
026800021025     C                   if        %open(fivabwwr)
026900021025     C                   close     fivabwwr
027000010330     C                   endif
027100040512     C                   if        %open(fivatwwr)
027200040512     C                   close     fivatwwr
027300040512     C                   endif
027400010601     C*
027500010601     C                   if        vlrpoi <> 999
027600010601     C                   eval      invfil = vlrpoi
027700010601     C                   endif
027800010330     C*
027900040119     C                   if        §ctrokvb > 0
028000010601     C                             and invfil > *zeros
028100010330     C                   exsr      invio
028200010330     C                   endif
028300010601     C*
028400010618     C                   if        flgGiro = '1'
028500010601     C                   exsr      endela
028600010618     C                   endif
028700010330     C*
028800010330     C                   ENDSR
028900010330     C***
029000040119
029100040119
029200040119
029300040119     C*----------------------------------------------------*
029400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
029500040119     C*----------------------------------------------------*
029600040510     C     WRIVAB        BEGSR
029700040119     C*
029800060331     C* Gestisco l'eventuale rottura x numero spedizione
029900070823     C                   if        VABNSP <> depspe
030000070301     C                   if        wVATNOT_E <> *blanks
030100070301     C                   eval      VABCTM = '7Q'
030200070301     C                   endif
030300040119     C                   write     fivab000                                     => scarico il VAB
030400060331     C                   endif
030500040119     C*
030600040119     C                   ENDSR
030700040119
030800040119
030900040119
031000040119     C*----------------------------------------------------*
031100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
031200040119     C*----------------------------------------------------*
031300040510     C     WRIVAT        BEGSR
031400060512     C*
031500060512     C* Inizializzo i campi chiave
031600060512     C                   eval      VATFGS = VABFGS
031700060512     C                   eval      VATAAS = VABAAS
031800060512     C                   eval      VATCCM = VABCCM
031900070823     C                   eval      VATNRS = VABNRS
032000060512     C                   eval      VATNSP = VABNSP
032100060512     C                   eval      VATLNP = VABLNP
032200040714     C*
032300040714     C* Solo se VATNOT valorizzato scarico il buffer
032400040714     C                   if        VATNOT <> *blanks
032500040119     C*
032600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
032700040510     C                   if        VATTRC = *blanks
032800040510     C                   eval      VATTRC = 'E'
032900040510     C                   endif
033000040510     C                   write     FIVAT000
033100040119     C                   add       1             §CTROKVT
033200040714     C*
033300040714     C                   endif
033400060331     C*
033500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033600060331     C                   if        wVATNOT_A <> *blanks
033700060331     C                   eval      VATNOT = wVATNOT_A
033800060331     C                   eval      VATTRC = 'A'
033900060331     C                   write     FIVAT000
034000060331     C                   add       1             §CTROKVT
034100060331     C                   endif
034200060331     C*
034300060331     C                   if        wVATNOT_B <> *blanks
034400060331     C                   eval      VATNOT = wVATNOT_B
034500060331     C                   eval      VATTRC = 'B'
034600060331     C                   write     FIVAT000
034700060331     C                   add       1             §CTROKVT
034800060331     C                   endif
034900060331     C*
035000060331     C                   if        wVATNOT_E <> *blanks
035100060331     C                   eval      VATNOT = wVATNOT_E
035200060331     C                   eval      VATTRC = 'E'
035300060331     C                   write     FIVAT000
035400060331     C                   add       1             §CTROKVT
035500060331     C                   endif
035600040119     C*
035700040119     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036300040119     C*----------------------------------------------------*
036400040119     C     PREVAT        BEGSR
036500040119     C*
036600040119     C* Compongo il nome del membro da dare al FIVATWWR
036700040119     C                   eval      parmbr = vlrhdl
036800040119     C                   movel     'M'           parmbr
036900040510     C                   eval      parccm = vlrksc
037000040119     C                   eval      paropz = '1'
037100040119     C* Effettuo la chiamata al CLLE preposto
037200040119     C                   call(e)   'TITVVTC'
037300040119     C                   parm                    parccm
037400040119     C                   parm                    parmbr
037500040119     C                   parm                    paropz
037600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037700040119     C                   if        %error
037800040119     C                   movel     '1'           chkcall
037900040119     C                   else
038000040119     C                   movel     '0'           chkcall
038100040119     C                   endif
038200040119     C*
038300040119     C                   ENDSR
038400010601
038500010601
038600010601
038700010330     C*----------------------------------------------------*
038800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038900010330     C*----------------------------------------------------*
039000040119     C     INZVAR        BEGSR
039100010330     C*
039200030822     C* Inizializzo variabili di wrk
039300020204     C                   Z-ADD     *zeros        Num5_0            5 0
039400060331     C                   MOVEL     *blanks       wVATNOT_A
039500060331     C                   MOVEL     *blanks       wVATNOT_B
039600060331     C                   MOVEL     *blanks       wVATNOT_E
039700030822     C*
039800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
039900030822     C                   CLEAR                   FIVAB000
040000040119     C                   CLEAR                   FIVAT000
040100030822     C                   CLEAR                   SkSplitCSV
040200030822     C                   EVAL      i = 1
040300030822     C                   EVAL      posDa = *zeros
040400030822     C                   EVAL      posA  = *zeros
040500030822     C*
040600030822     C* Reimposto i valori di default
040700030822     C                   EXSR      DEFCAM
040800030822     C*
040900010330     C                   ENDSR
041000010330     C*----------------------------------------------------*
041100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041200010330     C*----------------------------------------------------*
041300010330     C     DEFCAM        BEGSR
041400030822     C*
041500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
041600030715     C* e delimitatore testo.
041700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
041800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
041900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042000030822     C*
042100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
042300030715     C                   SETON                                        50
042400030715     C                   ELSE
042500030715     C                   SETOFF                                       50
042600030715     C                   ENDIF
042700030822     C*
042800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
042900030822     C                   EVAL      posDaDft = 1
043000030822     C                   EVAL      posADft  = 0
043100030822     C                   EVAL      wGiroDft = 0
043200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
043300030822     C                             posDaDft > 0
043400030822     C*
043500030822     C* Gestisco il 1° giro
043600030822     C                   IF        wGiroDft = 0
043700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
043800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
043900030822     C* Incremento il contatore dei "giri"
044000030822     C                   EVAL      wGiroDft = 1
044100030822     C                   ELSE
044200030822     C                   EVAL      posDaDft = posADft
044300030822     C                   ENDIF
044400030822     C* Eseguo lo scan x trovare la fine del campo corrente
044500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
044600030822     C*
044700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
044800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
044900030822     C                   IF        posDaDft > 0 AND
045000030822     C                             posADft  > 0
045100030822     C* NCL
045200030822     C                   IF        %subst(
045300030822     C                             %subst(vlrppt:posDaDft+1:
045400030822     C                             posADft-posDaDft-1):1:3)
045500030822     C                             = 'NCL'
045600030822     C                   EVAL      PiStr=%trim(%subst(
045700030822     C                             %subst(vlrppt:posDaDft+1:
045800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
045900030822     C                   EXSR      CHKNUM
046000030822     C                   IF        PiInt=*on
046100030822     C                   Z-ADD     PiVal         VABNCL
046200030822     C                   ENDIF
046300030822     C                   ENDIF
046400030822     C* CCM
046500030822     C                   IF        %subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):1:3)
046800030822     C                             = 'CCM'
046900030822     C                   EVAL      PiStr=%trim(%subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200030822     C                   EXSR      CHKNUM
047300030822     C                   IF        PiInt=*on
047400030822     C                   Z-ADD     PiVal         VABCCM
047500030822     C                   ENDIF
047600030822     C                   ENDIF
047700030822     C* LNP
047800030822     C                   IF        %subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):1:3)
048100030822     C                             = 'LNP'
048200030822     C                   EVAL      PiStr=%trim(%subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500030822     C                   EXSR      CHKNUM
048600030822     C                   IF        PiInt=*on
048700030822     C                   Z-ADD     PiVal         VABLNP
048800030822     C                   ENDIF
048900030822     C                   ENDIF
049000030822     C* NRS
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'NRS'
049500030822     C                   EVAL      PiStr=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   EXSR      CHKNUM
049900030822     C                   IF        PiInt=*on
050000030822     C                   Z-ADD     PiVal         VABNRS
050100030822     C                   ENDIF
050200030822     C                   ENDIF
050300030822     C* CTR
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'CTR'
050800030822     C                   EVAL      PiStr=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   EXSR      CHKNUM
051200030822     C                   IF        PiInt=*on
051300030822     C                   Z-ADD     PiVal         VABCTR
051400030822     C                   ENDIF
051500030822     C                   ENDIF
051600030822     C* PKB
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'PKB'
052100030822     C                   EVAL      PiStr=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   EXSR      CHKNUM
052500030822     C                   IF        PiNum=*on
052600030822     C                   Z-ADD     PiVal         VABPKB
052700030822     C                   ENDIF
052800030822     C                   ENDIF
052900030822     C* VLB
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'VLB'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiNum=*on
053900030822     C                   Z-ADD     PiVal         VABVLB
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* QFT
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'QFT'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiNum=*on
055200030822     C                   Z-ADD     PiVal         VABQFT
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* CBO
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'CBO'
056000030822     C                   EVAL      VABCBO=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   ENDIF
056400030822     C* TSP
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'TSP'
056900030822     C                   EVAL      VABTSP=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* VAS
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'VAS'
057800030822     C                   EVAL      VABVAS=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   ENDIF
058200030822     C* VCA
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'VCA'
058700030822     C                   EVAL      VABVCA=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   ENDIF
059100070925     C* TIC
059200070925     C                   IF        %subst(
059300070925     C                             %subst(vlrppt:posDaDft+1:
059400070925     C                             posADft-posDaDft-1):1:3)
059500070925     C                             = 'TIC'
059600070925     C                   EVAL      VABTIC=%trim(%subst(
059700070925     C                             %subst(vlrppt:posDaDft+1:
059800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900070925     C                   ENDIF
060000030822     C* GCA
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'GCA'
060500030822     C                   EVAL      VABGCA=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   ENDIF
060900030822     C* CTM
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CTM'
061400030822     C                   EVAL      VABCTM=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   ENDIF
061800030822     C* FFD
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'FFD'
062300030822     C                   EVAL      VABFFD=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   ENDIF
062700030822     C* VAD
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'VAD'
063200030822     C                   EVAL      VABVAD=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   ENDIF
063600030822     C* GMA
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'GMA'
064100030822     C                   EVAL      VABGMA=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   ENDIF
064500030822     C* GGA
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'GGA'
065000030822     C                   EVAL      VABGGA=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* GVA
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'GVA'
065900030822     C                   EVAL      VABGVA=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* TC1
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'TC1'
066800030822     C                   EVAL      VABTC1=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* TC2
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'TC2'
067700030822     C                   EVAL      VABTC2=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   ENDIF
068100040714     C* VATTRC
068200040714     C                   IF        %subst(
068300040714     C                             %subst(vlrppt:posDaDft+1:
068400040714     C                             posADft-posDaDft-1):1:3)
068500040714     C                             = 'TRC'
068600040714     C                   EVAL      VATTRC=%trim(%subst(
068700040714     C                             %subst(vlrppt:posDaDft+1:
068800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900040714     C                   ENDIF
069000030822     C* ...
069100030822     C                   ENDIF
069200030822     C                   ENDDO
069300020204     C*
069400010330     C                   ENDSR
069500010607     C*----------------------------------------------------*
069600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
069700010607     C*----------------------------------------------------*
069800010607     C     IMPVAB        BEGSR
069900010607     C*
070000010607     C                   SETOFF                                       3132
070100030515     C*
070200010607     C                   MOVEL     datcor        VABAAS
070300010607     C                   MOVE      datcor        VABMGS
070400010607     C*
070500030715     C**********
070600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
070700030715     C**********
070800030715     C                   IF        wGiro = *zeros
070900040113     C*
071000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
071100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
071200040510     C                             %trim(intcol(3))<>*blanks
071300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
071400040113     C                                      %trim(intcol(3))
071500070823     C     ';':CharCSV   XLATE     vindta        vindta
071600040510     C                   ENDIF
071700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
071800031201     C     minu:maiu     XLATE     vindta        vindta
071900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
072000030715     C                   DOW       posDa <= %len(%trim(vindta))
072100030715     C*
072200030715     C* Gestisco il 1° campo
072300030715     C                   IF        i = 1
072400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
072500030715     C                   EVAL      posDa = 1
072600030715     C* Eseguo lo scan x trovare la fine del primo campo
072700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
072800030715     C* A questo punto "estraggo" il campo corrente
072900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
073000030715     C                                                    (posA-posDa))
073100030715     C* X i campi successivi al 1°
073200030715     C                   ELSE
073300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
073400030715     C                   EVAL      posDa = posA + 1
073500030715     C* Eseguo lo scan x trovare la fine del campo corrente
073600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
073700030715     C* Gestisco l'ultimo campo
073800030715     C                   IF        posA = *zeros
073900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
074000030715     C* A questo punto "estraggo" il campo corrente
074100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074200030715     C                                                    (posA-posDa))
074300030715     C                   LEAVE
074400030715     C                   ELSE
074500030715     C* A questo punto "estraggo" il campo corrente
074600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074700030715     C                                                    (posA-posDa))
074800030715     C                   ENDIF
074900030715     C                   ENDIF
075000030715     C* Incremento il contatore d campo
075100030715     C                   EVAL      i = i +1
075200030715     C                   ENDDO
075300030715     C*
075400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
075500030715     C* delimitazione testo
075600030715     C                   EVAL      i = 1
075700030715     C                   DOW       i < %elem(SkSplitFLD)
075800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
075900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
076000030715     C                   EVAL      i = i + 1
076100030715     C                   ENDDO
076200030715     C                   EVAL      wGiro = 1
076300030715     C                   ELSE
076400030715     C                   EVAL      wGiro = 2
076500031204     C**********
076600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
076700031204     C* d separatore campo
076800031204     C**********
076900031204     C                   EVAL      vindta = %trim(vindta)
077000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
077100031204     C                   EVAL      vindta = %subst(vindta:2)
077200031204     C                   ENDDO
077300031204     C*
077400031204     C                   Z-ADD     *zeros        lunghInput        4 0
077500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
077700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
077800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
077900031204     C                   ENDDO
078000030715     C**********
078100030715     C* Effettuo lo split del campo dati d input
078200030715     C**********
078300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078400030715     C                   DOW       posDa <= %len(%trim(vindta))
078500030715     C*
078600030715     C* Gestisco il 1° campo
078700030715     C                   IF        i = 1
078800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078900030715     C                   EVAL      posDa = 1
079000030715     C* Eseguo lo scan x trovare la fine del primo campo
079100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079200030715     C* A questo punto "estraggo" il campo corrente
079300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
079400030715     C                                                    (posA-posDa))
079500030715     C* X i campi successivi al 1°
079600030715     C                   ELSE
079700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079800030715     C                   EVAL      posDa = posA + 1
079900031204     C* Verifico che nn vi sia il campo nn valorizzato
080000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
080100031204     C* Se campo nn valorizzato skippo al prossimo
080200031204     C                   EVAL      SkSplitCSV(i) = *blanks
080300031204     C                   EVAL      posA  = posA + 1
080400031204     C                   ELSE
080500030715     C* Eseguo lo scan x trovare la fine del campo corrente
080600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080700030715     C* Gestisco l'ultimo campo
080800030715     C                   IF        posA = *zeros
080900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   LEAVE
081400030715     C                   ELSE
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C                   ENDIF
081900031204     C                   ENDIF
082000030715     C                   ENDIF
082100030715     C* Incremento il contatore d campo
082200030715     C                   EVAL      i = i +1
082300030715     C                   ENDDO
082400030715     C*
082500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082600030715     C* delimitazione testo
082700030715     C                   EVAL      i = 1
082800030715     C                   DOW       i < %elem(SkSplitCSV)
082900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
083000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
083100030715     C                   EVAL      i = i + 1
083200030715     C                   ENDDO
083300030715     C*
083400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
083500030715     C                   EVAL      i = 1
083600030715     C                   DOW       i < %elem(SkSplitFLD)
083700030715     C*
083800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
083900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
084000030715     C                             SkSplitCSV(i) <> *blanks
084100030721     C*** ATB
084200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
084300030721     C                   EVAL      VABATB = SkSplitCSV(i)
084400030721     C                   ENDIF
084500030715     C*** RSD
084600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
084700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
084800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
084900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
085000030715     C* ==
085100030715     C                   ENDIF
085200030721     C*** RD2
085300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
085400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
085500030721     C                   ENDIF
085600030715     C*** IND
085700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
085800030715     C                   EVAL      VABIND = SkSplitCSV(i)
085900030715     C                   ENDIF
086000030715     C*** LOD
086100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
086200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
086300030715     C                   ENDIF
086400030715     C*** PRD
086500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
086600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
086700030715     C                   ENDIF
086800030721     C*** NZD
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
087000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030715     C*** RMA
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
087400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
087500030715     C                   ENDIF
087600030715     C*** NOT
087700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
087800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
087900030715     C                   ENDIF
088000030715     C*** NT2
088100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
088200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
088300030715     C                   ENDIF
088400030715     C*** VCA
088500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
088600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
088700030715     C                   ENDIF
088800030715     C*** VAS
088900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
089000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
089100030715     C                   ENDIF
089200030715     C*** FFD
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
089400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
089500030715     C                   ENDIF
089600030721     C*** GC1
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
089800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030721     C*** GC2
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
090200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030721     C*** TSP
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
090600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
090700030721     C                   ENDIF
090800040510     C*** CBO
090900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
091000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
091100040510     C                   ENDIF
091200030721     C*** NAS
091300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
091400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
091500030721     C                   ENDIF
091600030721     C*** TIC
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
091800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030721     C*** GCA
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
092200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030721     C*** XCO
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
092600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
092700030721     C                   ENDIF
092800030721     C*** CTM
092900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
093000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
093100030721     C                   ENDIF
093200030721     C*** TCR
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
093400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** CTS
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
093800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** FTM
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
094200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030721     C*** VAD
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
094600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** GMA
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
095000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** GGA
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
095400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** GVA
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
095800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** TC1
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
096200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TC2
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
096600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** SCL
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
097000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** RMO
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
097400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** NMO
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
097800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*
098100030721     C* Reperisco quindi i campi numerici...
098200040510     C*** NSP
098300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
098400040510     C                   EVAL      PiStr=SkSplitCSV(i)
098500040510     C                   EXSR      CHKNUM
098600040510     C                   IF        PiInt=*on
098700040510     C                   Z-ADD     PiVal         VABNSP
098800040510     C                   ELSE
098900040510     C                   SETON                                        31
099000040510     C                   EVAL      VABNSP = *zeros
099100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
099200040510     C                             + ' ' + 'VABNSP'
099300040510     C                   ENDIF
099400040510     C                   ENDIF
099500071106     C*** RMN
099600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
099700071106     C                   EVAL      PiStr=SkSplitCSV(i)
099800071106     C                   EXSR      CHKNUM
099900071106     C                   IF        PiInt=*on
100000071106     C                   Z-ADD     PiVal         VABRMN
100100071106     C                   ELSE
100200071106     C                   SETON                                        32
100300071106     C                   EVAL      VABRMN = *zeros
100400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
100500071106     C                             + ' ' + 'VABRMN'
100600071106     C                   ENDIF
100700071106     C                   ENDIF
100800030721     C*** CAD
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
101000030721     C                   EVAL      PiStr=SkSplitCSV(i)
101100030721     C                   EXSR      CHKNUM
101200030721     C                   IF        PiInt=*on
101300030721     C                   Z-ADD     PiVal         Num5_0
101400030721     C                   MOVEL(p)  Num5_0        VABCAD
101500030721     C                   ELSE
101600030721     C                   SETON                                        32
101700030721     C                   EVAL      VABCAD = *zeros
101800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
101900030721     C                             + ' ' + 'VABCAD'
102000030721     C                   ENDIF
102100030721     C                   ENDIF
102200030721     C*** NCL
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
102400030721     C                   EVAL      PiStr=SkSplitCSV(i)
102500030721     C                   EXSR      CHKNUM
102600030721     C                   IF        PiInt=*on
102700030721     C                   Z-ADD     PiVal         VABNCL
102800030721     C                   ELSE
102900030721     C                   SETON                                        32
103000030721     C                   EVAL      VABNCL = *zeros
103100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103200040122     C                             + ' ' + 'VABNCL'
103300030721     C                   ENDIF
103400030721     C                   ENDIF
103500030721     C*** PKB
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiNum=*on
104000030721     C                   Z-ADD     PiVal         VABPKB
104100030721     C                   ELSE
104200030721     C                   SETON                                        32
104300030721     C                   EVAL      VABPKB = *zeros
104400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104500030721     C                             + ' ' + 'VABPKB'
104600030721     C                   ENDIF
104700030721     C                   ENDIF
104800030721     C*** CAS
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
105000030721     C                   EVAL      PiStr=SkSplitCSV(i)
105100030721     C                   EXSR      CHKNUM
105200030721     C                   IF        PiNum=*on
105300030721     C                   Z-ADD     PiVal         VABCAS
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      VABCAS = *zeros
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800030721     C                             + ' ' + 'VABCAS'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** CCM
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on      AND
106600030721     C                             PiVal<=9999999 AND
106700030721     C                             PiVal>*zeros
106800030721     C                   Z-ADD     PiVal         VABCCM
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      VABCCM = *zeros
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300040510     C                             + ' ' + 'VABCCM'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** LNP
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiInt=*on    AND
108100030721     C                             PiVal<=999   AND
108200030721     C                             PiVal>*zeros
108300030721     C                   Z-ADD     PiVal         VABLNP
108400030721     C                   ELSE
108500040715     C                   SETON                                        32
108600030721     C                   EVAL      VABLNP = *zeros
108700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108800040510     C                             + ' ' + 'VABLNP'
108900030721     C                   ENDIF
109000030721     C                   ENDIF
109100030721     C*** NRS
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
109300030721     C                   EVAL      PiStr=SkSplitCSV(i)
109400030721     C                   EXSR      CHKNUM
109500030721     C                   IF        PiInt=*on AND
109600030721     C                             PiVal<=99
109700030721     C                   Z-ADD     PiVal         VABNRS
109800030721     C                   ELSE
109900030721     C                   SETON                                        32
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100030721     C                             + ' ' + 'VABNRS'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** LNA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiInt=*on    AND
110900030721     C                             PiVal<=999   AND
111000030721     C                             PiVal>*zeros
111100030721     C                   Z-ADD     PiVal         VABLNA
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      VABLNA = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600030721     C                             + ' ' + 'VABLNA'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** CTR
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on  AND
112400030721     C                             PiVal<=999
112500030721     C                   Z-ADD     PiVal         VABCTR
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABCTR'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** IAS
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiNum=*on
113700030721     C                   Z-ADD     PiVal         VABIAS
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABIAS'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** VLB
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiNum=*on
114900030721     C                   Z-ADD     PiVal         VABVLB
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABVLB'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** QFT
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiNum=*on
116100030721     C                   Z-ADD     PiVal         VABQFT
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABQFT'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** NCD
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on      AND
117300030721     C                             PiVal<=9999999
117400030721     C                   Z-ADD     PiVal         VABNCD
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABNCD'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** NCA
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on      AND
118600030721     C                             PiVal<=9999999
118700030721     C                   Z-ADD     PiVal         VABNCA
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABNCA'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** ZNC
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on AND
119900030721     C                             PiVal<=99
120000030721     C                   Z-ADD     PiVal         VABZNC
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABZNC'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** DCR
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on       AND
121200030721     C                             PiVal<=99999999
121300030721     C                   Z-ADD     PiVal         VABDCR
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABDCR'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** HCR
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on   AND
122500030721     C                             PiVal<=9999
122600030721     C                   Z-ADD     PiVal         VABHCR
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABHCR'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** VMD
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiNum=*on
123800030721     C                   Z-ADD     PiVal         VABVMD
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABVMD'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** ANT
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on        AND
125000030721     C                             PiVal<=999999999
125100030721     C                   Z-ADD     PiVal         VABANT
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABANT'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CMO
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on
126300030721     C                   Z-ADD     PiVal         Num5_0
126400030721     C                   MOVEL(p)  Num5_0        VABCMO
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      VABCMO = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABCMO'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200040510     C*
127300040510     C* Reperisco i campi del FIVAT
127400040510     C*** VATNOT
127500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
127600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
127700040510     C                   ENDIF
127800040510     C*** VATTRC
127900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
128000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
128100040510     C                   ENDIF
128200060331     C*** VATNOT_A
128300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
128400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
128500060331     C                   ENDIF
128600060331     C*** VATNOT_B
128700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
128800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
128900071106     C                   EVAL      VABNOT    = %trim(SkSplitCSV(i))
129000060331     C                   ENDIF
129100060331     C*** VATNOT_E
129200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
129300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
129400060331     C                   ENDIF
129500070823     C*
129600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
129700071029     C* NSP => Stacco un numeratore da AZNUM
129800071029     C                   clear                   TRUL33DS
129900071029     C                   eval      I33OPE = *zeros
130000071029     C                   eval      I33CNU = 302
130100071029     C                   eval      I33NUM = 1
130200071029     C                   movel     TRUL33DS      KPJBU
130300071029     C                   call      'TRUL33R'
130400071029     C                   parm                    KPJBA
130500071029     C                   movel     KPJBU         TRUL33DS
130600071029     C                   if        O33ERR = *zeros
130700071029     C                   z-add     O33NRF        VABNSP
130800071029     C                   z-add     O33NRF        VATNSP
130900071029     C                   else
131000071029     C                   SETON                                        31
131100071029     C                   EVAL      vinmsg = %trimr(vinmsg)
131200071029     C                             + ' ' + 'VABNSP VATNSP'
131300071029     C                   endif
131400120706     C*** RMN
131500120706     C                   eval      VABRMN = datcor
131600120706     C*** RMA
131700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
131800071106     C                   EVAL      VABRMA = SkSplitCSV(i)
131900120706     C***                EVAL      PiStr=%subst(SkSplitCSV(i):5:9)
132000120706     C***                EXSR      CHKNUM
132100120706     C***                IF        PiInt=*on
132200120706     C***                Z-ADD     PiVal         VABRMN
132300120706     C***                ELSE
132400120706     C***                SETON                                        32
132500120706     C***                EVAL      VABRMN = *zeros
132600120706     C***                EVAL      vinmsg = %trimr(vinmsg)
132700120706     C***                          + ' ' + 'VABRMN'
132800120706     C***                ENDIF
132900071106     C                   ENDIF
133000070925     C*
133100071029     C***  <===  -----------------------------------------
133200040510     C*
133300030715     C                   ENDIF
133400030715     C                   EVAL      i = i + 1
133500030715     C                   ENDDO
133600070925     C*
133700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
133800070925     C*
133900071029     C***  <===  -----------------------------------------
134000070925     C*
134100070925     C* Considerazioni finali su CBO/CAS
134200070925     C                   IF        WFLGCAS = '1'
134300070925     C                   IF        VABCBO = '1'
134400070925     C                   EVAL      VABCBO = '4'
134500070925     C                   ENDIF
134600070925     C                   IF        VABCBO = '2'
134700070925     C                   EVAL      VABCBO = '6'
134800070925     C                   ENDIF
134900070925     C                   ENDIF
135000070925     C***  <===  ----------------------------
135100040510     C*
135200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
135300040510     C                   IF        VABNSP = *zeros
135400040510     C                   SETON                                        31
135500040510     C                   ENDIF
135600040715     C*
135700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
135800040715     C                   IF        VABLNP = *zeros
135900040715     C                   SETON                                        31
136000040715     C                   ENDIF
136100040510     C*
136200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
136300040510     C                   IF        VABRMN = *zeros
136400040510     C                   Z-ADD     VABNSP        VABRMN
136500040510     C                   ENDIF
136600040510     C*
136700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
136800040510     C                   IF        VABRMA = *blanks
136900040714     C                   MOVEL     VABRMN        VABRMA
137000040510     C                   ENDIF
137100040128     C*
137200040510     C* Se provincia nn valorizzata la reperisco
137300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
137400040510     C                   IF        VABPRD = *blanks
137500040128     C                   CLEAR                   TISI95DS
137600040128     C                   EVAL      I95TCN = '3'
137700040128     C                   Z-ADD     datcor        I95DAT
137800040128     C                   EVAL      I95NAR = VABNZD
137900040128     C                   EVAL      I95CAP = VABCAD
138000040128     C                   EVAL      I95LOC = VABLOD
138100040128     C                   CALL      'TISI95R'
138200040128     C                   PARM                    TISI95DS
138300040128     C                   EVAL      VABPRD = O95PRV
138400040510     C                   ENDIF
138500040113     C*
138600040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
138700040113     C                   IF        VABCAS > *zeros
138800040119     C                   EVAL      VABVCA = 'EUR'
138900040119     C                   IF        VABCBO = '1'
139000040113     C                   EVAL      VABCBO = '4'
139100040119     C                   ENDIF
139200040119     C                   IF        VABCBO = '2'
139300040119     C                   EVAL      VABCBO = '6'
139400040119     C                   ENDIF
139500040113     C                   ENDIF
139600020204     C*
139700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
139800020204     C                   EXSR      CHKIMPDIV
139900030715     C                   ENDIF
140000020204     C*
140100010607     C                   ENDSR
140200010607     C*----------------------------------------------------*
140300010601
140400020204
140500020204     C*----------------------------------------------------*
140600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
140700020204     C*----------------------------------------------------*
140800020204     C     CHKIMPDIV     BEGSR
140900020204     C*
141000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141100020204     C                   Z-ADD     *zeros        wrkDec            9 9
141200020204     C*
141300020204     C* Come prima cosa effettuo considerazioni sulla divisa
141400020204     C                   IF        vabIAS > *zeros
141500020204     C                   IF        vabVAS <> 'EUR'
141600020204     C                   EVAL      vabVAS =  'ITL'
141700020204     C                   ENDIF
141800020204     C                   ENDIF
141900020204     C*
142000020204     C                   IF        vabCAS > *zeros
142100020204     C                   IF        vabVCA <> 'EUR'
142200020204     C                   EVAL      vabVCA =  'ITL'
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C                   IF        vabVMD > *zeros
142700020204     C                   IF        vabVAD <> 'EUR'
142800020204     C                   EVAL      vabVAD =  'ITL'
142900020204     C                   ENDIF
143000020204     C                   ENDIF
143100020204     C*
143200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143300020204     C                   Z-ADD     vabIAS        wrkDec
143400020204     C                   IF        wrkDec > *zeros
143500020204     C                   IF        vabVAS = 'ITL'
143600020204     C                   EVAL      vabIAS = *zeros
143700020204     C                   ENDIF
143800020204     C                   ENDIF
143900020204     C*
144000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144100020204     C                   Z-ADD     vabCAS        wrkDec
144200020204     C                   IF        wrkDec > *zeros
144300020204     C                   IF        vabVCA = 'ITL'
144400020204     C                   EVAL      vabCAS = *zeros
144500020204     C                   ENDIF
144600020204     C                   ENDIF
144700020204     C*
144800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
144900020204     C                   Z-ADD     vabVMD        wrkDec
145000020204     C                   IF        wrkDec > *zeros
145100020204     C                   IF        vabVAD = 'ITL'
145200020204     C                   EVAL      vabVMD = *zeros
145300020204     C                   ENDIF
145400020204     C                   ENDIF
145500020204     C*
145600020204     C                   ENDSR
145700020204     C***
145800020204
145900010330
146000010330
146100010330     C*----------------------------------------------------*
146200010330     C*  CONTROLLO NUMERICITA' CAMPI
146300010330     C*----------------------------------------------------*
146400010330     C     CHKNUM        BEGSR
146500010330     C*
146600010606     C                   IF        PiDecChr = *blanks
146700030715     C                   EVAL      PiDecChr = CharNUM
146800010606     C                   ENDIF
146900010606     C*
147000010606     C                   CALL(e)   'ISNUMERIC'
147100010330     C                   PARM                    PiStr            30
147200010606     C                   PARM                    PiDecChr          1
147300010330     C                   PARM      *ZEROS        PiVal            30 9
147400010330     C                   PARM      '0'           PiInt             1
147500010330     C                   PARM      '0'           PiNum             1
147600010330     C                   IF        %error
147700010606     C                   EVAL      PiNum=*off
147800010330     C                   ENDIF
147900010330     C*
148000010330     C                   ENDSR
148100010330     C***
148200010330
148300010601
148400010601
148500010601
148600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
148700010601     C     repfil        BEGSR
148800010601     C*
148900010601     C                   if        invfil = *zeros and
149000010601     C                             depfil > *zeros and
149100010629     C                             (vinflg = *blanks or
149200010629     C                              vinflg = *zeros)
149300010601     C
149400010601     C                   eval      invfil = depfil
149500010601     C                   endif
149600010601     C*
149700010601     C                   if        depfil <> invfil and
149800010601     C                             invfil > *zeros
149900010601     C                   eval      flgMulti = '1'
150000010601     C                   if        vinflg = *blanks
150100010601     C                   add       1             cntNonEl
150200010601     C                   endif
150300010601     C                   endif
150400010601     C*
150500010601     C                   if        vinflg = '2'
150600010601     C                   eval      flgStato = '2'
150700010601     C                   endif
150800010601     C*
150900010601     C                   ENDSR
151000010601     C***
151100010601
151200010601
151300010601
151400010330
151500040119      /TITLE Invio dei dati al punto operativo.
151600040119     C     invio         BEGSR
151700040119     C*
151800040119     C* 1° invio FIVAT
151900040119     C                   reset                   dscmz
152000040119     C                   move      vlrpoi        cmzdst
152100040119     C                   eval      cmzfld = 'FIVATWWR'
152200040119     C                   eval      cmzmbd = vlrhdl
152300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152400040119     C***                if        prmfir = *blanks
152500040119     C                   eval      cmzfla = 'FIVAT00F'
152600040119     C                   eval      cmzmba = 'FIVAT00F'
152700040119     C***                else
152800040119     C***                eval      cmzfla = prmfir
152900040119     C***                eval      cmzmba = prmfir
153000040119     C***                endif
153100040119     C                   eval      cmznrr = *zeros
153200040119     C                   move      §ctrokvt      cmznrr
153300040119     C                   eval      cmzlba = vlrfl1
153400040119     C                   call(e)   'TIS711C'
153500040119     C                   parm                    dscmz
153600040119     C                   parm      *blanks       esito
153700040119     C                   if        %error
153800040119     C                             or cmzerr = '1'
153900040119     C                             or esito  = '1'
154000040119     C                   eval      wrkesito = '3'
154100040119     C                   else
154200040119     C*
154300040119     C* 2° invio FIVAB
154400040119     C                   reset                   dscmz
154500040119     C                   move      vlrpoi        cmzdst
154600040119     C                   eval      cmzfld = vlrfou
154700040119     C                   eval      cmzmbd = vlrhdl
154800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154900040119     C***                if        prmfir = *blanks
155000040119     C                   eval      cmzfla = 'FIVAB00F'
155100040119     C                   eval      cmzmba = 'FIVAB00F'
155200040119     C***                else
155300040119     C***                eval      cmzfla = prmfir
155400040119     C***                eval      cmzmba = prmfir
155500040119     C***                endif
155600040119     C                   eval      cmznrr = *zeros
155700040119     C                   move      §ctrokvb      cmznrr
155800040119     C                   eval      cmzlba = vlrfl1
155900040119     C                   call(e)   'TIS711C'
156000040119     C                   parm                    dscmz
156100040119     C                   parm      *blanks       esito
156200040119     C                   if        %error
156300040119     C                             or cmzerr = '1'
156400040119     C                             or esito  = '1'
156500040119     C                   eval      wrkesito = '3'
156600040119     C                   endif
156700040119     C                   endif
156800040119     C*
156900040119     C                   ENDSR
157000040119     C***
157100010601
157200010601
157300010601
157400010601
157500010601      /TITLE Invio dei dati al punto operativo.
157600010601     C     opeini        BEGSR
157700010601     C*
157800010601     C* Inizializzo flag e contatori operativi
157900010601     C                   movel     '0'           flgGiro           1
158000010601     C                   movel     '0'           flgMulti          1
158100010601     C                   movel     '1'           flgStato          1
158200010615     C                   movel     '0'           flgOk             1
158300010601     C                   z-add     *zeros        cntNonEl         10 0
158400010601     C                   z-add     *zeros        depfil            3 0
158500010601     C                   z-add     *zeros        invfil            3 0
158600010601     C*
158700010601     C                   ENDSR
158800010601     C***
158900070326
159000070326
159100070326
159200070326
159300070326     C     *pssr         BEGSR
159400070329     C*
159500070329     C                   if        %open(tivin00r)
159600070329     C                   close     tivin00r
159700070329     C                   endif
159800070329     C                   if        %open(fivabwwr)
159900070329     C                   close     fivabwwr
160000070329     C                   endif
160100070329     C                   if        %open(fivatwwr)
160200070329     C                   close     fivatwwr
160300070329     C                   endif
160400070326     C*
160500070326     C* Effettuo la chiamata al CLLE preposto
160600070326     C                   call(e)   'TITVVTC'
160700070326     C                   parm                    parccm
160800070326     C                   parm                    parmbr
160900070326     C                   parm      '2'           paropz
161000070326     C*
161100070326     C                   eval      wrkesito = '2'
161200070404     C*
161300070404     C                   seton                                        LR
161400070326     C*
161500070326     C                   ENDSR     '*CANCL'
161600070326     C***
161700070326
161800070326
161900010330
162000010330
162100000613     C     *inzsr        BEGSR
162200990910     C*
162300990910     C     *entry        plist
162400990920     C                   parm                    tivlrds
162500990921     C                   parm      wrkesito      esito
162600000724     C                   parm                    prmlit
162700000710     C                   parm                    prmfir
162800010330     C*
162900010330     C* CALCOLA LA DATA CORRENTE
163000120706     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
163100120706     C                   eval      datcor = %dec(%date() : *ISO)
163200000613     C*
163300000613     C                   ENDSR
163400000613     C***
163500990908
163600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
163700071029VABRSD,FILL1,VABIND,VABLOD,VABPRD,VABCAD,VATNOT_B,FILL2,FILL3,FILL4,
163800071106FILL5,FILL6,FILL7,FILL8,FILL9,VABRMA,VABNCL,VABPKB,VABNAS,VABNT2,FILL10,
163900071029FILL11,FILL12
