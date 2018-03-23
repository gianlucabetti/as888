000100100315      /TITLE Upload via Internet: traduzione in FIVABWR/FIVATWR.
000200130215     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100315     FFIVABwwr  O    E             DISK    usropn
000600100315     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100090716     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
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
003000080125     D wFlgCAS         s              1    INZ(*blanks)
003100080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000100204     D CharSOS         S              1
004100030715     D posDa           S              3  0 INZ(*zeros)
004200030715     D posA            S              3  0 INZ(*zeros)
004300030715     D i               s              3  0 INZ(1)
004400030715     D wGiro           s              1  0 INZ(*zeros)
004500080125     D wVATNOT_A       s                   LIKE(VATNOT)
004600080125     D wVATNOT_B       s                   LIKE(VATNOT)
004700080125     D wVATNOT_E       s                   LIKE(VATNOT)
004800030822     D*------------------
004900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005000030822     D*------------------
005100030822     D posDaDft        S              3  0 INZ(*zeros)
005200030822     D posADft         S              3  0 INZ(*zeros)
005300030822     D j               s              3  0 INZ(1)
005400030822     D wGiroDft        s              1  0 INZ(*zeros)
005500031201     D*------------------
005600031201     D* Costanti
005700031201     D*------------------
005800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
005900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006000080617     D*------------------
006100080617
006200080617     D*------------------
006300080617     D* LINKING A DEFINIZIONI ESTERNE
006400080617     D*------------------
006500080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006700010330
006800010330
006900010330
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200081014     C                   reset                   wrkesito
007300010601     C*
007400010601     C                   exsr      opeini
007500010605     C                   exsr      rwvab
007600081014     C*
007700081014     C                   exsr      endela
007800010601     C*
007900010601     C                   seton                                        lr
008000010601
008100010601
008200010601
008300010601     C*--------------------------------------------------------
008400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010601     C*--------------------------------------------------------
008600010601     C     PREELA        BEGSR
008700010601     C*
008800010601     C* SE OCCORRE SPEDIRE IN FILIALE
008900010601     C                   if        invfil <> *zeros and
009000010601     C                             flgGiro = '0'
009100010601     C*
009200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010601     C                   eval      flgGiro = '1'
009400010601     C*
009500010601     C                   endif
009600010601     C*
009700010601     C                   ENDSR
009800010601     C***
009900010601
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010500010601     C*--------------------------------------------------------
010600010601     C     ENDELA        BEGSR
010700081014     C*
010800081014     C                   if        %open(tivin00r)
010900081014     C                   close     tivin00r
011000081014     C                   endif
011100100315     C                   if        %open(fivabwwr)
011200100315     C                   close     fivabwwr
011300081014     C                   endif
011400100315     C                   if        %open(fivatwwr)
011500100315     C                   close     fivatwwr
011600081014     C                   endif
011700040119     C*
011800040119     C* Effettuo la chiamata al CLLE preposto
011900100315     C                   call(e)   'TITVVTC'
012000040510     C                   parm                    parccm
012100040119     C                   parm                    parmbr
012200040119     C                   parm      '2'           paropz
012300050201     C*
012400050201     C* Effettuo lancio TISI95 solo x chiusura
012500050201     C                   CLEAR                   TISI95DS
012600050201     C                   EVAL      I95TLA = 'C'
012700050201     C                   CALL      'TISI95R'
012800050201     C                   PARM                    TISI95DS
012900000616     C*
013000010601     C                   ENDSR
013100010601     C***
013200000613
013300010601
013400010601
013500010330     C*--------------------------------------------------------
013600100315     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013700010330     C*--------------------------------------------------------
013800010605     C     RWVAB         BEGSR
013900010330     C*
014000010330     C                   if        not %open(tivin00r)
014100010330     C                   open      tivin00r
014200010330     C                   endif
014300100315     C                   if        not %open(fivabwwr)
014400100315     C                   open      fivabwwr
014500010330     C                   endif
014600040119     C*
014700100315     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014800040119     C                   exsr      prevat
014900040119     C*
015000040119     C                   if        chkcall = '0'
015100040119     C*
015200100315     C                   if        not %open(fivatwwr)
015300100315     C                   open      fivatwwr
015400040119     C                   endif
015500010330     C*
015600040119     C                   clear                   §CTROKVB          7 0
015700040119     C                   clear                   §CTROKVT          7 0
015800040119     C                   clear                   §CTRMO            7 0
015900040119     C                   clear                   §CTRNO            7 0
016000070823     C*
016100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016300100302     C***                exsr      inzvar
016400100302     C***                exsr      impvab
016500010330     C*
016600030822     C                   do        *HIVAL
016700010330     C*
016800030822     C                   read      tivin00r                               70
016900010618     C*
017000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017100010618     C                   if        vinflg = '1'
017200010618     C                   eval      flgOk = '1'
017300010618     C                   endif
017400040510     C*
017500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017600040510     C                   z-add     *zeros        wLenVINDTA        4 0
017700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018100040510     C                   z-add     1             wFlgVINDTA
018200040510     C                   leave
018300040510     C                   endif
018400040510     C                   enddo
018500090420     C*
018600090420     C                   add       1             rrnum
018700010618     C*
018800040510     C                   if        vindta > *blanks AND
018900090420     C                             wFlgVINDTA = 1
019000010330     C*
019100010601     C                   if        *in70 = *off and
019200010330     C                             (vinflg = *blanks
019300010330     C                              or vinflg = '0'
019400010330     C                              or vinflg = '2')
019500010330     C*
019600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019700010711     C                   if        vinflg = *blanks or vinflg = '0'
019800010711     C                   clear                   vinmsg
019900010711     C                   endif
020000100204     C*
020100100204     C                   exsr      inzvar
020200100204     C                   exsr      normCSV
020300010605     C                   exsr      impvab
020400010601     C*
020500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020600010605     C                   eval      depfil = VABLNP
020700010601     C                   exsr      repfil
020800010601     C                   if        depfil = invfil
020900021025     C                   if        vlrpoi = 999
021000030822     C                   move(P)   invfil        VABFGS
021100021025     C                   else
021200030822     C                   move(P)   vlrpoi        VABFGS
021300021025     C                   endif
021400010601     C*
021500010601     C                   exsr      PREELA
021600010601     C*
021700010604     C* Ebbene...
021800010604     C*
021900040119     C  N31              add       1             §CTROKVB
022000040119     C   32              add       1             §CTRMO
022100040119     C   31              add       1             §CTRNO
022200030822     C                   if        wGiro = 2
022300040510     C* Scrivo sempre il VAT
022400040510     C  N31              exsr      wrivat
022500040510     C* Scrivo il VAB solo a rottura d spedizione
022600040119     C                   if        depspe =  *zeros OR
022700040119     C                             depspe <> vabNSP
022800040119     C  N31              exsr      wrivab
022900040119     C  N31              eval      depspe = vabNSP
023000040119     C                   endif
023100040119     C                   endif
023200010604     C*
023300010604     C                   if        *in31 = *off and
023400010604     C                             *in32 = *off
023500010604     C                   eval      vinflg = '1'
023600010604     C                   else
023700010604     C                   eval      vinflg = '2'
023800010604     C                   endif
023900010604     C                   endif
024000010601     C*
024100010604     C                   endif
024200010604     C*
024300010330     C                   else
024400010330     C                   eval      vinflg = '1'
024500010330     C                   endif
024600010601     C*
024700010601     C  N70              update    tivin000
024800010330     C*
024900030822     C  N70              enddo
025000040119     C*
025100040119     C                   endif                                                  (endif - chkcall)
025200010601     C*
025300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025400010601     C                   if        cntNonEl = *zeros or
025500010601     C                             flgMulti = '0'
025600010330     C* Se non ci sono record con errori ...
025700010601     C                   if        §ctrno = 0 and
025800010604     C                             §ctrmo = 0 and
025900010601     C                             flgStato <> '2'
026000010330     C* ... restituisco esito OK.
026100010330     C                   eval      wrkesito = '0'
026200010330     C                   else
026300040119     C                   if        §ctrokvb > 0
026400010330     C                   eval      wrkesito = '1'
026500010330     C                   else
026600010615     C                   if        flgOk = '0'
026700010615     C                   eval      wrkesito = '2'
026800010615     C                   else
026900010615     C                   eval      wrkesito = '6'
027000010615     C                   endif
027100010330     C                   endif
027200010330     C                   endif
027300010601     C                   else
027400010601     C                   eval      wrkesito = '9'
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        %open(tivin00r)
027800010330     C                   close     tivin00r
027900010330     C                   endif
028000100315     C                   if        %open(fivabwwr)
028100100315     C                   close     fivabwwr
028200010330     C                   endif
028300100315     C                   if        %open(fivatwwr)
028400100315     C                   close     fivatwwr
028500040512     C                   endif
028600010601     C*
028700010601     C                   if        vlrpoi <> 999
028800010601     C                   eval      invfil = vlrpoi
028900010601     C                   endif
029000010330     C*
029100040119     C                   if        §ctrokvb > 0
029200010601     C                             and invfil > *zeros
029300010330     C                   exsr      invio
029400010330     C                   endif
029500010330     C*
029600010330     C                   ENDSR
029700010330     C***
029800040119
029900040119
030000040119
030100040119     C*----------------------------------------------------*
030200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
030300040119     C*----------------------------------------------------*
030400040510     C     WRIVAB        BEGSR
030500040119     C*
030600060331     C* Gestisco l'eventuale rottura x numero spedizione
030700070823     C                   if        VABNSP <> depspe
030800070301     C                   if        wVATNOT_E <> *blanks
030900080125     C                   if        VABCTM = *blanks
031000080125     C                   eval      VABCTM = '7Q'
031100080125     C                   endif
031200070301     C                   endif
031300100302     C*
031400100315     C                   write     FIVAB000                                     => scarico il VAB
031500060331     C                   endif
031600040119     C*
031700040119     C                   ENDSR
031800040119
031900040119
032000040119
032100040119     C*----------------------------------------------------*
032200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
032300040119     C*----------------------------------------------------*
032400040510     C     WRIVAT        BEGSR
032500060512     C*
032600060512     C* Inizializzo i campi chiave
032700060512     C                   eval      VATFGS = VABFGS
032800060512     C                   eval      VATAAS = VABAAS
032900060512     C                   eval      VATCCM = VABCCM
033000070823     C                   eval      VATNRS = VABNRS
033100060512     C                   eval      VATNSP = VABNSP
033200060512     C                   eval      VATLNP = VABLNP
033300060331     C*
033400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033500060331     C                   if        wVATNOT_A <> *blanks
033600060331     C                   eval      VATNOT = wVATNOT_A
033700060331     C                   eval      VATTRC = 'A'
033800100315     C                   write     FIVAT000
033900060331     C                   add       1             §CTROKVT
034000060331     C                   endif
034100060331     C*
034200060331     C                   if        wVATNOT_B <> *blanks
034300060331     C                   eval      VATNOT = wVATNOT_B
034400060331     C                   eval      VATTRC = 'B'
034500100315     C                   write     FIVAT000
034600060331     C                   add       1             §CTROKVT
034700060331     C                   endif
034800060331     C*
034900080625     C                   if        wVATNOT_E <> *blanks
035000060331     C                   eval      VATNOT = wVATNOT_E
035100060331     C                   eval      VATTRC = 'E'
035200100315     C                   write     FIVAT000
035300080716     C                   add       1             §CTROKVT
035400080625     C                   endif
035500040119     C*
035600040119     C                   ENDSR
035700040119
035800040119
035900040119
036000040119     C*----------------------------------------------------*
036100100315     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036200040119     C*----------------------------------------------------*
036300040119     C     PREVAT        BEGSR
036400040119     C*
036500100315     C* Compongo il nome del membro da dare al FIVATWWR
036600040119     C                   eval      parmbr = vlrhdl
036700040119     C                   movel     'M'           parmbr
036800040510     C                   eval      parccm = vlrksc
036900040119     C                   eval      paropz = '1'
037000040119     C* Effettuo la chiamata al CLLE preposto
037100100315     C                   call(e)   'TITVVTC'
037200040119     C                   parm                    parccm
037300040119     C                   parm                    parmbr
037400040119     C                   parm                    paropz
037500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
037600040119     C                   if        %error
037700040119     C                   movel     '1'           chkcall
037800040119     C                   else
037900040119     C                   movel     '0'           chkcall
038000040119     C                   endif
038100040119     C*
038200040119     C                   ENDSR
038300010601
038400010601
038500010601
038600010330     C*----------------------------------------------------*
038700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
038800010330     C*----------------------------------------------------*
038900040119     C     INZVAR        BEGSR
039000010330     C*
039100030822     C* Inizializzo variabili di wrk
039200020204     C                   Z-ADD     *zeros        Num5_0            5 0
039300060331     C                   MOVEL     *blanks       wVATNOT_A
039400060331     C                   MOVEL     *blanks       wVATNOT_B
039500060331     C                   MOVEL     *blanks       wVATNOT_E
039600090220     C*
039700080617     C                   EVAL      wFlgCAS = *blanks
039800030822     C*
039900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040000100315     C                   CLEAR                   FIVAB000
040100100315     C                   CLEAR                   FIVAT000
040200030822     C                   CLEAR                   SkSplitCSV
040300030822     C                   EVAL      i = 1
040400030822     C                   EVAL      posDa = *zeros
040500030822     C                   EVAL      posA  = *zeros
040600030822     C*
040700030822     C* Reimposto i valori di default
040800030822     C                   EXSR      DEFCAM
040900030822     C*
041000010330     C                   ENDSR
041100010330     C*----------------------------------------------------*
041200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
041300010330     C*----------------------------------------------------*
041400010330     C     DEFCAM        BEGSR
041500030822     C*
041600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
041700030715     C* e delimitatore testo.
041800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
041900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
042000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042100100204     C*
042200100204     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
042300100204     C                   EVAL      CharSOS = CharNUM
042400030822     C*
042500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
042600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
042700030715     C                   SETON                                        50
042800030715     C                   ELSE
042900030715     C                   SETOFF                                       50
043000030715     C                   ENDIF
043100090421     C*
043200090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
043300090421     C                   EVAL      VABCBO = '1'
043400090421     C                   EVAL      VABLNP = vlrpoi
043500090421     C                   EVAL      VABNSP = rrnum
043600030822     C*
043700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043800030822     C                   EVAL      posDaDft = 1
043900030822     C                   EVAL      posADft  = 0
044000030822     C                   EVAL      wGiroDft = 0
044100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044200030822     C                             posDaDft > 0
044300030822     C*
044400030822     C* Gestisco il 1° giro
044500030822     C                   IF        wGiroDft = 0
044600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044800030822     C* Incremento il contatore dei "giri"
044900030822     C                   EVAL      wGiroDft = 1
045000030822     C                   ELSE
045100030822     C                   EVAL      posDaDft = posADft
045200030822     C                   ENDIF
045300030822     C* Eseguo lo scan x trovare la fine del campo corrente
045400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045500030822     C*
045600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045800030822     C                   IF        posDaDft > 0 AND
045900030822     C                             posADft  > 0
046000030822     C* NCL
046100030822     C                   IF        %subst(
046200030822     C                             %subst(vlrppt:posDaDft+1:
046300030822     C                             posADft-posDaDft-1):1:3)
046400030822     C                             = 'NCL'
046500030822     C                   EVAL      PiStr=%trim(%subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046800030822     C                   EXSR      CHKNUM
046900030822     C                   IF        PiInt=*on
047000030822     C                   Z-ADD     PiVal         VABNCL
047100030822     C                   ENDIF
047200030822     C                   ENDIF
047300030822     C* CCM
047400030822     C                   IF        %subst(
047500030822     C                             %subst(vlrppt:posDaDft+1:
047600030822     C                             posADft-posDaDft-1):1:3)
047700030822     C                             = 'CCM'
047800030822     C                   EVAL      PiStr=%trim(%subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048100030822     C                   EXSR      CHKNUM
048200030822     C                   IF        PiInt=*on
048300030822     C                   Z-ADD     PiVal         VABCCM
048400030822     C                   ENDIF
048500030822     C                   ENDIF
048600030822     C* LNP
048700030822     C                   IF        %subst(
048800030822     C                             %subst(vlrppt:posDaDft+1:
048900030822     C                             posADft-posDaDft-1):1:3)
049000030822     C                             = 'LNP'
049100030822     C                   EVAL      PiStr=%trim(%subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049400030822     C                   EXSR      CHKNUM
049500030822     C                   IF        PiInt=*on
049600030822     C                   Z-ADD     PiVal         VABLNP
049700030822     C                   ENDIF
049800030822     C                   ENDIF
049900030822     C* NRS
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'NRS'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiInt=*on
050900030822     C                   Z-ADD     PiVal         VABNRS
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C* CTR
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'CTR'
051700030822     C                   EVAL      PiStr=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   EXSR      CHKNUM
052100030822     C                   IF        PiInt=*on
052200030822     C                   Z-ADD     PiVal         VABCTR
052300030822     C                   ENDIF
052400030822     C                   ENDIF
052500030822     C* PKB
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'PKB'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiNum=*on
053500030822     C                   Z-ADD     PiVal         VABPKB
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* VLB
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'VLB'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiNum=*on
054800030822     C                   Z-ADD     PiVal         VABVLB
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* QFT
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'QFT'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiNum=*on
056100030822     C                   Z-ADD     PiVal         VABQFT
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CBO
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CBO'
056900030822     C                   EVAL      VABCBO=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   ENDIF
057300030822     C* TSP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'TSP'
057800030822     C                   EVAL      VABTSP=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   ENDIF
058200030822     C* VAS
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'VAS'
058700030822     C                   EVAL      VABVAS=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   ENDIF
059100030822     C* VCA
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'VCA'
059600030822     C                   EVAL      VABVCA=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   ENDIF
060000070925     C* TIC
060100070925     C                   IF        %subst(
060200070925     C                             %subst(vlrppt:posDaDft+1:
060300070925     C                             posADft-posDaDft-1):1:3)
060400070925     C                             = 'TIC'
060500070925     C                   EVAL      VABTIC=%trim(%subst(
060600070925     C                             %subst(vlrppt:posDaDft+1:
060700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800070925     C                   ENDIF
060900030822     C* GCA
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'GCA'
061400030822     C                   EVAL      VABGCA=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   ENDIF
061800030822     C* CTM
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'CTM'
062300030822     C                   EVAL      VABCTM=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   ENDIF
062700030822     C* FFD
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'FFD'
063200030822     C                   EVAL      VABFFD=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   ENDIF
063600030822     C* VAD
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'VAD'
064100030822     C                   EVAL      VABVAD=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   ENDIF
064500030822     C* GMA
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'GMA'
065000030822     C                   EVAL      VABGMA=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   ENDIF
065400030822     C* GGA
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'GGA'
065900030822     C                   EVAL      VABGGA=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   ENDIF
066300030822     C* GVA
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'GVA'
066800030822     C                   EVAL      VABGVA=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   ENDIF
067200030822     C* TC1
067300030822     C                   IF        %subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):1:3)
067600030822     C                             = 'TC1'
067700030822     C                   EVAL      VABTC1=%trim(%subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068000030822     C                   ENDIF
068100030822     C* TC2
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'TC2'
068600030822     C                   EVAL      VABTC2=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   ENDIF
069000040714     C* VATTRC
069100040714     C                   IF        %subst(
069200040714     C                             %subst(vlrppt:posDaDft+1:
069300040714     C                             posADft-posDaDft-1):1:3)
069400040714     C                             = 'TRC'
069500040714     C                   EVAL      VATTRC=%trim(%subst(
069600040714     C                             %subst(vlrppt:posDaDft+1:
069700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800040714     C                   ENDIF
069900030822     C* ...
070000030822     C                   ENDIF
070100030822     C                   ENDDO
070200020204     C*
070300010330     C                   ENDSR
070400010607     C*----------------------------------------------------*
070500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070600010607     C*----------------------------------------------------*
070700010607     C     IMPVAB        BEGSR
070800010607     C*
070900010607     C                   SETOFF                                       3132
071000030515     C*
071100010607     C                   MOVEL     datcor        VABAAS
071200010607     C                   MOVE      datcor        VABMGS
071300010607     C*
071400030715     C**********
071500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071600030715     C**********
071700030715     C                   IF        wGiro = *zeros
071800080716     C*
071900080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
072000080716     C                   EVAL      depvinDTA = vinDTA
072100040113     C*
072200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
072300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
072400090716     C                             %trim(intcol(3))+%trim(intcol(4))+
072500090716     C                             %trim(intcol(5))<>*blanks
072600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
072700090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
072800090716     C                                      %trim(intcol(5))
072900100204     C     ';':CharCSV   XLATE     vindta        vindta
073000040510     C                   ENDIF
073100080626     C*
073200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
073300031201     C     minu:maiu     XLATE     vindta        vindta
073400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073500030715     C                   DOW       posDa <= %len(%trim(vindta))
073600030715     C*
073700030715     C* Gestisco il 1° campo
073800030715     C                   IF        i = 1
073900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074000030715     C                   EVAL      posDa = 1
074100030715     C* Eseguo lo scan x trovare la fine del primo campo
074200100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074300030715     C* A questo punto "estraggo" il campo corrente
074400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074500030715     C                                                    (posA-posDa))
074600030715     C* X i campi successivi al 1°
074700030715     C                   ELSE
074800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074900030715     C                   EVAL      posDa = posA + 1
075000030715     C* Eseguo lo scan x trovare la fine del campo corrente
075100100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075200030715     C* Gestisco l'ultimo campo
075300030715     C                   IF        posA = *zeros
075400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
075500030715     C* A questo punto "estraggo" il campo corrente
075600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075700030715     C                                                    (posA-posDa))
075800030715     C                   LEAVE
075900030715     C                   ELSE
076000030715     C* A questo punto "estraggo" il campo corrente
076100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076200030715     C                                                    (posA-posDa))
076300030715     C                   ENDIF
076400030715     C                   ENDIF
076500030715     C* Incremento il contatore d campo
076600030715     C                   EVAL      i = i +1
076700030715     C                   ENDDO
076800030715     C*
076900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077000030715     C* delimitazione testo
077100030715     C                   EVAL      i = 1
077200030715     C                   DOW       i < %elem(SkSplitFLD)
077300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
077400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
077500030715     C                   EVAL      i = i + 1
077600030715     C                   ENDDO
077700030715     C                   EVAL      wGiro = 1
077800080716     C* Ripristino la 1° riga originale
077900080716     C                   EVAL      vinDTA = depvinDTA
078000030715     C                   ELSE
078100030715     C                   EVAL      wGiro = 2
078200031204     C**********
078300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
078400031204     C* d separatore campo
078500031204     C**********
078600031204     C                   EVAL      vindta = %trim(vindta)
078700100204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
078800031204     C                   EVAL      vindta = %subst(vindta:2)
078900031204     C                   ENDDO
079000031204     C*
079100031204     C                   Z-ADD     *zeros        lunghInput        4 0
079200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079300100204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
079400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
079500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079600031204     C                   ENDDO
079700030715     C**********
079800030715     C* Effettuo lo split del campo dati d input
079900030715     C**********
080000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080100030715     C                   DOW       posDa <= %len(%trim(vindta))
080200030715     C*
080300030715     C* Gestisco il 1° campo
080400030715     C                   IF        i = 1
080500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080600030715     C                   EVAL      posDa = 1
080700030715     C* Eseguo lo scan x trovare la fine del primo campo
080800100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080900030715     C* A questo punto "estraggo" il campo corrente
081000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081100030715     C                                                    (posA-posDa))
081200030715     C* X i campi successivi al 1°
081300030715     C                   ELSE
081400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081500030715     C                   EVAL      posDa = posA + 1
081600031204     C* Verifico che nn vi sia il campo nn valorizzato
081700100204     C                   IF        %subst(vindta:posDa:1) = CharCSV
081800031204     C* Se campo nn valorizzato skippo al prossimo
081900031204     C                   EVAL      SkSplitCSV(i) = *blanks
082000031204     C                   EVAL      posA  = posA + 1
082100031204     C                   ELSE
082200030715     C* Eseguo lo scan x trovare la fine del campo corrente
082300100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* Gestisco l'ultimo campo
082500030715     C                   IF        posA = *zeros
082600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   LEAVE
083100030715     C                   ELSE
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   ENDIF
083600031204     C                   ENDIF
083700030715     C                   ENDIF
083800030715     C* Incremento il contatore d campo
083900030715     C                   EVAL      i = i +1
084000030715     C                   ENDDO
084100030715     C*
084200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084300030715     C* delimitazione testo
084400030715     C                   EVAL      i = 1
084500030715     C                   DOW       i < %elem(SkSplitCSV)
084600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
084700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
084800030715     C                   EVAL      i = i + 1
084900030715     C                   ENDDO
085000030715     C*
085100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitFLD)
085400030715     C*
085500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
085600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
085700030715     C                             SkSplitCSV(i) <> *blanks
085800030721     C*** ATB
085900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
086000030721     C                   EVAL      VABATB = SkSplitCSV(i)
086100030721     C                   ENDIF
086200030715     C*** RSD
086300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
086400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
086500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
086600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
086700030715     C* ==
086800030715     C                   ENDIF
086900030721     C*** RD2
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
087100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030715     C*** IND
087400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
087500030715     C                   EVAL      VABIND = SkSplitCSV(i)
087600030715     C                   ENDIF
087700030715     C*** LOD
087800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
087900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
088000030715     C                   ENDIF
088100030715     C*** PRD
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
088300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
088400030715     C                   ENDIF
088500030721     C*** NZD
088600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
088700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
088800030721     C                   ENDIF
088900030715     C*** RMA
089000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
089100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
089200030715     C                   ENDIF
089300030715     C*** NOT
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
089500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
089600030715     C                   ENDIF
089700030715     C*** NT2
089800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
089900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
090000030715     C                   ENDIF
090100030715     C*** VCA
090200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
090300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
090400030715     C                   ENDIF
090500030715     C*** VAS
090600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
090700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
090800030715     C                   ENDIF
090900030715     C*** FFD
091000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
091100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
091200030715     C                   ENDIF
091300030721     C*** GC1
091400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
091500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
091600030721     C                   ENDIF
091700030721     C*** GC2
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
091900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030721     C*** TSP
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
092300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
092400030721     C                   ENDIF
092500040510     C*** CBO
092600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
092700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
092800040510     C                   ENDIF
092900030721     C*** NAS
093000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
093200030721     C                   ENDIF
093300030721     C*** TIC
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
093500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030721     C*** GCA
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
093900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030721     C*** XCO
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
094300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030721     C*** CTM
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
094700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030721     C*** TCR
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
095200030721     C                   ENDIF
095300030721     C*** CTS
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
095500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030721     C*** FTM
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
095900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030721     C*** VAD
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
096300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030721     C*** GMA
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
096700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030721     C*** GGA
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
097200030721     C                   ENDIF
097300030721     C*** GVA
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
097500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** TC1
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
097900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** TC2
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
098300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** SCL
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
098700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** RMO
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** NMO
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
099500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*
099800030721     C* Reperisco quindi i campi numerici...
099900040510     C*** NSP
100000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
100100040510     C                   EVAL      PiStr=SkSplitCSV(i)
100200040510     C                   EXSR      CHKNUM
100300040510     C                   IF        PiInt=*on
100400040510     C                   Z-ADD     PiVal         VABNSP
100500040510     C                   ELSE
100600040510     C                   SETON                                        31
100700040510     C                   EVAL      VABNSP = *zeros
100800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
100900040510     C                             + ' ' + 'VABNSP'
101000040510     C                   ENDIF
101100040510     C                   ENDIF
101200071106     C*** RMN
101300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
101400071106     C                   EVAL      PiStr=SkSplitCSV(i)
101500071106     C                   EXSR      CHKNUM
101600071106     C                   IF        PiInt=*on
101700071106     C                   Z-ADD     PiVal         VABRMN
101800071106     C                   ELSE
101900071106     C                   SETON                                        32
102000071106     C                   EVAL      VABRMN = *zeros
102100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
102200071106     C                             + ' ' + 'VABRMN'
102300071106     C                   ENDIF
102400071106     C                   ENDIF
102500030721     C*** CAD
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
102700030721     C                   EVAL      PiStr=SkSplitCSV(i)
102800030721     C                   EXSR      CHKNUM
102900030721     C                   IF        PiInt=*on
103000030721     C                   Z-ADD     PiVal         Num5_0
103100030721     C                   MOVEL(p)  Num5_0        VABCAD
103200030721     C                   ELSE
103300090603     C                   EVAL      VABCAD = SkSplitCSV(i)
103400090603     C                   SETON                                        32
103500090603     C                   EVAL      VABCAD = *zeros
103600090603     C                   EVAL      vinmsg = %trimr(vinmsg)
103700090603     C                             + ' ' + 'VABCAD'
103800030721     C                   ENDIF
103900030721     C                   ENDIF
104000030721     C*** NCL
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104200030721     C                   EVAL      PiStr=SkSplitCSV(i)
104300030721     C                   EXSR      CHKNUM
104400030721     C                   IF        PiInt=*on
104500030721     C                   Z-ADD     PiVal         VABNCL
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABNCL = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000040122     C                             + ' ' + 'VABNCL'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** PKB
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiNum=*on
105800030721     C                   Z-ADD     PiVal         VABPKB
105900030721     C                   ELSE
106000030721     C                   SETON                                        32
106100030721     C                   EVAL      VABPKB = *zeros
106200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106300030721     C                             + ' ' + 'VABPKB'
106400030721     C                   ENDIF
106500030721     C                   ENDIF
106600030721     C*** CAS
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
106800080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
106900080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
107000081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
107100090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
107200090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
107300090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
107400090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
107500090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
107600090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
107700090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
107800081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
107900080125     C                   EVAL      wFlgCAS = '1'
108000080125     C                   ENDIF
108100030721     C                   EVAL      PiStr=SkSplitCSV(i)
108200030721     C                   EXSR      CHKNUM
108300030721     C                   IF        PiNum=*on
108400030721     C                   Z-ADD     PiVal         VABCAS
108500030721     C                   ELSE
108600030721     C                   SETON                                        32
108700030721     C                   EVAL      VABCAS = *zeros
108800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108900030721     C                             + ' ' + 'VABCAS'
109000030721     C                   ENDIF
109100030721     C                   ENDIF
109200030721     C*** CCM
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiInt=*on      AND
109700030721     C                             PiVal<=9999999 AND
109800030721     C                             PiVal>*zeros
109900030721     C                   Z-ADD     PiVal         VABCCM
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      VABCCM = *zeros
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400040510     C                             + ' ' + 'VABCCM'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** LNP
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on    AND
111200030721     C                             PiVal<=999   AND
111300030721     C                             PiVal>*zeros
111400030721     C                   Z-ADD     PiVal         VABLNP
111500030721     C                   ELSE
111600040715     C                   SETON                                        32
111700030721     C                   EVAL      VABLNP = *zeros
111800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111900040510     C                             + ' ' + 'VABLNP'
112000030721     C                   ENDIF
112100030721     C                   ENDIF
112200030721     C*** NRS
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112400030721     C                   EVAL      PiStr=SkSplitCSV(i)
112500030721     C                   EXSR      CHKNUM
112600030721     C                   IF        PiInt=*on AND
112700030721     C                             PiVal<=99
112800030721     C                   Z-ADD     PiVal         VABNRS
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABNRS'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** LNA
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on    AND
114000030721     C                             PiVal<=999   AND
114100030721     C                             PiVal>*zeros
114200030721     C                   Z-ADD     PiVal         VABLNA
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABLNA = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABLNA'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CTR
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on  AND
115500030721     C                             PiVal<=999
115600030721     C                   Z-ADD     PiVal         VABCTR
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABCTR'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** IAS
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABIAS
116900030721     C                   ELSE
117000030721     C                   SETON                                        32
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200030721     C                             + ' ' + 'VABIAS'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** VLB
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiNum=*on
118000030721     C                   Z-ADD     PiVal         VABVLB
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400030721     C                             + ' ' + 'VABVLB'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** QFT
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABQFT
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABQFT'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** NCD
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on      AND
120400030721     C                             PiVal<=9999999
120500030721     C                   Z-ADD     PiVal         VABNCD
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABNCD'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** NCA
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on      AND
121700030721     C                             PiVal<=9999999
121800030721     C                   Z-ADD     PiVal         VABNCA
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABNCA'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** ZNC
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on AND
123000030721     C                             PiVal<=99
123100030721     C                   Z-ADD     PiVal         VABZNC
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABZNC'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** DCR
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on       AND
124300030721     C                             PiVal<=99999999
124400030721     C                   Z-ADD     PiVal         VABDCR
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABDCR'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** HCR
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on   AND
125600030721     C                             PiVal<=9999
125700030721     C                   Z-ADD     PiVal         VABHCR
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABHCR'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VMD
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABVMD
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVMD'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** ANT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on        AND
128100030721     C                             PiVal<=999999999
128200030721     C                   Z-ADD     PiVal         VABANT
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABANT'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** CMO
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on
129400030721     C                   Z-ADD     PiVal         Num5_0
129500030721     C                   MOVEL(p)  Num5_0        VABCMO
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      VABCMO = *zeros
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABCMO'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300040510     C*
130400040510     C* Reperisco i campi del FIVAT
130500040510     C*** VATNOT
130600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
130800040510     C                   ENDIF
130900040510     C*** VATTRC
131000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
131100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
131200040510     C                   ENDIF
131300060331     C*** VATNOT_A
131400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
131500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
131600060331     C                   ENDIF
131700060331     C*** VATNOT_B
131800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
131900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
132000060331     C                   ENDIF
132100060331     C*** VATNOT_E
132200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
132300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
132400060331     C                   ENDIF
132500070823     C*
132600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
132700090421     C*
132800100315     C*** W_VABNSP/VABRMN
132900090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
133000090622     C                   EVAL      VABRMA = SkSplitCSV(i)
133100090622     C                   EVAL      PiStr=SkSplitCSV(i)
133200090622     C                   EXSR      CHKNUM
133300090622     C                   IF        PiInt=*on
133400090622     C                   Z-ADD     PiVal         VABRMN
133500090622     C                   Z-ADD     PiVal         VABNSP
133600090622     C                   ELSE
133700090622     C                   SETON                                        32
133800090622     C                   EVAL      VABRMN = *zeros
133900090622     C                   EVAL      VABNSP = *zeros
134000090622     C                   EVAL      vinmsg = %trimr(vinmsg)
134100090622     C                             + ' ' + 'VABRMN VABNSP'
134200090622     C                   ENDIF
134300090603     C                   ENDIF
134400100315     C*** TIC
134500100315     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
134600100315     C                   EVAL      VABTIC = SkSplitCSV(i)
134700100315     C                   IF        VABTIC <> *blanks
134800100315     C                   EVAL      wFlgCAS = '1'
134900100315     C                   ENDIF
135000100315     C                   ENDIF
135100070925     C*
135200071029     C***  <===  -----------------------------------------
135300040510     C*
135400030715     C                   ENDIF
135500030715     C                   EVAL      i = i + 1
135600030715     C                   ENDDO
135700070925     C*
135800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
135900080125     C*
136000070925     C*
136100071029     C***  <===  -----------------------------------------
136200070925     C*
136300070925     C* Considerazioni finali su CBO/CAS
136400080125     C                   IF        wFlgCAS = '1'
136500070925     C                   IF        VABCBO = '1'
136600070925     C                   EVAL      VABCBO = '4'
136700070925     C                   ENDIF
136800070925     C                   IF        VABCBO = '2'
136900070925     C                   EVAL      VABCBO = '6'
137000070925     C                   ENDIF
137100070925     C                   ENDIF
137200070925     C***  <===  ----------------------------
137300040510     C*
137400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
137500040510     C                   IF        VABNSP = *zeros
137600040510     C                   SETON                                        31
137700040510     C                   ENDIF
137800040715     C*
137900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
138000040715     C                   IF        VABLNP = *zeros
138100040715     C                   SETON                                        31
138200040715     C                   ENDIF
138300040510     C*
138400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
138500040510     C                   IF        VABRMN = *zeros
138600040510     C                   Z-ADD     VABNSP        VABRMN
138700040510     C                   ENDIF
138800040510     C*
138900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
139000040510     C                   IF        VABRMA = *blanks
139100040714     C                   MOVEL     VABRMN        VABRMA
139200040510     C                   ENDIF
139300040128     C*
139400040510     C* Se provincia nn valorizzata la reperisco
139500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
139600040510     C                   IF        VABPRD = *blanks
139700040128     C                   CLEAR                   TISI95DS
139800040128     C                   EVAL      I95TCN = '3'
139900040128     C                   Z-ADD     datcor        I95DAT
140000040128     C                   EVAL      I95NAR = VABNZD
140100040128     C                   EVAL      I95CAP = VABCAD
140200040128     C                   EVAL      I95LOC = VABLOD
140300040128     C                   CALL      'TISI95R'
140400040128     C                   PARM                    TISI95DS
140500040128     C                   EVAL      VABPRD = O95PRV
140600040510     C                   ENDIF
140700020204     C*
140800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140900020204     C                   EXSR      CHKIMPDIV
141000030715     C                   ENDIF
141100020204     C*
141200010607     C                   ENDSR
141300010607     C*----------------------------------------------------*
141400010601
141500020204
141600020204     C*----------------------------------------------------*
141700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141800020204     C*----------------------------------------------------*
141900020204     C     CHKIMPDIV     BEGSR
142000020204     C*
142100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
142200020204     C                   Z-ADD     *zeros        wrkDec            9 9
142300020204     C*
142400020204     C* Come prima cosa effettuo considerazioni sulla divisa
142500020204     C                   IF        vabIAS > *zeros
142600020204     C                   IF        vabVAS <> 'EUR'
142700020204     C                   EVAL      vabVAS =  'ITL'
142800020204     C                   ENDIF
142900020204     C                   ENDIF
143000020204     C*
143100020204     C                   IF        vabCAS > *zeros
143200020204     C                   IF        vabVCA <> 'EUR'
143300020204     C                   EVAL      vabVCA =  'ITL'
143400020204     C                   ENDIF
143500020204     C                   ENDIF
143600020204     C*
143700020204     C                   IF        vabVMD > *zeros
143800020204     C                   IF        vabVAD <> 'EUR'
143900020204     C                   EVAL      vabVAD =  'ITL'
144000020204     C                   ENDIF
144100020204     C                   ENDIF
144200020204     C*
144300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144400020204     C                   Z-ADD     vabIAS        wrkDec
144500020204     C                   IF        wrkDec > *zeros
144600020204     C                   IF        vabVAS = 'ITL'
144700020204     C                   EVAL      vabIAS = *zeros
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
145200020204     C                   Z-ADD     vabCAS        wrkDec
145300020204     C                   IF        wrkDec > *zeros
145400020204     C                   IF        vabVCA = 'ITL'
145500020204     C                   EVAL      vabCAS = *zeros
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
146000020204     C                   Z-ADD     vabVMD        wrkDec
146100020204     C                   IF        wrkDec > *zeros
146200020204     C                   IF        vabVAD = 'ITL'
146300020204     C                   EVAL      vabVMD = *zeros
146400020204     C                   ENDIF
146500020204     C                   ENDIF
146600020204     C*
146700020204     C                   ENDSR
146800020204     C***
146900080617
147000080617
147100080617
147200080617     C*----------------------------------------------------*
147300080617     C*  CONTROLLO NUMERICITA' CAMPI
147400080617     C*----------------------------------------------------*
147500080617     C     CHKNUM        BEGSR
147600080617     C*
147700080617     C                   IF        PiDecChr = *blanks
147800080617     C                   EVAL      PiDecChr = CharNUM
147900080617     C                   ENDIF
148000080617     C*
148100080617     C                   callp     UBISNUM_Check(PiStr
148200080617     C                                          :PiDecChr
148300080617     C                                          :PiVal
148400080617     C                                          :PiNum
148500080617     C                                          :PiInt)
148600080617     C*
148700080617     C                   ENDSR
148800080617     C***
148900010330
149000010601
149100010601
149200010601
149300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149400010601     C     repfil        BEGSR
149500010601     C*
149600010601     C                   if        invfil = *zeros and
149700010601     C                             depfil > *zeros and
149800010629     C                             (vinflg = *blanks or
149900010629     C                              vinflg = *zeros)
150000010601     C
150100010601     C                   eval      invfil = depfil
150200010601     C                   endif
150300010601     C*
150400010601     C                   if        depfil <> invfil and
150500010601     C                             invfil > *zeros
150600010601     C                   eval      flgMulti = '1'
150700010601     C                   if        vinflg = *blanks
150800010601     C                   add       1             cntNonEl
150900010601     C                   endif
151000010601     C                   endif
151100010601     C*
151200010601     C                   if        vinflg = '2'
151300010601     C                   eval      flgStato = '2'
151400010601     C                   endif
151500010601     C*
151600010601     C                   ENDSR
151700010601     C***
151800010601
151900010601
152000010601
152100010330
152200040119      /TITLE Invio dei dati al punto operativo.
152300040119     C     invio         BEGSR
152400040119     C*
152500040119     C* 1° invio FIVAT
152600040119     C                   reset                   dscmz
152700040119     C                   move      vlrpoi        cmzdst
152800100315     C                   eval      cmzfld = 'FIVATWWR'
152900040119     C                   eval      cmzmbd = vlrhdl
153000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153100040119     C***                if        prmfir = *blanks
153200100315     C                   eval      cmzfla = 'FIVAT00F'
153300100315     C                   eval      cmzmba = 'FIVAT00F'
153400040119     C***                else
153500040119     C***                eval      cmzfla = prmfir
153600040119     C***                eval      cmzmba = prmfir
153700040119     C***                endif
153800040119     C                   eval      cmznrr = *zeros
153900040119     C                   move      §ctrokvt      cmznrr
154000040119     C                   eval      cmzlba = vlrfl1
154100040119     C                   call(e)   'TIS711C'
154200040119     C                   parm                    dscmz
154300040119     C                   parm      *blanks       esito
154400040119     C                   if        %error
154500040119     C                             or cmzerr = '1'
154600040119     C                             or esito  = '1'
154700040119     C                   eval      wrkesito = '3'
154800040119     C                   else
154900040119     C*
155000040119     C* 2° invio FIVAB
155100040119     C                   reset                   dscmz
155200040119     C                   move      vlrpoi        cmzdst
155300040119     C                   eval      cmzfld = vlrfou
155400040119     C                   eval      cmzmbd = vlrhdl
155500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155600040119     C***                if        prmfir = *blanks
155700100315     C                   eval      cmzfla = 'FIVAB00F'
155800100315     C                   eval      cmzmba = 'FIVAB00F'
155900040119     C***                else
156000040119     C***                eval      cmzfla = prmfir
156100040119     C***                eval      cmzmba = prmfir
156200040119     C***                endif
156300040119     C                   eval      cmznrr = *zeros
156400040119     C                   move      §ctrokvb      cmznrr
156500040119     C                   eval      cmzlba = vlrfl1
156600040119     C                   call(e)   'TIS711C'
156700040119     C                   parm                    dscmz
156800040119     C                   parm      *blanks       esito
156900040119     C                   if        %error
157000040119     C                             or cmzerr = '1'
157100040119     C                             or esito  = '1'
157200040119     C                   eval      wrkesito = '3'
157300040119     C                   endif
157400040119     C                   endif
157500040119     C*
157600040119     C                   ENDSR
157700040119     C***
157800010601
157900010601
158000010601
158100010601
158200010601      /TITLE Invio dei dati al punto operativo.
158300010601     C     opeini        BEGSR
158400010601     C*
158500010601     C* Inizializzo flag e contatori operativi
158600010601     C                   movel     '0'           flgGiro           1
158700010601     C                   movel     '0'           flgMulti          1
158800010601     C                   movel     '1'           flgStato          1
158900010615     C                   movel     '0'           flgOk             1
159000010601     C                   z-add     *zeros        cntNonEl         10 0
159100010601     C                   z-add     *zeros        depfil            3 0
159200010601     C                   z-add     *zeros        invfil            3 0
159300010601     C*
159400010601     C                   ENDSR
159500010601     C***
159600070326
159700070326
159800070326
159900070326
160000080916     C     *pssr         BEGSR
160100070329     C*
160200080916     C                   if        %open(tivin00r)
160300080916     C                   close     tivin00r
160400080916     C                   endif
160500100315     C                   if        %open(fivabwwr)
160600100315     C                   close     fivabwwr
160700080916     C                   endif
160800100315     C                   if        %open(fivatwwr)
160900100315     C                   close     fivatwwr
161000080916     C                   endif
161100070326     C*
161200070326     C* Effettuo la chiamata al CLLE preposto
161300100315     C                   call(e)   'TITVVTC'
161400080916     C                   parm                    parccm
161500080916     C                   parm                    parmbr
161600080916     C                   parm      '2'           paropz
161700070326     C*
161800080916     C                   eval      wrkesito = '2'
161900070404     C*
162000080916     C                   seton                                        LR
162100070326     C*
162200080916     C                   ENDSR     '*CANCL'
162300070326     C***
162400070326
162500100204
162600100204
162700100204     C     normCSV       BEGSR
162800100204     C*
162900100204     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
163000100204     C* separatori campi
163100100204     C                   if        CharTXT <> *blanks
163200100204     C                   z-add     *zeros        wLenVINDTA        4 0
163300100204     C                   z-add     *zeros        wFlgVINDTA        1 0
163400100204     C                   setoff                                       80
163500100204     C                   dow       wLenVINDTA < %len(%trim(vindta))
163600100204     C                   eval      wLenVINDTA = wLenVINDTA + 1
163700100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
163800100204     C                   if        not *in80
163900100204     C                   seton                                        80
164000100204     C                   else
164100100204     C                   setoff                                       80
164200100204     C                   endif
164300100204     C                   endif
164400100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
164500100204     C                             and *in80 = *on
164600100204     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
164700100204     C                   endif
164800100204     C                   enddo
164900100204     C                   endif
165000100204     C*
165100100204     C                   ENDSR
165200100204     C***
165300070326
165400010330
165500010330
165600000613     C     *inzsr        BEGSR
165700990910     C*
165800990910     C     *entry        plist
165900990920     C                   parm                    tivlrds
166000990921     C                   parm      wrkesito      esito
166100000724     C                   parm                    prmlit
166200000710     C                   parm                    prmfir
166300010330     C*
166400010330     C* CALCOLA LA DATA CORRENTE
166500100302     C                   time                    wn14             14 0
166600100302     C                   movel     wn14          oracor            6 0          *ORA
166700100302     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
166800100204     C                   eval      datcor = %dec(%date() : *ISO)
166900100302     C*
167000100302     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
167100100302     C                   MOVE(p)   *blanks       wNomeFile        30
167200100302     C                   MOVE(p)   vlrMSG        wNomeFile
167300100302     C     minu:maiu     XLATE     wNomeFile     wNomeFile
167400000613     C*
167500000613     C                   ENDSR
167600000613     C***
167700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167800100316X001;X002;VABRMA;W_VABNSP;VABNCL;VABPKB;VABQFT;W_VABTIC;X003;
167900100316VABCAS;X005;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_E;X006
168000100302
168100100302
168200100302
