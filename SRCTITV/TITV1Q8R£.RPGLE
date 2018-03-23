000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
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
004800140113     D wVATNOT_I       s                   LIKE(VATNOT)
004900140113     D wVATNOT_J       s                   LIKE(VATNOT)
005000140113     D wVATNOT_IJ      s             70
005100030822     D*------------------
005200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005300030822     D*------------------
005400030822     D posDaDft        S              3  0 INZ(*zeros)
005500030822     D posADft         S              3  0 INZ(*zeros)
005600030822     D j               s              3  0 INZ(1)
005700030822     D wGiroDft        s              1  0 INZ(*zeros)
005800031201     D*------------------
005900031201     D* Costanti
006000031201     D*------------------
006100031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006200031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006300080617     D*------------------
006400080617
006500080617     D*------------------
006600080617     D* LINKING A DEFINIZIONI ESTERNE
006700080617     D*------------------
006800080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007000010330
007100010330
007200010330
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500081014     C                   reset                   wrkesito
007600010601     C*
007700010601     C                   exsr      opeini
007800010605     C                   exsr      rwvab
007900081014     C*
008000081014     C                   exsr      endela
008100010601     C*
008200010601     C                   seton                                        lr
008300010601
008400010601
008500010601
008600010601     C*--------------------------------------------------------
008700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008800010601     C*--------------------------------------------------------
008900010601     C     PREELA        BEGSR
009000010601     C*
009100010601     C* SE OCCORRE SPEDIRE IN FILIALE
009200010601     C                   if        invfil <> *zeros and
009300010601     C                             flgGiro = '0'
009400010601     C*
009500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009600010601     C                   eval      flgGiro = '1'
009700010601     C*
009800010601     C                   endif
009900010601     C*
010000010601     C                   ENDSR
010100010601     C***
010200010601
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010601     C*--------------------------------------------------------
010900010601     C     ENDELA        BEGSR
011000081014     C*
011100081014     C                   if        %open(tivin00r)
011200081014     C                   close     tivin00r
011300081014     C                   endif
011400081014     C                   if        %open(fivabwwr)
011500081014     C                   close     fivabwwr
011600081014     C                   endif
011700081014     C                   if        %open(fivatwwr)
011800081014     C                   close     fivatwwr
011900081014     C                   endif
012000040119     C*
012100040119     C* Effettuo la chiamata al CLLE preposto
012200040119     C                   call(e)   'TITVVTC'
012300040510     C                   parm                    parccm
012400040119     C                   parm                    parmbr
012500040119     C                   parm      '2'           paropz
012600050201     C*
012700050201     C* Effettuo lancio TISI95 solo x chiusura
012800050201     C                   CLEAR                   TISI95DS
012900050201     C                   EVAL      I95TLA = 'C'
013000050201     C                   CALL      'TISI95R'
013100050201     C                   PARM                    TISI95DS
013200000616     C*
013300010601     C                   ENDSR
013400010601     C***
013500000613
013600010601
013700010601
013800010330     C*--------------------------------------------------------
013900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014000010330     C*--------------------------------------------------------
014100010605     C     RWVAB         BEGSR
014200010330     C*
014300010330     C                   if        not %open(tivin00r)
014400010330     C                   open      tivin00r
014500010330     C                   endif
014600021025     C                   if        not %open(fivabwwr)
014700021025     C                   open      fivabwwr
014800010330     C                   endif
014900040119     C*
015000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015100040119     C                   exsr      prevat
015200040119     C*
015300040119     C                   if        chkcall = '0'
015400040119     C*
015500040119     C                   if        not %open(fivatwwr)
015600040119     C                   open      fivatwwr
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300070823     C*
016400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016600090420     C                   exsr      inzvar
016700090420     C                   exsr      impvab
016800010330     C*
016900030822     C                   do        *HIVAL
017000010330     C*
017100030822     C                   read      tivin00r                               70
017200010618     C*
017300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017400010618     C                   if        vinflg = '1'
017500010618     C                   eval      flgOk = '1'
017600010618     C                   endif
017700040510     C*
017800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017900040510     C                   z-add     *zeros        wLenVINDTA        4 0
018000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018400040510     C                   z-add     1             wFlgVINDTA
018500040510     C                   leave
018600040510     C                   endif
018700040510     C                   enddo
018800090420     C*
018900090420     C                   add       1             rrnum
019000010618     C*
019100040510     C                   if        vindta > *blanks AND
019200090420     C                             wFlgVINDTA = 1
019300010330     C*
019400010601     C                   if        *in70 = *off and
019500010330     C                             (vinflg = *blanks
019600010330     C                              or vinflg = '0'
019700010330     C                              or vinflg = '2')
019800010330     C*
019900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020000010711     C                   if        vinflg = *blanks or vinflg = '0'
020100010711     C                   clear                   vinmsg
020200010711     C                   endif
020300100204     C*
020400100204     C                   exsr      inzvar
020500100204     C                   exsr      normCSV
020600010605     C                   exsr      impvab
020700010601     C*
020800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010605     C                   eval      depfil = VABLNP
021000010601     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021025     C                   if        vlrpoi = 999
021300030822     C                   move(P)   invfil        VABFGS
021400021025     C                   else
021500030822     C                   move(P)   vlrpoi        VABFGS
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200040119     C  N31              add       1             §CTROKVB
022300040119     C   32              add       1             §CTRMO
022400040119     C   31              add       1             §CTRNO
022500030822     C                   if        wGiro = 2
022600040510     C* Scrivo sempre il VAT
022700040510     C  N31              exsr      wrivat
022800040510     C* Scrivo il VAB solo a rottura d spedizione
022900040119     C                   if        depspe =  *zeros OR
023000040119     C                             depspe <> vabNSP
023100040119     C  N31              exsr      wrivab
023200040119     C  N31              eval      depspe = vabNSP
023300040119     C                   endif
023400040119     C                   endif
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200030822     C  N70              enddo
025300040119     C*
025400040119     C                   endif                                                  (endif - chkcall)
025500010601     C*
025600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025700010601     C                   if        cntNonEl = *zeros or
025800010601     C                             flgMulti = '0'
025900010330     C* Se non ci sono record con errori ...
026000010601     C                   if        §ctrno = 0 and
026100010604     C                             §ctrmo = 0 and
026200010601     C                             flgStato <> '2'
026300010330     C* ... restituisco esito OK.
026400010330     C                   eval      wrkesito = '0'
026500010330     C                   else
026600040119     C                   if        §ctrokvb > 0
026700010330     C                   eval      wrkesito = '1'
026800010330     C                   else
026900010615     C                   if        flgOk = '0'
027000010615     C                   eval      wrkesito = '2'
027100010615     C                   else
027200010615     C                   eval      wrkesito = '6'
027300010615     C                   endif
027400010330     C                   endif
027500010330     C                   endif
027600010601     C                   else
027700010601     C                   eval      wrkesito = '9'
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        %open(tivin00r)
028100010330     C                   close     tivin00r
028200010330     C                   endif
028300021025     C                   if        %open(fivabwwr)
028400021025     C                   close     fivabwwr
028500010330     C                   endif
028600040512     C                   if        %open(fivatwwr)
028700040512     C                   close     fivatwwr
028800040512     C                   endif
028900010601     C*
029000010601     C                   if        vlrpoi <> 999
029100010601     C                   eval      invfil = vlrpoi
029200010601     C                   endif
029300010330     C*
029400040119     C                   if        §ctrokvb > 0
029500010601     C                             and invfil > *zeros
029600010330     C                   exsr      invio
029700010330     C                   endif
029800010330     C*
029900010330     C                   ENDSR
030000010330     C***
030100040119
030200040119
030300040119
030400040119     C*----------------------------------------------------*
030500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
030600040119     C*----------------------------------------------------*
030700040510     C     WRIVAB        BEGSR
030800040119     C*
030900060331     C* Gestisco l'eventuale rottura x numero spedizione
031000070823     C                   if        VABNSP <> depspe
031100070301     C                   if        wVATNOT_E <> *blanks
031200080125     C                   if        VABCTM = *blanks
031300080125     C                   eval      VABCTM = '7Q'
031400080125     C                   endif
031500070301     C                   endif
031600040119     C                   write     fivab000                                     => scarico il VAB
031700060331     C                   endif
031800040119     C*
031900040119     C                   ENDSR
032000040119
032100040119
032200040119
032300040119     C*----------------------------------------------------*
032400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
032500040119     C*----------------------------------------------------*
032600040510     C     WRIVAT        BEGSR
032700060512     C*
032800060512     C* Inizializzo i campi chiave
032900060512     C                   eval      VATFGS = VABFGS
033000060512     C                   eval      VATAAS = VABAAS
033100060512     C                   eval      VATCCM = VABCCM
033200070823     C                   eval      VATNRS = VABNRS
033300060512     C                   eval      VATNSP = VABNSP
033400060512     C                   eval      VATLNP = VABLNP
033500060331     C*
033600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033700060331     C                   if        wVATNOT_A <> *blanks
033800060331     C                   eval      VATNOT = wVATNOT_A
033900060331     C                   eval      VATTRC = 'A'
034000060331     C                   write     FIVAT000
034100060331     C                   add       1             §CTROKVT
034200060331     C                   endif
034300060331     C*
034400060331     C                   if        wVATNOT_B <> *blanks
034500060331     C                   eval      VATNOT = wVATNOT_B
034600060331     C                   eval      VATTRC = 'B'
034700060331     C                   write     FIVAT000
034800060331     C                   add       1             §CTROKVT
034900060331     C                   endif
035000060331     C*
035100080625     C                   if        wVATNOT_E <> *blanks
035200060331     C                   eval      VATNOT = wVATNOT_E
035300060331     C                   eval      VATTRC = 'E'
035400080716     C                   write     FIVAT000
035500080716     C                   add       1             §CTROKVT
035600080625     C                   endif
035700140113     C*
035800140113     C                   if        wVATNOT_I <> *blanks
035900140113     C                   eval      VATNOT = wVATNOT_I
036000140113     C                   eval      VATTRC = 'I'
036100140113     C                   write     FIVAT000
036200140113     C                   add       1             §CTROKVT
036300140113     C                   endif
036400140113     C*
036500140113     C                   if        wVATNOT_J <> *blanks
036600140113     C                   eval      VATNOT = wVATNOT_J
036700140113     C                   eval      VATTRC = 'J'
036800140113     C                   write     FIVAT000
036900140113     C                   add       1             §CTROKVT
037000140113     C                   endif
037100040119     C*
037200040119     C                   ENDSR
037300040119
037400040119
037500040119
037600040119     C*----------------------------------------------------*
037700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
037800040119     C*----------------------------------------------------*
037900040119     C     PREVAT        BEGSR
038000040119     C*
038100040119     C* Compongo il nome del membro da dare al FIVATWWR
038200040119     C                   eval      parmbr = vlrhdl
038300040119     C                   movel     'M'           parmbr
038400040510     C                   eval      parccm = vlrksc
038500040119     C                   eval      paropz = '1'
038600040119     C* Effettuo la chiamata al CLLE preposto
038700040119     C                   call(e)   'TITVVTC'
038800040119     C                   parm                    parccm
038900040119     C                   parm                    parmbr
039000040119     C                   parm                    paropz
039100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039200040119     C                   if        %error
039300040119     C                   movel     '1'           chkcall
039400040119     C                   else
039500040119     C                   movel     '0'           chkcall
039600040119     C                   endif
039700040119     C*
039800040119     C                   ENDSR
039900010601
040000010601
040100010601
040200010330     C*----------------------------------------------------*
040300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040400010330     C*----------------------------------------------------*
040500040119     C     INZVAR        BEGSR
040600010330     C*
040700030822     C* Inizializzo variabili di wrk
040800020204     C                   Z-ADD     *zeros        Num5_0            5 0
040900060331     C                   MOVEL     *blanks       wVATNOT_A
041000060331     C                   MOVEL     *blanks       wVATNOT_B
041100060331     C                   MOVEL     *blanks       wVATNOT_E
041200140113     C                   MOVEL     *blanks       wVATNOT_I
041300140113     C                   MOVEL     *blanks       wVATNOT_J
041400140113     C                   MOVEL     *blanks       wVATNOT_IJ
041500090220     C*
041600080617     C                   EVAL      wFlgCAS = *blanks
041700030822     C*
041800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
041900030822     C                   CLEAR                   FIVAB000
042000040119     C                   CLEAR                   FIVAT000
042100030822     C                   CLEAR                   SkSplitCSV
042200030822     C                   EVAL      i = 1
042300030822     C                   EVAL      posDa = *zeros
042400030822     C                   EVAL      posA  = *zeros
042500030822     C*
042600030822     C* Reimposto i valori di default
042700030822     C                   EXSR      DEFCAM
042800030822     C*
042900010330     C                   ENDSR
043000010330     C*----------------------------------------------------*
043100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043200010330     C*----------------------------------------------------*
043300010330     C     DEFCAM        BEGSR
043400030822     C*
043500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043600030715     C* e delimitatore testo.
043700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044000100204     C*
044100100204     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
044200100204     C                   EVAL      CharSOS = CharNUM
044300030822     C*
044400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
044500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044600030715     C                   SETON                                        50
044700030715     C                   ELSE
044800030715     C                   SETOFF                                       50
044900030715     C                   ENDIF
045000090421     C*
045100090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
045200090421     C                   EVAL      VABCBO = '1'
045300090421     C                   EVAL      VABLNP = vlrpoi
045400090421     C                   EVAL      VABNSP = rrnum
045500030822     C*
045600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045700030822     C                   EVAL      posDaDft = 1
045800030822     C                   EVAL      posADft  = 0
045900030822     C                   EVAL      wGiroDft = 0
046000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
046100030822     C                             posDaDft > 0
046200030822     C*
046300030822     C* Gestisco il 1° giro
046400030822     C                   IF        wGiroDft = 0
046500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
046600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046700030822     C* Incremento il contatore dei "giri"
046800030822     C                   EVAL      wGiroDft = 1
046900030822     C                   ELSE
047000030822     C                   EVAL      posDaDft = posADft
047100030822     C                   ENDIF
047200030822     C* Eseguo lo scan x trovare la fine del campo corrente
047300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
047400030822     C*
047500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
047600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047700030822     C                   IF        posDaDft > 0 AND
047800030822     C                             posADft  > 0
047900030822     C* NCL
048000030822     C                   IF        %subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):1:3)
048300030822     C                             = 'NCL'
048400030822     C                   EVAL      PiStr=%trim(%subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048700030822     C                   EXSR      CHKNUM
048800030822     C                   IF        PiInt=*on
048900030822     C                   Z-ADD     PiVal         VABNCL
049000030822     C                   ENDIF
049100030822     C                   ENDIF
049200030822     C* CCM
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'CCM'
049700030822     C                   EVAL      PiStr=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   EXSR      CHKNUM
050100030822     C                   IF        PiInt=*on
050200030822     C                   Z-ADD     PiVal         VABCCM
050300030822     C                   ENDIF
050400030822     C                   ENDIF
050500030822     C* LNP
050600030822     C                   IF        %subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):1:3)
050900030822     C                             = 'LNP'
051000030822     C                   EVAL      PiStr=%trim(%subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051300030822     C                   EXSR      CHKNUM
051400030822     C                   IF        PiInt=*on
051500030822     C                   Z-ADD     PiVal         VABLNP
051600030822     C                   ENDIF
051700030822     C                   ENDIF
051800030822     C* NRS
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'NRS'
052300030822     C                   EVAL      PiStr=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   EXSR      CHKNUM
052700030822     C                   IF        PiInt=*on
052800030822     C                   Z-ADD     PiVal         VABNRS
052900030822     C                   ENDIF
053000030822     C                   ENDIF
053100030822     C* CTR
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'CTR'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABCTR
054200030822     C                   ENDIF
054300030822     C                   ENDIF
054400030822     C* PKB
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'PKB'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiNum=*on
055400030822     C                   Z-ADD     PiVal         VABPKB
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* VLB
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'VLB'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiNum=*on
056700030822     C                   Z-ADD     PiVal         VABVLB
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* QFT
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'QFT'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiNum=*on
058000030822     C                   Z-ADD     PiVal         VABQFT
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* CBO
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'CBO'
058800030822     C                   EVAL      VABCBO=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* TSP
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'TSP'
059700030822     C                   EVAL      VABTSP=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* VAS
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'VAS'
060600030822     C                   EVAL      VABVAS=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* VCA
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'VCA'
061500030822     C                   EVAL      VABVCA=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900070925     C* TIC
062000070925     C                   IF        %subst(
062100070925     C                             %subst(vlrppt:posDaDft+1:
062200070925     C                             posADft-posDaDft-1):1:3)
062300070925     C                             = 'TIC'
062400070925     C                   EVAL      VABTIC=%trim(%subst(
062500070925     C                             %subst(vlrppt:posDaDft+1:
062600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700070925     C                   ENDIF
062800030822     C* GCA
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'GCA'
063300030822     C                   EVAL      VABGCA=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* CTM
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CTM'
064200030822     C                   EVAL      VABCTM=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* FFD
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'FFD'
065100030822     C                   EVAL      VABFFD=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* VAD
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VAD'
066000030822     C                   EVAL      VABVAD=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* GMA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'GMA'
066900030822     C                   EVAL      VABGMA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* GGA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'GGA'
067800030822     C                   EVAL      VABGGA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* GVA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'GVA'
068700030822     C                   EVAL      VABGVA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* TC1
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'TC1'
069600030822     C                   EVAL      VABTC1=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* TC2
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'TC2'
070500030822     C                   EVAL      VABTC2=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900040714     C* VATTRC
071000040714     C                   IF        %subst(
071100040714     C                             %subst(vlrppt:posDaDft+1:
071200040714     C                             posADft-posDaDft-1):1:3)
071300040714     C                             = 'TRC'
071400040714     C                   EVAL      VATTRC=%trim(%subst(
071500040714     C                             %subst(vlrppt:posDaDft+1:
071600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040714     C                   ENDIF
071800030822     C* ...
071900030822     C                   ENDIF
072000030822     C                   ENDDO
072100020204     C*
072200010330     C                   ENDSR
072300010607     C*----------------------------------------------------*
072400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072500010607     C*----------------------------------------------------*
072600010607     C     IMPVAB        BEGSR
072700010607     C*
072800010607     C                   SETOFF                                       3132
072900030515     C*
073000010607     C                   MOVEL     datcor        VABAAS
073100010607     C                   MOVE      datcor        VABMGS
073200010607     C*
073300030715     C**********
073400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073500030715     C**********
073600030715     C                   IF        wGiro = *zeros
073700080716     C*
073800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073900080716     C                   EVAL      depvinDTA = vinDTA
074000040113     C*
074100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
074200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
074300090716     C                             %trim(intcol(3))+%trim(intcol(4))+
074400090716     C                             %trim(intcol(5))<>*blanks
074500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074600090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
074700090716     C                                      %trim(intcol(5))
074800100204     C     ';':CharCSV   XLATE     vindta        vindta
074900040510     C                   ENDIF
075000080626     C*
075100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
075200031201     C     minu:maiu     XLATE     vindta        vindta
075300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
075400030715     C                   DOW       posDa <= %len(%trim(vindta))
075500030715     C*
075600030715     C* Gestisco il 1° campo
075700030715     C                   IF        i = 1
075800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075900030715     C                   EVAL      posDa = 1
076000030715     C* Eseguo lo scan x trovare la fine del primo campo
076100100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076200030715     C* A questo punto "estraggo" il campo corrente
076300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076400030715     C                                                    (posA-posDa))
076500030715     C* X i campi successivi al 1°
076600030715     C                   ELSE
076700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076800030715     C                   EVAL      posDa = posA + 1
076900030715     C* Eseguo lo scan x trovare la fine del campo corrente
077000100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077100030715     C* Gestisco l'ultimo campo
077200030715     C                   IF        posA = *zeros
077300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C                   LEAVE
077800030715     C                   ELSE
077900030715     C* A questo punto "estraggo" il campo corrente
078000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078100030715     C                                                    (posA-posDa))
078200030715     C                   ENDIF
078300030715     C                   ENDIF
078400030715     C* Incremento il contatore d campo
078500030715     C                   EVAL      i = i +1
078600030715     C                   ENDDO
078700030715     C*
078800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078900030715     C* delimitazione testo
079000030715     C                   EVAL      i = 1
079100030715     C                   DOW       i < %elem(SkSplitFLD)
079200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
079300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
079400030715     C                   EVAL      i = i + 1
079500030715     C                   ENDDO
079600030715     C                   EVAL      wGiro = 1
079700080716     C* Ripristino la 1° riga originale
079800080716     C                   EVAL      vinDTA = depvinDTA
079900030715     C                   ELSE
080000030715     C                   EVAL      wGiro = 2
080100031204     C**********
080200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
080300031204     C* d separatore campo
080400031204     C**********
080500031204     C                   EVAL      vindta = %trim(vindta)
080600100204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080700031204     C                   EVAL      vindta = %subst(vindta:2)
080800031204     C                   ENDDO
080900031204     C*
081000031204     C                   Z-ADD     *zeros        lunghInput        4 0
081100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081200100204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
081300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
081400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081500031204     C                   ENDDO
081600030715     C**********
081700030715     C* Effettuo lo split del campo dati d input
081800030715     C**********
081900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082000030715     C                   DOW       posDa <= %len(%trim(vindta))
082100030715     C*
082200030715     C* Gestisco il 1° campo
082300030715     C                   IF        i = 1
082400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082500030715     C                   EVAL      posDa = 1
082600030715     C* Eseguo lo scan x trovare la fine del primo campo
082700100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C* X i campi successivi al 1°
083200030715     C                   ELSE
083300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083400030715     C                   EVAL      posDa = posA + 1
083500031204     C* Verifico che nn vi sia il campo nn valorizzato
083600100204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083700031204     C* Se campo nn valorizzato skippo al prossimo
083800031204     C                   EVAL      SkSplitCSV(i) = *blanks
083900031204     C                   EVAL      posA  = posA + 1
084000031204     C                   ELSE
084100030715     C* Eseguo lo scan x trovare la fine del campo corrente
084200100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084300030715     C* Gestisco l'ultimo campo
084400030715     C                   IF        posA = *zeros
084500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   LEAVE
085000030715     C                   ELSE
085100030715     C* A questo punto "estraggo" il campo corrente
085200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085300030715     C                                                    (posA-posDa))
085400030715     C                   ENDIF
085500031204     C                   ENDIF
085600030715     C                   ENDIF
085700030715     C* Incremento il contatore d campo
085800030715     C                   EVAL      i = i +1
085900030715     C                   ENDDO
086000030715     C*
086100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086200030715     C* delimitazione testo
086300030715     C                   EVAL      i = 1
086400030715     C                   DOW       i < %elem(SkSplitCSV)
086500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086700030715     C                   EVAL      i = i + 1
086800030715     C                   ENDDO
086900030715     C*
087000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
087100030715     C                   EVAL      i = 1
087200030715     C                   DOW       i < %elem(SkSplitFLD)
087300030715     C*
087400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087600030715     C                             SkSplitCSV(i) <> *blanks
087700030721     C*** ATB
087800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087900030721     C                   EVAL      VABATB = SkSplitCSV(i)
088000030721     C                   ENDIF
088100030715     C*** RSD
088200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
088300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
088400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088600030715     C* ==
088700030715     C                   ENDIF
088800030721     C*** RD2
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
089000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030715     C*** IND
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
089400030715     C                   EVAL      VABIND = SkSplitCSV(i)
089500030715     C                   ENDIF
089600030715     C*** LOD
089700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089900030715     C                   ENDIF
090000030715     C*** PRD
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
090200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
090300030715     C                   ENDIF
090400030721     C*** NZD
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030715     C*** RMA
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
091000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** NOT
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
091400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** NT2
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** VCA
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
092200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** VAS
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** FFD
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
093000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030721     C*** GC1
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
093400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093500030721     C                   ENDIF
093600030721     C*** GC2
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030721     C*** TSP
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094300030721     C                   ENDIF
094400040510     C*** CBO
094500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
094600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
094700040510     C                   ENDIF
094800030721     C*** NAS
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
095000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** TIC
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** GCA
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** XCO
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** CTM
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** TCR
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
097000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** CTS
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** FTM
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** VAD
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** GMA
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GGA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
099000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** GVA
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** TC1
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TC2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** SCL
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** RMO
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
101000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** NMO
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*
101700030721     C* Reperisco quindi i campi numerici...
101800040510     C*** NSP
101900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
102000040510     C                   EVAL      PiStr=SkSplitCSV(i)
102100040510     C                   EXSR      CHKNUM
102200040510     C                   IF        PiInt=*on
102300040510     C                   Z-ADD     PiVal         VABNSP
102400040510     C                   ELSE
102500040510     C                   SETON                                        31
102600040510     C                   EVAL      VABNSP = *zeros
102700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
102800040510     C                             + ' ' + 'VABNSP'
102900040510     C                   ENDIF
103000040510     C                   ENDIF
103100071106     C*** RMN
103200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
103300071106     C                   EVAL      PiStr=SkSplitCSV(i)
103400071106     C                   EXSR      CHKNUM
103500071106     C                   IF        PiInt=*on
103600071106     C                   Z-ADD     PiVal         VABRMN
103700071106     C                   ELSE
103800071106     C                   SETON                                        32
103900071106     C                   EVAL      VABRMN = *zeros
104000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
104100071106     C                             + ' ' + 'VABRMN'
104200071106     C                   ENDIF
104300071106     C                   ENDIF
104400030721     C*** CAD
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104600150521     C* siccome spediscono anche all'estero si prende il CAP "as is"
104700150521     C                   EVAL      VABCAD=SkSplitCSV(i)
104800150521     C***                EVAL      PiStr=SkSplitCSV(i)
104900150521     C***                EXSR      CHKNUM
105000150521     C***                IF        PiInt=*on
105100150521     C***                Z-ADD     PiVal         Num5_0
105200150521     C***                MOVEL(p)  Num5_0        VABCAD
105300150521     C***                ELSE
105400150521     C***                EVAL      VABCAD = SkSplitCSV(i)
105500150521     C***                SETON                                        32
105600150521     C***                EVAL      VABCAD = *zeros
105700150521     C***                EVAL      vinmsg = %trimr(vinmsg)
105800150521     C***                          + ' ' + 'VABCAD'
105900150521     C***                ENDIF
106000030721     C                   ENDIF
106100030721     C*** NCL
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiInt=*on
106600030721     C                   Z-ADD     PiVal         VABNCL
106700030721     C                   ELSE
106800030721     C                   SETON                                        32
106900030721     C                   EVAL      VABNCL = *zeros
107000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107100040122     C                             + ' ' + 'VABNCL'
107200030721     C                   ENDIF
107300030721     C                   ENDIF
107400030721     C*** PKB
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
107600030721     C                   EVAL      PiStr=SkSplitCSV(i)
107700030721     C                   EXSR      CHKNUM
107800030721     C                   IF        PiNum=*on
107900030721     C                   Z-ADD     PiVal         VABPKB
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      VABPKB = *zeros
108300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108400030721     C                             + ' ' + 'VABPKB'
108500030721     C                   ENDIF
108600030721     C                   ENDIF
108700030721     C*** CAS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108900080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
109000080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
109100081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
109200090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
109300090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
109400090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
109500090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
109600090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
109700090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
109800090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
109900081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
110000080125     C                   EVAL      wFlgCAS = '1'
110100080125     C                   ENDIF
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiNum=*on
110500030721     C                   Z-ADD     PiVal         VABCAS
110600030721     C                   ELSE
110700030721     C                   SETON                                        32
110800030721     C                   EVAL      VABCAS = *zeros
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABCAS'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** CCM
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on      AND
111800030721     C                             PiVal<=9999999 AND
111900030721     C                             PiVal>*zeros
112000030721     C                   Z-ADD     PiVal         VABCCM
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      VABCCM = *zeros
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABCCM'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** LNP
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on    AND
113300030721     C                             PiVal<=999   AND
113400030721     C                             PiVal>*zeros
113500030721     C                   Z-ADD     PiVal         VABLNP
113600030721     C                   ELSE
113700040715     C                   SETON                                        32
113800030721     C                   EVAL      VABLNP = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040510     C                             + ' ' + 'VABLNP'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** NRS
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on AND
114800030721     C                             PiVal<=99
114900030721     C                   Z-ADD     PiVal         VABNRS
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABNRS'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** LNA
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on    AND
116100030721     C                             PiVal<=999   AND
116200030721     C                             PiVal>*zeros
116300030721     C                   Z-ADD     PiVal         VABLNA
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABLNA = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABLNA'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** CTR
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on  AND
117600030721     C                             PiVal<=999
117700030721     C                   Z-ADD     PiVal         VABCTR
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABCTR'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** IAS
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900030721     C                   Z-ADD     PiVal         VABIAS
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABIAS'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** VLB
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABVLB
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABVLB'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** QFT
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300030721     C                   Z-ADD     PiVal         VABQFT
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABQFT'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** NCD
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on      AND
122500030721     C                             PiVal<=9999999
122600030721     C                   Z-ADD     PiVal         VABNCD
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABNCD'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** NCA
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on      AND
123800030721     C                             PiVal<=9999999
123900030721     C                   Z-ADD     PiVal         VABNCA
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABNCA'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** ZNC
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on AND
125100030721     C                             PiVal<=99
125200030721     C                   Z-ADD     PiVal         VABZNC
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABZNC'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** DCR
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on       AND
126400030721     C                             PiVal<=99999999
126500030721     C                   Z-ADD     PiVal         VABDCR
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABDCR'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** HCR
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on   AND
127700030721     C                             PiVal<=9999
127800030721     C                   Z-ADD     PiVal         VABHCR
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABHCR'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** VMD
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABVMD
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABVMD'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** ANT
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on        AND
130200030721     C                             PiVal<=999999999
130300030721     C                   Z-ADD     PiVal         VABANT
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABANT'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** CMO
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on
131500030721     C                   Z-ADD     PiVal         Num5_0
131600030721     C                   MOVEL(p)  Num5_0        VABCMO
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      VABCMO = *zeros
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABCMO'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400040510     C*
132500040510     C* Reperisco i campi del FIVAT
132600040510     C*** VATNOT
132700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
132800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
132900040510     C                   ENDIF
133000040510     C*** VATTRC
133100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133300040510     C                   ENDIF
133400060331     C*** VATNOT_A
133500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
133700060331     C                   ENDIF
133800060331     C*** VATNOT_B
133900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134100060331     C                   ENDIF
134200060331     C*** VATNOT_E
134300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134500060331     C                   ENDIF
134600140113     C*** VATNOT_I
134700140113     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
134800140113     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
134900140113     C                   ENDIF
135000070823     C*
135100071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135200090421     C*
135300090716     C*** W_VABCCM
135400090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCCM'
135500090716     C                   SELECT
135600090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'D'
135700090716     C                   EVAL      VABCCM = 0591944
135800090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'H'
135900090716     C                   EVAL      VABCCM = 0593873
136000090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'T'
136100090716     C                   EVAL      VABCCM = 0593873
136200090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'G'
136300090716     C                   EVAL      VABCCM = 0593874
136400090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'R'
136500090716     C                   EVAL      VABCCM = 0593874
136600140207     C                   WHEN      %trim(SkSplitCSV(i)) = 'W'
136700140207     C                   EVAL      VABCCM = 0595570
136800150915     C                   WHEN      %trim(SkSplitCSV(i)) = '&'
136900150915     C                   EVAL      VABCCM = 0595993
137000090716     C                   ENDSL
137100090622     C                   ENDIF
137200110221     C*
137300110221     C*** W_VARIE
137400110221     C                   IF        %trim(SkSplitFLD(i)) = 'W_VARIE'
137500110221     C                   SELECT
137600110221     C                   WHEN      %subst(SkSplitCSV(i):1:3) = 'APP'
137700110221     C                   EVAL      VABTC1 = 'A'
137800110221     C                   ENDSL
137900110221     C*
138000110221     C                   SELECT
138100150521     C                   WHEN      %subst(SkSplitCSV(i):4:8) <> *zeros and
138200150521     C                             %subst(SkSplitCSV(i):4:8) <> *blank
138300150521     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:8)
138400150521     C                   EXSR      CHKNUM
138500150521     C                   IF        PiInt=*on
138600150521     C                   Z-ADD     PiVal         VABDCR
138700150521     C                   ELSE
138800150521     C                   SETON                                        32
138900150521     C                   EVAL      vinmsg = %trimr(vinmsg)
139000150521     C                             + ' ' + 'VABDCR'
139100150521     C                   ENDIF
139200150521     C***                EVAL      VABDCR = %dec(%subst(SkSplitCSV(i):4:8):8:0)
139300110221     C                   ENDSL
139400110221     C                   ENDIF
139500090716     C*** W_VABCBO
139600090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
139700090716     C                   SELECT
139800090716     C                   WHEN      %trim(SkSplitCSV(i)) =
139900090716     C                             'PORTO FRANCO'
140000090716     C                   EVAL      VABCBO = '1'
140100090716     C                   WHEN      %trim(SkSplitCSV(i)) =
140200090716     C                             'PORTO FRANCO FABBRICA'
140300090716     C                   EVAL      VABCBO = '2'
140400090716     C                   WHEN      %trim(SkSplitCSV(i)) =
140500090716     C                             'PORTO FRANCO CON ADDEBITO'
140600090716     C                   EVAL      VABCBO = '1'
140700090716     C                   ENDSL
140800090622     C                   ENDIF
140900090716     C*** W_VABNSP/VABRMN/VABRMA
141000090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141100090622     C                   EVAL      VABRMA = SkSplitCSV(i)
141200090622     C                   EVAL      PiStr=SkSplitCSV(i)
141300090622     C                   EXSR      CHKNUM
141400090622     C                   IF        PiInt=*on
141500090622     C                   Z-ADD     PiVal         VABRMN
141600090622     C                   Z-ADD     PiVal         VABNSP
141700090622     C                   ELSE
141800090622     C                   SETON                                        32
141900090622     C                   EVAL      VABRMN = *zeros
142000090622     C                   EVAL      VABNSP = *zeros
142100090622     C                   EVAL      vinmsg = %trimr(vinmsg)
142200090622     C                             + ' ' + 'VABRMN VABNSP'
142300090622     C                   ENDIF
142400090603     C                   ENDIF
142500090716     C*** W_VABTIC
142600090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
142700090716     C                   SELECT
142800090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'B'
142900090716     C                   EVAL      VABTIC = *blanks
143000090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'C'
143100090716     C                   EVAL      VABTIC = *blanks
143200090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'R'
143300090716     C                   EVAL      VABTIC = 'CM'
143400090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'M'
143500090716     C                   EVAL      VABTIC = 'BM'
143600090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'N'
143700090716     C                   EVAL      VABTIC = 'TM'
143800090716     C                   ENDSL
143900090716     C                   ENDIF
144000090716     C*** W_VABNCL_1
144100090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL_1'
144200090716     C                   EVAL      PiStr=SkSplitCSV(i)
144300090716     C                   EXSR      CHKNUM
144400090716     C                   IF        PiInt=*on
144500090716     C                   ADD       PiVal         VABNCL
144600090716     C                   ELSE
144700090716     C                   SETON                                        32
144800090716     C                   EVAL      VABNCL = *zeros
144900090716     C                   EVAL      vinmsg = %trimr(vinmsg)
145000090716     C                             + ' ' + 'VABNCL'
145100090716     C                   ENDIF
145200090716     C                   ENDIF
145300090716     C*** W_VABNCL_2
145400090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL_2'
145500090716     C                   EVAL      PiStr=SkSplitCSV(i)
145600090716     C                   EXSR      CHKNUM
145700090716     C                   IF        PiInt=*on
145800090716     C                   ADD       PiVal         VABNCL
145900090716     C                   ELSE
146000090716     C                   SETON                                        32
146100090716     C                   EVAL      VABNCL = *zeros
146200090716     C                   EVAL      vinmsg = %trimr(vinmsg)
146300090716     C                             + ' ' + 'VABNCL'
146400090716     C                   ENDIF
146500090716     C                   ENDIF
146600150520     C*** W_VABANT
146700150520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABANT'
146800150610     C**se il contenuto della colonna è un valore intero
146900150610     C***                EVAL      PiStr=SkSplitCSV(i)
147000150610     C***                EXSR      CHKNUM
147100150610     C**costruisco VABANT in questo modo:
147200150610     C**- 1° e 2° bytes = '02'
147300150610     C**- 3° e 4° bytes = valore numerico intero reperito
147400150610     C**- 5° e 6° bytes = '00'
147500150610     C**- 7° e 8° bytes = '00
147600150610     C**- 9° = '0'
147700150610     C***                IF        PiInt=*on
147800150610     C***                          and PiVal>0
147900150610     C***                EVAL      VABANT = 020000000 +
148000150610     C***                          PiVal * 100000
148100150610     C***                EVAL      VABGVA = 'B'
148200150610     C***                ELSE
148300150610     C**se non si può creare VABANT non do errore
148400150610     C***                ENDIF
148500150520     C                   ENDIF
148600070925     C*
148700071029     C***  <===  -----------------------------------------
148800040510     C*
148900030715     C                   ENDIF
149000030715     C                   EVAL      i = i + 1
149100030715     C                   ENDDO
149200070925     C*
149300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149400080125     C*
149500140113     C*
149600140113     C* email destinatario
149700140113     C                   IF        %trim(wVATNOT_IJ) <> *blanks
149800140113     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
149900140113     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
150000140113     C                   ENDIF
150100070925     C*
150200071029     C***  <===  -----------------------------------------
150300070925     C*
150400070925     C* Considerazioni finali su CBO/CAS
150500080125     C                   IF        wFlgCAS = '1'
150600070925     C                   IF        VABCBO = '1'
150700070925     C                   EVAL      VABCBO = '4'
150800070925     C                   ENDIF
150900070925     C                   IF        VABCBO = '2'
151000070925     C                   EVAL      VABCBO = '6'
151100070925     C                   ENDIF
151200070925     C                   ENDIF
151300070925     C***  <===  ----------------------------
151400040510     C*
151500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151600040510     C                   IF        VABNSP = *zeros
151700040510     C                   SETON                                        31
151800040510     C                   ENDIF
151900040715     C*
152000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152100040715     C                   IF        VABLNP = *zeros
152200040715     C                   SETON                                        31
152300040715     C                   ENDIF
152400040510     C*
152500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152600040510     C                   IF        VABRMN = *zeros
152700040510     C                   Z-ADD     VABNSP        VABRMN
152800040510     C                   ENDIF
152900040510     C*
153000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153100040510     C                   IF        VABRMA = *blanks
153200040714     C                   MOVEL     VABRMN        VABRMA
153300040510     C                   ENDIF
153400040128     C*
153500040510     C* Se provincia nn valorizzata la reperisco
153600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153700040510     C                   IF        VABPRD = *blanks
153800040128     C                   CLEAR                   TISI95DS
153900040128     C                   EVAL      I95TCN = '3'
154000040128     C                   Z-ADD     datcor        I95DAT
154100040128     C                   EVAL      I95NAR = VABNZD
154200040128     C                   EVAL      I95CAP = VABCAD
154300040128     C                   EVAL      I95LOC = VABLOD
154400040128     C                   CALL      'TISI95R'
154500040128     C                   PARM                    TISI95DS
154600040128     C                   EVAL      VABPRD = O95PRV
154700040510     C                   ENDIF
154800020204     C*
154900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155000020204     C                   EXSR      CHKIMPDIV
155100030715     C                   ENDIF
155200020204     C*
155300010607     C                   ENDSR
155400010607     C*----------------------------------------------------*
155500010601
155600020204
155700020204     C*----------------------------------------------------*
155800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155900020204     C*----------------------------------------------------*
156000020204     C     CHKIMPDIV     BEGSR
156100020204     C*
156200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156300020204     C                   Z-ADD     *zeros        wrkDec            9 9
156400020204     C*
156500020204     C* Come prima cosa effettuo considerazioni sulla divisa
156600020204     C                   IF        vabIAS > *zeros
156700020204     C                   IF        vabVAS <> 'EUR'
156800020204     C                   EVAL      vabVAS =  'ITL'
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   IF        vabCAS > *zeros
157300020204     C                   IF        vabVCA <> 'EUR'
157400020204     C                   EVAL      vabVCA =  'ITL'
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   IF        vabVMD > *zeros
157900020204     C                   IF        vabVAD <> 'EUR'
158000020204     C                   EVAL      vabVAD =  'ITL'
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158500020204     C                   Z-ADD     vabIAS        wrkDec
158600020204     C                   IF        wrkDec > *zeros
158700020204     C                   IF        vabVAS = 'ITL'
158800020204     C                   EVAL      vabIAS = *zeros
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159300020204     C                   Z-ADD     vabCAS        wrkDec
159400020204     C                   IF        wrkDec > *zeros
159500020204     C                   IF        vabVCA = 'ITL'
159600020204     C                   EVAL      vabCAS = *zeros
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160100020204     C                   Z-ADD     vabVMD        wrkDec
160200020204     C                   IF        wrkDec > *zeros
160300020204     C                   IF        vabVAD = 'ITL'
160400020204     C                   EVAL      vabVMD = *zeros
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C                   ENDSR
160900020204     C***
161000080617
161100080617
161200080617
161300080617     C*----------------------------------------------------*
161400080617     C*  CONTROLLO NUMERICITA' CAMPI
161500080617     C*----------------------------------------------------*
161600080617     C     CHKNUM        BEGSR
161700080617     C*
161800080617     C                   IF        PiDecChr = *blanks
161900080617     C                   EVAL      PiDecChr = CharNUM
162000080617     C                   ENDIF
162100080617     C*
162200080617     C                   callp     UBISNUM_Check(PiStr
162300080617     C                                          :PiDecChr
162400080617     C                                          :PiVal
162500080617     C                                          :PiNum
162600080617     C                                          :PiInt)
162700080617     C*
162800080617     C                   ENDSR
162900080617     C***
163000010330
163100010601
163200010601
163300010601
163400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163500010601     C     repfil        BEGSR
163600010601     C*
163700010601     C                   if        invfil = *zeros and
163800010601     C                             depfil > *zeros and
163900010629     C                             (vinflg = *blanks or
164000010629     C                              vinflg = *zeros)
164100010601     C
164200010601     C                   eval      invfil = depfil
164300010601     C                   endif
164400010601     C*
164500010601     C                   if        depfil <> invfil and
164600010601     C                             invfil > *zeros
164700010601     C                   eval      flgMulti = '1'
164800010601     C                   if        vinflg = *blanks
164900010601     C                   add       1             cntNonEl
165000010601     C                   endif
165100010601     C                   endif
165200010601     C*
165300010601     C                   if        vinflg = '2'
165400010601     C                   eval      flgStato = '2'
165500010601     C                   endif
165600010601     C*
165700010601     C                   ENDSR
165800010601     C***
165900010601
166000010601
166100010601
166200010330
166300040119      /TITLE Invio dei dati al punto operativo.
166400040119     C     invio         BEGSR
166500040119     C*
166600040119     C* 1° invio FIVAT
166700040119     C                   reset                   dscmz
166800040119     C                   move      vlrpoi        cmzdst
166900040119     C                   eval      cmzfld = 'FIVATWWR'
167000040119     C                   eval      cmzmbd = vlrhdl
167100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167200040119     C***                if        prmfir = *blanks
167300040119     C                   eval      cmzfla = 'FIVAT00F'
167400040119     C                   eval      cmzmba = 'FIVAT00F'
167500040119     C***                else
167600040119     C***                eval      cmzfla = prmfir
167700040119     C***                eval      cmzmba = prmfir
167800040119     C***                endif
167900040119     C                   eval      cmznrr = *zeros
168000040119     C                   move      §ctrokvt      cmznrr
168100040119     C                   eval      cmzlba = vlrfl1
168200040119     C                   call(e)   'TIS711C'
168300040119     C                   parm                    dscmz
168400040119     C                   parm      *blanks       esito
168500040119     C                   if        %error
168600040119     C                             or cmzerr = '1'
168700040119     C                             or esito  = '1'
168800040119     C                   eval      wrkesito = '3'
168900040119     C                   else
169000040119     C*
169100040119     C* 2° invio FIVAB
169200040119     C                   reset                   dscmz
169300040119     C                   move      vlrpoi        cmzdst
169400040119     C                   eval      cmzfld = vlrfou
169500040119     C                   eval      cmzmbd = vlrhdl
169600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169700040119     C***                if        prmfir = *blanks
169800040119     C                   eval      cmzfla = 'FIVAB00F'
169900040119     C                   eval      cmzmba = 'FIVAB00F'
170000040119     C***                else
170100040119     C***                eval      cmzfla = prmfir
170200040119     C***                eval      cmzmba = prmfir
170300040119     C***                endif
170400040119     C                   eval      cmznrr = *zeros
170500040119     C                   move      §ctrokvb      cmznrr
170600040119     C                   eval      cmzlba = vlrfl1
170700040119     C                   call(e)   'TIS711C'
170800040119     C                   parm                    dscmz
170900040119     C                   parm      *blanks       esito
171000040119     C                   if        %error
171100040119     C                             or cmzerr = '1'
171200040119     C                             or esito  = '1'
171300040119     C                   eval      wrkesito = '3'
171400040119     C                   endif
171500040119     C                   endif
171600040119     C*
171700040119     C                   ENDSR
171800040119     C***
171900010601
172000010601
172100010601
172200010601
172300010601      /TITLE Invio dei dati al punto operativo.
172400010601     C     opeini        BEGSR
172500010601     C*
172600010601     C* Inizializzo flag e contatori operativi
172700010601     C                   movel     '0'           flgGiro           1
172800010601     C                   movel     '0'           flgMulti          1
172900010601     C                   movel     '1'           flgStato          1
173000010615     C                   movel     '0'           flgOk             1
173100010601     C                   z-add     *zeros        cntNonEl         10 0
173200010601     C                   z-add     *zeros        depfil            3 0
173300010601     C                   z-add     *zeros        invfil            3 0
173400010601     C*
173500010601     C                   ENDSR
173600010601     C***
173700070326
173800070326
173900070326
174000070326
174100080916     C     *pssr         BEGSR
174200070329     C*
174300080916     C                   if        %open(tivin00r)
174400080916     C                   close     tivin00r
174500080916     C                   endif
174600080916     C                   if        %open(fivabwwr)
174700080916     C                   close     fivabwwr
174800080916     C                   endif
174900080916     C                   if        %open(fivatwwr)
175000080916     C                   close     fivatwwr
175100080916     C                   endif
175200070326     C*
175300070326     C* Effettuo la chiamata al CLLE preposto
175400080916     C                   call(e)   'TITVVTC'
175500080916     C                   parm                    parccm
175600080916     C                   parm                    parmbr
175700080916     C                   parm      '2'           paropz
175800070326     C*
175900080916     C                   eval      wrkesito = '2'
176000070404     C*
176100080916     C                   seton                                        LR
176200070326     C*
176300080916     C                   ENDSR     '*CANCL'
176400070326     C***
176500070326
176600100204
176700100204
176800100204     C     normCSV       BEGSR
176900100204     C*
177000100204     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177100100204     C* separatori campi
177200100204     C                   if        CharTXT <> *blanks
177300100204     C                   z-add     *zeros        wLenVINDTA        4 0
177400100204     C                   z-add     *zeros        wFlgVINDTA        1 0
177500100204     C                   setoff                                       80
177600100204     C                   dow       wLenVINDTA < %len(%trim(vindta))
177700100204     C                   eval      wLenVINDTA = wLenVINDTA + 1
177800100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177900100204     C                   if        not *in80
178000100204     C                   seton                                        80
178100100204     C                   else
178200100204     C                   setoff                                       80
178300100204     C                   endif
178400100204     C                   endif
178500100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178600100204     C                             and *in80 = *on
178700100204     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
178800100204     C                   endif
178900100204     C                   enddo
179000100204     C                   endif
179100100204     C*
179200100204     C                   ENDSR
179300100204     C***
179400070326
179500010330
179600010330
179700000613     C     *inzsr        BEGSR
179800990910     C*
179900990910     C     *entry        plist
180000990920     C                   parm                    tivlrds
180100990921     C                   parm      wrkesito      esito
180200000724     C                   parm                    prmlit
180300000710     C                   parm                    prmfir
180400010330     C*
180500010330     C* CALCOLA LA DATA CORRENTE
180600100204     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180700100204     C                   eval      datcor = %dec(%date() : *ISO)
180800000613     C*
180900000613     C                   ENDSR
181000000613     C***
181100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181200090716W_VABCCM;W_VABCBO;X001;X002;X003;X004;X005;X006;W_VABNSP;VABRSD;VABIND;
181300150520VABLOD;VABCAD;VABPRD;VABNZD;VABPKB;W_VABNCL_1;W_VABNCL_2;W_VABANT;X008;
181400090716VABVLB;X009;X010;VABCAS;W_VABTIC;X011;X012;X013;VABVCA;X014;X015;X016;
181500110221X017;X018;X019;X020;X021;X022;X023;W_VARIE;X024;X025;VATNOT_B;VABNOT;
181600140113X026;X027;W_VATNOT_I;X029;X030;X031
