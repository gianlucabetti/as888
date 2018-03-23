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
005100160217     D wVATNOT_S       s                   LIKE(VATNOT)
005200030822     D*------------------
005300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005400030822     D*------------------
005500030822     D posDaDft        S              3  0 INZ(*zeros)
005600030822     D posADft         S              3  0 INZ(*zeros)
005700030822     D j               s              3  0 INZ(1)
005800030822     D wGiroDft        s              1  0 INZ(*zeros)
005900031201     D*------------------
006000031201     D* Costanti
006100031201     D*------------------
006200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006400080617     D*------------------
006500080617
006600080617     D*------------------
006700080617     D* LINKING A DEFINIZIONI ESTERNE
006800080617     D*------------------
006900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100010330
007200010330
007300010330
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600081014     C                   reset                   wrkesito
007700010601     C*
007800010601     C                   exsr      opeini
007900010605     C                   exsr      rwvab
008000081014     C*
008100081014     C                   exsr      endela
008200010601     C*
008300010601     C                   seton                                        lr
008400010601
008500010601
008600010601
008700010601     C*--------------------------------------------------------
008800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008900010601     C*--------------------------------------------------------
009000010601     C     PREELA        BEGSR
009100010601     C*
009200010601     C* SE OCCORRE SPEDIRE IN FILIALE
009300010601     C                   if        invfil <> *zeros and
009400010601     C                             flgGiro = '0'
009500010601     C*
009600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009700010601     C                   eval      flgGiro = '1'
009800010601     C*
009900010601     C                   endif
010000010601     C*
010100010601     C                   ENDSR
010200010601     C***
010300010601
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010900010601     C*--------------------------------------------------------
011000010601     C     ENDELA        BEGSR
011100081014     C*
011200081014     C                   if        %open(tivin00r)
011300081014     C                   close     tivin00r
011400081014     C                   endif
011500081014     C                   if        %open(fivabwwr)
011600081014     C                   close     fivabwwr
011700081014     C                   endif
011800081014     C                   if        %open(fivatwwr)
011900081014     C                   close     fivatwwr
012000081014     C                   endif
012100040119     C*
012200040119     C* Effettuo la chiamata al CLLE preposto
012300040119     C                   call(e)   'TITVVTC'
012400040510     C                   parm                    parccm
012500040119     C                   parm                    parmbr
012600040119     C                   parm      '2'           paropz
012700050201     C*
012800050201     C* Effettuo lancio TISI95 solo x chiusura
012900050201     C                   CLEAR                   TISI95DS
013000050201     C                   EVAL      I95TLA = 'C'
013100050201     C                   CALL      'TISI95R'
013200050201     C                   PARM                    TISI95DS
013300000616     C*
013400010601     C                   ENDSR
013500010601     C***
013600000613
013700010601
013800010601
013900010330     C*--------------------------------------------------------
014000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014100010330     C*--------------------------------------------------------
014200010605     C     RWVAB         BEGSR
014300010330     C*
014400010330     C                   if        not %open(tivin00r)
014500010330     C                   open      tivin00r
014600010330     C                   endif
014700021025     C                   if        not %open(fivabwwr)
014800021025     C                   open      fivabwwr
014900010330     C                   endif
015000040119     C*
015100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015200040119     C                   exsr      prevat
015300040119     C*
015400040119     C                   if        chkcall = '0'
015500040119     C*
015600040119     C                   if        not %open(fivatwwr)
015700040119     C                   open      fivatwwr
015800040119     C                   endif
015900010330     C*
016000040119     C                   clear                   §CTROKVB          7 0
016100040119     C                   clear                   §CTROKVT          7 0
016200040119     C                   clear                   §CTRMO            7 0
016300040119     C                   clear                   §CTRNO            7 0
016400070823     C*
016500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016700090420     C                   exsr      inzvar
016800090420     C                   exsr      impvab
016900010330     C*
017000030822     C                   do        *HIVAL
017100010330     C*
017200030822     C                   read      tivin00r                               70
017300010618     C*
017400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017500010618     C                   if        vinflg = '1'
017600010618     C                   eval      flgOk = '1'
017700010618     C                   endif
017800040510     C*
017900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018000040510     C                   z-add     *zeros        wLenVINDTA        4 0
018100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018500040510     C                   z-add     1             wFlgVINDTA
018600040510     C                   leave
018700040510     C                   endif
018800040510     C                   enddo
018900090420     C*
019000090420     C                   add       1             rrnum
019100010618     C*
019200040510     C                   if        vindta > *blanks AND
019300090420     C                             wFlgVINDTA = 1
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010330     C*
020000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020100010711     C                   if        vinflg = *blanks or vinflg = '0'
020200010711     C                   clear                   vinmsg
020300010711     C                   endif
020400100204     C*
020500100204     C                   exsr      inzvar
020600100204     C                   exsr      normCSV
020700010605     C                   exsr      impvab
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010605     C                   eval      depfil = VABLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300021025     C                   if        vlrpoi = 999
021400030822     C                   move(P)   invfil        VABFGS
021500021025     C                   else
021600030822     C                   move(P)   vlrpoi        VABFGS
021700021025     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300040119     C  N31              add       1             §CTROKVB
022400040119     C   32              add       1             §CTRMO
022500040119     C   31              add       1             §CTRNO
022600030822     C                   if        wGiro = 2
022700040510     C* Scrivo sempre il VAT
022800040510     C  N31              exsr      wrivat
022900040510     C* Scrivo il VAB solo a rottura d spedizione
023000040119     C                   if        depspe =  *zeros OR
023100040119     C                             depspe <> vabNSP
023200040119     C  N31              exsr      wrivab
023300040119     C  N31              eval      depspe = vabNSP
023400040119     C                   endif
023500040119     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010604     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300030822     C  N70              enddo
025400040119     C*
025500040119     C                   endif                                                  (endif - chkcall)
025600010601     C*
025700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025800010601     C                   if        cntNonEl = *zeros or
025900010601     C                             flgMulti = '0'
026000010330     C* Se non ci sono record con errori ...
026100010601     C                   if        §ctrno = 0 and
026200010604     C                             §ctrmo = 0 and
026300010601     C                             flgStato <> '2'
026400010330     C* ... restituisco esito OK.
026500010330     C                   eval      wrkesito = '0'
026600010330     C                   else
026700040119     C                   if        §ctrokvb > 0
026800010330     C                   eval      wrkesito = '1'
026900010330     C                   else
027000010615     C                   if        flgOk = '0'
027100010615     C                   eval      wrkesito = '2'
027200010615     C                   else
027300010615     C                   eval      wrkesito = '6'
027400010615     C                   endif
027500010330     C                   endif
027600010330     C                   endif
027700010601     C                   else
027800010601     C                   eval      wrkesito = '9'
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        %open(tivin00r)
028200010330     C                   close     tivin00r
028300010330     C                   endif
028400021025     C                   if        %open(fivabwwr)
028500021025     C                   close     fivabwwr
028600010330     C                   endif
028700040512     C                   if        %open(fivatwwr)
028800040512     C                   close     fivatwwr
028900040512     C                   endif
029000010601     C*
029100010601     C                   if        vlrpoi <> 999
029200010601     C                   eval      invfil = vlrpoi
029300010601     C                   endif
029400010330     C*
029500040119     C                   if        §ctrokvb > 0
029600010601     C                             and invfil > *zeros
029700010330     C                   exsr      invio
029800010330     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200040119
030300040119
030400040119
030500040119     C*----------------------------------------------------*
030600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
030700040119     C*----------------------------------------------------*
030800040510     C     WRIVAB        BEGSR
030900040119     C*
031000060331     C* Gestisco l'eventuale rottura x numero spedizione
031100070823     C                   if        VABNSP <> depspe
031200070301     C                   if        wVATNOT_E <> *blanks
031300080125     C                   if        VABCTM = *blanks
031400080125     C                   eval      VABCTM = '7Q'
031500080125     C                   endif
031600070301     C                   endif
031700040119     C                   write     fivab000                                     => scarico il VAB
031800060331     C                   endif
031900040119     C*
032000040119     C                   ENDSR
032100040119
032200040119
032300040119
032400040119     C*----------------------------------------------------*
032500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
032600040119     C*----------------------------------------------------*
032700040510     C     WRIVAT        BEGSR
032800060512     C*
032900060512     C* Inizializzo i campi chiave
033000060512     C                   eval      VATFGS = VABFGS
033100060512     C                   eval      VATAAS = VABAAS
033200060512     C                   eval      VATCCM = VABCCM
033300070823     C                   eval      VATNRS = VABNRS
033400060512     C                   eval      VATNSP = VABNSP
033500060512     C                   eval      VATLNP = VABLNP
033600060331     C*
033700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
033800060331     C                   if        wVATNOT_A <> *blanks
033900060331     C                   eval      VATNOT = wVATNOT_A
034000060331     C                   eval      VATTRC = 'A'
034100060331     C                   write     FIVAT000
034200060331     C                   add       1             §CTROKVT
034300060331     C                   endif
034400060331     C*
034500060331     C                   if        wVATNOT_B <> *blanks
034600060331     C                   eval      VATNOT = wVATNOT_B
034700060331     C                   eval      VATTRC = 'B'
034800060331     C                   write     FIVAT000
034900060331     C                   add       1             §CTROKVT
035000060331     C                   endif
035100060331     C*
035200080625     C                   if        wVATNOT_E <> *blanks
035300060331     C                   eval      VATNOT = wVATNOT_E
035400060331     C                   eval      VATTRC = 'E'
035500080716     C                   write     FIVAT000
035600080716     C                   add       1             §CTROKVT
035700080625     C                   endif
035800140113     C*
035900140113     C                   if        wVATNOT_I <> *blanks
036000140113     C                   eval      VATNOT = wVATNOT_I
036100140113     C                   eval      VATTRC = 'I'
036200140113     C                   write     FIVAT000
036300140113     C                   add       1             §CTROKVT
036400140113     C                   endif
036500140113     C*
036600140113     C                   if        wVATNOT_J <> *blanks
036700140113     C                   eval      VATNOT = wVATNOT_J
036800140113     C                   eval      VATTRC = 'J'
036900140113     C                   write     FIVAT000
037000140113     C                   add       1             §CTROKVT
037100140113     C                   endif
037200160217     C*
037300160217     C                   if        wVATNOT_S <> *blanks
037400160217     C                   eval      VATNOT = wVATNOT_S
037500160217     C* i 2 flag devono essere blank
037600160217     C                   EVAL      %subst(VATNOT:17:1)=*blank
037700160217     C                   EVAL      %subst(VATNOT:18:1)=*blank
037800160217     C                   eval      VATTRC = 'S'
037900160217     C                   write     FIVAT000
038000160217     C                   add       1             §CTROKVT
038100160217     C                   endif
038200040119     C*
038300040119     C                   ENDSR
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038900040119     C*----------------------------------------------------*
039000040119     C     PREVAT        BEGSR
039100040119     C*
039200040119     C* Compongo il nome del membro da dare al FIVATWWR
039300040119     C                   eval      parmbr = vlrhdl
039400040119     C                   movel     'M'           parmbr
039500040510     C                   eval      parccm = vlrksc
039600040119     C                   eval      paropz = '1'
039700040119     C* Effettuo la chiamata al CLLE preposto
039800040119     C                   call(e)   'TITVVTC'
039900040119     C                   parm                    parccm
040000040119     C                   parm                    parmbr
040100040119     C                   parm                    paropz
040200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040300040119     C                   if        %error
040400040119     C                   movel     '1'           chkcall
040500040119     C                   else
040600040119     C                   movel     '0'           chkcall
040700040119     C                   endif
040800040119     C*
040900040119     C                   ENDSR
041000010601
041100010601
041200010601
041300010330     C*----------------------------------------------------*
041400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
041500010330     C*----------------------------------------------------*
041600040119     C     INZVAR        BEGSR
041700010330     C*
041800030822     C* Inizializzo variabili di wrk
041900020204     C                   Z-ADD     *zeros        Num5_0            5 0
042000060331     C                   MOVEL     *blanks       wVATNOT_A
042100060331     C                   MOVEL     *blanks       wVATNOT_B
042200060331     C                   MOVEL     *blanks       wVATNOT_E
042300140113     C                   MOVEL     *blanks       wVATNOT_I
042400140113     C                   MOVEL     *blanks       wVATNOT_J
042500140113     C                   MOVEL     *blanks       wVATNOT_IJ
042600160217     C                   MOVEL     *blanks       wVATNOT_S
042700090220     C*
042800080617     C                   EVAL      wFlgCAS = *blanks
042900030822     C*
043000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
043100030822     C                   CLEAR                   FIVAB000
043200040119     C                   CLEAR                   FIVAT000
043300030822     C                   CLEAR                   SkSplitCSV
043400030822     C                   EVAL      i = 1
043500030822     C                   EVAL      posDa = *zeros
043600030822     C                   EVAL      posA  = *zeros
043700030822     C*
043800030822     C* Reimposto i valori di default
043900030822     C                   EXSR      DEFCAM
044000030822     C*
044100010330     C                   ENDSR
044200010330     C*----------------------------------------------------*
044300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
044400010330     C*----------------------------------------------------*
044500010330     C     DEFCAM        BEGSR
044600030822     C*
044700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
044800030715     C* e delimitatore testo.
044900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
045000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
045100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
045200100204     C*
045300100204     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
045400100204     C                   EVAL      CharSOS = CharNUM
045500030822     C*
045600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
045700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
045800030715     C                   SETON                                        50
045900030715     C                   ELSE
046000030715     C                   SETOFF                                       50
046100030715     C                   ENDIF
046200090421     C*
046300090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
046400090421     C                   EVAL      VABCBO = '1'
046500090421     C                   EVAL      VABLNP = vlrpoi
046600090421     C                   EVAL      VABNSP = rrnum
046700030822     C*
046800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
046900030822     C                   EVAL      posDaDft = 1
047000030822     C                   EVAL      posADft  = 0
047100030822     C                   EVAL      wGiroDft = 0
047200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
047300030822     C                             posDaDft > 0
047400030822     C*
047500030822     C* Gestisco il 1° giro
047600030822     C                   IF        wGiroDft = 0
047700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
047800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
047900030822     C* Incremento il contatore dei "giri"
048000030822     C                   EVAL      wGiroDft = 1
048100030822     C                   ELSE
048200030822     C                   EVAL      posDaDft = posADft
048300030822     C                   ENDIF
048400030822     C* Eseguo lo scan x trovare la fine del campo corrente
048500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
048600030822     C*
048700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
048800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
048900030822     C                   IF        posDaDft > 0 AND
049000030822     C                             posADft  > 0
049100030822     C* NCL
049200030822     C                   IF        %subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):1:3)
049500030822     C                             = 'NCL'
049600030822     C                   EVAL      PiStr=%trim(%subst(
049700030822     C                             %subst(vlrppt:posDaDft+1:
049800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049900030822     C                   EXSR      CHKNUM
050000030822     C                   IF        PiInt=*on
050100030822     C                   Z-ADD     PiVal         VABNCL
050200030822     C                   ENDIF
050300030822     C                   ENDIF
050400030822     C* CCM
050500030822     C                   IF        %subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):1:3)
050800030822     C                             = 'CCM'
050900030822     C                   EVAL      PiStr=%trim(%subst(
051000030822     C                             %subst(vlrppt:posDaDft+1:
051100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051200030822     C                   EXSR      CHKNUM
051300030822     C                   IF        PiInt=*on
051400030822     C                   Z-ADD     PiVal         VABCCM
051500030822     C                   ENDIF
051600030822     C                   ENDIF
051700030822     C* LNP
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'LNP'
052200030822     C                   EVAL      PiStr=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   EXSR      CHKNUM
052600030822     C                   IF        PiInt=*on
052700030822     C                   Z-ADD     PiVal         VABLNP
052800030822     C                   ENDIF
052900030822     C                   ENDIF
053000030822     C* NRS
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'NRS'
053500030822     C                   EVAL      PiStr=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   EXSR      CHKNUM
053900030822     C                   IF        PiInt=*on
054000030822     C                   Z-ADD     PiVal         VABNRS
054100030822     C                   ENDIF
054200030822     C                   ENDIF
054300030822     C* CTR
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'CTR'
054800030822     C                   EVAL      PiStr=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   EXSR      CHKNUM
055200030822     C                   IF        PiInt=*on
055300030822     C                   Z-ADD     PiVal         VABCTR
055400030822     C                   ENDIF
055500030822     C                   ENDIF
055600030822     C* PKB
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'PKB'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiNum=*on
056600030822     C                   Z-ADD     PiVal         VABPKB
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900030822     C* VLB
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'VLB'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiNum=*on
057900030822     C                   Z-ADD     PiVal         VABVLB
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* QFT
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'QFT'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiNum=*on
059200030822     C                   Z-ADD     PiVal         VABQFT
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* CBO
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'CBO'
060000030822     C                   EVAL      VABCBO=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   ENDIF
060400030822     C* TSP
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'TSP'
060900030822     C                   EVAL      VABTSP=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   ENDIF
061300030822     C* VAS
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'VAS'
061800030822     C                   EVAL      VABVAS=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   ENDIF
062200030822     C* VCA
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'VCA'
062700030822     C                   EVAL      VABVCA=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   ENDIF
063100070925     C* TIC
063200070925     C                   IF        %subst(
063300070925     C                             %subst(vlrppt:posDaDft+1:
063400070925     C                             posADft-posDaDft-1):1:3)
063500070925     C                             = 'TIC'
063600070925     C                   EVAL      VABTIC=%trim(%subst(
063700070925     C                             %subst(vlrppt:posDaDft+1:
063800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900070925     C                   ENDIF
064000030822     C* GCA
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'GCA'
064500030822     C                   EVAL      VABGCA=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* CTM
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'CTM'
065400030822     C                   EVAL      VABCTM=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* FFD
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'FFD'
066300030822     C                   EVAL      VABFFD=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* VAD
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VAD'
067200030822     C                   EVAL      VABVAD=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* GMA
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'GMA'
068100030822     C                   EVAL      VABGMA=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* GGA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'GGA'
069000030822     C                   EVAL      VABGGA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* GVA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GVA'
069900030822     C                   EVAL      VABGVA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* TC1
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'TC1'
070800030822     C                   EVAL      VABTC1=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* TC2
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'TC2'
071700030822     C                   EVAL      VABTC2=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100040714     C* VATTRC
072200040714     C                   IF        %subst(
072300040714     C                             %subst(vlrppt:posDaDft+1:
072400040714     C                             posADft-posDaDft-1):1:3)
072500040714     C                             = 'TRC'
072600040714     C                   EVAL      VATTRC=%trim(%subst(
072700040714     C                             %subst(vlrppt:posDaDft+1:
072800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900040714     C                   ENDIF
073000030822     C* ...
073100030822     C                   ENDIF
073200030822     C                   ENDDO
073300020204     C*
073400010330     C                   ENDSR
073500010607     C*----------------------------------------------------*
073600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
073700010607     C*----------------------------------------------------*
073800010607     C     IMPVAB        BEGSR
073900010607     C*
074000010607     C                   SETOFF                                       3132
074100030515     C*
074200010607     C                   MOVEL     datcor        VABAAS
074300010607     C                   MOVE      datcor        VABMGS
074400010607     C*
074500030715     C**********
074600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
074700030715     C**********
074800030715     C                   IF        wGiro = *zeros
074900080716     C*
075000080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
075100080716     C                   EVAL      depvinDTA = vinDTA
075200040113     C*
075300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
075400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
075500090716     C                             %trim(intcol(3))+%trim(intcol(4))+
075600090716     C                             %trim(intcol(5))<>*blanks
075700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
075800090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
075900090716     C                                      %trim(intcol(5))
076000100204     C     ';':CharCSV   XLATE     vindta        vindta
076100040510     C                   ENDIF
076200080626     C*
076300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076400031201     C     minu:maiu     XLATE     vindta        vindta
076500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076600030715     C                   DOW       posDa <= %len(%trim(vindta))
076700030715     C*
076800030715     C* Gestisco il 1° campo
076900030715     C                   IF        i = 1
077000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077100030715     C                   EVAL      posDa = 1
077200030715     C* Eseguo lo scan x trovare la fine del primo campo
077300100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C* X i campi successivi al 1°
077800030715     C                   ELSE
077900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078000030715     C                   EVAL      posDa = posA + 1
078100030715     C* Eseguo lo scan x trovare la fine del campo corrente
078200100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078300030715     C* Gestisco l'ultimo campo
078400030715     C                   IF        posA = *zeros
078500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
078600030715     C* A questo punto "estraggo" il campo corrente
078700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078800030715     C                                                    (posA-posDa))
078900030715     C                   LEAVE
079000030715     C                   ELSE
079100030715     C* A questo punto "estraggo" il campo corrente
079200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079300030715     C                                                    (posA-posDa))
079400030715     C                   ENDIF
079500030715     C                   ENDIF
079600030715     C* Incremento il contatore d campo
079700030715     C                   EVAL      i = i +1
079800030715     C                   ENDDO
079900030715     C*
080000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080100030715     C* delimitazione testo
080200030715     C                   EVAL      i = 1
080300030715     C                   DOW       i < %elem(SkSplitFLD)
080400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080600030715     C                   EVAL      i = i + 1
080700030715     C                   ENDDO
080800030715     C                   EVAL      wGiro = 1
080900080716     C* Ripristino la 1° riga originale
081000080716     C                   EVAL      vinDTA = depvinDTA
081100030715     C                   ELSE
081200030715     C                   EVAL      wGiro = 2
081300031204     C**********
081400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
081500031204     C* d separatore campo
081600031204     C**********
081700031204     C                   EVAL      vindta = %trim(vindta)
081800100204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081900031204     C                   EVAL      vindta = %subst(vindta:2)
082000031204     C                   ENDDO
082100031204     C*
082200031204     C                   Z-ADD     *zeros        lunghInput        4 0
082300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082400100204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
082500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082700031204     C                   ENDDO
082800030715     C**********
082900030715     C* Effettuo lo split del campo dati d input
083000030715     C**********
083100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083200030715     C                   DOW       posDa <= %len(%trim(vindta))
083300030715     C*
083400030715     C* Gestisco il 1° campo
083500030715     C                   IF        i = 1
083600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083700030715     C                   EVAL      posDa = 1
083800030715     C* Eseguo lo scan x trovare la fine del primo campo
083900100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C* X i campi successivi al 1°
084400030715     C                   ELSE
084500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084600030715     C                   EVAL      posDa = posA + 1
084700031204     C* Verifico che nn vi sia il campo nn valorizzato
084800100204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084900031204     C* Se campo nn valorizzato skippo al prossimo
085000031204     C                   EVAL      SkSplitCSV(i) = *blanks
085100031204     C                   EVAL      posA  = posA + 1
085200031204     C                   ELSE
085300030715     C* Eseguo lo scan x trovare la fine del campo corrente
085400100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085500030715     C* Gestisco l'ultimo campo
085600030715     C                   IF        posA = *zeros
085700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085800030715     C* A questo punto "estraggo" il campo corrente
085900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086000030715     C                                                    (posA-posDa))
086100030715     C                   LEAVE
086200030715     C                   ELSE
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   ENDIF
086700031204     C                   ENDIF
086800030715     C                   ENDIF
086900030715     C* Incremento il contatore d campo
087000030715     C                   EVAL      i = i +1
087100030715     C                   ENDDO
087200030715     C*
087300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087400030715     C* delimitazione testo
087500030715     C                   EVAL      i = 1
087600030715     C                   DOW       i < %elem(SkSplitCSV)
087700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087900030715     C                   EVAL      i = i + 1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088300030715     C                   EVAL      i = 1
088400030715     C                   DOW       i < %elem(SkSplitFLD)
088500030715     C*
088600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088800030715     C                             SkSplitCSV(i) <> *blanks
088900030721     C*** ATB
089000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089100030721     C                   EVAL      VABATB = SkSplitCSV(i)
089200030721     C                   ENDIF
089300030715     C*** RSD
089400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
089500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
089600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
089700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
089800030715     C* ==
089900030715     C                   ENDIF
090000030721     C*** RD2
090100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090300030721     C                   ENDIF
090400030715     C*** IND
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
090600030715     C                   EVAL      VABIND = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** LOD
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** PRD
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030721     C*** NZD
091700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
091800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
091900030721     C                   ENDIF
092000030715     C*** RMA
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030715     C*** NOT
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
092600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
092700030715     C                   ENDIF
092800030715     C*** NT2
092900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093100030715     C                   ENDIF
093200030715     C*** VCA
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
093400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** VAS
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
093800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** FFD
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030721     C*** GC1
094500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
094600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
094700030721     C                   ENDIF
094800030721     C*** GC2
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** TSP
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
095400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
095500030721     C                   ENDIF
095600040510     C*** CBO
095700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
095800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
095900040510     C                   ENDIF
096000030721     C*** NAS
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
096200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** TIC
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
096600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** GCA
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** XCO
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
097400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** CTM
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
097800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** TCR
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
098200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** CTS
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
098600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** FTM
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** VAD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
099400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** GMA
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
099800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** GGA
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
100200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GVA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
100600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** TC1
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** TC2
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
101400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** SCL
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
101800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** RMO
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
102200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** NMO
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
102600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*
102900030721     C* Reperisco quindi i campi numerici...
103000040510     C*** NSP
103100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
103200040510     C                   EVAL      PiStr=SkSplitCSV(i)
103300040510     C                   EXSR      CHKNUM
103400040510     C                   IF        PiInt=*on
103500040510     C                   Z-ADD     PiVal         VABNSP
103600040510     C                   ELSE
103700040510     C                   SETON                                        31
103800040510     C                   EVAL      VABNSP = *zeros
103900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
104000040510     C                             + ' ' + 'VABNSP'
104100040510     C                   ENDIF
104200040510     C                   ENDIF
104300071106     C*** RMN
104400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
104500071106     C                   EVAL      PiStr=SkSplitCSV(i)
104600071106     C                   EXSR      CHKNUM
104700071106     C                   IF        PiInt=*on
104800071106     C                   Z-ADD     PiVal         VABRMN
104900071106     C                   ELSE
105000071106     C                   SETON                                        32
105100071106     C                   EVAL      VABRMN = *zeros
105200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
105300071106     C                             + ' ' + 'VABRMN'
105400071106     C                   ENDIF
105500071106     C                   ENDIF
105600030721     C*** CAD
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
105800150521     C* siccome spediscono anche all'estero si prende il CAP "as is"
105900150521     C                   EVAL      VABCAD=SkSplitCSV(i)
106000150521     C***                EVAL      PiStr=SkSplitCSV(i)
106100150521     C***                EXSR      CHKNUM
106200150521     C***                IF        PiInt=*on
106300150521     C***                Z-ADD     PiVal         Num5_0
106400150521     C***                MOVEL(p)  Num5_0        VABCAD
106500150521     C***                ELSE
106600150521     C***                EVAL      VABCAD = SkSplitCSV(i)
106700150521     C***                SETON                                        32
106800150521     C***                EVAL      VABCAD = *zeros
106900150521     C***                EVAL      vinmsg = %trimr(vinmsg)
107000150521     C***                          + ' ' + 'VABCAD'
107100150521     C***                ENDIF
107200030721     C                   ENDIF
107300030721     C*** NCL
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiInt=*on
107800030721     C                   Z-ADD     PiVal         VABNCL
107900030721     C                   ELSE
108000030721     C                   SETON                                        32
108100030721     C                   EVAL      VABNCL = *zeros
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300040122     C                             + ' ' + 'VABNCL'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** PKB
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100030721     C                   Z-ADD     PiVal         VABPKB
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      VABPKB = *zeros
109500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109600030721     C                             + ' ' + 'VABPKB'
109700030721     C                   ENDIF
109800030721     C                   ENDIF
109900030721     C*** CAS
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110100080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
110200080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
110300081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
110400090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
110500090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
110600090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
110700090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
110800090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
110900090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
111000090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
111100081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
111200080125     C                   EVAL      wFlgCAS = '1'
111300080125     C                   ENDIF
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiNum=*on
111700030721     C                   Z-ADD     PiVal         VABCAS
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABCAS = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABCAS'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** CCM
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on      AND
113000030721     C                             PiVal<=9999999 AND
113100030721     C                             PiVal>*zeros
113200030721     C                   Z-ADD     PiVal         VABCCM
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABCCM = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700040510     C                             + ' ' + 'VABCCM'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** LNP
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on    AND
114500030721     C                             PiVal<=999   AND
114600030721     C                             PiVal>*zeros
114700030721     C                   Z-ADD     PiVal         VABLNP
114800030721     C                   ELSE
114900040715     C                   SETON                                        32
115000030721     C                   EVAL      VABLNP = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040510     C                             + ' ' + 'VABLNP'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** NRS
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on AND
116000030721     C                             PiVal<=99
116100030721     C                   Z-ADD     PiVal         VABNRS
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABNRS'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** LNA
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on    AND
117300030721     C                             PiVal<=999   AND
117400030721     C                             PiVal>*zeros
117500030721     C                   Z-ADD     PiVal         VABLNA
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABLNA = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABLNA'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CTR
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on  AND
118800030721     C                             PiVal<=999
118900030721     C                   Z-ADD     PiVal         VABCTR
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABCTR'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** IAS
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABIAS
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABIAS'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** VLB
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300030721     C                   Z-ADD     PiVal         VABVLB
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABVLB'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** QFT
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiNum=*on
122500030721     C                   Z-ADD     PiVal         VABQFT
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABQFT'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** NCD
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on      AND
123700030721     C                             PiVal<=9999999
123800030721     C                   Z-ADD     PiVal         VABNCD
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABNCD'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** NCA
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on      AND
125000030721     C                             PiVal<=9999999
125100030721     C                   Z-ADD     PiVal         VABNCA
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABNCA'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** ZNC
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on AND
126300030721     C                             PiVal<=99
126400030721     C                   Z-ADD     PiVal         VABZNC
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABZNC'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** DCR
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on       AND
127600030721     C                             PiVal<=99999999
127700030721     C                   Z-ADD     PiVal         VABDCR
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABDCR'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** HCR
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on   AND
128900030721     C                             PiVal<=9999
129000030721     C                   Z-ADD     PiVal         VABHCR
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABHCR'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** VMD
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200030721     C                   Z-ADD     PiVal         VABVMD
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABVMD'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** ANT
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on        AND
131400030721     C                             PiVal<=999999999
131500030721     C                   Z-ADD     PiVal         VABANT
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABANT'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** CMO
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on
132700030721     C                   Z-ADD     PiVal         Num5_0
132800030721     C                   MOVEL(p)  Num5_0        VABCMO
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      VABCMO = *zeros
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABCMO'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600040510     C*
133700040510     C* Reperisco i campi del FIVAT
133800040510     C*** VATNOT
133900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
134000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
134100040510     C                   ENDIF
134200040510     C*** VATTRC
134300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
134400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
134500040510     C                   ENDIF
134600060331     C*** VATNOT_A
134700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134900060331     C                   ENDIF
135000060331     C*** VATNOT_B
135100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
135200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
135300060331     C                   ENDIF
135400060331     C*** VATNOT_E
135500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135700060331     C                   ENDIF
135800140113     C*** VATNOT_I
135900140113     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
136000140113     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
136100140113     C                   ENDIF
136200160217     C*** VATNOT_S
136300160217     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
136400160217     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
136500160217     C                   ENDIF
136600070823     C*
136700071029     C***  ===>  Gestione campi "particolari" dentro ciclo
136800090421     C*
136900090716     C*** W_VABCCM
137000090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCCM'
137100090716     C                   SELECT
137200090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'D'
137300090716     C                   EVAL      VABCCM = 0591944
137400090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'H'
137500090716     C                   EVAL      VABCCM = 0593873
137600090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'T'
137700090716     C                   EVAL      VABCCM = 0593873
137800090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'G'
137900090716     C                   EVAL      VABCCM = 0593874
138000090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'R'
138100090716     C                   EVAL      VABCCM = 0593874
138200140207     C                   WHEN      %trim(SkSplitCSV(i)) = 'W'
138300140207     C                   EVAL      VABCCM = 0595570
138400150915     C                   WHEN      %trim(SkSplitCSV(i)) = '&'
138500150915     C                   EVAL      VABCCM = 0595993
138600090716     C                   ENDSL
138700090622     C                   ENDIF
138800110221     C*
138900110221     C*** W_VARIE
139000110221     C                   IF        %trim(SkSplitFLD(i)) = 'W_VARIE'
139100110221     C                   SELECT
139200110221     C                   WHEN      %subst(SkSplitCSV(i):1:3) = 'APP'
139300110221     C                   EVAL      VABTC1 = 'A'
139400110221     C                   ENDSL
139500110221     C*
139600110221     C                   SELECT
139700150521     C                   WHEN      %subst(SkSplitCSV(i):4:8) <> *zeros and
139800150521     C                             %subst(SkSplitCSV(i):4:8) <> *blank
139900150521     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:8)
140000150521     C                   EXSR      CHKNUM
140100150521     C                   IF        PiInt=*on
140200150521     C                   Z-ADD     PiVal         VABDCR
140300150521     C                   ELSE
140400150521     C                   SETON                                        32
140500150521     C                   EVAL      vinmsg = %trimr(vinmsg)
140600150521     C                             + ' ' + 'VABDCR'
140700150521     C                   ENDIF
140800150521     C***                EVAL      VABDCR = %dec(%subst(SkSplitCSV(i):4:8):8:0)
140900110221     C                   ENDSL
141000110221     C                   ENDIF
141100090716     C*** W_VABCBO
141200090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
141300090716     C                   SELECT
141400090716     C                   WHEN      %trim(SkSplitCSV(i)) =
141500090716     C                             'PORTO FRANCO'
141600090716     C                   EVAL      VABCBO = '1'
141700090716     C                   WHEN      %trim(SkSplitCSV(i)) =
141800090716     C                             'PORTO FRANCO FABBRICA'
141900090716     C                   EVAL      VABCBO = '2'
142000090716     C                   WHEN      %trim(SkSplitCSV(i)) =
142100090716     C                             'PORTO FRANCO CON ADDEBITO'
142200090716     C                   EVAL      VABCBO = '1'
142300090716     C                   ENDSL
142400090622     C                   ENDIF
142500090716     C*** W_VABNSP/VABRMN/VABRMA
142600090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
142700090622     C                   EVAL      VABRMA = SkSplitCSV(i)
142800090622     C                   EVAL      PiStr=SkSplitCSV(i)
142900090622     C                   EXSR      CHKNUM
143000090622     C                   IF        PiInt=*on
143100090622     C                   Z-ADD     PiVal         VABRMN
143200090622     C                   Z-ADD     PiVal         VABNSP
143300090622     C                   ELSE
143400090622     C                   SETON                                        32
143500090622     C                   EVAL      VABRMN = *zeros
143600090622     C                   EVAL      VABNSP = *zeros
143700090622     C                   EVAL      vinmsg = %trimr(vinmsg)
143800090622     C                             + ' ' + 'VABRMN VABNSP'
143900090622     C                   ENDIF
144000090603     C                   ENDIF
144100090716     C*** W_VABTIC
144200090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
144300090716     C                   SELECT
144400090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'B'
144500090716     C                   EVAL      VABTIC = *blanks
144600090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'C'
144700090716     C                   EVAL      VABTIC = *blanks
144800090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'R'
144900090716     C                   EVAL      VABTIC = 'CM'
145000090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'M'
145100090716     C                   EVAL      VABTIC = 'BM'
145200090716     C                   WHEN      %trim(SkSplitCSV(i)) = 'N'
145300090716     C                   EVAL      VABTIC = 'TM'
145400090716     C                   ENDSL
145500090716     C                   ENDIF
145600090716     C*** W_VABNCL_1
145700090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL_1'
145800090716     C                   EVAL      PiStr=SkSplitCSV(i)
145900090716     C                   EXSR      CHKNUM
146000090716     C                   IF        PiInt=*on
146100090716     C                   ADD       PiVal         VABNCL
146200090716     C                   ELSE
146300090716     C                   SETON                                        32
146400090716     C                   EVAL      VABNCL = *zeros
146500090716     C                   EVAL      vinmsg = %trimr(vinmsg)
146600090716     C                             + ' ' + 'VABNCL'
146700090716     C                   ENDIF
146800090716     C                   ENDIF
146900090716     C*** W_VABNCL_2
147000090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNCL_2'
147100090716     C                   EVAL      PiStr=SkSplitCSV(i)
147200090716     C                   EXSR      CHKNUM
147300090716     C                   IF        PiInt=*on
147400090716     C                   ADD       PiVal         VABNCL
147500090716     C                   ELSE
147600090716     C                   SETON                                        32
147700090716     C                   EVAL      VABNCL = *zeros
147800090716     C                   EVAL      vinmsg = %trimr(vinmsg)
147900090716     C                             + ' ' + 'VABNCL'
148000090716     C                   ENDIF
148100090716     C                   ENDIF
148200150520     C*** W_VABANT
148300150520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABANT'
148400150610     C**se il contenuto della colonna è un valore intero
148500150610     C***                EVAL      PiStr=SkSplitCSV(i)
148600150610     C***                EXSR      CHKNUM
148700150610     C**costruisco VABANT in questo modo:
148800150610     C**- 1° e 2° bytes = '02'
148900150610     C**- 3° e 4° bytes = valore numerico intero reperito
149000150610     C**- 5° e 6° bytes = '00'
149100150610     C**- 7° e 8° bytes = '00
149200150610     C**- 9° = '0'
149300150610     C***                IF        PiInt=*on
149400150610     C***                          and PiVal>0
149500150610     C***                EVAL      VABANT = 020000000 +
149600150610     C***                          PiVal * 100000
149700150610     C***                EVAL      VABGVA = 'B'
149800150610     C***                ELSE
149900150610     C**se non si può creare VABANT non do errore
150000150610     C***                ENDIF
150100150520     C                   ENDIF
150200070925     C*
150300071029     C***  <===  -----------------------------------------
150400040510     C*
150500030715     C                   ENDIF
150600030715     C                   EVAL      i = i + 1
150700030715     C                   ENDDO
150800070925     C*
150900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151000080125     C*
151100140113     C*
151200140113     C* email destinatario
151300140113     C                   IF        %trim(wVATNOT_IJ) <> *blanks
151400140113     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
151500140113     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
151600140113     C                   ENDIF
151700070925     C*
151800071029     C***  <===  -----------------------------------------
151900070925     C*
152000070925     C* Considerazioni finali su CBO/CAS
152100080125     C                   IF        wFlgCAS = '1'
152200070925     C                   IF        VABCBO = '1'
152300070925     C                   EVAL      VABCBO = '4'
152400070925     C                   ENDIF
152500070925     C                   IF        VABCBO = '2'
152600070925     C                   EVAL      VABCBO = '6'
152700070925     C                   ENDIF
152800070925     C                   ENDIF
152900070925     C***  <===  ----------------------------
153000040510     C*
153100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153200040510     C                   IF        VABNSP = *zeros
153300040510     C                   SETON                                        31
153400040510     C                   ENDIF
153500040715     C*
153600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153700040715     C                   IF        VABLNP = *zeros
153800040715     C                   SETON                                        31
153900040715     C                   ENDIF
154000040510     C*
154100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154200040510     C                   IF        VABRMN = *zeros
154300040510     C                   Z-ADD     VABNSP        VABRMN
154400040510     C                   ENDIF
154500040510     C*
154600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
154700040510     C                   IF        VABRMA = *blanks
154800040714     C                   MOVEL     VABRMN        VABRMA
154900040510     C                   ENDIF
155000040128     C*
155100040510     C* Se provincia nn valorizzata la reperisco
155200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155300040510     C                   IF        VABPRD = *blanks
155400040128     C                   CLEAR                   TISI95DS
155500040128     C                   EVAL      I95TCN = '3'
155600040128     C                   Z-ADD     datcor        I95DAT
155700040128     C                   EVAL      I95NAR = VABNZD
155800040128     C                   EVAL      I95CAP = VABCAD
155900040128     C                   EVAL      I95LOC = VABLOD
156000040128     C                   CALL      'TISI95R'
156100040128     C                   PARM                    TISI95DS
156200040128     C                   EVAL      VABPRD = O95PRV
156300040510     C                   ENDIF
156400020204     C*
156500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
156600020204     C                   EXSR      CHKIMPDIV
156700030715     C                   ENDIF
156800020204     C*
156900010607     C                   ENDSR
157000010607     C*----------------------------------------------------*
157100010601
157200020204
157300020204     C*----------------------------------------------------*
157400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157500020204     C*----------------------------------------------------*
157600020204     C     CHKIMPDIV     BEGSR
157700020204     C*
157800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157900020204     C                   Z-ADD     *zeros        wrkDec            9 9
158000020204     C*
158100020204     C* Come prima cosa effettuo considerazioni sulla divisa
158200020204     C                   IF        vabIAS > *zeros
158300020204     C                   IF        vabVAS <> 'EUR'
158400020204     C                   EVAL      vabVAS =  'ITL'
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C                   IF        vabCAS > *zeros
158900020204     C                   IF        vabVCA <> 'EUR'
159000020204     C                   EVAL      vabVCA =  'ITL'
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   IF        vabVMD > *zeros
159500020204     C                   IF        vabVAD <> 'EUR'
159600020204     C                   EVAL      vabVAD =  'ITL'
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160100020204     C                   Z-ADD     vabIAS        wrkDec
160200020204     C                   IF        wrkDec > *zeros
160300020204     C                   IF        vabVAS = 'ITL'
160400020204     C                   EVAL      vabIAS = *zeros
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
160900020204     C                   Z-ADD     vabCAS        wrkDec
161000020204     C                   IF        wrkDec > *zeros
161100020204     C                   IF        vabVCA = 'ITL'
161200020204     C                   EVAL      vabCAS = *zeros
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
161700020204     C                   Z-ADD     vabVMD        wrkDec
161800020204     C                   IF        wrkDec > *zeros
161900020204     C                   IF        vabVAD = 'ITL'
162000020204     C                   EVAL      vabVMD = *zeros
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C                   ENDSR
162500020204     C***
162600080617
162700080617
162800080617
162900080617     C*----------------------------------------------------*
163000080617     C*  CONTROLLO NUMERICITA' CAMPI
163100080617     C*----------------------------------------------------*
163200080617     C     CHKNUM        BEGSR
163300080617     C*
163400080617     C                   IF        PiDecChr = *blanks
163500080617     C                   EVAL      PiDecChr = CharNUM
163600080617     C                   ENDIF
163700080617     C*
163800080617     C                   callp     UBISNUM_Check(PiStr
163900080617     C                                          :PiDecChr
164000080617     C                                          :PiVal
164100080617     C                                          :PiNum
164200080617     C                                          :PiInt)
164300080617     C*
164400080617     C                   ENDSR
164500080617     C***
164600010330
164700010601
164800010601
164900010601
165000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165100010601     C     repfil        BEGSR
165200010601     C*
165300010601     C                   if        invfil = *zeros and
165400010601     C                             depfil > *zeros and
165500010629     C                             (vinflg = *blanks or
165600010629     C                              vinflg = *zeros)
165700010601     C
165800010601     C                   eval      invfil = depfil
165900010601     C                   endif
166000010601     C*
166100010601     C                   if        depfil <> invfil and
166200010601     C                             invfil > *zeros
166300010601     C                   eval      flgMulti = '1'
166400010601     C                   if        vinflg = *blanks
166500010601     C                   add       1             cntNonEl
166600010601     C                   endif
166700010601     C                   endif
166800010601     C*
166900010601     C                   if        vinflg = '2'
167000010601     C                   eval      flgStato = '2'
167100010601     C                   endif
167200010601     C*
167300010601     C                   ENDSR
167400010601     C***
167500010601
167600010601
167700010601
167800010330
167900040119      /TITLE Invio dei dati al punto operativo.
168000040119     C     invio         BEGSR
168100040119     C*
168200040119     C* 1° invio FIVAT
168300040119     C                   reset                   dscmz
168400040119     C                   move      vlrpoi        cmzdst
168500040119     C                   eval      cmzfld = 'FIVATWWR'
168600040119     C                   eval      cmzmbd = vlrhdl
168700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168800040119     C***                if        prmfir = *blanks
168900040119     C                   eval      cmzfla = 'FIVAT00F'
169000040119     C                   eval      cmzmba = 'FIVAT00F'
169100040119     C***                else
169200040119     C***                eval      cmzfla = prmfir
169300040119     C***                eval      cmzmba = prmfir
169400040119     C***                endif
169500040119     C                   eval      cmznrr = *zeros
169600040119     C                   move      §ctrokvt      cmznrr
169700040119     C                   eval      cmzlba = vlrfl1
169800040119     C                   call(e)   'TIS711C'
169900040119     C                   parm                    dscmz
170000040119     C                   parm      *blanks       esito
170100040119     C                   if        %error
170200040119     C                             or cmzerr = '1'
170300040119     C                             or esito  = '1'
170400040119     C                   eval      wrkesito = '3'
170500040119     C                   else
170600040119     C*
170700040119     C* 2° invio FIVAB
170800040119     C                   reset                   dscmz
170900040119     C                   move      vlrpoi        cmzdst
171000040119     C                   eval      cmzfld = vlrfou
171100040119     C                   eval      cmzmbd = vlrhdl
171200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171300040119     C***                if        prmfir = *blanks
171400040119     C                   eval      cmzfla = 'FIVAB00F'
171500040119     C                   eval      cmzmba = 'FIVAB00F'
171600040119     C***                else
171700040119     C***                eval      cmzfla = prmfir
171800040119     C***                eval      cmzmba = prmfir
171900040119     C***                endif
172000040119     C                   eval      cmznrr = *zeros
172100040119     C                   move      §ctrokvb      cmznrr
172200040119     C                   eval      cmzlba = vlrfl1
172300040119     C                   call(e)   'TIS711C'
172400040119     C                   parm                    dscmz
172500040119     C                   parm      *blanks       esito
172600040119     C                   if        %error
172700040119     C                             or cmzerr = '1'
172800040119     C                             or esito  = '1'
172900040119     C                   eval      wrkesito = '3'
173000040119     C                   endif
173100040119     C                   endif
173200040119     C*
173300040119     C                   ENDSR
173400040119     C***
173500010601
173600010601
173700010601
173800010601
173900010601      /TITLE Invio dei dati al punto operativo.
174000010601     C     opeini        BEGSR
174100010601     C*
174200010601     C* Inizializzo flag e contatori operativi
174300010601     C                   movel     '0'           flgGiro           1
174400010601     C                   movel     '0'           flgMulti          1
174500010601     C                   movel     '1'           flgStato          1
174600010615     C                   movel     '0'           flgOk             1
174700010601     C                   z-add     *zeros        cntNonEl         10 0
174800010601     C                   z-add     *zeros        depfil            3 0
174900010601     C                   z-add     *zeros        invfil            3 0
175000010601     C*
175100010601     C                   ENDSR
175200010601     C***
175300070326
175400070326
175500070326
175600070326
175700080916     C     *pssr         BEGSR
175800070329     C*
175900080916     C                   if        %open(tivin00r)
176000080916     C                   close     tivin00r
176100080916     C                   endif
176200080916     C                   if        %open(fivabwwr)
176300080916     C                   close     fivabwwr
176400080916     C                   endif
176500080916     C                   if        %open(fivatwwr)
176600080916     C                   close     fivatwwr
176700080916     C                   endif
176800070326     C*
176900070326     C* Effettuo la chiamata al CLLE preposto
177000080916     C                   call(e)   'TITVVTC'
177100080916     C                   parm                    parccm
177200080916     C                   parm                    parmbr
177300080916     C                   parm      '2'           paropz
177400070326     C*
177500080916     C                   eval      wrkesito = '2'
177600070404     C*
177700080916     C                   seton                                        LR
177800070326     C*
177900080916     C                   ENDSR     '*CANCL'
178000070326     C***
178100070326
178200100204
178300100204
178400100204     C     normCSV       BEGSR
178500100204     C*
178600100204     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
178700100204     C* separatori campi
178800100204     C                   if        CharTXT <> *blanks
178900100204     C                   z-add     *zeros        wLenVINDTA        4 0
179000100204     C                   z-add     *zeros        wFlgVINDTA        1 0
179100100204     C                   setoff                                       80
179200100204     C                   dow       wLenVINDTA < %len(%trim(vindta))
179300100204     C                   eval      wLenVINDTA = wLenVINDTA + 1
179400100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179500100204     C                   if        not *in80
179600100204     C                   seton                                        80
179700100204     C                   else
179800100204     C                   setoff                                       80
179900100204     C                   endif
180000100204     C                   endif
180100100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180200100204     C                             and *in80 = *on
180300100204     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180400100204     C                   endif
180500100204     C                   enddo
180600100204     C                   endif
180700100204     C*
180800100204     C                   ENDSR
180900100204     C***
181000070326
181100010330
181200010330
181300000613     C     *inzsr        BEGSR
181400990910     C*
181500990910     C     *entry        plist
181600990920     C                   parm                    tivlrds
181700990921     C                   parm      wrkesito      esito
181800000724     C                   parm                    prmlit
181900000710     C                   parm                    prmfir
182000010330     C*
182100010330     C* CALCOLA LA DATA CORRENTE
182200100204     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182300100204     C                   eval      datcor = %dec(%date() : *ISO)
182400000613     C*
182500000613     C                   ENDSR
182600000613     C***
182700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182800090716W_VABCCM;W_VABCBO;X001;X002;X003;X004;X005;X006;W_VABNSP;VABRSD;VABIND;
182900150520VABLOD;VABCAD;VABPRD;VABNZD;VABPKB;W_VABNCL_1;W_VABNCL_2;W_VABANT;X008;
183000090716VABVLB;X009;X010;VABCAS;W_VABTIC;X011;X012;X013;VABVCA;X014;X015;X016;
183100110221X017;X018;X019;X020;X021;X022;X023;W_VARIE;X024;X025;VATNOT_B;VABNOT;
183200160217X026;VATNOT_S;W_VATNOT_I;X029;X030;X031
