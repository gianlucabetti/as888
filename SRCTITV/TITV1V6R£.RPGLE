000100100302      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200130514     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100302     FEDIVABwr  O    E             DISK    usropn
000600100302     FEDIVATwr  O    E             DISK    usropn
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
011100100302     C                   if        %open(edivabwr)
011200100302     C                   close     edivabwr
011300081014     C                   endif
011400100302     C                   if        %open(edivatwr)
011500100302     C                   close     edivatwr
011600081014     C                   endif
011700040119     C*
011800040119     C* Effettuo la chiamata al CLLE preposto
011900100302     C                   call(e)   'TITVEVTC'
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
013600100302     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013700010330     C*--------------------------------------------------------
013800010605     C     RWVAB         BEGSR
013900010330     C*
014000010330     C                   if        not %open(tivin00r)
014100010330     C                   open      tivin00r
014200010330     C                   endif
014300100302     C                   if        not %open(edivabwr)
014400100302     C                   open      edivabwr
014500010330     C                   endif
014600040119     C*
014700100302     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014800040119     C                   exsr      prevat
014900040119     C*
015000040119     C                   if        chkcall = '0'
015100040119     C*
015200100302     C                   if        not %open(edivatwr)
015300100302     C                   open      edivatwr
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
028000100302     C                   if        %open(edivabwr)
028100100302     C                   close     edivabwr
028200010330     C                   endif
028300100302     C                   if        %open(edivatwr)
028400100302     C                   close     edivatwr
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
031300100302     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
031400100302     C                   EVAL      VABCMR = wNomeFile
031500100302     C                   EVAL      VABDCM = DATCOR
031600100302     C                   EVAL      VABDTS = DATCOR
031700100302     C                   EVAL      VABHMS = ORACOR
031800100302     C                   EVAL      VABCNT = 1
031900100302     C*
032000100302     C                   write     EDIVAB00                                     => scarico il VAB
032100060331     C                   endif
032200040119     C*
032300040119     C                   ENDSR
032400040119
032500040119
032600040119
032700040119     C*----------------------------------------------------*
032800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
032900040119     C*----------------------------------------------------*
033000040510     C     WRIVAT        BEGSR
033100060512     C*
033200060512     C* Inizializzo i campi chiave
033300060512     C                   eval      VATFGS = VABFGS
033400060512     C                   eval      VATAAS = VABAAS
033500060512     C                   eval      VATCCM = VABCCM
033600070823     C                   eval      VATNRS = VABNRS
033700060512     C                   eval      VATNSP = VABNSP
033800060512     C                   eval      VATLNP = VABLNP
033900100302     C*
034000100302     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
034100100302     C                   EVAL      VATCMR = wNomeFile
034200100302     C                   EVAL      VATCNT = 1
034300060331     C*
034400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
034500060331     C                   if        wVATNOT_A <> *blanks
034600060331     C                   eval      VATNOT = wVATNOT_A
034700060331     C                   eval      VATTRC = 'A'
034800100302     C                   write     EDIVAT00
034900060331     C                   add       1             §CTROKVT
035000060331     C                   endif
035100060331     C*
035200060331     C                   if        wVATNOT_B <> *blanks
035300060331     C                   eval      VATNOT = wVATNOT_B
035400060331     C                   eval      VATTRC = 'B'
035500100302     C                   write     EDIVAT00
035600060331     C                   add       1             §CTROKVT
035700060331     C                   endif
035800060331     C*
035900080625     C                   if        wVATNOT_E <> *blanks
036000060331     C                   eval      VATNOT = wVATNOT_E
036100060331     C                   eval      VATTRC = 'E'
036200100302     C                   write     EDIVAT00
036300080716     C                   add       1             §CTROKVT
036400080625     C                   endif
036500040119     C*
036600040119     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100100302     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
037200040119     C*----------------------------------------------------*
037300040119     C     PREVAT        BEGSR
037400040119     C*
037500100302     C* Compongo il nome del membro da dare al EDIVATWR
037600040119     C                   eval      parmbr = vlrhdl
037700040119     C                   movel     'M'           parmbr
037800040510     C                   eval      parccm = vlrksc
037900040119     C                   eval      paropz = '1'
038000040119     C* Effettuo la chiamata al CLLE preposto
038100100302     C                   call(e)   'TITVEVTC'
038200040119     C                   parm                    parccm
038300040119     C                   parm                    parmbr
038400040119     C                   parm                    paropz
038500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038600040119     C                   if        %error
038700040119     C                   movel     '1'           chkcall
038800040119     C                   else
038900040119     C                   movel     '0'           chkcall
039000040119     C                   endif
039100040119     C*
039200040119     C                   ENDSR
039300010601
039400010601
039500010601
039600010330     C*----------------------------------------------------*
039700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039800010330     C*----------------------------------------------------*
039900040119     C     INZVAR        BEGSR
040000010330     C*
040100030822     C* Inizializzo variabili di wrk
040200020204     C                   Z-ADD     *zeros        Num5_0            5 0
040300060331     C                   MOVEL     *blanks       wVATNOT_A
040400060331     C                   MOVEL     *blanks       wVATNOT_B
040500060331     C                   MOVEL     *blanks       wVATNOT_E
040600090220     C*
040700080617     C                   EVAL      wFlgCAS = *blanks
040800030822     C*
040900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
041000100302     C                   CLEAR                   EDIVAB00
041100100302     C                   CLEAR                   EDIVAT00
041200030822     C                   CLEAR                   SkSplitCSV
041300030822     C                   EVAL      i = 1
041400030822     C                   EVAL      posDa = *zeros
041500030822     C                   EVAL      posA  = *zeros
041600030822     C*
041700030822     C* Reimposto i valori di default
041800030822     C                   EXSR      DEFCAM
041900030822     C*
042000010330     C                   ENDSR
042100010330     C*----------------------------------------------------*
042200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042300010330     C*----------------------------------------------------*
042400010330     C     DEFCAM        BEGSR
042500030822     C*
042600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042700030715     C* e delimitatore testo.
042800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
042900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043100100204     C*
043200100204     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
043300100204     C                   EVAL      CharSOS = CharNUM
043400030822     C*
043500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043600030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043700030715     C                   SETON                                        50
043800030715     C                   ELSE
043900030715     C                   SETOFF                                       50
044000030715     C                   ENDIF
044100090421     C*
044200090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
044300090421     C                   EVAL      VABCBO = '1'
044400090421     C                   EVAL      VABLNP = vlrpoi
044500090421     C                   EVAL      VABNSP = rrnum
044600030822     C*
044700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
044800030822     C                   EVAL      posDaDft = 1
044900030822     C                   EVAL      posADft  = 0
045000030822     C                   EVAL      wGiroDft = 0
045100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
045200030822     C                             posDaDft > 0
045300030822     C*
045400030822     C* Gestisco il 1° giro
045500030822     C                   IF        wGiroDft = 0
045600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
045700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
045800030822     C* Incremento il contatore dei "giri"
045900030822     C                   EVAL      wGiroDft = 1
046000030822     C                   ELSE
046100030822     C                   EVAL      posDaDft = posADft
046200030822     C                   ENDIF
046300030822     C* Eseguo lo scan x trovare la fine del campo corrente
046400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
046500030822     C*
046600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
046700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
046800030822     C                   IF        posDaDft > 0 AND
046900030822     C                             posADft  > 0
047000030822     C* NCL
047100030822     C                   IF        %subst(
047200030822     C                             %subst(vlrppt:posDaDft+1:
047300030822     C                             posADft-posDaDft-1):1:3)
047400030822     C                             = 'NCL'
047500030822     C                   EVAL      PiStr=%trim(%subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047800030822     C                   EXSR      CHKNUM
047900030822     C                   IF        PiInt=*on
048000030822     C                   Z-ADD     PiVal         VABNCL
048100030822     C                   ENDIF
048200030822     C                   ENDIF
048300030822     C* CCM
048400030822     C                   IF        %subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):1:3)
048700030822     C                             = 'CCM'
048800030822     C                   EVAL      PiStr=%trim(%subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049100030822     C                   EXSR      CHKNUM
049200030822     C                   IF        PiInt=*on
049300030822     C                   Z-ADD     PiVal         VABCCM
049400030822     C                   ENDIF
049500030822     C                   ENDIF
049600030822     C* LNP
049700030822     C                   IF        %subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):1:3)
050000030822     C                             = 'LNP'
050100030822     C                   EVAL      PiStr=%trim(%subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050400030822     C                   EXSR      CHKNUM
050500030822     C                   IF        PiInt=*on
050600030822     C                   Z-ADD     PiVal         VABLNP
050700030822     C                   ENDIF
050800030822     C                   ENDIF
050900030822     C* NRS
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'NRS'
051400030822     C                   EVAL      PiStr=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   EXSR      CHKNUM
051800030822     C                   IF        PiInt=*on
051900030822     C                   Z-ADD     PiVal         VABNRS
052000030822     C                   ENDIF
052100030822     C                   ENDIF
052200030822     C* CTR
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'CTR'
052700030822     C                   EVAL      PiStr=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   EXSR      CHKNUM
053100030822     C                   IF        PiInt=*on
053200030822     C                   Z-ADD     PiVal         VABCTR
053300030822     C                   ENDIF
053400030822     C                   ENDIF
053500030822     C* PKB
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'PKB'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiNum=*on
054500030822     C                   Z-ADD     PiVal         VABPKB
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* VLB
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'VLB'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiNum=*on
055800030822     C                   Z-ADD     PiVal         VABVLB
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* QFT
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'QFT'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiNum=*on
057100030822     C                   Z-ADD     PiVal         VABQFT
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* CBO
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'CBO'
057900030822     C                   EVAL      VABCBO=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* TSP
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'TSP'
058800030822     C                   EVAL      VABTSP=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* VAS
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'VAS'
059700030822     C                   EVAL      VABVAS=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* VCA
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'VCA'
060600030822     C                   EVAL      VABVCA=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000070925     C* TIC
061100070925     C                   IF        %subst(
061200070925     C                             %subst(vlrppt:posDaDft+1:
061300070925     C                             posADft-posDaDft-1):1:3)
061400070925     C                             = 'TIC'
061500070925     C                   EVAL      VABTIC=%trim(%subst(
061600070925     C                             %subst(vlrppt:posDaDft+1:
061700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800070925     C                   ENDIF
061900030822     C* GCA
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'GCA'
062400030822     C                   EVAL      VABGCA=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* CTM
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'CTM'
063300030822     C                   EVAL      VABCTM=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* FFD
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'FFD'
064200030822     C                   EVAL      VABFFD=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* VAD
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'VAD'
065100030822     C                   EVAL      VABVAD=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* GMA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'GMA'
066000030822     C                   EVAL      VABGMA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* GGA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'GGA'
066900030822     C                   EVAL      VABGGA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* GVA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'GVA'
067800030822     C                   EVAL      VABGVA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TC1
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TC1'
068700030822     C                   EVAL      VABTC1=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* TC2
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'TC2'
069600030822     C                   EVAL      VABTC2=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000040714     C* VATTRC
070100040714     C                   IF        %subst(
070200040714     C                             %subst(vlrppt:posDaDft+1:
070300040714     C                             posADft-posDaDft-1):1:3)
070400040714     C                             = 'TRC'
070500040714     C                   EVAL      VATTRC=%trim(%subst(
070600040714     C                             %subst(vlrppt:posDaDft+1:
070700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800040714     C                   ENDIF
070900030822     C* ...
071000030822     C                   ENDIF
071100030822     C                   ENDDO
071200020204     C*
071300010330     C                   ENDSR
071400010607     C*----------------------------------------------------*
071500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
071600010607     C*----------------------------------------------------*
071700010607     C     IMPVAB        BEGSR
071800010607     C*
071900010607     C                   SETOFF                                       3132
072000030515     C*
072100010607     C                   MOVEL     datcor        VABAAS
072200010607     C                   MOVE      datcor        VABMGS
072300010607     C*
072400030715     C**********
072500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
072600030715     C**********
072700030715     C                   IF        wGiro = *zeros
072800080716     C*
072900080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073000080716     C                   EVAL      depvinDTA = vinDTA
073100040113     C*
073200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
073300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
073400090716     C                             %trim(intcol(3))+%trim(intcol(4))+
073500090716     C                             %trim(intcol(5))<>*blanks
073600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
073700090716     C                                      %trim(intcol(3))+%trim(intcol(4))+
073800090716     C                                      %trim(intcol(5))
073900100204     C     ';':CharCSV   XLATE     vindta        vindta
074000040510     C                   ENDIF
074100080626     C*
074200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074300031201     C     minu:maiu     XLATE     vindta        vindta
074400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074500030715     C                   DOW       posDa <= %len(%trim(vindta))
074600030715     C*
074700030715     C* Gestisco il 1° campo
074800030715     C                   IF        i = 1
074900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075000030715     C                   EVAL      posDa = 1
075100030715     C* Eseguo lo scan x trovare la fine del primo campo
075200100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075300030715     C* A questo punto "estraggo" il campo corrente
075400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075500030715     C                                                    (posA-posDa))
075600030715     C* X i campi successivi al 1°
075700030715     C                   ELSE
075800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075900030715     C                   EVAL      posDa = posA + 1
076000030715     C* Eseguo lo scan x trovare la fine del campo corrente
076100100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076200030715     C* Gestisco l'ultimo campo
076300030715     C                   IF        posA = *zeros
076400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076500030715     C* A questo punto "estraggo" il campo corrente
076600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076700030715     C                                                    (posA-posDa))
076800030715     C                   LEAVE
076900030715     C                   ELSE
077000030715     C* A questo punto "estraggo" il campo corrente
077100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077200030715     C                                                    (posA-posDa))
077300030715     C                   ENDIF
077400030715     C                   ENDIF
077500030715     C* Incremento il contatore d campo
077600030715     C                   EVAL      i = i +1
077700030715     C                   ENDDO
077800030715     C*
077900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078000030715     C* delimitazione testo
078100030715     C                   EVAL      i = 1
078200030715     C                   DOW       i < %elem(SkSplitFLD)
078300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078500030715     C                   EVAL      i = i + 1
078600030715     C                   ENDDO
078700030715     C                   EVAL      wGiro = 1
078800080716     C* Ripristino la 1° riga originale
078900080716     C                   EVAL      vinDTA = depvinDTA
079000030715     C                   ELSE
079100030715     C                   EVAL      wGiro = 2
079200031204     C**********
079300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079400031204     C* d separatore campo
079500031204     C**********
079600031204     C                   EVAL      vindta = %trim(vindta)
079700100204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079800031204     C                   EVAL      vindta = %subst(vindta:2)
079900031204     C                   ENDDO
080000031204     C*
080100031204     C                   Z-ADD     *zeros        lunghInput        4 0
080200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080300100204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080600031204     C                   ENDDO
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input
080900030715     C**********
081000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081100030715     C                   DOW       posDa <= %len(%trim(vindta))
081200030715     C*
081300030715     C* Gestisco il 1° campo
081400030715     C                   IF        i = 1
081500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081600030715     C                   EVAL      posDa = 1
081700030715     C* Eseguo lo scan x trovare la fine del primo campo
081800100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* A questo punto "estraggo" il campo corrente
082000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082100030715     C                                                    (posA-posDa))
082200030715     C* X i campi successivi al 1°
082300030715     C                   ELSE
082400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082500030715     C                   EVAL      posDa = posA + 1
082600031204     C* Verifico che nn vi sia il campo nn valorizzato
082700100204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082800031204     C* Se campo nn valorizzato skippo al prossimo
082900031204     C                   EVAL      SkSplitCSV(i) = *blanks
083000031204     C                   EVAL      posA  = posA + 1
083100031204     C                   ELSE
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300100204     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600031204     C                   ENDIF
084700030715     C                   ENDIF
084800030715     C* Incremento il contatore d campo
084900030715     C                   EVAL      i = i +1
085000030715     C                   ENDDO
085100030715     C*
085200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085300030715     C* delimitazione testo
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitCSV)
085600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085800030715     C                   EVAL      i = i + 1
085900030715     C                   ENDDO
086000030715     C*
086100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086200030715     C                   EVAL      i = 1
086300030715     C                   DOW       i < %elem(SkSplitFLD)
086400030715     C*
086500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086700030715     C                             SkSplitCSV(i) <> *blanks
086800030721     C*** ATB
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087000030721     C                   EVAL      VABATB = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030715     C*** RSD
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087700030715     C* ==
087800030715     C                   ENDIF
087900030721     C*** RD2
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030715     C*** IND
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088500030715     C                   EVAL      VABIND = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** LOD
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
088900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** PRD
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030721     C*** NZD
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030715     C*** RMA
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** NOT
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** NT2
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
090900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** VCA
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** VAS
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030715     C*** FFD
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030721     C*** GC1
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030721     C*** GC2
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
092900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** TSP
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093400030721     C                   ENDIF
093500040510     C*** CBO
093600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
093700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
093800040510     C                   ENDIF
093900030721     C*** NAS
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** TIC
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** GCA
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** XCO
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** CTM
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
095700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** TCR
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** CTS
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** FTM
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** VAD
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** GMA
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
097700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** GGA
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GVA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** TC1
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** TC2
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** SCL
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
099700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** RMO
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** NMO
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*
100800030721     C* Reperisco quindi i campi numerici...
100900040510     C*** NSP
101000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101100040510     C                   EVAL      PiStr=SkSplitCSV(i)
101200040510     C                   EXSR      CHKNUM
101300040510     C                   IF        PiInt=*on
101400040510     C                   Z-ADD     PiVal         VABNSP
101500040510     C                   ELSE
101600040510     C                   SETON                                        31
101700040510     C                   EVAL      VABNSP = *zeros
101800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
101900040510     C                             + ' ' + 'VABNSP'
102000040510     C                   ENDIF
102100040510     C                   ENDIF
102200071106     C*** RMN
102300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102400071106     C                   EVAL      PiStr=SkSplitCSV(i)
102500071106     C                   EXSR      CHKNUM
102600071106     C                   IF        PiInt=*on
102700071106     C                   Z-ADD     PiVal         VABRMN
102800071106     C                   ELSE
102900071106     C                   SETON                                        32
103000071106     C                   EVAL      VABRMN = *zeros
103100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
103200071106     C                             + ' ' + 'VABRMN'
103300071106     C                   ENDIF
103400071106     C                   ENDIF
103500030721     C*** CAD
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
103700030721     C                   EVAL      PiStr=SkSplitCSV(i)
103800030721     C                   EXSR      CHKNUM
103900030721     C                   IF        PiInt=*on
104000030721     C                   Z-ADD     PiVal         Num5_0
104100030721     C                   MOVEL(p)  Num5_0        VABCAD
104200030721     C                   ELSE
104300090603     C                   EVAL      VABCAD = SkSplitCSV(i)
104400090603     C                   SETON                                        32
104500090603     C                   EVAL      VABCAD = *zeros
104600090603     C                   EVAL      vinmsg = %trimr(vinmsg)
104700090603     C                             + ' ' + 'VABCAD'
104800030721     C                   ENDIF
104900030721     C                   ENDIF
105000030721     C*** NCL
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105200030721     C                   EVAL      PiStr=SkSplitCSV(i)
105300030721     C                   EXSR      CHKNUM
105400030721     C                   IF        PiInt=*on
105500030721     C                   Z-ADD     PiVal         VABNCL
105600030721     C                   ELSE
105700030721     C                   SETON                                        32
105800030721     C                   EVAL      VABNCL = *zeros
105900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106000040122     C                             + ' ' + 'VABNCL'
106100030721     C                   ENDIF
106200030721     C                   ENDIF
106300030721     C*** PKB
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106500030721     C                   EVAL      PiStr=SkSplitCSV(i)
106600030721     C                   EXSR      CHKNUM
106700030721     C                   IF        PiNum=*on
106800030721     C                   Z-ADD     PiVal         VABPKB
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      VABPKB = *zeros
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABPKB'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** CAS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107800080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
107900080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
108000081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
108100090716     C                             %trim(SkSplitCSV(i)) <> '.0'     AND
108200090716     C                             %trim(SkSplitCSV(i)) <> '.00'    AND
108300090716     C                             %trim(SkSplitCSV(i)) <> ',0'     AND
108400090716     C                             %trim(SkSplitCSV(i)) <> ',00'    AND
108500090716     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
108600090716     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
108700090716     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
108800081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
108900080125     C                   EVAL      wFlgCAS = '1'
109000080125     C                   ENDIF
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiNum=*on
109400030721     C                   Z-ADD     PiVal         VABCAS
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      VABCAS = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABCAS'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** CCM
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on      AND
110700030721     C                             PiVal<=9999999 AND
110800030721     C                             PiVal>*zeros
110900030721     C                   Z-ADD     PiVal         VABCCM
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABCCM = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040510     C                             + ' ' + 'VABCCM'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** LNP
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on    AND
112200030721     C                             PiVal<=999   AND
112300030721     C                             PiVal>*zeros
112400030721     C                   Z-ADD     PiVal         VABLNP
112500030721     C                   ELSE
112600040715     C                   SETON                                        32
112700030721     C                   EVAL      VABLNP = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900040510     C                             + ' ' + 'VABLNP'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** NRS
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on AND
113700030721     C                             PiVal<=99
113800030721     C                   Z-ADD     PiVal         VABNRS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABNRS'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** LNA
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on    AND
115000030721     C                             PiVal<=999   AND
115100030721     C                             PiVal>*zeros
115200030721     C                   Z-ADD     PiVal         VABLNA
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABLNA = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABLNA'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** CTR
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on  AND
116500030721     C                             PiVal<=999
116600030721     C                   Z-ADD     PiVal         VABCTR
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABCTR'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** IAS
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABIAS
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABIAS'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** VLB
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiNum=*on
119000030721     C                   Z-ADD     PiVal         VABVLB
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABVLB'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** QFT
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABQFT
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABQFT'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** NCD
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on      AND
121400030721     C                             PiVal<=9999999
121500030721     C                   Z-ADD     PiVal         VABNCD
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABNCD'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** NCA
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on      AND
122700030721     C                             PiVal<=9999999
122800030721     C                   Z-ADD     PiVal         VABNCA
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABNCA'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** ZNC
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on AND
124000030721     C                             PiVal<=99
124100030721     C                   Z-ADD     PiVal         VABZNC
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABZNC'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** DCR
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on       AND
125300030721     C                             PiVal<=99999999
125400030721     C                   Z-ADD     PiVal         VABDCR
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABDCR'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** HCR
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on   AND
126600030721     C                             PiVal<=9999
126700030721     C                   Z-ADD     PiVal         VABHCR
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABHCR'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** VMD
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiNum=*on
127900030721     C                   Z-ADD     PiVal         VABVMD
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABVMD'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** ANT
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on        AND
129100030721     C                             PiVal<=999999999
129200030721     C                   Z-ADD     PiVal         VABANT
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABANT'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** CMO
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on
130400030721     C                   Z-ADD     PiVal         Num5_0
130500030721     C                   MOVEL(p)  Num5_0        VABCMO
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      VABCMO = *zeros
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABCMO'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300040510     C*
131400040510     C* Reperisco i campi del FIVAT
131500040510     C*** VATNOT
131600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
131700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
131800040510     C                   ENDIF
131900040510     C*** VATTRC
132000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
132100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
132200040510     C                   ENDIF
132300060331     C*** VATNOT_A
132400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
132500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
132600060331     C                   ENDIF
132700060331     C*** VATNOT_B
132800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
132900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
133000060331     C                   ENDIF
133100060331     C*** VATNOT_E
133200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
133300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
133400060331     C                   ENDIF
133500070823     C*
133600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
133700090421     C*
133800090716     C*** W_VABNSP/VABRMN/VABRMA
133900090716     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
134000090622     C                   EVAL      VABRMA = SkSplitCSV(i)
134100090622     C                   EVAL      PiStr=SkSplitCSV(i)
134200090622     C                   EXSR      CHKNUM
134300090622     C                   IF        PiInt=*on
134400090622     C                   Z-ADD     PiVal         VABRMN
134500090622     C                   Z-ADD     PiVal         VABNSP
134600090622     C                   ELSE
134700090622     C                   SETON                                        32
134800090622     C                   EVAL      VABRMN = *zeros
134900090622     C                   EVAL      VABNSP = *zeros
135000090622     C                   EVAL      vinmsg = %trimr(vinmsg)
135100090622     C                             + ' ' + 'VABRMN VABNSP'
135200090622     C                   ENDIF
135300090603     C                   ENDIF
135400070925     C*
135500071029     C***  <===  -----------------------------------------
135600040510     C*
135700030715     C                   ENDIF
135800030715     C                   EVAL      i = i + 1
135900030715     C                   ENDDO
136000070925     C*
136100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
136200080125     C*
136300070925     C*
136400071029     C***  <===  -----------------------------------------
136500070925     C*
136600070925     C* Considerazioni finali su CBO/CAS
136700080125     C                   IF        wFlgCAS = '1'
136800070925     C                   IF        VABCBO = '1'
136900070925     C                   EVAL      VABCBO = '4'
137000070925     C                   ENDIF
137100070925     C                   IF        VABCBO = '2'
137200070925     C                   EVAL      VABCBO = '6'
137300070925     C                   ENDIF
137400070925     C                   ENDIF
137500070925     C***  <===  ----------------------------
137600040510     C*
137700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
137800040510     C                   IF        VABNSP = *zeros
137900040510     C                   SETON                                        31
138000040510     C                   ENDIF
138100040715     C*
138200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
138300040715     C                   IF        VABLNP = *zeros
138400040715     C                   SETON                                        31
138500040715     C                   ENDIF
138600040510     C*
138700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
138800040510     C                   IF        VABRMN = *zeros
138900040510     C                   Z-ADD     VABNSP        VABRMN
139000040510     C                   ENDIF
139100040510     C*
139200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
139300040510     C                   IF        VABRMA = *blanks
139400040714     C                   MOVEL     VABRMN        VABRMA
139500040510     C                   ENDIF
139600040128     C*
139700040510     C* Se provincia nn valorizzata la reperisco
139800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
139900040510     C                   IF        VABPRD = *blanks
140000040128     C                   CLEAR                   TISI95DS
140100040128     C                   EVAL      I95TCN = '3'
140200040128     C                   Z-ADD     datcor        I95DAT
140300040128     C                   EVAL      I95NAR = VABNZD
140400040128     C                   EVAL      I95CAP = VABCAD
140500040128     C                   EVAL      I95LOC = VABLOD
140600040128     C                   CALL      'TISI95R'
140700040128     C                   PARM                    TISI95DS
140800040128     C                   EVAL      VABPRD = O95PRV
140900040510     C                   ENDIF
141000020204     C*
141100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
141200020204     C                   EXSR      CHKIMPDIV
141300030715     C                   ENDIF
141400020204     C*
141500010607     C                   ENDSR
141600010607     C*----------------------------------------------------*
141700010601
141800020204
141900020204     C*----------------------------------------------------*
142000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
142100020204     C*----------------------------------------------------*
142200020204     C     CHKIMPDIV     BEGSR
142300020204     C*
142400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
142500020204     C                   Z-ADD     *zeros        wrkDec            9 9
142600020204     C*
142700020204     C* Come prima cosa effettuo considerazioni sulla divisa
142800020204     C                   IF        vabIAS > *zeros
142900020204     C                   IF        vabVAS <> 'EUR'
143000020204     C                   EVAL      vabVAS =  'ITL'
143100020204     C                   ENDIF
143200020204     C                   ENDIF
143300020204     C*
143400020204     C                   IF        vabCAS > *zeros
143500020204     C                   IF        vabVCA <> 'EUR'
143600020204     C                   EVAL      vabVCA =  'ITL'
143700020204     C                   ENDIF
143800020204     C                   ENDIF
143900020204     C*
144000020204     C                   IF        vabVMD > *zeros
144100020204     C                   IF        vabVAD <> 'EUR'
144200020204     C                   EVAL      vabVAD =  'ITL'
144300020204     C                   ENDIF
144400020204     C                   ENDIF
144500020204     C*
144600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
144700020204     C                   Z-ADD     vabIAS        wrkDec
144800020204     C                   IF        wrkDec > *zeros
144900020204     C                   IF        vabVAS = 'ITL'
145000020204     C                   EVAL      vabIAS = *zeros
145100020204     C                   ENDIF
145200020204     C                   ENDIF
145300020204     C*
145400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
145500020204     C                   Z-ADD     vabCAS        wrkDec
145600020204     C                   IF        wrkDec > *zeros
145700020204     C                   IF        vabVCA = 'ITL'
145800020204     C                   EVAL      vabCAS = *zeros
145900020204     C                   ENDIF
146000020204     C                   ENDIF
146100020204     C*
146200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
146300020204     C                   Z-ADD     vabVMD        wrkDec
146400020204     C                   IF        wrkDec > *zeros
146500020204     C                   IF        vabVAD = 'ITL'
146600020204     C                   EVAL      vabVMD = *zeros
146700020204     C                   ENDIF
146800020204     C                   ENDIF
146900020204     C*
147000020204     C                   ENDSR
147100020204     C***
147200080617
147300080617
147400080617
147500080617     C*----------------------------------------------------*
147600080617     C*  CONTROLLO NUMERICITA' CAMPI
147700080617     C*----------------------------------------------------*
147800080617     C     CHKNUM        BEGSR
147900080617     C*
148000080617     C                   IF        PiDecChr = *blanks
148100080617     C                   EVAL      PiDecChr = CharNUM
148200080617     C                   ENDIF
148300080617     C*
148400080617     C                   callp     UBISNUM_Check(PiStr
148500080617     C                                          :PiDecChr
148600080617     C                                          :PiVal
148700080617     C                                          :PiNum
148800080617     C                                          :PiInt)
148900080617     C*
149000080617     C                   ENDSR
149100080617     C***
149200010330
149300010601
149400010601
149500010601
149600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149700010601     C     repfil        BEGSR
149800010601     C*
149900010601     C                   if        invfil = *zeros and
150000010601     C                             depfil > *zeros and
150100010629     C                             (vinflg = *blanks or
150200010629     C                              vinflg = *zeros)
150300010601     C
150400010601     C                   eval      invfil = depfil
150500010601     C                   endif
150600010601     C*
150700010601     C                   if        depfil <> invfil and
150800010601     C                             invfil > *zeros
150900010601     C                   eval      flgMulti = '1'
151000010601     C                   if        vinflg = *blanks
151100010601     C                   add       1             cntNonEl
151200010601     C                   endif
151300010601     C                   endif
151400010601     C*
151500010601     C                   if        vinflg = '2'
151600010601     C                   eval      flgStato = '2'
151700010601     C                   endif
151800010601     C*
151900010601     C                   ENDSR
152000010601     C***
152100010601
152200010601
152300010601
152400010330
152500040119      /TITLE Invio dei dati al punto operativo.
152600040119     C     invio         BEGSR
152700040119     C*
152800040119     C* 1° invio FIVAT
152900040119     C                   reset                   dscmz
153000040119     C                   move      vlrpoi        cmzdst
153100100302     C                   eval      cmzfld = 'EDIVATWR'
153200040119     C                   eval      cmzmbd = vlrhdl
153300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
153400040119     C***                if        prmfir = *blanks
153500100302     C                   eval      cmzfla = 'EDIVAT0F'
153600100302     C                   eval      cmzmba = 'EDIVAT0F'
153700040119     C***                else
153800040119     C***                eval      cmzfla = prmfir
153900040119     C***                eval      cmzmba = prmfir
154000040119     C***                endif
154100040119     C                   eval      cmznrr = *zeros
154200040119     C                   move      §ctrokvt      cmznrr
154300040119     C                   eval      cmzlba = vlrfl1
154400040119     C                   call(e)   'TIS711C'
154500040119     C                   parm                    dscmz
154600040119     C                   parm      *blanks       esito
154700040119     C                   if        %error
154800040119     C                             or cmzerr = '1'
154900040119     C                             or esito  = '1'
155000040119     C                   eval      wrkesito = '3'
155100040119     C                   else
155200040119     C*
155300040119     C* 2° invio FIVAB
155400040119     C                   reset                   dscmz
155500040119     C                   move      vlrpoi        cmzdst
155600040119     C                   eval      cmzfld = vlrfou
155700040119     C                   eval      cmzmbd = vlrhdl
155800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155900040119     C***                if        prmfir = *blanks
156000100302     C                   eval      cmzfla = 'EDIVAB0F'
156100100302     C                   eval      cmzmba = 'EDIVAB0F'
156200040119     C***                else
156300040119     C***                eval      cmzfla = prmfir
156400040119     C***                eval      cmzmba = prmfir
156500040119     C***                endif
156600040119     C                   eval      cmznrr = *zeros
156700040119     C                   move      §ctrokvb      cmznrr
156800040119     C                   eval      cmzlba = vlrfl1
156900040119     C                   call(e)   'TIS711C'
157000040119     C                   parm                    dscmz
157100040119     C                   parm      *blanks       esito
157200040119     C                   if        %error
157300040119     C                             or cmzerr = '1'
157400040119     C                             or esito  = '1'
157500040119     C                   eval      wrkesito = '3'
157600040119     C                   endif
157700040119     C                   endif
157800040119     C*
157900040119     C                   ENDSR
158000040119     C***
158100010601
158200010601
158300010601
158400010601
158500010601      /TITLE Invio dei dati al punto operativo.
158600010601     C     opeini        BEGSR
158700010601     C*
158800010601     C* Inizializzo flag e contatori operativi
158900010601     C                   movel     '0'           flgGiro           1
159000010601     C                   movel     '0'           flgMulti          1
159100010601     C                   movel     '1'           flgStato          1
159200010615     C                   movel     '0'           flgOk             1
159300010601     C                   z-add     *zeros        cntNonEl         10 0
159400010601     C                   z-add     *zeros        depfil            3 0
159500010601     C                   z-add     *zeros        invfil            3 0
159600010601     C*
159700010601     C                   ENDSR
159800010601     C***
159900070326
160000070326
160100070326
160200070326
160300080916     C     *pssr         BEGSR
160400070329     C*
160500080916     C                   if        %open(tivin00r)
160600080916     C                   close     tivin00r
160700080916     C                   endif
160800100302     C                   if        %open(edivabwr)
160900100302     C                   close     edivabwr
161000080916     C                   endif
161100100302     C                   if        %open(edivatwr)
161200100302     C                   close     edivatwr
161300080916     C                   endif
161400070326     C*
161500070326     C* Effettuo la chiamata al CLLE preposto
161600100302     C                   call(e)   'TITVEVTC'
161700080916     C                   parm                    parccm
161800080916     C                   parm                    parmbr
161900080916     C                   parm      '2'           paropz
162000070326     C*
162100080916     C                   eval      wrkesito = '2'
162200070404     C*
162300080916     C                   seton                                        LR
162400070326     C*
162500080916     C                   ENDSR     '*CANCL'
162600070326     C***
162700070326
162800100204
162900100204
163000100204     C     normCSV       BEGSR
163100100204     C*
163200100204     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
163300100204     C* separatori campi
163400100204     C                   if        CharTXT <> *blanks
163500100204     C                   z-add     *zeros        wLenVINDTA        4 0
163600100204     C                   z-add     *zeros        wFlgVINDTA        1 0
163700100204     C                   setoff                                       80
163800100204     C                   dow       wLenVINDTA < %len(%trim(vindta))
163900100204     C                   eval      wLenVINDTA = wLenVINDTA + 1
164000100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
164100100204     C                   if        not *in80
164200100204     C                   seton                                        80
164300100204     C                   else
164400100204     C                   setoff                                       80
164500100204     C                   endif
164600100204     C                   endif
164700100204     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
164800100204     C                             and *in80 = *on
164900100204     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
165000100204     C                   endif
165100100204     C                   enddo
165200100204     C                   endif
165300100204     C*
165400100204     C                   ENDSR
165500100204     C***
165600070326
165700010330
165800010330
165900000613     C     *inzsr        BEGSR
166000990910     C*
166100990910     C     *entry        plist
166200990920     C                   parm                    tivlrds
166300990921     C                   parm      wrkesito      esito
166400000724     C                   parm                    prmlit
166500000710     C                   parm                    prmfir
166600010330     C*
166700010330     C* CALCOLA LA DATA CORRENTE
166800100302     C                   time                    wn14             14 0
166900100302     C                   movel     wn14          oracor            6 0          *ORA
167000100302     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
167100100204     C                   eval      datcor = %dec(%date() : *ISO)
167200100302     C*
167300100302     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
167400100302     C                   MOVE(p)   *blanks       wNomeFile        30
167500100302     C                   MOVE(p)   vlrMSG        wNomeFile
167600100302     C     minu:maiu     XLATE     wNomeFile     wNomeFile
167700000613     C*
167800000613     C                   ENDSR
167900000613     C***
168000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
168100121115X001;X002;X003;VABNAS;VABPKB;VATNOT_E;W_VABNSP;VABRSD;VATNOT_A;
168200121115VABRD2;VABNT2;VABIND;X004;VABCAD;VABLOD;VABPRD;VABRMA;VATNOT_B;
168300121115VABNOT;VABNCL
168400100302
168500100302
