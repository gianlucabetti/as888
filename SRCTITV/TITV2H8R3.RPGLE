000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600110930     FEDIVATwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000110930     D edivabs       e ds                  extname(edivab0f)
002100110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002200110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800040510     D parccm          s              8    INZ(*blanks)
002900040119     D parmbr          s             10    INZ(*blanks)
003000040119     D paropz          s              1    INZ(*blanks)
003100040119     D chkcall         s              1    INZ(*blanks)
003200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003300080418     D wFlgCAS         s              1    INZ(*blanks)
003400130722     D*
003500130722     D DS_VATNOT_S     ds
003600130722     D   NOT_S_CELL                  16
003700130722     D   NOT_S_CODOPE                 1
003800100326     D*------------------
003900100326     D* DS REPERIMENTO NUMERATORE
004000100326     D*------------------
004100100326     D trul33ds      e ds                  inz
004200100326     D*------------------
004300100326     D* DS ARCHITETTURA
004400100326     D*------------------
004500100326     D kpjba         e ds                  inz
004600100326
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000030715     D SkSplitFLD      S             10    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500100318     D CharSOS         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000070823     D wVATNOT_A       s                   LIKE(VATNOT)
006100060331     D wVATNOT_B       s                   LIKE(VATNOT)
006200060331     D wVATNOT_E       s                   LIKE(VATNOT)
006300110927     D wVATNOT_I       s                   LIKE(VATNOT)
006400130722     D wVATNOT_S       s                   LIKE(VATNOT)
006500100831     D wVABRSD         s                   LIKE(VABRSD)
006600100831     D wVABIND         s                   LIKE(VABIND)
006700100831     D wVABCAD         s                   LIKE(VABCAD)
006800100831     D wVABLOD         s                   LIKE(VABLOD)
006900100831     D wVABPRD         s                   LIKE(VABPRD)
007000030822     D*------------------
007100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007200030822     D*------------------
007300030822     D posDaDft        S              3  0 INZ(*zeros)
007400030822     D posADft         S              3  0 INZ(*zeros)
007500030822     D j               s              3  0 INZ(1)
007600030822     D wGiroDft        s              1  0 INZ(*zeros)
007700080418     D*------------------
007800080418     D* Costanti
007900080418     D*------------------
008000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200100318
008300100318     D*------------------
008400100318     D* LINKING A DEFINIZIONI ESTERNE
008500100318     D*------------------
008600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008800100318
008900100909     D*------------------
009000100909     D* DS CALCOLO CHECK-DIGITS
009100100909     D*------------------
009200100909     D TRUL28DSX     e ds                  inz
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100040119     C*
013200040119     C* Effettuo la chiamata al CLLE preposto
013300110930     C                   call(e)   'TITVEVTC'
013400040510     C                   parm                    parccm
013500040119     C                   parm                    parmbr
013600040119     C                   parm      '2'           paropz
013700050201     C*
013800050201     C* Effettuo lancio TISI95 solo x chiusura
013900050201     C                   CLEAR                   TISI95DS
014000050201     C                   EVAL      I95TLA = 'C'
014100050201     C                   CALL      'TISI95R'
014200050201     C                   PARM                    TISI95DS
014300000616     C*
014400010601     C                   ENDSR
014500010601     C***
014600000613
014700010601
014800010601
014900010330     C*--------------------------------------------------------
015000110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015100010330     C*--------------------------------------------------------
015200010605     C     RWVAB         BEGSR
015300010330     C*
015400010330     C                   if        not %open(tivin00r)
015500010330     C                   open      tivin00r
015600010330     C                   endif
015700110930     C                   if        not %open(edivabwr)
015800110930     C                   open      edivabwr
015900010330     C                   endif
016000040119     C*
016100110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016200040119     C                   exsr      prevat
016300040119     C*
016400040119     C                   if        chkcall = '0'
016500040119     C*
016600110930     C                   if        not %open(edivatwr)
016700110930     C                   open      edivatwr
016800040119     C                   endif
016900010330     C*
017000040119     C                   clear                   §CTROKVB          7 0
017100040119     C                   clear                   §CTROKVT          7 0
017200040119     C                   clear                   §CTRMO            7 0
017300040119     C                   clear                   §CTRNO            7 0
017400070823     C*
017500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017700110912     C***                exsr      inzvar
017800110912     C***                exsr      impvab
017900110131     C*
018000110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
018100110912 xxx C***                read      tivin00r                               70
018200010330     C*
018300030822     C                   do        *HIVAL
018400010330     C*
018500030822     C                   read      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100040510     C*
019200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019300040510     C                   z-add     *zeros        wLenVINDTA        4 0
019400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019800040510     C                   z-add     1             wFlgVINDTA
019900040510     C                   leave
020000040510     C                   endif
020100040510     C                   enddo
020200010618     C*
020300040510     C                   if        vindta > *blanks AND
020400040510     C                             wFlgVINDTA = 1
020500010330     C                   add       1             rrnum
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021800070925     C                   eval      vindta  = ' ' + %trim(vindta)
021900070925     C*
022000080527     C                   exsr      inzvar
022100100318     C                   exsr      normCSV
022200010605     C                   exsr      impvab
022300010601     C*
022400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022500010605     C                   eval      depfil = VABLNP
022600010601     C                   exsr      repfil
022700010601     C                   if        depfil = invfil
022800021025     C                   if        vlrpoi = 999
022900030822     C                   move(P)   invfil        VABFGS
023000021025     C                   else
023100030822     C                   move(P)   vlrpoi        VABFGS
023200021025     C                   endif
023300010601     C*
023400010601     C                   exsr      PREELA
023500010601     C*
023600010604     C* Ebbene...
023700010604     C*
023800040119     C  N31              add       1             §CTROKVB
023900040119     C   32              add       1             §CTRMO
024000040119     C   31              add       1             §CTRNO
024100080418     C*
024200030822     C                   if        wGiro = 2
024300080513     C* Scrivo sempre il VAT
024400080513     C  N31              exsr      wrivat
024500110201     C                   exsr      chkvab
024600080513     C                   endif
024700010604     C*
024800010604     C                   if        *in31 = *off and
024900010604     C                             *in32 = *off
025000010604     C                   eval      vinflg = '1'
025100010604     C                   else
025200010604     C                   eval      vinflg = '2'
025300010604     C                   endif
025400010604     C                   endif
025500010601     C*
025600010604     C                   endif
025700010604     C*
025800010330     C                   else
025900010330     C                   eval      vinflg = '1'
026000010330     C                   endif
026100010601     C*
026200010601     C  N70              update    tivin000
026300010330     C*
026400030822     C  N70              enddo
026500040119     C*
026600080527     C* Verifico dati rimasti "in cannna"...
026700080527     C                   seton                                        65
026800110201     C                   exsr      chkvab
026900080527     C*
027000040119     C                   endif                                                  (endif - chkcall)
027100010601     C*
027200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027300010601     C                   if        cntNonEl = *zeros or
027400010601     C                             flgMulti = '0'
027500010330     C* Se non ci sono record con errori ...
027600010601     C                   if        §ctrno = 0 and
027700010604     C                             §ctrmo = 0 and
027800010601     C                             flgStato <> '2'
027900010330     C* ... restituisco esito OK.
028000010330     C                   eval      wrkesito = '0'
028100010330     C                   else
028200040119     C                   if        §ctrokvb > 0
028300010330     C                   eval      wrkesito = '1'
028400010330     C                   else
028500010615     C                   if        flgOk = '0'
028600010615     C                   eval      wrkesito = '2'
028700010615     C                   else
028800010615     C                   eval      wrkesito = '6'
028900010615     C                   endif
029000010330     C                   endif
029100010330     C                   endif
029200010601     C                   else
029300010601     C                   eval      wrkesito = '9'
029400010601     C                   endif
029500010330     C*
029600010330     C                   if        %open(tivin00r)
029700010330     C                   close     tivin00r
029800010330     C                   endif
029900110930     C                   if        %open(edivabwr)
030000110930     C                   close     edivabwr
030100010330     C                   endif
030200110930     C                   if        %open(edivatwr)
030300110930     C                   close     edivatwr
030400040512     C                   endif
030500010601     C*
030600010601     C                   if        vlrpoi <> 999
030700010601     C                   eval      invfil = vlrpoi
030800010601     C                   endif
030900010330     C*
031000040119     C                   if        §ctrokvb > 0
031100010601     C                             and invfil > *zeros
031200010330     C                   exsr      invio
031300010330     C                   endif
031400010601     C*
031500010618     C                   if        flgGiro = '1'
031600010601     C                   exsr      endela
031700010618     C                   endif
031800010330     C*
031900010330     C                   ENDSR
032000010330     C***
032100080527
032200080527
032300080527
032400080527     C*----------------------------------------------------*
032500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032600080527     C*----------------------------------------------------*
032700100413     C     CHKVAB        BEGSR
032800110201     C*
032900110201     C* Se richiesti ignorare colli / peso / volume
033000110315 xxx C***                if        *in35
033100110315 xxx C***                z-add     *zeros        VABNCL
033200110315 xxx C***                z-add     *zeros        VABPKB
033300110315 xxx C***                z-add     *zeros        VABVLB
033400110315 xxx C***                endif
033500080527     C*
033600080527     C* Scrivo il VAB solo a rottura d spedizione
033700080527     C                   if        depspe = *zeros
033800110930     C  N31              eval      edivabs_sav = edivabs
033900080527     C                   eval      depspe = vabNSP
034000080527     C                   else
034100080527     C                   if        depspe = vabNSP AND *in65 = *off
034200100413     C   51              add       VABNCL        sav_VABNCL
034300100413     C   51              add       VABPKB        sav_VABPKB
034400100413     C   51              add       VABVLB        sav_VABVLB
034500100720     C   52              z-add     VABNCL        sav_VABNCL
034600100720     C   52              z-add     VABPKB        sav_VABPKB
034700100720     C   52              z-add     VABVLB        sav_VABVLB
034800100720     C   53              add       VABNCL        sav_VABNCL
034900100720     C   53              z-add     VABPKB        sav_VABPKB
035000100720     C   53              z-add     VABVLB        sav_VABVLB
035100080527     C                   else
035200110930     C  N31              eval      edivabs_ok = edivabs_sav
035300080527     C  N31              exsr      wrivab
035400110930     C  N31              eval      edivabs_sav = edivabs
035500080527     C                   eval      depspe = vabNSP
035600080527     C                   endif
035700080527     C                   endif
035800080527     C*
035900080527     C                   ENDSR
036000040119
036100040119
036200040119
036300040119     C*----------------------------------------------------*
036400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036500040119     C*----------------------------------------------------*
036600040510     C     WRIVAB        BEGSR
036700080513     C*
036800080513     C* Gestisco l'eventuale rottura x numero spedizione
036900080527     C                   if        VABNSP <> depspe or *in65 = *on
037000070301     C                   if        wVATNOT_E <> *blanks
037100070301     C                   eval      VABCTM = '7Q'
037200070301     C                   endif
037300110930     C*
037400110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037500130128     C                   EVAL      ok_VABCMR = %char(datcor)+%char(oracor)
037600130128     C                   EVAL      ok_VABDCM = DATCOR
037700130128     C                   EVAL      ok_VABDTS = DATCOR
037800130128     C                   EVAL      ok_VABHMS = ORACOR
037900130128     C                   EVAL      ok_VABCNT = 1
038000110930     C*
038100110930     C                   write     edivab00
038200080513     C                   endif
038300040119     C*
038400040119     C                   ENDSR
038500040119
038600040119
038700040119
038800040119     C*----------------------------------------------------*
038900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039000040119     C*----------------------------------------------------*
039100040510     C     WRIVAT        BEGSR
039200060512     C*
039300060512     C* Inizializzo i campi chiave
039400060512     C                   eval      VATFGS = VABFGS
039500060512     C                   eval      VATAAS = VABAAS
039600060512     C                   eval      VATCCM = VABCCM
039700070823     C                   eval      VATNRS = VABNRS
039800060512     C                   eval      VATNSP = VABNSP
039900060512     C                   eval      VATLNP = VABLNP
040000040119     C*
040100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040200040510     C                   if        VATTRC = *blanks
040300040510     C                   eval      VATTRC = 'E'
040400040510     C                   endif
040500110318     C                   if        VATNOT <> *blanks
040600110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
040700110930     C                   eval      VATCNT = 1
040800110930     C                   write     EDIVAT00
040900040119     C                   add       1             §CTROKVT
041000110318     C                   endif
041100060331     C*
041200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041300060331     C                   if        wVATNOT_A <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_A
041500060331     C                   eval      VATTRC = 'A'
041600110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041700110930     C                   eval      VATCNT = 1
041800110930     C                   write     EDIVAT00
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200060331     C                   if        wVATNOT_B <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_B
042400060331     C                   eval      VATTRC = 'B'
042500110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
042600110930     C                   eval      VATCNT = 1
042700110930     C                   write     EDIVAT00
042800060331     C                   add       1             §CTROKVT
042900060331     C                   endif
043000130722     C*
043100130723     C***                if        wVATNOT_S <> *blanks
043200130723     C***                clear                   DS_VATNOT_S
043300130723     C***                eval      NOT_S_CELL   = wVATNOT_S
043400130723     C***                eval      NOT_S_CODOPE = 'S'
043500130723     C***                eval      VATNOT = DS_VATNOT_S
043600130723     C***                eval      VATTRC = 'S'
043700130723     C***                eval      VATCMR = %char(datcor)+%char(oracor)
043800130723     C***                eval      VATCNT = 1
043900130723     C***                write     EDIVAT00
044000130723     C***                add       1             §CTROKVT
044100130723     C***                endif
044200060331     C*
044300060331     C                   if        wVATNOT_E <> *blanks
044400110201     C                   eval      VATNOT = wVATNOT_E
044500060331     C                   eval      VATTRC = 'E'
044600110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044700110930     C                   eval      VATCNT = 1
044800110930     C                   write     EDIVAT00
044900110201     C                   add       1             §CTROKVT
045000110201     C                   endif
045100110927     C*
045200110927     C                   if        wVATNOT_I <> *blanks
045300110927     C                   eval      VATNOT = wVATNOT_I
045400110927     C                   eval      VATTRC = 'I'
045500110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
045600110930     C                   eval      VATCNT = 1
045700110930     C                   write     EDIVAT00
045800110927     C                   add       1             §CTROKVT
045900110927     C                   endif
046000040119     C*
046100040119     C                   ENDSR
046200040119
046300040119
046400040119
046500040119     C*----------------------------------------------------*
046600110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046700040119     C*----------------------------------------------------*
046800040119     C     PREVAT        BEGSR
046900040119     C*
047000110930     C* Compongo il nome del membro da dare al EDIVATWR
047100040119     C                   eval      parmbr = vlrhdl
047200040119     C                   movel     'M'           parmbr
047300040510     C                   eval      parccm = vlrksc
047400040119     C                   eval      paropz = '1'
047500040119     C* Effettuo la chiamata al CLLE preposto
047600110930     C                   call(e)   'TITVEVTC'
047700040119     C                   parm                    parccm
047800040119     C                   parm                    parmbr
047900040119     C                   parm                    paropz
048000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048100040119     C                   if        %error
048200040119     C                   movel     '1'           chkcall
048300040119     C                   else
048400040119     C                   movel     '0'           chkcall
048500040119     C                   endif
048600040119     C*
048700040119     C                   ENDSR
048800010601
048900010601
049000010601
049100010330     C*----------------------------------------------------*
049200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049300010330     C*----------------------------------------------------*
049400040119     C     INZVAR        BEGSR
049500080527     C*
049600080527     C                   setoff                                       65
049700010330     C*
049800030822     C* Inizializzo variabili di wrk
049900020204     C                   Z-ADD     *zeros        Num5_0            5 0
050000101018     C                   MOVEL     *blanks       wFlgCAS
050100060331     C                   MOVEL     *blanks       wVATNOT_A
050200060331     C                   MOVEL     *blanks       wVATNOT_B
050300060331     C                   MOVEL     *blanks       wVATNOT_E
050400110927     C                   MOVEL     *blanks       wVATNOT_I
050500130722     C                   MOVEL     *blanks       wVATNOT_S
050600100831     C                   MOVEL     *blanks       wVABRSD
050700100831     C                   MOVEL     *blanks       wVABIND
050800100831     C                   MOVEL     *blanks       wVABCAD
050900100831     C                   MOVEL     *blanks       wVABLOD
051000100831     C                   MOVEL     *blanks       wVABPRD
051100030822     C*
051200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051300030822     C                   CLEAR                   SkSplitCSV
051400030822     C                   EVAL      i = 1
051500030822     C                   EVAL      posDa = *zeros
051600030822     C                   EVAL      posA  = *zeros
051700080527     C*
051800110930     C                   CLEAR                   EDIVABS
051900110930     C                   CLEAR                   EDIVAT00
052000030822     C*
052100030822     C* Reimposto i valori di default
052200030822     C                   EXSR      DEFCAM
052300030822     C*
052400010330     C                   ENDSR
052500010330     C*----------------------------------------------------*
052600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052700010330     C*----------------------------------------------------*
052800010330     C     DEFCAM        BEGSR
052900030822     C*
053000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053100030715     C* e delimitatore testo.
053200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053500100318     C*
053600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053700100318     C                   EVAL      CharSOS = CharNUM
053800030822     C*
053900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054000100720     C                   SETOFF                                       505152
054100100720     C                   SETOFF                                       53
054200100413     C                   SELECT
054300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054400100720     C                   SETON                                        50
054500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054600100413     C                   SETON                                        51
054700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054800100720     C                   SETON                                        52
054900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055000100720     C                   SETON                                        53
055100100413     C                   ENDSL
055200030822     C*
055300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055400030822     C                   EVAL      posDaDft = 1
055500030822     C                   EVAL      posADft  = 0
055600030822     C                   EVAL      wGiroDft = 0
055700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055800030822     C                             posDaDft > 0
055900030822     C*
056000030822     C* Gestisco il 1° giro
056100030822     C                   IF        wGiroDft = 0
056200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056400030822     C* Incremento il contatore dei "giri"
056500030822     C                   EVAL      wGiroDft = 1
056600030822     C                   ELSE
056700030822     C                   EVAL      posDaDft = posADft
056800030822     C                   ENDIF
056900030822     C* Eseguo lo scan x trovare la fine del campo corrente
057000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057100030822     C*
057200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057400030822     C                   IF        posDaDft > 0 AND
057500030822     C                             posADft  > 0
057600030822     C* NCL
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'NCL'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABNCL
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* CCM
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'CCM'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABCCM
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* LNP
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'LNP'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABLNP
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* NRS
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'NRS'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABNRS
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* CTR
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'CTR'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiInt=*on
063800030822     C                   Z-ADD     PiVal         VABCTR
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* PKB
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'PKB'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiNum=*on
065100030822     C                   Z-ADD     PiVal         VABPKB
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* VLB
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'VLB'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABVLB
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* QFT
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'QFT'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700030822     C                   Z-ADD     PiVal         VABQFT
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* CBO
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'CBO'
068500030822     C                   EVAL      VABCBO=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* TSP
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'TSP'
069400030822     C                   EVAL      VABTSP=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* VAS
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'VAS'
070300030822     C                   EVAL      VABVAS=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VCA
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VCA'
071200030822     C                   EVAL      VABVCA=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600070925     C* TIC
071700070925     C                   IF        %subst(
071800070925     C                             %subst(vlrppt:posDaDft+1:
071900070925     C                             posADft-posDaDft-1):1:3)
072000070925     C                             = 'TIC'
072100070925     C                   EVAL      VABTIC=%trim(%subst(
072200070925     C                             %subst(vlrppt:posDaDft+1:
072300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400070925     C                   ENDIF
072500030822     C* GCA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'GCA'
073000030822     C                   EVAL      VABGCA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* CTM
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'CTM'
073900030822     C                   EVAL      VABCTM=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* FFD
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'FFD'
074800030822     C                   EVAL      VABFFD=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* VAD
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'VAD'
075700030822     C                   EVAL      VABVAD=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GMA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GMA'
076600030822     C                   EVAL      VABGMA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GGA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GGA'
077500030822     C                   EVAL      VABGGA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* GVA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GVA'
078400030822     C                   EVAL      VABGVA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* TC1
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'TC1'
079300030822     C                   EVAL      VABTC1=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* TC2
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'TC2'
080200030822     C                   EVAL      VABTC2=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600040714     C* VATTRC
080700040714     C                   IF        %subst(
080800040714     C                             %subst(vlrppt:posDaDft+1:
080900040714     C                             posADft-posDaDft-1):1:3)
081000040714     C                             = 'TRC'
081100040714     C                   EVAL      VATTRC=%trim(%subst(
081200040714     C                             %subst(vlrppt:posDaDft+1:
081300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400040714     C                   ENDIF
081500030822     C* ...
081600030822     C                   ENDIF
081700030822     C                   ENDDO
081800020204     C*
081900010330     C                   ENDSR
082000010607     C*----------------------------------------------------*
082100110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
082200010607     C*----------------------------------------------------*
082300010607     C     IMPVAB        BEGSR
082400010607     C*
082500010607     C                   SETOFF                                       3132
082600110315 xxx C***                SETON                                        35
082700030515     C*
082800010607     C                   MOVEL     datcor        VABAAS
082900010607     C                   MOVE      datcor        VABMGS
083000010607     C*
083100030715     C**********
083200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083300030715     C**********
083400030715     C                   IF        wGiro = *zeros
083500040113     C*
083600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083900080429     C                             %trim(intcol(5))<>*blanks
084000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084200080429     C                                      %trim(intcol(5))
084300070823     C     ';':CharCSV   XLATE     vindta        vindta
084400040510     C                   ENDIF
084500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084600031201     C     minu:maiu     XLATE     vindta        vindta
084700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084800030715     C                   DOW       posDa <= %len(%trim(vindta))
084900030715     C*
085000030715     C* Gestisco il 1° campo
085100030715     C                   IF        i = 1
085200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085300030715     C                   EVAL      posDa = 1
085400030715     C* Eseguo lo scan x trovare la fine del primo campo
085500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085600030715     C* A questo punto "estraggo" il campo corrente
085700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085800030715     C                                                    (posA-posDa))
085900030715     C* X i campi successivi al 1°
086000030715     C                   ELSE
086100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086200030715     C                   EVAL      posDa = posA + 1
086300030715     C* Eseguo lo scan x trovare la fine del campo corrente
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* Gestisco l'ultimo campo
086600030715     C                   IF        posA = *zeros
086700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C                   LEAVE
087200030715     C                   ELSE
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C                   ENDIF
087700030715     C                   ENDIF
087800030715     C* Incremento il contatore d campo
087900030715     C                   EVAL      i = i +1
088000030715     C                   ENDDO
088100030715     C*
088200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088300030715     C* delimitazione testo
088400030715     C                   EVAL      i = 1
088500030715     C                   DOW       i < %elem(SkSplitFLD)
088600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088800030715     C                   EVAL      i = i + 1
088900030715     C                   ENDDO
089000030715     C                   EVAL      wGiro = 1
089100030715     C                   ELSE
089200030715     C                   EVAL      wGiro = 2
089300031204     C**********
089400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089500031204     C* d separatore campo
089600031204     C**********
089700031204     C                   EVAL      vindta = %trim(vindta)
089800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089900031204     C                   EVAL      vindta = %subst(vindta:2)
090000031204     C                   ENDDO
090100031204     C*
090200031204     C                   Z-ADD     *zeros        lunghInput        4 0
090300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090700110912     C                   ENDDO
090800030715     C**********
090900030715     C* Effettuo lo split del campo dati d input
091000030715     C**********
091100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091200030715     C                   DOW       posDa <= %len(%trim(vindta))
091300030715     C*
091400030715     C* Gestisco il 1° campo
091500030715     C                   IF        i = 1
091600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091700030715     C                   EVAL      posDa = 1
091800030715     C* Eseguo lo scan x trovare la fine del primo campo
091900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C* X i campi successivi al 1°
092400030715     C                   ELSE
092500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092600030715     C                   EVAL      posDa = posA + 1
092700031204     C* Verifico che nn vi sia il campo nn valorizzato
092800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092900031204     C* Se campo nn valorizzato skippo al prossimo
093000031204     C                   EVAL      SkSplitCSV(i) = *blanks
093100031204     C                   EVAL      posA  = posA + 1
093200031204     C                   ELSE
093300030715     C* Eseguo lo scan x trovare la fine del campo corrente
093400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093500030715     C* Gestisco l'ultimo campo
093600030715     C                   IF        posA = *zeros
093700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093800030715     C* A questo punto "estraggo" il campo corrente
093900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094000030715     C                                                    (posA-posDa))
094100030715     C                   LEAVE
094200030715     C                   ELSE
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   ENDIF
094700031204     C                   ENDIF
094800030715     C                   ENDIF
094900030715     C* Incremento il contatore d campo
095000030715     C                   EVAL      i = i +1
095100030715     C                   ENDDO
095200030715     C*
095300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095400030715     C* delimitazione testo
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitCSV)
095700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095900030715     C                   EVAL      i = i + 1
096000030715     C                   ENDDO
096100100720     C*
096200100720     C* Stacco un progressivo x numero spedizione
096300120411 xxx C***                EXSR      repNSP
096400100326     C*
096500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096600030715     C                   EVAL      i = 1
096700030715     C                   DOW       i < %elem(SkSplitFLD)
096800030715     C*
096900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097100030715     C                             SkSplitCSV(i) <> *blanks
097200030721     C*** ATB
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097400030721     C                   EVAL      VABATB = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** RSD
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098100030715     C* ==
098200030715     C                   ENDIF
098300030721     C*** RD2
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030715     C*** IND
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098900030715     C                   EVAL      VABIND = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** LOD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** PRD
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030721     C*** NZD
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030715     C*** RMA
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** NOT
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** NT2
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** VCA
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** VAS
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** FFD
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030721     C*** GC1
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** GC2
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** TSP
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103800030721     C                   ENDIF
103900040510     C*** CBO
104000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104200040510     C                   ENDIF
104300030721     C*** NAS
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** TIC
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GCA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** XCO
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** CTM
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TCR
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** CTS
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** FTM
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** VAD
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** GMA
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** GGA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** GVA
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** TC1
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** TC2
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** SCL
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** RMO
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** NMO
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*
111200030721     C* Reperisco quindi i campi numerici...
111300040510     C*** NSP
111400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111500040510     C                   EVAL      PiStr=SkSplitCSV(i)
111600040510     C                   EXSR      CHKNUM
111700040510     C                   IF        PiInt=*on
111800040510     C                   Z-ADD     PiVal         VABNSP
111900040510     C                   ELSE
112000040510     C                   SETON                                        31
112100040510     C                   EVAL      VABNSP = *zeros
112200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112300040510     C                             + ' ' + 'VABNSP'
112400040510     C                   ENDIF
112500040510     C                   ENDIF
112600071106     C*** RMN
112700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112800071106     C                   EVAL      PiStr=SkSplitCSV(i)
112900071106     C                   EXSR      CHKNUM
113000071106     C                   IF        PiInt=*on
113100071106     C                   Z-ADD     PiVal         VABRMN
113200071106     C                   ELSE
113300071106     C                   SETON                                        32
113400071106     C                   EVAL      VABRMN = *zeros
113500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113600071106     C                             + ' ' + 'VABRMN'
113700071106     C                   ENDIF
113800071106     C                   ENDIF
113900030721     C*** CAD
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on
114400030721     C                   Z-ADD     PiVal         Num5_0
114500030721     C                   MOVEL(p)  Num5_0        VABCAD
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABCAD = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABCAD'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** NCL
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on
115800030721     C                   Z-ADD     PiVal         VABNCL
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABNCL = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040122     C                             + ' ' + 'VABNCL'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** PKB
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiNum=*on
117100030721     C                   Z-ADD     PiVal         VABPKB
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABPKB = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABPKB'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** CAS
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118200110506     C                             %trim(SkSplitCSV(i)) <> *zeros
118300101018     C                   EVAL      wFlgCAS = '1'
118400101018     C                   ENDIF
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABCAS
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCAS = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABCAS'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** CCM
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on      AND
120100030721     C                             PiVal<=9999999 AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABCCM
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      VABCCM = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800040510     C                             + ' ' + 'VABCCM'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** LNP
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on    AND
121600030721     C                             PiVal<=999   AND
121700030721     C                             PiVal>*zeros
121800030721     C                   Z-ADD     PiVal         VABLNP
121900030721     C                   ELSE
122000040715     C                   SETON                                        32
122100030721     C                   EVAL      VABLNP = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040510     C                             + ' ' + 'VABLNP'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** NRS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on AND
123100030721     C                             PiVal<=99
123200030721     C                   Z-ADD     PiVal         VABNRS
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABNRS'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** LNA
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on    AND
124400030721     C                             PiVal<=999   AND
124500030721     C                             PiVal>*zeros
124600030721     C                   Z-ADD     PiVal         VABLNA
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      VABLNA = *zeros
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABLNA'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** CTR
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on  AND
125900030721     C                             PiVal<=999
126000030721     C                   Z-ADD     PiVal         VABCTR
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABCTR'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** IAS
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200030721     C                   Z-ADD     PiVal         VABIAS
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABIAS'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** VLB
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiNum=*on
128400030721     C                   Z-ADD     PiVal         VABVLB
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABVLB'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** QFT
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiNum=*on
129600030721     C                   Z-ADD     PiVal         VABQFT
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABQFT'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** NCD
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on      AND
130800030721     C                             PiVal<=9999999
130900030721     C                   Z-ADD     PiVal         VABNCD
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABNCD'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** NCA
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on      AND
132100030721     C                             PiVal<=9999999
132200030721     C                   Z-ADD     PiVal         VABNCA
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABNCA'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** ZNC
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on AND
133400030721     C                             PiVal<=99
133500030721     C                   Z-ADD     PiVal         VABZNC
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABZNC'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** DCR
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on       AND
134700030721     C                             PiVal<=99999999
134800030721     C                   Z-ADD     PiVal         VABDCR
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABDCR'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** HCR
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on   AND
136000030721     C                             PiVal<=9999
136100030721     C                   Z-ADD     PiVal         VABHCR
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABHCR'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** VMD
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiNum=*on
137300030721     C                   Z-ADD     PiVal         VABVMD
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABVMD'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** ANT
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on        AND
138500030721     C                             PiVal<=999999999
138600030721     C                   Z-ADD     PiVal         VABANT
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABANT'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** CMO
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on
139800030721     C                   Z-ADD     PiVal         Num5_0
139900030721     C                   MOVEL(p)  Num5_0        VABCMO
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      VABCMO = *zeros
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABCMO'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700040510     C*
140800110930     C* Reperisco i campi del EDIVAT
140900040510     C*** VATNOT
141000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141200040510     C                   ENDIF
141300040510     C*** VATTRC
141400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141600040510     C                   ENDIF
141700060331     C*** VATNOT_A
141800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142000060331     C                   ENDIF
142100060331     C*** VATNOT_B
142200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142400060331     C                   ENDIF
142500060331     C*** VATNOT_E
142600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142800060331     C                   ENDIF
142900110927     C*** VATNOT_I
143000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143200110927     C                   ENDIF
143300070823     C*
143400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143500110421     C*
143600140808     C*** NSP / RMN / RMA
143700110912     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
143800140808     C                   EVAL      VABRMA=SkSplitCSV(i)
143900140808     C                   EVAL      PiStr=SkSplitCSV(i)
144000140808     C                   EXSR      CHKNUM
144100140808     C                   IF        PiNum=*on
144200140808     C                   Z-ADD     PiVal         VABNSP
144300140808     C                   Z-ADD     PiVal         VABRMN
144400140808     C                   ELSE
144500140808     C                   SETON                                        31
144600140808     C                   EVAL      VABRMN = 1
144700140808     C                   EVAL      vinmsg = %trimr(vinmsg)
144800140808     C                             + ' ' + 'VABRMN VABRMA'
144900140808     C                   ENDIF
145000110415     C                   ENDIF
145100070925     C*
145200071029     C***  <===  -----------------------------------------
145300040510     C*
145400030715     C                   ENDIF
145500030715     C                   EVAL      i = i + 1
145600030715     C                   ENDDO
145700070925     C*
145800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145900100831     C*
146000100831     C                   IF        wVABRSD <> *blanks
146100100831     C                   EVAL      VABRSD = wVABRSD
146200100831     C                   ENDIF
146300100831     C*
146400100831     C                   IF        wVABIND <> *blanks
146500100831     C                   EVAL      VABIND = wVABIND
146600100831     C                   ENDIF
146700100831     C*
146800100831     C                   IF        wVABCAD <> *blanks
146900100831     C                   EVAL      VABCAD = wVABCAD
147000100831     C                   ENDIF
147100100831     C*
147200100831     C                   IF        wVABLOD <> *blanks
147300100831     C                   EVAL      VABLOD = wVABLOD
147400100831     C                   ENDIF
147500100831     C*
147600100831     C                   IF        wVABPRD <> *blanks
147700100831     C                   EVAL      VABPRD = wVABPRD
147800100831     C                   ENDIF
147900070925     C*
148000071029     C***  <===  -----------------------------------------
148100070925     C*
148200070925     C* Considerazioni finali su CBO/CAS
148300100318     C                   IF        wFlgCAS = '1'    OR
148400100318     C                             VABCAS  > *zeros
148500070925     C                   IF        VABCBO = '1'
148600070925     C                   EVAL      VABCBO = '4'
148700070925     C                   ENDIF
148800070925     C                   IF        VABCBO = '2'
148900070925     C                   EVAL      VABCBO = '6'
149000070925     C                   ENDIF
149100070925     C                   ENDIF
149200070925     C***  <===  ----------------------------
149300040510     C*
149400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149500040510     C                   IF        VABNSP = *zeros
149600040510     C                   SETON                                        31
149700040510     C                   ENDIF
149800040715     C*
149900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150000040715     C                   IF        VABLNP = *zeros
150100040715     C                   SETON                                        31
150200040715     C                   ENDIF
150300040510     C*
150400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150500040510     C                   IF        VABRMN = *zeros
150600040510     C                   Z-ADD     VABNSP        VABRMN
150700040510     C                   ENDIF
150800040510     C*
150900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151000040510     C                   IF        VABRMA = *blanks
151100040714     C                   MOVEL     VABRMN        VABRMA
151200040510     C                   ENDIF
151300040128     C*
151400040510     C* Se provincia nn valorizzata la reperisco
151500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151600110912     C                   IF        VABPRD =  *blanks AND
151700110912     C                             VABNZD =  *blanks AND
151800110912     C                             VABCAD <> *blanks AND
151900110912     C                             VABLOD <> *blanks
152000040128     C                   CLEAR                   TISI95DS
152100040128     C                   EVAL      I95TCN = '3'
152200040128     C                   Z-ADD     datcor        I95DAT
152300040128     C                   EVAL      I95NAR = VABNZD
152400040128     C                   EVAL      I95CAP = VABCAD
152500040128     C                   EVAL      I95LOC = VABLOD
152600040128     C                   CALL      'TISI95R'
152700040128     C                   PARM                    TISI95DS
152800040128     C                   EVAL      VABPRD = O95PRV
152900040510     C                   ENDIF
153000020204     C*
153100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153200020204     C                   EXSR      CHKIMPDIV
153300030715     C                   ENDIF
153400020204     C*
153500010607     C                   ENDSR
153600010607     C*----------------------------------------------------*
153700010601
153800020204
153900020204     C*----------------------------------------------------*
154000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154100020204     C*----------------------------------------------------*
154200020204     C     CHKIMPDIV     BEGSR
154300020204     C*
154400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154500020204     C                   Z-ADD     *zeros        wrkDec            9 9
154600020204     C*
154700020204     C* Come prima cosa effettuo considerazioni sulla divisa
154800020204     C                   IF        vabIAS > *zeros
154900020204     C                   IF        vabVAS <> 'EUR'
155000020204     C                   EVAL      vabVAS =  'ITL'
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C                   IF        vabCAS > *zeros
155500020204     C                   IF        vabVCA <> 'EUR'
155600020204     C                   EVAL      vabVCA =  'ITL'
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C                   IF        vabVMD > *zeros
156100020204     C                   IF        vabVAD <> 'EUR'
156200020204     C                   EVAL      vabVAD =  'ITL'
156300020204     C                   ENDIF
156400020204     C                   ENDIF
156500020204     C*
156600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156700020204     C                   Z-ADD     vabIAS        wrkDec
156800020204     C                   IF        wrkDec > *zeros
156900020204     C                   IF        vabVAS = 'ITL'
157000020204     C                   EVAL      vabIAS = *zeros
157100020204     C                   ENDIF
157200020204     C                   ENDIF
157300020204     C*
157400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157500020204     C                   Z-ADD     vabCAS        wrkDec
157600020204     C                   IF        wrkDec > *zeros
157700020204     C                   IF        vabVCA = 'ITL'
157800020204     C                   EVAL      vabCAS = *zeros
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158300020204     C                   Z-ADD     vabVMD        wrkDec
158400020204     C                   IF        wrkDec > *zeros
158500020204     C                   IF        vabVAD = 'ITL'
158600020204     C                   EVAL      vabVMD = *zeros
158700020204     C                   ENDIF
158800020204     C                   ENDIF
158900020204     C*
159000020204     C                   ENDSR
159100020204     C***
159200010330
159300100318
159400100318
159500100318     C*----------------------------------------------------*
159600100318     C*  CONTROLLO NUMERICITA' CAMPI
159700100318     C*----------------------------------------------------*
159800100318     C     CHKNUM        BEGSR
159900100318     C*
160000100318     C                   IF        PiDecChr = *blanks
160100100318     C                   EVAL      PiDecChr = CharNUM
160200100318     C                   ENDIF
160300100318     C*
160400100318     C                   callp     UBISNUM_Check(PiStr
160500100318     C                                          :PiDecChr
160600100318     C                                          :PiVal
160700100318     C                                          :PiNum
160800100318     C                                          :PiInt)
160900100318     C*
161000100318     C                   ENDSR
161100100318     C***
161200010601
161300010601
161400010601
161500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161600010601     C     repfil        BEGSR
161700010601     C*
161800010601     C                   if        invfil = *zeros and
161900010601     C                             depfil > *zeros and
162000010629     C                             (vinflg = *blanks or
162100010629     C                              vinflg = *zeros)
162200010601     C
162300010601     C                   eval      invfil = depfil
162400010601     C                   endif
162500010601     C*
162600010601     C                   if        depfil <> invfil and
162700010601     C                             invfil > *zeros
162800010601     C                   eval      flgMulti = '1'
162900010601     C                   if        vinflg = *blanks
163000010601     C                   add       1             cntNonEl
163100010601     C                   endif
163200010601     C                   endif
163300010601     C*
163400010601     C                   if        vinflg = '2'
163500010601     C                   eval      flgStato = '2'
163600010601     C                   endif
163700010601     C*
163800010601     C                   ENDSR
163900010601     C***
164000010601
164100010601
164200010601
164300010330
164400040119      /TITLE Invio dei dati al punto operativo.
164500040119     C     invio         BEGSR
164600040119     C*
164700110930     C* 1° invio EDIVAT
164800040119     C                   reset                   dscmz
164900040119     C                   move      vlrpoi        cmzdst
165000110930     C                   eval      cmzfld = 'EDIVATWR'
165100040119     C                   eval      cmzmbd = vlrhdl
165200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165300040119     C***                if        prmfir = *blanks
165400110930     C                   eval      cmzfla = 'EDIVAT0F'
165500110930     C                   eval      cmzmba = 'EDIVAT0F'
165600040119     C***                else
165700040119     C***                eval      cmzfla = prmfir
165800040119     C***                eval      cmzmba = prmfir
165900040119     C***                endif
166000040119     C                   eval      cmznrr = *zeros
166100040119     C                   move      §ctrokvt      cmznrr
166200040119     C                   eval      cmzlba = vlrfl1
166300040119     C                   call(e)   'TIS711C'
166400040119     C                   parm                    dscmz
166500040119     C                   parm      *blanks       esito
166600040119     C                   if        %error
166700040119     C                             or cmzerr = '1'
166800040119     C                             or esito  = '1'
166900040119     C                   eval      wrkesito = '3'
167000040119     C                   else
167100040119     C*
167200110930     C* 2° invio EDIVAB
167300040119     C                   reset                   dscmz
167400040119     C                   move      vlrpoi        cmzdst
167500040119     C                   eval      cmzfld = vlrfou
167600040119     C                   eval      cmzmbd = vlrhdl
167700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167800040119     C***                if        prmfir = *blanks
167900110930     C                   eval      cmzfla = 'EDIVAB0F'
168000110930     C                   eval      cmzmba = 'EDIVAB0F'
168100040119     C***                else
168200040119     C***                eval      cmzfla = prmfir
168300040119     C***                eval      cmzmba = prmfir
168400040119     C***                endif
168500040119     C                   eval      cmznrr = *zeros
168600040119     C                   move      §ctrokvb      cmznrr
168700040119     C                   eval      cmzlba = vlrfl1
168800040119     C                   call(e)   'TIS711C'
168900040119     C                   parm                    dscmz
169000040119     C                   parm      *blanks       esito
169100040119     C                   if        %error
169200040119     C                             or cmzerr = '1'
169300040119     C                             or esito  = '1'
169400040119     C                   eval      wrkesito = '3'
169500040119     C                   endif
169600040119     C                   endif
169700040119     C*
169800040119     C                   ENDSR
169900040119     C***
170000010601
170100010601
170200010601
170300010601
170400010601      /TITLE Invio dei dati al punto operativo.
170500010601     C     opeini        BEGSR
170600010601     C*
170700010601     C* Inizializzo flag e contatori operativi
170800010601     C                   movel     '0'           flgGiro           1
170900010601     C                   movel     '0'           flgMulti          1
171000010601     C                   movel     '1'           flgStato          1
171100010615     C                   movel     '0'           flgOk             1
171200010601     C                   z-add     *zeros        cntNonEl         10 0
171300010601     C                   z-add     *zeros        depfil            3 0
171400010601     C                   z-add     *zeros        invfil            3 0
171500010601     C*
171600010601     C                   ENDSR
171700010601     C***
171800070326
171900070326
172000070326
172100070326
172200070326     C     *pssr         BEGSR
172300070329     C*
172400070329     C                   if        %open(tivin00r)
172500070329     C                   close     tivin00r
172600070329     C                   endif
172700110930     C                   if        %open(edivabwr)
172800110930     C                   close     edivabwr
172900070329     C                   endif
173000110930     C                   if        %open(edivatwr)
173100110930     C                   close     edivatwr
173200070329     C                   endif
173300070326     C*
173400070326     C* Effettuo la chiamata al CLLE preposto
173500110930     C                   call(e)   'TITVEVTC'
173600070326     C                   parm                    parccm
173700070326     C                   parm                    parmbr
173800070326     C                   parm      '2'           paropz
173900070326     C*
174000070326     C                   eval      wrkesito = '2'
174100070404     C*
174200070404     C                   seton                                        LR
174300070326     C*
174400070326     C                   ENDSR     '*CANCL'
174500070326     C***
174600100318
174700100318
174800100318
174900100318     C     normCSV       BEGSR
175000100318     C*
175100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175200100318     C* separatori campi
175300100318     C                   if        CharTXT <> *blanks
175400100318     C                   z-add     *zeros        wLenVINDTA        4 0
175500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175600100318     C                   setoff                                       80
175700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176000100318     C                   if        not *in80
176100100318     C                   seton                                        80
176200100318     C                   else
176300100318     C                   setoff                                       80
176400100318     C                   endif
176500100318     C                   endif
176600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176700100318     C                             and *in80 = *on
176800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176900100318     C                   endif
177000100318     C                   enddo
177100100318     C                   endif
177200100318     C*
177300100318     C                   ENDSR
177400100318     C***
177500070326
177600100326
177700100326
177800100326     C*----------------------------------------------------*
177900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178000100326     C*----------------------------------------------------*
178100100326     C     REPNSP        BEGSR
178200100326     C*
178300100326     C* NSP => Stacco un numeratore da AZNUM
178400100326     C                   clear                   TRUL33DS
178500100326     C                   eval      I33OPE = *zeros
178600100326     C                   eval      I33CNU = 302
178700100326     C                   eval      I33NUM = 1
178800100326     C                   movel     TRUL33DS      KPJBU
178900100326     C                   call      'TRUL33R'
179000100326     C                   parm                    KPJBA
179100100326     C                   movel     KPJBU         TRUL33DS
179200100326     C                   if        O33ERR = *zeros
179300100326     C                   z-add     O33NRF        VABNSP
179400100326     C                   z-add     O33NRF        VATNSP
179500100326     C                   else
179600100326     C                   SETON                                        31
179700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179800100326     C                             + ' ' + 'VABNSP VATNSP'
179900100326     C                   endif
180000100326     C*
180100100326     C                   ENDSR
180200010330
180300100326
180400010330
180500000613     C     *inzsr        BEGSR
180600990910     C*
180700990910     C     *entry        plist
180800990920     C                   parm                    tivlrds
180900990921     C                   parm      wrkesito      esito
181000000724     C                   parm                    prmlit
181100000710     C                   parm                    prmfir
181200010330     C*
181300010330     C* CALCOLA LA DATA CORRENTE
181400110930     C                   time                    wn14             14 0
181500110930     C                   movel     wn14          oracor            6 0          *ORA
181600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181700100318     C                   eval      datcor = %dec(%date() : *ISO)
181800000613     C*
181900000613     C                   ENDSR
182000000613     C***
182100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182200140808W_VABRMN;VABRSD;VABIND;VABRD2;VABLOD;VABPRD;VABCAD;X;VABNCL;VABPKB;X;X;X;
182300140808X;X;X;X;X;VATNOT_I;X;VABNOT;VATNOT_B;VATNOT_E;X;
182400140808
182500110315
182600100318
