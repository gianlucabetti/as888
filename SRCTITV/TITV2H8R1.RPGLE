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
043100150701     C                   if        wVATNOT_S <> *blanks
043200130723     C***                clear                   DS_VATNOT_S
043300130723     C***                eval      NOT_S_CELL   = wVATNOT_S
043400130723     C***                eval      NOT_S_CODOPE = 'S'
043500130723     C***                eval      VATNOT = DS_VATNOT_S
043600150701     C                   eval      VATNOT = wVATNOT_S
043700150701     C* i 2 flag devono essere blank
043800150701     C                   EVAL      %subst(VATNOT:17:1)=*blank
043900150701     C                   EVAL      %subst(VATNOT:18:1)=*blank
044000150701     C                   eval      VATTRC = 'S'
044100150701     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044200150701     C                   eval      VATCNT = 1
044300150701     C                   write     EDIVAT00
044400150701     C                   add       1             §CTROKVT
044500150701     C                   endif
044600060331     C*
044700060331     C                   if        wVATNOT_E <> *blanks
044800110201     C                   eval      VATNOT = wVATNOT_E
044900060331     C                   eval      VATTRC = 'E'
045000110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
045100110930     C                   eval      VATCNT = 1
045200110930     C                   write     EDIVAT00
045300110201     C                   add       1             §CTROKVT
045400110201     C                   endif
045500110927     C*
045600110927     C                   if        wVATNOT_I <> *blanks
045700110927     C                   eval      VATNOT = wVATNOT_I
045800110927     C                   eval      VATTRC = 'I'
045900110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046000110930     C                   eval      VATCNT = 1
046100110930     C                   write     EDIVAT00
046200110927     C                   add       1             §CTROKVT
046300110927     C                   endif
046400040119     C*
046500040119     C                   ENDSR
046600040119
046700040119
046800040119
046900040119     C*----------------------------------------------------*
047000110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047100040119     C*----------------------------------------------------*
047200040119     C     PREVAT        BEGSR
047300040119     C*
047400110930     C* Compongo il nome del membro da dare al EDIVATWR
047500040119     C                   eval      parmbr = vlrhdl
047600040119     C                   movel     'M'           parmbr
047700040510     C                   eval      parccm = vlrksc
047800040119     C                   eval      paropz = '1'
047900040119     C* Effettuo la chiamata al CLLE preposto
048000110930     C                   call(e)   'TITVEVTC'
048100040119     C                   parm                    parccm
048200040119     C                   parm                    parmbr
048300040119     C                   parm                    paropz
048400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048500040119     C                   if        %error
048600040119     C                   movel     '1'           chkcall
048700040119     C                   else
048800040119     C                   movel     '0'           chkcall
048900040119     C                   endif
049000040119     C*
049100040119     C                   ENDSR
049200010601
049300010601
049400010601
049500010330     C*----------------------------------------------------*
049600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049700010330     C*----------------------------------------------------*
049800040119     C     INZVAR        BEGSR
049900080527     C*
050000080527     C                   setoff                                       65
050100010330     C*
050200030822     C* Inizializzo variabili di wrk
050300020204     C                   Z-ADD     *zeros        Num5_0            5 0
050400101018     C                   MOVEL     *blanks       wFlgCAS
050500060331     C                   MOVEL     *blanks       wVATNOT_A
050600060331     C                   MOVEL     *blanks       wVATNOT_B
050700060331     C                   MOVEL     *blanks       wVATNOT_E
050800110927     C                   MOVEL     *blanks       wVATNOT_I
050900130722     C                   MOVEL     *blanks       wVATNOT_S
051000100831     C                   MOVEL     *blanks       wVABRSD
051100100831     C                   MOVEL     *blanks       wVABIND
051200100831     C                   MOVEL     *blanks       wVABCAD
051300100831     C                   MOVEL     *blanks       wVABLOD
051400100831     C                   MOVEL     *blanks       wVABPRD
051500030822     C*
051600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051700030822     C                   CLEAR                   SkSplitCSV
051800030822     C                   EVAL      i = 1
051900030822     C                   EVAL      posDa = *zeros
052000030822     C                   EVAL      posA  = *zeros
052100080527     C*
052200110930     C                   CLEAR                   EDIVABS
052300110930     C                   CLEAR                   EDIVAT00
052400030822     C*
052500030822     C* Reimposto i valori di default
052600030822     C                   EXSR      DEFCAM
052700030822     C*
052800010330     C                   ENDSR
052900010330     C*----------------------------------------------------*
053000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053100010330     C*----------------------------------------------------*
053200010330     C     DEFCAM        BEGSR
053300030822     C*
053400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053500030715     C* e delimitatore testo.
053600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053900100318     C*
054000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054100100318     C                   EVAL      CharSOS = CharNUM
054200030822     C*
054300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054400100720     C                   SETOFF                                       505152
054500100720     C                   SETOFF                                       53
054600100413     C                   SELECT
054700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054800100720     C                   SETON                                        50
054900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055000100413     C                   SETON                                        51
055100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055200100720     C                   SETON                                        52
055300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055400100720     C                   SETON                                        53
055500100413     C                   ENDSL
055600030822     C*
055700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055800030822     C                   EVAL      posDaDft = 1
055900030822     C                   EVAL      posADft  = 0
056000030822     C                   EVAL      wGiroDft = 0
056100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056200030822     C                             posDaDft > 0
056300030822     C*
056400030822     C* Gestisco il 1° giro
056500030822     C                   IF        wGiroDft = 0
056600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056800030822     C* Incremento il contatore dei "giri"
056900030822     C                   EVAL      wGiroDft = 1
057000030822     C                   ELSE
057100030822     C                   EVAL      posDaDft = posADft
057200030822     C                   ENDIF
057300030822     C* Eseguo lo scan x trovare la fine del campo corrente
057400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
057500030822     C*
057600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057800030822     C                   IF        posDaDft > 0 AND
057900030822     C                             posADft  > 0
058000030822     C* NCL
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'NCL'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABNCL
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* CCM
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'CCM'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABCCM
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* LNP
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'LNP'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABLNP
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* NRS
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'NRS'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABNRS
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* CTR
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'CTR'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABCTR
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* PKB
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'PKB'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500030822     C                   Z-ADD     PiVal         VABPKB
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* VLB
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'VLB'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800030822     C                   Z-ADD     PiVal         VABVLB
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* QFT
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'QFT'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiNum=*on
068100030822     C                   Z-ADD     PiVal         VABQFT
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* CBO
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'CBO'
068900030822     C                   EVAL      VABCBO=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* TSP
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'TSP'
069800030822     C                   EVAL      VABTSP=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* VAS
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'VAS'
070700030822     C                   EVAL      VABVAS=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* VCA
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'VCA'
071600030822     C                   EVAL      VABVCA=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000070925     C* TIC
072100070925     C                   IF        %subst(
072200070925     C                             %subst(vlrppt:posDaDft+1:
072300070925     C                             posADft-posDaDft-1):1:3)
072400070925     C                             = 'TIC'
072500070925     C                   EVAL      VABTIC=%trim(%subst(
072600070925     C                             %subst(vlrppt:posDaDft+1:
072700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800070925     C                   ENDIF
072900030822     C* GCA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GCA'
073400030822     C                   EVAL      VABGCA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* CTM
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'CTM'
074300030822     C                   EVAL      VABCTM=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* FFD
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'FFD'
075200030822     C                   EVAL      VABFFD=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* VAD
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'VAD'
076100030822     C                   EVAL      VABVAD=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* GMA
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'GMA'
077000030822     C                   EVAL      VABGMA=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* GGA
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'GGA'
077900030822     C                   EVAL      VABGGA=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GVA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GVA'
078800030822     C                   EVAL      VABGVA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* TC1
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'TC1'
079700030822     C                   EVAL      VABTC1=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* TC2
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'TC2'
080600030822     C                   EVAL      VABTC2=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000040714     C* VATTRC
081100040714     C                   IF        %subst(
081200040714     C                             %subst(vlrppt:posDaDft+1:
081300040714     C                             posADft-posDaDft-1):1:3)
081400040714     C                             = 'TRC'
081500040714     C                   EVAL      VATTRC=%trim(%subst(
081600040714     C                             %subst(vlrppt:posDaDft+1:
081700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800040714     C                   ENDIF
081900030822     C* ...
082000030822     C                   ENDIF
082100030822     C                   ENDDO
082200020204     C*
082300010330     C                   ENDSR
082400010607     C*----------------------------------------------------*
082500110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
082600010607     C*----------------------------------------------------*
082700010607     C     IMPVAB        BEGSR
082800010607     C*
082900010607     C                   SETOFF                                       3132
083000110315 xxx C***                SETON                                        35
083100030515     C*
083200010607     C                   MOVEL     datcor        VABAAS
083300010607     C                   MOVE      datcor        VABMGS
083400010607     C*
083500030715     C**********
083600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083700030715     C**********
083800030715     C                   IF        wGiro = *zeros
083900040113     C*
084000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084300080429     C                             %trim(intcol(5))<>*blanks
084400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
084500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084600080429     C                                      %trim(intcol(5))
084700070823     C     ';':CharCSV   XLATE     vindta        vindta
084800040510     C                   ENDIF
084900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085000031201     C     minu:maiu     XLATE     vindta        vindta
085100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085200030715     C                   DOW       posDa <= %len(%trim(vindta))
085300030715     C*
085400030715     C* Gestisco il 1° campo
085500030715     C                   IF        i = 1
085600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085700030715     C                   EVAL      posDa = 1
085800030715     C* Eseguo lo scan x trovare la fine del primo campo
085900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C* X i campi successivi al 1°
086400030715     C                   ELSE
086500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086600030715     C                   EVAL      posDa = posA + 1
086700030715     C* Eseguo lo scan x trovare la fine del campo corrente
086800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086900030715     C* Gestisco l'ultimo campo
087000030715     C                   IF        posA = *zeros
087100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087200030715     C* A questo punto "estraggo" il campo corrente
087300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087400030715     C                                                    (posA-posDa))
087500030715     C                   LEAVE
087600030715     C                   ELSE
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   ENDIF
088100030715     C                   ENDIF
088200030715     C* Incremento il contatore d campo
088300030715     C                   EVAL      i = i +1
088400030715     C                   ENDDO
088500030715     C*
088600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088700030715     C* delimitazione testo
088800030715     C                   EVAL      i = 1
088900030715     C                   DOW       i < %elem(SkSplitFLD)
089000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089200030715     C                   EVAL      i = i + 1
089300030715     C                   ENDDO
089400030715     C                   EVAL      wGiro = 1
089500030715     C                   ELSE
089600030715     C                   EVAL      wGiro = 2
089700031204     C**********
089800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089900031204     C* d separatore campo
090000031204     C**********
090100031204     C                   EVAL      vindta = %trim(vindta)
090200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090300031204     C                   EVAL      vindta = %subst(vindta:2)
090400031204     C                   ENDDO
090500031204     C*
090600031204     C                   Z-ADD     *zeros        lunghInput        4 0
090700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091100110912     C                   ENDDO
091200030715     C**********
091300030715     C* Effettuo lo split del campo dati d input
091400030715     C**********
091500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091600030715     C                   DOW       posDa <= %len(%trim(vindta))
091700030715     C*
091800030715     C* Gestisco il 1° campo
091900030715     C                   IF        i = 1
092000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092100030715     C                   EVAL      posDa = 1
092200030715     C* Eseguo lo scan x trovare la fine del primo campo
092300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092400030715     C* A questo punto "estraggo" il campo corrente
092500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092600030715     C                                                    (posA-posDa))
092700030715     C* X i campi successivi al 1°
092800030715     C                   ELSE
092900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093000030715     C                   EVAL      posDa = posA + 1
093100031204     C* Verifico che nn vi sia il campo nn valorizzato
093200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093300031204     C* Se campo nn valorizzato skippo al prossimo
093400031204     C                   EVAL      SkSplitCSV(i) = *blanks
093500031204     C                   EVAL      posA  = posA + 1
093600031204     C                   ELSE
093700030715     C* Eseguo lo scan x trovare la fine del campo corrente
093800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093900030715     C* Gestisco l'ultimo campo
094000030715     C                   IF        posA = *zeros
094100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094200030715     C* A questo punto "estraggo" il campo corrente
094300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094400030715     C                                                    (posA-posDa))
094500030715     C                   LEAVE
094600030715     C                   ELSE
094700030715     C* A questo punto "estraggo" il campo corrente
094800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094900030715     C                                                    (posA-posDa))
095000030715     C                   ENDIF
095100031204     C                   ENDIF
095200030715     C                   ENDIF
095300030715     C* Incremento il contatore d campo
095400030715     C                   EVAL      i = i +1
095500030715     C                   ENDDO
095600030715     C*
095700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095800030715     C* delimitazione testo
095900030715     C                   EVAL      i = 1
096000030715     C                   DOW       i < %elem(SkSplitCSV)
096100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096300030715     C                   EVAL      i = i + 1
096400030715     C                   ENDDO
096500100720     C*
096600100720     C* Stacco un progressivo x numero spedizione
096700120411 xxx C***                EXSR      repNSP
096800100326     C*
096900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097000030715     C                   EVAL      i = 1
097100030715     C                   DOW       i < %elem(SkSplitFLD)
097200030715     C*
097300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
097500030715     C                             SkSplitCSV(i) <> *blanks
097600030721     C*** ATB
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097800030721     C                   EVAL      VABATB = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030715     C*** RSD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
098500030715     C* ==
098600030715     C                   ENDIF
098700030721     C*** RD2
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030715     C*** IND
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099300030715     C                   EVAL      VABIND = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** LOD
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** PRD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030721     C*** NZD
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
100500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** RMA
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** NOT
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** NT2
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** VCA
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** VAS
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
102500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** FFD
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030721     C*** GC1
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** GC2
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TSP
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104200030721     C                   ENDIF
104300040510     C*** CBO
104400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
104500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104600040510     C                   ENDIF
104700030721     C*** NAS
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** TIC
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GCA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** XCO
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** CTM
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
106500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TCR
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** CTS
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** FTM
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** VAD
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** GMA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
108500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** GGA
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GVA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** TC1
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** TC2
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** SCL
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
110500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** RMO
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** NMO
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*
111600030721     C* Reperisco quindi i campi numerici...
111700040510     C*** NSP
111800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111900040510     C                   EVAL      PiStr=SkSplitCSV(i)
112000040510     C                   EXSR      CHKNUM
112100040510     C                   IF        PiInt=*on
112200040510     C                   Z-ADD     PiVal         VABNSP
112300040510     C                   ELSE
112400040510     C                   SETON                                        31
112500040510     C                   EVAL      VABNSP = *zeros
112600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040510     C                             + ' ' + 'VABNSP'
112800040510     C                   ENDIF
112900040510     C                   ENDIF
113000071106     C*** RMN
113100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113200071106     C                   EVAL      PiStr=SkSplitCSV(i)
113300071106     C                   EXSR      CHKNUM
113400071106     C                   IF        PiInt=*on
113500071106     C                   Z-ADD     PiVal         VABRMN
113600071106     C                   ELSE
113700071106     C                   SETON                                        32
113800071106     C                   EVAL      VABRMN = *zeros
113900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114000071106     C                             + ' ' + 'VABRMN'
114100071106     C                   ENDIF
114200071106     C                   ENDIF
114300030721     C*** CAD
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on
114800030721     C                   Z-ADD     PiVal         Num5_0
114900030721     C                   MOVEL(p)  Num5_0        VABCAD
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABCAD = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABCAD'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** NCL
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiInt=*on
116200030721     C                   Z-ADD     PiVal         VABNCL
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABNCL = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700040122     C                             + ' ' + 'VABNCL'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** PKB
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiNum=*on
117500030721     C                   Z-ADD     PiVal         VABPKB
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABPKB = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABPKB'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CAS
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118600110506     C                             %trim(SkSplitCSV(i)) <> *zeros
118700101018     C                   EVAL      wFlgCAS = '1'
118800101018     C                   ENDIF
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiNum=*on
119200030721     C                   Z-ADD     PiVal         VABCAS
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABCAS = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABCAS'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** CCM
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on      AND
120500030721     C                             PiVal<=9999999 AND
120600030721     C                             PiVal>*zeros
120700030721     C                   Z-ADD     PiVal         VABCCM
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABCCM = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200040510     C                             + ' ' + 'VABCCM'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNP
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNP
122300030721     C                   ELSE
122400040715     C                   SETON                                        32
122500030721     C                   EVAL      VABLNP = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700040510     C                             + ' ' + 'VABLNP'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** NRS
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on AND
123500030721     C                             PiVal<=99
123600030721     C                   Z-ADD     PiVal         VABNRS
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABNRS'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** LNA
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on    AND
124800030721     C                             PiVal<=999   AND
124900030721     C                             PiVal>*zeros
125000030721     C                   Z-ADD     PiVal         VABLNA
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      VABLNA = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABLNA'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** CTR
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on  AND
126300030721     C                             PiVal<=999
126400030721     C                   Z-ADD     PiVal         VABCTR
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABCTR'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** IAS
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiNum=*on
127600030721     C                   Z-ADD     PiVal         VABIAS
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABIAS'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** VLB
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiNum=*on
128800030721     C                   Z-ADD     PiVal         VABVLB
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABVLB'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** QFT
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABQFT
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABQFT'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** NCD
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on      AND
131200030721     C                             PiVal<=9999999
131300030721     C                   Z-ADD     PiVal         VABNCD
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABNCD'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** NCA
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on      AND
132500030721     C                             PiVal<=9999999
132600030721     C                   Z-ADD     PiVal         VABNCA
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABNCA'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** ZNC
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on AND
133800030721     C                             PiVal<=99
133900030721     C                   Z-ADD     PiVal         VABZNC
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABZNC'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** DCR
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on       AND
135100030721     C                             PiVal<=99999999
135200030721     C                   Z-ADD     PiVal         VABDCR
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABDCR'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** HCR
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on   AND
136400030721     C                             PiVal<=9999
136500030721     C                   Z-ADD     PiVal         VABHCR
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABHCR'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** VMD
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiNum=*on
137700030721     C                   Z-ADD     PiVal         VABVMD
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABVMD'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** ANT
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on        AND
138900030721     C                             PiVal<=999999999
139000030721     C                   Z-ADD     PiVal         VABANT
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABANT'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** CMO
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiInt=*on
140200030721     C                   Z-ADD     PiVal         Num5_0
140300030721     C                   MOVEL(p)  Num5_0        VABCMO
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      VABCMO = *zeros
140700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140800030721     C                             + ' ' + 'VABCMO'
140900030721     C                   ENDIF
141000030721     C                   ENDIF
141100040510     C*
141200110930     C* Reperisco i campi del EDIVAT
141300040510     C*** VATNOT
141400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141600040510     C                   ENDIF
141700040510     C*** VATTRC
141800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142000040510     C                   ENDIF
142100060331     C*** VATNOT_A
142200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142400060331     C                   ENDIF
142500060331     C*** VATNOT_B
142600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142800060331     C                   ENDIF
142900060331     C*** VATNOT_E
143000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143200060331     C                   ENDIF
143300110927     C*** VATNOT_I
143400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143600110927     C                   ENDIF
143700070823     C*
143800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143900110421     C*
144000110930     C*** RMN / RMA
144100150128     C*** in data 28/01/2015 VABRMA ha cambiato modo di valorizzazione impostandosi con lo stesso
144200150128     C*** valore di VATNOT_E
144300110912     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
144400130218     C* se in questa colonna il primo carattere è % va omesso
144500130218     C                   IF        %subst(SkSplitCSV(i) : 1 : 1) = '%'
144600130218     C                   EVAL      %subst(SkSplitCSV(i) : 1 : 1) = *blank
144700130218     C                   EVAL      SkSplitCSV(i) = %triml(SkSplitCSV(i))
144800130218     C                   ENDIF
144900150128     C***                EVAL      VABRMA=SkSplitCSV(i)
145000120508     C                   IF        %len(%trim(SkSplitCSV(i))) = 12
145100120508     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5)
145200120508     C                   ELSE
145300120508     C                   EVAL      PiStr=SkSplitCSV(i)
145400120508     C                   ENDIF
145500120508     C*
145600110912     C                   EXSR      CHKNUM
145700110912     C                   IF        PiInt=*on
145800110912     C                   Z-ADD     PiVal         VABRMN
145900120411     C                   Z-ADD     PiVal         VABNSP
146000120411     C                   Z-ADD     PiVal         VATNSP
146100110912     C                   ELSE
146200110912     C                   SETON                                        31
146300110912     C                   EVAL      VABRMN = 1
146400110912     C                   EVAL      vinmsg = %trimr(vinmsg)
146500110930     C                             + ' ' + 'VABRMN'
146600110912     C                   ENDIF
146700110415     C                   ENDIF
146800130603     C*** PKB
146900130603     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
147000130603     C                   EVAL      PiStr='0'+SkSplitCSV(i)
147100130603     C                   EXSR      CHKNUM
147200130603     C                   IF        PiNum=*on
147300130603     C                   Z-ADD     PiVal         VABPKB
147400130610     C                   IF        VABPKB = *zeros
147500130610     C                   EVAL      VABPKB = 0,5
147600130610     C                   ENDIF
147700130603     C                   ELSE
147800130603     C                   SETON                                        32
147900130603     C                   EVAL      VABPKB = *zeros
148000130603     C                   EVAL      vinmsg = %trimr(vinmsg)
148100130603     C                             + ' ' + 'VABPKB'
148200130603     C                   ENDIF
148300130603     C                   ENDIF
148400130603     C*** CMO
148500120510     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCMO'
148600120510     C                   EVAL      VABCMO = %trim(SkSplitCSV(i))
148700120510     C                   ENDIF
148800130722     C*** VATNOT_A / VATNOT_S
148900130722     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_A'
149000150701     C***                EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
149100130722     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149200130722     C                   ENDIF
149300070925     C*
149400071029     C***  <===  -----------------------------------------
149500040510     C*
149600030715     C                   ENDIF
149700030715     C                   EVAL      i = i + 1
149800030715     C                   ENDDO
149900070925     C*
150000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150100100831     C*
150200150128     C*** in data 28/01/2015 VABRMA ha cambiato modo di valorizzazione impostandosi con lo stesso
150300150128     C*** valore di VATNOT_E
150400150128     C                   EVAL      VABRMA = wVATNOT_E
150500150128     C*
150600100831     C                   IF        wVABRSD <> *blanks
150700100831     C                   EVAL      VABRSD = wVABRSD
150800100831     C                   ENDIF
150900100831     C*
151000100831     C                   IF        wVABIND <> *blanks
151100100831     C                   EVAL      VABIND = wVABIND
151200100831     C                   ENDIF
151300100831     C*
151400100831     C                   IF        wVABCAD <> *blanks
151500100831     C                   EVAL      VABCAD = wVABCAD
151600100831     C                   ENDIF
151700100831     C*
151800100831     C                   IF        wVABLOD <> *blanks
151900100831     C                   EVAL      VABLOD = wVABLOD
152000100831     C                   ENDIF
152100100831     C*
152200100831     C                   IF        wVABPRD <> *blanks
152300100831     C                   EVAL      VABPRD = wVABPRD
152400100831     C                   ENDIF
152500070925     C*
152600071029     C***  <===  -----------------------------------------
152700070925     C*
152800070925     C* Considerazioni finali su CBO/CAS
152900100318     C                   IF        wFlgCAS = '1'    OR
153000100318     C                             VABCAS  > *zeros
153100070925     C                   IF        VABCBO = '1'
153200070925     C                   EVAL      VABCBO = '4'
153300070925     C                   ENDIF
153400070925     C                   IF        VABCBO = '2'
153500070925     C                   EVAL      VABCBO = '6'
153600070925     C                   ENDIF
153700070925     C                   ENDIF
153800070925     C***  <===  ----------------------------
153900040510     C*
154000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154100040510     C                   IF        VABNSP = *zeros
154200040510     C                   SETON                                        31
154300040510     C                   ENDIF
154400040715     C*
154500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154600040715     C                   IF        VABLNP = *zeros
154700040715     C                   SETON                                        31
154800040715     C                   ENDIF
154900040510     C*
155000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155100040510     C                   IF        VABRMN = *zeros
155200040510     C                   Z-ADD     VABNSP        VABRMN
155300040510     C                   ENDIF
155400040510     C*
155500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155600040510     C                   IF        VABRMA = *blanks
155700040714     C                   MOVEL     VABRMN        VABRMA
155800040510     C                   ENDIF
155900040128     C*
156000040510     C* Se provincia nn valorizzata la reperisco
156100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156200110912     C                   IF        VABPRD =  *blanks AND
156300110912     C                             VABNZD =  *blanks AND
156400110912     C                             VABCAD <> *blanks AND
156500110912     C                             VABLOD <> *blanks
156600040128     C                   CLEAR                   TISI95DS
156700040128     C                   EVAL      I95TCN = '3'
156800040128     C                   Z-ADD     datcor        I95DAT
156900040128     C                   EVAL      I95NAR = VABNZD
157000040128     C                   EVAL      I95CAP = VABCAD
157100040128     C                   EVAL      I95LOC = VABLOD
157200040128     C                   CALL      'TISI95R'
157300040128     C                   PARM                    TISI95DS
157400040128     C                   EVAL      VABPRD = O95PRV
157500040510     C                   ENDIF
157600020204     C*
157700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157800020204     C                   EXSR      CHKIMPDIV
157900030715     C                   ENDIF
158000020204     C*
158100010607     C                   ENDSR
158200010607     C*----------------------------------------------------*
158300010601
158400020204
158500020204     C*----------------------------------------------------*
158600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158700020204     C*----------------------------------------------------*
158800020204     C     CHKIMPDIV     BEGSR
158900020204     C*
159000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159100020204     C                   Z-ADD     *zeros        wrkDec            9 9
159200020204     C*
159300020204     C* Come prima cosa effettuo considerazioni sulla divisa
159400020204     C                   IF        vabIAS > *zeros
159500020204     C                   IF        vabVAS <> 'EUR'
159600020204     C                   EVAL      vabVAS =  'ITL'
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C                   IF        vabCAS > *zeros
160100020204     C                   IF        vabVCA <> 'EUR'
160200020204     C                   EVAL      vabVCA =  'ITL'
160300020204     C                   ENDIF
160400020204     C                   ENDIF
160500020204     C*
160600020204     C                   IF        vabVMD > *zeros
160700020204     C                   IF        vabVAD <> 'EUR'
160800020204     C                   EVAL      vabVAD =  'ITL'
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161300020204     C                   Z-ADD     vabIAS        wrkDec
161400020204     C                   IF        wrkDec > *zeros
161500020204     C                   IF        vabVAS = 'ITL'
161600020204     C                   EVAL      vabIAS = *zeros
161700020204     C                   ENDIF
161800020204     C                   ENDIF
161900020204     C*
162000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162100020204     C                   Z-ADD     vabCAS        wrkDec
162200020204     C                   IF        wrkDec > *zeros
162300020204     C                   IF        vabVCA = 'ITL'
162400020204     C                   EVAL      vabCAS = *zeros
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162900020204     C                   Z-ADD     vabVMD        wrkDec
163000020204     C                   IF        wrkDec > *zeros
163100020204     C                   IF        vabVAD = 'ITL'
163200020204     C                   EVAL      vabVMD = *zeros
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C                   ENDSR
163700020204     C***
163800010330
163900100318
164000100318
164100100318     C*----------------------------------------------------*
164200100318     C*  CONTROLLO NUMERICITA' CAMPI
164300100318     C*----------------------------------------------------*
164400100318     C     CHKNUM        BEGSR
164500100318     C*
164600100318     C                   IF        PiDecChr = *blanks
164700100318     C                   EVAL      PiDecChr = CharNUM
164800100318     C                   ENDIF
164900100318     C*
165000100318     C                   callp     UBISNUM_Check(PiStr
165100100318     C                                          :PiDecChr
165200100318     C                                          :PiVal
165300100318     C                                          :PiNum
165400100318     C                                          :PiInt)
165500100318     C*
165600100318     C                   ENDSR
165700100318     C***
165800010601
165900010601
166000010601
166100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166200010601     C     repfil        BEGSR
166300010601     C*
166400010601     C                   if        invfil = *zeros and
166500010601     C                             depfil > *zeros and
166600010629     C                             (vinflg = *blanks or
166700010629     C                              vinflg = *zeros)
166800010601     C
166900010601     C                   eval      invfil = depfil
167000010601     C                   endif
167100010601     C*
167200010601     C                   if        depfil <> invfil and
167300010601     C                             invfil > *zeros
167400010601     C                   eval      flgMulti = '1'
167500010601     C                   if        vinflg = *blanks
167600010601     C                   add       1             cntNonEl
167700010601     C                   endif
167800010601     C                   endif
167900010601     C*
168000010601     C                   if        vinflg = '2'
168100010601     C                   eval      flgStato = '2'
168200010601     C                   endif
168300010601     C*
168400010601     C                   ENDSR
168500010601     C***
168600010601
168700010601
168800010601
168900010330
169000040119      /TITLE Invio dei dati al punto operativo.
169100040119     C     invio         BEGSR
169200040119     C*
169300110930     C* 1° invio EDIVAT
169400040119     C                   reset                   dscmz
169500040119     C                   move      vlrpoi        cmzdst
169600110930     C                   eval      cmzfld = 'EDIVATWR'
169700040119     C                   eval      cmzmbd = vlrhdl
169800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169900040119     C***                if        prmfir = *blanks
170000110930     C                   eval      cmzfla = 'EDIVAT0F'
170100110930     C                   eval      cmzmba = 'EDIVAT0F'
170200040119     C***                else
170300040119     C***                eval      cmzfla = prmfir
170400040119     C***                eval      cmzmba = prmfir
170500040119     C***                endif
170600040119     C                   eval      cmznrr = *zeros
170700040119     C                   move      §ctrokvt      cmznrr
170800040119     C                   eval      cmzlba = vlrfl1
170900040119     C                   call(e)   'TIS711C'
171000040119     C                   parm                    dscmz
171100040119     C                   parm      *blanks       esito
171200040119     C                   if        %error
171300040119     C                             or cmzerr = '1'
171400040119     C                             or esito  = '1'
171500040119     C                   eval      wrkesito = '3'
171600040119     C                   else
171700040119     C*
171800110930     C* 2° invio EDIVAB
171900040119     C                   reset                   dscmz
172000040119     C                   move      vlrpoi        cmzdst
172100040119     C                   eval      cmzfld = vlrfou
172200040119     C                   eval      cmzmbd = vlrhdl
172300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172400040119     C***                if        prmfir = *blanks
172500110930     C                   eval      cmzfla = 'EDIVAB0F'
172600110930     C                   eval      cmzmba = 'EDIVAB0F'
172700040119     C***                else
172800040119     C***                eval      cmzfla = prmfir
172900040119     C***                eval      cmzmba = prmfir
173000040119     C***                endif
173100040119     C                   eval      cmznrr = *zeros
173200040119     C                   move      §ctrokvb      cmznrr
173300040119     C                   eval      cmzlba = vlrfl1
173400040119     C                   call(e)   'TIS711C'
173500040119     C                   parm                    dscmz
173600040119     C                   parm      *blanks       esito
173700040119     C                   if        %error
173800040119     C                             or cmzerr = '1'
173900040119     C                             or esito  = '1'
174000040119     C                   eval      wrkesito = '3'
174100040119     C                   endif
174200040119     C                   endif
174300040119     C*
174400040119     C                   ENDSR
174500040119     C***
174600010601
174700010601
174800010601
174900010601
175000010601      /TITLE Invio dei dati al punto operativo.
175100010601     C     opeini        BEGSR
175200010601     C*
175300010601     C* Inizializzo flag e contatori operativi
175400010601     C                   movel     '0'           flgGiro           1
175500010601     C                   movel     '0'           flgMulti          1
175600010601     C                   movel     '1'           flgStato          1
175700010615     C                   movel     '0'           flgOk             1
175800010601     C                   z-add     *zeros        cntNonEl         10 0
175900010601     C                   z-add     *zeros        depfil            3 0
176000010601     C                   z-add     *zeros        invfil            3 0
176100010601     C*
176200010601     C                   ENDSR
176300010601     C***
176400070326
176500070326
176600070326
176700070326
176800070326     C     *pssr         BEGSR
176900070329     C*
177000070329     C                   if        %open(tivin00r)
177100070329     C                   close     tivin00r
177200070329     C                   endif
177300110930     C                   if        %open(edivabwr)
177400110930     C                   close     edivabwr
177500070329     C                   endif
177600110930     C                   if        %open(edivatwr)
177700110930     C                   close     edivatwr
177800070329     C                   endif
177900070326     C*
178000070326     C* Effettuo la chiamata al CLLE preposto
178100110930     C                   call(e)   'TITVEVTC'
178200070326     C                   parm                    parccm
178300070326     C                   parm                    parmbr
178400070326     C                   parm      '2'           paropz
178500070326     C*
178600070326     C                   eval      wrkesito = '2'
178700070404     C*
178800070404     C                   seton                                        LR
178900070326     C*
179000070326     C                   ENDSR     '*CANCL'
179100070326     C***
179200100318
179300100318
179400100318
179500100318     C     normCSV       BEGSR
179600100318     C*
179700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179800100318     C* separatori campi
179900100318     C                   if        CharTXT <> *blanks
180000100318     C                   z-add     *zeros        wLenVINDTA        4 0
180100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180200100318     C                   setoff                                       80
180300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180600100318     C                   if        not *in80
180700100318     C                   seton                                        80
180800100318     C                   else
180900100318     C                   setoff                                       80
181000100318     C                   endif
181100100318     C                   endif
181200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181300100318     C                             and *in80 = *on
181400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181500100318     C                   endif
181600100318     C                   enddo
181700100318     C                   endif
181800100318     C*
181900100318     C                   ENDSR
182000100318     C***
182100070326
182200100326
182300100326
182400100326     C*----------------------------------------------------*
182500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182600100326     C*----------------------------------------------------*
182700100326     C     REPNSP        BEGSR
182800100326     C*
182900100326     C* NSP => Stacco un numeratore da AZNUM
183000100326     C                   clear                   TRUL33DS
183100100326     C                   eval      I33OPE = *zeros
183200100326     C                   eval      I33CNU = 302
183300100326     C                   eval      I33NUM = 1
183400100326     C                   movel     TRUL33DS      KPJBU
183500100326     C                   call      'TRUL33R'
183600100326     C                   parm                    KPJBA
183700100326     C                   movel     KPJBU         TRUL33DS
183800100326     C                   if        O33ERR = *zeros
183900100326     C                   z-add     O33NRF        VABNSP
184000100326     C                   z-add     O33NRF        VATNSP
184100100326     C                   else
184200100326     C                   SETON                                        31
184300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184400100326     C                             + ' ' + 'VABNSP VATNSP'
184500100326     C                   endif
184600100326     C*
184700100326     C                   ENDSR
184800010330
184900100326
185000010330
185100000613     C     *inzsr        BEGSR
185200990910     C*
185300990910     C     *entry        plist
185400990920     C                   parm                    tivlrds
185500990921     C                   parm      wrkesito      esito
185600000724     C                   parm                    prmlit
185700000710     C                   parm                    prmfir
185800010330     C*
185900010330     C* CALCOLA LA DATA CORRENTE
186000110930     C                   time                    wn14             14 0
186100110930     C                   movel     wn14          oracor            6 0          *ORA
186200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186300100318     C                   eval      datcor = %dec(%date() : *ISO)
186400000613     C*
186500000613     C                   ENDSR
186600000613     C***
186700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186800130603W_VABRMN;VABRSD;VABIND;VABRD2;VABLOD;X;VABCAD;X;VABNCL;W_VABPKB;X;X;X;
186900130722X;X;X;X;X;VATNOT_I;X;VATNOT_B;W_VATNOT_A;VATNOT_E;X;X;X;X;X;X;X;X;VABRMO;
187000130603X;X;X;X;X_W_VABCMO;X;
187100110315
187200100318
