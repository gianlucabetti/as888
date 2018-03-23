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
003400100326     D*------------------
003500100326     D* DS REPERIMENTO NUMERATORE
003600100326     D*------------------
003700100326     D trul33ds      e ds                  inz
003800100326     D*------------------
003900100326     D* DS ARCHITETTURA
004000100326     D*------------------
004100170609     D KPJBA         e ds                  inz
004200160427     D*------------------
004300100326
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700160503     D SkSplitFLD      S             20    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200100318     D CharSOS         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700070823     D wVATNOT_A       s                   LIKE(VATNOT)
005800060331     D wVATNOT_B       s                   LIKE(VATNOT)
005900060331     D wVATNOT_E       s                   LIKE(VATNOT)
006000110927     D wVATNOT_I       s                   LIKE(VATNOT)
006100111124     D wVATNOT_J       s                   LIKE(VATNOT)
006200120405     D wVATNOT_4       s                   LIKE(VATNOT)
006300131018     D wVATNOT_S       s                   LIKE(VATNOT)
006400141128     D wVATNOT_Y       s                   LIKE(VATNOT)
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200080418     D*------------------
007300080418     D* Costanti
007400080418     D*------------------
007500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300010330
008400010330
008500010330
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200010601     C*
009300010601     C                   seton                                        lr
009400010601
009500010601
009600010601
009700010601     C*--------------------------------------------------------
009800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010601     C*--------------------------------------------------------
010000010601     C     PREELA        BEGSR
010100010601     C*
010200010601     C* SE OCCORRE SPEDIRE IN FILIALE
010300010601     C                   if        invfil <> *zeros and
010400010601     C                             flgGiro = '0'
010500010601     C*
010600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010601     C                   eval      flgGiro = '1'
010800010601     C*
010900010601     C                   endif
011000010601     C*
011100010601     C                   ENDSR
011200010601     C***
011300010601
011400010601
011500010601
011600010601
011700010601     C*--------------------------------------------------------
011800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011900010601     C*--------------------------------------------------------
012000010601     C     ENDELA        BEGSR
012100040119     C*
012200040119     C* Effettuo la chiamata al CLLE preposto
012300110930     C                   call(e)   'TITVEVTC'
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
014000110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
014100010330     C*--------------------------------------------------------
014200010605     C     RWVAB         BEGSR
014300010330     C*
014400010330     C                   if        not %open(tivin00r)
014500010330     C                   open      tivin00r
014600010330     C                   endif
014700110930     C                   if        not %open(edivabwr)
014800110930     C                   open      edivabwr
014900010330     C                   endif
015000040119     C*
015100110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015200040119     C                   exsr      prevat
015300040119     C*
015400040119     C                   if        chkcall = '0'
015500040119     C*
015600110930     C                   if        not %open(edivatwr)
015700110930     C                   open      edivatwr
015800040119     C                   endif
015900010330     C*
016000040119     C                   clear                   §CTROKVB          7 0
016100040119     C                   clear                   §CTROKVT          7 0
016200040119     C                   clear                   §CTRMO            7 0
016300040119     C                   clear                   §CTRNO            7 0
016400070823     C*
016500070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016600070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016700160421     C                   exsr      inzvar
016800160421     C                   exsr      impvab
016900110131     C*
017000110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017100110912 xxx C***                read      tivin00r                               70
017200010330     C*
017300030822     C                   do        *HIVAL
017400010330     C*
017500030822     C                   read      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100040510     C*
018200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018300040510     C                   z-add     *zeros        wLenVINDTA        4 0
018400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018800040510     C                   z-add     1             wFlgVINDTA
018900040510     C                   leave
019000040510     C                   endif
019100040510     C                   enddo
019200010618     C*
019300040510     C                   if        vindta > *blanks AND
019400040510     C                             wFlgVINDTA = 1
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010330     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010601     C*
020700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020800070925     C                   eval      vindta  = ' ' + %trim(vindta)
020900070925     C*
021000080527     C                   exsr      inzvar
021100100318     C                   exsr      normCSV
021200010605     C                   exsr      impvab
021300010601     C*
021400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021500010605     C                   eval      depfil = VABLNP
021600010601     C                   exsr      repfil
021700010601     C                   if        depfil = invfil
021800021025     C                   if        vlrpoi = 999
021900030822     C                   move(P)   invfil        VABFGS
022000021025     C                   else
022100030822     C                   move(P)   vlrpoi        VABFGS
022200021025     C                   endif
022300010601     C*
022400010601     C                   exsr      PREELA
022500010601     C*
022600010604     C* Ebbene...
022700010604     C*
022800040119     C  N31              add       1             §CTROKVB
022900040119     C   32              add       1             §CTRMO
023000040119     C   31              add       1             §CTRNO
023100080418     C*
023200030822     C                   if        wGiro = 2
023300080513     C* Scrivo sempre il VAT
023400080513     C  N31              exsr      wrivat
023500110201     C                   exsr      chkvab
023600080513     C                   endif
023700010604     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000010604     C                   eval      vinflg = '1'
024100010604     C                   else
024200010604     C                   eval      vinflg = '2'
024300010604     C                   endif
024400010604     C                   endif
024500010601     C*
024600010604     C                   endif
024700010604     C*
024800010330     C                   else
024900010330     C                   eval      vinflg = '1'
025000010330     C                   endif
025100010601     C*
025200010601     C  N70              update    tivin000
025300010330     C*
025400030822     C  N70              enddo
025500040119     C*
025600080527     C* Verifico dati rimasti "in cannna"...
025700080527     C                   seton                                        65
025800110201     C                   exsr      chkvab
025900080527     C*
026000040119     C                   endif                                                  (endif - chkcall)
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200040119     C                   if        §ctrokvb > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900110930     C                   if        %open(edivabwr)
029000110930     C                   close     edivabwr
029100010330     C                   endif
029200110930     C                   if        %open(edivatwr)
029300110930     C                   close     edivatwr
029400040512     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000040119     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010601     C*
030500010618     C                   if        flgGiro = '1'
030600010601     C                   exsr      endela
030700010618     C                   endif
030800010330     C*
030900010330     C                   ENDSR
031000010330     C***
031100080527
031200080527
031300080527
031400080527     C*----------------------------------------------------*
031500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031600080527     C*----------------------------------------------------*
031700100413     C     CHKVAB        BEGSR
031800080527     C*
031900080527     C* Scrivo il VAB solo a rottura d spedizione
032000080527     C                   if        depspe = *zeros
032100110930     C  N31              eval      edivabs_sav = edivabs
032200080527     C                   eval      depspe = vabNSP
032300080527     C                   else
032400080527     C                   if        depspe = vabNSP AND *in65 = *off
032500100413     C   51              add       VABNCL        sav_VABNCL
032600100413     C   51              add       VABPKB        sav_VABPKB
032700100413     C   51              add       VABVLB        sav_VABVLB
032800100720     C   52              z-add     VABNCL        sav_VABNCL
032900100720     C   52              z-add     VABPKB        sav_VABPKB
033000100720     C   52              z-add     VABVLB        sav_VABVLB
033100100720     C   53              add       VABNCL        sav_VABNCL
033200100720     C   53              z-add     VABPKB        sav_VABPKB
033300100720     C   53              z-add     VABVLB        sav_VABVLB
033400080527     C                   else
033500110930     C  N31              eval      edivabs_ok = edivabs_sav
033600080527     C  N31              exsr      wrivab
033700110930     C  N31              eval      edivabs_sav = edivabs
033800080527     C                   eval      depspe = vabNSP
033900080527     C                   endif
034000080527     C                   endif
034100080527     C*
034200080527     C                   ENDSR
034300040119
034400040119
034500040119
034600040119     C*----------------------------------------------------*
034700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034800040119     C*----------------------------------------------------*
034900040510     C     WRIVAB        BEGSR
035000080513     C*
035100080513     C* Gestisco l'eventuale rottura x numero spedizione
035200170609     C                   if        VABNSP <> depspe or *in65 = *on
035300070301     C                   if        wVATNOT_E <> *blanks
035400111124     C                   eval      ok_VABCTM = '7Q'
035500070301     C                   endif
035600110930     C*
035700110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
035800111124     C                   EVAL      ok_VABCMR = %char(datcor)+%char(oracor)
035900111124     C                   EVAL      ok_VABDCM = DATCOR
036000111124     C                   EVAL      ok_VABDTS = DATCOR
036100111124     C                   EVAL      ok_VABHMS = ORACOR
036200111124     C                   EVAL      ok_VABCNT = 1
036300110930     C*
036400110930     C                   write     edivab00
036500080513     C                   endif
036600040119     C*
036700040119     C                   ENDSR
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037300040119     C*----------------------------------------------------*
037400040510     C     WRIVAT        BEGSR
037500060512     C*
037600060512     C* Inizializzo i campi chiave
037700060512     C                   eval      VATFGS = VABFGS
037800060512     C                   eval      VATAAS = VABAAS
037900060512     C                   eval      VATCCM = VABCCM
038000070823     C                   eval      VATNRS = VABNRS
038100060512     C                   eval      VATNSP = VABNSP
038200060512     C                   eval      VATLNP = VABLNP
038300060331     C*
038400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038500170609     C*
038600170609     C*** W_VATNOT_E
038700170609     C                   IF        VABCTM = '7Q'
038800170609     C                   Z-ADD     1             wNumCollo         5 0
038900170609     C                   DOW       wNumCollo <= VABNCL
039000170609     C                   EVAL      VATNOT = %editc(vabCCM:'X') +
039100170609     C                                      %trim(vabRMA)      +
039200170609     C                                      %trim(%editc(wNumCollo:'4'))
039300170609     C*
039400170609     C                   eval      VATTRC = 'E'
039500170609     C                   eval      VATCMR = %char(datcor)+%char(oracor)
039600170609     C                   eval      VATCNT = 1
039700170609     C                   write     EDIVAT00
039800170609     C                   add       1             §CTROKVT
039900170609     C*
040000170609     C                   ADD       1             wNumCollo
040100170609     C                   ENDDO
040200170609     C                   ENDIF
040300170609     C*
040400060331     C                   if        wVATNOT_A <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_A
040600060331     C                   eval      VATTRC = 'A'
040700110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
040800110930     C                   eval      VATCNT = 1
040900110930     C                   write     EDIVAT00
041000060331     C                   add       1             §CTROKVT
041100060331     C                   endif
041200060331     C*
041300060331     C                   if        wVATNOT_B <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_B
041500060331     C                   eval      VATTRC = 'B'
041600110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041700110930     C                   eval      VATCNT = 1
041800110930     C                   write     EDIVAT00
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200060331     C                   if        wVATNOT_E <> *blanks
042300110201     C                   eval      VATNOT = wVATNOT_E
042400060331     C                   eval      VATTRC = 'E'
042500110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
042600110930     C                   eval      VATCNT = 1
042700110930     C                   write     EDIVAT00
042800110201     C                   add       1             §CTROKVT
042900110201     C                   endif
043000110927     C*
043100110927     C                   if        wVATNOT_I <> *blanks
043200110927     C                   eval      VATNOT = wVATNOT_I
043300110927     C                   eval      VATTRC = 'I'
043400110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
043500110930     C                   eval      VATCNT = 1
043600110930     C                   write     EDIVAT00
043700110927     C                   add       1             §CTROKVT
043800110927     C                   endif
043900111124     C*
044000111124     C                   if        wVATNOT_J <> *blanks
044100111124     C                   eval      VATNOT = wVATNOT_J
044200111124     C                   eval      VATTRC = 'J'
044300111124     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044400111124     C                   eval      VATCNT = 1
044500111124     C                   write     EDIVAT00
044600111124     C                   add       1             §CTROKVT
044700111124     C                   endif
044800120405     C*
044900120405     C                   if        wVATNOT_4 <> *blanks
045000120405     C                   eval      VATNOT = wVATNOT_4
045100120405     C                   eval      VATTRC = '4'
045200120405     C                   eval      VATCMR = %char(datcor)+%char(oracor)
045300120405     C                   eval      VATCNT = 1
045400120405     C                   write     EDIVAT00
045500120405     C                   add       1             §CTROKVT
045600120405     C                   endif
045700131018     C*
045800131018     C                   if        wVATNOT_S <> *blanks
045900131018     C                   eval      VATNOT = wVATNOT_S
046000131018     C                   eval      VATTRC = 'S'
046100131018     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046200131018     C                   eval      VATCNT = 1
046300131018     C                   write     EDIVAT00
046400131018     C                   add       1             §CTROKVT
046500131018     C                   endif
046600141128     C*
046700141128     C                   if        wVATNOT_Y <> *blanks
046800141128     C                   eval      VATNOT = wVATNOT_Y
046900141128     C                   eval      VATTRC = 'Y'
047000141128     C                   eval      VATCMR = %char(datcor)+%char(oracor)
047100141128     C                   eval      VATCNT = 1
047200141128     C                   write     EDIVAT00
047300141128     C                   add       1             §CTROKVT
047400141128     C                   endif
047500040119     C*
047600040119     C                   ENDSR
047700040119
047800040119
047900040119
048000040119     C*----------------------------------------------------*
048100110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048200040119     C*----------------------------------------------------*
048300040119     C     PREVAT        BEGSR
048400040119     C*
048500110930     C* Compongo il nome del membro da dare al EDIVATWR
048600040119     C                   eval      parmbr = vlrhdl
048700040119     C                   movel     'M'           parmbr
048800040510     C                   eval      parccm = vlrksc
048900040119     C                   eval      paropz = '1'
049000040119     C* Effettuo la chiamata al CLLE preposto
049100110930     C                   call(e)   'TITVEVTC'
049200040119     C                   parm                    parccm
049300040119     C                   parm                    parmbr
049400040119     C                   parm                    paropz
049500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049600040119     C                   if        %error
049700040119     C                   movel     '1'           chkcall
049800040119     C                   else
049900040119     C                   movel     '0'           chkcall
050000040119     C                   endif
050100040119     C*
050200040119     C                   ENDSR
050300010601
050400010601
050500010601
050600010330     C*----------------------------------------------------*
050700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050800010330     C*----------------------------------------------------*
050900040119     C     INZVAR        BEGSR
051000080527     C*
051100080527     C                   setoff                                       65
051200010330     C*
051300030822     C* Inizializzo variabili di wrk
051400020204     C                   Z-ADD     *zeros        Num5_0            5 0
051500101018     C                   MOVEL     *blanks       wFlgCAS
051600060331     C                   MOVEL     *blanks       wVATNOT_A
051700060331     C                   MOVEL     *blanks       wVATNOT_B
051800060331     C                   MOVEL     *blanks       wVATNOT_E
051900110927     C                   MOVEL     *blanks       wVATNOT_I
052000111124     C                   MOVEL     *blanks       wVATNOT_J
052100120405     C                   MOVEL     *blanks       wVATNOT_4
052200131018     C                   MOVEL     *blanks       wVATNOT_S
052300141128     C                   MOVEL     *blanks       wVATNOT_Y
052400030822     C*
052500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052600030822     C                   CLEAR                   SkSplitCSV
052700030822     C                   EVAL      i = 1
052800030822     C                   EVAL      posDa = *zeros
052900030822     C                   EVAL      posA  = *zeros
053000080527     C*
053100110930     C                   CLEAR                   EDIVABS
053200110930     C                   CLEAR                   EDIVAT00
053300030822     C*
053400030822     C* Reimposto i valori di default
053500030822     C                   EXSR      DEFCAM
053600030822     C*
053700010330     C                   ENDSR
053800010330     C*----------------------------------------------------*
053900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054000010330     C*----------------------------------------------------*
054100010330     C     DEFCAM        BEGSR
054200030822     C*
054300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054400030715     C* e delimitatore testo.
054500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054800100318     C*
054900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055000100318     C                   EVAL      CharSOS = CharNUM
055100030822     C*
055200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055300100720     C                   SETOFF                                       505152
055400100720     C                   SETOFF                                       53
055500100413     C                   SELECT
055600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055700100720     C                   SETON                                        50
055800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055900100413     C                   SETON                                        51
056000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056100100720     C                   SETON                                        52
056200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056300100720     C                   SETON                                        53
056400100413     C                   ENDSL
056500030822     C*
056600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056700030822     C                   EVAL      posDaDft = 1
056800030822     C                   EVAL      posADft  = 0
056900030822     C                   EVAL      wGiroDft = 0
057000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057100030822     C                             posDaDft > 0
057200030822     C*
057300030822     C* Gestisco il 1° giro
057400030822     C                   IF        wGiroDft = 0
057500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057700030822     C* Incremento il contatore dei "giri"
057800030822     C                   EVAL      wGiroDft = 1
057900030822     C                   ELSE
058000030822     C                   EVAL      posDaDft = posADft
058100030822     C                   ENDIF
058200030822     C* Eseguo lo scan x trovare la fine del campo corrente
058300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058400030822     C*
058500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058700030822     C                   IF        posDaDft > 0 AND
058800030822     C                             posADft  > 0
058900030822     C* NCL
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'NCL'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABNCL
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* CCM
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'CCM'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABCCM
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* LNP
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'LNP'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABLNP
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* NRS
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'NRS'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiInt=*on
063800030822     C                   Z-ADD     PiVal         VABNRS
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* CTR
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'CTR'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiInt=*on
065100030822     C                   Z-ADD     PiVal         VABCTR
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* PKB
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'PKB'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABPKB
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* VLB
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VLB'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700030822     C                   Z-ADD     PiVal         VABVLB
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* QFT
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'QFT'
068500030822     C                   EVAL      PiStr=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   EXSR      CHKNUM
068900030822     C                   IF        PiNum=*on
069000030822     C                   Z-ADD     PiVal         VABQFT
069100030822     C                   ENDIF
069200030822     C                   ENDIF
069300030822     C* CBO
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'CBO'
069800030822     C                   EVAL      VABCBO=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* TSP
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'TSP'
070700030822     C                   EVAL      VABTSP=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* VAS
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'VAS'
071600030822     C                   EVAL      VABVAS=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VCA
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VCA'
072500030822     C                   EVAL      VABVCA=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900070925     C* TIC
073000070925     C                   IF        %subst(
073100070925     C                             %subst(vlrppt:posDaDft+1:
073200070925     C                             posADft-posDaDft-1):1:3)
073300070925     C                             = 'TIC'
073400070925     C                   EVAL      VABTIC=%trim(%subst(
073500070925     C                             %subst(vlrppt:posDaDft+1:
073600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700070925     C                   ENDIF
073800030822     C* GCA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GCA'
074300030822     C                   EVAL      VABGCA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* CTM
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'CTM'
075200030822     C                   EVAL      VABCTM=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* FFD
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'FFD'
076100030822     C                   EVAL      VABFFD=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* VAD
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'VAD'
077000030822     C                   EVAL      VABVAD=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* GMA
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'GMA'
077900030822     C                   EVAL      VABGMA=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GGA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GGA'
078800030822     C                   EVAL      VABGGA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* GVA
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'GVA'
079700030822     C                   EVAL      VABGVA=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* TC1
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'TC1'
080600030822     C                   EVAL      VABTC1=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* TC2
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'TC2'
081500030822     C                   EVAL      VABTC2=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900040714     C* VATTRC
082000040714     C                   IF        %subst(
082100040714     C                             %subst(vlrppt:posDaDft+1:
082200040714     C                             posADft-posDaDft-1):1:3)
082300040714     C                             = 'TRC'
082400040714     C                   EVAL      VATTRC=%trim(%subst(
082500040714     C                             %subst(vlrppt:posDaDft+1:
082600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700040714     C                   ENDIF
082800030822     C* ...
082900030822     C                   ENDIF
083000030822     C                   ENDDO
083100020204     C*
083200010330     C                   ENDSR
083300010607     C*----------------------------------------------------*
083400110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083500010607     C*----------------------------------------------------*
083600010607     C     IMPVAB        BEGSR
083700010607     C*
083800010607     C                   SETOFF                                       3132
083900030515     C*
084000010607     C                   MOVEL     datcor        VABAAS
084100010607     C                   MOVE      datcor        VABMGS
084200010607     C*
084300030715     C**********
084400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084500030715     C**********
084600030715     C                   IF        wGiro = *zeros
084700040113     C*
084800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085100080429     C                             %trim(intcol(5))<>*blanks
085200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085400080429     C                                      %trim(intcol(5))
085500070823     C     ';':CharCSV   XLATE     vindta        vindta
085600040510     C                   ENDIF
085700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085800031201     C     minu:maiu     XLATE     vindta        vindta
085900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086000030715     C                   DOW       posDa <= %len(%trim(vindta))
086100030715     C*
086200030715     C* Gestisco il 1° campo
086300030715     C                   IF        i = 1
086400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086500030715     C                   EVAL      posDa = 1
086600030715     C* Eseguo lo scan x trovare la fine del primo campo
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C* X i campi successivi al 1°
087200030715     C                   ELSE
087300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087400030715     C                   EVAL      posDa = posA + 1
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900030715     C                   ENDIF
089000030715     C* Incremento il contatore d campo
089100030715     C                   EVAL      i = i +1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089500030715     C* delimitazione testo
089600030715     C                   EVAL      i = 1
089700030715     C                   DOW       i < %elem(SkSplitFLD)
089800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090000030715     C                   EVAL      i = i + 1
090100030715     C                   ENDDO
090200030715     C                   EVAL      wGiro = 1
090300030715     C                   ELSE
090400030715     C                   EVAL      wGiro = 2
090500031204     C**********
090600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090700031204     C* d separatore campo
090800031204     C**********
090900031204     C                   EVAL      vindta = %trim(vindta)
091000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091100031204     C                   EVAL      vindta = %subst(vindta:2)
091200031204     C                   ENDDO
091300031204     C*
091400031204     C                   Z-ADD     *zeros        lunghInput        4 0
091500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091900110912     C                   ENDDO
092000030715     C**********
092100030715     C* Effettuo lo split del campo dati d input
092200030715     C**********
092300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092400030715     C                   DOW       posDa <= %len(%trim(vindta))
092500030715     C*
092600030715     C* Gestisco il 1° campo
092700030715     C                   IF        i = 1
092800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092900030715     C                   EVAL      posDa = 1
093000030715     C* Eseguo lo scan x trovare la fine del primo campo
093100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C* X i campi successivi al 1°
093600030715     C                   ELSE
093700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093800030715     C                   EVAL      posDa = posA + 1
093900031204     C* Verifico che nn vi sia il campo nn valorizzato
094000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094100031204     C* Se campo nn valorizzato skippo al prossimo
094200031204     C                   EVAL      SkSplitCSV(i) = *blanks
094300031204     C                   EVAL      posA  = posA + 1
094400031204     C                   ELSE
094500030715     C* Eseguo lo scan x trovare la fine del campo corrente
094600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094700030715     C* Gestisco l'ultimo campo
094800030715     C                   IF        posA = *zeros
094900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095000030715     C* A questo punto "estraggo" il campo corrente
095100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095200030715     C                                                    (posA-posDa))
095300030715     C                   LEAVE
095400030715     C                   ELSE
095500030715     C* A questo punto "estraggo" il campo corrente
095600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095700030715     C                                                    (posA-posDa))
095800030715     C                   ENDIF
095900031204     C                   ENDIF
096000030715     C                   ENDIF
096100030715     C* Incremento il contatore d campo
096200030715     C                   EVAL      i = i +1
096300030715     C                   ENDDO
096400030715     C*
096500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096600030715     C* delimitazione testo
096700030715     C                   EVAL      i = 1
096800030715     C                   DOW       i < %elem(SkSplitCSV)
096900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097100030715     C                   EVAL      i = i + 1
097200030715     C                   ENDDO
097300100720     C*
097400100720     C* Stacco un progressivo x numero spedizione
097500170609     C                   EXSR      repNSP
097600100326     C*
097700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097800030715     C                   EVAL      i = 1
097900030715     C                   DOW       i < %elem(SkSplitFLD)
098000030715     C*
098100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098300030715     C                             SkSplitCSV(i) <> *blanks
098400030721     C*** ATB
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098600030721     C                   EVAL      VABATB = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030715     C*** RSD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099300030715     C* ==
099400030715     C                   ENDIF
099500030721     C*** RD2
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030715     C*** IND
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100100030715     C                   EVAL      VABIND = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** LOD
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** PRD
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030721     C*** NZD
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101400160429     C     minu:maiu     XLATE     VABNZD        VABNZD
101500160429     C                   IF        VABNZD = 'IT'  OR
101600160429     C                             VABNZD = 'ITA'
101700160429     C                   EVAL      VABNZD = *blanks
101800160429     C                   ENDIF
101900030721     C                   ENDIF
102000030715     C*** RMA
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** NOT
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** NT2
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** VCA
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** VAS
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** FFD
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030721     C*** GC1
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GC2
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TSP
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105500030721     C                   ENDIF
105600040510     C*** CBO
105700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105900040510     C                   ENDIF
106000030721     C*** NAS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TIC
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GCA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** XCO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTM
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TCR
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** CTS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** FTM
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** VAD
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GMA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GGA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** GVA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** TC1
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** TC2
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** SCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** RMO
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** NMO
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*
112900030721     C* Reperisco quindi i campi numerici...
113000040510     C*** NSP
113100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113200040510     C                   EVAL      PiStr=SkSplitCSV(i)
113300040510     C                   EXSR      CHKNUM
113400040510     C                   IF        PiInt=*on
113500040510     C                   Z-ADD     PiVal         VABNSP
113600040510     C                   ELSE
113700040510     C                   SETON                                        31
113800040510     C                   EVAL      VABNSP = *zeros
113900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040510     C                             + ' ' + 'VABNSP'
114100040510     C                   ENDIF
114200040510     C                   ENDIF
114300071106     C*** RMN
114400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114500071106     C                   EVAL      PiStr=SkSplitCSV(i)
114600071106     C                   EXSR      CHKNUM
114700071106     C                   IF        PiInt=*on
114800071106     C                   Z-ADD     PiVal         VABRMN
114900071106     C                   ELSE
115000071106     C                   SETON                                        32
115100071106     C                   EVAL      VABRMN = *zeros
115200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115300071106     C                             + ' ' + 'VABRMN'
115400071106     C                   ENDIF
115500071106     C                   ENDIF
115600170609     C*** CAD
115700160421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115800160421     C                   EVAL      VABCAD = SkSplitCSV(i)
115900160421     C                   ENDIF
116000030721     C*** NCL
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on
116500030721     C                   Z-ADD     PiVal         VABNCL
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABNCL = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040122     C                             + ' ' + 'VABNCL'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** PKB
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABPKB
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABPKB = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABPKB'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** CAS
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
118800110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118900111124     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119000140915     C                             %trim(SkSplitCSV(i)) <> '.000'  AND
119100161018     C                             %trim(SkSplitCSV(i)) <> ',000'  AND
119200170620     C                             %trim(SkSplitCSV(i)) <> '000000000.000' AND
119300170620     C                             %trim(SkSplitCSV(i)) <> '000000000,000' AND
119400170620     C                             %trim(SkSplitCSV(i)) <> '0.00' AND
119500170620     C                             %trim(SkSplitCSV(i)) <> '0,00'
119600101018     C                   EVAL      wFlgCAS = '1'
119700161018     C                   EVAL      VABVCA = 'EUR'
119800101018     C                   ENDIF
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABCAS
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABCAS = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABCAS'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** CCM
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on      AND
121500030721     C                             PiVal<=9999999 AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABCCM
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABCCM = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200040510     C                             + ' ' + 'VABCCM'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** LNP
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on    AND
123000030721     C                             PiVal<=999   AND
123100030721     C                             PiVal>*zeros
123200030721     C                   Z-ADD     PiVal         VABLNP
123300030721     C                   ELSE
123400040715     C                   SETON                                        32
123500030721     C                   EVAL      VABLNP = *zeros
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700040510     C                             + ' ' + 'VABLNP'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** NRS
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on AND
124500030721     C                             PiVal<=99
124600030721     C                   Z-ADD     PiVal         VABNRS
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABNRS'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** LNA
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on    AND
125800030721     C                             PiVal<=999   AND
125900030721     C                             PiVal>*zeros
126000030721     C                   Z-ADD     PiVal         VABLNA
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      VABLNA = *zeros
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABLNA'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** CTR
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on  AND
127300030721     C                             PiVal<=999
127400030721     C                   Z-ADD     PiVal         VABCTR
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABCTR'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** IAS
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiNum=*on
128600030721     C                   Z-ADD     PiVal         VABIAS
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABIAS'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** VLB
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiNum=*on
129800030721     C                   Z-ADD     PiVal         VABVLB
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABVLB'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** QFT
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiNum=*on
131000030721     C                   Z-ADD     PiVal         VABQFT
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABQFT'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** NCD
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on      AND
132200030721     C                             PiVal<=9999999
132300030721     C                   Z-ADD     PiVal         VABNCD
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABNCD'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** NCA
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on      AND
133500030721     C                             PiVal<=9999999
133600030721     C                   Z-ADD     PiVal         VABNCA
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABNCA'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** ZNC
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on AND
134800030721     C                             PiVal<=99
134900030721     C                   Z-ADD     PiVal         VABZNC
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABZNC'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** DCR
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on       AND
136100030721     C                             PiVal<=99999999
136200030721     C                   Z-ADD     PiVal         VABDCR
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABDCR'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** HCR
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on   AND
137400030721     C                             PiVal<=9999
137500030721     C                   Z-ADD     PiVal         VABHCR
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABHCR'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** VMD
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiNum=*on
138700030721     C                   Z-ADD     PiVal         VABVMD
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABVMD'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** ANT
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on        AND
139900030721     C                             PiVal<=999999999
140000030721     C                   Z-ADD     PiVal         VABANT
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABANT'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** CMO
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiInt=*on
141200030721     C                   Z-ADD     PiVal         Num5_0
141300030721     C                   MOVEL(p)  Num5_0        VABCMO
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      VABCMO = *zeros
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABCMO'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100040510     C*
142200110930     C* Reperisco i campi del EDIVAT
142300040510     C*** VATNOT
142400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142600040510     C                   ENDIF
142700040510     C*** VATTRC
142800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143000040510     C                   ENDIF
143100060331     C*** VATNOT_A
143200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143400060331     C                   ENDIF
143500060331     C*** VATNOT_B
143600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143800060331     C                   ENDIF
143900060331     C*** VATNOT_E
144000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300110927     C*** VATNOT_I
144400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144600110927     C                   ENDIF
144700111124     C*** VATNOT_J
144800111124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
144900111124     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
145000111124     C                   ENDIF
145100120405     C*** VATNOT_4
145200120405     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
145300120405     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
145400120405     C                   ENDIF
145500131018     C*** VATNOT_S
145600131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
145700131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
145800131018     C                   ENDIF
145900141128     C*** VATNOT_Y
146000141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
146100141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
146200141128     C                   ENDIF
146300070823     C*
146400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146500160421     C*
146600170609     C*** W_VATNOT_IJ
146700160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_IJ'
146800160421     C                   MOVEL     *blanks       wEMAIL           70
146900160421     C                   EVAL      wEMAIL = %trim(SkSplitCSV(i))
147000160421     C                   EVAL      wVATNOT_I = %subst(wEMAIL:1:35)
147100160421     C                   EVAL      wVATNOT_J = %subst(wEMAIL:36:35)
147200160421     C                   ENDIF
147300070925     C*
147400071029     C***  <===  -----------------------------------------
147500040510     C*
147600030715     C                   ENDIF
147700030715     C                   EVAL      i = i + 1
147800030715     C                   ENDDO
147900070925     C*
148000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148100100831     C*
148200070925     C*
148300071029     C***  <===  -----------------------------------------
148400070925     C*
148500070925     C* Considerazioni finali su CBO/CAS
148600100318     C                   IF        wFlgCAS = '1'    OR
148700100318     C                             VABCAS  > *zeros
148800070925     C                   IF        VABCBO = '1'
148900070925     C                   EVAL      VABCBO = '4'
149000070925     C                   ENDIF
149100070925     C                   IF        VABCBO = '2'
149200070925     C                   EVAL      VABCBO = '6'
149300070925     C                   ENDIF
149400070925     C                   ENDIF
149500070925     C***  <===  ----------------------------
149600040510     C*
149700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149800040510     C                   IF        VABNSP = *zeros
149900040510     C                   SETON                                        31
150000040510     C                   ENDIF
150100040715     C*
150200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150300040715     C                   IF        VABLNP = *zeros
150400040715     C                   SETON                                        31
150500040715     C                   ENDIF
150600040510     C*
150700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150800040510     C                   IF        VABRMN = *zeros
150900040510     C                   Z-ADD     VABNSP        VABRMN
151000040510     C                   ENDIF
151100040510     C*
151200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151300040510     C                   IF        VABRMA = *blanks
151400040714     C                   MOVEL     VABRMN        VABRMA
151500040510     C                   ENDIF
151600040128     C*
151700040510     C* Se provincia nn valorizzata la reperisco
151800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151900110912     C                   IF        VABPRD =  *blanks AND
152000110912     C                             VABNZD =  *blanks AND
152100110912     C                             VABCAD <> *blanks AND
152200110912     C                             VABLOD <> *blanks
152300040128     C                   CLEAR                   TISI95DS
152400040128     C                   EVAL      I95TCN = '3'
152500040128     C                   Z-ADD     datcor        I95DAT
152600040128     C                   EVAL      I95NAR = VABNZD
152700040128     C                   EVAL      I95CAP = VABCAD
152800040128     C                   EVAL      I95LOC = VABLOD
152900040128     C                   CALL      'TISI95R'
153000040128     C                   PARM                    TISI95DS
153100040128     C                   EVAL      VABPRD = O95PRV
153200040510     C                   ENDIF
153300020204     C*
153400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153500020204     C                   EXSR      CHKIMPDIV
153600030715     C                   ENDIF
153700020204     C*
153800010607     C                   ENDSR
153900010607     C*----------------------------------------------------*
154000010601
154100020204
154200020204     C*----------------------------------------------------*
154300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154400020204     C*----------------------------------------------------*
154500020204     C     CHKIMPDIV     BEGSR
154600020204     C*
154700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154800020204     C                   Z-ADD     *zeros        wrkDec            9 9
154900020204     C*
155000020204     C* Come prima cosa effettuo considerazioni sulla divisa
155100020204     C                   IF        vabIAS > *zeros
155200020204     C                   IF        vabVAS <> 'EUR'
155300020204     C                   EVAL      vabVAS =  'ITL'
155400020204     C                   ENDIF
155500020204     C                   ENDIF
155600020204     C*
155700020204     C                   IF        vabCAS > *zeros
155800020204     C                   IF        vabVCA <> 'EUR'
155900020204     C                   EVAL      vabVCA =  'ITL'
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C                   IF        vabVMD > *zeros
156400020204     C                   IF        vabVAD <> 'EUR'
156500020204     C                   EVAL      vabVAD =  'ITL'
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157000020204     C                   Z-ADD     vabIAS        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVAS = 'ITL'
157300020204     C                   EVAL      vabIAS = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157800020204     C                   Z-ADD     vabCAS        wrkDec
157900020204     C                   IF        wrkDec > *zeros
158000020204     C                   IF        vabVCA = 'ITL'
158100020204     C                   EVAL      vabCAS = *zeros
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158600020204     C                   Z-ADD     vabVMD        wrkDec
158700020204     C                   IF        wrkDec > *zeros
158800020204     C                   IF        vabVAD = 'ITL'
158900020204     C                   EVAL      vabVMD = *zeros
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C                   ENDSR
159400020204     C***
159500010330
159600100318
159700100318
159800100318     C*----------------------------------------------------*
159900100318     C*  CONTROLLO NUMERICITA' CAMPI
160000100318     C*----------------------------------------------------*
160100100318     C     CHKNUM        BEGSR
160200100318     C*
160300100318     C                   IF        PiDecChr = *blanks
160400100318     C                   EVAL      PiDecChr = CharNUM
160500100318     C                   ENDIF
160600100318     C*
160700100318     C                   callp     UBISNUM_Check(PiStr
160800100318     C                                          :PiDecChr
160900100318     C                                          :PiVal
161000100318     C                                          :PiNum
161100100318     C                                          :PiInt)
161200100318     C*
161300100318     C                   ENDSR
161400100318     C***
161500010601
161600010601
161700010601
161800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161900010601     C     repfil        BEGSR
162000010601     C*
162100010601     C                   if        invfil = *zeros and
162200010601     C                             depfil > *zeros and
162300010629     C                             (vinflg = *blanks or
162400010629     C                              vinflg = *zeros)
162500010601     C
162600010601     C                   eval      invfil = depfil
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        depfil <> invfil and
163000010601     C                             invfil > *zeros
163100010601     C                   eval      flgMulti = '1'
163200010601     C                   if        vinflg = *blanks
163300010601     C                   add       1             cntNonEl
163400010601     C                   endif
163500010601     C                   endif
163600010601     C*
163700010601     C                   if        vinflg = '2'
163800010601     C                   eval      flgStato = '2'
163900010601     C                   endif
164000010601     C*
164100010601     C                   ENDSR
164200010601     C***
164300010601
164400010601
164500010601
164600010330
164700040119      /TITLE Invio dei dati al punto operativo.
164800040119     C     invio         BEGSR
164900040119     C*
165000110930     C* 1° invio EDIVAT
165100040119     C                   reset                   dscmz
165200040119     C                   move      vlrpoi        cmzdst
165300110930     C                   eval      cmzfld = 'EDIVATWR'
165400040119     C                   eval      cmzmbd = vlrhdl
165500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165600040119     C***                if        prmfir = *blanks
165700110930     C                   eval      cmzfla = 'EDIVAT0F'
165800110930     C                   eval      cmzmba = 'EDIVAT0F'
165900040119     C***                else
166000040119     C***                eval      cmzfla = prmfir
166100040119     C***                eval      cmzmba = prmfir
166200040119     C***                endif
166300040119     C                   eval      cmznrr = *zeros
166400040119     C                   move      §ctrokvt      cmznrr
166500040119     C                   eval      cmzlba = vlrfl1
166600040119     C                   call(e)   'TIS711C'
166700040119     C                   parm                    dscmz
166800040119     C                   parm      *blanks       esito
166900040119     C                   if        %error
167000040119     C                             or cmzerr = '1'
167100040119     C                             or esito  = '1'
167200040119     C                   eval      wrkesito = '3'
167300040119     C                   else
167400040119     C*
167500110930     C* 2° invio EDIVAB
167600040119     C                   reset                   dscmz
167700040119     C                   move      vlrpoi        cmzdst
167800040119     C                   eval      cmzfld = vlrfou
167900040119     C                   eval      cmzmbd = vlrhdl
168000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168100040119     C***                if        prmfir = *blanks
168200110930     C                   eval      cmzfla = 'EDIVAB0F'
168300110930     C                   eval      cmzmba = 'EDIVAB0F'
168400040119     C***                else
168500040119     C***                eval      cmzfla = prmfir
168600040119     C***                eval      cmzmba = prmfir
168700040119     C***                endif
168800040119     C                   eval      cmznrr = *zeros
168900040119     C                   move      §ctrokvb      cmznrr
169000040119     C                   eval      cmzlba = vlrfl1
169100040119     C                   call(e)   'TIS711C'
169200040119     C                   parm                    dscmz
169300040119     C                   parm      *blanks       esito
169400040119     C                   if        %error
169500040119     C                             or cmzerr = '1'
169600040119     C                             or esito  = '1'
169700040119     C                   eval      wrkesito = '3'
169800040119     C                   endif
169900040119     C                   endif
170000040119     C*
170100040119     C                   ENDSR
170200040119     C***
170300010601
170400010601
170500010601
170600010601
170700010601      /TITLE Invio dei dati al punto operativo.
170800010601     C     opeini        BEGSR
170900010601     C*
171000010601     C* Inizializzo flag e contatori operativi
171100010601     C                   movel     '0'           flgGiro           1
171200010601     C                   movel     '0'           flgMulti          1
171300010601     C                   movel     '1'           flgStato          1
171400010615     C                   movel     '0'           flgOk             1
171500010601     C                   z-add     *zeros        cntNonEl         10 0
171600010601     C                   z-add     *zeros        depfil            3 0
171700010601     C                   z-add     *zeros        invfil            3 0
171800010601     C*
171900010601     C                   ENDSR
172000010601     C***
172100070326
172200070326
172300070326
172400070326
172500070326     C     *pssr         BEGSR
172600070329     C*
172700070329     C                   if        %open(tivin00r)
172800070329     C                   close     tivin00r
172900070329     C                   endif
173000110930     C                   if        %open(edivabwr)
173100110930     C                   close     edivabwr
173200070329     C                   endif
173300110930     C                   if        %open(edivatwr)
173400110930     C                   close     edivatwr
173500070329     C                   endif
173600070326     C*
173700070326     C* Effettuo la chiamata al CLLE preposto
173800110930     C                   call(e)   'TITVEVTC'
173900070326     C                   parm                    parccm
174000070326     C                   parm                    parmbr
174100070326     C                   parm      '2'           paropz
174200070326     C*
174300070326     C                   eval      wrkesito = '2'
174400070404     C*
174500070404     C                   seton                                        LR
174600070326     C*
174700070326     C                   ENDSR     '*CANCL'
174800070326     C***
174900100318
175000100318
175100100318
175200100318     C     normCSV       BEGSR
175300100318     C*
175400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175500100318     C* separatori campi
175600100318     C                   if        CharTXT <> *blanks
175700100318     C                   z-add     *zeros        wLenVINDTA        4 0
175800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175900100318     C                   setoff                                       80
176000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176300100318     C                   if        not *in80
176400100318     C                   seton                                        80
176500100318     C                   else
176600100318     C                   setoff                                       80
176700100318     C                   endif
176800100318     C                   endif
176900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177000100318     C                             and *in80 = *on
177100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177200100318     C                   endif
177300100318     C                   enddo
177400100318     C                   endif
177500100318     C*
177600100318     C                   ENDSR
177700100318     C***
177800070326
177900100326
178000100326
178100100326     C*----------------------------------------------------*
178200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178300100326     C*----------------------------------------------------*
178400100326     C     REPNSP        BEGSR
178500100326     C*
178600100326     C* NSP => Stacco un numeratore da AZNUM
178700100326     C                   clear                   TRUL33DS
178800100326     C                   eval      I33OPE = *zeros
178900100326     C                   eval      I33CNU = 302
179000100326     C                   eval      I33NUM = 1
179100100326     C                   movel     TRUL33DS      KPJBU
179200100326     C                   call      'TRUL33R'
179300100326     C                   parm                    KPJBA
179400100326     C                   movel     KPJBU         TRUL33DS
179500100326     C                   if        O33ERR = *zeros
179600100326     C                   z-add     O33NRF        VABNSP
179700100326     C                   z-add     O33NRF        VATNSP
179800100326     C                   else
179900100326     C                   SETON                                        31
180000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180100100326     C                             + ' ' + 'VABNSP VATNSP'
180200100326     C                   endif
180300100326     C*
180400100326     C                   ENDSR
180500010330
180600100326
180700010330
180800000613     C     *inzsr        BEGSR
180900990910     C*
181000990910     C     *entry        plist
181100990920     C                   parm                    tivlrds
181200990921     C                   parm      wrkesito      esito
181300000724     C                   parm                    prmlit
181400000710     C                   parm                    prmfir
181500010330     C*
181600010330     C* CALCOLA LA DATA CORRENTE
181700110930     C                   time                    wn14             14 0
181800110930     C                   movel     wn14          oracor            6 0          *ORA
181900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182000100318     C                   eval      datcor = %dec(%date() : *ISO)
182100000613     C*
182200000613     C                   ENDSR
182300000613     C***
182400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182500170609VABCCM;VABLNP;VABNSP;VABCBO;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;
182600170609VABNZD;VABCTR;VABTSP;VABNCL;VABPKB;VABCAS;VABTIC;VABRMN;VABRMA;VABNOT;
182700170609VABNT2;VABFFD;VABDCR;VABTCR;VATNOT_A;VABNOT_B;W_VATNOT_IJ;VATNOT_S;
182800170609
182900100318
