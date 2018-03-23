000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400161003      * - disk A
000500130122      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700120209     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001000110930     FEDIVATwr  O    E             DISK    usropn
001100000313     D*
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300040128     D tisi95ds      e ds
002400110930     D edivabs       e ds                  extname(edivab0f)
002500110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002600110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
002700010330     D esito           s              1
002800010330     D prmlit          s             10
002900010330     D prmfir          s             10
003000010330     D wrkesito        s                   like(esito)
003100010330     D rrnum           s              6  0 INZ(*zeros)
003200040510     D parccm          s              8    INZ(*blanks)
003300040119     D parmbr          s             10    INZ(*blanks)
003400040119     D paropz          s              1    INZ(*blanks)
003500040119     D chkcall         s              1    INZ(*blanks)
003600080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003700080418     D wFlgCAS         s              1    INZ(*blanks)
003800121030     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003900121030     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004000141007     D Intestazioni    s               n   inz(*on)
004100120203     D wNomeFile       s             30    INZ(*blanks)
004200100326     D*------------------
004300100326     D* DS REPERIMENTO NUMERATORE
004400100326     D*------------------
004500100326     D trul33ds      e ds                  inz
004600100326     D*------------------
004700100326     D* DS ARCHITETTURA
004800100326     D*------------------
004900100326     D kpjba         e ds                  inz
005000100326
005100030715     D*------------------
005200030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005300030715     D*------------------
005400030715     D SkSplitFLD      S             10    DIM(100)
005500030715     D SkSplitCSV      S            256    DIM(100)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900100318     D CharSOS         S              1
006000030715     D posDa           S              3  0 INZ(*zeros)
006100030715     D posA            S              3  0 INZ(*zeros)
006200030715     D i               s              3  0 INZ(1)
006300030715     D wGiro           s              1  0 INZ(*zeros)
006400070823     D wVATNOT_A       s                   LIKE(VATNOT)
006500060331     D wVATNOT_B       s                   LIKE(VATNOT)
006600060331     D wVATNOT_E       s                   LIKE(VATNOT)
006700110927     D wVATNOT_I       s                   LIKE(VATNOT)
006800120202     D wVATNOT_J       s                   LIKE(VATNOT)
006900120202     D wVATNOT_IJ      s             70
007000141007     D wVATNOT_S       s                   LIKE(VATNOT)
007100130128     D w70_1           s             70
007200130128     D w70_2           s             70
007300130128     D w70_3           s             70
007400130128     D w70_4           s             70
007500030822     D*------------------
007600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007700030822     D*------------------
007800030822     D posDaDft        S              3  0 INZ(*zeros)
007900030822     D posADft         S              3  0 INZ(*zeros)
008000030822     D j               s              3  0 INZ(1)
008100030822     D wGiroDft        s              1  0 INZ(*zeros)
008200080418     D*------------------
008300080418     D* Costanti
008400080418     D*------------------
008500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700100318
008800100318     D*------------------
008900100318     D* LINKING A DEFINIZIONI ESTERNE
009000100318     D*------------------
009100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300100318
009400100909     D*------------------
009500100909     D* DS CALCOLO CHECK-DIGITS
009600100909     D*------------------
009700100909     D TRUL28DSX     e ds                  inz
009800010330
009900010330
010000010330
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601     C*--------------------------------------------------------
013300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013400010601     C*--------------------------------------------------------
013500010601     C     ENDELA        BEGSR
013600040119     C*
013700040119     C* Effettuo la chiamata al CLLE preposto
013800110930     C                   call(e)   'TITVEVTC'
013900040510     C                   parm                    parccm
014000040119     C                   parm                    parmbr
014100040119     C                   parm      '2'           paropz
014200050201     C*
014300050201     C* Effettuo lancio TISI95 solo x chiusura
014400050201     C                   CLEAR                   TISI95DS
014500050201     C                   EVAL      I95TLA = 'C'
014600050201     C                   CALL      'TISI95R'
014700050201     C                   PARM                    TISI95DS
014800000616     C*
014900010601     C                   ENDSR
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000110930     C                   if        not %open(edivabwr)
016100110930     C                   open      edivabwr
016200010330     C                   endif
016300040119     C*
016400110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016500040119     C                   exsr      prevat
016600040119     C*
016700040119     C                   if        chkcall = '0'
016800040119     C*
016900110930     C                   if        not %open(edivatwr)
017000110930     C                   open      edivatwr
017100040119     C                   endif
017200010330     C*
017300040119     C                   clear                   §CTROKVB          7 0
017400040119     C                   clear                   §CTROKVT          7 0
017500040119     C                   clear                   §CTRMO            7 0
017600040119     C                   clear                   §CTRNO            7 0
017700121030     C*
017800121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017900121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018000121030     C                   IF        Intestazioni = *off
018100121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018200121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018300121030     C                   exsr      inzvar
018400121030     C                   exsr      impvab
018500121030     C                   ENDIF
018600010330     C*
018700030822     C                   do        *HIVAL
018800010330     C*
018900030822     C                   read      tivin00r                               70
019000010618     C*
019100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019200010618     C                   if        vinflg = '1'
019300010618     C                   eval      flgOk = '1'
019400010618     C                   endif
019500040510     C*
019600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019700040510     C                   z-add     *zeros        wLenVINDTA        4 0
019800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020200040510     C                   z-add     1             wFlgVINDTA
020300040510     C                   leave
020400040510     C                   endif
020500040510     C                   enddo
020600010618     C*
020700040510     C                   if        vindta > *blanks AND
020800040510     C                             wFlgVINDTA = 1
020900010330     C                   add       1             rrnum
021000010330     C*
021100010601     C                   if        *in70 = *off and
021200010330     C                             (vinflg = *blanks
021300010330     C                              or vinflg = '0'
021400010330     C                              or vinflg = '2')
021500010330     C*
021600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021700010711     C                   if        vinflg = *blanks or vinflg = '0'
021800010711     C                   clear                   vinmsg
021900010711     C                   endif
022000010601     C*
022100070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022200070925     C                   eval      vindta  = ' ' + %trim(vindta)
022300070925     C*
022400080527     C                   exsr      inzvar
022500100318     C                   exsr      normCSV
022600010605     C                   exsr      impvab
022700010601     C*
022800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022900010605     C                   eval      depfil = VABLNP
023000010601     C                   exsr      repfil
023100010601     C                   if        depfil = invfil
023200021025     C                   if        vlrpoi = 999
023300030822     C                   move(P)   invfil        VABFGS
023400021025     C                   else
023500030822     C                   move(P)   vlrpoi        VABFGS
023600021025     C                   endif
023700010601     C*
023800010601     C                   exsr      PREELA
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200040119     C  N31              add       1             §CTROKVB
024300040119     C   32              add       1             §CTRMO
024400040119     C   31              add       1             §CTRNO
024500080418     C*
024600030822     C                   if        wGiro = 2
024700080513     C* Scrivo sempre il VAT
024800080513     C  N31              exsr      wrivat
024900110201     C                   exsr      chkvab
025000080513     C                   endif
025100010604     C*
025200010604     C                   if        *in31 = *off and
025300010604     C                             *in32 = *off
025400010604     C                   eval      vinflg = '1'
025500010604     C                   else
025600010604     C                   eval      vinflg = '2'
025700010604     C                   endif
025800010604     C                   endif
025900010601     C*
026000010604     C                   endif
026100010604     C*
026200010330     C                   else
026300010330     C                   eval      vinflg = '1'
026400010330     C                   endif
026500010601     C*
026600010601     C  N70              update    tivin000
026700010330     C*
026800030822     C  N70              enddo
026900040119     C*
027000080527     C* Verifico dati rimasti "in cannna"...
027100080527     C                   seton                                        65
027200110201     C                   exsr      chkvab
027300080527     C*
027400040119     C                   endif                                                  (endif - chkcall)
027500010601     C*
027600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027700010601     C                   if        cntNonEl = *zeros or
027800010601     C                             flgMulti = '0'
027900010330     C* Se non ci sono record con errori ...
028000010601     C                   if        §ctrno = 0 and
028100010604     C                             §ctrmo = 0 and
028200010601     C                             flgStato <> '2'
028300010330     C* ... restituisco esito OK.
028400010330     C                   eval      wrkesito = '0'
028500010330     C                   else
028600040119     C                   if        §ctrokvb > 0
028700010330     C                   eval      wrkesito = '1'
028800010330     C                   else
028900010615     C                   if        flgOk = '0'
029000010615     C                   eval      wrkesito = '2'
029100010615     C                   else
029200010615     C                   eval      wrkesito = '6'
029300010615     C                   endif
029400010330     C                   endif
029500010330     C                   endif
029600010601     C                   else
029700010601     C                   eval      wrkesito = '9'
029800010601     C                   endif
029900010330     C*
030000010330     C                   if        %open(tivin00r)
030100010330     C                   close     tivin00r
030200010330     C                   endif
030300110930     C                   if        %open(edivabwr)
030400110930     C                   close     edivabwr
030500010330     C                   endif
030600110930     C                   if        %open(edivatwr)
030700110930     C                   close     edivatwr
030800040512     C                   endif
030900010601     C*
031000010601     C                   if        vlrpoi <> 999
031100010601     C                   eval      invfil = vlrpoi
031200010601     C                   endif
031300010330     C*
031400040119     C                   if        §ctrokvb > 0
031500010601     C                             and invfil > *zeros
031600010330     C                   exsr      invio
031700010330     C                   endif
031800010601     C*
031900010618     C                   if        flgGiro = '1'
032000010601     C                   exsr      endela
032100010618     C                   endif
032200010330     C*
032300010330     C                   ENDSR
032400010330     C***
032500080527
032600080527
032700080527
032800080527     C*----------------------------------------------------*
032900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033000080527     C*----------------------------------------------------*
033100100413     C     CHKVAB        BEGSR
033200110201     C*
033300110201     C* Se richiesti ignorare colli / peso / volume
033400110315 xxx C***                if        *in35
033500120203 xxx C***                z-add     *zeros        VABNCL              F0461
033600120203 xxx C***                z-add     *zF046190081  VABPKB
033700110315 xxx C***                z-add     *zeros        VABVLB
033800110315 xxx C***                endif
033900080527     C*
034000080527     C* Scrivo il VAB solo a rottura d spedizione
034100080527     C                   if        depspe = *zeros
034200110930     C  N31              eval      edivabs_sav = edivabs
034300080527     C                   eval      depspe = vabNSP
034400080527     C                   else
034500080527     C                   if        depspe = vabNSP AND *in65 = *off
034600100413     C   51              add       VABNCL        sav_VABNCL
034700100413     C   51              add       VABPKB        sav_VABPKB
034800100413     C   51              add       VABVLB        sav_VABVLB
034900100720     C   52              z-add     VABNCL        sav_VABNCL
035000100720     C   52              z-add     VABPKB        sav_VABPKB
035100100720     C   52              z-add     VABVLB        sav_VABVLB
035200100720     C   53              add       VABNCL        sav_VABNCL
035300100720     C   53              z-add     VABPKB        sav_VABPKB
035400100720     C   53              z-add     VABVLB        sav_VABVLB
035500080527     C                   else
035600120203     C  N31              eval      edivabs_ok = edivabs_sav
035700080527     C  N31              exsr      wrivab
035800110930     C  N31              eval      edivabs_sav = edivabs
035900080527     C                   eval      depspe = vabNSP
036000080527     C                   endif
036100080527     C                   endif
036200080527     C*
036300080527     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036900040119     C*----------------------------------------------------*
037000040510     C     WRIVAB        BEGSR
037100080513     C*
037200120203     C* EDIVABWR ha il prefisso "ok_"
037300120203     C*
037400120203     C* Gestisco l'eventuale rottura x numero spedizione
037500080527     C                   if        VABNSP <> depspe or *in65 = *on
037600070301     C                   if        wVATNOT_E <> *blanks
037700120203     C                   eval      ok_VABCTM = '7Q'
037800070301     C                   endif
037900110930     C*
038000110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038100120203     C                   MOVE(P)   vlrMSG        wNomeFile
038200130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
038300120203     C                   EVAL      ok_VABDCM = DATCOR
038400120203     C                   EVAL      ok_VABDTS = DATCOR
038500120203     C                   EVAL      ok_VABHMS = ORACOR
038600120203     C                   EVAL      ok_VABCNT = 1
038700110930     C*
038800110930     C                   write     edivab00
038900080513     C                   endif
039000040119     C*
039100040119     C                   ENDSR
039200040119
039300040119
039400040119
039500040119     C*----------------------------------------------------*
039600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039700040119     C*----------------------------------------------------*
039800040510     C     WRIVAT        BEGSR
039900060512     C*
040000060512     C* Inizializzo i campi chiave
040100060512     C                   eval      VATFGS = VABFGS
040200060512     C                   eval      VATAAS = VABAAS
040300060512     C                   eval      VATCCM = VABCCM
040400070823     C                   eval      VATNRS = VABNRS
040500060512     C                   eval      VATNSP = VABNSP
040600060512     C                   eval      VATLNP = VABLNP
040700040119     C*
040800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040900040510     C                   if        VATTRC = *blanks
041000040510     C                   eval      VATTRC = 'E'
041100040510     C                   endif
041200110318     C                   if        VATNOT <> *blanks
041300120203     C                   MOVE(P)   vlrMSG        wNomeFile
041400130122     C***                EVAL      VATCMR = wNomeFile
041500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
041600110930     C                   eval      VATCNT = 1
041700110930     C                   write     EDIVAT00
041800040119     C                   add       1             §CTROKVT
041900110318     C                   endif
042000060331     C*
042100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042200060331     C                   if        wVATNOT_A <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_A
042400060331     C                   eval      VATTRC = 'A'
042500120203     C                   MOVE(P)   vlrMSG        wNomeFile
042600130122     C***                EVAL      VATCMR = wNomeFile
042700141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
042800110930     C                   eval      VATCNT = 1
042900110930     C                   write     EDIVAT00
043000060331     C                   add       1             §CTROKVT
043100060331     C                   endif
043200060331     C*
043300060331     C                   if        wVATNOT_B <> *blanks
043400060331     C                   eval      VATNOT = wVATNOT_B
043500060331     C                   eval      VATTRC = 'B'
043600120203     C                   MOVE(P)   vlrMSG        wNomeFile
043700130122     C***                EVAL      VATCMR = wNomeFile
043800141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
043900110930     C                   eval      VATCNT = 1
044000110930     C                   write     EDIVAT00
044100060331     C                   add       1             §CTROKVT
044200060331     C                   endif
044300141007     C*
044400141007     C                   if        wVATNOT_IJ <> *blanks
044500141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044600141007     C                   eval      VATTRC = 'I'
044700141007     C***                EVAL      VATCMR = wNomeFile
044800141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044900141007     C                   eval      VATCNT = 1
045000141007     C                   write     EDIVAT00
045100141007     C                   add       1             §CTROKVT
045200141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045300141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045400141007     C                   eval      VATTRC = 'J'
045500141007     C***                EVAL      VATCMR = wNomeFile
045600141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
045700141007     C                   eval      VATCNT = 1
045800141007     C                   write     EDIVAT00
045900141007     C                   endif
046000141007     C                   endif
046100060331     C*
046200060331     C                   if        wVATNOT_E <> *blanks
046300060331     C                   eval      VATTRC = 'E'
046400121030     C                   MOVE(P)   vlrMSG        wNomeFile
046500130122     C***                EVAL      VATCMR = wNomeFile
046600141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
046700121030     C                   eval      VATCNT = 1
046800141007     C*
046900141007     C* NON Sviluppare i "CHI SONO"
047000141007     C*
047100141007     C                   eval      VATNOT = '00' + %trim(wVATNOT_E)
047200141007     C                   write     EDIVAT00
047300121030     C                   endif
047400110927     C*
047500110927     C                   if        wVATNOT_I <> *blanks
047600110927     C                   eval      VATNOT = wVATNOT_I
047700110927     C                   eval      VATTRC = 'I'
047800120203     C                   MOVE(P)   vlrMSG        wNomeFile
047900130122     C***                EVAL      VATCMR = wNomeFile
048000141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048100110930     C                   eval      VATCNT = 1
048200110930     C                   write     EDIVAT00
048300110927     C                   add       1             §CTROKVT
048400110927     C                   endif
048500120202     C*
048600120202     C                   if        wVATNOT_J <> *blanks
048700120202     C                   eval      VATNOT = wVATNOT_J
048800120202     C                   eval      VATTRC = 'J'
048900120203     C                   MOVE(P)   vlrMSG        wNomeFile
049000130122     C***                EVAL      VATCMR = wNomeFile
049100141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
049200120202     C                   eval      VATCNT = 1
049300120202     C                   write     EDIVAT00
049400120202     C                   add       1             §CTROKVT
049500120202     C                   endif
049600141007     C*
049700141007     C                   if        wVATNOT_S <> *blanks
049800141007     C                   eval      VATNOT = wVATNOT_S
049900141007     C* i 2 flag devono essere blank
050000141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
050100141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
050200141007     C                   eval      VATTRC = 'S'
050300141007     C***                EVAL      VATCMR = wNomeFile
050400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
050500141007     C                   eval      VATCNT = 1
050600141007     C                   write     EDIVAT00
050700141007     C                   add       1             §CTROKVT
050800141007     C                   endif
050900040119     C*
051000040119     C                   ENDSR
051100040119
051200040119
051300040119
051400040119     C*----------------------------------------------------*
051500110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051600040119     C*----------------------------------------------------*
051700040119     C     PREVAT        BEGSR
051800040119     C*
051900110930     C* Compongo il nome del membro da dare al EDIVATWR
052000040119     C                   eval      parmbr = vlrhdl
052100040119     C                   movel     'M'           parmbr
052200040510     C                   eval      parccm = vlrksc
052300040119     C                   eval      paropz = '1'
052400040119     C* Effettuo la chiamata al CLLE preposto
052500110930     C                   call(e)   'TITVEVTC'
052600040119     C                   parm                    parccm
052700040119     C                   parm                    parmbr
052800040119     C                   parm                    paropz
052900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053000040119     C                   if        %error
053100040119     C                   movel     '1'           chkcall
053200040119     C                   else
053300040119     C                   movel     '0'           chkcall
053400040119     C                   endif
053500040119     C*
053600040119     C                   ENDSR
053700010601
053800010601
053900010601
054000010330     C*----------------------------------------------------*
054100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054200010330     C*----------------------------------------------------*
054300040119     C     INZVAR        BEGSR
054400080527     C*
054500080527     C                   setoff                                       65
054600010330     C*
054700030822     C* Inizializzo variabili di wrk
054800130128     C                   Z-ADD     *zeros        Num3_0            3 0
054900020204     C                   Z-ADD     *zeros        Num5_0            5 0
055000101018     C                   MOVEL     *blanks       wFlgCAS
055100060331     C                   MOVEL     *blanks       wVATNOT_A
055200060331     C                   MOVEL     *blanks       wVATNOT_B
055300060331     C                   MOVEL     *blanks       wVATNOT_E
055400110927     C                   MOVEL     *blanks       wVATNOT_I
055500120202     C                   MOVEL     *blanks       wVATNOT_J
055600120202     C                   MOVEL     *blanks       wVATNOT_IJ
055700141007     C                   MOVEL     *blanks       wVATNOT_S
055800130128     C                   MOVEL     *blanks       w70_1
055900130128     C                   MOVEL     *blanks       w70_2
056000130128     C                   MOVEL     *blanks       w70_3
056100130128     C                   MOVEL     *blanks       w70_4
056200030822     C*
056300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056400030822     C                   CLEAR                   SkSplitCSV
056500030822     C                   EVAL      i = 1
056600030822     C                   EVAL      posDa = *zeros
056700030822     C                   EVAL      posA  = *zeros
056800080527     C*
056900110930     C                   CLEAR                   EDIVABS
057000110930     C                   CLEAR                   EDIVAT00
057100030822     C*
057200030822     C* Reimposto i valori di default
057300030822     C                   EXSR      DEFCAM
057400030822     C*
057500010330     C                   ENDSR
057600010330     C*----------------------------------------------------*
057700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057800010330     C*----------------------------------------------------*
057900010330     C     DEFCAM        BEGSR
058000030822     C*
058100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058200030715     C* e delimitatore testo.
058300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058600100318     C*
058700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058800100318     C                   EVAL      CharSOS = CharNUM
058900030822     C*
059000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059100100720     C                   SETOFF                                       505152
059200100720     C                   SETOFF                                       53
059300100413     C                   SELECT
059400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
059500100720     C                   SETON                                        50
059600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
059700100413     C                   SETON                                        51
059800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059900100720     C                   SETON                                        52
060000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
060100100720     C                   SETON                                        53
060200100413     C                   ENDSL
060300030822     C*
060400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060500030822     C                   EVAL      posDaDft = 1
060600030822     C                   EVAL      posADft  = 0
060700030822     C                   EVAL      wGiroDft = 0
060800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060900030822     C                             posDaDft > 0
061000030822     C*
061100030822     C* Gestisco il 1° giro
061200030822     C                   IF        wGiroDft = 0
061300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061500030822     C* Incremento il contatore dei "giri"
061600030822     C                   EVAL      wGiroDft = 1
061700030822     C                   ELSE
061800030822     C                   EVAL      posDaDft = posADft
061900030822     C                   ENDIF
062000030822     C* Eseguo lo scan x trovare la fine del campo corrente
062100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062200030822     C*
062300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062500030822     C                   IF        posDaDft > 0 AND
062600030822     C                             posADft  > 0
062700030822     C* NCL
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'NCL'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABNCL
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* CCM
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'CCM'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABCCM
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* LNP
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'LNP'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABLNP
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* NRS
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'NRS'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABNRS
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* CTR
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'CTR'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiInt=*on
068900030822     C                   Z-ADD     PiVal         VABCTR
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* PKB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'PKB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABPKB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* VLB
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'VLB'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABVLB
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* QFT
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'QFT'
072300030822     C                   EVAL      PiStr=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   EXSR      CHKNUM
072700030822     C                   IF        PiNum=*on
072800030822     C                   Z-ADD     PiVal         VABQFT
072900030822     C                   ENDIF
073000030822     C                   ENDIF
073100030822     C* CBO
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'CBO'
073600030822     C                   EVAL      VABCBO=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* TSP
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'TSP'
074500030822     C                   EVAL      VABTSP=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* VAS
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'VAS'
075400030822     C                   EVAL      VABVAS=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* VCA
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'VCA'
076300030822     C                   EVAL      VABVCA=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700070925     C* TIC
076800070925     C                   IF        %subst(
076900070925     C                             %subst(vlrppt:posDaDft+1:
077000070925     C                             posADft-posDaDft-1):1:3)
077100070925     C                             = 'TIC'
077200070925     C                   EVAL      VABTIC=%trim(%subst(
077300070925     C                             %subst(vlrppt:posDaDft+1:
077400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500070925     C                   ENDIF
077600030822     C* GCA
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'GCA'
078100030822     C                   EVAL      VABGCA=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* CTM
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'CTM'
079000030822     C                   EVAL      VABCTM=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* FFD
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'FFD'
079900030822     C                   EVAL      VABFFD=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* VAD
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'VAD'
080800030822     C                   EVAL      VABVAD=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* GMA
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'GMA'
081700030822     C                   EVAL      VABGMA=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100030822     C* GGA
082200030822     C                   IF        %subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):1:3)
082500030822     C                             = 'GGA'
082600030822     C                   EVAL      VABGGA=%trim(%subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900030822     C                   ENDIF
083000030822     C* GVA
083100030822     C                   IF        %subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):1:3)
083400030822     C                             = 'GVA'
083500030822     C                   EVAL      VABGVA=%trim(%subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083800030822     C                   ENDIF
083900030822     C* TC1
084000030822     C                   IF        %subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):1:3)
084300030822     C                             = 'TC1'
084400030822     C                   EVAL      VABTC1=%trim(%subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084700030822     C                   ENDIF
084800030822     C* TC2
084900030822     C                   IF        %subst(
085000030822     C                             %subst(vlrppt:posDaDft+1:
085100030822     C                             posADft-posDaDft-1):1:3)
085200030822     C                             = 'TC2'
085300030822     C                   EVAL      VABTC2=%trim(%subst(
085400030822     C                             %subst(vlrppt:posDaDft+1:
085500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085600030822     C                   ENDIF
085700040714     C* VATTRC
085800040714     C                   IF        %subst(
085900040714     C                             %subst(vlrppt:posDaDft+1:
086000040714     C                             posADft-posDaDft-1):1:3)
086100040714     C                             = 'TRC'
086200040714     C                   EVAL      VATTRC=%trim(%subst(
086300040714     C                             %subst(vlrppt:posDaDft+1:
086400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086500040714     C                   ENDIF
086600030822     C* ...
086700030822     C                   ENDIF
086800030822     C                   ENDDO
086900020204     C*
087000010330     C                   ENDSR
087100010607     C*----------------------------------------------------*
087200110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
087300010607     C*----------------------------------------------------*
087400010607     C     IMPVAB        BEGSR
087500010607     C*
087600010607     C                   SETOFF                                       3132
087700110315 xxx C***                SETON                                        35
087800030515     C*
087900010607     C                   MOVEL     datcor        VABAAS
088000010607     C                   MOVE      datcor        VABMGS
088100010607     C*
088200030715     C**********
088300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088400030715     C**********
088500030715     C                   IF        wGiro = *zeros
088600040113     C*
088700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
089000080429     C                             %trim(intcol(5))<>*blanks
089100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
089300080429     C                                      %trim(intcol(5))
089400070823     C     ';':CharCSV   XLATE     vindta        vindta
089500040510     C                   ENDIF
089600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089700031201     C     minu:maiu     XLATE     vindta        vindta
089800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089900030715     C                   DOW       posDa <= %len(%trim(vindta))
090000030715     C*
090100030715     C* Gestisco il 1° campo
090200030715     C                   IF        i = 1
090300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090400030715     C                   EVAL      posDa = 1
090500030715     C* Eseguo lo scan x trovare la fine del primo campo
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C* X i campi successivi al 1°
091100030715     C                   ELSE
091200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091300030715     C                   EVAL      posDa = posA + 1
091400030715     C* Eseguo lo scan x trovare la fine del campo corrente
091500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091600030715     C* Gestisco l'ultimo campo
091700030715     C                   IF        posA = *zeros
091800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   LEAVE
092300030715     C                   ELSE
092400030715     C* A questo punto "estraggo" il campo corrente
092500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092600030715     C                                                    (posA-posDa))
092700030715     C                   ENDIF
092800030715     C                   ENDIF
092900030715     C* Incremento il contatore d campo
093000030715     C                   EVAL      i = i +1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093400030715     C* delimitazione testo
093500030715     C                   EVAL      i = 1
093600030715     C                   DOW       i < %elem(SkSplitFLD)
093700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093900030715     C                   EVAL      i = i + 1
094000030715     C                   ENDDO
094100030715     C                   EVAL      wGiro = 1
094200030715     C                   ELSE
094300030715     C                   EVAL      wGiro = 2
094400031204     C**********
094500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094600031204     C* d separatore campo
094700031204     C**********
094800031204     C                   EVAL      vindta = %trim(vindta)
094900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
095000031204     C                   EVAL      vindta = %subst(vindta:2)
095100031204     C                   ENDDO
095200031204     C*
095300031204     C                   Z-ADD     *zeros        lunghInput        4 0
095400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095800110912     C                   ENDDO
095900030715     C**********
096000030715     C* Effettuo lo split del campo dati d input
096100030715     C**********
096200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096300030715     C                   DOW       posDa <= %len(%trim(vindta))
096400030715     C*
096500030715     C* Gestisco il 1° campo
096600030715     C                   IF        i = 1
096700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096800030715     C                   EVAL      posDa = 1
096900030715     C* Eseguo lo scan x trovare la fine del primo campo
097000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097100030715     C* A questo punto "estraggo" il campo corrente
097200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097300030715     C                                                    (posA-posDa))
097400030715     C* X i campi successivi al 1°
097500030715     C                   ELSE
097600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097700030715     C                   EVAL      posDa = posA + 1
097800031204     C* Verifico che nn vi sia il campo nn valorizzato
097900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
098000031204     C* Se campo nn valorizzato skippo al prossimo
098100031204     C                   EVAL      SkSplitCSV(i) = *blanks
098200031204     C                   EVAL      posA  = posA + 1
098300031204     C                   ELSE
098400030715     C* Eseguo lo scan x trovare la fine del campo corrente
098500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098600030715     C* Gestisco l'ultimo campo
098700030715     C                   IF        posA = *zeros
098800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098900030715     C* A questo punto "estraggo" il campo corrente
099000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099100030715     C                                                    (posA-posDa))
099200030715     C                   LEAVE
099300030715     C                   ELSE
099400030715     C* A questo punto "estraggo" il campo corrente
099500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099600030715     C                                                    (posA-posDa))
099700030715     C                   ENDIF
099800031204     C                   ENDIF
099900030715     C                   ENDIF
100000030715     C* Incremento il contatore d campo
100100030715     C                   EVAL      i = i +1
100200030715     C                   ENDDO
100300030715     C*
100400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100500030715     C* delimitazione testo
100600030715     C                   EVAL      i = 1
100700030715     C                   DOW       i < %elem(SkSplitCSV)
100800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
101000030715     C                   EVAL      i = i + 1
101100030715     C                   ENDDO
101200100720     C*
101300100720     C* Stacco un progressivo x numero spedizione
101400110930 xxx C                   EXSR      repNSP
101500100326     C*
101600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101700030715     C                   EVAL      i = 1
101800030715     C                   DOW       i < %elem(SkSplitFLD)
101900030715     C*
102000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102200030715     C                             SkSplitCSV(i) <> *blanks
102300030721     C*** ATB
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102500030721     C                   EVAL      VABATB = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030715     C*** RSD
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
103000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
103200030715     C* ==
103300030715     C                   ENDIF
103400030721     C*** RD2
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030715     C*** IND
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
104000030715     C                   EVAL      VABIND = SkSplitCSV(i)
104100030715     C                   ENDIF
104200030715     C*** LOD
104300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
104400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104500030715     C                   ENDIF
104600030715     C*** PRD
104700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
104800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104900030715     C                   ENDIF
105000030721     C*** NZD
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
105200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030715     C*** RMA
105500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105700030715     C                   ENDIF
105800030715     C*** NOT
105900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
106000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
106100030715     C                   ENDIF
106200030715     C*** NT2
106300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
106400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106500030715     C                   ENDIF
106600030715     C*** VCA
106700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
106800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106900030715     C                   ENDIF
107000030715     C*** VAS
107100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
107200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
107300030715     C                   ENDIF
107400030715     C*** FFD
107500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107700030715     C                   ENDIF
107800030721     C*** GC1
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
108000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** GC2
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
108400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** TSP
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
108800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108900030721     C                   ENDIF
109000040510     C*** CBO
109100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
109200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
109300040510     C                   ENDIF
109400030721     C*** NAS
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** TIC
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
110000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** GCA
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
110400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** XCO
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
110800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** CTM
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
111200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** TCR
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** CTS
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
112000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** FTM
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
112400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*** VAD
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
112800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112900030721     C                   ENDIF
113000030721     C*** GMA
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
113200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
113300030721     C                   ENDIF
113400030721     C*** GGA
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113700030721     C                   ENDIF
113800030721     C*** GVA
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
114000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
114100030721     C                   ENDIF
114200030721     C*** TC1
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
114400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114500030721     C                   ENDIF
114600030721     C*** TC2
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
114800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114900030721     C                   ENDIF
115000030721     C*** SCL
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
115200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
115300030721     C                   ENDIF
115400030721     C*** RMO
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115700030721     C                   ENDIF
115800030721     C*** NMO
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
116000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
116100030721     C                   ENDIF
116200030721     C*
116300030721     C* Reperisco quindi i campi numerici...
116400040510     C*** NSP
116500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116600040510     C                   EVAL      PiStr=SkSplitCSV(i)
116700040510     C                   EXSR      CHKNUM
116800040510     C                   IF        PiInt=*on
116900040510     C                   Z-ADD     PiVal         VABNSP
117000040510     C                   ELSE
117100040510     C                   SETON                                        31
117200040510     C                   EVAL      VABNSP = *zeros
117300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
117400040510     C                             + ' ' + 'VABNSP'
117500040510     C                   ENDIF
117600040510     C                   ENDIF
117700071106     C*** RMN
117800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117900071106     C                   EVAL      PiStr=SkSplitCSV(i)
118000071106     C                   EXSR      CHKNUM
118100071106     C                   IF        PiInt=*on
118200071106     C                   Z-ADD     PiVal         VABRMN
118300071106     C                   ELSE
118400071106     C                   SETON                                        32
118500071106     C                   EVAL      VABRMN = *zeros
118600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118700071106     C                             + ' ' + 'VABRMN'
118800071106     C                   ENDIF
118900071106     C                   ENDIF
119000030721     C*** CAD
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on
119500030721     C                   Z-ADD     PiVal         Num5_0
119600030721     C                   MOVEL(p)  Num5_0        VABCAD
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCAD = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABCAD'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** NCL
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on
120900030721     C                   Z-ADD     PiVal         VABNCL
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABNCL = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400040122     C                             + ' ' + 'VABNCL'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** PKB
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiNum=*on
122200030721     C                   Z-ADD     PiVal         VABPKB
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABPKB = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABPKB'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CAS
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
123200141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
123300141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
123400141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
123500141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
123600141007     C                             %trim(SkSplitCSV(i)) <> ',00'
123700141007     C                   EVAL      VABVCA = 'EUR'
123800101018     C                   EVAL      wFlgCAS = '1'
123900101018     C                   ENDIF
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABCAS
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      VABCAS = *zeros
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABCAS'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** CCM
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on      AND
125600030721     C                             PiVal<=9999999 AND
125700030721     C                             PiVal>*zeros
125800030721     C                   Z-ADD     PiVal         VABCCM
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      VABCCM = *zeros
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300040510     C                             + ' ' + 'VABCCM'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** LNP
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on    AND
127100030721     C                             PiVal<=999   AND
127200030721     C                             PiVal>*zeros
127300030721     C                   Z-ADD     PiVal         VABLNP
127400030721     C                   ELSE
127500040715     C                   SETON                                        32
127600030721     C                   EVAL      VABLNP = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800040510     C                             + ' ' + 'VABLNP'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** NRS
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on AND
128600030721     C                             PiVal<=99
128700030721     C                   Z-ADD     PiVal         VABNRS
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABNRS'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** LNA
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on    AND
129900030721     C                             PiVal<=999   AND
130000030721     C                             PiVal>*zeros
130100030721     C                   Z-ADD     PiVal         VABLNA
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      VABLNA = *zeros
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABLNA'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** CTR
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on  AND
131400030721     C                             PiVal<=999
131500030721     C                   Z-ADD     PiVal         VABCTR
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABCTR'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** IAS
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
132400141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
132500141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
132600141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
132700141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132800141007     C                             %trim(SkSplitCSV(i)) <> ',00'
132900141007     C                   EVAL      VABVAS = 'EUR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiNum=*on
133300030721     C                   Z-ADD     PiVal         VABIAS
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABIAS'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000141007     C                   ENDIF
134100030721     C*** VLB
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVLB
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVLB'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** QFT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABQFT
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABQFT'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** NCD
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on      AND
137000030721     C                             PiVal<=9999999
137100030721     C                   Z-ADD     PiVal         VABNCD
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABNCD'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** NCA
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on      AND
138300030721     C                             PiVal<=9999999
138400030721     C                   Z-ADD     PiVal         VABNCA
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABNCA'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** ZNC
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on AND
139600030721     C                             PiVal<=99
139700030721     C                   Z-ADD     PiVal         VABZNC
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABZNC'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** DCR
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on       AND
140900030721     C                             PiVal<=99999999
141000030721     C                   Z-ADD     PiVal         VABDCR
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABDCR'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** HCR
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiInt=*on   AND
142200030721     C                             PiVal<=9999
142300030721     C                   Z-ADD     PiVal         VABHCR
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142700030721     C                             + ' ' + 'VABHCR'
142800030721     C                   ENDIF
142900030721     C                   ENDIF
143000030721     C*** VMD
143100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
143200030721     C                   EVAL      PiStr=SkSplitCSV(i)
143300030721     C                   EXSR      CHKNUM
143400030721     C                   IF        PiNum=*on
143500030721     C                   Z-ADD     PiVal         VABVMD
143600030721     C                   ELSE
143700030721     C                   SETON                                        32
143800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143900030721     C                             + ' ' + 'VABVMD'
144000030721     C                   ENDIF
144100030721     C                   ENDIF
144200030721     C*** ANT
144300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
144400030721     C                   EVAL      PiStr=SkSplitCSV(i)
144500030721     C                   EXSR      CHKNUM
144600030721     C                   IF        PiInt=*on        AND
144700030721     C                             PiVal<=999999999
144800030721     C                   Z-ADD     PiVal         VABANT
144900030721     C                   ELSE
145000030721     C                   SETON                                        32
145100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145200030721     C                             + ' ' + 'VABANT'
145300030721     C                   ENDIF
145400030721     C                   ENDIF
145500030721     C*** CMO
145600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145700030721     C                   EVAL      PiStr=SkSplitCSV(i)
145800030721     C                   EXSR      CHKNUM
145900030721     C                   IF        PiInt=*on
146000030721     C                   Z-ADD     PiVal         Num5_0
146100030721     C                   MOVEL(p)  Num5_0        VABCMO
146200030721     C                   ELSE
146300030721     C                   SETON                                        32
146400030721     C                   EVAL      VABCMO = *zeros
146500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146600030721     C                             + ' ' + 'VABCMO'
146700030721     C                   ENDIF
146800030721     C                   ENDIF
146900040510     C*
147000110930     C* Reperisco i campi del EDIVAT
147100040510     C*** VATNOT
147200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
147300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
147400040510     C                   ENDIF
147500040510     C*** VATTRC
147600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147800040510     C                   ENDIF
147900060331     C*** VATNOT_A
148000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
148100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
148200060331     C                   ENDIF
148300060331     C*** VATNOT_B
148400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
148500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148600060331     C                   ENDIF
148700060331     C*** VATNOT_E
148800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
149000060331     C                   ENDIF
149100110927     C*** VATNOT_I
149200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
149300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
149400110927     C                   ENDIF
149500141007     C*** VATNOT_IJ
149600141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
149700141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149800141007     C                   ENDIF
149900141007     C*** VATNOT_S
150000141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
150100141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
150200141007     C                   ENDIF
150300070823     C*
150400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150500070925     C*
150600150601     C* TC1
150700150601     C* se nella stessa cella di VABNOT c'è il valore testato, allora valorizzo gli appuntamenti
150800150601     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
150900150601     C                   IF        %trim(SkSplitCSV(i)) =
151000150601     C                             'Consegna su Appuntamento Telefonico'
151100150601     C                   EVAL      VABTC1 = 'A'
151200150601     C                   ENDIF
151300150601     C                   ENDIF
151400130403
151500130403     C*
151600071029     C***  <===  -----------------------------------------
151700040510     C*
151800030715     C                   ENDIF
151900030715     C                   EVAL      i = i + 1
152000030715     C                   ENDDO
152100070925     C*
152200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152300121030     C*
152400070925     C*
152500071029     C***  <===  -----------------------------------------
152600070925     C*
152700070925     C* Considerazioni finali su CBO/CAS
152800100318     C                   IF        wFlgCAS = '1'    OR
152900100318     C                             VABCAS  > *zeros
153000130403     C                   IF        VABVCA = *blanks
153100130403     C                   EVAL      VABVCA = 'EUR'
153200130403     C                   EVAL      VABTIC = *blanks
153300130403     C                   ENDIF
153400070925     C                   IF        VABCBO = '1'
153500070925     C                   EVAL      VABCBO = '4'
153600070925     C                   ENDIF
153700070925     C                   IF        VABCBO = '2'
153800070925     C                   EVAL      VABCBO = '6'
153900070925     C                   ENDIF
154000130403     C                   ELSE
154100130403     C                   EVAL      vabVCA = *blank
154200070925     C                   ENDIF
154300070925     C***  <===  ----------------------------
154400040510     C*
154500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154600040510     C                   IF        VABNSP = *zeros
154700040510     C                   SETON                                        31
154800040510     C                   ENDIF
154900040715     C*
155000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155100040715     C                   IF        VABLNP = *zeros
155200040715     C                   SETON                                        31
155300040715     C                   ENDIF
155400040510     C*
155500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155600040510     C                   IF        VABRMN = *zeros
155700040510     C                   Z-ADD     VABNSP        VABRMN
155800040510     C                   ENDIF
155900040510     C*
156000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156100040510     C                   IF        VABRMA = *blanks
156200040714     C                   MOVEL     VABRMN        VABRMA
156300040510     C                   ENDIF
156400040128     C*
156500040510     C* Se provincia nn valorizzata la reperisco
156600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156700110912     C                   IF        VABPRD =  *blanks AND
156800110912     C                             VABNZD =  *blanks AND
156900110912     C                             VABCAD <> *blanks AND
157000110912     C                             VABLOD <> *blanks
157100040128     C                   CLEAR                   TISI95DS
157200040128     C                   EVAL      I95TCN = '3'
157300040128     C                   Z-ADD     datcor        I95DAT
157400040128     C                   EVAL      I95NAR = VABNZD
157500040128     C                   EVAL      I95CAP = VABCAD
157600040128     C                   EVAL      I95LOC = VABLOD
157700040128     C                   CALL      'TISI95R'
157800040128     C                   PARM                    TISI95DS
157900040128     C                   EVAL      VABPRD = O95PRV
158000040510     C                   ENDIF
158100020204     C*
158200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158300020204     C                   EXSR      CHKIMPDIV
158400030715     C                   ENDIF
158500020204     C*
158600010607     C                   ENDSR
158700010607     C*----------------------------------------------------*
158800010601
158900020204
159000020204     C*----------------------------------------------------*
159100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159200020204     C*----------------------------------------------------*
159300020204     C     CHKIMPDIV     BEGSR
159400020204     C*
159500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159600020204     C                   Z-ADD     *zeros        wrkDec            9 9
159700020204     C*
159800020204     C* Come prima cosa effettuo considerazioni sulla divisa
159900020204     C                   IF        vabIAS > *zeros
160000020204     C                   IF        vabVAS <> 'EUR'
160100020204     C                   EVAL      vabVAS =  'ITL'
160200020204     C                   ENDIF
160300020204     C                   ENDIF
160400020204     C*
160500020204     C                   IF        vabCAS > *zeros
160600020204     C                   IF        vabVCA <> 'EUR'
160700020204     C                   EVAL      vabVCA =  'ITL'
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C                   IF        vabVMD > *zeros
161200020204     C                   IF        vabVAD <> 'EUR'
161300020204     C                   EVAL      vabVAD =  'ITL'
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161800020204     C                   Z-ADD     vabIAS        wrkDec
161900020204     C                   IF        wrkDec > *zeros
162000020204     C                   IF        vabVAS = 'ITL'
162100020204     C                   EVAL      vabIAS = *zeros
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162600020204     C                   Z-ADD     vabCAS        wrkDec
162700020204     C                   IF        wrkDec > *zeros
162800020204     C                   IF        vabVCA = 'ITL'
162900020204     C                   EVAL      vabCAS = *zeros
163000020204     C                   ENDIF
163100020204     C                   ENDIF
163200020204     C*
163300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163400020204     C                   Z-ADD     vabVMD        wrkDec
163500020204     C                   IF        wrkDec > *zeros
163600020204     C                   IF        vabVAD = 'ITL'
163700020204     C                   EVAL      vabVMD = *zeros
163800020204     C                   ENDIF
163900020204     C                   ENDIF
164000020204     C*
164100020204     C                   ENDSR
164200020204     C***
164300010330
164400100318
164500100318
164600100318     C*----------------------------------------------------*
164700100318     C*  CONTROLLO NUMERICITA' CAMPI
164800100318     C*----------------------------------------------------*
164900100318     C     CHKNUM        BEGSR
165000100318     C*
165100100318     C                   IF        PiDecChr = *blanks
165200100318     C                   EVAL      PiDecChr = CharNUM
165300100318     C                   ENDIF
165400100318     C*
165500100318     C                   callp     UBISNUM_Check(PiStr
165600100318     C                                          :PiDecChr
165700100318     C                                          :PiVal
165800100318     C                                          :PiNum
165900100318     C                                          :PiInt)
166000100318     C*
166100100318     C                   ENDSR
166200100318     C***
166300010601
166400010601
166500010601
166600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166700010601     C     repfil        BEGSR
166800010601     C*
166900010601     C                   if        invfil = *zeros and
167000010601     C                             depfil > *zeros and
167100010629     C                             (vinflg = *blanks or
167200010629     C                              vinflg = *zeros)
167300010601     C
167400010601     C                   eval      invfil = depfil
167500010601     C                   endif
167600010601     C*
167700010601     C                   if        depfil <> invfil and
167800010601     C                             invfil > *zeros
167900010601     C                   eval      flgMulti = '1'
168000010601     C                   if        vinflg = *blanks
168100010601     C                   add       1             cntNonEl
168200010601     C                   endif
168300010601     C                   endif
168400010601     C*
168500010601     C                   if        vinflg = '2'
168600010601     C                   eval      flgStato = '2'
168700010601     C                   endif
168800010601     C*
168900010601     C                   ENDSR
169000010601     C***
169100010601
169200010601
169300010601
169400010330
169500040119      /TITLE Invio dei dati al punto operativo.
169600040119     C     invio         BEGSR
169700040119     C*
169800110930     C* 1° invio EDIVAT
169900040119     C                   reset                   dscmz
170000040119     C                   move      vlrpoi        cmzdst
170100110930     C                   eval      cmzfld = 'EDIVATWR'
170200040119     C                   eval      cmzmbd = vlrhdl
170300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170400040119     C***                if        prmfir = *blanks
170500110930     C                   eval      cmzfla = 'EDIVAT0F'
170600110930     C                   eval      cmzmba = 'EDIVAT0F'
170700040119     C***                else
170800040119     C***                eval      cmzfla = prmfir
170900040119     C***                eval      cmzmba = prmfir
171000040119     C***                endif
171100040119     C                   eval      cmznrr = *zeros
171200040119     C                   move      §ctrokvt      cmznrr
171300040119     C                   eval      cmzlba = vlrfl1
171400040119     C                   call(e)   'TIS711C'
171500040119     C                   parm                    dscmz
171600040119     C                   parm      *blanks       esito
171700040119     C                   if        %error
171800040119     C                             or cmzerr = '1'
171900040119     C                             or esito  = '1'
172000040119     C                   eval      wrkesito = '3'
172100040119     C                   else
172200040119     C*
172300110930     C* 2° invio EDIVAB
172400040119     C                   reset                   dscmz
172500040119     C                   move      vlrpoi        cmzdst
172600040119     C                   eval      cmzfld = vlrfou
172700040119     C                   eval      cmzmbd = vlrhdl
172800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172900040119     C***                if        prmfir = *blanks
173000110930     C                   eval      cmzfla = 'EDIVAB0F'
173100110930     C                   eval      cmzmba = 'EDIVAB0F'
173200040119     C***                else
173300040119     C***                eval      cmzfla = prmfir
173400040119     C***                eval      cmzmba = prmfir
173500040119     C***                endif
173600040119     C                   eval      cmznrr = *zeros
173700040119     C                   move      §ctrokvb      cmznrr
173800040119     C                   eval      cmzlba = vlrfl1
173900040119     C                   call(e)   'TIS711C'
174000040119     C                   parm                    dscmz
174100040119     C                   parm      *blanks       esito
174200040119     C                   if        %error
174300040119     C                             or cmzerr = '1'
174400040119     C                             or esito  = '1'
174500040119     C                   eval      wrkesito = '3'
174600040119     C                   endif
174700040119     C                   endif
174800040119     C*
174900040119     C                   ENDSR
175000040119     C***
175100010601
175200010601
175300010601
175400010601
175500010601      /TITLE Invio dei dati al punto operativo.
175600010601     C     opeini        BEGSR
175700010601     C*
175800010601     C* Inizializzo flag e contatori operativi
175900010601     C                   movel     '0'           flgGiro           1
176000010601     C                   movel     '0'           flgMulti          1
176100010601     C                   movel     '1'           flgStato          1
176200010615     C                   movel     '0'           flgOk             1
176300010601     C                   z-add     *zeros        cntNonEl         10 0
176400010601     C                   z-add     *zeros        depfil            3 0
176500010601     C                   z-add     *zeros        invfil            3 0
176600010601     C*
176700010601     C                   ENDSR
176800010601     C***
176900070326
177000070326
177100070326
177200070326
177300070326     C     *pssr         BEGSR
177400070329     C*
177500070329     C                   if        %open(tivin00r)
177600070329     C                   close     tivin00r
177700070329     C                   endif
177800110930     C                   if        %open(edivabwr)
177900110930     C                   close     edivabwr
178000070329     C                   endif
178100110930     C                   if        %open(edivatwr)
178200110930     C                   close     edivatwr
178300070329     C                   endif
178400070326     C*
178500070326     C* Effettuo la chiamata al CLLE preposto
178600110930     C                   call(e)   'TITVEVTC'
178700070326     C                   parm                    parccm
178800070326     C                   parm                    parmbr
178900070326     C                   parm      '2'           paropz
179000070326     C*
179100070326     C                   eval      wrkesito = '2'
179200070404     C*
179300070404     C                   seton                                        LR
179400070326     C*
179500070326     C                   ENDSR     '*CANCL'
179600070326     C***
179700100318
179800100318
179900100318
180000100318     C     normCSV       BEGSR
180100100318     C*
180200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180300100318     C* separatori campi
180400100318     C                   if        CharTXT <> *blanks
180500100318     C                   z-add     *zeros        wLenVINDTA        4 0
180600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180700100318     C                   setoff                                       80
180800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181100100318     C                   if        not *in80
181200100318     C                   seton                                        80
181300100318     C                   else
181400100318     C                   setoff                                       80
181500100318     C                   endif
181600100318     C                   endif
181700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181800100318     C                             and *in80 = *on
181900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182000100318     C                   endif
182100100318     C                   enddo
182200100318     C                   endif
182300100318     C*
182400100318     C                   ENDSR
182500100318     C***
182600070326
182700100326
182800100326
182900100326     C*----------------------------------------------------*
183000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183100100326     C*----------------------------------------------------*
183200100326     C     REPNSP        BEGSR
183300100326     C*
183400100326     C* NSP => Stacco un numeratore da AZNUM
183500100326     C                   clear                   TRUL33DS
183600100326     C                   eval      I33OPE = *zeros
183700100326     C                   eval      I33CNU = 302
183800100326     C                   eval      I33NUM = 1
183900100326     C                   movel     TRUL33DS      KPJBU
184000100326     C                   call      'TRUL33R'
184100100326     C                   parm                    KPJBA
184200100326     C                   movel     KPJBU         TRUL33DS
184300100326     C                   if        O33ERR = *zeros
184400100326     C                   z-add     O33NRF        VABNSP
184500100326     C                   z-add     O33NRF        VATNSP
184600100326     C                   else
184700100326     C                   SETON                                        31
184800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184900100326     C                             + ' ' + 'VABNSP VATNSP'
185000100326     C                   endif
185100100326     C*
185200100326     C                   ENDSR
185300010330
185400100326
185500010330
185600000613     C     *inzsr        BEGSR
185700990910     C*
185800990910     C     *entry        plist
185900990920     C                   parm                    tivlrds
186000990921     C                   parm      wrkesito      esito
186100000724     C                   parm                    prmlit
186200000710     C                   parm                    prmfir
186300010330     C*
186400010330     C* CALCOLA LA DATA CORRENTE
186500110930     C                   time                    wn14             14 0
186600110930     C                   movel     wn14          oracor            6 0          *ORA
186700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186800100318     C                   eval      datcor = %dec(%date() : *ISO)
186900000613     C*
187000000613     C                   ENDSR
187100000613     C***
187200990908
187300120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187400161003X;X;X;X;X;X;VABRMN;X;X;X;VABRMA;X;X;X;X;X;X;X;X;X;X;X;X;X;X;VABRSD;VABIND;X;VABCAD;VABLOD;
187500161003X;X;X;X;X;X;VABNCL;X;X;X;VABPKB;X;X;X;X;X;VABVLB;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
187600161003X;X;X;
187700141007
187800141007
