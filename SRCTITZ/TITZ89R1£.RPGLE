000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400130122      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600120209     F*
000700990910     Ftivin00r  uF   E             DISK    usropn
000800110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000900110930     FEDIVATwr  O    E             DISK    usropn
001000000313     D*
001100040113     D*------------
001200040113     D* SCHIEDE A PROGRAMMA
001300040113     D*------------
001400080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001500010330     D*----------------------------------------------------
001600010330     D* DICHIARAZIOINE VARIABILI DI WRK
001700010330     D*----------------------------------------------------
001800010330     D dscmz         e ds                  inz
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200040128     D tisi95ds      e ds
002300110930     D edivabs       e ds                  extname(edivab0f)
002400110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002500110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
002600010330     D esito           s              1
002700010330     D prmlit          s             10
002800010330     D prmfir          s             10
002900010330     D wrkesito        s                   like(esito)
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100040510     D parccm          s              8    INZ(*blanks)
003200040119     D parmbr          s             10    INZ(*blanks)
003300040119     D paropz          s              1    INZ(*blanks)
003400040119     D chkcall         s              1    INZ(*blanks)
003500080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003600080418     D wFlgCAS         s              1    INZ(*blanks)
003700121030     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003800121030     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
003900141007     D Intestazioni    s               n   inz(*on)
004000120203     D wNomeFile       s             30    INZ(*blanks)
004100100326     D*------------------
004200100326     D* DS REPERIMENTO NUMERATORE
004300100326     D*------------------
004400100326     D trul33ds      e ds                  inz
004500100326     D*------------------
004600100326     D* DS ARCHITETTURA
004700100326     D*------------------
004800100326     D kpjba         e ds                  inz
004900100326
005000030715     D*------------------
005100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005200030715     D*------------------
005300030715     D SkSplitFLD      S             10    DIM(100)
005400030715     D SkSplitCSV      S            256    DIM(100)
005500030715     D CharCSV         S              1
005600030715     D CharTXT         S              1
005700030715     D CharNUM         S              1
005800100318     D CharSOS         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300070823     D wVATNOT_A       s                   LIKE(VATNOT)
006400060331     D wVATNOT_B       s                   LIKE(VATNOT)
006500060331     D wVATNOT_E       s                   LIKE(VATNOT)
006600110927     D wVATNOT_I       s                   LIKE(VATNOT)
006700120202     D wVATNOT_J       s                   LIKE(VATNOT)
006800120202     D wVATNOT_IJ      s             70
006900141007     D wVATNOT_S       s                   LIKE(VATNOT)
007000130128     D w70_1           s             70
007100130128     D w70_2           s             70
007200130128     D w70_3           s             70
007300130128     D w70_4           s             70
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100080418     D*------------------
008200080418     D* Costanti
008300080418     D*------------------
008400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600100318
008700100318     D*------------------
008800100318     D* LINKING A DEFINIZIONI ESTERNE
008900100318     D*------------------
009000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200100318
009300100909     D*------------------
009400100909     D* DS CALCOLO CHECK-DIGITS
009500100909     D*------------------
009600100909     D TRUL28DSX     e ds                  inz
009700010330
009800010330
009900010330
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300010601     C*
010400010601     C                   exsr      opeini
010500010605     C                   exsr      rwvab
010600010601     C*
010700010601     C                   seton                                        lr
010800010601
010900010601
011000010601
011100010601     C*--------------------------------------------------------
011200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011300010601     C*--------------------------------------------------------
011400010601     C     PREELA        BEGSR
011500010601     C*
011600010601     C* SE OCCORRE SPEDIRE IN FILIALE
011700010601     C                   if        invfil <> *zeros and
011800010601     C                             flgGiro = '0'
011900010601     C*
012000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012100010601     C                   eval      flgGiro = '1'
012200010601     C*
012300010601     C                   endif
012400010601     C*
012500010601     C                   ENDSR
012600010601     C***
012700010601
012800010601
012900010601
013000010601
013100010601     C*--------------------------------------------------------
013200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013300010601     C*--------------------------------------------------------
013400010601     C     ENDELA        BEGSR
013500040119     C*
013600040119     C* Effettuo la chiamata al CLLE preposto
013700110930     C                   call(e)   'TITVEVTC'
013800040510     C                   parm                    parccm
013900040119     C                   parm                    parmbr
014000040119     C                   parm      '2'           paropz
014100050201     C*
014200050201     C* Effettuo lancio TISI95 solo x chiusura
014300050201     C                   CLEAR                   TISI95DS
014400050201     C                   EVAL      I95TLA = 'C'
014500050201     C                   CALL      'TISI95R'
014600050201     C                   PARM                    TISI95DS
014700000616     C*
014800010601     C                   ENDSR
014900010601
015000010601
015100010330     C*--------------------------------------------------------
015200110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015300010330     C*--------------------------------------------------------
015400010605     C     RWVAB         BEGSR
015500010330     C*
015600010330     C                   if        not %open(tivin00r)
015700010330     C                   open      tivin00r
015800010330     C                   endif
015900110930     C                   if        not %open(edivabwr)
016000110930     C                   open      edivabwr
016100010330     C                   endif
016200040119     C*
016300110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016400040119     C                   exsr      prevat
016500040119     C*
016600040119     C                   if        chkcall = '0'
016700040119     C*
016800110930     C                   if        not %open(edivatwr)
016900110930     C                   open      edivatwr
017000040119     C                   endif
017100010330     C*
017200040119     C                   clear                   §CTROKVB          7 0
017300040119     C                   clear                   §CTROKVT          7 0
017400040119     C                   clear                   §CTRMO            7 0
017500040119     C                   clear                   §CTRNO            7 0
017600121030     C*
017700121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017800121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017900121030     C                   IF        Intestazioni = *off
018000121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018100121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018200121030     C                   exsr      inzvar
018300121030     C                   exsr      impvab
018400121030     C                   ENDIF
018500010330     C*
018600030822     C                   do        *HIVAL
018700010330     C*
018800030822     C                   read      tivin00r                               70
018900010618     C*
019000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019100010618     C                   if        vinflg = '1'
019200010618     C                   eval      flgOk = '1'
019300010618     C                   endif
019400040510     C*
019500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019600040510     C                   z-add     *zeros        wLenVINDTA        4 0
019700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020100040510     C                   z-add     1             wFlgVINDTA
020200040510     C                   leave
020300040510     C                   endif
020400040510     C                   enddo
020500010618     C*
020600040510     C                   if        vindta > *blanks AND
020700040510     C                             wFlgVINDTA = 1
020800010330     C                   add       1             rrnum
020900010330     C*
021000010601     C                   if        *in70 = *off and
021100010330     C                             (vinflg = *blanks
021200010330     C                              or vinflg = '0'
021300010330     C                              or vinflg = '2')
021400010330     C*
021500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021600010711     C                   if        vinflg = *blanks or vinflg = '0'
021700010711     C                   clear                   vinmsg
021800010711     C                   endif
021900010601     C*
022000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022100070925     C                   eval      vindta  = ' ' + %trim(vindta)
022200070925     C*
022300080527     C                   exsr      inzvar
022400100318     C                   exsr      normCSV
022500010605     C                   exsr      impvab
022600010601     C*
022700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022800010605     C                   eval      depfil = VABLNP
022900010601     C                   exsr      repfil
023000010601     C                   if        depfil = invfil
023100021025     C                   if        vlrpoi = 999
023200030822     C                   move(P)   invfil        VABFGS
023300021025     C                   else
023400030822     C                   move(P)   vlrpoi        VABFGS
023500021025     C                   endif
023600010601     C*
023700010601     C                   exsr      PREELA
023800010601     C*
023900010604     C* Ebbene...
024000010604     C*
024100040119     C  N31              add       1             §CTROKVB
024200040119     C   32              add       1             §CTRMO
024300040119     C   31              add       1             §CTRNO
024400080418     C*
024500030822     C                   if        wGiro = 2
024600080513     C* Scrivo sempre il VAT
024700080513     C  N31              exsr      wrivat
024800110201     C                   exsr      chkvab
024900080513     C                   endif
025000010604     C*
025100010604     C                   if        *in31 = *off and
025200010604     C                             *in32 = *off
025300010604     C                   eval      vinflg = '1'
025400010604     C                   else
025500010604     C                   eval      vinflg = '2'
025600010604     C                   endif
025700010604     C                   endif
025800010601     C*
025900010604     C                   endif
026000010604     C*
026100010330     C                   else
026200010330     C                   eval      vinflg = '1'
026300010330     C                   endif
026400010601     C*
026500010601     C  N70              update    tivin000
026600010330     C*
026700030822     C  N70              enddo
026800040119     C*
026900080527     C* Verifico dati rimasti "in cannna"...
027000080527     C                   seton                                        65
027100110201     C                   exsr      chkvab
027200080527     C*
027300040119     C                   endif                                                  (endif - chkcall)
027400010601     C*
027500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027600010601     C                   if        cntNonEl = *zeros or
027700010601     C                             flgMulti = '0'
027800010330     C* Se non ci sono record con errori ...
027900010601     C                   if        §ctrno = 0 and
028000010604     C                             §ctrmo = 0 and
028100010601     C                             flgStato <> '2'
028200010330     C* ... restituisco esito OK.
028300010330     C                   eval      wrkesito = '0'
028400010330     C                   else
028500040119     C                   if        §ctrokvb > 0
028600010330     C                   eval      wrkesito = '1'
028700010330     C                   else
028800010615     C                   if        flgOk = '0'
028900010615     C                   eval      wrkesito = '2'
029000010615     C                   else
029100010615     C                   eval      wrkesito = '6'
029200010615     C                   endif
029300010330     C                   endif
029400010330     C                   endif
029500010601     C                   else
029600010601     C                   eval      wrkesito = '9'
029700010601     C                   endif
029800010330     C*
029900010330     C                   if        %open(tivin00r)
030000010330     C                   close     tivin00r
030100010330     C                   endif
030200110930     C                   if        %open(edivabwr)
030300110930     C                   close     edivabwr
030400010330     C                   endif
030500110930     C                   if        %open(edivatwr)
030600110930     C                   close     edivatwr
030700040512     C                   endif
030800010601     C*
030900010601     C                   if        vlrpoi <> 999
031000010601     C                   eval      invfil = vlrpoi
031100010601     C                   endif
031200010330     C*
031300040119     C                   if        §ctrokvb > 0
031400010601     C                             and invfil > *zeros
031500010330     C                   exsr      invio
031600010330     C                   endif
031700010601     C*
031800010618     C                   if        flgGiro = '1'
031900010601     C                   exsr      endela
032000010618     C                   endif
032100010330     C*
032200010330     C                   ENDSR
032300010330     C***
032400080527
032500080527
032600080527
032700080527     C*----------------------------------------------------*
032800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032900080527     C*----------------------------------------------------*
033000100413     C     CHKVAB        BEGSR
033100110201     C*
033200110201     C* Se richiesti ignorare colli / peso / volume
033300110315 xxx C***                if        *in35
033400120203 xxx C***                z-add     *zeros        VABNCL              F0461
033500120203 xxx C***                z-add     *zF046190081  VABPKB
033600110315 xxx C***                z-add     *zeros        VABVLB
033700110315 xxx C***                endif
033800080527     C*
033900080527     C* Scrivo il VAB solo a rottura d spedizione
034000080527     C                   if        depspe = *zeros
034100110930     C  N31              eval      edivabs_sav = edivabs
034200080527     C                   eval      depspe = vabNSP
034300080527     C                   else
034400080527     C                   if        depspe = vabNSP AND *in65 = *off
034500100413     C   51              add       VABNCL        sav_VABNCL
034600100413     C   51              add       VABPKB        sav_VABPKB
034700100413     C   51              add       VABVLB        sav_VABVLB
034800100720     C   52              z-add     VABNCL        sav_VABNCL
034900100720     C   52              z-add     VABPKB        sav_VABPKB
035000100720     C   52              z-add     VABVLB        sav_VABVLB
035100100720     C   53              add       VABNCL        sav_VABNCL
035200100720     C   53              z-add     VABPKB        sav_VABPKB
035300100720     C   53              z-add     VABVLB        sav_VABVLB
035400080527     C                   else
035500120203     C  N31              eval      edivabs_ok = edivabs_sav
035600080527     C  N31              exsr      wrivab
035700110930     C  N31              eval      edivabs_sav = edivabs
035800080527     C                   eval      depspe = vabNSP
035900080527     C                   endif
036000080527     C                   endif
036100080527     C*
036200080527     C                   ENDSR
036300040119
036400040119
036500040119
036600040119     C*----------------------------------------------------*
036700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036800040119     C*----------------------------------------------------*
036900040510     C     WRIVAB        BEGSR
037000080513     C*
037100120203     C* EDIVABWR ha il prefisso "ok_"
037200120203     C*
037300120203     C* Gestisco l'eventuale rottura x numero spedizione
037400080527     C                   if        VABNSP <> depspe or *in65 = *on
037500070301     C                   if        wVATNOT_E <> *blanks
037600120203     C                   eval      ok_VABCTM = '7Q'
037700070301     C                   endif
037800110930     C*
037900110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038000120203     C                   MOVE(P)   vlrMSG        wNomeFile
038100130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
038200120203     C                   EVAL      ok_VABDCM = DATCOR
038300120203     C                   EVAL      ok_VABDTS = DATCOR
038400120203     C                   EVAL      ok_VABHMS = ORACOR
038500120203     C                   EVAL      ok_VABCNT = 1
038600110930     C*
038700110930     C                   write     edivab00
038800080513     C                   endif
038900040119     C*
039000040119     C                   ENDSR
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAT        BEGSR
039800060512     C*
039900060512     C* Inizializzo i campi chiave
040000060512     C                   eval      VATFGS = VABFGS
040100060512     C                   eval      VATAAS = VABAAS
040200060512     C                   eval      VATCCM = VABCCM
040300070823     C                   eval      VATNRS = VABNRS
040400060512     C                   eval      VATNSP = VABNSP
040500060512     C                   eval      VATLNP = VABLNP
040600040119     C*
040700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040800040510     C                   if        VATTRC = *blanks
040900040510     C                   eval      VATTRC = 'E'
041000040510     C                   endif
041100110318     C                   if        VATNOT <> *blanks
041200120203     C                   MOVE(P)   vlrMSG        wNomeFile
041300130122     C***                EVAL      VATCMR = wNomeFile
041400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
041500110930     C                   eval      VATCNT = 1
041600110930     C                   write     EDIVAT00
041700040119     C                   add       1             §CTROKVT
041800110318     C                   endif
041900060331     C*
042000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042100060331     C                   if        wVATNOT_A <> *blanks
042200060331     C                   eval      VATNOT = wVATNOT_A
042300060331     C                   eval      VATTRC = 'A'
042400120203     C                   MOVE(P)   vlrMSG        wNomeFile
042500130122     C***                EVAL      VATCMR = wNomeFile
042600141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
042700110930     C                   eval      VATCNT = 1
042800110930     C                   write     EDIVAT00
042900060331     C                   add       1             §CTROKVT
043000060331     C                   endif
043100060331     C*
043200060331     C                   if        wVATNOT_B <> *blanks
043300060331     C                   eval      VATNOT = wVATNOT_B
043400060331     C                   eval      VATTRC = 'B'
043500120203     C                   MOVE(P)   vlrMSG        wNomeFile
043600130122     C***                EVAL      VATCMR = wNomeFile
043700141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
043800110930     C                   eval      VATCNT = 1
043900110930     C                   write     EDIVAT00
044000060331     C                   add       1             §CTROKVT
044100060331     C                   endif
044200141007     C*
044300141007     C                   if        wVATNOT_IJ <> *blanks
044400141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044500141007     C                   eval      VATTRC = 'I'
044600141007     C***                EVAL      VATCMR = wNomeFile
044700141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044800141007     C                   eval      VATCNT = 1
044900141007     C                   write     EDIVAT00
045000141007     C                   add       1             §CTROKVT
045100141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045200141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045300141007     C                   eval      VATTRC = 'J'
045400141007     C***                EVAL      VATCMR = wNomeFile
045500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
045600141007     C                   eval      VATCNT = 1
045700141007     C                   write     EDIVAT00
045800141007     C                   endif
045900141007     C                   endif
046000060331     C*
046100060331     C                   if        wVATNOT_E <> *blanks
046200060331     C                   eval      VATTRC = 'E'
046300121030     C                   MOVE(P)   vlrMSG        wNomeFile
046400130122     C***                EVAL      VATCMR = wNomeFile
046500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
046600121030     C                   eval      VATCNT = 1
046700141007     C*
046800141007     C* NON Sviluppare i "CHI SONO"
046900141007     C*
047000141007     C                   eval      VATNOT = '00' + %trim(wVATNOT_E)
047100141007     C                   write     EDIVAT00
047200121030     C                   endif
047300110927     C*
047400110927     C                   if        wVATNOT_I <> *blanks
047500110927     C                   eval      VATNOT = wVATNOT_I
047600110927     C                   eval      VATTRC = 'I'
047700120203     C                   MOVE(P)   vlrMSG        wNomeFile
047800130122     C***                EVAL      VATCMR = wNomeFile
047900141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048000110930     C                   eval      VATCNT = 1
048100110930     C                   write     EDIVAT00
048200110927     C                   add       1             §CTROKVT
048300110927     C                   endif
048400120202     C*
048500120202     C                   if        wVATNOT_J <> *blanks
048600120202     C                   eval      VATNOT = wVATNOT_J
048700120202     C                   eval      VATTRC = 'J'
048800120203     C                   MOVE(P)   vlrMSG        wNomeFile
048900130122     C***                EVAL      VATCMR = wNomeFile
049000141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
049100120202     C                   eval      VATCNT = 1
049200120202     C                   write     EDIVAT00
049300120202     C                   add       1             §CTROKVT
049400120202     C                   endif
049500141007     C*
049600141007     C                   if        wVATNOT_S <> *blanks
049700141007     C                   eval      VATNOT = wVATNOT_S
049800141007     C* i 2 flag devono essere blank
049900141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
050000141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
050100141007     C                   eval      VATTRC = 'S'
050200141007     C***                EVAL      VATCMR = wNomeFile
050300141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
050400141007     C                   eval      VATCNT = 1
050500141007     C                   write     EDIVAT00
050600141007     C                   add       1             §CTROKVT
050700141007     C                   endif
050800040119     C*
050900040119     C                   ENDSR
051000040119
051100040119
051200040119
051300040119     C*----------------------------------------------------*
051400110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051500040119     C*----------------------------------------------------*
051600040119     C     PREVAT        BEGSR
051700040119     C*
051800110930     C* Compongo il nome del membro da dare al EDIVATWR
051900040119     C                   eval      parmbr = vlrhdl
052000040119     C                   movel     'M'           parmbr
052100040510     C                   eval      parccm = vlrksc
052200040119     C                   eval      paropz = '1'
052300040119     C* Effettuo la chiamata al CLLE preposto
052400110930     C                   call(e)   'TITVEVTC'
052500040119     C                   parm                    parccm
052600040119     C                   parm                    parmbr
052700040119     C                   parm                    paropz
052800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052900040119     C                   if        %error
053000040119     C                   movel     '1'           chkcall
053100040119     C                   else
053200040119     C                   movel     '0'           chkcall
053300040119     C                   endif
053400040119     C*
053500040119     C                   ENDSR
053600010601
053700010601
053800010601
053900010330     C*----------------------------------------------------*
054000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054100010330     C*----------------------------------------------------*
054200040119     C     INZVAR        BEGSR
054300080527     C*
054400080527     C                   setoff                                       65
054500010330     C*
054600030822     C* Inizializzo variabili di wrk
054700130128     C                   Z-ADD     *zeros        Num3_0            3 0
054800020204     C                   Z-ADD     *zeros        Num5_0            5 0
054900101018     C                   MOVEL     *blanks       wFlgCAS
055000060331     C                   MOVEL     *blanks       wVATNOT_A
055100060331     C                   MOVEL     *blanks       wVATNOT_B
055200060331     C                   MOVEL     *blanks       wVATNOT_E
055300110927     C                   MOVEL     *blanks       wVATNOT_I
055400120202     C                   MOVEL     *blanks       wVATNOT_J
055500120202     C                   MOVEL     *blanks       wVATNOT_IJ
055600141007     C                   MOVEL     *blanks       wVATNOT_S
055700130128     C                   MOVEL     *blanks       w70_1
055800130128     C                   MOVEL     *blanks       w70_2
055900130128     C                   MOVEL     *blanks       w70_3
056000130128     C                   MOVEL     *blanks       w70_4
056100030822     C*
056200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056300030822     C                   CLEAR                   SkSplitCSV
056400030822     C                   EVAL      i = 1
056500030822     C                   EVAL      posDa = *zeros
056600030822     C                   EVAL      posA  = *zeros
056700080527     C*
056800110930     C                   CLEAR                   EDIVABS
056900110930     C                   CLEAR                   EDIVAT00
057000030822     C*
057100030822     C* Reimposto i valori di default
057200030822     C                   EXSR      DEFCAM
057300030822     C*
057400010330     C                   ENDSR
057500010330     C*----------------------------------------------------*
057600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057700010330     C*----------------------------------------------------*
057800010330     C     DEFCAM        BEGSR
057900030822     C*
058000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058100030715     C* e delimitatore testo.
058200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058500100318     C*
058600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058700100318     C                   EVAL      CharSOS = CharNUM
058800030822     C*
058900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
059000100720     C                   SETOFF                                       505152
059100100720     C                   SETOFF                                       53
059200100413     C                   SELECT
059300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
059400100720     C                   SETON                                        50
059500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
059600100413     C                   SETON                                        51
059700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059800100720     C                   SETON                                        52
059900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
060000100720     C                   SETON                                        53
060100100413     C                   ENDSL
060200030822     C*
060300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060400030822     C                   EVAL      posDaDft = 1
060500030822     C                   EVAL      posADft  = 0
060600030822     C                   EVAL      wGiroDft = 0
060700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060800030822     C                             posDaDft > 0
060900030822     C*
061000030822     C* Gestisco il 1° giro
061100030822     C                   IF        wGiroDft = 0
061200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061400030822     C* Incremento il contatore dei "giri"
061500030822     C                   EVAL      wGiroDft = 1
061600030822     C                   ELSE
061700030822     C                   EVAL      posDaDft = posADft
061800030822     C                   ENDIF
061900030822     C* Eseguo lo scan x trovare la fine del campo corrente
062000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062100030822     C*
062200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062400030822     C                   IF        posDaDft > 0 AND
062500030822     C                             posADft  > 0
062600030822     C* NCL
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'NCL'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABNCL
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* CCM
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CCM'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABCCM
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* LNP
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'LNP'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiInt=*on
066200030822     C                   Z-ADD     PiVal         VABLNP
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* NRS
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'NRS'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiInt=*on
067500030822     C                   Z-ADD     PiVal         VABNRS
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* CTR
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CTR'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiInt=*on
068800030822     C                   Z-ADD     PiVal         VABCTR
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* PKB
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'PKB'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiNum=*on
070100030822     C                   Z-ADD     PiVal         VABPKB
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400030822     C* VLB
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'VLB'
070900030822     C                   EVAL      PiStr=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   EXSR      CHKNUM
071300030822     C                   IF        PiNum=*on
071400030822     C                   Z-ADD     PiVal         VABVLB
071500030822     C                   ENDIF
071600030822     C                   ENDIF
071700030822     C* QFT
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'QFT'
072200030822     C                   EVAL      PiStr=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   EXSR      CHKNUM
072600030822     C                   IF        PiNum=*on
072700030822     C                   Z-ADD     PiVal         VABQFT
072800030822     C                   ENDIF
072900030822     C                   ENDIF
073000030822     C* CBO
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'CBO'
073500030822     C                   EVAL      VABCBO=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* TSP
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'TSP'
074400030822     C                   EVAL      VABTSP=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* VAS
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'VAS'
075300030822     C                   EVAL      VABVAS=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* VCA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'VCA'
076200030822     C                   EVAL      VABVCA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600070925     C* TIC
076700070925     C                   IF        %subst(
076800070925     C                             %subst(vlrppt:posDaDft+1:
076900070925     C                             posADft-posDaDft-1):1:3)
077000070925     C                             = 'TIC'
077100070925     C                   EVAL      VABTIC=%trim(%subst(
077200070925     C                             %subst(vlrppt:posDaDft+1:
077300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400070925     C                   ENDIF
077500030822     C* GCA
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'GCA'
078000030822     C                   EVAL      VABGCA=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* CTM
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'CTM'
078900030822     C                   EVAL      VABCTM=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* FFD
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'FFD'
079800030822     C                   EVAL      VABFFD=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* VAD
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'VAD'
080700030822     C                   EVAL      VABVAD=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* GMA
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'GMA'
081600030822     C                   EVAL      VABGMA=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* GGA
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'GGA'
082500030822     C                   EVAL      VABGGA=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* GVA
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'GVA'
083400030822     C                   EVAL      VABGVA=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800030822     C* TC1
083900030822     C                   IF        %subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):1:3)
084200030822     C                             = 'TC1'
084300030822     C                   EVAL      VABTC1=%trim(%subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600030822     C                   ENDIF
084700030822     C* TC2
084800030822     C                   IF        %subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):1:3)
085100030822     C                             = 'TC2'
085200030822     C                   EVAL      VABTC2=%trim(%subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085500030822     C                   ENDIF
085600040714     C* VATTRC
085700040714     C                   IF        %subst(
085800040714     C                             %subst(vlrppt:posDaDft+1:
085900040714     C                             posADft-posDaDft-1):1:3)
086000040714     C                             = 'TRC'
086100040714     C                   EVAL      VATTRC=%trim(%subst(
086200040714     C                             %subst(vlrppt:posDaDft+1:
086300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086400040714     C                   ENDIF
086500030822     C* ...
086600030822     C                   ENDIF
086700030822     C                   ENDDO
086800020204     C*
086900010330     C                   ENDSR
087000010607     C*----------------------------------------------------*
087100110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
087200010607     C*----------------------------------------------------*
087300010607     C     IMPVAB        BEGSR
087400010607     C*
087500010607     C                   SETOFF                                       3132
087600110315 xxx C***                SETON                                        35
087700030515     C*
087800010607     C                   MOVEL     datcor        VABAAS
087900010607     C                   MOVE      datcor        VABMGS
088000010607     C*
088100030715     C**********
088200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088300030715     C**********
088400030715     C                   IF        wGiro = *zeros
088500040113     C*
088600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088900080429     C                             %trim(intcol(5))<>*blanks
089000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
089200080429     C                                      %trim(intcol(5))
089300070823     C     ';':CharCSV   XLATE     vindta        vindta
089400040510     C                   ENDIF
089500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089600031201     C     minu:maiu     XLATE     vindta        vindta
089700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089800030715     C                   DOW       posDa <= %len(%trim(vindta))
089900030715     C*
090000030715     C* Gestisco il 1° campo
090100030715     C                   IF        i = 1
090200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090300030715     C                   EVAL      posDa = 1
090400030715     C* Eseguo lo scan x trovare la fine del primo campo
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C* X i campi successivi al 1°
091000030715     C                   ELSE
091100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091200030715     C                   EVAL      posDa = posA + 1
091300030715     C* Eseguo lo scan x trovare la fine del campo corrente
091400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091500030715     C* Gestisco l'ultimo campo
091600030715     C                   IF        posA = *zeros
091700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   LEAVE
092200030715     C                   ELSE
092300030715     C* A questo punto "estraggo" il campo corrente
092400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092500030715     C                                                    (posA-posDa))
092600030715     C                   ENDIF
092700030715     C                   ENDIF
092800030715     C* Incremento il contatore d campo
092900030715     C                   EVAL      i = i +1
093000030715     C                   ENDDO
093100030715     C*
093200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093300030715     C* delimitazione testo
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093800030715     C                   EVAL      i = i + 1
093900030715     C                   ENDDO
094000030715     C                   EVAL      wGiro = 1
094100030715     C                   ELSE
094200030715     C                   EVAL      wGiro = 2
094300031204     C**********
094400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094500031204     C* d separatore campo
094600031204     C**********
094700031204     C                   EVAL      vindta = %trim(vindta)
094800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094900031204     C                   EVAL      vindta = %subst(vindta:2)
095000031204     C                   ENDDO
095100031204     C*
095200031204     C                   Z-ADD     *zeros        lunghInput        4 0
095300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095700110912     C                   ENDDO
095800030715     C**********
095900030715     C* Effettuo lo split del campo dati d input
096000030715     C**********
096100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096200030715     C                   DOW       posDa <= %len(%trim(vindta))
096300030715     C*
096400030715     C* Gestisco il 1° campo
096500030715     C                   IF        i = 1
096600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096700030715     C                   EVAL      posDa = 1
096800030715     C* Eseguo lo scan x trovare la fine del primo campo
096900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097000030715     C* A questo punto "estraggo" il campo corrente
097100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097200030715     C                                                    (posA-posDa))
097300030715     C* X i campi successivi al 1°
097400030715     C                   ELSE
097500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097600030715     C                   EVAL      posDa = posA + 1
097700031204     C* Verifico che nn vi sia il campo nn valorizzato
097800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097900031204     C* Se campo nn valorizzato skippo al prossimo
098000031204     C                   EVAL      SkSplitCSV(i) = *blanks
098100031204     C                   EVAL      posA  = posA + 1
098200031204     C                   ELSE
098300030715     C* Eseguo lo scan x trovare la fine del campo corrente
098400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098500030715     C* Gestisco l'ultimo campo
098600030715     C                   IF        posA = *zeros
098700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098800030715     C* A questo punto "estraggo" il campo corrente
098900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099000030715     C                                                    (posA-posDa))
099100030715     C                   LEAVE
099200030715     C                   ELSE
099300030715     C* A questo punto "estraggo" il campo corrente
099400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099500030715     C                                                    (posA-posDa))
099600030715     C                   ENDIF
099700031204     C                   ENDIF
099800030715     C                   ENDIF
099900030715     C* Incremento il contatore d campo
100000030715     C                   EVAL      i = i +1
100100030715     C                   ENDDO
100200030715     C*
100300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100400030715     C* delimitazione testo
100500030715     C                   EVAL      i = 1
100600030715     C                   DOW       i < %elem(SkSplitCSV)
100700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100900030715     C                   EVAL      i = i + 1
101000030715     C                   ENDDO
101100100720     C*
101200100720     C* Stacco un progressivo x numero spedizione
101300110930 xxx C                   EXSR      repNSP
101400100326     C*
101500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101600030715     C                   EVAL      i = 1
101700030715     C                   DOW       i < %elem(SkSplitFLD)
101800030715     C*
101900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
102000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102100030715     C                             SkSplitCSV(i) <> *blanks
102200030721     C*** ATB
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102400030721     C                   EVAL      VABATB = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030715     C*** RSD
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
103000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
103100030715     C* ==
103200030715     C                   ENDIF
103300030721     C*** RD2
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030715     C*** IND
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103900030715     C                   EVAL      VABIND = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** LOD
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
104300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** PRD
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
104700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030721     C*** NZD
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
105100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030715     C*** RMA
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** NOT
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** NT2
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
106300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030715     C*** VCA
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
106700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030715     C*** VAS
107000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
107100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
107200030715     C                   ENDIF
107300030715     C*** FFD
107400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107600030715     C                   ENDIF
107700030721     C*** GC1
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GC2
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
108300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TSP
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
108700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108800030721     C                   ENDIF
108900040510     C*** CBO
109000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
109100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
109200040510     C                   ENDIF
109300030721     C*** NAS
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TIC
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** GCA
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
110300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** XCO
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
110700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** CTM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
111100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** TCR
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** CTS
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** FTM
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
112300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** VAD
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
112700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** GMA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
113100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** GGA
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** GVA
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** TC1
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
114300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** TC2
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
114700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** SCL
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
115100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** RMO
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*** NMO
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
116000030721     C                   ENDIF
116100030721     C*
116200030721     C* Reperisco quindi i campi numerici...
116300040510     C*** NSP
116400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116500040510     C                   EVAL      PiStr=SkSplitCSV(i)
116600040510     C                   EXSR      CHKNUM
116700040510     C                   IF        PiInt=*on
116800040510     C                   Z-ADD     PiVal         VABNSP
116900040510     C                   ELSE
117000040510     C                   SETON                                        31
117100040510     C                   EVAL      VABNSP = *zeros
117200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040510     C                             + ' ' + 'VABNSP'
117400040510     C                   ENDIF
117500040510     C                   ENDIF
117600071106     C*** RMN
117700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117800071106     C                   EVAL      PiStr=SkSplitCSV(i)
117900071106     C                   EXSR      CHKNUM
118000071106     C                   IF        PiInt=*on
118100071106     C                   Z-ADD     PiVal         VABRMN
118200071106     C                   ELSE
118300071106     C                   SETON                                        32
118400071106     C                   EVAL      VABRMN = *zeros
118500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118600071106     C                             + ' ' + 'VABRMN'
118700071106     C                   ENDIF
118800071106     C                   ENDIF
118900030721     C*** CAD
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on
119400030721     C                   Z-ADD     PiVal         Num5_0
119500030721     C                   MOVEL(p)  Num5_0        VABCAD
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABCAD = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABCAD'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** NCL
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on
120800030721     C                   Z-ADD     PiVal         VABNCL
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      VABNCL = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040122     C                             + ' ' + 'VABNCL'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** PKB
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiNum=*on
122100030721     C                   Z-ADD     PiVal         VABPKB
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABPKB = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABPKB'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** CAS
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
123100141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
123200141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
123300141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
123400141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
123500141007     C                             %trim(SkSplitCSV(i)) <> ',00'
123600141007     C                   EVAL      VABVCA = 'EUR'
123700101018     C                   EVAL      wFlgCAS = '1'
123800101018     C                   ENDIF
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiNum=*on
124200030721     C                   Z-ADD     PiVal         VABCAS
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      VABCAS = *zeros
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABCAS'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** CCM
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on      AND
125500030721     C                             PiVal<=9999999 AND
125600030721     C                             PiVal>*zeros
125700030721     C                   Z-ADD     PiVal         VABCCM
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      VABCCM = *zeros
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200040510     C                             + ' ' + 'VABCCM'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** LNP
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on    AND
127000030721     C                             PiVal<=999   AND
127100030721     C                             PiVal>*zeros
127200030721     C                   Z-ADD     PiVal         VABLNP
127300030721     C                   ELSE
127400040715     C                   SETON                                        32
127500030721     C                   EVAL      VABLNP = *zeros
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700040510     C                             + ' ' + 'VABLNP'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** NRS
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on AND
128500030721     C                             PiVal<=99
128600030721     C                   Z-ADD     PiVal         VABNRS
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABNRS'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** LNA
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on    AND
129800030721     C                             PiVal<=999   AND
129900030721     C                             PiVal>*zeros
130000030721     C                   Z-ADD     PiVal         VABLNA
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      VABLNA = *zeros
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABLNA'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** CTR
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiInt=*on  AND
131300030721     C                             PiVal<=999
131400030721     C                   Z-ADD     PiVal         VABCTR
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABCTR'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** IAS
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
132300141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
132400141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
132500141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
132600141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132700141007     C                             %trim(SkSplitCSV(i)) <> ',00'
132800141007     C                   EVAL      VABVAS = 'EUR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABIAS
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABIAS'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900141007     C                   ENDIF
134000030721     C*** VLB
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiNum=*on
134500030721     C                   Z-ADD     PiVal         VABVLB
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABVLB'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** QFT
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiNum=*on
135700030721     C                   Z-ADD     PiVal         VABQFT
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABQFT'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** NCD
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on      AND
136900030721     C                             PiVal<=9999999
137000030721     C                   Z-ADD     PiVal         VABNCD
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABNCD'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** NCA
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on      AND
138200030721     C                             PiVal<=9999999
138300030721     C                   Z-ADD     PiVal         VABNCA
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABNCA'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** ZNC
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiInt=*on AND
139500030721     C                             PiVal<=99
139600030721     C                   Z-ADD     PiVal         VABZNC
139700030721     C                   ELSE
139800030721     C                   SETON                                        32
139900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140000030721     C                             + ' ' + 'VABZNC'
140100030721     C                   ENDIF
140200030721     C                   ENDIF
140300030721     C*** DCR
140400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
140500030721     C                   EVAL      PiStr=SkSplitCSV(i)
140600030721     C                   EXSR      CHKNUM
140700030721     C                   IF        PiInt=*on       AND
140800030721     C                             PiVal<=99999999
140900030721     C                   Z-ADD     PiVal         VABDCR
141000030721     C                   ELSE
141100030721     C                   SETON                                        32
141200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141300030721     C                             + ' ' + 'VABDCR'
141400030721     C                   ENDIF
141500030721     C                   ENDIF
141600030721     C*** HCR
141700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141800030721     C                   EVAL      PiStr=SkSplitCSV(i)
141900030721     C                   EXSR      CHKNUM
142000030721     C                   IF        PiInt=*on   AND
142100030721     C                             PiVal<=9999
142200030721     C                   Z-ADD     PiVal         VABHCR
142300030721     C                   ELSE
142400030721     C                   SETON                                        32
142500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142600030721     C                             + ' ' + 'VABHCR'
142700030721     C                   ENDIF
142800030721     C                   ENDIF
142900030721     C*** VMD
143000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
143100030721     C                   EVAL      PiStr=SkSplitCSV(i)
143200030721     C                   EXSR      CHKNUM
143300030721     C                   IF        PiNum=*on
143400030721     C                   Z-ADD     PiVal         VABVMD
143500030721     C                   ELSE
143600030721     C                   SETON                                        32
143700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143800030721     C                             + ' ' + 'VABVMD'
143900030721     C                   ENDIF
144000030721     C                   ENDIF
144100030721     C*** ANT
144200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
144300030721     C                   EVAL      PiStr=SkSplitCSV(i)
144400030721     C                   EXSR      CHKNUM
144500030721     C                   IF        PiInt=*on        AND
144600030721     C                             PiVal<=999999999
144700030721     C                   Z-ADD     PiVal         VABANT
144800030721     C                   ELSE
144900030721     C                   SETON                                        32
145000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145100030721     C                             + ' ' + 'VABANT'
145200030721     C                   ENDIF
145300030721     C                   ENDIF
145400030721     C*** CMO
145500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145600030721     C                   EVAL      PiStr=SkSplitCSV(i)
145700030721     C                   EXSR      CHKNUM
145800030721     C                   IF        PiInt=*on
145900030721     C                   Z-ADD     PiVal         Num5_0
146000030721     C                   MOVEL(p)  Num5_0        VABCMO
146100030721     C                   ELSE
146200030721     C                   SETON                                        32
146300030721     C                   EVAL      VABCMO = *zeros
146400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146500030721     C                             + ' ' + 'VABCMO'
146600030721     C                   ENDIF
146700030721     C                   ENDIF
146800040510     C*
146900110930     C* Reperisco i campi del EDIVAT
147000040510     C*** VATNOT
147100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
147200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
147300040510     C                   ENDIF
147400040510     C*** VATTRC
147500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147700040510     C                   ENDIF
147800060331     C*** VATNOT_A
147900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
148000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
148100060331     C                   ENDIF
148200060331     C*** VATNOT_B
148300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
148400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148500060331     C                   ENDIF
148600060331     C*** VATNOT_E
148700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148900060331     C                   ENDIF
149000110927     C*** VATNOT_I
149100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
149200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
149300110927     C                   ENDIF
149400141007     C*** VATNOT_IJ
149500141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
149600141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149700141007     C                   ENDIF
149800141007     C*** VATNOT_S
149900141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
150000141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
150100141007     C                   ENDIF
150200070823     C*
150300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150400070925     C*
150500130403
150600130403     C*
150700071029     C***  <===  -----------------------------------------
150800040510     C*
150900030715     C                   ENDIF
151000030715     C                   EVAL      i = i + 1
151100030715     C                   ENDDO
151200070925     C*
151300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151400121030     C*
151500070925     C*
151600071029     C***  <===  -----------------------------------------
151700070925     C*
151800070925     C* Considerazioni finali su CBO/CAS
151900100318     C                   IF        wFlgCAS = '1'    OR
152000100318     C                             VABCAS  > *zeros
152100130403     C                   IF        VABVCA = *blanks
152200130403     C                   EVAL      VABVCA = 'EUR'
152300130403     C                   EVAL      VABTIC = *blanks
152400130403     C                   ENDIF
152500070925     C                   IF        VABCBO = '1'
152600070925     C                   EVAL      VABCBO = '4'
152700070925     C                   ENDIF
152800070925     C                   IF        VABCBO = '2'
152900070925     C                   EVAL      VABCBO = '6'
153000070925     C                   ENDIF
153100130403     C                   ELSE
153200130403     C                   EVAL      vabVCA = *blank
153300070925     C                   ENDIF
153400070925     C***  <===  ----------------------------
153500040510     C*
153600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153700040510     C                   IF        VABNSP = *zeros
153800040510     C                   SETON                                        31
153900040510     C                   ENDIF
154000040715     C*
154100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154200040715     C                   IF        VABLNP = *zeros
154300040715     C                   SETON                                        31
154400040715     C                   ENDIF
154500040510     C*
154600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154700040510     C                   IF        VABRMN = *zeros
154800040510     C                   Z-ADD     VABNSP        VABRMN
154900040510     C                   ENDIF
155000040510     C*
155100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155200040510     C                   IF        VABRMA = *blanks
155300040714     C                   MOVEL     VABRMN        VABRMA
155400040510     C                   ENDIF
155500040128     C*
155600040510     C* Se provincia nn valorizzata la reperisco
155700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155800110912     C                   IF        VABPRD =  *blanks AND
155900110912     C                             VABNZD =  *blanks AND
156000110912     C                             VABCAD <> *blanks AND
156100110912     C                             VABLOD <> *blanks
156200040128     C                   CLEAR                   TISI95DS
156300040128     C                   EVAL      I95TCN = '3'
156400040128     C                   Z-ADD     datcor        I95DAT
156500040128     C                   EVAL      I95NAR = VABNZD
156600040128     C                   EVAL      I95CAP = VABCAD
156700040128     C                   EVAL      I95LOC = VABLOD
156800040128     C                   CALL      'TISI95R'
156900040128     C                   PARM                    TISI95DS
157000040128     C                   EVAL      VABPRD = O95PRV
157100040510     C                   ENDIF
157200020204     C*
157300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157400020204     C                   EXSR      CHKIMPDIV
157500030715     C                   ENDIF
157600020204     C*
157700010607     C                   ENDSR
157800010607     C*----------------------------------------------------*
157900010601
158000020204
158100020204     C*----------------------------------------------------*
158200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158300020204     C*----------------------------------------------------*
158400020204     C     CHKIMPDIV     BEGSR
158500020204     C*
158600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158700020204     C                   Z-ADD     *zeros        wrkDec            9 9
158800020204     C*
158900020204     C* Come prima cosa effettuo considerazioni sulla divisa
159000020204     C                   IF        vabIAS > *zeros
159100020204     C                   IF        vabVAS <> 'EUR'
159200020204     C                   EVAL      vabVAS =  'ITL'
159300020204     C                   ENDIF
159400020204     C                   ENDIF
159500020204     C*
159600020204     C                   IF        vabCAS > *zeros
159700020204     C                   IF        vabVCA <> 'EUR'
159800020204     C                   EVAL      vabVCA =  'ITL'
159900020204     C                   ENDIF
160000020204     C                   ENDIF
160100020204     C*
160200020204     C                   IF        vabVMD > *zeros
160300020204     C                   IF        vabVAD <> 'EUR'
160400020204     C                   EVAL      vabVAD =  'ITL'
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160900020204     C                   Z-ADD     vabIAS        wrkDec
161000020204     C                   IF        wrkDec > *zeros
161100020204     C                   IF        vabVAS = 'ITL'
161200020204     C                   EVAL      vabIAS = *zeros
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161700020204     C                   Z-ADD     vabCAS        wrkDec
161800020204     C                   IF        wrkDec > *zeros
161900020204     C                   IF        vabVCA = 'ITL'
162000020204     C                   EVAL      vabCAS = *zeros
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162500020204     C                   Z-ADD     vabVMD        wrkDec
162600020204     C                   IF        wrkDec > *zeros
162700020204     C                   IF        vabVAD = 'ITL'
162800020204     C                   EVAL      vabVMD = *zeros
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C                   ENDSR
163300020204     C***
163400010330
163500100318
163600100318
163700100318     C*----------------------------------------------------*
163800100318     C*  CONTROLLO NUMERICITA' CAMPI
163900100318     C*----------------------------------------------------*
164000100318     C     CHKNUM        BEGSR
164100100318     C*
164200100318     C                   IF        PiDecChr = *blanks
164300100318     C                   EVAL      PiDecChr = CharNUM
164400100318     C                   ENDIF
164500100318     C*
164600100318     C                   callp     UBISNUM_Check(PiStr
164700100318     C                                          :PiDecChr
164800100318     C                                          :PiVal
164900100318     C                                          :PiNum
165000100318     C                                          :PiInt)
165100100318     C*
165200100318     C                   ENDSR
165300100318     C***
165400010601
165500010601
165600010601
165700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165800010601     C     repfil        BEGSR
165900010601     C*
166000010601     C                   if        invfil = *zeros and
166100010601     C                             depfil > *zeros and
166200010629     C                             (vinflg = *blanks or
166300010629     C                              vinflg = *zeros)
166400010601     C
166500010601     C                   eval      invfil = depfil
166600010601     C                   endif
166700010601     C*
166800010601     C                   if        depfil <> invfil and
166900010601     C                             invfil > *zeros
167000010601     C                   eval      flgMulti = '1'
167100010601     C                   if        vinflg = *blanks
167200010601     C                   add       1             cntNonEl
167300010601     C                   endif
167400010601     C                   endif
167500010601     C*
167600010601     C                   if        vinflg = '2'
167700010601     C                   eval      flgStato = '2'
167800010601     C                   endif
167900010601     C*
168000010601     C                   ENDSR
168100010601     C***
168200010601
168300010601
168400010601
168500010330
168600040119      /TITLE Invio dei dati al punto operativo.
168700040119     C     invio         BEGSR
168800040119     C*
168900110930     C* 1° invio EDIVAT
169000040119     C                   reset                   dscmz
169100040119     C                   move      vlrpoi        cmzdst
169200110930     C                   eval      cmzfld = 'EDIVATWR'
169300040119     C                   eval      cmzmbd = vlrhdl
169400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169500040119     C***                if        prmfir = *blanks
169600110930     C                   eval      cmzfla = 'EDIVAT0F'
169700110930     C                   eval      cmzmba = 'EDIVAT0F'
169800040119     C***                else
169900040119     C***                eval      cmzfla = prmfir
170000040119     C***                eval      cmzmba = prmfir
170100040119     C***                endif
170200040119     C                   eval      cmznrr = *zeros
170300040119     C                   move      §ctrokvt      cmznrr
170400040119     C                   eval      cmzlba = vlrfl1
170500040119     C                   call(e)   'TIS711C'
170600040119     C                   parm                    dscmz
170700040119     C                   parm      *blanks       esito
170800040119     C                   if        %error
170900040119     C                             or cmzerr = '1'
171000040119     C                             or esito  = '1'
171100040119     C                   eval      wrkesito = '3'
171200040119     C                   else
171300040119     C*
171400110930     C* 2° invio EDIVAB
171500040119     C                   reset                   dscmz
171600040119     C                   move      vlrpoi        cmzdst
171700040119     C                   eval      cmzfld = vlrfou
171800040119     C                   eval      cmzmbd = vlrhdl
171900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172000040119     C***                if        prmfir = *blanks
172100110930     C                   eval      cmzfla = 'EDIVAB0F'
172200110930     C                   eval      cmzmba = 'EDIVAB0F'
172300040119     C***                else
172400040119     C***                eval      cmzfla = prmfir
172500040119     C***                eval      cmzmba = prmfir
172600040119     C***                endif
172700040119     C                   eval      cmznrr = *zeros
172800040119     C                   move      §ctrokvb      cmznrr
172900040119     C                   eval      cmzlba = vlrfl1
173000040119     C                   call(e)   'TIS711C'
173100040119     C                   parm                    dscmz
173200040119     C                   parm      *blanks       esito
173300040119     C                   if        %error
173400040119     C                             or cmzerr = '1'
173500040119     C                             or esito  = '1'
173600040119     C                   eval      wrkesito = '3'
173700040119     C                   endif
173800040119     C                   endif
173900040119     C*
174000040119     C                   ENDSR
174100040119     C***
174200010601
174300010601
174400010601
174500010601
174600010601      /TITLE Invio dei dati al punto operativo.
174700010601     C     opeini        BEGSR
174800010601     C*
174900010601     C* Inizializzo flag e contatori operativi
175000010601     C                   movel     '0'           flgGiro           1
175100010601     C                   movel     '0'           flgMulti          1
175200010601     C                   movel     '1'           flgStato          1
175300010615     C                   movel     '0'           flgOk             1
175400010601     C                   z-add     *zeros        cntNonEl         10 0
175500010601     C                   z-add     *zeros        depfil            3 0
175600010601     C                   z-add     *zeros        invfil            3 0
175700010601     C*
175800010601     C                   ENDSR
175900010601     C***
176000070326
176100070326
176200070326
176300070326
176400070326     C     *pssr         BEGSR
176500070329     C*
176600070329     C                   if        %open(tivin00r)
176700070329     C                   close     tivin00r
176800070329     C                   endif
176900110930     C                   if        %open(edivabwr)
177000110930     C                   close     edivabwr
177100070329     C                   endif
177200110930     C                   if        %open(edivatwr)
177300110930     C                   close     edivatwr
177400070329     C                   endif
177500070326     C*
177600070326     C* Effettuo la chiamata al CLLE preposto
177700110930     C                   call(e)   'TITVEVTC'
177800070326     C                   parm                    parccm
177900070326     C                   parm                    parmbr
178000070326     C                   parm      '2'           paropz
178100070326     C*
178200070326     C                   eval      wrkesito = '2'
178300070404     C*
178400070404     C                   seton                                        LR
178500070326     C*
178600070326     C                   ENDSR     '*CANCL'
178700070326     C***
178800100318
178900100318
179000100318
179100100318     C     normCSV       BEGSR
179200100318     C*
179300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179400100318     C* separatori campi
179500100318     C                   if        CharTXT <> *blanks
179600100318     C                   z-add     *zeros        wLenVINDTA        4 0
179700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179800100318     C                   setoff                                       80
179900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180200100318     C                   if        not *in80
180300100318     C                   seton                                        80
180400100318     C                   else
180500100318     C                   setoff                                       80
180600100318     C                   endif
180700100318     C                   endif
180800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180900100318     C                             and *in80 = *on
181000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181100100318     C                   endif
181200100318     C                   enddo
181300100318     C                   endif
181400100318     C*
181500100318     C                   ENDSR
181600100318     C***
181700070326
181800100326
181900100326
182000100326     C*----------------------------------------------------*
182100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182200100326     C*----------------------------------------------------*
182300100326     C     REPNSP        BEGSR
182400100326     C*
182500100326     C* NSP => Stacco un numeratore da AZNUM
182600100326     C                   clear                   TRUL33DS
182700100326     C                   eval      I33OPE = *zeros
182800100326     C                   eval      I33CNU = 302
182900100326     C                   eval      I33NUM = 1
183000100326     C                   movel     TRUL33DS      KPJBU
183100100326     C                   call      'TRUL33R'
183200100326     C                   parm                    KPJBA
183300100326     C                   movel     KPJBU         TRUL33DS
183400100326     C                   if        O33ERR = *zeros
183500100326     C                   z-add     O33NRF        VABNSP
183600100326     C                   z-add     O33NRF        VATNSP
183700100326     C                   else
183800100326     C                   SETON                                        31
183900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184000100326     C                             + ' ' + 'VABNSP VATNSP'
184100100326     C                   endif
184200100326     C*
184300100326     C                   ENDSR
184400010330
184500100326
184600010330
184700000613     C     *inzsr        BEGSR
184800990910     C*
184900990910     C     *entry        plist
185000990920     C                   parm                    tivlrds
185100990921     C                   parm      wrkesito      esito
185200000724     C                   parm                    prmlit
185300000710     C                   parm                    prmfir
185400010330     C*
185500010330     C* CALCOLA LA DATA CORRENTE
185600110930     C                   time                    wn14             14 0
185700110930     C                   movel     wn14          oracor            6 0          *ORA
185800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185900100318     C                   eval      datcor = %dec(%date() : *ISO)
186000000613     C*
186100000613     C                   ENDSR
186200000613     C***
186300990908
186400120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186500141007VATNOT_E;X;VABRMA;X;X;X;VABPKB;VABRMN;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;VATNOT_S;VATNOT_IJ;
186600141007X;VABCAS;VABIAS;VABNT2;X;X;VABNOT;
186700141007
186800141007
186900141007
