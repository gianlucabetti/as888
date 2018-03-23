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
003900170919     D Intestazioni    s               n   inz(*off)
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
007000170919     D w70             s             70
007100030822     D*------------------
007200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007300030822     D*------------------
007400030822     D posDaDft        S              3  0 INZ(*zeros)
007500030822     D posADft         S              3  0 INZ(*zeros)
007600030822     D j               s              3  0 INZ(1)
007700030822     D wGiroDft        s              1  0 INZ(*zeros)
007800080418     D*------------------
007900080418     D* Costanti
008000080418     D*------------------
008100080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008200080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008300100318
008400100318     D*------------------
008500100318     D* LINKING A DEFINIZIONI ESTERNE
008600100318     D*------------------
008700100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008800100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008900100318
009000100909     D*------------------
009100100909     D* DS CALCOLO CHECK-DIGITS
009200100909     D*------------------
009300100909     D TRUL28DSX     e ds                  inz
009400010330
009500010330
009600010330
009700000913     C                   reset                   rrnum
009800990921     C                   reset                   esito
009900990921     C                   reset                   wrkesito
010000010601     C*
010100010601     C                   exsr      opeini
010200010605     C                   exsr      rwvab
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400010601     C                   if        invfil <> *zeros and
011500010601     C                             flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900010601     C*
012000010601     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013000010601     C*--------------------------------------------------------
013100010601     C     ENDELA        BEGSR
013200040119     C*
013300040119     C* Effettuo la chiamata al CLLE preposto
013400110930     C                   call(e)   'TITVEVTC'
013500040510     C                   parm                    parccm
013600040119     C                   parm                    parmbr
013700040119     C                   parm      '2'           paropz
013800050201     C*
013900050201     C* Effettuo lancio TISI95 solo x chiusura
014000050201     C                   CLEAR                   TISI95DS
014100050201     C                   EVAL      I95TLA = 'C'
014200050201     C                   CALL      'TISI95R'
014300050201     C                   PARM                    TISI95DS
014400000616     C*
014500010601     C                   ENDSR
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600110930     C                   if        not %open(edivabwr)
015700110930     C                   open      edivabwr
015800010330     C                   endif
015900040119     C*
016000110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016100040119     C                   exsr      prevat
016200040119     C*
016300040119     C                   if        chkcall = '0'
016400040119     C*
016500110930     C                   if        not %open(edivatwr)
016600110930     C                   open      edivatwr
016700040119     C                   endif
016800010330     C*
016900040119     C                   clear                   §CTROKVB          7 0
017000040119     C                   clear                   §CTROKVT          7 0
017100040119     C                   clear                   §CTRMO            7 0
017200040119     C                   clear                   §CTRNO            7 0
017300121030     C*
017400121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017500121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017600121030     C                   IF        Intestazioni = *off
017700121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017800121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017900121030     C                   exsr      inzvar
018000121030     C                   exsr      impvab
018100121030     C                   ENDIF
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
033100120203 xxx C***                z-add     *zeros        VABNCL              F0461
033200120203 xxx C***                z-add     *zF046190081  VABPKB
033300110315 xxx C***                z-add     *zeros        VABVLB
033400110315 xxx C***                endif
033500080527     C*
033600080527     C* Scrivo il VAB solo a rottura d spedizione
033700080527     C                   if        depspe = *zeros
033800170919
033900170919      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
034000170919      * spedizione
034100170919     C  N31              eval      vabncl = 1
034200170919
034300110930     C  N31              eval      edivabs_sav = edivabs
034400080527     C                   eval      depspe = vabNSP
034500080527     C                   else
034600080527     C                   if        depspe = vabNSP AND *in65 = *off
034700170919
034800170919      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
034900170919      * spedizione
035000170919     C                   eval      vabncl += 1
035100170919
035200100413     C   51              add       VABNCL        sav_VABNCL
035300100413     C   51              add       VABPKB        sav_VABPKB
035400100413     C   51              add       VABVLB        sav_VABVLB
035500100720     C   52              z-add     VABNCL        sav_VABNCL
035600100720     C   52              z-add     VABPKB        sav_VABPKB
035700100720     C   52              z-add     VABVLB        sav_VABVLB
035800100720     C   53              add       VABNCL        sav_VABNCL
035900100720     C   53              z-add     VABPKB        sav_VABPKB
036000100720     C   53              z-add     VABVLB        sav_VABVLB
036100080527     C                   else
036200120203     C  N31              eval      edivabs_ok = edivabs_sav
036300080527     C  N31              exsr      wrivab
036400110930     C  N31              eval      edivabs_sav = edivabs
036500170919
036600170919      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036700170919      * spedizione
036800170919     C  N31              eval      vabncl = 1
036900170919
037000080527     C                   eval      depspe = vabNSP
037100080527     C                   endif
037200080527     C                   endif
037300080527     C*
037400080527     C                   ENDSR
037500040119
037600040119
037700040119
037800040119     C*----------------------------------------------------*
037900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038000040119     C*----------------------------------------------------*
038100040510     C     WRIVAB        BEGSR
038200080513     C*
038300120203     C* EDIVABWR ha il prefisso "ok_"
038400120203     C*
038500120203     C* Gestisco l'eventuale rottura x numero spedizione
038600080527     C                   if        VABNSP <> depspe or *in65 = *on
038700170919     C                   if        wVATNOT_E <> *blanks AND ok_VABNZD = *blanks
038800120203     C                   eval      ok_VABCTM = '7Q'
038900070301     C                   endif
039000170919     C                   if        wVATNOT_E <> *blanks AND ok_VABNZD<> *blanks
039100170919     C                   eval      ok_VABCTM = '7R'
039200170919     C                   endif
039300110930     C*
039400110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
039500120203     C                   MOVE(P)   vlrMSG        wNomeFile
039600130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
039700120203     C                   EVAL      ok_VABDCM = DATCOR
039800120203     C                   EVAL      ok_VABDTS = DATCOR
039900120203     C                   EVAL      ok_VABHMS = ORACOR
040000120203     C                   EVAL      ok_VABCNT = 1
040100110930     C*
040200110930     C                   write     edivab00
040300080513     C                   endif
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041100040119     C*----------------------------------------------------*
041200040510     C     WRIVAT        BEGSR
041300060512     C*
041400060512     C* Inizializzo i campi chiave
041500060512     C                   eval      VATFGS = VABFGS
041600060512     C                   eval      VATAAS = VABAAS
041700060512     C                   eval      VATCCM = VABCCM
041800070823     C                   eval      VATNRS = VABNRS
041900060512     C                   eval      VATNSP = VABNSP
042000060512     C                   eval      VATLNP = VABLNP
042100040119     C*
042200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042300040510     C                   if        VATTRC = *blanks
042400040510     C                   eval      VATTRC = 'E'
042500040510     C                   endif
042600110318     C                   if        VATNOT <> *blanks
042700120203     C                   MOVE(P)   vlrMSG        wNomeFile
042800130122     C***                EVAL      VATCMR = wNomeFile
042900141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
043000110930     C                   eval      VATCNT = 1
043100110930     C                   write     EDIVAT00
043200040119     C                   add       1             §CTROKVT
043300110318     C                   endif
043400060331     C*
043500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043600060331     C                   if        wVATNOT_A <> *blanks
043700060331     C                   eval      VATNOT = wVATNOT_A
043800060331     C                   eval      VATTRC = 'A'
043900120203     C                   MOVE(P)   vlrMSG        wNomeFile
044000130122     C***                EVAL      VATCMR = wNomeFile
044100141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044200110930     C                   eval      VATCNT = 1
044300110930     C                   write     EDIVAT00
044400060331     C                   add       1             §CTROKVT
044500060331     C                   endif
044600060331     C*
044700060331     C                   if        wVATNOT_B <> *blanks
044800060331     C                   eval      VATNOT = wVATNOT_B
044900060331     C                   eval      VATTRC = 'B'
045000120203     C                   MOVE(P)   vlrMSG        wNomeFile
045100130122     C***                EVAL      VATCMR = wNomeFile
045200141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
045300110930     C                   eval      VATCNT = 1
045400110930     C                   write     EDIVAT00
045500060331     C                   add       1             §CTROKVT
045600060331     C                   endif
045700141007     C*
045800141007     C                   if        wVATNOT_IJ <> *blanks
045900141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
046000141007     C                   eval      VATTRC = 'I'
046100141007     C***                EVAL      VATCMR = wNomeFile
046200141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
046300141007     C                   eval      VATCNT = 1
046400141007     C                   write     EDIVAT00
046500141007     C                   add       1             §CTROKVT
046600141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
046700141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046800141007     C                   eval      VATTRC = 'J'
046900141007     C***                EVAL      VATCMR = wNomeFile
047000141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
047100141007     C                   eval      VATCNT = 1
047200141007     C                   write     EDIVAT00
047300141007     C                   endif
047400141007     C                   endif
047500060331     C*
047600060331     C                   if        wVATNOT_E <> *blanks
047700060331     C                   eval      VATTRC = 'E'
047800121030     C                   MOVE(P)   vlrMSG        wNomeFile
047900130122     C***                EVAL      VATCMR = wNomeFile
048000141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048100121030     C                   eval      VATCNT = 1
048200141007     C*
048300141007     C* NON Sviluppare i "CHI SONO"
048400141007     C*
048500170919     C                   EVAL      VATNOT = wVATNOT_E
048600141007     C                   write     EDIVAT00
048700121030     C                   endif
048800110927     C*
048900110927     C                   if        wVATNOT_I <> *blanks
049000110927     C                   eval      VATNOT = wVATNOT_I
049100110927     C                   eval      VATTRC = 'I'
049200120203     C                   MOVE(P)   vlrMSG        wNomeFile
049300130122     C***                EVAL      VATCMR = wNomeFile
049400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
049500110930     C                   eval      VATCNT = 1
049600110930     C                   write     EDIVAT00
049700110927     C                   add       1             §CTROKVT
049800110927     C                   endif
049900120202     C*
050000120202     C                   if        wVATNOT_J <> *blanks
050100120202     C                   eval      VATNOT = wVATNOT_J
050200120202     C                   eval      VATTRC = 'J'
050300120203     C                   MOVE(P)   vlrMSG        wNomeFile
050400130122     C***                EVAL      VATCMR = wNomeFile
050500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
050600120202     C                   eval      VATCNT = 1
050700120202     C                   write     EDIVAT00
050800120202     C                   add       1             §CTROKVT
050900120202     C                   endif
051000141007     C*
051100141007     C                   if        wVATNOT_S <> *blanks
051200141007     C                   eval      VATNOT = wVATNOT_S
051300141007     C* i 2 flag devono essere blank
051400141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
051500141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
051600141007     C                   eval      VATTRC = 'S'
051700141007     C***                EVAL      VATCMR = wNomeFile
051800141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
051900141007     C                   eval      VATCNT = 1
052000141007     C                   write     EDIVAT00
052100141007     C                   add       1             §CTROKVT
052200141007     C                   endif
052300040119     C*
052400040119     C                   ENDSR
052500040119
052600040119
052700040119
052800040119     C*----------------------------------------------------*
052900110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
053000040119     C*----------------------------------------------------*
053100040119     C     PREVAT        BEGSR
053200040119     C*
053300110930     C* Compongo il nome del membro da dare al EDIVATWR
053400040119     C                   eval      parmbr = vlrhdl
053500040119     C                   movel     'M'           parmbr
053600040510     C                   eval      parccm = vlrksc
053700040119     C                   eval      paropz = '1'
053800040119     C* Effettuo la chiamata al CLLE preposto
053900110930     C                   call(e)   'TITVEVTC'
054000040119     C                   parm                    parccm
054100040119     C                   parm                    parmbr
054200040119     C                   parm                    paropz
054300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
054400040119     C                   if        %error
054500040119     C                   movel     '1'           chkcall
054600040119     C                   else
054700040119     C                   movel     '0'           chkcall
054800040119     C                   endif
054900040119     C*
055000040119     C                   ENDSR
055100010601
055200010601
055300010601
055400010330     C*----------------------------------------------------*
055500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
055600010330     C*----------------------------------------------------*
055700040119     C     INZVAR        BEGSR
055800080527     C*
055900080527     C                   setoff                                       65
056000010330     C*
056100030822     C* Inizializzo variabili di wrk
056200130128     C                   Z-ADD     *zeros        Num3_0            3 0
056300020204     C                   Z-ADD     *zeros        Num5_0            5 0
056400101018     C                   MOVEL     *blanks       wFlgCAS
056500060331     C                   MOVEL     *blanks       wVATNOT_A
056600060331     C                   MOVEL     *blanks       wVATNOT_B
056700060331     C                   MOVEL     *blanks       wVATNOT_E
056800110927     C                   MOVEL     *blanks       wVATNOT_I
056900120202     C                   MOVEL     *blanks       wVATNOT_J
057000120202     C                   MOVEL     *blanks       wVATNOT_IJ
057100141007     C                   MOVEL     *blanks       wVATNOT_S
057200170919     C                   MOVEL     *blanks       w70
057300030822     C*
057400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
057500030822     C                   CLEAR                   SkSplitCSV
057600030822     C                   EVAL      i = 1
057700030822     C                   EVAL      posDa = *zeros
057800030822     C                   EVAL      posA  = *zeros
057900080527     C*
058000110930     C                   CLEAR                   EDIVABS
058100110930     C                   CLEAR                   EDIVAT00
058200030822     C*
058300030822     C* Reimposto i valori di default
058400030822     C                   EXSR      DEFCAM
058500030822     C*
058600010330     C                   ENDSR
058700010330     C*----------------------------------------------------*
058800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
058900010330     C*----------------------------------------------------*
059000010330     C     DEFCAM        BEGSR
059100030822     C*
059200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
059300030715     C* e delimitatore testo.
059400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
059500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
059600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
059700100318     C*
059800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
059900100318     C                   EVAL      CharSOS = CharNUM
060000030822     C*
060100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
060200100720     C                   SETOFF                                       505152
060300100720     C                   SETOFF                                       53
060400100413     C                   SELECT
060500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
060600100720     C                   SETON                                        50
060700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
060800100413     C                   SETON                                        51
060900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
061000100720     C                   SETON                                        52
061100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
061200100720     C                   SETON                                        53
061300100413     C                   ENDSL
061400030822     C*
061500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
061600030822     C                   EVAL      posDaDft = 1
061700030822     C                   EVAL      posADft  = 0
061800030822     C                   EVAL      wGiroDft = 0
061900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
062000030822     C                             posDaDft > 0
062100030822     C*
062200030822     C* Gestisco il 1° giro
062300030822     C                   IF        wGiroDft = 0
062400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
062500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
062600030822     C* Incremento il contatore dei "giri"
062700030822     C                   EVAL      wGiroDft = 1
062800030822     C                   ELSE
062900030822     C                   EVAL      posDaDft = posADft
063000030822     C                   ENDIF
063100030822     C* Eseguo lo scan x trovare la fine del campo corrente
063200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
063300030822     C*
063400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
063500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
063600030822     C                   IF        posDaDft > 0 AND
063700030822     C                             posADft  > 0
063800030822     C* NCL
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'NCL'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiInt=*on
064800030822     C                   Z-ADD     PiVal         VABNCL
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CCM
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CCM'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100030822     C                   Z-ADD     PiVal         VABCCM
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* LNP
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'LNP'
066900030822     C                   EVAL      PiStr=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   EXSR      CHKNUM
067300030822     C                   IF        PiInt=*on
067400030822     C                   Z-ADD     PiVal         VABLNP
067500030822     C                   ENDIF
067600030822     C                   ENDIF
067700030822     C* NRS
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'NRS'
068200030822     C                   EVAL      PiStr=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   EXSR      CHKNUM
068600030822     C                   IF        PiInt=*on
068700030822     C                   Z-ADD     PiVal         VABNRS
068800030822     C                   ENDIF
068900030822     C                   ENDIF
069000030822     C* CTR
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'CTR'
069500030822     C                   EVAL      PiStr=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   EXSR      CHKNUM
069900030822     C                   IF        PiInt=*on
070000030822     C                   Z-ADD     PiVal         VABCTR
070100030822     C                   ENDIF
070200030822     C                   ENDIF
070300030822     C* PKB
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'PKB'
070800030822     C                   EVAL      PiStr=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   EXSR      CHKNUM
071200030822     C                   IF        PiNum=*on
071300030822     C                   Z-ADD     PiVal         VABPKB
071400030822     C                   ENDIF
071500030822     C                   ENDIF
071600030822     C* VLB
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VLB'
072100030822     C                   EVAL      PiStr=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   EXSR      CHKNUM
072500030822     C                   IF        PiNum=*on
072600030822     C                   Z-ADD     PiVal         VABVLB
072700030822     C                   ENDIF
072800030822     C                   ENDIF
072900030822     C* QFT
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'QFT'
073400030822     C                   EVAL      PiStr=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   EXSR      CHKNUM
073800030822     C                   IF        PiNum=*on
073900030822     C                   Z-ADD     PiVal         VABQFT
074000030822     C                   ENDIF
074100030822     C                   ENDIF
074200030822     C* CBO
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'CBO'
074700030822     C                   EVAL      VABCBO=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* TSP
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'TSP'
075600030822     C                   EVAL      VABTSP=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* VAS
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'VAS'
076500030822     C                   EVAL      VABVAS=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* VCA
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'VCA'
077400030822     C                   EVAL      VABVCA=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800070925     C* TIC
077900070925     C                   IF        %subst(
078000070925     C                             %subst(vlrppt:posDaDft+1:
078100070925     C                             posADft-posDaDft-1):1:3)
078200070925     C                             = 'TIC'
078300070925     C                   EVAL      VABTIC=%trim(%subst(
078400070925     C                             %subst(vlrppt:posDaDft+1:
078500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600070925     C                   ENDIF
078700030822     C* GCA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GCA'
079200030822     C                   EVAL      VABGCA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* CTM
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'CTM'
080100030822     C                   EVAL      VABCTM=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* FFD
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'FFD'
081000030822     C                   EVAL      VABFFD=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* VAD
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'VAD'
081900030822     C                   EVAL      VABVAD=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* GMA
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'GMA'
082800030822     C                   EVAL      VABGMA=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200030822     C* GGA
083300030822     C                   IF        %subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):1:3)
083600030822     C                             = 'GGA'
083700030822     C                   EVAL      VABGGA=%trim(%subst(
083800030822     C                             %subst(vlrppt:posDaDft+1:
083900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000030822     C                   ENDIF
084100030822     C* GVA
084200030822     C                   IF        %subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):1:3)
084500030822     C                             = 'GVA'
084600030822     C                   EVAL      VABGVA=%trim(%subst(
084700030822     C                             %subst(vlrppt:posDaDft+1:
084800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084900030822     C                   ENDIF
085000030822     C* TC1
085100030822     C                   IF        %subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):1:3)
085400030822     C                             = 'TC1'
085500030822     C                   EVAL      VABTC1=%trim(%subst(
085600030822     C                             %subst(vlrppt:posDaDft+1:
085700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085800030822     C                   ENDIF
085900030822     C* TC2
086000030822     C                   IF        %subst(
086100030822     C                             %subst(vlrppt:posDaDft+1:
086200030822     C                             posADft-posDaDft-1):1:3)
086300030822     C                             = 'TC2'
086400030822     C                   EVAL      VABTC2=%trim(%subst(
086500030822     C                             %subst(vlrppt:posDaDft+1:
086600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086700030822     C                   ENDIF
086800040714     C* VATTRC
086900040714     C                   IF        %subst(
087000040714     C                             %subst(vlrppt:posDaDft+1:
087100040714     C                             posADft-posDaDft-1):1:3)
087200040714     C                             = 'TRC'
087300040714     C                   EVAL      VATTRC=%trim(%subst(
087400040714     C                             %subst(vlrppt:posDaDft+1:
087500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087600040714     C                   ENDIF
087700030822     C* ...
087800030822     C                   ENDIF
087900030822     C                   ENDDO
088000020204     C*
088100010330     C                   ENDSR
088200010607     C*----------------------------------------------------*
088300110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
088400010607     C*----------------------------------------------------*
088500010607     C     IMPVAB        BEGSR
088600010607     C*
088700010607     C                   SETOFF                                       3132
088800110315 xxx C***                SETON                                        35
088900030515     C*
089000010607     C                   MOVEL     datcor        VABAAS
089100010607     C                   MOVE      datcor        VABMGS
089200010607     C*
089300030715     C**********
089400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
089500030715     C**********
089600030715     C                   IF        wGiro = *zeros
089700040113     C*
089800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
089900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
090000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
090100080429     C                             %trim(intcol(5))<>*blanks
090200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
090300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
090400080429     C                                      %trim(intcol(5))
090500070823     C     ';':CharCSV   XLATE     vindta        vindta
090600040510     C                   ENDIF
090700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
090800031201     C     minu:maiu     XLATE     vindta        vindta
090900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091000030715     C                   DOW       posDa <= %len(%trim(vindta))
091100030715     C*
091200030715     C* Gestisco il 1° campo
091300030715     C                   IF        i = 1
091400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091500030715     C                   EVAL      posDa = 1
091600030715     C* Eseguo lo scan x trovare la fine del primo campo
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C* X i campi successivi al 1°
092200030715     C                   ELSE
092300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092400030715     C                   EVAL      posDa = posA + 1
092500030715     C* Eseguo lo scan x trovare la fine del campo corrente
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* Gestisco l'ultimo campo
092800030715     C                   IF        posA = *zeros
092900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   LEAVE
093400030715     C                   ELSE
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   ENDIF
093900030715     C                   ENDIF
094000030715     C* Incremento il contatore d campo
094100030715     C                   EVAL      i = i +1
094200030715     C                   ENDDO
094300030715     C*
094400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094500030715     C* delimitazione testo
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitFLD)
094800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
094900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
095000030715     C                   EVAL      i = i + 1
095100030715     C                   ENDDO
095200030715     C                   EVAL      wGiro = 1
095300030715     C                   ELSE
095400030715     C                   EVAL      wGiro = 2
095500031204     C**********
095600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
095700031204     C* d separatore campo
095800031204     C**********
095900031204     C                   EVAL      vindta = %trim(vindta)
096000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
096100031204     C                   EVAL      vindta = %subst(vindta:2)
096200031204     C                   ENDDO
096300031204     C*
096400031204     C                   Z-ADD     *zeros        lunghInput        4 0
096500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
096700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
096800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
096900110912     C                   ENDDO
097000030715     C**********
097100030715     C* Effettuo lo split del campo dati d input
097200030715     C**********
097300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
097400030715     C                   DOW       posDa <= %len(%trim(vindta))
097500030715     C*
097600030715     C* Gestisco il 1° campo
097700030715     C                   IF        i = 1
097800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
097900030715     C                   EVAL      posDa = 1
098000030715     C* Eseguo lo scan x trovare la fine del primo campo
098100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098200030715     C* A questo punto "estraggo" il campo corrente
098300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098400030715     C                                                    (posA-posDa))
098500030715     C* X i campi successivi al 1°
098600030715     C                   ELSE
098700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
098800030715     C                   EVAL      posDa = posA + 1
098900031204     C* Verifico che nn vi sia il campo nn valorizzato
099000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
099100031204     C* Se campo nn valorizzato skippo al prossimo
099200031204     C                   EVAL      SkSplitCSV(i) = *blanks
099300031204     C                   EVAL      posA  = posA + 1
099400031204     C                   ELSE
099500030715     C* Eseguo lo scan x trovare la fine del campo corrente
099600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
099700030715     C* Gestisco l'ultimo campo
099800030715     C                   IF        posA = *zeros
099900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
100000030715     C* A questo punto "estraggo" il campo corrente
100100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100200030715     C                                                    (posA-posDa))
100300030715     C                   LEAVE
100400030715     C                   ELSE
100500030715     C* A questo punto "estraggo" il campo corrente
100600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
100700030715     C                                                    (posA-posDa))
100800030715     C                   ENDIF
100900031204     C                   ENDIF
101000030715     C                   ENDIF
101100030715     C* Incremento il contatore d campo
101200030715     C                   EVAL      i = i +1
101300030715     C                   ENDDO
101400030715     C*
101500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
101600030715     C* delimitazione testo
101700030715     C                   EVAL      i = 1
101800030715     C                   DOW       i < %elem(SkSplitCSV)
101900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
102000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
102100030715     C                   EVAL      i = i + 1
102200030715     C                   ENDDO
102300100720     C*
102400100720     C* Stacco un progressivo x numero spedizione
102500110930 xxx C                   EXSR      repNSP
102600100326     C*
102700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
102800030715     C                   EVAL      i = 1
102900030715     C                   DOW       i < %elem(SkSplitFLD)
103000030715     C*
103100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
103200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
103300030715     C                             SkSplitCSV(i) <> *blanks
103400030721     C*** ATB
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
103600030721     C                   EVAL      VABATB = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030715     C*** RSD
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
104000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
104100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
104200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
104300030715     C* ==
104400030715     C                   ENDIF
104500030721     C*** RD2
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
104700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030715     C*** IND
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
105100030715     C                   EVAL      VABIND = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** LOD
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
105500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** PRD
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
105900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030721     C*** NZD
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
106300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030715     C*** RMA
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
106700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030715     C*** NOT
107000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
107100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
107200030715     C                   ENDIF
107300030715     C*** NT2
107400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
107500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
107600030715     C                   ENDIF
107700030715     C*** VCA
107800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
107900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
108000030715     C                   ENDIF
108100030715     C*** VAS
108200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
108300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
108400030715     C                   ENDIF
108500030715     C*** FFD
108600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
108700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
108800030715     C                   ENDIF
108900030721     C*** GC1
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
109100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GC2
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
109500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TSP
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
109900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
110000030721     C                   ENDIF
110100040510     C*** CBO
110200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
110300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
110400040510     C                   ENDIF
110500030721     C*** NAS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
110700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** TIC
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
111100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GCA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
111500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** XCO
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
111900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** CTM
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
112300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** TCR
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
112700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** CTS
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
113100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** FTM
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
113500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** VAD
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
113900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** GMA
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
114300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** GGA
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
114700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** GVA
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
115100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** TC1
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
115500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*** TC2
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
115900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
116000030721     C                   ENDIF
116100030721     C*** SCL
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
116300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
116400030721     C                   ENDIF
116500030721     C*** RMO
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
116700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
116800030721     C                   ENDIF
116900030721     C*** NMO
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
117100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
117200030721     C                   ENDIF
117300030721     C*
117400030721     C* Reperisco quindi i campi numerici...
117500040510     C*** NSP
117600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
117700040510     C                   EVAL      PiStr=SkSplitCSV(i)
117800040510     C                   EXSR      CHKNUM
117900040510     C                   IF        PiInt=*on
118000040510     C                   Z-ADD     PiVal         VABNSP
118100040510     C                   ELSE
118200040510     C                   SETON                                        31
118300040510     C                   EVAL      VABNSP = *zeros
118400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040510     C                             + ' ' + 'VABNSP'
118600040510     C                   ENDIF
118700040510     C                   ENDIF
118800071106     C*** RMN
118900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
119000071106     C                   EVAL      PiStr=SkSplitCSV(i)
119100071106     C                   EXSR      CHKNUM
119200071106     C                   IF        PiInt=*on
119300071106     C                   Z-ADD     PiVal         VABRMN
119400071106     C                   ELSE
119500071106     C                   SETON                                        32
119600071106     C                   EVAL      VABRMN = *zeros
119700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
119800071106     C                             + ' ' + 'VABRMN'
119900071106     C                   ENDIF
120000071106     C                   ENDIF
120100030721     C*** CAD
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
120300170919     C* se la nazione non è Italia prendo il CAP alfanumerico, altrimenti numerico
120400170919     C* (siccome la pulizia di IT è dopo, testo anche questo valore)
120500170919     C                   IF        VABNZD <> 'IT'  and VABNZD <> *blank
120600170919     C                   EVAL      VABCAD=SkSplitCSV(i)
120700170919     C                   ELSE
120800170919     C                   EVAL      PiStr=SkSplitCSV(i)
120900170919     C                   EXSR      CHKNUM
121000170919     C                   IF        PiInt=*on
121100170919     C                   Z-ADD     PiVal         Num5_0
121200170919     C                   MOVEL(p)  Num5_0        VABCAD
121300170919     C                   ELSE
121400170919     C                   SETON                                        32
121500170919     C                   EVAL      VABCAD = *zeros
121600170919     C                   EVAL      vinmsg = %trimr(vinmsg)
121700170919     C                             + ' ' + 'VABCAD'
121800170919     C                   ENDIF
121900170919     C                   ENDIF
122000170919     C                   ENDIF
122100030721     C*** NCL
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on
122600030721     C                   Z-ADD     PiVal         VABNCL
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      VABNCL = *zeros
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100040122     C                             + ' ' + 'VABNCL'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** PKB
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiNum=*on
123900170919     C                   Z-ADD(H)  PiVal         VABPKB
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABPKB = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABPKB'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** CAS
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
124900141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
125000141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
125100141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
125200141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
125300141007     C                             %trim(SkSplitCSV(i)) <> ',00'
125400141007     C                   EVAL      VABVCA = 'EUR'
125500170919     C                   EVAL      VABTIC = *blank
125600101018     C                   EVAL      wFlgCAS = '1'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABCAS
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      VABCAS = *zeros
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABCAS'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800170919     C                   ENDIF
126900030721     C*** CCM
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on      AND
127400030721     C                             PiVal<=9999999 AND
127500030721     C                             PiVal>*zeros
127600030721     C                   Z-ADD     PiVal         VABCCM
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      VABCCM = *zeros
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100040510     C                             + ' ' + 'VABCCM'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** LNP
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on    AND
128900030721     C                             PiVal<=999   AND
129000030721     C                             PiVal>*zeros
129100030721     C                   Z-ADD     PiVal         VABLNP
129200030721     C                   ELSE
129300040715     C                   SETON                                        32
129400030721     C                   EVAL      VABLNP = *zeros
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600040510     C                             + ' ' + 'VABLNP'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** NRS
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on AND
130400030721     C                             PiVal<=99
130500030721     C                   Z-ADD     PiVal         VABNRS
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABNRS'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** LNA
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on    AND
131700030721     C                             PiVal<=999   AND
131800030721     C                             PiVal>*zeros
131900030721     C                   Z-ADD     PiVal         VABLNA
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      VABLNA = *zeros
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABLNA'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** CTR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on  AND
133200030721     C                             PiVal<=999
133300030721     C                   Z-ADD     PiVal         VABCTR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABCTR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** IAS
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
134200141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
134300141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
134400141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
134500141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
134600141007     C                             %trim(SkSplitCSV(i)) <> ',00'
134700141007     C                   EVAL      VABVAS = 'EUR'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiNum=*on
135100030721     C                   Z-ADD     PiVal         VABIAS
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABIAS'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800141007     C                   ENDIF
135900030721     C*** VLB
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiNum=*on
136400030721     C                   Z-ADD     PiVal         VABVLB
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABVLB'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** QFT
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiNum=*on
137600030721     C                   Z-ADD     PiVal         VABQFT
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABQFT'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** NCD
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on      AND
138800030721     C                             PiVal<=9999999
138900030721     C                   Z-ADD     PiVal         VABNCD
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABNCD'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** NCA
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on      AND
140100030721     C                             PiVal<=9999999
140200030721     C                   Z-ADD     PiVal         VABNCA
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABNCA'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** ZNC
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on AND
141400030721     C                             PiVal<=99
141500030721     C                   Z-ADD     PiVal         VABZNC
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141900030721     C                             + ' ' + 'VABZNC'
142000030721     C                   ENDIF
142100030721     C                   ENDIF
142200030721     C*** DCR
142300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
142400030721     C                   EVAL      PiStr=SkSplitCSV(i)
142500030721     C                   EXSR      CHKNUM
142600030721     C                   IF        PiInt=*on       AND
142700030721     C                             PiVal<=99999999
142800030721     C                   Z-ADD     PiVal         VABDCR
142900030721     C                   ELSE
143000030721     C                   SETON                                        32
143100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143200030721     C                             + ' ' + 'VABDCR'
143300030721     C                   ENDIF
143400030721     C                   ENDIF
143500030721     C*** HCR
143600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
143700030721     C                   EVAL      PiStr=SkSplitCSV(i)
143800030721     C                   EXSR      CHKNUM
143900030721     C                   IF        PiInt=*on   AND
144000030721     C                             PiVal<=9999
144100030721     C                   Z-ADD     PiVal         VABHCR
144200030721     C                   ELSE
144300030721     C                   SETON                                        32
144400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144500030721     C                             + ' ' + 'VABHCR'
144600030721     C                   ENDIF
144700030721     C                   ENDIF
144800030721     C*** VMD
144900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
145000030721     C                   EVAL      PiStr=SkSplitCSV(i)
145100030721     C                   EXSR      CHKNUM
145200030721     C                   IF        PiNum=*on
145300030721     C                   Z-ADD     PiVal         VABVMD
145400030721     C                   ELSE
145500030721     C                   SETON                                        32
145600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145700030721     C                             + ' ' + 'VABVMD'
145800030721     C                   ENDIF
145900030721     C                   ENDIF
146000030721     C*** ANT
146100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
146200030721     C                   EVAL      PiStr=SkSplitCSV(i)
146300030721     C                   EXSR      CHKNUM
146400030721     C                   IF        PiInt=*on        AND
146500030721     C                             PiVal<=999999999
146600030721     C                   Z-ADD     PiVal         VABANT
146700030721     C                   ELSE
146800030721     C                   SETON                                        32
146900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
147000030721     C                             + ' ' + 'VABANT'
147100030721     C                   ENDIF
147200030721     C                   ENDIF
147300030721     C*** CMO
147400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
147500030721     C                   EVAL      PiStr=SkSplitCSV(i)
147600030721     C                   EXSR      CHKNUM
147700030721     C                   IF        PiInt=*on
147800030721     C                   Z-ADD     PiVal         Num5_0
147900030721     C                   MOVEL(p)  Num5_0        VABCMO
148000030721     C                   ELSE
148100030721     C                   SETON                                        32
148200030721     C                   EVAL      VABCMO = *zeros
148300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
148400030721     C                             + ' ' + 'VABCMO'
148500030721     C                   ENDIF
148600030721     C                   ENDIF
148700040510     C*
148800110930     C* Reperisco i campi del EDIVAT
148900040510     C*** VATNOT
149000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
149100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
149200040510     C                   ENDIF
149300040510     C*** VATTRC
149400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
149500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
149600040510     C                   ENDIF
149700060331     C*** VATNOT_A
149800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
149900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
150000060331     C                   ENDIF
150100060331     C*** VATNOT_B
150200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
150300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
150400060331     C                   ENDIF
150500060331     C*** VATNOT_E
150600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
150700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150800060331     C                   ENDIF
150900110927     C*** VATNOT_I
151000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
151100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
151200110927     C                   ENDIF
151300141007     C*** VATNOT_IJ
151400141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
151500141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
151600141007     C                   ENDIF
151700141007     C*** VATNOT_S
151800141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
151900141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
152000141007     C                   ENDIF
152100070823     C*
152200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
152300170919     C*
152400170919     C*** VATNOT_E/VABRMA/VABRMN
152500170919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
152600170919     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
152700170919     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
152800170919     C* RMN è dalla pos. 3 in poi
152900170919     C                   EVAL      PiStr=%subst(SkSplitCSV(i) : 3 :
153000170919     C                              %len(%trim(SkSplitCSV(i))) - 2)
153100170919     C                   EXSR      CHKNUM
153200170919     C                   IF        PiInt=*on
153300170919     C                   Z-ADD     PiVal         VABRMN
153400170919     C                   ELSE
153500170919     C                   SETON                                        32
153600170919     C                   EVAL      VABRMN = *zeros
153700170919     C                   EVAL      vinmsg = %trimr(vinmsg)
153800170919     C                             + ' ' + 'VABRMN'
153900170919     C                   ENDIF
154000170919     C                   ENDIF
154100170919
154200170919     C*** VATNOT_B/VATNOT_S
154300170919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_B'
154400170919     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
154500170919     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
154600170919     C                   ENDIF
154700170919
154800170919     C*** VABNOT/VABNT2
154900170919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
155000170919     C                   EVAL      w70 = %trim(SkSplitCSV(i))
155100170919     C                   if        %subst(w70:1:35) <> *blanks
155200170919     C                   eval      VABNOT = %trim(%subst(w70:1:35))
155300170919     C                   endif
155400170919     C                   if        %subst(w70:36:35) <> *blanks
155500170919     C                   eval      VABNT2 = %trim(%subst(w70:36:35))
155600170919     C                   endif
155700170919     C                   ENDIF
155800130403
155900130403     C*
156000071029     C***  <===  -----------------------------------------
156100040510     C*
156200030715     C                   ENDIF
156300030715     C                   EVAL      i = i + 1
156400030715     C                   ENDDO
156500070925     C*
156600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
156700121030     C*
156800170919     C* se la nazione è IT la sblanco
156900170919     C                   IF        VABNZD = 'IT'
157000170919     C                   EVAL      VABNZD = *blank
157100170919     C                   ENDIF
157200070925     C*
157300071029     C***  <===  -----------------------------------------
157400070925     C*
157500070925     C* Considerazioni finali su CBO/CAS
157600100318     C                   IF        wFlgCAS = '1'    OR
157700100318     C                             VABCAS  > *zeros
157800130403     C                   IF        VABVCA = *blanks
157900130403     C                   EVAL      VABVCA = 'EUR'
158000130403     C                   ENDIF
158100070925     C                   IF        VABCBO = '1'
158200070925     C                   EVAL      VABCBO = '4'
158300070925     C                   ENDIF
158400070925     C                   IF        VABCBO = '2'
158500070925     C                   EVAL      VABCBO = '6'
158600070925     C                   ENDIF
158700130403     C                   ELSE
158800130403     C                   EVAL      vabVCA = *blank
158900070925     C                   ENDIF
159000070925     C***  <===  ----------------------------
159100040510     C*
159200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
159300040510     C                   IF        VABNSP = *zeros
159400040510     C                   SETON                                        31
159500040510     C                   ENDIF
159600040715     C*
159700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
159800040715     C                   IF        VABLNP = *zeros
159900040715     C                   SETON                                        31
160000040715     C                   ENDIF
160100040510     C*
160200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
160300040510     C                   IF        VABRMN = *zeros
160400040510     C                   Z-ADD     VABNSP        VABRMN
160500040510     C                   ENDIF
160600040510     C*
160700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
160800040510     C                   IF        VABRMA = *blanks
160900040714     C                   MOVEL     VABRMN        VABRMA
161000040510     C                   ENDIF
161100040128     C*
161200040510     C* Se provincia nn valorizzata la reperisco
161300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
161400110912     C                   IF        VABPRD =  *blanks AND
161500110912     C                             VABNZD =  *blanks AND
161600110912     C                             VABCAD <> *blanks AND
161700110912     C                             VABLOD <> *blanks
161800040128     C                   CLEAR                   TISI95DS
161900040128     C                   EVAL      I95TCN = '3'
162000040128     C                   Z-ADD     datcor        I95DAT
162100040128     C                   EVAL      I95NAR = VABNZD
162200040128     C                   EVAL      I95CAP = VABCAD
162300040128     C                   EVAL      I95LOC = VABLOD
162400040128     C                   CALL      'TISI95R'
162500040128     C                   PARM                    TISI95DS
162600040128     C                   EVAL      VABPRD = O95PRV
162700040510     C                   ENDIF
162800020204     C*
162900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
163000020204     C                   EXSR      CHKIMPDIV
163100030715     C                   ENDIF
163200020204     C*
163300010607     C                   ENDSR
163400010607     C*----------------------------------------------------*
163500010601
163600020204
163700020204     C*----------------------------------------------------*
163800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
163900020204     C*----------------------------------------------------*
164000020204     C     CHKIMPDIV     BEGSR
164100020204     C*
164200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
164300020204     C                   Z-ADD     *zeros        wrkDec            9 9
164400020204     C*
164500020204     C* Come prima cosa effettuo considerazioni sulla divisa
164600020204     C                   IF        vabIAS > *zeros
164700020204     C                   IF        vabVAS <> 'EUR'
164800020204     C                   EVAL      vabVAS =  'ITL'
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C                   IF        vabCAS > *zeros
165300020204     C                   IF        vabVCA <> 'EUR'
165400020204     C                   EVAL      vabVCA =  'ITL'
165500020204     C                   ENDIF
165600020204     C                   ENDIF
165700020204     C*
165800020204     C                   IF        vabVMD > *zeros
165900020204     C                   IF        vabVAD <> 'EUR'
166000020204     C                   EVAL      vabVAD =  'ITL'
166100020204     C                   ENDIF
166200020204     C                   ENDIF
166300020204     C*
166400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
166500020204     C                   Z-ADD     vabIAS        wrkDec
166600020204     C                   IF        wrkDec > *zeros
166700020204     C                   IF        vabVAS = 'ITL'
166800020204     C                   EVAL      vabIAS = *zeros
166900020204     C                   ENDIF
167000020204     C                   ENDIF
167100020204     C*
167200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
167300020204     C                   Z-ADD     vabCAS        wrkDec
167400020204     C                   IF        wrkDec > *zeros
167500020204     C                   IF        vabVCA = 'ITL'
167600020204     C                   EVAL      vabCAS = *zeros
167700020204     C                   ENDIF
167800020204     C                   ENDIF
167900020204     C*
168000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
168100020204     C                   Z-ADD     vabVMD        wrkDec
168200020204     C                   IF        wrkDec > *zeros
168300020204     C                   IF        vabVAD = 'ITL'
168400020204     C                   EVAL      vabVMD = *zeros
168500020204     C                   ENDIF
168600020204     C                   ENDIF
168700020204     C*
168800020204     C                   ENDSR
168900020204     C***
169000010330
169100100318
169200100318
169300100318     C*----------------------------------------------------*
169400100318     C*  CONTROLLO NUMERICITA' CAMPI
169500100318     C*----------------------------------------------------*
169600100318     C     CHKNUM        BEGSR
169700100318     C*
169800100318     C                   IF        PiDecChr = *blanks
169900100318     C                   EVAL      PiDecChr = CharNUM
170000100318     C                   ENDIF
170100100318     C*
170200100318     C                   callp     UBISNUM_Check(PiStr
170300100318     C                                          :PiDecChr
170400100318     C                                          :PiVal
170500100318     C                                          :PiNum
170600100318     C                                          :PiInt)
170700100318     C*
170800100318     C                   ENDSR
170900100318     C***
171000010601
171100010601
171200010601
171300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
171400010601     C     repfil        BEGSR
171500010601     C*
171600010601     C                   if        invfil = *zeros and
171700010601     C                             depfil > *zeros and
171800010629     C                             (vinflg = *blanks or
171900010629     C                              vinflg = *zeros)
172000010601     C
172100010601     C                   eval      invfil = depfil
172200010601     C                   endif
172300010601     C*
172400010601     C                   if        depfil <> invfil and
172500010601     C                             invfil > *zeros
172600010601     C                   eval      flgMulti = '1'
172700010601     C                   if        vinflg = *blanks
172800010601     C                   add       1             cntNonEl
172900010601     C                   endif
173000010601     C                   endif
173100010601     C*
173200010601     C                   if        vinflg = '2'
173300010601     C                   eval      flgStato = '2'
173400010601     C                   endif
173500010601     C*
173600010601     C                   ENDSR
173700010601     C***
173800010601
173900010601
174000010601
174100010330
174200040119      /TITLE Invio dei dati al punto operativo.
174300040119     C     invio         BEGSR
174400040119     C*
174500110930     C* 1° invio EDIVAT
174600040119     C                   reset                   dscmz
174700040119     C                   move      vlrpoi        cmzdst
174800110930     C                   eval      cmzfld = 'EDIVATWR'
174900040119     C                   eval      cmzmbd = vlrhdl
175000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
175100040119     C***                if        prmfir = *blanks
175200110930     C                   eval      cmzfla = 'EDIVAT0F'
175300110930     C                   eval      cmzmba = 'EDIVAT0F'
175400040119     C***                else
175500040119     C***                eval      cmzfla = prmfir
175600040119     C***                eval      cmzmba = prmfir
175700040119     C***                endif
175800040119     C                   eval      cmznrr = *zeros
175900040119     C                   move      §ctrokvt      cmznrr
176000040119     C                   eval      cmzlba = vlrfl1
176100040119     C                   call(e)   'TIS711C'
176200040119     C                   parm                    dscmz
176300040119     C                   parm      *blanks       esito
176400040119     C                   if        %error
176500040119     C                             or cmzerr = '1'
176600040119     C                             or esito  = '1'
176700040119     C                   eval      wrkesito = '3'
176800040119     C                   else
176900040119     C*
177000110930     C* 2° invio EDIVAB
177100040119     C                   reset                   dscmz
177200040119     C                   move      vlrpoi        cmzdst
177300040119     C                   eval      cmzfld = vlrfou
177400040119     C                   eval      cmzmbd = vlrhdl
177500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
177600040119     C***                if        prmfir = *blanks
177700110930     C                   eval      cmzfla = 'EDIVAB0F'
177800110930     C                   eval      cmzmba = 'EDIVAB0F'
177900040119     C***                else
178000040119     C***                eval      cmzfla = prmfir
178100040119     C***                eval      cmzmba = prmfir
178200040119     C***                endif
178300040119     C                   eval      cmznrr = *zeros
178400040119     C                   move      §ctrokvb      cmznrr
178500040119     C                   eval      cmzlba = vlrfl1
178600040119     C                   call(e)   'TIS711C'
178700040119     C                   parm                    dscmz
178800040119     C                   parm      *blanks       esito
178900040119     C                   if        %error
179000040119     C                             or cmzerr = '1'
179100040119     C                             or esito  = '1'
179200040119     C                   eval      wrkesito = '3'
179300040119     C                   endif
179400040119     C                   endif
179500040119     C*
179600040119     C                   ENDSR
179700040119     C***
179800010601
179900010601
180000010601
180100010601
180200010601      /TITLE Invio dei dati al punto operativo.
180300010601     C     opeini        BEGSR
180400010601     C*
180500010601     C* Inizializzo flag e contatori operativi
180600010601     C                   movel     '0'           flgGiro           1
180700010601     C                   movel     '0'           flgMulti          1
180800010601     C                   movel     '1'           flgStato          1
180900010615     C                   movel     '0'           flgOk             1
181000010601     C                   z-add     *zeros        cntNonEl         10 0
181100010601     C                   z-add     *zeros        depfil            3 0
181200010601     C                   z-add     *zeros        invfil            3 0
181300010601     C*
181400010601     C                   ENDSR
181500010601     C***
181600070326
181700070326
181800070326
181900070326
182000070326     C     *pssr         BEGSR
182100070329     C*
182200070329     C                   if        %open(tivin00r)
182300070329     C                   close     tivin00r
182400070329     C                   endif
182500110930     C                   if        %open(edivabwr)
182600110930     C                   close     edivabwr
182700070329     C                   endif
182800110930     C                   if        %open(edivatwr)
182900110930     C                   close     edivatwr
183000070329     C                   endif
183100070326     C*
183200070326     C* Effettuo la chiamata al CLLE preposto
183300110930     C                   call(e)   'TITVEVTC'
183400070326     C                   parm                    parccm
183500070326     C                   parm                    parmbr
183600070326     C                   parm      '2'           paropz
183700070326     C*
183800070326     C                   eval      wrkesito = '2'
183900070404     C*
184000070404     C                   seton                                        LR
184100070326     C*
184200070326     C                   ENDSR     '*CANCL'
184300070326     C***
184400100318
184500100318
184600100318
184700100318     C     normCSV       BEGSR
184800100318     C*
184900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
185000100318     C* separatori campi
185100100318     C                   if        CharTXT <> *blanks
185200100318     C                   z-add     *zeros        wLenVINDTA        4 0
185300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
185400100318     C                   setoff                                       80
185500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
185600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
185700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
185800100318     C                   if        not *in80
185900100318     C                   seton                                        80
186000100318     C                   else
186100100318     C                   setoff                                       80
186200100318     C                   endif
186300100318     C                   endif
186400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
186500100318     C                             and *in80 = *on
186600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
186700100318     C                   endif
186800100318     C                   enddo
186900100318     C                   endif
187000100318     C*
187100100318     C                   ENDSR
187200100318     C***
187300070326
187400100326
187500100326
187600100326     C*----------------------------------------------------*
187700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
187800100326     C*----------------------------------------------------*
187900100326     C     REPNSP        BEGSR
188000100326     C*
188100100326     C* NSP => Stacco un numeratore da AZNUM
188200100326     C                   clear                   TRUL33DS
188300100326     C                   eval      I33OPE = *zeros
188400100326     C                   eval      I33CNU = 302
188500100326     C                   eval      I33NUM = 1
188600100326     C                   movel     TRUL33DS      KPJBU
188700100326     C                   call      'TRUL33R'
188800100326     C                   parm                    KPJBA
188900100326     C                   movel     KPJBU         TRUL33DS
189000100326     C                   if        O33ERR = *zeros
189100100326     C                   z-add     O33NRF        VABNSP
189200100326     C                   z-add     O33NRF        VATNSP
189300100326     C                   else
189400100326     C                   SETON                                        31
189500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
189600100326     C                             + ' ' + 'VABNSP VATNSP'
189700100326     C                   endif
189800100326     C*
189900100326     C                   ENDSR
190000010330
190100100326
190200010330
190300000613     C     *inzsr        BEGSR
190400990910     C*
190500990910     C     *entry        plist
190600990920     C                   parm                    tivlrds
190700990921     C                   parm      wrkesito      esito
190800000724     C                   parm                    prmlit
190900000710     C                   parm                    prmfir
191000010330     C*
191100010330     C* CALCOLA LA DATA CORRENTE
191200110930     C                   time                    wn14             14 0
191300110930     C                   movel     wn14          oracor            6 0          *ORA
191400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
191500100318     C                   eval      datcor = %dec(%date() : *ISO)
191600000613     C*
191700000613     C                   ENDSR
191800000613     C***
191900990908
192000120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
192100170919W_VABRMN;VABRSD;W_VATNOT_B;VATNOT_IJ;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VABPKB;VABNCL;VABRMO;
192200170919VABCCM;VABVMD;W_VABNOT;
192300141007
192400141007
192500141007
