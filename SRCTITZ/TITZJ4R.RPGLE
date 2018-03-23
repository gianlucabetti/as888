000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400130122      *
000500170317      * - rottura per RMN e non per NSP
000600170317      * - NSP non viene staccato (per come è articolato il file i VAT prenderebbero degli NSP <> dal
000700170317      * VAB) ma gli vengono associati gli ultimi 7 char dell'RMN
000800170317      *
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001000120209     F*
001100990910     Ftivin00r  uF   E             DISK    usropn
001200110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001300110930     FEDIVATwr  O    E             DISK    usropn
001400000313     D*
001500040113     D*------------
001600040113     D* SCHIEDE A PROGRAMMA
001700040113     D*------------
001800080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600040128     D tisi95ds      e ds
002700110930     D edivabs       e ds                  extname(edivab0f)
002800110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002900110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500040510     D parccm          s              8    INZ(*blanks)
003600040119     D parmbr          s             10    INZ(*blanks)
003700040119     D paropz          s              1    INZ(*blanks)
003800040119     D chkcall         s              1    INZ(*blanks)
003900170317     D depspe          s                   LIKE(VABRMN) INZ(*zeros)
004000080418     D wFlgCAS         s              1    INZ(*blanks)
004100121030     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004200121030     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004300170317     D Intestazioni    s               n   inz(*on)
004400120203     D wNomeFile       s             30    INZ(*blanks)
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300100326
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700030715     D SkSplitFLD      S             10    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200100318     D CharSOS         S              1
006300030715     D posDa           S              3  0 INZ(*zeros)
006400030715     D posA            S              3  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700070823     D wVATNOT_A       s                   LIKE(VATNOT)
006800060331     D wVATNOT_B       s                   LIKE(VATNOT)
006900060331     D wVATNOT_E       s                   LIKE(VATNOT)
007000110927     D wVATNOT_I       s                   LIKE(VATNOT)
007100120202     D wVATNOT_J       s                   LIKE(VATNOT)
007200120202     D wVATNOT_IJ      s             70
007300141007     D wVATNOT_S       s                   LIKE(VATNOT)
007400161102     D wTaglia         s              2
007500161102     D wNumCollo       s              2s 0
007600161102     D wPrgCollo       s              2s 0
007700030822     D*------------------
007800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007900030822     D*------------------
008000030822     D posDaDft        S              3  0 INZ(*zeros)
008100030822     D posADft         S              3  0 INZ(*zeros)
008200030822     D j               s              3  0 INZ(1)
008300030822     D wGiroDft        s              1  0 INZ(*zeros)
008400080418     D*------------------
008500080418     D* Costanti
008600080418     D*------------------
008700080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900100318
009000100318     D*------------------
009100100318     D* LINKING A DEFINIZIONI ESTERNE
009200100318     D*------------------
009300100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009500100318
009600100909     D*------------------
009700100909     D* DS CALCOLO CHECK-DIGITS
009800100909     D*------------------
009900100909     D TRUL28DSX     e ds                  inz
010000010330
010100010330
010200010330
010300000913     C                   reset                   rrnum
010400990921     C                   reset                   esito
010500990921     C                   reset                   wrkesito
010600010601     C*
010700010601     C                   exsr      opeini
010800010605     C                   exsr      rwvab
010900010601     C*
011000010601     C                   seton                                        lr
011100010601
011200010601
011300010601
011400010601     C*--------------------------------------------------------
011500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011600010601     C*--------------------------------------------------------
011700010601     C     PREELA        BEGSR
011800010601     C*
011900010601     C* SE OCCORRE SPEDIRE IN FILIALE
012000010601     C                   if        invfil <> *zeros and
012100010601     C                             flgGiro = '0'
012200010601     C*
012300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012400010601     C                   eval      flgGiro = '1'
012500010601     C*
012600010601     C                   endif
012700010601     C*
012800010601     C                   ENDSR
012900010601     C***
013000010601
013100010601
013200010601
013300010601
013400010601     C*--------------------------------------------------------
013500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013600010601     C*--------------------------------------------------------
013700010601     C     ENDELA        BEGSR
013800040119     C*
013900040119     C* Effettuo la chiamata al CLLE preposto
014000110930     C                   call(e)   'TITVEVTC'
014100040510     C                   parm                    parccm
014200040119     C                   parm                    parmbr
014300040119     C                   parm      '2'           paropz
014400050201     C*
014500050201     C* Effettuo lancio TISI95 solo x chiusura
014600050201     C                   CLEAR                   TISI95DS
014700050201     C                   EVAL      I95TLA = 'C'
014800050201     C                   CALL      'TISI95R'
014900050201     C                   PARM                    TISI95DS
015000000616     C*
015100010601     C                   ENDSR
015200010601
015300010601
015400010330     C*--------------------------------------------------------
015500110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015600010330     C*--------------------------------------------------------
015700010605     C     RWVAB         BEGSR
015800010330     C*
015900010330     C                   if        not %open(tivin00r)
016000010330     C                   open      tivin00r
016100010330     C                   endif
016200110930     C                   if        not %open(edivabwr)
016300110930     C                   open      edivabwr
016400010330     C                   endif
016500040119     C*
016600110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016700040119     C                   exsr      prevat
016800040119     C*
016900040119     C                   if        chkcall = '0'
017000040119     C*
017100110930     C                   if        not %open(edivatwr)
017200110930     C                   open      edivatwr
017300040119     C                   endif
017400010330     C*
017500040119     C                   clear                   §CTROKVB          7 0
017600040119     C                   clear                   §CTROKVT          7 0
017700040119     C                   clear                   §CTRMO            7 0
017800040119     C                   clear                   §CTRNO            7 0
017900121030     C*
018000121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018100121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018200121030     C                   IF        Intestazioni = *off
018300121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018400121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018500121030     C                   exsr      inzvar
018600121030     C                   exsr      impvab
018700121030     C                   ENDIF
018800010330     C*
018900030822     C                   do        *HIVAL
019000010330     C*
019100030822     C                   read      tivin00r                               70
019200010618     C*
019300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019400010618     C                   if        vinflg = '1'
019500010618     C                   eval      flgOk = '1'
019600010618     C                   endif
019700040510     C*
019800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019900040510     C                   z-add     *zeros        wLenVINDTA        4 0
020000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020400040510     C                   z-add     1             wFlgVINDTA
020500040510     C                   leave
020600040510     C                   endif
020700040510     C                   enddo
020800010618     C*
020900040510     C                   if        vindta > *blanks AND
021000040510     C                             wFlgVINDTA = 1
021100010330     C                   add       1             rrnum
021200010330     C*
021300010601     C                   if        *in70 = *off and
021400010330     C                             (vinflg = *blanks
021500010330     C                              or vinflg = '0'
021600010330     C                              or vinflg = '2')
021700010330     C*
021800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021900010711     C                   if        vinflg = *blanks or vinflg = '0'
022000010711     C                   clear                   vinmsg
022100010711     C                   endif
022200010601     C*
022300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022400070925     C                   eval      vindta  = ' ' + %trim(vindta)
022500070925     C*
022600080527     C                   exsr      inzvar
022700100318     C                   exsr      normCSV
022800010605     C                   exsr      impvab
022900010601     C*
023000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023100010605     C                   eval      depfil = VABLNP
023200010601     C                   exsr      repfil
023300010601     C                   if        depfil = invfil
023400021025     C                   if        vlrpoi = 999
023500030822     C                   move(P)   invfil        VABFGS
023600021025     C                   else
023700030822     C                   move(P)   vlrpoi        VABFGS
023800021025     C                   endif
023900010601     C*
024000010601     C                   exsr      PREELA
024100010601     C*
024200010604     C* Ebbene...
024300010604     C*
024400040119     C  N31              add       1             §CTROKVB
024500040119     C   32              add       1             §CTRMO
024600040119     C   31              add       1             §CTRNO
024700080418     C*
024800030822     C                   if        wGiro = 2
024900080513     C* Scrivo sempre il VAT
025000080513     C  N31              exsr      wrivat
025100110201     C                   exsr      chkvab
025200080513     C                   endif
025300010604     C*
025400010604     C                   if        *in31 = *off and
025500010604     C                             *in32 = *off
025600010604     C                   eval      vinflg = '1'
025700010604     C                   else
025800010604     C                   eval      vinflg = '2'
025900010604     C                   endif
026000010604     C                   endif
026100010601     C*
026200010604     C                   endif
026300010604     C*
026400010330     C                   else
026500010330     C                   eval      vinflg = '1'
026600010330     C                   endif
026700010601     C*
026800010601     C  N70              update    tivin000
026900010330     C*
027000030822     C  N70              enddo
027100040119     C*
027200080527     C* Verifico dati rimasti "in cannna"...
027300080527     C                   seton                                        65
027400110201     C                   exsr      chkvab
027500080527     C*
027600040119     C                   endif                                                  (endif - chkcall)
027700010601     C*
027800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027900010601     C                   if        cntNonEl = *zeros or
028000010601     C                             flgMulti = '0'
028100010330     C* Se non ci sono record con errori ...
028200010601     C                   if        §ctrno = 0 and
028300010604     C                             §ctrmo = 0 and
028400010601     C                             flgStato <> '2'
028500010330     C* ... restituisco esito OK.
028600010330     C                   eval      wrkesito = '0'
028700010330     C                   else
028800040119     C                   if        §ctrokvb > 0
028900010330     C                   eval      wrkesito = '1'
029000010330     C                   else
029100010615     C                   if        flgOk = '0'
029200010615     C                   eval      wrkesito = '2'
029300010615     C                   else
029400010615     C                   eval      wrkesito = '6'
029500010615     C                   endif
029600010330     C                   endif
029700010330     C                   endif
029800010601     C                   else
029900010601     C                   eval      wrkesito = '9'
030000010601     C                   endif
030100010330     C*
030200010330     C                   if        %open(tivin00r)
030300010330     C                   close     tivin00r
030400010330     C                   endif
030500110930     C                   if        %open(edivabwr)
030600110930     C                   close     edivabwr
030700010330     C                   endif
030800110930     C                   if        %open(edivatwr)
030900110930     C                   close     edivatwr
031000040512     C                   endif
031100010601     C*
031200010601     C                   if        vlrpoi <> 999
031300010601     C                   eval      invfil = vlrpoi
031400010601     C                   endif
031500010330     C*
031600040119     C                   if        §ctrokvb > 0
031700010601     C                             and invfil > *zeros
031800010330     C                   exsr      invio
031900010330     C                   endif
032000010601     C*
032100010618     C                   if        flgGiro = '1'
032200010601     C                   exsr      endela
032300010618     C                   endif
032400010330     C*
032500010330     C                   ENDSR
032600010330     C***
032700080527
032800080527
032900080527
033000080527     C*----------------------------------------------------*
033100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033200080527     C*----------------------------------------------------*
033300100413     C     CHKVAB        BEGSR
033400110201     C*
033500110201     C* Se richiesti ignorare colli / peso / volume
033600110315 xxx C***                if        *in35
033700120203 xxx C***                z-add     *zeros        VABNCL              F0461
033800120203 xxx C***                z-add     *zF046190081  VABPKB
033900110315 xxx C***                z-add     *zeros        VABVLB
034000110315 xxx C***                endif
034100080527     C*
034200170317     C*** Scrivo il VAB solo a rottura d spedizione
034300170317     C* in questo caso Scrivo il VAB solo a rottura di RMN
034400080527     C                   if        depspe = *zeros
034500110930     C  N31              eval      edivabs_sav = edivabs
034600170317     C                   eval      depspe = vabRMN
034700080527     C                   else
034800170317     C                   if        depspe = vabRMN AND *in65 = *off
034900100413     C   51              add       VABNCL        sav_VABNCL
035000100413     C   51              add       VABPKB        sav_VABPKB
035100100413     C   51              add       VABVLB        sav_VABVLB
035200100720     C   52              z-add     VABNCL        sav_VABNCL
035300100720     C   52              z-add     VABPKB        sav_VABPKB
035400100720     C   52              z-add     VABVLB        sav_VABVLB
035500100720     C   53              add       VABNCL        sav_VABNCL
035600100720     C   53              z-add     VABPKB        sav_VABPKB
035700100720     C   53              z-add     VABVLB        sav_VABVLB
035800080527     C                   else
035900120203     C  N31              eval      edivabs_ok = edivabs_sav
036000080527     C  N31              exsr      wrivab
036100110930     C  N31              eval      edivabs_sav = edivabs
036200170317     C                   eval      depspe = vabRMN
036300080527     C                   endif
036400080527     C                   endif
036500080527     C*
036600080527     C                   ENDSR
036700040119
036800040119
036900040119
037000040119     C*----------------------------------------------------*
037100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037200040119     C*----------------------------------------------------*
037300040510     C     WRIVAB        BEGSR
037400080513     C*
037500120203     C* EDIVABWR ha il prefisso "ok_"
037600120203     C*
037700110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037800120203     C                   MOVE(P)   vlrMSG        wNomeFile
037900130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
038000120203     C                   EVAL      ok_VABDCM = DATCOR
038100120203     C                   EVAL      ok_VABDTS = DATCOR
038200120203     C                   EVAL      ok_VABHMS = ORACOR
038300120203     C                   EVAL      ok_VABCNT = 1
038400110930     C*
038500110930     C                   write     edivab00
038600170317     C*
038700040119     C                   ENDSR
038800040119
038900040119
039000040119
039100040119     C*----------------------------------------------------*
039200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039300040119     C*----------------------------------------------------*
039400040510     C     WRIVAT        BEGSR
039500060512     C*
039600060512     C* Inizializzo i campi chiave
039700060512     C                   eval      VATFGS = VABFGS
039800060512     C                   eval      VATAAS = VABAAS
039900060512     C                   eval      VATCCM = VABCCM
040000070823     C                   eval      VATNRS = VABNRS
040100060512     C                   eval      VATNSP = VABNSP
040200060512     C                   eval      VATLNP = VABLNP
040300040119     C*
040400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040500040510     C                   if        VATTRC = *blanks
040600040510     C                   eval      VATTRC = 'E'
040700040510     C                   endif
040800110318     C                   if        VATNOT <> *blanks
040900120203     C                   MOVE(P)   vlrMSG        wNomeFile
041000130122     C***                EVAL      VATCMR = wNomeFile
041100141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
041200110930     C                   eval      VATCNT = 1
041300110930     C                   write     EDIVAT00
041400040119     C                   add       1             §CTROKVT
041500110318     C                   endif
041600060331     C*
041700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041800060331     C                   if        wVATNOT_A <> *blanks
041900060331     C                   eval      VATNOT = wVATNOT_A
042000060331     C                   eval      VATTRC = 'A'
042100120203     C                   MOVE(P)   vlrMSG        wNomeFile
042200130122     C***                EVAL      VATCMR = wNomeFile
042300141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
042400110930     C                   eval      VATCNT = 1
042500110930     C                   write     EDIVAT00
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800060331     C*
042900060331     C                   if        wVATNOT_B <> *blanks
043000060331     C                   eval      VATNOT = wVATNOT_B
043100060331     C                   eval      VATTRC = 'B'
043200120203     C                   MOVE(P)   vlrMSG        wNomeFile
043300130122     C***                EVAL      VATCMR = wNomeFile
043400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
043500110930     C                   eval      VATCNT = 1
043600110930     C                   write     EDIVAT00
043700060331     C                   add       1             §CTROKVT
043800060331     C                   endif
043900141007     C*
044000141007     C                   if        wVATNOT_IJ <> *blanks
044100141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044200141007     C                   eval      VATTRC = 'I'
044300141007     C***                EVAL      VATCMR = wNomeFile
044400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044500141007     C                   eval      VATCNT = 1
044600141007     C                   write     EDIVAT00
044700141007     C                   add       1             §CTROKVT
044800141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
044900141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045000141007     C                   eval      VATTRC = 'J'
045100141007     C***                EVAL      VATCMR = wNomeFile
045200141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
045300141007     C                   eval      VATCNT = 1
045400141007     C                   write     EDIVAT00
045500141007     C                   endif
045600141007     C                   endif
045700060331     C*
045800060331     C                   if        wVATNOT_E <> *blanks
045900060331     C                   eval      VATTRC = 'E'
046000121030     C                   MOVE(P)   vlrMSG        wNomeFile
046100130122     C***                EVAL      VATCMR = wNomeFile
046200141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
046300121030     C                   eval      VATCNT = 1
046400141007     C*
046500170307     C* i "CHI SONO" non vanno sviluppati
046600170307     C                   eval      VATNOT = %trim(wVATNOT_E)
046700141007     C                   write     EDIVAT00
046800161102     C                   ENDIF
046900161102     C*
047000110927     C                   if        wVATNOT_I <> *blanks
047100110927     C                   eval      VATNOT = wVATNOT_I
047200110927     C                   eval      VATTRC = 'I'
047300120203     C                   MOVE(P)   vlrMSG        wNomeFile
047400130122     C***                EVAL      VATCMR = wNomeFile
047500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
047600110930     C                   eval      VATCNT = 1
047700110930     C                   write     EDIVAT00
047800110927     C                   add       1             §CTROKVT
047900110927     C                   endif
048000120202     C*
048100120202     C                   if        wVATNOT_J <> *blanks
048200120202     C                   eval      VATNOT = wVATNOT_J
048300120202     C                   eval      VATTRC = 'J'
048400120203     C                   MOVE(P)   vlrMSG        wNomeFile
048500130122     C***                EVAL      VATCMR = wNomeFile
048600141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048700120202     C                   eval      VATCNT = 1
048800120202     C                   write     EDIVAT00
048900120202     C                   add       1             §CTROKVT
049000120202     C                   endif
049100141007     C*
049200141007     C                   if        wVATNOT_S <> *blanks
049300141007     C                   eval      VATNOT = wVATNOT_S
049400141007     C* i 2 flag devono essere blank
049500141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
049600141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
049700141007     C                   eval      VATTRC = 'S'
049800141007     C***                EVAL      VATCMR = wNomeFile
049900141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
050000141007     C                   eval      VATCNT = 1
050100141007     C                   write     EDIVAT00
050200141007     C                   add       1             §CTROKVT
050300141007     C                   endif
050400040119     C*
050500040119     C                   ENDSR
050600040119
050700040119
050800040119
050900040119     C*----------------------------------------------------*
051000110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051100040119     C*----------------------------------------------------*
051200040119     C     PREVAT        BEGSR
051300040119     C*
051400110930     C* Compongo il nome del membro da dare al EDIVATWR
051500040119     C                   eval      parmbr = vlrhdl
051600040119     C                   movel     'M'           parmbr
051700040510     C                   eval      parccm = vlrksc
051800040119     C                   eval      paropz = '1'
051900040119     C* Effettuo la chiamata al CLLE preposto
052000110930     C                   call(e)   'TITVEVTC'
052100040119     C                   parm                    parccm
052200040119     C                   parm                    parmbr
052300040119     C                   parm                    paropz
052400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052500040119     C                   if        %error
052600040119     C                   movel     '1'           chkcall
052700040119     C                   else
052800040119     C                   movel     '0'           chkcall
052900040119     C                   endif
053000040119     C*
053100040119     C                   ENDSR
053200010601
053300010601
053400010601
053500010330     C*----------------------------------------------------*
053600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053700010330     C*----------------------------------------------------*
053800040119     C     INZVAR        BEGSR
053900080527     C*
054000080527     C                   setoff                                       65
054100010330     C*
054200030822     C* Inizializzo variabili di wrk
054300130128     C                   Z-ADD     *zeros        Num3_0            3 0
054400020204     C                   Z-ADD     *zeros        Num5_0            5 0
054500101018     C                   MOVEL     *blanks       wFlgCAS
054600060331     C                   MOVEL     *blanks       wVATNOT_A
054700060331     C                   MOVEL     *blanks       wVATNOT_B
054800060331     C                   MOVEL     *blanks       wVATNOT_E
054900110927     C                   MOVEL     *blanks       wVATNOT_I
055000120202     C                   MOVEL     *blanks       wVATNOT_J
055100120202     C                   MOVEL     *blanks       wVATNOT_IJ
055200141007     C                   MOVEL     *blanks       wVATNOT_S
055300161102     C                   MOVEL     *blanks       wTaglia
055400030822     C*
055500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055600030822     C                   CLEAR                   SkSplitCSV
055700030822     C                   EVAL      i = 1
055800030822     C                   EVAL      posDa = *zeros
055900030822     C                   EVAL      posA  = *zeros
056000080527     C*
056100110930     C                   CLEAR                   EDIVABS
056200110930     C                   CLEAR                   EDIVAT00
056300030822     C*
056400030822     C* Reimposto i valori di default
056500030822     C                   EXSR      DEFCAM
056600030822     C*
056700010330     C                   ENDSR
056800010330     C*----------------------------------------------------*
056900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057000010330     C*----------------------------------------------------*
057100010330     C     DEFCAM        BEGSR
057200030822     C*
057300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057400030715     C* e delimitatore testo.
057500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057800100318     C*
057900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058000100318     C                   EVAL      CharSOS = CharNUM
058100030822     C*
058200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058300100720     C                   SETOFF                                       505152
058400100720     C                   SETOFF                                       53
058500100413     C                   SELECT
058600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058700100720     C                   SETON                                        50
058800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058900100413     C                   SETON                                        51
059000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059100100720     C                   SETON                                        52
059200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059300100720     C                   SETON                                        53
059400100413     C                   ENDSL
059500030822     C*
059600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059700030822     C                   EVAL      posDaDft = 1
059800030822     C                   EVAL      posADft  = 0
059900030822     C                   EVAL      wGiroDft = 0
060000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060100030822     C                             posDaDft > 0
060200030822     C*
060300030822     C* Gestisco il 1° giro
060400030822     C                   IF        wGiroDft = 0
060500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060700030822     C* Incremento il contatore dei "giri"
060800030822     C                   EVAL      wGiroDft = 1
060900030822     C                   ELSE
061000030822     C                   EVAL      posDaDft = posADft
061100030822     C                   ENDIF
061200030822     C* Eseguo lo scan x trovare la fine del campo corrente
061300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061400030822     C*
061500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061700030822     C                   IF        posDaDft > 0 AND
061800030822     C                             posADft  > 0
061900030822     C* NCL
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'NCL'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABNCL
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* CCM
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'CCM'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABCCM
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* LNP
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'LNP'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiInt=*on
065500030822     C                   Z-ADD     PiVal         VABLNP
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* NRS
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'NRS'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiInt=*on
066800030822     C                   Z-ADD     PiVal         VABNRS
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* CTR
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'CTR'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiInt=*on
068100030822     C                   Z-ADD     PiVal         VABCTR
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* PKB
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'PKB'
068900030822     C                   EVAL      PiStr=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   EXSR      CHKNUM
069300030822     C                   IF        PiNum=*on
069400030822     C                   Z-ADD     PiVal         VABPKB
069500030822     C                   ENDIF
069600030822     C                   ENDIF
069700030822     C* VLB
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'VLB'
070200030822     C                   EVAL      PiStr=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   EXSR      CHKNUM
070600030822     C                   IF        PiNum=*on
070700030822     C                   Z-ADD     PiVal         VABVLB
070800030822     C                   ENDIF
070900030822     C                   ENDIF
071000030822     C* QFT
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'QFT'
071500030822     C                   EVAL      PiStr=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   EXSR      CHKNUM
071900030822     C                   IF        PiNum=*on
072000030822     C                   Z-ADD     PiVal         VABQFT
072100030822     C                   ENDIF
072200030822     C                   ENDIF
072300030822     C* CBO
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'CBO'
072800030822     C                   EVAL      VABCBO=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TSP
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TSP'
073700030822     C                   EVAL      VABTSP=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* VAS
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'VAS'
074600030822     C                   EVAL      VABVAS=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* VCA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'VCA'
075500030822     C                   EVAL      VABVCA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900070925     C* TIC
076000070925     C                   IF        %subst(
076100070925     C                             %subst(vlrppt:posDaDft+1:
076200070925     C                             posADft-posDaDft-1):1:3)
076300070925     C                             = 'TIC'
076400070925     C                   EVAL      VABTIC=%trim(%subst(
076500070925     C                             %subst(vlrppt:posDaDft+1:
076600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700070925     C                   ENDIF
076800030822     C* GCA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GCA'
077300030822     C                   EVAL      VABGCA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* CTM
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'CTM'
078200030822     C                   EVAL      VABCTM=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* FFD
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'FFD'
079100030822     C                   EVAL      VABFFD=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* VAD
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'VAD'
080000030822     C                   EVAL      VABVAD=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400030822     C* GMA
080500030822     C                   IF        %subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):1:3)
080800030822     C                             = 'GMA'
080900030822     C                   EVAL      VABGMA=%trim(%subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200030822     C                   ENDIF
081300030822     C* GGA
081400030822     C                   IF        %subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):1:3)
081700030822     C                             = 'GGA'
081800030822     C                   EVAL      VABGGA=%trim(%subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100030822     C                   ENDIF
082200030822     C* GVA
082300030822     C                   IF        %subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):1:3)
082600030822     C                             = 'GVA'
082700030822     C                   EVAL      VABGVA=%trim(%subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083000030822     C                   ENDIF
083100030822     C* TC1
083200030822     C                   IF        %subst(
083300030822     C                             %subst(vlrppt:posDaDft+1:
083400030822     C                             posADft-posDaDft-1):1:3)
083500030822     C                             = 'TC1'
083600030822     C                   EVAL      VABTC1=%trim(%subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083900030822     C                   ENDIF
084000030822     C* TC2
084100030822     C                   IF        %subst(
084200030822     C                             %subst(vlrppt:posDaDft+1:
084300030822     C                             posADft-posDaDft-1):1:3)
084400030822     C                             = 'TC2'
084500030822     C                   EVAL      VABTC2=%trim(%subst(
084600030822     C                             %subst(vlrppt:posDaDft+1:
084700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084800030822     C                   ENDIF
084900040714     C* VATTRC
085000040714     C                   IF        %subst(
085100040714     C                             %subst(vlrppt:posDaDft+1:
085200040714     C                             posADft-posDaDft-1):1:3)
085300040714     C                             = 'TRC'
085400040714     C                   EVAL      VATTRC=%trim(%subst(
085500040714     C                             %subst(vlrppt:posDaDft+1:
085600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085700040714     C                   ENDIF
085800030822     C* ...
085900030822     C                   ENDIF
086000030822     C                   ENDDO
086100020204     C*
086200010330     C                   ENDSR
086300010607     C*----------------------------------------------------*
086400110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086500010607     C*----------------------------------------------------*
086600010607     C     IMPVAB        BEGSR
086700010607     C*
086800010607     C                   SETOFF                                       3132
086900110315 xxx C***                SETON                                        35
087000030515     C*
087100010607     C                   MOVEL     datcor        VABAAS
087200010607     C                   MOVE      datcor        VABMGS
087300010607     C*
087400030715     C**********
087500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087600030715     C**********
087700030715     C                   IF        wGiro = *zeros
087800040113     C*
087900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088200080429     C                             %trim(intcol(5))<>*blanks
088300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088500080429     C                                      %trim(intcol(5))
088600070823     C     ';':CharCSV   XLATE     vindta        vindta
088700040510     C                   ENDIF
088800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088900031201     C     minu:maiu     XLATE     vindta        vindta
089000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089100030715     C                   DOW       posDa <= %len(%trim(vindta))
089200030715     C*
089300030715     C* Gestisco il 1° campo
089400030715     C                   IF        i = 1
089500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089600030715     C                   EVAL      posDa = 1
089700030715     C* Eseguo lo scan x trovare la fine del primo campo
089800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C* X i campi successivi al 1°
090300030715     C                   ELSE
090400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090500030715     C                   EVAL      posDa = posA + 1
090600030715     C* Eseguo lo scan x trovare la fine del campo corrente
090700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090800030715     C* Gestisco l'ultimo campo
090900030715     C                   IF        posA = *zeros
091000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   LEAVE
091500030715     C                   ELSE
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C                   ENDIF
092000030715     C                   ENDIF
092100030715     C* Incremento il contatore d campo
092200030715     C                   EVAL      i = i +1
092300030715     C                   ENDDO
092400030715     C*
092500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092600030715     C* delimitazione testo
092700030715     C                   EVAL      i = 1
092800030715     C                   DOW       i < %elem(SkSplitFLD)
092900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093100030715     C                   EVAL      i = i + 1
093200030715     C                   ENDDO
093300030715     C                   EVAL      wGiro = 1
093400030715     C                   ELSE
093500030715     C                   EVAL      wGiro = 2
093600031204     C**********
093700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093800031204     C* d separatore campo
093900031204     C**********
094000031204     C                   EVAL      vindta = %trim(vindta)
094100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094200031204     C                   EVAL      vindta = %subst(vindta:2)
094300031204     C                   ENDDO
094400031204     C*
094500031204     C                   Z-ADD     *zeros        lunghInput        4 0
094600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095000110912     C                   ENDDO
095100030715     C**********
095200030715     C* Effettuo lo split del campo dati d input
095300030715     C**********
095400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095500030715     C                   DOW       posDa <= %len(%trim(vindta))
095600030715     C*
095700030715     C* Gestisco il 1° campo
095800030715     C                   IF        i = 1
095900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096000030715     C                   EVAL      posDa = 1
096100030715     C* Eseguo lo scan x trovare la fine del primo campo
096200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C* X i campi successivi al 1°
096700030715     C                   ELSE
096800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096900030715     C                   EVAL      posDa = posA + 1
097000031204     C* Verifico che nn vi sia il campo nn valorizzato
097100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097200031204     C* Se campo nn valorizzato skippo al prossimo
097300031204     C                   EVAL      SkSplitCSV(i) = *blanks
097400031204     C                   EVAL      posA  = posA + 1
097500031204     C                   ELSE
097600030715     C* Eseguo lo scan x trovare la fine del campo corrente
097700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097800030715     C* Gestisco l'ultimo campo
097900030715     C                   IF        posA = *zeros
098000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098100030715     C* A questo punto "estraggo" il campo corrente
098200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098300030715     C                                                    (posA-posDa))
098400030715     C                   LEAVE
098500030715     C                   ELSE
098600030715     C* A questo punto "estraggo" il campo corrente
098700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098800030715     C                                                    (posA-posDa))
098900030715     C                   ENDIF
099000031204     C                   ENDIF
099100030715     C                   ENDIF
099200030715     C* Incremento il contatore d campo
099300030715     C                   EVAL      i = i +1
099400030715     C                   ENDDO
099500030715     C*
099600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099700030715     C* delimitazione testo
099800030715     C                   EVAL      i = 1
099900030715     C                   DOW       i < %elem(SkSplitCSV)
100000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100200030715     C                   EVAL      i = i + 1
100300030715     C                   ENDDO
100400170317     C*
100500170317 xxx C***                EXSR      repNSP
100600100326     C*
100700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100800030715     C                   EVAL      i = 1
100900030715     C                   DOW       i < %elem(SkSplitFLD)
101000030715     C*
101100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101300030715     C                             SkSplitCSV(i) <> *blanks
101400030721     C*** ATB
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101600030721     C                   EVAL      VABATB = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030715     C*** RSD
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102300030715     C* ==
102400030715     C                   ENDIF
102500030721     C*** RD2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030715     C*** IND
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103100030715     C                   EVAL      VABIND = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** LOD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** PRD
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030721     C*** NZD
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030715     C*** RMA
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NOT
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** NT2
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VCA
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** VAS
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030715     C*** FFD
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030721     C*** GC1
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GC2
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** TSP
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108000030721     C                   ENDIF
108100040510     C*** CBO
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108400040510     C                   ENDIF
108500030721     C*** NAS
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TIC
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GCA
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** XCO
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** CTM
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** TCR
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** CTS
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** FTM
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** VAD
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GMA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GGA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** GVA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC1
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** TC2
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** SCL
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** RMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** NMO
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*
115400030721     C* Reperisco quindi i campi numerici...
115500040510     C*** NSP
115600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115700040510     C                   EVAL      PiStr=SkSplitCSV(i)
115800040510     C                   EXSR      CHKNUM
115900040510     C                   IF        PiInt=*on
116000040510     C                   Z-ADD     PiVal         VABNSP
116100040510     C                   ELSE
116200040510     C                   SETON                                        31
116300040510     C                   EVAL      VABNSP = *zeros
116400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040510     C                             + ' ' + 'VABNSP'
116600040510     C                   ENDIF
116700040510     C                   ENDIF
116800071106     C*** RMN
116900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117000071106     C                   EVAL      PiStr=SkSplitCSV(i)
117100071106     C                   EXSR      CHKNUM
117200071106     C                   IF        PiInt=*on
117300071106     C                   Z-ADD     PiVal         VABRMN
117400071106     C                   ELSE
117500071106     C                   SETON                                        32
117600071106     C                   EVAL      VABRMN = *zeros
117700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117800071106     C                             + ' ' + 'VABRMN'
117900071106     C                   ENDIF
118000071106     C                   ENDIF
118100030721     C*** CAD
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on
118600030721     C                   Z-ADD     PiVal         Num5_0
118700030721     C                   MOVEL(p)  Num5_0        VABCAD
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      VABCAD = *zeros
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABCAD'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** NCL
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on
120000030721     C                   Z-ADD     PiVal         VABNCL
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      VABNCL = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500040122     C                             + ' ' + 'VABNCL'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** PKB
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300030721     C                   Z-ADD     PiVal         VABPKB
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      VABPKB = *zeros
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABPKB'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** CAS
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122300141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122400141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122500141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122600141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122700141007     C                             %trim(SkSplitCSV(i)) <> ',00'
122800141007     C                   EVAL      VABVCA = 'EUR'
122900101018     C                   EVAL      wFlgCAS = '1'
123000101018     C                   ENDIF
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABCAS
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABCAS = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABCAS'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** CCM
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on      AND
124700030721     C                             PiVal<=9999999 AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABCCM
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABCCM = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040510     C                             + ' ' + 'VABCCM'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** LNP
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on    AND
126200030721     C                             PiVal<=999   AND
126300030721     C                             PiVal>*zeros
126400030721     C                   Z-ADD     PiVal         VABLNP
126500030721     C                   ELSE
126600040715     C                   SETON                                        32
126700030721     C                   EVAL      VABLNP = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900040510     C                             + ' ' + 'VABLNP'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** NRS
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on AND
127700030721     C                             PiVal<=99
127800030721     C                   Z-ADD     PiVal         VABNRS
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABNRS'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** LNA
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on    AND
129000030721     C                             PiVal<=999   AND
129100030721     C                             PiVal>*zeros
129200030721     C                   Z-ADD     PiVal         VABLNA
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      VABLNA = *zeros
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABLNA'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** CTR
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on  AND
130500030721     C                             PiVal<=999
130600030721     C                   Z-ADD     PiVal         VABCTR
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABCTR'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** IAS
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131500141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131600141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131700141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131800141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
131900141007     C                             %trim(SkSplitCSV(i)) <> ',00'
132000141007     C                   EVAL      VABVAS = 'EUR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiNum=*on
132400030721     C                   Z-ADD     PiVal         VABIAS
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABIAS'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100141007     C                   ENDIF
133200030721     C*** VLB
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABVLB
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABVLB'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** QFT
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900030721     C                   Z-ADD     PiVal         VABQFT
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABQFT'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** NCD
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on      AND
136100030721     C                             PiVal<=9999999
136200030721     C                   Z-ADD     PiVal         VABNCD
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABNCD'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** NCA
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on      AND
137400030721     C                             PiVal<=9999999
137500030721     C                   Z-ADD     PiVal         VABNCA
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABNCA'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** ZNC
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on AND
138700030721     C                             PiVal<=99
138800030721     C                   Z-ADD     PiVal         VABZNC
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABZNC'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** DCR
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiInt=*on       AND
140000030721     C                             PiVal<=99999999
140100030721     C                   Z-ADD     PiVal         VABDCR
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABDCR'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800030721     C*** HCR
140900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141000030721     C                   EVAL      PiStr=SkSplitCSV(i)
141100030721     C                   EXSR      CHKNUM
141200030721     C                   IF        PiInt=*on   AND
141300030721     C                             PiVal<=9999
141400030721     C                   Z-ADD     PiVal         VABHCR
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABHCR'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030721     C*** VMD
142200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142300030721     C                   EVAL      PiStr=SkSplitCSV(i)
142400030721     C                   EXSR      CHKNUM
142500030721     C                   IF        PiNum=*on
142600030721     C                   Z-ADD     PiVal         VABVMD
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000030721     C                             + ' ' + 'VABVMD'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300030721     C*** ANT
143400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143500030721     C                   EVAL      PiStr=SkSplitCSV(i)
143600030721     C                   EXSR      CHKNUM
143700030721     C                   IF        PiInt=*on        AND
143800030721     C                             PiVal<=999999999
143900030721     C                   Z-ADD     PiVal         VABANT
144000030721     C                   ELSE
144100030721     C                   SETON                                        32
144200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144300030721     C                             + ' ' + 'VABANT'
144400030721     C                   ENDIF
144500030721     C                   ENDIF
144600030721     C*** CMO
144700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144800030721     C                   EVAL      PiStr=SkSplitCSV(i)
144900030721     C                   EXSR      CHKNUM
145000030721     C                   IF        PiInt=*on
145100030721     C                   Z-ADD     PiVal         Num5_0
145200030721     C                   MOVEL(p)  Num5_0        VABCMO
145300030721     C                   ELSE
145400030721     C                   SETON                                        32
145500030721     C                   EVAL      VABCMO = *zeros
145600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145700030721     C                             + ' ' + 'VABCMO'
145800030721     C                   ENDIF
145900030721     C                   ENDIF
146000040510     C*
146100110930     C* Reperisco i campi del EDIVAT
146200040510     C*** VATNOT
146300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146500040510     C                   ENDIF
146600040510     C*** VATTRC
146700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146900040510     C                   ENDIF
147000060331     C*** VATNOT_A
147100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147300060331     C                   ENDIF
147400060331     C*** VATNOT_B
147500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147700060331     C                   ENDIF
147800060331     C*** VATNOT_E
147900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148100060331     C                   ENDIF
148200110927     C*** VATNOT_I
148300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148500110927     C                   ENDIF
148600141007     C*** VATNOT_IJ
148700141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148800141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148900141007     C                   ENDIF
149000141007     C*** VATNOT_S
149100141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149200141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149300141007     C                   ENDIF
149400070823     C*
149500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149600130403
149700170317     C*
149800170317     C*** RMN / NSP
149900170317     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
150000170317     C                   EVAL      PiStr=SkSplitCSV(i)
150100170317     C                   EXSR      CHKNUM
150200170317     C                   IF        PiInt=*on
150300170317     C                   Z-ADD     PiVal         VABRMN
150400170317     c                   eval      VABNSP = %dec(%subst(
150500170317     c                                       %editc(VABRMN:'X')
150600170317     c                                       :9:7) : 7 : 0)
150700170317     C                   ELSE
150800170317     C                   SETON                                        32
150900170317     C                   EVAL      VABRMN = *zeros
151000170317     C                   EVAL      VABNSP = *zeros
151100170317     C                   EVAL      vinmsg = %trimr(vinmsg)
151200170317     C                             + ' ' + 'VABRMN VABNSP'
151300170317     C                   ENDIF
151400170317     C                   ENDIF
151500130403     C*
151600170317     C*** CBO
151700170317     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
151800170901     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
151900170901     C     minu:maiu     XLATE     SkSplitCSV(i) SkSplitCSV(i)
152000170317     C                   SELECT
152100170901     C                   WHEN      SkSplitCSV(i) = 'FRANCO CON ADDEBITO'
152200170317     C                   EVAL      VABCBO = '1'
152300170901     C                   WHEN      SkSplitCSV(i) = 'ASSEGNATO'
152400170317     C                   EVAL      VABCBO = '2'
152500170317     C                   WHEN      SkSplitCSV(i) =
152600170901     C                              'FRANCO CON ADDEBITO+IMPORTO C/ASSEGNO'
152700170317     C                   EVAL      VABCBO = '4'
152800170901     C                   WHEN      SkSplitCSV(i) = 'ASSEGNATO+IMPORTO C/ASSEGNO'
152900170317     C                   EVAL      VABCBO = '6'
153000170317     C                   ENDSL
153100170317     C                   ENDIF
153200040510     C*
153300030715     C                   ENDIF
153400030715     C                   EVAL      i = i + 1
153500030715     C                   ENDDO
153600170317     C*
153700170317     C***  <===  -----------------------------------------
153800170317     C*
153900170317
154000070925     C*
154100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
154200121030     C*
154300070925     C*
154400071029     C***  <===  -----------------------------------------
154500070925     C*
154600070925     C* Considerazioni finali su CBO/CAS
154700100318     C                   IF        wFlgCAS = '1'    OR
154800100318     C                             VABCAS  > *zeros
154900130403     C                   IF        VABVCA = *blanks
155000130403     C                   EVAL      VABVCA = 'EUR'
155100130403     C                   EVAL      VABTIC = *blanks
155200130403     C                   ENDIF
155300070925     C                   IF        VABCBO = '1'
155400070925     C                   EVAL      VABCBO = '4'
155500070925     C                   ENDIF
155600070925     C                   IF        VABCBO = '2'
155700070925     C                   EVAL      VABCBO = '6'
155800070925     C                   ENDIF
155900130403     C                   ELSE
156000130403     C                   EVAL      vabVCA = *blank
156100070925     C                   ENDIF
156200070925     C***  <===  ----------------------------
156300040510     C*
156400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156500040510     C                   IF        VABNSP = *zeros
156600040510     C                   SETON                                        31
156700040510     C                   ENDIF
156800040715     C*
156900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
157000040715     C                   IF        VABLNP = *zeros
157100040715     C                   SETON                                        31
157200040715     C                   ENDIF
157300040510     C*
157400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157500040510     C                   IF        VABRMN = *zeros
157600040510     C                   Z-ADD     VABNSP        VABRMN
157700040510     C                   ENDIF
157800040510     C*
157900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
158000040510     C                   IF        VABRMA = *blanks
158100040714     C                   MOVEL     VABRMN        VABRMA
158200040510     C                   ENDIF
158300040128     C*
158400040510     C* Se provincia nn valorizzata la reperisco
158500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158600110912     C                   IF        VABPRD =  *blanks AND
158700110912     C                             VABNZD =  *blanks AND
158800110912     C                             VABCAD <> *blanks AND
158900110912     C                             VABLOD <> *blanks
159000040128     C                   CLEAR                   TISI95DS
159100040128     C                   EVAL      I95TCN = '3'
159200040128     C                   Z-ADD     datcor        I95DAT
159300040128     C                   EVAL      I95NAR = VABNZD
159400040128     C                   EVAL      I95CAP = VABCAD
159500040128     C                   EVAL      I95LOC = VABLOD
159600040128     C                   CALL      'TISI95R'
159700040128     C                   PARM                    TISI95DS
159800040128     C                   EVAL      VABPRD = O95PRV
159900040510     C                   ENDIF
160000020204     C*
160100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
160200020204     C                   EXSR      CHKIMPDIV
160300030715     C                   ENDIF
160400020204     C*
160500010607     C                   ENDSR
160600010607     C*----------------------------------------------------*
160700010601
160800020204
160900020204     C*----------------------------------------------------*
161000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
161100020204     C*----------------------------------------------------*
161200020204     C     CHKIMPDIV     BEGSR
161300020204     C*
161400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
161500020204     C                   Z-ADD     *zeros        wrkDec            9 9
161600020204     C*
161700020204     C* Come prima cosa effettuo considerazioni sulla divisa
161800020204     C                   IF        vabIAS > *zeros
161900020204     C                   IF        vabVAS <> 'EUR'
162000020204     C                   EVAL      vabVAS =  'ITL'
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C                   IF        vabCAS > *zeros
162500020204     C                   IF        vabVCA <> 'EUR'
162600020204     C                   EVAL      vabVCA =  'ITL'
162700020204     C                   ENDIF
162800020204     C                   ENDIF
162900020204     C*
163000020204     C                   IF        vabVMD > *zeros
163100020204     C                   IF        vabVAD <> 'EUR'
163200020204     C                   EVAL      vabVAD =  'ITL'
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
163700020204     C                   Z-ADD     vabIAS        wrkDec
163800020204     C                   IF        wrkDec > *zeros
163900020204     C                   IF        vabVAS = 'ITL'
164000020204     C                   EVAL      vabIAS = *zeros
164100020204     C                   ENDIF
164200020204     C                   ENDIF
164300020204     C*
164400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
164500020204     C                   Z-ADD     vabCAS        wrkDec
164600020204     C                   IF        wrkDec > *zeros
164700020204     C                   IF        vabVCA = 'ITL'
164800020204     C                   EVAL      vabCAS = *zeros
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
165300020204     C                   Z-ADD     vabVMD        wrkDec
165400020204     C                   IF        wrkDec > *zeros
165500020204     C                   IF        vabVAD = 'ITL'
165600020204     C                   EVAL      vabVMD = *zeros
165700020204     C                   ENDIF
165800020204     C                   ENDIF
165900020204     C*
166000020204     C                   ENDSR
166100020204     C***
166200010330
166300100318
166400100318
166500100318     C*----------------------------------------------------*
166600100318     C*  CONTROLLO NUMERICITA' CAMPI
166700100318     C*----------------------------------------------------*
166800100318     C     CHKNUM        BEGSR
166900100318     C*
167000100318     C                   IF        PiDecChr = *blanks
167100100318     C                   EVAL      PiDecChr = CharNUM
167200100318     C                   ENDIF
167300100318     C*
167400100318     C                   callp     UBISNUM_Check(PiStr
167500100318     C                                          :PiDecChr
167600100318     C                                          :PiVal
167700100318     C                                          :PiNum
167800100318     C                                          :PiInt)
167900100318     C*
168000100318     C                   ENDSR
168100100318     C***
168200010601
168300010601
168400010601
168500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168600010601     C     repfil        BEGSR
168700010601     C*
168800010601     C                   if        invfil = *zeros and
168900010601     C                             depfil > *zeros and
169000010629     C                             (vinflg = *blanks or
169100010629     C                              vinflg = *zeros)
169200010601     C
169300010601     C                   eval      invfil = depfil
169400010601     C                   endif
169500010601     C*
169600010601     C                   if        depfil <> invfil and
169700010601     C                             invfil > *zeros
169800010601     C                   eval      flgMulti = '1'
169900010601     C                   if        vinflg = *blanks
170000010601     C                   add       1             cntNonEl
170100010601     C                   endif
170200010601     C                   endif
170300010601     C*
170400010601     C                   if        vinflg = '2'
170500010601     C                   eval      flgStato = '2'
170600010601     C                   endif
170700010601     C*
170800010601     C                   ENDSR
170900010601     C***
171000010601
171100010601
171200010601
171300010330
171400040119      /TITLE Invio dei dati al punto operativo.
171500040119     C     invio         BEGSR
171600040119     C*
171700110930     C* 1° invio EDIVAT
171800040119     C                   reset                   dscmz
171900040119     C                   move      vlrpoi        cmzdst
172000110930     C                   eval      cmzfld = 'EDIVATWR'
172100040119     C                   eval      cmzmbd = vlrhdl
172200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172300040119     C***                if        prmfir = *blanks
172400110930     C                   eval      cmzfla = 'EDIVAT0F'
172500110930     C                   eval      cmzmba = 'EDIVAT0F'
172600040119     C***                else
172700040119     C***                eval      cmzfla = prmfir
172800040119     C***                eval      cmzmba = prmfir
172900040119     C***                endif
173000040119     C                   eval      cmznrr = *zeros
173100040119     C                   move      §ctrokvt      cmznrr
173200040119     C                   eval      cmzlba = vlrfl1
173300040119     C                   call(e)   'TIS711C'
173400040119     C                   parm                    dscmz
173500040119     C                   parm      *blanks       esito
173600040119     C                   if        %error
173700040119     C                             or cmzerr = '1'
173800040119     C                             or esito  = '1'
173900040119     C                   eval      wrkesito = '3'
174000040119     C                   else
174100040119     C*
174200110930     C* 2° invio EDIVAB
174300040119     C                   reset                   dscmz
174400040119     C                   move      vlrpoi        cmzdst
174500040119     C                   eval      cmzfld = vlrfou
174600040119     C                   eval      cmzmbd = vlrhdl
174700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174800040119     C***                if        prmfir = *blanks
174900110930     C                   eval      cmzfla = 'EDIVAB0F'
175000110930     C                   eval      cmzmba = 'EDIVAB0F'
175100040119     C***                else
175200040119     C***                eval      cmzfla = prmfir
175300040119     C***                eval      cmzmba = prmfir
175400040119     C***                endif
175500040119     C                   eval      cmznrr = *zeros
175600040119     C                   move      §ctrokvb      cmznrr
175700040119     C                   eval      cmzlba = vlrfl1
175800040119     C                   call(e)   'TIS711C'
175900040119     C                   parm                    dscmz
176000040119     C                   parm      *blanks       esito
176100040119     C                   if        %error
176200040119     C                             or cmzerr = '1'
176300040119     C                             or esito  = '1'
176400040119     C                   eval      wrkesito = '3'
176500040119     C                   endif
176600040119     C                   endif
176700040119     C*
176800040119     C                   ENDSR
176900040119     C***
177000010601
177100010601
177200010601
177300010601
177400010601      /TITLE Invio dei dati al punto operativo.
177500010601     C     opeini        BEGSR
177600010601     C*
177700010601     C* Inizializzo flag e contatori operativi
177800010601     C                   movel     '0'           flgGiro           1
177900010601     C                   movel     '0'           flgMulti          1
178000010601     C                   movel     '1'           flgStato          1
178100010615     C                   movel     '0'           flgOk             1
178200010601     C                   z-add     *zeros        cntNonEl         10 0
178300010601     C                   z-add     *zeros        depfil            3 0
178400010601     C                   z-add     *zeros        invfil            3 0
178500010601     C*
178600010601     C                   ENDSR
178700010601     C***
178800070326
178900070326
179000070326
179100070326
179200070326     C     *pssr         BEGSR
179300070329     C*
179400070329     C                   if        %open(tivin00r)
179500070329     C                   close     tivin00r
179600070329     C                   endif
179700110930     C                   if        %open(edivabwr)
179800110930     C                   close     edivabwr
179900070329     C                   endif
180000110930     C                   if        %open(edivatwr)
180100110930     C                   close     edivatwr
180200070329     C                   endif
180300070326     C*
180400070326     C* Effettuo la chiamata al CLLE preposto
180500110930     C                   call(e)   'TITVEVTC'
180600070326     C                   parm                    parccm
180700070326     C                   parm                    parmbr
180800070326     C                   parm      '2'           paropz
180900070326     C*
181000070326     C                   eval      wrkesito = '2'
181100070404     C*
181200070404     C                   seton                                        LR
181300070326     C*
181400070326     C                   ENDSR     '*CANCL'
181500070326     C***
181600100318
181700100318
181800100318
181900100318     C     normCSV       BEGSR
182000100318     C*
182100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
182200100318     C* separatori campi
182300100318     C                   if        CharTXT <> *blanks
182400100318     C                   z-add     *zeros        wLenVINDTA        4 0
182500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
182600100318     C                   setoff                                       80
182700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
182800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
182900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
183000100318     C                   if        not *in80
183100100318     C                   seton                                        80
183200100318     C                   else
183300100318     C                   setoff                                       80
183400100318     C                   endif
183500100318     C                   endif
183600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
183700100318     C                             and *in80 = *on
183800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
183900100318     C                   endif
184000100318     C                   enddo
184100100318     C                   endif
184200100318     C*
184300100318     C                   ENDSR
184400100318     C***
184500070326
184600100326
184700100326
184800100326     C*----------------------------------------------------*
184900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
185000100326     C*----------------------------------------------------*
185100100326     C     REPNSP        BEGSR
185200100326     C*
185300100326     C* NSP => Stacco un numeratore da AZNUM
185400100326     C                   clear                   TRUL33DS
185500100326     C                   eval      I33OPE = *zeros
185600100326     C                   eval      I33CNU = 302
185700100326     C                   eval      I33NUM = 1
185800100326     C                   movel     TRUL33DS      KPJBU
185900100326     C                   call      'TRUL33R'
186000100326     C                   parm                    KPJBA
186100100326     C                   movel     KPJBU         TRUL33DS
186200100326     C                   if        O33ERR = *zeros
186300100326     C                   z-add     O33NRF        VABNSP
186400100326     C                   z-add     O33NRF        VATNSP
186500100326     C                   else
186600100326     C                   SETON                                        31
186700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
186800100326     C                             + ' ' + 'VABNSP VATNSP'
186900100326     C                   endif
187000100326     C*
187100100326     C                   ENDSR
187200010330
187300100326
187400010330
187500000613     C     *inzsr        BEGSR
187600990910     C*
187700990910     C     *entry        plist
187800990920     C                   parm                    tivlrds
187900990921     C                   parm      wrkesito      esito
188000000724     C                   parm                    prmlit
188100000710     C                   parm                    prmfir
188200010330     C*
188300010330     C* CALCOLA LA DATA CORRENTE
188400110930     C                   time                    wn14             14 0
188500110930     C                   movel     wn14          oracor            6 0          *ORA
188600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188700100318     C                   eval      datcor = %dec(%date() : *ISO)
188800000613     C*
188900000613     C                   ENDSR
189000000613     C***
189100990908
189200120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189300170317X;W_VABRMN;X;VABRMA;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;VATNOT_IJ;VABPKB;VABNCL;X;
189400170317W_VABCBO;X;X;VABCAS;VABTIC;X;X;VATNOT_E;
189500161102
189600141007
189700141007
