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
007000161102     D wTaglia         s              2
007100161102     D wNumCollo       s              2s 0
007200161102     D wPrgCollo       s              2s 0
007300030822     D*------------------
007400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007500030822     D*------------------
007600030822     D posDaDft        S              3  0 INZ(*zeros)
007700030822     D posADft         S              3  0 INZ(*zeros)
007800030822     D j               s              3  0 INZ(1)
007900030822     D wGiroDft        s              1  0 INZ(*zeros)
008000080418     D*------------------
008100080418     D* Costanti
008200080418     D*------------------
008300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008500100318
008600100318     D*------------------
008700100318     D* LINKING A DEFINIZIONI ESTERNE
008800100318     D*------------------
008900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009100100318
009200100909     D*------------------
009300100909     D* DS CALCOLO CHECK-DIGITS
009400100909     D*------------------
009500100909     D TRUL28DSX     e ds                  inz
009600010330
009700010330
009800010330
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200010601     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601
013000010601     C*--------------------------------------------------------
013100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013200010601     C*--------------------------------------------------------
013300010601     C     ENDELA        BEGSR
013400040119     C*
013500040119     C* Effettuo la chiamata al CLLE preposto
013600110930     C                   call(e)   'TITVEVTC'
013700040510     C                   parm                    parccm
013800040119     C                   parm                    parmbr
013900040119     C                   parm      '2'           paropz
014000050201     C*
014100050201     C* Effettuo lancio TISI95 solo x chiusura
014200050201     C                   CLEAR                   TISI95DS
014300050201     C                   EVAL      I95TLA = 'C'
014400050201     C                   CALL      'TISI95R'
014500050201     C                   PARM                    TISI95DS
014600000616     C*
014700010601     C                   ENDSR
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800110930     C                   if        not %open(edivabwr)
015900110930     C                   open      edivabwr
016000010330     C                   endif
016100040119     C*
016200110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016300040119     C                   exsr      prevat
016400040119     C*
016500040119     C                   if        chkcall = '0'
016600040119     C*
016700110930     C                   if        not %open(edivatwr)
016800110930     C                   open      edivatwr
016900040119     C                   endif
017000010330     C*
017100040119     C                   clear                   §CTROKVB          7 0
017200040119     C                   clear                   §CTROKVT          7 0
017300040119     C                   clear                   §CTRMO            7 0
017400040119     C                   clear                   §CTRNO            7 0
017500121030     C*
017600121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017700121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017800121030     C                   IF        Intestazioni = *off
017900121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018000121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018100121030     C                   exsr      inzvar
018200121030     C                   exsr      impvab
018300121030     C                   ENDIF
018400010330     C*
018500030822     C                   do        *HIVAL
018600010330     C*
018700030822     C                   read      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300040510     C*
019400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019500040510     C                   z-add     *zeros        wLenVINDTA        4 0
019600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020000040510     C                   z-add     1             wFlgVINDTA
020100040510     C                   leave
020200040510     C                   endif
020300040510     C                   enddo
020400010618     C*
020500040510     C                   if        vindta > *blanks AND
020600040510     C                             wFlgVINDTA = 1
020700010330     C                   add       1             rrnum
020800010330     C*
020900010601     C                   if        *in70 = *off and
021000010330     C                             (vinflg = *blanks
021100010330     C                              or vinflg = '0'
021200010330     C                              or vinflg = '2')
021300010330     C*
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800010601     C*
021900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022000070925     C                   eval      vindta  = ' ' + %trim(vindta)
022100070925     C*
022200080527     C                   exsr      inzvar
022300100318     C                   exsr      normCSV
022400010605     C                   exsr      impvab
022500010601     C*
022600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022700010605     C                   eval      depfil = VABLNP
022800010601     C                   exsr      repfil
022900010601     C                   if        depfil = invfil
023000021025     C                   if        vlrpoi = 999
023100030822     C                   move(P)   invfil        VABFGS
023200021025     C                   else
023300030822     C                   move(P)   vlrpoi        VABFGS
023400021025     C                   endif
023500010601     C*
023600010601     C                   exsr      PREELA
023700010601     C*
023800010604     C* Ebbene...
023900010604     C*
024000040119     C  N31              add       1             §CTROKVB
024100040119     C   32              add       1             §CTRMO
024200040119     C   31              add       1             §CTRNO
024300080418     C*
024400030822     C                   if        wGiro = 2
024500080513     C* Scrivo sempre il VAT
024600080513     C  N31              exsr      wrivat
024700110201     C                   exsr      chkvab
024800080513     C                   endif
024900010604     C*
025000010604     C                   if        *in31 = *off and
025100010604     C                             *in32 = *off
025200010604     C                   eval      vinflg = '1'
025300010604     C                   else
025400010604     C                   eval      vinflg = '2'
025500010604     C                   endif
025600010604     C                   endif
025700010601     C*
025800010604     C                   endif
025900010604     C*
026000010330     C                   else
026100010330     C                   eval      vinflg = '1'
026200010330     C                   endif
026300010601     C*
026400010601     C  N70              update    tivin000
026500010330     C*
026600030822     C  N70              enddo
026700040119     C*
026800080527     C* Verifico dati rimasti "in cannna"...
026900080527     C                   seton                                        65
027000110201     C                   exsr      chkvab
027100080527     C*
027200040119     C                   endif                                                  (endif - chkcall)
027300010601     C*
027400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027500010601     C                   if        cntNonEl = *zeros or
027600010601     C                             flgMulti = '0'
027700010330     C* Se non ci sono record con errori ...
027800010601     C                   if        §ctrno = 0 and
027900010604     C                             §ctrmo = 0 and
028000010601     C                             flgStato <> '2'
028100010330     C* ... restituisco esito OK.
028200010330     C                   eval      wrkesito = '0'
028300010330     C                   else
028400040119     C                   if        §ctrokvb > 0
028500010330     C                   eval      wrkesito = '1'
028600010330     C                   else
028700010615     C                   if        flgOk = '0'
028800010615     C                   eval      wrkesito = '2'
028900010615     C                   else
029000010615     C                   eval      wrkesito = '6'
029100010615     C                   endif
029200010330     C                   endif
029300010330     C                   endif
029400010601     C                   else
029500010601     C                   eval      wrkesito = '9'
029600010601     C                   endif
029700010330     C*
029800010330     C                   if        %open(tivin00r)
029900010330     C                   close     tivin00r
030000010330     C                   endif
030100110930     C                   if        %open(edivabwr)
030200110930     C                   close     edivabwr
030300010330     C                   endif
030400110930     C                   if        %open(edivatwr)
030500110930     C                   close     edivatwr
030600040512     C                   endif
030700010601     C*
030800010601     C                   if        vlrpoi <> 999
030900010601     C                   eval      invfil = vlrpoi
031000010601     C                   endif
031100010330     C*
031200040119     C                   if        §ctrokvb > 0
031300010601     C                             and invfil > *zeros
031400010330     C                   exsr      invio
031500010330     C                   endif
031600010601     C*
031700010618     C                   if        flgGiro = '1'
031800010601     C                   exsr      endela
031900010618     C                   endif
032000010330     C*
032100010330     C                   ENDSR
032200010330     C***
032300080527
032400080527
032500080527
032600080527     C*----------------------------------------------------*
032700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032800080527     C*----------------------------------------------------*
032900100413     C     CHKVAB        BEGSR
033000110201     C*
033100110201     C* Se richiesti ignorare colli / peso / volume
033200110315 xxx C***                if        *in35
033300120203 xxx C***                z-add     *zeros        VABNCL              F0461
033400120203 xxx C***                z-add     *zF046190081  VABPKB
033500110315 xxx C***                z-add     *zeros        VABVLB
033600110315 xxx C***                endif
033700080527     C*
033800080527     C* Scrivo il VAB solo a rottura d spedizione
033900080527     C                   if        depspe = *zeros
034000110930     C  N31              eval      edivabs_sav = edivabs
034100080527     C                   eval      depspe = vabNSP
034200080527     C                   else
034300080527     C                   if        depspe = vabNSP AND *in65 = *off
034400100413     C   51              add       VABNCL        sav_VABNCL
034500100413     C   51              add       VABPKB        sav_VABPKB
034600100413     C   51              add       VABVLB        sav_VABVLB
034700100720     C   52              z-add     VABNCL        sav_VABNCL
034800100720     C   52              z-add     VABPKB        sav_VABPKB
034900100720     C   52              z-add     VABVLB        sav_VABVLB
035000100720     C   53              add       VABNCL        sav_VABNCL
035100100720     C   53              z-add     VABPKB        sav_VABPKB
035200100720     C   53              z-add     VABVLB        sav_VABVLB
035300080527     C                   else
035400120203     C  N31              eval      edivabs_ok = edivabs_sav
035500080527     C  N31              exsr      wrivab
035600110930     C  N31              eval      edivabs_sav = edivabs
035700080527     C                   eval      depspe = vabNSP
035800080527     C                   endif
035900080527     C                   endif
036000080527     C*
036100080527     C                   ENDSR
036200040119
036300040119
036400040119
036500040119     C*----------------------------------------------------*
036600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036700040119     C*----------------------------------------------------*
036800040510     C     WRIVAB        BEGSR
036900080513     C*
037000120203     C* EDIVABWR ha il prefisso "ok_"
037100120203     C*
037200110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037300120203     C                   MOVE(P)   vlrMSG        wNomeFile
037400130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
037500120203     C                   EVAL      ok_VABDCM = DATCOR
037600120203     C                   EVAL      ok_VABDTS = DATCOR
037700120203     C                   EVAL      ok_VABHMS = ORACOR
037800120203     C                   EVAL      ok_VABCNT = 1
037900110930     C*
038000110930     C                   write     edivab00
038100040119     C*
038200040119     C                   ENDSR
038300040119
038400040119
038500040119
038600040119     C*----------------------------------------------------*
038700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038800040119     C*----------------------------------------------------*
038900040510     C     WRIVAT        BEGSR
039000060512     C*
039100060512     C* Inizializzo i campi chiave
039200060512     C                   eval      VATFGS = VABFGS
039300060512     C                   eval      VATAAS = VABAAS
039400060512     C                   eval      VATCCM = VABCCM
039500070823     C                   eval      VATNRS = VABNRS
039600060512     C                   eval      VATNSP = VABNSP
039700060512     C                   eval      VATLNP = VABLNP
039800040119     C*
039900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040000040510     C                   if        VATTRC = *blanks
040100040510     C                   eval      VATTRC = 'E'
040200040510     C                   endif
040300110318     C                   if        VATNOT <> *blanks
040400120203     C                   MOVE(P)   vlrMSG        wNomeFile
040500130122     C***                EVAL      VATCMR = wNomeFile
040600141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
040700110930     C                   eval      VATCNT = 1
040800110930     C                   write     EDIVAT00
040900040119     C                   add       1             §CTROKVT
041000110318     C                   endif
041100060331     C*
041200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041300060331     C                   if        wVATNOT_A <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_A
041500060331     C                   eval      VATTRC = 'A'
041600120203     C                   MOVE(P)   vlrMSG        wNomeFile
041700130122     C***                EVAL      VATCMR = wNomeFile
041800141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
041900110930     C                   eval      VATCNT = 1
042000110930     C                   write     EDIVAT00
042100060331     C                   add       1             §CTROKVT
042200060331     C                   endif
042300060331     C*
042400060331     C                   if        wVATNOT_B <> *blanks
042500060331     C                   eval      VATNOT = wVATNOT_B
042600060331     C                   eval      VATTRC = 'B'
042700120203     C                   MOVE(P)   vlrMSG        wNomeFile
042800130122     C***                EVAL      VATCMR = wNomeFile
042900141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
043000110930     C                   eval      VATCNT = 1
043100110930     C                   write     EDIVAT00
043200060331     C                   add       1             §CTROKVT
043300060331     C                   endif
043400141007     C*
043500141007     C                   if        wVATNOT_IJ <> *blanks
043600141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043700141007     C                   eval      VATTRC = 'I'
043800141007     C***                EVAL      VATCMR = wNomeFile
043900141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044000141007     C                   eval      VATCNT = 1
044100141007     C                   write     EDIVAT00
044200141007     C                   add       1             §CTROKVT
044300141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
044400141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044500141007     C                   eval      VATTRC = 'J'
044600141007     C***                EVAL      VATCMR = wNomeFile
044700141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
044800141007     C                   eval      VATCNT = 1
044900141007     C                   write     EDIVAT00
045000141007     C                   endif
045100141007     C                   endif
045200060331     C*
045300060331     C                   if        wVATNOT_E <> *blanks
045400060331     C                   eval      VATTRC = 'E'
045500121030     C                   MOVE(P)   vlrMSG        wNomeFile
045600130122     C***                EVAL      VATCMR = wNomeFile
045700141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
045800121030     C                   eval      VATCNT = 1
045900141007     C*
046000161102     C* Sviluppati i "CHI SONO" in relazione al numero colli
046100161102     C*  per ogni collo va aggiunto al segnacollo '-xx' dove xx è il numero collo - 1
046200161102     C*  tranne per il primo collo che invece prende il valore del segnacollo che è stato passato
046300161102     C     1             DO        VABNCL        wNumCollo
046400161102     C                   IF        wNumCollo > 1
046500161102     C                   eval      wPrgCollo = wNumCollo - 1
046600161102     C                   eval      VATNOT = %trim(wVATNOT_E) + '-' +
046700161102     C                              %editc(wPrgCollo : 'X')
046800161102     C                   ELSE
046900161102     C                   eval      VATNOT = %trim(wVATNOT_E)
047000161102     C                   ENDIF
047100141007     C                   write     EDIVAT00
047200161102     C                   ENDDO
047300110927     C*
047400161102     C                   ENDIF
047500161102     C*
047600110927     C                   if        wVATNOT_I <> *blanks
047700110927     C                   eval      VATNOT = wVATNOT_I
047800110927     C                   eval      VATTRC = 'I'
047900120203     C                   MOVE(P)   vlrMSG        wNomeFile
048000130122     C***                EVAL      VATCMR = wNomeFile
048100141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048200110930     C                   eval      VATCNT = 1
048300110930     C                   write     EDIVAT00
048400110927     C                   add       1             §CTROKVT
048500110927     C                   endif
048600120202     C*
048700120202     C                   if        wVATNOT_J <> *blanks
048800120202     C                   eval      VATNOT = wVATNOT_J
048900120202     C                   eval      VATTRC = 'J'
049000120203     C                   MOVE(P)   vlrMSG        wNomeFile
049100130122     C***                EVAL      VATCMR = wNomeFile
049200141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
049300120202     C                   eval      VATCNT = 1
049400120202     C                   write     EDIVAT00
049500120202     C                   add       1             §CTROKVT
049600120202     C                   endif
049700141007     C*
049800141007     C                   if        wVATNOT_S <> *blanks
049900141007     C                   eval      VATNOT = wVATNOT_S
050000141007     C* i 2 flag devono essere blank
050100141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
050200141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
050300141007     C                   eval      VATTRC = 'S'
050400141007     C***                EVAL      VATCMR = wNomeFile
050500141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
050600141007     C                   eval      VATCNT = 1
050700141007     C                   write     EDIVAT00
050800141007     C                   add       1             §CTROKVT
050900141007     C                   endif
051000040119     C*
051100040119     C                   ENDSR
051200040119
051300040119
051400040119
051500040119     C*----------------------------------------------------*
051600110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
051700040119     C*----------------------------------------------------*
051800040119     C     PREVAT        BEGSR
051900040119     C*
052000110930     C* Compongo il nome del membro da dare al EDIVATWR
052100040119     C                   eval      parmbr = vlrhdl
052200040119     C                   movel     'M'           parmbr
052300040510     C                   eval      parccm = vlrksc
052400040119     C                   eval      paropz = '1'
052500040119     C* Effettuo la chiamata al CLLE preposto
052600110930     C                   call(e)   'TITVEVTC'
052700040119     C                   parm                    parccm
052800040119     C                   parm                    parmbr
052900040119     C                   parm                    paropz
053000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
053100040119     C                   if        %error
053200040119     C                   movel     '1'           chkcall
053300040119     C                   else
053400040119     C                   movel     '0'           chkcall
053500040119     C                   endif
053600040119     C*
053700040119     C                   ENDSR
053800010601
053900010601
054000010601
054100010330     C*----------------------------------------------------*
054200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054300010330     C*----------------------------------------------------*
054400040119     C     INZVAR        BEGSR
054500080527     C*
054600080527     C                   setoff                                       65
054700010330     C*
054800030822     C* Inizializzo variabili di wrk
054900130128     C                   Z-ADD     *zeros        Num3_0            3 0
055000020204     C                   Z-ADD     *zeros        Num5_0            5 0
055100101018     C                   MOVEL     *blanks       wFlgCAS
055200060331     C                   MOVEL     *blanks       wVATNOT_A
055300060331     C                   MOVEL     *blanks       wVATNOT_B
055400060331     C                   MOVEL     *blanks       wVATNOT_E
055500110927     C                   MOVEL     *blanks       wVATNOT_I
055600120202     C                   MOVEL     *blanks       wVATNOT_J
055700120202     C                   MOVEL     *blanks       wVATNOT_IJ
055800141007     C                   MOVEL     *blanks       wVATNOT_S
055900161102     C                   MOVEL     *blanks       wTaglia
056000030822     C*
056100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056200030822     C                   CLEAR                   SkSplitCSV
056300030822     C                   EVAL      i = 1
056400030822     C                   EVAL      posDa = *zeros
056500030822     C                   EVAL      posA  = *zeros
056600080527     C*
056700110930     C                   CLEAR                   EDIVABS
056800110930     C                   CLEAR                   EDIVAT00
056900030822     C*
057000030822     C* Reimposto i valori di default
057100030822     C                   EXSR      DEFCAM
057200030822     C*
057300010330     C                   ENDSR
057400010330     C*----------------------------------------------------*
057500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057600010330     C*----------------------------------------------------*
057700010330     C     DEFCAM        BEGSR
057800030822     C*
057900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
058000030715     C* e delimitatore testo.
058100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058400100318     C*
058500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058600100318     C                   EVAL      CharSOS = CharNUM
058700030822     C*
058800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058900100720     C                   SETOFF                                       505152
059000100720     C                   SETOFF                                       53
059100100413     C                   SELECT
059200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
059300100720     C                   SETON                                        50
059400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
059500100413     C                   SETON                                        51
059600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059700100720     C                   SETON                                        52
059800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059900100720     C                   SETON                                        53
060000100413     C                   ENDSL
060100030822     C*
060200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060300030822     C                   EVAL      posDaDft = 1
060400030822     C                   EVAL      posADft  = 0
060500030822     C                   EVAL      wGiroDft = 0
060600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060700030822     C                             posDaDft > 0
060800030822     C*
060900030822     C* Gestisco il 1° giro
061000030822     C                   IF        wGiroDft = 0
061100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061300030822     C* Incremento il contatore dei "giri"
061400030822     C                   EVAL      wGiroDft = 1
061500030822     C                   ELSE
061600030822     C                   EVAL      posDaDft = posADft
061700030822     C                   ENDIF
061800030822     C* Eseguo lo scan x trovare la fine del campo corrente
061900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
062000030822     C*
062100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062300030822     C                   IF        posDaDft > 0 AND
062400030822     C                             posADft  > 0
062500030822     C* NCL
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'NCL'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiInt=*on
063500030822     C                   Z-ADD     PiVal         VABNCL
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* CCM
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'CCM'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiInt=*on
064800030822     C                   Z-ADD     PiVal         VABCCM
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* LNP
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'LNP'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100030822     C                   Z-ADD     PiVal         VABLNP
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* NRS
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'NRS'
066900030822     C                   EVAL      PiStr=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   EXSR      CHKNUM
067300030822     C                   IF        PiInt=*on
067400030822     C                   Z-ADD     PiVal         VABNRS
067500030822     C                   ENDIF
067600030822     C                   ENDIF
067700030822     C* CTR
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'CTR'
068200030822     C                   EVAL      PiStr=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   EXSR      CHKNUM
068600030822     C                   IF        PiInt=*on
068700030822     C                   Z-ADD     PiVal         VABCTR
068800030822     C                   ENDIF
068900030822     C                   ENDIF
069000030822     C* PKB
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'PKB'
069500030822     C                   EVAL      PiStr=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   EXSR      CHKNUM
069900030822     C                   IF        PiNum=*on
070000030822     C                   Z-ADD     PiVal         VABPKB
070100030822     C                   ENDIF
070200030822     C                   ENDIF
070300030822     C* VLB
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'VLB'
070800030822     C                   EVAL      PiStr=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   EXSR      CHKNUM
071200030822     C                   IF        PiNum=*on
071300030822     C                   Z-ADD     PiVal         VABVLB
071400030822     C                   ENDIF
071500030822     C                   ENDIF
071600030822     C* QFT
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'QFT'
072100030822     C                   EVAL      PiStr=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   EXSR      CHKNUM
072500030822     C                   IF        PiNum=*on
072600030822     C                   Z-ADD     PiVal         VABQFT
072700030822     C                   ENDIF
072800030822     C                   ENDIF
072900030822     C* CBO
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'CBO'
073400030822     C                   EVAL      VABCBO=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* TSP
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'TSP'
074300030822     C                   EVAL      VABTSP=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* VAS
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'VAS'
075200030822     C                   EVAL      VABVAS=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* VCA
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'VCA'
076100030822     C                   EVAL      VABVCA=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500070925     C* TIC
076600070925     C                   IF        %subst(
076700070925     C                             %subst(vlrppt:posDaDft+1:
076800070925     C                             posADft-posDaDft-1):1:3)
076900070925     C                             = 'TIC'
077000070925     C                   EVAL      VABTIC=%trim(%subst(
077100070925     C                             %subst(vlrppt:posDaDft+1:
077200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300070925     C                   ENDIF
077400030822     C* GCA
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'GCA'
077900030822     C                   EVAL      VABGCA=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* CTM
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'CTM'
078800030822     C                   EVAL      VABCTM=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* FFD
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'FFD'
079700030822     C                   EVAL      VABFFD=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* VAD
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'VAD'
080600030822     C                   EVAL      VABVAD=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* GMA
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'GMA'
081500030822     C                   EVAL      VABGMA=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* GGA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'GGA'
082400030822     C                   EVAL      VABGGA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* GVA
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'GVA'
083300030822     C                   EVAL      VABGVA=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* TC1
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'TC1'
084200030822     C                   EVAL      VABTC1=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600030822     C* TC2
084700030822     C                   IF        %subst(
084800030822     C                             %subst(vlrppt:posDaDft+1:
084900030822     C                             posADft-posDaDft-1):1:3)
085000030822     C                             = 'TC2'
085100030822     C                   EVAL      VABTC2=%trim(%subst(
085200030822     C                             %subst(vlrppt:posDaDft+1:
085300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400030822     C                   ENDIF
085500040714     C* VATTRC
085600040714     C                   IF        %subst(
085700040714     C                             %subst(vlrppt:posDaDft+1:
085800040714     C                             posADft-posDaDft-1):1:3)
085900040714     C                             = 'TRC'
086000040714     C                   EVAL      VATTRC=%trim(%subst(
086100040714     C                             %subst(vlrppt:posDaDft+1:
086200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086300040714     C                   ENDIF
086400030822     C* ...
086500030822     C                   ENDIF
086600030822     C                   ENDDO
086700020204     C*
086800010330     C                   ENDSR
086900010607     C*----------------------------------------------------*
087000110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
087100010607     C*----------------------------------------------------*
087200010607     C     IMPVAB        BEGSR
087300010607     C*
087400010607     C                   SETOFF                                       3132
087500110315 xxx C***                SETON                                        35
087600030515     C*
087700010607     C                   MOVEL     datcor        VABAAS
087800010607     C                   MOVE      datcor        VABMGS
087900010607     C*
088000030715     C**********
088100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088200030715     C**********
088300030715     C                   IF        wGiro = *zeros
088400040113     C*
088500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088800080429     C                             %trim(intcol(5))<>*blanks
088900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
089000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
089100080429     C                                      %trim(intcol(5))
089200070823     C     ';':CharCSV   XLATE     vindta        vindta
089300040510     C                   ENDIF
089400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089500031201     C     minu:maiu     XLATE     vindta        vindta
089600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089700030715     C                   DOW       posDa <= %len(%trim(vindta))
089800030715     C*
089900030715     C* Gestisco il 1° campo
090000030715     C                   IF        i = 1
090100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090200030715     C                   EVAL      posDa = 1
090300030715     C* Eseguo lo scan x trovare la fine del primo campo
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* A questo punto "estraggo" il campo corrente
090600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090700030715     C                                                    (posA-posDa))
090800030715     C* X i campi successivi al 1°
090900030715     C                   ELSE
091000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091100030715     C                   EVAL      posDa = posA + 1
091200030715     C* Eseguo lo scan x trovare la fine del campo corrente
091300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091400030715     C* Gestisco l'ultimo campo
091500030715     C                   IF        posA = *zeros
091600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091700030715     C* A questo punto "estraggo" il campo corrente
091800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091900030715     C                                                    (posA-posDa))
092000030715     C                   LEAVE
092100030715     C                   ELSE
092200030715     C* A questo punto "estraggo" il campo corrente
092300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092400030715     C                                                    (posA-posDa))
092500030715     C                   ENDIF
092600030715     C                   ENDIF
092700030715     C* Incremento il contatore d campo
092800030715     C                   EVAL      i = i +1
092900030715     C                   ENDDO
093000030715     C*
093100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093200030715     C* delimitazione testo
093300030715     C                   EVAL      i = 1
093400030715     C                   DOW       i < %elem(SkSplitFLD)
093500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093700030715     C                   EVAL      i = i + 1
093800030715     C                   ENDDO
093900030715     C                   EVAL      wGiro = 1
094000030715     C                   ELSE
094100030715     C                   EVAL      wGiro = 2
094200031204     C**********
094300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094400031204     C* d separatore campo
094500031204     C**********
094600031204     C                   EVAL      vindta = %trim(vindta)
094700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094800031204     C                   EVAL      vindta = %subst(vindta:2)
094900031204     C                   ENDDO
095000031204     C*
095100031204     C                   Z-ADD     *zeros        lunghInput        4 0
095200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095600110912     C                   ENDDO
095700030715     C**********
095800030715     C* Effettuo lo split del campo dati d input
095900030715     C**********
096000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
096100030715     C                   DOW       posDa <= %len(%trim(vindta))
096200030715     C*
096300030715     C* Gestisco il 1° campo
096400030715     C                   IF        i = 1
096500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096600030715     C                   EVAL      posDa = 1
096700030715     C* Eseguo lo scan x trovare la fine del primo campo
096800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096900030715     C* A questo punto "estraggo" il campo corrente
097000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097100030715     C                                                    (posA-posDa))
097200030715     C* X i campi successivi al 1°
097300030715     C                   ELSE
097400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097500030715     C                   EVAL      posDa = posA + 1
097600031204     C* Verifico che nn vi sia il campo nn valorizzato
097700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097800031204     C* Se campo nn valorizzato skippo al prossimo
097900031204     C                   EVAL      SkSplitCSV(i) = *blanks
098000031204     C                   EVAL      posA  = posA + 1
098100031204     C                   ELSE
098200030715     C* Eseguo lo scan x trovare la fine del campo corrente
098300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098400030715     C* Gestisco l'ultimo campo
098500030715     C                   IF        posA = *zeros
098600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098700030715     C* A questo punto "estraggo" il campo corrente
098800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098900030715     C                                                    (posA-posDa))
099000030715     C                   LEAVE
099100030715     C                   ELSE
099200030715     C* A questo punto "estraggo" il campo corrente
099300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099400030715     C                                                    (posA-posDa))
099500030715     C                   ENDIF
099600031204     C                   ENDIF
099700030715     C                   ENDIF
099800030715     C* Incremento il contatore d campo
099900030715     C                   EVAL      i = i +1
100000030715     C                   ENDDO
100100030715     C*
100200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100300030715     C* delimitazione testo
100400030715     C                   EVAL      i = 1
100500030715     C                   DOW       i < %elem(SkSplitCSV)
100600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100800030715     C                   EVAL      i = i + 1
100900030715     C                   ENDDO
101000100720     C*
101100100720     C* Stacco un progressivo x numero spedizione
101200110930 xxx C                   EXSR      repNSP
101300100326     C*
101400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101500030715     C                   EVAL      i = 1
101600030715     C                   DOW       i < %elem(SkSplitFLD)
101700030715     C*
101800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
102000030715     C                             SkSplitCSV(i) <> *blanks
102100030721     C*** ATB
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102300030721     C                   EVAL      VABATB = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** RSD
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
103000030715     C* ==
103100030715     C                   ENDIF
103200030721     C*** RD2
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030715     C*** IND
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103800030715     C                   EVAL      VABIND = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** LOD
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
104200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** PRD
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
104600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030721     C*** NZD
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
105000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030715     C*** RMA
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** NOT
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** NT2
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
106200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030715     C*** VCA
106500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
106600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106700030715     C                   ENDIF
106800030715     C*** VAS
106900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
107000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
107100030715     C                   ENDIF
107200030715     C*** FFD
107300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107500030715     C                   ENDIF
107600030721     C*** GC1
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GC2
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
108200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TSP
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
108600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108700030721     C                   ENDIF
108800040510     C*** CBO
108900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
109000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
109100040510     C                   ENDIF
109200030721     C*** NAS
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** TIC
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GCA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
110200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** XCO
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
110600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** CTM
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
111000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** TCR
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** CTS
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** FTM
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
112200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** VAD
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
112600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** GMA
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
113000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** GGA
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** GVA
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** TC1
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
114200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** TC2
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
114600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*** SCL
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
115000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
115100030721     C                   ENDIF
115200030721     C*** RMO
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115500030721     C                   ENDIF
115600030721     C*** NMO
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115900030721     C                   ENDIF
116000030721     C*
116100030721     C* Reperisco quindi i campi numerici...
116200040510     C*** NSP
116300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116400040510     C                   EVAL      PiStr=SkSplitCSV(i)
116500040510     C                   EXSR      CHKNUM
116600040510     C                   IF        PiInt=*on
116700040510     C                   Z-ADD     PiVal         VABNSP
116800040510     C                   ELSE
116900040510     C                   SETON                                        31
117000040510     C                   EVAL      VABNSP = *zeros
117100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040510     C                             + ' ' + 'VABNSP'
117300040510     C                   ENDIF
117400040510     C                   ENDIF
117500071106     C*** RMN
117600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117700071106     C                   EVAL      PiStr=SkSplitCSV(i)
117800071106     C                   EXSR      CHKNUM
117900071106     C                   IF        PiInt=*on
118000071106     C                   Z-ADD     PiVal         VABRMN
118100071106     C                   ELSE
118200071106     C                   SETON                                        32
118300071106     C                   EVAL      VABRMN = *zeros
118400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118500071106     C                             + ' ' + 'VABRMN'
118600071106     C                   ENDIF
118700071106     C                   ENDIF
118800030721     C*** CAD
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiInt=*on
119300030721     C                   Z-ADD     PiVal         Num5_0
119400030721     C                   MOVEL(p)  Num5_0        VABCAD
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABCAD = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABCAD'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NCL
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on
120700030721     C                   Z-ADD     PiVal         VABNCL
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABNCL = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200040122     C                             + ' ' + 'VABNCL'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** PKB
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiNum=*on
122000030721     C                   Z-ADD     PiVal         VABPKB
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      VABPKB = *zeros
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABPKB'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** CAS
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
123000141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
123100141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
123200141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
123300141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
123400141007     C                             %trim(SkSplitCSV(i)) <> ',00'
123500141007     C                   EVAL      VABVCA = 'EUR'
123600101018     C                   EVAL      wFlgCAS = '1'
123700101018     C                   ENDIF
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABCAS
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCAS = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABCAS'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** CCM
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on      AND
125400030721     C                             PiVal<=9999999 AND
125500030721     C                             PiVal>*zeros
125600030721     C                   Z-ADD     PiVal         VABCCM
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      VABCCM = *zeros
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100040510     C                             + ' ' + 'VABCCM'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** LNP
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on    AND
126900030721     C                             PiVal<=999   AND
127000030721     C                             PiVal>*zeros
127100030721     C                   Z-ADD     PiVal         VABLNP
127200030721     C                   ELSE
127300040715     C                   SETON                                        32
127400030721     C                   EVAL      VABLNP = *zeros
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600040510     C                             + ' ' + 'VABLNP'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** NRS
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on AND
128400030721     C                             PiVal<=99
128500030721     C                   Z-ADD     PiVal         VABNRS
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABNRS'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** LNA
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on    AND
129700030721     C                             PiVal<=999   AND
129800030721     C                             PiVal>*zeros
129900030721     C                   Z-ADD     PiVal         VABLNA
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      VABLNA = *zeros
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABLNA'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** CTR
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on  AND
131200030721     C                             PiVal<=999
131300030721     C                   Z-ADD     PiVal         VABCTR
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABCTR'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** IAS
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
132200141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
132300141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
132400141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
132500141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132600141007     C                             %trim(SkSplitCSV(i)) <> ',00'
132700141007     C                   EVAL      VABVAS = 'EUR'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiNum=*on
133100030721     C                   Z-ADD     PiVal         VABIAS
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABIAS'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800141007     C                   ENDIF
133900030721     C*** VLB
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiNum=*on
134400030721     C                   Z-ADD     PiVal         VABVLB
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABVLB'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** QFT
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiNum=*on
135600030721     C                   Z-ADD     PiVal         VABQFT
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABQFT'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300030721     C*** NCD
136400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiInt=*on      AND
136800030721     C                             PiVal<=9999999
136900030721     C                   Z-ADD     PiVal         VABNCD
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137300030721     C                             + ' ' + 'VABNCD'
137400030721     C                   ENDIF
137500030721     C                   ENDIF
137600030721     C*** NCA
137700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137800030721     C                   EVAL      PiStr=SkSplitCSV(i)
137900030721     C                   EXSR      CHKNUM
138000030721     C                   IF        PiInt=*on      AND
138100030721     C                             PiVal<=9999999
138200030721     C                   Z-ADD     PiVal         VABNCA
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600030721     C                             + ' ' + 'VABNCA'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900030721     C*** ZNC
139000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
139100030721     C                   EVAL      PiStr=SkSplitCSV(i)
139200030721     C                   EXSR      CHKNUM
139300030721     C                   IF        PiInt=*on AND
139400030721     C                             PiVal<=99
139500030721     C                   Z-ADD     PiVal         VABZNC
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABZNC'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200030721     C*** DCR
140300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
140400030721     C                   EVAL      PiStr=SkSplitCSV(i)
140500030721     C                   EXSR      CHKNUM
140600030721     C                   IF        PiInt=*on       AND
140700030721     C                             PiVal<=99999999
140800030721     C                   Z-ADD     PiVal         VABDCR
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABDCR'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500030721     C*** HCR
141600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141700030721     C                   EVAL      PiStr=SkSplitCSV(i)
141800030721     C                   EXSR      CHKNUM
141900030721     C                   IF        PiInt=*on   AND
142000030721     C                             PiVal<=9999
142100030721     C                   Z-ADD     PiVal         VABHCR
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142500030721     C                             + ' ' + 'VABHCR'
142600030721     C                   ENDIF
142700030721     C                   ENDIF
142800030721     C*** VMD
142900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
143000030721     C                   EVAL      PiStr=SkSplitCSV(i)
143100030721     C                   EXSR      CHKNUM
143200030721     C                   IF        PiNum=*on
143300030721     C                   Z-ADD     PiVal         VABVMD
143400030721     C                   ELSE
143500030721     C                   SETON                                        32
143600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143700030721     C                             + ' ' + 'VABVMD'
143800030721     C                   ENDIF
143900030721     C                   ENDIF
144000030721     C*** ANT
144100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
144200030721     C                   EVAL      PiStr=SkSplitCSV(i)
144300030721     C                   EXSR      CHKNUM
144400030721     C                   IF        PiInt=*on        AND
144500030721     C                             PiVal<=999999999
144600030721     C                   Z-ADD     PiVal         VABANT
144700030721     C                   ELSE
144800030721     C                   SETON                                        32
144900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145000030721     C                             + ' ' + 'VABANT'
145100030721     C                   ENDIF
145200030721     C                   ENDIF
145300030721     C*** CMO
145400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145500030721     C                   EVAL      PiStr=SkSplitCSV(i)
145600030721     C                   EXSR      CHKNUM
145700030721     C                   IF        PiInt=*on
145800030721     C                   Z-ADD     PiVal         Num5_0
145900030721     C                   MOVEL(p)  Num5_0        VABCMO
146000030721     C                   ELSE
146100030721     C                   SETON                                        32
146200030721     C                   EVAL      VABCMO = *zeros
146300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146400030721     C                             + ' ' + 'VABCMO'
146500030721     C                   ENDIF
146600030721     C                   ENDIF
146700040510     C*
146800110930     C* Reperisco i campi del EDIVAT
146900040510     C*** VATNOT
147000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
147100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
147200040510     C                   ENDIF
147300040510     C*** VATTRC
147400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147600040510     C                   ENDIF
147700060331     C*** VATNOT_A
147800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
148000060331     C                   ENDIF
148100060331     C*** VATNOT_B
148200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
148300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148400060331     C                   ENDIF
148500060331     C*** VATNOT_E
148600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148800060331     C                   ENDIF
148900110927     C*** VATNOT_I
149000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
149100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
149200110927     C                   ENDIF
149300141007     C*** VATNOT_IJ
149400141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
149500141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149600141007     C                   ENDIF
149700141007     C*** VATNOT_S
149800141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149900141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
150000141007     C                   ENDIF
150100070823     C*
150200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150300070925     C*
150400161102     C* PKB
150500161102     C* nel campo W_VABPKB c'è la taglia che memorizzo per calcolare il peso fuori ciclo
150600161102     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
150700161102     C                   EVAL      wTaglia = %trim(SkSplitCSV(i))
150800150601     C                   ENDIF
150900161102     C* VATNOT_E
151000161102     C* nel campo wVATNOT_E andrà poi sviluppato
151100161102     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
151200161102     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
151300161102     C                   ENDIF
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
152400161102     C* PKB
152500161102     C* nel campo W_VABPKB c'è la taglia e il peso va calcolato motiplicando il numero colli VABNCL
152600161102     C* per il valore indicato nella seguente tabella:
152700161102     C*   Taglia  Peso kg
152800161102     C*   XS            1
152900161102     C*   S             3
153000161102     C*   M             5
153100161102     C*   L            10
153200161102     C*   XL           20
153300161102     C                   SELECT
153400161102     C                   WHEN      wTaglia = 'XS'
153500161102     C                   EVAL      VABPKB = VABNCL * 1
153600161102     C                   WHEN      wTaglia = 'S'
153700161102     C                   EVAL      VABPKB = VABNCL * 3
153800161102     C                   WHEN      wTaglia = 'M'
153900161102     C                   EVAL      VABPKB = VABNCL * 5
154000161102     C                   WHEN      wTaglia = 'L'
154100161102     C                   EVAL      VABPKB = VABNCL * 10
154200161102     C                   WHEN      wTaglia = 'XL'
154300161102     C                   EVAL      VABPKB = VABNCL * 20
154400161102     C                   ENDSL
154500070925     C*
154600071029     C***  <===  -----------------------------------------
154700070925     C*
154800070925     C* Considerazioni finali su CBO/CAS
154900100318     C                   IF        wFlgCAS = '1'    OR
155000100318     C                             VABCAS  > *zeros
155100130403     C                   IF        VABVCA = *blanks
155200130403     C                   EVAL      VABVCA = 'EUR'
155300130403     C                   EVAL      VABTIC = *blanks
155400130403     C                   ENDIF
155500070925     C                   IF        VABCBO = '1'
155600070925     C                   EVAL      VABCBO = '4'
155700070925     C                   ENDIF
155800070925     C                   IF        VABCBO = '2'
155900070925     C                   EVAL      VABCBO = '6'
156000070925     C                   ENDIF
156100130403     C                   ELSE
156200130403     C                   EVAL      vabVCA = *blank
156300070925     C                   ENDIF
156400070925     C***  <===  ----------------------------
156500040510     C*
156600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156700040510     C                   IF        VABNSP = *zeros
156800040510     C                   SETON                                        31
156900040510     C                   ENDIF
157000040715     C*
157100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
157200040715     C                   IF        VABLNP = *zeros
157300040715     C                   SETON                                        31
157400040715     C                   ENDIF
157500040510     C*
157600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157700040510     C                   IF        VABRMN = *zeros
157800040510     C                   Z-ADD     VABNSP        VABRMN
157900040510     C                   ENDIF
158000040510     C*
158100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
158200040510     C                   IF        VABRMA = *blanks
158300040714     C                   MOVEL     VABRMN        VABRMA
158400040510     C                   ENDIF
158500040128     C*
158600040510     C* Se provincia nn valorizzata la reperisco
158700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158800110912     C                   IF        VABPRD =  *blanks AND
158900110912     C                             VABNZD =  *blanks AND
159000110912     C                             VABCAD <> *blanks AND
159100110912     C                             VABLOD <> *blanks
159200040128     C                   CLEAR                   TISI95DS
159300040128     C                   EVAL      I95TCN = '3'
159400040128     C                   Z-ADD     datcor        I95DAT
159500040128     C                   EVAL      I95NAR = VABNZD
159600040128     C                   EVAL      I95CAP = VABCAD
159700040128     C                   EVAL      I95LOC = VABLOD
159800040128     C                   CALL      'TISI95R'
159900040128     C                   PARM                    TISI95DS
160000040128     C                   EVAL      VABPRD = O95PRV
160100040510     C                   ENDIF
160200020204     C*
160300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
160400020204     C                   EXSR      CHKIMPDIV
160500030715     C                   ENDIF
160600020204     C*
160700010607     C                   ENDSR
160800010607     C*----------------------------------------------------*
160900010601
161000020204
161100020204     C*----------------------------------------------------*
161200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
161300020204     C*----------------------------------------------------*
161400020204     C     CHKIMPDIV     BEGSR
161500020204     C*
161600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
161700020204     C                   Z-ADD     *zeros        wrkDec            9 9
161800020204     C*
161900020204     C* Come prima cosa effettuo considerazioni sulla divisa
162000020204     C                   IF        vabIAS > *zeros
162100020204     C                   IF        vabVAS <> 'EUR'
162200020204     C                   EVAL      vabVAS =  'ITL'
162300020204     C                   ENDIF
162400020204     C                   ENDIF
162500020204     C*
162600020204     C                   IF        vabCAS > *zeros
162700020204     C                   IF        vabVCA <> 'EUR'
162800020204     C                   EVAL      vabVCA =  'ITL'
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C                   IF        vabVMD > *zeros
163300020204     C                   IF        vabVAD <> 'EUR'
163400020204     C                   EVAL      vabVAD =  'ITL'
163500020204     C                   ENDIF
163600020204     C                   ENDIF
163700020204     C*
163800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
163900020204     C                   Z-ADD     vabIAS        wrkDec
164000020204     C                   IF        wrkDec > *zeros
164100020204     C                   IF        vabVAS = 'ITL'
164200020204     C                   EVAL      vabIAS = *zeros
164300020204     C                   ENDIF
164400020204     C                   ENDIF
164500020204     C*
164600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
164700020204     C                   Z-ADD     vabCAS        wrkDec
164800020204     C                   IF        wrkDec > *zeros
164900020204     C                   IF        vabVCA = 'ITL'
165000020204     C                   EVAL      vabCAS = *zeros
165100020204     C                   ENDIF
165200020204     C                   ENDIF
165300020204     C*
165400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
165500020204     C                   Z-ADD     vabVMD        wrkDec
165600020204     C                   IF        wrkDec > *zeros
165700020204     C                   IF        vabVAD = 'ITL'
165800020204     C                   EVAL      vabVMD = *zeros
165900020204     C                   ENDIF
166000020204     C                   ENDIF
166100020204     C*
166200020204     C                   ENDSR
166300020204     C***
166400010330
166500100318
166600100318
166700100318     C*----------------------------------------------------*
166800100318     C*  CONTROLLO NUMERICITA' CAMPI
166900100318     C*----------------------------------------------------*
167000100318     C     CHKNUM        BEGSR
167100100318     C*
167200100318     C                   IF        PiDecChr = *blanks
167300100318     C                   EVAL      PiDecChr = CharNUM
167400100318     C                   ENDIF
167500100318     C*
167600100318     C                   callp     UBISNUM_Check(PiStr
167700100318     C                                          :PiDecChr
167800100318     C                                          :PiVal
167900100318     C                                          :PiNum
168000100318     C                                          :PiInt)
168100100318     C*
168200100318     C                   ENDSR
168300100318     C***
168400010601
168500010601
168600010601
168700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168800010601     C     repfil        BEGSR
168900010601     C*
169000010601     C                   if        invfil = *zeros and
169100010601     C                             depfil > *zeros and
169200010629     C                             (vinflg = *blanks or
169300010629     C                              vinflg = *zeros)
169400010601     C
169500010601     C                   eval      invfil = depfil
169600010601     C                   endif
169700010601     C*
169800010601     C                   if        depfil <> invfil and
169900010601     C                             invfil > *zeros
170000010601     C                   eval      flgMulti = '1'
170100010601     C                   if        vinflg = *blanks
170200010601     C                   add       1             cntNonEl
170300010601     C                   endif
170400010601     C                   endif
170500010601     C*
170600010601     C                   if        vinflg = '2'
170700010601     C                   eval      flgStato = '2'
170800010601     C                   endif
170900010601     C*
171000010601     C                   ENDSR
171100010601     C***
171200010601
171300010601
171400010601
171500010330
171600040119      /TITLE Invio dei dati al punto operativo.
171700040119     C     invio         BEGSR
171800040119     C*
171900110930     C* 1° invio EDIVAT
172000040119     C                   reset                   dscmz
172100040119     C                   move      vlrpoi        cmzdst
172200110930     C                   eval      cmzfld = 'EDIVATWR'
172300040119     C                   eval      cmzmbd = vlrhdl
172400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172500040119     C***                if        prmfir = *blanks
172600110930     C                   eval      cmzfla = 'EDIVAT0F'
172700110930     C                   eval      cmzmba = 'EDIVAT0F'
172800040119     C***                else
172900040119     C***                eval      cmzfla = prmfir
173000040119     C***                eval      cmzmba = prmfir
173100040119     C***                endif
173200040119     C                   eval      cmznrr = *zeros
173300040119     C                   move      §ctrokvt      cmznrr
173400040119     C                   eval      cmzlba = vlrfl1
173500040119     C                   call(e)   'TIS711C'
173600040119     C                   parm                    dscmz
173700040119     C                   parm      *blanks       esito
173800040119     C                   if        %error
173900040119     C                             or cmzerr = '1'
174000040119     C                             or esito  = '1'
174100040119     C                   eval      wrkesito = '3'
174200040119     C                   else
174300040119     C*
174400110930     C* 2° invio EDIVAB
174500040119     C                   reset                   dscmz
174600040119     C                   move      vlrpoi        cmzdst
174700040119     C                   eval      cmzfld = vlrfou
174800040119     C                   eval      cmzmbd = vlrhdl
174900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
175000040119     C***                if        prmfir = *blanks
175100110930     C                   eval      cmzfla = 'EDIVAB0F'
175200110930     C                   eval      cmzmba = 'EDIVAB0F'
175300040119     C***                else
175400040119     C***                eval      cmzfla = prmfir
175500040119     C***                eval      cmzmba = prmfir
175600040119     C***                endif
175700040119     C                   eval      cmznrr = *zeros
175800040119     C                   move      §ctrokvb      cmznrr
175900040119     C                   eval      cmzlba = vlrfl1
176000040119     C                   call(e)   'TIS711C'
176100040119     C                   parm                    dscmz
176200040119     C                   parm      *blanks       esito
176300040119     C                   if        %error
176400040119     C                             or cmzerr = '1'
176500040119     C                             or esito  = '1'
176600040119     C                   eval      wrkesito = '3'
176700040119     C                   endif
176800040119     C                   endif
176900040119     C*
177000040119     C                   ENDSR
177100040119     C***
177200010601
177300010601
177400010601
177500010601
177600010601      /TITLE Invio dei dati al punto operativo.
177700010601     C     opeini        BEGSR
177800010601     C*
177900010601     C* Inizializzo flag e contatori operativi
178000010601     C                   movel     '0'           flgGiro           1
178100010601     C                   movel     '0'           flgMulti          1
178200010601     C                   movel     '1'           flgStato          1
178300010615     C                   movel     '0'           flgOk             1
178400010601     C                   z-add     *zeros        cntNonEl         10 0
178500010601     C                   z-add     *zeros        depfil            3 0
178600010601     C                   z-add     *zeros        invfil            3 0
178700010601     C*
178800010601     C                   ENDSR
178900010601     C***
179000070326
179100070326
179200070326
179300070326
179400070326     C     *pssr         BEGSR
179500070329     C*
179600070329     C                   if        %open(tivin00r)
179700070329     C                   close     tivin00r
179800070329     C                   endif
179900110930     C                   if        %open(edivabwr)
180000110930     C                   close     edivabwr
180100070329     C                   endif
180200110930     C                   if        %open(edivatwr)
180300110930     C                   close     edivatwr
180400070329     C                   endif
180500070326     C*
180600070326     C* Effettuo la chiamata al CLLE preposto
180700110930     C                   call(e)   'TITVEVTC'
180800070326     C                   parm                    parccm
180900070326     C                   parm                    parmbr
181000070326     C                   parm      '2'           paropz
181100070326     C*
181200070326     C                   eval      wrkesito = '2'
181300070404     C*
181400070404     C                   seton                                        LR
181500070326     C*
181600070326     C                   ENDSR     '*CANCL'
181700070326     C***
181800100318
181900100318
182000100318
182100100318     C     normCSV       BEGSR
182200100318     C*
182300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
182400100318     C* separatori campi
182500100318     C                   if        CharTXT <> *blanks
182600100318     C                   z-add     *zeros        wLenVINDTA        4 0
182700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
182800100318     C                   setoff                                       80
182900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
183000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
183100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
183200100318     C                   if        not *in80
183300100318     C                   seton                                        80
183400100318     C                   else
183500100318     C                   setoff                                       80
183600100318     C                   endif
183700100318     C                   endif
183800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
183900100318     C                             and *in80 = *on
184000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
184100100318     C                   endif
184200100318     C                   enddo
184300100318     C                   endif
184400100318     C*
184500100318     C                   ENDSR
184600100318     C***
184700070326
184800100326
184900100326
185000100326     C*----------------------------------------------------*
185100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
185200100326     C*----------------------------------------------------*
185300100326     C     REPNSP        BEGSR
185400100326     C*
185500100326     C* NSP => Stacco un numeratore da AZNUM
185600100326     C                   clear                   TRUL33DS
185700100326     C                   eval      I33OPE = *zeros
185800100326     C                   eval      I33CNU = 302
185900100326     C                   eval      I33NUM = 1
186000100326     C                   movel     TRUL33DS      KPJBU
186100100326     C                   call      'TRUL33R'
186200100326     C                   parm                    KPJBA
186300100326     C                   movel     KPJBU         TRUL33DS
186400100326     C                   if        O33ERR = *zeros
186500100326     C                   z-add     O33NRF        VABNSP
186600100326     C                   z-add     O33NRF        VATNSP
186700100326     C                   else
186800100326     C                   SETON                                        31
186900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
187000100326     C                             + ' ' + 'VABNSP VATNSP'
187100100326     C                   endif
187200100326     C*
187300100326     C                   ENDSR
187400010330
187500100326
187600010330
187700000613     C     *inzsr        BEGSR
187800990910     C*
187900990910     C     *entry        plist
188000990920     C                   parm                    tivlrds
188100990921     C                   parm      wrkesito      esito
188200000724     C                   parm                    prmlit
188300000710     C                   parm                    prmfir
188400010330     C*
188500010330     C* CALCOLA LA DATA CORRENTE
188600110930     C                   time                    wn14             14 0
188700110930     C                   movel     wn14          oracor            6 0          *ORA
188800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188900100318     C                   eval      datcor = %dec(%date() : *ISO)
189000000613     C*
189100000613     C                   ENDSR
189200000613     C***
189300990908
189400120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189500161102X;VABRSD;VABRD2;X;VABIND;VABNOT;VABCAD;X;VABLOD;VABPRD;X;VABNCL;W_VABPKB;X;X;X;VABRMN;
189600161102X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;W_VATNOT_E;X;X;
189700161102
189800141007
189900141007
