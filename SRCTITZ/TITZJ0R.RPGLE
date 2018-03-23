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
003900170307     D Intestazioni    s               n   inz(*off)
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
046000170307     C* i "CHI SONO" non vanno sviluppati
046100170307     C                   eval      VATNOT = %trim(wVATNOT_E)
046200141007     C                   write     EDIVAT00
046300161102     C                   ENDIF
046400161102     C*
046500110927     C                   if        wVATNOT_I <> *blanks
046600110927     C                   eval      VATNOT = wVATNOT_I
046700110927     C                   eval      VATTRC = 'I'
046800120203     C                   MOVE(P)   vlrMSG        wNomeFile
046900130122     C***                EVAL      VATCMR = wNomeFile
047000141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
047100110930     C                   eval      VATCNT = 1
047200110930     C                   write     EDIVAT00
047300110927     C                   add       1             §CTROKVT
047400110927     C                   endif
047500120202     C*
047600120202     C                   if        wVATNOT_J <> *blanks
047700120202     C                   eval      VATNOT = wVATNOT_J
047800120202     C                   eval      VATTRC = 'J'
047900120203     C                   MOVE(P)   vlrMSG        wNomeFile
048000130122     C***                EVAL      VATCMR = wNomeFile
048100141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
048200120202     C                   eval      VATCNT = 1
048300120202     C                   write     EDIVAT00
048400120202     C                   add       1             §CTROKVT
048500120202     C                   endif
048600141007     C*
048700141007     C                   if        wVATNOT_S <> *blanks
048800141007     C                   eval      VATNOT = wVATNOT_S
048900141007     C* i 2 flag devono essere blank
049000141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
049100141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
049200141007     C                   eval      VATTRC = 'S'
049300141007     C***                EVAL      VATCMR = wNomeFile
049400141007     C                   eval      VATCMR = %char(datcor)+'-'+%char(oracor)
049500141007     C                   eval      VATCNT = 1
049600141007     C                   write     EDIVAT00
049700141007     C                   add       1             §CTROKVT
049800141007     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100040119
050200040119
050300040119
050400040119     C*----------------------------------------------------*
050500110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050600040119     C*----------------------------------------------------*
050700040119     C     PREVAT        BEGSR
050800040119     C*
050900110930     C* Compongo il nome del membro da dare al EDIVATWR
051000040119     C                   eval      parmbr = vlrhdl
051100040119     C                   movel     'M'           parmbr
051200040510     C                   eval      parccm = vlrksc
051300040119     C                   eval      paropz = '1'
051400040119     C* Effettuo la chiamata al CLLE preposto
051500110930     C                   call(e)   'TITVEVTC'
051600040119     C                   parm                    parccm
051700040119     C                   parm                    parmbr
051800040119     C                   parm                    paropz
051900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052000040119     C                   if        %error
052100040119     C                   movel     '1'           chkcall
052200040119     C                   else
052300040119     C                   movel     '0'           chkcall
052400040119     C                   endif
052500040119     C*
052600040119     C                   ENDSR
052700010601
052800010601
052900010601
053000010330     C*----------------------------------------------------*
053100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053200010330     C*----------------------------------------------------*
053300040119     C     INZVAR        BEGSR
053400080527     C*
053500080527     C                   setoff                                       65
053600010330     C*
053700030822     C* Inizializzo variabili di wrk
053800130128     C                   Z-ADD     *zeros        Num3_0            3 0
053900020204     C                   Z-ADD     *zeros        Num5_0            5 0
054000101018     C                   MOVEL     *blanks       wFlgCAS
054100060331     C                   MOVEL     *blanks       wVATNOT_A
054200060331     C                   MOVEL     *blanks       wVATNOT_B
054300060331     C                   MOVEL     *blanks       wVATNOT_E
054400110927     C                   MOVEL     *blanks       wVATNOT_I
054500120202     C                   MOVEL     *blanks       wVATNOT_J
054600120202     C                   MOVEL     *blanks       wVATNOT_IJ
054700141007     C                   MOVEL     *blanks       wVATNOT_S
054800161102     C                   MOVEL     *blanks       wTaglia
054900030822     C*
055000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055100030822     C                   CLEAR                   SkSplitCSV
055200030822     C                   EVAL      i = 1
055300030822     C                   EVAL      posDa = *zeros
055400030822     C                   EVAL      posA  = *zeros
055500080527     C*
055600110930     C                   CLEAR                   EDIVABS
055700110930     C                   CLEAR                   EDIVAT00
055800030822     C*
055900030822     C* Reimposto i valori di default
056000030822     C                   EXSR      DEFCAM
056100030822     C*
056200010330     C                   ENDSR
056300010330     C*----------------------------------------------------*
056400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056500010330     C*----------------------------------------------------*
056600010330     C     DEFCAM        BEGSR
056700030822     C*
056800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056900030715     C* e delimitatore testo.
057000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057300100318     C*
057400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057500100318     C                   EVAL      CharSOS = CharNUM
057600030822     C*
057700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057800100720     C                   SETOFF                                       505152
057900100720     C                   SETOFF                                       53
058000100413     C                   SELECT
058100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058200100720     C                   SETON                                        50
058300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058400100413     C                   SETON                                        51
058500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058600100720     C                   SETON                                        52
058700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058800100720     C                   SETON                                        53
058900100413     C                   ENDSL
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400030822     C* NCL
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'NCL'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABNCL
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* CCM
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'CCM'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABCCM
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* LNP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'LNP'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABLNP
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* NRS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NRS'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNRS
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CTR
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTR'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCTR
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* PKB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'PKB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABPKB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* VLB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VLB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABVLB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* QFT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'QFT'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABQFT
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* CBO
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CBO'
072300030822     C                   EVAL      VABCBO=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TSP
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TSP'
073200030822     C                   EVAL      VABTSP=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAS
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAS'
074100030822     C                   EVAL      VABVAS=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VCA'
075000030822     C                   EVAL      VABVCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400070925     C* TIC
075500070925     C                   IF        %subst(
075600070925     C                             %subst(vlrppt:posDaDft+1:
075700070925     C                             posADft-posDaDft-1):1:3)
075800070925     C                             = 'TIC'
075900070925     C                   EVAL      VABTIC=%trim(%subst(
076000070925     C                             %subst(vlrppt:posDaDft+1:
076100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200070925     C                   ENDIF
076300030822     C* GCA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GCA'
076800030822     C                   EVAL      VABGCA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* CTM
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'CTM'
077700030822     C                   EVAL      VABCTM=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* FFD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'FFD'
078600030822     C                   EVAL      VABFFD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* VAD
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'VAD'
079500030822     C                   EVAL      VABVAD=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GMA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GMA'
080400030822     C                   EVAL      VABGMA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GGA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GGA'
081300030822     C                   EVAL      VABGGA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GVA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GVA'
082200030822     C                   EVAL      VABGVA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC1
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC1'
083100030822     C                   EVAL      VABTC1=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* TC2
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'TC2'
084000030822     C                   EVAL      VABTC2=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400040714     C* VATTRC
084500040714     C                   IF        %subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):1:3)
084800040714     C                             = 'TRC'
084900040714     C                   EVAL      VATTRC=%trim(%subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040714     C                   ENDIF
085300030822     C* ...
085400030822     C                   ENDIF
085500030822     C                   ENDDO
085600020204     C*
085700010330     C                   ENDSR
085800010607     C*----------------------------------------------------*
085900110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400110315 xxx C***                SETON                                        35
086500030515     C*
086600010607     C                   MOVEL     datcor        VABAAS
086700010607     C                   MOVE      datcor        VABMGS
086800010607     C*
086900030715     C**********
087000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087100030715     C**********
087200030715     C                   IF        wGiro = *zeros
087300040113     C*
087400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087700080429     C                             %trim(intcol(5))<>*blanks
087800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088000080429     C                                      %trim(intcol(5))
088100070823     C     ';':CharCSV   XLATE     vindta        vindta
088200040510     C                   ENDIF
088300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088400031201     C     minu:maiu     XLATE     vindta        vindta
088500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088600030715     C                   DOW       posDa <= %len(%trim(vindta))
088700030715     C*
088800030715     C* Gestisco il 1° campo
088900030715     C                   IF        i = 1
089000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089100030715     C                   EVAL      posDa = 1
089200030715     C* Eseguo lo scan x trovare la fine del primo campo
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C* X i campi successivi al 1°
089800030715     C                   ELSE
089900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090000030715     C                   EVAL      posDa = posA + 1
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500030715     C                   ENDIF
091600030715     C* Incremento il contatore d campo
091700030715     C                   EVAL      i = i +1
091800030715     C                   ENDDO
091900030715     C*
092000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092100030715     C* delimitazione testo
092200030715     C                   EVAL      i = 1
092300030715     C                   DOW       i < %elem(SkSplitFLD)
092400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092600030715     C                   EVAL      i = i + 1
092700030715     C                   ENDDO
092800030715     C                   EVAL      wGiro = 1
092900030715     C                   ELSE
093000030715     C                   EVAL      wGiro = 2
093100031204     C**********
093200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093300031204     C* d separatore campo
093400031204     C**********
093500031204     C                   EVAL      vindta = %trim(vindta)
093600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093700031204     C                   EVAL      vindta = %subst(vindta:2)
093800031204     C                   ENDDO
093900031204     C*
094000031204     C                   Z-ADD     *zeros        lunghInput        4 0
094100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500110912     C                   ENDDO
094600030715     C**********
094700030715     C* Effettuo lo split del campo dati d input
094800030715     C**********
094900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095000030715     C                   DOW       posDa <= %len(%trim(vindta))
095100030715     C*
095200030715     C* Gestisco il 1° campo
095300030715     C                   IF        i = 1
095400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095500030715     C                   EVAL      posDa = 1
095600030715     C* Eseguo lo scan x trovare la fine del primo campo
095700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C* X i campi successivi al 1°
096200030715     C                   ELSE
096300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096400030715     C                   EVAL      posDa = posA + 1
096500031204     C* Verifico che nn vi sia il campo nn valorizzato
096600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096700031204     C* Se campo nn valorizzato skippo al prossimo
096800031204     C                   EVAL      SkSplitCSV(i) = *blanks
096900031204     C                   EVAL      posA  = posA + 1
097000031204     C                   ELSE
097100030715     C* Eseguo lo scan x trovare la fine del campo corrente
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* Gestisco l'ultimo campo
097400030715     C                   IF        posA = *zeros
097500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097600030715     C* A questo punto "estraggo" il campo corrente
097700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097800030715     C                                                    (posA-posDa))
097900030715     C                   LEAVE
098000030715     C                   ELSE
098100030715     C* A questo punto "estraggo" il campo corrente
098200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098300030715     C                                                    (posA-posDa))
098400030715     C                   ENDIF
098500031204     C                   ENDIF
098600030715     C                   ENDIF
098700030715     C* Incremento il contatore d campo
098800030715     C                   EVAL      i = i +1
098900030715     C                   ENDDO
099000030715     C*
099100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099200030715     C* delimitazione testo
099300030715     C                   EVAL      i = 1
099400030715     C                   DOW       i < %elem(SkSplitCSV)
099500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099700030715     C                   EVAL      i = i + 1
099800030715     C                   ENDDO
099900100720     C*
100000100720     C* Stacco un progressivo x numero spedizione
100100110930 xxx C                   EXSR      repNSP
100200100326     C*
100300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100400030715     C                   EVAL      i = 1
100500030715     C                   DOW       i < %elem(SkSplitFLD)
100600030715     C*
100700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100900030715     C                             SkSplitCSV(i) <> *blanks
101000030721     C*** ATB
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101200030721     C                   EVAL      VABATB = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RSD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101900030715     C* ==
102000030715     C                   ENDIF
102100030721     C*** RD2
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** IND
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102700030715     C                   EVAL      VABIND = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** LOD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** PRD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030721     C*** NZD
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030715     C*** RMA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** NOT
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NT2
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** VCA
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VAS
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** FFD
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030721     C*** GC1
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GC2
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TSP
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107600030721     C                   ENDIF
107700040510     C*** CBO
107800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108000040510     C                   ENDIF
108100030721     C*** NAS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TIC
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GCA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** XCO
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** CTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** TCR
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** CTS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** FTM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** VAD
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** GMA
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GGA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GVA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** TC1
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC2
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** SCL
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** RMO
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** NMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*
115000030721     C* Reperisco quindi i campi numerici...
115100040510     C*** NSP
115200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115300040510     C                   EVAL      PiStr=SkSplitCSV(i)
115400040510     C                   EXSR      CHKNUM
115500040510     C                   IF        PiInt=*on
115600040510     C                   Z-ADD     PiVal         VABNSP
115700040510     C                   ELSE
115800040510     C                   SETON                                        31
115900040510     C                   EVAL      VABNSP = *zeros
116000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040510     C                             + ' ' + 'VABNSP'
116200040510     C                   ENDIF
116300040510     C                   ENDIF
116400071106     C*** RMN
116500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116600071106     C                   EVAL      PiStr=SkSplitCSV(i)
116700071106     C                   EXSR      CHKNUM
116800071106     C                   IF        PiInt=*on
116900071106     C                   Z-ADD     PiVal         VABRMN
117000071106     C                   ELSE
117100071106     C                   SETON                                        32
117200071106     C                   EVAL      VABRMN = *zeros
117300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117400071106     C                             + ' ' + 'VABRMN'
117500071106     C                   ENDIF
117600071106     C                   ENDIF
117700030721     C*** CAD
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         Num5_0
118300030721     C                   MOVEL(p)  Num5_0        VABCAD
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABCAD = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABCAD'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** NCL
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on
119600030721     C                   Z-ADD     PiVal         VABNCL
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABNCL = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040122     C                             + ' ' + 'VABNCL'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** PKB
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABPKB
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABPKB = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABPKB'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** CAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121900141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122000141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122100141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122200141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122300141007     C                             %trim(SkSplitCSV(i)) <> ',00'
122400141007     C                   EVAL      VABVCA = 'EUR'
122500101018     C                   EVAL      wFlgCAS = '1'
122600101018     C                   ENDIF
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiNum=*on
123000030721     C                   Z-ADD     PiVal         VABCAS
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABCAS = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCAS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** CCM
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on      AND
124300030721     C                             PiVal<=9999999 AND
124400030721     C                             PiVal>*zeros
124500030721     C                   Z-ADD     PiVal         VABCCM
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      VABCCM = *zeros
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040510     C                             + ' ' + 'VABCCM'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** LNP
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on    AND
125800030721     C                             PiVal<=999   AND
125900030721     C                             PiVal>*zeros
126000030721     C                   Z-ADD     PiVal         VABLNP
126100030721     C                   ELSE
126200040715     C                   SETON                                        32
126300030721     C                   EVAL      VABLNP = *zeros
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500040510     C                             + ' ' + 'VABLNP'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** NRS
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on AND
127300030721     C                             PiVal<=99
127400030721     C                   Z-ADD     PiVal         VABNRS
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABNRS'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** LNA
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on    AND
128600030721     C                             PiVal<=999   AND
128700030721     C                             PiVal>*zeros
128800030721     C                   Z-ADD     PiVal         VABLNA
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      VABLNA = *zeros
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABLNA'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** CTR
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on  AND
130100030721     C                             PiVal<=999
130200030721     C                   Z-ADD     PiVal         VABCTR
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABCTR'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** IAS
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131100141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131200141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131300141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131400141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
131500141007     C                             %trim(SkSplitCSV(i)) <> ',00'
131600141007     C                   EVAL      VABVAS = 'EUR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiNum=*on
132000030721     C                   Z-ADD     PiVal         VABIAS
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABIAS'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700141007     C                   ENDIF
132800030721     C*** VLB
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiNum=*on
133300030721     C                   Z-ADD     PiVal         VABVLB
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABVLB'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** QFT
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiNum=*on
134500030721     C                   Z-ADD     PiVal         VABQFT
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABQFT'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** NCD
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on      AND
135700030721     C                             PiVal<=9999999
135800030721     C                   Z-ADD     PiVal         VABNCD
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABNCD'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** NCA
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on      AND
137000030721     C                             PiVal<=9999999
137100030721     C                   Z-ADD     PiVal         VABNCA
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABNCA'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** ZNC
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on AND
138300030721     C                             PiVal<=99
138400030721     C                   Z-ADD     PiVal         VABZNC
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138800030721     C                             + ' ' + 'VABZNC'
138900030721     C                   ENDIF
139000030721     C                   ENDIF
139100030721     C*** DCR
139200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139300030721     C                   EVAL      PiStr=SkSplitCSV(i)
139400030721     C                   EXSR      CHKNUM
139500030721     C                   IF        PiInt=*on       AND
139600030721     C                             PiVal<=99999999
139700030721     C                   Z-ADD     PiVal         VABDCR
139800030721     C                   ELSE
139900030721     C                   SETON                                        32
140000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140100030721     C                             + ' ' + 'VABDCR'
140200030721     C                   ENDIF
140300030721     C                   ENDIF
140400030721     C*** HCR
140500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140600030721     C                   EVAL      PiStr=SkSplitCSV(i)
140700030721     C                   EXSR      CHKNUM
140800030721     C                   IF        PiInt=*on   AND
140900030721     C                             PiVal<=9999
141000030721     C                   Z-ADD     PiVal         VABHCR
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABHCR'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** VMD
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiNum=*on
142200030721     C                   Z-ADD     PiVal         VABVMD
142300030721     C                   ELSE
142400030721     C                   SETON                                        32
142500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142600030721     C                             + ' ' + 'VABVMD'
142700030721     C                   ENDIF
142800030721     C                   ENDIF
142900030721     C*** ANT
143000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143100030721     C                   EVAL      PiStr=SkSplitCSV(i)
143200030721     C                   EXSR      CHKNUM
143300030721     C                   IF        PiInt=*on        AND
143400030721     C                             PiVal<=999999999
143500030721     C                   Z-ADD     PiVal         VABANT
143600030721     C                   ELSE
143700030721     C                   SETON                                        32
143800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143900030721     C                             + ' ' + 'VABANT'
144000030721     C                   ENDIF
144100030721     C                   ENDIF
144200030721     C*** CMO
144300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144400030721     C                   EVAL      PiStr=SkSplitCSV(i)
144500030721     C                   EXSR      CHKNUM
144600030721     C                   IF        PiInt=*on
144700030721     C                   Z-ADD     PiVal         Num5_0
144800030721     C                   MOVEL(p)  Num5_0        VABCMO
144900030721     C                   ELSE
145000030721     C                   SETON                                        32
145100030721     C                   EVAL      VABCMO = *zeros
145200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145300030721     C                             + ' ' + 'VABCMO'
145400030721     C                   ENDIF
145500030721     C                   ENDIF
145600040510     C*
145700110930     C* Reperisco i campi del EDIVAT
145800040510     C*** VATNOT
145900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146100040510     C                   ENDIF
146200040510     C*** VATTRC
146300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146500040510     C                   ENDIF
146600060331     C*** VATNOT_A
146700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
146800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
146900060331     C                   ENDIF
147000060331     C*** VATNOT_B
147100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147300060331     C                   ENDIF
147400060331     C*** VATNOT_E
147500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
147600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147700060331     C                   ENDIF
147800110927     C*** VATNOT_I
147900110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148000110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148100110927     C                   ENDIF
148200141007     C*** VATNOT_IJ
148300141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148400141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148500141007     C                   ENDIF
148600141007     C*** VATNOT_S
148700141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
148800141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
148900141007     C                   ENDIF
149000070823     C*
149100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149200130403
149300130403     C*
149400071029     C***  <===  -----------------------------------------
149500040510     C*
149600030715     C                   ENDIF
149700030715     C                   EVAL      i = i + 1
149800030715     C                   ENDDO
149900070925     C*
150000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150100121030     C*
150200161102     C* PKB
150300161102     C* nel campo W_VABPKB c'è la taglia e il peso va calcolato motiplicando il numero colli VABNCL
150400161102     C* per il valore indicato nella seguente tabella:
150500161102     C*   Taglia  Peso kg
150600161102     C*   XS            1
150700161102     C*   S             3
150800161102     C*   M             5
150900161102     C*   L            10
151000161102     C*   XL           20
151100161102     C                   SELECT
151200161102     C                   WHEN      wTaglia = 'XS'
151300161102     C                   EVAL      VABPKB = VABNCL * 1
151400161102     C                   WHEN      wTaglia = 'S'
151500161102     C                   EVAL      VABPKB = VABNCL * 3
151600161102     C                   WHEN      wTaglia = 'M'
151700161102     C                   EVAL      VABPKB = VABNCL * 5
151800161102     C                   WHEN      wTaglia = 'L'
151900161102     C                   EVAL      VABPKB = VABNCL * 10
152000161102     C                   WHEN      wTaglia = 'XL'
152100161102     C                   EVAL      VABPKB = VABNCL * 20
152200161102     C                   ENDSL
152300070925     C*
152400071029     C***  <===  -----------------------------------------
152500070925     C*
152600070925     C* Considerazioni finali su CBO/CAS
152700100318     C                   IF        wFlgCAS = '1'    OR
152800100318     C                             VABCAS  > *zeros
152900130403     C                   IF        VABVCA = *blanks
153000130403     C                   EVAL      VABVCA = 'EUR'
153100130403     C                   EVAL      VABTIC = *blanks
153200130403     C                   ENDIF
153300070925     C                   IF        VABCBO = '1'
153400070925     C                   EVAL      VABCBO = '4'
153500070925     C                   ENDIF
153600070925     C                   IF        VABCBO = '2'
153700070925     C                   EVAL      VABCBO = '6'
153800070925     C                   ENDIF
153900130403     C                   ELSE
154000130403     C                   EVAL      vabVCA = *blank
154100070925     C                   ENDIF
154200070925     C***  <===  ----------------------------
154300040510     C*
154400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154500040510     C                   IF        VABNSP = *zeros
154600040510     C                   SETON                                        31
154700040510     C                   ENDIF
154800040715     C*
154900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155000040715     C                   IF        VABLNP = *zeros
155100040715     C                   SETON                                        31
155200040715     C                   ENDIF
155300040510     C*
155400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155500040510     C                   IF        VABRMN = *zeros
155600040510     C                   Z-ADD     VABNSP        VABRMN
155700040510     C                   ENDIF
155800040510     C*
155900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156000040510     C                   IF        VABRMA = *blanks
156100040714     C                   MOVEL     VABRMN        VABRMA
156200040510     C                   ENDIF
156300040128     C*
156400040510     C* Se provincia nn valorizzata la reperisco
156500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156600110912     C                   IF        VABPRD =  *blanks AND
156700110912     C                             VABNZD =  *blanks AND
156800110912     C                             VABCAD <> *blanks AND
156900110912     C                             VABLOD <> *blanks
157000040128     C                   CLEAR                   TISI95DS
157100040128     C                   EVAL      I95TCN = '3'
157200040128     C                   Z-ADD     datcor        I95DAT
157300040128     C                   EVAL      I95NAR = VABNZD
157400040128     C                   EVAL      I95CAP = VABCAD
157500040128     C                   EVAL      I95LOC = VABLOD
157600040128     C                   CALL      'TISI95R'
157700040128     C                   PARM                    TISI95DS
157800040128     C                   EVAL      VABPRD = O95PRV
157900040510     C                   ENDIF
158000020204     C*
158100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158200020204     C                   EXSR      CHKIMPDIV
158300030715     C                   ENDIF
158400020204     C*
158500010607     C                   ENDSR
158600010607     C*----------------------------------------------------*
158700010601
158800020204
158900020204     C*----------------------------------------------------*
159000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159100020204     C*----------------------------------------------------*
159200020204     C     CHKIMPDIV     BEGSR
159300020204     C*
159400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159500020204     C                   Z-ADD     *zeros        wrkDec            9 9
159600020204     C*
159700020204     C* Come prima cosa effettuo considerazioni sulla divisa
159800020204     C                   IF        vabIAS > *zeros
159900020204     C                   IF        vabVAS <> 'EUR'
160000020204     C                   EVAL      vabVAS =  'ITL'
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C                   IF        vabCAS > *zeros
160500020204     C                   IF        vabVCA <> 'EUR'
160600020204     C                   EVAL      vabVCA =  'ITL'
160700020204     C                   ENDIF
160800020204     C                   ENDIF
160900020204     C*
161000020204     C                   IF        vabVMD > *zeros
161100020204     C                   IF        vabVAD <> 'EUR'
161200020204     C                   EVAL      vabVAD =  'ITL'
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161700020204     C                   Z-ADD     vabIAS        wrkDec
161800020204     C                   IF        wrkDec > *zeros
161900020204     C                   IF        vabVAS = 'ITL'
162000020204     C                   EVAL      vabIAS = *zeros
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162500020204     C                   Z-ADD     vabCAS        wrkDec
162600020204     C                   IF        wrkDec > *zeros
162700020204     C                   IF        vabVCA = 'ITL'
162800020204     C                   EVAL      vabCAS = *zeros
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163300020204     C                   Z-ADD     vabVMD        wrkDec
163400020204     C                   IF        wrkDec > *zeros
163500020204     C                   IF        vabVAD = 'ITL'
163600020204     C                   EVAL      vabVMD = *zeros
163700020204     C                   ENDIF
163800020204     C                   ENDIF
163900020204     C*
164000020204     C                   ENDSR
164100020204     C***
164200010330
164300100318
164400100318
164500100318     C*----------------------------------------------------*
164600100318     C*  CONTROLLO NUMERICITA' CAMPI
164700100318     C*----------------------------------------------------*
164800100318     C     CHKNUM        BEGSR
164900100318     C*
165000100318     C                   IF        PiDecChr = *blanks
165100100318     C                   EVAL      PiDecChr = CharNUM
165200100318     C                   ENDIF
165300100318     C*
165400100318     C                   callp     UBISNUM_Check(PiStr
165500100318     C                                          :PiDecChr
165600100318     C                                          :PiVal
165700100318     C                                          :PiNum
165800100318     C                                          :PiInt)
165900100318     C*
166000100318     C                   ENDSR
166100100318     C***
166200010601
166300010601
166400010601
166500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166600010601     C     repfil        BEGSR
166700010601     C*
166800010601     C                   if        invfil = *zeros and
166900010601     C                             depfil > *zeros and
167000010629     C                             (vinflg = *blanks or
167100010629     C                              vinflg = *zeros)
167200010601     C
167300010601     C                   eval      invfil = depfil
167400010601     C                   endif
167500010601     C*
167600010601     C                   if        depfil <> invfil and
167700010601     C                             invfil > *zeros
167800010601     C                   eval      flgMulti = '1'
167900010601     C                   if        vinflg = *blanks
168000010601     C                   add       1             cntNonEl
168100010601     C                   endif
168200010601     C                   endif
168300010601     C*
168400010601     C                   if        vinflg = '2'
168500010601     C                   eval      flgStato = '2'
168600010601     C                   endif
168700010601     C*
168800010601     C                   ENDSR
168900010601     C***
169000010601
169100010601
169200010601
169300010330
169400040119      /TITLE Invio dei dati al punto operativo.
169500040119     C     invio         BEGSR
169600040119     C*
169700110930     C* 1° invio EDIVAT
169800040119     C                   reset                   dscmz
169900040119     C                   move      vlrpoi        cmzdst
170000110930     C                   eval      cmzfld = 'EDIVATWR'
170100040119     C                   eval      cmzmbd = vlrhdl
170200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170300040119     C***                if        prmfir = *blanks
170400110930     C                   eval      cmzfla = 'EDIVAT0F'
170500110930     C                   eval      cmzmba = 'EDIVAT0F'
170600040119     C***                else
170700040119     C***                eval      cmzfla = prmfir
170800040119     C***                eval      cmzmba = prmfir
170900040119     C***                endif
171000040119     C                   eval      cmznrr = *zeros
171100040119     C                   move      §ctrokvt      cmznrr
171200040119     C                   eval      cmzlba = vlrfl1
171300040119     C                   call(e)   'TIS711C'
171400040119     C                   parm                    dscmz
171500040119     C                   parm      *blanks       esito
171600040119     C                   if        %error
171700040119     C                             or cmzerr = '1'
171800040119     C                             or esito  = '1'
171900040119     C                   eval      wrkesito = '3'
172000040119     C                   else
172100040119     C*
172200110930     C* 2° invio EDIVAB
172300040119     C                   reset                   dscmz
172400040119     C                   move      vlrpoi        cmzdst
172500040119     C                   eval      cmzfld = vlrfou
172600040119     C                   eval      cmzmbd = vlrhdl
172700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172800040119     C***                if        prmfir = *blanks
172900110930     C                   eval      cmzfla = 'EDIVAB0F'
173000110930     C                   eval      cmzmba = 'EDIVAB0F'
173100040119     C***                else
173200040119     C***                eval      cmzfla = prmfir
173300040119     C***                eval      cmzmba = prmfir
173400040119     C***                endif
173500040119     C                   eval      cmznrr = *zeros
173600040119     C                   move      §ctrokvb      cmznrr
173700040119     C                   eval      cmzlba = vlrfl1
173800040119     C                   call(e)   'TIS711C'
173900040119     C                   parm                    dscmz
174000040119     C                   parm      *blanks       esito
174100040119     C                   if        %error
174200040119     C                             or cmzerr = '1'
174300040119     C                             or esito  = '1'
174400040119     C                   eval      wrkesito = '3'
174500040119     C                   endif
174600040119     C                   endif
174700040119     C*
174800040119     C                   ENDSR
174900040119     C***
175000010601
175100010601
175200010601
175300010601
175400010601      /TITLE Invio dei dati al punto operativo.
175500010601     C     opeini        BEGSR
175600010601     C*
175700010601     C* Inizializzo flag e contatori operativi
175800010601     C                   movel     '0'           flgGiro           1
175900010601     C                   movel     '0'           flgMulti          1
176000010601     C                   movel     '1'           flgStato          1
176100010615     C                   movel     '0'           flgOk             1
176200010601     C                   z-add     *zeros        cntNonEl         10 0
176300010601     C                   z-add     *zeros        depfil            3 0
176400010601     C                   z-add     *zeros        invfil            3 0
176500010601     C*
176600010601     C                   ENDSR
176700010601     C***
176800070326
176900070326
177000070326
177100070326
177200070326     C     *pssr         BEGSR
177300070329     C*
177400070329     C                   if        %open(tivin00r)
177500070329     C                   close     tivin00r
177600070329     C                   endif
177700110930     C                   if        %open(edivabwr)
177800110930     C                   close     edivabwr
177900070329     C                   endif
178000110930     C                   if        %open(edivatwr)
178100110930     C                   close     edivatwr
178200070329     C                   endif
178300070326     C*
178400070326     C* Effettuo la chiamata al CLLE preposto
178500110930     C                   call(e)   'TITVEVTC'
178600070326     C                   parm                    parccm
178700070326     C                   parm                    parmbr
178800070326     C                   parm      '2'           paropz
178900070326     C*
179000070326     C                   eval      wrkesito = '2'
179100070404     C*
179200070404     C                   seton                                        LR
179300070326     C*
179400070326     C                   ENDSR     '*CANCL'
179500070326     C***
179600100318
179700100318
179800100318
179900100318     C     normCSV       BEGSR
180000100318     C*
180100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180200100318     C* separatori campi
180300100318     C                   if        CharTXT <> *blanks
180400100318     C                   z-add     *zeros        wLenVINDTA        4 0
180500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180600100318     C                   setoff                                       80
180700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181000100318     C                   if        not *in80
181100100318     C                   seton                                        80
181200100318     C                   else
181300100318     C                   setoff                                       80
181400100318     C                   endif
181500100318     C                   endif
181600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181700100318     C                             and *in80 = *on
181800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181900100318     C                   endif
182000100318     C                   enddo
182100100318     C                   endif
182200100318     C*
182300100318     C                   ENDSR
182400100318     C***
182500070326
182600100326
182700100326
182800100326     C*----------------------------------------------------*
182900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183000100326     C*----------------------------------------------------*
183100100326     C     REPNSP        BEGSR
183200100326     C*
183300100326     C* NSP => Stacco un numeratore da AZNUM
183400100326     C                   clear                   TRUL33DS
183500100326     C                   eval      I33OPE = *zeros
183600100326     C                   eval      I33CNU = 302
183700100326     C                   eval      I33NUM = 1
183800100326     C                   movel     TRUL33DS      KPJBU
183900100326     C                   call      'TRUL33R'
184000100326     C                   parm                    KPJBA
184100100326     C                   movel     KPJBU         TRUL33DS
184200100326     C                   if        O33ERR = *zeros
184300100326     C                   z-add     O33NRF        VABNSP
184400100326     C                   z-add     O33NRF        VATNSP
184500100326     C                   else
184600100326     C                   SETON                                        31
184700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184800100326     C                             + ' ' + 'VABNSP VATNSP'
184900100326     C                   endif
185000100326     C*
185100100326     C                   ENDSR
185200010330
185300100326
185400010330
185500000613     C     *inzsr        BEGSR
185600990910     C*
185700990910     C     *entry        plist
185800990920     C                   parm                    tivlrds
185900990921     C                   parm      wrkesito      esito
186000000724     C                   parm                    prmlit
186100000710     C                   parm                    prmfir
186200010330     C*
186300010330     C* CALCOLA LA DATA CORRENTE
186400110930     C                   time                    wn14             14 0
186500110930     C                   movel     wn14          oracor            6 0          *ORA
186600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186700100318     C                   eval      datcor = %dec(%date() : *ISO)
186800000613     C*
186900000613     C                   ENDSR
187000000613     C***
187100990908
187200120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187300170307X;X;X;VABCCM;X;VABRMA;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
187400170307X;X;X;X;X;X;VATNOT_E;X;X;X;X;VABPKB;X;X;X;X;VABNOT;VABNT2;X;VABRMN
187500161102
187600141007
187700141007
