000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400150528      * il loro nr.DDT è sia VABNSP sia VABRMN sia VABNSP,
000500150528      *  i numerici cono solo gli ultimi char a dx numerici
000600150528      *  mentre RMA è la composizione dei primi 6 char + dall'11° in poi
000700150528      * il peso sarà sempre 0
000800150528      * Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
000900130122      *
001000121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
001400110930     FEDIVATwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700040128     D tisi95ds      e ds
002800110930     D edivabs       e ds                  extname(edivab0f)
002900110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
003000110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
003100010330     D esito           s              1
003200010330     D prmlit          s             10
003300010330     D prmfir          s             10
003400010330     D wrkesito        s                   like(esito)
003500010330     D rrnum           s              6  0 INZ(*zeros)
003600040510     D parccm          s              8    INZ(*blanks)
003700040119     D parmbr          s             10    INZ(*blanks)
003800040119     D paropz          s              1    INZ(*blanks)
003900040119     D chkcall         s              1    INZ(*blanks)
004000080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004100080418     D wFlgCAS         s              1    INZ(*blanks)
004200121030     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300121030     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400150528     D Intestazioni    s               n   inz(*off)
004500120203     D wNomeFile       s             30    INZ(*blanks)
004600100326     D*------------------
004700100326     D* DS REPERIMENTO NUMERATORE
004800100326     D*------------------
004900100326     D trul33ds      e ds                  inz
005000100326     D*------------------
005100100326     D* DS ARCHITETTURA
005200100326     D*------------------
005300100326     D kpjba         e ds                  inz
005400100326
005500030715     D*------------------
005600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005700030715     D*------------------
005800030715     D SkSplitFLD      S             10    DIM(100)
005900030715     D SkSplitCSV      S            256    DIM(100)
006000030715     D CharCSV         S              1
006100030715     D CharTXT         S              1
006200030715     D CharNUM         S              1
006300100318     D CharSOS         S              1
006400030715     D posDa           S              3  0 INZ(*zeros)
006500030715     D posA            S              3  0 INZ(*zeros)
006600030715     D i               s              3  0 INZ(1)
006700030715     D wGiro           s              1  0 INZ(*zeros)
006800070823     D wVATNOT_A       s                   LIKE(VATNOT)
006900060331     D wVATNOT_B       s                   LIKE(VATNOT)
007000060331     D wVATNOT_E       s                   LIKE(VATNOT)
007100110927     D wVATNOT_I       s                   LIKE(VATNOT)
007200120202     D wVATNOT_J       s                   LIKE(VATNOT)
007300120202     D wVATNOT_IJ      s             70
007400141007     D wVATNOT_S       s                   LIKE(VATNOT)
007500150528     D w50             s             50
007600150528     D wX              s              3  0
007700150528     D num             c                   const('1234567890 ')
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100030822     D posDaDft        S              3  0 INZ(*zeros)
008200030822     D posADft         S              3  0 INZ(*zeros)
008300030822     D j               s              3  0 INZ(1)
008400030822     D wGiroDft        s              1  0 INZ(*zeros)
008500080418     D*------------------
008600080418     D* Costanti
008700080418     D*------------------
008800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009000100318
009100100318     D*------------------
009200100318     D* LINKING A DEFINIZIONI ESTERNE
009300100318     D*------------------
009400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009600100318
009700100909     D*------------------
009800100909     D* DS CALCOLO CHECK-DIGITS
009900100909     D*------------------
010000100909     D TRUL28DSX     e ds                  inz
010100010330
010200010330
010300010330
010400000913     C                   reset                   rrnum
010500990921     C                   reset                   esito
010600990921     C                   reset                   wrkesito
010700010601     C*
010800010601     C                   exsr      opeini
010900010605     C                   exsr      rwvab
011000010601     C*
011100010601     C                   seton                                        lr
011200010601
011300010601
011400010601
011500010601     C*--------------------------------------------------------
011600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011700010601     C*--------------------------------------------------------
011800010601     C     PREELA        BEGSR
011900010601     C*
012000010601     C* SE OCCORRE SPEDIRE IN FILIALE
012100010601     C                   if        invfil <> *zeros and
012200010601     C                             flgGiro = '0'
012300010601     C*
012400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012500010601     C                   eval      flgGiro = '1'
012600010601     C*
012700010601     C                   endif
012800010601     C*
012900010601     C                   ENDSR
013000010601     C***
013100010601
013200010601
013300010601
013400010601
013500010601     C*--------------------------------------------------------
013600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013700010601     C*--------------------------------------------------------
013800010601     C     ENDELA        BEGSR
013900040119     C*
014000040119     C* Effettuo la chiamata al CLLE preposto
014100110930     C                   call(e)   'TITVEVTC'
014200040510     C                   parm                    parccm
014300040119     C                   parm                    parmbr
014400040119     C                   parm      '2'           paropz
014500050201     C*
014600050201     C* Effettuo lancio TISI95 solo x chiusura
014700050201     C                   CLEAR                   TISI95DS
014800050201     C                   EVAL      I95TLA = 'C'
014900050201     C                   CALL      'TISI95R'
015000050201     C                   PARM                    TISI95DS
015100000616     C*
015200010601     C                   ENDSR
015300010601
015400010601
015500010330     C*--------------------------------------------------------
015600110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015700010330     C*--------------------------------------------------------
015800010605     C     RWVAB         BEGSR
015900010330     C*
016000010330     C                   if        not %open(tivin00r)
016100010330     C                   open      tivin00r
016200010330     C                   endif
016300110930     C                   if        not %open(edivabwr)
016400110930     C                   open      edivabwr
016500010330     C                   endif
016600040119     C*
016700110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016800040119     C                   exsr      prevat
016900040119     C*
017000040119     C                   if        chkcall = '0'
017100040119     C*
017200110930     C                   if        not %open(edivatwr)
017300110930     C                   open      edivatwr
017400040119     C                   endif
017500010330     C*
017600040119     C                   clear                   §CTROKVB          7 0
017700040119     C                   clear                   §CTROKVT          7 0
017800040119     C                   clear                   §CTRMO            7 0
017900040119     C                   clear                   §CTRNO            7 0
018000121030     C*
018100121030     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018200121030     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018300121030     C                   IF        Intestazioni = *off
018400121030     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018500121030     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018600121030     C                   exsr      inzvar
018700121030     C                   exsr      impvab
018800121030     C                   ENDIF
018900010330     C*
019000030822     C                   do        *HIVAL
019100010330     C*
019200030822     C                   read      tivin00r                               70
019300010618     C*
019400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019500010618     C                   if        vinflg = '1'
019600010618     C                   eval      flgOk = '1'
019700010618     C                   endif
019800040510     C*
019900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020000040510     C                   z-add     *zeros        wLenVINDTA        4 0
020100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020500040510     C                   z-add     1             wFlgVINDTA
020600040510     C                   leave
020700040510     C                   endif
020800040510     C                   enddo
020900010618     C*
021000040510     C                   if        vindta > *blanks AND
021100040510     C                             wFlgVINDTA = 1
021200010330     C                   add       1             rrnum
021300010330     C*
021400010601     C                   if        *in70 = *off and
021500010330     C                             (vinflg = *blanks
021600010330     C                              or vinflg = '0'
021700010330     C                              or vinflg = '2')
021800010330     C*
021900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022000010711     C                   if        vinflg = *blanks or vinflg = '0'
022100010711     C                   clear                   vinmsg
022200010711     C                   endif
022300010601     C*
022400070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022500150604     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022600150604     C* evitare che vengano elisi i blank
022700150604     C                   eval      vindta  = '*' + %trim(vindta)
022800070925     C*
022900080527     C                   exsr      inzvar
023000100318     C                   exsr      normCSV
023100010605     C                   exsr      impvab
023200010601     C*
023300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023400010605     C                   eval      depfil = VABLNP
023500010601     C                   exsr      repfil
023600010601     C                   if        depfil = invfil
023700021025     C                   if        vlrpoi = 999
023800030822     C                   move(P)   invfil        VABFGS
023900021025     C                   else
024000030822     C                   move(P)   vlrpoi        VABFGS
024100021025     C                   endif
024200010601     C*
024300010601     C                   exsr      PREELA
024400010601     C*
024500010604     C* Ebbene...
024600010604     C*
024700040119     C  N31              add       1             §CTROKVB
024800040119     C   32              add       1             §CTRMO
024900040119     C   31              add       1             §CTRNO
025000080418     C*
025100030822     C                   if        wGiro = 2
025200080513     C* Scrivo sempre il VAT
025300080513     C  N31              exsr      wrivat
025400110201     C                   exsr      chkvab
025500080513     C                   endif
025600010604     C*
025700010604     C                   if        *in31 = *off and
025800010604     C                             *in32 = *off
025900010604     C                   eval      vinflg = '1'
026000010604     C                   else
026100010604     C                   eval      vinflg = '2'
026200010604     C                   endif
026300010604     C                   endif
026400010601     C*
026500010604     C                   endif
026600010604     C*
026700010330     C                   else
026800010330     C                   eval      vinflg = '1'
026900010330     C                   endif
027000010601     C*
027100010601     C  N70              update    tivin000
027200010330     C*
027300030822     C  N70              enddo
027400040119     C*
027500080527     C* Verifico dati rimasti "in cannna"...
027600080527     C                   seton                                        65
027700110201     C                   exsr      chkvab
027800080527     C*
027900040119     C                   endif                                                  (endif - chkcall)
028000010601     C*
028100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028200010601     C                   if        cntNonEl = *zeros or
028300010601     C                             flgMulti = '0'
028400010330     C* Se non ci sono record con errori ...
028500010601     C                   if        §ctrno = 0 and
028600010604     C                             §ctrmo = 0 and
028700010601     C                             flgStato <> '2'
028800010330     C* ... restituisco esito OK.
028900010330     C                   eval      wrkesito = '0'
029000010330     C                   else
029100040119     C                   if        §ctrokvb > 0
029200010330     C                   eval      wrkesito = '1'
029300010330     C                   else
029400010615     C                   if        flgOk = '0'
029500010615     C                   eval      wrkesito = '2'
029600010615     C                   else
029700010615     C                   eval      wrkesito = '6'
029800010615     C                   endif
029900010330     C                   endif
030000010330     C                   endif
030100010601     C                   else
030200010601     C                   eval      wrkesito = '9'
030300010601     C                   endif
030400010330     C*
030500010330     C                   if        %open(tivin00r)
030600010330     C                   close     tivin00r
030700010330     C                   endif
030800110930     C                   if        %open(edivabwr)
030900110930     C                   close     edivabwr
031000010330     C                   endif
031100110930     C                   if        %open(edivatwr)
031200110930     C                   close     edivatwr
031300040512     C                   endif
031400010601     C*
031500010601     C                   if        vlrpoi <> 999
031600010601     C                   eval      invfil = vlrpoi
031700010601     C                   endif
031800010330     C*
031900040119     C                   if        §ctrokvb > 0
032000010601     C                             and invfil > *zeros
032100010330     C                   exsr      invio
032200010330     C                   endif
032300010601     C*
032400010618     C                   if        flgGiro = '1'
032500010601     C                   exsr      endela
032600010618     C                   endif
032700010330     C*
032800010330     C                   ENDSR
032900010330     C***
033000080527
033100080527
033200080527
033300080527     C*----------------------------------------------------*
033400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033500080527     C*----------------------------------------------------*
033600100413     C     CHKVAB        BEGSR
033700110201     C*
033800110201     C* Se richiesti ignorare colli / peso / volume
033900110315 xxx C***                if        *in35
034000120203 xxx C***                z-add     *zeros        VABNCL              F0461
034100120203 xxx C***                z-add     *zF046190081  VABPKB
034200110315 xxx C***                z-add     *zeros        VABVLB
034300110315 xxx C***                endif
034400080527     C*
034500080527     C* Scrivo il VAB solo a rottura d spedizione
034600080527     C                   if        depspe = *zeros
034700110930     C  N31              eval      edivabs_sav = edivabs
034800080527     C                   eval      depspe = vabNSP
034900080527     C                   else
035000080527     C                   if        depspe = vabNSP AND *in65 = *off
035100100413     C   51              add       VABNCL        sav_VABNCL
035200100413     C   51              add       VABPKB        sav_VABPKB
035300100413     C   51              add       VABVLB        sav_VABVLB
035400100720     C   52              z-add     VABNCL        sav_VABNCL
035500100720     C   52              z-add     VABPKB        sav_VABPKB
035600100720     C   52              z-add     VABVLB        sav_VABVLB
035700100720     C   53              add       VABNCL        sav_VABNCL
035800100720     C   53              z-add     VABPKB        sav_VABPKB
035900100720     C   53              z-add     VABVLB        sav_VABVLB
036000080527     C                   else
036100120203     C  N31              eval      edivabs_ok = edivabs_sav
036200080527     C  N31              exsr      wrivab
036300110930     C  N31              eval      edivabs_sav = edivabs
036400080527     C                   eval      depspe = vabNSP
036500080527     C                   endif
036600080527     C                   endif
036700080527     C*
036800080527     C                   ENDSR
036900040119
037000040119
037100040119
037200040119     C*----------------------------------------------------*
037300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037400040119     C*----------------------------------------------------*
037500040510     C     WRIVAB        BEGSR
037600080513     C*
037700120203     C* EDIVABWR ha il prefisso "ok_"
037800120203     C*
037900120203     C* Gestisco l'eventuale rottura x numero spedizione
038000080527     C                   if        VABNSP <> depspe or *in65 = *on
038100070301     C                   if        wVATNOT_E <> *blanks
038200120203     C                   eval      ok_VABCTM = '7Q'
038300070301     C                   endif
038400110930     C*
038500110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038600120203     C                   MOVE(P)   vlrMSG        wNomeFile
038700150529     C                   EVAL      ok_VABCMR = 'GAO'
038800120203     C                   EVAL      ok_VABDCM = DATCOR
038900120203     C                   EVAL      ok_VABDTS = DATCOR
039000120203     C                   EVAL      ok_VABHMS = ORACOR
039100120203     C                   EVAL      ok_VABCNT = 1
039200110930     C*
039300110930     C                   write     edivab00
039400080513     C                   endif
039500040119     C*
039600040119     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040200040119     C*----------------------------------------------------*
040300040510     C     WRIVAT        BEGSR
040400060512     C*
040500060512     C* Inizializzo i campi chiave
040600060512     C                   eval      VATFGS = VABFGS
040700060512     C                   eval      VATAAS = VABAAS
040800060512     C                   eval      VATCCM = VABCCM
040900070823     C                   eval      VATNRS = VABNRS
041000060512     C                   eval      VATNSP = VABNSP
041100060512     C                   eval      VATLNP = VABLNP
041200040119     C*
041300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041400040510     C                   if        VATTRC = *blanks
041500040510     C                   eval      VATTRC = 'E'
041600040510     C                   endif
041700110318     C                   if        VATNOT <> *blanks
041800150529     C                   eval      VATCMR = 'GAO'
041900110930     C                   eval      VATCNT = 1
042000110930     C                   write     EDIVAT00
042100040119     C                   add       1             §CTROKVT
042200110318     C                   endif
042300060331     C*
042400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042500060331     C                   if        wVATNOT_A <> *blanks
042600060331     C                   eval      VATNOT = wVATNOT_A
042700060331     C                   eval      VATTRC = 'A'
042800150529     C                   eval      VATCMR = 'GAO'
042900110930     C                   eval      VATCNT = 1
043000110930     C                   write     EDIVAT00
043100060331     C                   add       1             §CTROKVT
043200060331     C                   endif
043300060331     C*
043400060331     C                   if        wVATNOT_B <> *blanks
043500060331     C                   eval      VATNOT = wVATNOT_B
043600060331     C                   eval      VATTRC = 'B'
043700150529     C                   eval      VATCMR = 'GAO'
043800110930     C                   eval      VATCNT = 1
043900110930     C                   write     EDIVAT00
044000060331     C                   add       1             §CTROKVT
044100060331     C                   endif
044200141007     C*
044300141007     C                   if        wVATNOT_IJ <> *blanks
044400141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044500141007     C                   eval      VATTRC = 'I'
044600150529     C                   eval      VATCMR = 'GAO'
044700141007     C                   eval      VATCNT = 1
044800141007     C                   write     EDIVAT00
044900141007     C                   add       1             §CTROKVT
045000141007     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045100141007     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045200141007     C                   eval      VATTRC = 'J'
045300150529     c                   eval      VATCMR = 'GAO'
045400141007     C                   eval      VATCNT = 1
045500141007     C                   write     EDIVAT00
045600141007     C                   endif
045700141007     C                   endif
045800060331     C*
045900060331     C                   if        wVATNOT_E <> *blanks
046000150528     C* questo traduttore non gestisce i TRC=E
046100150528     C***                eval      VATTRC = 'E'
046200150528     C***                MOVE(P)   vlrMSG        wNomeFile
046300150528     C***                EVAL      VATCMR = wNomeFile
046400150528     C***                eval      VATCMR = %char(datcor)+'-'+%char(oracor)
046500150528     C***                eval      VATCNT = 1
046600150528     C***
046700150528     C**NON Sviluppare i "CHI SONO"
046800150528     C***
046900150528     C***                eval      VATNOT = '00' + %trim(wVATNOT_E)
047000150528     C***                write     EDIVAT00
047100121030     C                   endif
047200110927     C*
047300110927     C                   if        wVATNOT_I <> *blanks
047400110927     C                   eval      VATNOT = wVATNOT_I
047500110927     C                   eval      VATTRC = 'I'
047600150529     c                   eval      VATCMR = 'GAO'
047700110930     C                   eval      VATCNT = 1
047800110930     C                   write     EDIVAT00
047900110927     C                   add       1             §CTROKVT
048000110927     C                   endif
048100120202     C*
048200120202     C                   if        wVATNOT_J <> *blanks
048300120202     C                   eval      VATNOT = wVATNOT_J
048400120202     C                   eval      VATTRC = 'J'
048500150529     c                   eval      VATCMR = 'GAO'
048600120202     C                   eval      VATCNT = 1
048700120202     C                   write     EDIVAT00
048800120202     C                   add       1             §CTROKVT
048900120202     C                   endif
049000141007     C*
049100141007     C                   if        wVATNOT_S <> *blanks
049200141007     C                   eval      VATNOT = wVATNOT_S
049300141007     C* i 2 flag devono essere blank
049400141007     C                   EVAL      %subst(VATNOT:17:1)=*blank
049500141007     C                   EVAL      %subst(VATNOT:18:1)=*blank
049600141007     C                   eval      VATTRC = 'S'
049700150529     c                   eval      VATCMR = 'GAO'
049800141007     C                   eval      VATCNT = 1
049900141007     C                   write     EDIVAT00
050000141007     C                   add       1             §CTROKVT
050100141007     C                   endif
050200040119     C*
050300040119     C                   ENDSR
050400040119
050500040119
050600040119
050700040119     C*----------------------------------------------------*
050800110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
050900040119     C*----------------------------------------------------*
051000040119     C     PREVAT        BEGSR
051100040119     C*
051200110930     C* Compongo il nome del membro da dare al EDIVATWR
051300040119     C                   eval      parmbr = vlrhdl
051400040119     C                   movel     'M'           parmbr
051500040510     C                   eval      parccm = vlrksc
051600040119     C                   eval      paropz = '1'
051700040119     C* Effettuo la chiamata al CLLE preposto
051800110930     C                   call(e)   'TITVEVTC'
051900040119     C                   parm                    parccm
052000040119     C                   parm                    parmbr
052100040119     C                   parm                    paropz
052200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300040119     C                   if        %error
052400040119     C                   movel     '1'           chkcall
052500040119     C                   else
052600040119     C                   movel     '0'           chkcall
052700040119     C                   endif
052800040119     C*
052900040119     C                   ENDSR
053000010601
053100010601
053200010601
053300010330     C*----------------------------------------------------*
053400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053500010330     C*----------------------------------------------------*
053600040119     C     INZVAR        BEGSR
053700080527     C*
053800080527     C                   setoff                                       65
053900010330     C*
054000030822     C* Inizializzo variabili di wrk
054100130128     C                   Z-ADD     *zeros        Num3_0            3 0
054200020204     C                   Z-ADD     *zeros        Num5_0            5 0
054300101018     C                   MOVEL     *blanks       wFlgCAS
054400060331     C                   MOVEL     *blanks       wVATNOT_A
054500060331     C                   MOVEL     *blanks       wVATNOT_B
054600060331     C                   MOVEL     *blanks       wVATNOT_E
054700110927     C                   MOVEL     *blanks       wVATNOT_I
054800120202     C                   MOVEL     *blanks       wVATNOT_J
054900120202     C                   MOVEL     *blanks       wVATNOT_IJ
055000141007     C                   MOVEL     *blanks       wVATNOT_S
055100030822     C*
055200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055300030822     C                   CLEAR                   SkSplitCSV
055400030822     C                   EVAL      i = 1
055500030822     C                   EVAL      posDa = *zeros
055600030822     C                   EVAL      posA  = *zeros
055700080527     C*
055800110930     C                   CLEAR                   EDIVABS
055900110930     C                   CLEAR                   EDIVAT00
056000030822     C*
056100030822     C* Reimposto i valori di default
056200030822     C                   EXSR      DEFCAM
056300030822     C*
056400010330     C                   ENDSR
056500010330     C*----------------------------------------------------*
056600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056700010330     C*----------------------------------------------------*
056800010330     C     DEFCAM        BEGSR
056900030822     C*
057000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057100030715     C* e delimitatore testo.
057200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057500100318     C*
057600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057700100318     C                   EVAL      CharSOS = CharNUM
057800030822     C*
057900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058000100720     C                   SETOFF                                       505152
058100100720     C                   SETOFF                                       53
058200100413     C                   SELECT
058300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058400100720     C                   SETON                                        50
058500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058600100413     C                   SETON                                        51
058700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058800100720     C                   SETON                                        52
058900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059000100720     C                   SETON                                        53
059100100413     C                   ENDSL
059200030822     C*
059300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059400030822     C                   EVAL      posDaDft = 1
059500030822     C                   EVAL      posADft  = 0
059600030822     C                   EVAL      wGiroDft = 0
059700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059800030822     C                             posDaDft > 0
059900030822     C*
060000030822     C* Gestisco il 1° giro
060100030822     C                   IF        wGiroDft = 0
060200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060400030822     C* Incremento il contatore dei "giri"
060500030822     C                   EVAL      wGiroDft = 1
060600030822     C                   ELSE
060700030822     C                   EVAL      posDaDft = posADft
060800030822     C                   ENDIF
060900030822     C* Eseguo lo scan x trovare la fine del campo corrente
061000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061100030822     C*
061200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061400030822     C                   IF        posDaDft > 0 AND
061500030822     C                             posADft  > 0
061600030822     C* NCL
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'NCL'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABNCL
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CCM
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CCM'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABCCM
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* LNP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'LNP'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiInt=*on
065200030822     C                   Z-ADD     PiVal         VABLNP
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* NRS
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'NRS'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiInt=*on
066500030822     C                   Z-ADD     PiVal         VABNRS
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* CTR
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'CTR'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiInt=*on
067800030822     C                   Z-ADD     PiVal         VABCTR
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* PKB
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'PKB'
068600030822     C                   EVAL      PiStr=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   EXSR      CHKNUM
069000030822     C                   IF        PiNum=*on
069100030822     C                   Z-ADD     PiVal         VABPKB
069200030822     C                   ENDIF
069300030822     C                   ENDIF
069400030822     C* VLB
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VLB'
069900030822     C                   EVAL      PiStr=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   EXSR      CHKNUM
070300030822     C                   IF        PiNum=*on
070400030822     C                   Z-ADD     PiVal         VABVLB
070500030822     C                   ENDIF
070600030822     C                   ENDIF
070700030822     C* QFT
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'QFT'
071200030822     C                   EVAL      PiStr=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   EXSR      CHKNUM
071600030822     C                   IF        PiNum=*on
071700030822     C                   Z-ADD     PiVal         VABQFT
071800030822     C                   ENDIF
071900030822     C                   ENDIF
072000030822     C* CBO
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'CBO'
072500030822     C                   EVAL      VABCBO=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* TSP
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'TSP'
073400030822     C                   EVAL      VABTSP=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* VAS
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'VAS'
074300030822     C                   EVAL      VABVAS=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* VCA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'VCA'
075200030822     C                   EVAL      VABVCA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600070925     C* TIC
075700070925     C                   IF        %subst(
075800070925     C                             %subst(vlrppt:posDaDft+1:
075900070925     C                             posADft-posDaDft-1):1:3)
076000070925     C                             = 'TIC'
076100070925     C                   EVAL      VABTIC=%trim(%subst(
076200070925     C                             %subst(vlrppt:posDaDft+1:
076300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400070925     C                   ENDIF
076500030822     C* GCA
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'GCA'
077000030822     C                   EVAL      VABGCA=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* CTM
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'CTM'
077900030822     C                   EVAL      VABCTM=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* FFD
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'FFD'
078800030822     C                   EVAL      VABFFD=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* VAD
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'VAD'
079700030822     C                   EVAL      VABVAD=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* GMA
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'GMA'
080600030822     C                   EVAL      VABGMA=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* GGA
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'GGA'
081500030822     C                   EVAL      VABGGA=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* GVA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'GVA'
082400030822     C                   EVAL      VABGVA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* TC1
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'TC1'
083300030822     C                   EVAL      VABTC1=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* TC2
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'TC2'
084200030822     C                   EVAL      VABTC2=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600040714     C* VATTRC
084700040714     C                   IF        %subst(
084800040714     C                             %subst(vlrppt:posDaDft+1:
084900040714     C                             posADft-posDaDft-1):1:3)
085000040714     C                             = 'TRC'
085100040714     C                   EVAL      VATTRC=%trim(%subst(
085200040714     C                             %subst(vlrppt:posDaDft+1:
085300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400040714     C                   ENDIF
085500030822     C* ...
085600030822     C                   ENDIF
085700030822     C                   ENDDO
085800020204     C*
085900010330     C                   ENDSR
086000010607     C*----------------------------------------------------*
086100110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
086200010607     C*----------------------------------------------------*
086300010607     C     IMPVAB        BEGSR
086400010607     C*
086500010607     C                   SETOFF                                       3132
086600110315 xxx C***                SETON                                        35
086700030515     C*
086800010607     C                   MOVEL     datcor        VABAAS
086900010607     C                   MOVE      datcor        VABMGS
087000010607     C*
087100030715     C**********
087200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087300030715     C**********
087400030715     C                   IF        wGiro = *zeros
087500040113     C*
087600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087900080429     C                             %trim(intcol(5))<>*blanks
088000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088200080429     C                                      %trim(intcol(5))
088300070823     C     ';':CharCSV   XLATE     vindta        vindta
088400040510     C                   ENDIF
088500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088600031201     C     minu:maiu     XLATE     vindta        vindta
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300030715     C* Eseguo lo scan x trovare la fine del campo corrente
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* Gestisco l'ultimo campo
090600030715     C                   IF        posA = *zeros
090700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   LEAVE
091200030715     C                   ELSE
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitFLD)
092600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000030715     C                   EVAL      wGiro = 1
093100030715     C                   ELSE
093200030715     C                   EVAL      wGiro = 2
093300031204     C**********
093400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093500031204     C* d separatore campo
093600031204     C**********
093700031204     C                   EVAL      vindta = %trim(vindta)
093800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093900031204     C                   EVAL      vindta = %subst(vindta:2)
094000031204     C                   ENDDO
094100031204     C*
094200031204     C                   Z-ADD     *zeros        lunghInput        4 0
094300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094700110912     C                   ENDDO
094800030715     C**********
094900030715     C* Effettuo lo split del campo dati d input
095000030715     C**********
095100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095200030715     C                   DOW       posDa <= %len(%trim(vindta))
095300030715     C*
095400030715     C* Gestisco il 1° campo
095500030715     C                   IF        i = 1
095600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095700030715     C                   EVAL      posDa = 1
095800030715     C* Eseguo lo scan x trovare la fine del primo campo
095900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C* X i campi successivi al 1°
096400030715     C                   ELSE
096500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096600030715     C                   EVAL      posDa = posA + 1
096700031204     C* Verifico che nn vi sia il campo nn valorizzato
096800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096900031204     C* Se campo nn valorizzato skippo al prossimo
097000031204     C                   EVAL      SkSplitCSV(i) = *blanks
097100031204     C                   EVAL      posA  = posA + 1
097200031204     C                   ELSE
097300030715     C* Eseguo lo scan x trovare la fine del campo corrente
097400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097500030715     C* Gestisco l'ultimo campo
097600030715     C                   IF        posA = *zeros
097700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097800030715     C* A questo punto "estraggo" il campo corrente
097900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098000030715     C                                                    (posA-posDa))
098100030715     C                   LEAVE
098200030715     C                   ELSE
098300030715     C* A questo punto "estraggo" il campo corrente
098400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098500030715     C                                                    (posA-posDa))
098600030715     C                   ENDIF
098700031204     C                   ENDIF
098800030715     C                   ENDIF
098900030715     C* Incremento il contatore d campo
099000030715     C                   EVAL      i = i +1
099100030715     C                   ENDDO
099200030715     C*
099300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099400030715     C* delimitazione testo
099500030715     C                   EVAL      i = 1
099600030715     C                   DOW       i < %elem(SkSplitCSV)
099700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099900030715     C                   EVAL      i = i + 1
100000030715     C                   ENDDO
100100100720     C*
100200100720     C* Stacco un progressivo x numero spedizione
100300110930 xxx C                   EXSR      repNSP
100400100326     C*
100500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100600030715     C                   EVAL      i = 1
100700030715     C                   DOW       i < %elem(SkSplitFLD)
100800030715     C*
100900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101100030715     C                             SkSplitCSV(i) <> *blanks
101200030721     C*** ATB
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101400030721     C                   EVAL      VABATB = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030715     C*** RSD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102100030715     C* ==
102200030715     C                   ENDIF
102300030721     C*** RD2
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030715     C*** IND
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102900030715     C                   EVAL      VABIND = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** LOD
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** PRD
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030721     C*** NZD
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030715     C*** RMA
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030715     C*** NOT
104800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105000030715     C                   ENDIF
105100030715     C*** NT2
105200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105400030715     C                   ENDIF
105500030715     C*** VCA
105600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105800030715     C                   ENDIF
105900030715     C*** VAS
106000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106200030715     C                   ENDIF
106300030715     C*** FFD
106400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106600030715     C                   ENDIF
106700030721     C*** GC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TSP
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107800030721     C                   ENDIF
107900040510     C*** CBO
108000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108200040510     C                   ENDIF
108300030721     C*** NAS
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TIC
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GCA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** XCO
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** CTM
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** TCR
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** CTS
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** FTM
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** VAD
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** GMA
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** GGA
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** GVA
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*** TC1
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113400030721     C                   ENDIF
113500030721     C*** TC2
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113800030721     C                   ENDIF
113900030721     C*** SCL
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114200030721     C                   ENDIF
114300030721     C*** RMO
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114600030721     C                   ENDIF
114700030721     C*** NMO
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115000030721     C                   ENDIF
115100030721     C*
115200030721     C* Reperisco quindi i campi numerici...
115300040510     C*** NSP
115400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115500040510     C                   EVAL      PiStr=SkSplitCSV(i)
115600040510     C                   EXSR      CHKNUM
115700040510     C                   IF        PiInt=*on
115800040510     C                   Z-ADD     PiVal         VABNSP
115900040510     C                   ELSE
116000040510     C                   SETON                                        31
116100040510     C                   EVAL      VABNSP = *zeros
116200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040510     C                             + ' ' + 'VABNSP'
116400040510     C                   ENDIF
116500040510     C                   ENDIF
116600071106     C*** RMN
116700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116800071106     C                   EVAL      PiStr=SkSplitCSV(i)
116900071106     C                   EXSR      CHKNUM
117000071106     C                   IF        PiInt=*on
117100071106     C                   Z-ADD     PiVal         VABRMN
117200071106     C                   ELSE
117300071106     C                   SETON                                        32
117400071106     C                   EVAL      VABRMN = *zeros
117500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117600071106     C                             + ' ' + 'VABRMN'
117700071106     C                   ENDIF
117800071106     C                   ENDIF
117900030721     C*** CAD
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on
118400030721     C                   Z-ADD     PiVal         Num5_0
118500030721     C                   MOVEL(p)  Num5_0        VABCAD
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      VABCAD = *zeros
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABCAD'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** NCL
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on
119800030721     C                   Z-ADD     PiVal         VABNCL
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      VABNCL = *zeros
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300040122     C                             + ' ' + 'VABNCL'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** PKB
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiNum=*on
121100030721     C                   Z-ADD     PiVal         VABPKB
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABPKB = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABPKB'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** CAS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122100141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122200141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122300141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122400141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122500141007     C                             %trim(SkSplitCSV(i)) <> ',00'
122600141007     C                   EVAL      VABVCA = 'EUR'
122700101018     C                   EVAL      wFlgCAS = '1'
122800101018     C                   ENDIF
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200030721     C                   Z-ADD     PiVal         VABCAS
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      VABCAS = *zeros
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABCAS'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** CCM
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on      AND
124500030721     C                             PiVal<=9999999 AND
124600030721     C                             PiVal>*zeros
124700030721     C                   Z-ADD     PiVal         VABCCM
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      VABCCM = *zeros
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200040510     C                             + ' ' + 'VABCCM'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** LNP
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on    AND
126000030721     C                             PiVal<=999   AND
126100030721     C                             PiVal>*zeros
126200030721     C                   Z-ADD     PiVal         VABLNP
126300030721     C                   ELSE
126400040715     C                   SETON                                        32
126500030721     C                   EVAL      VABLNP = *zeros
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700040510     C                             + ' ' + 'VABLNP'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** NRS
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on AND
127500030721     C                             PiVal<=99
127600030721     C                   Z-ADD     PiVal         VABNRS
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABNRS'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** LNA
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on    AND
128800030721     C                             PiVal<=999   AND
128900030721     C                             PiVal>*zeros
129000030721     C                   Z-ADD     PiVal         VABLNA
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      VABLNA = *zeros
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABLNA'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** CTR
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiInt=*on  AND
130300030721     C                             PiVal<=999
130400030721     C                   Z-ADD     PiVal         VABCTR
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABCTR'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** IAS
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131300141007     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131400141007     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131500141007     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131600141007     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
131700141007     C                             %trim(SkSplitCSV(i)) <> ',00'
131800141007     C                   EVAL      VABVAS = 'EUR'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiNum=*on
132200030721     C                   Z-ADD     PiVal         VABIAS
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABIAS'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900141007     C                   ENDIF
133000030721     C*** VLB
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiNum=*on
133500030721     C                   Z-ADD     PiVal         VABVLB
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABVLB'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** QFT
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiNum=*on
134700030721     C                   Z-ADD     PiVal         VABQFT
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABQFT'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** NCD
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on      AND
135900030721     C                             PiVal<=9999999
136000030721     C                   Z-ADD     PiVal         VABNCD
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABNCD'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** NCA
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on      AND
137200030721     C                             PiVal<=9999999
137300030721     C                   Z-ADD     PiVal         VABNCA
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABNCA'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** ZNC
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on AND
138500030721     C                             PiVal<=99
138600030721     C                   Z-ADD     PiVal         VABZNC
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABZNC'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** DCR
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on       AND
139800030721     C                             PiVal<=99999999
139900030721     C                   Z-ADD     PiVal         VABDCR
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABDCR'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** HCR
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiInt=*on   AND
141100030721     C                             PiVal<=9999
141200030721     C                   Z-ADD     PiVal         VABHCR
141300030721     C                   ELSE
141400030721     C                   SETON                                        32
141500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141600030721     C                             + ' ' + 'VABHCR'
141700030721     C                   ENDIF
141800030721     C                   ENDIF
141900030721     C*** VMD
142000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142100030721     C                   EVAL      PiStr=SkSplitCSV(i)
142200030721     C                   EXSR      CHKNUM
142300030721     C                   IF        PiNum=*on
142400030721     C                   Z-ADD     PiVal         VABVMD
142500030721     C                   ELSE
142600030721     C                   SETON                                        32
142700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142800030721     C                             + ' ' + 'VABVMD'
142900030721     C                   ENDIF
143000030721     C                   ENDIF
143100030721     C*** ANT
143200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143300030721     C                   EVAL      PiStr=SkSplitCSV(i)
143400030721     C                   EXSR      CHKNUM
143500030721     C                   IF        PiInt=*on        AND
143600030721     C                             PiVal<=999999999
143700030721     C                   Z-ADD     PiVal         VABANT
143800030721     C                   ELSE
143900030721     C                   SETON                                        32
144000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144100030721     C                             + ' ' + 'VABANT'
144200030721     C                   ENDIF
144300030721     C                   ENDIF
144400030721     C*** CMO
144500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144600030721     C                   EVAL      PiStr=SkSplitCSV(i)
144700030721     C                   EXSR      CHKNUM
144800030721     C                   IF        PiInt=*on
144900030721     C                   Z-ADD     PiVal         Num5_0
145000030721     C                   MOVEL(p)  Num5_0        VABCMO
145100030721     C                   ELSE
145200030721     C                   SETON                                        32
145300030721     C                   EVAL      VABCMO = *zeros
145400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145500030721     C                             + ' ' + 'VABCMO'
145600030721     C                   ENDIF
145700030721     C                   ENDIF
145800040510     C*
145900110930     C* Reperisco i campi del EDIVAT
146000040510     C*** VATNOT
146100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146300040510     C                   ENDIF
146400040510     C*** VATTRC
146500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146700040510     C                   ENDIF
146800060331     C*** VATNOT_A
146900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147100060331     C                   ENDIF
147200060331     C*** VATNOT_B
147300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147500060331     C                   ENDIF
147600060331     C*** VATNOT_E
147700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
147800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147900060331     C                   ENDIF
148000110927     C*** VATNOT_I
148100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148300110927     C                   ENDIF
148400141007     C*** VATNOT_IJ
148500141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148600141007     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148700141007     C                   ENDIF
148800141007     C*** VATNOT_S
148900141007     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149000141007     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149100141007     C                   ENDIF
149200070823     C*
149300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149400150528
149500150528      *** W_VABNSP: è VABNSP/VABRMN/VABRMA, i numerici solo gli ultimi char a dx numerici
149600150528      *    mentre RMA è la composizione dei primi 6 char + dall'11° in poi
149700150528     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
149800150528     C                   EVAL      w50 = %trim(SkSplitCSV(i))
149900150528     C* trovo l'ultimo carattere non numerico e non blank
150000150528     C                   EVAL      wX = %checkR(num : W50) + 1
150100150528
150200150528     C                   EVAL      VABNSP = %dec(
150300150528     C                                   %subst(w50:wX:
150400150528     C                                    %scan(' ':w50)-wX)
150500150528     C                                       :7:0)
150600150528     C                   EVAL      VABRMN = VABNSP
150700150528     C                   EVAL      VABRMA = %subst(w50:1:6) +
150800150528     C                                      %subst(w50:11:9)
150900150528     C                   ENDIF
151000070925     C*
151100130403
151200130403     C*
151300071029     C***  <===  -----------------------------------------
151400040510     C*
151500030715     C                   ENDIF
151600030715     C                   EVAL      i = i + 1
151700030715     C                   ENDDO
151800070925     C*
151900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152000150528     C*
152100150528     C* Se la nazione che mi è stata passata è I o IT la pulisco, altrimenti non calcola la prov.
152200150528     C                   IF        VABNZD = 'I' or
152300150528     C                             VABNZD = 'IT'
152400150528     C                   EVAL      VABNZD = *blank
152500150528     C                   ENDIF
152600070925     C*
152700071029     C***  <===  -----------------------------------------
152800070925     C*
152900070925     C* Considerazioni finali su CBO/CAS
153000100318     C                   IF        wFlgCAS = '1'    OR
153100100318     C                             VABCAS  > *zeros
153200130403     C                   IF        VABVCA = *blanks
153300130403     C                   EVAL      VABVCA = 'EUR'
153400130403     C                   EVAL      VABTIC = *blanks
153500130403     C                   ENDIF
153600070925     C                   IF        VABCBO = '1'
153700070925     C                   EVAL      VABCBO = '4'
153800070925     C                   ENDIF
153900070925     C                   IF        VABCBO = '2'
154000070925     C                   EVAL      VABCBO = '6'
154100070925     C                   ENDIF
154200130403     C                   ELSE
154300130403     C                   EVAL      vabVCA = *blank
154400070925     C                   ENDIF
154500070925     C***  <===  ----------------------------
154600040510     C*
154700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154800040510     C                   IF        VABNSP = *zeros
154900040510     C                   SETON                                        31
155000040510     C                   ENDIF
155100040715     C*
155200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155300040715     C                   IF        VABLNP = *zeros
155400040715     C                   SETON                                        31
155500040715     C                   ENDIF
155600040510     C*
155700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155800040510     C                   IF        VABRMN = *zeros
155900040510     C                   Z-ADD     VABNSP        VABRMN
156000040510     C                   ENDIF
156100040510     C*
156200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156300040510     C                   IF        VABRMA = *blanks
156400040714     C                   MOVEL     VABRMN        VABRMA
156500040510     C                   ENDIF
156600040128     C*
156700040510     C* Se provincia nn valorizzata la reperisco
156800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156900110912     C                   IF        VABPRD =  *blanks AND
157000110912     C                             VABNZD =  *blanks AND
157100110912     C                             VABCAD <> *blanks AND
157200110912     C                             VABLOD <> *blanks
157300040128     C                   CLEAR                   TISI95DS
157400040128     C                   EVAL      I95TCN = '3'
157500040128     C                   Z-ADD     datcor        I95DAT
157600040128     C                   EVAL      I95NAR = VABNZD
157700040128     C                   EVAL      I95CAP = VABCAD
157800040128     C                   EVAL      I95LOC = VABLOD
157900040128     C                   CALL      'TISI95R'
158000040128     C                   PARM                    TISI95DS
158100040128     C                   EVAL      VABPRD = O95PRV
158200040510     C                   ENDIF
158300020204     C*
158400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158500020204     C                   EXSR      CHKIMPDIV
158600030715     C                   ENDIF
158700020204     C*
158800010607     C                   ENDSR
158900010607     C*----------------------------------------------------*
159000010601
159100020204
159200020204     C*----------------------------------------------------*
159300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159400020204     C*----------------------------------------------------*
159500020204     C     CHKIMPDIV     BEGSR
159600020204     C*
159700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159800020204     C                   Z-ADD     *zeros        wrkDec            9 9
159900020204     C*
160000020204     C* Come prima cosa effettuo considerazioni sulla divisa
160100020204     C                   IF        vabIAS > *zeros
160200020204     C                   IF        vabVAS <> 'EUR'
160300020204     C                   EVAL      vabVAS =  'ITL'
160400020204     C                   ENDIF
160500020204     C                   ENDIF
160600020204     C*
160700020204     C                   IF        vabCAS > *zeros
160800020204     C                   IF        vabVCA <> 'EUR'
160900020204     C                   EVAL      vabVCA =  'ITL'
161000020204     C                   ENDIF
161100020204     C                   ENDIF
161200020204     C*
161300020204     C                   IF        vabVMD > *zeros
161400020204     C                   IF        vabVAD <> 'EUR'
161500020204     C                   EVAL      vabVAD =  'ITL'
161600020204     C                   ENDIF
161700020204     C                   ENDIF
161800020204     C*
161900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162000020204     C                   Z-ADD     vabIAS        wrkDec
162100020204     C                   IF        wrkDec > *zeros
162200020204     C                   IF        vabVAS = 'ITL'
162300020204     C                   EVAL      vabIAS = *zeros
162400020204     C                   ENDIF
162500020204     C                   ENDIF
162600020204     C*
162700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162800020204     C                   Z-ADD     vabCAS        wrkDec
162900020204     C                   IF        wrkDec > *zeros
163000020204     C                   IF        vabVCA = 'ITL'
163100020204     C                   EVAL      vabCAS = *zeros
163200020204     C                   ENDIF
163300020204     C                   ENDIF
163400020204     C*
163500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163600020204     C                   Z-ADD     vabVMD        wrkDec
163700020204     C                   IF        wrkDec > *zeros
163800020204     C                   IF        vabVAD = 'ITL'
163900020204     C                   EVAL      vabVMD = *zeros
164000020204     C                   ENDIF
164100020204     C                   ENDIF
164200020204     C*
164300020204     C                   ENDSR
164400020204     C***
164500010330
164600100318
164700100318
164800100318     C*----------------------------------------------------*
164900100318     C*  CONTROLLO NUMERICITA' CAMPI
165000100318     C*----------------------------------------------------*
165100100318     C     CHKNUM        BEGSR
165200100318     C*
165300100318     C                   IF        PiDecChr = *blanks
165400100318     C                   EVAL      PiDecChr = CharNUM
165500100318     C                   ENDIF
165600100318     C*
165700100318     C                   callp     UBISNUM_Check(PiStr
165800100318     C                                          :PiDecChr
165900100318     C                                          :PiVal
166000100318     C                                          :PiNum
166100100318     C                                          :PiInt)
166200100318     C*
166300100318     C                   ENDSR
166400100318     C***
166500010601
166600010601
166700010601
166800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166900010601     C     repfil        BEGSR
167000010601     C*
167100010601     C                   if        invfil = *zeros and
167200010601     C                             depfil > *zeros and
167300010629     C                             (vinflg = *blanks or
167400010629     C                              vinflg = *zeros)
167500010601     C
167600010601     C                   eval      invfil = depfil
167700010601     C                   endif
167800010601     C*
167900010601     C                   if        depfil <> invfil and
168000010601     C                             invfil > *zeros
168100010601     C                   eval      flgMulti = '1'
168200010601     C                   if        vinflg = *blanks
168300010601     C                   add       1             cntNonEl
168400010601     C                   endif
168500010601     C                   endif
168600010601     C*
168700010601     C                   if        vinflg = '2'
168800010601     C                   eval      flgStato = '2'
168900010601     C                   endif
169000010601     C*
169100010601     C                   ENDSR
169200010601     C***
169300010601
169400010601
169500010601
169600010330
169700040119      /TITLE Invio dei dati al punto operativo.
169800040119     C     invio         BEGSR
169900040119     C*
170000110930     C* 1° invio EDIVAT
170100040119     C                   reset                   dscmz
170200040119     C                   move      vlrpoi        cmzdst
170300110930     C                   eval      cmzfld = 'EDIVATWR'
170400040119     C                   eval      cmzmbd = vlrhdl
170500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170600040119     C***                if        prmfir = *blanks
170700110930     C                   eval      cmzfla = 'EDIVAT0F'
170800110930     C                   eval      cmzmba = 'EDIVAT0F'
170900040119     C***                else
171000040119     C***                eval      cmzfla = prmfir
171100040119     C***                eval      cmzmba = prmfir
171200040119     C***                endif
171300040119     C                   eval      cmznrr = *zeros
171400040119     C                   move      §ctrokvt      cmznrr
171500040119     C                   eval      cmzlba = vlrfl1
171600040119     C                   call(e)   'TIS711C'
171700040119     C                   parm                    dscmz
171800040119     C                   parm      *blanks       esito
171900040119     C                   if        %error
172000040119     C                             or cmzerr = '1'
172100040119     C                             or esito  = '1'
172200040119     C                   eval      wrkesito = '3'
172300040119     C                   else
172400040119     C*
172500110930     C* 2° invio EDIVAB
172600040119     C                   reset                   dscmz
172700040119     C                   move      vlrpoi        cmzdst
172800040119     C                   eval      cmzfld = vlrfou
172900040119     C                   eval      cmzmbd = vlrhdl
173000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173100040119     C***                if        prmfir = *blanks
173200110930     C                   eval      cmzfla = 'EDIVAB0F'
173300110930     C                   eval      cmzmba = 'EDIVAB0F'
173400040119     C***                else
173500040119     C***                eval      cmzfla = prmfir
173600040119     C***                eval      cmzmba = prmfir
173700040119     C***                endif
173800040119     C                   eval      cmznrr = *zeros
173900040119     C                   move      §ctrokvb      cmznrr
174000040119     C                   eval      cmzlba = vlrfl1
174100040119     C                   call(e)   'TIS711C'
174200040119     C                   parm                    dscmz
174300040119     C                   parm      *blanks       esito
174400040119     C                   if        %error
174500040119     C                             or cmzerr = '1'
174600040119     C                             or esito  = '1'
174700040119     C                   eval      wrkesito = '3'
174800040119     C                   endif
174900040119     C                   endif
175000040119     C*
175100040119     C                   ENDSR
175200040119     C***
175300010601
175400010601
175500010601
175600010601
175700010601      /TITLE Invio dei dati al punto operativo.
175800010601     C     opeini        BEGSR
175900010601     C*
176000010601     C* Inizializzo flag e contatori operativi
176100010601     C                   movel     '0'           flgGiro           1
176200010601     C                   movel     '0'           flgMulti          1
176300010601     C                   movel     '1'           flgStato          1
176400010615     C                   movel     '0'           flgOk             1
176500010601     C                   z-add     *zeros        cntNonEl         10 0
176600010601     C                   z-add     *zeros        depfil            3 0
176700010601     C                   z-add     *zeros        invfil            3 0
176800010601     C*
176900010601     C                   ENDSR
177000010601     C***
177100070326
177200070326
177300070326
177400070326
177500070326     C     *pssr         BEGSR
177600070329     C*
177700070329     C                   if        %open(tivin00r)
177800070329     C                   close     tivin00r
177900070329     C                   endif
178000110930     C                   if        %open(edivabwr)
178100110930     C                   close     edivabwr
178200070329     C                   endif
178300110930     C                   if        %open(edivatwr)
178400110930     C                   close     edivatwr
178500070329     C                   endif
178600070326     C*
178700070326     C* Effettuo la chiamata al CLLE preposto
178800110930     C                   call(e)   'TITVEVTC'
178900070326     C                   parm                    parccm
179000070326     C                   parm                    parmbr
179100070326     C                   parm      '2'           paropz
179200070326     C*
179300070326     C                   eval      wrkesito = '2'
179400070404     C*
179500070404     C                   seton                                        LR
179600070326     C*
179700070326     C                   ENDSR     '*CANCL'
179800070326     C***
179900100318
180000100318
180100100318
180200100318     C     normCSV       BEGSR
180300100318     C*
180400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180500100318     C* separatori campi
180600100318     C                   if        CharTXT <> *blanks
180700100318     C                   z-add     *zeros        wLenVINDTA        4 0
180800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180900100318     C                   setoff                                       80
181000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181300100318     C                   if        not *in80
181400100318     C                   seton                                        80
181500100318     C                   else
181600100318     C                   setoff                                       80
181700100318     C                   endif
181800100318     C                   endif
181900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182000100318     C                             and *in80 = *on
182100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182200100318     C                   endif
182300100318     C                   enddo
182400100318     C                   endif
182500100318     C*
182600100318     C                   ENDSR
182700100318     C***
182800070326
182900100326
183000100326
183100100326     C*----------------------------------------------------*
183200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183300100326     C*----------------------------------------------------*
183400100326     C     REPNSP        BEGSR
183500100326     C*
183600100326     C* NSP => Stacco un numeratore da AZNUM
183700100326     C                   clear                   TRUL33DS
183800100326     C                   eval      I33OPE = *zeros
183900100326     C                   eval      I33CNU = 302
184000100326     C                   eval      I33NUM = 1
184100100326     C                   movel     TRUL33DS      KPJBU
184200100326     C                   call      'TRUL33R'
184300100326     C                   parm                    KPJBA
184400100326     C                   movel     KPJBU         TRUL33DS
184500100326     C                   if        O33ERR = *zeros
184600100326     C                   z-add     O33NRF        VABNSP
184700100326     C                   z-add     O33NRF        VATNSP
184800100326     C                   else
184900100326     C                   SETON                                        31
185000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185100100326     C                             + ' ' + 'VABNSP VATNSP'
185200100326     C                   endif
185300100326     C*
185400100326     C                   ENDSR
185500010330
185600100326
185700010330
185800000613     C     *inzsr        BEGSR
185900990910     C*
186000990910     C     *entry        plist
186100990920     C                   parm                    tivlrds
186200990921     C                   parm      wrkesito      esito
186300000724     C                   parm                    prmlit
186400000710     C                   parm                    prmfir
186500010330     C*
186600010330     C* CALCOLA LA DATA CORRENTE
186700110930     C                   time                    wn14             14 0
186800110930     C                   movel     wn14          oracor            6 0          *ORA
186900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187000100318     C                   eval      datcor = %dec(%date() : *ISO)
187100000613     C*
187200000613     C                   ENDSR
187300000613     C***
187400990908
187500120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187600150528X;X;X;X;X;X;X;X;X;X;X;VABRSD;X;X;X;X;X;VABLOD;X;VABIND;VABNZD;X;VABCAD;X;X;X;
187700150528X;X;W_VABNSP;X;X;
187800150528
187900150528
188000150528
