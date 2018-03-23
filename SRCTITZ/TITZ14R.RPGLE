000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209     F*
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
003400120203     D wNomeFile       s             30    INZ(*blanks)
003500100326     D*------------------
003600100326     D* DS REPERIMENTO NUMERATORE
003700100326     D*------------------
003800100326     D trul33ds      e ds                  inz
003900100326     D*------------------
004000100326     D* DS ARCHITETTURA
004100100326     D*------------------
004200100326     D kpjba         e ds                  inz
004300100326
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
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
006100120202     D wVATNOT_J       s                   LIKE(VATNOT)
006200120202     D wVATNOT_IJ      s             70
006300100831     D wVABRSD         s                   LIKE(VABRSD)
006400100831     D wVABIND         s                   LIKE(VABIND)
006500100831     D wVABCAD         s                   LIKE(VABCAD)
006600100831     D wVABLOD         s                   LIKE(VABLOD)
006700100831     D wVABPRD         s                   LIKE(VABPRD)
006800030822     D*------------------
006900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007000030822     D*------------------
007100030822     D posDaDft        S              3  0 INZ(*zeros)
007200030822     D posADft         S              3  0 INZ(*zeros)
007300030822     D j               s              3  0 INZ(1)
007400030822     D wGiroDft        s              1  0 INZ(*zeros)
007500080418     D*------------------
007600080418     D* Costanti
007700080418     D*------------------
007800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000100318
008100100318     D*------------------
008200100318     D* LINKING A DEFINIZIONI ESTERNE
008300100318     D*------------------
008400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008600100318
008700100909     D*------------------
008800100909     D* DS CALCOLO CHECK-DIGITS
008900100909     D*------------------
009000100909     D TRUL28DSX     e ds                  inz
009100010330
009200010330
009300010330
009400000913     C                   reset                   rrnum
009500990921     C                   reset                   esito
009600990921     C                   reset                   wrkesito
009700010601     C*
009800010601     C                   exsr      opeini
009900010605     C                   exsr      rwvab
010000010601     C*
010100010601     C                   seton                                        lr
010200010601
010300010601
010400010601
010500010601     C*--------------------------------------------------------
010600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010700010601     C*--------------------------------------------------------
010800010601     C     PREELA        BEGSR
010900010601     C*
011000010601     C* SE OCCORRE SPEDIRE IN FILIALE
011100010601     C                   if        invfil <> *zeros and
011200010601     C                             flgGiro = '0'
011300010601     C*
011400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011500010601     C                   eval      flgGiro = '1'
011600010601     C*
011700010601     C                   endif
011800010601     C*
011900010601     C                   ENDSR
012000010601     C***
012100010601
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012700010601     C*--------------------------------------------------------
012800010601     C     ENDELA        BEGSR
012900040119     C*
013000040119     C* Effettuo la chiamata al CLLE preposto
013100110930     C                   call(e)   'TITVEVTC'
013200040510     C                   parm                    parccm
013300040119     C                   parm                    parmbr
013400040119     C                   parm      '2'           paropz
013500050201     C*
013600050201     C* Effettuo lancio TISI95 solo x chiusura
013700050201     C                   CLEAR                   TISI95DS
013800050201     C                   EVAL      I95TLA = 'C'
013900050201     C                   CALL      'TISI95R'
014000050201     C                   PARM                    TISI95DS
014100000616     C*
014200010601     C                   ENDSR
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
014700010330     C*--------------------------------------------------------
014800010605     C     RWVAB         BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300110930     C                   if        not %open(edivabwr)
015400110930     C                   open      edivabwr
015500010330     C                   endif
015600040119     C*
015700110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015800040119     C                   exsr      prevat
015900040119     C*
016000040119     C                   if        chkcall = '0'
016100040119     C*
016200110930     C                   if        not %open(edivatwr)
016300110930     C                   open      edivatwr
016400040119     C                   endif
016500010330     C*
016600040119     C                   clear                   §CTROKVB          7 0
016700040119     C                   clear                   §CTROKVT          7 0
016800040119     C                   clear                   §CTRMO            7 0
016900040119     C                   clear                   §CTRNO            7 0
017000070823     C*
017100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017300110912     C***                exsr      inzvar
017400110912     C***                exsr      impvab
017500110131     C*
017600110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017700110912 xxx C***                read      tivin00r                               70
017800010330     C*
017900030822     C                   do        *HIVAL
018000010330     C*
018100030822     C                   read      tivin00r                               70
018200010618     C*
018300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018400010618     C                   if        vinflg = '1'
018500010618     C                   eval      flgOk = '1'
018600010618     C                   endif
018700040510     C*
018800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018900040510     C                   z-add     *zeros        wLenVINDTA        4 0
019000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019400040510     C                   z-add     1             wFlgVINDTA
019500040510     C                   leave
019600040510     C                   endif
019700040510     C                   enddo
019800010618     C*
019900040510     C                   if        vindta > *blanks AND
020000040510     C                             wFlgVINDTA = 1
020100010330     C                   add       1             rrnum
020200010330     C*
020300010601     C                   if        *in70 = *off and
020400010330     C                             (vinflg = *blanks
020500010330     C                              or vinflg = '0'
020600010330     C                              or vinflg = '2')
020700010330     C*
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010601     C*
021300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021400070925     C                   eval      vindta  = ' ' + %trim(vindta)
021500070925     C*
021600080527     C                   exsr      inzvar
021700100318     C                   exsr      normCSV
021800010605     C                   exsr      impvab
021900010601     C*
022000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022100010605     C                   eval      depfil = VABLNP
022200010601     C                   exsr      repfil
022300010601     C                   if        depfil = invfil
022400021025     C                   if        vlrpoi = 999
022500030822     C                   move(P)   invfil        VABFGS
022600021025     C                   else
022700030822     C                   move(P)   vlrpoi        VABFGS
022800021025     C                   endif
022900010601     C*
023000010601     C                   exsr      PREELA
023100010601     C*
023200010604     C* Ebbene...
023300010604     C*
023400040119     C  N31              add       1             §CTROKVB
023500040119     C   32              add       1             §CTRMO
023600040119     C   31              add       1             §CTRNO
023700080418     C*
023800030822     C                   if        wGiro = 2
023900080513     C* Scrivo sempre il VAT
024000080513     C  N31              exsr      wrivat
024100110201     C                   exsr      chkvab
024200080513     C                   endif
024300010604     C*
024400010604     C                   if        *in31 = *off and
024500010604     C                             *in32 = *off
024600010604     C                   eval      vinflg = '1'
024700010604     C                   else
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000030822     C  N70              enddo
026100040119     C*
026200080527     C* Verifico dati rimasti "in cannna"...
026300080527     C                   seton                                        65
026400110201     C                   exsr      chkvab
026500080527     C*
026600040119     C                   endif                                                  (endif - chkcall)
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800040119     C                   if        §ctrokvb > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500110930     C                   if        %open(edivabwr)
029600110930     C                   close     edivabwr
029700010330     C                   endif
029800110930     C                   if        %open(edivatwr)
029900110930     C                   close     edivatwr
030000040512     C                   endif
030100010601     C*
030200010601     C                   if        vlrpoi <> 999
030300010601     C                   eval      invfil = vlrpoi
030400010601     C                   endif
030500010330     C*
030600040119     C                   if        §ctrokvb > 0
030700010601     C                             and invfil > *zeros
030800010330     C                   exsr      invio
030900010330     C                   endif
031000010601     C*
031100010618     C                   if        flgGiro = '1'
031200010601     C                   exsr      endela
031300010618     C                   endif
031400010330     C*
031500010330     C                   ENDSR
031600010330     C***
031700080527
031800080527
031900080527
032000080527     C*----------------------------------------------------*
032100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032200080527     C*----------------------------------------------------*
032300100413     C     CHKVAB        BEGSR
032400110201     C*
032500110201     C* Se richiesti ignorare colli / peso / volume
032600110315 xxx C***                if        *in35
032700120203 xxx C***                z-add     *zeros        VABNCL              F0461
032800120203 xxx C***                z-add     *zF046190081  VABPKB
032900110315 xxx C***                z-add     *zeros        VABVLB
033000110315 xxx C***                endif
033100080527     C*
033200080527     C* Scrivo il VAB solo a rottura d spedizione
033300080527     C                   if        depspe = *zeros
033400110930     C  N31              eval      edivabs_sav = edivabs
033500080527     C                   eval      depspe = vabNSP
033600080527     C                   else
033700080527     C                   if        depspe = vabNSP AND *in65 = *off
033800100413     C   51              add       VABNCL        sav_VABNCL
033900100413     C   51              add       VABPKB        sav_VABPKB
034000100413     C   51              add       VABVLB        sav_VABVLB
034100100720     C   52              z-add     VABNCL        sav_VABNCL
034200100720     C   52              z-add     VABPKB        sav_VABPKB
034300100720     C   52              z-add     VABVLB        sav_VABVLB
034400100720     C   53              add       VABNCL        sav_VABNCL
034500100720     C   53              z-add     VABPKB        sav_VABPKB
034600100720     C   53              z-add     VABVLB        sav_VABVLB
034700080527     C                   else
034800120203     C  N31              eval      edivabs_ok = edivabs_sav
034900080527     C  N31              exsr      wrivab
035000110930     C  N31              eval      edivabs_sav = edivabs
035100080527     C                   eval      depspe = vabNSP
035200080527     C                   endif
035300080527     C                   endif
035400080527     C*
035500080527     C                   ENDSR
035600040119
035700040119
035800040119
035900040119     C*----------------------------------------------------*
036000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036100040119     C*----------------------------------------------------*
036200040510     C     WRIVAB        BEGSR
036300080513     C*
036400120203     C* EDIVABWR ha il prefisso "ok_"
036500120203     C*
036600120203     C* Gestisco l'eventuale rottura x numero spedizione
036700080527     C                   if        VABNSP <> depspe or *in65 = *on
036800070301     C                   if        wVATNOT_E <> *blanks
036900120203     C                   eval      ok_VABCTM = '7Q'
037000070301     C                   endif
037100110930     C*
037200110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037300120203     C                   MOVE(P)   vlrMSG        wNomeFile
037400120203     C                   EVAL      ok_VABCMR = wNomeFile
037500120203     C                   EVAL      ok_VABDCM = DATCOR
037600120203     C                   EVAL      ok_VABDTS = DATCOR
037700120203     C                   EVAL      ok_VABHMS = ORACOR
037800120203     C                   EVAL      ok_VABCNT = 1
037900110930     C*
038000110930     C                   write     edivab00
038100080513     C                   endif
038200040119     C*
038300040119     C                   ENDSR
038400040119
038500040119
038600040119
038700040119     C*----------------------------------------------------*
038800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038900040119     C*----------------------------------------------------*
039000040510     C     WRIVAT        BEGSR
039100060512     C*
039200060512     C* Inizializzo i campi chiave
039300060512     C                   eval      VATFGS = VABFGS
039400060512     C                   eval      VATAAS = VABAAS
039500060512     C                   eval      VATCCM = VABCCM
039600070823     C                   eval      VATNRS = VABNRS
039700060512     C                   eval      VATNSP = VABNSP
039800060512     C                   eval      VATLNP = VABLNP
039900040119     C*
040000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040100040510     C                   if        VATTRC = *blanks
040200040510     C                   eval      VATTRC = 'E'
040300040510     C                   endif
040400110318     C                   if        VATNOT <> *blanks
040500120203     C                   MOVE(P)   vlrMSG        wNomeFile
040600120203     C                   EVAL      VATCMR = wNomeFile
040700120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
040800110930     C                   eval      VATCNT = 1
040900110930     C                   write     EDIVAT00
041000040119     C                   add       1             §CTROKVT
041100110318     C                   endif
041200060331     C*
041300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041400060331     C                   if        wVATNOT_A <> *blanks
041500060331     C                   eval      VATNOT = wVATNOT_A
041600060331     C                   eval      VATTRC = 'A'
041700120203     C                   MOVE(P)   vlrMSG        wNomeFile
041800120203     C                   EVAL      VATCMR = wNomeFile
041900120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
042000110930     C                   eval      VATCNT = 1
042100110930     C                   write     EDIVAT00
042200060331     C                   add       1             §CTROKVT
042300060331     C                   endif
042400060331     C*
042500060331     C                   if        wVATNOT_B <> *blanks
042600060331     C                   eval      VATNOT = wVATNOT_B
042700060331     C                   eval      VATTRC = 'B'
042800120203     C                   MOVE(P)   vlrMSG        wNomeFile
042900120203     C                   EVAL      VATCMR = wNomeFile
043000120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
043100110930     C                   eval      VATCNT = 1
043200110930     C                   write     EDIVAT00
043300060331     C                   add       1             §CTROKVT
043400060331     C                   endif
043500060331     C*
043600060331     C                   if        wVATNOT_E <> *blanks
043700110201     C                   eval      VATNOT = wVATNOT_E
043800060331     C                   eval      VATTRC = 'E'
043900120203     C                   MOVE(P)   vlrMSG        wNomeFile
044000120203     C                   EVAL      VATCMR = wNomeFile
044100120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
044200110930     C                   eval      VATCNT = 1
044300110930     C                   write     EDIVAT00
044400110201     C                   add       1             §CTROKVT
044500110201     C                   endif
044600110927     C*
044700110927     C                   if        wVATNOT_I <> *blanks
044800110927     C                   eval      VATNOT = wVATNOT_I
044900110927     C                   eval      VATTRC = 'I'
045000120203     C                   MOVE(P)   vlrMSG        wNomeFile
045100120203     C                   EVAL      VATCMR = wNomeFile
045200120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
045300110930     C                   eval      VATCNT = 1
045400110930     C                   write     EDIVAT00
045500110927     C                   add       1             §CTROKVT
045600110927     C                   endif
045700120202     C*
045800120202     C                   if        wVATNOT_J <> *blanks
045900120202     C                   eval      VATNOT = wVATNOT_J
046000120202     C                   eval      VATTRC = 'J'
046100120203     C                   MOVE(P)   vlrMSG        wNomeFile
046200120203     C                   EVAL      VATCMR = wNomeFile
046300120203     C***                eval      VATCMR = %char(datcor)+%char(oracor)
046400120202     C                   eval      VATCNT = 1
046500120202     C                   write     EDIVAT00
046600120202     C                   add       1             §CTROKVT
046700120202     C                   endif
046800040119     C*
046900040119     C                   ENDSR
047000040119
047100040119
047200040119
047300040119     C*----------------------------------------------------*
047400110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047500040119     C*----------------------------------------------------*
047600040119     C     PREVAT        BEGSR
047700040119     C*
047800110930     C* Compongo il nome del membro da dare al EDIVATWR
047900040119     C                   eval      parmbr = vlrhdl
048000040119     C                   movel     'M'           parmbr
048100040510     C                   eval      parccm = vlrksc
048200040119     C                   eval      paropz = '1'
048300040119     C* Effettuo la chiamata al CLLE preposto
048400110930     C                   call(e)   'TITVEVTC'
048500040119     C                   parm                    parccm
048600040119     C                   parm                    parmbr
048700040119     C                   parm                    paropz
048800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048900040119     C                   if        %error
049000040119     C                   movel     '1'           chkcall
049100040119     C                   else
049200040119     C                   movel     '0'           chkcall
049300040119     C                   endif
049400040119     C*
049500040119     C                   ENDSR
049600010601
049700010601
049800010601
049900010330     C*----------------------------------------------------*
050000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050100010330     C*----------------------------------------------------*
050200040119     C     INZVAR        BEGSR
050300080527     C*
050400080527     C                   setoff                                       65
050500010330     C*
050600030822     C* Inizializzo variabili di wrk
050700020204     C                   Z-ADD     *zeros        Num5_0            5 0
050800101018     C                   MOVEL     *blanks       wFlgCAS
050900060331     C                   MOVEL     *blanks       wVATNOT_A
051000060331     C                   MOVEL     *blanks       wVATNOT_B
051100060331     C                   MOVEL     *blanks       wVATNOT_E
051200110927     C                   MOVEL     *blanks       wVATNOT_I
051300120202     C                   MOVEL     *blanks       wVATNOT_J
051400120202     C                   MOVEL     *blanks       wVATNOT_IJ
051500100831     C                   MOVEL     *blanks       wVABRSD
051600100831     C                   MOVEL     *blanks       wVABIND
051700100831     C                   MOVEL     *blanks       wVABCAD
051800100831     C                   MOVEL     *blanks       wVABLOD
051900100831     C                   MOVEL     *blanks       wVABPRD
052000030822     C*
052100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052200030822     C                   CLEAR                   SkSplitCSV
052300030822     C                   EVAL      i = 1
052400030822     C                   EVAL      posDa = *zeros
052500030822     C                   EVAL      posA  = *zeros
052600080527     C*
052700110930     C                   CLEAR                   EDIVABS
052800110930     C                   CLEAR                   EDIVAT00
052900030822     C*
053000030822     C* Reimposto i valori di default
053100030822     C                   EXSR      DEFCAM
053200030822     C*
053300010330     C                   ENDSR
053400010330     C*----------------------------------------------------*
053500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053600010330     C*----------------------------------------------------*
053700010330     C     DEFCAM        BEGSR
053800030822     C*
053900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054000030715     C* e delimitatore testo.
054100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054400100318     C*
054500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054600100318     C                   EVAL      CharSOS = CharNUM
054700030822     C*
054800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
054900100720     C                   SETOFF                                       505152
055000100720     C                   SETOFF                                       53
055100100413     C                   SELECT
055200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055300100720     C                   SETON                                        50
055400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055500100413     C                   SETON                                        51
055600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055700100720     C                   SETON                                        52
055800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
055900100720     C                   SETON                                        53
056000100413     C                   ENDSL
056100030822     C*
056200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056300030822     C                   EVAL      posDaDft = 1
056400030822     C                   EVAL      posADft  = 0
056500030822     C                   EVAL      wGiroDft = 0
056600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056700030822     C                             posDaDft > 0
056800030822     C*
056900030822     C* Gestisco il 1° giro
057000030822     C                   IF        wGiroDft = 0
057100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057300030822     C* Incremento il contatore dei "giri"
057400030822     C                   EVAL      wGiroDft = 1
057500030822     C                   ELSE
057600030822     C                   EVAL      posDaDft = posADft
057700030822     C                   ENDIF
057800030822     C* Eseguo lo scan x trovare la fine del campo corrente
057900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058000030822     C*
058100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058300030822     C                   IF        posDaDft > 0 AND
058400030822     C                             posADft  > 0
058500030822     C* NCL
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NCL'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNCL
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CCM
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CCM'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCCM
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* LNP
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'LNP'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABLNP
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* NRS
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'NRS'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABNRS
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* CTR
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CTR'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiInt=*on
064700030822     C                   Z-ADD     PiVal         VABCTR
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* PKB
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'PKB'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiNum=*on
066000030822     C                   Z-ADD     PiVal         VABPKB
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300030822     C* VLB
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'VLB'
066800030822     C                   EVAL      PiStr=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   EXSR      CHKNUM
067200030822     C                   IF        PiNum=*on
067300030822     C                   Z-ADD     PiVal         VABVLB
067400030822     C                   ENDIF
067500030822     C                   ENDIF
067600030822     C* QFT
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'QFT'
068100030822     C                   EVAL      PiStr=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   EXSR      CHKNUM
068500030822     C                   IF        PiNum=*on
068600030822     C                   Z-ADD     PiVal         VABQFT
068700030822     C                   ENDIF
068800030822     C                   ENDIF
068900030822     C* CBO
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'CBO'
069400030822     C                   EVAL      VABCBO=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* TSP
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'TSP'
070300030822     C                   EVAL      VABTSP=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VAS
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VAS'
071200030822     C                   EVAL      VABVAS=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* VCA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VCA'
072100030822     C                   EVAL      VABVCA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500070925     C* TIC
072600070925     C                   IF        %subst(
072700070925     C                             %subst(vlrppt:posDaDft+1:
072800070925     C                             posADft-posDaDft-1):1:3)
072900070925     C                             = 'TIC'
073000070925     C                   EVAL      VABTIC=%trim(%subst(
073100070925     C                             %subst(vlrppt:posDaDft+1:
073200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300070925     C                   ENDIF
073400030822     C* GCA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GCA'
073900030822     C                   EVAL      VABGCA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* CTM
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'CTM'
074800030822     C                   EVAL      VABCTM=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* FFD
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'FFD'
075700030822     C                   EVAL      VABFFD=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* VAD
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'VAD'
076600030822     C                   EVAL      VABVAD=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GMA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GMA'
077500030822     C                   EVAL      VABGMA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* GGA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GGA'
078400030822     C                   EVAL      VABGGA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* GVA
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'GVA'
079300030822     C                   EVAL      VABGVA=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* TC1
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'TC1'
080200030822     C                   EVAL      VABTC1=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* TC2
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'TC2'
081100030822     C                   EVAL      VABTC2=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500040714     C* VATTRC
081600040714     C                   IF        %subst(
081700040714     C                             %subst(vlrppt:posDaDft+1:
081800040714     C                             posADft-posDaDft-1):1:3)
081900040714     C                             = 'TRC'
082000040714     C                   EVAL      VATTRC=%trim(%subst(
082100040714     C                             %subst(vlrppt:posDaDft+1:
082200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300040714     C                   ENDIF
082400030822     C* ...
082500030822     C                   ENDIF
082600030822     C                   ENDDO
082700020204     C*
082800010330     C                   ENDSR
082900010607     C*----------------------------------------------------*
083000110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083100010607     C*----------------------------------------------------*
083200010607     C     IMPVAB        BEGSR
083300010607     C*
083400010607     C                   SETOFF                                       3132
083500110315 xxx C***                SETON                                        35
083600030515     C*
083700010607     C                   MOVEL     datcor        VABAAS
083800010607     C                   MOVE      datcor        VABMGS
083900010607     C*
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084200030715     C**********
084300030715     C                   IF        wGiro = *zeros
084400040113     C*
084500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
084800080429     C                             %trim(intcol(5))<>*blanks
084900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085100080429     C                                      %trim(intcol(5))
085200070823     C     ';':CharCSV   XLATE     vindta        vindta
085300040510     C                   ENDIF
085400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085500031201     C     minu:maiu     XLATE     vindta        vindta
085600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085700030715     C                   DOW       posDa <= %len(%trim(vindta))
085800030715     C*
085900030715     C* Gestisco il 1° campo
086000030715     C                   IF        i = 1
086100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086200030715     C                   EVAL      posDa = 1
086300030715     C* Eseguo lo scan x trovare la fine del primo campo
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C* X i campi successivi al 1°
086900030715     C                   ELSE
087000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087100030715     C                   EVAL      posDa = posA + 1
087200030715     C* Eseguo lo scan x trovare la fine del campo corrente
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* Gestisco l'ultimo campo
087500030715     C                   IF        posA = *zeros
087600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087700030715     C* A questo punto "estraggo" il campo corrente
087800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087900030715     C                                                    (posA-posDa))
088000030715     C                   LEAVE
088100030715     C                   ELSE
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   ENDIF
088600030715     C                   ENDIF
088700030715     C* Incremento il contatore d campo
088800030715     C                   EVAL      i = i +1
088900030715     C                   ENDDO
089000030715     C*
089100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089200030715     C* delimitazione testo
089300030715     C                   EVAL      i = 1
089400030715     C                   DOW       i < %elem(SkSplitFLD)
089500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089700030715     C                   EVAL      i = i + 1
089800030715     C                   ENDDO
089900030715     C                   EVAL      wGiro = 1
090000030715     C                   ELSE
090100030715     C                   EVAL      wGiro = 2
090200031204     C**********
090300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090400031204     C* d separatore campo
090500031204     C**********
090600031204     C                   EVAL      vindta = %trim(vindta)
090700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090800031204     C                   EVAL      vindta = %subst(vindta:2)
090900031204     C                   ENDDO
091000031204     C*
091100031204     C                   Z-ADD     *zeros        lunghInput        4 0
091200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091600110912     C                   ENDDO
091700030715     C**********
091800030715     C* Effettuo lo split del campo dati d input
091900030715     C**********
092000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092100030715     C                   DOW       posDa <= %len(%trim(vindta))
092200030715     C*
092300030715     C* Gestisco il 1° campo
092400030715     C                   IF        i = 1
092500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092600030715     C                   EVAL      posDa = 1
092700030715     C* Eseguo lo scan x trovare la fine del primo campo
092800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092900030715     C* A questo punto "estraggo" il campo corrente
093000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093100030715     C                                                    (posA-posDa))
093200030715     C* X i campi successivi al 1°
093300030715     C                   ELSE
093400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093500030715     C                   EVAL      posDa = posA + 1
093600031204     C* Verifico che nn vi sia il campo nn valorizzato
093700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093800031204     C* Se campo nn valorizzato skippo al prossimo
093900031204     C                   EVAL      SkSplitCSV(i) = *blanks
094000031204     C                   EVAL      posA  = posA + 1
094100031204     C                   ELSE
094200030715     C* Eseguo lo scan x trovare la fine del campo corrente
094300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094400030715     C* Gestisco l'ultimo campo
094500030715     C                   IF        posA = *zeros
094600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094700030715     C* A questo punto "estraggo" il campo corrente
094800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094900030715     C                                                    (posA-posDa))
095000030715     C                   LEAVE
095100030715     C                   ELSE
095200030715     C* A questo punto "estraggo" il campo corrente
095300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095400030715     C                                                    (posA-posDa))
095500030715     C                   ENDIF
095600031204     C                   ENDIF
095700030715     C                   ENDIF
095800030715     C* Incremento il contatore d campo
095900030715     C                   EVAL      i = i +1
096000030715     C                   ENDDO
096100030715     C*
096200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096300030715     C* delimitazione testo
096400030715     C                   EVAL      i = 1
096500030715     C                   DOW       i < %elem(SkSplitCSV)
096600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096800030715     C                   EVAL      i = i + 1
096900030715     C                   ENDDO
097000100720     C*
097100100720     C* Stacco un progressivo x numero spedizione
097200110930 xxx C                   EXSR      repNSP
097300100326     C*
097400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitFLD)
097700030715     C*
097800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098000030715     C                             SkSplitCSV(i) <> *blanks
098100030721     C*** ATB
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098300030721     C                   EVAL      VABATB = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030715     C*** RSD
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
098800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
098900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099000030715     C* ==
099100030715     C                   ENDIF
099200030721     C*** RD2
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** IND
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
099800030715     C                   EVAL      VABIND = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** LOD
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** PRD
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030721     C*** NZD
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030715     C*** RMA
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** NOT
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
101800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** NT2
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** VCA
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** VAS
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** FFD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030721     C*** GC1
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
103800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GC2
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** TSP
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104700030721     C                   ENDIF
104800040510     C*** CBO
104900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105100040510     C                   ENDIF
105200030721     C*** NAS
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TIC
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
105800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GCA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** XCO
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** CTM
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TCR
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
107800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** FTM
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** VAD
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GMA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GGA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GVA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
109800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TC1
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TC2
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** SCL
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** RMO
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** NMO
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
111800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*
112100030721     C* Reperisco quindi i campi numerici...
112200040510     C*** NSP
112300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112400040510     C                   EVAL      PiStr=SkSplitCSV(i)
112500040510     C                   EXSR      CHKNUM
112600040510     C                   IF        PiInt=*on
112700040510     C                   Z-ADD     PiVal         VABNSP
112800040510     C                   ELSE
112900040510     C                   SETON                                        31
113000040510     C                   EVAL      VABNSP = *zeros
113100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113200040510     C                             + ' ' + 'VABNSP'
113300040510     C                   ENDIF
113400040510     C                   ENDIF
113500071106     C*** RMN
113600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113700071106     C                   EVAL      PiStr=SkSplitCSV(i)
113800071106     C                   EXSR      CHKNUM
113900071106     C                   IF        PiInt=*on
114000071106     C                   Z-ADD     PiVal         VABRMN
114100071106     C                   ELSE
114200071106     C                   SETON                                        32
114300071106     C                   EVAL      VABRMN = *zeros
114400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114500071106     C                             + ' ' + 'VABRMN'
114600071106     C                   ENDIF
114700071106     C                   ENDIF
114800030721     C*** CAD
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on
115300030721     C                   Z-ADD     PiVal         Num5_0
115400030721     C                   MOVEL(p)  Num5_0        VABCAD
115500030721     C                   ELSE
115600030721     C                   SETON                                        32
115700030721     C                   EVAL      VABCAD = *zeros
115800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115900030721     C                             + ' ' + 'VABCAD'
116000030721     C                   ENDIF
116100030721     C                   ENDIF
116200030721     C*** NCL
116300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116400030721     C                   EVAL      PiStr=SkSplitCSV(i)
116500030721     C                   EXSR      CHKNUM
116600030721     C                   IF        PiInt=*on
116700030721     C                   Z-ADD     PiVal         VABNCL
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABNCL = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040122     C                             + ' ' + 'VABNCL'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** PKB
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiNum=*on
118000030721     C                   Z-ADD     PiVal         VABPKB
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      VABPKB = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500030721     C                             + ' ' + 'VABPKB'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** CAS
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119000110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119100110506     C                             %trim(SkSplitCSV(i)) <> *zeros
119200101018     C                   EVAL      wFlgCAS = '1'
119300101018     C                   ENDIF
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABCAS
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABCAS = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABCAS'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** CCM
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on      AND
121000030721     C                             PiVal<=9999999 AND
121100030721     C                             PiVal>*zeros
121200030721     C                   Z-ADD     PiVal         VABCCM
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABCCM = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700040510     C                             + ' ' + 'VABCCM'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** LNP
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on    AND
122500030721     C                             PiVal<=999   AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABLNP
122800030721     C                   ELSE
122900040715     C                   SETON                                        32
123000030721     C                   EVAL      VABLNP = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200040510     C                             + ' ' + 'VABLNP'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** NRS
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on AND
124000030721     C                             PiVal<=99
124100030721     C                   Z-ADD     PiVal         VABNRS
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABNRS'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** LNA
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on    AND
125300030721     C                             PiVal<=999   AND
125400030721     C                             PiVal>*zeros
125500030721     C                   Z-ADD     PiVal         VABLNA
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      VABLNA = *zeros
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABLNA'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** CTR
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on  AND
126800030721     C                             PiVal<=999
126900030721     C                   Z-ADD     PiVal         VABCTR
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABCTR'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** IAS
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABIAS
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABIAS'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** VLB
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiNum=*on
129300030721     C                   Z-ADD     PiVal         VABVLB
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABVLB'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** QFT
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiNum=*on
130500030721     C                   Z-ADD     PiVal         VABQFT
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABQFT'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** NCD
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on      AND
131700030721     C                             PiVal<=9999999
131800030721     C                   Z-ADD     PiVal         VABNCD
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABNCD'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** NCA
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on      AND
133000030721     C                             PiVal<=9999999
133100030721     C                   Z-ADD     PiVal         VABNCA
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABNCA'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** ZNC
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on AND
134300030721     C                             PiVal<=99
134400030721     C                   Z-ADD     PiVal         VABZNC
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABZNC'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** DCR
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on       AND
135600030721     C                             PiVal<=99999999
135700030721     C                   Z-ADD     PiVal         VABDCR
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABDCR'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** HCR
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on   AND
136900030721     C                             PiVal<=9999
137000030721     C                   Z-ADD     PiVal         VABHCR
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABHCR'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** VMD
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiNum=*on
138200030721     C                   Z-ADD     PiVal         VABVMD
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138600030721     C                             + ' ' + 'VABVMD'
138700030721     C                   ENDIF
138800030721     C                   ENDIF
138900030721     C*** ANT
139000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139100030721     C                   EVAL      PiStr=SkSplitCSV(i)
139200030721     C                   EXSR      CHKNUM
139300030721     C                   IF        PiInt=*on        AND
139400030721     C                             PiVal<=999999999
139500030721     C                   Z-ADD     PiVal         VABANT
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABANT'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200030721     C*** CMO
140300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140400030721     C                   EVAL      PiStr=SkSplitCSV(i)
140500030721     C                   EXSR      CHKNUM
140600030721     C                   IF        PiInt=*on
140700030721     C                   Z-ADD     PiVal         Num5_0
140800030721     C                   MOVEL(p)  Num5_0        VABCMO
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      VABCMO = *zeros
141200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141300030721     C                             + ' ' + 'VABCMO'
141400030721     C                   ENDIF
141500030721     C                   ENDIF
141600040510     C*
141700110930     C* Reperisco i campi del EDIVAT
141800040510     C*** VATNOT
141900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142100040510     C                   ENDIF
142200040510     C*** VATTRC
142300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142500040510     C                   ENDIF
142600060331     C*** VATNOT_A
142700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142900060331     C                   ENDIF
143000060331     C*** VATNOT_B
143100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143300060331     C                   ENDIF
143400060331     C*** VATNOT_E
143500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
143600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143700060331     C                   ENDIF
143800110927     C*** VATNOT_I
143900110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144000110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144100110927     C                   ENDIF
144200070823     C*
144300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
144400120202     C*
144500120202     C*** RSD / RD2
144600120202     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
144700120202     C                   MOVEL     *blanks       wRSD             70
144800120202     C                   EVAL      wRSD=SkSplitCSV(i)
144900120202     C                   EVAL      VABRSD=%subst(wRSD:1:35)
145000120202     C                   EVAL      VABRD2=%subst(wRSD:36:35)
145100120202     C                   ENDIF
145200110421     C*
145300120202     C*** RMN / NSP
145400110912     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
145500110912     C                   EVAL      PiStr=SkSplitCSV(i)
145600110912     C                   EXSR      CHKNUM
145700110912     C                   IF        PiInt=*on
145800110912     C                   Z-ADD     PiVal         VABRMN
145900120419     C***                Z-ADD     PiVal         VABNSP
146000110912     C                   ELSE
146100110912     C                   SETON                                        31
146200110912     C                   EVAL      VABRMN = 1
146300110912     C                   EVAL      vinmsg = %trimr(vinmsg)
146400110930     C                             + ' ' + 'VABRMN'
146500120419     C***                EVAL      VABNSP = *zeros
146600120419     C***                EVAL      vinmsg = %trimr(vinmsg)
146700120419     C***                          + ' ' + 'VABNSP'
146800110912     C                   ENDIF
146900110415     C                   ENDIF
147000120202     C*
147100120202     C*** VATNOT_I/J
147200120203     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
147300120202     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147400120202     C                   EVAL      wVATNOT_I=%subst(wVATNOT_IJ:1:35)
147500120202     C                   EVAL      wVATNOT_J=%subst(wVATNOT_IJ:36:35)
147600120202     C                   ENDIF
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900040510     C*
148000030715     C                   ENDIF
148100030715     C                   EVAL      i = i + 1
148200030715     C                   ENDDO
148300070925     C*
148400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148500100831     C*
148600100831     C                   IF        wVABRSD <> *blanks
148700100831     C                   EVAL      VABRSD = wVABRSD
148800100831     C                   ENDIF
148900100831     C*
149000100831     C                   IF        wVABIND <> *blanks
149100100831     C                   EVAL      VABIND = wVABIND
149200100831     C                   ENDIF
149300100831     C*
149400100831     C                   IF        wVABCAD <> *blanks
149500100831     C                   EVAL      VABCAD = wVABCAD
149600100831     C                   ENDIF
149700100831     C*
149800100831     C                   IF        wVABLOD <> *blanks
149900100831     C                   EVAL      VABLOD = wVABLOD
150000100831     C                   ENDIF
150100100831     C*
150200100831     C                   IF        wVABPRD <> *blanks
150300100831     C                   EVAL      VABPRD = wVABPRD
150400100831     C                   ENDIF
150500070925     C*
150600071029     C***  <===  -----------------------------------------
150700070925     C*
150800070925     C* Considerazioni finali su CBO/CAS
150900100318     C                   IF        wFlgCAS = '1'    OR
151000100318     C                             VABCAS  > *zeros
151100070925     C                   IF        VABCBO = '1'
151200070925     C                   EVAL      VABCBO = '4'
151300070925     C                   ENDIF
151400070925     C                   IF        VABCBO = '2'
151500070925     C                   EVAL      VABCBO = '6'
151600070925     C                   ENDIF
151700070925     C                   ENDIF
151800070925     C***  <===  ----------------------------
151900040510     C*
152000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
152100040510     C                   IF        VABNSP = *zeros
152200040510     C                   SETON                                        31
152300040510     C                   ENDIF
152400040715     C*
152500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152600040715     C                   IF        VABLNP = *zeros
152700040715     C                   SETON                                        31
152800040715     C                   ENDIF
152900040510     C*
153000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
153100040510     C                   IF        VABRMN = *zeros
153200040510     C                   Z-ADD     VABNSP        VABRMN
153300040510     C                   ENDIF
153400040510     C*
153500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153600040510     C                   IF        VABRMA = *blanks
153700040714     C                   MOVEL     VABRMN        VABRMA
153800040510     C                   ENDIF
153900040128     C*
154000040510     C* Se provincia nn valorizzata la reperisco
154100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
154200110912     C                   IF        VABPRD =  *blanks AND
154300110912     C                             VABNZD =  *blanks AND
154400110912     C                             VABCAD <> *blanks AND
154500110912     C                             VABLOD <> *blanks
154600040128     C                   CLEAR                   TISI95DS
154700040128     C                   EVAL      I95TCN = '3'
154800040128     C                   Z-ADD     datcor        I95DAT
154900040128     C                   EVAL      I95NAR = VABNZD
155000040128     C                   EVAL      I95CAP = VABCAD
155100040128     C                   EVAL      I95LOC = VABLOD
155200040128     C                   CALL      'TISI95R'
155300040128     C                   PARM                    TISI95DS
155400040128     C                   EVAL      VABPRD = O95PRV
155500040510     C                   ENDIF
155600020204     C*
155700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155800020204     C                   EXSR      CHKIMPDIV
155900030715     C                   ENDIF
156000020204     C*
156100010607     C                   ENDSR
156200010607     C*----------------------------------------------------*
156300010601
156400020204
156500020204     C*----------------------------------------------------*
156600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156700020204     C*----------------------------------------------------*
156800020204     C     CHKIMPDIV     BEGSR
156900020204     C*
157000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157100020204     C                   Z-ADD     *zeros        wrkDec            9 9
157200020204     C*
157300020204     C* Come prima cosa effettuo considerazioni sulla divisa
157400020204     C                   IF        vabIAS > *zeros
157500020204     C                   IF        vabVAS <> 'EUR'
157600020204     C                   EVAL      vabVAS =  'ITL'
157700020204     C                   ENDIF
157800020204     C                   ENDIF
157900020204     C*
158000020204     C                   IF        vabCAS > *zeros
158100020204     C                   IF        vabVCA <> 'EUR'
158200020204     C                   EVAL      vabVCA =  'ITL'
158300020204     C                   ENDIF
158400020204     C                   ENDIF
158500020204     C*
158600020204     C                   IF        vabVMD > *zeros
158700020204     C                   IF        vabVAD <> 'EUR'
158800020204     C                   EVAL      vabVAD =  'ITL'
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
159300020204     C                   Z-ADD     vabIAS        wrkDec
159400020204     C                   IF        wrkDec > *zeros
159500020204     C                   IF        vabVAS = 'ITL'
159600020204     C                   EVAL      vabIAS = *zeros
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
160100020204     C                   Z-ADD     vabCAS        wrkDec
160200020204     C                   IF        wrkDec > *zeros
160300020204     C                   IF        vabVCA = 'ITL'
160400020204     C                   EVAL      vabCAS = *zeros
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160900020204     C                   Z-ADD     vabVMD        wrkDec
161000020204     C                   IF        wrkDec > *zeros
161100020204     C                   IF        vabVAD = 'ITL'
161200020204     C                   EVAL      vabVMD = *zeros
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C                   ENDSR
161700020204     C***
161800010330
161900100318
162000100318
162100100318     C*----------------------------------------------------*
162200100318     C*  CONTROLLO NUMERICITA' CAMPI
162300100318     C*----------------------------------------------------*
162400100318     C     CHKNUM        BEGSR
162500100318     C*
162600100318     C                   IF        PiDecChr = *blanks
162700100318     C                   EVAL      PiDecChr = CharNUM
162800100318     C                   ENDIF
162900100318     C*
163000100318     C                   callp     UBISNUM_Check(PiStr
163100100318     C                                          :PiDecChr
163200100318     C                                          :PiVal
163300100318     C                                          :PiNum
163400100318     C                                          :PiInt)
163500100318     C*
163600100318     C                   ENDSR
163700100318     C***
163800010601
163900010601
164000010601
164100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
164200010601     C     repfil        BEGSR
164300010601     C*
164400010601     C                   if        invfil = *zeros and
164500010601     C                             depfil > *zeros and
164600010629     C                             (vinflg = *blanks or
164700010629     C                              vinflg = *zeros)
164800010601     C
164900010601     C                   eval      invfil = depfil
165000010601     C                   endif
165100010601     C*
165200010601     C                   if        depfil <> invfil and
165300010601     C                             invfil > *zeros
165400010601     C                   eval      flgMulti = '1'
165500010601     C                   if        vinflg = *blanks
165600010601     C                   add       1             cntNonEl
165700010601     C                   endif
165800010601     C                   endif
165900010601     C*
166000010601     C                   if        vinflg = '2'
166100010601     C                   eval      flgStato = '2'
166200010601     C                   endif
166300010601     C*
166400010601     C                   ENDSR
166500010601     C***
166600010601
166700010601
166800010601
166900010330
167000040119      /TITLE Invio dei dati al punto operativo.
167100040119     C     invio         BEGSR
167200040119     C*
167300110930     C* 1° invio EDIVAT
167400040119     C                   reset                   dscmz
167500040119     C                   move      vlrpoi        cmzdst
167600110930     C                   eval      cmzfld = 'EDIVATWR'
167700040119     C                   eval      cmzmbd = vlrhdl
167800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167900040119     C***                if        prmfir = *blanks
168000110930     C                   eval      cmzfla = 'EDIVAT0F'
168100110930     C                   eval      cmzmba = 'EDIVAT0F'
168200040119     C***                else
168300040119     C***                eval      cmzfla = prmfir
168400040119     C***                eval      cmzmba = prmfir
168500040119     C***                endif
168600040119     C                   eval      cmznrr = *zeros
168700040119     C                   move      §ctrokvt      cmznrr
168800040119     C                   eval      cmzlba = vlrfl1
168900040119     C                   call(e)   'TIS711C'
169000040119     C                   parm                    dscmz
169100040119     C                   parm      *blanks       esito
169200040119     C                   if        %error
169300040119     C                             or cmzerr = '1'
169400040119     C                             or esito  = '1'
169500040119     C                   eval      wrkesito = '3'
169600040119     C                   else
169700040119     C*
169800110930     C* 2° invio EDIVAB
169900040119     C                   reset                   dscmz
170000040119     C                   move      vlrpoi        cmzdst
170100040119     C                   eval      cmzfld = vlrfou
170200040119     C                   eval      cmzmbd = vlrhdl
170300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170400040119     C***                if        prmfir = *blanks
170500110930     C                   eval      cmzfla = 'EDIVAB0F'
170600110930     C                   eval      cmzmba = 'EDIVAB0F'
170700040119     C***                else
170800040119     C***                eval      cmzfla = prmfir
170900040119     C***                eval      cmzmba = prmfir
171000040119     C***                endif
171100040119     C                   eval      cmznrr = *zeros
171200040119     C                   move      §ctrokvb      cmznrr
171300040119     C                   eval      cmzlba = vlrfl1
171400040119     C                   call(e)   'TIS711C'
171500040119     C                   parm                    dscmz
171600040119     C                   parm      *blanks       esito
171700040119     C                   if        %error
171800040119     C                             or cmzerr = '1'
171900040119     C                             or esito  = '1'
172000040119     C                   eval      wrkesito = '3'
172100040119     C                   endif
172200040119     C                   endif
172300040119     C*
172400040119     C                   ENDSR
172500040119     C***
172600010601
172700010601
172800010601
172900010601
173000010601      /TITLE Invio dei dati al punto operativo.
173100010601     C     opeini        BEGSR
173200010601     C*
173300010601     C* Inizializzo flag e contatori operativi
173400010601     C                   movel     '0'           flgGiro           1
173500010601     C                   movel     '0'           flgMulti          1
173600010601     C                   movel     '1'           flgStato          1
173700010615     C                   movel     '0'           flgOk             1
173800010601     C                   z-add     *zeros        cntNonEl         10 0
173900010601     C                   z-add     *zeros        depfil            3 0
174000010601     C                   z-add     *zeros        invfil            3 0
174100010601     C*
174200010601     C                   ENDSR
174300010601     C***
174400070326
174500070326
174600070326
174700070326
174800070326     C     *pssr         BEGSR
174900070329     C*
175000070329     C                   if        %open(tivin00r)
175100070329     C                   close     tivin00r
175200070329     C                   endif
175300110930     C                   if        %open(edivabwr)
175400110930     C                   close     edivabwr
175500070329     C                   endif
175600110930     C                   if        %open(edivatwr)
175700110930     C                   close     edivatwr
175800070329     C                   endif
175900070326     C*
176000070326     C* Effettuo la chiamata al CLLE preposto
176100110930     C                   call(e)   'TITVEVTC'
176200070326     C                   parm                    parccm
176300070326     C                   parm                    parmbr
176400070326     C                   parm      '2'           paropz
176500070326     C*
176600070326     C                   eval      wrkesito = '2'
176700070404     C*
176800070404     C                   seton                                        LR
176900070326     C*
177000070326     C                   ENDSR     '*CANCL'
177100070326     C***
177200100318
177300100318
177400100318
177500100318     C     normCSV       BEGSR
177600100318     C*
177700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177800100318     C* separatori campi
177900100318     C                   if        CharTXT <> *blanks
178000100318     C                   z-add     *zeros        wLenVINDTA        4 0
178100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178200100318     C                   setoff                                       80
178300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178600100318     C                   if        not *in80
178700100318     C                   seton                                        80
178800100318     C                   else
178900100318     C                   setoff                                       80
179000100318     C                   endif
179100100318     C                   endif
179200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179300100318     C                             and *in80 = *on
179400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179500100318     C                   endif
179600100318     C                   enddo
179700100318     C                   endif
179800100318     C*
179900100318     C                   ENDSR
180000100318     C***
180100070326
180200100326
180300100326
180400100326     C*----------------------------------------------------*
180500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180600100326     C*----------------------------------------------------*
180700100326     C     REPNSP        BEGSR
180800100326     C*
180900100326     C* NSP => Stacco un numeratore da AZNUM
181000100326     C                   clear                   TRUL33DS
181100100326     C                   eval      I33OPE = *zeros
181200100326     C                   eval      I33CNU = 302
181300100326     C                   eval      I33NUM = 1
181400100326     C                   movel     TRUL33DS      KPJBU
181500100326     C                   call      'TRUL33R'
181600100326     C                   parm                    KPJBA
181700100326     C                   movel     KPJBU         TRUL33DS
181800100326     C                   if        O33ERR = *zeros
181900100326     C                   z-add     O33NRF        VABNSP
182000100326     C                   z-add     O33NRF        VATNSP
182100100326     C                   else
182200100326     C                   SETON                                        31
182300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182400100326     C                             + ' ' + 'VABNSP VATNSP'
182500100326     C                   endif
182600100326     C*
182700100326     C                   ENDSR
182800010330
182900100326
183000010330
183100000613     C     *inzsr        BEGSR
183200990910     C*
183300990910     C     *entry        plist
183400990920     C                   parm                    tivlrds
183500990921     C                   parm      wrkesito      esito
183600000724     C                   parm                    prmlit
183700000710     C                   parm                    prmfir
183800010330     C*
183900010330     C* CALCOLA LA DATA CORRENTE
184000110930     C                   time                    wn14             14 0
184100110930     C                   movel     wn14          oracor            6 0          *ORA
184200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
184300100318     C                   eval      datcor = %dec(%date() : *ISO)
184400000613     C*
184500000613     C                   ENDSR
184600000613     C***
184700990908
184800120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184900120203W_VABRSD;VABIND;VABCAD;VABLOD;W_VATNOT_I;VATNOT_B;VABRMA;VABNCL;VABPKB;W_VABRMN;
185000120202
185100120202
185200120202
185300120202
