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
003900130403     D Intestazioni    s               n   inz(*off)
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
006900130128     D w70_1           s             70
007000130128     D w70_2           s             70
007100130128     D w70_3           s             70
007200130128     D w70_4           s             70
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
037200120203     C* Gestisco l'eventuale rottura x numero spedizione
037300080527     C                   if        VABNSP <> depspe or *in65 = *on
037400070301     C                   if        wVATNOT_E <> *blanks
037500120203     C                   eval      ok_VABCTM = '7Q'
037600070301     C                   endif
037700110930     C*
037800110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037900120203     C                   MOVE(P)   vlrMSG        wNomeFile
038000130122     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
038100120203     C                   EVAL      ok_VABDCM = DATCOR
038200120203     C                   EVAL      ok_VABDTS = DATCOR
038300120203     C                   EVAL      ok_VABHMS = ORACOR
038400120203     C                   EVAL      ok_VABCNT = 1
038500110930     C*
038600110930     C                   write     edivab00
038700080513     C                   endif
038800040119     C*
038900040119     C                   ENDSR
039000040119
039100040119
039200040119
039300040119     C*----------------------------------------------------*
039400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039500040119     C*----------------------------------------------------*
039600040510     C     WRIVAT        BEGSR
039700060512     C*
039800060512     C* Inizializzo i campi chiave
039900060512     C                   eval      VATFGS = VABFGS
040000060512     C                   eval      VATAAS = VABAAS
040100060512     C                   eval      VATCCM = VABCCM
040200070823     C                   eval      VATNRS = VABNRS
040300060512     C                   eval      VATNSP = VABNSP
040400060512     C                   eval      VATLNP = VABLNP
040500040119     C*
040600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040700040510     C                   if        VATTRC = *blanks
040800040510     C                   eval      VATTRC = 'E'
040900040510     C                   endif
041000110318     C                   if        VATNOT <> *blanks
041100120203     C                   MOVE(P)   vlrMSG        wNomeFile
041200130122     C***                EVAL      VATCMR = wNomeFile
041300130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041400110930     C                   eval      VATCNT = 1
041500110930     C                   write     EDIVAT00
041600040119     C                   add       1             §CTROKVT
041700110318     C                   endif
041800060331     C*
041900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042000060331     C                   if        wVATNOT_A <> *blanks
042100060331     C                   eval      VATNOT = wVATNOT_A
042200060331     C                   eval      VATTRC = 'A'
042300120203     C                   MOVE(P)   vlrMSG        wNomeFile
042400130122     C***                EVAL      VATCMR = wNomeFile
042500130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
042600110930     C                   eval      VATCNT = 1
042700110930     C                   write     EDIVAT00
042800060331     C                   add       1             §CTROKVT
042900060331     C                   endif
043000060331     C*
043100060331     C                   if        wVATNOT_B <> *blanks
043200060331     C                   eval      VATNOT = wVATNOT_B
043300060331     C                   eval      VATTRC = 'B'
043400120203     C                   MOVE(P)   vlrMSG        wNomeFile
043500130122     C***                EVAL      VATCMR = wNomeFile
043600130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
043700110930     C                   eval      VATCNT = 1
043800110930     C                   write     EDIVAT00
043900060331     C                   add       1             §CTROKVT
044000060331     C                   endif
044100060331     C*
044200060331     C                   if        wVATNOT_E <> *blanks
044300110201     C                   eval      VATNOT = wVATNOT_E
044400060331     C                   eval      VATTRC = 'E'
044500121030     C                   MOVE(P)   vlrMSG        wNomeFile
044600130122     C***                EVAL      VATCMR = wNomeFile
044700130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044800121030     C                   eval      VATCNT = 1
044900121030     C*
045000121030     C* Sviluppati i "CHI SONO" in relazione al numero colli
045100121030     C*
045200121030     C     1             DO        VABNCL        wNumCollo         3 0
045300121030     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
045400121030     C*** significativi
045500121030     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
045600121030     C                                %editc(wNumCollo:'X')
045700121030     C                   write     EDIVAT00
045800121030     C                   add       1             §CTROKVT
045900121030     C                   ENDDO
046000121030     C*
046100121030     C                   endif
046200110927     C*
046300110927     C                   if        wVATNOT_I <> *blanks
046400110927     C                   eval      VATNOT = wVATNOT_I
046500110927     C                   eval      VATTRC = 'I'
046600120203     C                   MOVE(P)   vlrMSG        wNomeFile
046700130122     C***                EVAL      VATCMR = wNomeFile
046800130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046900110930     C                   eval      VATCNT = 1
047000110930     C                   write     EDIVAT00
047100110927     C                   add       1             §CTROKVT
047200110927     C                   endif
047300120202     C*
047400120202     C                   if        wVATNOT_J <> *blanks
047500120202     C                   eval      VATNOT = wVATNOT_J
047600120202     C                   eval      VATTRC = 'J'
047700120203     C                   MOVE(P)   vlrMSG        wNomeFile
047800130122     C***                EVAL      VATCMR = wNomeFile
047900130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
048000120202     C                   eval      VATCNT = 1
048100120202     C                   write     EDIVAT00
048200120202     C                   add       1             §CTROKVT
048300120202     C                   endif
048400040119     C*
048500040119     C                   ENDSR
048600040119
048700040119
048800040119
048900040119     C*----------------------------------------------------*
049000110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049100040119     C*----------------------------------------------------*
049200040119     C     PREVAT        BEGSR
049300040119     C*
049400110930     C* Compongo il nome del membro da dare al EDIVATWR
049500040119     C                   eval      parmbr = vlrhdl
049600040119     C                   movel     'M'           parmbr
049700040510     C                   eval      parccm = vlrksc
049800040119     C                   eval      paropz = '1'
049900040119     C* Effettuo la chiamata al CLLE preposto
050000110930     C                   call(e)   'TITVEVTC'
050100040119     C                   parm                    parccm
050200040119     C                   parm                    parmbr
050300040119     C                   parm                    paropz
050400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050500040119     C                   if        %error
050600040119     C                   movel     '1'           chkcall
050700040119     C                   else
050800040119     C                   movel     '0'           chkcall
050900040119     C                   endif
051000040119     C*
051100040119     C                   ENDSR
051200010601
051300010601
051400010601
051500010330     C*----------------------------------------------------*
051600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051700010330     C*----------------------------------------------------*
051800040119     C     INZVAR        BEGSR
051900080527     C*
052000080527     C                   setoff                                       65
052100010330     C*
052200030822     C* Inizializzo variabili di wrk
052300130128     C                   Z-ADD     *zeros        Num3_0            3 0
052400020204     C                   Z-ADD     *zeros        Num5_0            5 0
052500101018     C                   MOVEL     *blanks       wFlgCAS
052600060331     C                   MOVEL     *blanks       wVATNOT_A
052700060331     C                   MOVEL     *blanks       wVATNOT_B
052800060331     C                   MOVEL     *blanks       wVATNOT_E
052900110927     C                   MOVEL     *blanks       wVATNOT_I
053000120202     C                   MOVEL     *blanks       wVATNOT_J
053100120202     C                   MOVEL     *blanks       wVATNOT_IJ
053200130128     C                   MOVEL     *blanks       w70_1
053300130128     C                   MOVEL     *blanks       w70_2
053400130128     C                   MOVEL     *blanks       w70_3
053500130128     C                   MOVEL     *blanks       w70_4
053600030822     C*
053700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053800030822     C                   CLEAR                   SkSplitCSV
053900030822     C                   EVAL      i = 1
054000030822     C                   EVAL      posDa = *zeros
054100030822     C                   EVAL      posA  = *zeros
054200080527     C*
054300110930     C                   CLEAR                   EDIVABS
054400110930     C                   CLEAR                   EDIVAT00
054500030822     C*
054600030822     C* Reimposto i valori di default
054700030822     C                   EXSR      DEFCAM
054800030822     C*
054900010330     C                   ENDSR
055000010330     C*----------------------------------------------------*
055100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055200010330     C*----------------------------------------------------*
055300010330     C     DEFCAM        BEGSR
055400030822     C*
055500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055600030715     C* e delimitatore testo.
055700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056000100318     C*
056100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056200100318     C                   EVAL      CharSOS = CharNUM
056300030822     C*
056400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056500100720     C                   SETOFF                                       505152
056600100720     C                   SETOFF                                       53
056700100413     C                   SELECT
056800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056900100720     C                   SETON                                        50
057000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057100100413     C                   SETON                                        51
057200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057300100720     C                   SETON                                        52
057400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057500100720     C                   SETON                                        53
057600100413     C                   ENDSL
057700030822     C*
057800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057900030822     C                   EVAL      posDaDft = 1
058000030822     C                   EVAL      posADft  = 0
058100030822     C                   EVAL      wGiroDft = 0
058200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058300030822     C                             posDaDft > 0
058400030822     C*
058500030822     C* Gestisco il 1° giro
058600030822     C                   IF        wGiroDft = 0
058700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058900030822     C* Incremento il contatore dei "giri"
059000030822     C                   EVAL      wGiroDft = 1
059100030822     C                   ELSE
059200030822     C                   EVAL      posDaDft = posADft
059300030822     C                   ENDIF
059400030822     C* Eseguo lo scan x trovare la fine del campo corrente
059500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059600030822     C*
059700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059900030822     C                   IF        posDaDft > 0 AND
060000030822     C                             posADft  > 0
060100030822     C* NCL
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'NCL'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100030822     C                   Z-ADD     PiVal         VABNCL
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* CCM
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'CCM'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABCCM
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* LNP
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'LNP'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABLNP
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* NRS
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'NRS'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABNRS
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* CTR
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CTR'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABCTR
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* PKB
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'PKB'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiNum=*on
067600030822     C                   Z-ADD     PiVal         VABPKB
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* VLB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'VLB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABVLB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* QFT
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'QFT'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200030822     C                   Z-ADD     PiVal         VABQFT
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* CBO
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CBO'
071000030822     C                   EVAL      VABCBO=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* TSP
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'TSP'
071900030822     C                   EVAL      VABTSP=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VAS
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VAS'
072800030822     C                   EVAL      VABVAS=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* VCA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'VCA'
073700030822     C                   EVAL      VABVCA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100070925     C* TIC
074200070925     C                   IF        %subst(
074300070925     C                             %subst(vlrppt:posDaDft+1:
074400070925     C                             posADft-posDaDft-1):1:3)
074500070925     C                             = 'TIC'
074600070925     C                   EVAL      VABTIC=%trim(%subst(
074700070925     C                             %subst(vlrppt:posDaDft+1:
074800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900070925     C                   ENDIF
075000030822     C* GCA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GCA'
075500030822     C                   EVAL      VABGCA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* CTM
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'CTM'
076400030822     C                   EVAL      VABCTM=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* FFD
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'FFD'
077300030822     C                   EVAL      VABFFD=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* VAD
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'VAD'
078200030822     C                   EVAL      VABVAD=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* GMA
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'GMA'
079100030822     C                   EVAL      VABGMA=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* GGA
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'GGA'
080000030822     C                   EVAL      VABGGA=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400030822     C* GVA
080500030822     C                   IF        %subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):1:3)
080800030822     C                             = 'GVA'
080900030822     C                   EVAL      VABGVA=%trim(%subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200030822     C                   ENDIF
081300030822     C* TC1
081400030822     C                   IF        %subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):1:3)
081700030822     C                             = 'TC1'
081800030822     C                   EVAL      VABTC1=%trim(%subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100030822     C                   ENDIF
082200030822     C* TC2
082300030822     C                   IF        %subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):1:3)
082600030822     C                             = 'TC2'
082700030822     C                   EVAL      VABTC2=%trim(%subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083000030822     C                   ENDIF
083100040714     C* VATTRC
083200040714     C                   IF        %subst(
083300040714     C                             %subst(vlrppt:posDaDft+1:
083400040714     C                             posADft-posDaDft-1):1:3)
083500040714     C                             = 'TRC'
083600040714     C                   EVAL      VATTRC=%trim(%subst(
083700040714     C                             %subst(vlrppt:posDaDft+1:
083800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083900040714     C                   ENDIF
084000030822     C* ...
084100030822     C                   ENDIF
084200030822     C                   ENDDO
084300020204     C*
084400010330     C                   ENDSR
084500010607     C*----------------------------------------------------*
084600110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
084700010607     C*----------------------------------------------------*
084800010607     C     IMPVAB        BEGSR
084900010607     C*
085000010607     C                   SETOFF                                       3132
085100110315 xxx C***                SETON                                        35
085200030515     C*
085300010607     C                   MOVEL     datcor        VABAAS
085400010607     C                   MOVE      datcor        VABMGS
085500010607     C*
085600030715     C**********
085700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085800030715     C**********
085900030715     C                   IF        wGiro = *zeros
086000040113     C*
086100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086400080429     C                             %trim(intcol(5))<>*blanks
086500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086700080429     C                                      %trim(intcol(5))
086800070823     C     ';':CharCSV   XLATE     vindta        vindta
086900040510     C                   ENDIF
087000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087100031201     C     minu:maiu     XLATE     vindta        vindta
087200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087300030715     C                   DOW       posDa <= %len(%trim(vindta))
087400030715     C*
087500030715     C* Gestisco il 1° campo
087600030715     C                   IF        i = 1
087700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087800030715     C                   EVAL      posDa = 1
087900030715     C* Eseguo lo scan x trovare la fine del primo campo
088000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C* X i campi successivi al 1°
088500030715     C                   ELSE
088600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088700030715     C                   EVAL      posDa = posA + 1
088800030715     C* Eseguo lo scan x trovare la fine del campo corrente
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* Gestisco l'ultimo campo
089100030715     C                   IF        posA = *zeros
089200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   LEAVE
089700030715     C                   ELSE
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C                   ENDIF
090200030715     C                   ENDIF
090300030715     C* Incremento il contatore d campo
090400030715     C                   EVAL      i = i +1
090500030715     C                   ENDDO
090600030715     C*
090700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090800030715     C* delimitazione testo
090900030715     C                   EVAL      i = 1
091000030715     C                   DOW       i < %elem(SkSplitFLD)
091100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091300030715     C                   EVAL      i = i + 1
091400030715     C                   ENDDO
091500030715     C                   EVAL      wGiro = 1
091600030715     C                   ELSE
091700030715     C                   EVAL      wGiro = 2
091800031204     C**********
091900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092000031204     C* d separatore campo
092100031204     C**********
092200031204     C                   EVAL      vindta = %trim(vindta)
092300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092400031204     C                   EVAL      vindta = %subst(vindta:2)
092500031204     C                   ENDDO
092600031204     C*
092700031204     C                   Z-ADD     *zeros        lunghInput        4 0
092800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093200110912     C                   ENDDO
093300030715     C**********
093400030715     C* Effettuo lo split del campo dati d input
093500030715     C**********
093600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093700030715     C                   DOW       posDa <= %len(%trim(vindta))
093800030715     C*
093900030715     C* Gestisco il 1° campo
094000030715     C                   IF        i = 1
094100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094200030715     C                   EVAL      posDa = 1
094300030715     C* Eseguo lo scan x trovare la fine del primo campo
094400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094500030715     C* A questo punto "estraggo" il campo corrente
094600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094700030715     C                                                    (posA-posDa))
094800030715     C* X i campi successivi al 1°
094900030715     C                   ELSE
095000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095100030715     C                   EVAL      posDa = posA + 1
095200031204     C* Verifico che nn vi sia il campo nn valorizzato
095300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095400031204     C* Se campo nn valorizzato skippo al prossimo
095500031204     C                   EVAL      SkSplitCSV(i) = *blanks
095600031204     C                   EVAL      posA  = posA + 1
095700031204     C                   ELSE
095800030715     C* Eseguo lo scan x trovare la fine del campo corrente
095900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096000030715     C* Gestisco l'ultimo campo
096100030715     C                   IF        posA = *zeros
096200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   LEAVE
096700030715     C                   ELSE
096800030715     C* A questo punto "estraggo" il campo corrente
096900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097000030715     C                                                    (posA-posDa))
097100030715     C                   ENDIF
097200031204     C                   ENDIF
097300030715     C                   ENDIF
097400030715     C* Incremento il contatore d campo
097500030715     C                   EVAL      i = i +1
097600030715     C                   ENDDO
097700030715     C*
097800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097900030715     C* delimitazione testo
098000030715     C                   EVAL      i = 1
098100030715     C                   DOW       i < %elem(SkSplitCSV)
098200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098400030715     C                   EVAL      i = i + 1
098500030715     C                   ENDDO
098600100720     C*
098700100720     C* Stacco un progressivo x numero spedizione
098800110930 xxx C                   EXSR      repNSP
098900100326     C*
099000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099100030715     C                   EVAL      i = 1
099200030715     C                   DOW       i < %elem(SkSplitFLD)
099300030715     C*
099400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099600030715     C                             SkSplitCSV(i) <> *blanks
099700030721     C*** ATB
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099900030721     C                   EVAL      VABATB = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030715     C*** RSD
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100600030715     C* ==
100700030715     C                   ENDIF
100800030721     C*** RD2
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030715     C*** IND
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101400030715     C                   EVAL      VABIND = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** LOD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** PRD
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030721     C*** NZD
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030715     C*** RMA
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** NOT
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** NT2
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** VCA
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** VAS
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** FFD
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030721     C*** GC1
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GC2
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TSP
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106300030721     C                   ENDIF
106400040510     C*** CBO
106500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106700040510     C                   ENDIF
106800030721     C*** NAS
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TIC
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GCA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** XCO
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** CTM
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TCR
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** CTS
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** FTM
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** VAD
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** GMA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** GGA
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** GVA
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** TC1
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** TC2
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** SCL
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** RMO
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** NMO
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*
113700030721     C* Reperisco quindi i campi numerici...
113800040510     C*** NSP
113900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114000040510     C                   EVAL      PiStr=SkSplitCSV(i)
114100040510     C                   EXSR      CHKNUM
114200040510     C                   IF        PiInt=*on
114300040510     C                   Z-ADD     PiVal         VABNSP
114400040510     C                   ELSE
114500040510     C                   SETON                                        31
114600040510     C                   EVAL      VABNSP = *zeros
114700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040510     C                             + ' ' + 'VABNSP'
114900040510     C                   ENDIF
115000040510     C                   ENDIF
115100071106     C*** RMN
115200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115300071106     C                   EVAL      PiStr=SkSplitCSV(i)
115400071106     C                   EXSR      CHKNUM
115500071106     C                   IF        PiInt=*on
115600071106     C                   Z-ADD     PiVal         VABRMN
115700071106     C                   ELSE
115800071106     C                   SETON                                        32
115900071106     C                   EVAL      VABRMN = *zeros
116000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116100071106     C                             + ' ' + 'VABRMN'
116200071106     C                   ENDIF
116300071106     C                   ENDIF
116400030721     C*** CAD
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         Num5_0
117000030721     C                   MOVEL(p)  Num5_0        VABCAD
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      VABCAD = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500030721     C                             + ' ' + 'VABCAD'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** NCL
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiInt=*on
118300030721     C                   Z-ADD     PiVal         VABNCL
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABNCL = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800040122     C                             + ' ' + 'VABNCL'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** PKB
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiNum=*on
119600030721     C                   Z-ADD     PiVal         VABPKB
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABPKB = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABPKB'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CAS
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120700130125     C                             %trim(SkSplitCSV(i)) <> '0,00'  AND
120800130125     C                             %trim(SkSplitCSV(i)) <> *zeros
120900101018     C                   EVAL      wFlgCAS = '1'
121000101018     C                   ENDIF
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiNum=*on
121400030721     C                   Z-ADD     PiVal         VABCAS
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      VABCAS = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABCAS'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** CCM
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on      AND
122700030721     C                             PiVal<=9999999 AND
122800030721     C                             PiVal>*zeros
122900030721     C                   Z-ADD     PiVal         VABCCM
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      VABCCM = *zeros
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400040510     C                             + ' ' + 'VABCCM'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** LNP
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on    AND
124200030721     C                             PiVal<=999   AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABLNP
124500030721     C                   ELSE
124600040715     C                   SETON                                        32
124700030721     C                   EVAL      VABLNP = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040510     C                             + ' ' + 'VABLNP'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** NRS
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on AND
125700030721     C                             PiVal<=99
125800030721     C                   Z-ADD     PiVal         VABNRS
125900030721     C                   ELSE
126000030721     C                   SETON                                        32
126100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126200030721     C                             + ' ' + 'VABNRS'
126300030721     C                   ENDIF
126400030721     C                   ENDIF
126500030721     C*** LNA
126600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126700030721     C                   EVAL      PiStr=SkSplitCSV(i)
126800030721     C                   EXSR      CHKNUM
126900030721     C                   IF        PiInt=*on    AND
127000030721     C                             PiVal<=999   AND
127100030721     C                             PiVal>*zeros
127200030721     C                   Z-ADD     PiVal         VABLNA
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      VABLNA = *zeros
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABLNA'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** CTR
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on  AND
128500030721     C                             PiVal<=999
128600030721     C                   Z-ADD     PiVal         VABCTR
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABCTR'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** IAS
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiNum=*on
129800030721     C                   Z-ADD     PiVal         VABIAS
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABIAS'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** VLB
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiNum=*on
131000030721     C                   Z-ADD     PiVal         VABVLB
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABVLB'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** QFT
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiNum=*on
132200030721     C                   Z-ADD     PiVal         VABQFT
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABQFT'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** NCD
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on      AND
133400030721     C                             PiVal<=9999999
133500030721     C                   Z-ADD     PiVal         VABNCD
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABNCD'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** NCA
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on      AND
134700030721     C                             PiVal<=9999999
134800030721     C                   Z-ADD     PiVal         VABNCA
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABNCA'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** ZNC
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on AND
136000030721     C                             PiVal<=99
136100030721     C                   Z-ADD     PiVal         VABZNC
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABZNC'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** DCR
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on       AND
137300030721     C                             PiVal<=99999999
137400030721     C                   Z-ADD     PiVal         VABDCR
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABDCR'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** HCR
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on   AND
138600030721     C                             PiVal<=9999
138700030721     C                   Z-ADD     PiVal         VABHCR
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABHCR'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** VMD
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiNum=*on
139900030721     C                   Z-ADD     PiVal         VABVMD
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABVMD'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** ANT
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiInt=*on        AND
141100030721     C                             PiVal<=999999999
141200030721     C                   Z-ADD     PiVal         VABANT
141300030721     C                   ELSE
141400030721     C                   SETON                                        32
141500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141600030721     C                             + ' ' + 'VABANT'
141700030721     C                   ENDIF
141800030721     C                   ENDIF
141900030721     C*** CMO
142000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142100030721     C                   EVAL      PiStr=SkSplitCSV(i)
142200030721     C                   EXSR      CHKNUM
142300030721     C                   IF        PiInt=*on
142400030721     C                   Z-ADD     PiVal         Num5_0
142500030721     C                   MOVEL(p)  Num5_0        VABCMO
142600030721     C                   ELSE
142700030721     C                   SETON                                        32
142800030721     C                   EVAL      VABCMO = *zeros
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000030721     C                             + ' ' + 'VABCMO'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300040510     C*
143400110930     C* Reperisco i campi del EDIVAT
143500040510     C*** VATNOT
143600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143800040510     C                   ENDIF
143900040510     C*** VATTRC
144000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144200040510     C                   ENDIF
144300060331     C*** VATNOT_A
144400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144600060331     C                   ENDIF
144700060331     C*** VATNOT_B
144800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145000060331     C                   ENDIF
145100060331     C*** VATNOT_E
145200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145400060331     C                   ENDIF
145500110927     C*** VATNOT_I
145600110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145700110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145800110927     C                   ENDIF
145900070823     C*
146000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146100070925     C*
146200130403
146300130403     C*
146400071029     C***  <===  -----------------------------------------
146500040510     C*
146600030715     C                   ENDIF
146700030715     C                   EVAL      i = i + 1
146800030715     C                   ENDDO
146900070925     C*
147000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147100121030     C*
147200130403     C                   Z-ADD     VABRMN        VABNSP
147300070925     C*
147400071029     C***  <===  -----------------------------------------
147500070925     C*
147600070925     C* Considerazioni finali su CBO/CAS
147700100318     C                   IF        wFlgCAS = '1'    OR
147800100318     C                             VABCAS  > *zeros
147900130403     C                   IF        VABVCA = *blanks
148000130403     C                   EVAL      VABVCA = 'EUR'
148100130403     C                   EVAL      VABTIC = *blanks
148200130403     C                   ENDIF
148300070925     C                   IF        VABCBO = '1'
148400070925     C                   EVAL      VABCBO = '4'
148500070925     C                   ENDIF
148600070925     C                   IF        VABCBO = '2'
148700070925     C                   EVAL      VABCBO = '6'
148800070925     C                   ENDIF
148900130403     C                   ELSE
149000130403     C                   EVAL      vabVCA = *blank
149100070925     C                   ENDIF
149200070925     C***  <===  ----------------------------
149300040510     C*
149400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149500040510     C                   IF        VABNSP = *zeros
149600040510     C                   SETON                                        31
149700040510     C                   ENDIF
149800040715     C*
149900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150000040715     C                   IF        VABLNP = *zeros
150100040715     C                   SETON                                        31
150200040715     C                   ENDIF
150300040510     C*
150400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150500040510     C                   IF        VABRMN = *zeros
150600040510     C                   Z-ADD     VABNSP        VABRMN
150700040510     C                   ENDIF
150800040510     C*
150900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151000040510     C                   IF        VABRMA = *blanks
151100040714     C                   MOVEL     VABRMN        VABRMA
151200040510     C                   ENDIF
151300040128     C*
151400040510     C* Se provincia nn valorizzata la reperisco
151500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151600110912     C                   IF        VABPRD =  *blanks AND
151700110912     C                             VABNZD =  *blanks AND
151800110912     C                             VABCAD <> *blanks AND
151900110912     C                             VABLOD <> *blanks
152000040128     C                   CLEAR                   TISI95DS
152100040128     C                   EVAL      I95TCN = '3'
152200040128     C                   Z-ADD     datcor        I95DAT
152300040128     C                   EVAL      I95NAR = VABNZD
152400040128     C                   EVAL      I95CAP = VABCAD
152500040128     C                   EVAL      I95LOC = VABLOD
152600040128     C                   CALL      'TISI95R'
152700040128     C                   PARM                    TISI95DS
152800040128     C                   EVAL      VABPRD = O95PRV
152900040510     C                   ENDIF
153000020204     C*
153100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153200020204     C                   EXSR      CHKIMPDIV
153300030715     C                   ENDIF
153400020204     C*
153500010607     C                   ENDSR
153600010607     C*----------------------------------------------------*
153700010601
153800020204
153900020204     C*----------------------------------------------------*
154000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154100020204     C*----------------------------------------------------*
154200020204     C     CHKIMPDIV     BEGSR
154300020204     C*
154400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154500020204     C                   Z-ADD     *zeros        wrkDec            9 9
154600020204     C*
154700020204     C* Come prima cosa effettuo considerazioni sulla divisa
154800020204     C                   IF        vabIAS > *zeros
154900020204     C                   IF        vabVAS <> 'EUR'
155000020204     C                   EVAL      vabVAS =  'ITL'
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C                   IF        vabCAS > *zeros
155500020204     C                   IF        vabVCA <> 'EUR'
155600020204     C                   EVAL      vabVCA =  'ITL'
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C                   IF        vabVMD > *zeros
156100020204     C                   IF        vabVAD <> 'EUR'
156200020204     C                   EVAL      vabVAD =  'ITL'
156300020204     C                   ENDIF
156400020204     C                   ENDIF
156500020204     C*
156600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156700020204     C                   Z-ADD     vabIAS        wrkDec
156800020204     C                   IF        wrkDec > *zeros
156900020204     C                   IF        vabVAS = 'ITL'
157000020204     C                   EVAL      vabIAS = *zeros
157100020204     C                   ENDIF
157200020204     C                   ENDIF
157300020204     C*
157400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157500020204     C                   Z-ADD     vabCAS        wrkDec
157600020204     C                   IF        wrkDec > *zeros
157700020204     C                   IF        vabVCA = 'ITL'
157800020204     C                   EVAL      vabCAS = *zeros
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158300020204     C                   Z-ADD     vabVMD        wrkDec
158400020204     C                   IF        wrkDec > *zeros
158500020204     C                   IF        vabVAD = 'ITL'
158600020204     C                   EVAL      vabVMD = *zeros
158700020204     C                   ENDIF
158800020204     C                   ENDIF
158900020204     C*
159000020204     C                   ENDSR
159100020204     C***
159200010330
159300100318
159400100318
159500100318     C*----------------------------------------------------*
159600100318     C*  CONTROLLO NUMERICITA' CAMPI
159700100318     C*----------------------------------------------------*
159800100318     C     CHKNUM        BEGSR
159900100318     C*
160000100318     C                   IF        PiDecChr = *blanks
160100100318     C                   EVAL      PiDecChr = CharNUM
160200100318     C                   ENDIF
160300100318     C*
160400100318     C                   callp     UBISNUM_Check(PiStr
160500100318     C                                          :PiDecChr
160600100318     C                                          :PiVal
160700100318     C                                          :PiNum
160800100318     C                                          :PiInt)
160900100318     C*
161000100318     C                   ENDSR
161100100318     C***
161200010601
161300010601
161400010601
161500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161600010601     C     repfil        BEGSR
161700010601     C*
161800010601     C                   if        invfil = *zeros and
161900010601     C                             depfil > *zeros and
162000010629     C                             (vinflg = *blanks or
162100010629     C                              vinflg = *zeros)
162200010601     C
162300010601     C                   eval      invfil = depfil
162400010601     C                   endif
162500010601     C*
162600010601     C                   if        depfil <> invfil and
162700010601     C                             invfil > *zeros
162800010601     C                   eval      flgMulti = '1'
162900010601     C                   if        vinflg = *blanks
163000010601     C                   add       1             cntNonEl
163100010601     C                   endif
163200010601     C                   endif
163300010601     C*
163400010601     C                   if        vinflg = '2'
163500010601     C                   eval      flgStato = '2'
163600010601     C                   endif
163700010601     C*
163800010601     C                   ENDSR
163900010601     C***
164000010601
164100010601
164200010601
164300010330
164400040119      /TITLE Invio dei dati al punto operativo.
164500040119     C     invio         BEGSR
164600040119     C*
164700110930     C* 1° invio EDIVAT
164800040119     C                   reset                   dscmz
164900040119     C                   move      vlrpoi        cmzdst
165000110930     C                   eval      cmzfld = 'EDIVATWR'
165100040119     C                   eval      cmzmbd = vlrhdl
165200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165300040119     C***                if        prmfir = *blanks
165400110930     C                   eval      cmzfla = 'EDIVAT0F'
165500110930     C                   eval      cmzmba = 'EDIVAT0F'
165600040119     C***                else
165700040119     C***                eval      cmzfla = prmfir
165800040119     C***                eval      cmzmba = prmfir
165900040119     C***                endif
166000040119     C                   eval      cmznrr = *zeros
166100040119     C                   move      §ctrokvt      cmznrr
166200040119     C                   eval      cmzlba = vlrfl1
166300040119     C                   call(e)   'TIS711C'
166400040119     C                   parm                    dscmz
166500040119     C                   parm      *blanks       esito
166600040119     C                   if        %error
166700040119     C                             or cmzerr = '1'
166800040119     C                             or esito  = '1'
166900040119     C                   eval      wrkesito = '3'
167000040119     C                   else
167100040119     C*
167200110930     C* 2° invio EDIVAB
167300040119     C                   reset                   dscmz
167400040119     C                   move      vlrpoi        cmzdst
167500040119     C                   eval      cmzfld = vlrfou
167600040119     C                   eval      cmzmbd = vlrhdl
167700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167800040119     C***                if        prmfir = *blanks
167900110930     C                   eval      cmzfla = 'EDIVAB0F'
168000110930     C                   eval      cmzmba = 'EDIVAB0F'
168100040119     C***                else
168200040119     C***                eval      cmzfla = prmfir
168300040119     C***                eval      cmzmba = prmfir
168400040119     C***                endif
168500040119     C                   eval      cmznrr = *zeros
168600040119     C                   move      §ctrokvb      cmznrr
168700040119     C                   eval      cmzlba = vlrfl1
168800040119     C                   call(e)   'TIS711C'
168900040119     C                   parm                    dscmz
169000040119     C                   parm      *blanks       esito
169100040119     C                   if        %error
169200040119     C                             or cmzerr = '1'
169300040119     C                             or esito  = '1'
169400040119     C                   eval      wrkesito = '3'
169500040119     C                   endif
169600040119     C                   endif
169700040119     C*
169800040119     C                   ENDSR
169900040119     C***
170000010601
170100010601
170200010601
170300010601
170400010601      /TITLE Invio dei dati al punto operativo.
170500010601     C     opeini        BEGSR
170600010601     C*
170700010601     C* Inizializzo flag e contatori operativi
170800010601     C                   movel     '0'           flgGiro           1
170900010601     C                   movel     '0'           flgMulti          1
171000010601     C                   movel     '1'           flgStato          1
171100010615     C                   movel     '0'           flgOk             1
171200010601     C                   z-add     *zeros        cntNonEl         10 0
171300010601     C                   z-add     *zeros        depfil            3 0
171400010601     C                   z-add     *zeros        invfil            3 0
171500010601     C*
171600010601     C                   ENDSR
171700010601     C***
171800070326
171900070326
172000070326
172100070326
172200070326     C     *pssr         BEGSR
172300070329     C*
172400070329     C                   if        %open(tivin00r)
172500070329     C                   close     tivin00r
172600070329     C                   endif
172700110930     C                   if        %open(edivabwr)
172800110930     C                   close     edivabwr
172900070329     C                   endif
173000110930     C                   if        %open(edivatwr)
173100110930     C                   close     edivatwr
173200070329     C                   endif
173300070326     C*
173400070326     C* Effettuo la chiamata al CLLE preposto
173500110930     C                   call(e)   'TITVEVTC'
173600070326     C                   parm                    parccm
173700070326     C                   parm                    parmbr
173800070326     C                   parm      '2'           paropz
173900070326     C*
174000070326     C                   eval      wrkesito = '2'
174100070404     C*
174200070404     C                   seton                                        LR
174300070326     C*
174400070326     C                   ENDSR     '*CANCL'
174500070326     C***
174600100318
174700100318
174800100318
174900100318     C     normCSV       BEGSR
175000100318     C*
175100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175200100318     C* separatori campi
175300100318     C                   if        CharTXT <> *blanks
175400100318     C                   z-add     *zeros        wLenVINDTA        4 0
175500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175600100318     C                   setoff                                       80
175700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
176000100318     C                   if        not *in80
176100100318     C                   seton                                        80
176200100318     C                   else
176300100318     C                   setoff                                       80
176400100318     C                   endif
176500100318     C                   endif
176600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176700100318     C                             and *in80 = *on
176800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176900100318     C                   endif
177000100318     C                   enddo
177100100318     C                   endif
177200100318     C*
177300100318     C                   ENDSR
177400100318     C***
177500070326
177600100326
177700100326
177800100326     C*----------------------------------------------------*
177900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
178000100326     C*----------------------------------------------------*
178100100326     C     REPNSP        BEGSR
178200100326     C*
178300100326     C* NSP => Stacco un numeratore da AZNUM
178400100326     C                   clear                   TRUL33DS
178500100326     C                   eval      I33OPE = *zeros
178600100326     C                   eval      I33CNU = 302
178700100326     C                   eval      I33NUM = 1
178800100326     C                   movel     TRUL33DS      KPJBU
178900100326     C                   call      'TRUL33R'
179000100326     C                   parm                    KPJBA
179100100326     C                   movel     KPJBU         TRUL33DS
179200100326     C                   if        O33ERR = *zeros
179300100326     C                   z-add     O33NRF        VABNSP
179400100326     C                   z-add     O33NRF        VATNSP
179500100326     C                   else
179600100326     C                   SETON                                        31
179700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179800100326     C                             + ' ' + 'VABNSP VATNSP'
179900100326     C                   endif
180000100326     C*
180100100326     C                   ENDSR
180200010330
180300100326
180400010330
180500000613     C     *inzsr        BEGSR
180600990910     C*
180700990910     C     *entry        plist
180800990920     C                   parm                    tivlrds
180900990921     C                   parm      wrkesito      esito
181000000724     C                   parm                    prmlit
181100000710     C                   parm                    prmfir
181200010330     C*
181300010330     C* CALCOLA LA DATA CORRENTE
181400110930     C                   time                    wn14             14 0
181500110930     C                   movel     wn14          oracor            6 0          *ORA
181600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181700100318     C                   eval      datcor = %dec(%date() : *ISO)
181800000613     C*
181900000613     C                   ENDSR
182000000613     C***
182100990908
182200120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182300130403FILL1;VABRSD;VABIND;VABLOD;VABPRD;VABCAD;VABNCL;VABPKB;VABRMN;FILL2;
182400130403FILL3
182500130403
182600120202
182700120202
