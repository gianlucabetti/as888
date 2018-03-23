000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121030      *
000300121030      * PARTICOLARITA':
000400130122      * il telefono (cellulare e fisso) e le note sono messe in VAB
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
004000121030     D Intestazioni    s               n   inz(*on)
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
041400130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
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
042600130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
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
043700130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
043800110930     C                   eval      VATCNT = 1
043900110930     C                   write     EDIVAT00
044000060331     C                   add       1             §CTROKVT
044100060331     C                   endif
044200060331     C*
044300060331     C                   if        wVATNOT_E <> *blanks
044400110201     C                   eval      VATNOT = wVATNOT_E
044500060331     C                   eval      VATTRC = 'E'
044600121030     C                   MOVE(P)   vlrMSG        wNomeFile
044700130122     C***                EVAL      VATCMR = wNomeFile
044800130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044900121030     C                   eval      VATCNT = 1
045000121030     C*
045100121030     C* Sviluppati i "CHI SONO" in relazione al numero colli
045200121030     C*
045300121030     C     1             DO        VABNCL        wNumCollo         3 0
045400121030     C*** da analisi del POC il progressivo colli è max 3 char e deve ccontiene gli zeri non
045500121030     C*** significativi
045600121030     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
045700121030     C                                %editc(wNumCollo:'X')
045800121030     C                   write     EDIVAT00
045900121030     C                   add       1             §CTROKVT
046000121030     C                   ENDDO
046100121030     C*
046200121030     C                   endif
046300110927     C*
046400110927     C                   if        wVATNOT_I <> *blanks
046500110927     C                   eval      VATNOT = wVATNOT_I
046600110927     C                   eval      VATTRC = 'I'
046700120203     C                   MOVE(P)   vlrMSG        wNomeFile
046800130122     C***                EVAL      VATCMR = wNomeFile
046900130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
047000110930     C                   eval      VATCNT = 1
047100110930     C                   write     EDIVAT00
047200110927     C                   add       1             §CTROKVT
047300110927     C                   endif
047400120202     C*
047500120202     C                   if        wVATNOT_J <> *blanks
047600120202     C                   eval      VATNOT = wVATNOT_J
047700120202     C                   eval      VATTRC = 'J'
047800120203     C                   MOVE(P)   vlrMSG        wNomeFile
047900130122     C***                EVAL      VATCMR = wNomeFile
048000130122     C                   eval      VATCMR = %char(datcor)+%char(oracor)
048100120202     C                   eval      VATCNT = 1
048200120202     C                   write     EDIVAT00
048300120202     C                   add       1             §CTROKVT
048400120202     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700040119
048800040119
048900040119
049000040119     C*----------------------------------------------------*
049100110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049200040119     C*----------------------------------------------------*
049300040119     C     PREVAT        BEGSR
049400040119     C*
049500110930     C* Compongo il nome del membro da dare al EDIVATWR
049600040119     C                   eval      parmbr = vlrhdl
049700040119     C                   movel     'M'           parmbr
049800040510     C                   eval      parccm = vlrksc
049900040119     C                   eval      paropz = '1'
050000040119     C* Effettuo la chiamata al CLLE preposto
050100110930     C                   call(e)   'TITVEVTC'
050200040119     C                   parm                    parccm
050300040119     C                   parm                    parmbr
050400040119     C                   parm                    paropz
050500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600040119     C                   if        %error
050700040119     C                   movel     '1'           chkcall
050800040119     C                   else
050900040119     C                   movel     '0'           chkcall
051000040119     C                   endif
051100040119     C*
051200040119     C                   ENDSR
051300010601
051400010601
051500010601
051600010330     C*----------------------------------------------------*
051700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051800010330     C*----------------------------------------------------*
051900040119     C     INZVAR        BEGSR
052000080527     C*
052100080527     C                   setoff                                       65
052200010330     C*
052300030822     C* Inizializzo variabili di wrk
052400130128     C                   Z-ADD     *zeros        Num3_0            3 0
052500020204     C                   Z-ADD     *zeros        Num5_0            5 0
052600101018     C                   MOVEL     *blanks       wFlgCAS
052700060331     C                   MOVEL     *blanks       wVATNOT_A
052800060331     C                   MOVEL     *blanks       wVATNOT_B
052900060331     C                   MOVEL     *blanks       wVATNOT_E
053000110927     C                   MOVEL     *blanks       wVATNOT_I
053100120202     C                   MOVEL     *blanks       wVATNOT_J
053200120202     C                   MOVEL     *blanks       wVATNOT_IJ
053300130128     C                   MOVEL     *blanks       w70_1
053400130128     C                   MOVEL     *blanks       w70_2
053500130128     C                   MOVEL     *blanks       w70_3
053600130128     C                   MOVEL     *blanks       w70_4
053700030822     C*
053800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053900030822     C                   CLEAR                   SkSplitCSV
054000030822     C                   EVAL      i = 1
054100030822     C                   EVAL      posDa = *zeros
054200030822     C                   EVAL      posA  = *zeros
054300080527     C*
054400110930     C                   CLEAR                   EDIVABS
054500110930     C                   CLEAR                   EDIVAT00
054600030822     C*
054700030822     C* Reimposto i valori di default
054800030822     C                   EXSR      DEFCAM
054900030822     C*
055000010330     C                   ENDSR
055100010330     C*----------------------------------------------------*
055200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055300010330     C*----------------------------------------------------*
055400010330     C     DEFCAM        BEGSR
055500030822     C*
055600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055700030715     C* e delimitatore testo.
055800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056100100318     C*
056200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056300100318     C                   EVAL      CharSOS = CharNUM
056400030822     C*
056500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056600100720     C                   SETOFF                                       505152
056700100720     C                   SETOFF                                       53
056800100413     C                   SELECT
056900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057000100720     C                   SETON                                        50
057100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057200100413     C                   SETON                                        51
057300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057400100720     C                   SETON                                        52
057500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057600100720     C                   SETON                                        53
057700100413     C                   ENDSL
057800030822     C*
057900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
058000030822     C                   EVAL      posDaDft = 1
058100030822     C                   EVAL      posADft  = 0
058200030822     C                   EVAL      wGiroDft = 0
058300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058400030822     C                             posDaDft > 0
058500030822     C*
058600030822     C* Gestisco il 1° giro
058700030822     C                   IF        wGiroDft = 0
058800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
059000030822     C* Incremento il contatore dei "giri"
059100030822     C                   EVAL      wGiroDft = 1
059200030822     C                   ELSE
059300030822     C                   EVAL      posDaDft = posADft
059400030822     C                   ENDIF
059500030822     C* Eseguo lo scan x trovare la fine del campo corrente
059600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059700030822     C*
059800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
060000030822     C                   IF        posDaDft > 0 AND
060100030822     C                             posADft  > 0
060200030822     C* NCL
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NCL'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNCL
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CCM
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CCM'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCCM
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* LNP
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'LNP'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiInt=*on
063800030822     C                   Z-ADD     PiVal         VABLNP
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* NRS
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'NRS'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiInt=*on
065100030822     C                   Z-ADD     PiVal         VABNRS
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* CTR
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'CTR'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiInt=*on
066400030822     C                   Z-ADD     PiVal         VABCTR
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* PKB
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'PKB'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700030822     C                   Z-ADD     PiVal         VABPKB
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* VLB
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VLB'
068500030822     C                   EVAL      PiStr=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   EXSR      CHKNUM
068900030822     C                   IF        PiNum=*on
069000030822     C                   Z-ADD     PiVal         VABVLB
069100030822     C                   ENDIF
069200030822     C                   ENDIF
069300030822     C* QFT
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'QFT'
069800030822     C                   EVAL      PiStr=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   EXSR      CHKNUM
070200030822     C                   IF        PiNum=*on
070300030822     C                   Z-ADD     PiVal         VABQFT
070400030822     C                   ENDIF
070500030822     C                   ENDIF
070600030822     C* CBO
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'CBO'
071100030822     C                   EVAL      VABCBO=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* TSP
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'TSP'
072000030822     C                   EVAL      VABTSP=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VAS
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VAS'
072900030822     C                   EVAL      VABVAS=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* VCA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'VCA'
073800030822     C                   EVAL      VABVCA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200070925     C* TIC
074300070925     C                   IF        %subst(
074400070925     C                             %subst(vlrppt:posDaDft+1:
074500070925     C                             posADft-posDaDft-1):1:3)
074600070925     C                             = 'TIC'
074700070925     C                   EVAL      VABTIC=%trim(%subst(
074800070925     C                             %subst(vlrppt:posDaDft+1:
074900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000070925     C                   ENDIF
075100030822     C* GCA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GCA'
075600030822     C                   EVAL      VABGCA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* CTM
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'CTM'
076500030822     C                   EVAL      VABCTM=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* FFD
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'FFD'
077400030822     C                   EVAL      VABFFD=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* VAD
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'VAD'
078300030822     C                   EVAL      VABVAD=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GMA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GMA'
079200030822     C                   EVAL      VABGMA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* GGA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'GGA'
080100030822     C                   EVAL      VABGGA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* GVA
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'GVA'
081000030822     C                   EVAL      VABGVA=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* TC1
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'TC1'
081900030822     C                   EVAL      VABTC1=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* TC2
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'TC2'
082800030822     C                   EVAL      VABTC2=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200040714     C* VATTRC
083300040714     C                   IF        %subst(
083400040714     C                             %subst(vlrppt:posDaDft+1:
083500040714     C                             posADft-posDaDft-1):1:3)
083600040714     C                             = 'TRC'
083700040714     C                   EVAL      VATTRC=%trim(%subst(
083800040714     C                             %subst(vlrppt:posDaDft+1:
083900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000040714     C                   ENDIF
084100030822     C* ...
084200030822     C                   ENDIF
084300030822     C                   ENDDO
084400020204     C*
084500010330     C                   ENDSR
084600010607     C*----------------------------------------------------*
084700110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
084800010607     C*----------------------------------------------------*
084900010607     C     IMPVAB        BEGSR
085000010607     C*
085100010607     C                   SETOFF                                       3132
085200110315 xxx C***                SETON                                        35
085300030515     C*
085400010607     C                   MOVEL     datcor        VABAAS
085500010607     C                   MOVE      datcor        VABMGS
085600010607     C*
085700030715     C**********
085800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085900030715     C**********
086000030715     C                   IF        wGiro = *zeros
086100040113     C*
086200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086500080429     C                             %trim(intcol(5))<>*blanks
086600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086800080429     C                                      %trim(intcol(5))
086900070823     C     ';':CharCSV   XLATE     vindta        vindta
087000040510     C                   ENDIF
087100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087200031201     C     minu:maiu     XLATE     vindta        vindta
087300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087400030715     C                   DOW       posDa <= %len(%trim(vindta))
087500030715     C*
087600030715     C* Gestisco il 1° campo
087700030715     C                   IF        i = 1
087800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087900030715     C                   EVAL      posDa = 1
088000030715     C* Eseguo lo scan x trovare la fine del primo campo
088100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C* X i campi successivi al 1°
088600030715     C                   ELSE
088700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088800030715     C                   EVAL      posDa = posA + 1
088900030715     C* Eseguo lo scan x trovare la fine del campo corrente
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* Gestisco l'ultimo campo
089200030715     C                   IF        posA = *zeros
089300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C                   LEAVE
089800030715     C                   ELSE
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C                   ENDIF
090300030715     C                   ENDIF
090400030715     C* Incremento il contatore d campo
090500030715     C                   EVAL      i = i +1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090900030715     C* delimitazione testo
091000030715     C                   EVAL      i = 1
091100030715     C                   DOW       i < %elem(SkSplitFLD)
091200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091400030715     C                   EVAL      i = i + 1
091500030715     C                   ENDDO
091600030715     C                   EVAL      wGiro = 1
091700030715     C                   ELSE
091800030715     C                   EVAL      wGiro = 2
091900031204     C**********
092000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092100031204     C* d separatore campo
092200031204     C**********
092300031204     C                   EVAL      vindta = %trim(vindta)
092400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092500031204     C                   EVAL      vindta = %subst(vindta:2)
092600031204     C                   ENDDO
092700031204     C*
092800031204     C                   Z-ADD     *zeros        lunghInput        4 0
092900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093300110912     C                   ENDDO
093400030715     C**********
093500030715     C* Effettuo lo split del campo dati d input
093600030715     C**********
093700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093800030715     C                   DOW       posDa <= %len(%trim(vindta))
093900030715     C*
094000030715     C* Gestisco il 1° campo
094100030715     C                   IF        i = 1
094200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094300030715     C                   EVAL      posDa = 1
094400030715     C* Eseguo lo scan x trovare la fine del primo campo
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C* X i campi successivi al 1°
095000030715     C                   ELSE
095100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095200030715     C                   EVAL      posDa = posA + 1
095300031204     C* Verifico che nn vi sia il campo nn valorizzato
095400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095500031204     C* Se campo nn valorizzato skippo al prossimo
095600031204     C                   EVAL      SkSplitCSV(i) = *blanks
095700031204     C                   EVAL      posA  = posA + 1
095800031204     C                   ELSE
095900030715     C* Eseguo lo scan x trovare la fine del campo corrente
096000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096100030715     C* Gestisco l'ultimo campo
096200030715     C                   IF        posA = *zeros
096300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096400030715     C* A questo punto "estraggo" il campo corrente
096500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096600030715     C                                                    (posA-posDa))
096700030715     C                   LEAVE
096800030715     C                   ELSE
096900030715     C* A questo punto "estraggo" il campo corrente
097000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097100030715     C                                                    (posA-posDa))
097200030715     C                   ENDIF
097300031204     C                   ENDIF
097400030715     C                   ENDIF
097500030715     C* Incremento il contatore d campo
097600030715     C                   EVAL      i = i +1
097700030715     C                   ENDDO
097800030715     C*
097900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098000030715     C* delimitazione testo
098100030715     C                   EVAL      i = 1
098200030715     C                   DOW       i < %elem(SkSplitCSV)
098300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098500030715     C                   EVAL      i = i + 1
098600030715     C                   ENDDO
098700100720     C*
098800100720     C* Stacco un progressivo x numero spedizione
098900110930 xxx C                   EXSR      repNSP
099000100326     C*
099100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitFLD)
099400030715     C*
099500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099700030715     C                             SkSplitCSV(i) <> *blanks
099800030721     C*** ATB
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100000030721     C                   EVAL      VABATB = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030715     C*** RSD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100700030715     C* ==
100800030715     C                   ENDIF
100900030721     C*** RD2
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030715     C*** IND
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101500030715     C                   EVAL      VABIND = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** LOD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** PRD
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030721     C*** NZD
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030715     C*** RMA
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** NOT
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** NT2
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** VCA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** VAS
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** FFD
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030721     C*** GC1
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GC2
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TSP
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106400030721     C                   ENDIF
106500040510     C*** CBO
106600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106800040510     C                   ENDIF
106900030721     C*** NAS
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TIC
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GCA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** XCO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** CTM
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TCR
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** CTS
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** FTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** VAD
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** GMA
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** GGA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GVA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TC1
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** TC2
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** SCL
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** RMO
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** NMO
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*
113800030721     C* Reperisco quindi i campi numerici...
113900040510     C*** NSP
114000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114100040510     C                   EVAL      PiStr=SkSplitCSV(i)
114200040510     C                   EXSR      CHKNUM
114300040510     C                   IF        PiInt=*on
114400040510     C                   Z-ADD     PiVal         VABNSP
114500040510     C                   ELSE
114600040510     C                   SETON                                        31
114700040510     C                   EVAL      VABNSP = *zeros
114800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114900040510     C                             + ' ' + 'VABNSP'
115000040510     C                   ENDIF
115100040510     C                   ENDIF
115200071106     C*** RMN
115300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115400071106     C                   EVAL      PiStr=SkSplitCSV(i)
115500071106     C                   EXSR      CHKNUM
115600071106     C                   IF        PiInt=*on
115700071106     C                   Z-ADD     PiVal         VABRMN
115800071106     C                   ELSE
115900071106     C                   SETON                                        32
116000071106     C                   EVAL      VABRMN = *zeros
116100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116200071106     C                             + ' ' + 'VABRMN'
116300071106     C                   ENDIF
116400071106     C                   ENDIF
116500030721     C*** CAD
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on
117000030721     C                   Z-ADD     PiVal         Num5_0
117100030721     C                   MOVEL(p)  Num5_0        VABCAD
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCAD = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABCAD'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** NCL
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on
118400030721     C                   Z-ADD     PiVal         VABNCL
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABNCL = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040122     C                             + ' ' + 'VABNCL'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** PKB
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABPKB
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABPKB = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABPKB'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** CAS
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120800130125     C                             %trim(SkSplitCSV(i)) <> '0,00'  AND
120900130125     C                             %trim(SkSplitCSV(i)) <> *zeros
121000101018     C                   EVAL      wFlgCAS = '1'
121100101018     C                   ENDIF
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABCAS
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCAS = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABCAS'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** CCM
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on      AND
122800030721     C                             PiVal<=9999999 AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABCCM
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABCCM = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500040510     C                             + ' ' + 'VABCCM'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** LNP
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on    AND
124300030721     C                             PiVal<=999   AND
124400030721     C                             PiVal>*zeros
124500030721     C                   Z-ADD     PiVal         VABLNP
124600030721     C                   ELSE
124700040715     C                   SETON                                        32
124800030721     C                   EVAL      VABLNP = *zeros
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040510     C                             + ' ' + 'VABLNP'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NRS
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on AND
125800030721     C                             PiVal<=99
125900030721     C                   Z-ADD     PiVal         VABNRS
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNRS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** LNA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on    AND
127100030721     C                             PiVal<=999   AND
127200030721     C                             PiVal>*zeros
127300030721     C                   Z-ADD     PiVal         VABLNA
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      VABLNA = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABLNA'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** CTR
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on  AND
128600030721     C                             PiVal<=999
128700030721     C                   Z-ADD     PiVal         VABCTR
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABCTR'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** IAS
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiNum=*on
129900030721     C                   Z-ADD     PiVal         VABIAS
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABIAS'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** VLB
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiNum=*on
131100030721     C                   Z-ADD     PiVal         VABVLB
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABVLB'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** QFT
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABQFT
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABQFT'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** NCD
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on      AND
133500030721     C                             PiVal<=9999999
133600030721     C                   Z-ADD     PiVal         VABNCD
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABNCD'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** NCA
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on      AND
134800030721     C                             PiVal<=9999999
134900030721     C                   Z-ADD     PiVal         VABNCA
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABNCA'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** ZNC
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on AND
136100030721     C                             PiVal<=99
136200030721     C                   Z-ADD     PiVal         VABZNC
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABZNC'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** DCR
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on       AND
137400030721     C                             PiVal<=99999999
137500030721     C                   Z-ADD     PiVal         VABDCR
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABDCR'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** HCR
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on   AND
138700030721     C                             PiVal<=9999
138800030721     C                   Z-ADD     PiVal         VABHCR
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABHCR'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** VMD
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiNum=*on
140000030721     C                   Z-ADD     PiVal         VABVMD
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABVMD'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** ANT
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiInt=*on        AND
141200030721     C                             PiVal<=999999999
141300030721     C                   Z-ADD     PiVal         VABANT
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABANT'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000030721     C*** CMO
142100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142200030721     C                   EVAL      PiStr=SkSplitCSV(i)
142300030721     C                   EXSR      CHKNUM
142400030721     C                   IF        PiInt=*on
142500030721     C                   Z-ADD     PiVal         Num5_0
142600030721     C                   MOVEL(p)  Num5_0        VABCMO
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      VABCMO = *zeros
143000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143100030721     C                             + ' ' + 'VABCMO'
143200030721     C                   ENDIF
143300030721     C                   ENDIF
143400040510     C*
143500110930     C* Reperisco i campi del EDIVAT
143600040510     C*** VATNOT
143700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143900040510     C                   ENDIF
144000040510     C*** VATTRC
144100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144300040510     C                   ENDIF
144400060331     C*** VATNOT_A
144500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144700060331     C                   ENDIF
144800060331     C*** VATNOT_B
144900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145100060331     C                   ENDIF
145200060331     C*** VATNOT_E
145300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145500060331     C                   ENDIF
145600110927     C*** VATNOT_I
145700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145900110927     C                   ENDIF
146000070823     C*
146100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146200130125     C*
146300130125     C*** VABRSD
146400130125     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
146500130125     C                   EVAL      VABRSD = %trim(SkSplitCSV(i))
146600130125     C                   IF        %subst(VABRSD:1:10) = 'STRANO SPA'
146700130125     C                   EVAL      VABCTR = 405
146800130125     C                   ENDIF
146900130125     C                   ENDIF
147000130128     C*
147100130128     C*** VABRMA
147200130128     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
147300130128     C                   EVAL      PiStr=SkSplitCSV(i)
147400130128     C                   EXSR      CHKNUM
147500130128     C                   IF        PiInt=*on
147600130128     C                   Z-ADD     PiVal         Num3_0
147700130128     C                   MOVEL(p)  Num3_0        VABRMA
147800130128     C                   ELSE
147900130128     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
148000130128     C                   ENDIF
148100130128     C                   ENDIF
148200121030     C*
148300130122     C*** VABNOT_1
148400130122     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
148500130128     C                   EVAL      w70_1 = %trim(SkSplitCSV(i))
148600121030     C                   ENDIF
148700130122     C*
148800130122     C*** VABNOT_2
148900130122     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
149000130128     C                   EVAL      w70_2 = %trim(SkSplitCSV(i))
149100130122     C                   ENDIF
149200130122     C*
149300130122     C*** VABNOT_3
149400130122     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_3'
149500130128     C                   EVAL      w70_3 = %trim(SkSplitCSV(i))
149600130122     C                   ENDIF
149700130128     C*
149800130128     C*** VABNOT_4
149900130128     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_4'
150000130128     C                   EVAL      w70_4 = %trim(SkSplitCSV(i))
150100130128     C                   ENDIF
150200070925     C*
150300071029     C***  <===  -----------------------------------------
150400040510     C*
150500030715     C                   ENDIF
150600030715     C                   EVAL      i = i + 1
150700030715     C                   ENDDO
150800070925     C*
150900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151000121030     C*
151100121030     C* se valorizzato il 2 prendo quello, altrimenti l'1
151200130128     C                   IF        w70_1 <> *blanks OR
151300130128     C                             w70_2 <> *blanks OR
151400130128     C                             w70_3 <> *blanks OR
151500130128     C                             w70_4 <> *blanks
151600130128     C                   MOVEL     *blanks       w70              70
151700130128     C                   EVAL      w70 = %trim(w70_1) + ' ' +
151800130128     C                                   %trim(w70_2) + ' ' +
151900130128     C                                   %trim(w70_3) + ' ' +
152000130128     C                                   %trim(w70_4)
152100130122     C                   EVAL      VABNOT = %subst(w70 : 1 : 35)
152200130122     C                   EVAL      VABNT2 = %subst(w70 : 36 : 35)
152300121030     C                   ENDIF
152400070925     C*
152500071029     C***  <===  -----------------------------------------
152600070925     C*
152700070925     C* Considerazioni finali su CBO/CAS
152800130122     C                   EVAL      VABTIC = *blank
152900130122     C                   EVAL      vabVCA = *blank
153000100318     C                   IF        wFlgCAS = '1'    OR
153100100318     C                             VABCAS  > *zeros
153200130122     C                   EVAL      VABTIC = 'BM'
153300130122     C                   EVAL      vabVCA = 'EUR'
153400070925     C                   IF        VABCBO = '1'
153500070925     C                   EVAL      VABCBO = '4'
153600070925     C                   ENDIF
153700070925     C                   IF        VABCBO = '2'
153800070925     C                   EVAL      VABCBO = '6'
153900070925     C                   ENDIF
154000070925     C                   ENDIF
154100070925     C***  <===  ----------------------------
154200040510     C*
154300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154400040510     C                   IF        VABNSP = *zeros
154500040510     C                   SETON                                        31
154600040510     C                   ENDIF
154700040715     C*
154800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154900040715     C                   IF        VABLNP = *zeros
155000040715     C                   SETON                                        31
155100040715     C                   ENDIF
155200040510     C*
155300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155400040510     C                   IF        VABRMN = *zeros
155500040510     C                   Z-ADD     VABNSP        VABRMN
155600040510     C                   ENDIF
155700040510     C*
155800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155900040510     C                   IF        VABRMA = *blanks
156000040714     C                   MOVEL     VABRMN        VABRMA
156100040510     C                   ENDIF
156200040128     C*
156300040510     C* Se provincia nn valorizzata la reperisco
156400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156500110912     C                   IF        VABPRD =  *blanks AND
156600110912     C                             VABNZD =  *blanks AND
156700110912     C                             VABCAD <> *blanks AND
156800110912     C                             VABLOD <> *blanks
156900040128     C                   CLEAR                   TISI95DS
157000040128     C                   EVAL      I95TCN = '3'
157100040128     C                   Z-ADD     datcor        I95DAT
157200040128     C                   EVAL      I95NAR = VABNZD
157300040128     C                   EVAL      I95CAP = VABCAD
157400040128     C                   EVAL      I95LOC = VABLOD
157500040128     C                   CALL      'TISI95R'
157600040128     C                   PARM                    TISI95DS
157700040128     C                   EVAL      VABPRD = O95PRV
157800040510     C                   ENDIF
157900020204     C*
158000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158100020204     C                   EXSR      CHKIMPDIV
158200030715     C                   ENDIF
158300020204     C*
158400010607     C                   ENDSR
158500010607     C*----------------------------------------------------*
158600010601
158700020204
158800020204     C*----------------------------------------------------*
158900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159000020204     C*----------------------------------------------------*
159100020204     C     CHKIMPDIV     BEGSR
159200020204     C*
159300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159400020204     C                   Z-ADD     *zeros        wrkDec            9 9
159500020204     C*
159600020204     C* Come prima cosa effettuo considerazioni sulla divisa
159700020204     C                   IF        vabIAS > *zeros
159800020204     C                   IF        vabVAS <> 'EUR'
159900020204     C                   EVAL      vabVAS =  'ITL'
160000020204     C                   ENDIF
160100020204     C                   ENDIF
160200020204     C*
160300020204     C                   IF        vabCAS > *zeros
160400020204     C                   IF        vabVCA <> 'EUR'
160500020204     C                   EVAL      vabVCA =  'ITL'
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C                   IF        vabVMD > *zeros
161000020204     C                   IF        vabVAD <> 'EUR'
161100020204     C                   EVAL      vabVAD =  'ITL'
161200020204     C                   ENDIF
161300020204     C                   ENDIF
161400020204     C*
161500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161600020204     C                   Z-ADD     vabIAS        wrkDec
161700020204     C                   IF        wrkDec > *zeros
161800020204     C                   IF        vabVAS = 'ITL'
161900020204     C                   EVAL      vabIAS = *zeros
162000020204     C                   ENDIF
162100020204     C                   ENDIF
162200020204     C*
162300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162400020204     C                   Z-ADD     vabCAS        wrkDec
162500020204     C                   IF        wrkDec > *zeros
162600020204     C                   IF        vabVCA = 'ITL'
162700020204     C                   EVAL      vabCAS = *zeros
162800020204     C                   ENDIF
162900020204     C                   ENDIF
163000020204     C*
163100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163200020204     C                   Z-ADD     vabVMD        wrkDec
163300020204     C                   IF        wrkDec > *zeros
163400020204     C                   IF        vabVAD = 'ITL'
163500020204     C                   EVAL      vabVMD = *zeros
163600020204     C                   ENDIF
163700020204     C                   ENDIF
163800020204     C*
163900020204     C                   ENDSR
164000020204     C***
164100010330
164200100318
164300100318
164400100318     C*----------------------------------------------------*
164500100318     C*  CONTROLLO NUMERICITA' CAMPI
164600100318     C*----------------------------------------------------*
164700100318     C     CHKNUM        BEGSR
164800100318     C*
164900100318     C                   IF        PiDecChr = *blanks
165000100318     C                   EVAL      PiDecChr = CharNUM
165100100318     C                   ENDIF
165200100318     C*
165300100318     C                   callp     UBISNUM_Check(PiStr
165400100318     C                                          :PiDecChr
165500100318     C                                          :PiVal
165600100318     C                                          :PiNum
165700100318     C                                          :PiInt)
165800100318     C*
165900100318     C                   ENDSR
166000100318     C***
166100010601
166200010601
166300010601
166400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166500010601     C     repfil        BEGSR
166600010601     C*
166700010601     C                   if        invfil = *zeros and
166800010601     C                             depfil > *zeros and
166900010629     C                             (vinflg = *blanks or
167000010629     C                              vinflg = *zeros)
167100010601     C
167200010601     C                   eval      invfil = depfil
167300010601     C                   endif
167400010601     C*
167500010601     C                   if        depfil <> invfil and
167600010601     C                             invfil > *zeros
167700010601     C                   eval      flgMulti = '1'
167800010601     C                   if        vinflg = *blanks
167900010601     C                   add       1             cntNonEl
168000010601     C                   endif
168100010601     C                   endif
168200010601     C*
168300010601     C                   if        vinflg = '2'
168400010601     C                   eval      flgStato = '2'
168500010601     C                   endif
168600010601     C*
168700010601     C                   ENDSR
168800010601     C***
168900010601
169000010601
169100010601
169200010330
169300040119      /TITLE Invio dei dati al punto operativo.
169400040119     C     invio         BEGSR
169500040119     C*
169600110930     C* 1° invio EDIVAT
169700040119     C                   reset                   dscmz
169800040119     C                   move      vlrpoi        cmzdst
169900110930     C                   eval      cmzfld = 'EDIVATWR'
170000040119     C                   eval      cmzmbd = vlrhdl
170100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170200040119     C***                if        prmfir = *blanks
170300110930     C                   eval      cmzfla = 'EDIVAT0F'
170400110930     C                   eval      cmzmba = 'EDIVAT0F'
170500040119     C***                else
170600040119     C***                eval      cmzfla = prmfir
170700040119     C***                eval      cmzmba = prmfir
170800040119     C***                endif
170900040119     C                   eval      cmznrr = *zeros
171000040119     C                   move      §ctrokvt      cmznrr
171100040119     C                   eval      cmzlba = vlrfl1
171200040119     C                   call(e)   'TIS711C'
171300040119     C                   parm                    dscmz
171400040119     C                   parm      *blanks       esito
171500040119     C                   if        %error
171600040119     C                             or cmzerr = '1'
171700040119     C                             or esito  = '1'
171800040119     C                   eval      wrkesito = '3'
171900040119     C                   else
172000040119     C*
172100110930     C* 2° invio EDIVAB
172200040119     C                   reset                   dscmz
172300040119     C                   move      vlrpoi        cmzdst
172400040119     C                   eval      cmzfld = vlrfou
172500040119     C                   eval      cmzmbd = vlrhdl
172600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172700040119     C***                if        prmfir = *blanks
172800110930     C                   eval      cmzfla = 'EDIVAB0F'
172900110930     C                   eval      cmzmba = 'EDIVAB0F'
173000040119     C***                else
173100040119     C***                eval      cmzfla = prmfir
173200040119     C***                eval      cmzmba = prmfir
173300040119     C***                endif
173400040119     C                   eval      cmznrr = *zeros
173500040119     C                   move      §ctrokvb      cmznrr
173600040119     C                   eval      cmzlba = vlrfl1
173700040119     C                   call(e)   'TIS711C'
173800040119     C                   parm                    dscmz
173900040119     C                   parm      *blanks       esito
174000040119     C                   if        %error
174100040119     C                             or cmzerr = '1'
174200040119     C                             or esito  = '1'
174300040119     C                   eval      wrkesito = '3'
174400040119     C                   endif
174500040119     C                   endif
174600040119     C*
174700040119     C                   ENDSR
174800040119     C***
174900010601
175000010601
175100010601
175200010601
175300010601      /TITLE Invio dei dati al punto operativo.
175400010601     C     opeini        BEGSR
175500010601     C*
175600010601     C* Inizializzo flag e contatori operativi
175700010601     C                   movel     '0'           flgGiro           1
175800010601     C                   movel     '0'           flgMulti          1
175900010601     C                   movel     '1'           flgStato          1
176000010615     C                   movel     '0'           flgOk             1
176100010601     C                   z-add     *zeros        cntNonEl         10 0
176200010601     C                   z-add     *zeros        depfil            3 0
176300010601     C                   z-add     *zeros        invfil            3 0
176400010601     C*
176500010601     C                   ENDSR
176600010601     C***
176700070326
176800070326
176900070326
177000070326
177100070326     C     *pssr         BEGSR
177200070329     C*
177300070329     C                   if        %open(tivin00r)
177400070329     C                   close     tivin00r
177500070329     C                   endif
177600110930     C                   if        %open(edivabwr)
177700110930     C                   close     edivabwr
177800070329     C                   endif
177900110930     C                   if        %open(edivatwr)
178000110930     C                   close     edivatwr
178100070329     C                   endif
178200070326     C*
178300070326     C* Effettuo la chiamata al CLLE preposto
178400110930     C                   call(e)   'TITVEVTC'
178500070326     C                   parm                    parccm
178600070326     C                   parm                    parmbr
178700070326     C                   parm      '2'           paropz
178800070326     C*
178900070326     C                   eval      wrkesito = '2'
179000070404     C*
179100070404     C                   seton                                        LR
179200070326     C*
179300070326     C                   ENDSR     '*CANCL'
179400070326     C***
179500100318
179600100318
179700100318
179800100318     C     normCSV       BEGSR
179900100318     C*
180000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180100100318     C* separatori campi
180200100318     C                   if        CharTXT <> *blanks
180300100318     C                   z-add     *zeros        wLenVINDTA        4 0
180400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180500100318     C                   setoff                                       80
180600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180900100318     C                   if        not *in80
181000100318     C                   seton                                        80
181100100318     C                   else
181200100318     C                   setoff                                       80
181300100318     C                   endif
181400100318     C                   endif
181500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181600100318     C                             and *in80 = *on
181700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181800100318     C                   endif
181900100318     C                   enddo
182000100318     C                   endif
182100100318     C*
182200100318     C                   ENDSR
182300100318     C***
182400070326
182500100326
182600100326
182700100326     C*----------------------------------------------------*
182800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182900100326     C*----------------------------------------------------*
183000100326     C     REPNSP        BEGSR
183100100326     C*
183200100326     C* NSP => Stacco un numeratore da AZNUM
183300100326     C                   clear                   TRUL33DS
183400100326     C                   eval      I33OPE = *zeros
183500100326     C                   eval      I33CNU = 302
183600100326     C                   eval      I33NUM = 1
183700100326     C                   movel     TRUL33DS      KPJBU
183800100326     C                   call      'TRUL33R'
183900100326     C                   parm                    KPJBA
184000100326     C                   movel     KPJBU         TRUL33DS
184100100326     C                   if        O33ERR = *zeros
184200100326     C                   z-add     O33NRF        VABNSP
184300100326     C                   z-add     O33NRF        VATNSP
184400100326     C                   else
184500100326     C                   SETON                                        31
184600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184700100326     C                             + ' ' + 'VABNSP VATNSP'
184800100326     C                   endif
184900100326     C*
185000100326     C                   ENDSR
185100010330
185200100326
185300010330
185400000613     C     *inzsr        BEGSR
185500990910     C*
185600990910     C     *entry        plist
185700990920     C                   parm                    tivlrds
185800990921     C                   parm      wrkesito      esito
185900000724     C                   parm                    prmlit
186000000710     C                   parm                    prmfir
186100010330     C*
186200010330     C* CALCOLA LA DATA CORRENTE
186300110930     C                   time                    wn14             14 0
186400110930     C                   movel     wn14          oracor            6 0          *ORA
186500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186600100318     C                   eval      datcor = %dec(%date() : *ISO)
186700000613     C*
186800000613     C                   ENDSR
186900000613     C***
187000990908
187100120202** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187200130128W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABRMN;W_VABRMA;VABQFT;
187300130128VABCAS;VABTIC;VABVCA;W_VABNOT_4;W_VABNOT_3;W_VABNOT_1;W_VABNOT_2;
187400130122
187500120202
187600120202
