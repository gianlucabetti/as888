000100160121      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400160630      * disk A
000500160630      * - La prima riga del CSV va ignorata, le intestazioni di colonna sono nella seconda
000600160624      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800130917     H decedit('0,')
000900120209     F*
001000990910     Ftivin00r  uF   E             DISK    usropn
001100100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001200040119     FFIVATwwr  O    E             DISK    usropn
001300000313     D*
001400040113     D*------------
001500040113     D* SCHIEDE A PROGRAMMA
001600040113     D*------------
001700080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001800141003     D*------------
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600040128     D tisi95ds      e ds
002700100413     D fivabds       e ds                  extname(fivab00f)
002800100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002900100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003000010330     D esito           s              1
003100010330     D prmlit          s             10
003200010330     D prmfir          s             10
003300010330     D wrkesito        s                   like(esito)
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500040510     D parccm          s              8    INZ(*blanks)
003600040119     D parmbr          s             10    INZ(*blanks)
003700040119     D paropz          s              1    INZ(*blanks)
003800040119     D chkcall         s              1    INZ(*blanks)
003900080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000080418     D wFlgCAS         s              1    INZ(*blanks)
004100120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004200120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004300160630     D* in questo caso è la seconda
004400150114     D Intestazioni    s               n   inz(*on)
004500100326     D*------------------
004600100326     D* DS REPERIMENTO NUMERATORE
004700100326     D*------------------
004800100326     D trul33ds      e ds                  inz
004900100326     D*------------------
005000100326     D* DS ARCHITETTURA
005100100326     D*------------------
005200100326     D kpjba         e ds                  inz
005300130611     D*------------------
005400130611     D TRUL28DSX     e ds                  inz
005500100326
005600030715     D*------------------
005700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005800030715     D*------------------
005900030715     D SkSplitFLD      S             10    DIM(100)
006000030715     D SkSplitCSV      S            256    DIM(100)
006100030715     D CharCSV         S              1
006200030715     D CharTXT         S              1
006300030715     D CharNUM         S              1
006400100318     D CharSOS         S              1
006500130626     D posDa           S              4  0 INZ(*zeros)
006600130626     D posA            S              4  0 INZ(*zeros)
006700030715     D i               s              3  0 INZ(1)
006800030715     D wGiro           s              1  0 INZ(*zeros)
006900070823     D wVATNOT_A       s                   LIKE(VATNOT)
007000060331     D wVATNOT_B       s                   LIKE(VATNOT)
007100160624     D wVATNOT_E       s                   LIKE(VATNOT)
007200110927     D wVATNOT_I       s                   LIKE(VATNOT)
007300140220     D wVATNOT_J       s                   LIKE(VATNOT)
007400140220     D wVATNOT_IJ      s             70
007500140319     D wVATNOT_S       s                   LIKE(VATNOT)
007600140919     D wNOT            s             70
007700120411     D Data8A          s              8
007800140527     D wBarcodeN       s             11s 0
007900140527     D wX              s              3s 0
008000140711     D TxtInOut        S           2048
008100140711     D ElencoChar      S            256
008200140711     D TipoElenco      S              1
008300140711     D CharSost        S              1
008400140711     D UpperCase       S              1
008500140711     D ChkNull         S              1
008600140711     D CharNull        S              1
008700140711     D EsitoBon        S              1
008800030822     D*------------------
008900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009000030822     D*------------------
009100030822     D posDaDft        S              3  0 INZ(*zeros)
009200030822     D posADft         S              3  0 INZ(*zeros)
009300030822     D j               s              3  0 INZ(1)
009400030822     D wGiroDft        s              1  0 INZ(*zeros)
009500080418     D*------------------
009600080418     D* Costanti
009700080418     D*------------------
009800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010000100318
010100100318     D*------------------
010200100318     D* LINKING A DEFINIZIONI ESTERNE
010300100318     D*------------------
010400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010600100318
010700010330
010800010330
010900010330
011000000913     C                   reset                   rrnum
011100990921     C                   reset                   esito
011200990921     C                   reset                   wrkesito
011300010601     C*
011400010601     C                   exsr      opeini
011500010605     C                   exsr      rwvab
011600010601     C*
011700010601     C                   seton                                        lr
011800010601
011900010601
012000010601
012100010601     C*--------------------------------------------------------
012200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012300010601     C*--------------------------------------------------------
012400010601     C     PREELA        BEGSR
012500010601     C*
012600010601     C* SE OCCORRE SPEDIRE IN FILIALE
012700010601     C                   if        invfil <> *zeros and
012800010601     C                             flgGiro = '0'
012900010601     C*
013000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013100010601     C                   eval      flgGiro = '1'
013200010601     C*
013300010601     C                   endif
013400010601     C*
013500010601     C                   ENDSR
013600010601     C***
013700010601
013800010601
013900010601
014000010601
014100010601     C*--------------------------------------------------------
014200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014300010601     C*--------------------------------------------------------
014400010601     C     ENDELA        BEGSR
014500040119     C*
014600040119     C* Effettuo la chiamata al CLLE preposto
014700040119     C                   call(e)   'TITVVTC'
014800040510     C                   parm                    parccm
014900040119     C                   parm                    parmbr
015000040119     C                   parm      '2'           paropz
015100050201     C*
015200050201     C* Effettuo lancio TISI95 solo x chiusura
015300050201     C                   CLEAR                   TISI95DS
015400050201     C                   EVAL      I95TLA = 'C'
015500050201     C                   CALL      'TISI95R'
015600050201     C                   PARM                    TISI95DS
015700000616     C*
015800010601     C                   ENDSR
015900010601
016000010601
016100010330     C*--------------------------------------------------------
016200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016300010330     C*--------------------------------------------------------
016400010605     C     RWVAB         BEGSR
016500010330     C*
016600010330     C                   if        not %open(tivin00r)
016700010330     C                   open      tivin00r
016800010330     C                   endif
016900021025     C                   if        not %open(fivabwwr)
017000021025     C                   open      fivabwwr
017100010330     C                   endif
017200040119     C*
017300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017400040119     C                   exsr      prevat
017500040119     C*
017600040119     C                   if        chkcall = '0'
017700040119     C*
017800040119     C                   if        not %open(fivatwwr)
017900040119     C                   open      fivatwwr
018000040119     C                   endif
018100010330     C*
018200040119     C                   clear                   §CTROKVB          7 0
018300040119     C                   clear                   §CTROKVT          7 0
018400040119     C                   clear                   §CTRMO            7 0
018500040119     C                   clear                   §CTRNO            7 0
018600160630     C*
018700160630     C* siccome so che la prima riga del CSV va ignorata, faccio una lettura a vuoto
018800160630     C                   read      tivin00r
018900120410     C*
019000120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019100120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019200120410     C                   IF        Intestazioni = *off
019300120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019400120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019500120410     C                   exsr      inzvar
019600120410     C                   exsr      impvab
019700120410     C                   ENDIF
019800010330     C*
019900030822     C                   do        *HIVAL
020000010330     C*
020100030822     C                   read      tivin00r                               70
020200010618     C*
020300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020400010618     C                   if        vinflg = '1'
020500010618     C                   eval      flgOk = '1'
020600010618     C                   endif
020700040510     C*
020800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020900040510     C                   z-add     *zeros        wLenVINDTA        4 0
021000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021400040510     C                   z-add     1             wFlgVINDTA
021500040510     C                   leave
021600040510     C                   endif
021700040510     C                   enddo
021800010618     C*
021900040510     C                   if        vindta > *blanks AND
022000040510     C                             wFlgVINDTA = 1
022100010330     C                   add       1             rrnum
022200010330     C*
022300010601     C                   if        *in70 = *off and
022400010330     C                             (vinflg = *blanks
022500010330     C                              or vinflg = '0'
022600010330     C                              or vinflg = '2')
022700010330     C*
022800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022900010711     C                   if        vinflg = *blanks or vinflg = '0'
023000010711     C                   clear                   vinmsg
023100010711     C                   endif
023200130917     C*
023300130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023400130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023500130917     C* evitare che vengano elisi i blank
023600140220     C***                eval      vindta  = '*' + %trim(vindta)
023700130917     C*
023800080527     C                   exsr      inzvar
023900100318     C                   exsr      normCSV
024000010605     C                   exsr      impvab
024100010601     C*
024200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024300141014     C                   eval      depfil = VABLNP
024400141014     C                   exsr      repfil
024500141014     C                   if        depfil = invfil
024600150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
024700150430     C                   if        vlrpoi = 999
024800150430     C                   move(P)   invfil        VABFGS
024900150430     C                   else
025000150430     C                   move(P)   vlrpoi        VABFGS
025100150430     C                   endif
025200010601     C*
025300010601     C                   exsr      PREELA
025400010601     C*
025500010604     C* Ebbene...
025600010604     C*
025700040119     C  N31              add       1             §CTROKVB
025800040119     C   32              add       1             §CTRMO
025900040119     C   31              add       1             §CTRNO
026000080418     C*
026100030822     C                   if        wGiro = 2
026200080513     C* Scrivo sempre il VAT
026300080513     C  N31              exsr      wrivat
026400110201     C                   exsr      chkvab
026500080513     C                   endif
026600010604     C*
026700010604     C                   if        *in31 = *off and
026800010604     C                             *in32 = *off
026900010604     C                   eval      vinflg = '1'
027000010604     C                   else
027100010604     C                   eval      vinflg = '2'
027200010604     C                   endif
027300141014     C                   endif
027400010601     C*
027500010604     C                   endif
027600010604     C*
027700010330     C                   else
027800010330     C                   eval      vinflg = '1'
027900010330     C                   endif
028000010601     C*
028100010601     C  N70              update    tivin000
028200010330     C*
028300030822     C  N70              enddo
028400040119     C*
028500080527     C* Verifico dati rimasti "in cannna"...
028600080527     C                   seton                                        65
028700110201     C                   exsr      chkvab
028800080527     C*
028900040119     C                   endif                                                  (endif - chkcall)
029000010601     C*
029100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029200010601     C                   if        cntNonEl = *zeros or
029300010601     C                             flgMulti = '0'
029400010330     C* Se non ci sono record con errori ...
029500010601     C                   if        §ctrno = 0 and
029600010604     C                             §ctrmo = 0 and
029700010601     C                             flgStato <> '2'
029800010330     C* ... restituisco esito OK.
029900010330     C                   eval      wrkesito = '0'
030000010330     C                   else
030100040119     C                   if        §ctrokvb > 0
030200010330     C                   eval      wrkesito = '1'
030300010330     C                   else
030400010615     C                   if        flgOk = '0'
030500010615     C                   eval      wrkesito = '2'
030600010615     C                   else
030700010615     C                   eval      wrkesito = '6'
030800010615     C                   endif
030900010330     C                   endif
031000010330     C                   endif
031100010601     C                   else
031200010601     C                   eval      wrkesito = '9'
031300010601     C                   endif
031400010330     C*
031500010330     C                   if        %open(tivin00r)
031600010330     C                   close     tivin00r
031700010330     C                   endif
031800021025     C                   if        %open(fivabwwr)
031900021025     C                   close     fivabwwr
032000010330     C                   endif
032100040512     C                   if        %open(fivatwwr)
032200040512     C                   close     fivatwwr
032300040512     C                   endif
032400010601     C*
032500010601     C                   if        vlrpoi <> 999
032600010601     C                   eval      invfil = vlrpoi
032700010601     C                   endif
032800010330     C*
032900040119     C                   if        §ctrokvb > 0
033000010601     C                             and invfil > *zeros
033100010330     C                   exsr      invio
033200010330     C                   endif
033300010601     C*
033400010618     C                   if        flgGiro = '1'
033500010601     C                   exsr      endela
033600010618     C                   endif
033700010330     C*
033800010330     C                   ENDSR
033900010330     C***
034000080527
034100080527
034200080527
034300080527     C*----------------------------------------------------*
034400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034500080527     C*----------------------------------------------------*
034600100413     C     CHKVAB        BEGSR
034700110201     C*
034800110201     C* Se richiesti ignorare colli / peso / volume
034900110315 xxx C***                if        *in35
035000110315 xxx C***                z-add     *zeros        VABNCL
035100110315 xxx C***                z-add     *zeros        VABPKB
035200110315 xxx C***                z-add     *zeros        VABVLB
035300110315 xxx C***                endif
035400080527     C*
035500080527     C* Scrivo il VAB solo a rottura d spedizione
035600080527     C                   if        depspe = *zeros
035700080527     C  N31              eval      fivabds_sav = fivabds
035800080527     C                   eval      depspe = vabNSP
035900080527     C                   else
036000080527     C                   if        depspe = vabNSP AND *in65 = *off
036100100413     C   51              add       VABNCL        sav_VABNCL
036200100413     C   51              add       VABPKB        sav_VABPKB
036300100413     C   51              add       VABVLB        sav_VABVLB
036400100720     C   52              z-add     VABNCL        sav_VABNCL
036500100720     C   52              z-add     VABPKB        sav_VABPKB
036600100720     C   52              z-add     VABVLB        sav_VABVLB
036700100720     C   53              add       VABNCL        sav_VABNCL
036800100720     C   53              z-add     VABPKB        sav_VABPKB
036900100720     C   53              z-add     VABVLB        sav_VABVLB
037000160624
037100160624     C* PERSONALIZZAZIONE
037200160624     C* ogni rcd della spedizione equivale a un collo, poi devo sommere anche il peso
037300160624     C                   IF        *in54 = *on
037400160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
037500160624     C                   EVAL      sav_VABPKB = sav_VABPKB + VABPKB
037600160624     C                   ENDIF
037700160624
037800080527     C                   else
037900160624     C* PERSONALIZZAZIONE
038000160624     C* siccome il nr.colli lo gestisco solo a questo punto e devo aggiungere 1 per ogni rcd della
038100160624     C* spedizione, devo aggiungere 1 anche quando scrivo VAB perché non l'ho ancora fatto
038200160624     C                   IF        *in54 = *on
038300160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
038400160624     C                   ENDIF
038500080527     C  N31              eval      fivabds_ok = fivabds_sav
038600080527     C  N31              exsr      wrivab
038700080527     C  N31              eval      fivabds_sav = fivabds
038800080527     C                   eval      depspe = vabNSP
038900080527     C                   endif
039000080527     C                   endif
039100080527     C*
039200080527     C                   ENDSR
039300040119
039400040119
039500040119
039600040119     C*----------------------------------------------------*
039700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039800040119     C*----------------------------------------------------*
039900040510     C     WRIVAB        BEGSR
040000080513     C*
040100080513     C* Gestisco l'eventuale rottura x numero spedizione
040200080527     C                   if        VABNSP <> depspe or *in65 = *on
040300070301     C                   if        wVATNOT_E <> *blanks
040400130610     C                   eval      ok_VABCTM = '7Q'
040500070301     C                   endif
040600100413     C                   write     fivab000
040700080513     C                   endif
040800040119     C*
040900040119     C                   ENDSR
041000040119
041100040119
041200040119
041300040119     C*----------------------------------------------------*
041400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041500040119     C*----------------------------------------------------*
041600040510     C     WRIVAT        BEGSR
041700060512     C*
041800060512     C* Inizializzo i campi chiave
041900060512     C                   eval      VATFGS = VABFGS
042000060512     C                   eval      VATAAS = VABAAS
042100060512     C                   eval      VATCCM = VABCCM
042200070823     C                   eval      VATNRS = VABNRS
042300060512     C                   eval      VATNSP = VABNSP
042400060512     C                   eval      VATLNP = VABLNP
042500040119     C*
042600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042700040510     C                   if        VATTRC = *blanks
042800040510     C                   eval      VATTRC = 'E'
042900040510     C                   endif
043000110318     C                   if        VATNOT <> *blanks
043100040510     C                   write     FIVAT000
043200040119     C                   add       1             §CTROKVT
043300110318     C                   endif
043400060331     C*
043500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043600060331     C                   if        wVATNOT_A <> *blanks
043700060331     C                   eval      VATNOT = wVATNOT_A
043800060331     C                   eval      VATTRC = 'A'
043900060331     C                   write     FIVAT000
044000060331     C                   add       1             §CTROKVT
044100060331     C                   endif
044200060331     C*
044300060331     C                   if        wVATNOT_B <> *blanks
044400060331     C                   eval      VATNOT = wVATNOT_B
044500060331     C                   eval      VATTRC = 'B'
044600060331     C                   write     FIVAT000
044700060331     C                   add       1             §CTROKVT
044800060331     C                   endif
044900140711     C*
045000140711     C                   if        wVATNOT_IJ <> *blanks
045100140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045200140711     C                   eval      VATTRC = 'I'
045300140711     C                   write     FIVAT000
045400140711     C                   add       1             §CTROKVT
045500140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045600140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045700140711     C                   eval      VATTRC = 'J'
045800140711     C                   write     FIVAT000
045900140711     C                   endif
046000140711     C                   endif
046100060331     C*
046200150316     C                   if        wVATNOT_E <> *blanks
046300160624     C                   eval      VATNOT = wVATNOT_E
046400060331     C                   eval      VATTRC = 'E'
046500160624     C                   write     FIVAT000
046600150316     C                   endif
046700150316     C*
046800110927     C                   if        wVATNOT_I <> *blanks
046900110927     C                   eval      VATNOT = wVATNOT_I
047000140711     C                   eval      VATTRC = 'I'
047100110927     C                   write     FIVAT000
047200110927     C                   add       1             §CTROKVT
047300110927     C                   endif
047400140220     C*
047500140220     C                   if        wVATNOT_J <> *blanks
047600140220     C                   eval      VATNOT = wVATNOT_J
047700140220     C                   eval      VATTRC = 'J'
047800140220     C                   write     FIVAT000
047900140220     C                   add       1             §CTROKVT
048000140220     C                   endif
048100140319     C*
048200140319     C                   if        wVATNOT_S <> *blanks
048300140319     C                   eval      VATNOT = wVATNOT_S
048400140319     C* i 2 flag devono essere blank
048500140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
048600140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
048700140319     C                   eval      VATTRC = 'S'
048800140319     C                   write     FIVAT000
048900140319     C                   add       1             §CTROKVT
049000140319     C                   endif
049100040119     C*
049200040119     C                   ENDSR
049300040119
049400040119
049500040119
049600040119     C*----------------------------------------------------*
049700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049800040119     C*----------------------------------------------------*
049900040119     C     PREVAT        BEGSR
050000040119     C*
050100040119     C* Compongo il nome del membro da dare al FIVATWWR
050200040119     C                   eval      parmbr = vlrhdl
050300040119     C                   movel     'M'           parmbr
050400040510     C                   eval      parccm = vlrksc
050500040119     C                   eval      paropz = '1'
050600040119     C* Effettuo la chiamata al CLLE preposto
050700040119     C                   call(e)   'TITVVTC'
050800040119     C                   parm                    parccm
050900040119     C                   parm                    parmbr
051000040119     C                   parm                    paropz
051100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051200040119     C                   if        %error
051300040119     C                   movel     '1'           chkcall
051400040119     C                   else
051500040119     C                   movel     '0'           chkcall
051600040119     C                   endif
051700040119     C*
051800040119     C                   ENDSR
051900010601
052000010601
052100010601
052200010330     C*----------------------------------------------------*
052300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052400010330     C*----------------------------------------------------*
052500040119     C     INZVAR        BEGSR
052600080527     C*
052700080527     C                   setoff                                       65
052800010330     C*
052900030822     C* Inizializzo variabili di wrk
053000020204     C                   Z-ADD     *zeros        Num5_0            5 0
053100101018     C                   MOVEL     *blanks       wFlgCAS
053200060331     C                   MOVEL     *blanks       wVATNOT_A
053300060331     C                   MOVEL     *blanks       wVATNOT_B
053400060331     C                   MOVEL     *blanks       wVATNOT_E
053500110927     C                   MOVEL     *blanks       wVATNOT_I
053600140220     C                   MOVEL     *blanks       wVATNOT_J
053700140220     C                   MOVEL     *blanks       wVATNOT_IJ
053800140319     C                   MOVEL     *blanks       wVATNOT_S
053900120411     C                   MOVEL     *blanks       wNOT
054000120411     C                   MOVEL     *blanks       Data8A
054100030822     C*
054200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054300030822     C                   CLEAR                   SkSplitCSV
054400030822     C                   EVAL      i = 1
054500030822     C                   EVAL      posDa = *zeros
054600030822     C                   EVAL      posA  = *zeros
054700080527     C*
054800080527     C                   CLEAR                   FIVABDS
054900080527     C                   CLEAR                   FIVAT000
055000030822     C*
055100030822     C* Reimposto i valori di default
055200030822     C                   EXSR      DEFCAM
055300030822     C*
055400010330     C                   ENDSR
055500010330     C*----------------------------------------------------*
055600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055700010330     C*----------------------------------------------------*
055800010330     C     DEFCAM        BEGSR
055900030822     C*
056000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056100030715     C* e delimitatore testo.
056200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056500100318     C*
056600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056700100318     C                   EVAL      CharSOS = CharNUM
056800030822     C*
056900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057000100720     C                   SETOFF                                       505152
057100100720     C                   SETOFF                                       53
057200160624     C                   SETOFF                                       54
057300100413     C                   SELECT
057400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057500100720     C                   SETON                                        50
057600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057700100413     C                   SETON                                        51
057800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057900100720     C                   SETON                                        52
058000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058100100720     C                   SETON                                        53
058200160624     C* PERSONALIZZAZIONE
058300160624     C                   WHEN      %subst(vlrppt:1:1) = 'P'
058400160624     C                   SETON                                        54
058500100413     C                   ENDSL
058600030822     C*
058700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
058800030822     C                   EVAL      posDaDft = 1
058900030822     C                   EVAL      posADft  = 0
059000030822     C                   EVAL      wGiroDft = 0
059100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059200030822     C                             posDaDft > 0
059300030822     C*
059400030822     C* Gestisco il 1° giro
059500030822     C                   IF        wGiroDft = 0
059600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
059700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
059800030822     C* Incremento il contatore dei "giri"
059900030822     C                   EVAL      wGiroDft = 1
060000030822     C                   ELSE
060100030822     C                   EVAL      posDaDft = posADft
060200030822     C                   ENDIF
060300030822     C* Eseguo lo scan x trovare la fine del campo corrente
060400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060500030822     C*
060600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
060700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
060800030822     C                   IF        posDaDft > 0 AND
060900030822     C                             posADft  > 0
061000030822     C* NCL
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'NCL'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABNCL
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* CCM
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'CCM'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiInt=*on
063300030822     C                   Z-ADD     PiVal         VABCCM
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* LNP
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'LNP'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiInt=*on
064600030822     C                   Z-ADD     PiVal         VABLNP
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* NRS
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'NRS'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiInt=*on
065900030822     C                   Z-ADD     PiVal         VABNRS
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* CTR
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'CTR'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiInt=*on
067200030822     C                   Z-ADD     PiVal         VABCTR
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500030822     C* PKB
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'PKB'
068000030822     C                   EVAL      PiStr=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   EXSR      CHKNUM
068400030822     C                   IF        PiNum=*on
068500130610     C                   Z-ADD     PiVal         VABPKB
068600030822     C                   ENDIF
068700030822     C                   ENDIF
068800030822     C* VLB
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'VLB'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiNum=*on
069800130610     C                   Z-ADD     PiVal         VABVLB
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100030822     C* QFT
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'QFT'
070600030822     C                   EVAL      PiStr=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   EXSR      CHKNUM
071000030822     C                   IF        PiNum=*on
071100030822     C                   Z-ADD     PiVal         VABQFT
071200030822     C                   ENDIF
071300030822     C                   ENDIF
071400030822     C* CBO
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'CBO'
071900030822     C                   EVAL      VABCBO=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* TSP
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'TSP'
072800030822     C                   EVAL      VABTSP=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* VAS
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'VAS'
073700030822     C                   EVAL      VABVAS=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* VCA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'VCA'
074600030822     C                   EVAL      VABVCA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000070925     C* TIC
075100070925     C                   IF        %subst(
075200070925     C                             %subst(vlrppt:posDaDft+1:
075300070925     C                             posADft-posDaDft-1):1:3)
075400070925     C                             = 'TIC'
075500070925     C                   EVAL      VABTIC=%trim(%subst(
075600070925     C                             %subst(vlrppt:posDaDft+1:
075700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800070925     C                   ENDIF
075900030822     C* GCA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GCA'
076400030822     C                   EVAL      VABGCA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* CTM
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'CTM'
077300030822     C                   EVAL      VABCTM=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* FFD
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'FFD'
078200030822     C                   EVAL      VABFFD=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* VAD
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'VAD'
079100030822     C                   EVAL      VABVAD=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* GMA
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'GMA'
080000030822     C                   EVAL      VABGMA=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400030822     C* GGA
080500030822     C                   IF        %subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):1:3)
080800030822     C                             = 'GGA'
080900030822     C                   EVAL      VABGGA=%trim(%subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200030822     C                   ENDIF
081300030822     C* GVA
081400030822     C                   IF        %subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):1:3)
081700030822     C                             = 'GVA'
081800030822     C                   EVAL      VABGVA=%trim(%subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082100030822     C                   ENDIF
082200030822     C* TC1
082300030822     C                   IF        %subst(
082400030822     C                             %subst(vlrppt:posDaDft+1:
082500030822     C                             posADft-posDaDft-1):1:3)
082600030822     C                             = 'TC1'
082700030822     C                   EVAL      VABTC1=%trim(%subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083000030822     C                   ENDIF
083100030822     C* TC2
083200030822     C                   IF        %subst(
083300030822     C                             %subst(vlrppt:posDaDft+1:
083400030822     C                             posADft-posDaDft-1):1:3)
083500030822     C                             = 'TC2'
083600030822     C                   EVAL      VABTC2=%trim(%subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083900030822     C                   ENDIF
084000040714     C* VATTRC
084100040714     C                   IF        %subst(
084200040714     C                             %subst(vlrppt:posDaDft+1:
084300040714     C                             posADft-posDaDft-1):1:3)
084400040714     C                             = 'TRC'
084500040714     C                   EVAL      VATTRC=%trim(%subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084800040714     C                   ENDIF
084900030822     C* ...
085000030822     C                   ENDIF
085100030822     C                   ENDDO
085200020204     C*
085300010330     C                   ENDSR
085400010607     C*----------------------------------------------------*
085500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
085600010607     C*----------------------------------------------------*
085700010607     C     IMPVAB        BEGSR
085800010607     C*
085900010607     C                   SETOFF                                       3132
086000110315 xxx C***                SETON                                        35
086100030515     C*
086200010607     C                   MOVEL     datcor        VABAAS
086300010607     C                   MOVE      datcor        VABMGS
086400010607     C*
086500030715     C**********
086600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086700030715     C**********
086800030715     C                   IF        wGiro = *zeros
086900040113     C*
087000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087300080429     C                             %trim(intcol(5))<>*blanks
087400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087600080429     C                                      %trim(intcol(5))
087700070823     C     ';':CharCSV   XLATE     vindta        vindta
087800040510     C                   ENDIF
087900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088000031201     C     minu:maiu     XLATE     vindta        vindta
088100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088200030715     C                   DOW       posDa <= %len(%trim(vindta))
088300030715     C*
088400030715     C* Gestisco il 1° campo
088500030715     C                   IF        i = 1
088600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088700030715     C                   EVAL      posDa = 1
088800030715     C* Eseguo lo scan x trovare la fine del primo campo
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C* X i campi successivi al 1°
089400030715     C                   ELSE
089500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089600030715     C                   EVAL      posDa = posA + 1
089700030715     C* Eseguo lo scan x trovare la fine del campo corrente
089800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089900030715     C* Gestisco l'ultimo campo
090000030715     C                   IF        posA = *zeros
090100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   LEAVE
090600030715     C                   ELSE
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   ENDIF
091100030715     C                   ENDIF
091200030715     C* Incremento il contatore d campo
091300030715     C                   EVAL      i = i +1
091400030715     C                   ENDDO
091500030715     C*
091600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091700030715     C* delimitazione testo
091800030715     C                   EVAL      i = 1
091900030715     C                   DOW       i < %elem(SkSplitFLD)
092000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092200030715     C                   EVAL      i = i + 1
092300030715     C                   ENDDO
092400030715     C                   EVAL      wGiro = 1
092500030715     C                   ELSE
092600030715     C                   EVAL      wGiro = 2
092700031204     C**********
092800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092900031204     C* d separatore campo
093000031204     C**********
093100031204     C                   EVAL      vindta = %trim(vindta)
093200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093300031204     C                   EVAL      vindta = %subst(vindta:2)
093400031204     C                   ENDDO
093500031204     C*
093600031204     C                   Z-ADD     *zeros        lunghInput        4 0
093700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094100110912     C                   ENDDO
094200030715     C**********
094300030715     C* Effettuo lo split del campo dati d input
094400030715     C**********
094500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094600030715     C                   DOW       posDa <= %len(%trim(vindta))
094700030715     C*
094800030715     C* Gestisco il 1° campo
094900030715     C                   IF        i = 1
095000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095100030715     C                   EVAL      posDa = 1
095200030715     C* Eseguo lo scan x trovare la fine del primo campo
095300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C* X i campi successivi al 1°
095800030715     C                   ELSE
095900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096000030715     C                   EVAL      posDa = posA + 1
096100031204     C* Verifico che nn vi sia il campo nn valorizzato
096200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096300031204     C* Se campo nn valorizzato skippo al prossimo
096400031204     C                   EVAL      SkSplitCSV(i) = *blanks
096500031204     C                   EVAL      posA  = posA + 1
096600031204     C                   ELSE
096700030715     C* Eseguo lo scan x trovare la fine del campo corrente
096800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096900030715     C* Gestisco l'ultimo campo
097000030715     C                   IF        posA = *zeros
097100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097200030715     C* A questo punto "estraggo" il campo corrente
097300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097400030715     C                                                    (posA-posDa))
097500030715     C                   LEAVE
097600030715     C                   ELSE
097700030715     C* A questo punto "estraggo" il campo corrente
097800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097900030715     C                                                    (posA-posDa))
098000030715     C                   ENDIF
098100031204     C                   ENDIF
098200030715     C                   ENDIF
098300030715     C* Incremento il contatore d campo
098400030715     C                   EVAL      i = i +1
098500030715     C                   ENDDO
098600030715     C*
098700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098800030715     C* delimitazione testo
098900030715     C                   EVAL      i = 1
099000030715     C                   DOW       i < %elem(SkSplitCSV)
099100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099300030715     C                   EVAL      i = i + 1
099400030715     C                   ENDDO
099500100720     C*
099600100720     C* Stacco un progressivo x numero spedizione
099700150430 xxx C                   EXSR      repNSP
099800100326     C*
099900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100000030715     C                   EVAL      i = 1
100100030715     C                   DOW       i < %elem(SkSplitFLD)
100200030715     C*
100300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100500030715     C                             SkSplitCSV(i) <> *blanks
100600030721     C*** ATB
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100800030721     C                   EVAL      VABATB = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030715     C*** RSD
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101500030715     C* ==
101600130528     C                   ENDIF
101700030721     C*** RD2
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030715     C*** IND
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102300030715     C                   EVAL      VABIND = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** LOD
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
102700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** PRD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030721     C*** NZD
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030715     C*** RMA
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** NOT
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** NT2
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
104700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** VCA
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** VAS
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** FFD
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030721     C*** GC1
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GC2
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
106700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TSP
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107200030721     C                   ENDIF
107300040510     C*** CBO
107400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107600040510     C                   ENDIF
107700030721     C*** NAS
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** TIC
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GCA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
108700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** XCO
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** CTM
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** TCR
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** CTS
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** FTM
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
110700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** VAD
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GMA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** GGA
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GVA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** TC1
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
112700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** TC2
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** SCL
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** RMO
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** NMO
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*
114600030721     C* Reperisco quindi i campi numerici...
114700040510     C*** NSP
114800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114900040510     C                   EVAL      PiStr=SkSplitCSV(i)
115000040510     C                   EXSR      CHKNUM
115100040510     C                   IF        PiInt=*on
115200040510     C                   Z-ADD     PiVal         VABNSP
115300040510     C                   ELSE
115400040510     C                   SETON                                        31
115500040510     C                   EVAL      VABNSP = *zeros
115600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
115700040510     C                             + ' ' + 'VABNSP'
115800040510     C                   ENDIF
115900040510     C                   ENDIF
116000071106     C*** RMN
116100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116200071106     C                   EVAL      PiStr=SkSplitCSV(i)
116300071106     C                   EXSR      CHKNUM
116400071106     C                   IF        PiInt=*on
116500071106     C                   Z-ADD     PiVal         VABRMN
116600071106     C                   ELSE
116700071106     C                   SETON                                        32
116800071106     C                   EVAL      VABRMN = *zeros
116900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117000071106     C                             + ' ' + 'VABRMN'
117100071106     C                   ENDIF
117200071106     C                   ENDIF
117300030721     C*** CAD
117400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117500130610     C                   EVAL      PiStr=SkSplitCSV(i)
117600130610     C                   EXSR      CHKNUM
117700130610     C                   IF        PiInt=*on
117800130610     C                   Z-ADD     PiVal         Num5_0
117900130610     C                   MOVEL(p)  Num5_0        VABCAD
118000130610     C                   ELSE
118100130610     C                   SETON                                        32
118200130610     C                   EVAL      VABCAD = *zeros
118300130610     C                   EVAL      vinmsg = %trimr(vinmsg)
118400130610     C                             + ' ' + 'VABCAD'
118500130610     C                   ENDIF
118600130605     C                   ENDIF
118700030721     C*** NCL
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on
119200030721     C                   Z-ADD     PiVal         VABNCL
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      VABNCL = *zeros
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700040122     C                             + ' ' + 'VABNCL'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** PKB
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500160624     C*                  Z-ADD(H)  PiVal         VABPKB
120600160630     C                   EVAL(H)   VABPKB = PiVal
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABPKB = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABPKB'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** CAS
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
121700111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
121800160404     C                             %trim(SkSplitCSV(i)) <> '0,00'
121900130703     C                   EVAL      VABVCA = 'EUR'
122000160630     C                   EVAL      VABTIC = 'BM'
122100140513     C                   EVAL      wFlgCAS = '1'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiNum=*on
122500030721     C                   Z-ADD     PiVal         VABCAS
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      VABCAS = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABCAS'
123100030721     C                   ENDIF
123200140513     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** CCM
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on      AND
123900030721     C                             PiVal<=9999999 AND
124000030721     C                             PiVal>*zeros
124100030721     C                   Z-ADD     PiVal         VABCCM
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      VABCCM = *zeros
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600040510     C                             + ' ' + 'VABCCM'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** LNP
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiInt=*on    AND
125400030721     C                             PiVal<=999   AND
125500030721     C                             PiVal>*zeros
125600030721     C                   Z-ADD     PiVal         VABLNP
125700030721     C                   ELSE
125800040715     C                   SETON                                        32
125900030721     C                   EVAL      VABLNP = *zeros
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100040510     C                             + ' ' + 'VABLNP'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** NRS
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on AND
126900030721     C                             PiVal<=99
127000030721     C                   Z-ADD     PiVal         VABNRS
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABNRS'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** LNA
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on    AND
128200030721     C                             PiVal<=999   AND
128300030721     C                             PiVal>*zeros
128400030721     C                   Z-ADD     PiVal         VABLNA
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      VABLNA = *zeros
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABLNA'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** CTR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on  AND
129700030721     C                             PiVal<=999
129800030721     C                   Z-ADD     PiVal         VABCTR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABCTR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** IAS
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130700141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
130800141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
130900150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
131000141006     C                   EVAL      VABVAS = 'EUR'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABIAS
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABIAS'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100141003     C                   ENDIF
132200030721     C*** VLB
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiNum=*on
132700130610     C                   Z-ADD(H)  PiVal         VABVLB
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABVLB'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** QFT
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiNum=*on
133900030721     C                   Z-ADD     PiVal         VABQFT
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABQFT'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** NCD
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on      AND
135100030721     C                             PiVal<=9999999
135200030721     C                   Z-ADD     PiVal         VABNCD
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABNCD'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** NCA
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on      AND
136400030721     C                             PiVal<=9999999
136500030721     C                   Z-ADD     PiVal         VABNCA
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABNCA'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** ZNC
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on AND
137700030721     C                             PiVal<=99
137800030721     C                   Z-ADD     PiVal         VABZNC
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABZNC'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** DCR
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiInt=*on       AND
139000030721     C                             PiVal<=99999999
139100030721     C                   Z-ADD     PiVal         VABDCR
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABDCR'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** HCR
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiInt=*on   AND
140300030721     C                             PiVal<=9999
140400030721     C                   Z-ADD     PiVal         VABHCR
140500030721     C                   ELSE
140600030721     C                   SETON                                        32
140700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140800030721     C                             + ' ' + 'VABHCR'
140900030721     C                   ENDIF
141000030721     C                   ENDIF
141100030721     C*** VMD
141200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141300030721     C                   EVAL      PiStr=SkSplitCSV(i)
141400030721     C                   EXSR      CHKNUM
141500030721     C                   IF        PiNum=*on
141600030721     C                   Z-ADD     PiVal         VABVMD
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142000030721     C                             + ' ' + 'VABVMD'
142100030721     C                   ENDIF
142200030721     C                   ENDIF
142300030721     C*** ANT
142400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142500030721     C                   EVAL      PiStr=SkSplitCSV(i)
142600030721     C                   EXSR      CHKNUM
142700030721     C                   IF        PiInt=*on        AND
142800030721     C                             PiVal<=999999999
142900030721     C                   Z-ADD     PiVal         VABANT
143000030721     C                   ELSE
143100030721     C                   SETON                                        32
143200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143300030721     C                             + ' ' + 'VABANT'
143400030721     C                   ENDIF
143500030721     C                   ENDIF
143600030721     C*** CMO
143700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143800030721     C                   EVAL      PiStr=SkSplitCSV(i)
143900030721     C                   EXSR      CHKNUM
144000030721     C                   IF        PiInt=*on
144100030721     C                   Z-ADD     PiVal         Num5_0
144200030721     C                   MOVEL(p)  Num5_0        VABCMO
144300030721     C                   ELSE
144400030721     C                   SETON                                        32
144500030721     C                   EVAL      VABCMO = *zeros
144600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144700030721     C                             + ' ' + 'VABCMO'
144800030721     C                   ENDIF
144900030721     C                   ENDIF
145000040510     C*
145100040510     C* Reperisco i campi del FIVAT
145200040510     C*** VATNOT
145300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
145400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145500040510     C                   ENDIF
145600040510     C*** VATTRC
145700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145900040510     C                   ENDIF
146000060331     C*** VATNOT_A
146100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
146200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
146300060331     C                   ENDIF
146400060331     C*** VATNOT_B
146500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146700060331     C                   ENDIF
146800060331     C*** VATNOT_E
146900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
147000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147100060331     C                   ENDIF
147200110927     C*** VATNOT_I
147300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
147400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
147500110927     C                   ENDIF
147600141003     C*** VATNOT_IJ
147700141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
147800141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147900141003     C                   ENDIF
148000140319     C*** VATNOT_S
148100140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
148200140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
148300140319     C                   ENDIF
148400070823     C*
148500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148600150309     C*
148700150316
148800141003     C*
148900071029     C***  <===  -----------------------------------------
149000040510     C*
149100030715     C                   ENDIF
149200030715     C                   EVAL      i = i + 1
149300030715     C                   ENDDO
149400070925     C*
149500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149600150603
149700070925     C*
149800160404     C* pulisco la nazione se vuol essere Italia
149900160404     C                   IF        VABNZD = 'I'     OR
150000160404     C                             VABNZD = 'IT'    OR
150100160404     C                             VABNZD = 'ITA'
150200160404     C                   EVAL      VABNZD = *blank
150300160404     C                   ENDIF
150400160404     C*
150500070925     C* Considerazioni finali su CBO/CAS
150600140220     C                   IF        wFlgCAS = '1'    OR
150700140220     C                             VABCAS  > *zeros
150800140220     C                   IF        VABCBO = '1'
150900140220     C                   EVAL      VABCBO = '4'
151000140220     C                   ENDIF
151100140220     C                   IF        VABCBO = '2'
151200140220     C                   EVAL      VABCBO = '6'
151300140220     C                   ENDIF
151400140220     C                   ENDIF
151500070925     C***  <===  ----------------------------
151600040510     C*
151700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151800040510     C                   IF        VABNSP = *zeros
151900040510     C                   SETON                                        31
152000040510     C                   ENDIF
152100040715     C*
152200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152300040715     C                   IF        VABLNP = *zeros
152400040715     C                   SETON                                        31
152500040715     C                   ENDIF
152600040510     C*
152700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152800120411     C                   IF        VABRMN = *zeros
152900120411     C                   Z-ADD     VABNSP        VABRMN
153000120411     C                   ENDIF
153100120328     C* IN QUESTO CASO LO FORZO = 1
153200120411     C***                IF        VABRMN = *zeros
153300120411     C***                Z-ADD     1             VABRMN
153400120411     C***                ENDIF
153500040510     C*
153600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153700040510     C                   IF        VABRMA = *blanks
153800040714     C                   MOVEL     VABRMN        VABRMA
153900040510     C                   ENDIF
154000040128     C*
154100040510     C* Se provincia nn valorizzata la reperisco
154200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
154300110912     C                   IF        VABPRD =  *blanks AND
154400110912     C                             VABNZD =  *blanks AND
154500110912     C                             VABCAD <> *blanks AND
154600110912     C                             VABLOD <> *blanks
154700040128     C                   CLEAR                   TISI95DS
154800040128     C                   EVAL      I95TCN = '3'
154900040128     C                   Z-ADD     datcor        I95DAT
155000040128     C                   EVAL      I95NAR = VABNZD
155100040128     C                   EVAL      I95CAP = VABCAD
155200040128     C                   EVAL      I95LOC = VABLOD
155300040128     C                   CALL      'TISI95R'
155400040128     C                   PARM                    TISI95DS
155500040128     C                   EVAL      VABPRD = O95PRV
155600040510     C                   ENDIF
155700020204     C*
155800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155900020204     C                   EXSR      CHKIMPDIV
156000030715     C                   ENDIF
156100020204     C*
156200010607     C                   ENDSR
156300010607     C*----------------------------------------------------*
156400010601
156500020204
156600020204     C*----------------------------------------------------*
156700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156800020204     C*----------------------------------------------------*
156900020204     C     CHKIMPDIV     BEGSR
157000020204     C*
157100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157200020204     C                   Z-ADD     *zeros        wrkDec            9 9
157300020204     C*
157400020204     C* Come prima cosa effettuo considerazioni sulla divisa
157500020204     C                   IF        vabIAS > *zeros
157600020204     C                   IF        vabVAS <> 'EUR'
157700020204     C                   EVAL      vabVAS =  'ITL'
157800020204     C                   ENDIF
157900020204     C                   ENDIF
158000020204     C*
158100020204     C                   IF        vabCAS > *zeros
158200020204     C                   IF        vabVCA <> 'EUR'
158300020204     C                   EVAL      vabVCA =  'ITL'
158400020204     C                   ENDIF
158500020204     C                   ENDIF
158600020204     C*
158700020204     C                   IF        vabVMD > *zeros
158800020204     C                   IF        vabVAD <> 'EUR'
158900020204     C                   EVAL      vabVAD =  'ITL'
159000020204     C                   ENDIF
159100020204     C                   ENDIF
159200020204     C*
159300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
159400020204     C                   Z-ADD     vabIAS        wrkDec
159500020204     C                   IF        wrkDec > *zeros
159600020204     C                   IF        vabVAS = 'ITL'
159700020204     C                   EVAL      vabIAS = *zeros
159800020204     C                   ENDIF
159900020204     C                   ENDIF
160000020204     C*
160100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
160200020204     C                   Z-ADD     vabCAS        wrkDec
160300020204     C                   IF        wrkDec > *zeros
160400020204     C                   IF        vabVCA = 'ITL'
160500020204     C                   EVAL      vabCAS = *zeros
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
161000020204     C                   Z-ADD     vabVMD        wrkDec
161100020204     C                   IF        wrkDec > *zeros
161200020204     C                   IF        vabVAD = 'ITL'
161300020204     C                   EVAL      vabVMD = *zeros
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C                   ENDSR
161800020204     C***
161900010330
162000100318
162100100318
162200100318     C*----------------------------------------------------*
162300100318     C*  CONTROLLO NUMERICITA' CAMPI
162400100318     C*----------------------------------------------------*
162500100318     C     CHKNUM        BEGSR
162600100318     C*
162700100318     C                   IF        PiDecChr = *blanks
162800100318     C                   EVAL      PiDecChr = CharNUM
162900100318     C                   ENDIF
163000100318     C*
163100100318     C                   callp     UBISNUM_Check(PiStr
163200100318     C                                          :PiDecChr
163300100318     C                                          :PiVal
163400100318     C                                          :PiNum
163500100318     C                                          :PiInt)
163600100318     C*
163700100318     C                   ENDSR
163800100318     C***
163900010601
164000010601
164100010601
164200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
164300141006      ***
164400141014     C     repfil        BEGSR
164500141014     C*
164600141014     C                   if        invfil = *zeros and
164700141014     C                             depfil > *zeros and
164800141014     C                             (vinflg = *blanks or
164900141014     C                              vinflg = *zeros)
165000141014     C*
165100141014     C                   eval      invfil = depfil
165200141014     C                   endif
165300141014     C*
165400141014     C                   if        depfil <> invfil and
165500141014     C                             invfil > *zeros
165600141014     C                   eval      flgMulti = '1'
165700141014     C                   if        vinflg = *blanks
165800141014     C                   add       1             cntNonEl
165900141014     C                   endif
166000141014     C                   endif
166100141014     C*
166200141014     C                   if        vinflg = '2'
166300141014     C                   eval      flgStato = '2'
166400141014     C                   endif
166500141014     C*
166600141014     C                   ENDSR
166700141014     C*
166800010601
166900010601
167000010601
167100010330
167200040119      /TITLE Invio dei dati al punto operativo.
167300040119     C     invio         BEGSR
167400040119     C*
167500040119     C* 1° invio FIVAT
167600040119     C                   reset                   dscmz
167700040119     C                   move      vlrpoi        cmzdst
167800040119     C                   eval      cmzfld = 'FIVATWWR'
167900040119     C                   eval      cmzmbd = vlrhdl
168000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168100040119     C***                if        prmfir = *blanks
168200040119     C                   eval      cmzfla = 'FIVAT00F'
168300040119     C                   eval      cmzmba = 'FIVAT00F'
168400040119     C***                else
168500040119     C***                eval      cmzfla = prmfir
168600040119     C***                eval      cmzmba = prmfir
168700040119     C***                endif
168800040119     C                   eval      cmznrr = *zeros
168900040119     C                   move      §ctrokvt      cmznrr
169000040119     C                   eval      cmzlba = vlrfl1
169100040119     C                   call(e)   'TIS711C'
169200040119     C                   parm                    dscmz
169300040119     C                   parm      *blanks       esito
169400040119     C                   if        %error
169500040119     C                             or cmzerr = '1'
169600040119     C                             or esito  = '1'
169700040119     C                   eval      wrkesito = '3'
169800040119     C                   else
169900040119     C*
170000040119     C* 2° invio FIVAB
170100040119     C                   reset                   dscmz
170200040119     C                   move      vlrpoi        cmzdst
170300040119     C                   eval      cmzfld = vlrfou
170400040119     C                   eval      cmzmbd = vlrhdl
170500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170600040119     C***                if        prmfir = *blanks
170700040119     C                   eval      cmzfla = 'FIVAB00F'
170800040119     C                   eval      cmzmba = 'FIVAB00F'
170900040119     C***                else
171000040119     C***                eval      cmzfla = prmfir
171100040119     C***                eval      cmzmba = prmfir
171200040119     C***                endif
171300040119     C                   eval      cmznrr = *zeros
171400040119     C                   move      §ctrokvb      cmznrr
171500040119     C                   eval      cmzlba = vlrfl1
171600040119     C                   call(e)   'TIS711C'
171700040119     C                   parm                    dscmz
171800040119     C                   parm      *blanks       esito
171900040119     C                   if        %error
172000040119     C                             or cmzerr = '1'
172100040119     C                             or esito  = '1'
172200040119     C                   eval      wrkesito = '3'
172300040119     C                   endif
172400040119     C                   endif
172500040119     C*
172600040119     C                   ENDSR
172700040119     C***
172800010601
172900010601
173000010601
173100010601
173200010601      /TITLE Invio dei dati al punto operativo.
173300010601     C     opeini        BEGSR
173400010601     C*
173500010601     C* Inizializzo flag e contatori operativi
173600010601     C                   movel     '0'           flgGiro           1
173700010601     C                   movel     '0'           flgMulti          1
173800010601     C                   movel     '1'           flgStato          1
173900010615     C                   movel     '0'           flgOk             1
174000010601     C                   z-add     *zeros        cntNonEl         10 0
174100010601     C                   z-add     *zeros        depfil            3 0
174200010601     C                   z-add     *zeros        invfil            3 0
174300010601     C*
174400010601     C                   ENDSR
174500010601     C***
174600070326
174700070326
174800070326
174900070326
175000070326     C     *pssr         BEGSR
175100070329     C*
175200070329     C                   if        %open(tivin00r)
175300070329     C                   close     tivin00r
175400070329     C                   endif
175500070329     C                   if        %open(fivabwwr)
175600070329     C                   close     fivabwwr
175700070329     C                   endif
175800070329     C                   if        %open(fivatwwr)
175900070329     C                   close     fivatwwr
176000070329     C                   endif
176100070326     C*
176200070326     C* Effettuo la chiamata al CLLE preposto
176300070326     C                   call(e)   'TITVVTC'
176400070326     C                   parm                    parccm
176500070326     C                   parm                    parmbr
176600070326     C                   parm      '2'           paropz
176700070326     C*
176800070326     C                   eval      wrkesito = '2'
176900070404     C*
177000070404     C                   seton                                        LR
177100070326     C*
177200070326     C                   ENDSR     '*CANCL'
177300070326     C***
177400100318
177500100318
177600100318
177700100318     C     normCSV       BEGSR
177800100318     C*
177900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
178000100318     C* separatori campi
178100100318     C                   if        CharTXT <> *blanks
178200100318     C                   z-add     *zeros        wLenVINDTA        4 0
178300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178400100318     C                   setoff                                       80
178500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178800100318     C                   if        not *in80
178900100318     C                   seton                                        80
179000100318     C                   else
179100100318     C                   setoff                                       80
179200100318     C                   endif
179300100318     C                   endif
179400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179500100318     C                             and *in80 = *on
179600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179700100318     C                   endif
179800100318     C                   enddo
179900100318     C                   endif
180000100318     C*
180100100318     C                   ENDSR
180200100318     C***
180300070326
180400100326
180500100326
180600100326     C*----------------------------------------------------*
180700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180800100326     C*----------------------------------------------------*
180900100326     C     REPNSP        BEGSR
181000100326     C*
181100100326     C* NSP => Stacco un numeratore da AZNUM
181200100326     C                   clear                   TRUL33DS
181300100326     C                   eval      I33OPE = *zeros
181400100326     C                   eval      I33CNU = 302
181500100326     C                   eval      I33NUM = 1
181600100326     C                   movel     TRUL33DS      KPJBU
181700100326     C                   call      'TRUL33R'
181800100326     C                   parm                    KPJBA
181900100326     C                   movel     KPJBU         TRUL33DS
182000100326     C                   if        O33ERR = *zeros
182100100326     C                   z-add     O33NRF        VABNSP
182200100326     C                   z-add     O33NRF        VATNSP
182300100326     C                   else
182400100326     C                   SETON                                        31
182500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182600140711     C                             + ' ' + 'VABNSP VATNSP'
182700100326     C                   endif
182800100326     C*
182900100326     C                   ENDSR
183000010330
183100100326
183200010330
183300000613     C     *inzsr        BEGSR
183400990910     C*
183500990910     C     *entry        plist
183600990920     C                   parm                    tivlrds
183700990921     C                   parm      wrkesito      esito
183800000724     C                   parm                    prmlit
183900000710     C                   parm                    prmfir
184000010330     C*
184100010330     C* CALCOLA LA DATA CORRENTE
184200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
184300100318     C                   eval      datcor = %dec(%date() : *ISO)
184400000613     C*
184500000613     C                   ENDSR
184600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184700160630VABRMN;X;VABRMA;X;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;X;X;X;X;VABNCL;VABPKB;X;X;X;X;X;X;X;X;X;X;
184800160630X;VABNOT;VABCAS;
184900150428
185000150428
185100150428
