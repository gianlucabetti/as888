000100160121      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400160624      * disk C
000500160624      * - Il numero colli và calcolato considerandone 1 per ogni rcd della spedizione
000600160624      * - Il peso sul rcd è quello del singolo collo e quindi va sommato, tenendo conto che è
000700160624      *   espresso in gr.
000800160624      *
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001000130917     H decedit('0,')
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001400040119     FFIVATwwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000141003     D*------------
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040128     D tisi95ds      e ds
002900100413     D fivabds       e ds                  extname(fivab00f)
003000100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003100100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003200010330     D esito           s              1
003300010330     D prmlit          s             10
003400010330     D prmfir          s             10
003500010330     D wrkesito        s                   like(esito)
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700040510     D parccm          s              8    INZ(*blanks)
003800040119     D parmbr          s             10    INZ(*blanks)
003900040119     D paropz          s              1    INZ(*blanks)
004000040119     D chkcall         s              1    INZ(*blanks)
004100080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004200080418     D wFlgCAS         s              1    INZ(*blanks)
004300120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004400120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004500150114     D Intestazioni    s               n   inz(*on)
004600100326     D*------------------
004700100326     D* DS REPERIMENTO NUMERATORE
004800100326     D*------------------
004900100326     D trul33ds      e ds                  inz
005000100326     D*------------------
005100100326     D* DS ARCHITETTURA
005200100326     D*------------------
005300100326     D kpjba         e ds                  inz
005400130611     D*------------------
005500130611     D TRUL28DSX     e ds                  inz
005600100326
005700030715     D*------------------
005800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000030715     D SkSplitFLD      S             10    DIM(100)
006100030715     D SkSplitCSV      S            256    DIM(100)
006200030715     D CharCSV         S              1
006300030715     D CharTXT         S              1
006400030715     D CharNUM         S              1
006500100318     D CharSOS         S              1
006600130626     D posDa           S              4  0 INZ(*zeros)
006700130626     D posA            S              4  0 INZ(*zeros)
006800030715     D i               s              3  0 INZ(1)
006900030715     D wGiro           s              1  0 INZ(*zeros)
007000070823     D wVATNOT_A       s                   LIKE(VATNOT)
007100060331     D wVATNOT_B       s                   LIKE(VATNOT)
007200160624     D wVATNOT_E       s                   LIKE(VATNOT)
007300110927     D wVATNOT_I       s                   LIKE(VATNOT)
007400140220     D wVATNOT_J       s                   LIKE(VATNOT)
007500140220     D wVATNOT_IJ      s             70
007600140319     D wVATNOT_S       s                   LIKE(VATNOT)
007700140919     D wNOT            s             70
007800120411     D Data8A          s              8
007900140527     D wBarcodeN       s             11s 0
008000140527     D wX              s              3s 0
008100140711     D TxtInOut        S           2048
008200140711     D ElencoChar      S            256
008300140711     D TipoElenco      S              1
008400140711     D CharSost        S              1
008500140711     D UpperCase       S              1
008600140711     D ChkNull         S              1
008700140711     D CharNull        S              1
008800140711     D EsitoBon        S              1
008900030822     D*------------------
009000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009100030822     D*------------------
009200030822     D posDaDft        S              3  0 INZ(*zeros)
009300030822     D posADft         S              3  0 INZ(*zeros)
009400030822     D j               s              3  0 INZ(1)
009500030822     D wGiroDft        s              1  0 INZ(*zeros)
009600080418     D*------------------
009700080418     D* Costanti
009800080418     D*------------------
009900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010100100318
010200100318     D*------------------
010300100318     D* LINKING A DEFINIZIONI ESTERNE
010400100318     D*------------------
010500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010700100318
010800010330
010900010330
011000010330
011100000913     C                   reset                   rrnum
011200990921     C                   reset                   esito
011300990921     C                   reset                   wrkesito
011400010601     C*
011500010601     C                   exsr      opeini
011600010605     C                   exsr      rwvab
011700010601     C*
011800010601     C                   seton                                        lr
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012400010601     C*--------------------------------------------------------
012500010601     C     PREELA        BEGSR
012600010601     C*
012700010601     C* SE OCCORRE SPEDIRE IN FILIALE
012800010601     C                   if        invfil <> *zeros and
012900010601     C                             flgGiro = '0'
013000010601     C*
013100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013200010601     C                   eval      flgGiro = '1'
013300010601     C*
013400010601     C                   endif
013500010601     C*
013600010601     C                   ENDSR
013700010601     C***
013800010601
013900010601
014000010601
014100010601
014200010601     C*--------------------------------------------------------
014300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014400010601     C*--------------------------------------------------------
014500010601     C     ENDELA        BEGSR
014600040119     C*
014700040119     C* Effettuo la chiamata al CLLE preposto
014800040119     C                   call(e)   'TITVVTC'
014900040510     C                   parm                    parccm
015000040119     C                   parm                    parmbr
015100040119     C                   parm      '2'           paropz
015200050201     C*
015300050201     C* Effettuo lancio TISI95 solo x chiusura
015400050201     C                   CLEAR                   TISI95DS
015500050201     C                   EVAL      I95TLA = 'C'
015600050201     C                   CALL      'TISI95R'
015700050201     C                   PARM                    TISI95DS
015800000616     C*
015900010601     C                   ENDSR
016000010601
016100010601
016200010330     C*--------------------------------------------------------
016300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016400010330     C*--------------------------------------------------------
016500010605     C     RWVAB         BEGSR
016600010330     C*
016700010330     C                   if        not %open(tivin00r)
016800010330     C                   open      tivin00r
016900010330     C                   endif
017000021025     C                   if        not %open(fivabwwr)
017100021025     C                   open      fivabwwr
017200010330     C                   endif
017300040119     C*
017400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017500040119     C                   exsr      prevat
017600040119     C*
017700040119     C                   if        chkcall = '0'
017800040119     C*
017900040119     C                   if        not %open(fivatwwr)
018000040119     C                   open      fivatwwr
018100040119     C                   endif
018200010330     C*
018300040119     C                   clear                   §CTROKVB          7 0
018400040119     C                   clear                   §CTROKVT          7 0
018500040119     C                   clear                   §CTRMO            7 0
018600040119     C                   clear                   §CTRNO            7 0
018700120410     C*
018800120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018900120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019000120410     C                   IF        Intestazioni = *off
019100120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019200120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019300120410     C                   exsr      inzvar
019400120410     C                   exsr      impvab
019500120410     C                   ENDIF
019600010330     C*
019700030822     C                   do        *HIVAL
019800010330     C*
019900030822     C                   read      tivin00r                               70
020000010618     C*
020100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020200010618     C                   if        vinflg = '1'
020300010618     C                   eval      flgOk = '1'
020400010618     C                   endif
020500040510     C*
020600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020700040510     C                   z-add     *zeros        wLenVINDTA        4 0
020800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021200040510     C                   z-add     1             wFlgVINDTA
021300040510     C                   leave
021400040510     C                   endif
021500040510     C                   enddo
021600010618     C*
021700040510     C                   if        vindta > *blanks AND
021800040510     C                             wFlgVINDTA = 1
021900010330     C                   add       1             rrnum
022000010330     C*
022100010601     C                   if        *in70 = *off and
022200010330     C                             (vinflg = *blanks
022300010330     C                              or vinflg = '0'
022400010330     C                              or vinflg = '2')
022500010330     C*
022600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022700010711     C                   if        vinflg = *blanks or vinflg = '0'
022800010711     C                   clear                   vinmsg
022900010711     C                   endif
023000130917     C*
023100130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023200130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023300130917     C* evitare che vengano elisi i blank
023400140220     C***                eval      vindta  = '*' + %trim(vindta)
023500130917     C*
023600080527     C                   exsr      inzvar
023700100318     C                   exsr      normCSV
023800010605     C                   exsr      impvab
023900010601     C*
024000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024100141014     C                   eval      depfil = VABLNP
024200141014     C                   exsr      repfil
024300141014     C                   if        depfil = invfil
024400150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
024500150430     C                   if        vlrpoi = 999
024600150430     C                   move(P)   invfil        VABFGS
024700150430     C                   else
024800150430     C                   move(P)   vlrpoi        VABFGS
024900150430     C                   endif
025000010601     C*
025100010601     C                   exsr      PREELA
025200010601     C*
025300010604     C* Ebbene...
025400010604     C*
025500040119     C  N31              add       1             §CTROKVB
025600040119     C   32              add       1             §CTRMO
025700040119     C   31              add       1             §CTRNO
025800080418     C*
025900030822     C                   if        wGiro = 2
026000080513     C* Scrivo sempre il VAT
026100080513     C  N31              exsr      wrivat
026200110201     C                   exsr      chkvab
026300080513     C                   endif
026400010604     C*
026500010604     C                   if        *in31 = *off and
026600010604     C                             *in32 = *off
026700010604     C                   eval      vinflg = '1'
026800010604     C                   else
026900010604     C                   eval      vinflg = '2'
027000010604     C                   endif
027100141014     C                   endif
027200010601     C*
027300010604     C                   endif
027400010604     C*
027500010330     C                   else
027600010330     C                   eval      vinflg = '1'
027700010330     C                   endif
027800010601     C*
027900010601     C  N70              update    tivin000
028000010330     C*
028100030822     C  N70              enddo
028200040119     C*
028300080527     C* Verifico dati rimasti "in cannna"...
028400080527     C                   seton                                        65
028500110201     C                   exsr      chkvab
028600080527     C*
028700040119     C                   endif                                                  (endif - chkcall)
028800010601     C*
028900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029000010601     C                   if        cntNonEl = *zeros or
029100010601     C                             flgMulti = '0'
029200010330     C* Se non ci sono record con errori ...
029300010601     C                   if        §ctrno = 0 and
029400010604     C                             §ctrmo = 0 and
029500010601     C                             flgStato <> '2'
029600010330     C* ... restituisco esito OK.
029700010330     C                   eval      wrkesito = '0'
029800010330     C                   else
029900040119     C                   if        §ctrokvb > 0
030000010330     C                   eval      wrkesito = '1'
030100010330     C                   else
030200010615     C                   if        flgOk = '0'
030300010615     C                   eval      wrkesito = '2'
030400010615     C                   else
030500010615     C                   eval      wrkesito = '6'
030600010615     C                   endif
030700010330     C                   endif
030800010330     C                   endif
030900010601     C                   else
031000010601     C                   eval      wrkesito = '9'
031100010601     C                   endif
031200010330     C*
031300010330     C                   if        %open(tivin00r)
031400010330     C                   close     tivin00r
031500010330     C                   endif
031600021025     C                   if        %open(fivabwwr)
031700021025     C                   close     fivabwwr
031800010330     C                   endif
031900040512     C                   if        %open(fivatwwr)
032000040512     C                   close     fivatwwr
032100040512     C                   endif
032200010601     C*
032300010601     C                   if        vlrpoi <> 999
032400010601     C                   eval      invfil = vlrpoi
032500010601     C                   endif
032600010330     C*
032700040119     C                   if        §ctrokvb > 0
032800010601     C                             and invfil > *zeros
032900010330     C                   exsr      invio
033000010330     C                   endif
033100010601     C*
033200010618     C                   if        flgGiro = '1'
033300010601     C                   exsr      endela
033400010618     C                   endif
033500010330     C*
033600010330     C                   ENDSR
033700010330     C***
033800080527
033900080527
034000080527
034100080527     C*----------------------------------------------------*
034200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034300080527     C*----------------------------------------------------*
034400100413     C     CHKVAB        BEGSR
034500110201     C*
034600110201     C* Se richiesti ignorare colli / peso / volume
034700110315 xxx C***                if        *in35
034800110315 xxx C***                z-add     *zeros        VABNCL
034900110315 xxx C***                z-add     *zeros        VABPKB
035000110315 xxx C***                z-add     *zeros        VABVLB
035100110315 xxx C***                endif
035200080527     C*
035300080527     C* Scrivo il VAB solo a rottura d spedizione
035400080527     C                   if        depspe = *zeros
035500080527     C  N31              eval      fivabds_sav = fivabds
035600080527     C                   eval      depspe = vabNSP
035700080527     C                   else
035800080527     C                   if        depspe = vabNSP AND *in65 = *off
035900100413     C   51              add       VABNCL        sav_VABNCL
036000100413     C   51              add       VABPKB        sav_VABPKB
036100100413     C   51              add       VABVLB        sav_VABVLB
036200100720     C   52              z-add     VABNCL        sav_VABNCL
036300100720     C   52              z-add     VABPKB        sav_VABPKB
036400100720     C   52              z-add     VABVLB        sav_VABVLB
036500100720     C   53              add       VABNCL        sav_VABNCL
036600100720     C   53              z-add     VABPKB        sav_VABPKB
036700100720     C   53              z-add     VABVLB        sav_VABVLB
036800160624
036900160624     C* PERSONALIZZAZIONE
037000160624     C* ogni rcd della spedizione equivale a un collo, poi devo sommere anche il peso
037100160624     C                   IF        *in54 = *on
037200160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
037300160624     C                   EVAL      sav_VABPKB = sav_VABPKB + VABPKB
037400160624     C                   ENDIF
037500160624
037600080527     C                   else
037700160624     C* PERSONALIZZAZIONE
037800160624     C* siccome il nr.colli lo gestisco solo a questo punto e devo aggiungere 1 per ogni rcd della
037900160624     C* spedizione, devo aggiungere 1 anche quando scrivo VAB perché non l'ho ancora fatto
038000160624     C                   IF        *in54 = *on
038100160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
038200160624     C                   ENDIF
038300080527     C  N31              eval      fivabds_ok = fivabds_sav
038400080527     C  N31              exsr      wrivab
038500080527     C  N31              eval      fivabds_sav = fivabds
038600080527     C                   eval      depspe = vabNSP
038700080527     C                   endif
038800080527     C                   endif
038900080527     C*
039000080527     C                   ENDSR
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAB        BEGSR
039800080513     C*
039900080513     C* Gestisco l'eventuale rottura x numero spedizione
040000080527     C                   if        VABNSP <> depspe or *in65 = *on
040100070301     C                   if        wVATNOT_E <> *blanks
040200130610     C                   eval      ok_VABCTM = '7Q'
040300070301     C                   endif
040400100413     C                   write     fivab000
040500080513     C                   endif
040600040119     C*
040700040119     C                   ENDSR
040800040119
040900040119
041000040119
041100040119     C*----------------------------------------------------*
041200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041300040119     C*----------------------------------------------------*
041400040510     C     WRIVAT        BEGSR
041500060512     C*
041600060512     C* Inizializzo i campi chiave
041700060512     C                   eval      VATFGS = VABFGS
041800060512     C                   eval      VATAAS = VABAAS
041900060512     C                   eval      VATCCM = VABCCM
042000070823     C                   eval      VATNRS = VABNRS
042100060512     C                   eval      VATNSP = VABNSP
042200060512     C                   eval      VATLNP = VABLNP
042300040119     C*
042400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042500040510     C                   if        VATTRC = *blanks
042600040510     C                   eval      VATTRC = 'E'
042700040510     C                   endif
042800110318     C                   if        VATNOT <> *blanks
042900040510     C                   write     FIVAT000
043000040119     C                   add       1             §CTROKVT
043100110318     C                   endif
043200060331     C*
043300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043400060331     C                   if        wVATNOT_A <> *blanks
043500060331     C                   eval      VATNOT = wVATNOT_A
043600060331     C                   eval      VATTRC = 'A'
043700060331     C                   write     FIVAT000
043800060331     C                   add       1             §CTROKVT
043900060331     C                   endif
044000060331     C*
044100060331     C                   if        wVATNOT_B <> *blanks
044200060331     C                   eval      VATNOT = wVATNOT_B
044300060331     C                   eval      VATTRC = 'B'
044400060331     C                   write     FIVAT000
044500060331     C                   add       1             §CTROKVT
044600060331     C                   endif
044700140711     C*
044800140711     C                   if        wVATNOT_IJ <> *blanks
044900140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045000140711     C                   eval      VATTRC = 'I'
045100140711     C                   write     FIVAT000
045200140711     C                   add       1             §CTROKVT
045300140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045400140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045500140711     C                   eval      VATTRC = 'J'
045600140711     C                   write     FIVAT000
045700140711     C                   endif
045800140711     C                   endif
045900060331     C*
046000150316     C                   if        wVATNOT_E <> *blanks
046100160624     C                   eval      VATNOT = wVATNOT_E
046200060331     C                   eval      VATTRC = 'E'
046300160624     C                   write     FIVAT000
046400150316     C                   endif
046500150316     C*
046600110927     C                   if        wVATNOT_I <> *blanks
046700110927     C                   eval      VATNOT = wVATNOT_I
046800140711     C                   eval      VATTRC = 'I'
046900110927     C                   write     FIVAT000
047000110927     C                   add       1             §CTROKVT
047100110927     C                   endif
047200140220     C*
047300140220     C                   if        wVATNOT_J <> *blanks
047400140220     C                   eval      VATNOT = wVATNOT_J
047500140220     C                   eval      VATTRC = 'J'
047600140220     C                   write     FIVAT000
047700140220     C                   add       1             §CTROKVT
047800140220     C                   endif
047900140319     C*
048000140319     C                   if        wVATNOT_S <> *blanks
048100140319     C                   eval      VATNOT = wVATNOT_S
048200140319     C* i 2 flag devono essere blank
048300140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
048400140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
048500140319     C                   eval      VATTRC = 'S'
048600140319     C                   write     FIVAT000
048700140319     C                   add       1             §CTROKVT
048800140319     C                   endif
048900040119     C*
049000040119     C                   ENDSR
049100040119
049200040119
049300040119
049400040119     C*----------------------------------------------------*
049500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049600040119     C*----------------------------------------------------*
049700040119     C     PREVAT        BEGSR
049800040119     C*
049900040119     C* Compongo il nome del membro da dare al FIVATWWR
050000040119     C                   eval      parmbr = vlrhdl
050100040119     C                   movel     'M'           parmbr
050200040510     C                   eval      parccm = vlrksc
050300040119     C                   eval      paropz = '1'
050400040119     C* Effettuo la chiamata al CLLE preposto
050500040119     C                   call(e)   'TITVVTC'
050600040119     C                   parm                    parccm
050700040119     C                   parm                    parmbr
050800040119     C                   parm                    paropz
050900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051000040119     C                   if        %error
051100040119     C                   movel     '1'           chkcall
051200040119     C                   else
051300040119     C                   movel     '0'           chkcall
051400040119     C                   endif
051500040119     C*
051600040119     C                   ENDSR
051700010601
051800010601
051900010601
052000010330     C*----------------------------------------------------*
052100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052200010330     C*----------------------------------------------------*
052300040119     C     INZVAR        BEGSR
052400080527     C*
052500080527     C                   setoff                                       65
052600010330     C*
052700030822     C* Inizializzo variabili di wrk
052800020204     C                   Z-ADD     *zeros        Num5_0            5 0
052900101018     C                   MOVEL     *blanks       wFlgCAS
053000060331     C                   MOVEL     *blanks       wVATNOT_A
053100060331     C                   MOVEL     *blanks       wVATNOT_B
053200060331     C                   MOVEL     *blanks       wVATNOT_E
053300110927     C                   MOVEL     *blanks       wVATNOT_I
053400140220     C                   MOVEL     *blanks       wVATNOT_J
053500140220     C                   MOVEL     *blanks       wVATNOT_IJ
053600140319     C                   MOVEL     *blanks       wVATNOT_S
053700120411     C                   MOVEL     *blanks       wNOT
053800120411     C                   MOVEL     *blanks       Data8A
053900030822     C*
054000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054100030822     C                   CLEAR                   SkSplitCSV
054200030822     C                   EVAL      i = 1
054300030822     C                   EVAL      posDa = *zeros
054400030822     C                   EVAL      posA  = *zeros
054500080527     C*
054600080527     C                   CLEAR                   FIVABDS
054700080527     C                   CLEAR                   FIVAT000
054800030822     C*
054900030822     C* Reimposto i valori di default
055000030822     C                   EXSR      DEFCAM
055100030822     C*
055200010330     C                   ENDSR
055300010330     C*----------------------------------------------------*
055400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055500010330     C*----------------------------------------------------*
055600010330     C     DEFCAM        BEGSR
055700030822     C*
055800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055900030715     C* e delimitatore testo.
056000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056300100318     C*
056400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056500100318     C                   EVAL      CharSOS = CharNUM
056600030822     C*
056700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056800100720     C                   SETOFF                                       505152
056900100720     C                   SETOFF                                       53
057000160624     C                   SETOFF                                       54
057100100413     C                   SELECT
057200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057300100720     C                   SETON                                        50
057400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057500100413     C                   SETON                                        51
057600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057700100720     C                   SETON                                        52
057800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057900100720     C                   SETON                                        53
058000160624     C* PERSONALIZZAZIONE
058100160624     C                   WHEN      %subst(vlrppt:1:1) = 'P'
058200160624     C                   SETON                                        54
058300100413     C                   ENDSL
058400030822     C*
058500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
058600030822     C                   EVAL      posDaDft = 1
058700030822     C                   EVAL      posADft  = 0
058800030822     C                   EVAL      wGiroDft = 0
058900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059000030822     C                             posDaDft > 0
059100030822     C*
059200030822     C* Gestisco il 1° giro
059300030822     C                   IF        wGiroDft = 0
059400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
059500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
059600030822     C* Incremento il contatore dei "giri"
059700030822     C                   EVAL      wGiroDft = 1
059800030822     C                   ELSE
059900030822     C                   EVAL      posDaDft = posADft
060000030822     C                   ENDIF
060100030822     C* Eseguo lo scan x trovare la fine del campo corrente
060200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060300030822     C*
060400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
060500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
060600030822     C                   IF        posDaDft > 0 AND
060700030822     C                             posADft  > 0
060800030822     C* NCL
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'NCL'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABNCL
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CCM
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CCM'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABCCM
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* LNP
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'LNP'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiInt=*on
064400030822     C                   Z-ADD     PiVal         VABLNP
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* NRS
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'NRS'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiInt=*on
065700030822     C                   Z-ADD     PiVal         VABNRS
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* CTR
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'CTR'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiInt=*on
067000030822     C                   Z-ADD     PiVal         VABCTR
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* PKB
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'PKB'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiNum=*on
068300130610     C                   Z-ADD     PiVal         VABPKB
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* VLB
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'VLB'
069100030822     C                   EVAL      PiStr=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   EXSR      CHKNUM
069500030822     C                   IF        PiNum=*on
069600130610     C                   Z-ADD     PiVal         VABVLB
069700030822     C                   ENDIF
069800030822     C                   ENDIF
069900030822     C* QFT
070000030822     C                   IF        %subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):1:3)
070300030822     C                             = 'QFT'
070400030822     C                   EVAL      PiStr=%trim(%subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070700030822     C                   EXSR      CHKNUM
070800030822     C                   IF        PiNum=*on
070900030822     C                   Z-ADD     PiVal         VABQFT
071000030822     C                   ENDIF
071100030822     C                   ENDIF
071200030822     C* CBO
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'CBO'
071700030822     C                   EVAL      VABCBO=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* TSP
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'TSP'
072600030822     C                   EVAL      VABTSP=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* VAS
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'VAS'
073500030822     C                   EVAL      VABVAS=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* VCA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'VCA'
074400030822     C                   EVAL      VABVCA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800070925     C* TIC
074900070925     C                   IF        %subst(
075000070925     C                             %subst(vlrppt:posDaDft+1:
075100070925     C                             posADft-posDaDft-1):1:3)
075200070925     C                             = 'TIC'
075300070925     C                   EVAL      VABTIC=%trim(%subst(
075400070925     C                             %subst(vlrppt:posDaDft+1:
075500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600070925     C                   ENDIF
075700030822     C* GCA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GCA'
076200030822     C                   EVAL      VABGCA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* CTM
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'CTM'
077100030822     C                   EVAL      VABCTM=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* FFD
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'FFD'
078000030822     C                   EVAL      VABFFD=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* VAD
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'VAD'
078900030822     C                   EVAL      VABVAD=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* GMA
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'GMA'
079800030822     C                   EVAL      VABGMA=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* GGA
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'GGA'
080700030822     C                   EVAL      VABGGA=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* GVA
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'GVA'
081600030822     C                   EVAL      VABGVA=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* TC1
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'TC1'
082500030822     C                   EVAL      VABTC1=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* TC2
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'TC2'
083400030822     C                   EVAL      VABTC2=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800040714     C* VATTRC
083900040714     C                   IF        %subst(
084000040714     C                             %subst(vlrppt:posDaDft+1:
084100040714     C                             posADft-posDaDft-1):1:3)
084200040714     C                             = 'TRC'
084300040714     C                   EVAL      VATTRC=%trim(%subst(
084400040714     C                             %subst(vlrppt:posDaDft+1:
084500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600040714     C                   ENDIF
084700030822     C* ...
084800030822     C                   ENDIF
084900030822     C                   ENDDO
085000020204     C*
085100010330     C                   ENDSR
085200010607     C*----------------------------------------------------*
085300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
085400010607     C*----------------------------------------------------*
085500010607     C     IMPVAB        BEGSR
085600010607     C*
085700010607     C                   SETOFF                                       3132
085800110315 xxx C***                SETON                                        35
085900030515     C*
086000010607     C                   MOVEL     datcor        VABAAS
086100010607     C                   MOVE      datcor        VABMGS
086200010607     C*
086300030715     C**********
086400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086500030715     C**********
086600030715     C                   IF        wGiro = *zeros
086700040113     C*
086800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087100080429     C                             %trim(intcol(5))<>*blanks
087200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087400080429     C                                      %trim(intcol(5))
087500070823     C     ';':CharCSV   XLATE     vindta        vindta
087600040510     C                   ENDIF
087700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087800031201     C     minu:maiu     XLATE     vindta        vindta
087900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000030715     C                   DOW       posDa <= %len(%trim(vindta))
088100030715     C*
088200030715     C* Gestisco il 1° campo
088300030715     C                   IF        i = 1
088400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500030715     C                   EVAL      posDa = 1
088600030715     C* Eseguo lo scan x trovare la fine del primo campo
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C* X i campi successivi al 1°
089200030715     C                   ELSE
089300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400030715     C                   EVAL      posDa = posA + 1
089500030715     C* Eseguo lo scan x trovare la fine del campo corrente
089600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089700030715     C* Gestisco l'ultimo campo
089800030715     C                   IF        posA = *zeros
089900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090000030715     C* A questo punto "estraggo" il campo corrente
090100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090200030715     C                                                    (posA-posDa))
090300030715     C                   LEAVE
090400030715     C                   ELSE
090500030715     C* A questo punto "estraggo" il campo corrente
090600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090700030715     C                                                    (posA-posDa))
090800030715     C                   ENDIF
090900030715     C                   ENDIF
091000030715     C* Incremento il contatore d campo
091100030715     C                   EVAL      i = i +1
091200030715     C                   ENDDO
091300030715     C*
091400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091500030715     C* delimitazione testo
091600030715     C                   EVAL      i = 1
091700030715     C                   DOW       i < %elem(SkSplitFLD)
091800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092000030715     C                   EVAL      i = i + 1
092100030715     C                   ENDDO
092200030715     C                   EVAL      wGiro = 1
092300030715     C                   ELSE
092400030715     C                   EVAL      wGiro = 2
092500031204     C**********
092600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092700031204     C* d separatore campo
092800031204     C**********
092900031204     C                   EVAL      vindta = %trim(vindta)
093000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093100031204     C                   EVAL      vindta = %subst(vindta:2)
093200031204     C                   ENDDO
093300031204     C*
093400031204     C                   Z-ADD     *zeros        lunghInput        4 0
093500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093900110912     C                   ENDDO
094000030715     C**********
094100030715     C* Effettuo lo split del campo dati d input
094200030715     C**********
094300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094400030715     C                   DOW       posDa <= %len(%trim(vindta))
094500030715     C*
094600030715     C* Gestisco il 1° campo
094700030715     C                   IF        i = 1
094800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094900030715     C                   EVAL      posDa = 1
095000030715     C* Eseguo lo scan x trovare la fine del primo campo
095100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095200030715     C* A questo punto "estraggo" il campo corrente
095300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095400030715     C                                                    (posA-posDa))
095500030715     C* X i campi successivi al 1°
095600030715     C                   ELSE
095700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095800030715     C                   EVAL      posDa = posA + 1
095900031204     C* Verifico che nn vi sia il campo nn valorizzato
096000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096100031204     C* Se campo nn valorizzato skippo al prossimo
096200031204     C                   EVAL      SkSplitCSV(i) = *blanks
096300031204     C                   EVAL      posA  = posA + 1
096400031204     C                   ELSE
096500030715     C* Eseguo lo scan x trovare la fine del campo corrente
096600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096700030715     C* Gestisco l'ultimo campo
096800030715     C                   IF        posA = *zeros
096900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097000030715     C* A questo punto "estraggo" il campo corrente
097100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097200030715     C                                                    (posA-posDa))
097300030715     C                   LEAVE
097400030715     C                   ELSE
097500030715     C* A questo punto "estraggo" il campo corrente
097600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097700030715     C                                                    (posA-posDa))
097800030715     C                   ENDIF
097900031204     C                   ENDIF
098000030715     C                   ENDIF
098100030715     C* Incremento il contatore d campo
098200030715     C                   EVAL      i = i +1
098300030715     C                   ENDDO
098400030715     C*
098500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098600030715     C* delimitazione testo
098700030715     C                   EVAL      i = 1
098800030715     C                   DOW       i < %elem(SkSplitCSV)
098900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099100030715     C                   EVAL      i = i + 1
099200030715     C                   ENDDO
099300100720     C*
099400100720     C* Stacco un progressivo x numero spedizione
099500150430 xxx C                   EXSR      repNSP
099600100326     C*
099700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099800030715     C                   EVAL      i = 1
099900030715     C                   DOW       i < %elem(SkSplitFLD)
100000030715     C*
100100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100300030715     C                             SkSplitCSV(i) <> *blanks
100400030721     C*** ATB
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100600030721     C                   EVAL      VABATB = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030715     C*** RSD
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101300030715     C* ==
101400130528     C                   ENDIF
101500030721     C*** RD2
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030715     C*** IND
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102100030715     C                   EVAL      VABIND = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** LOD
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
102500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** PRD
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030721     C*** NZD
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030715     C*** RMA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** NOT
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** NT2
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
104500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030715     C*** VCA
104800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105000030715     C                   ENDIF
105100030715     C*** VAS
105200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105400030715     C                   ENDIF
105500030715     C*** FFD
105600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105800030715     C                   ENDIF
105900030721     C*** GC1
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GC2
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
106500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TSP
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107000030721     C                   ENDIF
107100040510     C*** CBO
107200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107400040510     C                   ENDIF
107500030721     C*** NAS
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** TIC
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** GCA
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
108500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** XCO
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** CTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** TCR
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** CTS
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** FTM
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
110500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** VAD
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** GMA
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** GGA
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** GVA
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** TC1
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
112500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** TC2
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*** SCL
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113400030721     C                   ENDIF
113500030721     C*** RMO
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113800030721     C                   ENDIF
113900030721     C*** NMO
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114200030721     C                   ENDIF
114300030721     C*
114400030721     C* Reperisco quindi i campi numerici...
114500040510     C*** NSP
114600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114700040510     C                   EVAL      PiStr=SkSplitCSV(i)
114800040510     C                   EXSR      CHKNUM
114900040510     C                   IF        PiInt=*on
115000040510     C                   Z-ADD     PiVal         VABNSP
115100040510     C                   ELSE
115200040510     C                   SETON                                        31
115300040510     C                   EVAL      VABNSP = *zeros
115400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040510     C                             + ' ' + 'VABNSP'
115600040510     C                   ENDIF
115700040510     C                   ENDIF
115800071106     C*** RMN
115900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116000071106     C                   EVAL      PiStr=SkSplitCSV(i)
116100071106     C                   EXSR      CHKNUM
116200071106     C                   IF        PiInt=*on
116300071106     C                   Z-ADD     PiVal         VABRMN
116400071106     C                   ELSE
116500071106     C                   SETON                                        32
116600071106     C                   EVAL      VABRMN = *zeros
116700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116800071106     C                             + ' ' + 'VABRMN'
116900071106     C                   ENDIF
117000071106     C                   ENDIF
117100030721     C*** CAD
117200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117300130610     C                   EVAL      PiStr=SkSplitCSV(i)
117400130610     C                   EXSR      CHKNUM
117500130610     C                   IF        PiInt=*on
117600130610     C                   Z-ADD     PiVal         Num5_0
117700130610     C                   MOVEL(p)  Num5_0        VABCAD
117800130610     C                   ELSE
117900130610     C                   SETON                                        32
118000130610     C                   EVAL      VABCAD = *zeros
118100130610     C                   EVAL      vinmsg = %trimr(vinmsg)
118200130610     C                             + ' ' + 'VABCAD'
118300130610     C                   ENDIF
118400130605     C                   ENDIF
118500030721     C*** NCL
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on
119000030721     C                   Z-ADD     PiVal         VABNCL
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      VABNCL = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040122     C                             + ' ' + 'VABNCL'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** PKB
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiNum=*on
120300160624     C*                  Z-ADD(H)  PiVal         VABPKB
120400160624     C                   EVAL(H)   VABPKB = PiVal/1000
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABPKB = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABPKB'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CAS
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
121500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
121600160404     C                             %trim(SkSplitCSV(i)) <> '0,00'
121700130703     C                   EVAL      VABVCA = 'EUR'
121800160404     C                   EVAL      VABTIC = 'BM'
121900140513     C                   EVAL      wFlgCAS = '1'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiNum=*on
122300030721     C                   Z-ADD     PiVal         VABCAS
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      VABCAS = *zeros
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABCAS'
122900030721     C                   ENDIF
123000140513     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** CCM
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on      AND
123700030721     C                             PiVal<=9999999 AND
123800030721     C                             PiVal>*zeros
123900030721     C                   Z-ADD     PiVal         VABCCM
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABCCM = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400040510     C                             + ' ' + 'VABCCM'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** LNP
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on    AND
125200030721     C                             PiVal<=999   AND
125300030721     C                             PiVal>*zeros
125400030721     C                   Z-ADD     PiVal         VABLNP
125500030721     C                   ELSE
125600040715     C                   SETON                                        32
125700030721     C                   EVAL      VABLNP = *zeros
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900040510     C                             + ' ' + 'VABLNP'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** NRS
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on AND
126700030721     C                             PiVal<=99
126800030721     C                   Z-ADD     PiVal         VABNRS
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABNRS'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** LNA
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on    AND
128000030721     C                             PiVal<=999   AND
128100030721     C                             PiVal>*zeros
128200030721     C                   Z-ADD     PiVal         VABLNA
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      VABLNA = *zeros
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABLNA'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** CTR
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on  AND
129500030721     C                             PiVal<=999
129600030721     C                   Z-ADD     PiVal         VABCTR
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABCTR'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** IAS
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130500141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
130600141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
130700150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
130800141006     C                   EVAL      VABVAS = 'EUR'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABIAS
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABIAS'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900141003     C                   ENDIF
132000030721     C*** VLB
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500130610     C                   Z-ADD(H)  PiVal         VABVLB
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABVLB'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** QFT
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABQFT
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABQFT'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** NCD
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiInt=*on      AND
134900030721     C                             PiVal<=9999999
135000030721     C                   Z-ADD     PiVal         VABNCD
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABNCD'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** NCA
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on      AND
136200030721     C                             PiVal<=9999999
136300030721     C                   Z-ADD     PiVal         VABNCA
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABNCA'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** ZNC
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on AND
137500030721     C                             PiVal<=99
137600030721     C                   Z-ADD     PiVal         VABZNC
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABZNC'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** DCR
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on       AND
138800030721     C                             PiVal<=99999999
138900030721     C                   Z-ADD     PiVal         VABDCR
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABDCR'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** HCR
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on   AND
140100030721     C                             PiVal<=9999
140200030721     C                   Z-ADD     PiVal         VABHCR
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABHCR'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** VMD
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiNum=*on
141400030721     C                   Z-ADD     PiVal         VABVMD
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABVMD'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030721     C*** ANT
142200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142300030721     C                   EVAL      PiStr=SkSplitCSV(i)
142400030721     C                   EXSR      CHKNUM
142500030721     C                   IF        PiInt=*on        AND
142600030721     C                             PiVal<=999999999
142700030721     C                   Z-ADD     PiVal         VABANT
142800030721     C                   ELSE
142900030721     C                   SETON                                        32
143000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143100030721     C                             + ' ' + 'VABANT'
143200030721     C                   ENDIF
143300030721     C                   ENDIF
143400030721     C*** CMO
143500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143600030721     C                   EVAL      PiStr=SkSplitCSV(i)
143700030721     C                   EXSR      CHKNUM
143800030721     C                   IF        PiInt=*on
143900030721     C                   Z-ADD     PiVal         Num5_0
144000030721     C                   MOVEL(p)  Num5_0        VABCMO
144100030721     C                   ELSE
144200030721     C                   SETON                                        32
144300030721     C                   EVAL      VABCMO = *zeros
144400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144500030721     C                             + ' ' + 'VABCMO'
144600030721     C                   ENDIF
144700030721     C                   ENDIF
144800040510     C*
144900040510     C* Reperisco i campi del FIVAT
145000040510     C*** VATNOT
145100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
145200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145300040510     C                   ENDIF
145400040510     C*** VATTRC
145500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145700040510     C                   ENDIF
145800060331     C*** VATNOT_A
145900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
146000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
146100060331     C                   ENDIF
146200060331     C*** VATNOT_B
146300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146500060331     C                   ENDIF
146600060331     C*** VATNOT_E
146700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146900060331     C                   ENDIF
147000110927     C*** VATNOT_I
147100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
147200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
147300110927     C                   ENDIF
147400141003     C*** VATNOT_IJ
147500141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
147600141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147700141003     C                   ENDIF
147800140319     C*** VATNOT_S
147900140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
148000140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
148100140319     C                   ENDIF
148200070823     C*
148300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148400150309     C*
148500160624     C*** VABNSP/VABRMN/VABRMA
148600160404     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
148700160404     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
148800160404     C                   EXSR      CHKNUM
148900160404     C                   IF        PiInt=*on
149000160404     C                   Z-ADD     PiVal         VABRMN
149100160404     C                   Z-ADD     PiVal         VABNSP
149200160624     C                   Z-ADD     PiVal         VATNSP
149300160624     C                   EVAL      VABRMA = %editc(VABRMN:'X')
149400160404     C                   ELSE
149500160404     C                   SETON                                        32
149600160404     C                   EVAL      VABRMN = *zeros
149700160404     C                   EVAL      VABNSP = *zeros
149800160624     C                   EVAL      VATNSP = *zeros
149900160404     C                   EVAL      vinmsg = %trimr(vinmsg)
150000160624     C                             + ' ' + 'VABRMN VABNSP VATNSP VABRMA'
150100160404     C                   ENDIF
150200160404     C                   ENDIF
150300150316
150400141003     C*
150500071029     C***  <===  -----------------------------------------
150600040510     C*
150700030715     C                   ENDIF
150800030715     C                   EVAL      i = i + 1
150900030715     C                   ENDDO
151000070925     C*
151100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151200150603
151300070925     C*
151400160404     C* pulisco la nazione se vuol essere Italia
151500160404     C                   IF        VABNZD = 'I'     OR
151600160404     C                             VABNZD = 'IT'    OR
151700160404     C                             VABNZD = 'ITA'
151800160404     C                   EVAL      VABNZD = *blank
151900160404     C                   ENDIF
152000160404     C*
152100070925     C* Considerazioni finali su CBO/CAS
152200140220     C                   IF        wFlgCAS = '1'    OR
152300140220     C                             VABCAS  > *zeros
152400140220     C                   IF        VABCBO = '1'
152500140220     C                   EVAL      VABCBO = '4'
152600140220     C                   ENDIF
152700140220     C                   IF        VABCBO = '2'
152800140220     C                   EVAL      VABCBO = '6'
152900140220     C                   ENDIF
153000140220     C                   ENDIF
153100070925     C***  <===  ----------------------------
153200040510     C*
153300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153400040510     C                   IF        VABNSP = *zeros
153500040510     C                   SETON                                        31
153600040510     C                   ENDIF
153700040715     C*
153800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153900040715     C                   IF        VABLNP = *zeros
154000040715     C                   SETON                                        31
154100040715     C                   ENDIF
154200040510     C*
154300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154400120411     C                   IF        VABRMN = *zeros
154500120411     C                   Z-ADD     VABNSP        VABRMN
154600120411     C                   ENDIF
154700120328     C* IN QUESTO CASO LO FORZO = 1
154800120411     C***                IF        VABRMN = *zeros
154900120411     C***                Z-ADD     1             VABRMN
155000120411     C***                ENDIF
155100040510     C*
155200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155300040510     C                   IF        VABRMA = *blanks
155400040714     C                   MOVEL     VABRMN        VABRMA
155500040510     C                   ENDIF
155600040128     C*
155700040510     C* Se provincia nn valorizzata la reperisco
155800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155900110912     C                   IF        VABPRD =  *blanks AND
156000110912     C                             VABNZD =  *blanks AND
156100110912     C                             VABCAD <> *blanks AND
156200110912     C                             VABLOD <> *blanks
156300040128     C                   CLEAR                   TISI95DS
156400040128     C                   EVAL      I95TCN = '3'
156500040128     C                   Z-ADD     datcor        I95DAT
156600040128     C                   EVAL      I95NAR = VABNZD
156700040128     C                   EVAL      I95CAP = VABCAD
156800040128     C                   EVAL      I95LOC = VABLOD
156900040128     C                   CALL      'TISI95R'
157000040128     C                   PARM                    TISI95DS
157100040128     C                   EVAL      VABPRD = O95PRV
157200040510     C                   ENDIF
157300020204     C*
157400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157500020204     C                   EXSR      CHKIMPDIV
157600030715     C                   ENDIF
157700020204     C*
157800010607     C                   ENDSR
157900010607     C*----------------------------------------------------*
158000010601
158100020204
158200020204     C*----------------------------------------------------*
158300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158400020204     C*----------------------------------------------------*
158500020204     C     CHKIMPDIV     BEGSR
158600020204     C*
158700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158800020204     C                   Z-ADD     *zeros        wrkDec            9 9
158900020204     C*
159000020204     C* Come prima cosa effettuo considerazioni sulla divisa
159100020204     C                   IF        vabIAS > *zeros
159200020204     C                   IF        vabVAS <> 'EUR'
159300020204     C                   EVAL      vabVAS =  'ITL'
159400020204     C                   ENDIF
159500020204     C                   ENDIF
159600020204     C*
159700020204     C                   IF        vabCAS > *zeros
159800020204     C                   IF        vabVCA <> 'EUR'
159900020204     C                   EVAL      vabVCA =  'ITL'
160000020204     C                   ENDIF
160100020204     C                   ENDIF
160200020204     C*
160300020204     C                   IF        vabVMD > *zeros
160400020204     C                   IF        vabVAD <> 'EUR'
160500020204     C                   EVAL      vabVAD =  'ITL'
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161000020204     C                   Z-ADD     vabIAS        wrkDec
161100020204     C                   IF        wrkDec > *zeros
161200020204     C                   IF        vabVAS = 'ITL'
161300020204     C                   EVAL      vabIAS = *zeros
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161800020204     C                   Z-ADD     vabCAS        wrkDec
161900020204     C                   IF        wrkDec > *zeros
162000020204     C                   IF        vabVCA = 'ITL'
162100020204     C                   EVAL      vabCAS = *zeros
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162600020204     C                   Z-ADD     vabVMD        wrkDec
162700020204     C                   IF        wrkDec > *zeros
162800020204     C                   IF        vabVAD = 'ITL'
162900020204     C                   EVAL      vabVMD = *zeros
163000020204     C                   ENDIF
163100020204     C                   ENDIF
163200020204     C*
163300020204     C                   ENDSR
163400020204     C***
163500010330
163600100318
163700100318
163800100318     C*----------------------------------------------------*
163900100318     C*  CONTROLLO NUMERICITA' CAMPI
164000100318     C*----------------------------------------------------*
164100100318     C     CHKNUM        BEGSR
164200100318     C*
164300100318     C                   IF        PiDecChr = *blanks
164400100318     C                   EVAL      PiDecChr = CharNUM
164500100318     C                   ENDIF
164600100318     C*
164700100318     C                   callp     UBISNUM_Check(PiStr
164800100318     C                                          :PiDecChr
164900100318     C                                          :PiVal
165000100318     C                                          :PiNum
165100100318     C                                          :PiInt)
165200100318     C*
165300100318     C                   ENDSR
165400100318     C***
165500010601
165600010601
165700010601
165800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165900141006      ***
166000141014     C     repfil        BEGSR
166100141014     C*
166200141014     C                   if        invfil = *zeros and
166300141014     C                             depfil > *zeros and
166400141014     C                             (vinflg = *blanks or
166500141014     C                              vinflg = *zeros)
166600141014     C*
166700141014     C                   eval      invfil = depfil
166800141014     C                   endif
166900141014     C*
167000141014     C                   if        depfil <> invfil and
167100141014     C                             invfil > *zeros
167200141014     C                   eval      flgMulti = '1'
167300141014     C                   if        vinflg = *blanks
167400141014     C                   add       1             cntNonEl
167500141014     C                   endif
167600141014     C                   endif
167700141014     C*
167800141014     C                   if        vinflg = '2'
167900141014     C                   eval      flgStato = '2'
168000141014     C                   endif
168100141014     C*
168200141014     C                   ENDSR
168300141014     C*
168400010601
168500010601
168600010601
168700010330
168800040119      /TITLE Invio dei dati al punto operativo.
168900040119     C     invio         BEGSR
169000040119     C*
169100040119     C* 1° invio FIVAT
169200040119     C                   reset                   dscmz
169300040119     C                   move      vlrpoi        cmzdst
169400040119     C                   eval      cmzfld = 'FIVATWWR'
169500040119     C                   eval      cmzmbd = vlrhdl
169600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169700040119     C***                if        prmfir = *blanks
169800040119     C                   eval      cmzfla = 'FIVAT00F'
169900040119     C                   eval      cmzmba = 'FIVAT00F'
170000040119     C***                else
170100040119     C***                eval      cmzfla = prmfir
170200040119     C***                eval      cmzmba = prmfir
170300040119     C***                endif
170400040119     C                   eval      cmznrr = *zeros
170500040119     C                   move      §ctrokvt      cmznrr
170600040119     C                   eval      cmzlba = vlrfl1
170700040119     C                   call(e)   'TIS711C'
170800040119     C                   parm                    dscmz
170900040119     C                   parm      *blanks       esito
171000040119     C                   if        %error
171100040119     C                             or cmzerr = '1'
171200040119     C                             or esito  = '1'
171300040119     C                   eval      wrkesito = '3'
171400040119     C                   else
171500040119     C*
171600040119     C* 2° invio FIVAB
171700040119     C                   reset                   dscmz
171800040119     C                   move      vlrpoi        cmzdst
171900040119     C                   eval      cmzfld = vlrfou
172000040119     C                   eval      cmzmbd = vlrhdl
172100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172200040119     C***                if        prmfir = *blanks
172300040119     C                   eval      cmzfla = 'FIVAB00F'
172400040119     C                   eval      cmzmba = 'FIVAB00F'
172500040119     C***                else
172600040119     C***                eval      cmzfla = prmfir
172700040119     C***                eval      cmzmba = prmfir
172800040119     C***                endif
172900040119     C                   eval      cmznrr = *zeros
173000040119     C                   move      §ctrokvb      cmznrr
173100040119     C                   eval      cmzlba = vlrfl1
173200040119     C                   call(e)   'TIS711C'
173300040119     C                   parm                    dscmz
173400040119     C                   parm      *blanks       esito
173500040119     C                   if        %error
173600040119     C                             or cmzerr = '1'
173700040119     C                             or esito  = '1'
173800040119     C                   eval      wrkesito = '3'
173900040119     C                   endif
174000040119     C                   endif
174100040119     C*
174200040119     C                   ENDSR
174300040119     C***
174400010601
174500010601
174600010601
174700010601
174800010601      /TITLE Invio dei dati al punto operativo.
174900010601     C     opeini        BEGSR
175000010601     C*
175100010601     C* Inizializzo flag e contatori operativi
175200010601     C                   movel     '0'           flgGiro           1
175300010601     C                   movel     '0'           flgMulti          1
175400010601     C                   movel     '1'           flgStato          1
175500010615     C                   movel     '0'           flgOk             1
175600010601     C                   z-add     *zeros        cntNonEl         10 0
175700010601     C                   z-add     *zeros        depfil            3 0
175800010601     C                   z-add     *zeros        invfil            3 0
175900010601     C*
176000010601     C                   ENDSR
176100010601     C***
176200070326
176300070326
176400070326
176500070326
176600070326     C     *pssr         BEGSR
176700070329     C*
176800070329     C                   if        %open(tivin00r)
176900070329     C                   close     tivin00r
177000070329     C                   endif
177100070329     C                   if        %open(fivabwwr)
177200070329     C                   close     fivabwwr
177300070329     C                   endif
177400070329     C                   if        %open(fivatwwr)
177500070329     C                   close     fivatwwr
177600070329     C                   endif
177700070326     C*
177800070326     C* Effettuo la chiamata al CLLE preposto
177900070326     C                   call(e)   'TITVVTC'
178000070326     C                   parm                    parccm
178100070326     C                   parm                    parmbr
178200070326     C                   parm      '2'           paropz
178300070326     C*
178400070326     C                   eval      wrkesito = '2'
178500070404     C*
178600070404     C                   seton                                        LR
178700070326     C*
178800070326     C                   ENDSR     '*CANCL'
178900070326     C***
179000100318
179100100318
179200100318
179300100318     C     normCSV       BEGSR
179400100318     C*
179500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179600100318     C* separatori campi
179700100318     C                   if        CharTXT <> *blanks
179800100318     C                   z-add     *zeros        wLenVINDTA        4 0
179900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180000100318     C                   setoff                                       80
180100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180400100318     C                   if        not *in80
180500100318     C                   seton                                        80
180600100318     C                   else
180700100318     C                   setoff                                       80
180800100318     C                   endif
180900100318     C                   endif
181000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181100100318     C                             and *in80 = *on
181200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181300100318     C                   endif
181400100318     C                   enddo
181500100318     C                   endif
181600100318     C*
181700100318     C                   ENDSR
181800100318     C***
181900070326
182000100326
182100100326
182200100326     C*----------------------------------------------------*
182300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182400100326     C*----------------------------------------------------*
182500100326     C     REPNSP        BEGSR
182600100326     C*
182700100326     C* NSP => Stacco un numeratore da AZNUM
182800100326     C                   clear                   TRUL33DS
182900100326     C                   eval      I33OPE = *zeros
183000100326     C                   eval      I33CNU = 302
183100100326     C                   eval      I33NUM = 1
183200100326     C                   movel     TRUL33DS      KPJBU
183300100326     C                   call      'TRUL33R'
183400100326     C                   parm                    KPJBA
183500100326     C                   movel     KPJBU         TRUL33DS
183600100326     C                   if        O33ERR = *zeros
183700100326     C                   z-add     O33NRF        VABNSP
183800100326     C                   z-add     O33NRF        VATNSP
183900100326     C                   else
184000100326     C                   SETON                                        31
184100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184200140711     C                             + ' ' + 'VABNSP VATNSP'
184300100326     C                   endif
184400100326     C*
184500100326     C                   ENDSR
184600010330
184700100326
184800010330
184900000613     C     *inzsr        BEGSR
185000990910     C*
185100990910     C     *entry        plist
185200990920     C                   parm                    tivlrds
185300990921     C                   parm      wrkesito      esito
185400000724     C                   parm                    prmlit
185500000710     C                   parm                    prmfir
185600010330     C*
185700010330     C* CALCOLA LA DATA CORRENTE
185800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185900100318     C                   eval      datcor = %dec(%date() : *ISO)
186000000613     C*
186100000613     C                   ENDSR
186200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186300160624X;W_VABRMN;X;VABRSD;VABIND;VABRD2;VABCAD;VABLOD;VATNOT_E;X;VABPKB;
186400160624
186500150428
186600150428
186700150428
