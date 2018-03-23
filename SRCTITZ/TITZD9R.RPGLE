000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400140930      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600130917     H decedit('0,')
000700120209     F*
000800990910     Ftivin00r  uF   E             DISK    usropn
000900100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001000040119     FFIVATwwr  O    E             DISK    usropn
001100000313     D*
001200040113     D*------------
001300040113     D* SCHIEDE A PROGRAMMA
001400040113     D*------------
001500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001600141003     D*------------
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400040128     D tisi95ds      e ds
002500100413     D fivabds       e ds                  extname(fivab00f)
002600100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002700100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300040510     D parccm          s              8    INZ(*blanks)
003400040119     D parmbr          s             10    INZ(*blanks)
003500040119     D paropz          s              1    INZ(*blanks)
003600040119     D chkcall         s              1    INZ(*blanks)
003700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003800080418     D wFlgCAS         s              1    INZ(*blanks)
003900120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004000120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004100141014     D Intestazioni    s               n   inz(*off)
004200100326     D*------------------
004300100326     D* DS REPERIMENTO NUMERATORE
004400100326     D*------------------
004500100326     D trul33ds      e ds                  inz
004600100326     D*------------------
004700100326     D* DS ARCHITETTURA
004800100326     D*------------------
004900100326     D kpjba         e ds                  inz
005000130611     D*------------------
005100130611     D TRUL28DSX     e ds                  inz
005200100326
005300030715     D*------------------
005400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005500030715     D*------------------
005600030715     D SkSplitFLD      S             10    DIM(100)
005700030715     D SkSplitCSV      S            256    DIM(100)
005800030715     D CharCSV         S              1
005900030715     D CharTXT         S              1
006000030715     D CharNUM         S              1
006100100318     D CharSOS         S              1
006200130626     D posDa           S              4  0 INZ(*zeros)
006300130626     D posA            S              4  0 INZ(*zeros)
006400030715     D i               s              3  0 INZ(1)
006500030715     D wGiro           s              1  0 INZ(*zeros)
006600070823     D wVATNOT_A       s                   LIKE(VATNOT)
006700060331     D wVATNOT_B       s                   LIKE(VATNOT)
006800060331     D wVATNOT_E       s                   LIKE(VATNOT)
006900110927     D wVATNOT_I       s                   LIKE(VATNOT)
007000140220     D wVATNOT_J       s                   LIKE(VATNOT)
007100140220     D wVATNOT_IJ      s             70
007200140319     D wVATNOT_S       s                   LIKE(VATNOT)
007300140919     D w70             s             70
007400141006     D w11             s             11
007500140919     D wNOT            s             70
007600140919     D wRSD1           S            256
007700140919     D wRSD2           S            256
007800140919     D wRSD3           S            256
007900120411     D Data8A          s              8
008000140527     D wBarcodeN       s             11s 0
008100140527     D wX              s              3s 0
008200140711     D wRMA            s                   like(VABRMA)
008300140711     D TxtInOut        S           2048
008400140711     D ElencoChar      S            256
008500140711     D TipoElenco      S              1
008600140711     D CharSost        S              1
008700140711     D UpperCase       S              1
008800140711     D ChkNull         S              1
008900140711     D CharNull        S              1
009000140711     D EsitoBon        S              1
009100030822     D*------------------
009200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009300030822     D*------------------
009400030822     D posDaDft        S              3  0 INZ(*zeros)
009500030822     D posADft         S              3  0 INZ(*zeros)
009600030822     D j               s              3  0 INZ(1)
009700030822     D wGiroDft        s              1  0 INZ(*zeros)
009800080418     D*------------------
009900080418     D* Costanti
010000080418     D*------------------
010100080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010200080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010300100318
010400100318     D*------------------
010500100318     D* LINKING A DEFINIZIONI ESTERNE
010600100318     D*------------------
010700100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010800100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010900100318
011000010330
011100010330
011200010330
011300000913     C                   reset                   rrnum
011400990921     C                   reset                   esito
011500990921     C                   reset                   wrkesito
011600010601     C*
011700010601     C                   exsr      opeini
011800010605     C                   exsr      rwvab
011900010601     C*
012000010601     C                   seton                                        lr
012100010601
012200010601
012300010601
012400010601     C*--------------------------------------------------------
012500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012600010601     C*--------------------------------------------------------
012700010601     C     PREELA        BEGSR
012800010601     C*
012900010601     C* SE OCCORRE SPEDIRE IN FILIALE
013000010601     C                   if        invfil <> *zeros and
013100010601     C                             flgGiro = '0'
013200010601     C*
013300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013400010601     C                   eval      flgGiro = '1'
013500010601     C*
013600010601     C                   endif
013700010601     C*
013800010601     C                   ENDSR
013900010601     C***
014000010601
014100010601
014200010601
014300010601
014400010601     C*--------------------------------------------------------
014500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014600010601     C*--------------------------------------------------------
014700010601     C     ENDELA        BEGSR
014800040119     C*
014900040119     C* Effettuo la chiamata al CLLE preposto
015000040119     C                   call(e)   'TITVVTC'
015100040510     C                   parm                    parccm
015200040119     C                   parm                    parmbr
015300040119     C                   parm      '2'           paropz
015400050201     C*
015500050201     C* Effettuo lancio TISI95 solo x chiusura
015600050201     C                   CLEAR                   TISI95DS
015700050201     C                   EVAL      I95TLA = 'C'
015800050201     C                   CALL      'TISI95R'
015900050201     C                   PARM                    TISI95DS
016000000616     C*
016100010601     C                   ENDSR
016200010601
016300010601
016400010330     C*--------------------------------------------------------
016500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016600010330     C*--------------------------------------------------------
016700010605     C     RWVAB         BEGSR
016800010330     C*
016900010330     C                   if        not %open(tivin00r)
017000010330     C                   open      tivin00r
017100010330     C                   endif
017200021025     C                   if        not %open(fivabwwr)
017300021025     C                   open      fivabwwr
017400010330     C                   endif
017500040119     C*
017600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017700040119     C                   exsr      prevat
017800040119     C*
017900040119     C                   if        chkcall = '0'
018000040119     C*
018100040119     C                   if        not %open(fivatwwr)
018200040119     C                   open      fivatwwr
018300040119     C                   endif
018400010330     C*
018500040119     C                   clear                   §CTROKVB          7 0
018600040119     C                   clear                   §CTROKVT          7 0
018700040119     C                   clear                   §CTRMO            7 0
018800040119     C                   clear                   §CTRNO            7 0
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
024600141014     C                   if        vlrpoi = 999
024700141014     C                   move(P)   invfil        VABFGS
024800141014     C                   else
024900141014     C                   move(P)   vlrpoi        VABFGS
025000141014     C                   endif
025100010601     C*
025200010601     C                   exsr      PREELA
025300010601     C*
025400010604     C* Ebbene...
025500010604     C*
025600040119     C  N31              add       1             §CTROKVB
025700040119     C   32              add       1             §CTRMO
025800040119     C   31              add       1             §CTRNO
025900080418     C*
026000030822     C                   if        wGiro = 2
026100080513     C* Scrivo sempre il VAT
026200080513     C  N31              exsr      wrivat
026300110201     C                   exsr      chkvab
026400080513     C                   endif
026500010604     C*
026600010604     C                   if        *in31 = *off and
026700010604     C                             *in32 = *off
026800010604     C                   eval      vinflg = '1'
026900010604     C                   else
027000010604     C                   eval      vinflg = '2'
027100010604     C                   endif
027200141014     C                   endif
027300010601     C*
027400010604     C                   endif
027500010604     C*
027600010330     C                   else
027700010330     C                   eval      vinflg = '1'
027800010330     C                   endif
027900010601     C*
028000010601     C  N70              update    tivin000
028100010330     C*
028200030822     C  N70              enddo
028300040119     C*
028400080527     C* Verifico dati rimasti "in cannna"...
028500080527     C                   seton                                        65
028600110201     C                   exsr      chkvab
028700080527     C*
028800040119     C                   endif                                                  (endif - chkcall)
028900010601     C*
029000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029100010601     C                   if        cntNonEl = *zeros or
029200010601     C                             flgMulti = '0'
029300010330     C* Se non ci sono record con errori ...
029400010601     C                   if        §ctrno = 0 and
029500010604     C                             §ctrmo = 0 and
029600010601     C                             flgStato <> '2'
029700010330     C* ... restituisco esito OK.
029800010330     C                   eval      wrkesito = '0'
029900010330     C                   else
030000040119     C                   if        §ctrokvb > 0
030100010330     C                   eval      wrkesito = '1'
030200010330     C                   else
030300010615     C                   if        flgOk = '0'
030400010615     C                   eval      wrkesito = '2'
030500010615     C                   else
030600010615     C                   eval      wrkesito = '6'
030700010615     C                   endif
030800010330     C                   endif
030900010330     C                   endif
031000010601     C                   else
031100010601     C                   eval      wrkesito = '9'
031200010601     C                   endif
031300010330     C*
031400010330     C                   if        %open(tivin00r)
031500010330     C                   close     tivin00r
031600010330     C                   endif
031700021025     C                   if        %open(fivabwwr)
031800021025     C                   close     fivabwwr
031900010330     C                   endif
032000040512     C                   if        %open(fivatwwr)
032100040512     C                   close     fivatwwr
032200040512     C                   endif
032300010601     C*
032400010601     C                   if        vlrpoi <> 999
032500010601     C                   eval      invfil = vlrpoi
032600010601     C                   endif
032700010330     C*
032800040119     C                   if        §ctrokvb > 0
032900010601     C                             and invfil > *zeros
033000010330     C                   exsr      invio
033100010330     C                   endif
033200010601     C*
033300010618     C                   if        flgGiro = '1'
033400010601     C                   exsr      endela
033500010618     C                   endif
033600010330     C*
033700010330     C                   ENDSR
033800010330     C***
033900080527
034000080527
034100080527
034200080527     C*----------------------------------------------------*
034300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034400080527     C*----------------------------------------------------*
034500100413     C     CHKVAB        BEGSR
034600110201     C*
034700110201     C* Se richiesti ignorare colli / peso / volume
034800110315 xxx C***                if        *in35
034900110315 xxx C***                z-add     *zeros        VABNCL
035000110315 xxx C***                z-add     *zeros        VABPKB
035100110315 xxx C***                z-add     *zeros        VABVLB
035200110315 xxx C***                endif
035300080527     C*
035400080527     C* Scrivo il VAB solo a rottura d spedizione
035500080527     C                   if        depspe = *zeros
035600140930
035700140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
035800140930      * spedizione
035900140930     C  N31              eval      vabncl = 1
036000140930
036100080527     C  N31              eval      fivabds_sav = fivabds
036200080527     C                   eval      depspe = vabNSP
036300080527     C                   else
036400080527     C                   if        depspe = vabNSP AND *in65 = *off
036500140930
036600140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036700140930      * spedizione
036800140930     C                   eval      vabncl += 1
036900140930
037000100413     C   51              add       VABNCL        sav_VABNCL
037100100413     C   51              add       VABPKB        sav_VABPKB
037200100413     C   51              add       VABVLB        sav_VABVLB
037300100720     C   52              z-add     VABNCL        sav_VABNCL
037400100720     C   52              z-add     VABPKB        sav_VABPKB
037500100720     C   52              z-add     VABVLB        sav_VABVLB
037600100720     C   53              add       VABNCL        sav_VABNCL
037700100720     C   53              z-add     VABPKB        sav_VABPKB
037800100720     C   53              z-add     VABVLB        sav_VABVLB
037900080527     C                   else
038000080527     C  N31              eval      fivabds_ok = fivabds_sav
038100080527     C  N31              exsr      wrivab
038200080527     C  N31              eval      fivabds_sav = fivabds
038300140930
038400140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
038500140930      * spedizione
038600140930     C  N31              eval      vabncl = 1
038700140930
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
040300150908     C                   if        wVATNOT_E <> *blanks AND ok_VABNZD = *blanks
040400130610     C                   eval      ok_VABCTM = '7Q'
040500070301     C                   endif
040600150908     C                   if        wVATNOT_E <> *blanks AND ok_VABNZD<> *blanks
040700150908     C                   eval      ok_VABCTM = '7R'
040800150908     C                   endif
040900100413     C                   write     fivab000
041000080513     C                   endif
041100040119     C*
041200040119     C                   ENDSR
041300040119
041400040119
041500040119
041600040119     C*----------------------------------------------------*
041700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041800040119     C*----------------------------------------------------*
041900040510     C     WRIVAT        BEGSR
042000060512     C*
042100060512     C* Inizializzo i campi chiave
042200060512     C                   eval      VATFGS = VABFGS
042300060512     C                   eval      VATAAS = VABAAS
042400060512     C                   eval      VATCCM = VABCCM
042500070823     C                   eval      VATNRS = VABNRS
042600060512     C                   eval      VATNSP = VABNSP
042700060512     C                   eval      VATLNP = VABLNP
042800040119     C*
042900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043000040510     C                   if        VATTRC = *blanks
043100040510     C                   eval      VATTRC = 'E'
043200040510     C                   endif
043300110318     C                   if        VATNOT <> *blanks
043400040510     C                   write     FIVAT000
043500040119     C                   add       1             §CTROKVT
043600110318     C                   endif
043700060331     C*
043800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043900060331     C                   if        wVATNOT_A <> *blanks
044000060331     C                   eval      VATNOT = wVATNOT_A
044100060331     C                   eval      VATTRC = 'A'
044200060331     C                   write     FIVAT000
044300060331     C                   add       1             §CTROKVT
044400060331     C                   endif
044500060331     C*
044600060331     C                   if        wVATNOT_B <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_B
044800060331     C                   eval      VATTRC = 'B'
044900060331     C                   write     FIVAT000
045000060331     C                   add       1             §CTROKVT
045100060331     C                   endif
045200140711     C*
045300140711     C                   if        wVATNOT_IJ <> *blanks
045400140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045500140711     C                   eval      VATTRC = 'I'
045600140711     C                   write     FIVAT000
045700140711     C                   add       1             §CTROKVT
045800140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045900140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046000140711     C                   eval      VATTRC = 'J'
046100140711     C                   write     FIVAT000
046200140711     C                   endif
046300140711     C                   endif
046400060331     C*
046500060331     C                   if        wVATNOT_E <> *blanks
046600060331     C                   eval      VATTRC = 'E'
046700140527     C*
046800140711     C* NON Sviluppare i "CHI SONO"
046900140527     C*
047000140711     C                   EVAL      VATNOT = wVATNOT_E
047100140711     C                   write     FIVAT000
047200140711     C                   add       1             §CTROKVT
047300120320     C                   endif
047400110927     C*
047500110927     C                   if        wVATNOT_I <> *blanks
047600110927     C                   eval      VATNOT = wVATNOT_I
047700140711     C                   eval      VATTRC = 'I'
047800110927     C                   write     FIVAT000
047900110927     C                   add       1             §CTROKVT
048000110927     C                   endif
048100140220     C*
048200140220     C                   if        wVATNOT_J <> *blanks
048300140220     C                   eval      VATNOT = wVATNOT_J
048400140220     C                   eval      VATTRC = 'J'
048500140220     C                   write     FIVAT000
048600140220     C                   add       1             §CTROKVT
048700140220     C                   endif
048800140319     C*
048900140319     C                   if        wVATNOT_S <> *blanks
049000140319     C                   eval      VATNOT = wVATNOT_S
049100140319     C* i 2 flag devono essere blank
049200140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049300140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049400140319     C                   eval      VATTRC = 'S'
049500140319     C                   write     FIVAT000
049600140319     C                   add       1             §CTROKVT
049700140319     C                   endif
049800040119     C*
049900040119     C                   ENDSR
050000040119
050100040119
050200040119
050300040119     C*----------------------------------------------------*
050400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050500040119     C*----------------------------------------------------*
050600040119     C     PREVAT        BEGSR
050700040119     C*
050800040119     C* Compongo il nome del membro da dare al FIVATWWR
050900040119     C                   eval      parmbr = vlrhdl
051000040119     C                   movel     'M'           parmbr
051100040510     C                   eval      parccm = vlrksc
051200040119     C                   eval      paropz = '1'
051300040119     C* Effettuo la chiamata al CLLE preposto
051400040119     C                   call(e)   'TITVVTC'
051500040119     C                   parm                    parccm
051600040119     C                   parm                    parmbr
051700040119     C                   parm                    paropz
051800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051900040119     C                   if        %error
052000040119     C                   movel     '1'           chkcall
052100040119     C                   else
052200040119     C                   movel     '0'           chkcall
052300040119     C                   endif
052400040119     C*
052500040119     C                   ENDSR
052600010601
052700010601
052800010601
052900010330     C*----------------------------------------------------*
053000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053100010330     C*----------------------------------------------------*
053200040119     C     INZVAR        BEGSR
053300080527     C*
053400080527     C                   setoff                                       65
053500010330     C*
053600030822     C* Inizializzo variabili di wrk
053700020204     C                   Z-ADD     *zeros        Num5_0            5 0
053800101018     C                   MOVEL     *blanks       wFlgCAS
053900060331     C                   MOVEL     *blanks       wVATNOT_A
054000060331     C                   MOVEL     *blanks       wVATNOT_B
054100060331     C                   MOVEL     *blanks       wVATNOT_E
054200110927     C                   MOVEL     *blanks       wVATNOT_I
054300140220     C                   MOVEL     *blanks       wVATNOT_J
054400140220     C                   MOVEL     *blanks       wVATNOT_IJ
054500140319     C                   MOVEL     *blanks       wVATNOT_S
054600120411     C                   MOVEL     *blanks       wNOT
054700120411     C                   MOVEL     *blanks       Data8A
054800030822     C*
054900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055000030822     C                   CLEAR                   SkSplitCSV
055100030822     C                   EVAL      i = 1
055200030822     C                   EVAL      posDa = *zeros
055300030822     C                   EVAL      posA  = *zeros
055400080527     C*
055500080527     C                   CLEAR                   FIVABDS
055600080527     C                   CLEAR                   FIVAT000
055700030822     C*
055800030822     C* Reimposto i valori di default
055900030822     C                   EXSR      DEFCAM
056000030822     C*
056100010330     C                   ENDSR
056200010330     C*----------------------------------------------------*
056300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056400010330     C*----------------------------------------------------*
056500010330     C     DEFCAM        BEGSR
056600030822     C*
056700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056800030715     C* e delimitatore testo.
056900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057200100318     C*
057300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057400100318     C                   EVAL      CharSOS = CharNUM
057500030822     C*
057600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057700100720     C                   SETOFF                                       505152
057800100720     C                   SETOFF                                       53
057900100413     C                   SELECT
058000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058100100720     C                   SETON                                        50
058200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058300100413     C                   SETON                                        51
058400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058500100720     C                   SETON                                        52
058600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058700100720     C                   SETON                                        53
058800100413     C                   ENDSL
058900030822     C*
059000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059100030822     C                   EVAL      posDaDft = 1
059200030822     C                   EVAL      posADft  = 0
059300030822     C                   EVAL      wGiroDft = 0
059400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059500030822     C                             posDaDft > 0
059600030822     C*
059700030822     C* Gestisco il 1° giro
059800030822     C                   IF        wGiroDft = 0
059900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060100030822     C* Incremento il contatore dei "giri"
060200030822     C                   EVAL      wGiroDft = 1
060300030822     C                   ELSE
060400030822     C                   EVAL      posDaDft = posADft
060500030822     C                   ENDIF
060600030822     C* Eseguo lo scan x trovare la fine del campo corrente
060700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060800030822     C*
060900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061100030822     C                   IF        posDaDft > 0 AND
061200030822     C                             posADft  > 0
061300030822     C* NCL
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'NCL'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABNCL
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* CCM
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'CCM'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABCCM
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* LNP
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'LNP'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABLNP
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* NRS
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'NRS'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiInt=*on
066200030822     C                   Z-ADD     PiVal         VABNRS
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* CTR
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'CTR'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiInt=*on
067500030822     C                   Z-ADD     PiVal         VABCTR
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* PKB
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'PKB'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiNum=*on
068800130610     C                   Z-ADD     PiVal         VABPKB
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* VLB
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VLB'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiNum=*on
070100130610     C                   Z-ADD     PiVal         VABVLB
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400030822     C* QFT
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'QFT'
070900030822     C                   EVAL      PiStr=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   EXSR      CHKNUM
071300030822     C                   IF        PiNum=*on
071400030822     C                   Z-ADD     PiVal         VABQFT
071500030822     C                   ENDIF
071600030822     C                   ENDIF
071700030822     C* CBO
071800030822     C                   IF        %subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):1:3)
072100030822     C                             = 'CBO'
072200030822     C                   EVAL      VABCBO=%trim(%subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072500030822     C                   ENDIF
072600030822     C* TSP
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'TSP'
073100030822     C                   EVAL      VABTSP=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* VAS
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'VAS'
074000030822     C                   EVAL      VABVAS=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* VCA
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'VCA'
074900030822     C                   EVAL      VABVCA=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300070925     C* TIC
075400070925     C                   IF        %subst(
075500070925     C                             %subst(vlrppt:posDaDft+1:
075600070925     C                             posADft-posDaDft-1):1:3)
075700070925     C                             = 'TIC'
075800070925     C                   EVAL      VABTIC=%trim(%subst(
075900070925     C                             %subst(vlrppt:posDaDft+1:
076000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100070925     C                   ENDIF
076200030822     C* GCA
076300030822     C                   IF        %subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):1:3)
076600030822     C                             = 'GCA'
076700030822     C                   EVAL      VABGCA=%trim(%subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000030822     C                   ENDIF
077100030822     C* CTM
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'CTM'
077600030822     C                   EVAL      VABCTM=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* FFD
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'FFD'
078500030822     C                   EVAL      VABFFD=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* VAD
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'VAD'
079400030822     C                   EVAL      VABVAD=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* GMA
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'GMA'
080300030822     C                   EVAL      VABGMA=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* GGA
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'GGA'
081200030822     C                   EVAL      VABGGA=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* GVA
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'GVA'
082100030822     C                   EVAL      VABGVA=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* TC1
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'TC1'
083000030822     C                   EVAL      VABTC1=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* TC2
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'TC2'
083900030822     C                   EVAL      VABTC2=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300040714     C* VATTRC
084400040714     C                   IF        %subst(
084500040714     C                             %subst(vlrppt:posDaDft+1:
084600040714     C                             posADft-posDaDft-1):1:3)
084700040714     C                             = 'TRC'
084800040714     C                   EVAL      VATTRC=%trim(%subst(
084900040714     C                             %subst(vlrppt:posDaDft+1:
085000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100040714     C                   ENDIF
085200030822     C* ...
085300030822     C                   ENDIF
085400030822     C                   ENDDO
085500020204     C*
085600010330     C                   ENDSR
085700010607     C*----------------------------------------------------*
085800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
085900010607     C*----------------------------------------------------*
086000010607     C     IMPVAB        BEGSR
086100010607     C*
086200010607     C                   SETOFF                                       3132
086300110315 xxx C***                SETON                                        35
086400030515     C*
086500010607     C                   MOVEL     datcor        VABAAS
086600010607     C                   MOVE      datcor        VABMGS
086700010607     C*
086800030715     C**********
086900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087000030715     C**********
087100030715     C                   IF        wGiro = *zeros
087200040113     C*
087300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087600080429     C                             %trim(intcol(5))<>*blanks
087700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087900080429     C                                      %trim(intcol(5))
088000070823     C     ';':CharCSV   XLATE     vindta        vindta
088100040510     C                   ENDIF
088200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088300031201     C     minu:maiu     XLATE     vindta        vindta
088400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088500030715     C                   DOW       posDa <= %len(%trim(vindta))
088600030715     C*
088700030715     C* Gestisco il 1° campo
088800030715     C                   IF        i = 1
088900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089000030715     C                   EVAL      posDa = 1
089100030715     C* Eseguo lo scan x trovare la fine del primo campo
089200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C* X i campi successivi al 1°
089700030715     C                   ELSE
089800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089900030715     C                   EVAL      posDa = posA + 1
090000030715     C* Eseguo lo scan x trovare la fine del campo corrente
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* Gestisco l'ultimo campo
090300030715     C                   IF        posA = *zeros
090400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090500030715     C* A questo punto "estraggo" il campo corrente
090600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090700030715     C                                                    (posA-posDa))
090800030715     C                   LEAVE
090900030715     C                   ELSE
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   ENDIF
091400030715     C                   ENDIF
091500030715     C* Incremento il contatore d campo
091600030715     C                   EVAL      i = i +1
091700030715     C                   ENDDO
091800030715     C*
091900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092000030715     C* delimitazione testo
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitFLD)
092300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092500030715     C                   EVAL      i = i + 1
092600030715     C                   ENDDO
092700030715     C                   EVAL      wGiro = 1
092800030715     C                   ELSE
092900030715     C                   EVAL      wGiro = 2
093000031204     C**********
093100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093200031204     C* d separatore campo
093300031204     C**********
093400031204     C                   EVAL      vindta = %trim(vindta)
093500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093600031204     C                   EVAL      vindta = %subst(vindta:2)
093700031204     C                   ENDDO
093800031204     C*
093900031204     C                   Z-ADD     *zeros        lunghInput        4 0
094000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094400110912     C                   ENDDO
094500030715     C**********
094600030715     C* Effettuo lo split del campo dati d input
094700030715     C**********
094800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094900030715     C                   DOW       posDa <= %len(%trim(vindta))
095000030715     C*
095100030715     C* Gestisco il 1° campo
095200030715     C                   IF        i = 1
095300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095400030715     C                   EVAL      posDa = 1
095500030715     C* Eseguo lo scan x trovare la fine del primo campo
095600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095700030715     C* A questo punto "estraggo" il campo corrente
095800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095900030715     C                                                    (posA-posDa))
096000030715     C* X i campi successivi al 1°
096100030715     C                   ELSE
096200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096300030715     C                   EVAL      posDa = posA + 1
096400031204     C* Verifico che nn vi sia il campo nn valorizzato
096500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096600031204     C* Se campo nn valorizzato skippo al prossimo
096700031204     C                   EVAL      SkSplitCSV(i) = *blanks
096800031204     C                   EVAL      posA  = posA + 1
096900031204     C                   ELSE
097000030715     C* Eseguo lo scan x trovare la fine del campo corrente
097100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097200030715     C* Gestisco l'ultimo campo
097300030715     C                   IF        posA = *zeros
097400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097500030715     C* A questo punto "estraggo" il campo corrente
097600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097700030715     C                                                    (posA-posDa))
097800030715     C                   LEAVE
097900030715     C                   ELSE
098000030715     C* A questo punto "estraggo" il campo corrente
098100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098200030715     C                                                    (posA-posDa))
098300030715     C                   ENDIF
098400031204     C                   ENDIF
098500030715     C                   ENDIF
098600030715     C* Incremento il contatore d campo
098700030715     C                   EVAL      i = i +1
098800030715     C                   ENDDO
098900030715     C*
099000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099100030715     C* delimitazione testo
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitCSV)
099400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099600030715     C                   EVAL      i = i + 1
099700030715     C                   ENDDO
099800100720     C*
099900100720     C* Stacco un progressivo x numero spedizione
100000141006 xxx C                   EXSR      repNSP
100100100326     C*
100200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100300030715     C                   EVAL      i = 1
100400030715     C                   DOW       i < %elem(SkSplitFLD)
100500030715     C*
100600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100800030715     C                             SkSplitCSV(i) <> *blanks
100900030721     C*** ATB
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101100030721     C                   EVAL      VABATB = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030715     C*** RSD
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101800030715     C* ==
101900130528     C                   ENDIF
102000030721     C*** RD2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030715     C*** IND
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102600030715     C                   EVAL      VABIND = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** LOD
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** PRD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030721     C*** NZD
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030715     C*** RMA
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** NOT
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** NT2
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030715     C*** VCA
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** VAS
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** FFD
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030721     C*** GC1
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GC2
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** TSP
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107500030721     C                   ENDIF
107600040510     C*** CBO
107700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107900040510     C                   ENDIF
108000030721     C*** NAS
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TIC
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** GCA
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** XCO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** CTM
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** TCR
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** CTS
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** FTM
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** VAD
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** GMA
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** GGA
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** GVA
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** TC1
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** TC2
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** SCL
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** RMO
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** NMO
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*
114900030721     C* Reperisco quindi i campi numerici...
115000040510     C*** NSP
115100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115200040510     C                   EVAL      PiStr=SkSplitCSV(i)
115300040510     C                   EXSR      CHKNUM
115400040510     C                   IF        PiInt=*on
115500040510     C                   Z-ADD     PiVal         VABNSP
115600040510     C                   ELSE
115700040510     C                   SETON                                        31
115800040510     C                   EVAL      VABNSP = *zeros
115900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116000040510     C                             + ' ' + 'VABNSP'
116100040510     C                   ENDIF
116200040510     C                   ENDIF
116300071106     C*** RMN
116400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116500071106     C                   EVAL      PiStr=SkSplitCSV(i)
116600071106     C                   EXSR      CHKNUM
116700071106     C                   IF        PiInt=*on
116800071106     C                   Z-ADD     PiVal         VABRMN
116900071106     C                   ELSE
117000071106     C                   SETON                                        32
117100071106     C                   EVAL      VABRMN = *zeros
117200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117300071106     C                             + ' ' + 'VABRMN'
117400071106     C                   ENDIF
117500071106     C                   ENDIF
117600030721     C*** CAD
117700130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117800160315     C* se la nazione non è Italia prendo il CAP alfanumerico, altrimenti numerico
117900160315     C* (siccome la pulizia di IT è dopo, testo anche questo valore)
118000160315     C                   IF        VABNZD <> 'IT'  and VABNZD <> *blank
118100160210     C                   EVAL      VABCAD=SkSplitCSV(i)
118200160315     C                   ELSE
118300160315     C                   EVAL      PiStr=SkSplitCSV(i)
118400160315     C                   EXSR      CHKNUM
118500160315     C                   IF        PiInt=*on
118600160315     C                   Z-ADD     PiVal         Num5_0
118700160315     C                   MOVEL(p)  Num5_0        VABCAD
118800160315     C                   ELSE
118900160315     C                   SETON                                        32
119000160315     C                   EVAL      VABCAD = *zeros
119100160315     C                   EVAL      vinmsg = %trimr(vinmsg)
119200160315     C                             + ' ' + 'VABCAD'
119300160315     C                   ENDIF
119400130605     C                   ENDIF
119500160315     C                   ENDIF
119600030721     C*** NCL
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on
120100030721     C                   Z-ADD     PiVal         VABNCL
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABNCL = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600040122     C                             + ' ' + 'VABNCL'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** PKB
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiNum=*on
121400130610     C                   Z-ADD(H)  PiVal         VABPKB
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      VABPKB = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABPKB'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** CAS
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122600140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122700140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122800140711     C                             %trim(SkSplitCSV(i)) <> ',00'
122900130703     C                   EVAL      VABVCA = 'EUR'
123000140930     C                   EVAL      VABTIC = *blank
123100140513     C                   EVAL      wFlgCAS = '1'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiNum=*on
123500030721     C                   Z-ADD     PiVal         VABCAS
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      VABCAS = *zeros
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCAS'
124100030721     C                   ENDIF
124200140513     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** CCM
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on      AND
124900030721     C                             PiVal<=9999999 AND
125000030721     C                             PiVal>*zeros
125100030721     C                   Z-ADD     PiVal         VABCCM
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      VABCCM = *zeros
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600040510     C                             + ' ' + 'VABCCM'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** LNP
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on    AND
126400030721     C                             PiVal<=999   AND
126500030721     C                             PiVal>*zeros
126600030721     C                   Z-ADD     PiVal         VABLNP
126700030721     C                   ELSE
126800040715     C                   SETON                                        32
126900030721     C                   EVAL      VABLNP = *zeros
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100040510     C                             + ' ' + 'VABLNP'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** NRS
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on AND
127900030721     C                             PiVal<=99
128000030721     C                   Z-ADD     PiVal         VABNRS
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABNRS'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** LNA
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on    AND
129200030721     C                             PiVal<=999   AND
129300030721     C                             PiVal>*zeros
129400030721     C                   Z-ADD     PiVal         VABLNA
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      VABLNA = *zeros
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABLNA'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** CTR
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on  AND
130700030721     C                             PiVal<=999
130800030721     C                   Z-ADD     PiVal         VABCTR
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABCTR'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** IAS
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131700141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131800141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131900141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
132000141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132100141003     C                             %trim(SkSplitCSV(i)) <> ',00'
132200141006     C                   EVAL      VABVAS = 'EUR'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiNum=*on
132600030721     C                   Z-ADD     PiVal         VABIAS
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABIAS'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300141003     C                   ENDIF
133400030721     C*** VLB
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiNum=*on
133900130610     C                   Z-ADD(H)  PiVal         VABVLB
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABVLB'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** QFT
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiNum=*on
135100030721     C                   Z-ADD     PiVal         VABQFT
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABQFT'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** NCD
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on      AND
136300030721     C                             PiVal<=9999999
136400030721     C                   Z-ADD     PiVal         VABNCD
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABNCD'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** NCA
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on      AND
137600030721     C                             PiVal<=9999999
137700030721     C                   Z-ADD     PiVal         VABNCA
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABNCA'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** ZNC
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on AND
138900030721     C                             PiVal<=99
139000030721     C                   Z-ADD     PiVal         VABZNC
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABZNC'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** DCR
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiInt=*on       AND
140200030721     C                             PiVal<=99999999
140300030721     C                   Z-ADD     PiVal         VABDCR
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABDCR'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000030721     C*** HCR
141100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiInt=*on   AND
141500030721     C                             PiVal<=9999
141600030721     C                   Z-ADD     PiVal         VABHCR
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142000030721     C                             + ' ' + 'VABHCR'
142100030721     C                   ENDIF
142200030721     C                   ENDIF
142300030721     C*** VMD
142400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142500030721     C                   EVAL      PiStr=SkSplitCSV(i)
142600030721     C                   EXSR      CHKNUM
142700030721     C                   IF        PiNum=*on
142800030721     C                   Z-ADD     PiVal         VABVMD
142900030721     C                   ELSE
143000030721     C                   SETON                                        32
143100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143200030721     C                             + ' ' + 'VABVMD'
143300030721     C                   ENDIF
143400030721     C                   ENDIF
143500030721     C*** ANT
143600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143700030721     C                   EVAL      PiStr=SkSplitCSV(i)
143800030721     C                   EXSR      CHKNUM
143900030721     C                   IF        PiInt=*on        AND
144000030721     C                             PiVal<=999999999
144100030721     C                   Z-ADD     PiVal         VABANT
144200030721     C                   ELSE
144300030721     C                   SETON                                        32
144400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144500030721     C                             + ' ' + 'VABANT'
144600030721     C                   ENDIF
144700030721     C                   ENDIF
144800030721     C*** CMO
144900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145000030721     C                   EVAL      PiStr=SkSplitCSV(i)
145100030721     C                   EXSR      CHKNUM
145200030721     C                   IF        PiInt=*on
145300030721     C                   Z-ADD     PiVal         Num5_0
145400030721     C                   MOVEL(p)  Num5_0        VABCMO
145500030721     C                   ELSE
145600030721     C                   SETON                                        32
145700030721     C                   EVAL      VABCMO = *zeros
145800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145900030721     C                             + ' ' + 'VABCMO'
146000030721     C                   ENDIF
146100030721     C                   ENDIF
146200040510     C*
146300040510     C* Reperisco i campi del FIVAT
146400040510     C*** VATNOT
146500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146700040510     C                   ENDIF
146800040510     C*** VATTRC
146900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147100040510     C                   ENDIF
147200060331     C*** VATNOT_A
147300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147500060331     C                   ENDIF
147600060331     C*** VATNOT_B
147700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147900060331     C                   ENDIF
148000060331     C*** VATNOT_E
148100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148300060331     C                   ENDIF
148400110927     C*** VATNOT_I
148500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148700110927     C                   ENDIF
148800141003     C*** VATNOT_IJ
148900141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
149000141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149100141003     C                   ENDIF
149200140319     C*** VATNOT_S
149300140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149400140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149500140319     C                   ENDIF
149600070823     C*
149700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149800070925     C*
149900141014     C*** VATNOT_E/VABRMA/VABRMN
150000141014     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
150100141014     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150200141014     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
150300141014     C* RMN è dalla pos. 3 in poi
150400141014     C                   EVAL      PiStr=%subst(SkSplitCSV(i) : 3 :
150500141014     C                              %len(%trim(SkSplitCSV(i))) - 2)
150600141014     C                   EXSR      CHKNUM
150700141014     C                   IF        PiInt=*on
150800141014     C                   Z-ADD     PiVal         VABRMN
150900141014     C                   ELSE
151000141014     C                   SETON                                        32
151100141014     C                   EVAL      VABRMN = *zeros
151200141014     C                   EVAL      vinmsg = %trimr(vinmsg)
151300141014     C                             + ' ' + 'VABRMN'
151400141014     C                   ENDIF
151500141006     C                   ENDIF
151600141014
151700141014     C*** VATNOT_B/VATNOT_S
151800141014     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_B'
151900141014     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
152000141014     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
152100141014     C                   ENDIF
152200160107
152300160107     C*** VABNOT/VABNT2
152400160107     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
152500160107     C                   EVAL      w70 = %trim(SkSplitCSV(i))
152600160107     C                   if        %subst(w70:1:35) <> *blanks
152700160107     C                   eval      VABNOT = %trim(%subst(w70:1:35))
152800160107     C                   endif
152900160107     C                   if        %subst(w70:36:35) <> *blanks
153000160107     C                   eval      VABNT2 = %trim(%subst(w70:36:35))
153100160107     C                   endif
153200160107     C                   ENDIF
153300141003     C*
153400071029     C***  <===  -----------------------------------------
153500040510     C*
153600030715     C                   ENDIF
153700030715     C                   EVAL      i = i + 1
153800030715     C                   ENDDO
153900070925     C*
154000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
154100140319     C*
154200141014     C* se la nazione è IT la sblanco
154300141014     C                   IF        VABNZD = 'IT'
154400141014     C                   EVAL      VABNZD = *blank
154500141014     C                   ENDIF
154600070925     C*
154700070925     C* Considerazioni finali su CBO/CAS
154800140220     C                   IF        wFlgCAS = '1'    OR
154900140220     C                             VABCAS  > *zeros
155000140220     C                   IF        VABCBO = '1'
155100140220     C                   EVAL      VABCBO = '4'
155200140220     C                   ENDIF
155300140220     C                   IF        VABCBO = '2'
155400140220     C                   EVAL      VABCBO = '6'
155500140220     C                   ENDIF
155600140220     C                   ENDIF
155700070925     C***  <===  ----------------------------
155800040510     C*
155900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156000040510     C                   IF        VABNSP = *zeros
156100040510     C                   SETON                                        31
156200040510     C                   ENDIF
156300040715     C*
156400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
156500040715     C                   IF        VABLNP = *zeros
156600040715     C                   SETON                                        31
156700040715     C                   ENDIF
156800040510     C*
156900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157000120411     C                   IF        VABRMN = *zeros
157100120411     C                   Z-ADD     VABNSP        VABRMN
157200120411     C                   ENDIF
157300120328     C* IN QUESTO CASO LO FORZO = 1
157400120411     C***                IF        VABRMN = *zeros
157500120411     C***                Z-ADD     1             VABRMN
157600120411     C***                ENDIF
157700040510     C*
157800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
157900040510     C                   IF        VABRMA = *blanks
158000040714     C                   MOVEL     VABRMN        VABRMA
158100040510     C                   ENDIF
158200040128     C*
158300040510     C* Se provincia nn valorizzata la reperisco
158400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158500110912     C                   IF        VABPRD =  *blanks AND
158600110912     C                             VABNZD =  *blanks AND
158700110912     C                             VABCAD <> *blanks AND
158800110912     C                             VABLOD <> *blanks
158900040128     C                   CLEAR                   TISI95DS
159000040128     C                   EVAL      I95TCN = '3'
159100040128     C                   Z-ADD     datcor        I95DAT
159200040128     C                   EVAL      I95NAR = VABNZD
159300040128     C                   EVAL      I95CAP = VABCAD
159400040128     C                   EVAL      I95LOC = VABLOD
159500040128     C                   CALL      'TISI95R'
159600040128     C                   PARM                    TISI95DS
159700040128     C                   EVAL      VABPRD = O95PRV
159800040510     C                   ENDIF
159900020204     C*
160000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
160100020204     C                   EXSR      CHKIMPDIV
160200030715     C                   ENDIF
160300020204     C*
160400010607     C                   ENDSR
160500010607     C*----------------------------------------------------*
160600010601
160700020204
160800020204     C*----------------------------------------------------*
160900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
161000020204     C*----------------------------------------------------*
161100020204     C     CHKIMPDIV     BEGSR
161200020204     C*
161300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
161400020204     C                   Z-ADD     *zeros        wrkDec            9 9
161500020204     C*
161600020204     C* Come prima cosa effettuo considerazioni sulla divisa
161700020204     C                   IF        vabIAS > *zeros
161800020204     C                   IF        vabVAS <> 'EUR'
161900020204     C                   EVAL      vabVAS =  'ITL'
162000020204     C                   ENDIF
162100020204     C                   ENDIF
162200020204     C*
162300020204     C                   IF        vabCAS > *zeros
162400020204     C                   IF        vabVCA <> 'EUR'
162500020204     C                   EVAL      vabVCA =  'ITL'
162600020204     C                   ENDIF
162700020204     C                   ENDIF
162800020204     C*
162900020204     C                   IF        vabVMD > *zeros
163000020204     C                   IF        vabVAD <> 'EUR'
163100020204     C                   EVAL      vabVAD =  'ITL'
163200020204     C                   ENDIF
163300020204     C                   ENDIF
163400020204     C*
163500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
163600020204     C                   Z-ADD     vabIAS        wrkDec
163700020204     C                   IF        wrkDec > *zeros
163800020204     C                   IF        vabVAS = 'ITL'
163900020204     C                   EVAL      vabIAS = *zeros
164000020204     C                   ENDIF
164100020204     C                   ENDIF
164200020204     C*
164300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
164400020204     C                   Z-ADD     vabCAS        wrkDec
164500020204     C                   IF        wrkDec > *zeros
164600020204     C                   IF        vabVCA = 'ITL'
164700020204     C                   EVAL      vabCAS = *zeros
164800020204     C                   ENDIF
164900020204     C                   ENDIF
165000020204     C*
165100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
165200020204     C                   Z-ADD     vabVMD        wrkDec
165300020204     C                   IF        wrkDec > *zeros
165400020204     C                   IF        vabVAD = 'ITL'
165500020204     C                   EVAL      vabVMD = *zeros
165600020204     C                   ENDIF
165700020204     C                   ENDIF
165800020204     C*
165900020204     C                   ENDSR
166000020204     C***
166100010330
166200100318
166300100318
166400100318     C*----------------------------------------------------*
166500100318     C*  CONTROLLO NUMERICITA' CAMPI
166600100318     C*----------------------------------------------------*
166700100318     C     CHKNUM        BEGSR
166800100318     C*
166900100318     C                   IF        PiDecChr = *blanks
167000100318     C                   EVAL      PiDecChr = CharNUM
167100100318     C                   ENDIF
167200100318     C*
167300100318     C                   callp     UBISNUM_Check(PiStr
167400100318     C                                          :PiDecChr
167500100318     C                                          :PiVal
167600100318     C                                          :PiNum
167700100318     C                                          :PiInt)
167800100318     C*
167900100318     C                   ENDSR
168000100318     C***
168100010601
168200010601
168300010601
168400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168500141006      ***
168600141014     C     repfil        BEGSR
168700141014     C*
168800141014     C                   if        invfil = *zeros and
168900141014     C                             depfil > *zeros and
169000141014     C                             (vinflg = *blanks or
169100141014     C                              vinflg = *zeros)
169200141014     C*
169300141014     C                   eval      invfil = depfil
169400141014     C                   endif
169500141014     C*
169600141014     C                   if        depfil <> invfil and
169700141014     C                             invfil > *zeros
169800141014     C                   eval      flgMulti = '1'
169900141014     C                   if        vinflg = *blanks
170000141014     C                   add       1             cntNonEl
170100141014     C                   endif
170200141014     C                   endif
170300141014     C*
170400141014     C                   if        vinflg = '2'
170500141014     C                   eval      flgStato = '2'
170600141014     C                   endif
170700141014     C*
170800141014     C                   ENDSR
170900141014     C*
171000010601
171100010601
171200010601
171300010330
171400040119      /TITLE Invio dei dati al punto operativo.
171500040119     C     invio         BEGSR
171600040119     C*
171700040119     C* 1° invio FIVAT
171800040119     C                   reset                   dscmz
171900040119     C                   move      vlrpoi        cmzdst
172000040119     C                   eval      cmzfld = 'FIVATWWR'
172100040119     C                   eval      cmzmbd = vlrhdl
172200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172300040119     C***                if        prmfir = *blanks
172400040119     C                   eval      cmzfla = 'FIVAT00F'
172500040119     C                   eval      cmzmba = 'FIVAT00F'
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
174200040119     C* 2° invio FIVAB
174300040119     C                   reset                   dscmz
174400040119     C                   move      vlrpoi        cmzdst
174500040119     C                   eval      cmzfld = vlrfou
174600040119     C                   eval      cmzmbd = vlrhdl
174700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174800040119     C***                if        prmfir = *blanks
174900040119     C                   eval      cmzfla = 'FIVAB00F'
175000040119     C                   eval      cmzmba = 'FIVAB00F'
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
179700070329     C                   if        %open(fivabwwr)
179800070329     C                   close     fivabwwr
179900070329     C                   endif
180000070329     C                   if        %open(fivatwwr)
180100070329     C                   close     fivatwwr
180200070329     C                   endif
180300070326     C*
180400070326     C* Effettuo la chiamata al CLLE preposto
180500070326     C                   call(e)   'TITVVTC'
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
186800140711     C                             + ' ' + 'VABNSP VATNSP'
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
188400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188500100318     C                   eval      datcor = %dec(%date() : *ISO)
188600000613     C*
188700000613     C                   ENDSR
188800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188900141014W_VABRMN;VABRSD;W_VATNOT_B;VATNOT_IJ;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VABPKB;VABNCL;VABRMO;
189000160107VABCCM;VABVMD;W_VABNOT;
189100140512
189200111116
189300100318
