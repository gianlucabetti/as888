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
009100151105     D wFlagAccessori1...
009200151105     D                 S              5
009300151105     D wFlagAccessori2...
009400151105     D                 S              5
009500030822     D*------------------
009600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009700030822     D*------------------
009800030822     D posDaDft        S              3  0 INZ(*zeros)
009900030822     D posADft         S              3  0 INZ(*zeros)
010000030822     D j               s              3  0 INZ(1)
010100030822     D wGiroDft        s              1  0 INZ(*zeros)
010200080418     D*------------------
010300080418     D* Costanti
010400080418     D*------------------
010500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010700100318
010800100318     D*------------------
010900100318     D* LINKING A DEFINIZIONI ESTERNE
011000100318     D*------------------
011100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
011300100318
011400010330
011500010330
011600010330
011700000913     C                   reset                   rrnum
011800990921     C                   reset                   esito
011900990921     C                   reset                   wrkesito
012000010601     C*
012100010601     C                   exsr      opeini
012200010605     C                   exsr      rwvab
012300010601     C*
012400010601     C                   seton                                        lr
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013000010601     C*--------------------------------------------------------
013100010601     C     PREELA        BEGSR
013200010601     C*
013300010601     C* SE OCCORRE SPEDIRE IN FILIALE
013400010601     C                   if        invfil <> *zeros and
013500010601     C                             flgGiro = '0'
013600010601     C*
013700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013800010601     C                   eval      flgGiro = '1'
013900010601     C*
014000010601     C                   endif
014100010601     C*
014200010601     C                   ENDSR
014300010601     C***
014400010601
014500010601
014600010601
014700010601
014800010601     C*--------------------------------------------------------
014900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015000010601     C*--------------------------------------------------------
015100010601     C     ENDELA        BEGSR
015200040119     C*
015300040119     C* Effettuo la chiamata al CLLE preposto
015400040119     C                   call(e)   'TITVVTC'
015500040510     C                   parm                    parccm
015600040119     C                   parm                    parmbr
015700040119     C                   parm      '2'           paropz
015800050201     C*
015900050201     C* Effettuo lancio TISI95 solo x chiusura
016000050201     C                   CLEAR                   TISI95DS
016100050201     C                   EVAL      I95TLA = 'C'
016200050201     C                   CALL      'TISI95R'
016300050201     C                   PARM                    TISI95DS
016400000616     C*
016500010601     C                   ENDSR
016600010601
016700010601
016800010330     C*--------------------------------------------------------
016900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
017000010330     C*--------------------------------------------------------
017100010605     C     RWVAB         BEGSR
017200010330     C*
017300010330     C                   if        not %open(tivin00r)
017400010330     C                   open      tivin00r
017500010330     C                   endif
017600021025     C                   if        not %open(fivabwwr)
017700021025     C                   open      fivabwwr
017800010330     C                   endif
017900040119     C*
018000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018100040119     C                   exsr      prevat
018200040119     C*
018300040119     C                   if        chkcall = '0'
018400040119     C*
018500040119     C                   if        not %open(fivatwwr)
018600040119     C                   open      fivatwwr
018700040119     C                   endif
018800010330     C*
018900040119     C                   clear                   §CTROKVB          7 0
019000040119     C                   clear                   §CTROKVT          7 0
019100040119     C                   clear                   §CTRMO            7 0
019200040119     C                   clear                   §CTRNO            7 0
019300120410     C*
019400120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019500120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019600120410     C                   IF        Intestazioni = *off
019700120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019800120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019900120410     C                   exsr      inzvar
020000120410     C                   exsr      impvab
020100120410     C                   ENDIF
020200010330     C*
020300030822     C                   do        *HIVAL
020400010330     C*
020500030822     C                   read      tivin00r                               70
020600010618     C*
020700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020800010618     C                   if        vinflg = '1'
020900010618     C                   eval      flgOk = '1'
021000010618     C                   endif
021100040510     C*
021200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021300040510     C                   z-add     *zeros        wLenVINDTA        4 0
021400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021800040510     C                   z-add     1             wFlgVINDTA
021900040510     C                   leave
022000040510     C                   endif
022100040510     C                   enddo
022200010618     C*
022300040510     C                   if        vindta > *blanks AND
022400040510     C                             wFlgVINDTA = 1
022500010330     C                   add       1             rrnum
022600010330     C*
022700010601     C                   if        *in70 = *off and
022800010330     C                             (vinflg = *blanks
022900010330     C                              or vinflg = '0'
023000010330     C                              or vinflg = '2')
023100010330     C*
023200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023300010711     C                   if        vinflg = *blanks or vinflg = '0'
023400010711     C                   clear                   vinmsg
023500010711     C                   endif
023600130917     C*
023700130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023800130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023900130917     C* evitare che vengano elisi i blank
024000140220     C***                eval      vindta  = '*' + %trim(vindta)
024100130917     C*
024200080527     C                   exsr      inzvar
024300100318     C                   exsr      normCSV
024400010605     C                   exsr      impvab
024500010601     C*
024600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024700141014     C                   eval      depfil = VABLNP
024800141014     C                   exsr      repfil
024900141014     C                   if        depfil = invfil
025000141014     C                   if        vlrpoi = 999
025100141014     C                   move(P)   invfil        VABFGS
025200141014     C                   else
025300141014     C                   move(P)   vlrpoi        VABFGS
025400141014     C                   endif
025500010601     C*
025600010601     C                   exsr      PREELA
025700010601     C*
025800010604     C* Ebbene...
025900010604     C*
026000040119     C  N31              add       1             §CTROKVB
026100040119     C   32              add       1             §CTRMO
026200040119     C   31              add       1             §CTRNO
026300080418     C*
026400030822     C                   if        wGiro = 2
026500080513     C* Scrivo sempre il VAT
026600080513     C  N31              exsr      wrivat
026700110201     C                   exsr      chkvab
026800080513     C                   endif
026900010604     C*
027000010604     C                   if        *in31 = *off and
027100010604     C                             *in32 = *off
027200010604     C                   eval      vinflg = '1'
027300010604     C                   else
027400010604     C                   eval      vinflg = '2'
027500010604     C                   endif
027600141014     C                   endif
027700010601     C*
027800010604     C                   endif
027900010604     C*
028000010330     C                   else
028100010330     C                   eval      vinflg = '1'
028200010330     C                   endif
028300010601     C*
028400010601     C  N70              update    tivin000
028500010330     C*
028600030822     C  N70              enddo
028700040119     C*
028800080527     C* Verifico dati rimasti "in cannna"...
028900080527     C                   seton                                        65
029000110201     C                   exsr      chkvab
029100080527     C*
029200040119     C                   endif                                                  (endif - chkcall)
029300010601     C*
029400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029500010601     C                   if        cntNonEl = *zeros or
029600010601     C                             flgMulti = '0'
029700010330     C* Se non ci sono record con errori ...
029800010601     C                   if        §ctrno = 0 and
029900010604     C                             §ctrmo = 0 and
030000010601     C                             flgStato <> '2'
030100010330     C* ... restituisco esito OK.
030200010330     C                   eval      wrkesito = '0'
030300010330     C                   else
030400040119     C                   if        §ctrokvb > 0
030500010330     C                   eval      wrkesito = '1'
030600010330     C                   else
030700010615     C                   if        flgOk = '0'
030800010615     C                   eval      wrkesito = '2'
030900010615     C                   else
031000010615     C                   eval      wrkesito = '6'
031100010615     C                   endif
031200010330     C                   endif
031300010330     C                   endif
031400010601     C                   else
031500010601     C                   eval      wrkesito = '9'
031600010601     C                   endif
031700010330     C*
031800010330     C                   if        %open(tivin00r)
031900010330     C                   close     tivin00r
032000010330     C                   endif
032100021025     C                   if        %open(fivabwwr)
032200021025     C                   close     fivabwwr
032300010330     C                   endif
032400040512     C                   if        %open(fivatwwr)
032500040512     C                   close     fivatwwr
032600040512     C                   endif
032700010601     C*
032800010601     C                   if        vlrpoi <> 999
032900010601     C                   eval      invfil = vlrpoi
033000010601     C                   endif
033100010330     C*
033200040119     C                   if        §ctrokvb > 0
033300010601     C                             and invfil > *zeros
033400010330     C                   exsr      invio
033500010330     C                   endif
033600010601     C*
033700010618     C                   if        flgGiro = '1'
033800010601     C                   exsr      endela
033900010618     C                   endif
034000010330     C*
034100010330     C                   ENDSR
034200010330     C***
034300080527
034400080527
034500080527
034600080527     C*----------------------------------------------------*
034700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034800080527     C*----------------------------------------------------*
034900100413     C     CHKVAB        BEGSR
035000110201     C*
035100110201     C* Se richiesti ignorare colli / peso / volume
035200110315 xxx C***                if        *in35
035300110315 xxx C***                z-add     *zeros        VABNCL
035400110315 xxx C***                z-add     *zeros        VABPKB
035500110315 xxx C***                z-add     *zeros        VABVLB
035600110315 xxx C***                endif
035700080527     C*
035800080527     C* Scrivo il VAB solo a rottura d spedizione
035900080527     C                   if        depspe = *zeros
036000140930
036100140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036200140930      * spedizione
036300140930     C  N31              eval      vabncl = 1
036400140930
036500080527     C  N31              eval      fivabds_sav = fivabds
036600080527     C                   eval      depspe = vabNSP
036700080527     C                   else
036800080527     C                   if        depspe = vabNSP AND *in65 = *off
036900140930
037000140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
037100140930      * spedizione
037200140930     C                   eval      vabncl += 1
037300140930
037400100413     C   51              add       VABNCL        sav_VABNCL
037500100413     C   51              add       VABPKB        sav_VABPKB
037600100413     C   51              add       VABVLB        sav_VABVLB
037700100720     C   52              z-add     VABNCL        sav_VABNCL
037800100720     C   52              z-add     VABPKB        sav_VABPKB
037900100720     C   52              z-add     VABVLB        sav_VABVLB
038000100720     C   53              add       VABNCL        sav_VABNCL
038100100720     C   53              z-add     VABPKB        sav_VABPKB
038200100720     C   53              z-add     VABVLB        sav_VABVLB
038300080527     C                   else
038400080527     C  N31              eval      fivabds_ok = fivabds_sav
038500080527     C  N31              exsr      wrivab
038600080527     C  N31              eval      fivabds_sav = fivabds
038700140930
038800140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
038900140930      * spedizione
039000140930     C  N31              eval      vabncl = 1
039100140930
039200080527     C                   eval      depspe = vabNSP
039300080527     C                   endif
039400080527     C                   endif
039500080527     C*
039600080527     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040200040119     C*----------------------------------------------------*
040300040510     C     WRIVAB        BEGSR
040400080513     C*
040500080513     C* Gestisco l'eventuale rottura x numero spedizione
040600080527     C                   if        VABNSP <> depspe or *in65 = *on
040700070301     C                   if        wVATNOT_E <> *blanks
040800130610     C                   eval      ok_VABCTM = '7Q'
040900070301     C                   endif
041000100413     C                   write     fivab000
041100080513     C                   endif
041200040119     C*
041300040119     C                   ENDSR
041400040119
041500040119
041600040119
041700040119     C*----------------------------------------------------*
041800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041900040119     C*----------------------------------------------------*
042000040510     C     WRIVAT        BEGSR
042100060512     C*
042200060512     C* Inizializzo i campi chiave
042300060512     C                   eval      VATFGS = VABFGS
042400060512     C                   eval      VATAAS = VABAAS
042500060512     C                   eval      VATCCM = VABCCM
042600070823     C                   eval      VATNRS = VABNRS
042700060512     C                   eval      VATNSP = VABNSP
042800060512     C                   eval      VATLNP = VABLNP
042900040119     C*
043000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043100040510     C                   if        VATTRC = *blanks
043200040510     C                   eval      VATTRC = 'E'
043300040510     C                   endif
043400110318     C                   if        VATNOT <> *blanks
043500040510     C                   write     FIVAT000
043600040119     C                   add       1             §CTROKVT
043700110318     C                   endif
043800060331     C*
043900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044000060331     C                   if        wVATNOT_A <> *blanks
044100060331     C                   eval      VATNOT = wVATNOT_A
044200060331     C                   eval      VATTRC = 'A'
044300060331     C                   write     FIVAT000
044400060331     C                   add       1             §CTROKVT
044500060331     C                   endif
044600060331     C*
044700060331     C                   if        wVATNOT_B <> *blanks
044800060331     C                   eval      VATNOT = wVATNOT_B
044900060331     C                   eval      VATTRC = 'B'
045000060331     C                   write     FIVAT000
045100060331     C                   add       1             §CTROKVT
045200060331     C                   endif
045300140711     C*
045400140711     C                   if        wVATNOT_IJ <> *blanks
045500140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045600140711     C                   eval      VATTRC = 'I'
045700140711     C                   write     FIVAT000
045800140711     C                   add       1             §CTROKVT
045900140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
046000140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046100140711     C                   eval      VATTRC = 'J'
046200140711     C                   write     FIVAT000
046300140711     C                   endif
046400140711     C                   endif
046500060331     C*
046600060331     C                   if        wVATNOT_E <> *blanks
046700060331     C                   eval      VATTRC = 'E'
046800140527     C*
046900140711     C* NON Sviluppare i "CHI SONO"
047000140527     C*
047100140711     C                   EVAL      VATNOT = wVATNOT_E
047200140711     C                   write     FIVAT000
047300140711     C                   add       1             §CTROKVT
047400120320     C                   endif
047500110927     C*
047600110927     C                   if        wVATNOT_I <> *blanks
047700110927     C                   eval      VATNOT = wVATNOT_I
047800140711     C                   eval      VATTRC = 'I'
047900110927     C                   write     FIVAT000
048000110927     C                   add       1             §CTROKVT
048100110927     C                   endif
048200140220     C*
048300140220     C                   if        wVATNOT_J <> *blanks
048400140220     C                   eval      VATNOT = wVATNOT_J
048500140220     C                   eval      VATTRC = 'J'
048600140220     C                   write     FIVAT000
048700140220     C                   add       1             §CTROKVT
048800140220     C                   endif
048900140319     C*
049000140319     C                   if        wVATNOT_S <> *blanks
049100140319     C                   eval      VATNOT = wVATNOT_S
049200140319     C* i 2 flag devono essere blank
049300140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049400140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049500140319     C                   eval      VATTRC = 'S'
049600140319     C                   write     FIVAT000
049700140319     C                   add       1             §CTROKVT
049800140319     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100040119
050200040119
050300040119
050400040119     C*----------------------------------------------------*
050500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050600040119     C*----------------------------------------------------*
050700040119     C     PREVAT        BEGSR
050800040119     C*
050900040119     C* Compongo il nome del membro da dare al FIVATWWR
051000040119     C                   eval      parmbr = vlrhdl
051100040119     C                   movel     'M'           parmbr
051200040510     C                   eval      parccm = vlrksc
051300040119     C                   eval      paropz = '1'
051400040119     C* Effettuo la chiamata al CLLE preposto
051500040119     C                   call(e)   'TITVVTC'
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
053800020204     C                   Z-ADD     *zeros        Num5_0            5 0
053900101018     C                   MOVEL     *blanks       wFlgCAS
054000060331     C                   MOVEL     *blanks       wVATNOT_A
054100060331     C                   MOVEL     *blanks       wVATNOT_B
054200060331     C                   MOVEL     *blanks       wVATNOT_E
054300110927     C                   MOVEL     *blanks       wVATNOT_I
054400140220     C                   MOVEL     *blanks       wVATNOT_J
054500140220     C                   MOVEL     *blanks       wVATNOT_IJ
054600140319     C                   MOVEL     *blanks       wVATNOT_S
054700120411     C                   MOVEL     *blanks       wNOT
054800120411     C                   MOVEL     *blanks       Data8A
054900151105     C                   EVAL      wFlagAccessori1 = *blank
055000151105     C                   EVAL      wFlagAccessori2 = *blank
055100030822     C*
055200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055300030822     C                   CLEAR                   SkSplitCSV
055400030822     C                   EVAL      i = 1
055500030822     C                   EVAL      posDa = *zeros
055600030822     C                   EVAL      posA  = *zeros
055700080527     C*
055800080527     C                   CLEAR                   FIVABDS
055900080527     C                   CLEAR                   FIVAT000
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
069100130610     C                   Z-ADD     PiVal         VABPKB
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
070400130610     C                   Z-ADD     PiVal         VABVLB
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
086100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
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
100300141006 xxx C                   EXSR      repNSP
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
102200130528     C                   ENDIF
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
118000130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118100130610     C                   EVAL      PiStr=SkSplitCSV(i)
118200130610     C                   EXSR      CHKNUM
118300130610     C                   IF        PiInt=*on
118400130610     C                   Z-ADD     PiVal         Num5_0
118500130610     C                   MOVEL(p)  Num5_0        VABCAD
118600130610     C                   ELSE
118700130610     C                   SETON                                        32
118800130610     C                   EVAL      VABCAD = *zeros
118900130610     C                   EVAL      vinmsg = %trimr(vinmsg)
119000130610     C                             + ' ' + 'VABCAD'
119100130610     C                   ENDIF
119200130605     C                   ENDIF
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
121100130610     C                   Z-ADD(H)  PiVal         VABPKB
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      VABPKB = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABPKB'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** CAS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122200111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122300140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122400140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122500140711     C                             %trim(SkSplitCSV(i)) <> ',00'
122600130703     C                   EVAL      VABVCA = 'EUR'
122700140930     C                   EVAL      VABTIC = *blank
122800140513     C                   EVAL      wFlgCAS = '1'
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
123900140513     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** CCM
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on      AND
124600030721     C                             PiVal<=9999999 AND
124700030721     C                             PiVal>*zeros
124800030721     C                   Z-ADD     PiVal         VABCCM
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      VABCCM = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040510     C                             + ' ' + 'VABCCM'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** LNP
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on    AND
126100030721     C                             PiVal<=999   AND
126200030721     C                             PiVal>*zeros
126300030721     C                   Z-ADD     PiVal         VABLNP
126400030721     C                   ELSE
126500040715     C                   SETON                                        32
126600030721     C                   EVAL      VABLNP = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800040510     C                             + ' ' + 'VABLNP'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NRS
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on AND
127600030721     C                             PiVal<=99
127700030721     C                   Z-ADD     PiVal         VABNRS
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNRS'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** LNA
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on    AND
128900030721     C                             PiVal<=999   AND
129000030721     C                             PiVal>*zeros
129100030721     C                   Z-ADD     PiVal         VABLNA
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      VABLNA = *zeros
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABLNA'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** CTR
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on  AND
130400030721     C                             PiVal<=999
130500030721     C                   Z-ADD     PiVal         VABCTR
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABCTR'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** IAS
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131400141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131500141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131600141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131700141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
131800141003     C                             %trim(SkSplitCSV(i)) <> ',00'
131900141006     C                   EVAL      VABVAS = 'EUR'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABIAS
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABIAS'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000141003     C                   ENDIF
133100030721     C*** VLB
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiNum=*on
133600130610     C                   Z-ADD(H)  PiVal         VABVLB
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABVLB'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** QFT
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiNum=*on
134800030721     C                   Z-ADD     PiVal         VABQFT
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABQFT'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** NCD
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on      AND
136000030721     C                             PiVal<=9999999
136100030721     C                   Z-ADD     PiVal         VABNCD
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABNCD'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** NCA
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on      AND
137300030721     C                             PiVal<=9999999
137400030721     C                   Z-ADD     PiVal         VABNCA
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABNCA'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** ZNC
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on AND
138600030721     C                             PiVal<=99
138700030721     C                   Z-ADD     PiVal         VABZNC
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABZNC'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** DCR
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on       AND
139900030721     C                             PiVal<=99999999
140000030721     C                   Z-ADD     PiVal         VABDCR
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABDCR'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** HCR
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiInt=*on   AND
141200030721     C                             PiVal<=9999
141300030721     C                   Z-ADD     PiVal         VABHCR
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABHCR'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000030721     C*** VMD
142100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142200030721     C                   EVAL      PiStr=SkSplitCSV(i)
142300030721     C                   EXSR      CHKNUM
142400030721     C                   IF        PiNum=*on
142500030721     C                   Z-ADD     PiVal         VABVMD
142600030721     C                   ELSE
142700030721     C                   SETON                                        32
142800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142900030721     C                             + ' ' + 'VABVMD'
143000030721     C                   ENDIF
143100030721     C                   ENDIF
143200030721     C*** ANT
143300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143400030721     C                   EVAL      PiStr=SkSplitCSV(i)
143500030721     C                   EXSR      CHKNUM
143600030721     C                   IF        PiInt=*on        AND
143700030721     C                             PiVal<=999999999
143800030721     C                   Z-ADD     PiVal         VABANT
143900030721     C                   ELSE
144000030721     C                   SETON                                        32
144100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144200030721     C                             + ' ' + 'VABANT'
144300030721     C                   ENDIF
144400030721     C                   ENDIF
144500030721     C*** CMO
144600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144700030721     C                   EVAL      PiStr=SkSplitCSV(i)
144800030721     C                   EXSR      CHKNUM
144900030721     C                   IF        PiInt=*on
145000030721     C                   Z-ADD     PiVal         Num5_0
145100030721     C                   MOVEL(p)  Num5_0        VABCMO
145200030721     C                   ELSE
145300030721     C                   SETON                                        32
145400030721     C                   EVAL      VABCMO = *zeros
145500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145600030721     C                             + ' ' + 'VABCMO'
145700030721     C                   ENDIF
145800030721     C                   ENDIF
145900040510     C*
146000040510     C* Reperisco i campi del FIVAT
146100040510     C*** VATNOT
146200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146400040510     C                   ENDIF
146500040510     C*** VATTRC
146600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146800040510     C                   ENDIF
146900060331     C*** VATNOT_A
147000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147200060331     C                   ENDIF
147300060331     C*** VATNOT_B
147400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147600060331     C                   ENDIF
147700060331     C*** VATNOT_E
147800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
147900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148000060331     C                   ENDIF
148100110927     C*** VATNOT_I
148200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148400110927     C                   ENDIF
148500141003     C*** VATNOT_IJ
148600141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148700141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148800141003     C                   ENDIF
148900140319     C*** VATNOT_S
149000140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149100140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149200140319     C                   ENDIF
149300070823     C*
149400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149500070925     C*
149600150123     C*** VATNOT_E/VABRMA/VABCCM
149700141112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
149800141014     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
149900141014     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
150000150123     C                   SELECT
150100150123     C                   WHEN      %subst(VABRMA:1:3) = 'BIT'
150200150126     C                   EVAL      VABCCM = 1162984
150300150123     C                   WHEN      %subst(VABRMA:1:3) = 'GIT'
150400150126     C                   EVAL      VABCCM = 1163038
150500150123     C                   WHEN      %subst(VABRMA:1:4) = 'MFIT'
150600150126     C                   EVAL      VABCCM = 1163039
150700150123     C                   WHEN      %subst(VABRMA:1:2) = 'MT'
150800150126     C                   EVAL      VABCCM = 1163040
150900150123     C                   WHEN      %subst(VABRMA:1:2) = 'FM'
151000150126     C                   EVAL      VABCCM = 1163041
151100160119     C                   WHEN      %subst(VABRMA:1:2) = 'BB'
151200160119     C                   EVAL      VABCCM = 1163298
151300150123     C                   ENDSL
151400141006     C                   ENDIF
151500141014
151600141112     C*** VABNOT/VABNT2
151700141112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
151800141112     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
151900141112     C                   EVAL      VABNOT = %subst(wNOT:1:35)
152000141112     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
152100141014     C                   ENDIF
152200151105
152300151105     C*** Flag accessori 1
152400151105     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTC1'
152500151105     C                   EVAL      wFlagAccessori1 = %trim(SkSplitCSV(i))
152600151105     C                   ENDIF
152700151105
152800151105     C*** Flag accessori 2
152900151105     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTC2'
153000151105     C                   EVAL      wFlagAccessori2 = %trim(SkSplitCSV(i))
153100151105     C                   ENDIF
153200141003     C*
153300071029     C***  <===  -----------------------------------------
153400040510     C*
153500030715     C                   ENDIF
153600030715     C                   EVAL      i = i + 1
153700030715     C                   ENDDO
153800070925     C*
153900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
154000151105
154100151105     C* griglia per valorizzazione in base ai flag accessori 1 e 2
154200151105     C                   SELECT
154300151105     C                   WHEN      %trim(wFlagAccessori1)='AFP' and
154400151105     C                             %trim(wFlagAccessori2)='AF02'
154500151105     C                   EVAL      VABTC1 = 'P'
154600151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
154700151105     C                             %trim(wFlagAccessori2)='AC29'
154800151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
154900151105     C                             %trim(wFlagAccessori2)='AC30'
155000151105     C                   EVAL      VABTC2 = 'A'
155100151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
155200151105     C                             %trim(wFlagAccessori2)='AC31'
155300151105     C                   WHEN      %trim(wFlagAccessori1)='TDF' and
155400151105     C                             %trim(wFlagAccessori2)='T001'
155500151105     C                   WHEN      %trim(wFlagAccessori1)='TDF' and
155600151105     C                             %trim(wFlagAccessori2)='T002'
155700151105     C                   WHEN      %trim(wFlagAccessori1)='TDF' and
155800151105     C                             %trim(wFlagAccessori2)='T003'
155900151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
156000151105     C                              %trim(wFlagAccessori1)='CNT')   and
156100151105     C                             (%trim(wFlagAccessori2)='AF02' or
156200151105     C                              %trim(wFlagAccessori2)='AC29')
156300151105     C                   EVAL      VABTC1 = 'P'
156400151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
156500151105     C                              %trim(wFlagAccessori1)='CNT')   and
156600151105     C                             (%trim(wFlagAccessori2)='AF02' or
156700151105     C                              %trim(wFlagAccessori2)='AC30')
156800151105     C                   EVAL      VABTC1 = 'P'
156900151105     C                   EVAL      VABTC2 = 'A'
157000151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
157100151105     C                              %trim(wFlagAccessori1)='CNT')   and
157200151105     C                             (%trim(wFlagAccessori2)='AF02' or
157300151105     C                              %trim(wFlagAccessori2)='AC31')
157400151105     C                   EVAL      VABTC1 = 'P'
157500151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
157600151105     C                              %trim(wFlagAccessori1)='TDF')   and
157700151105     C                             (%trim(wFlagAccessori2)='AF02' or
157800151105     C                              %trim(wFlagAccessori2)='T001')
157900151105     C                   EVAL      VABTC1 = 'P'
158000151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
158100151105     C                              %trim(wFlagAccessori1)='TDF')   and
158200151105     C                             (%trim(wFlagAccessori2)='AF02' or
158300151105     C                              %trim(wFlagAccessori2)='T002')
158400151105     C                   EVAL      VABTC1 = 'P'
158500151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
158600151105     C                              %trim(wFlagAccessori1)='TDF')   and
158700151105     C                             (%trim(wFlagAccessori2)='AF02' or
158800151105     C                              %trim(wFlagAccessori2)='T003')
158900151105     C                   EVAL      VABTC1 = 'P'
159000151105     C                   WHEN      (%trim(wFlagAccessori1)='CNT' or
159100151105     C                              %trim(wFlagAccessori1)='CNT')   and
159200151105     C                             (%trim(wFlagAccessori2)='AC29' or
159300151105     C                              %trim(wFlagAccessori2)='AC30')
159400151105     C                   EVAL      VABTC2 = 'A'
159500151105     C                   WHEN      (%trim(wFlagAccessori1)='CNT' or
159600151105     C                              %trim(wFlagAccessori1)='CNT')   and
159700151105     C                             (%trim(wFlagAccessori2)='AC30' or
159800151105     C                              %trim(wFlagAccessori2)='AC31')
159900151105     C                   EVAL      VABTC2 = 'A'
160000151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
160100151105     C                              %trim(wFlagAccessori1)='CNT' or
160200151105     C                              %trim(wFlagAccessori1)='CNT')   and
160300151105     C                             (%trim(wFlagAccessori2)='AF02' or
160400151105     C                              %trim(wFlagAccessori2)='AC29' or
160500151105     C                              %trim(wFlagAccessori2)='AC30')
160600151105     C                   EVAL      VABTC1 = 'P'
160700151105     C                   EVAL      VABTC2 = 'A'
160800151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
160900151105     C                              %trim(wFlagAccessori1)='CNT' or
161000151105     C                              %trim(wFlagAccessori1)='CNT')   and
161100151105     C                             (%trim(wFlagAccessori2)='AF02' or
161200151105     C                              %trim(wFlagAccessori2)='AC30' or
161300151105     C                              %trim(wFlagAccessori2)='AC31')
161400151105     C                   EVAL      VABTC1 = 'P'
161500151105     C                   EVAL      VABTC2 = 'A'
161600151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
161700151105     C                             %trim(wFlagAccessori2)='AC39'
161800151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
161900151105     C                             %trim(wFlagAccessori2)='AC40'
162000151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
162100151105     C                              %trim(wFlagAccessori1)='CNT')   and
162200151105     C                             (%trim(wFlagAccessori2)='AF02' or
162300151105     C                              %trim(wFlagAccessori2)='AC39')
162400151105     C                   EVAL      VABTC1 = 'P'
162500151105     C                   WHEN      (%trim(wFlagAccessori1)='AFP' or
162600151105     C                              %trim(wFlagAccessori1)='CNT')   and
162700151105     C                             (%trim(wFlagAccessori2)='AF02' or
162800151105     C                              %trim(wFlagAccessori2)='AC40')
162900151105     C                   EVAL      VABTC1 = 'P'
163000151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
163100151105     C                             %trim(wFlagAccessori2)='AC41'
163200151105     C                   WHEN      %trim(wFlagAccessori1)='CNT' and
163300151105     C                             %trim(wFlagAccessori2)='AC42'
163400151105     C                   ENDSL
163500151105
163600140319     C*
163700141014     C* se la nazione è IT la sblanco
163800141112     C                   IF        VABNZD = 'IT' or
163900141112     C                             VABNZD = 'ITA'
164000141014     C                   EVAL      VABNZD = *blank
164100141014     C                   ENDIF
164200070925     C*
164300070925     C* Considerazioni finali su CBO/CAS
164400140220     C                   IF        wFlgCAS = '1'    OR
164500140220     C                             VABCAS  > *zeros
164600140220     C                   IF        VABCBO = '1'
164700140220     C                   EVAL      VABCBO = '4'
164800140220     C                   ENDIF
164900140220     C                   IF        VABCBO = '2'
165000140220     C                   EVAL      VABCBO = '6'
165100140220     C                   ENDIF
165200140220     C                   ENDIF
165300070925     C***  <===  ----------------------------
165400040510     C*
165500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
165600040510     C                   IF        VABNSP = *zeros
165700040510     C                   SETON                                        31
165800040510     C                   ENDIF
165900040715     C*
166000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
166100040715     C                   IF        VABLNP = *zeros
166200040715     C                   SETON                                        31
166300040715     C                   ENDIF
166400040510     C*
166500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
166600120411     C                   IF        VABRMN = *zeros
166700120411     C                   Z-ADD     VABNSP        VABRMN
166800120411     C                   ENDIF
166900120328     C* IN QUESTO CASO LO FORZO = 1
167000120411     C***                IF        VABRMN = *zeros
167100120411     C***                Z-ADD     1             VABRMN
167200120411     C***                ENDIF
167300040510     C*
167400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
167500040510     C                   IF        VABRMA = *blanks
167600040714     C                   MOVEL     VABRMN        VABRMA
167700040510     C                   ENDIF
167800040128     C*
167900040510     C* Se provincia nn valorizzata la reperisco
168000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
168100110912     C                   IF        VABPRD =  *blanks AND
168200110912     C                             VABNZD =  *blanks AND
168300110912     C                             VABCAD <> *blanks AND
168400110912     C                             VABLOD <> *blanks
168500040128     C                   CLEAR                   TISI95DS
168600040128     C                   EVAL      I95TCN = '3'
168700040128     C                   Z-ADD     datcor        I95DAT
168800040128     C                   EVAL      I95NAR = VABNZD
168900040128     C                   EVAL      I95CAP = VABCAD
169000040128     C                   EVAL      I95LOC = VABLOD
169100040128     C                   CALL      'TISI95R'
169200040128     C                   PARM                    TISI95DS
169300040128     C                   EVAL      VABPRD = O95PRV
169400040510     C                   ENDIF
169500020204     C*
169600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
169700020204     C                   EXSR      CHKIMPDIV
169800030715     C                   ENDIF
169900020204     C*
170000010607     C                   ENDSR
170100010607     C*----------------------------------------------------*
170200010601
170300020204
170400020204     C*----------------------------------------------------*
170500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
170600020204     C*----------------------------------------------------*
170700020204     C     CHKIMPDIV     BEGSR
170800020204     C*
170900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
171000020204     C                   Z-ADD     *zeros        wrkDec            9 9
171100020204     C*
171200020204     C* Come prima cosa effettuo considerazioni sulla divisa
171300020204     C                   IF        vabIAS > *zeros
171400020204     C                   IF        vabVAS <> 'EUR'
171500020204     C                   EVAL      vabVAS =  'ITL'
171600020204     C                   ENDIF
171700020204     C                   ENDIF
171800020204     C*
171900020204     C                   IF        vabCAS > *zeros
172000020204     C                   IF        vabVCA <> 'EUR'
172100020204     C                   EVAL      vabVCA =  'ITL'
172200020204     C                   ENDIF
172300020204     C                   ENDIF
172400020204     C*
172500020204     C                   IF        vabVMD > *zeros
172600020204     C                   IF        vabVAD <> 'EUR'
172700020204     C                   EVAL      vabVAD =  'ITL'
172800020204     C                   ENDIF
172900020204     C                   ENDIF
173000020204     C*
173100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
173200020204     C                   Z-ADD     vabIAS        wrkDec
173300020204     C                   IF        wrkDec > *zeros
173400020204     C                   IF        vabVAS = 'ITL'
173500020204     C                   EVAL      vabIAS = *zeros
173600020204     C                   ENDIF
173700020204     C                   ENDIF
173800020204     C*
173900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
174000020204     C                   Z-ADD     vabCAS        wrkDec
174100020204     C                   IF        wrkDec > *zeros
174200020204     C                   IF        vabVCA = 'ITL'
174300020204     C                   EVAL      vabCAS = *zeros
174400020204     C                   ENDIF
174500020204     C                   ENDIF
174600020204     C*
174700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
174800020204     C                   Z-ADD     vabVMD        wrkDec
174900020204     C                   IF        wrkDec > *zeros
175000020204     C                   IF        vabVAD = 'ITL'
175100020204     C                   EVAL      vabVMD = *zeros
175200020204     C                   ENDIF
175300020204     C                   ENDIF
175400020204     C*
175500020204     C                   ENDSR
175600020204     C***
175700010330
175800100318
175900100318
176000100318     C*----------------------------------------------------*
176100100318     C*  CONTROLLO NUMERICITA' CAMPI
176200100318     C*----------------------------------------------------*
176300100318     C     CHKNUM        BEGSR
176400100318     C*
176500100318     C                   IF        PiDecChr = *blanks
176600100318     C                   EVAL      PiDecChr = CharNUM
176700100318     C                   ENDIF
176800100318     C*
176900100318     C                   callp     UBISNUM_Check(PiStr
177000100318     C                                          :PiDecChr
177100100318     C                                          :PiVal
177200100318     C                                          :PiNum
177300100318     C                                          :PiInt)
177400100318     C*
177500100318     C                   ENDSR
177600100318     C***
177700010601
177800010601
177900010601
178000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
178100141006      ***
178200141014     C     repfil        BEGSR
178300141014     C*
178400141014     C                   if        invfil = *zeros and
178500141014     C                             depfil > *zeros and
178600141014     C                             (vinflg = *blanks or
178700141014     C                              vinflg = *zeros)
178800141014     C*
178900141014     C                   eval      invfil = depfil
179000141014     C                   endif
179100141014     C*
179200141014     C                   if        depfil <> invfil and
179300141014     C                             invfil > *zeros
179400141014     C                   eval      flgMulti = '1'
179500141014     C                   if        vinflg = *blanks
179600141014     C                   add       1             cntNonEl
179700141014     C                   endif
179800141014     C                   endif
179900141014     C*
180000141014     C                   if        vinflg = '2'
180100141014     C                   eval      flgStato = '2'
180200141014     C                   endif
180300141014     C*
180400141014     C                   ENDSR
180500141014     C*
180600010601
180700010601
180800010601
180900010330
181000040119      /TITLE Invio dei dati al punto operativo.
181100040119     C     invio         BEGSR
181200040119     C*
181300040119     C* 1° invio FIVAT
181400040119     C                   reset                   dscmz
181500040119     C                   move      vlrpoi        cmzdst
181600040119     C                   eval      cmzfld = 'FIVATWWR'
181700040119     C                   eval      cmzmbd = vlrhdl
181800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
181900040119     C***                if        prmfir = *blanks
182000040119     C                   eval      cmzfla = 'FIVAT00F'
182100040119     C                   eval      cmzmba = 'FIVAT00F'
182200040119     C***                else
182300040119     C***                eval      cmzfla = prmfir
182400040119     C***                eval      cmzmba = prmfir
182500040119     C***                endif
182600040119     C                   eval      cmznrr = *zeros
182700040119     C                   move      §ctrokvt      cmznrr
182800040119     C                   eval      cmzlba = vlrfl1
182900040119     C                   call(e)   'TIS711C'
183000040119     C                   parm                    dscmz
183100040119     C                   parm      *blanks       esito
183200040119     C                   if        %error
183300040119     C                             or cmzerr = '1'
183400040119     C                             or esito  = '1'
183500040119     C                   eval      wrkesito = '3'
183600040119     C                   else
183700040119     C*
183800040119     C* 2° invio FIVAB
183900040119     C                   reset                   dscmz
184000040119     C                   move      vlrpoi        cmzdst
184100040119     C                   eval      cmzfld = vlrfou
184200040119     C                   eval      cmzmbd = vlrhdl
184300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
184400040119     C***                if        prmfir = *blanks
184500040119     C                   eval      cmzfla = 'FIVAB00F'
184600040119     C                   eval      cmzmba = 'FIVAB00F'
184700040119     C***                else
184800040119     C***                eval      cmzfla = prmfir
184900040119     C***                eval      cmzmba = prmfir
185000040119     C***                endif
185100040119     C                   eval      cmznrr = *zeros
185200040119     C                   move      §ctrokvb      cmznrr
185300040119     C                   eval      cmzlba = vlrfl1
185400040119     C                   call(e)   'TIS711C'
185500040119     C                   parm                    dscmz
185600040119     C                   parm      *blanks       esito
185700040119     C                   if        %error
185800040119     C                             or cmzerr = '1'
185900040119     C                             or esito  = '1'
186000040119     C                   eval      wrkesito = '3'
186100040119     C                   endif
186200040119     C                   endif
186300040119     C*
186400040119     C                   ENDSR
186500040119     C***
186600010601
186700010601
186800010601
186900010601
187000010601      /TITLE Invio dei dati al punto operativo.
187100010601     C     opeini        BEGSR
187200010601     C*
187300010601     C* Inizializzo flag e contatori operativi
187400010601     C                   movel     '0'           flgGiro           1
187500010601     C                   movel     '0'           flgMulti          1
187600010601     C                   movel     '1'           flgStato          1
187700010615     C                   movel     '0'           flgOk             1
187800010601     C                   z-add     *zeros        cntNonEl         10 0
187900010601     C                   z-add     *zeros        depfil            3 0
188000010601     C                   z-add     *zeros        invfil            3 0
188100010601     C*
188200010601     C                   ENDSR
188300010601     C***
188400070326
188500070326
188600070326
188700070326
188800070326     C     *pssr         BEGSR
188900070329     C*
189000070329     C                   if        %open(tivin00r)
189100070329     C                   close     tivin00r
189200070329     C                   endif
189300070329     C                   if        %open(fivabwwr)
189400070329     C                   close     fivabwwr
189500070329     C                   endif
189600070329     C                   if        %open(fivatwwr)
189700070329     C                   close     fivatwwr
189800070329     C                   endif
189900070326     C*
190000070326     C* Effettuo la chiamata al CLLE preposto
190100070326     C                   call(e)   'TITVVTC'
190200070326     C                   parm                    parccm
190300070326     C                   parm                    parmbr
190400070326     C                   parm      '2'           paropz
190500070326     C*
190600070326     C                   eval      wrkesito = '2'
190700070404     C*
190800070404     C                   seton                                        LR
190900070326     C*
191000070326     C                   ENDSR     '*CANCL'
191100070326     C***
191200100318
191300100318
191400100318
191500100318     C     normCSV       BEGSR
191600100318     C*
191700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
191800100318     C* separatori campi
191900100318     C                   if        CharTXT <> *blanks
192000100318     C                   z-add     *zeros        wLenVINDTA        4 0
192100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
192200100318     C                   setoff                                       80
192300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
192400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
192500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
192600100318     C                   if        not *in80
192700100318     C                   seton                                        80
192800100318     C                   else
192900100318     C                   setoff                                       80
193000100318     C                   endif
193100100318     C                   endif
193200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
193300100318     C                             and *in80 = *on
193400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
193500100318     C                   endif
193600100318     C                   enddo
193700100318     C                   endif
193800100318     C*
193900100318     C                   ENDSR
194000100318     C***
194100070326
194200100326
194300100326
194400100326     C*----------------------------------------------------*
194500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
194600100326     C*----------------------------------------------------*
194700100326     C     REPNSP        BEGSR
194800100326     C*
194900100326     C* NSP => Stacco un numeratore da AZNUM
195000100326     C                   clear                   TRUL33DS
195100100326     C                   eval      I33OPE = *zeros
195200100326     C                   eval      I33CNU = 302
195300100326     C                   eval      I33NUM = 1
195400100326     C                   movel     TRUL33DS      KPJBU
195500100326     C                   call      'TRUL33R'
195600100326     C                   parm                    KPJBA
195700100326     C                   movel     KPJBU         TRUL33DS
195800100326     C                   if        O33ERR = *zeros
195900100326     C                   z-add     O33NRF        VABNSP
196000100326     C                   z-add     O33NRF        VATNSP
196100100326     C                   else
196200100326     C                   SETON                                        31
196300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
196400140711     C                             + ' ' + 'VABNSP VATNSP'
196500100326     C                   endif
196600100326     C*
196700100326     C                   ENDSR
196800010330
196900100326
197000010330
197100000613     C     *inzsr        BEGSR
197200990910     C*
197300990910     C     *entry        plist
197400990920     C                   parm                    tivlrds
197500990921     C                   parm      wrkesito      esito
197600000724     C                   parm                    prmlit
197700000710     C                   parm                    prmfir
197800010330     C*
197900010330     C* CALCOLA LA DATA CORRENTE
198000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
198100100318     C                   eval      datcor = %dec(%date() : *ISO)
198200000613     C*
198300000613     C                   ENDSR
198400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
198500151105VABRSD;W_VATNOT_A;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VATNOT_B;X;VATNOT_S;VATNOT_IJ;X;X;W_VABTC1;
198600151105W_VABTC2;W_VATNOT_E;VABNCL;VABPKB;VABCAS;X;X;VABNAS;X;X;X;X;X;VABVMD;VABIAS;W_VABNOT;
198700151105X;X;X;X;X;X;X;X;
198800111116
198900100318
