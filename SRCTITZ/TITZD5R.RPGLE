000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400140930      * - Il campo 1 indica anche il numero di spedizione cliente sul quale effettuare una rottura
000500140930      * di codice
000600140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
000700140930      * spedizione
000800140930      *
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
002000010330     D*----------------------------------------------------
002100010330     D* DICHIARAZIOINE VARIABILI DI WRK
002200010330     D*----------------------------------------------------
002300010330     D dscmz         e ds                  inz
002400010330     D psds           sds
002500010330     D  procname         *PROC
002600010330     D tivlrds       e ds                  extname(tivlr00f)
002700040128     D tisi95ds      e ds
002800100413     D fivabds       e ds                  extname(fivab00f)
002900100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003000100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
004200120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004300120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004400140930     D Intestazioni    s               n   inz(*on)
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
007100060331     D wVATNOT_E       s                   LIKE(VATNOT)
007200110927     D wVATNOT_I       s                   LIKE(VATNOT)
007300140220     D wVATNOT_J       s                   LIKE(VATNOT)
007400140220     D wVATNOT_IJ      s             70
007500140319     D wVATNOT_S       s                   LIKE(VATNOT)
007600140919     D w70             s             70
007700140919     D wNOT            s             70
007800140919     D wRSD1           S            256
007900140919     D wRSD2           S            256
008000140919     D wRSD3           S            256
008100120411     D Data8A          s              8
008200140527     D wBarcodeN       s             11s 0
008300140527     D wX              s              3s 0
008400140711     D wRMA            s                   like(VABRMA)
008500140711     D TxtInOut        S           2048
008600140711     D ElencoChar      S            256
008700140711     D TipoElenco      S              1
008800140711     D CharSost        S              1
008900140711     D UpperCase       S              1
009000140711     D ChkNull         S              1
009100140711     D CharNull        S              1
009200140711     D EsitoBon        S              1
009300030822     D*------------------
009400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009500030822     D*------------------
009600030822     D posDaDft        S              3  0 INZ(*zeros)
009700030822     D posADft         S              3  0 INZ(*zeros)
009800030822     D j               s              3  0 INZ(1)
009900030822     D wGiroDft        s              1  0 INZ(*zeros)
010000080418     D*------------------
010100080418     D* Costanti
010200080418     D*------------------
010300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010500100318
010600100318     D*------------------
010700100318     D* LINKING A DEFINIZIONI ESTERNE
010800100318     D*------------------
010900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
011100100318
011200010330
011300010330
011400010330
011500000913     C                   reset                   rrnum
011600990921     C                   reset                   esito
011700990921     C                   reset                   wrkesito
011800010601     C*
011900010601     C                   exsr      opeini
012000010605     C                   exsr      rwvab
012100010601     C*
012200010601     C                   seton                                        lr
012300010601
012400010601
012500010601
012600010601     C*--------------------------------------------------------
012700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012800010601     C*--------------------------------------------------------
012900010601     C     PREELA        BEGSR
013000010601     C*
013100010601     C* SE OCCORRE SPEDIRE IN FILIALE
013200010601     C                   if        invfil <> *zeros and
013300010601     C                             flgGiro = '0'
013400010601     C*
013500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013600010601     C                   eval      flgGiro = '1'
013700010601     C*
013800010601     C                   endif
013900010601     C*
014000010601     C                   ENDSR
014100010601     C***
014200010601
014300010601
014400010601
014500010601
014600010601     C*--------------------------------------------------------
014700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014800010601     C*--------------------------------------------------------
014900010601     C     ENDELA        BEGSR
015000040119     C*
015100040119     C* Effettuo la chiamata al CLLE preposto
015200040119     C                   call(e)   'TITVVTC'
015300040510     C                   parm                    parccm
015400040119     C                   parm                    parmbr
015500040119     C                   parm      '2'           paropz
015600050201     C*
015700050201     C* Effettuo lancio TISI95 solo x chiusura
015800050201     C                   CLEAR                   TISI95DS
015900050201     C                   EVAL      I95TLA = 'C'
016000050201     C                   CALL      'TISI95R'
016100050201     C                   PARM                    TISI95DS
016200000616     C*
016300010601     C                   ENDSR
016400010601
016500010601
016600010330     C*--------------------------------------------------------
016700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016800010330     C*--------------------------------------------------------
016900010605     C     RWVAB         BEGSR
017000010330     C*
017100010330     C                   if        not %open(tivin00r)
017200010330     C                   open      tivin00r
017300010330     C                   endif
017400021025     C                   if        not %open(fivabwwr)
017500021025     C                   open      fivabwwr
017600010330     C                   endif
017700040119     C*
017800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017900040119     C                   exsr      prevat
018000040119     C*
018100040119     C                   if        chkcall = '0'
018200040119     C*
018300040119     C                   if        not %open(fivatwwr)
018400040119     C                   open      fivatwwr
018500040119     C                   endif
018600010330     C*
018700040119     C                   clear                   §CTROKVB          7 0
018800040119     C                   clear                   §CTROKVT          7 0
018900040119     C                   clear                   §CTRMO            7 0
019000040119     C                   clear                   §CTRNO            7 0
019100120410     C*
019200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019400120410     C                   IF        Intestazioni = *off
019500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019700120410     C                   exsr      inzvar
019800120410     C                   exsr      impvab
019900120410     C                   ENDIF
020000010330     C*
020100030822     C                   do        *HIVAL
020200010330     C*
020300030822     C                   read      tivin00r                               70
020400010618     C*
020500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020600010618     C                   if        vinflg = '1'
020700010618     C                   eval      flgOk = '1'
020800010618     C                   endif
020900040510     C*
021000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021100040510     C                   z-add     *zeros        wLenVINDTA        4 0
021200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021600040510     C                   z-add     1             wFlgVINDTA
021700040510     C                   leave
021800040510     C                   endif
021900040510     C                   enddo
022000010618     C*
022100040510     C                   if        vindta > *blanks AND
022200040510     C                             wFlgVINDTA = 1
022300010330     C                   add       1             rrnum
022400010330     C*
022500010601     C                   if        *in70 = *off and
022600010330     C                             (vinflg = *blanks
022700010330     C                              or vinflg = '0'
022800010330     C                              or vinflg = '2')
022900010330     C*
023000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023100010711     C                   if        vinflg = *blanks or vinflg = '0'
023200010711     C                   clear                   vinmsg
023300010711     C                   endif
023400130917     C*
023500130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023600130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023700130917     C* evitare che vengano elisi i blank
023800140220     C***                eval      vindta  = '*' + %trim(vindta)
023900130917     C*
024000080527     C                   exsr      inzvar
024100100318     C                   exsr      normCSV
024200010605     C                   exsr      impvab
024300010601     C*
024400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024500010605     C                   eval      depfil = VABLNP
024600010601     C                   exsr      repfil
024700010601     C                   if        depfil = invfil
024800021025     C                   if        vlrpoi = 999
024900030822     C                   move(P)   invfil        VABFGS
025000021025     C                   else
025100030822     C                   move(P)   vlrpoi        VABFGS
025200021025     C                   endif
025300010601     C*
025400010601     C                   exsr      PREELA
025500010601     C*
025600010604     C* Ebbene...
025700010604     C*
025800040119     C  N31              add       1             §CTROKVB
025900040119     C   32              add       1             §CTRMO
026000040119     C   31              add       1             §CTRNO
026100080418     C*
026200030822     C                   if        wGiro = 2
026300080513     C* Scrivo sempre il VAT
026400080513     C  N31              exsr      wrivat
026500110201     C                   exsr      chkvab
026600080513     C                   endif
026700010604     C*
026800010604     C                   if        *in31 = *off and
026900010604     C                             *in32 = *off
027000010604     C                   eval      vinflg = '1'
027100010604     C                   else
027200010604     C                   eval      vinflg = '2'
027300010604     C                   endif
027400010604     C                   endif
027500010601     C*
027600010604     C                   endif
027700010604     C*
027800010330     C                   else
027900010330     C                   eval      vinflg = '1'
028000010330     C                   endif
028100010601     C*
028200010601     C  N70              update    tivin000
028300010330     C*
028400030822     C  N70              enddo
028500040119     C*
028600080527     C* Verifico dati rimasti "in cannna"...
028700080527     C                   seton                                        65
028800110201     C                   exsr      chkvab
028900080527     C*
029000040119     C                   endif                                                  (endif - chkcall)
029100010601     C*
029200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029300010601     C                   if        cntNonEl = *zeros or
029400010601     C                             flgMulti = '0'
029500010330     C* Se non ci sono record con errori ...
029600010601     C                   if        §ctrno = 0 and
029700010604     C                             §ctrmo = 0 and
029800010601     C                             flgStato <> '2'
029900010330     C* ... restituisco esito OK.
030000010330     C                   eval      wrkesito = '0'
030100010330     C                   else
030200040119     C                   if        §ctrokvb > 0
030300010330     C                   eval      wrkesito = '1'
030400010330     C                   else
030500010615     C                   if        flgOk = '0'
030600010615     C                   eval      wrkesito = '2'
030700010615     C                   else
030800010615     C                   eval      wrkesito = '6'
030900010615     C                   endif
031000010330     C                   endif
031100010330     C                   endif
031200010601     C                   else
031300010601     C                   eval      wrkesito = '9'
031400010601     C                   endif
031500010330     C*
031600010330     C                   if        %open(tivin00r)
031700010330     C                   close     tivin00r
031800010330     C                   endif
031900021025     C                   if        %open(fivabwwr)
032000021025     C                   close     fivabwwr
032100010330     C                   endif
032200040512     C                   if        %open(fivatwwr)
032300040512     C                   close     fivatwwr
032400040512     C                   endif
032500010601     C*
032600010601     C                   if        vlrpoi <> 999
032700010601     C                   eval      invfil = vlrpoi
032800010601     C                   endif
032900010330     C*
033000040119     C                   if        §ctrokvb > 0
033100010601     C                             and invfil > *zeros
033200010330     C                   exsr      invio
033300010330     C                   endif
033400010601     C*
033500010618     C                   if        flgGiro = '1'
033600010601     C                   exsr      endela
033700010618     C                   endif
033800010330     C*
033900010330     C                   ENDSR
034000010330     C***
034100080527
034200080527
034300080527
034400080527     C*----------------------------------------------------*
034500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034600080527     C*----------------------------------------------------*
034700100413     C     CHKVAB        BEGSR
034800110201     C*
034900110201     C* Se richiesti ignorare colli / peso / volume
035000110315 xxx C***                if        *in35
035100110315 xxx C***                z-add     *zeros        VABNCL
035200110315 xxx C***                z-add     *zeros        VABPKB
035300110315 xxx C***                z-add     *zeros        VABVLB
035400110315 xxx C***                endif
035500080527     C*
035600080527     C* Scrivo il VAB solo a rottura d spedizione
035700080527     C                   if        depspe = *zeros
035800140930
035900140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036000140930      * spedizione
036100140930     C  N31              eval      vabncl = 1
036200140930
036300080527     C  N31              eval      fivabds_sav = fivabds
036400080527     C                   eval      depspe = vabNSP
036500080527     C                   else
036600080527     C                   if        depspe = vabNSP AND *in65 = *off
036700140930
036800140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036900140930      * spedizione
037000140930     C                   eval      vabncl += 1
037100140930
037200100413     C   51              add       VABNCL        sav_VABNCL
037300100413     C   51              add       VABPKB        sav_VABPKB
037400100413     C   51              add       VABVLB        sav_VABVLB
037500100720     C   52              z-add     VABNCL        sav_VABNCL
037600100720     C   52              z-add     VABPKB        sav_VABPKB
037700100720     C   52              z-add     VABVLB        sav_VABVLB
037800100720     C   53              add       VABNCL        sav_VABNCL
037900100720     C   53              z-add     VABPKB        sav_VABPKB
038000100720     C   53              z-add     VABVLB        sav_VABVLB
038100080527     C                   else
038200080527     C  N31              eval      fivabds_ok = fivabds_sav
038300080527     C  N31              exsr      wrivab
038400080527     C  N31              eval      fivabds_sav = fivabds
038500140930
038600140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
038700140930      * spedizione
038800140930     C  N31              eval      vabncl = 1
038900140930
039000080527     C                   eval      depspe = vabNSP
039100080527     C                   endif
039200080527     C                   endif
039300080527     C*
039400080527     C                   ENDSR
039500040119
039600040119
039700040119
039800040119     C*----------------------------------------------------*
039900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040000040119     C*----------------------------------------------------*
040100040510     C     WRIVAB        BEGSR
040200080513     C*
040300080513     C* Gestisco l'eventuale rottura x numero spedizione
040400080527     C                   if        VABNSP <> depspe or *in65 = *on
040500070301     C                   if        wVATNOT_E <> *blanks
040600130610     C                   eval      ok_VABCTM = '7Q'
040700070301     C                   endif
040800100413     C                   write     fivab000
040900080513     C                   endif
041000040119     C*
041100040119     C                   ENDSR
041200040119
041300040119
041400040119
041500040119     C*----------------------------------------------------*
041600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041700040119     C*----------------------------------------------------*
041800040510     C     WRIVAT        BEGSR
041900060512     C*
042000060512     C* Inizializzo i campi chiave
042100060512     C                   eval      VATFGS = VABFGS
042200060512     C                   eval      VATAAS = VABAAS
042300060512     C                   eval      VATCCM = VABCCM
042400070823     C                   eval      VATNRS = VABNRS
042500060512     C                   eval      VATNSP = VABNSP
042600060512     C                   eval      VATLNP = VABLNP
042700040119     C*
042800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042900040510     C                   if        VATTRC = *blanks
043000040510     C                   eval      VATTRC = 'E'
043100040510     C                   endif
043200110318     C                   if        VATNOT <> *blanks
043300040510     C                   write     FIVAT000
043400040119     C                   add       1             §CTROKVT
043500110318     C                   endif
043600060331     C*
043700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043800060331     C                   if        wVATNOT_A <> *blanks
043900060331     C                   eval      VATNOT = wVATNOT_A
044000060331     C                   eval      VATTRC = 'A'
044100060331     C                   write     FIVAT000
044200060331     C                   add       1             §CTROKVT
044300060331     C                   endif
044400060331     C*
044500060331     C                   if        wVATNOT_B <> *blanks
044600060331     C                   eval      VATNOT = wVATNOT_B
044700060331     C                   eval      VATTRC = 'B'
044800060331     C                   write     FIVAT000
044900060331     C                   add       1             §CTROKVT
045000060331     C                   endif
045100140711     C*
045200140711     C                   if        wVATNOT_IJ <> *blanks
045300140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045400140711     C                   eval      VATTRC = 'I'
045500140711     C                   write     FIVAT000
045600140711     C                   add       1             §CTROKVT
045700140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045800140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045900140711     C                   eval      VATTRC = 'J'
046000140711     C                   write     FIVAT000
046100140711     C                   endif
046200140711     C                   endif
046300060331     C*
046400060331     C                   if        wVATNOT_E <> *blanks
046500060331     C                   eval      VATTRC = 'E'
046600140527     C*
046700140711     C* NON Sviluppare i "CHI SONO"
046800140527     C*
046900140711     C                   EVAL      VATNOT = wVATNOT_E
047000140711     C                   write     FIVAT000
047100140711     C                   add       1             §CTROKVT
047200120320     C                   endif
047300110927     C*
047400110927     C                   if        wVATNOT_I <> *blanks
047500110927     C                   eval      VATNOT = wVATNOT_I
047600140711     C                   eval      VATTRC = 'I'
047700110927     C                   write     FIVAT000
047800110927     C                   add       1             §CTROKVT
047900110927     C                   endif
048000140220     C*
048100140220     C                   if        wVATNOT_J <> *blanks
048200140220     C                   eval      VATNOT = wVATNOT_J
048300140220     C                   eval      VATTRC = 'J'
048400140220     C                   write     FIVAT000
048500140220     C                   add       1             §CTROKVT
048600140220     C                   endif
048700140319     C*
048800140319     C                   if        wVATNOT_S <> *blanks
048900140319     C                   eval      VATNOT = wVATNOT_S
049000140319     C* i 2 flag devono essere blank
049100140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049200140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049300140319     C                   eval      VATTRC = 'S'
049400140319     C                   write     FIVAT000
049500140319     C                   add       1             §CTROKVT
049600140319     C                   endif
049700040119     C*
049800040119     C                   ENDSR
049900040119
050000040119
050100040119
050200040119     C*----------------------------------------------------*
050300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050400040119     C*----------------------------------------------------*
050500040119     C     PREVAT        BEGSR
050600040119     C*
050700040119     C* Compongo il nome del membro da dare al FIVATWWR
050800040119     C                   eval      parmbr = vlrhdl
050900040119     C                   movel     'M'           parmbr
051000040510     C                   eval      parccm = vlrksc
051100040119     C                   eval      paropz = '1'
051200040119     C* Effettuo la chiamata al CLLE preposto
051300040119     C                   call(e)   'TITVVTC'
051400040119     C                   parm                    parccm
051500040119     C                   parm                    parmbr
051600040119     C                   parm                    paropz
051700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051800040119     C                   if        %error
051900040119     C                   movel     '1'           chkcall
052000040119     C                   else
052100040119     C                   movel     '0'           chkcall
052200040119     C                   endif
052300040119     C*
052400040119     C                   ENDSR
052500010601
052600010601
052700010601
052800010330     C*----------------------------------------------------*
052900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053000010330     C*----------------------------------------------------*
053100040119     C     INZVAR        BEGSR
053200080527     C*
053300080527     C                   setoff                                       65
053400010330     C*
053500030822     C* Inizializzo variabili di wrk
053600020204     C                   Z-ADD     *zeros        Num5_0            5 0
053700101018     C                   MOVEL     *blanks       wFlgCAS
053800060331     C                   MOVEL     *blanks       wVATNOT_A
053900060331     C                   MOVEL     *blanks       wVATNOT_B
054000060331     C                   MOVEL     *blanks       wVATNOT_E
054100110927     C                   MOVEL     *blanks       wVATNOT_I
054200140220     C                   MOVEL     *blanks       wVATNOT_J
054300140220     C                   MOVEL     *blanks       wVATNOT_IJ
054400140319     C                   MOVEL     *blanks       wVATNOT_S
054500120411     C                   MOVEL     *blanks       wNOT
054600120411     C                   MOVEL     *blanks       Data8A
054700030822     C*
054800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054900030822     C                   CLEAR                   SkSplitCSV
055000030822     C                   EVAL      i = 1
055100030822     C                   EVAL      posDa = *zeros
055200030822     C                   EVAL      posA  = *zeros
055300080527     C*
055400080527     C                   CLEAR                   FIVABDS
055500080527     C                   CLEAR                   FIVAT000
055600030822     C*
055700030822     C* Reimposto i valori di default
055800030822     C                   EXSR      DEFCAM
055900030822     C*
056000010330     C                   ENDSR
056100010330     C*----------------------------------------------------*
056200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056300010330     C*----------------------------------------------------*
056400010330     C     DEFCAM        BEGSR
056500030822     C*
056600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056700030715     C* e delimitatore testo.
056800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057100100318     C*
057200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057300100318     C                   EVAL      CharSOS = CharNUM
057400030822     C*
057500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057600100720     C                   SETOFF                                       505152
057700100720     C                   SETOFF                                       53
057800100413     C                   SELECT
057900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058000100720     C                   SETON                                        50
058100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058200100413     C                   SETON                                        51
058300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058400100720     C                   SETON                                        52
058500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058600100720     C                   SETON                                        53
058700100413     C                   ENDSL
058800030822     C*
058900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059000030822     C                   EVAL      posDaDft = 1
059100030822     C                   EVAL      posADft  = 0
059200030822     C                   EVAL      wGiroDft = 0
059300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059400030822     C                             posDaDft > 0
059500030822     C*
059600030822     C* Gestisco il 1° giro
059700030822     C                   IF        wGiroDft = 0
059800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
059900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060000030822     C* Incremento il contatore dei "giri"
060100030822     C                   EVAL      wGiroDft = 1
060200030822     C                   ELSE
060300030822     C                   EVAL      posDaDft = posADft
060400030822     C                   ENDIF
060500030822     C* Eseguo lo scan x trovare la fine del campo corrente
060600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060700030822     C*
060800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
060900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061000030822     C                   IF        posDaDft > 0 AND
061100030822     C                             posADft  > 0
061200030822     C* NCL
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'NCL'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiInt=*on
062200030822     C                   Z-ADD     PiVal         VABNCL
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* CCM
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'CCM'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiInt=*on
063500030822     C                   Z-ADD     PiVal         VABCCM
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* LNP
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'LNP'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiInt=*on
064800030822     C                   Z-ADD     PiVal         VABLNP
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* NRS
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'NRS'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiInt=*on
066100030822     C                   Z-ADD     PiVal         VABNRS
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* CTR
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'CTR'
066900030822     C                   EVAL      PiStr=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   EXSR      CHKNUM
067300030822     C                   IF        PiInt=*on
067400030822     C                   Z-ADD     PiVal         VABCTR
067500030822     C                   ENDIF
067600030822     C                   ENDIF
067700030822     C* PKB
067800030822     C                   IF        %subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):1:3)
068100030822     C                             = 'PKB'
068200030822     C                   EVAL      PiStr=%trim(%subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068500030822     C                   EXSR      CHKNUM
068600030822     C                   IF        PiNum=*on
068700130610     C                   Z-ADD     PiVal         VABPKB
068800030822     C                   ENDIF
068900030822     C                   ENDIF
069000030822     C* VLB
069100030822     C                   IF        %subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):1:3)
069400030822     C                             = 'VLB'
069500030822     C                   EVAL      PiStr=%trim(%subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069800030822     C                   EXSR      CHKNUM
069900030822     C                   IF        PiNum=*on
070000130610     C                   Z-ADD     PiVal         VABVLB
070100030822     C                   ENDIF
070200030822     C                   ENDIF
070300030822     C* QFT
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'QFT'
070800030822     C                   EVAL      PiStr=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   EXSR      CHKNUM
071200030822     C                   IF        PiNum=*on
071300030822     C                   Z-ADD     PiVal         VABQFT
071400030822     C                   ENDIF
071500030822     C                   ENDIF
071600030822     C* CBO
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'CBO'
072100030822     C                   EVAL      VABCBO=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* TSP
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'TSP'
073000030822     C                   EVAL      VABTSP=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* VAS
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'VAS'
073900030822     C                   EVAL      VABVAS=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* VCA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'VCA'
074800030822     C                   EVAL      VABVCA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200070925     C* TIC
075300070925     C                   IF        %subst(
075400070925     C                             %subst(vlrppt:posDaDft+1:
075500070925     C                             posADft-posDaDft-1):1:3)
075600070925     C                             = 'TIC'
075700070925     C                   EVAL      VABTIC=%trim(%subst(
075800070925     C                             %subst(vlrppt:posDaDft+1:
075900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000070925     C                   ENDIF
076100030822     C* GCA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GCA'
076600030822     C                   EVAL      VABGCA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* CTM
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'CTM'
077500030822     C                   EVAL      VABCTM=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* FFD
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'FFD'
078400030822     C                   EVAL      VABFFD=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* VAD
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'VAD'
079300030822     C                   EVAL      VABVAD=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* GMA
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'GMA'
080200030822     C                   EVAL      VABGMA=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600030822     C* GGA
080700030822     C                   IF        %subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):1:3)
081000030822     C                             = 'GGA'
081100030822     C                   EVAL      VABGGA=%trim(%subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400030822     C                   ENDIF
081500030822     C* GVA
081600030822     C                   IF        %subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):1:3)
081900030822     C                             = 'GVA'
082000030822     C                   EVAL      VABGVA=%trim(%subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082300030822     C                   ENDIF
082400030822     C* TC1
082500030822     C                   IF        %subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):1:3)
082800030822     C                             = 'TC1'
082900030822     C                   EVAL      VABTC1=%trim(%subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083200030822     C                   ENDIF
083300030822     C* TC2
083400030822     C                   IF        %subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):1:3)
083700030822     C                             = 'TC2'
083800030822     C                   EVAL      VABTC2=%trim(%subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084100030822     C                   ENDIF
084200040714     C* VATTRC
084300040714     C                   IF        %subst(
084400040714     C                             %subst(vlrppt:posDaDft+1:
084500040714     C                             posADft-posDaDft-1):1:3)
084600040714     C                             = 'TRC'
084700040714     C                   EVAL      VATTRC=%trim(%subst(
084800040714     C                             %subst(vlrppt:posDaDft+1:
084900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085000040714     C                   ENDIF
085100030822     C* ...
085200030822     C                   ENDIF
085300030822     C                   ENDDO
085400020204     C*
085500010330     C                   ENDSR
085600010607     C*----------------------------------------------------*
085700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
085800010607     C*----------------------------------------------------*
085900010607     C     IMPVAB        BEGSR
086000010607     C*
086100010607     C                   SETOFF                                       3132
086200110315 xxx C***                SETON                                        35
086300030515     C*
086400010607     C                   MOVEL     datcor        VABAAS
086500010607     C                   MOVE      datcor        VABMGS
086600010607     C*
086700030715     C**********
086800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086900030715     C**********
087000030715     C                   IF        wGiro = *zeros
087100040113     C*
087200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087500080429     C                             %trim(intcol(5))<>*blanks
087600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087800080429     C                                      %trim(intcol(5))
087900070823     C     ';':CharCSV   XLATE     vindta        vindta
088000040510     C                   ENDIF
088100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088200031201     C     minu:maiu     XLATE     vindta        vindta
088300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088400030715     C                   DOW       posDa <= %len(%trim(vindta))
088500030715     C*
088600030715     C* Gestisco il 1° campo
088700030715     C                   IF        i = 1
088800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088900030715     C                   EVAL      posDa = 1
089000030715     C* Eseguo lo scan x trovare la fine del primo campo
089100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C* X i campi successivi al 1°
089600030715     C                   ELSE
089700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089800030715     C                   EVAL      posDa = posA + 1
089900030715     C* Eseguo lo scan x trovare la fine del campo corrente
090000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090100030715     C* Gestisco l'ultimo campo
090200030715     C                   IF        posA = *zeros
090300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C                   LEAVE
090800030715     C                   ELSE
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   ENDIF
091300030715     C                   ENDIF
091400030715     C* Incremento il contatore d campo
091500030715     C                   EVAL      i = i +1
091600030715     C                   ENDDO
091700030715     C*
091800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091900030715     C* delimitazione testo
092000030715     C                   EVAL      i = 1
092100030715     C                   DOW       i < %elem(SkSplitFLD)
092200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092400030715     C                   EVAL      i = i + 1
092500030715     C                   ENDDO
092600030715     C                   EVAL      wGiro = 1
092700030715     C                   ELSE
092800030715     C                   EVAL      wGiro = 2
092900031204     C**********
093000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093100031204     C* d separatore campo
093200031204     C**********
093300031204     C                   EVAL      vindta = %trim(vindta)
093400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093500031204     C                   EVAL      vindta = %subst(vindta:2)
093600031204     C                   ENDDO
093700031204     C*
093800031204     C                   Z-ADD     *zeros        lunghInput        4 0
093900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094300110912     C                   ENDDO
094400030715     C**********
094500030715     C* Effettuo lo split del campo dati d input
094600030715     C**********
094700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094800030715     C                   DOW       posDa <= %len(%trim(vindta))
094900030715     C*
095000030715     C* Gestisco il 1° campo
095100030715     C                   IF        i = 1
095200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095300030715     C                   EVAL      posDa = 1
095400030715     C* Eseguo lo scan x trovare la fine del primo campo
095500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095600030715     C* A questo punto "estraggo" il campo corrente
095700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095800030715     C                                                    (posA-posDa))
095900030715     C* X i campi successivi al 1°
096000030715     C                   ELSE
096100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096200030715     C                   EVAL      posDa = posA + 1
096300031204     C* Verifico che nn vi sia il campo nn valorizzato
096400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096500031204     C* Se campo nn valorizzato skippo al prossimo
096600031204     C                   EVAL      SkSplitCSV(i) = *blanks
096700031204     C                   EVAL      posA  = posA + 1
096800031204     C                   ELSE
096900030715     C* Eseguo lo scan x trovare la fine del campo corrente
097000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097100030715     C* Gestisco l'ultimo campo
097200030715     C                   IF        posA = *zeros
097300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097400030715     C* A questo punto "estraggo" il campo corrente
097500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097600030715     C                                                    (posA-posDa))
097700030715     C                   LEAVE
097800030715     C                   ELSE
097900030715     C* A questo punto "estraggo" il campo corrente
098000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098100030715     C                                                    (posA-posDa))
098200030715     C                   ENDIF
098300031204     C                   ENDIF
098400030715     C                   ENDIF
098500030715     C* Incremento il contatore d campo
098600030715     C                   EVAL      i = i +1
098700030715     C                   ENDDO
098800030715     C*
098900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099000030715     C* delimitazione testo
099100030715     C                   EVAL      i = 1
099200030715     C                   DOW       i < %elem(SkSplitCSV)
099300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099500030715     C                   EVAL      i = i + 1
099600030715     C                   ENDDO
099700100720     C*
099800100720     C* Stacco un progressivo x numero spedizione
099900140711 xxx C***                EXSR      repNSP
100000100326     C*
100100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100200030715     C                   EVAL      i = 1
100300030715     C                   DOW       i < %elem(SkSplitFLD)
100400030715     C*
100500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100700030715     C                             SkSplitCSV(i) <> *blanks
100800030721     C*** ATB
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101000030721     C                   EVAL      VABATB = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030715     C*** RSD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101700030715     C* ==
101800130528     C                   ENDIF
101900030721     C*** RD2
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** IND
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102500030715     C                   EVAL      VABIND = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** LOD
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
102900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** PRD
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030721     C*** NZD
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030715     C*** RMA
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** NOT
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030715     C*** NT2
104800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
104900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105000030715     C                   ENDIF
105100030715     C*** VCA
105200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105400030715     C                   ENDIF
105500030715     C*** VAS
105600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105800030715     C                   ENDIF
105900030715     C*** FFD
106000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106200030715     C                   ENDIF
106300030721     C*** GC1
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GC2
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
106900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TSP
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107400030721     C                   ENDIF
107500040510     C*** CBO
107600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107800040510     C                   ENDIF
107900030721     C*** NAS
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TIC
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** GCA
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
108900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** XCO
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** CTM
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** TCR
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** CTS
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** FTM
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
110900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** VAD
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** GMA
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** GGA
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** GVA
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** TC1
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
112900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*** TC2
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113400030721     C                   ENDIF
113500030721     C*** SCL
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113800030721     C                   ENDIF
113900030721     C*** RMO
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114200030721     C                   ENDIF
114300030721     C*** NMO
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114600030721     C                   ENDIF
114700030721     C*
114800030721     C* Reperisco quindi i campi numerici...
114900040510     C*** NSP
115000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115100040510     C                   EVAL      PiStr=SkSplitCSV(i)
115200040510     C                   EXSR      CHKNUM
115300040510     C                   IF        PiInt=*on
115400040510     C                   Z-ADD     PiVal         VABNSP
115500040510     C                   ELSE
115600040510     C                   SETON                                        31
115700040510     C                   EVAL      VABNSP = *zeros
115800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
115900040510     C                             + ' ' + 'VABNSP'
116000040510     C                   ENDIF
116100040510     C                   ENDIF
116200071106     C*** RMN
116300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116400071106     C                   EVAL      PiStr=SkSplitCSV(i)
116500071106     C                   EXSR      CHKNUM
116600071106     C                   IF        PiInt=*on
116700071106     C                   Z-ADD     PiVal         VABRMN
116800071106     C                   ELSE
116900071106     C                   SETON                                        32
117000071106     C                   EVAL      VABRMN = *zeros
117100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117200071106     C                             + ' ' + 'VABRMN'
117300071106     C                   ENDIF
117400071106     C                   ENDIF
117500030721     C*** CAD
117600130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117700130610     C                   EVAL      PiStr=SkSplitCSV(i)
117800130610     C                   EXSR      CHKNUM
117900130610     C                   IF        PiInt=*on
118000130610     C                   Z-ADD     PiVal         Num5_0
118100130610     C                   MOVEL(p)  Num5_0        VABCAD
118200130610     C                   ELSE
118300130610     C                   SETON                                        32
118400130610     C                   EVAL      VABCAD = *zeros
118500130610     C                   EVAL      vinmsg = %trimr(vinmsg)
118600130610     C                             + ' ' + 'VABCAD'
118700130610     C                   ENDIF
118800130605     C                   ENDIF
118900030721     C*** NCL
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on
119400030721     C                   Z-ADD     PiVal         VABNCL
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABNCL = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040122     C                             + ' ' + 'VABNCL'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** PKB
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiNum=*on
120700130610     C                   Z-ADD(H)  PiVal         VABPKB
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABPKB = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABPKB'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** CAS
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
121700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
121800111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
121900140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122000140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122100140711     C                             %trim(SkSplitCSV(i)) <> ',00'
122200130703     C                   EVAL      VABVCA = 'EUR'
122300140930     C                   EVAL      VABTIC = *blank
122400140513     C                   EVAL      wFlgCAS = '1'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiNum=*on
122800030721     C                   Z-ADD     PiVal         VABCAS
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      VABCAS = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABCAS'
123400030721     C                   ENDIF
123500140513     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** CCM
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on      AND
124200030721     C                             PiVal<=9999999 AND
124300030721     C                             PiVal>*zeros
124400030721     C                   Z-ADD     PiVal         VABCCM
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      VABCCM = *zeros
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900040510     C                             + ' ' + 'VABCCM'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** LNP
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on    AND
125700030721     C                             PiVal<=999   AND
125800030721     C                             PiVal>*zeros
125900030721     C                   Z-ADD     PiVal         VABLNP
126000030721     C                   ELSE
126100040715     C                   SETON                                        32
126200030721     C                   EVAL      VABLNP = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400040510     C                             + ' ' + 'VABLNP'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** NRS
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on AND
127200030721     C                             PiVal<=99
127300030721     C                   Z-ADD     PiVal         VABNRS
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABNRS'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** LNA
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on    AND
128500030721     C                             PiVal<=999   AND
128600030721     C                             PiVal>*zeros
128700030721     C                   Z-ADD     PiVal         VABLNA
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      VABLNA = *zeros
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABLNA'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** CTR
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiInt=*on  AND
130000030721     C                             PiVal<=999
130100030721     C                   Z-ADD     PiVal         VABCTR
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABCTR'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** IAS
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300030721     C                   Z-ADD     PiVal         VABIAS
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABIAS'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
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
147400140319     C*** VATNOT_S
147500140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147600140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147700140319     C                   ENDIF
147800070823     C*
147900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148000140919
148100140919     C* VABNCL ==> 1 collo ogni riga della spedizione
148200140919     C*            per cui simulo che si sommino solo i colli e forzo 1 come collo X riga
148300140919     C                   EVAL      VABNCL = 1
148400140919
148500140930     C*** VATNOT_B / S
148600140930     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_B'
148700140930     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148800140930     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
148900140919     C                   ENDIF
149000070925     C*
149100071029     C***  <===  -----------------------------------------
149200040510     C*
149300030715     C                   ENDIF
149400030715     C                   EVAL      i = i + 1
149500030715     C                   ENDDO
149600070925     C*
149700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149800140319     C*
149900070925     C*
150000071029     C***  <===  -----------------------------------------
150100070925     C*
150200070925     C* Considerazioni finali su CBO/CAS
150300140220     C                   IF        wFlgCAS = '1'    OR
150400140220     C                             VABCAS  > *zeros
150500140220     C                   IF        VABCBO = '1'
150600140220     C                   EVAL      VABCBO = '4'
150700140220     C                   ENDIF
150800140220     C                   IF        VABCBO = '2'
150900140220     C                   EVAL      VABCBO = '6'
151000140220     C                   ENDIF
151100140220     C                   ENDIF
151200070925     C***  <===  ----------------------------
151300040510     C*
151400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151500040510     C                   IF        VABNSP = *zeros
151600040510     C                   SETON                                        31
151700040510     C                   ENDIF
151800040715     C*
151900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152000040715     C                   IF        VABLNP = *zeros
152100040715     C                   SETON                                        31
152200040715     C                   ENDIF
152300040510     C*
152400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152500120411     C                   IF        VABRMN = *zeros
152600120411     C                   Z-ADD     VABNSP        VABRMN
152700120411     C                   ENDIF
152800120328     C* IN QUESTO CASO LO FORZO = 1
152900120411     C***                IF        VABRMN = *zeros
153000120411     C***                Z-ADD     1             VABRMN
153100120411     C***                ENDIF
153200040510     C*
153300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153400040510     C                   IF        VABRMA = *blanks
153500040714     C                   MOVEL     VABRMN        VABRMA
153600040510     C                   ENDIF
153700040128     C*
153800040510     C* Se provincia nn valorizzata la reperisco
153900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
154000110912     C                   IF        VABPRD =  *blanks AND
154100110912     C                             VABNZD =  *blanks AND
154200110912     C                             VABCAD <> *blanks AND
154300110912     C                             VABLOD <> *blanks
154400040128     C                   CLEAR                   TISI95DS
154500040128     C                   EVAL      I95TCN = '3'
154600040128     C                   Z-ADD     datcor        I95DAT
154700040128     C                   EVAL      I95NAR = VABNZD
154800040128     C                   EVAL      I95CAP = VABCAD
154900040128     C                   EVAL      I95LOC = VABLOD
155000040128     C                   CALL      'TISI95R'
155100040128     C                   PARM                    TISI95DS
155200040128     C                   EVAL      VABPRD = O95PRV
155300040510     C                   ENDIF
155400020204     C*
155500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155600020204     C                   EXSR      CHKIMPDIV
155700030715     C                   ENDIF
155800020204     C*
155900010607     C                   ENDSR
156000010607     C*----------------------------------------------------*
156100010601
156200020204
156300020204     C*----------------------------------------------------*
156400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156500020204     C*----------------------------------------------------*
156600020204     C     CHKIMPDIV     BEGSR
156700020204     C*
156800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156900020204     C                   Z-ADD     *zeros        wrkDec            9 9
157000020204     C*
157100020204     C* Come prima cosa effettuo considerazioni sulla divisa
157200020204     C                   IF        vabIAS > *zeros
157300020204     C                   IF        vabVAS <> 'EUR'
157400020204     C                   EVAL      vabVAS =  'ITL'
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   IF        vabCAS > *zeros
157900020204     C                   IF        vabVCA <> 'EUR'
158000020204     C                   EVAL      vabVCA =  'ITL'
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C                   IF        vabVMD > *zeros
158500020204     C                   IF        vabVAD <> 'EUR'
158600020204     C                   EVAL      vabVAD =  'ITL'
158700020204     C                   ENDIF
158800020204     C                   ENDIF
158900020204     C*
159000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
159100020204     C                   Z-ADD     vabIAS        wrkDec
159200020204     C                   IF        wrkDec > *zeros
159300020204     C                   IF        vabVAS = 'ITL'
159400020204     C                   EVAL      vabIAS = *zeros
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159900020204     C                   Z-ADD     vabCAS        wrkDec
160000020204     C                   IF        wrkDec > *zeros
160100020204     C                   IF        vabVCA = 'ITL'
160200020204     C                   EVAL      vabCAS = *zeros
160300020204     C                   ENDIF
160400020204     C                   ENDIF
160500020204     C*
160600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160700020204     C                   Z-ADD     vabVMD        wrkDec
160800020204     C                   IF        wrkDec > *zeros
160900020204     C                   IF        vabVAD = 'ITL'
161000020204     C                   EVAL      vabVMD = *zeros
161100020204     C                   ENDIF
161200020204     C                   ENDIF
161300020204     C*
161400020204     C                   ENDSR
161500020204     C***
161600010330
161700100318
161800100318
161900100318     C*----------------------------------------------------*
162000100318     C*  CONTROLLO NUMERICITA' CAMPI
162100100318     C*----------------------------------------------------*
162200100318     C     CHKNUM        BEGSR
162300100318     C*
162400100318     C                   IF        PiDecChr = *blanks
162500100318     C                   EVAL      PiDecChr = CharNUM
162600100318     C                   ENDIF
162700100318     C*
162800100318     C                   callp     UBISNUM_Check(PiStr
162900100318     C                                          :PiDecChr
163000100318     C                                          :PiVal
163100100318     C                                          :PiNum
163200100318     C                                          :PiInt)
163300100318     C*
163400100318     C                   ENDSR
163500100318     C***
163600010601
163700010601
163800010601
163900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
164000010601     C     repfil        BEGSR
164100010601     C*
164200010601     C                   if        invfil = *zeros and
164300010601     C                             depfil > *zeros and
164400010629     C                             (vinflg = *blanks or
164500010629     C                              vinflg = *zeros)
164600010601     C
164700010601     C                   eval      invfil = depfil
164800010601     C                   endif
164900010601     C*
165000010601     C                   if        depfil <> invfil and
165100010601     C                             invfil > *zeros
165200010601     C                   eval      flgMulti = '1'
165300010601     C                   if        vinflg = *blanks
165400010601     C                   add       1             cntNonEl
165500010601     C                   endif
165600010601     C                   endif
165700010601     C*
165800010601     C                   if        vinflg = '2'
165900010601     C                   eval      flgStato = '2'
166000010601     C                   endif
166100010601     C*
166200010601     C                   ENDSR
166300010601     C***
166400010601
166500010601
166600010601
166700010330
166800040119      /TITLE Invio dei dati al punto operativo.
166900040119     C     invio         BEGSR
167000040119     C*
167100040119     C* 1° invio FIVAT
167200040119     C                   reset                   dscmz
167300040119     C                   move      vlrpoi        cmzdst
167400040119     C                   eval      cmzfld = 'FIVATWWR'
167500040119     C                   eval      cmzmbd = vlrhdl
167600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167700040119     C***                if        prmfir = *blanks
167800040119     C                   eval      cmzfla = 'FIVAT00F'
167900040119     C                   eval      cmzmba = 'FIVAT00F'
168000040119     C***                else
168100040119     C***                eval      cmzfla = prmfir
168200040119     C***                eval      cmzmba = prmfir
168300040119     C***                endif
168400040119     C                   eval      cmznrr = *zeros
168500040119     C                   move      §ctrokvt      cmznrr
168600040119     C                   eval      cmzlba = vlrfl1
168700040119     C                   call(e)   'TIS711C'
168800040119     C                   parm                    dscmz
168900040119     C                   parm      *blanks       esito
169000040119     C                   if        %error
169100040119     C                             or cmzerr = '1'
169200040119     C                             or esito  = '1'
169300040119     C                   eval      wrkesito = '3'
169400040119     C                   else
169500040119     C*
169600040119     C* 2° invio FIVAB
169700040119     C                   reset                   dscmz
169800040119     C                   move      vlrpoi        cmzdst
169900040119     C                   eval      cmzfld = vlrfou
170000040119     C                   eval      cmzmbd = vlrhdl
170100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170200040119     C***                if        prmfir = *blanks
170300040119     C                   eval      cmzfla = 'FIVAB00F'
170400040119     C                   eval      cmzmba = 'FIVAB00F'
170500040119     C***                else
170600040119     C***                eval      cmzfla = prmfir
170700040119     C***                eval      cmzmba = prmfir
170800040119     C***                endif
170900040119     C                   eval      cmznrr = *zeros
171000040119     C                   move      §ctrokvb      cmznrr
171100040119     C                   eval      cmzlba = vlrfl1
171200040119     C                   call(e)   'TIS711C'
171300040119     C                   parm                    dscmz
171400040119     C                   parm      *blanks       esito
171500040119     C                   if        %error
171600040119     C                             or cmzerr = '1'
171700040119     C                             or esito  = '1'
171800040119     C                   eval      wrkesito = '3'
171900040119     C                   endif
172000040119     C                   endif
172100040119     C*
172200040119     C                   ENDSR
172300040119     C***
172400010601
172500010601
172600010601
172700010601
172800010601      /TITLE Invio dei dati al punto operativo.
172900010601     C     opeini        BEGSR
173000010601     C*
173100010601     C* Inizializzo flag e contatori operativi
173200010601     C                   movel     '0'           flgGiro           1
173300010601     C                   movel     '0'           flgMulti          1
173400010601     C                   movel     '1'           flgStato          1
173500010615     C                   movel     '0'           flgOk             1
173600010601     C                   z-add     *zeros        cntNonEl         10 0
173700010601     C                   z-add     *zeros        depfil            3 0
173800010601     C                   z-add     *zeros        invfil            3 0
173900010601     C*
174000010601     C                   ENDSR
174100010601     C***
174200070326
174300070326
174400070326
174500070326
174600070326     C     *pssr         BEGSR
174700070329     C*
174800070329     C                   if        %open(tivin00r)
174900070329     C                   close     tivin00r
175000070329     C                   endif
175100070329     C                   if        %open(fivabwwr)
175200070329     C                   close     fivabwwr
175300070329     C                   endif
175400070329     C                   if        %open(fivatwwr)
175500070329     C                   close     fivatwwr
175600070329     C                   endif
175700070326     C*
175800070326     C* Effettuo la chiamata al CLLE preposto
175900070326     C                   call(e)   'TITVVTC'
176000070326     C                   parm                    parccm
176100070326     C                   parm                    parmbr
176200070326     C                   parm      '2'           paropz
176300070326     C*
176400070326     C                   eval      wrkesito = '2'
176500070404     C*
176600070404     C                   seton                                        LR
176700070326     C*
176800070326     C                   ENDSR     '*CANCL'
176900070326     C***
177000100318
177100100318
177200100318
177300100318     C     normCSV       BEGSR
177400100318     C*
177500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177600100318     C* separatori campi
177700100318     C                   if        CharTXT <> *blanks
177800100318     C                   z-add     *zeros        wLenVINDTA        4 0
177900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178000100318     C                   setoff                                       80
178100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178400100318     C                   if        not *in80
178500100318     C                   seton                                        80
178600100318     C                   else
178700100318     C                   setoff                                       80
178800100318     C                   endif
178900100318     C                   endif
179000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179100100318     C                             and *in80 = *on
179200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179300100318     C                   endif
179400100318     C                   enddo
179500100318     C                   endif
179600100318     C*
179700100318     C                   ENDSR
179800100318     C***
179900070326
180000100326
180100100326
180200100326     C*----------------------------------------------------*
180300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180400100326     C*----------------------------------------------------*
180500100326     C     REPNSP        BEGSR
180600100326     C*
180700100326     C* NSP => Stacco un numeratore da AZNUM
180800100326     C                   clear                   TRUL33DS
180900100326     C                   eval      I33OPE = *zeros
181000100326     C                   eval      I33CNU = 302
181100100326     C                   eval      I33NUM = 1
181200100326     C                   movel     TRUL33DS      KPJBU
181300100326     C                   call      'TRUL33R'
181400100326     C                   parm                    KPJBA
181500100326     C                   movel     KPJBU         TRUL33DS
181600100326     C                   if        O33ERR = *zeros
181700100326     C                   z-add     O33NRF        VABNSP
181800100326     C                   z-add     O33NRF        VATNSP
181900100326     C                   else
182000100326     C                   SETON                                        31
182100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182200140711     C                             + ' ' + 'VABNSP VATNSP'
182300100326     C                   endif
182400100326     C*
182500100326     C                   ENDSR
182600010330
182700100326
182800010330
182900000613     C     *inzsr        BEGSR
183000990910     C*
183100990910     C     *entry        plist
183200990920     C                   parm                    tivlrds
183300990921     C                   parm      wrkesito      esito
183400000724     C                   parm                    prmlit
183500000710     C                   parm                    prmfir
183600010330     C*
183700010330     C* CALCOLA LA DATA CORRENTE
183800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
183900100318     C                   eval      datcor = %dec(%date() : *ISO)
184000000613     C*
184100000613     C                   ENDSR
184200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184300140930VABNSP;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABRMA;VABPKB;W_VATNOT_B;VATNOT_A;VABCAS;VABNAS;
184400140930
184500140512
184600111116
184700100318
