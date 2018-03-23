000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400150114      *
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
004100150114     D Intestazioni    s               n   inz(*on)
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
035600080527     C  N31              eval      fivabds_sav = fivabds
035700080527     C                   eval      depspe = vabNSP
035800080527     C                   else
035900080527     C                   if        depspe = vabNSP AND *in65 = *off
036000100413     C   51              add       VABNCL        sav_VABNCL
036100100413     C   51              add       VABPKB        sav_VABPKB
036200100413     C   51              add       VABVLB        sav_VABVLB
036300100720     C   52              z-add     VABNCL        sav_VABNCL
036400100720     C   52              z-add     VABPKB        sav_VABPKB
036500100720     C   52              z-add     VABVLB        sav_VABVLB
036600100720     C   53              add       VABNCL        sav_VABNCL
036700100720     C   53              z-add     VABPKB        sav_VABPKB
036800100720     C   53              z-add     VABVLB        sav_VABVLB
036900080527     C                   else
037000080527     C  N31              eval      fivabds_ok = fivabds_sav
037100080527     C  N31              exsr      wrivab
037200080527     C  N31              eval      fivabds_sav = fivabds
037300080527     C                   eval      depspe = vabNSP
037400080527     C                   endif
037500080527     C                   endif
037600080527     C*
037700080527     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAB        BEGSR
038500080513     C*
038600080513     C* Gestisco l'eventuale rottura x numero spedizione
038700080527     C                   if        VABNSP <> depspe or *in65 = *on
038800070301     C                   if        wVATNOT_E <> *blanks
038900130610     C                   eval      ok_VABCTM = '7Q'
039000070301     C                   endif
039100100413     C                   write     fivab000
039200080513     C                   endif
039300040119     C*
039400040119     C                   ENDSR
039500040119
039600040119
039700040119
039800040119     C*----------------------------------------------------*
039900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040000040119     C*----------------------------------------------------*
040100040510     C     WRIVAT        BEGSR
040200060512     C*
040300060512     C* Inizializzo i campi chiave
040400060512     C                   eval      VATFGS = VABFGS
040500060512     C                   eval      VATAAS = VABAAS
040600060512     C                   eval      VATCCM = VABCCM
040700070823     C                   eval      VATNRS = VABNRS
040800060512     C                   eval      VATNSP = VABNSP
040900060512     C                   eval      VATLNP = VABLNP
041000040119     C*
041100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041200040510     C                   if        VATTRC = *blanks
041300040510     C                   eval      VATTRC = 'E'
041400040510     C                   endif
041500110318     C                   if        VATNOT <> *blanks
041600040510     C                   write     FIVAT000
041700040119     C                   add       1             §CTROKVT
041800110318     C                   endif
041900060331     C*
042000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042100060331     C                   if        wVATNOT_A <> *blanks
042200060331     C                   eval      VATNOT = wVATNOT_A
042300060331     C                   eval      VATTRC = 'A'
042400060331     C                   write     FIVAT000
042500060331     C                   add       1             §CTROKVT
042600060331     C                   endif
042700060331     C*
042800060331     C                   if        wVATNOT_B <> *blanks
042900060331     C                   eval      VATNOT = wVATNOT_B
043000060331     C                   eval      VATTRC = 'B'
043100060331     C                   write     FIVAT000
043200060331     C                   add       1             §CTROKVT
043300060331     C                   endif
043400140711     C*
043500140711     C                   if        wVATNOT_IJ <> *blanks
043600140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043700140711     C                   eval      VATTRC = 'I'
043800140711     C                   write     FIVAT000
043900140711     C                   add       1             §CTROKVT
044000140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
044100140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044200140711     C                   eval      VATTRC = 'J'
044300140711     C                   write     FIVAT000
044400140711     C                   endif
044500140711     C                   endif
044600060331     C*
044700060331     C                   if        wVATNOT_E <> *blanks
044800060331     C                   eval      VATTRC = 'E'
044900140527     C*
045000150309     C* NON Sviluppo i "CHI SONO"
045100150114     C* però quello che mi è stato passato è l'ultimo collo (il progressivo sarebbe di 3)
045200140527     C*
045300150309     C                   eval      VATNOT = wVATNOT_E
045400150114     C                   write     FIVAT000
045500150114     C                   add       1             §CTROKVT
045600120320     C                   endif
045700110927     C*
045800110927     C                   if        wVATNOT_I <> *blanks
045900110927     C                   eval      VATNOT = wVATNOT_I
046000140711     C                   eval      VATTRC = 'I'
046100110927     C                   write     FIVAT000
046200110927     C                   add       1             §CTROKVT
046300110927     C                   endif
046400140220     C*
046500140220     C                   if        wVATNOT_J <> *blanks
046600140220     C                   eval      VATNOT = wVATNOT_J
046700140220     C                   eval      VATTRC = 'J'
046800140220     C                   write     FIVAT000
046900140220     C                   add       1             §CTROKVT
047000140220     C                   endif
047100140319     C*
047200140319     C                   if        wVATNOT_S <> *blanks
047300140319     C                   eval      VATNOT = wVATNOT_S
047400140319     C* i 2 flag devono essere blank
047500140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
047600140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047700140319     C                   eval      VATTRC = 'S'
047800140319     C                   write     FIVAT000
047900140319     C                   add       1             §CTROKVT
048000140319     C                   endif
048100040119     C*
048200040119     C                   ENDSR
048300040119
048400040119
048500040119
048600040119     C*----------------------------------------------------*
048700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048800040119     C*----------------------------------------------------*
048900040119     C     PREVAT        BEGSR
049000040119     C*
049100040119     C* Compongo il nome del membro da dare al FIVATWWR
049200040119     C                   eval      parmbr = vlrhdl
049300040119     C                   movel     'M'           parmbr
049400040510     C                   eval      parccm = vlrksc
049500040119     C                   eval      paropz = '1'
049600040119     C* Effettuo la chiamata al CLLE preposto
049700040119     C                   call(e)   'TITVVTC'
049800040119     C                   parm                    parccm
049900040119     C                   parm                    parmbr
050000040119     C                   parm                    paropz
050100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050200040119     C                   if        %error
050300040119     C                   movel     '1'           chkcall
050400040119     C                   else
050500040119     C                   movel     '0'           chkcall
050600040119     C                   endif
050700040119     C*
050800040119     C                   ENDSR
050900010601
051000010601
051100010601
051200010330     C*----------------------------------------------------*
051300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051400010330     C*----------------------------------------------------*
051500040119     C     INZVAR        BEGSR
051600080527     C*
051700080527     C                   setoff                                       65
051800010330     C*
051900030822     C* Inizializzo variabili di wrk
052000020204     C                   Z-ADD     *zeros        Num5_0            5 0
052100101018     C                   MOVEL     *blanks       wFlgCAS
052200060331     C                   MOVEL     *blanks       wVATNOT_A
052300060331     C                   MOVEL     *blanks       wVATNOT_B
052400060331     C                   MOVEL     *blanks       wVATNOT_E
052500110927     C                   MOVEL     *blanks       wVATNOT_I
052600140220     C                   MOVEL     *blanks       wVATNOT_J
052700140220     C                   MOVEL     *blanks       wVATNOT_IJ
052800140319     C                   MOVEL     *blanks       wVATNOT_S
052900120411     C                   MOVEL     *blanks       wNOT
053000120411     C                   MOVEL     *blanks       Data8A
053100030822     C*
053200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053300030822     C                   CLEAR                   SkSplitCSV
053400030822     C                   EVAL      i = 1
053500030822     C                   EVAL      posDa = *zeros
053600030822     C                   EVAL      posA  = *zeros
053700080527     C*
053800080527     C                   CLEAR                   FIVABDS
053900080527     C                   CLEAR                   FIVAT000
054000030822     C*
054100030822     C* Reimposto i valori di default
054200030822     C                   EXSR      DEFCAM
054300030822     C*
054400010330     C                   ENDSR
054500010330     C*----------------------------------------------------*
054600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054700010330     C*----------------------------------------------------*
054800010330     C     DEFCAM        BEGSR
054900030822     C*
055000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055100030715     C* e delimitatore testo.
055200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055500100318     C*
055600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055700100318     C                   EVAL      CharSOS = CharNUM
055800030822     C*
055900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056000100720     C                   SETOFF                                       505152
056100100720     C                   SETOFF                                       53
056200100413     C                   SELECT
056300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056400100720     C                   SETON                                        50
056500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056600100413     C                   SETON                                        51
056700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056800100720     C                   SETON                                        52
056900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057000100720     C                   SETON                                        53
057100100413     C                   ENDSL
057200030822     C*
057300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057400030822     C                   EVAL      posDaDft = 1
057500030822     C                   EVAL      posADft  = 0
057600030822     C                   EVAL      wGiroDft = 0
057700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057800030822     C                             posDaDft > 0
057900030822     C*
058000030822     C* Gestisco il 1° giro
058100030822     C                   IF        wGiroDft = 0
058200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058400030822     C* Incremento il contatore dei "giri"
058500030822     C                   EVAL      wGiroDft = 1
058600030822     C                   ELSE
058700030822     C                   EVAL      posDaDft = posADft
058800030822     C                   ENDIF
058900030822     C* Eseguo lo scan x trovare la fine del campo corrente
059000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059100030822     C*
059200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059400030822     C                   IF        posDaDft > 0 AND
059500030822     C                             posADft  > 0
059600030822     C* NCL
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'NCL'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABNCL
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* CCM
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CCM'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABCCM
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* LNP
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'LNP'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200030822     C                   Z-ADD     PiVal         VABLNP
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* NRS
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'NRS'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiInt=*on
064500030822     C                   Z-ADD     PiVal         VABNRS
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* CTR
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CTR'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiInt=*on
065800030822     C                   Z-ADD     PiVal         VABCTR
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* PKB
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'PKB'
066600030822     C                   EVAL      PiStr=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   EXSR      CHKNUM
067000030822     C                   IF        PiNum=*on
067100130610     C                   Z-ADD     PiVal         VABPKB
067200030822     C                   ENDIF
067300030822     C                   ENDIF
067400030822     C* VLB
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VLB'
067900030822     C                   EVAL      PiStr=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   EXSR      CHKNUM
068300030822     C                   IF        PiNum=*on
068400130610     C                   Z-ADD     PiVal         VABVLB
068500030822     C                   ENDIF
068600030822     C                   ENDIF
068700030822     C* QFT
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'QFT'
069200030822     C                   EVAL      PiStr=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   EXSR      CHKNUM
069600030822     C                   IF        PiNum=*on
069700030822     C                   Z-ADD     PiVal         VABQFT
069800030822     C                   ENDIF
069900030822     C                   ENDIF
070000030822     C* CBO
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'CBO'
070500030822     C                   EVAL      VABCBO=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* TSP
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'TSP'
071400030822     C                   EVAL      VABTSP=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VAS
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VAS'
072300030822     C                   EVAL      VABVAS=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VCA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VCA'
073200030822     C                   EVAL      VABVCA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600070925     C* TIC
073700070925     C                   IF        %subst(
073800070925     C                             %subst(vlrppt:posDaDft+1:
073900070925     C                             posADft-posDaDft-1):1:3)
074000070925     C                             = 'TIC'
074100070925     C                   EVAL      VABTIC=%trim(%subst(
074200070925     C                             %subst(vlrppt:posDaDft+1:
074300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400070925     C                   ENDIF
074500030822     C* GCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GCA'
075000030822     C                   EVAL      VABGCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* CTM
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'CTM'
075900030822     C                   EVAL      VABCTM=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* FFD
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'FFD'
076800030822     C                   EVAL      VABFFD=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* VAD
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'VAD'
077700030822     C                   EVAL      VABVAD=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GMA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GMA'
078600030822     C                   EVAL      VABGMA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GGA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GGA'
079500030822     C                   EVAL      VABGGA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GVA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GVA'
080400030822     C                   EVAL      VABGVA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* TC1
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'TC1'
081300030822     C                   EVAL      VABTC1=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* TC2
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'TC2'
082200030822     C                   EVAL      VABTC2=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600040714     C* VATTRC
082700040714     C                   IF        %subst(
082800040714     C                             %subst(vlrppt:posDaDft+1:
082900040714     C                             posADft-posDaDft-1):1:3)
083000040714     C                             = 'TRC'
083100040714     C                   EVAL      VATTRC=%trim(%subst(
083200040714     C                             %subst(vlrppt:posDaDft+1:
083300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400040714     C                   ENDIF
083500030822     C* ...
083600030822     C                   ENDIF
083700030822     C                   ENDDO
083800020204     C*
083900010330     C                   ENDSR
084000010607     C*----------------------------------------------------*
084100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084200010607     C*----------------------------------------------------*
084300010607     C     IMPVAB        BEGSR
084400010607     C*
084500010607     C                   SETOFF                                       3132
084600110315 xxx C***                SETON                                        35
084700030515     C*
084800010607     C                   MOVEL     datcor        VABAAS
084900010607     C                   MOVE      datcor        VABMGS
085000010607     C*
085100030715     C**********
085200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085300030715     C**********
085400030715     C                   IF        wGiro = *zeros
085500040113     C*
085600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085900080429     C                             %trim(intcol(5))<>*blanks
086000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086200080429     C                                      %trim(intcol(5))
086300070823     C     ';':CharCSV   XLATE     vindta        vindta
086400040510     C                   ENDIF
086500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086600031201     C     minu:maiu     XLATE     vindta        vindta
086700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086800030715     C                   DOW       posDa <= %len(%trim(vindta))
086900030715     C*
087000030715     C* Gestisco il 1° campo
087100030715     C                   IF        i = 1
087200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087300030715     C                   EVAL      posDa = 1
087400030715     C* Eseguo lo scan x trovare la fine del primo campo
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C* X i campi successivi al 1°
088000030715     C                   ELSE
088100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088200030715     C                   EVAL      posDa = posA + 1
088300030715     C* Eseguo lo scan x trovare la fine del campo corrente
088400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088500030715     C* Gestisco l'ultimo campo
088600030715     C                   IF        posA = *zeros
088700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   LEAVE
089200030715     C                   ELSE
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   ENDIF
089700030715     C                   ENDIF
089800030715     C* Incremento il contatore d campo
089900030715     C                   EVAL      i = i +1
090000030715     C                   ENDDO
090100030715     C*
090200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090300030715     C* delimitazione testo
090400030715     C                   EVAL      i = 1
090500030715     C                   DOW       i < %elem(SkSplitFLD)
090600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090800030715     C                   EVAL      i = i + 1
090900030715     C                   ENDDO
091000030715     C                   EVAL      wGiro = 1
091100030715     C                   ELSE
091200030715     C                   EVAL      wGiro = 2
091300031204     C**********
091400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091500031204     C* d separatore campo
091600031204     C**********
091700031204     C                   EVAL      vindta = %trim(vindta)
091800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091900031204     C                   EVAL      vindta = %subst(vindta:2)
092000031204     C                   ENDDO
092100031204     C*
092200031204     C                   Z-ADD     *zeros        lunghInput        4 0
092300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092700110912     C                   ENDDO
092800030715     C**********
092900030715     C* Effettuo lo split del campo dati d input
093000030715     C**********
093100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093200030715     C                   DOW       posDa <= %len(%trim(vindta))
093300030715     C*
093400030715     C* Gestisco il 1° campo
093500030715     C                   IF        i = 1
093600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093700030715     C                   EVAL      posDa = 1
093800030715     C* Eseguo lo scan x trovare la fine del primo campo
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C* X i campi successivi al 1°
094400030715     C                   ELSE
094500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094600030715     C                   EVAL      posDa = posA + 1
094700031204     C* Verifico che nn vi sia il campo nn valorizzato
094800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094900031204     C* Se campo nn valorizzato skippo al prossimo
095000031204     C                   EVAL      SkSplitCSV(i) = *blanks
095100031204     C                   EVAL      posA  = posA + 1
095200031204     C                   ELSE
095300030715     C* Eseguo lo scan x trovare la fine del campo corrente
095400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095500030715     C* Gestisco l'ultimo campo
095600030715     C                   IF        posA = *zeros
095700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   LEAVE
096200030715     C                   ELSE
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   ENDIF
096700031204     C                   ENDIF
096800030715     C                   ENDIF
096900030715     C* Incremento il contatore d campo
097000030715     C                   EVAL      i = i +1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097400030715     C* delimitazione testo
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitCSV)
097700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097900030715     C                   EVAL      i = i + 1
098000030715     C                   ENDDO
098100100720     C*
098200100720     C* Stacco un progressivo x numero spedizione
098300150309 xxx C***                EXSR      repNSP
098400100326     C*
098500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098600030715     C                   EVAL      i = 1
098700030715     C                   DOW       i < %elem(SkSplitFLD)
098800030715     C*
098900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099100030715     C                             SkSplitCSV(i) <> *blanks
099200030721     C*** ATB
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099400030721     C                   EVAL      VABATB = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RSD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100100030715     C* ==
100200130528     C                   ENDIF
100300030721     C*** RD2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** IND
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100900030715     C                   EVAL      VABIND = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** LOD
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** PRD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030721     C*** NZD
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** RMA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NOT
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NT2
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040510     C*** CBO
106000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106200040510     C                   ENDIF
106300030721     C*** NAS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TIC
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GCA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** XCO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TCR
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** CTS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** FTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** VAD
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GMA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GGA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GVA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC1
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC2
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** SCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** RMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** NMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*
113200030721     C* Reperisco quindi i campi numerici...
113300040510     C*** NSP
113400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113500040510     C                   EVAL      PiStr=SkSplitCSV(i)
113600040510     C                   EXSR      CHKNUM
113700040510     C                   IF        PiInt=*on
113800040510     C                   Z-ADD     PiVal         VABNSP
113900040510     C                   ELSE
114000040510     C                   SETON                                        31
114100040510     C                   EVAL      VABNSP = *zeros
114200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABNSP'
114400040510     C                   ENDIF
114500040510     C                   ENDIF
114600071106     C*** RMN
114700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114800071106     C                   EVAL      PiStr=SkSplitCSV(i)
114900071106     C                   EXSR      CHKNUM
115000071106     C                   IF        PiInt=*on
115100071106     C                   Z-ADD     PiVal         VABRMN
115200071106     C                   ELSE
115300071106     C                   SETON                                        32
115400071106     C                   EVAL      VABRMN = *zeros
115500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115600071106     C                             + ' ' + 'VABRMN'
115700071106     C                   ENDIF
115800071106     C                   ENDIF
115900030721     C*** CAD
116000130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116100130610     C                   EVAL      PiStr=SkSplitCSV(i)
116200130610     C                   EXSR      CHKNUM
116300130610     C                   IF        PiInt=*on
116400130610     C                   Z-ADD     PiVal         Num5_0
116500130610     C                   MOVEL(p)  Num5_0        VABCAD
116600130610     C                   ELSE
116700130610     C                   SETON                                        32
116800130610     C                   EVAL      VABCAD = *zeros
116900130610     C                   EVAL      vinmsg = %trimr(vinmsg)
117000130610     C                             + ' ' + 'VABCAD'
117100130610     C                   ENDIF
117200130605     C                   ENDIF
117300030721     C*** NCL
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         VABNCL
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABNCL = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040122     C                             + ' ' + 'VABNCL'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** PKB
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100130610     C                   Z-ADD(H)  PiVal         VABPKB
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABPKB = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABPKB'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CAS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120200111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120300140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
120400140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
120500140711     C                             %trim(SkSplitCSV(i)) <> ',00'
120600130703     C                   EVAL      VABVCA = 'EUR'
120700140930     C                   EVAL      VABTIC = *blank
120800140513     C                   EVAL      wFlgCAS = '1'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABCAS
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABCAS = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABCAS'
121800030721     C                   ENDIF
121900140513     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** CCM
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on      AND
122600030721     C                             PiVal<=9999999 AND
122700030721     C                             PiVal>*zeros
122800030721     C                   Z-ADD     PiVal         VABCCM
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      VABCCM = *zeros
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300040510     C                             + ' ' + 'VABCCM'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** LNP
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiInt=*on    AND
124100030721     C                             PiVal<=999   AND
124200030721     C                             PiVal>*zeros
124300030721     C                   Z-ADD     PiVal         VABLNP
124400030721     C                   ELSE
124500040715     C                   SETON                                        32
124600030721     C                   EVAL      VABLNP = *zeros
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800040510     C                             + ' ' + 'VABLNP'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** NRS
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiInt=*on AND
125600030721     C                             PiVal<=99
125700030721     C                   Z-ADD     PiVal         VABNRS
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABNRS'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** LNA
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on    AND
126900030721     C                             PiVal<=999   AND
127000030721     C                             PiVal>*zeros
127100030721     C                   Z-ADD     PiVal         VABLNA
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      VABLNA = *zeros
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABLNA'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** CTR
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on  AND
128400030721     C                             PiVal<=999
128500030721     C                   Z-ADD     PiVal         VABCTR
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABCTR'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** IAS
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129400141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
129500141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
129600141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
129700141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
129800141003     C                             %trim(SkSplitCSV(i)) <> ',00'
129900141006     C                   EVAL      VABVAS = 'EUR'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABIAS
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABIAS'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000141003     C                   ENDIF
131100030721     C*** VLB
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiNum=*on
131600130610     C                   Z-ADD(H)  PiVal         VABVLB
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABVLB'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** QFT
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiNum=*on
132800030721     C                   Z-ADD     PiVal         VABQFT
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABQFT'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** NCD
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on      AND
134000030721     C                             PiVal<=9999999
134100030721     C                   Z-ADD     PiVal         VABNCD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABNCD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** NCA
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on      AND
135300030721     C                             PiVal<=9999999
135400030721     C                   Z-ADD     PiVal         VABNCA
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABNCA'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** ZNC
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on AND
136600030721     C                             PiVal<=99
136700030721     C                   Z-ADD     PiVal         VABZNC
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABZNC'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** DCR
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on       AND
137900030721     C                             PiVal<=99999999
138000030721     C                   Z-ADD     PiVal         VABDCR
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABDCR'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** HCR
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on   AND
139200030721     C                             PiVal<=9999
139300030721     C                   Z-ADD     PiVal         VABHCR
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABHCR'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** VMD
140100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiNum=*on
140500030721     C                   Z-ADD     PiVal         VABVMD
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABVMD'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200030721     C*** ANT
141300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141400030721     C                   EVAL      PiStr=SkSplitCSV(i)
141500030721     C                   EXSR      CHKNUM
141600030721     C                   IF        PiInt=*on        AND
141700030721     C                             PiVal<=999999999
141800030721     C                   Z-ADD     PiVal         VABANT
141900030721     C                   ELSE
142000030721     C                   SETON                                        32
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABANT'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500030721     C*** CMO
142600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142700030721     C                   EVAL      PiStr=SkSplitCSV(i)
142800030721     C                   EXSR      CHKNUM
142900030721     C                   IF        PiInt=*on
143000030721     C                   Z-ADD     PiVal         Num5_0
143100030721     C                   MOVEL(p)  Num5_0        VABCMO
143200030721     C                   ELSE
143300030721     C                   SETON                                        32
143400030721     C                   EVAL      VABCMO = *zeros
143500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143600030721     C                             + ' ' + 'VABCMO'
143700030721     C                   ENDIF
143800030721     C                   ENDIF
143900040510     C*
144000040510     C* Reperisco i campi del FIVAT
144100040510     C*** VATNOT
144200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144400040510     C                   ENDIF
144500040510     C*** VATTRC
144600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144800040510     C                   ENDIF
144900060331     C*** VATNOT_A
145000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145200060331     C                   ENDIF
145300060331     C*** VATNOT_B
145400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145600060331     C                   ENDIF
145700060331     C*** VATNOT_E
145800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146000060331     C                   ENDIF
146100110927     C*** VATNOT_I
146200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146400110927     C                   ENDIF
146500141003     C*** VATNOT_IJ
146600141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
146700141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
146800141003     C                   ENDIF
146900140319     C*** VATNOT_S
147000140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147100140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147200140319     C                   ENDIF
147300070823     C*
147400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147500150309     C*
147600150309     C*** NSP / RMN
147700150309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
147800150309     C                   EVAL      PiStr=SkSplitCSV(i)
147900150309     C                   EXSR      CHKNUM
148000150309     C                   IF        PiInt=*on
148100150309     C                   Z-ADD     PiVal         VABNSP
148200150309     C                   Z-ADD     PiVal         VABRMN
148300150309     C                   ELSE
148400150309     C                   SETON                                        31
148500150309     C                   EVAL      VABNSP = *zeros
148600150309     C                   EVAL      VABRMN = *zeros
148700150309     C                   EVAL      vinmsg = %trimr(vinmsg)
148800150309     C                             + ' ' + 'VABNSP VABRMN'
148900150309     C                   ENDIF
149000150309     C                   ENDIF
149100141003     C*
149200071029     C***  <===  -----------------------------------------
149300040510     C*
149400030715     C                   ENDIF
149500030715     C                   EVAL      i = i + 1
149600030715     C                   ENDDO
149700070925     C*
149800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
149900140319     C*
150000141014     C* se la nazione è IT la sblanco
150100141112     C                   IF        VABNZD = 'IT' or
150200141112     C                             VABNZD = 'ITA'
150300141014     C                   EVAL      VABNZD = *blank
150400141014     C                   ENDIF
150500070925     C*
150600070925     C* Considerazioni finali su CBO/CAS
150700140220     C                   IF        wFlgCAS = '1'    OR
150800140220     C                             VABCAS  > *zeros
150900140220     C                   IF        VABCBO = '1'
151000140220     C                   EVAL      VABCBO = '4'
151100140220     C                   ENDIF
151200140220     C                   IF        VABCBO = '2'
151300140220     C                   EVAL      VABCBO = '6'
151400140220     C                   ENDIF
151500140220     C                   ENDIF
151600070925     C***  <===  ----------------------------
151700040510     C*
151800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151900040510     C                   IF        VABNSP = *zeros
152000040510     C                   SETON                                        31
152100040510     C                   ENDIF
152200040715     C*
152300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152400040715     C                   IF        VABLNP = *zeros
152500040715     C                   SETON                                        31
152600040715     C                   ENDIF
152700040510     C*
152800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152900120411     C                   IF        VABRMN = *zeros
153000120411     C                   Z-ADD     VABNSP        VABRMN
153100120411     C                   ENDIF
153200120328     C* IN QUESTO CASO LO FORZO = 1
153300120411     C***                IF        VABRMN = *zeros
153400120411     C***                Z-ADD     1             VABRMN
153500120411     C***                ENDIF
153600040510     C*
153700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153800040510     C                   IF        VABRMA = *blanks
153900040714     C                   MOVEL     VABRMN        VABRMA
154000040510     C                   ENDIF
154100040128     C*
154200040510     C* Se provincia nn valorizzata la reperisco
154300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
154400110912     C                   IF        VABPRD =  *blanks AND
154500110912     C                             VABNZD =  *blanks AND
154600110912     C                             VABCAD <> *blanks AND
154700110912     C                             VABLOD <> *blanks
154800040128     C                   CLEAR                   TISI95DS
154900040128     C                   EVAL      I95TCN = '3'
155000040128     C                   Z-ADD     datcor        I95DAT
155100040128     C                   EVAL      I95NAR = VABNZD
155200040128     C                   EVAL      I95CAP = VABCAD
155300040128     C                   EVAL      I95LOC = VABLOD
155400040128     C                   CALL      'TISI95R'
155500040128     C                   PARM                    TISI95DS
155600040128     C                   EVAL      VABPRD = O95PRV
155700040510     C                   ENDIF
155800020204     C*
155900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
156000020204     C                   EXSR      CHKIMPDIV
156100030715     C                   ENDIF
156200020204     C*
156300010607     C                   ENDSR
156400010607     C*----------------------------------------------------*
156500010601
156600020204
156700020204     C*----------------------------------------------------*
156800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
156900020204     C*----------------------------------------------------*
157000020204     C     CHKIMPDIV     BEGSR
157100020204     C*
157200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
157300020204     C                   Z-ADD     *zeros        wrkDec            9 9
157400020204     C*
157500020204     C* Come prima cosa effettuo considerazioni sulla divisa
157600020204     C                   IF        vabIAS > *zeros
157700020204     C                   IF        vabVAS <> 'EUR'
157800020204     C                   EVAL      vabVAS =  'ITL'
157900020204     C                   ENDIF
158000020204     C                   ENDIF
158100020204     C*
158200020204     C                   IF        vabCAS > *zeros
158300020204     C                   IF        vabVCA <> 'EUR'
158400020204     C                   EVAL      vabVCA =  'ITL'
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C                   IF        vabVMD > *zeros
158900020204     C                   IF        vabVAD <> 'EUR'
159000020204     C                   EVAL      vabVAD =  'ITL'
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
159500020204     C                   Z-ADD     vabIAS        wrkDec
159600020204     C                   IF        wrkDec > *zeros
159700020204     C                   IF        vabVAS = 'ITL'
159800020204     C                   EVAL      vabIAS = *zeros
159900020204     C                   ENDIF
160000020204     C                   ENDIF
160100020204     C*
160200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
160300020204     C                   Z-ADD     vabCAS        wrkDec
160400020204     C                   IF        wrkDec > *zeros
160500020204     C                   IF        vabVCA = 'ITL'
160600020204     C                   EVAL      vabCAS = *zeros
160700020204     C                   ENDIF
160800020204     C                   ENDIF
160900020204     C*
161000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
161100020204     C                   Z-ADD     vabVMD        wrkDec
161200020204     C                   IF        wrkDec > *zeros
161300020204     C                   IF        vabVAD = 'ITL'
161400020204     C                   EVAL      vabVMD = *zeros
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C                   ENDSR
161900020204     C***
162000010330
162100100318
162200100318
162300100318     C*----------------------------------------------------*
162400100318     C*  CONTROLLO NUMERICITA' CAMPI
162500100318     C*----------------------------------------------------*
162600100318     C     CHKNUM        BEGSR
162700100318     C*
162800100318     C                   IF        PiDecChr = *blanks
162900100318     C                   EVAL      PiDecChr = CharNUM
163000100318     C                   ENDIF
163100100318     C*
163200100318     C                   callp     UBISNUM_Check(PiStr
163300100318     C                                          :PiDecChr
163400100318     C                                          :PiVal
163500100318     C                                          :PiNum
163600100318     C                                          :PiInt)
163700100318     C*
163800100318     C                   ENDSR
163900100318     C***
164000010601
164100010601
164200010601
164300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
164400141006      ***
164500141014     C     repfil        BEGSR
164600141014     C*
164700141014     C                   if        invfil = *zeros and
164800141014     C                             depfil > *zeros and
164900141014     C                             (vinflg = *blanks or
165000141014     C                              vinflg = *zeros)
165100141014     C*
165200141014     C                   eval      invfil = depfil
165300141014     C                   endif
165400141014     C*
165500141014     C                   if        depfil <> invfil and
165600141014     C                             invfil > *zeros
165700141014     C                   eval      flgMulti = '1'
165800141014     C                   if        vinflg = *blanks
165900141014     C                   add       1             cntNonEl
166000141014     C                   endif
166100141014     C                   endif
166200141014     C*
166300141014     C                   if        vinflg = '2'
166400141014     C                   eval      flgStato = '2'
166500141014     C                   endif
166600141014     C*
166700141014     C                   ENDSR
166800141014     C*
166900010601
167000010601
167100010601
167200010330
167300040119      /TITLE Invio dei dati al punto operativo.
167400040119     C     invio         BEGSR
167500040119     C*
167600040119     C* 1° invio FIVAT
167700040119     C                   reset                   dscmz
167800040119     C                   move      vlrpoi        cmzdst
167900040119     C                   eval      cmzfld = 'FIVATWWR'
168000040119     C                   eval      cmzmbd = vlrhdl
168100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168200040119     C***                if        prmfir = *blanks
168300040119     C                   eval      cmzfla = 'FIVAT00F'
168400040119     C                   eval      cmzmba = 'FIVAT00F'
168500040119     C***                else
168600040119     C***                eval      cmzfla = prmfir
168700040119     C***                eval      cmzmba = prmfir
168800040119     C***                endif
168900040119     C                   eval      cmznrr = *zeros
169000040119     C                   move      §ctrokvt      cmznrr
169100040119     C                   eval      cmzlba = vlrfl1
169200040119     C                   call(e)   'TIS711C'
169300040119     C                   parm                    dscmz
169400040119     C                   parm      *blanks       esito
169500040119     C                   if        %error
169600040119     C                             or cmzerr = '1'
169700040119     C                             or esito  = '1'
169800040119     C                   eval      wrkesito = '3'
169900040119     C                   else
170000040119     C*
170100040119     C* 2° invio FIVAB
170200040119     C                   reset                   dscmz
170300040119     C                   move      vlrpoi        cmzdst
170400040119     C                   eval      cmzfld = vlrfou
170500040119     C                   eval      cmzmbd = vlrhdl
170600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170700040119     C***                if        prmfir = *blanks
170800040119     C                   eval      cmzfla = 'FIVAB00F'
170900040119     C                   eval      cmzmba = 'FIVAB00F'
171000040119     C***                else
171100040119     C***                eval      cmzfla = prmfir
171200040119     C***                eval      cmzmba = prmfir
171300040119     C***                endif
171400040119     C                   eval      cmznrr = *zeros
171500040119     C                   move      §ctrokvb      cmznrr
171600040119     C                   eval      cmzlba = vlrfl1
171700040119     C                   call(e)   'TIS711C'
171800040119     C                   parm                    dscmz
171900040119     C                   parm      *blanks       esito
172000040119     C                   if        %error
172100040119     C                             or cmzerr = '1'
172200040119     C                             or esito  = '1'
172300040119     C                   eval      wrkesito = '3'
172400040119     C                   endif
172500040119     C                   endif
172600040119     C*
172700040119     C                   ENDSR
172800040119     C***
172900010601
173000010601
173100010601
173200010601
173300010601      /TITLE Invio dei dati al punto operativo.
173400010601     C     opeini        BEGSR
173500010601     C*
173600010601     C* Inizializzo flag e contatori operativi
173700010601     C                   movel     '0'           flgGiro           1
173800010601     C                   movel     '0'           flgMulti          1
173900010601     C                   movel     '1'           flgStato          1
174000010615     C                   movel     '0'           flgOk             1
174100010601     C                   z-add     *zeros        cntNonEl         10 0
174200010601     C                   z-add     *zeros        depfil            3 0
174300010601     C                   z-add     *zeros        invfil            3 0
174400010601     C*
174500010601     C                   ENDSR
174600010601     C***
174700070326
174800070326
174900070326
175000070326
175100070326     C     *pssr         BEGSR
175200070329     C*
175300070329     C                   if        %open(tivin00r)
175400070329     C                   close     tivin00r
175500070329     C                   endif
175600070329     C                   if        %open(fivabwwr)
175700070329     C                   close     fivabwwr
175800070329     C                   endif
175900070329     C                   if        %open(fivatwwr)
176000070329     C                   close     fivatwwr
176100070329     C                   endif
176200070326     C*
176300070326     C* Effettuo la chiamata al CLLE preposto
176400070326     C                   call(e)   'TITVVTC'
176500070326     C                   parm                    parccm
176600070326     C                   parm                    parmbr
176700070326     C                   parm      '2'           paropz
176800070326     C*
176900070326     C                   eval      wrkesito = '2'
177000070404     C*
177100070404     C                   seton                                        LR
177200070326     C*
177300070326     C                   ENDSR     '*CANCL'
177400070326     C***
177500100318
177600100318
177700100318
177800100318     C     normCSV       BEGSR
177900100318     C*
178000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
178100100318     C* separatori campi
178200100318     C                   if        CharTXT <> *blanks
178300100318     C                   z-add     *zeros        wLenVINDTA        4 0
178400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
178500100318     C                   setoff                                       80
178600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
178700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
178800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
178900100318     C                   if        not *in80
179000100318     C                   seton                                        80
179100100318     C                   else
179200100318     C                   setoff                                       80
179300100318     C                   endif
179400100318     C                   endif
179500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
179600100318     C                             and *in80 = *on
179700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
179800100318     C                   endif
179900100318     C                   enddo
180000100318     C                   endif
180100100318     C*
180200100318     C                   ENDSR
180300100318     C***
180400070326
180500100326
180600100326
180700100326     C*----------------------------------------------------*
180800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
180900100326     C*----------------------------------------------------*
181000100326     C     REPNSP        BEGSR
181100100326     C*
181200100326     C* NSP => Stacco un numeratore da AZNUM
181300100326     C                   clear                   TRUL33DS
181400100326     C                   eval      I33OPE = *zeros
181500100326     C                   eval      I33CNU = 302
181600100326     C                   eval      I33NUM = 1
181700100326     C                   movel     TRUL33DS      KPJBU
181800100326     C                   call      'TRUL33R'
181900100326     C                   parm                    KPJBA
182000100326     C                   movel     KPJBU         TRUL33DS
182100100326     C                   if        O33ERR = *zeros
182200100326     C                   z-add     O33NRF        VABNSP
182300100326     C                   z-add     O33NRF        VATNSP
182400100326     C                   else
182500100326     C                   SETON                                        31
182600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
182700140711     C                             + ' ' + 'VABNSP VATNSP'
182800100326     C                   endif
182900100326     C*
183000100326     C                   ENDSR
183100010330
183200100326
183300010330
183400000613     C     *inzsr        BEGSR
183500990910     C*
183600990910     C     *entry        plist
183700990920     C                   parm                    tivlrds
183800990921     C                   parm      wrkesito      esito
183900000724     C                   parm                    prmlit
184000000710     C                   parm                    prmfir
184100010330     C*
184200010330     C* CALCOLA LA DATA CORRENTE
184300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
184400100318     C                   eval      datcor = %dec(%date() : *ISO)
184500000613     C*
184600000613     C                   ENDSR
184700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
184800150309VATNOT_E;W_VABNSP;X;VABNCL;VATNOT_IJ;VATNOT_S;VABRSD;VABIND;VABRD2;VABCAD;VABLOD;VABPKB;VABCAS;
184900150309
185000140512
185100111116
185200100318
