000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400140220      *
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
001600010330     D*----------------------------------------------------
001700010330     D* DICHIARAZIOINE VARIABILI DI WRK
001800010330     D*----------------------------------------------------
001900010330     D dscmz         e ds                  inz
002000010330     D psds           sds
002100010330     D  procname         *PROC
002200010330     D tivlrds       e ds                  extname(tivlr00f)
002300040128     D tisi95ds      e ds
002400100413     D fivabds       e ds                  extname(fivab00f)
002500100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002600100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
003800120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003900120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004000140711     D Intestazioni    s               n   inz(*off)
004100100326     D*------------------
004200100326     D* DS REPERIMENTO NUMERATORE
004300100326     D*------------------
004400100326     D trul33ds      e ds                  inz
004500100326     D*------------------
004600100326     D* DS ARCHITETTURA
004700100326     D*------------------
004800100326     D kpjba         e ds                  inz
004900130611     D*------------------
005000130611     D TRUL28DSX     e ds                  inz
005100100326
005200030715     D*------------------
005300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005400030715     D*------------------
005500030715     D SkSplitFLD      S             10    DIM(100)
005600030715     D SkSplitCSV      S            256    DIM(100)
005700030715     D CharCSV         S              1
005800030715     D CharTXT         S              1
005900030715     D CharNUM         S              1
006000100318     D CharSOS         S              1
006100130626     D posDa           S              4  0 INZ(*zeros)
006200130626     D posA            S              4  0 INZ(*zeros)
006300030715     D i               s              3  0 INZ(1)
006400030715     D wGiro           s              1  0 INZ(*zeros)
006500070823     D wVATNOT_A       s                   LIKE(VATNOT)
006600060331     D wVATNOT_B       s                   LIKE(VATNOT)
006700060331     D wVATNOT_E       s                   LIKE(VATNOT)
006800110927     D wVATNOT_I       s                   LIKE(VATNOT)
006900140220     D wVATNOT_J       s                   LIKE(VATNOT)
007000140220     D wVATNOT_IJ      s             70
007100140319     D wVATNOT_S       s                   LIKE(VATNOT)
007200120411     D wNOT            s             70
007300120411     D Data8A          s              8
007400140527     D wBarcodeN       s             11s 0
007500140527     D wX              s              3s 0
007600140711     D wRMA            s                   like(VABRMA)
007700140711     D TxtInOut        S           2048
007800140711     D ElencoChar      S            256
007900140711     D TipoElenco      S              1
008000140711     D CharSost        S              1
008100140711     D UpperCase       S              1
008200140711     D ChkNull         S              1
008300140711     D CharNull        S              1
008400140711     D EsitoBon        S              1
008500030822     D*------------------
008600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008700030822     D*------------------
008800030822     D posDaDft        S              3  0 INZ(*zeros)
008900030822     D posADft         S              3  0 INZ(*zeros)
009000030822     D j               s              3  0 INZ(1)
009100030822     D wGiroDft        s              1  0 INZ(*zeros)
009200080418     D*------------------
009300080418     D* Costanti
009400080418     D*------------------
009500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009700100318
009800100318     D*------------------
009900100318     D* LINKING A DEFINIZIONI ESTERNE
010000100318     D*------------------
010100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010300100318
010400010330
010500010330
010600010330
010700000913     C                   reset                   rrnum
010800990921     C                   reset                   esito
010900990921     C                   reset                   wrkesito
011000010601     C*
011100010601     C                   exsr      opeini
011200010605     C                   exsr      rwvab
011300010601     C*
011400010601     C                   seton                                        lr
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012000010601     C*--------------------------------------------------------
012100010601     C     PREELA        BEGSR
012200010601     C*
012300010601     C* SE OCCORRE SPEDIRE IN FILIALE
012400010601     C                   if        invfil <> *zeros and
012500010601     C                             flgGiro = '0'
012600010601     C*
012700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012800010601     C                   eval      flgGiro = '1'
012900010601     C*
013000010601     C                   endif
013100010601     C*
013200010601     C                   ENDSR
013300010601     C***
013400010601
013500010601
013600010601
013700010601
013800010601     C*--------------------------------------------------------
013900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014000010601     C*--------------------------------------------------------
014100010601     C     ENDELA        BEGSR
014200040119     C*
014300040119     C* Effettuo la chiamata al CLLE preposto
014400040119     C                   call(e)   'TITVVTC'
014500040510     C                   parm                    parccm
014600040119     C                   parm                    parmbr
014700040119     C                   parm      '2'           paropz
014800050201     C*
014900050201     C* Effettuo lancio TISI95 solo x chiusura
015000050201     C                   CLEAR                   TISI95DS
015100050201     C                   EVAL      I95TLA = 'C'
015200050201     C                   CALL      'TISI95R'
015300050201     C                   PARM                    TISI95DS
015400000616     C*
015500010601     C                   ENDSR
015600010601
015700010601
015800010330     C*--------------------------------------------------------
015900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016000010330     C*--------------------------------------------------------
016100010605     C     RWVAB         BEGSR
016200010330     C*
016300010330     C                   if        not %open(tivin00r)
016400010330     C                   open      tivin00r
016500010330     C                   endif
016600021025     C                   if        not %open(fivabwwr)
016700021025     C                   open      fivabwwr
016800010330     C                   endif
016900040119     C*
017000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017100040119     C                   exsr      prevat
017200040119     C*
017300040119     C                   if        chkcall = '0'
017400040119     C*
017500040119     C                   if        not %open(fivatwwr)
017600040119     C                   open      fivatwwr
017700040119     C                   endif
017800010330     C*
017900040119     C                   clear                   §CTROKVB          7 0
018000040119     C                   clear                   §CTROKVT          7 0
018100040119     C                   clear                   §CTRMO            7 0
018200040119     C                   clear                   §CTRNO            7 0
018300120410     C*
018400120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018500120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018600120410     C                   IF        Intestazioni = *off
018700120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018800120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018900120410     C                   exsr      inzvar
019000120410     C                   exsr      impvab
019100120410     C                   ENDIF
019200010330     C*
019300030822     C                   do        *HIVAL
019400010330     C*
019500030822     C                   read      tivin00r                               70
019600010618     C*
019700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019800010618     C                   if        vinflg = '1'
019900010618     C                   eval      flgOk = '1'
020000010618     C                   endif
020100040510     C*
020200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020300040510     C                   z-add     *zeros        wLenVINDTA        4 0
020400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020800040510     C                   z-add     1             wFlgVINDTA
020900040510     C                   leave
021000040510     C                   endif
021100040510     C                   enddo
021200010618     C*
021300040510     C                   if        vindta > *blanks AND
021400040510     C                             wFlgVINDTA = 1
021500010330     C                   add       1             rrnum
021600010330     C*
021700010601     C                   if        *in70 = *off and
021800010330     C                             (vinflg = *blanks
021900010330     C                              or vinflg = '0'
022000010330     C                              or vinflg = '2')
022100010330     C*
022200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022300010711     C                   if        vinflg = *blanks or vinflg = '0'
022400010711     C                   clear                   vinmsg
022500010711     C                   endif
022600130917     C*
022700130917     C* Aggiungo un byte a blanks all'inizio d ogni record
022800130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022900130917     C* evitare che vengano elisi i blank
023000140220     C***                eval      vindta  = '*' + %trim(vindta)
023100130917     C*
023200080527     C                   exsr      inzvar
023300100318     C                   exsr      normCSV
023400010605     C                   exsr      impvab
023500010601     C*
023600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023700010605     C                   eval      depfil = VABLNP
023800010601     C                   exsr      repfil
023900010601     C                   if        depfil = invfil
024000021025     C                   if        vlrpoi = 999
024100030822     C                   move(P)   invfil        VABFGS
024200021025     C                   else
024300030822     C                   move(P)   vlrpoi        VABFGS
024400021025     C                   endif
024500010601     C*
024600010601     C                   exsr      PREELA
024700010601     C*
024800010604     C* Ebbene...
024900010604     C*
025000040119     C  N31              add       1             §CTROKVB
025100040119     C   32              add       1             §CTRMO
025200040119     C   31              add       1             §CTRNO
025300080418     C*
025400030822     C                   if        wGiro = 2
025500080513     C* Scrivo sempre il VAT
025600080513     C  N31              exsr      wrivat
025700110201     C                   exsr      chkvab
025800080513     C                   endif
025900010604     C*
026000010604     C                   if        *in31 = *off and
026100010604     C                             *in32 = *off
026200010604     C                   eval      vinflg = '1'
026300010604     C                   else
026400010604     C                   eval      vinflg = '2'
026500010604     C                   endif
026600010604     C                   endif
026700010601     C*
026800010604     C                   endif
026900010604     C*
027000010330     C                   else
027100010330     C                   eval      vinflg = '1'
027200010330     C                   endif
027300010601     C*
027400010601     C  N70              update    tivin000
027500010330     C*
027600030822     C  N70              enddo
027700040119     C*
027800080527     C* Verifico dati rimasti "in cannna"...
027900080527     C                   seton                                        65
028000110201     C                   exsr      chkvab
028100080527     C*
028200040119     C                   endif                                                  (endif - chkcall)
028300010601     C*
028400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028500010601     C                   if        cntNonEl = *zeros or
028600010601     C                             flgMulti = '0'
028700010330     C* Se non ci sono record con errori ...
028800010601     C                   if        §ctrno = 0 and
028900010604     C                             §ctrmo = 0 and
029000010601     C                             flgStato <> '2'
029100010330     C* ... restituisco esito OK.
029200010330     C                   eval      wrkesito = '0'
029300010330     C                   else
029400040119     C                   if        §ctrokvb > 0
029500010330     C                   eval      wrkesito = '1'
029600010330     C                   else
029700010615     C                   if        flgOk = '0'
029800010615     C                   eval      wrkesito = '2'
029900010615     C                   else
030000010615     C                   eval      wrkesito = '6'
030100010615     C                   endif
030200010330     C                   endif
030300010330     C                   endif
030400010601     C                   else
030500010601     C                   eval      wrkesito = '9'
030600010601     C                   endif
030700010330     C*
030800010330     C                   if        %open(tivin00r)
030900010330     C                   close     tivin00r
031000010330     C                   endif
031100021025     C                   if        %open(fivabwwr)
031200021025     C                   close     fivabwwr
031300010330     C                   endif
031400040512     C                   if        %open(fivatwwr)
031500040512     C                   close     fivatwwr
031600040512     C                   endif
031700010601     C*
031800010601     C                   if        vlrpoi <> 999
031900010601     C                   eval      invfil = vlrpoi
032000010601     C                   endif
032100010330     C*
032200040119     C                   if        §ctrokvb > 0
032300010601     C                             and invfil > *zeros
032400010330     C                   exsr      invio
032500010330     C                   endif
032600010601     C*
032700010618     C                   if        flgGiro = '1'
032800010601     C                   exsr      endela
032900010618     C                   endif
033000010330     C*
033100010330     C                   ENDSR
033200010330     C***
033300080527
033400080527
033500080527
033600080527     C*----------------------------------------------------*
033700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033800080527     C*----------------------------------------------------*
033900100413     C     CHKVAB        BEGSR
034000110201     C*
034100110201     C* Se richiesti ignorare colli / peso / volume
034200110315 xxx C***                if        *in35
034300110315 xxx C***                z-add     *zeros        VABNCL
034400110315 xxx C***                z-add     *zeros        VABPKB
034500110315 xxx C***                z-add     *zeros        VABVLB
034600110315 xxx C***                endif
034700080527     C*
034800080527     C* Scrivo il VAB solo a rottura d spedizione
034900080527     C                   if        depspe = *zeros
035000080527     C  N31              eval      fivabds_sav = fivabds
035100080527     C                   eval      depspe = vabNSP
035200080527     C                   else
035300080527     C                   if        depspe = vabNSP AND *in65 = *off
035400100413     C   51              add       VABNCL        sav_VABNCL
035500100413     C   51              add       VABPKB        sav_VABPKB
035600100413     C   51              add       VABVLB        sav_VABVLB
035700100720     C   52              z-add     VABNCL        sav_VABNCL
035800100720     C   52              z-add     VABPKB        sav_VABPKB
035900100720     C   52              z-add     VABVLB        sav_VABVLB
036000100720     C   53              add       VABNCL        sav_VABNCL
036100100720     C   53              z-add     VABPKB        sav_VABPKB
036200100720     C   53              z-add     VABVLB        sav_VABVLB
036300080527     C                   else
036400080527     C  N31              eval      fivabds_ok = fivabds_sav
036500080527     C  N31              exsr      wrivab
036600080527     C  N31              eval      fivabds_sav = fivabds
036700080527     C                   eval      depspe = vabNSP
036800080527     C                   endif
036900080527     C                   endif
037000080527     C*
037100080527     C                   ENDSR
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037700040119     C*----------------------------------------------------*
037800040510     C     WRIVAB        BEGSR
037900080513     C*
038000080513     C* Gestisco l'eventuale rottura x numero spedizione
038100080527     C                   if        VABNSP <> depspe or *in65 = *on
038200070301     C                   if        wVATNOT_E <> *blanks
038300130610     C                   eval      ok_VABCTM = '7Q'
038400070301     C                   endif
038500100413     C                   write     fivab000
038600080513     C                   endif
038700040119     C*
038800040119     C                   ENDSR
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039400040119     C*----------------------------------------------------*
039500040510     C     WRIVAT        BEGSR
039600060512     C*
039700060512     C* Inizializzo i campi chiave
039800060512     C                   eval      VATFGS = VABFGS
039900060512     C                   eval      VATAAS = VABAAS
040000060512     C                   eval      VATCCM = VABCCM
040100070823     C                   eval      VATNRS = VABNRS
040200060512     C                   eval      VATNSP = VABNSP
040300060512     C                   eval      VATLNP = VABLNP
040400040119     C*
040500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040600040510     C                   if        VATTRC = *blanks
040700040510     C                   eval      VATTRC = 'E'
040800040510     C                   endif
040900110318     C                   if        VATNOT <> *blanks
041000040510     C                   write     FIVAT000
041100040119     C                   add       1             §CTROKVT
041200110318     C                   endif
041300060331     C*
041400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041500060331     C                   if        wVATNOT_A <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_A
041700060331     C                   eval      VATTRC = 'A'
041800060331     C                   write     FIVAT000
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100060331     C*
042200060331     C                   if        wVATNOT_B <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_B
042400060331     C                   eval      VATTRC = 'B'
042500060331     C                   write     FIVAT000
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800140711     C*
042900140711     C                   if        wVATNOT_IJ <> *blanks
043000140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043100140711     C                   eval      VATTRC = 'I'
043200140711     C                   write     FIVAT000
043300140711     C                   add       1             §CTROKVT
043400140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
043500140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
043600140711     C                   eval      VATTRC = 'J'
043700140711     C                   write     FIVAT000
043800140711     C                   endif
043900140711     C                   endif
044000060331     C*
044100060331     C                   if        wVATNOT_E <> *blanks
044200060331     C                   eval      VATTRC = 'E'
044300140527     C*
044400140711     C* NON Sviluppare i "CHI SONO"
044500140527     C*
044600140711     C                   EVAL      VATNOT = wVATNOT_E
044700140711     C                   write     FIVAT000
044800140711     C                   add       1             §CTROKVT
044900120320     C                   endif
045000110927     C*
045100110927     C                   if        wVATNOT_I <> *blanks
045200110927     C                   eval      VATNOT = wVATNOT_I
045300140711     C                   eval      VATTRC = 'I'
045400110927     C                   write     FIVAT000
045500110927     C                   add       1             §CTROKVT
045600110927     C                   endif
045700140220     C*
045800140220     C                   if        wVATNOT_J <> *blanks
045900140220     C                   eval      VATNOT = wVATNOT_J
046000140220     C                   eval      VATTRC = 'J'
046100140220     C                   write     FIVAT000
046200140220     C                   add       1             §CTROKVT
046300140220     C                   endif
046400140319     C*
046500140319     C                   if        wVATNOT_S <> *blanks
046600140319     C                   eval      VATNOT = wVATNOT_S
046700140319     C* i 2 flag devono essere blank
046800140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
046900140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047000140319     C                   eval      VATTRC = 'S'
047100140319     C                   write     FIVAT000
047200140319     C                   add       1             §CTROKVT
047300140319     C                   endif
047400040119     C*
047500040119     C                   ENDSR
047600040119
047700040119
047800040119
047900040119     C*----------------------------------------------------*
048000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048100040119     C*----------------------------------------------------*
048200040119     C     PREVAT        BEGSR
048300040119     C*
048400040119     C* Compongo il nome del membro da dare al FIVATWWR
048500040119     C                   eval      parmbr = vlrhdl
048600040119     C                   movel     'M'           parmbr
048700040510     C                   eval      parccm = vlrksc
048800040119     C                   eval      paropz = '1'
048900040119     C* Effettuo la chiamata al CLLE preposto
049000040119     C                   call(e)   'TITVVTC'
049100040119     C                   parm                    parccm
049200040119     C                   parm                    parmbr
049300040119     C                   parm                    paropz
049400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049500040119     C                   if        %error
049600040119     C                   movel     '1'           chkcall
049700040119     C                   else
049800040119     C                   movel     '0'           chkcall
049900040119     C                   endif
050000040119     C*
050100040119     C                   ENDSR
050200010601
050300010601
050400010601
050500010330     C*----------------------------------------------------*
050600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050700010330     C*----------------------------------------------------*
050800040119     C     INZVAR        BEGSR
050900080527     C*
051000080527     C                   setoff                                       65
051100010330     C*
051200030822     C* Inizializzo variabili di wrk
051300020204     C                   Z-ADD     *zeros        Num5_0            5 0
051400101018     C                   MOVEL     *blanks       wFlgCAS
051500060331     C                   MOVEL     *blanks       wVATNOT_A
051600060331     C                   MOVEL     *blanks       wVATNOT_B
051700060331     C                   MOVEL     *blanks       wVATNOT_E
051800110927     C                   MOVEL     *blanks       wVATNOT_I
051900140220     C                   MOVEL     *blanks       wVATNOT_J
052000140220     C                   MOVEL     *blanks       wVATNOT_IJ
052100140319     C                   MOVEL     *blanks       wVATNOT_S
052200120411     C                   MOVEL     *blanks       wNOT
052300120411     C                   MOVEL     *blanks       Data8A
052400030822     C*
052500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052600030822     C                   CLEAR                   SkSplitCSV
052700030822     C                   EVAL      i = 1
052800030822     C                   EVAL      posDa = *zeros
052900030822     C                   EVAL      posA  = *zeros
053000080527     C*
053100080527     C                   CLEAR                   FIVABDS
053200080527     C                   CLEAR                   FIVAT000
053300030822     C*
053400030822     C* Reimposto i valori di default
053500030822     C                   EXSR      DEFCAM
053600030822     C*
053700010330     C                   ENDSR
053800010330     C*----------------------------------------------------*
053900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054000010330     C*----------------------------------------------------*
054100010330     C     DEFCAM        BEGSR
054200030822     C*
054300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054400030715     C* e delimitatore testo.
054500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054800100318     C*
054900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055000100318     C                   EVAL      CharSOS = CharNUM
055100030822     C*
055200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055300100720     C                   SETOFF                                       505152
055400100720     C                   SETOFF                                       53
055500100413     C                   SELECT
055600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055700100720     C                   SETON                                        50
055800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055900100413     C                   SETON                                        51
056000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056100100720     C                   SETON                                        52
056200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056300100720     C                   SETON                                        53
056400100413     C                   ENDSL
056500030822     C*
056600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056700030822     C                   EVAL      posDaDft = 1
056800030822     C                   EVAL      posADft  = 0
056900030822     C                   EVAL      wGiroDft = 0
057000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057100030822     C                             posDaDft > 0
057200030822     C*
057300030822     C* Gestisco il 1° giro
057400030822     C                   IF        wGiroDft = 0
057500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057700030822     C* Incremento il contatore dei "giri"
057800030822     C                   EVAL      wGiroDft = 1
057900030822     C                   ELSE
058000030822     C                   EVAL      posDaDft = posADft
058100030822     C                   ENDIF
058200030822     C* Eseguo lo scan x trovare la fine del campo corrente
058300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058400030822     C*
058500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058700030822     C                   IF        posDaDft > 0 AND
058800030822     C                             posADft  > 0
058900030822     C* NCL
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'NCL'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABNCL
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* CCM
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'CCM'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABCCM
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* LNP
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'LNP'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABLNP
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* NRS
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'NRS'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiInt=*on
063800030822     C                   Z-ADD     PiVal         VABNRS
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* CTR
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'CTR'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiInt=*on
065100030822     C                   Z-ADD     PiVal         VABCTR
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* PKB
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'PKB'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400130610     C                   Z-ADD     PiVal         VABPKB
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* VLB
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'VLB'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700130610     C                   Z-ADD     PiVal         VABVLB
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* QFT
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'QFT'
068500030822     C                   EVAL      PiStr=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   EXSR      CHKNUM
068900030822     C                   IF        PiNum=*on
069000030822     C                   Z-ADD     PiVal         VABQFT
069100030822     C                   ENDIF
069200030822     C                   ENDIF
069300030822     C* CBO
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'CBO'
069800030822     C                   EVAL      VABCBO=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* TSP
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'TSP'
070700030822     C                   EVAL      VABTSP=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* VAS
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'VAS'
071600030822     C                   EVAL      VABVAS=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VCA
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VCA'
072500030822     C                   EVAL      VABVCA=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900070925     C* TIC
073000070925     C                   IF        %subst(
073100070925     C                             %subst(vlrppt:posDaDft+1:
073200070925     C                             posADft-posDaDft-1):1:3)
073300070925     C                             = 'TIC'
073400070925     C                   EVAL      VABTIC=%trim(%subst(
073500070925     C                             %subst(vlrppt:posDaDft+1:
073600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700070925     C                   ENDIF
073800030822     C* GCA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GCA'
074300030822     C                   EVAL      VABGCA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* CTM
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'CTM'
075200030822     C                   EVAL      VABCTM=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* FFD
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'FFD'
076100030822     C                   EVAL      VABFFD=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* VAD
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'VAD'
077000030822     C                   EVAL      VABVAD=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* GMA
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'GMA'
077900030822     C                   EVAL      VABGMA=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GGA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GGA'
078800030822     C                   EVAL      VABGGA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* GVA
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'GVA'
079700030822     C                   EVAL      VABGVA=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* TC1
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'TC1'
080600030822     C                   EVAL      VABTC1=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* TC2
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'TC2'
081500030822     C                   EVAL      VABTC2=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900040714     C* VATTRC
082000040714     C                   IF        %subst(
082100040714     C                             %subst(vlrppt:posDaDft+1:
082200040714     C                             posADft-posDaDft-1):1:3)
082300040714     C                             = 'TRC'
082400040714     C                   EVAL      VATTRC=%trim(%subst(
082500040714     C                             %subst(vlrppt:posDaDft+1:
082600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700040714     C                   ENDIF
082800030822     C* ...
082900030822     C                   ENDIF
083000030822     C                   ENDDO
083100020204     C*
083200010330     C                   ENDSR
083300010607     C*----------------------------------------------------*
083400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083500010607     C*----------------------------------------------------*
083600010607     C     IMPVAB        BEGSR
083700010607     C*
083800010607     C                   SETOFF                                       3132
083900110315 xxx C***                SETON                                        35
084000030515     C*
084100010607     C                   MOVEL     datcor        VABAAS
084200010607     C                   MOVE      datcor        VABMGS
084300010607     C*
084400030715     C**********
084500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084600030715     C**********
084700030715     C                   IF        wGiro = *zeros
084800040113     C*
084900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085200080429     C                             %trim(intcol(5))<>*blanks
085300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085500080429     C                                      %trim(intcol(5))
085600070823     C     ';':CharCSV   XLATE     vindta        vindta
085700040510     C                   ENDIF
085800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085900031201     C     minu:maiu     XLATE     vindta        vindta
086000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086100030715     C                   DOW       posDa <= %len(%trim(vindta))
086200030715     C*
086300030715     C* Gestisco il 1° campo
086400030715     C                   IF        i = 1
086500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086600030715     C                   EVAL      posDa = 1
086700030715     C* Eseguo lo scan x trovare la fine del primo campo
086800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086900030715     C* A questo punto "estraggo" il campo corrente
087000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087100030715     C                                                    (posA-posDa))
087200030715     C* X i campi successivi al 1°
087300030715     C                   ELSE
087400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087500030715     C                   EVAL      posDa = posA + 1
087600030715     C* Eseguo lo scan x trovare la fine del campo corrente
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* Gestisco l'ultimo campo
087900030715     C                   IF        posA = *zeros
088000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088100030715     C* A questo punto "estraggo" il campo corrente
088200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088300030715     C                                                    (posA-posDa))
088400030715     C                   LEAVE
088500030715     C                   ELSE
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C                   ENDIF
089000030715     C                   ENDIF
089100030715     C* Incremento il contatore d campo
089200030715     C                   EVAL      i = i +1
089300030715     C                   ENDDO
089400030715     C*
089500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089600030715     C* delimitazione testo
089700030715     C                   EVAL      i = 1
089800030715     C                   DOW       i < %elem(SkSplitFLD)
089900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090100030715     C                   EVAL      i = i + 1
090200030715     C                   ENDDO
090300030715     C                   EVAL      wGiro = 1
090400030715     C                   ELSE
090500030715     C                   EVAL      wGiro = 2
090600031204     C**********
090700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090800031204     C* d separatore campo
090900031204     C**********
091000031204     C                   EVAL      vindta = %trim(vindta)
091100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091200031204     C                   EVAL      vindta = %subst(vindta:2)
091300031204     C                   ENDDO
091400031204     C*
091500031204     C                   Z-ADD     *zeros        lunghInput        4 0
091600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092000110912     C                   ENDDO
092100030715     C**********
092200030715     C* Effettuo lo split del campo dati d input
092300030715     C**********
092400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092500030715     C                   DOW       posDa <= %len(%trim(vindta))
092600030715     C*
092700030715     C* Gestisco il 1° campo
092800030715     C                   IF        i = 1
092900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093000030715     C                   EVAL      posDa = 1
093100030715     C* Eseguo lo scan x trovare la fine del primo campo
093200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093300030715     C* A questo punto "estraggo" il campo corrente
093400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093500030715     C                                                    (posA-posDa))
093600030715     C* X i campi successivi al 1°
093700030715     C                   ELSE
093800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093900030715     C                   EVAL      posDa = posA + 1
094000031204     C* Verifico che nn vi sia il campo nn valorizzato
094100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094200031204     C* Se campo nn valorizzato skippo al prossimo
094300031204     C                   EVAL      SkSplitCSV(i) = *blanks
094400031204     C                   EVAL      posA  = posA + 1
094500031204     C                   ELSE
094600030715     C* Eseguo lo scan x trovare la fine del campo corrente
094700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094800030715     C* Gestisco l'ultimo campo
094900030715     C                   IF        posA = *zeros
095000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095100030715     C* A questo punto "estraggo" il campo corrente
095200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095300030715     C                                                    (posA-posDa))
095400030715     C                   LEAVE
095500030715     C                   ELSE
095600030715     C* A questo punto "estraggo" il campo corrente
095700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095800030715     C                                                    (posA-posDa))
095900030715     C                   ENDIF
096000031204     C                   ENDIF
096100030715     C                   ENDIF
096200030715     C* Incremento il contatore d campo
096300030715     C                   EVAL      i = i +1
096400030715     C                   ENDDO
096500030715     C*
096600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096700030715     C* delimitazione testo
096800030715     C                   EVAL      i = 1
096900030715     C                   DOW       i < %elem(SkSplitCSV)
097000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097200030715     C                   EVAL      i = i + 1
097300030715     C                   ENDDO
097400100720     C*
097500100720     C* Stacco un progressivo x numero spedizione
097600140711 xxx C***                EXSR      repNSP
097700100326     C*
097800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097900030715     C                   EVAL      i = 1
098000030715     C                   DOW       i < %elem(SkSplitFLD)
098100030715     C*
098200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098400030715     C                             SkSplitCSV(i) <> *blanks
098500030721     C*** ATB
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098700030721     C                   EVAL      VABATB = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030715     C*** RSD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099400030715     C* ==
099500130528     C                   ENDIF
099600030721     C*** RD2
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030715     C*** IND
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100200030715     C                   EVAL      VABIND = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** LOD
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** PRD
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030721     C*** NZD
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030715     C*** RMA
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030715     C*** NOT
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** NT2
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** VCA
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** VAS
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** FFD
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030721     C*** GC1
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** GC2
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** TSP
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105100030721     C                   ENDIF
105200040510     C*** CBO
105300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105500040510     C                   ENDIF
105600030721     C*** NAS
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TIC
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GCA
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** XCO
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** CTM
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TCR
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** CTS
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** FTM
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** VAD
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GMA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GGA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GVA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TC1
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** TC2
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** SCL
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** RMO
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** NMO
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*
112500030721     C* Reperisco quindi i campi numerici...
112600040510     C*** NSP
112700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112800040510     C                   EVAL      PiStr=SkSplitCSV(i)
112900040510     C                   EXSR      CHKNUM
113000040510     C                   IF        PiInt=*on
113100040510     C                   Z-ADD     PiVal         VABNSP
113200040510     C                   ELSE
113300040510     C                   SETON                                        31
113400040510     C                   EVAL      VABNSP = *zeros
113500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040510     C                             + ' ' + 'VABNSP'
113700040510     C                   ENDIF
113800040510     C                   ENDIF
113900071106     C*** RMN
114000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114100071106     C                   EVAL      PiStr=SkSplitCSV(i)
114200071106     C                   EXSR      CHKNUM
114300071106     C                   IF        PiInt=*on
114400071106     C                   Z-ADD     PiVal         VABRMN
114500071106     C                   ELSE
114600071106     C                   SETON                                        32
114700071106     C                   EVAL      VABRMN = *zeros
114800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114900071106     C                             + ' ' + 'VABRMN'
115000071106     C                   ENDIF
115100071106     C                   ENDIF
115200030721     C*** CAD
115300130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115400130610     C                   EVAL      PiStr=SkSplitCSV(i)
115500130610     C                   EXSR      CHKNUM
115600130610     C                   IF        PiInt=*on
115700130610     C                   Z-ADD     PiVal         Num5_0
115800130610     C                   MOVEL(p)  Num5_0        VABCAD
115900130610     C                   ELSE
116000130610     C                   SETON                                        32
116100130610     C                   EVAL      VABCAD = *zeros
116200130610     C                   EVAL      vinmsg = %trimr(vinmsg)
116300130610     C                             + ' ' + 'VABCAD'
116400130610     C                   ENDIF
116500130605     C                   ENDIF
116600030721     C*** NCL
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on
117100030721     C                   Z-ADD     PiVal         VABNCL
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABNCL = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600040122     C                             + ' ' + 'VABNCL'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** PKB
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiNum=*on
118400130610     C                   Z-ADD(H)  PiVal         VABPKB
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABPKB = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABPKB'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** CAS
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119600140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
119700140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
119800140711     C                             %trim(SkSplitCSV(i)) <> ',00'
119900130703     C                   EVAL      VABVCA = 'EUR'
120000140711     C                   EVAL      VABTIC = 'TM'
120100140513     C                   EVAL      wFlgCAS = '1'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500030721     C                   Z-ADD     PiVal         VABCAS
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABCAS = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABCAS'
121100030721     C                   ENDIF
121200140513     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** CCM
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on      AND
121900030721     C                             PiVal<=9999999 AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABCCM
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABCCM = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040510     C                             + ' ' + 'VABCCM'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** LNP
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on    AND
123400030721     C                             PiVal<=999   AND
123500030721     C                             PiVal>*zeros
123600030721     C                   Z-ADD     PiVal         VABLNP
123700030721     C                   ELSE
123800040715     C                   SETON                                        32
123900030721     C                   EVAL      VABLNP = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100040510     C                             + ' ' + 'VABLNP'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** NRS
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on AND
124900030721     C                             PiVal<=99
125000030721     C                   Z-ADD     PiVal         VABNRS
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABNRS'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** LNA
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on    AND
126200030721     C                             PiVal<=999   AND
126300030721     C                             PiVal>*zeros
126400030721     C                   Z-ADD     PiVal         VABLNA
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      VABLNA = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABLNA'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** CTR
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on  AND
127700030721     C                             PiVal<=999
127800030721     C                   Z-ADD     PiVal         VABCTR
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABCTR'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** IAS
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABIAS
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABIAS'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** VLB
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200130610     C                   Z-ADD(H)  PiVal         VABVLB
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABVLB'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** QFT
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABQFT
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABQFT'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** NCD
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on      AND
132600030721     C                             PiVal<=9999999
132700030721     C                   Z-ADD     PiVal         VABNCD
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABNCD'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** NCA
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on      AND
133900030721     C                             PiVal<=9999999
134000030721     C                   Z-ADD     PiVal         VABNCA
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABNCA'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** ZNC
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on AND
135200030721     C                             PiVal<=99
135300030721     C                   Z-ADD     PiVal         VABZNC
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABZNC'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** DCR
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on       AND
136500030721     C                             PiVal<=99999999
136600030721     C                   Z-ADD     PiVal         VABDCR
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABDCR'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** HCR
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on   AND
137800030721     C                             PiVal<=9999
137900030721     C                   Z-ADD     PiVal         VABHCR
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABHCR'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** VMD
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiNum=*on
139100030721     C                   Z-ADD     PiVal         VABVMD
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABVMD'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** ANT
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiInt=*on        AND
140300030721     C                             PiVal<=999999999
140400030721     C                   Z-ADD     PiVal         VABANT
140500030721     C                   ELSE
140600030721     C                   SETON                                        32
140700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140800030721     C                             + ' ' + 'VABANT'
140900030721     C                   ENDIF
141000030721     C                   ENDIF
141100030721     C*** CMO
141200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141300030721     C                   EVAL      PiStr=SkSplitCSV(i)
141400030721     C                   EXSR      CHKNUM
141500030721     C                   IF        PiInt=*on
141600030721     C                   Z-ADD     PiVal         Num5_0
141700030721     C                   MOVEL(p)  Num5_0        VABCMO
141800030721     C                   ELSE
141900030721     C                   SETON                                        32
142000030721     C                   EVAL      VABCMO = *zeros
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABCMO'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500040510     C*
142600040510     C* Reperisco i campi del FIVAT
142700040510     C*** VATNOT
142800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143000040510     C                   ENDIF
143100040510     C*** VATTRC
143200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143400040510     C                   ENDIF
143500060331     C*** VATNOT_A
143600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143800060331     C                   ENDIF
143900060331     C*** VATNOT_B
144000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300060331     C*** VATNOT_E
144400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144600060331     C                   ENDIF
144700110927     C*** VATNOT_I
144800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145000110927     C                   ENDIF
145100140319     C*** VATNOT_S
145200140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
145300140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
145400140319     C                   ENDIF
145500070823     C*
145600100831     C***  ===>  Gestione campi "particolari" dentro ciclo
145700140513
145800140711     C*** W_VABNZD
145900140711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
146000140711     C                   EVAL      VABNZD = %trim(SkSplitCSV(i))
146100140711     C                   IF        VABNZD = 'IT'
146200140711     C                   EVAL      VABNZD = *blank
146300140527     C                   ENDIF
146400140711     C                   IF        VABNZD <> *blank
146500140711     C                   EVAL      VABCTR = 060
146600140711     C                   ENDIF
146700140527     C                   ENDIF
146800140711
146900140711     C*** W_VABRMN
147000140711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
147100140711     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo wRMA
147200140711     C                   EVAL      ElencoChar = '1234567890'
147300140711     C                   EVAL      TxtInOut   = %trim(SkSplitCSV(i))
147400140711     C                   EVAL      TipoElenco = *blanks
147500140711     C                   EVAL      CharSost   = *blanks
147600140711     C                   EVAL      UpperCase  = *blanks
147700140711     C                   EVAL      ChkNull    = '1'
147800140711     C                   EVAL      CharNull   = *blanks
147900140711     C                   EVAL      EsitoBon   = *blanks
148000140711     C*
148100140711     C                   CALL      'XCHKCHAR'
148200140711     C                   PARM                    TxtInOut
148300140711     C                   PARM                    ElencoChar
148400140711     C                   PARM                    TipoElenco
148500140711     C                   PARM                    CharSost
148600140711     C                   PARM                    UpperCase
148700140711     C                   PARM                    ChkNull
148800140711     C                   PARM                    CharNull
148900140711     C                   PARM                    EsitoBon
149000140711     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
149100140711     C                   IF        EsitoBon = '1'
149200140711     C* tolgo gli 0 non significativi
149300140711     C                   EVALR     VABRMA = %trim(TxtInOut)
149400140711     C                   EVAL      VABRMA=%xlate(' ':'0':VABRMA)
149500140711     C                   MOVE      VABRMA        VABRMN
149600140711     C                   MOVE      VABRMA        VABNSP
149700140711     C                   ENDIF
149800140711     C                   ENDIF
149900140527
150000140711     C*** W_VATNOTIJ
150100140711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
150200140711     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
150300140711     C                   ENDIF
150400140711
150500140711     C*** W_VABCBO
150600140711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
150700140711     C                   IF        %trim(SkSplitCSV(i)) = 'A'
150800140711     C                   EVAL      VABCBO = '2'
150900140711     C                   ENDIF
151000140711     C                   ENDIF
151100140711
151200140711     C*** W_VATNOT_E
151300140711     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
151400140711     C                   EVAL      wVATNOT_E = %subst(%trim(SkSplitCSV(i)):2:12)
151500140711     C                   ENDIF
151600070925     C*
151700071029     C***  <===  -----------------------------------------
151800040510     C*
151900030715     C                   ENDIF
152000030715     C                   EVAL      i = i + 1
152100030715     C                   ENDDO
152200070925     C*
152300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152400140319     C*
152500070925     C*
152600071029     C***  <===  -----------------------------------------
152700070925     C*
152800070925     C* Considerazioni finali su CBO/CAS
152900140220     C                   IF        wFlgCAS = '1'    OR
153000140220     C                             VABCAS  > *zeros
153100140220     C                   IF        VABCBO = '1'
153200140220     C                   EVAL      VABCBO = '4'
153300140220     C                   ENDIF
153400140220     C                   IF        VABCBO = '2'
153500140220     C                   EVAL      VABCBO = '6'
153600140220     C                   ENDIF
153700140220     C                   ENDIF
153800070925     C***  <===  ----------------------------
153900040510     C*
154000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154100040510     C                   IF        VABNSP = *zeros
154200040510     C                   SETON                                        31
154300040510     C                   ENDIF
154400040715     C*
154500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154600040715     C                   IF        VABLNP = *zeros
154700040715     C                   SETON                                        31
154800040715     C                   ENDIF
154900040510     C*
155000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155100120411     C                   IF        VABRMN = *zeros
155200120411     C                   Z-ADD     VABNSP        VABRMN
155300120411     C                   ENDIF
155400120328     C* IN QUESTO CASO LO FORZO = 1
155500120411     C***                IF        VABRMN = *zeros
155600120411     C***                Z-ADD     1             VABRMN
155700120411     C***                ENDIF
155800040510     C*
155900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156000040510     C                   IF        VABRMA = *blanks
156100040714     C                   MOVEL     VABRMN        VABRMA
156200040510     C                   ENDIF
156300040128     C*
156400040510     C* Se provincia nn valorizzata la reperisco
156500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156600110912     C                   IF        VABPRD =  *blanks AND
156700110912     C                             VABNZD =  *blanks AND
156800110912     C                             VABCAD <> *blanks AND
156900110912     C                             VABLOD <> *blanks
157000040128     C                   CLEAR                   TISI95DS
157100040128     C                   EVAL      I95TCN = '3'
157200040128     C                   Z-ADD     datcor        I95DAT
157300040128     C                   EVAL      I95NAR = VABNZD
157400040128     C                   EVAL      I95CAP = VABCAD
157500040128     C                   EVAL      I95LOC = VABLOD
157600040128     C                   CALL      'TISI95R'
157700040128     C                   PARM                    TISI95DS
157800040128     C                   EVAL      VABPRD = O95PRV
157900040510     C                   ENDIF
158000020204     C*
158100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158200020204     C                   EXSR      CHKIMPDIV
158300030715     C                   ENDIF
158400020204     C*
158500010607     C                   ENDSR
158600010607     C*----------------------------------------------------*
158700010601
158800020204
158900020204     C*----------------------------------------------------*
159000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159100020204     C*----------------------------------------------------*
159200020204     C     CHKIMPDIV     BEGSR
159300020204     C*
159400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159500020204     C                   Z-ADD     *zeros        wrkDec            9 9
159600020204     C*
159700020204     C* Come prima cosa effettuo considerazioni sulla divisa
159800020204     C                   IF        vabIAS > *zeros
159900020204     C                   IF        vabVAS <> 'EUR'
160000020204     C                   EVAL      vabVAS =  'ITL'
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C                   IF        vabCAS > *zeros
160500020204     C                   IF        vabVCA <> 'EUR'
160600020204     C                   EVAL      vabVCA =  'ITL'
160700020204     C                   ENDIF
160800020204     C                   ENDIF
160900020204     C*
161000020204     C                   IF        vabVMD > *zeros
161100020204     C                   IF        vabVAD <> 'EUR'
161200020204     C                   EVAL      vabVAD =  'ITL'
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161700020204     C                   Z-ADD     vabIAS        wrkDec
161800020204     C                   IF        wrkDec > *zeros
161900020204     C                   IF        vabVAS = 'ITL'
162000020204     C                   EVAL      vabIAS = *zeros
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162500020204     C                   Z-ADD     vabCAS        wrkDec
162600020204     C                   IF        wrkDec > *zeros
162700020204     C                   IF        vabVCA = 'ITL'
162800020204     C                   EVAL      vabCAS = *zeros
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163300020204     C                   Z-ADD     vabVMD        wrkDec
163400020204     C                   IF        wrkDec > *zeros
163500020204     C                   IF        vabVAD = 'ITL'
163600020204     C                   EVAL      vabVMD = *zeros
163700020204     C                   ENDIF
163800020204     C                   ENDIF
163900020204     C*
164000020204     C                   ENDSR
164100020204     C***
164200010330
164300100318
164400100318
164500100318     C*----------------------------------------------------*
164600100318     C*  CONTROLLO NUMERICITA' CAMPI
164700100318     C*----------------------------------------------------*
164800100318     C     CHKNUM        BEGSR
164900100318     C*
165000100318     C                   IF        PiDecChr = *blanks
165100100318     C                   EVAL      PiDecChr = CharNUM
165200100318     C                   ENDIF
165300100318     C*
165400100318     C                   callp     UBISNUM_Check(PiStr
165500100318     C                                          :PiDecChr
165600100318     C                                          :PiVal
165700100318     C                                          :PiNum
165800100318     C                                          :PiInt)
165900100318     C*
166000100318     C                   ENDSR
166100100318     C***
166200010601
166300010601
166400010601
166500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166600010601     C     repfil        BEGSR
166700010601     C*
166800010601     C                   if        invfil = *zeros and
166900010601     C                             depfil > *zeros and
167000010629     C                             (vinflg = *blanks or
167100010629     C                              vinflg = *zeros)
167200010601     C
167300010601     C                   eval      invfil = depfil
167400010601     C                   endif
167500010601     C*
167600010601     C                   if        depfil <> invfil and
167700010601     C                             invfil > *zeros
167800010601     C                   eval      flgMulti = '1'
167900010601     C                   if        vinflg = *blanks
168000010601     C                   add       1             cntNonEl
168100010601     C                   endif
168200010601     C                   endif
168300010601     C*
168400010601     C                   if        vinflg = '2'
168500010601     C                   eval      flgStato = '2'
168600010601     C                   endif
168700010601     C*
168800010601     C                   ENDSR
168900010601     C***
169000010601
169100010601
169200010601
169300010330
169400040119      /TITLE Invio dei dati al punto operativo.
169500040119     C     invio         BEGSR
169600040119     C*
169700040119     C* 1° invio FIVAT
169800040119     C                   reset                   dscmz
169900040119     C                   move      vlrpoi        cmzdst
170000040119     C                   eval      cmzfld = 'FIVATWWR'
170100040119     C                   eval      cmzmbd = vlrhdl
170200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170300040119     C***                if        prmfir = *blanks
170400040119     C                   eval      cmzfla = 'FIVAT00F'
170500040119     C                   eval      cmzmba = 'FIVAT00F'
170600040119     C***                else
170700040119     C***                eval      cmzfla = prmfir
170800040119     C***                eval      cmzmba = prmfir
170900040119     C***                endif
171000040119     C                   eval      cmznrr = *zeros
171100040119     C                   move      §ctrokvt      cmznrr
171200040119     C                   eval      cmzlba = vlrfl1
171300040119     C                   call(e)   'TIS711C'
171400040119     C                   parm                    dscmz
171500040119     C                   parm      *blanks       esito
171600040119     C                   if        %error
171700040119     C                             or cmzerr = '1'
171800040119     C                             or esito  = '1'
171900040119     C                   eval      wrkesito = '3'
172000040119     C                   else
172100040119     C*
172200040119     C* 2° invio FIVAB
172300040119     C                   reset                   dscmz
172400040119     C                   move      vlrpoi        cmzdst
172500040119     C                   eval      cmzfld = vlrfou
172600040119     C                   eval      cmzmbd = vlrhdl
172700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172800040119     C***                if        prmfir = *blanks
172900040119     C                   eval      cmzfla = 'FIVAB00F'
173000040119     C                   eval      cmzmba = 'FIVAB00F'
173100040119     C***                else
173200040119     C***                eval      cmzfla = prmfir
173300040119     C***                eval      cmzmba = prmfir
173400040119     C***                endif
173500040119     C                   eval      cmznrr = *zeros
173600040119     C                   move      §ctrokvb      cmznrr
173700040119     C                   eval      cmzlba = vlrfl1
173800040119     C                   call(e)   'TIS711C'
173900040119     C                   parm                    dscmz
174000040119     C                   parm      *blanks       esito
174100040119     C                   if        %error
174200040119     C                             or cmzerr = '1'
174300040119     C                             or esito  = '1'
174400040119     C                   eval      wrkesito = '3'
174500040119     C                   endif
174600040119     C                   endif
174700040119     C*
174800040119     C                   ENDSR
174900040119     C***
175000010601
175100010601
175200010601
175300010601
175400010601      /TITLE Invio dei dati al punto operativo.
175500010601     C     opeini        BEGSR
175600010601     C*
175700010601     C* Inizializzo flag e contatori operativi
175800010601     C                   movel     '0'           flgGiro           1
175900010601     C                   movel     '0'           flgMulti          1
176000010601     C                   movel     '1'           flgStato          1
176100010615     C                   movel     '0'           flgOk             1
176200010601     C                   z-add     *zeros        cntNonEl         10 0
176300010601     C                   z-add     *zeros        depfil            3 0
176400010601     C                   z-add     *zeros        invfil            3 0
176500010601     C*
176600010601     C                   ENDSR
176700010601     C***
176800070326
176900070326
177000070326
177100070326
177200070326     C     *pssr         BEGSR
177300070329     C*
177400070329     C                   if        %open(tivin00r)
177500070329     C                   close     tivin00r
177600070329     C                   endif
177700070329     C                   if        %open(fivabwwr)
177800070329     C                   close     fivabwwr
177900070329     C                   endif
178000070329     C                   if        %open(fivatwwr)
178100070329     C                   close     fivatwwr
178200070329     C                   endif
178300070326     C*
178400070326     C* Effettuo la chiamata al CLLE preposto
178500070326     C                   call(e)   'TITVVTC'
178600070326     C                   parm                    parccm
178700070326     C                   parm                    parmbr
178800070326     C                   parm      '2'           paropz
178900070326     C*
179000070326     C                   eval      wrkesito = '2'
179100070404     C*
179200070404     C                   seton                                        LR
179300070326     C*
179400070326     C                   ENDSR     '*CANCL'
179500070326     C***
179600100318
179700100318
179800100318
179900100318     C     normCSV       BEGSR
180000100318     C*
180100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180200100318     C* separatori campi
180300100318     C                   if        CharTXT <> *blanks
180400100318     C                   z-add     *zeros        wLenVINDTA        4 0
180500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180600100318     C                   setoff                                       80
180700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181000100318     C                   if        not *in80
181100100318     C                   seton                                        80
181200100318     C                   else
181300100318     C                   setoff                                       80
181400100318     C                   endif
181500100318     C                   endif
181600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181700100318     C                             and *in80 = *on
181800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181900100318     C                   endif
182000100318     C                   enddo
182100100318     C                   endif
182200100318     C*
182300100318     C                   ENDSR
182400100318     C***
182500070326
182600100326
182700100326
182800100326     C*----------------------------------------------------*
182900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183000100326     C*----------------------------------------------------*
183100100326     C     REPNSP        BEGSR
183200100326     C*
183300100326     C* NSP => Stacco un numeratore da AZNUM
183400100326     C                   clear                   TRUL33DS
183500100326     C                   eval      I33OPE = *zeros
183600100326     C                   eval      I33CNU = 302
183700100326     C                   eval      I33NUM = 1
183800100326     C                   movel     TRUL33DS      KPJBU
183900100326     C                   call      'TRUL33R'
184000100326     C                   parm                    KPJBA
184100100326     C                   movel     KPJBU         TRUL33DS
184200100326     C                   if        O33ERR = *zeros
184300100326     C                   z-add     O33NRF        VABNSP
184400100326     C                   z-add     O33NRF        VATNSP
184500100326     C                   else
184600100326     C                   SETON                                        31
184700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184800140711     C                             + ' ' + 'VABNSP VATNSP'
184900100326     C                   endif
185000100326     C*
185100100326     C                   ENDSR
185200010330
185300100326
185400010330
185500000613     C     *inzsr        BEGSR
185600990910     C*
185700990910     C     *entry        plist
185800990920     C                   parm                    tivlrds
185900990921     C                   parm      wrkesito      esito
186000000724     C                   parm                    prmlit
186100000710     C                   parm                    prmfir
186200010330     C*
186300010330     C* CALCOLA LA DATA CORRENTE
186400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186500100318     C                   eval      datcor = %dec(%date() : *ISO)
186600000613     C*
186700000613     C                   ENDSR
186800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186900140711VABRSD;VABIND;VABLOD;VABCAD;VABPRD;W_VABNZD;W_VABRMN;VABPKB;VABVLB;VABNOT;VATNOT_B;VABCAS;
187000140711W_VATNOTIJ;X;X;W_VABCBO;VABNCL;W_VATNOT_E;X;
187100140512
187200111116
187300100318
