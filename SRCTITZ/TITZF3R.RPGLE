000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400150316      * i "CHI SONO" sono in orizzontale e al massimo 20
000500150316      * VABLNP/VATLNP/VABFGS/VATFGS dipendono dal cod.cliente
000600150114      *
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
004300150114     D Intestazioni    s               n   inz(*on)
004400100326     D*------------------
004500100326     D* DS REPERIMENTO NUMERATORE
004600100326     D*------------------
004700100326     D trul33ds      e ds                  inz
004800100326     D*------------------
004900100326     D* DS ARCHITETTURA
005000100326     D*------------------
005100100326     D kpjba         e ds                  inz
005200130611     D*------------------
005300130611     D TRUL28DSX     e ds                  inz
005400100326
005500030715     D*------------------
005600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005700030715     D*------------------
005800030715     D SkSplitFLD      S             10    DIM(100)
005900030715     D SkSplitCSV      S            256    DIM(100)
006000030715     D CharCSV         S              1
006100030715     D CharTXT         S              1
006200030715     D CharNUM         S              1
006300100318     D CharSOS         S              1
006400130626     D posDa           S              4  0 INZ(*zeros)
006500130626     D posA            S              4  0 INZ(*zeros)
006600030715     D i               s              3  0 INZ(1)
006700030715     D wGiro           s              1  0 INZ(*zeros)
006800070823     D wVATNOT_A       s                   LIKE(VATNOT)
006900060331     D wVATNOT_B       s                   LIKE(VATNOT)
007000150316     D*wVATNOT_E       s                   LIKE(VATNOT)
007100150316     D wVATNOT_E       DS           700
007200150316     D NOT_E                         35    DIM(20)
007300110927     D wVATNOT_I       s                   LIKE(VATNOT)
007400140220     D wVATNOT_J       s                   LIKE(VATNOT)
007500140220     D wVATNOT_IJ      s             70
007600140319     D wVATNOT_S       s                   LIKE(VATNOT)
007700140919     D wNOT            s             70
007800120411     D Data8A          s              8
007900140527     D wBarcodeN       s             11s 0
008000140527     D wX              s              3s 0
008100140711     D wRMA            s                   like(VABRMA)
008200140711     D TxtInOut        S           2048
008300140711     D ElencoChar      S            256
008400140711     D TipoElenco      S              1
008500140711     D CharSost        S              1
008600140711     D UpperCase       S              1
008700140711     D ChkNull         S              1
008800140711     D CharNull        S              1
008900140711     D EsitoBon        S              1
009000030822     D*------------------
009100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009200030822     D*------------------
009300030822     D posDaDft        S              3  0 INZ(*zeros)
009400030822     D posADft         S              3  0 INZ(*zeros)
009500030822     D j               s              3  0 INZ(1)
009600030822     D wGiroDft        s              1  0 INZ(*zeros)
009700080418     D*------------------
009800080418     D* Costanti
009900080418     D*------------------
010000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010200100318
010300100318     D*------------------
010400100318     D* LINKING A DEFINIZIONI ESTERNE
010500100318     D*------------------
010600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010800100318
010900010330
011000010330
011100010330
011200000913     C                   reset                   rrnum
011300990921     C                   reset                   esito
011400990921     C                   reset                   wrkesito
011500010601     C*
011600010601     C                   exsr      opeini
011700010605     C                   exsr      rwvab
011800010601     C*
011900010601     C                   seton                                        lr
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012500010601     C*--------------------------------------------------------
012600010601     C     PREELA        BEGSR
012700010601     C*
012800010601     C* SE OCCORRE SPEDIRE IN FILIALE
012900010601     C                   if        invfil <> *zeros and
013000010601     C                             flgGiro = '0'
013100010601     C*
013200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013300010601     C                   eval      flgGiro = '1'
013400010601     C*
013500010601     C                   endif
013600010601     C*
013700010601     C                   ENDSR
013800010601     C***
013900010601
014000010601
014100010601
014200010601
014300010601     C*--------------------------------------------------------
014400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014500010601     C*--------------------------------------------------------
014600010601     C     ENDELA        BEGSR
014700040119     C*
014800040119     C* Effettuo la chiamata al CLLE preposto
014900040119     C                   call(e)   'TITVVTC'
015000040510     C                   parm                    parccm
015100040119     C                   parm                    parmbr
015200040119     C                   parm      '2'           paropz
015300050201     C*
015400050201     C* Effettuo lancio TISI95 solo x chiusura
015500050201     C                   CLEAR                   TISI95DS
015600050201     C                   EVAL      I95TLA = 'C'
015700050201     C                   CALL      'TISI95R'
015800050201     C                   PARM                    TISI95DS
015900000616     C*
016000010601     C                   ENDSR
016100010601
016200010601
016300010330     C*--------------------------------------------------------
016400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016500010330     C*--------------------------------------------------------
016600010605     C     RWVAB         BEGSR
016700010330     C*
016800010330     C                   if        not %open(tivin00r)
016900010330     C                   open      tivin00r
017000010330     C                   endif
017100021025     C                   if        not %open(fivabwwr)
017200021025     C                   open      fivabwwr
017300010330     C                   endif
017400040119     C*
017500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017600040119     C                   exsr      prevat
017700040119     C*
017800040119     C                   if        chkcall = '0'
017900040119     C*
018000040119     C                   if        not %open(fivatwwr)
018100040119     C                   open      fivatwwr
018200040119     C                   endif
018300010330     C*
018400040119     C                   clear                   §CTROKVB          7 0
018500040119     C                   clear                   §CTROKVT          7 0
018600040119     C                   clear                   §CTRMO            7 0
018700040119     C                   clear                   §CTRNO            7 0
018800120410     C*
018900120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019000120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019100120410     C                   IF        Intestazioni = *off
019200120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019300120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019400120410     C                   exsr      inzvar
019500120410     C                   exsr      impvab
019600120410     C                   ENDIF
019700010330     C*
019800030822     C                   do        *HIVAL
019900010330     C*
020000030822     C                   read      tivin00r                               70
020100010618     C*
020200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020300010618     C                   if        vinflg = '1'
020400010618     C                   eval      flgOk = '1'
020500010618     C                   endif
020600040510     C*
020700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020800040510     C                   z-add     *zeros        wLenVINDTA        4 0
020900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021300040510     C                   z-add     1             wFlgVINDTA
021400040510     C                   leave
021500040510     C                   endif
021600040510     C                   enddo
021700010618     C*
021800040510     C                   if        vindta > *blanks AND
021900040510     C                             wFlgVINDTA = 1
022000010330     C                   add       1             rrnum
022100010330     C*
022200010601     C                   if        *in70 = *off and
022300010330     C                             (vinflg = *blanks
022400010330     C                              or vinflg = '0'
022500010330     C                              or vinflg = '2')
022600010330     C*
022700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022800010711     C                   if        vinflg = *blanks or vinflg = '0'
022900010711     C                   clear                   vinmsg
023000010711     C                   endif
023100130917     C*
023200130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023300130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023400130917     C* evitare che vengano elisi i blank
023500140220     C***                eval      vindta  = '*' + %trim(vindta)
023600130917     C*
023700080527     C                   exsr      inzvar
023800100318     C                   exsr      normCSV
023900010605     C                   exsr      impvab
024000010601     C*
024100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024200141014     C                   eval      depfil = VABLNP
024300141014     C                   exsr      repfil
024400141014     C                   if        depfil = invfil
024500150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
024600150317     C***                if        vlrpoi = 999
024700150317     C***                move(P)   invfil        VABFGS
024800150317     C***                else
024900150317     C***                move(P)   vlrpoi        VABFGS
025000150317     C***                endif
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
044700150316     C                   if        wVATNOT_E <> *blanks
044800060331     C                   eval      VATTRC = 'E'
044900140527     C*
045000150316     C* i "CHI SONO" sono in orizzontale e al massimo 20
045100140527     C*
045200150316     C                   FOR       wX = 1 to 20
045300150316     C                   IF        NOT_E(wX) <> *blank
045400150316     C                   eval      VATNOT = NOT_E(wX)
045500150114     C                   write     FIVAT000
045600150114     C                   add       1             §CTROKVT
045700120320     C                   endif
045800150316     C                   ENDFOR
045900110927     C*
046000150316     C                   endif
046100150316     C*
046200110927     C                   if        wVATNOT_I <> *blanks
046300110927     C                   eval      VATNOT = wVATNOT_I
046400140711     C                   eval      VATTRC = 'I'
046500110927     C                   write     FIVAT000
046600110927     C                   add       1             §CTROKVT
046700110927     C                   endif
046800140220     C*
046900140220     C                   if        wVATNOT_J <> *blanks
047000140220     C                   eval      VATNOT = wVATNOT_J
047100140220     C                   eval      VATTRC = 'J'
047200140220     C                   write     FIVAT000
047300140220     C                   add       1             §CTROKVT
047400140220     C                   endif
047500140319     C*
047600140319     C                   if        wVATNOT_S <> *blanks
047700140319     C                   eval      VATNOT = wVATNOT_S
047800140319     C* i 2 flag devono essere blank
047900140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
048000140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
048100140319     C                   eval      VATTRC = 'S'
048200140319     C                   write     FIVAT000
048300140319     C                   add       1             §CTROKVT
048400140319     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700040119
048800040119
048900040119
049000040119     C*----------------------------------------------------*
049100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049200040119     C*----------------------------------------------------*
049300040119     C     PREVAT        BEGSR
049400040119     C*
049500040119     C* Compongo il nome del membro da dare al FIVATWWR
049600040119     C                   eval      parmbr = vlrhdl
049700040119     C                   movel     'M'           parmbr
049800040510     C                   eval      parccm = vlrksc
049900040119     C                   eval      paropz = '1'
050000040119     C* Effettuo la chiamata al CLLE preposto
050100040119     C                   call(e)   'TITVVTC'
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
052400020204     C                   Z-ADD     *zeros        Num5_0            5 0
052500101018     C                   MOVEL     *blanks       wFlgCAS
052600060331     C                   MOVEL     *blanks       wVATNOT_A
052700060331     C                   MOVEL     *blanks       wVATNOT_B
052800060331     C                   MOVEL     *blanks       wVATNOT_E
052900110927     C                   MOVEL     *blanks       wVATNOT_I
053000140220     C                   MOVEL     *blanks       wVATNOT_J
053100140220     C                   MOVEL     *blanks       wVATNOT_IJ
053200140319     C                   MOVEL     *blanks       wVATNOT_S
053300120411     C                   MOVEL     *blanks       wNOT
053400120411     C                   MOVEL     *blanks       Data8A
053500030822     C*
053600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053700030822     C                   CLEAR                   SkSplitCSV
053800030822     C                   EVAL      i = 1
053900030822     C                   EVAL      posDa = *zeros
054000030822     C                   EVAL      posA  = *zeros
054100080527     C*
054200080527     C                   CLEAR                   FIVABDS
054300080527     C                   CLEAR                   FIVAT000
054400030822     C*
054500030822     C* Reimposto i valori di default
054600030822     C                   EXSR      DEFCAM
054700030822     C*
054800010330     C                   ENDSR
054900010330     C*----------------------------------------------------*
055000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055100010330     C*----------------------------------------------------*
055200010330     C     DEFCAM        BEGSR
055300030822     C*
055400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055500030715     C* e delimitatore testo.
055600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055900100318     C*
056000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056100100318     C                   EVAL      CharSOS = CharNUM
056200030822     C*
056300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056400100720     C                   SETOFF                                       505152
056500100720     C                   SETOFF                                       53
056600100413     C                   SELECT
056700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056800100720     C                   SETON                                        50
056900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057000100413     C                   SETON                                        51
057100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057200100720     C                   SETON                                        52
057300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057400100720     C                   SETON                                        53
057500100413     C                   ENDSL
057600030822     C*
057700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057800030822     C                   EVAL      posDaDft = 1
057900030822     C                   EVAL      posADft  = 0
058000030822     C                   EVAL      wGiroDft = 0
058100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058200030822     C                             posDaDft > 0
058300030822     C*
058400030822     C* Gestisco il 1° giro
058500030822     C                   IF        wGiroDft = 0
058600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058800030822     C* Incremento il contatore dei "giri"
058900030822     C                   EVAL      wGiroDft = 1
059000030822     C                   ELSE
059100030822     C                   EVAL      posDaDft = posADft
059200030822     C                   ENDIF
059300030822     C* Eseguo lo scan x trovare la fine del campo corrente
059400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059500030822     C*
059600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059800030822     C                   IF        posDaDft > 0 AND
059900030822     C                             posADft  > 0
060000030822     C* NCL
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'NCL'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABNCL
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* CCM
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'CCM'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABCCM
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* LNP
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'LNP'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABLNP
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* NRS
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'NRS'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABNRS
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* CTR
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'CTR'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiInt=*on
066200030822     C                   Z-ADD     PiVal         VABCTR
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* PKB
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'PKB'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500130610     C                   Z-ADD     PiVal         VABPKB
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* VLB
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VLB'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiNum=*on
068800130610     C                   Z-ADD     PiVal         VABVLB
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* QFT
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'QFT'
069600030822     C                   EVAL      PiStr=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   EXSR      CHKNUM
070000030822     C                   IF        PiNum=*on
070100030822     C                   Z-ADD     PiVal         VABQFT
070200030822     C                   ENDIF
070300030822     C                   ENDIF
070400030822     C* CBO
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'CBO'
070900030822     C                   EVAL      VABCBO=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* TSP
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'TSP'
071800030822     C                   EVAL      VABTSP=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VAS
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VAS'
072700030822     C                   EVAL      VABVAS=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* VCA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'VCA'
073600030822     C                   EVAL      VABVCA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000070925     C* TIC
074100070925     C                   IF        %subst(
074200070925     C                             %subst(vlrppt:posDaDft+1:
074300070925     C                             posADft-posDaDft-1):1:3)
074400070925     C                             = 'TIC'
074500070925     C                   EVAL      VABTIC=%trim(%subst(
074600070925     C                             %subst(vlrppt:posDaDft+1:
074700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800070925     C                   ENDIF
074900030822     C* GCA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'GCA'
075400030822     C                   EVAL      VABGCA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* CTM
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'CTM'
076300030822     C                   EVAL      VABCTM=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* FFD
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'FFD'
077200030822     C                   EVAL      VABFFD=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* VAD
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'VAD'
078100030822     C                   EVAL      VABVAD=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* GMA
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'GMA'
079000030822     C                   EVAL      VABGMA=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* GGA
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'GGA'
079900030822     C                   EVAL      VABGGA=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* GVA
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'GVA'
080800030822     C                   EVAL      VABGVA=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* TC1
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'TC1'
081700030822     C                   EVAL      VABTC1=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100030822     C* TC2
082200030822     C                   IF        %subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):1:3)
082500030822     C                             = 'TC2'
082600030822     C                   EVAL      VABTC2=%trim(%subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900030822     C                   ENDIF
083000040714     C* VATTRC
083100040714     C                   IF        %subst(
083200040714     C                             %subst(vlrppt:posDaDft+1:
083300040714     C                             posADft-posDaDft-1):1:3)
083400040714     C                             = 'TRC'
083500040714     C                   EVAL      VATTRC=%trim(%subst(
083600040714     C                             %subst(vlrppt:posDaDft+1:
083700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083800040714     C                   ENDIF
083900030822     C* ...
084000030822     C                   ENDIF
084100030822     C                   ENDDO
084200020204     C*
084300010330     C                   ENDSR
084400010607     C*----------------------------------------------------*
084500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084600010607     C*----------------------------------------------------*
084700010607     C     IMPVAB        BEGSR
084800010607     C*
084900010607     C                   SETOFF                                       3132
085000110315 xxx C***                SETON                                        35
085100030515     C*
085200010607     C                   MOVEL     datcor        VABAAS
085300010607     C                   MOVE      datcor        VABMGS
085400010607     C*
085500030715     C**********
085600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085700030715     C**********
085800030715     C                   IF        wGiro = *zeros
085900040113     C*
086000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086300080429     C                             %trim(intcol(5))<>*blanks
086400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086600080429     C                                      %trim(intcol(5))
086700070823     C     ';':CharCSV   XLATE     vindta        vindta
086800040510     C                   ENDIF
086900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087000031201     C     minu:maiu     XLATE     vindta        vindta
087100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087200030715     C                   DOW       posDa <= %len(%trim(vindta))
087300030715     C*
087400030715     C* Gestisco il 1° campo
087500030715     C                   IF        i = 1
087600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087700030715     C                   EVAL      posDa = 1
087800030715     C* Eseguo lo scan x trovare la fine del primo campo
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C* X i campi successivi al 1°
088400030715     C                   ELSE
088500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088600030715     C                   EVAL      posDa = posA + 1
088700030715     C* Eseguo lo scan x trovare la fine del campo corrente
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* Gestisco l'ultimo campo
089000030715     C                   IF        posA = *zeros
089100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C                   LEAVE
089600030715     C                   ELSE
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   ENDIF
090100030715     C                   ENDIF
090200030715     C* Incremento il contatore d campo
090300030715     C                   EVAL      i = i +1
090400030715     C                   ENDDO
090500030715     C*
090600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090700030715     C* delimitazione testo
090800030715     C                   EVAL      i = 1
090900030715     C                   DOW       i < %elem(SkSplitFLD)
091000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091200030715     C                   EVAL      i = i + 1
091300030715     C                   ENDDO
091400030715     C                   EVAL      wGiro = 1
091500030715     C                   ELSE
091600030715     C                   EVAL      wGiro = 2
091700031204     C**********
091800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091900031204     C* d separatore campo
092000031204     C**********
092100031204     C                   EVAL      vindta = %trim(vindta)
092200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092300031204     C                   EVAL      vindta = %subst(vindta:2)
092400031204     C                   ENDDO
092500031204     C*
092600031204     C                   Z-ADD     *zeros        lunghInput        4 0
092700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093100110912     C                   ENDDO
093200030715     C**********
093300030715     C* Effettuo lo split del campo dati d input
093400030715     C**********
093500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093600030715     C                   DOW       posDa <= %len(%trim(vindta))
093700030715     C*
093800030715     C* Gestisco il 1° campo
093900030715     C                   IF        i = 1
094000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094100030715     C                   EVAL      posDa = 1
094200030715     C* Eseguo lo scan x trovare la fine del primo campo
094300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094400030715     C* A questo punto "estraggo" il campo corrente
094500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094600030715     C                                                    (posA-posDa))
094700030715     C* X i campi successivi al 1°
094800030715     C                   ELSE
094900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095000030715     C                   EVAL      posDa = posA + 1
095100031204     C* Verifico che nn vi sia il campo nn valorizzato
095200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095300031204     C* Se campo nn valorizzato skippo al prossimo
095400031204     C                   EVAL      SkSplitCSV(i) = *blanks
095500031204     C                   EVAL      posA  = posA + 1
095600031204     C                   ELSE
095700030715     C* Eseguo lo scan x trovare la fine del campo corrente
095800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095900030715     C* Gestisco l'ultimo campo
096000030715     C                   IF        posA = *zeros
096100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096200030715     C* A questo punto "estraggo" il campo corrente
096300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096400030715     C                                                    (posA-posDa))
096500030715     C                   LEAVE
096600030715     C                   ELSE
096700030715     C* A questo punto "estraggo" il campo corrente
096800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096900030715     C                                                    (posA-posDa))
097000030715     C                   ENDIF
097100031204     C                   ENDIF
097200030715     C                   ENDIF
097300030715     C* Incremento il contatore d campo
097400030715     C                   EVAL      i = i +1
097500030715     C                   ENDDO
097600030715     C*
097700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097800030715     C* delimitazione testo
097900030715     C                   EVAL      i = 1
098000030715     C                   DOW       i < %elem(SkSplitCSV)
098100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098300030715     C                   EVAL      i = i + 1
098400030715     C                   ENDDO
098500100720     C*
098600100720     C* Stacco un progressivo x numero spedizione
098700150316 xxx C                   EXSR      repNSP
098800100326     C*
098900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099000030715     C                   EVAL      i = 1
099100030715     C                   DOW       i < %elem(SkSplitFLD)
099200030715     C*
099300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099500030715     C                             SkSplitCSV(i) <> *blanks
099600030721     C*** ATB
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099800030721     C                   EVAL      VABATB = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030715     C*** RSD
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100500030715     C* ==
100600130528     C                   ENDIF
100700030721     C*** RD2
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030715     C*** IND
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101300030715     C                   EVAL      VABIND = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** LOD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** PRD
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030721     C*** NZD
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030715     C*** RMA
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NOT
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** NT2
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VCA
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** VAS
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030715     C*** FFD
104800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105000030715     C                   ENDIF
105100030721     C*** GC1
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GC2
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TSP
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106200030721     C                   ENDIF
106300040510     C*** CBO
106400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106600040510     C                   ENDIF
106700030721     C*** NAS
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TIC
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** GCA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** XCO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** CTM
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TCR
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** CTS
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** FTM
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** VAD
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GMA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GGA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** GVA
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC1
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** TC2
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** SCL
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** RMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*** NMO
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113400030721     C                   ENDIF
113500030721     C*
113600030721     C* Reperisco quindi i campi numerici...
113700040510     C*** NSP
113800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113900040510     C                   EVAL      PiStr=SkSplitCSV(i)
114000040510     C                   EXSR      CHKNUM
114100040510     C                   IF        PiInt=*on
114200040510     C                   Z-ADD     PiVal         VABNSP
114300040510     C                   ELSE
114400040510     C                   SETON                                        31
114500040510     C                   EVAL      VABNSP = *zeros
114600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114700040510     C                             + ' ' + 'VABNSP'
114800040510     C                   ENDIF
114900040510     C                   ENDIF
115000071106     C*** RMN
115100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115200071106     C                   EVAL      PiStr=SkSplitCSV(i)
115300071106     C                   EXSR      CHKNUM
115400071106     C                   IF        PiInt=*on
115500071106     C                   Z-ADD     PiVal         VABRMN
115600071106     C                   ELSE
115700071106     C                   SETON                                        32
115800071106     C                   EVAL      VABRMN = *zeros
115900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116000071106     C                             + ' ' + 'VABRMN'
116100071106     C                   ENDIF
116200071106     C                   ENDIF
116300030721     C*** CAD
116400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116500130610     C                   EVAL      PiStr=SkSplitCSV(i)
116600130610     C                   EXSR      CHKNUM
116700130610     C                   IF        PiInt=*on
116800130610     C                   Z-ADD     PiVal         Num5_0
116900130610     C                   MOVEL(p)  Num5_0        VABCAD
117000130610     C                   ELSE
117100130610     C                   SETON                                        32
117200130610     C                   EVAL      VABCAD = *zeros
117300130610     C                   EVAL      vinmsg = %trimr(vinmsg)
117400130610     C                             + ' ' + 'VABCAD'
117500130610     C                   ENDIF
117600130605     C                   ENDIF
117700030721     C*** NCL
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on
118200030721     C                   Z-ADD     PiVal         VABNCL
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABNCL = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700040122     C                             + ' ' + 'VABNCL'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** PKB
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiNum=*on
119500130610     C                   Z-ADD(H)  PiVal         VABPKB
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      VABPKB = *zeros
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABPKB'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** CAS
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120600111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120700140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
120800140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
120900140711     C                             %trim(SkSplitCSV(i)) <> ',00'
121000130703     C                   EVAL      VABVCA = 'EUR'
121100140513     C                   EVAL      wFlgCAS = '1'
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
122200140513     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** CCM
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on      AND
122900030721     C                             PiVal<=9999999 AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABCCM
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      VABCCM = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040510     C                             + ' ' + 'VABCCM'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** LNP
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on    AND
124400030721     C                             PiVal<=999   AND
124500030721     C                             PiVal>*zeros
124600030721     C                   Z-ADD     PiVal         VABLNP
124700030721     C                   ELSE
124800040715     C                   SETON                                        32
124900030721     C                   EVAL      VABLNP = *zeros
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100040510     C                             + ' ' + 'VABLNP'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** NRS
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on AND
125900030721     C                             PiVal<=99
126000030721     C                   Z-ADD     PiVal         VABNRS
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABNRS'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** LNA
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on    AND
127200030721     C                             PiVal<=999   AND
127300030721     C                             PiVal>*zeros
127400030721     C                   Z-ADD     PiVal         VABLNA
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      VABLNA = *zeros
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABLNA'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** CTR
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on  AND
128700030721     C                             PiVal<=999
128800030721     C                   Z-ADD     PiVal         VABCTR
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABCTR'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** IAS
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129700141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
129800141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
129900141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
130000141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
130100141003     C                             %trim(SkSplitCSV(i)) <> ',00'
130200141006     C                   EVAL      VABVAS = 'EUR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiNum=*on
130600030721     C                   Z-ADD     PiVal         VABIAS
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABIAS'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300141003     C                   ENDIF
131400030721     C*** VLB
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiNum=*on
131900130610     C                   Z-ADD(H)  PiVal         VABVLB
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABVLB'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600030721     C*** QFT
132700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiNum=*on
133100030721     C                   Z-ADD     PiVal         VABQFT
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABQFT'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** NCD
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on      AND
134300030721     C                             PiVal<=9999999
134400030721     C                   Z-ADD     PiVal         VABNCD
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABNCD'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** NCA
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on      AND
135600030721     C                             PiVal<=9999999
135700030721     C                   Z-ADD     PiVal         VABNCA
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABNCA'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** ZNC
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on AND
136900030721     C                             PiVal<=99
137000030721     C                   Z-ADD     PiVal         VABZNC
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABZNC'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** DCR
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on       AND
138200030721     C                             PiVal<=99999999
138300030721     C                   Z-ADD     PiVal         VABDCR
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABDCR'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** HCR
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiInt=*on   AND
139500030721     C                             PiVal<=9999
139600030721     C                   Z-ADD     PiVal         VABHCR
139700030721     C                   ELSE
139800030721     C                   SETON                                        32
139900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140000030721     C                             + ' ' + 'VABHCR'
140100030721     C                   ENDIF
140200030721     C                   ENDIF
140300030721     C*** VMD
140400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140500030721     C                   EVAL      PiStr=SkSplitCSV(i)
140600030721     C                   EXSR      CHKNUM
140700030721     C                   IF        PiNum=*on
140800030721     C                   Z-ADD     PiVal         VABVMD
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABVMD'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500030721     C*** ANT
141600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141700030721     C                   EVAL      PiStr=SkSplitCSV(i)
141800030721     C                   EXSR      CHKNUM
141900030721     C                   IF        PiInt=*on        AND
142000030721     C                             PiVal<=999999999
142100030721     C                   Z-ADD     PiVal         VABANT
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142500030721     C                             + ' ' + 'VABANT'
142600030721     C                   ENDIF
142700030721     C                   ENDIF
142800030721     C*** CMO
142900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143000030721     C                   EVAL      PiStr=SkSplitCSV(i)
143100030721     C                   EXSR      CHKNUM
143200030721     C                   IF        PiInt=*on
143300030721     C                   Z-ADD     PiVal         Num5_0
143400030721     C                   MOVEL(p)  Num5_0        VABCMO
143500030721     C                   ELSE
143600030721     C                   SETON                                        32
143700030721     C                   EVAL      VABCMO = *zeros
143800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143900030721     C                             + ' ' + 'VABCMO'
144000030721     C                   ENDIF
144100030721     C                   ENDIF
144200040510     C*
144300040510     C* Reperisco i campi del FIVAT
144400040510     C*** VATNOT
144500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144700040510     C                   ENDIF
144800040510     C*** VATTRC
144900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145100040510     C                   ENDIF
145200060331     C*** VATNOT_A
145300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145500060331     C                   ENDIF
145600060331     C*** VATNOT_B
145700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145900060331     C                   ENDIF
146000060331     C*** VATNOT_E
146100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146300060331     C                   ENDIF
146400110927     C*** VATNOT_I
146500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146700110927     C                   ENDIF
146800141003     C*** VATNOT_IJ
146900141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
147000141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147100141003     C                   ENDIF
147200140319     C*** VATNOT_S
147300140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147400140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147500140319     C                   ENDIF
147600070823     C*
147700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147800150309     C*
147900150316     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
148000150316     C* VABLNP/VATLNP/VABFGS/VATFGS dipendono dal cod.cliente
148100150316     C                   SELECT
148200150316     C                   WHEN      VABCCM = 0433721
148300150316     C                   eval      VABLNP = 043
148400150316     C                   eval      VABFGS = VABLNP
148500150316     C                   eval      depfil = VABLNP
148600150316     C                   eval      invfil = VABLNP
148700150316     C                   WHEN      VABCCM = 0436669
148800150316     C                   eval      VABLNP = 002
148900150316     C                   eval      VABFGS = VABLNP
149000150316     C                   eval      depfil = VABLNP
149100150316     C                   eval      invfil = VABLNP
149200150316     C                   WHEN      VABCCM = 0436670
149300150316     C                   eval      VABLNP = 116
149400150316     C                   eval      VABFGS = VABLNP
149500150316     C                   eval      depfil = VABLNP
149600150316     C                   eval      invfil = VABLNP
149700160623     C                   WHEN      VABCCM = 0024685
149800160623     C                   eval      VABLNP = 002
149900160623     C                   eval      VABFGS = VABLNP
150000160623     C                   eval      depfil = VABLNP
150100160623     C                   eval      invfil = VABLNP
150200160623     C                   WHEN      VABCCM = 0436838
150300160623     C                   eval      VABLNP = 002
150400160623     C                   eval      VABFGS = VABLNP
150500160623     C                   eval      depfil = VABLNP
150600160623     C                   eval      invfil = VABLNP
150700160623     C                   WHEN      VABCCM = 0436839
150800160623     C                   eval      VABLNP = 116
150900160623     C                   eval      VABFGS = VABLNP
151000160623     C                   eval      depfil = VABLNP
151100160623     C                   eval      invfil = VABLNP
151200150316     C                   ENDSL
151300150316     C                   ENDIF
151400150316
151500150316     C*** VABNOT
151600150316     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
151700150316     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
151800150316     C                   EVAL      VABNOT = %subst(wNOT:1:35)
151900150316     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
152000150309     C                   ENDIF
152100150316
152200150316     C*** VATNOT_E
152300150316     C                   SELECT
152400150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E01' and
152500150316     C                             %trim(SkSplitCSV(i)) <> *blank
152600150316     C                   EVAL      NOT_E(01) = %trim(SkSplitCSV(i))
152700150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E02' and
152800150316     C                             %trim(SkSplitCSV(i)) <> *blank
152900150316     C                   EVAL      NOT_E(02) = %trim(SkSplitCSV(i))
153000150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E03' and
153100150316     C                             %trim(SkSplitCSV(i)) <> *blank
153200150316     C                   EVAL      NOT_E(03) = %trim(SkSplitCSV(i))
153300150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E04' and
153400150316     C                             %trim(SkSplitCSV(i)) <> *blank
153500150316     C                   EVAL      NOT_E(04) = %trim(SkSplitCSV(i))
153600150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E05' and
153700150316     C                             %trim(SkSplitCSV(i)) <> *blank
153800150316     C                   EVAL      NOT_E(05) = %trim(SkSplitCSV(i))
153900150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E06' and
154000150316     C                             %trim(SkSplitCSV(i)) <> *blank
154100150316     C                   EVAL      NOT_E(06) = %trim(SkSplitCSV(i))
154200150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E07' and
154300150316     C                             %trim(SkSplitCSV(i)) <> *blank
154400150316     C                   EVAL      NOT_E(07) = %trim(SkSplitCSV(i))
154500150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E08' and
154600150316     C                             %trim(SkSplitCSV(i)) <> *blank
154700150316     C                   EVAL      NOT_E(08) = %trim(SkSplitCSV(i))
154800150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E09' and
154900150316     C                             %trim(SkSplitCSV(i)) <> *blank
155000150316     C                   EVAL      NOT_E(09) = %trim(SkSplitCSV(i))
155100150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E10' and
155200150316     C                             %trim(SkSplitCSV(i)) <> *blank
155300150316     C                   EVAL      NOT_E(10) = %trim(SkSplitCSV(i))
155400150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E11' and
155500150316     C                             %trim(SkSplitCSV(i)) <> *blank
155600150316     C                   EVAL      NOT_E(11) = %trim(SkSplitCSV(i))
155700150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E12' and
155800150316     C                             %trim(SkSplitCSV(i)) <> *blank
155900150316     C                   EVAL      NOT_E(12) = %trim(SkSplitCSV(i))
156000150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E13' and
156100150316     C                             %trim(SkSplitCSV(i)) <> *blank
156200150316     C                   EVAL      NOT_E(13) = %trim(SkSplitCSV(i))
156300150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E14' and
156400150316     C                             %trim(SkSplitCSV(i)) <> *blank
156500150316     C                   EVAL      NOT_E(14) = %trim(SkSplitCSV(i))
156600150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E15' and
156700150316     C                             %trim(SkSplitCSV(i)) <> *blank
156800150316     C                   EVAL      NOT_E(15) = %trim(SkSplitCSV(i))
156900150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E16' and
157000150316     C                             %trim(SkSplitCSV(i)) <> *blank
157100150316     C                   EVAL      NOT_E(16) = %trim(SkSplitCSV(i))
157200150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E17' and
157300150316     C                             %trim(SkSplitCSV(i)) <> *blank
157400150316     C                   EVAL      NOT_E(17) = %trim(SkSplitCSV(i))
157500150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E18' and
157600150316     C                             %trim(SkSplitCSV(i)) <> *blank
157700150316     C                   EVAL      NOT_E(18) = %trim(SkSplitCSV(i))
157800150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E19' and
157900150316     C                             %trim(SkSplitCSV(i)) <> *blank
158000150316     C                   EVAL      NOT_E(19) = %trim(SkSplitCSV(i))
158100150316     C                   WHEN      %trim(SkSplitFLD(i)) = 'W_NOT_E20' and
158200150316     C                             %trim(SkSplitCSV(i)) <> *blank
158300150316     C                   EVAL      NOT_E(20) = %trim(SkSplitCSV(i))
158400150316     C                   ENDSL
158500141003     C*
158600071029     C***  <===  -----------------------------------------
158700040510     C*
158800030715     C                   ENDIF
158900030715     C                   EVAL      i = i + 1
159000030715     C                   ENDDO
159100070925     C*
159200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
159300140319     C*
159400141014     C* se la nazione è IT la sblanco
159500141112     C                   IF        VABNZD = 'IT' or
159600141112     C                             VABNZD = 'ITA'
159700141014     C                   EVAL      VABNZD = *blank
159800141014     C                   ENDIF
159900070925     C*
160000070925     C* Considerazioni finali su CBO/CAS
160100140220     C                   IF        wFlgCAS = '1'    OR
160200140220     C                             VABCAS  > *zeros
160300140220     C                   IF        VABCBO = '1'
160400140220     C                   EVAL      VABCBO = '4'
160500140220     C                   ENDIF
160600140220     C                   IF        VABCBO = '2'
160700140220     C                   EVAL      VABCBO = '6'
160800140220     C                   ENDIF
160900140220     C                   ENDIF
161000070925     C***  <===  ----------------------------
161100040510     C*
161200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
161300040510     C                   IF        VABNSP = *zeros
161400040510     C                   SETON                                        31
161500040510     C                   ENDIF
161600040715     C*
161700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
161800040715     C                   IF        VABLNP = *zeros
161900040715     C                   SETON                                        31
162000040715     C                   ENDIF
162100040510     C*
162200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
162300120411     C                   IF        VABRMN = *zeros
162400120411     C                   Z-ADD     VABNSP        VABRMN
162500120411     C                   ENDIF
162600120328     C* IN QUESTO CASO LO FORZO = 1
162700120411     C***                IF        VABRMN = *zeros
162800120411     C***                Z-ADD     1             VABRMN
162900120411     C***                ENDIF
163000040510     C*
163100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
163200040510     C                   IF        VABRMA = *blanks
163300040714     C                   MOVEL     VABRMN        VABRMA
163400040510     C                   ENDIF
163500040128     C*
163600040510     C* Se provincia nn valorizzata la reperisco
163700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
163800110912     C                   IF        VABPRD =  *blanks AND
163900110912     C                             VABNZD =  *blanks AND
164000110912     C                             VABCAD <> *blanks AND
164100110912     C                             VABLOD <> *blanks
164200040128     C                   CLEAR                   TISI95DS
164300040128     C                   EVAL      I95TCN = '3'
164400040128     C                   Z-ADD     datcor        I95DAT
164500040128     C                   EVAL      I95NAR = VABNZD
164600040128     C                   EVAL      I95CAP = VABCAD
164700040128     C                   EVAL      I95LOC = VABLOD
164800040128     C                   CALL      'TISI95R'
164900040128     C                   PARM                    TISI95DS
165000040128     C                   EVAL      VABPRD = O95PRV
165100040510     C                   ENDIF
165200020204     C*
165300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
165400020204     C                   EXSR      CHKIMPDIV
165500030715     C                   ENDIF
165600020204     C*
165700010607     C                   ENDSR
165800010607     C*----------------------------------------------------*
165900010601
166000020204
166100020204     C*----------------------------------------------------*
166200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
166300020204     C*----------------------------------------------------*
166400020204     C     CHKIMPDIV     BEGSR
166500020204     C*
166600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
166700020204     C                   Z-ADD     *zeros        wrkDec            9 9
166800020204     C*
166900020204     C* Come prima cosa effettuo considerazioni sulla divisa
167000020204     C                   IF        vabIAS > *zeros
167100020204     C                   IF        vabVAS <> 'EUR'
167200020204     C                   EVAL      vabVAS =  'ITL'
167300020204     C                   ENDIF
167400020204     C                   ENDIF
167500020204     C*
167600020204     C                   IF        vabCAS > *zeros
167700020204     C                   IF        vabVCA <> 'EUR'
167800020204     C                   EVAL      vabVCA =  'ITL'
167900020204     C                   ENDIF
168000020204     C                   ENDIF
168100020204     C*
168200020204     C                   IF        vabVMD > *zeros
168300020204     C                   IF        vabVAD <> 'EUR'
168400020204     C                   EVAL      vabVAD =  'ITL'
168500020204     C                   ENDIF
168600020204     C                   ENDIF
168700020204     C*
168800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
168900020204     C                   Z-ADD     vabIAS        wrkDec
169000020204     C                   IF        wrkDec > *zeros
169100020204     C                   IF        vabVAS = 'ITL'
169200020204     C                   EVAL      vabIAS = *zeros
169300020204     C                   ENDIF
169400020204     C                   ENDIF
169500020204     C*
169600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
169700020204     C                   Z-ADD     vabCAS        wrkDec
169800020204     C                   IF        wrkDec > *zeros
169900020204     C                   IF        vabVCA = 'ITL'
170000020204     C                   EVAL      vabCAS = *zeros
170100020204     C                   ENDIF
170200020204     C                   ENDIF
170300020204     C*
170400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
170500020204     C                   Z-ADD     vabVMD        wrkDec
170600020204     C                   IF        wrkDec > *zeros
170700020204     C                   IF        vabVAD = 'ITL'
170800020204     C                   EVAL      vabVMD = *zeros
170900020204     C                   ENDIF
171000020204     C                   ENDIF
171100020204     C*
171200020204     C                   ENDSR
171300020204     C***
171400010330
171500100318
171600100318
171700100318     C*----------------------------------------------------*
171800100318     C*  CONTROLLO NUMERICITA' CAMPI
171900100318     C*----------------------------------------------------*
172000100318     C     CHKNUM        BEGSR
172100100318     C*
172200100318     C                   IF        PiDecChr = *blanks
172300100318     C                   EVAL      PiDecChr = CharNUM
172400100318     C                   ENDIF
172500100318     C*
172600100318     C                   callp     UBISNUM_Check(PiStr
172700100318     C                                          :PiDecChr
172800100318     C                                          :PiVal
172900100318     C                                          :PiNum
173000100318     C                                          :PiInt)
173100100318     C*
173200100318     C                   ENDSR
173300100318     C***
173400010601
173500010601
173600010601
173700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
173800141006      ***
173900141014     C     repfil        BEGSR
174000141014     C*
174100141014     C                   if        invfil = *zeros and
174200141014     C                             depfil > *zeros and
174300141014     C                             (vinflg = *blanks or
174400141014     C                              vinflg = *zeros)
174500141014     C*
174600141014     C                   eval      invfil = depfil
174700141014     C                   endif
174800141014     C*
174900141014     C                   if        depfil <> invfil and
175000141014     C                             invfil > *zeros
175100141014     C                   eval      flgMulti = '1'
175200141014     C                   if        vinflg = *blanks
175300141014     C                   add       1             cntNonEl
175400141014     C                   endif
175500141014     C                   endif
175600141014     C*
175700141014     C                   if        vinflg = '2'
175800141014     C                   eval      flgStato = '2'
175900141014     C                   endif
176000141014     C*
176100141014     C                   ENDSR
176200141014     C*
176300010601
176400010601
176500010601
176600010330
176700040119      /TITLE Invio dei dati al punto operativo.
176800040119     C     invio         BEGSR
176900040119     C*
177000040119     C* 1° invio FIVAT
177100040119     C                   reset                   dscmz
177200040119     C                   move      vlrpoi        cmzdst
177300040119     C                   eval      cmzfld = 'FIVATWWR'
177400040119     C                   eval      cmzmbd = vlrhdl
177500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
177600040119     C***                if        prmfir = *blanks
177700040119     C                   eval      cmzfla = 'FIVAT00F'
177800040119     C                   eval      cmzmba = 'FIVAT00F'
177900040119     C***                else
178000040119     C***                eval      cmzfla = prmfir
178100040119     C***                eval      cmzmba = prmfir
178200040119     C***                endif
178300040119     C                   eval      cmznrr = *zeros
178400040119     C                   move      §ctrokvt      cmznrr
178500040119     C                   eval      cmzlba = vlrfl1
178600040119     C                   call(e)   'TIS711C'
178700040119     C                   parm                    dscmz
178800040119     C                   parm      *blanks       esito
178900040119     C                   if        %error
179000040119     C                             or cmzerr = '1'
179100040119     C                             or esito  = '1'
179200040119     C                   eval      wrkesito = '3'
179300040119     C                   else
179400040119     C*
179500040119     C* 2° invio FIVAB
179600040119     C                   reset                   dscmz
179700040119     C                   move      vlrpoi        cmzdst
179800040119     C                   eval      cmzfld = vlrfou
179900040119     C                   eval      cmzmbd = vlrhdl
180000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
180100040119     C***                if        prmfir = *blanks
180200040119     C                   eval      cmzfla = 'FIVAB00F'
180300040119     C                   eval      cmzmba = 'FIVAB00F'
180400040119     C***                else
180500040119     C***                eval      cmzfla = prmfir
180600040119     C***                eval      cmzmba = prmfir
180700040119     C***                endif
180800040119     C                   eval      cmznrr = *zeros
180900040119     C                   move      §ctrokvb      cmznrr
181000040119     C                   eval      cmzlba = vlrfl1
181100040119     C                   call(e)   'TIS711C'
181200040119     C                   parm                    dscmz
181300040119     C                   parm      *blanks       esito
181400040119     C                   if        %error
181500040119     C                             or cmzerr = '1'
181600040119     C                             or esito  = '1'
181700040119     C                   eval      wrkesito = '3'
181800040119     C                   endif
181900040119     C                   endif
182000040119     C*
182100040119     C                   ENDSR
182200040119     C***
182300010601
182400010601
182500010601
182600010601
182700010601      /TITLE Invio dei dati al punto operativo.
182800010601     C     opeini        BEGSR
182900010601     C*
183000010601     C* Inizializzo flag e contatori operativi
183100010601     C                   movel     '0'           flgGiro           1
183200010601     C                   movel     '0'           flgMulti          1
183300010601     C                   movel     '1'           flgStato          1
183400010615     C                   movel     '0'           flgOk             1
183500010601     C                   z-add     *zeros        cntNonEl         10 0
183600010601     C                   z-add     *zeros        depfil            3 0
183700010601     C                   z-add     *zeros        invfil            3 0
183800010601     C*
183900010601     C                   ENDSR
184000010601     C***
184100070326
184200070326
184300070326
184400070326
184500070326     C     *pssr         BEGSR
184600070329     C*
184700070329     C                   if        %open(tivin00r)
184800070329     C                   close     tivin00r
184900070329     C                   endif
185000070329     C                   if        %open(fivabwwr)
185100070329     C                   close     fivabwwr
185200070329     C                   endif
185300070329     C                   if        %open(fivatwwr)
185400070329     C                   close     fivatwwr
185500070329     C                   endif
185600070326     C*
185700070326     C* Effettuo la chiamata al CLLE preposto
185800070326     C                   call(e)   'TITVVTC'
185900070326     C                   parm                    parccm
186000070326     C                   parm                    parmbr
186100070326     C                   parm      '2'           paropz
186200070326     C*
186300070326     C                   eval      wrkesito = '2'
186400070404     C*
186500070404     C                   seton                                        LR
186600070326     C*
186700070326     C                   ENDSR     '*CANCL'
186800070326     C***
186900100318
187000100318
187100100318
187200100318     C     normCSV       BEGSR
187300100318     C*
187400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
187500100318     C* separatori campi
187600100318     C                   if        CharTXT <> *blanks
187700100318     C                   z-add     *zeros        wLenVINDTA        4 0
187800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
187900100318     C                   setoff                                       80
188000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
188100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
188200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
188300100318     C                   if        not *in80
188400100318     C                   seton                                        80
188500100318     C                   else
188600100318     C                   setoff                                       80
188700100318     C                   endif
188800100318     C                   endif
188900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
189000100318     C                             and *in80 = *on
189100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
189200100318     C                   endif
189300100318     C                   enddo
189400100318     C                   endif
189500100318     C*
189600100318     C                   ENDSR
189700100318     C***
189800070326
189900100326
190000100326
190100100326     C*----------------------------------------------------*
190200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
190300100326     C*----------------------------------------------------*
190400100326     C     REPNSP        BEGSR
190500100326     C*
190600100326     C* NSP => Stacco un numeratore da AZNUM
190700100326     C                   clear                   TRUL33DS
190800100326     C                   eval      I33OPE = *zeros
190900100326     C                   eval      I33CNU = 302
191000100326     C                   eval      I33NUM = 1
191100100326     C                   movel     TRUL33DS      KPJBU
191200100326     C                   call      'TRUL33R'
191300100326     C                   parm                    KPJBA
191400100326     C                   movel     KPJBU         TRUL33DS
191500100326     C                   if        O33ERR = *zeros
191600100326     C                   z-add     O33NRF        VABNSP
191700100326     C                   z-add     O33NRF        VATNSP
191800100326     C                   else
191900100326     C                   SETON                                        31
192000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
192100140711     C                             + ' ' + 'VABNSP VATNSP'
192200100326     C                   endif
192300100326     C*
192400100326     C                   ENDSR
192500010330
192600100326
192700010330
192800000613     C     *inzsr        BEGSR
192900990910     C*
193000990910     C     *entry        plist
193100990920     C                   parm                    tivlrds
193200990921     C                   parm      wrkesito      esito
193300000724     C                   parm                    prmlit
193400000710     C                   parm                    prmfir
193500010330     C*
193600010330     C* CALCOLA LA DATA CORRENTE
193700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
193800100318     C                   eval      datcor = %dec(%date() : *ISO)
193900000613     C*
194000000613     C                   ENDSR
194100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
194200150408VABCCM;VABRMN;X;X;X;VABRSD;X;X;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;W_NOT_E01;
194300150408W_NOT_E02;
194400150316W_NOT_E03;W_NOT_E04;W_NOT_E05;W_NOT_E06;W_NOT_E07;W_NOT_E08;W_NOT_E09;W_NOT_E10;W_NOT_E11;
194500150316W_NOT_E12;W_NOT_E13;W_NOT_E14;W_NOT_E15;W_NOT_E16;W_NOT_E17;W_NOT_E18;W_NOT_E19;W_NOT_E20;
194600160610VATNOT_IJ;X;VABTIC;VABCAS;W_VABNOT;VABCTR;
