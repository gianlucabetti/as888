000100160121      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400160404      * disk A
000500150114      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700130917     H decedit('0,')
000800120209     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001100040119     FFIVATwwr  O    E             DISK    usropn
001200000313     D*
001300040113     D*------------
001400040113     D* SCHIEDE A PROGRAMMA
001500040113     D*------------
001600080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001700141003     D*------------
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500040128     D tisi95ds      e ds
002600100413     D fivabds       e ds                  extname(fivab00f)
002700100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002800100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400040510     D parccm          s              8    INZ(*blanks)
003500040119     D parmbr          s             10    INZ(*blanks)
003600040119     D paropz          s              1    INZ(*blanks)
003700040119     D chkcall         s              1    INZ(*blanks)
003800080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900080418     D wFlgCAS         s              1    INZ(*blanks)
004000120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004100120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004200150114     D Intestazioni    s               n   inz(*on)
004300100326     D*------------------
004400100326     D* DS REPERIMENTO NUMERATORE
004500100326     D*------------------
004600100326     D trul33ds      e ds                  inz
004700100326     D*------------------
004800100326     D* DS ARCHITETTURA
004900100326     D*------------------
005000100326     D kpjba         e ds                  inz
005100130611     D*------------------
005200130611     D TRUL28DSX     e ds                  inz
005300100326
005400030715     D*------------------
005500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005600030715     D*------------------
005700170413     D SkSplitFLD      S             15    DIM(100)
005800030715     D SkSplitCSV      S            256    DIM(100)
005900030715     D CharCSV         S              1
006000030715     D CharTXT         S              1
006100030715     D CharNUM         S              1
006200100318     D CharSOS         S              1
006300130626     D posDa           S              4  0 INZ(*zeros)
006400130626     D posA            S              4  0 INZ(*zeros)
006500030715     D i               s              3  0 INZ(1)
006600030715     D wGiro           s              1  0 INZ(*zeros)
006700070823     D wVATNOT_A       s                   LIKE(VATNOT)
006800060331     D wVATNOT_B       s                   LIKE(VATNOT)
006900160520     D wVATNOT_E       s                   LIKE(VATNOT)
007000110927     D wVATNOT_I       s                   LIKE(VATNOT)
007100140220     D wVATNOT_J       s                   LIKE(VATNOT)
007200140220     D wVATNOT_IJ      s             70
007300140319     D wVATNOT_S       s                   LIKE(VATNOT)
007400160520     D w70             s             70
007500120411     D Data8A          s              8
007600140527     D wBarcodeN       s             11s 0
007700140527     D wX              s              3s 0
007800140711     D TxtInOut        S           2048
007900140711     D ElencoChar      S            256
008000140711     D TipoElenco      S              1
008100140711     D CharSost        S              1
008200140711     D UpperCase       S              1
008300140711     D ChkNull         S              1
008400140711     D CharNull        S              1
008500140711     D EsitoBon        S              1
008600030822     D*------------------
008700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008800030822     D*------------------
008900030822     D posDaDft        S              3  0 INZ(*zeros)
009000030822     D posADft         S              3  0 INZ(*zeros)
009100030822     D j               s              3  0 INZ(1)
009200030822     D wGiroDft        s              1  0 INZ(*zeros)
009300080418     D*------------------
009400080418     D* Costanti
009500080418     D*------------------
009600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009800100318
009900100318     D*------------------
010000100318     D* LINKING A DEFINIZIONI ESTERNE
010100100318     D*------------------
010200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010400100318
010500010330
010600010330
010700010330
010800000913     C                   reset                   rrnum
010900990921     C                   reset                   esito
011000990921     C                   reset                   wrkesito
011100010601     C*
011200010601     C                   exsr      opeini
011300010605     C                   exsr      rwvab
011400010601     C*
011500010601     C                   seton                                        lr
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012100010601     C*--------------------------------------------------------
012200010601     C     PREELA        BEGSR
012300010601     C*
012400010601     C* SE OCCORRE SPEDIRE IN FILIALE
012500010601     C                   if        invfil <> *zeros and
012600010601     C                             flgGiro = '0'
012700010601     C*
012800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012900010601     C                   eval      flgGiro = '1'
013000010601     C*
013100010601     C                   endif
013200010601     C*
013300010601     C                   ENDSR
013400010601     C***
013500010601
013600010601
013700010601
013800010601
013900010601     C*--------------------------------------------------------
014000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014100010601     C*--------------------------------------------------------
014200010601     C     ENDELA        BEGSR
014300040119     C*
014400040119     C* Effettuo la chiamata al CLLE preposto
014500040119     C                   call(e)   'TITVVTC'
014600040510     C                   parm                    parccm
014700040119     C                   parm                    parmbr
014800040119     C                   parm      '2'           paropz
014900050201     C*
015000050201     C* Effettuo lancio TISI95 solo x chiusura
015100050201     C                   CLEAR                   TISI95DS
015200050201     C                   EVAL      I95TLA = 'C'
015300050201     C                   CALL      'TISI95R'
015400050201     C                   PARM                    TISI95DS
015500000616     C*
015600010601     C                   ENDSR
015700010601
015800010601
015900010330     C*--------------------------------------------------------
016000021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016100010330     C*--------------------------------------------------------
016200010605     C     RWVAB         BEGSR
016300010330     C*
016400010330     C                   if        not %open(tivin00r)
016500010330     C                   open      tivin00r
016600010330     C                   endif
016700021025     C                   if        not %open(fivabwwr)
016800021025     C                   open      fivabwwr
016900010330     C                   endif
017000040119     C*
017100040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017200040119     C                   exsr      prevat
017300040119     C*
017400040119     C                   if        chkcall = '0'
017500040119     C*
017600040119     C                   if        not %open(fivatwwr)
017700040119     C                   open      fivatwwr
017800040119     C                   endif
017900010330     C*
018000040119     C                   clear                   §CTROKVB          7 0
018100040119     C                   clear                   §CTROKVT          7 0
018200040119     C                   clear                   §CTRMO            7 0
018300040119     C                   clear                   §CTRNO            7 0
018400120410     C*
018500120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018600120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018700120410     C                   IF        Intestazioni = *off
018800120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018900120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019000120410     C                   exsr      inzvar
019100120410     C                   exsr      impvab
019200120410     C                   ENDIF
019300010330     C*
019400030822     C                   do        *HIVAL
019500010330     C*
019600030822     C                   read      tivin00r                               70
019700010618     C*
019800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019900010618     C                   if        vinflg = '1'
020000010618     C                   eval      flgOk = '1'
020100010618     C                   endif
020200040510     C*
020300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020400040510     C                   z-add     *zeros        wLenVINDTA        4 0
020500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020900040510     C                   z-add     1             wFlgVINDTA
021000040510     C                   leave
021100040510     C                   endif
021200040510     C                   enddo
021300010618     C*
021400040510     C                   if        vindta > *blanks AND
021500040510     C                             wFlgVINDTA = 1
021600010330     C                   add       1             rrnum
021700010330     C*
021800010601     C                   if        *in70 = *off and
021900010330     C                             (vinflg = *blanks
022000010330     C                              or vinflg = '0'
022100010330     C                              or vinflg = '2')
022200010330     C*
022300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022400010711     C                   if        vinflg = *blanks or vinflg = '0'
022500010711     C                   clear                   vinmsg
022600010711     C                   endif
022700130917     C*
022800130917     C* Aggiungo un byte a blanks all'inizio d ogni record
022900130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023000130917     C* evitare che vengano elisi i blank
023100140220     C***                eval      vindta  = '*' + %trim(vindta)
023200130917     C*
023300080527     C                   exsr      inzvar
023400100318     C                   exsr      normCSV
023500010605     C                   exsr      impvab
023600010601     C*
023700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023800141014     C                   eval      depfil = VABLNP
023900141014     C                   exsr      repfil
024000141014     C                   if        depfil = invfil
024100150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
024200150430     C                   if        vlrpoi = 999
024300150430     C                   move(P)   invfil        VABFGS
024400150430     C                   else
024500150430     C                   move(P)   vlrpoi        VABFGS
024600150430     C                   endif
024700010601     C*
024800010601     C                   exsr      PREELA
024900010601     C*
025000010604     C* Ebbene...
025100010604     C*
025200040119     C  N31              add       1             §CTROKVB
025300040119     C   32              add       1             §CTRMO
025400040119     C   31              add       1             §CTRNO
025500080418     C*
025600030822     C                   if        wGiro = 2
025700080513     C* Scrivo sempre il VAT
025800080513     C  N31              exsr      wrivat
025900110201     C                   exsr      chkvab
026000080513     C                   endif
026100010604     C*
026200010604     C                   if        *in31 = *off and
026300010604     C                             *in32 = *off
026400010604     C                   eval      vinflg = '1'
026500010604     C                   else
026600010604     C                   eval      vinflg = '2'
026700010604     C                   endif
026800141014     C                   endif
026900010601     C*
027000010604     C                   endif
027100010604     C*
027200010330     C                   else
027300010330     C                   eval      vinflg = '1'
027400010330     C                   endif
027500010601     C*
027600010601     C  N70              update    tivin000
027700010330     C*
027800030822     C  N70              enddo
027900040119     C*
028000080527     C* Verifico dati rimasti "in cannna"...
028100080527     C                   seton                                        65
028200110201     C                   exsr      chkvab
028300080527     C*
028400040119     C                   endif                                                  (endif - chkcall)
028500010601     C*
028600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028700010601     C                   if        cntNonEl = *zeros or
028800010601     C                             flgMulti = '0'
028900010330     C* Se non ci sono record con errori ...
029000010601     C                   if        §ctrno = 0 and
029100010604     C                             §ctrmo = 0 and
029200010601     C                             flgStato <> '2'
029300010330     C* ... restituisco esito OK.
029400010330     C                   eval      wrkesito = '0'
029500010330     C                   else
029600040119     C                   if        §ctrokvb > 0
029700010330     C                   eval      wrkesito = '1'
029800010330     C                   else
029900010615     C                   if        flgOk = '0'
030000010615     C                   eval      wrkesito = '2'
030100010615     C                   else
030200010615     C                   eval      wrkesito = '6'
030300010615     C                   endif
030400010330     C                   endif
030500010330     C                   endif
030600010601     C                   else
030700010601     C                   eval      wrkesito = '9'
030800010601     C                   endif
030900010330     C*
031000010330     C                   if        %open(tivin00r)
031100010330     C                   close     tivin00r
031200010330     C                   endif
031300021025     C                   if        %open(fivabwwr)
031400021025     C                   close     fivabwwr
031500010330     C                   endif
031600040512     C                   if        %open(fivatwwr)
031700040512     C                   close     fivatwwr
031800040512     C                   endif
031900010601     C*
032000010601     C                   if        vlrpoi <> 999
032100010601     C                   eval      invfil = vlrpoi
032200010601     C                   endif
032300010330     C*
032400040119     C                   if        §ctrokvb > 0
032500010601     C                             and invfil > *zeros
032600010330     C                   exsr      invio
032700010330     C                   endif
032800010601     C*
032900010618     C                   if        flgGiro = '1'
033000010601     C                   exsr      endela
033100010618     C                   endif
033200010330     C*
033300010330     C                   ENDSR
033400010330     C***
033500080527
033600080527
033700080527
033800080527     C*----------------------------------------------------*
033900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034000080527     C*----------------------------------------------------*
034100100413     C     CHKVAB        BEGSR
034200110201     C*
034300110201     C* Se richiesti ignorare colli / peso / volume
034400110315 xxx C***                if        *in35
034500110315 xxx C***                z-add     *zeros        VABNCL
034600110315 xxx C***                z-add     *zeros        VABPKB
034700110315 xxx C***                z-add     *zeros        VABVLB
034800110315 xxx C***                endif
034900080527     C*
035000080527     C* Scrivo il VAB solo a rottura d spedizione
035100080527     C                   if        depspe = *zeros
035200080527     C  N31              eval      fivabds_sav = fivabds
035300080527     C                   eval      depspe = vabNSP
035400080527     C                   else
035500080527     C                   if        depspe = vabNSP AND *in65 = *off
035600100413     C   51              add       VABNCL        sav_VABNCL
035700100413     C   51              add       VABPKB        sav_VABPKB
035800100413     C   51              add       VABVLB        sav_VABVLB
035900100720     C   52              z-add     VABNCL        sav_VABNCL
036000100720     C   52              z-add     VABPKB        sav_VABPKB
036100100720     C   52              z-add     VABVLB        sav_VABVLB
036200100720     C   53              add       VABNCL        sav_VABNCL
036300100720     C   53              z-add     VABPKB        sav_VABPKB
036400100720     C   53              z-add     VABVLB        sav_VABVLB
036500080527     C                   else
036600080527     C  N31              eval      fivabds_ok = fivabds_sav
036700080527     C  N31              exsr      wrivab
036800080527     C  N31              eval      fivabds_sav = fivabds
036900080527     C                   eval      depspe = vabNSP
037000080527     C                   endif
037100080527     C                   endif
037200080527     C*
037300080527     C                   ENDSR
037400040119
037500040119
037600040119
037700040119     C*----------------------------------------------------*
037800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037900040119     C*----------------------------------------------------*
038000040510     C     WRIVAB        BEGSR
038100080513     C*
038200080513     C* Gestisco l'eventuale rottura x numero spedizione
038300080527     C                   if        VABNSP <> depspe or *in65 = *on
038400070301     C                   if        wVATNOT_E <> *blanks
038500130610     C                   eval      ok_VABCTM = '7Q'
038600070301     C                   endif
038700100413     C                   write     fivab000
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
041200040510     C                   write     FIVAT000
041300040119     C                   add       1             §CTROKVT
041400110318     C                   endif
041500060331     C*
041600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041700060331     C                   if        wVATNOT_A <> *blanks
041800060331     C                   eval      VATNOT = wVATNOT_A
041900060331     C                   eval      VATTRC = 'A'
042000060331     C                   write     FIVAT000
042100060331     C                   add       1             §CTROKVT
042200060331     C                   endif
042300060331     C*
042400060331     C                   if        wVATNOT_B <> *blanks
042500060331     C                   eval      VATNOT = wVATNOT_B
042600060331     C                   eval      VATTRC = 'B'
042700060331     C                   write     FIVAT000
042800060331     C                   add       1             §CTROKVT
042900060331     C                   endif
043000140711     C*
043100140711     C                   if        wVATNOT_IJ <> *blanks
043200140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043300140711     C                   eval      VATTRC = 'I'
043400140711     C                   write     FIVAT000
043500140711     C                   add       1             §CTROKVT
043600140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
043700140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
043800140711     C                   eval      VATTRC = 'J'
043900140711     C                   write     FIVAT000
044000140711     C                   endif
044100140711     C                   endif
044200060331     C*
044300150316     C                   if        wVATNOT_E <> *blanks
044400060331     C                   eval      VATTRC = 'E'
044500110927     C*
044600150430     C* è un disk A
044700150430     C*
044800150316     C                   endif
044900150316     C*
045000110927     C                   if        wVATNOT_I <> *blanks
045100110927     C                   eval      VATNOT = wVATNOT_I
045200140711     C                   eval      VATTRC = 'I'
045300110927     C                   write     FIVAT000
045400110927     C                   add       1             §CTROKVT
045500110927     C                   endif
045600140220     C*
045700140220     C                   if        wVATNOT_J <> *blanks
045800140220     C                   eval      VATNOT = wVATNOT_J
045900140220     C                   eval      VATTRC = 'J'
046000140220     C                   write     FIVAT000
046100140220     C                   add       1             §CTROKVT
046200140220     C                   endif
046300140319     C*
046400140319     C                   if        wVATNOT_S <> *blanks
046500140319     C                   eval      VATNOT = wVATNOT_S
046600140319     C* i 2 flag devono essere blank
046700140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
046800140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
046900140319     C                   eval      VATTRC = 'S'
047000140319     C                   write     FIVAT000
047100140319     C                   add       1             §CTROKVT
047200140319     C                   endif
047300040119     C*
047400040119     C                   ENDSR
047500040119
047600040119
047700040119
047800040119     C*----------------------------------------------------*
047900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048000040119     C*----------------------------------------------------*
048100040119     C     PREVAT        BEGSR
048200040119     C*
048300040119     C* Compongo il nome del membro da dare al FIVATWWR
048400040119     C                   eval      parmbr = vlrhdl
048500040119     C                   movel     'M'           parmbr
048600040510     C                   eval      parccm = vlrksc
048700040119     C                   eval      paropz = '1'
048800040119     C* Effettuo la chiamata al CLLE preposto
048900040119     C                   call(e)   'TITVVTC'
049000040119     C                   parm                    parccm
049100040119     C                   parm                    parmbr
049200040119     C                   parm                    paropz
049300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049400040119     C                   if        %error
049500040119     C                   movel     '1'           chkcall
049600040119     C                   else
049700040119     C                   movel     '0'           chkcall
049800040119     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100010601
050200010601
050300010601
050400010330     C*----------------------------------------------------*
050500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050600010330     C*----------------------------------------------------*
050700040119     C     INZVAR        BEGSR
050800080527     C*
050900080527     C                   setoff                                       65
051000010330     C*
051100030822     C* Inizializzo variabili di wrk
051200020204     C                   Z-ADD     *zeros        Num5_0            5 0
051300101018     C                   MOVEL     *blanks       wFlgCAS
051400060331     C                   MOVEL     *blanks       wVATNOT_A
051500060331     C                   MOVEL     *blanks       wVATNOT_B
051600060331     C                   MOVEL     *blanks       wVATNOT_E
051700110927     C                   MOVEL     *blanks       wVATNOT_I
051800140220     C                   MOVEL     *blanks       wVATNOT_J
051900140220     C                   MOVEL     *blanks       wVATNOT_IJ
052000140319     C                   MOVEL     *blanks       wVATNOT_S
052100160520     C                   MOVEL     *blanks       w70
052200120411     C                   MOVEL     *blanks       Data8A
052300030822     C*
052400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052500030822     C                   CLEAR                   SkSplitCSV
052600030822     C                   EVAL      i = 1
052700030822     C                   EVAL      posDa = *zeros
052800030822     C                   EVAL      posA  = *zeros
052900080527     C*
053000080527     C                   CLEAR                   FIVABDS
053100080527     C                   CLEAR                   FIVAT000
053200030822     C*
053300030822     C* Reimposto i valori di default
053400030822     C                   EXSR      DEFCAM
053500030822     C*
053600010330     C                   ENDSR
053700010330     C*----------------------------------------------------*
053800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053900010330     C*----------------------------------------------------*
054000010330     C     DEFCAM        BEGSR
054100030822     C*
054200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054300030715     C* e delimitatore testo.
054400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054700100318     C*
054800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054900100318     C                   EVAL      CharSOS = CharNUM
055000030822     C*
055100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055200100720     C                   SETOFF                                       505152
055300100720     C                   SETOFF                                       53
055400100413     C                   SELECT
055500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055600100720     C                   SETON                                        50
055700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055800100413     C                   SETON                                        51
055900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056000100720     C                   SETON                                        52
056100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056200100720     C                   SETON                                        53
056300100413     C                   ENDSL
056400030822     C*
056500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056600030822     C                   EVAL      posDaDft = 1
056700030822     C                   EVAL      posADft  = 0
056800030822     C                   EVAL      wGiroDft = 0
056900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057000030822     C                             posDaDft > 0
057100030822     C*
057200030822     C* Gestisco il 1° giro
057300030822     C                   IF        wGiroDft = 0
057400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057600030822     C* Incremento il contatore dei "giri"
057700030822     C                   EVAL      wGiroDft = 1
057800030822     C                   ELSE
057900030822     C                   EVAL      posDaDft = posADft
058000030822     C                   ENDIF
058100030822     C* Eseguo lo scan x trovare la fine del campo corrente
058200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058300030822     C*
058400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058600030822     C                   IF        posDaDft > 0 AND
058700030822     C                             posADft  > 0
058800030822     C* NCL
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'NCL'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiInt=*on
059800030822     C                   Z-ADD     PiVal         VABNCL
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* CCM
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CCM'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100030822     C                   Z-ADD     PiVal         VABCCM
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* LNP
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'LNP'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABLNP
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* NRS
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'NRS'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABNRS
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* CTR
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'CTR'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABCTR
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* PKB
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'PKB'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiNum=*on
066300130610     C                   Z-ADD     PiVal         VABPKB
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* VLB
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VLB'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiNum=*on
067600130610     C                   Z-ADD     PiVal         VABVLB
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* QFT
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'QFT'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900030822     C                   Z-ADD     PiVal         VABQFT
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* CBO
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'CBO'
069700030822     C                   EVAL      VABCBO=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* TSP
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'TSP'
070600030822     C                   EVAL      VABTSP=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* VAS
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'VAS'
071500030822     C                   EVAL      VABVAS=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* VCA
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'VCA'
072400030822     C                   EVAL      VABVCA=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800070925     C* TIC
072900070925     C                   IF        %subst(
073000070925     C                             %subst(vlrppt:posDaDft+1:
073100070925     C                             posADft-posDaDft-1):1:3)
073200070925     C                             = 'TIC'
073300070925     C                   EVAL      VABTIC=%trim(%subst(
073400070925     C                             %subst(vlrppt:posDaDft+1:
073500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600070925     C                   ENDIF
073700030822     C* GCA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GCA'
074200030822     C                   EVAL      VABGCA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* CTM
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'CTM'
075100030822     C                   EVAL      VABCTM=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* FFD
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'FFD'
076000030822     C                   EVAL      VABFFD=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* VAD
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'VAD'
076900030822     C                   EVAL      VABVAD=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* GMA
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'GMA'
077800030822     C                   EVAL      VABGMA=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200030822     C* GGA
078300030822     C                   IF        %subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):1:3)
078600030822     C                             = 'GGA'
078700030822     C                   EVAL      VABGGA=%trim(%subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000030822     C                   ENDIF
079100030822     C* GVA
079200030822     C                   IF        %subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):1:3)
079500030822     C                             = 'GVA'
079600030822     C                   EVAL      VABGVA=%trim(%subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079900030822     C                   ENDIF
080000030822     C* TC1
080100030822     C                   IF        %subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):1:3)
080400030822     C                             = 'TC1'
080500030822     C                   EVAL      VABTC1=%trim(%subst(
080600030822     C                             %subst(vlrppt:posDaDft+1:
080700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080800030822     C                   ENDIF
080900030822     C* TC2
081000030822     C                   IF        %subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):1:3)
081300030822     C                             = 'TC2'
081400030822     C                   EVAL      VABTC2=%trim(%subst(
081500030822     C                             %subst(vlrppt:posDaDft+1:
081600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081700030822     C                   ENDIF
081800040714     C* VATTRC
081900040714     C                   IF        %subst(
082000040714     C                             %subst(vlrppt:posDaDft+1:
082100040714     C                             posADft-posDaDft-1):1:3)
082200040714     C                             = 'TRC'
082300040714     C                   EVAL      VATTRC=%trim(%subst(
082400040714     C                             %subst(vlrppt:posDaDft+1:
082500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082600040714     C                   ENDIF
082700030822     C* ...
082800030822     C                   ENDIF
082900030822     C                   ENDDO
083000020204     C*
083100010330     C                   ENDSR
083200010607     C*----------------------------------------------------*
083300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083400010607     C*----------------------------------------------------*
083500010607     C     IMPVAB        BEGSR
083600010607     C*
083700010607     C                   SETOFF                                       3132
083800110315 xxx C***                SETON                                        35
083900030515     C*
084000010607     C                   MOVEL     datcor        VABAAS
084100010607     C                   MOVE      datcor        VABMGS
084200010607     C*
084300030715     C**********
084400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084500030715     C**********
084600030715     C                   IF        wGiro = *zeros
084700040113     C*
084800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085100080429     C                             %trim(intcol(5))<>*blanks
085200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085400080429     C                                      %trim(intcol(5))
085500070823     C     ';':CharCSV   XLATE     vindta        vindta
085600040510     C                   ENDIF
085700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085800031201     C     minu:maiu     XLATE     vindta        vindta
085900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086000030715     C                   DOW       posDa <= %len(%trim(vindta))
086100030715     C*
086200030715     C* Gestisco il 1° campo
086300030715     C                   IF        i = 1
086400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086500030715     C                   EVAL      posDa = 1
086600030715     C* Eseguo lo scan x trovare la fine del primo campo
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C* X i campi successivi al 1°
087200030715     C                   ELSE
087300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087400030715     C                   EVAL      posDa = posA + 1
087500030715     C* Eseguo lo scan x trovare la fine del campo corrente
087600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087700030715     C* Gestisco l'ultimo campo
087800030715     C                   IF        posA = *zeros
087900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   LEAVE
088400030715     C                   ELSE
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   ENDIF
088900030715     C                   ENDIF
089000030715     C* Incremento il contatore d campo
089100030715     C                   EVAL      i = i +1
089200030715     C                   ENDDO
089300030715     C*
089400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089500030715     C* delimitazione testo
089600030715     C                   EVAL      i = 1
089700030715     C                   DOW       i < %elem(SkSplitFLD)
089800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090000030715     C                   EVAL      i = i + 1
090100030715     C                   ENDDO
090200030715     C                   EVAL      wGiro = 1
090300030715     C                   ELSE
090400030715     C                   EVAL      wGiro = 2
090500031204     C**********
090600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090700031204     C* d separatore campo
090800031204     C**********
090900031204     C                   EVAL      vindta = %trim(vindta)
091000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091100031204     C                   EVAL      vindta = %subst(vindta:2)
091200031204     C                   ENDDO
091300031204     C*
091400031204     C                   Z-ADD     *zeros        lunghInput        4 0
091500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091900110912     C                   ENDDO
092000030715     C**********
092100030715     C* Effettuo lo split del campo dati d input
092200030715     C**********
092300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092400030715     C                   DOW       posDa <= %len(%trim(vindta))
092500030715     C*
092600030715     C* Gestisco il 1° campo
092700030715     C                   IF        i = 1
092800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092900030715     C                   EVAL      posDa = 1
093000030715     C* Eseguo lo scan x trovare la fine del primo campo
093100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C* X i campi successivi al 1°
093600030715     C                   ELSE
093700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093800030715     C                   EVAL      posDa = posA + 1
093900031204     C* Verifico che nn vi sia il campo nn valorizzato
094000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094100031204     C* Se campo nn valorizzato skippo al prossimo
094200031204     C                   EVAL      SkSplitCSV(i) = *blanks
094300031204     C                   EVAL      posA  = posA + 1
094400031204     C                   ELSE
094500030715     C* Eseguo lo scan x trovare la fine del campo corrente
094600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094700030715     C* Gestisco l'ultimo campo
094800030715     C                   IF        posA = *zeros
094900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095000030715     C* A questo punto "estraggo" il campo corrente
095100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095200030715     C                                                    (posA-posDa))
095300030715     C                   LEAVE
095400030715     C                   ELSE
095500030715     C* A questo punto "estraggo" il campo corrente
095600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095700030715     C                                                    (posA-posDa))
095800030715     C                   ENDIF
095900031204     C                   ENDIF
096000030715     C                   ENDIF
096100030715     C* Incremento il contatore d campo
096200030715     C                   EVAL      i = i +1
096300030715     C                   ENDDO
096400030715     C*
096500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096600030715     C* delimitazione testo
096700030715     C                   EVAL      i = 1
096800030715     C                   DOW       i < %elem(SkSplitCSV)
096900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097100030715     C                   EVAL      i = i + 1
097200030715     C                   ENDDO
097300100720     C*
097400100720     C* Stacco un progressivo x numero spedizione
097500150430 xxx C                   EXSR      repNSP
097600100326     C*
097700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097800030715     C                   EVAL      i = 1
097900030715     C                   DOW       i < %elem(SkSplitFLD)
098000030715     C*
098100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098300030715     C                             SkSplitCSV(i) <> *blanks
098400030721     C*** ATB
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098600030721     C                   EVAL      VABATB = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030715     C*** RSD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099300030715     C* ==
099400130528     C                   ENDIF
099500030721     C*** RD2
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030715     C*** IND
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100100030715     C                   EVAL      VABIND = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** LOD
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** PRD
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030721     C*** NZD
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030715     C*** RMA
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030715     C*** NOT
102000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102200030715     C                   ENDIF
102300030715     C*** NT2
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** VCA
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** VAS
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** FFD
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030721     C*** GC1
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** GC2
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** TSP
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105000030721     C                   ENDIF
105100040510     C*** CBO
105200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105400040510     C                   ENDIF
105500030721     C*** NAS
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TIC
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GCA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** XCO
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** CTM
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TCR
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTS
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** FTM
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** VAD
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** GMA
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** GGA
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GVA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** TC1
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** TC2
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** SCL
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** RMO
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** NMO
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*
112400030721     C* Reperisco quindi i campi numerici...
112500040510     C*** NSP
112600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112700040510     C                   EVAL      PiStr=SkSplitCSV(i)
112800040510     C                   EXSR      CHKNUM
112900040510     C                   IF        PiInt=*on
113000040510     C                   Z-ADD     PiVal         VABNSP
113100040510     C                   ELSE
113200040510     C                   SETON                                        31
113300040510     C                   EVAL      VABNSP = *zeros
113400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113500040510     C                             + ' ' + 'VABNSP'
113600040510     C                   ENDIF
113700040510     C                   ENDIF
113800071106     C*** RMN
113900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114000071106     C                   EVAL      PiStr=SkSplitCSV(i)
114100071106     C                   EXSR      CHKNUM
114200071106     C                   IF        PiInt=*on
114300071106     C                   Z-ADD     PiVal         VABRMN
114400071106     C                   ELSE
114500071106     C                   SETON                                        32
114600071106     C                   EVAL      VABRMN = *zeros
114700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114800071106     C                             + ' ' + 'VABRMN'
114900071106     C                   ENDIF
115000071106     C                   ENDIF
115100030721     C*** CAD
115200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115300130610     C                   EVAL      PiStr=SkSplitCSV(i)
115400130610     C                   EXSR      CHKNUM
115500130610     C                   IF        PiInt=*on
115600130610     C                   Z-ADD     PiVal         Num5_0
115700130610     C                   MOVEL(p)  Num5_0        VABCAD
115800130610     C                   ELSE
115900130610     C                   SETON                                        32
116000130610     C                   EVAL      VABCAD = *zeros
116100130610     C                   EVAL      vinmsg = %trimr(vinmsg)
116200130610     C                             + ' ' + 'VABCAD'
116300130610     C                   ENDIF
116400130605     C                   ENDIF
116500030721     C*** NCL
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiInt=*on
117000030721     C                   Z-ADD     PiVal         VABNCL
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      VABNCL = *zeros
117400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117500040122     C                             + ' ' + 'VABNCL'
117600030721     C                   ENDIF
117700030721     C                   ENDIF
117800030721     C*** PKB
117900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118000030721     C                   EVAL      PiStr=SkSplitCSV(i)
118100030721     C                   EXSR      CHKNUM
118200030721     C                   IF        PiNum=*on
118300130610     C                   Z-ADD(H)  PiVal         VABPKB
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABPKB = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABPKB'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** CAS
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119500160404     C                             %trim(SkSplitCSV(i)) <> '0,00'
119600130703     C                   EVAL      VABVCA = 'EUR'
119700160404     C                   EVAL      VABTIC = 'BM'
119800140513     C                   EVAL      wFlgCAS = '1'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiNum=*on
120200030721     C                   Z-ADD     PiVal         VABCAS
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABCAS = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABCAS'
120800030721     C                   ENDIF
120900140513     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** CCM
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on      AND
121600030721     C                             PiVal<=9999999 AND
121700030721     C                             PiVal>*zeros
121800030721     C                   Z-ADD     PiVal         VABCCM
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCCM = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300040510     C                             + ' ' + 'VABCCM'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** LNP
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on    AND
123100030721     C                             PiVal<=999   AND
123200030721     C                             PiVal>*zeros
123300030721     C                   Z-ADD     PiVal         VABLNP
123400030721     C                   ELSE
123500040715     C                   SETON                                        32
123600030721     C                   EVAL      VABLNP = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800040510     C                             + ' ' + 'VABLNP'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** NRS
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on AND
124600030721     C                             PiVal<=99
124700030721     C                   Z-ADD     PiVal         VABNRS
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABNRS'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** LNA
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on    AND
125900030721     C                             PiVal<=999   AND
126000030721     C                             PiVal>*zeros
126100030721     C                   Z-ADD     PiVal         VABLNA
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      VABLNA = *zeros
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABLNA'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** CTR
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on  AND
127400030721     C                             PiVal<=999
127500030721     C                   Z-ADD     PiVal         VABCTR
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABCTR'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** IAS
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128400141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
128500141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
128600150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
128700141006     C                   EVAL      VABVAS = 'EUR'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiNum=*on
129100030721     C                   Z-ADD     PiVal         VABIAS
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABIAS'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800141003     C                   ENDIF
129900030721     C*** VLB
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400130610     C                   Z-ADD(H)  PiVal         VABVLB
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABVLB'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** QFT
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiNum=*on
131600030721     C                   Z-ADD     PiVal         VABQFT
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABQFT'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** NCD
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on      AND
132800030721     C                             PiVal<=9999999
132900030721     C                   Z-ADD     PiVal         VABNCD
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABNCD'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** NCA
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on      AND
134100030721     C                             PiVal<=9999999
134200030721     C                   Z-ADD     PiVal         VABNCA
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABNCA'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** ZNC
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on AND
135400030721     C                             PiVal<=99
135500030721     C                   Z-ADD     PiVal         VABZNC
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABZNC'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** DCR
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on       AND
136700030721     C                             PiVal<=99999999
136800030721     C                   Z-ADD     PiVal         VABDCR
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABDCR'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** HCR
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on   AND
138000030721     C                             PiVal<=9999
138100030721     C                   Z-ADD     PiVal         VABHCR
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABHCR'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** VMD
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiNum=*on
139300030721     C                   Z-ADD     PiVal         VABVMD
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABVMD'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** ANT
140100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiInt=*on        AND
140500030721     C                             PiVal<=999999999
140600030721     C                   Z-ADD     PiVal         VABANT
140700030721     C                   ELSE
140800030721     C                   SETON                                        32
140900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141000030721     C                             + ' ' + 'VABANT'
141100030721     C                   ENDIF
141200030721     C                   ENDIF
141300030721     C*** CMO
141400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141500030721     C                   EVAL      PiStr=SkSplitCSV(i)
141600030721     C                   EXSR      CHKNUM
141700030721     C                   IF        PiInt=*on
141800030721     C                   Z-ADD     PiVal         Num5_0
141900030721     C                   MOVEL(p)  Num5_0        VABCMO
142000030721     C                   ELSE
142100030721     C                   SETON                                        32
142200030721     C                   EVAL      VABCMO = *zeros
142300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142400030721     C                             + ' ' + 'VABCMO'
142500030721     C                   ENDIF
142600030721     C                   ENDIF
142700040510     C*
142800040510     C* Reperisco i campi del FIVAT
142900040510     C*** VATNOT
143000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143200040510     C                   ENDIF
143300040510     C*** VATTRC
143400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143600040510     C                   ENDIF
143700060331     C*** VATNOT_A
143800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144000060331     C                   ENDIF
144100060331     C*** VATNOT_B
144200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144400060331     C                   ENDIF
144500060331     C*** VATNOT_E
144600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144800060331     C                   ENDIF
144900110927     C*** VATNOT_I
145000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145200110927     C                   ENDIF
145300170413     C*** VATNOT_J
145400170413     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
145500170413     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145600170413     C                   ENDIF
145700140319     C*** VATNOT_S
145800140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
145900140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
146000140319     C                   ENDIF
146100070823     C*
146200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146300150309     C*
146400160404     C*** VABNSP/VABRMN
146500160404     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
146600160404     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
146700160404     C                   EXSR      CHKNUM
146800160404     C                   IF        PiInt=*on
146900160404     C                   Z-ADD     PiVal         VABRMN
147000160404     C                   Z-ADD     PiVal         VABNSP
147100160404     C                   ELSE
147200160404     C                   SETON                                        32
147300160404     C                   EVAL      VABRMN = *zeros
147400160404     C                   EVAL      VABNSP = *zeros
147500160404     C                   EVAL      vinmsg = %trimr(vinmsg)
147600160404     C                             + ' ' + 'VABRMN VABNSP VATNSP'
147700160404     C                   ENDIF
147800160404     C                   ENDIF
147900160404     C*
148000160404      *** W_VABRSD: ciò che eccede i 35 char va in VABRD2
148100160404     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
148200160520     C                   MOVEL     *blanks       w70
148300160520     C                   EVAL      w70 =%trim(SkSplitCSV(i))
148400160520     C                   EVAL      VABRSD=%subst(w70:1:35)
148500160520     C                   EVAL      VABRD2=%subst(w70:36:35)
148600160404     C                   ENDIF
148700160520     C*
148800160520      *** W_VABNOT: ciò che eccede i 35 char va in VABNT2
148900160520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
149000160520     C                   MOVEL     *blanks       w70
149100160520     C                   EVAL      w70=%trim(SkSplitCSV(i))
149200160520     C                   EVAL      VABNOT=%subst(w70:1:35)
149300160520     C                   EVAL      VABNT2=%subst(w70:36:35)
149400160520     C                   ENDIF
149500170413     C*
149600170413     C*** W_VATNOT_IJ
149700170413     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_IJ'
149800170413     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149900170413     C                   ENDIF
150000150316
150100141003     C*
150200071029     C***  <===  -----------------------------------------
150300040510     C*
150400030715     C                   ENDIF
150500030715     C                   EVAL      i = i + 1
150600030715     C                   ENDDO
150700070925     C*
150800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150900150603
151000070925     C*
151100160404     C* pulisco la nazione se vuol essere Italia
151200160404     C                   IF        VABNZD = 'I'     OR
151300160404     C                             VABNZD = 'IT'    OR
151400160404     C                             VABNZD = 'ITA'
151500160404     C                   EVAL      VABNZD = *blank
151600160404     C                   ENDIF
151700160404     C*
151800070925     C* Considerazioni finali su CBO/CAS
151900140220     C                   IF        wFlgCAS = '1'    OR
152000140220     C                             VABCAS  > *zeros
152100140220     C                   IF        VABCBO = '1'
152200140220     C                   EVAL      VABCBO = '4'
152300140220     C                   ENDIF
152400140220     C                   IF        VABCBO = '2'
152500140220     C                   EVAL      VABCBO = '6'
152600140220     C                   ENDIF
152700140220     C                   ENDIF
152800070925     C***  <===  ----------------------------
152900040510     C*
153000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153100040510     C                   IF        VABNSP = *zeros
153200040510     C                   SETON                                        31
153300040510     C                   ENDIF
153400040715     C*
153500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153600040715     C                   IF        VABLNP = *zeros
153700040715     C                   SETON                                        31
153800040715     C                   ENDIF
153900040510     C*
154000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154100120411     C                   IF        VABRMN = *zeros
154200120411     C                   Z-ADD     VABNSP        VABRMN
154300120411     C                   ENDIF
154400120328     C* IN QUESTO CASO LO FORZO = 1
154500120411     C***                IF        VABRMN = *zeros
154600120411     C***                Z-ADD     1             VABRMN
154700120411     C***                ENDIF
154800040510     C*
154900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155000040510     C                   IF        VABRMA = *blanks
155100040714     C                   MOVEL     VABRMN        VABRMA
155200040510     C                   ENDIF
155300040128     C*
155400040510     C* Se provincia nn valorizzata la reperisco
155500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155600110912     C                   IF        VABPRD =  *blanks AND
155700110912     C                             VABNZD =  *blanks AND
155800110912     C                             VABCAD <> *blanks AND
155900110912     C                             VABLOD <> *blanks
156000040128     C                   CLEAR                   TISI95DS
156100040128     C                   EVAL      I95TCN = '3'
156200040128     C                   Z-ADD     datcor        I95DAT
156300040128     C                   EVAL      I95NAR = VABNZD
156400040128     C                   EVAL      I95CAP = VABCAD
156500040128     C                   EVAL      I95LOC = VABLOD
156600040128     C                   CALL      'TISI95R'
156700040128     C                   PARM                    TISI95DS
156800040128     C                   EVAL      VABPRD = O95PRV
156900040510     C                   ENDIF
157000020204     C*
157100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157200020204     C                   EXSR      CHKIMPDIV
157300030715     C                   ENDIF
157400020204     C*
157500010607     C                   ENDSR
157600010607     C*----------------------------------------------------*
157700010601
157800020204
157900020204     C*----------------------------------------------------*
158000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158100020204     C*----------------------------------------------------*
158200020204     C     CHKIMPDIV     BEGSR
158300020204     C*
158400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158500020204     C                   Z-ADD     *zeros        wrkDec            9 9
158600020204     C*
158700020204     C* Come prima cosa effettuo considerazioni sulla divisa
158800020204     C                   IF        vabIAS > *zeros
158900020204     C                   IF        vabVAS <> 'EUR'
159000020204     C                   EVAL      vabVAS =  'ITL'
159100020204     C                   ENDIF
159200020204     C                   ENDIF
159300020204     C*
159400020204     C                   IF        vabCAS > *zeros
159500020204     C                   IF        vabVCA <> 'EUR'
159600020204     C                   EVAL      vabVCA =  'ITL'
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C                   IF        vabVMD > *zeros
160100020204     C                   IF        vabVAD <> 'EUR'
160200020204     C                   EVAL      vabVAD =  'ITL'
160300020204     C                   ENDIF
160400020204     C                   ENDIF
160500020204     C*
160600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160700020204     C                   Z-ADD     vabIAS        wrkDec
160800020204     C                   IF        wrkDec > *zeros
160900020204     C                   IF        vabVAS = 'ITL'
161000020204     C                   EVAL      vabIAS = *zeros
161100020204     C                   ENDIF
161200020204     C                   ENDIF
161300020204     C*
161400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161500020204     C                   Z-ADD     vabCAS        wrkDec
161600020204     C                   IF        wrkDec > *zeros
161700020204     C                   IF        vabVCA = 'ITL'
161800020204     C                   EVAL      vabCAS = *zeros
161900020204     C                   ENDIF
162000020204     C                   ENDIF
162100020204     C*
162200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162300020204     C                   Z-ADD     vabVMD        wrkDec
162400020204     C                   IF        wrkDec > *zeros
162500020204     C                   IF        vabVAD = 'ITL'
162600020204     C                   EVAL      vabVMD = *zeros
162700020204     C                   ENDIF
162800020204     C                   ENDIF
162900020204     C*
163000020204     C                   ENDSR
163100020204     C***
163200010330
163300100318
163400100318
163500100318     C*----------------------------------------------------*
163600100318     C*  CONTROLLO NUMERICITA' CAMPI
163700100318     C*----------------------------------------------------*
163800100318     C     CHKNUM        BEGSR
163900100318     C*
164000100318     C                   IF        PiDecChr = *blanks
164100100318     C                   EVAL      PiDecChr = CharNUM
164200100318     C                   ENDIF
164300100318     C*
164400100318     C                   callp     UBISNUM_Check(PiStr
164500100318     C                                          :PiDecChr
164600100318     C                                          :PiVal
164700100318     C                                          :PiNum
164800100318     C                                          :PiInt)
164900100318     C*
165000100318     C                   ENDSR
165100100318     C***
165200010601
165300010601
165400010601
165500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165600141006      ***
165700141014     C     repfil        BEGSR
165800141014     C*
165900141014     C                   if        invfil = *zeros and
166000141014     C                             depfil > *zeros and
166100141014     C                             (vinflg = *blanks or
166200141014     C                              vinflg = *zeros)
166300141014     C*
166400141014     C                   eval      invfil = depfil
166500141014     C                   endif
166600141014     C*
166700141014     C                   if        depfil <> invfil and
166800141014     C                             invfil > *zeros
166900141014     C                   eval      flgMulti = '1'
167000141014     C                   if        vinflg = *blanks
167100141014     C                   add       1             cntNonEl
167200141014     C                   endif
167300141014     C                   endif
167400141014     C*
167500141014     C                   if        vinflg = '2'
167600141014     C                   eval      flgStato = '2'
167700141014     C                   endif
167800141014     C*
167900141014     C                   ENDSR
168000141014     C*
168100010601
168200010601
168300010601
168400010330
168500040119      /TITLE Invio dei dati al punto operativo.
168600040119     C     invio         BEGSR
168700040119     C*
168800040119     C* 1° invio FIVAT
168900040119     C                   reset                   dscmz
169000040119     C                   move      vlrpoi        cmzdst
169100040119     C                   eval      cmzfld = 'FIVATWWR'
169200040119     C                   eval      cmzmbd = vlrhdl
169300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169400040119     C***                if        prmfir = *blanks
169500040119     C                   eval      cmzfla = 'FIVAT00F'
169600040119     C                   eval      cmzmba = 'FIVAT00F'
169700040119     C***                else
169800040119     C***                eval      cmzfla = prmfir
169900040119     C***                eval      cmzmba = prmfir
170000040119     C***                endif
170100040119     C                   eval      cmznrr = *zeros
170200040119     C                   move      §ctrokvt      cmznrr
170300040119     C                   eval      cmzlba = vlrfl1
170400040119     C                   call(e)   'TIS711C'
170500040119     C                   parm                    dscmz
170600040119     C                   parm      *blanks       esito
170700040119     C                   if        %error
170800040119     C                             or cmzerr = '1'
170900040119     C                             or esito  = '1'
171000040119     C                   eval      wrkesito = '3'
171100040119     C                   else
171200040119     C*
171300040119     C* 2° invio FIVAB
171400040119     C                   reset                   dscmz
171500040119     C                   move      vlrpoi        cmzdst
171600040119     C                   eval      cmzfld = vlrfou
171700040119     C                   eval      cmzmbd = vlrhdl
171800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171900040119     C***                if        prmfir = *blanks
172000040119     C                   eval      cmzfla = 'FIVAB00F'
172100040119     C                   eval      cmzmba = 'FIVAB00F'
172200040119     C***                else
172300040119     C***                eval      cmzfla = prmfir
172400040119     C***                eval      cmzmba = prmfir
172500040119     C***                endif
172600040119     C                   eval      cmznrr = *zeros
172700040119     C                   move      §ctrokvb      cmznrr
172800040119     C                   eval      cmzlba = vlrfl1
172900040119     C                   call(e)   'TIS711C'
173000040119     C                   parm                    dscmz
173100040119     C                   parm      *blanks       esito
173200040119     C                   if        %error
173300040119     C                             or cmzerr = '1'
173400040119     C                             or esito  = '1'
173500040119     C                   eval      wrkesito = '3'
173600040119     C                   endif
173700040119     C                   endif
173800040119     C*
173900040119     C                   ENDSR
174000040119     C***
174100010601
174200010601
174300010601
174400010601
174500010601      /TITLE Invio dei dati al punto operativo.
174600010601     C     opeini        BEGSR
174700010601     C*
174800010601     C* Inizializzo flag e contatori operativi
174900010601     C                   movel     '0'           flgGiro           1
175000010601     C                   movel     '0'           flgMulti          1
175100010601     C                   movel     '1'           flgStato          1
175200010615     C                   movel     '0'           flgOk             1
175300010601     C                   z-add     *zeros        cntNonEl         10 0
175400010601     C                   z-add     *zeros        depfil            3 0
175500010601     C                   z-add     *zeros        invfil            3 0
175600010601     C*
175700010601     C                   ENDSR
175800010601     C***
175900070326
176000070326
176100070326
176200070326
176300070326     C     *pssr         BEGSR
176400070329     C*
176500070329     C                   if        %open(tivin00r)
176600070329     C                   close     tivin00r
176700070329     C                   endif
176800070329     C                   if        %open(fivabwwr)
176900070329     C                   close     fivabwwr
177000070329     C                   endif
177100070329     C                   if        %open(fivatwwr)
177200070329     C                   close     fivatwwr
177300070329     C                   endif
177400070326     C*
177500070326     C* Effettuo la chiamata al CLLE preposto
177600070326     C                   call(e)   'TITVVTC'
177700070326     C                   parm                    parccm
177800070326     C                   parm                    parmbr
177900070326     C                   parm      '2'           paropz
178000070326     C*
178100070326     C                   eval      wrkesito = '2'
178200070404     C*
178300070404     C                   seton                                        LR
178400070326     C*
178500070326     C                   ENDSR     '*CANCL'
178600070326     C***
178700100318
178800100318
178900100318
179000100318     C     normCSV       BEGSR
179100100318     C*
179200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179300100318     C* separatori campi
179400100318     C                   if        CharTXT <> *blanks
179500100318     C                   z-add     *zeros        wLenVINDTA        4 0
179600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179700100318     C                   setoff                                       80
179800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180100100318     C                   if        not *in80
180200100318     C                   seton                                        80
180300100318     C                   else
180400100318     C                   setoff                                       80
180500100318     C                   endif
180600100318     C                   endif
180700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180800100318     C                             and *in80 = *on
180900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181000100318     C                   endif
181100100318     C                   enddo
181200100318     C                   endif
181300100318     C*
181400100318     C                   ENDSR
181500100318     C***
181600070326
181700100326
181800100326
181900100326     C*----------------------------------------------------*
182000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182100100326     C*----------------------------------------------------*
182200100326     C     REPNSP        BEGSR
182300100326     C*
182400100326     C* NSP => Stacco un numeratore da AZNUM
182500100326     C                   clear                   TRUL33DS
182600100326     C                   eval      I33OPE = *zeros
182700100326     C                   eval      I33CNU = 302
182800100326     C                   eval      I33NUM = 1
182900100326     C                   movel     TRUL33DS      KPJBU
183000100326     C                   call      'TRUL33R'
183100100326     C                   parm                    KPJBA
183200100326     C                   movel     KPJBU         TRUL33DS
183300100326     C                   if        O33ERR = *zeros
183400100326     C                   z-add     O33NRF        VABNSP
183500100326     C                   z-add     O33NRF        VATNSP
183600100326     C                   else
183700100326     C                   SETON                                        31
183800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183900140711     C                             + ' ' + 'VABNSP VATNSP'
184000100326     C                   endif
184100100326     C*
184200100326     C                   ENDSR
184300010330
184400100326
184500010330
184600000613     C     *inzsr        BEGSR
184700990910     C*
184800990910     C     *entry        plist
184900990920     C                   parm                    tivlrds
185000990921     C                   parm      wrkesito      esito
185100000724     C                   parm                    prmlit
185200000710     C                   parm                    prmfir
185300010330     C*
185400010330     C* CALCOLA LA DATA CORRENTE
185500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185600100318     C                   eval      datcor = %dec(%date() : *ISO)
185700000613     C*
185800000613     C                   ENDSR
185900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186000160520X;W_VABRMN;W_VABRSD;W_VABNOT;VABIND;VABCAD;VABLOD;VABPRD;
186100170413VABNCL;VABPKB;VABVLB;W_VATNOT_IJ;
186200150428
186300150428
186400150428
