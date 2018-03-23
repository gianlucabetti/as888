000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400150428      * disk A
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
005700030715     D SkSplitFLD      S             10    DIM(100)
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
006900150316     D*wVATNOT_E       s                   LIKE(VATNOT)
007000150316     D wVATNOT_E       DS           700
007100150316     D NOT_E                         35    DIM(20)
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
018600120410     C*
018700120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018800120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018900120410     C                   IF        Intestazioni = *off
019000120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019100120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019200120410     C                   exsr      inzvar
019300120410     C                   exsr      impvab
019400120410     C                   ENDIF
019500010330     C*
019600030822     C                   do        *HIVAL
019700010330     C*
019800030822     C                   read      tivin00r                               70
019900010618     C*
020000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020100010618     C                   if        vinflg = '1'
020200010618     C                   eval      flgOk = '1'
020300010618     C                   endif
020400040510     C*
020500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020600040510     C                   z-add     *zeros        wLenVINDTA        4 0
020700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021100040510     C                   z-add     1             wFlgVINDTA
021200040510     C                   leave
021300040510     C                   endif
021400040510     C                   enddo
021500010618     C*
021600040510     C                   if        vindta > *blanks AND
021700040510     C                             wFlgVINDTA = 1
021800010330     C                   add       1             rrnum
021900010330     C*
022000010601     C                   if        *in70 = *off and
022100010330     C                             (vinflg = *blanks
022200010330     C                              or vinflg = '0'
022300010330     C                              or vinflg = '2')
022400010330     C*
022500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022600010711     C                   if        vinflg = *blanks or vinflg = '0'
022700010711     C                   clear                   vinmsg
022800010711     C                   endif
022900130917     C*
023000130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023100130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023200130917     C* evitare che vengano elisi i blank
023300140220     C***                eval      vindta  = '*' + %trim(vindta)
023400130917     C*
023500080527     C                   exsr      inzvar
023600100318     C                   exsr      normCSV
023700010605     C                   exsr      impvab
023800010601     C*
023900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024000141014     C                   eval      depfil = VABLNP
024100141014     C                   exsr      repfil
024200141014     C                   if        depfil = invfil
024300150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
024400150430     C                   if        vlrpoi = 999
024500150430     C                   move(P)   invfil        VABFGS
024600150430     C                   else
024700150430     C                   move(P)   vlrpoi        VABFGS
024800150430     C                   endif
024900010601     C*
025000010601     C                   exsr      PREELA
025100010601     C*
025200010604     C* Ebbene...
025300010604     C*
025400040119     C  N31              add       1             §CTROKVB
025500040119     C   32              add       1             §CTRMO
025600040119     C   31              add       1             §CTRNO
025700080418     C*
025800030822     C                   if        wGiro = 2
025900080513     C* Scrivo sempre il VAT
026000080513     C  N31              exsr      wrivat
026100110201     C                   exsr      chkvab
026200080513     C                   endif
026300010604     C*
026400010604     C                   if        *in31 = *off and
026500010604     C                             *in32 = *off
026600010604     C                   eval      vinflg = '1'
026700010604     C                   else
026800010604     C                   eval      vinflg = '2'
026900010604     C                   endif
027000141014     C                   endif
027100010601     C*
027200010604     C                   endif
027300010604     C*
027400010330     C                   else
027500010330     C                   eval      vinflg = '1'
027600010330     C                   endif
027700010601     C*
027800010601     C  N70              update    tivin000
027900010330     C*
028000030822     C  N70              enddo
028100040119     C*
028200080527     C* Verifico dati rimasti "in cannna"...
028300080527     C                   seton                                        65
028400110201     C                   exsr      chkvab
028500080527     C*
028600040119     C                   endif                                                  (endif - chkcall)
028700010601     C*
028800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028900010601     C                   if        cntNonEl = *zeros or
029000010601     C                             flgMulti = '0'
029100010330     C* Se non ci sono record con errori ...
029200010601     C                   if        §ctrno = 0 and
029300010604     C                             §ctrmo = 0 and
029400010601     C                             flgStato <> '2'
029500010330     C* ... restituisco esito OK.
029600010330     C                   eval      wrkesito = '0'
029700010330     C                   else
029800040119     C                   if        §ctrokvb > 0
029900010330     C                   eval      wrkesito = '1'
030000010330     C                   else
030100010615     C                   if        flgOk = '0'
030200010615     C                   eval      wrkesito = '2'
030300010615     C                   else
030400010615     C                   eval      wrkesito = '6'
030500010615     C                   endif
030600010330     C                   endif
030700010330     C                   endif
030800010601     C                   else
030900010601     C                   eval      wrkesito = '9'
031000010601     C                   endif
031100010330     C*
031200010330     C                   if        %open(tivin00r)
031300010330     C                   close     tivin00r
031400010330     C                   endif
031500021025     C                   if        %open(fivabwwr)
031600021025     C                   close     fivabwwr
031700010330     C                   endif
031800040512     C                   if        %open(fivatwwr)
031900040512     C                   close     fivatwwr
032000040512     C                   endif
032100010601     C*
032200010601     C                   if        vlrpoi <> 999
032300010601     C                   eval      invfil = vlrpoi
032400010601     C                   endif
032500010330     C*
032600040119     C                   if        §ctrokvb > 0
032700010601     C                             and invfil > *zeros
032800010330     C                   exsr      invio
032900010330     C                   endif
033000010601     C*
033100010618     C                   if        flgGiro = '1'
033200010601     C                   exsr      endela
033300010618     C                   endif
033400010330     C*
033500010330     C                   ENDSR
033600010330     C***
033700080527
033800080527
033900080527
034000080527     C*----------------------------------------------------*
034100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034200080527     C*----------------------------------------------------*
034300100413     C     CHKVAB        BEGSR
034400110201     C*
034500110201     C* Se richiesti ignorare colli / peso / volume
034600110315 xxx C***                if        *in35
034700110315 xxx C***                z-add     *zeros        VABNCL
034800110315 xxx C***                z-add     *zeros        VABPKB
034900110315 xxx C***                z-add     *zeros        VABVLB
035000110315 xxx C***                endif
035100080527     C*
035200080527     C* Scrivo il VAB solo a rottura d spedizione
035300080527     C                   if        depspe = *zeros
035400080527     C  N31              eval      fivabds_sav = fivabds
035500080527     C                   eval      depspe = vabNSP
035600080527     C                   else
035700080527     C                   if        depspe = vabNSP AND *in65 = *off
035800100413     C   51              add       VABNCL        sav_VABNCL
035900100413     C   51              add       VABPKB        sav_VABPKB
036000100413     C   51              add       VABVLB        sav_VABVLB
036100100720     C   52              z-add     VABNCL        sav_VABNCL
036200100720     C   52              z-add     VABPKB        sav_VABPKB
036300100720     C   52              z-add     VABVLB        sav_VABVLB
036400100720     C   53              add       VABNCL        sav_VABNCL
036500100720     C   53              z-add     VABPKB        sav_VABPKB
036600100720     C   53              z-add     VABVLB        sav_VABVLB
036700080527     C                   else
036800080527     C  N31              eval      fivabds_ok = fivabds_sav
036900080527     C  N31              exsr      wrivab
037000080527     C  N31              eval      fivabds_sav = fivabds
037100080527     C                   eval      depspe = vabNSP
037200080527     C                   endif
037300080527     C                   endif
037400080527     C*
037500080527     C                   ENDSR
037600040119
037700040119
037800040119
037900040119     C*----------------------------------------------------*
038000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038100040119     C*----------------------------------------------------*
038200040510     C     WRIVAB        BEGSR
038300080513     C*
038400080513     C* Gestisco l'eventuale rottura x numero spedizione
038500080527     C                   if        VABNSP <> depspe or *in65 = *on
038600070301     C                   if        wVATNOT_E <> *blanks
038700130610     C                   eval      ok_VABCTM = '7Q'
038800070301     C                   endif
038900100413     C                   write     fivab000
039000080513     C                   endif
039100040119     C*
039200040119     C                   ENDSR
039300040119
039400040119
039500040119
039600040119     C*----------------------------------------------------*
039700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039800040119     C*----------------------------------------------------*
039900040510     C     WRIVAT        BEGSR
040000060512     C*
040100060512     C* Inizializzo i campi chiave
040200060512     C                   eval      VATFGS = VABFGS
040300060512     C                   eval      VATAAS = VABAAS
040400060512     C                   eval      VATCCM = VABCCM
040500070823     C                   eval      VATNRS = VABNRS
040600060512     C                   eval      VATNSP = VABNSP
040700060512     C                   eval      VATLNP = VABLNP
040800040119     C*
040900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041000040510     C                   if        VATTRC = *blanks
041100040510     C                   eval      VATTRC = 'E'
041200040510     C                   endif
041300110318     C                   if        VATNOT <> *blanks
041400040510     C                   write     FIVAT000
041500040119     C                   add       1             §CTROKVT
041600110318     C                   endif
041700060331     C*
041800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041900060331     C                   if        wVATNOT_A <> *blanks
042000060331     C                   eval      VATNOT = wVATNOT_A
042100060331     C                   eval      VATTRC = 'A'
042200060331     C                   write     FIVAT000
042300060331     C                   add       1             §CTROKVT
042400060331     C                   endif
042500060331     C*
042600060331     C                   if        wVATNOT_B <> *blanks
042700060331     C                   eval      VATNOT = wVATNOT_B
042800060331     C                   eval      VATTRC = 'B'
042900060331     C                   write     FIVAT000
043000060331     C                   add       1             §CTROKVT
043100060331     C                   endif
043200140711     C*
043300140711     C                   if        wVATNOT_IJ <> *blanks
043400140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043500140711     C                   eval      VATTRC = 'I'
043600140711     C                   write     FIVAT000
043700140711     C                   add       1             §CTROKVT
043800140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
043900140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044000140711     C                   eval      VATTRC = 'J'
044100140711     C                   write     FIVAT000
044200140711     C                   endif
044300140711     C                   endif
044400060331     C*
044500150316     C                   if        wVATNOT_E <> *blanks
044600060331     C                   eval      VATTRC = 'E'
044700110927     C*
044800150430     C* è un disk A
044900150430     C*
045000150316     C                   endif
045100150316     C*
045200110927     C                   if        wVATNOT_I <> *blanks
045300110927     C                   eval      VATNOT = wVATNOT_I
045400140711     C                   eval      VATTRC = 'I'
045500110927     C                   write     FIVAT000
045600110927     C                   add       1             §CTROKVT
045700110927     C                   endif
045800140220     C*
045900140220     C                   if        wVATNOT_J <> *blanks
046000140220     C                   eval      VATNOT = wVATNOT_J
046100140220     C                   eval      VATTRC = 'J'
046200140220     C                   write     FIVAT000
046300140220     C                   add       1             §CTROKVT
046400140220     C                   endif
046500140319     C*
046600140319     C                   if        wVATNOT_S <> *blanks
046700140319     C                   eval      VATNOT = wVATNOT_S
046800140319     C* i 2 flag devono essere blank
046900140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
047000140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047100140319     C                   eval      VATTRC = 'S'
047200140319     C                   write     FIVAT000
047300140319     C                   add       1             §CTROKVT
047400140319     C                   endif
047500040119     C*
047600040119     C                   ENDSR
047700040119
047800040119
047900040119
048000040119     C*----------------------------------------------------*
048100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048200040119     C*----------------------------------------------------*
048300040119     C     PREVAT        BEGSR
048400040119     C*
048500040119     C* Compongo il nome del membro da dare al FIVATWWR
048600040119     C                   eval      parmbr = vlrhdl
048700040119     C                   movel     'M'           parmbr
048800040510     C                   eval      parccm = vlrksc
048900040119     C                   eval      paropz = '1'
049000040119     C* Effettuo la chiamata al CLLE preposto
049100040119     C                   call(e)   'TITVVTC'
049200040119     C                   parm                    parccm
049300040119     C                   parm                    parmbr
049400040119     C                   parm                    paropz
049500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049600040119     C                   if        %error
049700040119     C                   movel     '1'           chkcall
049800040119     C                   else
049900040119     C                   movel     '0'           chkcall
050000040119     C                   endif
050100040119     C*
050200040119     C                   ENDSR
050300010601
050400010601
050500010601
050600010330     C*----------------------------------------------------*
050700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050800010330     C*----------------------------------------------------*
050900040119     C     INZVAR        BEGSR
051000080527     C*
051100080527     C                   setoff                                       65
051200010330     C*
051300030822     C* Inizializzo variabili di wrk
051400020204     C                   Z-ADD     *zeros        Num5_0            5 0
051500101018     C                   MOVEL     *blanks       wFlgCAS
051600060331     C                   MOVEL     *blanks       wVATNOT_A
051700060331     C                   MOVEL     *blanks       wVATNOT_B
051800060331     C                   MOVEL     *blanks       wVATNOT_E
051900110927     C                   MOVEL     *blanks       wVATNOT_I
052000140220     C                   MOVEL     *blanks       wVATNOT_J
052100140220     C                   MOVEL     *blanks       wVATNOT_IJ
052200140319     C                   MOVEL     *blanks       wVATNOT_S
052300120411     C                   MOVEL     *blanks       wNOT
052400120411     C                   MOVEL     *blanks       Data8A
052500030822     C*
052600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052700030822     C                   CLEAR                   SkSplitCSV
052800030822     C                   EVAL      i = 1
052900030822     C                   EVAL      posDa = *zeros
053000030822     C                   EVAL      posA  = *zeros
053100080527     C*
053200080527     C                   CLEAR                   FIVABDS
053300080527     C                   CLEAR                   FIVAT000
053400030822     C*
053500030822     C* Reimposto i valori di default
053600030822     C                   EXSR      DEFCAM
053700030822     C*
053800010330     C                   ENDSR
053900010330     C*----------------------------------------------------*
054000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054100010330     C*----------------------------------------------------*
054200010330     C     DEFCAM        BEGSR
054300030822     C*
054400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054500030715     C* e delimitatore testo.
054600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054900100318     C*
055000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055100100318     C                   EVAL      CharSOS = CharNUM
055200030822     C*
055300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055400100720     C                   SETOFF                                       505152
055500100720     C                   SETOFF                                       53
055600100413     C                   SELECT
055700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055800100720     C                   SETON                                        50
055900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056000100413     C                   SETON                                        51
056100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056200100720     C                   SETON                                        52
056300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056400100720     C                   SETON                                        53
056500100413     C                   ENDSL
056600030822     C*
056700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056800030822     C                   EVAL      posDaDft = 1
056900030822     C                   EVAL      posADft  = 0
057000030822     C                   EVAL      wGiroDft = 0
057100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057200030822     C                             posDaDft > 0
057300030822     C*
057400030822     C* Gestisco il 1° giro
057500030822     C                   IF        wGiroDft = 0
057600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057800030822     C* Incremento il contatore dei "giri"
057900030822     C                   EVAL      wGiroDft = 1
058000030822     C                   ELSE
058100030822     C                   EVAL      posDaDft = posADft
058200030822     C                   ENDIF
058300030822     C* Eseguo lo scan x trovare la fine del campo corrente
058400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058500030822     C*
058600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058800030822     C                   IF        posDaDft > 0 AND
058900030822     C                             posADft  > 0
059000030822     C* NCL
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'NCL'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABNCL
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CCM
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CCM'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABCCM
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* LNP
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'LNP'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABLNP
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* NRS
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'NRS'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABNRS
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* CTR
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'CTR'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiInt=*on
065200030822     C                   Z-ADD     PiVal         VABCTR
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* PKB
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'PKB'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiNum=*on
066500130610     C                   Z-ADD     PiVal         VABPKB
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* VLB
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'VLB'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiNum=*on
067800130610     C                   Z-ADD     PiVal         VABVLB
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* QFT
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'QFT'
068600030822     C                   EVAL      PiStr=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   EXSR      CHKNUM
069000030822     C                   IF        PiNum=*on
069100030822     C                   Z-ADD     PiVal         VABQFT
069200030822     C                   ENDIF
069300030822     C                   ENDIF
069400030822     C* CBO
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'CBO'
069900030822     C                   EVAL      VABCBO=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* TSP
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'TSP'
070800030822     C                   EVAL      VABTSP=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* VAS
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'VAS'
071700030822     C                   EVAL      VABVAS=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* VCA
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'VCA'
072600030822     C                   EVAL      VABVCA=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000070925     C* TIC
073100070925     C                   IF        %subst(
073200070925     C                             %subst(vlrppt:posDaDft+1:
073300070925     C                             posADft-posDaDft-1):1:3)
073400070925     C                             = 'TIC'
073500070925     C                   EVAL      VABTIC=%trim(%subst(
073600070925     C                             %subst(vlrppt:posDaDft+1:
073700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800070925     C                   ENDIF
073900030822     C* GCA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GCA'
074400030822     C                   EVAL      VABGCA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* CTM
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'CTM'
075300030822     C                   EVAL      VABCTM=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* FFD
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'FFD'
076200030822     C                   EVAL      VABFFD=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* VAD
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'VAD'
077100030822     C                   EVAL      VABVAD=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* GMA
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'GMA'
078000030822     C                   EVAL      VABGMA=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* GGA
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'GGA'
078900030822     C                   EVAL      VABGGA=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* GVA
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'GVA'
079800030822     C                   EVAL      VABGVA=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* TC1
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'TC1'
080700030822     C                   EVAL      VABTC1=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* TC2
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'TC2'
081600030822     C                   EVAL      VABTC2=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000040714     C* VATTRC
082100040714     C                   IF        %subst(
082200040714     C                             %subst(vlrppt:posDaDft+1:
082300040714     C                             posADft-posDaDft-1):1:3)
082400040714     C                             = 'TRC'
082500040714     C                   EVAL      VATTRC=%trim(%subst(
082600040714     C                             %subst(vlrppt:posDaDft+1:
082700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800040714     C                   ENDIF
082900030822     C* ...
083000030822     C                   ENDIF
083100030822     C                   ENDDO
083200020204     C*
083300010330     C                   ENDSR
083400010607     C*----------------------------------------------------*
083500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083600010607     C*----------------------------------------------------*
083700010607     C     IMPVAB        BEGSR
083800010607     C*
083900010607     C                   SETOFF                                       3132
084000110315 xxx C***                SETON                                        35
084100030515     C*
084200010607     C                   MOVEL     datcor        VABAAS
084300010607     C                   MOVE      datcor        VABMGS
084400010607     C*
084500030715     C**********
084600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084700030715     C**********
084800030715     C                   IF        wGiro = *zeros
084900040113     C*
085000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085300080429     C                             %trim(intcol(5))<>*blanks
085400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085600080429     C                                      %trim(intcol(5))
085700070823     C     ';':CharCSV   XLATE     vindta        vindta
085800040510     C                   ENDIF
085900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086000031201     C     minu:maiu     XLATE     vindta        vindta
086100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086200030715     C                   DOW       posDa <= %len(%trim(vindta))
086300030715     C*
086400030715     C* Gestisco il 1° campo
086500030715     C                   IF        i = 1
086600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086700030715     C                   EVAL      posDa = 1
086800030715     C* Eseguo lo scan x trovare la fine del primo campo
086900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C* X i campi successivi al 1°
087400030715     C                   ELSE
087500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087600030715     C                   EVAL      posDa = posA + 1
087700030715     C* Eseguo lo scan x trovare la fine del campo corrente
087800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087900030715     C* Gestisco l'ultimo campo
088000030715     C                   IF        posA = *zeros
088100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C                   LEAVE
088600030715     C                   ELSE
088700030715     C* A questo punto "estraggo" il campo corrente
088800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088900030715     C                                                    (posA-posDa))
089000030715     C                   ENDIF
089100030715     C                   ENDIF
089200030715     C* Incremento il contatore d campo
089300030715     C                   EVAL      i = i +1
089400030715     C                   ENDDO
089500030715     C*
089600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089700030715     C* delimitazione testo
089800030715     C                   EVAL      i = 1
089900030715     C                   DOW       i < %elem(SkSplitFLD)
090000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090200030715     C                   EVAL      i = i + 1
090300030715     C                   ENDDO
090400030715     C                   EVAL      wGiro = 1
090500030715     C                   ELSE
090600030715     C                   EVAL      wGiro = 2
090700031204     C**********
090800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090900031204     C* d separatore campo
091000031204     C**********
091100031204     C                   EVAL      vindta = %trim(vindta)
091200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091300031204     C                   EVAL      vindta = %subst(vindta:2)
091400031204     C                   ENDDO
091500031204     C*
091600031204     C                   Z-ADD     *zeros        lunghInput        4 0
091700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092100110912     C                   ENDDO
092200030715     C**********
092300030715     C* Effettuo lo split del campo dati d input
092400030715     C**********
092500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092600030715     C                   DOW       posDa <= %len(%trim(vindta))
092700030715     C*
092800030715     C* Gestisco il 1° campo
092900030715     C                   IF        i = 1
093000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093100030715     C                   EVAL      posDa = 1
093200030715     C* Eseguo lo scan x trovare la fine del primo campo
093300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C* X i campi successivi al 1°
093800030715     C                   ELSE
093900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094000030715     C                   EVAL      posDa = posA + 1
094100031204     C* Verifico che nn vi sia il campo nn valorizzato
094200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094300031204     C* Se campo nn valorizzato skippo al prossimo
094400031204     C                   EVAL      SkSplitCSV(i) = *blanks
094500031204     C                   EVAL      posA  = posA + 1
094600031204     C                   ELSE
094700030715     C* Eseguo lo scan x trovare la fine del campo corrente
094800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094900030715     C* Gestisco l'ultimo campo
095000030715     C                   IF        posA = *zeros
095100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095200030715     C* A questo punto "estraggo" il campo corrente
095300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095400030715     C                                                    (posA-posDa))
095500030715     C                   LEAVE
095600030715     C                   ELSE
095700030715     C* A questo punto "estraggo" il campo corrente
095800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095900030715     C                                                    (posA-posDa))
096000030715     C                   ENDIF
096100031204     C                   ENDIF
096200030715     C                   ENDIF
096300030715     C* Incremento il contatore d campo
096400030715     C                   EVAL      i = i +1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096800030715     C* delimitazione testo
096900030715     C                   EVAL      i = 1
097000030715     C                   DOW       i < %elem(SkSplitCSV)
097100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097300030715     C                   EVAL      i = i + 1
097400030715     C                   ENDDO
097500100720     C*
097600100720     C* Stacco un progressivo x numero spedizione
097700150430 xxx C                   EXSR      repNSP
097800100326     C*
097900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098000030715     C                   EVAL      i = 1
098100030715     C                   DOW       i < %elem(SkSplitFLD)
098200030715     C*
098300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098500030715     C                             SkSplitCSV(i) <> *blanks
098600030721     C*** ATB
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098800030721     C                   EVAL      VABATB = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030715     C*** RSD
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099500030715     C* ==
099600130528     C                   ENDIF
099700030721     C*** RD2
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030715     C*** IND
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100300030715     C                   EVAL      VABIND = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** LOD
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** PRD
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030721     C*** NZD
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030715     C*** RMA
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** NOT
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030715     C*** NT2
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** VCA
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** VAS
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** FFD
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030721     C*** GC1
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GC2
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TSP
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105200030721     C                   ENDIF
105300040510     C*** CBO
105400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105600040510     C                   ENDIF
105700030721     C*** NAS
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TIC
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GCA
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** XCO
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** CTM
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** TCR
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** CTS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** FTM
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** VAD
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GMA
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** GGA
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** GVA
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** TC1
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** TC2
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** SCL
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** RMO
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** NMO
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*
112600030721     C* Reperisco quindi i campi numerici...
112700040510     C*** NSP
112800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112900040510     C                   EVAL      PiStr=SkSplitCSV(i)
113000040510     C                   EXSR      CHKNUM
113100040510     C                   IF        PiInt=*on
113200040510     C                   Z-ADD     PiVal         VABNSP
113300040510     C                   ELSE
113400040510     C                   SETON                                        31
113500040510     C                   EVAL      VABNSP = *zeros
113600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113700040510     C                             + ' ' + 'VABNSP'
113800040510     C                   ENDIF
113900040510     C                   ENDIF
114000071106     C*** RMN
114100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114200071106     C                   EVAL      PiStr=SkSplitCSV(i)
114300071106     C                   EXSR      CHKNUM
114400071106     C                   IF        PiInt=*on
114500071106     C                   Z-ADD     PiVal         VABRMN
114600071106     C                   ELSE
114700071106     C                   SETON                                        32
114800071106     C                   EVAL      VABRMN = *zeros
114900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115000071106     C                             + ' ' + 'VABRMN'
115100071106     C                   ENDIF
115200071106     C                   ENDIF
115300030721     C*** CAD
115400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115500130610     C                   EVAL      PiStr=SkSplitCSV(i)
115600130610     C                   EXSR      CHKNUM
115700130610     C                   IF        PiInt=*on
115800130610     C                   Z-ADD     PiVal         Num5_0
115900130610     C                   MOVEL(p)  Num5_0        VABCAD
116000130610     C                   ELSE
116100130610     C                   SETON                                        32
116200130610     C                   EVAL      VABCAD = *zeros
116300130610     C                   EVAL      vinmsg = %trimr(vinmsg)
116400130610     C                             + ' ' + 'VABCAD'
116500130610     C                   ENDIF
116600130605     C                   ENDIF
116700030721     C*** NCL
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on
117200030721     C                   Z-ADD     PiVal         VABNCL
117300030721     C                   ELSE
117400030721     C                   SETON                                        32
117500030721     C                   EVAL      VABNCL = *zeros
117600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117700040122     C                             + ' ' + 'VABNCL'
117800030721     C                   ENDIF
117900030721     C                   ENDIF
118000030721     C*** PKB
118100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118200030721     C                   EVAL      PiStr=SkSplitCSV(i)
118300030721     C                   EXSR      CHKNUM
118400030721     C                   IF        PiNum=*on
118500130610     C                   Z-ADD(H)  PiVal         VABPKB
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      VABPKB = *zeros
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABPKB'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** CAS
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119600111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119700150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
119800130703     C                   EVAL      VABVCA = 'EUR'
119900150428     C                   EVAL      VABTIC = *blank
120000140513     C                   EVAL      wFlgCAS = '1'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiNum=*on
120400030721     C                   Z-ADD     PiVal         VABCAS
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCAS = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABCAS'
121000030721     C                   ENDIF
121100140513     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CCM
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on      AND
121800030721     C                             PiVal<=9999999 AND
121900030721     C                             PiVal>*zeros
122000030721     C                   Z-ADD     PiVal         VABCCM
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      VABCCM = *zeros
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500040510     C                             + ' ' + 'VABCCM'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** LNP
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on    AND
123300030721     C                             PiVal<=999   AND
123400030721     C                             PiVal>*zeros
123500030721     C                   Z-ADD     PiVal         VABLNP
123600030721     C                   ELSE
123700040715     C                   SETON                                        32
123800030721     C                   EVAL      VABLNP = *zeros
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000040510     C                             + ' ' + 'VABLNP'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** NRS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on AND
124800030721     C                             PiVal<=99
124900030721     C                   Z-ADD     PiVal         VABNRS
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABNRS'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** LNA
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on    AND
126100030721     C                             PiVal<=999   AND
126200030721     C                             PiVal>*zeros
126300030721     C                   Z-ADD     PiVal         VABLNA
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      VABLNA = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABLNA'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** CTR
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on  AND
127600030721     C                             PiVal<=999
127700030721     C                   Z-ADD     PiVal         VABCTR
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABCTR'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** IAS
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128600141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
128700141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
128800150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
128900141006     C                   EVAL      VABVAS = 'EUR'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiNum=*on
129300030721     C                   Z-ADD     PiVal         VABIAS
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABIAS'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000141003     C                   ENDIF
130100030721     C*** VLB
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiNum=*on
130600130610     C                   Z-ADD(H)  PiVal         VABVLB
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABVLB'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** QFT
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiNum=*on
131800030721     C                   Z-ADD     PiVal         VABQFT
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABQFT'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** NCD
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on      AND
133000030721     C                             PiVal<=9999999
133100030721     C                   Z-ADD     PiVal         VABNCD
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABNCD'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** NCA
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on      AND
134300030721     C                             PiVal<=9999999
134400030721     C                   Z-ADD     PiVal         VABNCA
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABNCA'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ZNC
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on AND
135600030721     C                             PiVal<=99
135700030721     C                   Z-ADD     PiVal         VABZNC
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABZNC'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** DCR
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on       AND
136900030721     C                             PiVal<=99999999
137000030721     C                   Z-ADD     PiVal         VABDCR
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137400030721     C                             + ' ' + 'VABDCR'
137500030721     C                   ENDIF
137600030721     C                   ENDIF
137700030721     C*** HCR
137800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137900030721     C                   EVAL      PiStr=SkSplitCSV(i)
138000030721     C                   EXSR      CHKNUM
138100030721     C                   IF        PiInt=*on   AND
138200030721     C                             PiVal<=9999
138300030721     C                   Z-ADD     PiVal         VABHCR
138400030721     C                   ELSE
138500030721     C                   SETON                                        32
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABHCR'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000030721     C*** VMD
139100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139200030721     C                   EVAL      PiStr=SkSplitCSV(i)
139300030721     C                   EXSR      CHKNUM
139400030721     C                   IF        PiNum=*on
139500030721     C                   Z-ADD     PiVal         VABVMD
139600030721     C                   ELSE
139700030721     C                   SETON                                        32
139800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139900030721     C                             + ' ' + 'VABVMD'
140000030721     C                   ENDIF
140100030721     C                   ENDIF
140200030721     C*** ANT
140300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140400030721     C                   EVAL      PiStr=SkSplitCSV(i)
140500030721     C                   EXSR      CHKNUM
140600030721     C                   IF        PiInt=*on        AND
140700030721     C                             PiVal<=999999999
140800030721     C                   Z-ADD     PiVal         VABANT
140900030721     C                   ELSE
141000030721     C                   SETON                                        32
141100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141200030721     C                             + ' ' + 'VABANT'
141300030721     C                   ENDIF
141400030721     C                   ENDIF
141500030721     C*** CMO
141600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141700030721     C                   EVAL      PiStr=SkSplitCSV(i)
141800030721     C                   EXSR      CHKNUM
141900030721     C                   IF        PiInt=*on
142000030721     C                   Z-ADD     PiVal         Num5_0
142100030721     C                   MOVEL(p)  Num5_0        VABCMO
142200030721     C                   ELSE
142300030721     C                   SETON                                        32
142400030721     C                   EVAL      VABCMO = *zeros
142500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142600030721     C                             + ' ' + 'VABCMO'
142700030721     C                   ENDIF
142800030721     C                   ENDIF
142900040510     C*
143000040510     C* Reperisco i campi del FIVAT
143100040510     C*** VATNOT
143200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143400040510     C                   ENDIF
143500040510     C*** VATTRC
143600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143800040510     C                   ENDIF
143900060331     C*** VATNOT_A
144000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300060331     C*** VATNOT_B
144400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144600060331     C                   ENDIF
144700060331     C*** VATNOT_E
144800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145000060331     C                   ENDIF
145100110927     C*** VATNOT_I
145200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145400110927     C                   ENDIF
145500141003     C*** VATNOT_IJ
145600141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
145700141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
145800141003     C                   ENDIF
145900140319     C*** VATNOT_S
146000140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
146100140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
146200140319     C                   ENDIF
146300070823     C*
146400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146500150309     C*
146600150428
146700150428     C*** RSD/RD2
146800150428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
146900150428     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
147000150428     C                   EVAL      VABRSD = %subst(wNOT:1:35)
147100150428     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
147200150428     C     '@':'A'       XLATE     VABRSD        VABRSD
147300150428     C                   EVAL      VABRD2 = %subst(wNOT:36:35)
147400150428     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
147500150428     C     '@':'A'       XLATE     VABRD2        VABRD2
147600150428     C                   ENDIF
147700150428
147800150430     C*** RMN/RMA
147900150430     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
148000150430     C                   EVAL      PiStr=SkSplitCSV(i)
148100150430     C                   EXSR      CHKNUM
148200150430     C                   IF        PiInt=*on
148300150430     C                   Z-ADD     PiVal         VABRMN
148400150430     C                   EVAL      VABRMA = %char(VABRMN)
148500150430     C                   ELSE
148600150430     C                   SETON                                        32
148700150430     C                   EVAL      VABRMN = *zeros
148800150430     C                   EVAL      vinmsg = %trimr(vinmsg)
148900150430     C                             + ' ' + 'VABRMN VABRMA'
149000150430     C                   ENDIF
149100150430     C                   ENDIF
149200150430
149300150428     C*** VABNOT
149400150428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
149500150428     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
149600150428     C                   EVAL      VABNOT = %subst(wNOT:1:35)
149700150428     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
149800150428     C                   ENDIF
149900150316
150000141003     C*
150100071029     C***  <===  -----------------------------------------
150200040510     C*
150300030715     C                   ENDIF
150400030715     C                   EVAL      i = i + 1
150500030715     C                   ENDDO
150600070925     C*
150700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150800140319     C*
150900141014     C* se la nazione è IT la sblanco
151000141112     C                   IF        VABNZD = 'IT' or
151100141112     C                             VABNZD = 'ITA'
151200141014     C                   EVAL      VABNZD = *blank
151300141014     C                   ENDIF
151400070925     C*
151500070925     C* Considerazioni finali su CBO/CAS
151600140220     C                   IF        wFlgCAS = '1'    OR
151700140220     C                             VABCAS  > *zeros
151800140220     C                   IF        VABCBO = '1'
151900140220     C                   EVAL      VABCBO = '4'
152000140220     C                   ENDIF
152100140220     C                   IF        VABCBO = '2'
152200140220     C                   EVAL      VABCBO = '6'
152300140220     C                   ENDIF
152400140220     C                   ENDIF
152500070925     C***  <===  ----------------------------
152600040510     C*
152700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
152800040510     C                   IF        VABNSP = *zeros
152900040510     C                   SETON                                        31
153000040510     C                   ENDIF
153100040715     C*
153200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153300040715     C                   IF        VABLNP = *zeros
153400040715     C                   SETON                                        31
153500040715     C                   ENDIF
153600040510     C*
153700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
153800120411     C                   IF        VABRMN = *zeros
153900120411     C                   Z-ADD     VABNSP        VABRMN
154000120411     C                   ENDIF
154100120328     C* IN QUESTO CASO LO FORZO = 1
154200120411     C***                IF        VABRMN = *zeros
154300120411     C***                Z-ADD     1             VABRMN
154400120411     C***                ENDIF
154500040510     C*
154600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
154700040510     C                   IF        VABRMA = *blanks
154800040714     C                   MOVEL     VABRMN        VABRMA
154900040510     C                   ENDIF
155000040128     C*
155100040510     C* Se provincia nn valorizzata la reperisco
155200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155300110912     C                   IF        VABPRD =  *blanks AND
155400110912     C                             VABNZD =  *blanks AND
155500110912     C                             VABCAD <> *blanks AND
155600110912     C                             VABLOD <> *blanks
155700040128     C                   CLEAR                   TISI95DS
155800040128     C                   EVAL      I95TCN = '3'
155900040128     C                   Z-ADD     datcor        I95DAT
156000040128     C                   EVAL      I95NAR = VABNZD
156100040128     C                   EVAL      I95CAP = VABCAD
156200040128     C                   EVAL      I95LOC = VABLOD
156300040128     C                   CALL      'TISI95R'
156400040128     C                   PARM                    TISI95DS
156500040128     C                   EVAL      VABPRD = O95PRV
156600040510     C                   ENDIF
156700020204     C*
156800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
156900020204     C                   EXSR      CHKIMPDIV
157000030715     C                   ENDIF
157100020204     C*
157200010607     C                   ENDSR
157300010607     C*----------------------------------------------------*
157400010601
157500020204
157600020204     C*----------------------------------------------------*
157700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157800020204     C*----------------------------------------------------*
157900020204     C     CHKIMPDIV     BEGSR
158000020204     C*
158100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158200020204     C                   Z-ADD     *zeros        wrkDec            9 9
158300020204     C*
158400020204     C* Come prima cosa effettuo considerazioni sulla divisa
158500020204     C                   IF        vabIAS > *zeros
158600020204     C                   IF        vabVAS <> 'EUR'
158700020204     C                   EVAL      vabVAS =  'ITL'
158800020204     C                   ENDIF
158900020204     C                   ENDIF
159000020204     C*
159100020204     C                   IF        vabCAS > *zeros
159200020204     C                   IF        vabVCA <> 'EUR'
159300020204     C                   EVAL      vabVCA =  'ITL'
159400020204     C                   ENDIF
159500020204     C                   ENDIF
159600020204     C*
159700020204     C                   IF        vabVMD > *zeros
159800020204     C                   IF        vabVAD <> 'EUR'
159900020204     C                   EVAL      vabVAD =  'ITL'
160000020204     C                   ENDIF
160100020204     C                   ENDIF
160200020204     C*
160300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160400020204     C                   Z-ADD     vabIAS        wrkDec
160500020204     C                   IF        wrkDec > *zeros
160600020204     C                   IF        vabVAS = 'ITL'
160700020204     C                   EVAL      vabIAS = *zeros
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161200020204     C                   Z-ADD     vabCAS        wrkDec
161300020204     C                   IF        wrkDec > *zeros
161400020204     C                   IF        vabVCA = 'ITL'
161500020204     C                   EVAL      vabCAS = *zeros
161600020204     C                   ENDIF
161700020204     C                   ENDIF
161800020204     C*
161900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162000020204     C                   Z-ADD     vabVMD        wrkDec
162100020204     C                   IF        wrkDec > *zeros
162200020204     C                   IF        vabVAD = 'ITL'
162300020204     C                   EVAL      vabVMD = *zeros
162400020204     C                   ENDIF
162500020204     C                   ENDIF
162600020204     C*
162700020204     C                   ENDSR
162800020204     C***
162900010330
163000100318
163100100318
163200100318     C*----------------------------------------------------*
163300100318     C*  CONTROLLO NUMERICITA' CAMPI
163400100318     C*----------------------------------------------------*
163500100318     C     CHKNUM        BEGSR
163600100318     C*
163700100318     C                   IF        PiDecChr = *blanks
163800100318     C                   EVAL      PiDecChr = CharNUM
163900100318     C                   ENDIF
164000100318     C*
164100100318     C                   callp     UBISNUM_Check(PiStr
164200100318     C                                          :PiDecChr
164300100318     C                                          :PiVal
164400100318     C                                          :PiNum
164500100318     C                                          :PiInt)
164600100318     C*
164700100318     C                   ENDSR
164800100318     C***
164900010601
165000010601
165100010601
165200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165300141006      ***
165400141014     C     repfil        BEGSR
165500141014     C*
165600141014     C                   if        invfil = *zeros and
165700141014     C                             depfil > *zeros and
165800141014     C                             (vinflg = *blanks or
165900141014     C                              vinflg = *zeros)
166000141014     C*
166100141014     C                   eval      invfil = depfil
166200141014     C                   endif
166300141014     C*
166400141014     C                   if        depfil <> invfil and
166500141014     C                             invfil > *zeros
166600141014     C                   eval      flgMulti = '1'
166700141014     C                   if        vinflg = *blanks
166800141014     C                   add       1             cntNonEl
166900141014     C                   endif
167000141014     C                   endif
167100141014     C*
167200141014     C                   if        vinflg = '2'
167300141014     C                   eval      flgStato = '2'
167400141014     C                   endif
167500141014     C*
167600141014     C                   ENDSR
167700141014     C*
167800010601
167900010601
168000010601
168100010330
168200040119      /TITLE Invio dei dati al punto operativo.
168300040119     C     invio         BEGSR
168400040119     C*
168500040119     C* 1° invio FIVAT
168600040119     C                   reset                   dscmz
168700040119     C                   move      vlrpoi        cmzdst
168800040119     C                   eval      cmzfld = 'FIVATWWR'
168900040119     C                   eval      cmzmbd = vlrhdl
169000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169100040119     C***                if        prmfir = *blanks
169200040119     C                   eval      cmzfla = 'FIVAT00F'
169300040119     C                   eval      cmzmba = 'FIVAT00F'
169400040119     C***                else
169500040119     C***                eval      cmzfla = prmfir
169600040119     C***                eval      cmzmba = prmfir
169700040119     C***                endif
169800040119     C                   eval      cmznrr = *zeros
169900040119     C                   move      §ctrokvt      cmznrr
170000040119     C                   eval      cmzlba = vlrfl1
170100040119     C                   call(e)   'TIS711C'
170200040119     C                   parm                    dscmz
170300040119     C                   parm      *blanks       esito
170400040119     C                   if        %error
170500040119     C                             or cmzerr = '1'
170600040119     C                             or esito  = '1'
170700040119     C                   eval      wrkesito = '3'
170800040119     C                   else
170900040119     C*
171000040119     C* 2° invio FIVAB
171100040119     C                   reset                   dscmz
171200040119     C                   move      vlrpoi        cmzdst
171300040119     C                   eval      cmzfld = vlrfou
171400040119     C                   eval      cmzmbd = vlrhdl
171500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171600040119     C***                if        prmfir = *blanks
171700040119     C                   eval      cmzfla = 'FIVAB00F'
171800040119     C                   eval      cmzmba = 'FIVAB00F'
171900040119     C***                else
172000040119     C***                eval      cmzfla = prmfir
172100040119     C***                eval      cmzmba = prmfir
172200040119     C***                endif
172300040119     C                   eval      cmznrr = *zeros
172400040119     C                   move      §ctrokvb      cmznrr
172500040119     C                   eval      cmzlba = vlrfl1
172600040119     C                   call(e)   'TIS711C'
172700040119     C                   parm                    dscmz
172800040119     C                   parm      *blanks       esito
172900040119     C                   if        %error
173000040119     C                             or cmzerr = '1'
173100040119     C                             or esito  = '1'
173200040119     C                   eval      wrkesito = '3'
173300040119     C                   endif
173400040119     C                   endif
173500040119     C*
173600040119     C                   ENDSR
173700040119     C***
173800010601
173900010601
174000010601
174100010601
174200010601      /TITLE Invio dei dati al punto operativo.
174300010601     C     opeini        BEGSR
174400010601     C*
174500010601     C* Inizializzo flag e contatori operativi
174600010601     C                   movel     '0'           flgGiro           1
174700010601     C                   movel     '0'           flgMulti          1
174800010601     C                   movel     '1'           flgStato          1
174900010615     C                   movel     '0'           flgOk             1
175000010601     C                   z-add     *zeros        cntNonEl         10 0
175100010601     C                   z-add     *zeros        depfil            3 0
175200010601     C                   z-add     *zeros        invfil            3 0
175300010601     C*
175400010601     C                   ENDSR
175500010601     C***
175600070326
175700070326
175800070326
175900070326
176000070326     C     *pssr         BEGSR
176100070329     C*
176200070329     C                   if        %open(tivin00r)
176300070329     C                   close     tivin00r
176400070329     C                   endif
176500070329     C                   if        %open(fivabwwr)
176600070329     C                   close     fivabwwr
176700070329     C                   endif
176800070329     C                   if        %open(fivatwwr)
176900070329     C                   close     fivatwwr
177000070329     C                   endif
177100070326     C*
177200070326     C* Effettuo la chiamata al CLLE preposto
177300070326     C                   call(e)   'TITVVTC'
177400070326     C                   parm                    parccm
177500070326     C                   parm                    parmbr
177600070326     C                   parm      '2'           paropz
177700070326     C*
177800070326     C                   eval      wrkesito = '2'
177900070404     C*
178000070404     C                   seton                                        LR
178100070326     C*
178200070326     C                   ENDSR     '*CANCL'
178300070326     C***
178400100318
178500100318
178600100318
178700100318     C     normCSV       BEGSR
178800100318     C*
178900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179000100318     C* separatori campi
179100100318     C                   if        CharTXT <> *blanks
179200100318     C                   z-add     *zeros        wLenVINDTA        4 0
179300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179400100318     C                   setoff                                       80
179500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
179700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179800100318     C                   if        not *in80
179900100318     C                   seton                                        80
180000100318     C                   else
180100100318     C                   setoff                                       80
180200100318     C                   endif
180300100318     C                   endif
180400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180500100318     C                             and *in80 = *on
180600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180700100318     C                   endif
180800100318     C                   enddo
180900100318     C                   endif
181000100318     C*
181100100318     C                   ENDSR
181200100318     C***
181300070326
181400100326
181500100326
181600100326     C*----------------------------------------------------*
181700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
181800100326     C*----------------------------------------------------*
181900100326     C     REPNSP        BEGSR
182000100326     C*
182100100326     C* NSP => Stacco un numeratore da AZNUM
182200100326     C                   clear                   TRUL33DS
182300100326     C                   eval      I33OPE = *zeros
182400100326     C                   eval      I33CNU = 302
182500100326     C                   eval      I33NUM = 1
182600100326     C                   movel     TRUL33DS      KPJBU
182700100326     C                   call      'TRUL33R'
182800100326     C                   parm                    KPJBA
182900100326     C                   movel     KPJBU         TRUL33DS
183000100326     C                   if        O33ERR = *zeros
183100100326     C                   z-add     O33NRF        VABNSP
183200100326     C                   z-add     O33NRF        VATNSP
183300100326     C                   else
183400100326     C                   SETON                                        31
183500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183600140711     C                             + ' ' + 'VABNSP VATNSP'
183700100326     C                   endif
183800100326     C*
183900100326     C                   ENDSR
184000010330
184100100326
184200010330
184300000613     C     *inzsr        BEGSR
184400990910     C*
184500990910     C     *entry        plist
184600990920     C                   parm                    tivlrds
184700990921     C                   parm      wrkesito      esito
184800000724     C                   parm                    prmlit
184900000710     C                   parm                    prmfir
185000010330     C*
185100010330     C* CALCOLA LA DATA CORRENTE
185200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185300100318     C                   eval      datcor = %dec(%date() : *ISO)
185400000613     C*
185500000613     C                   ENDSR
185600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185700150430X;X;W_VABRMN;W_VABRSD;X;X;VABNCL;VABPKB;VABVLB;X;X;VABPRD;VABIND;VABCAD;VATNOT_B;
185800150430X;X;VABLOD;VABCAS;X;W_VABNOT;
185900150428
186000150428
186100150428
