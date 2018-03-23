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
008000140711     D wRMA            s                   like(VABRMA)
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
044700140527     C*
044800150316     C* i "CHI SONO" sono in orizzontale e al massimo 20
044900140527     C*
045000150316     C                   FOR       wX = 1 to 20
045100150316     C                   IF        NOT_E(wX) <> *blank
045200150316     C                   eval      VATNOT = NOT_E(wX)
045300150114     C                   write     FIVAT000
045400150114     C                   add       1             §CTROKVT
045500120320     C                   endif
045600150316     C                   ENDFOR
045700110927     C*
045800150316     C                   endif
045900150316     C*
046000110927     C                   if        wVATNOT_I <> *blanks
046100110927     C                   eval      VATNOT = wVATNOT_I
046200140711     C                   eval      VATTRC = 'I'
046300110927     C                   write     FIVAT000
046400110927     C                   add       1             §CTROKVT
046500110927     C                   endif
046600140220     C*
046700140220     C                   if        wVATNOT_J <> *blanks
046800140220     C                   eval      VATNOT = wVATNOT_J
046900140220     C                   eval      VATTRC = 'J'
047000140220     C                   write     FIVAT000
047100140220     C                   add       1             §CTROKVT
047200140220     C                   endif
047300140319     C*
047400140319     C                   if        wVATNOT_S <> *blanks
047500140319     C                   eval      VATNOT = wVATNOT_S
047600140319     C* i 2 flag devono essere blank
047700140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
047800140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047900140319     C                   eval      VATTRC = 'S'
048000140319     C                   write     FIVAT000
048100140319     C                   add       1             §CTROKVT
048200140319     C                   endif
048300040119     C*
048400040119     C                   ENDSR
048500040119
048600040119
048700040119
048800040119     C*----------------------------------------------------*
048900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049000040119     C*----------------------------------------------------*
049100040119     C     PREVAT        BEGSR
049200040119     C*
049300040119     C* Compongo il nome del membro da dare al FIVATWWR
049400040119     C                   eval      parmbr = vlrhdl
049500040119     C                   movel     'M'           parmbr
049600040510     C                   eval      parccm = vlrksc
049700040119     C                   eval      paropz = '1'
049800040119     C* Effettuo la chiamata al CLLE preposto
049900040119     C                   call(e)   'TITVVTC'
050000040119     C                   parm                    parccm
050100040119     C                   parm                    parmbr
050200040119     C                   parm                    paropz
050300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050400040119     C                   if        %error
050500040119     C                   movel     '1'           chkcall
050600040119     C                   else
050700040119     C                   movel     '0'           chkcall
050800040119     C                   endif
050900040119     C*
051000040119     C                   ENDSR
051100010601
051200010601
051300010601
051400010330     C*----------------------------------------------------*
051500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051600010330     C*----------------------------------------------------*
051700040119     C     INZVAR        BEGSR
051800080527     C*
051900080527     C                   setoff                                       65
052000010330     C*
052100030822     C* Inizializzo variabili di wrk
052200020204     C                   Z-ADD     *zeros        Num5_0            5 0
052300101018     C                   MOVEL     *blanks       wFlgCAS
052400060331     C                   MOVEL     *blanks       wVATNOT_A
052500060331     C                   MOVEL     *blanks       wVATNOT_B
052600060331     C                   MOVEL     *blanks       wVATNOT_E
052700110927     C                   MOVEL     *blanks       wVATNOT_I
052800140220     C                   MOVEL     *blanks       wVATNOT_J
052900140220     C                   MOVEL     *blanks       wVATNOT_IJ
053000140319     C                   MOVEL     *blanks       wVATNOT_S
053100120411     C                   MOVEL     *blanks       wNOT
053200120411     C                   MOVEL     *blanks       Data8A
053300030822     C*
053400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053500030822     C                   CLEAR                   SkSplitCSV
053600030822     C                   EVAL      i = 1
053700030822     C                   EVAL      posDa = *zeros
053800030822     C                   EVAL      posA  = *zeros
053900080527     C*
054000080527     C                   CLEAR                   FIVABDS
054100080527     C                   CLEAR                   FIVAT000
054200030822     C*
054300030822     C* Reimposto i valori di default
054400030822     C                   EXSR      DEFCAM
054500030822     C*
054600010330     C                   ENDSR
054700010330     C*----------------------------------------------------*
054800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054900010330     C*----------------------------------------------------*
055000010330     C     DEFCAM        BEGSR
055100030822     C*
055200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055300030715     C* e delimitatore testo.
055400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055700100318     C*
055800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055900100318     C                   EVAL      CharSOS = CharNUM
056000030822     C*
056100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056200100720     C                   SETOFF                                       505152
056300100720     C                   SETOFF                                       53
056400100413     C                   SELECT
056500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056600100720     C                   SETON                                        50
056700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056800100413     C                   SETON                                        51
056900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057000100720     C                   SETON                                        52
057100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057200100720     C                   SETON                                        53
057300100413     C                   ENDSL
057400030822     C*
057500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057600030822     C                   EVAL      posDaDft = 1
057700030822     C                   EVAL      posADft  = 0
057800030822     C                   EVAL      wGiroDft = 0
057900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058000030822     C                             posDaDft > 0
058100030822     C*
058200030822     C* Gestisco il 1° giro
058300030822     C                   IF        wGiroDft = 0
058400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058600030822     C* Incremento il contatore dei "giri"
058700030822     C                   EVAL      wGiroDft = 1
058800030822     C                   ELSE
058900030822     C                   EVAL      posDaDft = posADft
059000030822     C                   ENDIF
059100030822     C* Eseguo lo scan x trovare la fine del campo corrente
059200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059300030822     C*
059400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059600030822     C                   IF        posDaDft > 0 AND
059700030822     C                             posADft  > 0
059800030822     C* NCL
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'NCL'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABNCL
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* CCM
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'CCM'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABCCM
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* LNP
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'LNP'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABLNP
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* NRS
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'NRS'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiInt=*on
064700030822     C                   Z-ADD     PiVal         VABNRS
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* CTR
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'CTR'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiInt=*on
066000030822     C                   Z-ADD     PiVal         VABCTR
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300030822     C* PKB
066400030822     C                   IF        %subst(
066500030822     C                             %subst(vlrppt:posDaDft+1:
066600030822     C                             posADft-posDaDft-1):1:3)
066700030822     C                             = 'PKB'
066800030822     C                   EVAL      PiStr=%trim(%subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100030822     C                   EXSR      CHKNUM
067200030822     C                   IF        PiNum=*on
067300130610     C                   Z-ADD     PiVal         VABPKB
067400030822     C                   ENDIF
067500030822     C                   ENDIF
067600030822     C* VLB
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VLB'
068100030822     C                   EVAL      PiStr=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   EXSR      CHKNUM
068500030822     C                   IF        PiNum=*on
068600130610     C                   Z-ADD     PiVal         VABVLB
068700030822     C                   ENDIF
068800030822     C                   ENDIF
068900030822     C* QFT
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'QFT'
069400030822     C                   EVAL      PiStr=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   EXSR      CHKNUM
069800030822     C                   IF        PiNum=*on
069900030822     C                   Z-ADD     PiVal         VABQFT
070000030822     C                   ENDIF
070100030822     C                   ENDIF
070200030822     C* CBO
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'CBO'
070700030822     C                   EVAL      VABCBO=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TSP
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TSP'
071600030822     C                   EVAL      VABTSP=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VAS
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VAS'
072500030822     C                   EVAL      VABVAS=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* VCA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'VCA'
073400030822     C                   EVAL      VABVCA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800070925     C* TIC
073900070925     C                   IF        %subst(
074000070925     C                             %subst(vlrppt:posDaDft+1:
074100070925     C                             posADft-posDaDft-1):1:3)
074200070925     C                             = 'TIC'
074300070925     C                   EVAL      VABTIC=%trim(%subst(
074400070925     C                             %subst(vlrppt:posDaDft+1:
074500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600070925     C                   ENDIF
074700030822     C* GCA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GCA'
075200030822     C                   EVAL      VABGCA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* CTM
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'CTM'
076100030822     C                   EVAL      VABCTM=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* FFD
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'FFD'
077000030822     C                   EVAL      VABFFD=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* VAD
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'VAD'
077900030822     C                   EVAL      VABVAD=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GMA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GMA'
078800030822     C                   EVAL      VABGMA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* GGA
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'GGA'
079700030822     C                   EVAL      VABGGA=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* GVA
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'GVA'
080600030822     C                   EVAL      VABGVA=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* TC1
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'TC1'
081500030822     C                   EVAL      VABTC1=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* TC2
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'TC2'
082400030822     C                   EVAL      VABTC2=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800040714     C* VATTRC
082900040714     C                   IF        %subst(
083000040714     C                             %subst(vlrppt:posDaDft+1:
083100040714     C                             posADft-posDaDft-1):1:3)
083200040714     C                             = 'TRC'
083300040714     C                   EVAL      VATTRC=%trim(%subst(
083400040714     C                             %subst(vlrppt:posDaDft+1:
083500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600040714     C                   ENDIF
083700030822     C* ...
083800030822     C                   ENDIF
083900030822     C                   ENDDO
084000020204     C*
084100010330     C                   ENDSR
084200010607     C*----------------------------------------------------*
084300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084400010607     C*----------------------------------------------------*
084500010607     C     IMPVAB        BEGSR
084600010607     C*
084700010607     C                   SETOFF                                       3132
084800110315 xxx C***                SETON                                        35
084900030515     C*
085000010607     C                   MOVEL     datcor        VABAAS
085100010607     C                   MOVE      datcor        VABMGS
085200010607     C*
085300030715     C**********
085400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085500030715     C**********
085600030715     C                   IF        wGiro = *zeros
085700040113     C*
085800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086100080429     C                             %trim(intcol(5))<>*blanks
086200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086400080429     C                                      %trim(intcol(5))
086500070823     C     ';':CharCSV   XLATE     vindta        vindta
086600040510     C                   ENDIF
086700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086800031201     C     minu:maiu     XLATE     vindta        vindta
086900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087000030715     C                   DOW       posDa <= %len(%trim(vindta))
087100030715     C*
087200030715     C* Gestisco il 1° campo
087300030715     C                   IF        i = 1
087400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087500030715     C                   EVAL      posDa = 1
087600030715     C* Eseguo lo scan x trovare la fine del primo campo
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C* X i campi successivi al 1°
088200030715     C                   ELSE
088300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088400030715     C                   EVAL      posDa = posA + 1
088500030715     C* Eseguo lo scan x trovare la fine del campo corrente
088600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088700030715     C* Gestisco l'ultimo campo
088800030715     C                   IF        posA = *zeros
088900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C                   LEAVE
089400030715     C                   ELSE
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C                   ENDIF
089900030715     C                   ENDIF
090000030715     C* Incremento il contatore d campo
090100030715     C                   EVAL      i = i +1
090200030715     C                   ENDDO
090300030715     C*
090400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090500030715     C* delimitazione testo
090600030715     C                   EVAL      i = 1
090700030715     C                   DOW       i < %elem(SkSplitFLD)
090800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091000030715     C                   EVAL      i = i + 1
091100030715     C                   ENDDO
091200030715     C                   EVAL      wGiro = 1
091300030715     C                   ELSE
091400030715     C                   EVAL      wGiro = 2
091500031204     C**********
091600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091700031204     C* d separatore campo
091800031204     C**********
091900031204     C                   EVAL      vindta = %trim(vindta)
092000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092100031204     C                   EVAL      vindta = %subst(vindta:2)
092200031204     C                   ENDDO
092300031204     C*
092400031204     C                   Z-ADD     *zeros        lunghInput        4 0
092500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092900110912     C                   ENDDO
093000030715     C**********
093100030715     C* Effettuo lo split del campo dati d input
093200030715     C**********
093300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093400030715     C                   DOW       posDa <= %len(%trim(vindta))
093500030715     C*
093600030715     C* Gestisco il 1° campo
093700030715     C                   IF        i = 1
093800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093900030715     C                   EVAL      posDa = 1
094000030715     C* Eseguo lo scan x trovare la fine del primo campo
094100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094200030715     C* A questo punto "estraggo" il campo corrente
094300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094400030715     C                                                    (posA-posDa))
094500030715     C* X i campi successivi al 1°
094600030715     C                   ELSE
094700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094800030715     C                   EVAL      posDa = posA + 1
094900031204     C* Verifico che nn vi sia il campo nn valorizzato
095000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095100031204     C* Se campo nn valorizzato skippo al prossimo
095200031204     C                   EVAL      SkSplitCSV(i) = *blanks
095300031204     C                   EVAL      posA  = posA + 1
095400031204     C                   ELSE
095500030715     C* Eseguo lo scan x trovare la fine del campo corrente
095600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095700030715     C* Gestisco l'ultimo campo
095800030715     C                   IF        posA = *zeros
095900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C                   LEAVE
096400030715     C                   ELSE
096500030715     C* A questo punto "estraggo" il campo corrente
096600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096700030715     C                                                    (posA-posDa))
096800030715     C                   ENDIF
096900031204     C                   ENDIF
097000030715     C                   ENDIF
097100030715     C* Incremento il contatore d campo
097200030715     C                   EVAL      i = i +1
097300030715     C                   ENDDO
097400030715     C*
097500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097600030715     C* delimitazione testo
097700030715     C                   EVAL      i = 1
097800030715     C                   DOW       i < %elem(SkSplitCSV)
097900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098100030715     C                   EVAL      i = i + 1
098200030715     C                   ENDDO
098300100720     C*
098400100720     C* Stacco un progressivo x numero spedizione
098500150428 xxx C***                EXSR      repNSP
098600100326     C*
098700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098800030715     C                   EVAL      i = 1
098900030715     C                   DOW       i < %elem(SkSplitFLD)
099000030715     C*
099100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099300030715     C                             SkSplitCSV(i) <> *blanks
099400030721     C*** ATB
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099600030721     C                   EVAL      VABATB = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030715     C*** RSD
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100300030715     C* ==
100400130528     C                   ENDIF
100500030721     C*** RD2
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030715     C*** IND
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101100030715     C                   EVAL      VABIND = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** LOD
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** PRD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030721     C*** NZD
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** RMA
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** NOT
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** NT2
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** VCA
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** VAS
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** FFD
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030721     C*** GC1
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GC2
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** TSP
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106000030721     C                   ENDIF
106100040510     C*** CBO
106200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106400040510     C                   ENDIF
106500030721     C*** NAS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TIC
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GCA
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** XCO
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** CTM
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TCR
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** CTS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** FTM
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** VAD
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** GMA
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** GGA
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** GVA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** TC1
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TC2
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** SCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** RMO
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** NMO
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*
113400030721     C* Reperisco quindi i campi numerici...
113500040510     C*** NSP
113600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113700040510     C                   EVAL      PiStr=SkSplitCSV(i)
113800040510     C                   EXSR      CHKNUM
113900040510     C                   IF        PiInt=*on
114000040510     C                   Z-ADD     PiVal         VABNSP
114100040510     C                   ELSE
114200040510     C                   SETON                                        31
114300040510     C                   EVAL      VABNSP = *zeros
114400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040510     C                             + ' ' + 'VABNSP'
114600040510     C                   ENDIF
114700040510     C                   ENDIF
114800071106     C*** RMN
114900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115000071106     C                   EVAL      PiStr=SkSplitCSV(i)
115100071106     C                   EXSR      CHKNUM
115200071106     C                   IF        PiInt=*on
115300071106     C                   Z-ADD     PiVal         VABRMN
115400071106     C                   ELSE
115500071106     C                   SETON                                        32
115600071106     C                   EVAL      VABRMN = *zeros
115700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115800071106     C                             + ' ' + 'VABRMN'
115900071106     C                   ENDIF
116000071106     C                   ENDIF
116100030721     C*** CAD
116200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116300130610     C                   EVAL      PiStr=SkSplitCSV(i)
116400130610     C                   EXSR      CHKNUM
116500130610     C                   IF        PiInt=*on
116600130610     C                   Z-ADD     PiVal         Num5_0
116700130610     C                   MOVEL(p)  Num5_0        VABCAD
116800130610     C                   ELSE
116900130610     C                   SETON                                        32
117000130610     C                   EVAL      VABCAD = *zeros
117100130610     C                   EVAL      vinmsg = %trimr(vinmsg)
117200130610     C                             + ' ' + 'VABCAD'
117300130610     C                   ENDIF
117400130605     C                   ENDIF
117500030721     C*** NCL
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on
118000030721     C                   Z-ADD     PiVal         VABNCL
118100030721     C                   ELSE
118200030721     C                   SETON                                        32
118300030721     C                   EVAL      VABNCL = *zeros
118400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118500040122     C                             + ' ' + 'VABNCL'
118600030721     C                   ENDIF
118700030721     C                   ENDIF
118800030721     C*** PKB
118900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119000030721     C                   EVAL      PiStr=SkSplitCSV(i)
119100030721     C                   EXSR      CHKNUM
119200030721     C                   IF        PiNum=*on
119300130610     C                   Z-ADD(H)  PiVal         VABPKB
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABPKB = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800030721     C                             + ' ' + 'VABPKB'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** CAS
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120500150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
120600130703     C                   EVAL      VABVCA = 'EUR'
120700150507     C***                EVAL      VABTIC = *blank
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
129600150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
129700141006     C                   EVAL      VABVAS = 'EUR'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABIAS
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABIAS'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800141003     C                   ENDIF
130900030721     C*** VLB
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400130610     C                   Z-ADD(H)  PiVal         VABVLB
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABVLB'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** QFT
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiNum=*on
132600030721     C                   Z-ADD     PiVal         VABQFT
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABQFT'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** NCD
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on      AND
133800030721     C                             PiVal<=9999999
133900030721     C                   Z-ADD     PiVal         VABNCD
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABNCD'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** NCA
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on      AND
135100030721     C                             PiVal<=9999999
135200030721     C                   Z-ADD     PiVal         VABNCA
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABNCA'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** ZNC
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on AND
136400030721     C                             PiVal<=99
136500030721     C                   Z-ADD     PiVal         VABZNC
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABZNC'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** DCR
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on       AND
137700030721     C                             PiVal<=99999999
137800030721     C                   Z-ADD     PiVal         VABDCR
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABDCR'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** HCR
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiInt=*on   AND
139000030721     C                             PiVal<=9999
139100030721     C                   Z-ADD     PiVal         VABHCR
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABHCR'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** VMD
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiNum=*on
140300030721     C                   Z-ADD     PiVal         VABVMD
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABVMD'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000030721     C*** ANT
141100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiInt=*on        AND
141500030721     C                             PiVal<=999999999
141600030721     C                   Z-ADD     PiVal         VABANT
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142000030721     C                             + ' ' + 'VABANT'
142100030721     C                   ENDIF
142200030721     C                   ENDIF
142300030721     C*** CMO
142400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142500030721     C                   EVAL      PiStr=SkSplitCSV(i)
142600030721     C                   EXSR      CHKNUM
142700030721     C                   IF        PiInt=*on
142800030721     C                   Z-ADD     PiVal         Num5_0
142900030721     C                   MOVEL(p)  Num5_0        VABCMO
143000030721     C                   ELSE
143100030721     C                   SETON                                        32
143200030721     C                   EVAL      VABCMO = *zeros
143300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143400030721     C                             + ' ' + 'VABCMO'
143500030721     C                   ENDIF
143600030721     C                   ENDIF
143700040510     C*
143800040510     C* Reperisco i campi del FIVAT
143900040510     C*** VATNOT
144000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144200040510     C                   ENDIF
144300040510     C*** VATTRC
144400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144600040510     C                   ENDIF
144700060331     C*** VATNOT_A
144800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145000060331     C                   ENDIF
145100060331     C*** VATNOT_B
145200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145400060331     C                   ENDIF
145500060331     C*** VATNOT_E
145600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145800060331     C                   ENDIF
145900110927     C*** VATNOT_I
146000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146200110927     C                   ENDIF
146300141003     C*** VATNOT_IJ
146400141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
146500141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
146600141003     C                   ENDIF
146700140319     C*** VATNOT_S
146800140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
146900140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147000140319     C                   ENDIF
147100070823     C*
147200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147300150309     C*
147400150428
147500150428     C*** RSD/RD2
147600150428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
147700150428     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
147800150428     C                   EVAL      VABRSD = %subst(wNOT:1:35)
147900150428     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
148000150428     C     '@':'A'       XLATE     VABRSD        VABRSD
148100150428     C                   EVAL      VABRD2 = %subst(wNOT:36:35)
148200150428     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
148300150428     C     '@':'A'       XLATE     VABRD2        VABRD2
148400150428     C                   ENDIF
148500150428
148600150428     C*** VABNOT
148700150428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
148800150428     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
148900150428     C                   EVAL      VABNOT = %subst(wNOT:1:35)
149000150428     C                   EVAL      VABNT2 = %subst(wNOT:36:35)
149100150428     C                   ENDIF
149200150428
149300150428     C*** VABCBO
149400150428     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
149500150428     C                   SELECT
149600150428     C                   WHEN      %trim(SkSplitCSV(i)) ='0'
149700150428     C                   EVAL      VABCBO = '1'
149800150428     C                   WHEN      %trim(SkSplitCSV(i)) ='1'
149900150428     C                   EVAL      VABCBO = '4'
150000150428     C                   ENDSL
150100150428     C                   ENDIF
150200150316
150300141003     C*
150400071029     C***  <===  -----------------------------------------
150500040510     C*
150600030715     C                   ENDIF
150700030715     C                   EVAL      i = i + 1
150800030715     C                   ENDDO
150900070925     C*
151000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151100140319     C*
151200141014     C* se la nazione è IT la sblanco
151300141112     C                   IF        VABNZD = 'IT' or
151400141112     C                             VABNZD = 'ITA'
151500141014     C                   EVAL      VABNZD = *blank
151600141014     C                   ENDIF
151700070925     C*
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
186000150428VABNSP;VATNOT_A;VATNOT_IJ;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;VATNOT_B;W_VABNOT;W_VABCBO;
186100150902VABCAS;VABNCL;X;VABIAS;VABPKB;VABCTR;VABTIC;
186200150428
186300150507
186400150428
