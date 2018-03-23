000100160121      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400160624      *
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
004100160630     D* in questo caso è la seconda
004200170712     D Intestazioni    s               n   inz(*on)
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
006900160624     D wVATNOT_E       s                   LIKE(VATNOT)
007000110927     D wVATNOT_I       s                   LIKE(VATNOT)
007100140220     D wVATNOT_J       s                   LIKE(VATNOT)
007200140220     D wVATNOT_IJ      s             70
007300140319     D wVATNOT_S       s                   LIKE(VATNOT)
007400140919     D wNOT            s             70
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
008600170713     D w2_0            s              2s 0
008700170713     D w7_0            s              7s 0
008800170712     D w70             s             70
008900170712     D wVABRMA1        s              2
009000170712     D wVABRMA2        s              7
009100170712     D wVABIAS1        s              1
009200170712     D wVABIAS2        s                   like(PiStr)
009300170712     D wVABCAS1        s              1
009400170712     D wVABCAS2        s                   like(PiStr)
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
025000150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
025100150430     C                   if        vlrpoi = 999
025200150430     C                   move(P)   invfil        VABFGS
025300150430     C                   else
025400150430     C                   move(P)   vlrpoi        VABFGS
025500150430     C                   endif
025600010601     C*
025700010601     C                   exsr      PREELA
025800010601     C*
025900010604     C* Ebbene...
026000010604     C*
026100040119     C  N31              add       1             §CTROKVB
026200040119     C   32              add       1             §CTRMO
026300040119     C   31              add       1             §CTRNO
026400080418     C*
026500030822     C                   if        wGiro = 2
026600080513     C* Scrivo sempre il VAT
026700080513     C  N31              exsr      wrivat
026800110201     C                   exsr      chkvab
026900080513     C                   endif
027000010604     C*
027100010604     C                   if        *in31 = *off and
027200010604     C                             *in32 = *off
027300010604     C                   eval      vinflg = '1'
027400010604     C                   else
027500010604     C                   eval      vinflg = '2'
027600010604     C                   endif
027700141014     C                   endif
027800010601     C*
027900010604     C                   endif
028000010604     C*
028100010330     C                   else
028200010330     C                   eval      vinflg = '1'
028300010330     C                   endif
028400010601     C*
028500010601     C  N70              update    tivin000
028600010330     C*
028700030822     C  N70              enddo
028800040119     C*
028900080527     C* Verifico dati rimasti "in cannna"...
029000080527     C                   seton                                        65
029100110201     C                   exsr      chkvab
029200080527     C*
029300040119     C                   endif                                                  (endif - chkcall)
029400010601     C*
029500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029600010601     C                   if        cntNonEl = *zeros or
029700010601     C                             flgMulti = '0'
029800010330     C* Se non ci sono record con errori ...
029900010601     C                   if        §ctrno = 0 and
030000010604     C                             §ctrmo = 0 and
030100010601     C                             flgStato <> '2'
030200010330     C* ... restituisco esito OK.
030300010330     C                   eval      wrkesito = '0'
030400010330     C                   else
030500040119     C                   if        §ctrokvb > 0
030600010330     C                   eval      wrkesito = '1'
030700010330     C                   else
030800010615     C                   if        flgOk = '0'
030900010615     C                   eval      wrkesito = '2'
031000010615     C                   else
031100010615     C                   eval      wrkesito = '6'
031200010615     C                   endif
031300010330     C                   endif
031400010330     C                   endif
031500010601     C                   else
031600010601     C                   eval      wrkesito = '9'
031700010601     C                   endif
031800010330     C*
031900010330     C                   if        %open(tivin00r)
032000010330     C                   close     tivin00r
032100010330     C                   endif
032200021025     C                   if        %open(fivabwwr)
032300021025     C                   close     fivabwwr
032400010330     C                   endif
032500040512     C                   if        %open(fivatwwr)
032600040512     C                   close     fivatwwr
032700040512     C                   endif
032800010601     C*
032900010601     C                   if        vlrpoi <> 999
033000010601     C                   eval      invfil = vlrpoi
033100010601     C                   endif
033200010330     C*
033300040119     C                   if        §ctrokvb > 0
033400010601     C                             and invfil > *zeros
033500010330     C                   exsr      invio
033600010330     C                   endif
033700010601     C*
033800010618     C                   if        flgGiro = '1'
033900010601     C                   exsr      endela
034000010618     C                   endif
034100010330     C*
034200010330     C                   ENDSR
034300010330     C***
034400080527
034500080527
034600080527
034700080527     C*----------------------------------------------------*
034800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034900080527     C*----------------------------------------------------*
035000100413     C     CHKVAB        BEGSR
035100110201     C*
035200110201     C* Se richiesti ignorare colli / peso / volume
035300110315 xxx C***                if        *in35
035400110315 xxx C***                z-add     *zeros        VABNCL
035500110315 xxx C***                z-add     *zeros        VABPKB
035600110315 xxx C***                z-add     *zeros        VABVLB
035700110315 xxx C***                endif
035800080527     C*
035900080527     C* Scrivo il VAB solo a rottura d spedizione
036000080527     C                   if        depspe = *zeros
036100080527     C  N31              eval      fivabds_sav = fivabds
036200080527     C                   eval      depspe = vabNSP
036300080527     C                   else
036400080527     C                   if        depspe = vabNSP AND *in65 = *off
036500100413     C   51              add       VABNCL        sav_VABNCL
036600100413     C   51              add       VABPKB        sav_VABPKB
036700100413     C   51              add       VABVLB        sav_VABVLB
036800100720     C   52              z-add     VABNCL        sav_VABNCL
036900100720     C   52              z-add     VABPKB        sav_VABPKB
037000100720     C   52              z-add     VABVLB        sav_VABVLB
037100100720     C   53              add       VABNCL        sav_VABNCL
037200100720     C   53              z-add     VABPKB        sav_VABPKB
037300100720     C   53              z-add     VABVLB        sav_VABVLB
037400160624
037500160624     C* PERSONALIZZAZIONE
037600160624     C* ogni rcd della spedizione equivale a un collo, poi devo sommere anche il peso
037700160624     C                   IF        *in54 = *on
037800160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
037900160624     C                   EVAL      sav_VABPKB = sav_VABPKB + VABPKB
038000160624     C                   ENDIF
038100160624
038200080527     C                   else
038300160624     C* PERSONALIZZAZIONE
038400160624     C* siccome il nr.colli lo gestisco solo a questo punto e devo aggiungere 1 per ogni rcd della
038500160624     C* spedizione, devo aggiungere 1 anche quando scrivo VAB perché non l'ho ancora fatto
038600160624     C                   IF        *in54 = *on
038700160624     C                   EVAL      sav_VABNCL = sav_VABNCL + 1
038800160624     C                   ENDIF
038900080527     C  N31              eval      fivabds_ok = fivabds_sav
039000080527     C  N31              exsr      wrivab
039100080527     C  N31              eval      fivabds_sav = fivabds
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
046600150316     C                   if        wVATNOT_E <> *blanks
046700060331     C                   eval      VATTRC = 'E'
046800170712     C*
046900170712     C* Sviluppati i "CHI SONO" in relazione al numero colli
047000170712     C* (VABRMA o wVATNOT_E valgono lo stesso valore)
047100170712     C*
047200170712     C     1             DO        VABNCL        wNumCollo         3 0
047300170712     C                   EVAL      VATNOT = %editc(VABCCM:'X') +
047400170712     C                                      %trimr(VABRMA) +
047500170712     C                                %editc(wNumCollo:'X')
047600160624     C                   write     FIVAT000
047700170712     C                   ENDDO
047800150316     C                   endif
047900150316     C*
048000110927     C                   if        wVATNOT_I <> *blanks
048100110927     C                   eval      VATNOT = wVATNOT_I
048200140711     C                   eval      VATTRC = 'I'
048300110927     C                   write     FIVAT000
048400110927     C                   add       1             §CTROKVT
048500110927     C                   endif
048600140220     C*
048700140220     C                   if        wVATNOT_J <> *blanks
048800140220     C                   eval      VATNOT = wVATNOT_J
048900140220     C                   eval      VATTRC = 'J'
049000140220     C                   write     FIVAT000
049100140220     C                   add       1             §CTROKVT
049200140220     C                   endif
049300140319     C*
049400140319     C                   if        wVATNOT_S <> *blanks
049500140319     C                   eval      VATNOT = wVATNOT_S
049600140319     C* i 2 flag devono essere blank
049700140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049800140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049900140319     C                   eval      VATTRC = 'S'
050000140319     C                   write     FIVAT000
050100140319     C                   add       1             §CTROKVT
050200140319     C                   endif
050300040119     C*
050400040119     C                   ENDSR
050500040119
050600040119
050700040119
050800040119     C*----------------------------------------------------*
050900040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051000040119     C*----------------------------------------------------*
051100040119     C     PREVAT        BEGSR
051200040119     C*
051300040119     C* Compongo il nome del membro da dare al FIVATWWR
051400040119     C                   eval      parmbr = vlrhdl
051500040119     C                   movel     'M'           parmbr
051600040510     C                   eval      parccm = vlrksc
051700040119     C                   eval      paropz = '1'
051800040119     C* Effettuo la chiamata al CLLE preposto
051900040119     C                   call(e)   'TITVVTC'
052000040119     C                   parm                    parccm
052100040119     C                   parm                    parmbr
052200040119     C                   parm                    paropz
052300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052400040119     C                   if        %error
052500040119     C                   movel     '1'           chkcall
052600040119     C                   else
052700040119     C                   movel     '0'           chkcall
052800040119     C                   endif
052900040119     C*
053000040119     C                   ENDSR
053100010601
053200010601
053300010601
053400010330     C*----------------------------------------------------*
053500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053600010330     C*----------------------------------------------------*
053700040119     C     INZVAR        BEGSR
053800080527     C*
053900080527     C                   setoff                                       65
054000010330     C*
054100030822     C* Inizializzo variabili di wrk
054200020204     C                   Z-ADD     *zeros        Num5_0            5 0
054300101018     C                   MOVEL     *blanks       wFlgCAS
054400060331     C                   MOVEL     *blanks       wVATNOT_A
054500060331     C                   MOVEL     *blanks       wVATNOT_B
054600060331     C                   MOVEL     *blanks       wVATNOT_E
054700110927     C                   MOVEL     *blanks       wVATNOT_I
054800140220     C                   MOVEL     *blanks       wVATNOT_J
054900140220     C                   MOVEL     *blanks       wVATNOT_IJ
055000140319     C                   MOVEL     *blanks       wVATNOT_S
055100120411     C                   MOVEL     *blanks       wNOT
055200120411     C                   MOVEL     *blanks       Data8A
055300030822     C*
055400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055500030822     C                   CLEAR                   SkSplitCSV
055600030822     C                   EVAL      i = 1
055700030822     C                   EVAL      posDa = *zeros
055800030822     C                   EVAL      posA  = *zeros
055900080527     C*
056000080527     C                   CLEAR                   FIVABDS
056100080527     C                   CLEAR                   FIVAT000
056200030822     C*
056300030822     C* Reimposto i valori di default
056400030822     C                   EXSR      DEFCAM
056500030822     C*
056600010330     C                   ENDSR
056700010330     C*----------------------------------------------------*
056800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056900010330     C*----------------------------------------------------*
057000010330     C     DEFCAM        BEGSR
057100030822     C*
057200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057300030715     C* e delimitatore testo.
057400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057700100318     C*
057800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057900100318     C                   EVAL      CharSOS = CharNUM
058000030822     C*
058100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058200100720     C                   SETOFF                                       505152
058300100720     C                   SETOFF                                       53
058400160624     C                   SETOFF                                       54
058500100413     C                   SELECT
058600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058700100720     C                   SETON                                        50
058800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058900100413     C                   SETON                                        51
059000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059100100720     C                   SETON                                        52
059200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059300100720     C                   SETON                                        53
059400160624     C* PERSONALIZZAZIONE
059500160624     C                   WHEN      %subst(vlrppt:1:1) = 'P'
059600160624     C                   SETON                                        54
059700100413     C                   ENDSL
059800030822     C*
059900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060000030822     C                   EVAL      posDaDft = 1
060100030822     C                   EVAL      posADft  = 0
060200030822     C                   EVAL      wGiroDft = 0
060300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060400030822     C                             posDaDft > 0
060500030822     C*
060600030822     C* Gestisco il 1° giro
060700030822     C                   IF        wGiroDft = 0
060800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061000030822     C* Incremento il contatore dei "giri"
061100030822     C                   EVAL      wGiroDft = 1
061200030822     C                   ELSE
061300030822     C                   EVAL      posDaDft = posADft
061400030822     C                   ENDIF
061500030822     C* Eseguo lo scan x trovare la fine del campo corrente
061600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061700030822     C*
061800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062000030822     C                   IF        posDaDft > 0 AND
062100030822     C                             posADft  > 0
062200030822     C* NCL
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'NCL'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200030822     C                   Z-ADD     PiVal         VABNCL
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* CCM
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'CCM'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiInt=*on
064500030822     C                   Z-ADD     PiVal         VABCCM
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* LNP
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'LNP'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiInt=*on
065800030822     C                   Z-ADD     PiVal         VABLNP
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* NRS
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'NRS'
066600030822     C                   EVAL      PiStr=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   EXSR      CHKNUM
067000030822     C                   IF        PiInt=*on
067100030822     C                   Z-ADD     PiVal         VABNRS
067200030822     C                   ENDIF
067300030822     C                   ENDIF
067400030822     C* CTR
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'CTR'
067900030822     C                   EVAL      PiStr=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   EXSR      CHKNUM
068300030822     C                   IF        PiInt=*on
068400030822     C                   Z-ADD     PiVal         VABCTR
068500030822     C                   ENDIF
068600030822     C                   ENDIF
068700030822     C* PKB
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'PKB'
069200030822     C                   EVAL      PiStr=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   EXSR      CHKNUM
069600030822     C                   IF        PiNum=*on
069700130610     C                   Z-ADD     PiVal         VABPKB
069800030822     C                   ENDIF
069900030822     C                   ENDIF
070000030822     C* VLB
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VLB'
070500030822     C                   EVAL      PiStr=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   EXSR      CHKNUM
070900030822     C                   IF        PiNum=*on
071000130610     C                   Z-ADD     PiVal         VABVLB
071100030822     C                   ENDIF
071200030822     C                   ENDIF
071300030822     C* QFT
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'QFT'
071800030822     C                   EVAL      PiStr=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   EXSR      CHKNUM
072200030822     C                   IF        PiNum=*on
072300030822     C                   Z-ADD     PiVal         VABQFT
072400030822     C                   ENDIF
072500030822     C                   ENDIF
072600030822     C* CBO
072700030822     C                   IF        %subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):1:3)
073000030822     C                             = 'CBO'
073100030822     C                   EVAL      VABCBO=%trim(%subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073400030822     C                   ENDIF
073500030822     C* TSP
073600030822     C                   IF        %subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):1:3)
073900030822     C                             = 'TSP'
074000030822     C                   EVAL      VABTSP=%trim(%subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074300030822     C                   ENDIF
074400030822     C* VAS
074500030822     C                   IF        %subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):1:3)
074800030822     C                             = 'VAS'
074900030822     C                   EVAL      VABVAS=%trim(%subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075200030822     C                   ENDIF
075300030822     C* VCA
075400030822     C                   IF        %subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):1:3)
075700030822     C                             = 'VCA'
075800030822     C                   EVAL      VABVCA=%trim(%subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076100030822     C                   ENDIF
076200070925     C* TIC
076300070925     C                   IF        %subst(
076400070925     C                             %subst(vlrppt:posDaDft+1:
076500070925     C                             posADft-posDaDft-1):1:3)
076600070925     C                             = 'TIC'
076700070925     C                   EVAL      VABTIC=%trim(%subst(
076800070925     C                             %subst(vlrppt:posDaDft+1:
076900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077000070925     C                   ENDIF
077100030822     C* GCA
077200030822     C                   IF        %subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):1:3)
077500030822     C                             = 'GCA'
077600030822     C                   EVAL      VABGCA=%trim(%subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077900030822     C                   ENDIF
078000030822     C* CTM
078100030822     C                   IF        %subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):1:3)
078400030822     C                             = 'CTM'
078500030822     C                   EVAL      VABCTM=%trim(%subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078800030822     C                   ENDIF
078900030822     C* FFD
079000030822     C                   IF        %subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):1:3)
079300030822     C                             = 'FFD'
079400030822     C                   EVAL      VABFFD=%trim(%subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079700030822     C                   ENDIF
079800030822     C* VAD
079900030822     C                   IF        %subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):1:3)
080200030822     C                             = 'VAD'
080300030822     C                   EVAL      VABVAD=%trim(%subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080600030822     C                   ENDIF
080700030822     C* GMA
080800030822     C                   IF        %subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):1:3)
081100030822     C                             = 'GMA'
081200030822     C                   EVAL      VABGMA=%trim(%subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081500030822     C                   ENDIF
081600030822     C* GGA
081700030822     C                   IF        %subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):1:3)
082000030822     C                             = 'GGA'
082100030822     C                   EVAL      VABGGA=%trim(%subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082400030822     C                   ENDIF
082500030822     C* GVA
082600030822     C                   IF        %subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):1:3)
082900030822     C                             = 'GVA'
083000030822     C                   EVAL      VABGVA=%trim(%subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083300030822     C                   ENDIF
083400030822     C* TC1
083500030822     C                   IF        %subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):1:3)
083800030822     C                             = 'TC1'
083900030822     C                   EVAL      VABTC1=%trim(%subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084200030822     C                   ENDIF
084300030822     C* TC2
084400030822     C                   IF        %subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):1:3)
084700030822     C                             = 'TC2'
084800030822     C                   EVAL      VABTC2=%trim(%subst(
084900030822     C                             %subst(vlrppt:posDaDft+1:
085000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085100030822     C                   ENDIF
085200040714     C* VATTRC
085300040714     C                   IF        %subst(
085400040714     C                             %subst(vlrppt:posDaDft+1:
085500040714     C                             posADft-posDaDft-1):1:3)
085600040714     C                             = 'TRC'
085700040714     C                   EVAL      VATTRC=%trim(%subst(
085800040714     C                             %subst(vlrppt:posDaDft+1:
085900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086000040714     C                   ENDIF
086100030822     C* ...
086200030822     C                   ENDIF
086300030822     C                   ENDDO
086400020204     C*
086500010330     C                   ENDSR
086600010607     C*----------------------------------------------------*
086700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086800010607     C*----------------------------------------------------*
086900010607     C     IMPVAB        BEGSR
087000010607     C*
087100010607     C                   SETOFF                                       3132
087200110315 xxx C***                SETON                                        35
087300030515     C*
087400010607     C                   MOVEL     datcor        VABAAS
087500010607     C                   MOVE      datcor        VABMGS
087600010607     C*
087700030715     C**********
087800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087900030715     C**********
088000030715     C                   IF        wGiro = *zeros
088100040113     C*
088200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088500080429     C                             %trim(intcol(5))<>*blanks
088600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088800080429     C                                      %trim(intcol(5))
088900070823     C     ';':CharCSV   XLATE     vindta        vindta
089000040510     C                   ENDIF
089100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089200031201     C     minu:maiu     XLATE     vindta        vindta
089300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089400030715     C                   DOW       posDa <= %len(%trim(vindta))
089500030715     C*
089600030715     C* Gestisco il 1° campo
089700030715     C                   IF        i = 1
089800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089900030715     C                   EVAL      posDa = 1
090000030715     C* Eseguo lo scan x trovare la fine del primo campo
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C* X i campi successivi al 1°
090600030715     C                   ELSE
090700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090800030715     C                   EVAL      posDa = posA + 1
090900030715     C* Eseguo lo scan x trovare la fine del campo corrente
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* Gestisco l'ultimo campo
091200030715     C                   IF        posA = *zeros
091300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   LEAVE
091800030715     C                   ELSE
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   ENDIF
092300030715     C                   ENDIF
092400030715     C* Incremento il contatore d campo
092500030715     C                   EVAL      i = i +1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092900030715     C* delimitazione testo
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitFLD)
093200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093400030715     C                   EVAL      i = i + 1
093500030715     C                   ENDDO
093600030715     C                   EVAL      wGiro = 1
093700030715     C                   ELSE
093800030715     C                   EVAL      wGiro = 2
093900031204     C**********
094000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094100031204     C* d separatore campo
094200031204     C**********
094300031204     C                   EVAL      vindta = %trim(vindta)
094400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094500031204     C                   EVAL      vindta = %subst(vindta:2)
094600031204     C                   ENDDO
094700031204     C*
094800031204     C                   Z-ADD     *zeros        lunghInput        4 0
094900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095300110912     C                   ENDDO
095400030715     C**********
095500030715     C* Effettuo lo split del campo dati d input
095600030715     C**********
095700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095800030715     C                   DOW       posDa <= %len(%trim(vindta))
095900030715     C*
096000030715     C* Gestisco il 1° campo
096100030715     C                   IF        i = 1
096200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096300030715     C                   EVAL      posDa = 1
096400030715     C* Eseguo lo scan x trovare la fine del primo campo
096500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096600030715     C* A questo punto "estraggo" il campo corrente
096700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096800030715     C                                                    (posA-posDa))
096900030715     C* X i campi successivi al 1°
097000030715     C                   ELSE
097100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097200030715     C                   EVAL      posDa = posA + 1
097300031204     C* Verifico che nn vi sia il campo nn valorizzato
097400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097500031204     C* Se campo nn valorizzato skippo al prossimo
097600031204     C                   EVAL      SkSplitCSV(i) = *blanks
097700031204     C                   EVAL      posA  = posA + 1
097800031204     C                   ELSE
097900030715     C* Eseguo lo scan x trovare la fine del campo corrente
098000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098100030715     C* Gestisco l'ultimo campo
098200030715     C                   IF        posA = *zeros
098300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098400030715     C* A questo punto "estraggo" il campo corrente
098500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098600030715     C                                                    (posA-posDa))
098700030715     C                   LEAVE
098800030715     C                   ELSE
098900030715     C* A questo punto "estraggo" il campo corrente
099000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099100030715     C                                                    (posA-posDa))
099200030715     C                   ENDIF
099300031204     C                   ENDIF
099400030715     C                   ENDIF
099500030715     C* Incremento il contatore d campo
099600030715     C                   EVAL      i = i +1
099700030715     C                   ENDDO
099800030715     C*
099900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100000030715     C* delimitazione testo
100100030715     C                   EVAL      i = 1
100200030715     C                   DOW       i < %elem(SkSplitCSV)
100300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100500030715     C                   EVAL      i = i + 1
100600030715     C                   ENDDO
100700100720     C*
100800100720     C* Stacco un progressivo x numero spedizione
100900170712 xxx C                   EXSR      repNSP
101000100326     C*
101100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101200030715     C                   EVAL      i = 1
101300030715     C                   DOW       i < %elem(SkSplitFLD)
101400030715     C*
101500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101700030715     C                             SkSplitCSV(i) <> *blanks
101800030721     C*** ATB
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102000030721     C                   EVAL      VABATB = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030715     C*** RSD
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102700030715     C* ==
102800130528     C                   ENDIF
102900030721     C*** RD2
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030715     C*** IND
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103500030715     C                   EVAL      VABIND = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** LOD
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** PRD
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
104300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030721     C*** NZD
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030715     C*** RMA
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** NOT
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** NT2
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** VCA
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
106300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030715     C*** VAS
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030715     C*** FFD
107000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107200030715     C                   ENDIF
107300030721     C*** GC1
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GC2
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** TSP
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
108300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108400030721     C                   ENDIF
108500040510     C*** CBO
108600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108800040510     C                   ENDIF
108900030721     C*** NAS
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** TIC
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** GCA
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** XCO
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
110300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** CTM
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** TCR
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** CTS
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** FTM
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** VAD
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
112300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GMA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** GGA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** GVA
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** TC1
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** TC2
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
114300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** SCL
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** RMO
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*** NMO
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115600030721     C                   ENDIF
115700030721     C*
115800030721     C* Reperisco quindi i campi numerici...
115900040510     C*** NSP
116000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116100040510     C                   EVAL      PiStr=SkSplitCSV(i)
116200040510     C                   EXSR      CHKNUM
116300040510     C                   IF        PiInt=*on
116400040510     C                   Z-ADD     PiVal         VABNSP
116500040510     C                   ELSE
116600040510     C                   SETON                                        31
116700040510     C                   EVAL      VABNSP = *zeros
116800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116900040510     C                             + ' ' + 'VABNSP'
117000040510     C                   ENDIF
117100040510     C                   ENDIF
117200071106     C*** RMN
117300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117400071106     C                   EVAL      PiStr=SkSplitCSV(i)
117500071106     C                   EXSR      CHKNUM
117600071106     C                   IF        PiInt=*on
117700071106     C                   Z-ADD     PiVal         VABRMN
117800071106     C                   ELSE
117900071106     C                   SETON                                        32
118000071106     C                   EVAL      VABRMN = *zeros
118100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118200071106     C                             + ' ' + 'VABRMN'
118300071106     C                   ENDIF
118400071106     C                   ENDIF
118500030721     C*** CAD
118600130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118700130610     C                   EVAL      PiStr=SkSplitCSV(i)
118800130610     C                   EXSR      CHKNUM
118900130610     C                   IF        PiInt=*on
119000130610     C                   Z-ADD     PiVal         Num5_0
119100130610     C                   MOVEL(p)  Num5_0        VABCAD
119200130610     C                   ELSE
119300130610     C                   SETON                                        32
119400130610     C                   EVAL      VABCAD = *zeros
119500130610     C                   EVAL      vinmsg = %trimr(vinmsg)
119600130610     C                             + ' ' + 'VABCAD'
119700130610     C                   ENDIF
119800130605     C                   ENDIF
119900030721     C*** NCL
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on
120400030721     C                   Z-ADD     PiVal         VABNCL
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABNCL = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040122     C                             + ' ' + 'VABNCL'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** PKB
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiNum=*on
121700160624     C*                  Z-ADD(H)  PiVal         VABPKB
121800160630     C                   EVAL(H)   VABPKB = PiVal
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABPKB = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABPKB'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** CAS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122800110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122900111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
123000160404     C                             %trim(SkSplitCSV(i)) <> '0,00'
123100130703     C                   EVAL      VABVCA = 'EUR'
123200160630     C                   EVAL      VABTIC = 'BM'
123300140513     C                   EVAL      wFlgCAS = '1'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiNum=*on
123700030721     C                   Z-ADD     PiVal         VABCAS
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      VABCAS = *zeros
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABCAS'
124300030721     C                   ENDIF
124400140513     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** CCM
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiInt=*on      AND
125100030721     C                             PiVal<=9999999 AND
125200030721     C                             PiVal>*zeros
125300030721     C                   Z-ADD     PiVal         VABCCM
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      VABCCM = *zeros
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800040510     C                             + ' ' + 'VABCCM'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** LNP
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on    AND
126600030721     C                             PiVal<=999   AND
126700030721     C                             PiVal>*zeros
126800030721     C                   Z-ADD     PiVal         VABLNP
126900030721     C                   ELSE
127000040715     C                   SETON                                        32
127100030721     C                   EVAL      VABLNP = *zeros
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300040510     C                             + ' ' + 'VABLNP'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NRS
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on AND
128100030721     C                             PiVal<=99
128200030721     C                   Z-ADD     PiVal         VABNRS
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNRS'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** LNA
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on    AND
129400030721     C                             PiVal<=999   AND
129500030721     C                             PiVal>*zeros
129600030721     C                   Z-ADD     PiVal         VABLNA
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      VABLNA = *zeros
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABLNA'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** CTR
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on  AND
130900030721     C                             PiVal<=999
131000030721     C                   Z-ADD     PiVal         VABCTR
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABCTR'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** IAS
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131900141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
132000141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
132100150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
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
149800150309     C*
149900170712
150000170713     C*** W_VABCBO
150100170713     C* serve perché il cliente passa 01 invece che 1
150200170713     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
150300170713     C                   EVAL      w2_0 = %dec(%trim(SkSplitCSV(i)):2:0)
150400170713     C                   EVAL      VABCBO = %char(w2_0)
150500170712     C                   ENDIF
150600170713
150700170713     C*** W_VATNOT_E / VABRMA
150800170713     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
150900170713     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
151000170713     C                   EVAL      wVABRMA1 = %trim(SkSplitCSV(i))
151100170713     C                   ENDIF
151200170712
151300170712      *** W_VABRMN: è sia VABRMN che la seconda parte di VABRMA
151400170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
151500170712     C                   EVAL      PiStr=SkSplitCSV(i)
151600170712     C                   EXSR      CHKNUM
151700170712     C                   IF        PiInt=*on
151800170712     C                   Z-ADD     PiVal         VABRMN
151900170712     C                   Z-ADD     VABRMN        w7_0
152000170712     C                   EVAL      wVABRMA2 = %editc(w7_0:'X')
152100170712     C                   ELSE
152200170712     C                   SETON                                        32
152300170712     C                   EVAL      VABRMN = *zeros
152400170712     C                   EVAL      vinmsg = %trimr(vinmsg)
152500170712     C                             + ' ' + 'VABRMN'
152600170712     C                   ENDIF
152700170712     C                   ENDIF
152800170712
152900170712      *** W_VABRSD_I: è sia VABRSD che VABRD2
153000170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
153100170712     C                   EVAL      w70 = %trim(SkSplitCSV(i))
153200170712     C                   if        %subst(w70:1:35) <> *blanks
153300170712     C                   eval      VABRSD = %trim(%subst(w70:1:35))
153400170712     C                   endif
153500170712     C                   if        %subst(w70:36:35) <> *blanks
153600170712     C                   eval      VABRD2 = %trim(%subst(w70:36:35))
153700170712     C                   endif
153800170712     C                   ENDIF
153900170712
154000170712      *** W_VATNOT_I: è sia VATNOT_I che VATNOT_J
154100170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
154200170712     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
154300170712     C                   ENDIF
154400170712
154500170712      *** W_VABIAS1
154600170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIAS1'
154700170712     C                   EVAL      wVABIAS1 = %trim(SkSplitCSV(i))
154800170712     C                   ENDIF
154900170712      *** W_VABIAS2
155000170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIAS2'
155100170712     C                   EVAL      wVABIAS2 = %trim(SkSplitCSV(i))
155200170712     C                   ENDIF
155300170712
155400170712      *** W_VABCAS1
155500170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAS1'
155600170712     C                   EVAL      wVABCAS1 = %trim(SkSplitCSV(i))
155700170712     C                   ENDIF
155800170712      *** W_VABCAS2
155900170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAS2'
156000170712     C                   EVAL      wVABCAS2 = %trim(SkSplitCSV(i))
156100170712     C                   ENDIF
156200170712
156300170712      *** W_VABTIC
156400170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
156500170712     C                   SELECT
156600170712     C                   WHEN      %trim(SkSplitCSV(i)) = 'C'
156700170712     C                   EVAL      VABTIC = *blank
156800170712     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
156900170712     C                   EVAL      VABTIC = 'BM'
157000170712     C                   OTHER
157100170712     C                   EVAL      VABTIC = *blank
157200170712     C                   ENDSL
157300170712     C                   ENDIF
157400170712
157500141003     C*
157600071029     C***  <===  -----------------------------------------
157700040510     C*
157800030715     C                   ENDIF
157900030715     C                   EVAL      i = i + 1
158000030715     C                   ENDDO
158100070925     C*
158200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
158300170706     C*
158400170712
158500170712     C                   EVAL      VABRMA = wVABRMA1 + wVABRMA2
158600170712
158700170712     C*** VABIAS
158800170712     C                   IF        wVABIAS1 = 'S'
158900170712     C                   IF        %trim(wVABIAS2) <> *blanks AND
159000170712     C                             %trim(wVABIAS2) <> *zeros
159100170712     C                   EVAL      VABVAS = 'EUR'
159200170712     C                   EVAL      PiStr=wVABIAS2
159300170712     C                   EXSR      CHKNUM
159400170712     C                   IF        PiNum=*on
159500170712     C                   Z-ADD     PiVal         VABIAS
159600170712     C                   ELSE
159700170712     C                   SETON                                        32
159800170712     C                   EVAL      VABIAS = *zeros
159900170712     C                   EVAL      vinmsg = %trimr(vinmsg)
160000170712     C                             + ' ' + 'VABIAS'
160100170712     C                   ENDIF
160200170712     C                   ENDIF
160300170712     C                   ENDIF
160400170712
160500170712     C*** VABCAS
160600170712     C                   IF        wVABCAS1 = 'S'
160700170712     C                   IF        %trim(wVABCAS2) <> *blanks AND
160800170712     C                             %trim(wVABCAS2) <> *zeros
160900170712     C                   EVAL      wFlgCAS = '1'
161000170712     C                   EVAL      VABVCA = 'EUR'
161100170712     C                   EVAL      PiStr=wVABCAS2
161200170712     C                   EXSR      CHKNUM
161300170712     C                   IF        PiNum=*on
161400170712     C                   Z-ADD     PiVal         VABCAS
161500170712     C                   ELSE
161600170712     C                   SETON                                        32
161700170712     C                   EVAL      VABCAS = *zeros
161800170712     C                   EVAL      vinmsg = %trimr(vinmsg)
161900170712     C                             + ' ' + 'VABCAS'
162000170712     C                   ENDIF
162100170712     C                   ENDIF
162200170712     C                   ENDIF
162300070925     C*
162400160404     C* pulisco la nazione se vuol essere Italia
162500160404     C                   IF        VABNZD = 'I'     OR
162600160404     C                             VABNZD = 'IT'    OR
162700160404     C                             VABNZD = 'ITA'
162800160404     C                   EVAL      VABNZD = *blank
162900160404     C                   ENDIF
163000160404     C*
163100070925     C* Considerazioni finali su CBO/CAS
163200140220     C                   IF        wFlgCAS = '1'    OR
163300140220     C                             VABCAS  > *zeros
163400140220     C                   IF        VABCBO = '1'
163500140220     C                   EVAL      VABCBO = '4'
163600140220     C                   ENDIF
163700140220     C                   IF        VABCBO = '2'
163800140220     C                   EVAL      VABCBO = '6'
163900140220     C                   ENDIF
164000140220     C                   ENDIF
164100070925     C***  <===  ----------------------------
164200040510     C*
164300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
164400040510     C                   IF        VABNSP = *zeros
164500040510     C                   SETON                                        31
164600040510     C                   ENDIF
164700040715     C*
164800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
164900040715     C                   IF        VABLNP = *zeros
165000040715     C                   SETON                                        31
165100040715     C                   ENDIF
165200040510     C*
165300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
165400120411     C                   IF        VABRMN = *zeros
165500120411     C                   Z-ADD     VABNSP        VABRMN
165600120411     C                   ENDIF
165700120328     C* IN QUESTO CASO LO FORZO = 1
165800120411     C***                IF        VABRMN = *zeros
165900120411     C***                Z-ADD     1             VABRMN
166000120411     C***                ENDIF
166100040510     C*
166200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
166300040510     C                   IF        VABRMA = *blanks
166400040714     C                   MOVEL     VABRMN        VABRMA
166500040510     C                   ENDIF
166600040128     C*
166700040510     C* Se provincia nn valorizzata la reperisco
166800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
166900110912     C                   IF        VABPRD =  *blanks AND
167000110912     C                             VABNZD =  *blanks AND
167100110912     C                             VABCAD <> *blanks AND
167200110912     C                             VABLOD <> *blanks
167300040128     C                   CLEAR                   TISI95DS
167400040128     C                   EVAL      I95TCN = '3'
167500040128     C                   Z-ADD     datcor        I95DAT
167600040128     C                   EVAL      I95NAR = VABNZD
167700040128     C                   EVAL      I95CAP = VABCAD
167800040128     C                   EVAL      I95LOC = VABLOD
167900040128     C                   CALL      'TISI95R'
168000040128     C                   PARM                    TISI95DS
168100040128     C                   EVAL      VABPRD = O95PRV
168200040510     C                   ENDIF
168300020204     C*
168400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
168500020204     C                   EXSR      CHKIMPDIV
168600030715     C                   ENDIF
168700020204     C*
168800010607     C                   ENDSR
168900010607     C*----------------------------------------------------*
169000010601
169100020204
169200020204     C*----------------------------------------------------*
169300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
169400020204     C*----------------------------------------------------*
169500020204     C     CHKIMPDIV     BEGSR
169600020204     C*
169700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
169800020204     C                   Z-ADD     *zeros        wrkDec            9 9
169900020204     C*
170000020204     C* Come prima cosa effettuo considerazioni sulla divisa
170100020204     C                   IF        vabIAS > *zeros
170200020204     C                   IF        vabVAS <> 'EUR'
170300020204     C                   EVAL      vabVAS =  'ITL'
170400020204     C                   ENDIF
170500020204     C                   ENDIF
170600020204     C*
170700020204     C                   IF        vabCAS > *zeros
170800020204     C                   IF        vabVCA <> 'EUR'
170900020204     C                   EVAL      vabVCA =  'ITL'
171000020204     C                   ENDIF
171100020204     C                   ENDIF
171200020204     C*
171300020204     C                   IF        vabVMD > *zeros
171400020204     C                   IF        vabVAD <> 'EUR'
171500020204     C                   EVAL      vabVAD =  'ITL'
171600020204     C                   ENDIF
171700020204     C                   ENDIF
171800020204     C*
171900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
172000020204     C                   Z-ADD     vabIAS        wrkDec
172100020204     C                   IF        wrkDec > *zeros
172200020204     C                   IF        vabVAS = 'ITL'
172300020204     C                   EVAL      vabIAS = *zeros
172400020204     C                   ENDIF
172500020204     C                   ENDIF
172600020204     C*
172700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
172800020204     C                   Z-ADD     vabCAS        wrkDec
172900020204     C                   IF        wrkDec > *zeros
173000020204     C                   IF        vabVCA = 'ITL'
173100020204     C                   EVAL      vabCAS = *zeros
173200020204     C                   ENDIF
173300020204     C                   ENDIF
173400020204     C*
173500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
173600020204     C                   Z-ADD     vabVMD        wrkDec
173700020204     C                   IF        wrkDec > *zeros
173800020204     C                   IF        vabVAD = 'ITL'
173900020204     C                   EVAL      vabVMD = *zeros
174000020204     C                   ENDIF
174100020204     C                   ENDIF
174200020204     C*
174300020204     C                   ENDSR
174400020204     C***
174500010330
174600100318
174700100318
174800100318     C*----------------------------------------------------*
174900100318     C*  CONTROLLO NUMERICITA' CAMPI
175000100318     C*----------------------------------------------------*
175100100318     C     CHKNUM        BEGSR
175200100318     C*
175300100318     C                   IF        PiDecChr = *blanks
175400100318     C                   EVAL      PiDecChr = CharNUM
175500100318     C                   ENDIF
175600100318     C*
175700100318     C                   callp     UBISNUM_Check(PiStr
175800100318     C                                          :PiDecChr
175900100318     C                                          :PiVal
176000100318     C                                          :PiNum
176100100318     C                                          :PiInt)
176200100318     C*
176300100318     C                   ENDSR
176400100318     C***
176500010601
176600010601
176700010601
176800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
176900141006      ***
177000141014     C     repfil        BEGSR
177100141014     C*
177200141014     C                   if        invfil = *zeros and
177300141014     C                             depfil > *zeros and
177400141014     C                             (vinflg = *blanks or
177500141014     C                              vinflg = *zeros)
177600141014     C*
177700141014     C                   eval      invfil = depfil
177800141014     C                   endif
177900141014     C*
178000141014     C                   if        depfil <> invfil and
178100141014     C                             invfil > *zeros
178200141014     C                   eval      flgMulti = '1'
178300141014     C                   if        vinflg = *blanks
178400141014     C                   add       1             cntNonEl
178500141014     C                   endif
178600141014     C                   endif
178700141014     C*
178800141014     C                   if        vinflg = '2'
178900141014     C                   eval      flgStato = '2'
179000141014     C                   endif
179100141014     C*
179200141014     C                   ENDSR
179300141014     C*
179400010601
179500010601
179600010601
179700010330
179800040119      /TITLE Invio dei dati al punto operativo.
179900040119     C     invio         BEGSR
180000040119     C*
180100040119     C* 1° invio FIVAT
180200040119     C                   reset                   dscmz
180300040119     C                   move      vlrpoi        cmzdst
180400040119     C                   eval      cmzfld = 'FIVATWWR'
180500040119     C                   eval      cmzmbd = vlrhdl
180600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
180700040119     C***                if        prmfir = *blanks
180800040119     C                   eval      cmzfla = 'FIVAT00F'
180900040119     C                   eval      cmzmba = 'FIVAT00F'
181000040119     C***                else
181100040119     C***                eval      cmzfla = prmfir
181200040119     C***                eval      cmzmba = prmfir
181300040119     C***                endif
181400040119     C                   eval      cmznrr = *zeros
181500040119     C                   move      §ctrokvt      cmznrr
181600040119     C                   eval      cmzlba = vlrfl1
181700040119     C                   call(e)   'TIS711C'
181800040119     C                   parm                    dscmz
181900040119     C                   parm      *blanks       esito
182000040119     C                   if        %error
182100040119     C                             or cmzerr = '1'
182200040119     C                             or esito  = '1'
182300040119     C                   eval      wrkesito = '3'
182400040119     C                   else
182500040119     C*
182600040119     C* 2° invio FIVAB
182700040119     C                   reset                   dscmz
182800040119     C                   move      vlrpoi        cmzdst
182900040119     C                   eval      cmzfld = vlrfou
183000040119     C                   eval      cmzmbd = vlrhdl
183100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
183200040119     C***                if        prmfir = *blanks
183300040119     C                   eval      cmzfla = 'FIVAB00F'
183400040119     C                   eval      cmzmba = 'FIVAB00F'
183500040119     C***                else
183600040119     C***                eval      cmzfla = prmfir
183700040119     C***                eval      cmzmba = prmfir
183800040119     C***                endif
183900040119     C                   eval      cmznrr = *zeros
184000040119     C                   move      §ctrokvb      cmznrr
184100040119     C                   eval      cmzlba = vlrfl1
184200040119     C                   call(e)   'TIS711C'
184300040119     C                   parm                    dscmz
184400040119     C                   parm      *blanks       esito
184500040119     C                   if        %error
184600040119     C                             or cmzerr = '1'
184700040119     C                             or esito  = '1'
184800040119     C                   eval      wrkesito = '3'
184900040119     C                   endif
185000040119     C                   endif
185100040119     C*
185200040119     C                   ENDSR
185300040119     C***
185400010601
185500010601
185600010601
185700010601
185800010601      /TITLE Invio dei dati al punto operativo.
185900010601     C     opeini        BEGSR
186000010601     C*
186100010601     C* Inizializzo flag e contatori operativi
186200010601     C                   movel     '0'           flgGiro           1
186300010601     C                   movel     '0'           flgMulti          1
186400010601     C                   movel     '1'           flgStato          1
186500010615     C                   movel     '0'           flgOk             1
186600010601     C                   z-add     *zeros        cntNonEl         10 0
186700010601     C                   z-add     *zeros        depfil            3 0
186800010601     C                   z-add     *zeros        invfil            3 0
186900010601     C*
187000010601     C                   ENDSR
187100010601     C***
187200070326
187300070326
187400070326
187500070326
187600070326     C     *pssr         BEGSR
187700070329     C*
187800070329     C                   if        %open(tivin00r)
187900070329     C                   close     tivin00r
188000070329     C                   endif
188100070329     C                   if        %open(fivabwwr)
188200070329     C                   close     fivabwwr
188300070329     C                   endif
188400070329     C                   if        %open(fivatwwr)
188500070329     C                   close     fivatwwr
188600070329     C                   endif
188700070326     C*
188800070326     C* Effettuo la chiamata al CLLE preposto
188900070326     C                   call(e)   'TITVVTC'
189000070326     C                   parm                    parccm
189100070326     C                   parm                    parmbr
189200070326     C                   parm      '2'           paropz
189300070326     C*
189400070326     C                   eval      wrkesito = '2'
189500070404     C*
189600070404     C                   seton                                        LR
189700070326     C*
189800070326     C                   ENDSR     '*CANCL'
189900070326     C***
190000100318
190100100318
190200100318
190300100318     C     normCSV       BEGSR
190400100318     C*
190500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
190600100318     C* separatori campi
190700100318     C                   if        CharTXT <> *blanks
190800100318     C                   z-add     *zeros        wLenVINDTA        4 0
190900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
191000100318     C                   setoff                                       80
191100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
191200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
191300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
191400100318     C                   if        not *in80
191500100318     C                   seton                                        80
191600100318     C                   else
191700100318     C                   setoff                                       80
191800100318     C                   endif
191900100318     C                   endif
192000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
192100100318     C                             and *in80 = *on
192200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
192300100318     C                   endif
192400100318     C                   enddo
192500100318     C                   endif
192600100318     C*
192700100318     C                   ENDSR
192800100318     C***
192900070326
193000100326
193100100326
193200100326     C*----------------------------------------------------*
193300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
193400100326     C*----------------------------------------------------*
193500100326     C     REPNSP        BEGSR
193600100326     C*
193700100326     C* NSP => Stacco un numeratore da AZNUM
193800100326     C                   clear                   TRUL33DS
193900100326     C                   eval      I33OPE = *zeros
194000100326     C                   eval      I33CNU = 302
194100100326     C                   eval      I33NUM = 1
194200100326     C                   movel     TRUL33DS      KPJBU
194300100326     C                   call      'TRUL33R'
194400100326     C                   parm                    KPJBA
194500100326     C                   movel     KPJBU         TRUL33DS
194600100326     C                   if        O33ERR = *zeros
194700100326     C                   z-add     O33NRF        VABNSP
194800100326     C                   z-add     O33NRF        VATNSP
194900100326     C                   else
195000100326     C                   SETON                                        31
195100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
195200140711     C                             + ' ' + 'VABNSP VATNSP'
195300100326     C                   endif
195400100326     C*
195500100326     C                   ENDSR
195600010330
195700100326
195800010330
195900000613     C     *inzsr        BEGSR
196000990910     C*
196100990910     C     *entry        plist
196200990920     C                   parm                    tivlrds
196300990921     C                   parm      wrkesito      esito
196400000724     C                   parm                    prmlit
196500000710     C                   parm                    prmfir
196600010330     C*
196700010330     C* CALCOLA LA DATA CORRENTE
196800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
196900100318     C                   eval      datcor = %dec(%date() : *ISO)
197000000613     C*
197100000613     C                   ENDSR
197200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
197300170713W_VATNOT_E;X;W_VABRMN;X;W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;W_VATNOT_I;W_VABCBO;VABNCL;
197400170712VABPKB;W_VABIAS1;W_VABIAS2;W_VABCAS1;W_VABCAS2;W_VABTIC;
197500150428
197600150428
197700150428
