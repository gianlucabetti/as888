000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400141006      * - LNP lo reperisco cercando nella schiera Cod l'elemento che coincido con i primi 3 char del
000500141003      * barcode; con quell'indice reperisco l'elemento dalla schiera Fil.
000600141006      * - eliminata la gestione multifiliale perché altrimenti LNP che è calcolato dà errore
000700180201      * - aggiunta gestione appuntamento, usando come telefono quello per l'alert via SMS solo
000800180201      * se c'è la richiesta di appuntamento.
000900140930      *
001000121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001100130917     H decedit('0,')
001200120209     F*
001300990910     Ftivin00r  uF   E             DISK    usropn
001400100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001500040119     FFIVATwwr  O    E             DISK    usropn
001600000313     D*
001700040113     D*------------
001800040113     D* SCHIEDE A PROGRAMMA
001900040113     D*------------
002000080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002100141003     D*------------
002200171127     D Cod             S              3    DIM(234) CTDATA PERRCD(1)
002300171127     D Fil             S              4    DIM(234) ALT(Cod)
002400010330     D*----------------------------------------------------
002500010330     D* DICHIARAZIOINE VARIABILI DI WRK
002600010330     D*----------------------------------------------------
002700010330     D dscmz         e ds                  inz
002800010330     D psds           sds
002900010330     D  procname         *PROC
003000010330     D tivlrds       e ds                  extname(tivlr00f)
003100040128     D tisi95ds      e ds
003200100413     D fivabds       e ds                  extname(fivab00f)
003300100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003400100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003500010330     D esito           s              1
003600010330     D prmlit          s             10
003700010330     D prmfir          s             10
003800010330     D wrkesito        s                   like(esito)
003900010330     D rrnum           s              6  0 INZ(*zeros)
004000040510     D parccm          s              8    INZ(*blanks)
004100040119     D parmbr          s             10    INZ(*blanks)
004200040119     D paropz          s              1    INZ(*blanks)
004300040119     D chkcall         s              1    INZ(*blanks)
004400080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004500080418     D wFlgCAS         s              1    INZ(*blanks)
004600120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004700120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004800140930     D Intestazioni    s               n   inz(*on)
004900100326     D*------------------
005000100326     D* DS REPERIMENTO NUMERATORE
005100100326     D*------------------
005200100326     D trul33ds      e ds                  inz
005300100326     D*------------------
005400100326     D* DS ARCHITETTURA
005500100326     D*------------------
005600100326     D kpjba         e ds                  inz
005700130611     D*------------------
005800130611     D TRUL28DSX     e ds                  inz
005900100326
006000030715     D*------------------
006100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006200030715     D*------------------
006300030715     D SkSplitFLD      S             10    DIM(100)
006400030715     D SkSplitCSV      S            256    DIM(100)
006500030715     D CharCSV         S              1
006600030715     D CharTXT         S              1
006700030715     D CharNUM         S              1
006800100318     D CharSOS         S              1
006900130626     D posDa           S              4  0 INZ(*zeros)
007000130626     D posA            S              4  0 INZ(*zeros)
007100030715     D i               s              3  0 INZ(1)
007200030715     D wGiro           s              1  0 INZ(*zeros)
007300070823     D wVATNOT_A       s                   LIKE(VATNOT)
007400060331     D wVATNOT_B       s                   LIKE(VATNOT)
007500060331     D wVATNOT_E       s                   LIKE(VATNOT)
007600110927     D wVATNOT_I       s                   LIKE(VATNOT)
007700140220     D wVATNOT_J       s                   LIKE(VATNOT)
007800140220     D wVATNOT_IJ      s             70
007900140319     D wVATNOT_S       s                   LIKE(VATNOT)
008000140919     D w70             s             70
008100141006     D w11             s             11
008200140919     D wNOT            s             70
008300140919     D wRSD1           S            256
008400140919     D wRSD2           S            256
008500140919     D wRSD3           S            256
008600120411     D Data8A          s              8
008700140527     D wBarcodeN       s             11s 0
008800140527     D wX              s              3s 0
008900140711     D wRMA            s                   like(VABRMA)
009000140711     D TxtInOut        S           2048
009100140711     D ElencoChar      S            256
009200140711     D TipoElenco      S              1
009300140711     D CharSost        S              1
009400140711     D UpperCase       S              1
009500140711     D ChkNull         S              1
009600140711     D CharNull        S              1
009700140711     D EsitoBon        S              1
009800030822     D*------------------
009900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
010000030822     D*------------------
010100030822     D posDaDft        S              3  0 INZ(*zeros)
010200030822     D posADft         S              3  0 INZ(*zeros)
010300030822     D j               s              3  0 INZ(1)
010400030822     D wGiroDft        s              1  0 INZ(*zeros)
010500080418     D*------------------
010600080418     D* Costanti
010700080418     D*------------------
010800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011000100318
011100100318     D*------------------
011200100318     D* LINKING A DEFINIZIONI ESTERNE
011300100318     D*------------------
011400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
011600100318
011700010330
011800010330
011900010330
012000000913     C                   reset                   rrnum
012100990921     C                   reset                   esito
012200990921     C                   reset                   wrkesito
012300010601     C*
012400010601     C                   exsr      opeini
012500010605     C                   exsr      rwvab
012600010601     C*
012700010601     C                   seton                                        lr
012800010601
012900010601
013000010601
013100010601     C*--------------------------------------------------------
013200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013300010601     C*--------------------------------------------------------
013400010601     C     PREELA        BEGSR
013500010601     C*
013600010601     C* SE OCCORRE SPEDIRE IN FILIALE
013700010601     C                   if        invfil <> *zeros and
013800010601     C                             flgGiro = '0'
013900010601     C*
014000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014100010601     C                   eval      flgGiro = '1'
014200010601     C*
014300010601     C                   endif
014400010601     C*
014500010601     C                   ENDSR
014600010601     C***
014700010601
014800010601
014900010601
015000010601
015100010601     C*--------------------------------------------------------
015200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015300010601     C*--------------------------------------------------------
015400010601     C     ENDELA        BEGSR
015500040119     C*
015600040119     C* Effettuo la chiamata al CLLE preposto
015700040119     C                   call(e)   'TITVVTC'
015800040510     C                   parm                    parccm
015900040119     C                   parm                    parmbr
016000040119     C                   parm      '2'           paropz
016100050201     C*
016200050201     C* Effettuo lancio TISI95 solo x chiusura
016300050201     C                   CLEAR                   TISI95DS
016400050201     C                   EVAL      I95TLA = 'C'
016500050201     C                   CALL      'TISI95R'
016600050201     C                   PARM                    TISI95DS
016700000616     C*
016800010601     C                   ENDSR
016900010601
017000010601
017100010330     C*--------------------------------------------------------
017200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
017300010330     C*--------------------------------------------------------
017400010605     C     RWVAB         BEGSR
017500010330     C*
017600010330     C                   if        not %open(tivin00r)
017700010330     C                   open      tivin00r
017800010330     C                   endif
017900021025     C                   if        not %open(fivabwwr)
018000021025     C                   open      fivabwwr
018100010330     C                   endif
018200040119     C*
018300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018400040119     C                   exsr      prevat
018500040119     C*
018600040119     C                   if        chkcall = '0'
018700040119     C*
018800040119     C                   if        not %open(fivatwwr)
018900040119     C                   open      fivatwwr
019000040119     C                   endif
019100010330     C*
019200040119     C                   clear                   §CTROKVB          7 0
019300040119     C                   clear                   §CTROKVT          7 0
019400040119     C                   clear                   §CTRMO            7 0
019500040119     C                   clear                   §CTRNO            7 0
019600120410     C*
019700120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019800120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019900120410     C                   IF        Intestazioni = *off
020000120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020100120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020200120410     C                   exsr      inzvar
020300120410     C                   exsr      impvab
020400120410     C                   ENDIF
020500010330     C*
020600030822     C                   do        *HIVAL
020700010330     C*
020800030822     C                   read      tivin00r                               70
020900010618     C*
021000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021100010618     C                   if        vinflg = '1'
021200010618     C                   eval      flgOk = '1'
021300010618     C                   endif
021400040510     C*
021500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021600040510     C                   z-add     *zeros        wLenVINDTA        4 0
021700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022100040510     C                   z-add     1             wFlgVINDTA
022200040510     C                   leave
022300040510     C                   endif
022400040510     C                   enddo
022500010618     C*
022600040510     C                   if        vindta > *blanks AND
022700040510     C                             wFlgVINDTA = 1
022800010330     C                   add       1             rrnum
022900010330     C*
023000010601     C                   if        *in70 = *off and
023100010330     C                             (vinflg = *blanks
023200010330     C                              or vinflg = '0'
023300010330     C                              or vinflg = '2')
023400010330     C*
023500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023600010711     C                   if        vinflg = *blanks or vinflg = '0'
023700010711     C                   clear                   vinmsg
023800010711     C                   endif
023900130917     C*
024000130917     C* Aggiungo un byte a blanks all'inizio d ogni record
024100130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
024200130917     C* evitare che vengano elisi i blank
024300140220     C***                eval      vindta  = '*' + %trim(vindta)
024400130917     C*
024500080527     C                   exsr      inzvar
024600100318     C                   exsr      normCSV
024700010605     C                   exsr      impvab
024800010601     C*
024900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025000141006     C***  elimino la gestione multifiliale perché altrimenti LNP che è calcolato dà errore
025100141006     C***                eval      depfil = VABLNP
025200141006     C***                exsr      repfil
025300141006     C***                if        depfil = invfil
025400141006     C***                if        vlrpoi = 999
025500141006     C***                move(P)   invfil        VABFGS
025600141006     C***                else
025700141006     C***                move(P)   vlrpoi        VABFGS
025800141006     C***                endif
025900010601     C*
026000010601     C                   exsr      PREELA
026100010601     C*
026200010604     C* Ebbene...
026300010604     C*
026400040119     C  N31              add       1             §CTROKVB
026500040119     C   32              add       1             §CTRMO
026600040119     C   31              add       1             §CTRNO
026700080418     C*
026800030822     C                   if        wGiro = 2
026900080513     C* Scrivo sempre il VAT
027000080513     C  N31              exsr      wrivat
027100110201     C                   exsr      chkvab
027200080513     C                   endif
027300010604     C*
027400010604     C                   if        *in31 = *off and
027500010604     C                             *in32 = *off
027600010604     C                   eval      vinflg = '1'
027700010604     C                   else
027800010604     C                   eval      vinflg = '2'
027900010604     C                   endif
028000141006     C***                endif
028100010601     C*
028200010604     C                   endif
028300010604     C*
028400010330     C                   else
028500010330     C                   eval      vinflg = '1'
028600010330     C                   endif
028700010601     C*
028800010601     C  N70              update    tivin000
028900010330     C*
029000030822     C  N70              enddo
029100040119     C*
029200080527     C* Verifico dati rimasti "in cannna"...
029300080527     C                   seton                                        65
029400110201     C                   exsr      chkvab
029500080527     C*
029600040119     C                   endif                                                  (endif - chkcall)
029700010601     C*
029800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029900010601     C                   if        cntNonEl = *zeros or
030000010601     C                             flgMulti = '0'
030100010330     C* Se non ci sono record con errori ...
030200010601     C                   if        §ctrno = 0 and
030300010604     C                             §ctrmo = 0 and
030400010601     C                             flgStato <> '2'
030500010330     C* ... restituisco esito OK.
030600010330     C                   eval      wrkesito = '0'
030700010330     C                   else
030800040119     C                   if        §ctrokvb > 0
030900010330     C                   eval      wrkesito = '1'
031000010330     C                   else
031100010615     C                   if        flgOk = '0'
031200010615     C                   eval      wrkesito = '2'
031300010615     C                   else
031400010615     C                   eval      wrkesito = '6'
031500010615     C                   endif
031600010330     C                   endif
031700010330     C                   endif
031800010601     C                   else
031900010601     C                   eval      wrkesito = '9'
032000010601     C                   endif
032100010330     C*
032200010330     C                   if        %open(tivin00r)
032300010330     C                   close     tivin00r
032400010330     C                   endif
032500021025     C                   if        %open(fivabwwr)
032600021025     C                   close     fivabwwr
032700010330     C                   endif
032800040512     C                   if        %open(fivatwwr)
032900040512     C                   close     fivatwwr
033000040512     C                   endif
033100010601     C*
033200010601     C                   if        vlrpoi <> 999
033300010601     C                   eval      invfil = vlrpoi
033400010601     C                   endif
033500010330     C*
033600040119     C                   if        §ctrokvb > 0
033700010601     C                             and invfil > *zeros
033800010330     C                   exsr      invio
033900010330     C                   endif
034000010601     C*
034100010618     C                   if        flgGiro = '1'
034200010601     C                   exsr      endela
034300010618     C                   endif
034400010330     C*
034500010330     C                   ENDSR
034600010330     C***
034700080527
034800080527
034900080527
035000080527     C*----------------------------------------------------*
035100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
035200080527     C*----------------------------------------------------*
035300100413     C     CHKVAB        BEGSR
035400110201     C*
035500110201     C* Se richiesti ignorare colli / peso / volume
035600110315 xxx C***                if        *in35
035700110315 xxx C***                z-add     *zeros        VABNCL
035800110315 xxx C***                z-add     *zeros        VABPKB
035900110315 xxx C***                z-add     *zeros        VABVLB
036000110315 xxx C***                endif
036100080527     C*
036200080527     C* Scrivo il VAB solo a rottura d spedizione
036300080527     C                   if        depspe = *zeros
036400140930
036500140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
036600140930      * spedizione
036700140930     C  N31              eval      vabncl = 1
036800140930
036900080527     C  N31              eval      fivabds_sav = fivabds
037000080527     C                   eval      depspe = vabNSP
037100080527     C                   else
037200080527     C                   if        depspe = vabNSP AND *in65 = *off
037300140930
037400140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
037500140930      * spedizione
037600140930     C                   eval      vabncl += 1
037700140930
037800100413     C   51              add       VABNCL        sav_VABNCL
037900100413     C   51              add       VABPKB        sav_VABPKB
038000100413     C   51              add       VABVLB        sav_VABVLB
038100100720     C   52              z-add     VABNCL        sav_VABNCL
038200100720     C   52              z-add     VABPKB        sav_VABPKB
038300100720     C   52              z-add     VABVLB        sav_VABVLB
038400100720     C   53              add       VABNCL        sav_VABNCL
038500100720     C   53              z-add     VABPKB        sav_VABPKB
038600100720     C   53              z-add     VABVLB        sav_VABVLB
038700080527     C                   else
038800080527     C  N31              eval      fivabds_ok = fivabds_sav
038900080527     C  N31              exsr      wrivab
039000080527     C  N31              eval      fivabds_sav = fivabds
039100140930
039200140930      * - Valorizzare VABNCL incrementalmente (+1) per ogni record avente medesimo numero di
039300140930      * spedizione
039400140930     C  N31              eval      vabncl = 1
039500140930
039600080527     C                   eval      depspe = vabNSP
039700080527     C                   endif
039800080527     C                   endif
039900080527     C*
040000080527     C                   ENDSR
040100040119
040200040119
040300040119
040400040119     C*----------------------------------------------------*
040500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
040600040119     C*----------------------------------------------------*
040700040510     C     WRIVAB        BEGSR
040800080513     C*
040900080513     C* Gestisco l'eventuale rottura x numero spedizione
041000080527     C                   if        VABNSP <> depspe or *in65 = *on
041100070301     C                   if        wVATNOT_E <> *blanks
041200130610     C                   eval      ok_VABCTM = '7Q'
041300070301     C                   endif
041400100413     C                   write     fivab000
041500080513     C                   endif
041600040119     C*
041700040119     C                   ENDSR
041800040119
041900040119
042000040119
042100040119     C*----------------------------------------------------*
042200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
042300040119     C*----------------------------------------------------*
042400040510     C     WRIVAT        BEGSR
042500060512     C*
042600060512     C* Inizializzo i campi chiave
042700060512     C                   eval      VATFGS = VABFGS
042800060512     C                   eval      VATAAS = VABAAS
042900060512     C                   eval      VATCCM = VABCCM
043000070823     C                   eval      VATNRS = VABNRS
043100060512     C                   eval      VATNSP = VABNSP
043200060512     C                   eval      VATLNP = VABLNP
043300040119     C*
043400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043500040510     C                   if        VATTRC = *blanks
043600040510     C                   eval      VATTRC = 'E'
043700040510     C                   endif
043800110318     C                   if        VATNOT <> *blanks
043900040510     C                   write     FIVAT000
044000040119     C                   add       1             §CTROKVT
044100110318     C                   endif
044200060331     C*
044300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
044400060331     C                   if        wVATNOT_A <> *blanks
044500060331     C                   eval      VATNOT = wVATNOT_A
044600060331     C                   eval      VATTRC = 'A'
044700060331     C                   write     FIVAT000
044800060331     C                   add       1             §CTROKVT
044900060331     C                   endif
045000060331     C*
045100060331     C                   if        wVATNOT_B <> *blanks
045200060331     C                   eval      VATNOT = wVATNOT_B
045300060331     C                   eval      VATTRC = 'B'
045400060331     C                   write     FIVAT000
045500060331     C                   add       1             §CTROKVT
045600060331     C                   endif
045700140711     C*
045800140711     C                   if        wVATNOT_IJ <> *blanks
045900140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
046000140711     C                   eval      VATTRC = 'I'
046100140711     C                   write     FIVAT000
046200140711     C                   add       1             §CTROKVT
046300140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
046400140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046500140711     C                   eval      VATTRC = 'J'
046600140711     C                   write     FIVAT000
046700140711     C                   endif
046800140711     C                   endif
046900060331     C*
047000060331     C                   if        wVATNOT_E <> *blanks
047100060331     C                   eval      VATTRC = 'E'
047200140527     C*
047300140711     C* NON Sviluppare i "CHI SONO"
047400140527     C*
047500140711     C                   EVAL      VATNOT = wVATNOT_E
047600140711     C                   write     FIVAT000
047700140711     C                   add       1             §CTROKVT
047800120320     C                   endif
047900110927     C*
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
058400100413     C                   SELECT
058500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058600100720     C                   SETON                                        50
058700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058800100413     C                   SETON                                        51
058900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059000100720     C                   SETON                                        52
059100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059200100720     C                   SETON                                        53
059300100413     C                   ENDSL
059400030822     C*
059500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059600030822     C                   EVAL      posDaDft = 1
059700030822     C                   EVAL      posADft  = 0
059800030822     C                   EVAL      wGiroDft = 0
059900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060000030822     C                             posDaDft > 0
060100030822     C*
060200030822     C* Gestisco il 1° giro
060300030822     C                   IF        wGiroDft = 0
060400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060600030822     C* Incremento il contatore dei "giri"
060700030822     C                   EVAL      wGiroDft = 1
060800030822     C                   ELSE
060900030822     C                   EVAL      posDaDft = posADft
061000030822     C                   ENDIF
061100030822     C* Eseguo lo scan x trovare la fine del campo corrente
061200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061300030822     C*
061400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061600030822     C                   IF        posDaDft > 0 AND
061700030822     C                             posADft  > 0
061800030822     C* NCL
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'NCL'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiInt=*on
062800030822     C                   Z-ADD     PiVal         VABNCL
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* CCM
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'CCM'
063600030822     C                   EVAL      PiStr=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   EXSR      CHKNUM
064000030822     C                   IF        PiInt=*on
064100030822     C                   Z-ADD     PiVal         VABCCM
064200030822     C                   ENDIF
064300030822     C                   ENDIF
064400030822     C* LNP
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'LNP'
064900030822     C                   EVAL      PiStr=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   EXSR      CHKNUM
065300030822     C                   IF        PiInt=*on
065400030822     C                   Z-ADD     PiVal         VABLNP
065500030822     C                   ENDIF
065600030822     C                   ENDIF
065700030822     C* NRS
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'NRS'
066200030822     C                   EVAL      PiStr=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   EXSR      CHKNUM
066600030822     C                   IF        PiInt=*on
066700030822     C                   Z-ADD     PiVal         VABNRS
066800030822     C                   ENDIF
066900030822     C                   ENDIF
067000030822     C* CTR
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'CTR'
067500030822     C                   EVAL      PiStr=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   EXSR      CHKNUM
067900030822     C                   IF        PiInt=*on
068000030822     C                   Z-ADD     PiVal         VABCTR
068100030822     C                   ENDIF
068200030822     C                   ENDIF
068300030822     C* PKB
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'PKB'
068800030822     C                   EVAL      PiStr=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   EXSR      CHKNUM
069200030822     C                   IF        PiNum=*on
069300130610     C                   Z-ADD     PiVal         VABPKB
069400030822     C                   ENDIF
069500030822     C                   ENDIF
069600030822     C* VLB
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VLB'
070100030822     C                   EVAL      PiStr=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   EXSR      CHKNUM
070500030822     C                   IF        PiNum=*on
070600130610     C                   Z-ADD     PiVal         VABVLB
070700030822     C                   ENDIF
070800030822     C                   ENDIF
070900030822     C* QFT
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'QFT'
071400030822     C                   EVAL      PiStr=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   EXSR      CHKNUM
071800030822     C                   IF        PiNum=*on
071900030822     C                   Z-ADD     PiVal         VABQFT
072000030822     C                   ENDIF
072100030822     C                   ENDIF
072200030822     C* CBO
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'CBO'
072700030822     C                   EVAL      VABCBO=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* TSP
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'TSP'
073600030822     C                   EVAL      VABTSP=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* VAS
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'VAS'
074500030822     C                   EVAL      VABVAS=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* VCA
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'VCA'
075400030822     C                   EVAL      VABVCA=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800070925     C* TIC
075900070925     C                   IF        %subst(
076000070925     C                             %subst(vlrppt:posDaDft+1:
076100070925     C                             posADft-posDaDft-1):1:3)
076200070925     C                             = 'TIC'
076300070925     C                   EVAL      VABTIC=%trim(%subst(
076400070925     C                             %subst(vlrppt:posDaDft+1:
076500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600070925     C                   ENDIF
076700030822     C* GCA
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'GCA'
077200030822     C                   EVAL      VABGCA=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* CTM
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'CTM'
078100030822     C                   EVAL      VABCTM=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* FFD
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'FFD'
079000030822     C                   EVAL      VABFFD=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* VAD
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'VAD'
079900030822     C                   EVAL      VABVAD=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* GMA
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'GMA'
080800030822     C                   EVAL      VABGMA=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* GGA
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'GGA'
081700030822     C                   EVAL      VABGGA=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100030822     C* GVA
082200030822     C                   IF        %subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):1:3)
082500030822     C                             = 'GVA'
082600030822     C                   EVAL      VABGVA=%trim(%subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900030822     C                   ENDIF
083000030822     C* TC1
083100030822     C                   IF        %subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):1:3)
083400030822     C                             = 'TC1'
083500030822     C                   EVAL      VABTC1=%trim(%subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083800030822     C                   ENDIF
083900030822     C* TC2
084000030822     C                   IF        %subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):1:3)
084300030822     C                             = 'TC2'
084400030822     C                   EVAL      VABTC2=%trim(%subst(
084500030822     C                             %subst(vlrppt:posDaDft+1:
084600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084700030822     C                   ENDIF
084800040714     C* VATTRC
084900040714     C                   IF        %subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):1:3)
085200040714     C                             = 'TRC'
085300040714     C                   EVAL      VATTRC=%trim(%subst(
085400040714     C                             %subst(vlrppt:posDaDft+1:
085500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085600040714     C                   ENDIF
085700030822     C* ...
085800030822     C                   ENDIF
085900030822     C                   ENDDO
086000020204     C*
086100010330     C                   ENDSR
086200010607     C*----------------------------------------------------*
086300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086400010607     C*----------------------------------------------------*
086500010607     C     IMPVAB        BEGSR
086600010607     C*
086700010607     C                   SETOFF                                       3132
086800110315 xxx C***                SETON                                        35
086900030515     C*
087000010607     C                   MOVEL     datcor        VABAAS
087100010607     C                   MOVE      datcor        VABMGS
087200010607     C*
087300030715     C**********
087400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087500030715     C**********
087600030715     C                   IF        wGiro = *zeros
087700040113     C*
087800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088100080429     C                             %trim(intcol(5))<>*blanks
088200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088400080429     C                                      %trim(intcol(5))
088500070823     C     ';':CharCSV   XLATE     vindta        vindta
088600040510     C                   ENDIF
088700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088800031201     C     minu:maiu     XLATE     vindta        vindta
088900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089000030715     C                   DOW       posDa <= %len(%trim(vindta))
089100030715     C*
089200030715     C* Gestisco il 1° campo
089300030715     C                   IF        i = 1
089400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089500030715     C                   EVAL      posDa = 1
089600030715     C* Eseguo lo scan x trovare la fine del primo campo
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C* X i campi successivi al 1°
090200030715     C                   ELSE
090300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090400030715     C                   EVAL      posDa = posA + 1
090500030715     C* Eseguo lo scan x trovare la fine del campo corrente
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* Gestisco l'ultimo campo
090800030715     C                   IF        posA = *zeros
090900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091000030715     C* A questo punto "estraggo" il campo corrente
091100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091200030715     C                                                    (posA-posDa))
091300030715     C                   LEAVE
091400030715     C                   ELSE
091500030715     C* A questo punto "estraggo" il campo corrente
091600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091700030715     C                                                    (posA-posDa))
091800030715     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitFLD)
092800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200030715     C                   EVAL      wGiro = 1
093300030715     C                   ELSE
093400030715     C                   EVAL      wGiro = 2
093500031204     C**********
093600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093700031204     C* d separatore campo
093800031204     C**********
093900031204     C                   EVAL      vindta = %trim(vindta)
094000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094100031204     C                   EVAL      vindta = %subst(vindta:2)
094200031204     C                   ENDDO
094300031204     C*
094400031204     C                   Z-ADD     *zeros        lunghInput        4 0
094500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094900110912     C                   ENDDO
095000030715     C**********
095100030715     C* Effettuo lo split del campo dati d input
095200030715     C**********
095300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095400030715     C                   DOW       posDa <= %len(%trim(vindta))
095500030715     C*
095600030715     C* Gestisco il 1° campo
095700030715     C                   IF        i = 1
095800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095900030715     C                   EVAL      posDa = 1
096000030715     C* Eseguo lo scan x trovare la fine del primo campo
096100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096200030715     C* A questo punto "estraggo" il campo corrente
096300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096400030715     C                                                    (posA-posDa))
096500030715     C* X i campi successivi al 1°
096600030715     C                   ELSE
096700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096800030715     C                   EVAL      posDa = posA + 1
096900031204     C* Verifico che nn vi sia il campo nn valorizzato
097000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097100031204     C* Se campo nn valorizzato skippo al prossimo
097200031204     C                   EVAL      SkSplitCSV(i) = *blanks
097300031204     C                   EVAL      posA  = posA + 1
097400031204     C                   ELSE
097500030715     C* Eseguo lo scan x trovare la fine del campo corrente
097600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097700030715     C* Gestisco l'ultimo campo
097800030715     C                   IF        posA = *zeros
097900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098000030715     C* A questo punto "estraggo" il campo corrente
098100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098200030715     C                                                    (posA-posDa))
098300030715     C                   LEAVE
098400030715     C                   ELSE
098500030715     C* A questo punto "estraggo" il campo corrente
098600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098700030715     C                                                    (posA-posDa))
098800030715     C                   ENDIF
098900031204     C                   ENDIF
099000030715     C                   ENDIF
099100030715     C* Incremento il contatore d campo
099200030715     C                   EVAL      i = i +1
099300030715     C                   ENDDO
099400030715     C*
099500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099600030715     C* delimitazione testo
099700030715     C                   EVAL      i = 1
099800030715     C                   DOW       i < %elem(SkSplitCSV)
099900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100100030715     C                   EVAL      i = i + 1
100200030715     C                   ENDDO
100300100720     C*
100400100720     C* Stacco un progressivo x numero spedizione
100500141006 xxx C                   EXSR      repNSP
100600100326     C*
100700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100800030715     C                   EVAL      i = 1
100900030715     C                   DOW       i < %elem(SkSplitFLD)
101000030715     C*
101100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101300030715     C                             SkSplitCSV(i) <> *blanks
101400030721     C*** ATB
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101600030721     C                   EVAL      VABATB = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030715     C*** RSD
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102300030715     C* ==
102400130528     C                   ENDIF
102500030721     C*** RD2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030715     C*** IND
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103100030715     C                   EVAL      VABIND = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** LOD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** PRD
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030721     C*** NZD
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030715     C*** RMA
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NOT
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** NT2
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VCA
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** VAS
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030715     C*** FFD
106600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106800030715     C                   ENDIF
106900030721     C*** GC1
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** GC2
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** TSP
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108000030721     C                   ENDIF
108100040510     C*** CBO
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108400040510     C                   ENDIF
108500030721     C*** NAS
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TIC
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** GCA
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** XCO
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** CTM
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** TCR
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** CTS
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** FTM
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** VAD
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GMA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GGA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** GVA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC1
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** TC2
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** SCL
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** RMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*** NMO
115000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115200030721     C                   ENDIF
115300030721     C*
115400030721     C* Reperisco quindi i campi numerici...
115500040510     C*** NSP
115600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115700040510     C                   EVAL      PiStr=SkSplitCSV(i)
115800040510     C                   EXSR      CHKNUM
115900040510     C                   IF        PiInt=*on
116000040510     C                   Z-ADD     PiVal         VABNSP
116100040510     C                   ELSE
116200040510     C                   SETON                                        31
116300040510     C                   EVAL      VABNSP = *zeros
116400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040510     C                             + ' ' + 'VABNSP'
116600040510     C                   ENDIF
116700040510     C                   ENDIF
116800071106     C*** RMN
116900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117000071106     C                   EVAL      PiStr=SkSplitCSV(i)
117100071106     C                   EXSR      CHKNUM
117200071106     C                   IF        PiInt=*on
117300071106     C                   Z-ADD     PiVal         VABRMN
117400071106     C                   ELSE
117500071106     C                   SETON                                        32
117600071106     C                   EVAL      VABRMN = *zeros
117700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117800071106     C                             + ' ' + 'VABRMN'
117900071106     C                   ENDIF
118000071106     C                   ENDIF
118100030721     C*** CAD
118200130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118300130610     C                   EVAL      PiStr=SkSplitCSV(i)
118400130610     C                   EXSR      CHKNUM
118500130610     C                   IF        PiInt=*on
118600130610     C                   Z-ADD     PiVal         Num5_0
118700130610     C                   MOVEL(p)  Num5_0        VABCAD
118800130610     C                   ELSE
118900130610     C                   SETON                                        32
119000130610     C                   EVAL      VABCAD = *zeros
119100130610     C                   EVAL      vinmsg = %trimr(vinmsg)
119200130610     C                             + ' ' + 'VABCAD'
119300130610     C                   ENDIF
119400130605     C                   ENDIF
119500030721     C*** NCL
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on
120000030721     C                   Z-ADD     PiVal         VABNCL
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      VABNCL = *zeros
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500040122     C                             + ' ' + 'VABNCL'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** PKB
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300130610     C                   Z-ADD(H)  PiVal         VABPKB
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      VABPKB = *zeros
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABPKB'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** CAS
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122500140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122600140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122700140711     C                             %trim(SkSplitCSV(i)) <> ',00'
122800130703     C                   EVAL      VABVCA = 'EUR'
122900140930     C                   EVAL      VABTIC = *blank
123000140513     C                   EVAL      wFlgCAS = '1'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABCAS
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABCAS = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABCAS'
124000030721     C                   ENDIF
124100140513     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** CCM
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on      AND
124800030721     C                             PiVal<=9999999 AND
124900030721     C                             PiVal>*zeros
125000030721     C                   Z-ADD     PiVal         VABCCM
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      VABCCM = *zeros
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500040510     C                             + ' ' + 'VABCCM'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** LNP
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on    AND
126300030721     C                             PiVal<=999   AND
126400030721     C                             PiVal>*zeros
126500030721     C                   Z-ADD     PiVal         VABLNP
126600030721     C                   ELSE
126700040715     C                   SETON                                        32
126800030721     C                   EVAL      VABLNP = *zeros
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000040510     C                             + ' ' + 'VABLNP'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** NRS
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on AND
127800030721     C                             PiVal<=99
127900030721     C                   Z-ADD     PiVal         VABNRS
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABNRS'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** LNA
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on    AND
129100030721     C                             PiVal<=999   AND
129200030721     C                             PiVal>*zeros
129300030721     C                   Z-ADD     PiVal         VABLNA
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      VABLNA = *zeros
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABLNA'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** CTR
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on  AND
130600030721     C                             PiVal<=999
130700030721     C                   Z-ADD     PiVal         VABCTR
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABCTR'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** IAS
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131600141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131700141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131800141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131900141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132000141003     C                             %trim(SkSplitCSV(i)) <> ',00'
132100141006     C                   EVAL      VABVAS = 'EUR'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABIAS
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABIAS'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200141003     C                   ENDIF
133300030721     C*** VLB
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiNum=*on
133800130610     C                   Z-ADD(H)  PiVal         VABVLB
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABVLB'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** QFT
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiNum=*on
135000030721     C                   Z-ADD     PiVal         VABQFT
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABQFT'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** NCD
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on      AND
136200030721     C                             PiVal<=9999999
136300030721     C                   Z-ADD     PiVal         VABNCD
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABNCD'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** NCA
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on      AND
137500030721     C                             PiVal<=9999999
137600030721     C                   Z-ADD     PiVal         VABNCA
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABNCA'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** ZNC
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on AND
138800030721     C                             PiVal<=99
138900030721     C                   Z-ADD     PiVal         VABZNC
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139300030721     C                             + ' ' + 'VABZNC'
139400030721     C                   ENDIF
139500030721     C                   ENDIF
139600030721     C*** DCR
139700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139800030721     C                   EVAL      PiStr=SkSplitCSV(i)
139900030721     C                   EXSR      CHKNUM
140000030721     C                   IF        PiInt=*on       AND
140100030721     C                             PiVal<=99999999
140200030721     C                   Z-ADD     PiVal         VABDCR
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABDCR'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** HCR
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on   AND
141400030721     C                             PiVal<=9999
141500030721     C                   Z-ADD     PiVal         VABHCR
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141900030721     C                             + ' ' + 'VABHCR'
142000030721     C                   ENDIF
142100030721     C                   ENDIF
142200030721     C*** VMD
142300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142400030721     C                   EVAL      PiStr=SkSplitCSV(i)
142500030721     C                   EXSR      CHKNUM
142600030721     C                   IF        PiNum=*on
142700030721     C                   Z-ADD     PiVal         VABVMD
142800030721     C                   ELSE
142900030721     C                   SETON                                        32
143000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143100030721     C                             + ' ' + 'VABVMD'
143200030721     C                   ENDIF
143300030721     C                   ENDIF
143400030721     C*** ANT
143500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143600030721     C                   EVAL      PiStr=SkSplitCSV(i)
143700030721     C                   EXSR      CHKNUM
143800030721     C                   IF        PiInt=*on        AND
143900030721     C                             PiVal<=999999999
144000030721     C                   Z-ADD     PiVal         VABANT
144100030721     C                   ELSE
144200030721     C                   SETON                                        32
144300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144400030721     C                             + ' ' + 'VABANT'
144500030721     C                   ENDIF
144600030721     C                   ENDIF
144700030721     C*** CMO
144800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144900030721     C                   EVAL      PiStr=SkSplitCSV(i)
145000030721     C                   EXSR      CHKNUM
145100030721     C                   IF        PiInt=*on
145200030721     C                   Z-ADD     PiVal         Num5_0
145300030721     C                   MOVEL(p)  Num5_0        VABCMO
145400030721     C                   ELSE
145500030721     C                   SETON                                        32
145600030721     C                   EVAL      VABCMO = *zeros
145700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145800030721     C                             + ' ' + 'VABCMO'
145900030721     C                   ENDIF
146000030721     C                   ENDIF
146100040510     C*
146200040510     C* Reperisco i campi del FIVAT
146300040510     C*** VATNOT
146400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146600040510     C                   ENDIF
146700040510     C*** VATTRC
146800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147000040510     C                   ENDIF
147100060331     C*** VATNOT_A
147200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147400060331     C                   ENDIF
147500060331     C*** VATNOT_B
147600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147800060331     C                   ENDIF
147900060331     C*** VATNOT_E
148000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148200060331     C                   ENDIF
148300110927     C*** VATNOT_I
148400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148600110927     C                   ENDIF
148700141003     C*** VATNOT_IJ
148800141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148900141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149000141003     C                   ENDIF
149100140319     C*** VATNOT_S
149200140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149300140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149400140319     C                   ENDIF
149500070823     C*
149600100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149700180201     C*
149800180201     C*** VATNOT_B/_S
149900180201     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_S'
150000180201     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
150100180201     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
150200180201     C                   ENDIF
150300180201     C*
150400180201     C*** VATTC1
150500180201     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTC1'
150600180201     C                   IF        SkSplitCSV(i) <> *blank
150700180201     C                   EVAL      VABTC1 = 'A'
150800180201     C                   ENDIF
150900180201     C                   ENDIF
151000070925     C*
151100141003     C*** VABLNP/FGS
151200141006      * LNP lo reperisco cercando nella schiera Cod l'elemento che coincide con i primi 3 char del
151300141003      * barcode; con quell'indice reperisco l'elemento dalla schiera Fil.
151400141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
151500141006     C* estraggo il barcode come un campo di 11, lo riempio di 0 e ne prendo solo i primi 3 char
151600141006     C                   EVAL      w11 = *zeros
151700180201     C***                EVALR     w11 = %trim(SkSplitCSV(i))
151800141006     C                   MONITOR
151900141006     C                   EVALR     %subst(w11 : 12 - %len(%trim(SkSplitCSV(i)))
152000141006     C                              : %len(%trim(SkSplitCSV(i)))) =
152100141006     C                              %trim(SkSplitCSV(i))
152200141006     C                   EVAL      VABLNP = %dec( Fil(
152300141006     C                             %lookup(
152400141006     C                             %subst(w11 : 1 : 3)
152500141006     C                             : cod)) : 3 : 0)
152600141006     C                   EVAL      VABFGS=VABLNP
152700141006     C                   ON-ERROR
152800141006     C                   SETON                                        31
152900141006     C                   EVAL      vinmsg = %trimr(vinmsg)
153000141006     C                             + ' ' + 'VABLNP VATLNP VABFGS'
153100141006     C                   ENDMON
153200141006     C                   ENDIF
153300141003     C*
153400071029     C***  <===  -----------------------------------------
153500040510     C*
153600030715     C                   ENDIF
153700030715     C                   EVAL      i = i + 1
153800030715     C                   ENDDO
153900070925     C*
154000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
154100140319     C*
154200180201     C* se non ho richiesta di appuntamento, non salvo neanche il telefono
154300180201     C                   IF        VABTC1 <> 'A'
154400180201     C                   EVAL      wVATNOT_B = *blank
154500180201     C                   ENDIF
154600070925     C*
154700141003
154800070925     C*
154900070925     C* Considerazioni finali su CBO/CAS
155000140220     C                   IF        wFlgCAS = '1'    OR
155100140220     C                             VABCAS  > *zeros
155200140220     C                   IF        VABCBO = '1'
155300140220     C                   EVAL      VABCBO = '4'
155400140220     C                   ENDIF
155500140220     C                   IF        VABCBO = '2'
155600140220     C                   EVAL      VABCBO = '6'
155700140220     C                   ENDIF
155800140220     C                   ENDIF
155900070925     C***  <===  ----------------------------
156000040510     C*
156100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
156200040510     C                   IF        VABNSP = *zeros
156300040510     C                   SETON                                        31
156400040510     C                   ENDIF
156500040715     C*
156600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
156700040715     C                   IF        VABLNP = *zeros
156800040715     C                   SETON                                        31
156900040715     C                   ENDIF
157000040510     C*
157100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
157200120411     C                   IF        VABRMN = *zeros
157300120411     C                   Z-ADD     VABNSP        VABRMN
157400120411     C                   ENDIF
157500120328     C* IN QUESTO CASO LO FORZO = 1
157600120411     C***                IF        VABRMN = *zeros
157700120411     C***                Z-ADD     1             VABRMN
157800120411     C***                ENDIF
157900040510     C*
158000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
158100040510     C                   IF        VABRMA = *blanks
158200040714     C                   MOVEL     VABRMN        VABRMA
158300040510     C                   ENDIF
158400040128     C*
158500040510     C* Se provincia nn valorizzata la reperisco
158600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
158700110912     C                   IF        VABPRD =  *blanks AND
158800110912     C                             VABNZD =  *blanks AND
158900110912     C                             VABCAD <> *blanks AND
159000110912     C                             VABLOD <> *blanks
159100040128     C                   CLEAR                   TISI95DS
159200040128     C                   EVAL      I95TCN = '3'
159300040128     C                   Z-ADD     datcor        I95DAT
159400040128     C                   EVAL      I95NAR = VABNZD
159500040128     C                   EVAL      I95CAP = VABCAD
159600040128     C                   EVAL      I95LOC = VABLOD
159700040128     C                   CALL      'TISI95R'
159800040128     C                   PARM                    TISI95DS
159900040128     C                   EVAL      VABPRD = O95PRV
160000040510     C                   ENDIF
160100020204     C*
160200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
160300020204     C                   EXSR      CHKIMPDIV
160400030715     C                   ENDIF
160500020204     C*
160600010607     C                   ENDSR
160700010607     C*----------------------------------------------------*
160800010601
160900020204
161000020204     C*----------------------------------------------------*
161100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
161200020204     C*----------------------------------------------------*
161300020204     C     CHKIMPDIV     BEGSR
161400020204     C*
161500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
161600020204     C                   Z-ADD     *zeros        wrkDec            9 9
161700020204     C*
161800020204     C* Come prima cosa effettuo considerazioni sulla divisa
161900020204     C                   IF        vabIAS > *zeros
162000020204     C                   IF        vabVAS <> 'EUR'
162100020204     C                   EVAL      vabVAS =  'ITL'
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C                   IF        vabCAS > *zeros
162600020204     C                   IF        vabVCA <> 'EUR'
162700020204     C                   EVAL      vabVCA =  'ITL'
162800020204     C                   ENDIF
162900020204     C                   ENDIF
163000020204     C*
163100020204     C                   IF        vabVMD > *zeros
163200020204     C                   IF        vabVAD <> 'EUR'
163300020204     C                   EVAL      vabVAD =  'ITL'
163400020204     C                   ENDIF
163500020204     C                   ENDIF
163600020204     C*
163700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
163800020204     C                   Z-ADD     vabIAS        wrkDec
163900020204     C                   IF        wrkDec > *zeros
164000020204     C                   IF        vabVAS = 'ITL'
164100020204     C                   EVAL      vabIAS = *zeros
164200020204     C                   ENDIF
164300020204     C                   ENDIF
164400020204     C*
164500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
164600020204     C                   Z-ADD     vabCAS        wrkDec
164700020204     C                   IF        wrkDec > *zeros
164800020204     C                   IF        vabVCA = 'ITL'
164900020204     C                   EVAL      vabCAS = *zeros
165000020204     C                   ENDIF
165100020204     C                   ENDIF
165200020204     C*
165300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
165400020204     C                   Z-ADD     vabVMD        wrkDec
165500020204     C                   IF        wrkDec > *zeros
165600020204     C                   IF        vabVAD = 'ITL'
165700020204     C                   EVAL      vabVMD = *zeros
165800020204     C                   ENDIF
165900020204     C                   ENDIF
166000020204     C*
166100020204     C                   ENDSR
166200020204     C***
166300010330
166400100318
166500100318
166600100318     C*----------------------------------------------------*
166700100318     C*  CONTROLLO NUMERICITA' CAMPI
166800100318     C*----------------------------------------------------*
166900100318     C     CHKNUM        BEGSR
167000100318     C*
167100100318     C                   IF        PiDecChr = *blanks
167200100318     C                   EVAL      PiDecChr = CharNUM
167300100318     C                   ENDIF
167400100318     C*
167500100318     C                   callp     UBISNUM_Check(PiStr
167600100318     C                                          :PiDecChr
167700100318     C                                          :PiVal
167800100318     C                                          :PiNum
167900100318     C                                          :PiInt)
168000100318     C*
168100100318     C                   ENDSR
168200100318     C***
168300010601
168400010601
168500010601
168600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
168700141006      ***
168800141006     C***  repfil        BEGSR
168900141006     C***
169000141006     C***                if        invfil = *zeros and
169100141006     C***                          depfil > *zeros and
169200141006     C***                          (vinflg = *blanks or
169300141006     C***                           vinflg = *zeros)
169400141006     C***
169500141006     C***                eval      invfil = depfil
169600141006     C***                endif
169700141006     C***
169800141006     C***                if        depfil <> invfil and
169900141006     C***                          invfil > *zeros
170000141006     C***                eval      flgMulti = '1'
170100141006     C***                if        vinflg = *blanks
170200141006     C***                add       1             cntNonEl
170300141006     C***                endif
170400141006     C***                endif
170500141006     C***
170600141006     C***                if        vinflg = '2'
170700141006     C***                eval      flgStato = '2'
170800141006     C***                endif
170900141006     C***
171000141006     C***                ENDSR
171100010601     C***
171200010601
171300010601
171400010601
171500010330
171600040119      /TITLE Invio dei dati al punto operativo.
171700040119     C     invio         BEGSR
171800040119     C*
171900040119     C* 1° invio FIVAT
172000040119     C                   reset                   dscmz
172100040119     C                   move      vlrpoi        cmzdst
172200040119     C                   eval      cmzfld = 'FIVATWWR'
172300040119     C                   eval      cmzmbd = vlrhdl
172400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172500040119     C***                if        prmfir = *blanks
172600040119     C                   eval      cmzfla = 'FIVAT00F'
172700040119     C                   eval      cmzmba = 'FIVAT00F'
172800040119     C***                else
172900040119     C***                eval      cmzfla = prmfir
173000040119     C***                eval      cmzmba = prmfir
173100040119     C***                endif
173200040119     C                   eval      cmznrr = *zeros
173300040119     C                   move      §ctrokvt      cmznrr
173400040119     C                   eval      cmzlba = vlrfl1
173500040119     C                   call(e)   'TIS711C'
173600040119     C                   parm                    dscmz
173700040119     C                   parm      *blanks       esito
173800040119     C                   if        %error
173900040119     C                             or cmzerr = '1'
174000040119     C                             or esito  = '1'
174100040119     C                   eval      wrkesito = '3'
174200040119     C                   else
174300040119     C*
174400040119     C* 2° invio FIVAB
174500040119     C                   reset                   dscmz
174600040119     C                   move      vlrpoi        cmzdst
174700040119     C                   eval      cmzfld = vlrfou
174800040119     C                   eval      cmzmbd = vlrhdl
174900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
175000040119     C***                if        prmfir = *blanks
175100040119     C                   eval      cmzfla = 'FIVAB00F'
175200040119     C                   eval      cmzmba = 'FIVAB00F'
175300040119     C***                else
175400040119     C***                eval      cmzfla = prmfir
175500040119     C***                eval      cmzmba = prmfir
175600040119     C***                endif
175700040119     C                   eval      cmznrr = *zeros
175800040119     C                   move      §ctrokvb      cmznrr
175900040119     C                   eval      cmzlba = vlrfl1
176000040119     C                   call(e)   'TIS711C'
176100040119     C                   parm                    dscmz
176200040119     C                   parm      *blanks       esito
176300040119     C                   if        %error
176400040119     C                             or cmzerr = '1'
176500040119     C                             or esito  = '1'
176600040119     C                   eval      wrkesito = '3'
176700040119     C                   endif
176800040119     C                   endif
176900040119     C*
177000040119     C                   ENDSR
177100040119     C***
177200010601
177300010601
177400010601
177500010601
177600010601      /TITLE Invio dei dati al punto operativo.
177700010601     C     opeini        BEGSR
177800010601     C*
177900010601     C* Inizializzo flag e contatori operativi
178000010601     C                   movel     '0'           flgGiro           1
178100010601     C                   movel     '0'           flgMulti          1
178200010601     C                   movel     '1'           flgStato          1
178300010615     C                   movel     '0'           flgOk             1
178400010601     C                   z-add     *zeros        cntNonEl         10 0
178500010601     C                   z-add     *zeros        depfil            3 0
178600010601     C                   z-add     *zeros        invfil            3 0
178700010601     C*
178800010601     C                   ENDSR
178900010601     C***
179000070326
179100070326
179200070326
179300070326
179400070326     C     *pssr         BEGSR
179500070329     C*
179600070329     C                   if        %open(tivin00r)
179700070329     C                   close     tivin00r
179800070329     C                   endif
179900070329     C                   if        %open(fivabwwr)
180000070329     C                   close     fivabwwr
180100070329     C                   endif
180200070329     C                   if        %open(fivatwwr)
180300070329     C                   close     fivatwwr
180400070329     C                   endif
180500070326     C*
180600070326     C* Effettuo la chiamata al CLLE preposto
180700070326     C                   call(e)   'TITVVTC'
180800070326     C                   parm                    parccm
180900070326     C                   parm                    parmbr
181000070326     C                   parm      '2'           paropz
181100070326     C*
181200070326     C                   eval      wrkesito = '2'
181300070404     C*
181400070404     C                   seton                                        LR
181500070326     C*
181600070326     C                   ENDSR     '*CANCL'
181700070326     C***
181800100318
181900100318
182000100318
182100100318     C     normCSV       BEGSR
182200100318     C*
182300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
182400100318     C* separatori campi
182500100318     C                   if        CharTXT <> *blanks
182600100318     C                   z-add     *zeros        wLenVINDTA        4 0
182700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
182800100318     C                   setoff                                       80
182900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
183000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
183100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
183200100318     C                   if        not *in80
183300100318     C                   seton                                        80
183400100318     C                   else
183500100318     C                   setoff                                       80
183600100318     C                   endif
183700100318     C                   endif
183800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
183900100318     C                             and *in80 = *on
184000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
184100100318     C                   endif
184200100318     C                   enddo
184300100318     C                   endif
184400100318     C*
184500100318     C                   ENDSR
184600100318     C***
184700070326
184800100326
184900100326
185000100326     C*----------------------------------------------------*
185100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
185200100326     C*----------------------------------------------------*
185300100326     C     REPNSP        BEGSR
185400100326     C*
185500100326     C* NSP => Stacco un numeratore da AZNUM
185600100326     C                   clear                   TRUL33DS
185700100326     C                   eval      I33OPE = *zeros
185800100326     C                   eval      I33CNU = 302
185900100326     C                   eval      I33NUM = 1
186000100326     C                   movel     TRUL33DS      KPJBU
186100100326     C                   call      'TRUL33R'
186200100326     C                   parm                    KPJBA
186300100326     C                   movel     KPJBU         TRUL33DS
186400100326     C                   if        O33ERR = *zeros
186500100326     C                   z-add     O33NRF        VABNSP
186600100326     C                   z-add     O33NRF        VATNSP
186700100326     C                   else
186800100326     C                   SETON                                        31
186900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
187000140711     C                             + ' ' + 'VABNSP VATNSP'
187100100326     C                   endif
187200100326     C*
187300100326     C                   ENDSR
187400010330
187500100326
187600010330
187700000613     C     *inzsr        BEGSR
187800990910     C*
187900990910     C     *entry        plist
188000990920     C                   parm                    tivlrds
188100990921     C                   parm      wrkesito      esito
188200000724     C                   parm                    prmlit
188300000710     C                   parm                    prmfir
188400010330     C*
188500010330     C* CALCOLA LA DATA CORRENTE
188600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
188700100318     C                   eval      datcor = %dec(%date() : *ISO)
188800000613     C*
188900000613     C                   ENDSR
189000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
189100180201VATNOT_E;X;VABRMA;X;X;X;VABPKB;VABRMN;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;W_VATNOT_S;VATNOT_IJ;
189200180201X;VABCAS;VABIAS;VABNOT;X;X;W_VABTC1;
189300140512
189400111116
189500100318
189600141003** COD - FIL - associazione codici - filiali
189700170628015 011
189800170628016 173
189900170628017 097
190000170628018 050
190100170628019 089
190200170628020 095
190300170628021 087
190400170628022 106
190500170628023 087
190600170628024 028
190700170628025 106
190800170628026 095
190900170628027 089
191000170628028 070
191100170628029 117
191200170628030 170
191300170628031 007
191400170628032 098
191500170628033 041
191600170628034 133
191700170628035 016
191800170628036 006
191900170628037 133
192000170628038 064
192100170628039 064
192200170628040 059
192300170628041 166
192400170628042 064
192500170628043 006
192600170628044 006
192700170628045 043
192800170628046 139
192900170628047 006
193000170628048 064
193100170628049 060
193200170628051 126
193300170628052 043
193400170628053 079
193500170628054 152
193600170628055 065
193700170628056 012
193800170628057 059
193900170628058 019
194000170628059 163
194100170628060 073
194200170628061 105
194300170628062 071
194400170628063 058
194500170628064 163
194600170628065 026
194700170628066 020
194800170628067 025
194900170628068 049
195000170628069 057
195100170628070 074
195200170628071 073
195300170628072 131
195400170628073 036
195500180219074 255
195600170628075 109
195700170628076 140
195800170628077 165
195900170628078 024
196000170628079 025
196100170628081 103
196200170628082 093
196300170628083 089
196400170628084 013
196500170628086 134
196600170628087 123
196700170628088 023
196800170628089 105
196900170628090 084
197000170628091 053
197100170628092 128
197200170628093 140
197300170628094 165
197400170628095 001
197500170628096 005
197600170628097 156
197700170628099 020
197800171127114 143
197900170630140 074
198000170630141 074
198100170630142 149
198200170630143 149
198300170630144 149
198400170630145 114
198500170630146 155
198600170630147 057
198700170630148 055
198800170630149 062
198900170630151 062
199000170630152 114
199100170630153 103
199200170630154 114
199300170630155 114
199400170630156 073
199500170630157 073
199600170630158 073
199700170630159 073
199800170630171 114
199900170630172 073
200000171127173 053
200100171127174 001
200200171127175 001
200300171127176 125
200400171127177 070
200500171127178 052
200600171127179 023
200700171127180 128
200800171127181 051
200900171127182 053
201000171127183 128
201100171127184 052
201200171127185 018
201300171127186 140
201400171127187 134
201500171127190 024
201600171127192 140
201700171127193 052
201800171127194 053
201900170628231 092
202000170628232 060
202100170628233 136
202200170628234 004
202300170628235 166
202400170628236 076
202500170628237 075
202600170628238 130
202700170628239 007
202800170628241 120
202900170628242 016
203000170628243 110
203100170628244 077
203200170628245 043
203300170628246 138
203400170628247 003
203500170628248 121
203600170628249 018
203700170628250 045
203800170628252 166
203900170628253 049
204000170628254 002
204100170628255 003
204200170628256 119
204300170628257 070
204400170628258 008
204500170628259 074
204600170628260 097
204700170628261 086
204800170628262 077
204900170628263 135
205000170628265 002
205100170628266 002
205200170628268 053
205300170628269 004
205400170628270 021
205500170628271 044
205600170628272 015
205700170628273 073
205800170628274 153
205900170628275 050
206000170628276 091
206100170628277 153
206200170628278 167
206300170628279 118
206400170628280 030
206500170628281 058
206600170628282 130
206700170628283 171
206800170628284 077
206900170628285 170
207000170628286 011
207100170628287 056
207200170628288 059
207300170628289 113
207400170628290 095
207500170628291 104
207600170628292 054
207700170628293 059
207800170628294 026
207900170628295 062
208000170628296 022
208100170628297 119
208200170628298 099
208300170628299 069
208400170628300 013
208500170628301 084
208600170628303 034
208700170628304 119
208800170628305 103
208900170628306 063
209000170628307 072
209100170628308 123
209200170628309 109
209300170628310 061
209400170628311 077
209500170628312 081
209600170628313 002
209700170628314 080
209800170628315 033
209900170628316 095
210000170628317 097
210100170628318 153
210200170628319 041
210300170628320 049
210400170628321 049
210500170628322 115
210600170628323 133
210700170628324 098
210800180219325 255
210900170628326 087
211000170628327 012
211100170628328 166
211200170628329 071
211300170628330 070
211400170628331 091
211500170628335 033
211600170628336 059
211700170628337 048
211800170628339 059
211900170628340 089
212000170628341 008
212100170628343 049
212200180219344 255
212300170628345 114
212400170628346 166
212500171127347 018
212600170628348 019
212700170628349 110
212800171024350 135
212900171024352 056
213000171024353 103
