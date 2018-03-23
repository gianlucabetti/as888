000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400150114      * in precedenza il cliente usava il traduttore standard TITV92R, poi ha chiesto di sviluppare
000500150114      * i barcode  rispetto ai colli però al contrario
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
007000060331     D wVATNOT_E       s                   LIKE(VATNOT)
007100110927     D wVATNOT_I       s                   LIKE(VATNOT)
007200140220     D wVATNOT_J       s                   LIKE(VATNOT)
007300140220     D wVATNOT_IJ      s             70
007400140319     D wVATNOT_S       s                   LIKE(VATNOT)
007500140919     D w70             s             70
007600141006     D w11             s             11
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
024500141014     C                   eval      depfil = VABLNP
024600141014     C                   exsr      repfil
024700141014     C                   if        depfil = invfil
024800141014     C                   if        vlrpoi = 999
024900141014     C                   move(P)   invfil        VABFGS
025000141014     C                   else
025100141014     C                   move(P)   vlrpoi        VABFGS
025200141014     C                   endif
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
027400141014     C                   endif
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
046700150114     C* Sviluppati i "CHI SONO" in relazione al numero colli
046800150114     C* però quello che mi è stato passato è l'ultimo collo (il progressivo sarebbe di 3)
046900140527     C*
047000150114     C                   FOR       wX = VABNCL - 1 downto 0
047100150114     C                   EVAL      VATNOT= %editc(%dec(%trim(wVATNOT_E)
047200150114     C                              : 11 : 0) - wX : 'X')
047300150114     C                   write     FIVAT000
047400150114     C                   add       1             §CTROKVT
047500150114     C                   ENDFOR
047600150114     C*
047700120320     C                   endif
047800110927     C*
047900110927     C                   if        wVATNOT_I <> *blanks
048000110927     C                   eval      VATNOT = wVATNOT_I
048100140711     C                   eval      VATTRC = 'I'
048200110927     C                   write     FIVAT000
048300110927     C                   add       1             §CTROKVT
048400110927     C                   endif
048500140220     C*
048600140220     C                   if        wVATNOT_J <> *blanks
048700140220     C                   eval      VATNOT = wVATNOT_J
048800140220     C                   eval      VATTRC = 'J'
048900140220     C                   write     FIVAT000
049000140220     C                   add       1             §CTROKVT
049100140220     C                   endif
049200140319     C*
049300140319     C                   if        wVATNOT_S <> *blanks
049400140319     C                   eval      VATNOT = wVATNOT_S
049500140319     C* i 2 flag devono essere blank
049600140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049700140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049800140319     C                   eval      VATTRC = 'S'
049900140319     C                   write     FIVAT000
050000140319     C                   add       1             §CTROKVT
050100140319     C                   endif
050200040119     C*
050300040119     C                   ENDSR
050400040119
050500040119
050600040119
050700040119     C*----------------------------------------------------*
050800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050900040119     C*----------------------------------------------------*
051000040119     C     PREVAT        BEGSR
051100040119     C*
051200040119     C* Compongo il nome del membro da dare al FIVATWWR
051300040119     C                   eval      parmbr = vlrhdl
051400040119     C                   movel     'M'           parmbr
051500040510     C                   eval      parccm = vlrksc
051600040119     C                   eval      paropz = '1'
051700040119     C* Effettuo la chiamata al CLLE preposto
051800040119     C                   call(e)   'TITVVTC'
051900040119     C                   parm                    parccm
052000040119     C                   parm                    parmbr
052100040119     C                   parm                    paropz
052200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052300040119     C                   if        %error
052400040119     C                   movel     '1'           chkcall
052500040119     C                   else
052600040119     C                   movel     '0'           chkcall
052700040119     C                   endif
052800040119     C*
052900040119     C                   ENDSR
053000010601
053100010601
053200010601
053300010330     C*----------------------------------------------------*
053400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053500010330     C*----------------------------------------------------*
053600040119     C     INZVAR        BEGSR
053700080527     C*
053800080527     C                   setoff                                       65
053900010330     C*
054000030822     C* Inizializzo variabili di wrk
054100020204     C                   Z-ADD     *zeros        Num5_0            5 0
054200101018     C                   MOVEL     *blanks       wFlgCAS
054300060331     C                   MOVEL     *blanks       wVATNOT_A
054400060331     C                   MOVEL     *blanks       wVATNOT_B
054500060331     C                   MOVEL     *blanks       wVATNOT_E
054600110927     C                   MOVEL     *blanks       wVATNOT_I
054700140220     C                   MOVEL     *blanks       wVATNOT_J
054800140220     C                   MOVEL     *blanks       wVATNOT_IJ
054900140319     C                   MOVEL     *blanks       wVATNOT_S
055000120411     C                   MOVEL     *blanks       wNOT
055100120411     C                   MOVEL     *blanks       Data8A
055200030822     C*
055300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055400030822     C                   CLEAR                   SkSplitCSV
055500030822     C                   EVAL      i = 1
055600030822     C                   EVAL      posDa = *zeros
055700030822     C                   EVAL      posA  = *zeros
055800080527     C*
055900080527     C                   CLEAR                   FIVABDS
056000080527     C                   CLEAR                   FIVAT000
056100030822     C*
056200030822     C* Reimposto i valori di default
056300030822     C                   EXSR      DEFCAM
056400030822     C*
056500010330     C                   ENDSR
056600010330     C*----------------------------------------------------*
056700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056800010330     C*----------------------------------------------------*
056900010330     C     DEFCAM        BEGSR
057000030822     C*
057100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057200030715     C* e delimitatore testo.
057300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057600100318     C*
057700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057800100318     C                   EVAL      CharSOS = CharNUM
057900030822     C*
058000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058100100720     C                   SETOFF                                       505152
058200100720     C                   SETOFF                                       53
058300100413     C                   SELECT
058400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058500100720     C                   SETON                                        50
058600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058700100413     C                   SETON                                        51
058800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058900100720     C                   SETON                                        52
059000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059100100720     C                   SETON                                        53
059200100413     C                   ENDSL
059300030822     C*
059400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059500030822     C                   EVAL      posDaDft = 1
059600030822     C                   EVAL      posADft  = 0
059700030822     C                   EVAL      wGiroDft = 0
059800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059900030822     C                             posDaDft > 0
060000030822     C*
060100030822     C* Gestisco il 1° giro
060200030822     C                   IF        wGiroDft = 0
060300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060500030822     C* Incremento il contatore dei "giri"
060600030822     C                   EVAL      wGiroDft = 1
060700030822     C                   ELSE
060800030822     C                   EVAL      posDaDft = posADft
060900030822     C                   ENDIF
061000030822     C* Eseguo lo scan x trovare la fine del campo corrente
061100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061200030822     C*
061300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061500030822     C                   IF        posDaDft > 0 AND
061600030822     C                             posADft  > 0
061700030822     C* NCL
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'NCL'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABNCL
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* CCM
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'CCM'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABCCM
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* LNP
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'LNP'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiInt=*on
065300030822     C                   Z-ADD     PiVal         VABLNP
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* NRS
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'NRS'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiInt=*on
066600030822     C                   Z-ADD     PiVal         VABNRS
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CTR
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CTR'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiInt=*on
067900030822     C                   Z-ADD     PiVal         VABCTR
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* PKB
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'PKB'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiNum=*on
069200130610     C                   Z-ADD     PiVal         VABPKB
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* VLB
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'VLB'
070000030822     C                   EVAL      PiStr=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   EXSR      CHKNUM
070400030822     C                   IF        PiNum=*on
070500130610     C                   Z-ADD     PiVal         VABVLB
070600030822     C                   ENDIF
070700030822     C                   ENDIF
070800030822     C* QFT
070900030822     C                   IF        %subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):1:3)
071200030822     C                             = 'QFT'
071300030822     C                   EVAL      PiStr=%trim(%subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071600030822     C                   EXSR      CHKNUM
071700030822     C                   IF        PiNum=*on
071800030822     C                   Z-ADD     PiVal         VABQFT
071900030822     C                   ENDIF
072000030822     C                   ENDIF
072100030822     C* CBO
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'CBO'
072600030822     C                   EVAL      VABCBO=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* TSP
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'TSP'
073500030822     C                   EVAL      VABTSP=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* VAS
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'VAS'
074400030822     C                   EVAL      VABVAS=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* VCA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'VCA'
075300030822     C                   EVAL      VABVCA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700070925     C* TIC
075800070925     C                   IF        %subst(
075900070925     C                             %subst(vlrppt:posDaDft+1:
076000070925     C                             posADft-posDaDft-1):1:3)
076100070925     C                             = 'TIC'
076200070925     C                   EVAL      VABTIC=%trim(%subst(
076300070925     C                             %subst(vlrppt:posDaDft+1:
076400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500070925     C                   ENDIF
076600030822     C* GCA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GCA'
077100030822     C                   EVAL      VABGCA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* CTM
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'CTM'
078000030822     C                   EVAL      VABCTM=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* FFD
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'FFD'
078900030822     C                   EVAL      VABFFD=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300030822     C* VAD
079400030822     C                   IF        %subst(
079500030822     C                             %subst(vlrppt:posDaDft+1:
079600030822     C                             posADft-posDaDft-1):1:3)
079700030822     C                             = 'VAD'
079800030822     C                   EVAL      VABVAD=%trim(%subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100030822     C                   ENDIF
080200030822     C* GMA
080300030822     C                   IF        %subst(
080400030822     C                             %subst(vlrppt:posDaDft+1:
080500030822     C                             posADft-posDaDft-1):1:3)
080600030822     C                             = 'GMA'
080700030822     C                   EVAL      VABGMA=%trim(%subst(
080800030822     C                             %subst(vlrppt:posDaDft+1:
080900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081000030822     C                   ENDIF
081100030822     C* GGA
081200030822     C                   IF        %subst(
081300030822     C                             %subst(vlrppt:posDaDft+1:
081400030822     C                             posADft-posDaDft-1):1:3)
081500030822     C                             = 'GGA'
081600030822     C                   EVAL      VABGGA=%trim(%subst(
081700030822     C                             %subst(vlrppt:posDaDft+1:
081800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081900030822     C                   ENDIF
082000030822     C* GVA
082100030822     C                   IF        %subst(
082200030822     C                             %subst(vlrppt:posDaDft+1:
082300030822     C                             posADft-posDaDft-1):1:3)
082400030822     C                             = 'GVA'
082500030822     C                   EVAL      VABGVA=%trim(%subst(
082600030822     C                             %subst(vlrppt:posDaDft+1:
082700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082800030822     C                   ENDIF
082900030822     C* TC1
083000030822     C                   IF        %subst(
083100030822     C                             %subst(vlrppt:posDaDft+1:
083200030822     C                             posADft-posDaDft-1):1:3)
083300030822     C                             = 'TC1'
083400030822     C                   EVAL      VABTC1=%trim(%subst(
083500030822     C                             %subst(vlrppt:posDaDft+1:
083600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083700030822     C                   ENDIF
083800030822     C* TC2
083900030822     C                   IF        %subst(
084000030822     C                             %subst(vlrppt:posDaDft+1:
084100030822     C                             posADft-posDaDft-1):1:3)
084200030822     C                             = 'TC2'
084300030822     C                   EVAL      VABTC2=%trim(%subst(
084400030822     C                             %subst(vlrppt:posDaDft+1:
084500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084600030822     C                   ENDIF
084700040714     C* VATTRC
084800040714     C                   IF        %subst(
084900040714     C                             %subst(vlrppt:posDaDft+1:
085000040714     C                             posADft-posDaDft-1):1:3)
085100040714     C                             = 'TRC'
085200040714     C                   EVAL      VATTRC=%trim(%subst(
085300040714     C                             %subst(vlrppt:posDaDft+1:
085400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085500040714     C                   ENDIF
085600030822     C* ...
085700030822     C                   ENDIF
085800030822     C                   ENDDO
085900020204     C*
086000010330     C                   ENDSR
086100010607     C*----------------------------------------------------*
086200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086300010607     C*----------------------------------------------------*
086400010607     C     IMPVAB        BEGSR
086500010607     C*
086600010607     C                   SETOFF                                       3132
086700110315 xxx C***                SETON                                        35
086800030515     C*
086900010607     C                   MOVEL     datcor        VABAAS
087000010607     C                   MOVE      datcor        VABMGS
087100010607     C*
087200030715     C**********
087300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087400030715     C**********
087500030715     C                   IF        wGiro = *zeros
087600040113     C*
087700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088000080429     C                             %trim(intcol(5))<>*blanks
088100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088300080429     C                                      %trim(intcol(5))
088400070823     C     ';':CharCSV   XLATE     vindta        vindta
088500040510     C                   ENDIF
088600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088700031201     C     minu:maiu     XLATE     vindta        vindta
088800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088900030715     C                   DOW       posDa <= %len(%trim(vindta))
089000030715     C*
089100030715     C* Gestisco il 1° campo
089200030715     C                   IF        i = 1
089300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089400030715     C                   EVAL      posDa = 1
089500030715     C* Eseguo lo scan x trovare la fine del primo campo
089600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C* X i campi successivi al 1°
090100030715     C                   ELSE
090200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090300030715     C                   EVAL      posDa = posA + 1
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800030715     C                   ENDIF
091900030715     C* Incremento il contatore d campo
092000030715     C                   EVAL      i = i +1
092100030715     C                   ENDDO
092200030715     C*
092300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092400030715     C* delimitazione testo
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitFLD)
092700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092900030715     C                   EVAL      i = i + 1
093000030715     C                   ENDDO
093100030715     C                   EVAL      wGiro = 1
093200030715     C                   ELSE
093300030715     C                   EVAL      wGiro = 2
093400031204     C**********
093500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093600031204     C* d separatore campo
093700031204     C**********
093800031204     C                   EVAL      vindta = %trim(vindta)
093900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094000031204     C                   EVAL      vindta = %subst(vindta:2)
094100031204     C                   ENDDO
094200031204     C*
094300031204     C                   Z-ADD     *zeros        lunghInput        4 0
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094800110912     C                   ENDDO
094900030715     C**********
095000030715     C* Effettuo lo split del campo dati d input
095100030715     C**********
095200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095300030715     C                   DOW       posDa <= %len(%trim(vindta))
095400030715     C*
095500030715     C* Gestisco il 1° campo
095600030715     C                   IF        i = 1
095700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095800030715     C                   EVAL      posDa = 1
095900030715     C* Eseguo lo scan x trovare la fine del primo campo
096000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096100030715     C* A questo punto "estraggo" il campo corrente
096200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096300030715     C                                                    (posA-posDa))
096400030715     C* X i campi successivi al 1°
096500030715     C                   ELSE
096600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096700030715     C                   EVAL      posDa = posA + 1
096800031204     C* Verifico che nn vi sia il campo nn valorizzato
096900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097000031204     C* Se campo nn valorizzato skippo al prossimo
097100031204     C                   EVAL      SkSplitCSV(i) = *blanks
097200031204     C                   EVAL      posA  = posA + 1
097300031204     C                   ELSE
097400030715     C* Eseguo lo scan x trovare la fine del campo corrente
097500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097600030715     C* Gestisco l'ultimo campo
097700030715     C                   IF        posA = *zeros
097800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097900030715     C* A questo punto "estraggo" il campo corrente
098000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098100030715     C                                                    (posA-posDa))
098200030715     C                   LEAVE
098300030715     C                   ELSE
098400030715     C* A questo punto "estraggo" il campo corrente
098500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098600030715     C                                                    (posA-posDa))
098700030715     C                   ENDIF
098800031204     C                   ENDIF
098900030715     C                   ENDIF
099000030715     C* Incremento il contatore d campo
099100030715     C                   EVAL      i = i +1
099200030715     C                   ENDDO
099300030715     C*
099400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099500030715     C* delimitazione testo
099600030715     C                   EVAL      i = 1
099700030715     C                   DOW       i < %elem(SkSplitCSV)
099800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100000030715     C                   EVAL      i = i + 1
100100030715     C                   ENDDO
100200100720     C*
100300100720     C* Stacco un progressivo x numero spedizione
100400141006 xxx C                   EXSR      repNSP
100500100326     C*
100600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100700030715     C                   EVAL      i = 1
100800030715     C                   DOW       i < %elem(SkSplitFLD)
100900030715     C*
101000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101200030715     C                             SkSplitCSV(i) <> *blanks
101300030721     C*** ATB
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101500030721     C                   EVAL      VABATB = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030715     C*** RSD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102200030715     C* ==
102300130528     C                   ENDIF
102400030721     C*** RD2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030715     C*** IND
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103000030715     C                   EVAL      VABIND = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** LOD
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** PRD
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030721     C*** NZD
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030715     C*** RMA
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** NOT
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030715     C*** NT2
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** VCA
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030715     C*** VAS
106100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106300030715     C                   ENDIF
106400030715     C*** FFD
106500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106700030715     C                   ENDIF
106800030721     C*** GC1
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GC2
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** TSP
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107900030721     C                   ENDIF
108000040510     C*** CBO
108100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108300040510     C                   ENDIF
108400030721     C*** NAS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TIC
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** GCA
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** XCO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** CTM
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** TCR
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** CTS
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** FTM
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** VAD
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** GMA
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** GGA
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** GVA
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** TC1
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** TC2
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** SCL
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*** RMO
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114700030721     C                   ENDIF
114800030721     C*** NMO
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115100030721     C                   ENDIF
115200030721     C*
115300030721     C* Reperisco quindi i campi numerici...
115400040510     C*** NSP
115500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115600040510     C                   EVAL      PiStr=SkSplitCSV(i)
115700040510     C                   EXSR      CHKNUM
115800040510     C                   IF        PiInt=*on
115900040510     C                   Z-ADD     PiVal         VABNSP
116000040510     C                   ELSE
116100040510     C                   SETON                                        31
116200040510     C                   EVAL      VABNSP = *zeros
116300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116400040510     C                             + ' ' + 'VABNSP'
116500040510     C                   ENDIF
116600040510     C                   ENDIF
116700071106     C*** RMN
116800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116900071106     C                   EVAL      PiStr=SkSplitCSV(i)
117000071106     C                   EXSR      CHKNUM
117100071106     C                   IF        PiInt=*on
117200071106     C                   Z-ADD     PiVal         VABRMN
117300071106     C                   ELSE
117400071106     C                   SETON                                        32
117500071106     C                   EVAL      VABRMN = *zeros
117600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117700071106     C                             + ' ' + 'VABRMN'
117800071106     C                   ENDIF
117900071106     C                   ENDIF
118000030721     C*** CAD
118100130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118200130610     C                   EVAL      PiStr=SkSplitCSV(i)
118300130610     C                   EXSR      CHKNUM
118400130610     C                   IF        PiInt=*on
118500130610     C                   Z-ADD     PiVal         Num5_0
118600130610     C                   MOVEL(p)  Num5_0        VABCAD
118700130610     C                   ELSE
118800130610     C                   SETON                                        32
118900130610     C                   EVAL      VABCAD = *zeros
119000130610     C                   EVAL      vinmsg = %trimr(vinmsg)
119100130610     C                             + ' ' + 'VABCAD'
119200130610     C                   ENDIF
119300130605     C                   ENDIF
119400030721     C*** NCL
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on
119900030721     C                   Z-ADD     PiVal         VABNCL
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABNCL = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040122     C                             + ' ' + 'VABNCL'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** PKB
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200130610     C                   Z-ADD(H)  PiVal         VABPKB
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      VABPKB = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABPKB'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** CAS
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
122200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
122300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
122400140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
122500140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
122600140711     C                             %trim(SkSplitCSV(i)) <> ',00'
122700130703     C                   EVAL      VABVCA = 'EUR'
122800140930     C                   EVAL      VABTIC = *blank
122900140513     C                   EVAL      wFlgCAS = '1'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiNum=*on
123300030721     C                   Z-ADD     PiVal         VABCAS
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABCAS = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABCAS'
123900030721     C                   ENDIF
124000140513     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** CCM
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on      AND
124700030721     C                             PiVal<=9999999 AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABCCM
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      VABCCM = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040510     C                             + ' ' + 'VABCCM'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** LNP
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on    AND
126200030721     C                             PiVal<=999   AND
126300030721     C                             PiVal>*zeros
126400030721     C                   Z-ADD     PiVal         VABLNP
126500030721     C                   ELSE
126600040715     C                   SETON                                        32
126700030721     C                   EVAL      VABLNP = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900040510     C                             + ' ' + 'VABLNP'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** NRS
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on AND
127700030721     C                             PiVal<=99
127800030721     C                   Z-ADD     PiVal         VABNRS
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABNRS'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** LNA
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on    AND
129000030721     C                             PiVal<=999   AND
129100030721     C                             PiVal>*zeros
129200030721     C                   Z-ADD     PiVal         VABLNA
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      VABLNA = *zeros
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABLNA'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** CTR
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on  AND
130500030721     C                             PiVal<=999
130600030721     C                   Z-ADD     PiVal         VABCTR
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABCTR'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** IAS
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131500141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131600141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131700141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
131800141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
131900141003     C                             %trim(SkSplitCSV(i)) <> ',00'
132000141006     C                   EVAL      VABVAS = 'EUR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiNum=*on
132400030721     C                   Z-ADD     PiVal         VABIAS
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABIAS'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100141003     C                   ENDIF
133200030721     C*** VLB
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700130610     C                   Z-ADD(H)  PiVal         VABVLB
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABVLB'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** QFT
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900030721     C                   Z-ADD     PiVal         VABQFT
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABQFT'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** NCD
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on      AND
136100030721     C                             PiVal<=9999999
136200030721     C                   Z-ADD     PiVal         VABNCD
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABNCD'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** NCA
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on      AND
137400030721     C                             PiVal<=9999999
137500030721     C                   Z-ADD     PiVal         VABNCA
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABNCA'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** ZNC
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on AND
138700030721     C                             PiVal<=99
138800030721     C                   Z-ADD     PiVal         VABZNC
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABZNC'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** DCR
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiInt=*on       AND
140000030721     C                             PiVal<=99999999
140100030721     C                   Z-ADD     PiVal         VABDCR
140200030721     C                   ELSE
140300030721     C                   SETON                                        32
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABDCR'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800030721     C*** HCR
140900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141000030721     C                   EVAL      PiStr=SkSplitCSV(i)
141100030721     C                   EXSR      CHKNUM
141200030721     C                   IF        PiInt=*on   AND
141300030721     C                             PiVal<=9999
141400030721     C                   Z-ADD     PiVal         VABHCR
141500030721     C                   ELSE
141600030721     C                   SETON                                        32
141700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141800030721     C                             + ' ' + 'VABHCR'
141900030721     C                   ENDIF
142000030721     C                   ENDIF
142100030721     C*** VMD
142200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142300030721     C                   EVAL      PiStr=SkSplitCSV(i)
142400030721     C                   EXSR      CHKNUM
142500030721     C                   IF        PiNum=*on
142600030721     C                   Z-ADD     PiVal         VABVMD
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143000030721     C                             + ' ' + 'VABVMD'
143100030721     C                   ENDIF
143200030721     C                   ENDIF
143300030721     C*** ANT
143400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143500030721     C                   EVAL      PiStr=SkSplitCSV(i)
143600030721     C                   EXSR      CHKNUM
143700030721     C                   IF        PiInt=*on        AND
143800030721     C                             PiVal<=999999999
143900030721     C                   Z-ADD     PiVal         VABANT
144000030721     C                   ELSE
144100030721     C                   SETON                                        32
144200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144300030721     C                             + ' ' + 'VABANT'
144400030721     C                   ENDIF
144500030721     C                   ENDIF
144600030721     C*** CMO
144700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144800030721     C                   EVAL      PiStr=SkSplitCSV(i)
144900030721     C                   EXSR      CHKNUM
145000030721     C                   IF        PiInt=*on
145100030721     C                   Z-ADD     PiVal         Num5_0
145200030721     C                   MOVEL(p)  Num5_0        VABCMO
145300030721     C                   ELSE
145400030721     C                   SETON                                        32
145500030721     C                   EVAL      VABCMO = *zeros
145600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145700030721     C                             + ' ' + 'VABCMO'
145800030721     C                   ENDIF
145900030721     C                   ENDIF
146000040510     C*
146100040510     C* Reperisco i campi del FIVAT
146200040510     C*** VATNOT
146300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146500040510     C                   ENDIF
146600040510     C*** VATTRC
146700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146900040510     C                   ENDIF
147000060331     C*** VATNOT_A
147100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147300060331     C                   ENDIF
147400060331     C*** VATNOT_B
147500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147700060331     C                   ENDIF
147800060331     C*** VATNOT_E
147900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148100060331     C                   ENDIF
148200110927     C*** VATNOT_I
148300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148500110927     C                   ENDIF
148600141003     C*** VATNOT_IJ
148700141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148800141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148900141003     C                   ENDIF
149000140319     C*** VATNOT_S
149100140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149200140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149300140319     C                   ENDIF
149400070823     C*
149500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149600070925     C*
149700150114     C*** VATNOT_E finale
149800141112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
149900141014     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150000141006     C                   ENDIF
150100141003     C*
150200071029     C***  <===  -----------------------------------------
150300040510     C*
150400030715     C                   ENDIF
150500030715     C                   EVAL      i = i + 1
150600030715     C                   ENDDO
150700070925     C*
150800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150900140319     C*
151000141014     C* se la nazione è IT la sblanco
151100141112     C                   IF        VABNZD = 'IT' or
151200141112     C                             VABNZD = 'ITA'
151300141014     C                   EVAL      VABNZD = *blank
151400141014     C                   ENDIF
151500070925     C*
151600070925     C* Considerazioni finali su CBO/CAS
151700140220     C                   IF        wFlgCAS = '1'    OR
151800140220     C                             VABCAS  > *zeros
151900140220     C                   IF        VABCBO = '1'
152000140220     C                   EVAL      VABCBO = '4'
152100140220     C                   ENDIF
152200140220     C                   IF        VABCBO = '2'
152300140220     C                   EVAL      VABCBO = '6'
152400140220     C                   ENDIF
152500140220     C                   ENDIF
152600070925     C***  <===  ----------------------------
152700040510     C*
152800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
152900040510     C                   IF        VABNSP = *zeros
153000040510     C                   SETON                                        31
153100040510     C                   ENDIF
153200040715     C*
153300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153400040715     C                   IF        VABLNP = *zeros
153500040715     C                   SETON                                        31
153600040715     C                   ENDIF
153700040510     C*
153800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
153900120411     C                   IF        VABRMN = *zeros
154000120411     C                   Z-ADD     VABNSP        VABRMN
154100120411     C                   ENDIF
154200120328     C* IN QUESTO CASO LO FORZO = 1
154300120411     C***                IF        VABRMN = *zeros
154400120411     C***                Z-ADD     1             VABRMN
154500120411     C***                ENDIF
154600040510     C*
154700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
154800040510     C                   IF        VABRMA = *blanks
154900040714     C                   MOVEL     VABRMN        VABRMA
155000040510     C                   ENDIF
155100040128     C*
155200040510     C* Se provincia nn valorizzata la reperisco
155300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155400110912     C                   IF        VABPRD =  *blanks AND
155500110912     C                             VABNZD =  *blanks AND
155600110912     C                             VABCAD <> *blanks AND
155700110912     C                             VABLOD <> *blanks
155800040128     C                   CLEAR                   TISI95DS
155900040128     C                   EVAL      I95TCN = '3'
156000040128     C                   Z-ADD     datcor        I95DAT
156100040128     C                   EVAL      I95NAR = VABNZD
156200040128     C                   EVAL      I95CAP = VABCAD
156300040128     C                   EVAL      I95LOC = VABLOD
156400040128     C                   CALL      'TISI95R'
156500040128     C                   PARM                    TISI95DS
156600040128     C                   EVAL      VABPRD = O95PRV
156700040510     C                   ENDIF
156800020204     C*
156900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157000020204     C                   EXSR      CHKIMPDIV
157100030715     C                   ENDIF
157200020204     C*
157300010607     C                   ENDSR
157400010607     C*----------------------------------------------------*
157500010601
157600020204
157700020204     C*----------------------------------------------------*
157800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157900020204     C*----------------------------------------------------*
158000020204     C     CHKIMPDIV     BEGSR
158100020204     C*
158200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158300020204     C                   Z-ADD     *zeros        wrkDec            9 9
158400020204     C*
158500020204     C* Come prima cosa effettuo considerazioni sulla divisa
158600020204     C                   IF        vabIAS > *zeros
158700020204     C                   IF        vabVAS <> 'EUR'
158800020204     C                   EVAL      vabVAS =  'ITL'
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C                   IF        vabCAS > *zeros
159300020204     C                   IF        vabVCA <> 'EUR'
159400020204     C                   EVAL      vabVCA =  'ITL'
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C                   IF        vabVMD > *zeros
159900020204     C                   IF        vabVAD <> 'EUR'
160000020204     C                   EVAL      vabVAD =  'ITL'
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160500020204     C                   Z-ADD     vabIAS        wrkDec
160600020204     C                   IF        wrkDec > *zeros
160700020204     C                   IF        vabVAS = 'ITL'
160800020204     C                   EVAL      vabIAS = *zeros
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161300020204     C                   Z-ADD     vabCAS        wrkDec
161400020204     C                   IF        wrkDec > *zeros
161500020204     C                   IF        vabVCA = 'ITL'
161600020204     C                   EVAL      vabCAS = *zeros
161700020204     C                   ENDIF
161800020204     C                   ENDIF
161900020204     C*
162000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162100020204     C                   Z-ADD     vabVMD        wrkDec
162200020204     C                   IF        wrkDec > *zeros
162300020204     C                   IF        vabVAD = 'ITL'
162400020204     C                   EVAL      vabVMD = *zeros
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C                   ENDSR
162900020204     C***
163000010330
163100100318
163200100318
163300100318     C*----------------------------------------------------*
163400100318     C*  CONTROLLO NUMERICITA' CAMPI
163500100318     C*----------------------------------------------------*
163600100318     C     CHKNUM        BEGSR
163700100318     C*
163800100318     C                   IF        PiDecChr = *blanks
163900100318     C                   EVAL      PiDecChr = CharNUM
164000100318     C                   ENDIF
164100100318     C*
164200100318     C                   callp     UBISNUM_Check(PiStr
164300100318     C                                          :PiDecChr
164400100318     C                                          :PiVal
164500100318     C                                          :PiNum
164600100318     C                                          :PiInt)
164700100318     C*
164800100318     C                   ENDSR
164900100318     C***
165000010601
165100010601
165200010601
165300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165400141006      ***
165500141014     C     repfil        BEGSR
165600141014     C*
165700141014     C                   if        invfil = *zeros and
165800141014     C                             depfil > *zeros and
165900141014     C                             (vinflg = *blanks or
166000141014     C                              vinflg = *zeros)
166100141014     C*
166200141014     C                   eval      invfil = depfil
166300141014     C                   endif
166400141014     C*
166500141014     C                   if        depfil <> invfil and
166600141014     C                             invfil > *zeros
166700141014     C                   eval      flgMulti = '1'
166800141014     C                   if        vinflg = *blanks
166900141014     C                   add       1             cntNonEl
167000141014     C                   endif
167100141014     C                   endif
167200141014     C*
167300141014     C                   if        vinflg = '2'
167400141014     C                   eval      flgStato = '2'
167500141014     C                   endif
167600141014     C*
167700141014     C                   ENDSR
167800141014     C*
167900010601
168000010601
168100010601
168200010330
168300040119      /TITLE Invio dei dati al punto operativo.
168400040119     C     invio         BEGSR
168500040119     C*
168600040119     C* 1° invio FIVAT
168700040119     C                   reset                   dscmz
168800040119     C                   move      vlrpoi        cmzdst
168900040119     C                   eval      cmzfld = 'FIVATWWR'
169000040119     C                   eval      cmzmbd = vlrhdl
169100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169200040119     C***                if        prmfir = *blanks
169300040119     C                   eval      cmzfla = 'FIVAT00F'
169400040119     C                   eval      cmzmba = 'FIVAT00F'
169500040119     C***                else
169600040119     C***                eval      cmzfla = prmfir
169700040119     C***                eval      cmzmba = prmfir
169800040119     C***                endif
169900040119     C                   eval      cmznrr = *zeros
170000040119     C                   move      §ctrokvt      cmznrr
170100040119     C                   eval      cmzlba = vlrfl1
170200040119     C                   call(e)   'TIS711C'
170300040119     C                   parm                    dscmz
170400040119     C                   parm      *blanks       esito
170500040119     C                   if        %error
170600040119     C                             or cmzerr = '1'
170700040119     C                             or esito  = '1'
170800040119     C                   eval      wrkesito = '3'
170900040119     C                   else
171000040119     C*
171100040119     C* 2° invio FIVAB
171200040119     C                   reset                   dscmz
171300040119     C                   move      vlrpoi        cmzdst
171400040119     C                   eval      cmzfld = vlrfou
171500040119     C                   eval      cmzmbd = vlrhdl
171600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171700040119     C***                if        prmfir = *blanks
171800040119     C                   eval      cmzfla = 'FIVAB00F'
171900040119     C                   eval      cmzmba = 'FIVAB00F'
172000040119     C***                else
172100040119     C***                eval      cmzfla = prmfir
172200040119     C***                eval      cmzmba = prmfir
172300040119     C***                endif
172400040119     C                   eval      cmznrr = *zeros
172500040119     C                   move      §ctrokvb      cmznrr
172600040119     C                   eval      cmzlba = vlrfl1
172700040119     C                   call(e)   'TIS711C'
172800040119     C                   parm                    dscmz
172900040119     C                   parm      *blanks       esito
173000040119     C                   if        %error
173100040119     C                             or cmzerr = '1'
173200040119     C                             or esito  = '1'
173300040119     C                   eval      wrkesito = '3'
173400040119     C                   endif
173500040119     C                   endif
173600040119     C*
173700040119     C                   ENDSR
173800040119     C***
173900010601
174000010601
174100010601
174200010601
174300010601      /TITLE Invio dei dati al punto operativo.
174400010601     C     opeini        BEGSR
174500010601     C*
174600010601     C* Inizializzo flag e contatori operativi
174700010601     C                   movel     '0'           flgGiro           1
174800010601     C                   movel     '0'           flgMulti          1
174900010601     C                   movel     '1'           flgStato          1
175000010615     C                   movel     '0'           flgOk             1
175100010601     C                   z-add     *zeros        cntNonEl         10 0
175200010601     C                   z-add     *zeros        depfil            3 0
175300010601     C                   z-add     *zeros        invfil            3 0
175400010601     C*
175500010601     C                   ENDSR
175600010601     C***
175700070326
175800070326
175900070326
176000070326
176100070326     C     *pssr         BEGSR
176200070329     C*
176300070329     C                   if        %open(tivin00r)
176400070329     C                   close     tivin00r
176500070329     C                   endif
176600070329     C                   if        %open(fivabwwr)
176700070329     C                   close     fivabwwr
176800070329     C                   endif
176900070329     C                   if        %open(fivatwwr)
177000070329     C                   close     fivatwwr
177100070329     C                   endif
177200070326     C*
177300070326     C* Effettuo la chiamata al CLLE preposto
177400070326     C                   call(e)   'TITVVTC'
177500070326     C                   parm                    parccm
177600070326     C                   parm                    parmbr
177700070326     C                   parm      '2'           paropz
177800070326     C*
177900070326     C                   eval      wrkesito = '2'
178000070404     C*
178100070404     C                   seton                                        LR
178200070326     C*
178300070326     C                   ENDSR     '*CANCL'
178400070326     C***
178500100318
178600100318
178700100318
178800100318     C     normCSV       BEGSR
178900100318     C*
179000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179100100318     C* separatori campi
179200100318     C                   if        CharTXT <> *blanks
179300100318     C                   z-add     *zeros        wLenVINDTA        4 0
179400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179500100318     C                   setoff                                       80
179600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
179800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179900100318     C                   if        not *in80
180000100318     C                   seton                                        80
180100100318     C                   else
180200100318     C                   setoff                                       80
180300100318     C                   endif
180400100318     C                   endif
180500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180600100318     C                             and *in80 = *on
180700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180800100318     C                   endif
180900100318     C                   enddo
181000100318     C                   endif
181100100318     C*
181200100318     C                   ENDSR
181300100318     C***
181400070326
181500100326
181600100326
181700100326     C*----------------------------------------------------*
181800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
181900100326     C*----------------------------------------------------*
182000100326     C     REPNSP        BEGSR
182100100326     C*
182200100326     C* NSP => Stacco un numeratore da AZNUM
182300100326     C                   clear                   TRUL33DS
182400100326     C                   eval      I33OPE = *zeros
182500100326     C                   eval      I33CNU = 302
182600100326     C                   eval      I33NUM = 1
182700100326     C                   movel     TRUL33DS      KPJBU
182800100326     C                   call      'TRUL33R'
182900100326     C                   parm                    KPJBA
183000100326     C                   movel     KPJBU         TRUL33DS
183100100326     C                   if        O33ERR = *zeros
183200100326     C                   z-add     O33NRF        VABNSP
183300100326     C                   z-add     O33NRF        VATNSP
183400100326     C                   else
183500100326     C                   SETON                                        31
183600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183700140711     C                             + ' ' + 'VABNSP VATNSP'
183800100326     C                   endif
183900100326     C*
184000100326     C                   ENDSR
184100010330
184200100326
184300010330
184400000613     C     *inzsr        BEGSR
184500990910     C*
184600990910     C     *entry        plist
184700990920     C                   parm                    tivlrds
184800990921     C                   parm      wrkesito      esito
184900000724     C                   parm                    prmlit
185000000710     C                   parm                    prmfir
185100010330     C*
185200010330     C* CALCOLA LA DATA CORRENTE
185300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185400100318     C                   eval      datcor = %dec(%date() : *ISO)
185500000613     C*
185600000613     C                   ENDSR
185700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185800150115VABCCM;VABCTR;VABLNP;VABNSP;VABCBO;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABNCL;VABPKB;
185900150114VATNOT_B;VATNOT_A;VABCAS;W_VATNOT_E;VATNOT_S;VATNOT_IJ;
186000140512
186100111116
186200100318
