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
047100150309     C*** questa istruzione mette uno 0 davanti al risultato (!!!)
047200150309     C*** scompattandola no
047300150309     C***                EVAL      VATNOT= %editc(%dec(%trim(wVATNOT_E)
047400150309     C***                           : 11 : 0) - wX : 'X')
047500150309     C                   EVAL      wBarcodeN =    %dec(%trim(wVATNOT_E)
047600150309     C                              : 11 : 0) - wX
047700150309     C                   EVAL      VATNOT= %editc(wBarcodeN
047800150309     C                                             : 'X')
047900150114     C                   write     FIVAT000
048000150114     C                   add       1             §CTROKVT
048100150114     C                   ENDFOR
048200150114     C*
048300120320     C                   endif
048400110927     C*
048500110927     C                   if        wVATNOT_I <> *blanks
048600110927     C                   eval      VATNOT = wVATNOT_I
048700140711     C                   eval      VATTRC = 'I'
048800110927     C                   write     FIVAT000
048900110927     C                   add       1             §CTROKVT
049000110927     C                   endif
049100140220     C*
049200140220     C                   if        wVATNOT_J <> *blanks
049300140220     C                   eval      VATNOT = wVATNOT_J
049400140220     C                   eval      VATTRC = 'J'
049500140220     C                   write     FIVAT000
049600140220     C                   add       1             §CTROKVT
049700140220     C                   endif
049800140319     C*
049900140319     C                   if        wVATNOT_S <> *blanks
050000140319     C                   eval      VATNOT = wVATNOT_S
050100140319     C* i 2 flag devono essere blank
050200140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
050300140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
050400140319     C                   eval      VATTRC = 'S'
050500140319     C                   write     FIVAT000
050600140319     C                   add       1             §CTROKVT
050700140319     C                   endif
050800040119     C*
050900040119     C                   ENDSR
051000040119
051100040119
051200040119
051300040119     C*----------------------------------------------------*
051400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
051500040119     C*----------------------------------------------------*
051600040119     C     PREVAT        BEGSR
051700040119     C*
051800040119     C* Compongo il nome del membro da dare al FIVATWWR
051900040119     C                   eval      parmbr = vlrhdl
052000040119     C                   movel     'M'           parmbr
052100040510     C                   eval      parccm = vlrksc
052200040119     C                   eval      paropz = '1'
052300040119     C* Effettuo la chiamata al CLLE preposto
052400040119     C                   call(e)   'TITVVTC'
052500040119     C                   parm                    parccm
052600040119     C                   parm                    parmbr
052700040119     C                   parm                    paropz
052800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
052900040119     C                   if        %error
053000040119     C                   movel     '1'           chkcall
053100040119     C                   else
053200040119     C                   movel     '0'           chkcall
053300040119     C                   endif
053400040119     C*
053500040119     C                   ENDSR
053600010601
053700010601
053800010601
053900010330     C*----------------------------------------------------*
054000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
054100010330     C*----------------------------------------------------*
054200040119     C     INZVAR        BEGSR
054300080527     C*
054400080527     C                   setoff                                       65
054500010330     C*
054600030822     C* Inizializzo variabili di wrk
054700020204     C                   Z-ADD     *zeros        Num5_0            5 0
054800101018     C                   MOVEL     *blanks       wFlgCAS
054900060331     C                   MOVEL     *blanks       wVATNOT_A
055000060331     C                   MOVEL     *blanks       wVATNOT_B
055100060331     C                   MOVEL     *blanks       wVATNOT_E
055200110927     C                   MOVEL     *blanks       wVATNOT_I
055300140220     C                   MOVEL     *blanks       wVATNOT_J
055400140220     C                   MOVEL     *blanks       wVATNOT_IJ
055500140319     C                   MOVEL     *blanks       wVATNOT_S
055600120411     C                   MOVEL     *blanks       wNOT
055700120411     C                   MOVEL     *blanks       Data8A
055800030822     C*
055900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
056000030822     C                   CLEAR                   SkSplitCSV
056100030822     C                   EVAL      i = 1
056200030822     C                   EVAL      posDa = *zeros
056300030822     C                   EVAL      posA  = *zeros
056400080527     C*
056500080527     C                   CLEAR                   FIVABDS
056600080527     C                   CLEAR                   FIVAT000
056700030822     C*
056800030822     C* Reimposto i valori di default
056900030822     C                   EXSR      DEFCAM
057000030822     C*
057100010330     C                   ENDSR
057200010330     C*----------------------------------------------------*
057300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
057400010330     C*----------------------------------------------------*
057500010330     C     DEFCAM        BEGSR
057600030822     C*
057700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
057800030715     C* e delimitatore testo.
057900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
058000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
058100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
058200100318     C*
058300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
058400100318     C                   EVAL      CharSOS = CharNUM
058500030822     C*
058600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
058700100720     C                   SETOFF                                       505152
058800100720     C                   SETOFF                                       53
058900100413     C                   SELECT
059000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
059100100720     C                   SETON                                        50
059200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
059300100413     C                   SETON                                        51
059400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
059500100720     C                   SETON                                        52
059600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
059700100720     C                   SETON                                        53
059800100413     C                   ENDSL
059900030822     C*
060000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
060100030822     C                   EVAL      posDaDft = 1
060200030822     C                   EVAL      posADft  = 0
060300030822     C                   EVAL      wGiroDft = 0
060400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
060500030822     C                             posDaDft > 0
060600030822     C*
060700030822     C* Gestisco il 1° giro
060800030822     C                   IF        wGiroDft = 0
060900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
061000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
061100030822     C* Incremento il contatore dei "giri"
061200030822     C                   EVAL      wGiroDft = 1
061300030822     C                   ELSE
061400030822     C                   EVAL      posDaDft = posADft
061500030822     C                   ENDIF
061600030822     C* Eseguo lo scan x trovare la fine del campo corrente
061700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061800030822     C*
061900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
062000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
062100030822     C                   IF        posDaDft > 0 AND
062200030822     C                             posADft  > 0
062300030822     C* NCL
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'NCL'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiInt=*on
063300030822     C                   Z-ADD     PiVal         VABNCL
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* CCM
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'CCM'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiInt=*on
064600030822     C                   Z-ADD     PiVal         VABCCM
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* LNP
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'LNP'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiInt=*on
065900030822     C                   Z-ADD     PiVal         VABLNP
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* NRS
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'NRS'
066700030822     C                   EVAL      PiStr=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   EXSR      CHKNUM
067100030822     C                   IF        PiInt=*on
067200030822     C                   Z-ADD     PiVal         VABNRS
067300030822     C                   ENDIF
067400030822     C                   ENDIF
067500030822     C* CTR
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'CTR'
068000030822     C                   EVAL      PiStr=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   EXSR      CHKNUM
068400030822     C                   IF        PiInt=*on
068500030822     C                   Z-ADD     PiVal         VABCTR
068600030822     C                   ENDIF
068700030822     C                   ENDIF
068800030822     C* PKB
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'PKB'
069300030822     C                   EVAL      PiStr=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   EXSR      CHKNUM
069700030822     C                   IF        PiNum=*on
069800130610     C                   Z-ADD     PiVal         VABPKB
069900030822     C                   ENDIF
070000030822     C                   ENDIF
070100030822     C* VLB
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'VLB'
070600030822     C                   EVAL      PiStr=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   EXSR      CHKNUM
071000030822     C                   IF        PiNum=*on
071100130610     C                   Z-ADD     PiVal         VABVLB
071200030822     C                   ENDIF
071300030822     C                   ENDIF
071400030822     C* QFT
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'QFT'
071900030822     C                   EVAL      PiStr=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   EXSR      CHKNUM
072300030822     C                   IF        PiNum=*on
072400030822     C                   Z-ADD     PiVal         VABQFT
072500030822     C                   ENDIF
072600030822     C                   ENDIF
072700030822     C* CBO
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'CBO'
073200030822     C                   EVAL      VABCBO=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* TSP
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'TSP'
074100030822     C                   EVAL      VABTSP=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VAS
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VAS'
075000030822     C                   EVAL      VABVAS=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* VCA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'VCA'
075900030822     C                   EVAL      VABVCA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300070925     C* TIC
076400070925     C                   IF        %subst(
076500070925     C                             %subst(vlrppt:posDaDft+1:
076600070925     C                             posADft-posDaDft-1):1:3)
076700070925     C                             = 'TIC'
076800070925     C                   EVAL      VABTIC=%trim(%subst(
076900070925     C                             %subst(vlrppt:posDaDft+1:
077000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100070925     C                   ENDIF
077200030822     C* GCA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GCA'
077700030822     C                   EVAL      VABGCA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* CTM
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'CTM'
078600030822     C                   EVAL      VABCTM=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* FFD
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'FFD'
079500030822     C                   EVAL      VABFFD=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* VAD
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'VAD'
080400030822     C                   EVAL      VABVAD=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GMA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GMA'
081300030822     C                   EVAL      VABGMA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GGA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GGA'
082200030822     C                   EVAL      VABGGA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* GVA
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'GVA'
083100030822     C                   EVAL      VABGVA=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* TC1
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'TC1'
084000030822     C                   EVAL      VABTC1=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400030822     C* TC2
084500030822     C                   IF        %subst(
084600030822     C                             %subst(vlrppt:posDaDft+1:
084700030822     C                             posADft-posDaDft-1):1:3)
084800030822     C                             = 'TC2'
084900030822     C                   EVAL      VABTC2=%trim(%subst(
085000030822     C                             %subst(vlrppt:posDaDft+1:
085100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200030822     C                   ENDIF
085300040714     C* VATTRC
085400040714     C                   IF        %subst(
085500040714     C                             %subst(vlrppt:posDaDft+1:
085600040714     C                             posADft-posDaDft-1):1:3)
085700040714     C                             = 'TRC'
085800040714     C                   EVAL      VATTRC=%trim(%subst(
085900040714     C                             %subst(vlrppt:posDaDft+1:
086000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086100040714     C                   ENDIF
086200030822     C* ...
086300030822     C                   ENDIF
086400030822     C                   ENDDO
086500020204     C*
086600010330     C                   ENDSR
086700010607     C*----------------------------------------------------*
086800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086900010607     C*----------------------------------------------------*
087000010607     C     IMPVAB        BEGSR
087100010607     C*
087200010607     C                   SETOFF                                       3132
087300110315 xxx C***                SETON                                        35
087400030515     C*
087500010607     C                   MOVEL     datcor        VABAAS
087600010607     C                   MOVE      datcor        VABMGS
087700010607     C*
087800030715     C**********
087900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
088000030715     C**********
088100030715     C                   IF        wGiro = *zeros
088200040113     C*
088300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
088400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
088500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
088600080429     C                             %trim(intcol(5))<>*blanks
088700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088900080429     C                                      %trim(intcol(5))
089000070823     C     ';':CharCSV   XLATE     vindta        vindta
089100040510     C                   ENDIF
089200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
089300031201     C     minu:maiu     XLATE     vindta        vindta
089400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089500030715     C                   DOW       posDa <= %len(%trim(vindta))
089600030715     C*
089700030715     C* Gestisco il 1° campo
089800030715     C                   IF        i = 1
089900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090000030715     C                   EVAL      posDa = 1
090100030715     C* Eseguo lo scan x trovare la fine del primo campo
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* A questo punto "estraggo" il campo corrente
090400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090500030715     C                                                    (posA-posDa))
090600030715     C* X i campi successivi al 1°
090700030715     C                   ELSE
090800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090900030715     C                   EVAL      posDa = posA + 1
091000030715     C* Eseguo lo scan x trovare la fine del campo corrente
091100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091200030715     C* Gestisco l'ultimo campo
091300030715     C                   IF        posA = *zeros
091400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091500030715     C* A questo punto "estraggo" il campo corrente
091600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091700030715     C                                                    (posA-posDa))
091800030715     C                   LEAVE
091900030715     C                   ELSE
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C                   ENDIF
092400030715     C                   ENDIF
092500030715     C* Incremento il contatore d campo
092600030715     C                   EVAL      i = i +1
092700030715     C                   ENDDO
092800030715     C*
092900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093000030715     C* delimitazione testo
093100030715     C                   EVAL      i = 1
093200030715     C                   DOW       i < %elem(SkSplitFLD)
093300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
093400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
093500030715     C                   EVAL      i = i + 1
093600030715     C                   ENDDO
093700030715     C                   EVAL      wGiro = 1
093800030715     C                   ELSE
093900030715     C                   EVAL      wGiro = 2
094000031204     C**********
094100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
094200031204     C* d separatore campo
094300031204     C**********
094400031204     C                   EVAL      vindta = %trim(vindta)
094500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
094600031204     C                   EVAL      vindta = %subst(vindta:2)
094700031204     C                   ENDDO
094800031204     C*
094900031204     C                   Z-ADD     *zeros        lunghInput        4 0
095000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
095200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
095300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
095400110912     C                   ENDDO
095500030715     C**********
095600030715     C* Effettuo lo split del campo dati d input
095700030715     C**********
095800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095900030715     C                   DOW       posDa <= %len(%trim(vindta))
096000030715     C*
096100030715     C* Gestisco il 1° campo
096200030715     C                   IF        i = 1
096300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
096400030715     C                   EVAL      posDa = 1
096500030715     C* Eseguo lo scan x trovare la fine del primo campo
096600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096700030715     C* A questo punto "estraggo" il campo corrente
096800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096900030715     C                                                    (posA-posDa))
097000030715     C* X i campi successivi al 1°
097100030715     C                   ELSE
097200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
097300030715     C                   EVAL      posDa = posA + 1
097400031204     C* Verifico che nn vi sia il campo nn valorizzato
097500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
097600031204     C* Se campo nn valorizzato skippo al prossimo
097700031204     C                   EVAL      SkSplitCSV(i) = *blanks
097800031204     C                   EVAL      posA  = posA + 1
097900031204     C                   ELSE
098000030715     C* Eseguo lo scan x trovare la fine del campo corrente
098100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
098200030715     C* Gestisco l'ultimo campo
098300030715     C                   IF        posA = *zeros
098400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
098500030715     C* A questo punto "estraggo" il campo corrente
098600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098700030715     C                                                    (posA-posDa))
098800030715     C                   LEAVE
098900030715     C                   ELSE
099000030715     C* A questo punto "estraggo" il campo corrente
099100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
099200030715     C                                                    (posA-posDa))
099300030715     C                   ENDIF
099400031204     C                   ENDIF
099500030715     C                   ENDIF
099600030715     C* Incremento il contatore d campo
099700030715     C                   EVAL      i = i +1
099800030715     C                   ENDDO
099900030715     C*
100000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
100100030715     C* delimitazione testo
100200030715     C                   EVAL      i = 1
100300030715     C                   DOW       i < %elem(SkSplitCSV)
100400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
100500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
100600030715     C                   EVAL      i = i + 1
100700030715     C                   ENDDO
100800100720     C*
100900100720     C* Stacco un progressivo x numero spedizione
101000141006 xxx C                   EXSR      repNSP
101100100326     C*
101200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
101300030715     C                   EVAL      i = 1
101400030715     C                   DOW       i < %elem(SkSplitFLD)
101500030715     C*
101600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
101700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
101800030715     C                             SkSplitCSV(i) <> *blanks
101900030721     C*** ATB
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
102100030721     C                   EVAL      VABATB = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** RSD
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
102500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
102600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
102700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
102800030715     C* ==
102900130528     C                   ENDIF
103000030721     C*** RD2
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
103200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030715     C*** IND
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
103600030715     C                   EVAL      VABIND = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030715     C*** LOD
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
104000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
104100030715     C                   ENDIF
104200030715     C*** PRD
104300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
104400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
104500030715     C                   ENDIF
104600030721     C*** NZD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
104800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030715     C*** RMA
105100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
105200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
105300030715     C                   ENDIF
105400030715     C*** NOT
105500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
105600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
105700030715     C                   ENDIF
105800030715     C*** NT2
105900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
106000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
106100030715     C                   ENDIF
106200030715     C*** VCA
106300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
106400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
106500030715     C                   ENDIF
106600030715     C*** VAS
106700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
106800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106900030715     C                   ENDIF
107000030715     C*** FFD
107100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
107200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
107300030715     C                   ENDIF
107400030721     C*** GC1
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
107600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** GC2
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
108000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** TSP
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
108400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
108500030721     C                   ENDIF
108600040510     C*** CBO
108700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
108800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108900040510     C                   ENDIF
109000030721     C*** NAS
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
109200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** TIC
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
109600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GCA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
110000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** XCO
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
110400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** CTM
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
110800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** TCR
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
111200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** CTS
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
111600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** FTM
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
112000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** VAD
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
112400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*** GMA
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
112800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112900030721     C                   ENDIF
113000030721     C*** GGA
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
113200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
113300030721     C                   ENDIF
113400030721     C*** GVA
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
113600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
113700030721     C                   ENDIF
113800030721     C*** TC1
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
114000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
114100030721     C                   ENDIF
114200030721     C*** TC2
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
114400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
114500030721     C                   ENDIF
114600030721     C*** SCL
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
114800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114900030721     C                   ENDIF
115000030721     C*** RMO
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
115200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
115300030721     C                   ENDIF
115400030721     C*** NMO
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
115600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
115700030721     C                   ENDIF
115800030721     C*
115900030721     C* Reperisco quindi i campi numerici...
116000040510     C*** NSP
116100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
116200040510     C                   EVAL      PiStr=SkSplitCSV(i)
116300040510     C                   EXSR      CHKNUM
116400040510     C                   IF        PiInt=*on
116500040510     C                   Z-ADD     PiVal         VABNSP
116600040510     C                   ELSE
116700040510     C                   SETON                                        31
116800040510     C                   EVAL      VABNSP = *zeros
116900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
117000040510     C                             + ' ' + 'VABNSP'
117100040510     C                   ENDIF
117200040510     C                   ENDIF
117300071106     C*** RMN
117400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
117500071106     C                   EVAL      PiStr=SkSplitCSV(i)
117600071106     C                   EXSR      CHKNUM
117700071106     C                   IF        PiInt=*on
117800071106     C                   Z-ADD     PiVal         VABRMN
117900071106     C                   ELSE
118000071106     C                   SETON                                        32
118100071106     C                   EVAL      VABRMN = *zeros
118200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
118300071106     C                             + ' ' + 'VABRMN'
118400071106     C                   ENDIF
118500071106     C                   ENDIF
118600030721     C*** CAD
118700130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
118800130610     C                   EVAL      PiStr=SkSplitCSV(i)
118900130610     C                   EXSR      CHKNUM
119000130610     C                   IF        PiInt=*on
119100130610     C                   Z-ADD     PiVal         Num5_0
119200130610     C                   MOVEL(p)  Num5_0        VABCAD
119300130610     C                   ELSE
119400130610     C                   SETON                                        32
119500130610     C                   EVAL      VABCAD = *zeros
119600130610     C                   EVAL      vinmsg = %trimr(vinmsg)
119700130610     C                             + ' ' + 'VABCAD'
119800130610     C                   ENDIF
119900130605     C                   ENDIF
120000030721     C*** NCL
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on
120500030721     C                   Z-ADD     PiVal         VABNCL
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABNCL = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040122     C                             + ' ' + 'VABNCL'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** PKB
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiNum=*on
121800130610     C                   Z-ADD(H)  PiVal         VABPKB
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
123000140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
123100140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
123200140711     C                             %trim(SkSplitCSV(i)) <> ',00'
123300130703     C                   EVAL      VABVCA = 'EUR'
123400140930     C                   EVAL      VABTIC = *blank
123500140513     C                   EVAL      wFlgCAS = '1'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiNum=*on
123900030721     C                   Z-ADD     PiVal         VABCAS
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABCAS = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABCAS'
124500030721     C                   ENDIF
124600140513     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** CCM
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on      AND
125300030721     C                             PiVal<=9999999 AND
125400030721     C                             PiVal>*zeros
125500030721     C                   Z-ADD     PiVal         VABCCM
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      VABCCM = *zeros
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000040510     C                             + ' ' + 'VABCCM'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** LNP
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on    AND
126800030721     C                             PiVal<=999   AND
126900030721     C                             PiVal>*zeros
127000030721     C                   Z-ADD     PiVal         VABLNP
127100030721     C                   ELSE
127200040715     C                   SETON                                        32
127300030721     C                   EVAL      VABLNP = *zeros
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500040510     C                             + ' ' + 'VABLNP'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** NRS
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiInt=*on AND
128300030721     C                             PiVal<=99
128400030721     C                   Z-ADD     PiVal         VABNRS
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABNRS'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** LNA
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on    AND
129600030721     C                             PiVal<=999   AND
129700030721     C                             PiVal>*zeros
129800030721     C                   Z-ADD     PiVal         VABLNA
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      VABLNA = *zeros
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABLNA'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** CTR
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on  AND
131100030721     C                             PiVal<=999
131200030721     C                   Z-ADD     PiVal         VABCTR
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABCTR'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** IAS
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
132100141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
132200141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
132300141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
132400141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
132500141003     C                             %trim(SkSplitCSV(i)) <> ',00'
132600141006     C                   EVAL      VABVAS = 'EUR'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiNum=*on
133000030721     C                   Z-ADD     PiVal         VABIAS
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABIAS'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700141003     C                   ENDIF
133800030721     C*** VLB
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiNum=*on
134300130610     C                   Z-ADD(H)  PiVal         VABVLB
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABVLB'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** QFT
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiNum=*on
135500030721     C                   Z-ADD     PiVal         VABQFT
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABQFT'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** NCD
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiInt=*on      AND
136700030721     C                             PiVal<=9999999
136800030721     C                   Z-ADD     PiVal         VABNCD
136900030721     C                   ELSE
137000030721     C                   SETON                                        32
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABNCD'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500030721     C*** NCA
137600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
137700030721     C                   EVAL      PiStr=SkSplitCSV(i)
137800030721     C                   EXSR      CHKNUM
137900030721     C                   IF        PiInt=*on      AND
138000030721     C                             PiVal<=9999999
138100030721     C                   Z-ADD     PiVal         VABNCA
138200030721     C                   ELSE
138300030721     C                   SETON                                        32
138400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138500030721     C                             + ' ' + 'VABNCA'
138600030721     C                   ENDIF
138700030721     C                   ENDIF
138800030721     C*** ZNC
138900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
139000030721     C                   EVAL      PiStr=SkSplitCSV(i)
139100030721     C                   EXSR      CHKNUM
139200030721     C                   IF        PiInt=*on AND
139300030721     C                             PiVal<=99
139400030721     C                   Z-ADD     PiVal         VABZNC
139500030721     C                   ELSE
139600030721     C                   SETON                                        32
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABZNC'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100030721     C*** DCR
140200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
140300030721     C                   EVAL      PiStr=SkSplitCSV(i)
140400030721     C                   EXSR      CHKNUM
140500030721     C                   IF        PiInt=*on       AND
140600030721     C                             PiVal<=99999999
140700030721     C                   Z-ADD     PiVal         VABDCR
140800030721     C                   ELSE
140900030721     C                   SETON                                        32
141000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141100030721     C                             + ' ' + 'VABDCR'
141200030721     C                   ENDIF
141300030721     C                   ENDIF
141400030721     C*** HCR
141500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
141600030721     C                   EVAL      PiStr=SkSplitCSV(i)
141700030721     C                   EXSR      CHKNUM
141800030721     C                   IF        PiInt=*on   AND
141900030721     C                             PiVal<=9999
142000030721     C                   Z-ADD     PiVal         VABHCR
142100030721     C                   ELSE
142200030721     C                   SETON                                        32
142300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142400030721     C                             + ' ' + 'VABHCR'
142500030721     C                   ENDIF
142600030721     C                   ENDIF
142700030721     C*** VMD
142800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142900030721     C                   EVAL      PiStr=SkSplitCSV(i)
143000030721     C                   EXSR      CHKNUM
143100030721     C                   IF        PiNum=*on
143200030721     C                   Z-ADD     PiVal         VABVMD
143300030721     C                   ELSE
143400030721     C                   SETON                                        32
143500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143600030721     C                             + ' ' + 'VABVMD'
143700030721     C                   ENDIF
143800030721     C                   ENDIF
143900030721     C*** ANT
144000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
144100030721     C                   EVAL      PiStr=SkSplitCSV(i)
144200030721     C                   EXSR      CHKNUM
144300030721     C                   IF        PiInt=*on        AND
144400030721     C                             PiVal<=999999999
144500030721     C                   Z-ADD     PiVal         VABANT
144600030721     C                   ELSE
144700030721     C                   SETON                                        32
144800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144900030721     C                             + ' ' + 'VABANT'
145000030721     C                   ENDIF
145100030721     C                   ENDIF
145200030721     C*** CMO
145300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
145400030721     C                   EVAL      PiStr=SkSplitCSV(i)
145500030721     C                   EXSR      CHKNUM
145600030721     C                   IF        PiInt=*on
145700030721     C                   Z-ADD     PiVal         Num5_0
145800030721     C                   MOVEL(p)  Num5_0        VABCMO
145900030721     C                   ELSE
146000030721     C                   SETON                                        32
146100030721     C                   EVAL      VABCMO = *zeros
146200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
146300030721     C                             + ' ' + 'VABCMO'
146400030721     C                   ENDIF
146500030721     C                   ENDIF
146600040510     C*
146700040510     C* Reperisco i campi del FIVAT
146800040510     C*** VATNOT
146900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
147000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
147100040510     C                   ENDIF
147200040510     C*** VATTRC
147300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
147400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
147500040510     C                   ENDIF
147600060331     C*** VATNOT_A
147700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
147800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147900060331     C                   ENDIF
148000060331     C*** VATNOT_B
148100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
148200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
148300060331     C                   ENDIF
148400060331     C*** VATNOT_E
148500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
148600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148700060331     C                   ENDIF
148800110927     C*** VATNOT_I
148900110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
149000110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
149100110927     C                   ENDIF
149200141003     C*** VATNOT_IJ
149300141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
149400141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
149500141003     C                   ENDIF
149600140319     C*** VATNOT_S
149700140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
149800140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149900140319     C                   ENDIF
150000070823     C*
150100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
150200070925     C*
150300150114     C*** VATNOT_E finale
150400141112     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
150500141014     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
150600141006     C                   ENDIF
150700141003     C*
150800071029     C***  <===  -----------------------------------------
150900040510     C*
151000030715     C                   ENDIF
151100030715     C                   EVAL      i = i + 1
151200030715     C                   ENDDO
151300070925     C*
151400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151500140319     C*
151600141014     C* se la nazione è IT la sblanco
151700141112     C                   IF        VABNZD = 'IT' or
151800141112     C                             VABNZD = 'ITA'
151900141014     C                   EVAL      VABNZD = *blank
152000141014     C                   ENDIF
152100070925     C*
152200070925     C* Considerazioni finali su CBO/CAS
152300140220     C                   IF        wFlgCAS = '1'    OR
152400140220     C                             VABCAS  > *zeros
152500140220     C                   IF        VABCBO = '1'
152600140220     C                   EVAL      VABCBO = '4'
152700140220     C                   ENDIF
152800140220     C                   IF        VABCBO = '2'
152900140220     C                   EVAL      VABCBO = '6'
153000140220     C                   ENDIF
153100140220     C                   ENDIF
153200070925     C***  <===  ----------------------------
153300040510     C*
153400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153500040510     C                   IF        VABNSP = *zeros
153600040510     C                   SETON                                        31
153700040510     C                   ENDIF
153800040715     C*
153900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154000040715     C                   IF        VABLNP = *zeros
154100040715     C                   SETON                                        31
154200040715     C                   ENDIF
154300040510     C*
154400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154500120411     C                   IF        VABRMN = *zeros
154600120411     C                   Z-ADD     VABNSP        VABRMN
154700120411     C                   ENDIF
154800120328     C* IN QUESTO CASO LO FORZO = 1
154900120411     C***                IF        VABRMN = *zeros
155000120411     C***                Z-ADD     1             VABRMN
155100120411     C***                ENDIF
155200040510     C*
155300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155400040510     C                   IF        VABRMA = *blanks
155500040714     C                   MOVEL     VABRMN        VABRMA
155600040510     C                   ENDIF
155700040128     C*
155800040510     C* Se provincia nn valorizzata la reperisco
155900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156000110912     C                   IF        VABPRD =  *blanks AND
156100110912     C                             VABNZD =  *blanks AND
156200110912     C                             VABCAD <> *blanks AND
156300110912     C                             VABLOD <> *blanks
156400040128     C                   CLEAR                   TISI95DS
156500040128     C                   EVAL      I95TCN = '3'
156600040128     C                   Z-ADD     datcor        I95DAT
156700040128     C                   EVAL      I95NAR = VABNZD
156800040128     C                   EVAL      I95CAP = VABCAD
156900040128     C                   EVAL      I95LOC = VABLOD
157000040128     C                   CALL      'TISI95R'
157100040128     C                   PARM                    TISI95DS
157200040128     C                   EVAL      VABPRD = O95PRV
157300040510     C                   ENDIF
157400020204     C*
157500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157600020204     C                   EXSR      CHKIMPDIV
157700030715     C                   ENDIF
157800020204     C*
157900010607     C                   ENDSR
158000010607     C*----------------------------------------------------*
158100010601
158200020204
158300020204     C*----------------------------------------------------*
158400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158500020204     C*----------------------------------------------------*
158600020204     C     CHKIMPDIV     BEGSR
158700020204     C*
158800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158900020204     C                   Z-ADD     *zeros        wrkDec            9 9
159000020204     C*
159100020204     C* Come prima cosa effettuo considerazioni sulla divisa
159200020204     C                   IF        vabIAS > *zeros
159300020204     C                   IF        vabVAS <> 'EUR'
159400020204     C                   EVAL      vabVAS =  'ITL'
159500020204     C                   ENDIF
159600020204     C                   ENDIF
159700020204     C*
159800020204     C                   IF        vabCAS > *zeros
159900020204     C                   IF        vabVCA <> 'EUR'
160000020204     C                   EVAL      vabVCA =  'ITL'
160100020204     C                   ENDIF
160200020204     C                   ENDIF
160300020204     C*
160400020204     C                   IF        vabVMD > *zeros
160500020204     C                   IF        vabVAD <> 'EUR'
160600020204     C                   EVAL      vabVAD =  'ITL'
160700020204     C                   ENDIF
160800020204     C                   ENDIF
160900020204     C*
161000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161100020204     C                   Z-ADD     vabIAS        wrkDec
161200020204     C                   IF        wrkDec > *zeros
161300020204     C                   IF        vabVAS = 'ITL'
161400020204     C                   EVAL      vabIAS = *zeros
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161900020204     C                   Z-ADD     vabCAS        wrkDec
162000020204     C                   IF        wrkDec > *zeros
162100020204     C                   IF        vabVCA = 'ITL'
162200020204     C                   EVAL      vabCAS = *zeros
162300020204     C                   ENDIF
162400020204     C                   ENDIF
162500020204     C*
162600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162700020204     C                   Z-ADD     vabVMD        wrkDec
162800020204     C                   IF        wrkDec > *zeros
162900020204     C                   IF        vabVAD = 'ITL'
163000020204     C                   EVAL      vabVMD = *zeros
163100020204     C                   ENDIF
163200020204     C                   ENDIF
163300020204     C*
163400020204     C                   ENDSR
163500020204     C***
163600010330
163700100318
163800100318
163900100318     C*----------------------------------------------------*
164000100318     C*  CONTROLLO NUMERICITA' CAMPI
164100100318     C*----------------------------------------------------*
164200100318     C     CHKNUM        BEGSR
164300100318     C*
164400100318     C                   IF        PiDecChr = *blanks
164500100318     C                   EVAL      PiDecChr = CharNUM
164600100318     C                   ENDIF
164700100318     C*
164800100318     C                   callp     UBISNUM_Check(PiStr
164900100318     C                                          :PiDecChr
165000100318     C                                          :PiVal
165100100318     C                                          :PiNum
165200100318     C                                          :PiInt)
165300100318     C*
165400100318     C                   ENDSR
165500100318     C***
165600010601
165700010601
165800010601
165900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166000141006      ***
166100141014     C     repfil        BEGSR
166200141014     C*
166300141014     C                   if        invfil = *zeros and
166400141014     C                             depfil > *zeros and
166500141014     C                             (vinflg = *blanks or
166600141014     C                              vinflg = *zeros)
166700141014     C*
166800141014     C                   eval      invfil = depfil
166900141014     C                   endif
167000141014     C*
167100141014     C                   if        depfil <> invfil and
167200141014     C                             invfil > *zeros
167300141014     C                   eval      flgMulti = '1'
167400141014     C                   if        vinflg = *blanks
167500141014     C                   add       1             cntNonEl
167600141014     C                   endif
167700141014     C                   endif
167800141014     C*
167900141014     C                   if        vinflg = '2'
168000141014     C                   eval      flgStato = '2'
168100141014     C                   endif
168200141014     C*
168300141014     C                   ENDSR
168400141014     C*
168500010601
168600010601
168700010601
168800010330
168900040119      /TITLE Invio dei dati al punto operativo.
169000040119     C     invio         BEGSR
169100040119     C*
169200040119     C* 1° invio FIVAT
169300040119     C                   reset                   dscmz
169400040119     C                   move      vlrpoi        cmzdst
169500040119     C                   eval      cmzfld = 'FIVATWWR'
169600040119     C                   eval      cmzmbd = vlrhdl
169700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169800040119     C***                if        prmfir = *blanks
169900040119     C                   eval      cmzfla = 'FIVAT00F'
170000040119     C                   eval      cmzmba = 'FIVAT00F'
170100040119     C***                else
170200040119     C***                eval      cmzfla = prmfir
170300040119     C***                eval      cmzmba = prmfir
170400040119     C***                endif
170500040119     C                   eval      cmznrr = *zeros
170600040119     C                   move      §ctrokvt      cmznrr
170700040119     C                   eval      cmzlba = vlrfl1
170800040119     C                   call(e)   'TIS711C'
170900040119     C                   parm                    dscmz
171000040119     C                   parm      *blanks       esito
171100040119     C                   if        %error
171200040119     C                             or cmzerr = '1'
171300040119     C                             or esito  = '1'
171400040119     C                   eval      wrkesito = '3'
171500040119     C                   else
171600040119     C*
171700040119     C* 2° invio FIVAB
171800040119     C                   reset                   dscmz
171900040119     C                   move      vlrpoi        cmzdst
172000040119     C                   eval      cmzfld = vlrfou
172100040119     C                   eval      cmzmbd = vlrhdl
172200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172300040119     C***                if        prmfir = *blanks
172400040119     C                   eval      cmzfla = 'FIVAB00F'
172500040119     C                   eval      cmzmba = 'FIVAB00F'
172600040119     C***                else
172700040119     C***                eval      cmzfla = prmfir
172800040119     C***                eval      cmzmba = prmfir
172900040119     C***                endif
173000040119     C                   eval      cmznrr = *zeros
173100040119     C                   move      §ctrokvb      cmznrr
173200040119     C                   eval      cmzlba = vlrfl1
173300040119     C                   call(e)   'TIS711C'
173400040119     C                   parm                    dscmz
173500040119     C                   parm      *blanks       esito
173600040119     C                   if        %error
173700040119     C                             or cmzerr = '1'
173800040119     C                             or esito  = '1'
173900040119     C                   eval      wrkesito = '3'
174000040119     C                   endif
174100040119     C                   endif
174200040119     C*
174300040119     C                   ENDSR
174400040119     C***
174500010601
174600010601
174700010601
174800010601
174900010601      /TITLE Invio dei dati al punto operativo.
175000010601     C     opeini        BEGSR
175100010601     C*
175200010601     C* Inizializzo flag e contatori operativi
175300010601     C                   movel     '0'           flgGiro           1
175400010601     C                   movel     '0'           flgMulti          1
175500010601     C                   movel     '1'           flgStato          1
175600010615     C                   movel     '0'           flgOk             1
175700010601     C                   z-add     *zeros        cntNonEl         10 0
175800010601     C                   z-add     *zeros        depfil            3 0
175900010601     C                   z-add     *zeros        invfil            3 0
176000010601     C*
176100010601     C                   ENDSR
176200010601     C***
176300070326
176400070326
176500070326
176600070326
176700070326     C     *pssr         BEGSR
176800070329     C*
176900070329     C                   if        %open(tivin00r)
177000070329     C                   close     tivin00r
177100070329     C                   endif
177200070329     C                   if        %open(fivabwwr)
177300070329     C                   close     fivabwwr
177400070329     C                   endif
177500070329     C                   if        %open(fivatwwr)
177600070329     C                   close     fivatwwr
177700070329     C                   endif
177800070326     C*
177900070326     C* Effettuo la chiamata al CLLE preposto
178000070326     C                   call(e)   'TITVVTC'
178100070326     C                   parm                    parccm
178200070326     C                   parm                    parmbr
178300070326     C                   parm      '2'           paropz
178400070326     C*
178500070326     C                   eval      wrkesito = '2'
178600070404     C*
178700070404     C                   seton                                        LR
178800070326     C*
178900070326     C                   ENDSR     '*CANCL'
179000070326     C***
179100100318
179200100318
179300100318
179400100318     C     normCSV       BEGSR
179500100318     C*
179600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179700100318     C* separatori campi
179800100318     C                   if        CharTXT <> *blanks
179900100318     C                   z-add     *zeros        wLenVINDTA        4 0
180000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180100100318     C                   setoff                                       80
180200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180500100318     C                   if        not *in80
180600100318     C                   seton                                        80
180700100318     C                   else
180800100318     C                   setoff                                       80
180900100318     C                   endif
181000100318     C                   endif
181100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181200100318     C                             and *in80 = *on
181300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181400100318     C                   endif
181500100318     C                   enddo
181600100318     C                   endif
181700100318     C*
181800100318     C                   ENDSR
181900100318     C***
182000070326
182100100326
182200100326
182300100326     C*----------------------------------------------------*
182400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182500100326     C*----------------------------------------------------*
182600100326     C     REPNSP        BEGSR
182700100326     C*
182800100326     C* NSP => Stacco un numeratore da AZNUM
182900100326     C                   clear                   TRUL33DS
183000100326     C                   eval      I33OPE = *zeros
183100100326     C                   eval      I33CNU = 302
183200100326     C                   eval      I33NUM = 1
183300100326     C                   movel     TRUL33DS      KPJBU
183400100326     C                   call      'TRUL33R'
183500100326     C                   parm                    KPJBA
183600100326     C                   movel     KPJBU         TRUL33DS
183700100326     C                   if        O33ERR = *zeros
183800100326     C                   z-add     O33NRF        VABNSP
183900100326     C                   z-add     O33NRF        VATNSP
184000100326     C                   else
184100100326     C                   SETON                                        31
184200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184300140711     C                             + ' ' + 'VABNSP VATNSP'
184400100326     C                   endif
184500100326     C*
184600100326     C                   ENDSR
184700010330
184800100326
184900010330
185000000613     C     *inzsr        BEGSR
185100990910     C*
185200990910     C     *entry        plist
185300990920     C                   parm                    tivlrds
185400990921     C                   parm      wrkesito      esito
185500000724     C                   parm                    prmlit
185600000710     C                   parm                    prmfir
185700010330     C*
185800010330     C* CALCOLA LA DATA CORRENTE
185900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186000100318     C                   eval      datcor = %dec(%date() : *ISO)
186100000613     C*
186200000613     C                   ENDSR
186300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186400150115VABCCM;VABCTR;VABLNP;VABNSP;VABCBO;VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABNCL;VABPKB;
186500150114VATNOT_B;VATNOT_A;VABCAS;W_VATNOT_E;VATNOT_S;VATNOT_IJ;
186600140512
186700111116
186800100318
