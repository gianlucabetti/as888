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
007200140919     D w70             s             70
007300140919     D wNOT            s             70
007400140919     D wRSD1           S            256
007500140919     D wRSD2           S            256
007600140919     D wRSD3           S            256
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
024100010605     C                   eval      depfil = VABLNP
024200010601     C                   exsr      repfil
024300010601     C                   if        depfil = invfil
024400021025     C                   if        vlrpoi = 999
024500030822     C                   move(P)   invfil        VABFGS
024600021025     C                   else
024700030822     C                   move(P)   vlrpoi        VABFGS
024800021025     C                   endif
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
027000010604     C                   endif
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
044500060331     C                   if        wVATNOT_E <> *blanks
044600060331     C                   eval      VATTRC = 'E'
044700140527     C*
044800140711     C* NON Sviluppare i "CHI SONO"
044900140527     C*
045000140711     C                   EVAL      VATNOT = wVATNOT_E
045100140711     C                   write     FIVAT000
045200140711     C                   add       1             §CTROKVT
045300120320     C                   endif
045400110927     C*
045500110927     C                   if        wVATNOT_I <> *blanks
045600110927     C                   eval      VATNOT = wVATNOT_I
045700140711     C                   eval      VATTRC = 'I'
045800110927     C                   write     FIVAT000
045900110927     C                   add       1             §CTROKVT
046000110927     C                   endif
046100140220     C*
046200140220     C                   if        wVATNOT_J <> *blanks
046300140220     C                   eval      VATNOT = wVATNOT_J
046400140220     C                   eval      VATTRC = 'J'
046500140220     C                   write     FIVAT000
046600140220     C                   add       1             §CTROKVT
046700140220     C                   endif
046800140319     C*
046900140319     C                   if        wVATNOT_S <> *blanks
047000140319     C                   eval      VATNOT = wVATNOT_S
047100140319     C* i 2 flag devono essere blank
047200140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
047300140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047400140319     C                   eval      VATTRC = 'S'
047500140319     C                   write     FIVAT000
047600140319     C                   add       1             §CTROKVT
047700140319     C                   endif
047800040119     C*
047900040119     C                   ENDSR
048000040119
048100040119
048200040119
048300040119     C*----------------------------------------------------*
048400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048500040119     C*----------------------------------------------------*
048600040119     C     PREVAT        BEGSR
048700040119     C*
048800040119     C* Compongo il nome del membro da dare al FIVATWWR
048900040119     C                   eval      parmbr = vlrhdl
049000040119     C                   movel     'M'           parmbr
049100040510     C                   eval      parccm = vlrksc
049200040119     C                   eval      paropz = '1'
049300040119     C* Effettuo la chiamata al CLLE preposto
049400040119     C                   call(e)   'TITVVTC'
049500040119     C                   parm                    parccm
049600040119     C                   parm                    parmbr
049700040119     C                   parm                    paropz
049800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049900040119     C                   if        %error
050000040119     C                   movel     '1'           chkcall
050100040119     C                   else
050200040119     C                   movel     '0'           chkcall
050300040119     C                   endif
050400040119     C*
050500040119     C                   ENDSR
050600010601
050700010601
050800010601
050900010330     C*----------------------------------------------------*
051000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051100010330     C*----------------------------------------------------*
051200040119     C     INZVAR        BEGSR
051300080527     C*
051400080527     C                   setoff                                       65
051500010330     C*
051600030822     C* Inizializzo variabili di wrk
051700020204     C                   Z-ADD     *zeros        Num5_0            5 0
051800101018     C                   MOVEL     *blanks       wFlgCAS
051900060331     C                   MOVEL     *blanks       wVATNOT_A
052000060331     C                   MOVEL     *blanks       wVATNOT_B
052100060331     C                   MOVEL     *blanks       wVATNOT_E
052200110927     C                   MOVEL     *blanks       wVATNOT_I
052300140220     C                   MOVEL     *blanks       wVATNOT_J
052400140220     C                   MOVEL     *blanks       wVATNOT_IJ
052500140319     C                   MOVEL     *blanks       wVATNOT_S
052600120411     C                   MOVEL     *blanks       wNOT
052700120411     C                   MOVEL     *blanks       Data8A
052800030822     C*
052900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053000030822     C                   CLEAR                   SkSplitCSV
053100030822     C                   EVAL      i = 1
053200030822     C                   EVAL      posDa = *zeros
053300030822     C                   EVAL      posA  = *zeros
053400080527     C*
053500080527     C                   CLEAR                   FIVABDS
053600080527     C                   CLEAR                   FIVAT000
053700030822     C*
053800030822     C* Reimposto i valori di default
053900030822     C                   EXSR      DEFCAM
054000030822     C*
054100010330     C                   ENDSR
054200010330     C*----------------------------------------------------*
054300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054400010330     C*----------------------------------------------------*
054500010330     C     DEFCAM        BEGSR
054600030822     C*
054700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054800030715     C* e delimitatore testo.
054900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055200100318     C*
055300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055400100318     C                   EVAL      CharSOS = CharNUM
055500030822     C*
055600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055700100720     C                   SETOFF                                       505152
055800100720     C                   SETOFF                                       53
055900100413     C                   SELECT
056000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056100100720     C                   SETON                                        50
056200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056300100413     C                   SETON                                        51
056400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056500100720     C                   SETON                                        52
056600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056700100720     C                   SETON                                        53
056800100413     C                   ENDSL
056900030822     C*
057000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057100030822     C                   EVAL      posDaDft = 1
057200030822     C                   EVAL      posADft  = 0
057300030822     C                   EVAL      wGiroDft = 0
057400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057500030822     C                             posDaDft > 0
057600030822     C*
057700030822     C* Gestisco il 1° giro
057800030822     C                   IF        wGiroDft = 0
057900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058100030822     C* Incremento il contatore dei "giri"
058200030822     C                   EVAL      wGiroDft = 1
058300030822     C                   ELSE
058400030822     C                   EVAL      posDaDft = posADft
058500030822     C                   ENDIF
058600030822     C* Eseguo lo scan x trovare la fine del campo corrente
058700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058800030822     C*
058900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059100030822     C                   IF        posDaDft > 0 AND
059200030822     C                             posADft  > 0
059300030822     C* NCL
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'NCL'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABNCL
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CCM
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CCM'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABCCM
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* LNP
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'LNP'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABLNP
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* NRS
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'NRS'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiInt=*on
064200030822     C                   Z-ADD     PiVal         VABNRS
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* CTR
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'CTR'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiInt=*on
065500030822     C                   Z-ADD     PiVal         VABCTR
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* PKB
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'PKB'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800130610     C                   Z-ADD     PiVal         VABPKB
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* VLB
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VLB'
067600030822     C                   EVAL      PiStr=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   EXSR      CHKNUM
068000030822     C                   IF        PiNum=*on
068100130610     C                   Z-ADD     PiVal         VABVLB
068200030822     C                   ENDIF
068300030822     C                   ENDIF
068400030822     C* QFT
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'QFT'
068900030822     C                   EVAL      PiStr=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   EXSR      CHKNUM
069300030822     C                   IF        PiNum=*on
069400030822     C                   Z-ADD     PiVal         VABQFT
069500030822     C                   ENDIF
069600030822     C                   ENDIF
069700030822     C* CBO
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'CBO'
070200030822     C                   EVAL      VABCBO=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* TSP
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'TSP'
071100030822     C                   EVAL      VABTSP=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* VAS
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'VAS'
072000030822     C                   EVAL      VABVAS=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VCA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VCA'
072900030822     C                   EVAL      VABVCA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300070925     C* TIC
073400070925     C                   IF        %subst(
073500070925     C                             %subst(vlrppt:posDaDft+1:
073600070925     C                             posADft-posDaDft-1):1:3)
073700070925     C                             = 'TIC'
073800070925     C                   EVAL      VABTIC=%trim(%subst(
073900070925     C                             %subst(vlrppt:posDaDft+1:
074000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100070925     C                   ENDIF
074200030822     C* GCA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GCA'
074700030822     C                   EVAL      VABGCA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* CTM
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'CTM'
075600030822     C                   EVAL      VABCTM=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* FFD
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'FFD'
076500030822     C                   EVAL      VABFFD=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* VAD
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'VAD'
077400030822     C                   EVAL      VABVAD=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* GMA
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'GMA'
078300030822     C                   EVAL      VABGMA=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GGA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GGA'
079200030822     C                   EVAL      VABGGA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* GVA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'GVA'
080100030822     C                   EVAL      VABGVA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* TC1
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'TC1'
081000030822     C                   EVAL      VABTC1=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* TC2
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'TC2'
081900030822     C                   EVAL      VABTC2=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300040714     C* VATTRC
082400040714     C                   IF        %subst(
082500040714     C                             %subst(vlrppt:posDaDft+1:
082600040714     C                             posADft-posDaDft-1):1:3)
082700040714     C                             = 'TRC'
082800040714     C                   EVAL      VATTRC=%trim(%subst(
082900040714     C                             %subst(vlrppt:posDaDft+1:
083000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100040714     C                   ENDIF
083200030822     C* ...
083300030822     C                   ENDIF
083400030822     C                   ENDDO
083500020204     C*
083600010330     C                   ENDSR
083700010607     C*----------------------------------------------------*
083800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083900010607     C*----------------------------------------------------*
084000010607     C     IMPVAB        BEGSR
084100010607     C*
084200010607     C                   SETOFF                                       3132
084300110315 xxx C***                SETON                                        35
084400030515     C*
084500010607     C                   MOVEL     datcor        VABAAS
084600010607     C                   MOVE      datcor        VABMGS
084700010607     C*
084800030715     C**********
084900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085000030715     C**********
085100030715     C                   IF        wGiro = *zeros
085200040113     C*
085300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085600080429     C                             %trim(intcol(5))<>*blanks
085700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085900080429     C                                      %trim(intcol(5))
086000070823     C     ';':CharCSV   XLATE     vindta        vindta
086100040510     C                   ENDIF
086200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086300031201     C     minu:maiu     XLATE     vindta        vindta
086400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086500030715     C                   DOW       posDa <= %len(%trim(vindta))
086600030715     C*
086700030715     C* Gestisco il 1° campo
086800030715     C                   IF        i = 1
086900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087000030715     C                   EVAL      posDa = 1
087100030715     C* Eseguo lo scan x trovare la fine del primo campo
087200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C* X i campi successivi al 1°
087700030715     C                   ELSE
087800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087900030715     C                   EVAL      posDa = posA + 1
088000030715     C* Eseguo lo scan x trovare la fine del campo corrente
088100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088200030715     C* Gestisco l'ultimo campo
088300030715     C                   IF        posA = *zeros
088400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C                   LEAVE
088900030715     C                   ELSE
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C                   ENDIF
089400030715     C                   ENDIF
089500030715     C* Incremento il contatore d campo
089600030715     C                   EVAL      i = i +1
089700030715     C                   ENDDO
089800030715     C*
089900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090000030715     C* delimitazione testo
090100030715     C                   EVAL      i = 1
090200030715     C                   DOW       i < %elem(SkSplitFLD)
090300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090500030715     C                   EVAL      i = i + 1
090600030715     C                   ENDDO
090700030715     C                   EVAL      wGiro = 1
090800030715     C                   ELSE
090900030715     C                   EVAL      wGiro = 2
091000031204     C**********
091100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091200031204     C* d separatore campo
091300031204     C**********
091400031204     C                   EVAL      vindta = %trim(vindta)
091500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091600031204     C                   EVAL      vindta = %subst(vindta:2)
091700031204     C                   ENDDO
091800031204     C*
091900031204     C                   Z-ADD     *zeros        lunghInput        4 0
092000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092400110912     C                   ENDDO
092500030715     C**********
092600030715     C* Effettuo lo split del campo dati d input
092700030715     C**********
092800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092900030715     C                   DOW       posDa <= %len(%trim(vindta))
093000030715     C*
093100030715     C* Gestisco il 1° campo
093200030715     C                   IF        i = 1
093300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093400030715     C                   EVAL      posDa = 1
093500030715     C* Eseguo lo scan x trovare la fine del primo campo
093600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093700030715     C* A questo punto "estraggo" il campo corrente
093800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093900030715     C                                                    (posA-posDa))
094000030715     C* X i campi successivi al 1°
094100030715     C                   ELSE
094200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094300030715     C                   EVAL      posDa = posA + 1
094400031204     C* Verifico che nn vi sia il campo nn valorizzato
094500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094600031204     C* Se campo nn valorizzato skippo al prossimo
094700031204     C                   EVAL      SkSplitCSV(i) = *blanks
094800031204     C                   EVAL      posA  = posA + 1
094900031204     C                   ELSE
095000030715     C* Eseguo lo scan x trovare la fine del campo corrente
095100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095200030715     C* Gestisco l'ultimo campo
095300030715     C                   IF        posA = *zeros
095400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095500030715     C* A questo punto "estraggo" il campo corrente
095600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095700030715     C                                                    (posA-posDa))
095800030715     C                   LEAVE
095900030715     C                   ELSE
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C                   ENDIF
096400031204     C                   ENDIF
096500030715     C                   ENDIF
096600030715     C* Incremento il contatore d campo
096700030715     C                   EVAL      i = i +1
096800030715     C                   ENDDO
096900030715     C*
097000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097100030715     C* delimitazione testo
097200030715     C                   EVAL      i = 1
097300030715     C                   DOW       i < %elem(SkSplitCSV)
097400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097600030715     C                   EVAL      i = i + 1
097700030715     C                   ENDDO
097800100720     C*
097900100720     C* Stacco un progressivo x numero spedizione
098000140711 xxx C***                EXSR      repNSP
098100100326     C*
098200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098300030715     C                   EVAL      i = 1
098400030715     C                   DOW       i < %elem(SkSplitFLD)
098500030715     C*
098600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098800030715     C                             SkSplitCSV(i) <> *blanks
098900030721     C*** ATB
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099100030721     C                   EVAL      VABATB = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030715     C*** RSD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099800030715     C* ==
099900130528     C                   ENDIF
100000030721     C*** RD2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030715     C*** IND
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100600030715     C                   EVAL      VABIND = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** LOD
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** PRD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030721     C*** NZD
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030715     C*** RMA
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** NOT
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** NT2
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030715     C*** VCA
103300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103500030715     C                   ENDIF
103600030715     C*** VAS
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** FFD
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030721     C*** GC1
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GC2
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TSP
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105500030721     C                   ENDIF
105600040510     C*** CBO
105700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105900040510     C                   ENDIF
106000030721     C*** NAS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TIC
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GCA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** XCO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** CTM
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TCR
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** CTS
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** FTM
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** VAD
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** GMA
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** GGA
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** GVA
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** TC1
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** TC2
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** SCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** RMO
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** NMO
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*
112900030721     C* Reperisco quindi i campi numerici...
113000040510     C*** NSP
113100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113200040510     C                   EVAL      PiStr=SkSplitCSV(i)
113300040510     C                   EXSR      CHKNUM
113400040510     C                   IF        PiInt=*on
113500040510     C                   Z-ADD     PiVal         VABNSP
113600040510     C                   ELSE
113700040510     C                   SETON                                        31
113800040510     C                   EVAL      VABNSP = *zeros
113900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040510     C                             + ' ' + 'VABNSP'
114100040510     C                   ENDIF
114200040510     C                   ENDIF
114300071106     C*** RMN
114400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114500071106     C                   EVAL      PiStr=SkSplitCSV(i)
114600071106     C                   EXSR      CHKNUM
114700071106     C                   IF        PiInt=*on
114800071106     C                   Z-ADD     PiVal         VABRMN
114900071106     C                   ELSE
115000071106     C                   SETON                                        32
115100071106     C                   EVAL      VABRMN = *zeros
115200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115300071106     C                             + ' ' + 'VABRMN'
115400071106     C                   ENDIF
115500071106     C                   ENDIF
115600030721     C*** CAD
115700130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115800130610     C                   EVAL      PiStr=SkSplitCSV(i)
115900130610     C                   EXSR      CHKNUM
116000130610     C                   IF        PiInt=*on
116100130610     C                   Z-ADD     PiVal         Num5_0
116200130610     C                   MOVEL(p)  Num5_0        VABCAD
116300130610     C                   ELSE
116400130610     C                   SETON                                        32
116500130610     C                   EVAL      VABCAD = *zeros
116600130610     C                   EVAL      vinmsg = %trimr(vinmsg)
116700130610     C                             + ' ' + 'VABCAD'
116800130610     C                   ENDIF
116900130605     C                   ENDIF
117000030721     C*** NCL
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on
117500030721     C                   Z-ADD     PiVal         VABNCL
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABNCL = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040122     C                             + ' ' + 'VABNCL'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** PKB
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800130610     C                   Z-ADD(H)  PiVal         VABPKB
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABPKB = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABPKB'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** CAS
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119800110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119900111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120000140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
120100140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
120200140711     C                             %trim(SkSplitCSV(i)) <> ',00'
120300130703     C                   EVAL      VABVCA = 'EUR'
120400140711     C                   EVAL      VABTIC = 'TM'
120500140513     C                   EVAL      wFlgCAS = '1'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABCAS
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABCAS = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABCAS'
121500030721     C                   ENDIF
121600140513     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** CCM
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on      AND
122300030721     C                             PiVal<=9999999 AND
122400030721     C                             PiVal>*zeros
122500030721     C                   Z-ADD     PiVal         VABCCM
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      VABCCM = *zeros
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000040510     C                             + ' ' + 'VABCCM'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** LNP
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on    AND
123800030721     C                             PiVal<=999   AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABLNP
124100030721     C                   ELSE
124200040715     C                   SETON                                        32
124300030721     C                   EVAL      VABLNP = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500040510     C                             + ' ' + 'VABLNP'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** NRS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on AND
125300030721     C                             PiVal<=99
125400030721     C                   Z-ADD     PiVal         VABNRS
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABNRS'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** LNA
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on    AND
126600030721     C                             PiVal<=999   AND
126700030721     C                             PiVal>*zeros
126800030721     C                   Z-ADD     PiVal         VABLNA
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      VABLNA = *zeros
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABLNA'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** CTR
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on  AND
128100030721     C                             PiVal<=999
128200030721     C                   Z-ADD     PiVal         VABCTR
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABCTR'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** IAS
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABIAS
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABIAS'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
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
145500140319     C*** VATNOT_S
145600140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
145700140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
145800140319     C                   ENDIF
145900070823     C*
146000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146100140919
146200140919     C* VABNCL ==> 1 collo ogni riga della spedizione
146300140919     C*            per cui simulo che si sommino solo i colli e forzo 1 come collo X riga
146400140919     C                   EVAL      VABNCL = 1
146500140513
146600140919     C*** W_VABRSD*
146700140919     C* VABRSD/RD2 si compone da 3 parti
146800140919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_1'
146900140919     C                   EVAL      wRSD1 = %trim(SkSplitCSV(i))
147000140527     C                   ENDIF
147100140919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_2'
147200140919     C                   EVAL      wRSD2 = %trim(SkSplitCSV(i))
147300140919     C                   ENDIF
147400140919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_3'
147500140919     C                   EVAL      wRSD3 = %trim(SkSplitCSV(i))
147600140919     C                   ENDIF
147700140919
147800140919     C*** RMN
147900140919     C* RMN si prende dalla stessa colonna di NSP ma non si può valorizzare con NSP perché questo
148000140919     C* è più piccolo di dimensione e il cliente passa valori alti
148100140919     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
148200140919     C                   EVAL      PiStr=SkSplitCSV(i)
148300140919     C                   EXSR      CHKNUM
148400140919     C                   IF        PiInt=*on
148500140919     C                   Z-ADD     PiVal         VABRMN
148600140919     C                   ELSE
148700140919     C                   SETON                                        32
148800140919     C                   EVAL      VABRMN = *zeros
148900140919     C                   EVAL      vinmsg = %trimr(vinmsg)
149000140919     C                             + ' ' + 'VABRMN'
149100140919     C                   ENDIF
149200140919     C                   ENDIF
149300140919
149400140919     C*** W_VABNZD
149500140919     C* VABRSD/RD2 si compone da 3 parti
149600140919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
149700140919     C                   EVAL      VABNZD = %trim(SkSplitCSV(i))
149800140919     C                   IF        VABNZD = 'IT'
149900140919     C                   EVAL      VABNZD = *blank
150000140919     C                   ENDIF
150100140919     C                   ENDIF
150200140919
150300140919     C*** VATNOT_IJ
150400140919     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
150500140919     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
150600140919     C                   ENDIF
150700070925     C*
150800071029     C***  <===  -----------------------------------------
150900040510     C*
151000030715     C                   ENDIF
151100030715     C                   EVAL      i = i + 1
151200030715     C                   ENDDO
151300070925     C*
151400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151500140319     C*
151600140919     C*** W_VABRSD*
151700140919     C* VABRSD/RD2 si compone da 3 parti
151800140919     C                   EVAL      w70 = %trim(wRSD1) + %trim(wRSD2) +
151900140919     C                                   %trim(wRSD3)
152000140919     C                   EVAL      VABRSD = %subst(w70:1:35)
152100140919     C                   EVAL      VABRD2 = %subst(w70:36:35)
152200140919     C
152300070925     C*
152400071029     C***  <===  -----------------------------------------
152500070925     C*
152600070925     C* Considerazioni finali su CBO/CAS
152700140220     C                   IF        wFlgCAS = '1'    OR
152800140220     C                             VABCAS  > *zeros
152900140220     C                   IF        VABCBO = '1'
153000140220     C                   EVAL      VABCBO = '4'
153100140220     C                   ENDIF
153200140220     C                   IF        VABCBO = '2'
153300140220     C                   EVAL      VABCBO = '6'
153400140220     C                   ENDIF
153500140220     C                   ENDIF
153600070925     C***  <===  ----------------------------
153700040510     C*
153800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153900040510     C                   IF        VABNSP = *zeros
154000040510     C                   SETON                                        31
154100040510     C                   ENDIF
154200040715     C*
154300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154400040715     C                   IF        VABLNP = *zeros
154500040715     C                   SETON                                        31
154600040715     C                   ENDIF
154700040510     C*
154800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154900120411     C                   IF        VABRMN = *zeros
155000120411     C                   Z-ADD     VABNSP        VABRMN
155100120411     C                   ENDIF
155200120328     C* IN QUESTO CASO LO FORZO = 1
155300120411     C***                IF        VABRMN = *zeros
155400120411     C***                Z-ADD     1             VABRMN
155500120411     C***                ENDIF
155600040510     C*
155700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155800040510     C                   IF        VABRMA = *blanks
155900040714     C                   MOVEL     VABRMN        VABRMA
156000040510     C                   ENDIF
156100040128     C*
156200040510     C* Se provincia nn valorizzata la reperisco
156300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156400110912     C                   IF        VABPRD =  *blanks AND
156500110912     C                             VABNZD =  *blanks AND
156600110912     C                             VABCAD <> *blanks AND
156700110912     C                             VABLOD <> *blanks
156800040128     C                   CLEAR                   TISI95DS
156900040128     C                   EVAL      I95TCN = '3'
157000040128     C                   Z-ADD     datcor        I95DAT
157100040128     C                   EVAL      I95NAR = VABNZD
157200040128     C                   EVAL      I95CAP = VABCAD
157300040128     C                   EVAL      I95LOC = VABLOD
157400040128     C                   CALL      'TISI95R'
157500040128     C                   PARM                    TISI95DS
157600040128     C                   EVAL      VABPRD = O95PRV
157700040510     C                   ENDIF
157800020204     C*
157900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158000020204     C                   EXSR      CHKIMPDIV
158100030715     C                   ENDIF
158200020204     C*
158300010607     C                   ENDSR
158400010607     C*----------------------------------------------------*
158500010601
158600020204
158700020204     C*----------------------------------------------------*
158800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158900020204     C*----------------------------------------------------*
159000020204     C     CHKIMPDIV     BEGSR
159100020204     C*
159200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159300020204     C                   Z-ADD     *zeros        wrkDec            9 9
159400020204     C*
159500020204     C* Come prima cosa effettuo considerazioni sulla divisa
159600020204     C                   IF        vabIAS > *zeros
159700020204     C                   IF        vabVAS <> 'EUR'
159800020204     C                   EVAL      vabVAS =  'ITL'
159900020204     C                   ENDIF
160000020204     C                   ENDIF
160100020204     C*
160200020204     C                   IF        vabCAS > *zeros
160300020204     C                   IF        vabVCA <> 'EUR'
160400020204     C                   EVAL      vabVCA =  'ITL'
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C                   IF        vabVMD > *zeros
160900020204     C                   IF        vabVAD <> 'EUR'
161000020204     C                   EVAL      vabVAD =  'ITL'
161100020204     C                   ENDIF
161200020204     C                   ENDIF
161300020204     C*
161400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161500020204     C                   Z-ADD     vabIAS        wrkDec
161600020204     C                   IF        wrkDec > *zeros
161700020204     C                   IF        vabVAS = 'ITL'
161800020204     C                   EVAL      vabIAS = *zeros
161900020204     C                   ENDIF
162000020204     C                   ENDIF
162100020204     C*
162200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162300020204     C                   Z-ADD     vabCAS        wrkDec
162400020204     C                   IF        wrkDec > *zeros
162500020204     C                   IF        vabVCA = 'ITL'
162600020204     C                   EVAL      vabCAS = *zeros
162700020204     C                   ENDIF
162800020204     C                   ENDIF
162900020204     C*
163000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163100020204     C                   Z-ADD     vabVMD        wrkDec
163200020204     C                   IF        wrkDec > *zeros
163300020204     C                   IF        vabVAD = 'ITL'
163400020204     C                   EVAL      vabVMD = *zeros
163500020204     C                   ENDIF
163600020204     C                   ENDIF
163700020204     C*
163800020204     C                   ENDSR
163900020204     C***
164000010330
164100100318
164200100318
164300100318     C*----------------------------------------------------*
164400100318     C*  CONTROLLO NUMERICITA' CAMPI
164500100318     C*----------------------------------------------------*
164600100318     C     CHKNUM        BEGSR
164700100318     C*
164800100318     C                   IF        PiDecChr = *blanks
164900100318     C                   EVAL      PiDecChr = CharNUM
165000100318     C                   ENDIF
165100100318     C*
165200100318     C                   callp     UBISNUM_Check(PiStr
165300100318     C                                          :PiDecChr
165400100318     C                                          :PiVal
165500100318     C                                          :PiNum
165600100318     C                                          :PiInt)
165700100318     C*
165800100318     C                   ENDSR
165900100318     C***
166000010601
166100010601
166200010601
166300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166400010601     C     repfil        BEGSR
166500010601     C*
166600010601     C                   if        invfil = *zeros and
166700010601     C                             depfil > *zeros and
166800010629     C                             (vinflg = *blanks or
166900010629     C                              vinflg = *zeros)
167000010601     C
167100010601     C                   eval      invfil = depfil
167200010601     C                   endif
167300010601     C*
167400010601     C                   if        depfil <> invfil and
167500010601     C                             invfil > *zeros
167600010601     C                   eval      flgMulti = '1'
167700010601     C                   if        vinflg = *blanks
167800010601     C                   add       1             cntNonEl
167900010601     C                   endif
168000010601     C                   endif
168100010601     C*
168200010601     C                   if        vinflg = '2'
168300010601     C                   eval      flgStato = '2'
168400010601     C                   endif
168500010601     C*
168600010601     C                   ENDSR
168700010601     C***
168800010601
168900010601
169000010601
169100010330
169200040119      /TITLE Invio dei dati al punto operativo.
169300040119     C     invio         BEGSR
169400040119     C*
169500040119     C* 1° invio FIVAT
169600040119     C                   reset                   dscmz
169700040119     C                   move      vlrpoi        cmzdst
169800040119     C                   eval      cmzfld = 'FIVATWWR'
169900040119     C                   eval      cmzmbd = vlrhdl
170000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170100040119     C***                if        prmfir = *blanks
170200040119     C                   eval      cmzfla = 'FIVAT00F'
170300040119     C                   eval      cmzmba = 'FIVAT00F'
170400040119     C***                else
170500040119     C***                eval      cmzfla = prmfir
170600040119     C***                eval      cmzmba = prmfir
170700040119     C***                endif
170800040119     C                   eval      cmznrr = *zeros
170900040119     C                   move      §ctrokvt      cmznrr
171000040119     C                   eval      cmzlba = vlrfl1
171100040119     C                   call(e)   'TIS711C'
171200040119     C                   parm                    dscmz
171300040119     C                   parm      *blanks       esito
171400040119     C                   if        %error
171500040119     C                             or cmzerr = '1'
171600040119     C                             or esito  = '1'
171700040119     C                   eval      wrkesito = '3'
171800040119     C                   else
171900040119     C*
172000040119     C* 2° invio FIVAB
172100040119     C                   reset                   dscmz
172200040119     C                   move      vlrpoi        cmzdst
172300040119     C                   eval      cmzfld = vlrfou
172400040119     C                   eval      cmzmbd = vlrhdl
172500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
172600040119     C***                if        prmfir = *blanks
172700040119     C                   eval      cmzfla = 'FIVAB00F'
172800040119     C                   eval      cmzmba = 'FIVAB00F'
172900040119     C***                else
173000040119     C***                eval      cmzfla = prmfir
173100040119     C***                eval      cmzmba = prmfir
173200040119     C***                endif
173300040119     C                   eval      cmznrr = *zeros
173400040119     C                   move      §ctrokvb      cmznrr
173500040119     C                   eval      cmzlba = vlrfl1
173600040119     C                   call(e)   'TIS711C'
173700040119     C                   parm                    dscmz
173800040119     C                   parm      *blanks       esito
173900040119     C                   if        %error
174000040119     C                             or cmzerr = '1'
174100040119     C                             or esito  = '1'
174200040119     C                   eval      wrkesito = '3'
174300040119     C                   endif
174400040119     C                   endif
174500040119     C*
174600040119     C                   ENDSR
174700040119     C***
174800010601
174900010601
175000010601
175100010601
175200010601      /TITLE Invio dei dati al punto operativo.
175300010601     C     opeini        BEGSR
175400010601     C*
175500010601     C* Inizializzo flag e contatori operativi
175600010601     C                   movel     '0'           flgGiro           1
175700010601     C                   movel     '0'           flgMulti          1
175800010601     C                   movel     '1'           flgStato          1
175900010615     C                   movel     '0'           flgOk             1
176000010601     C                   z-add     *zeros        cntNonEl         10 0
176100010601     C                   z-add     *zeros        depfil            3 0
176200010601     C                   z-add     *zeros        invfil            3 0
176300010601     C*
176400010601     C                   ENDSR
176500010601     C***
176600070326
176700070326
176800070326
176900070326
177000070326     C     *pssr         BEGSR
177100070329     C*
177200070329     C                   if        %open(tivin00r)
177300070329     C                   close     tivin00r
177400070329     C                   endif
177500070329     C                   if        %open(fivabwwr)
177600070329     C                   close     fivabwwr
177700070329     C                   endif
177800070329     C                   if        %open(fivatwwr)
177900070329     C                   close     fivatwwr
178000070329     C                   endif
178100070326     C*
178200070326     C* Effettuo la chiamata al CLLE preposto
178300070326     C                   call(e)   'TITVVTC'
178400070326     C                   parm                    parccm
178500070326     C                   parm                    parmbr
178600070326     C                   parm      '2'           paropz
178700070326     C*
178800070326     C                   eval      wrkesito = '2'
178900070404     C*
179000070404     C                   seton                                        LR
179100070326     C*
179200070326     C                   ENDSR     '*CANCL'
179300070326     C***
179400100318
179500100318
179600100318
179700100318     C     normCSV       BEGSR
179800100318     C*
179900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180000100318     C* separatori campi
180100100318     C                   if        CharTXT <> *blanks
180200100318     C                   z-add     *zeros        wLenVINDTA        4 0
180300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180400100318     C                   setoff                                       80
180500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
180600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180800100318     C                   if        not *in80
180900100318     C                   seton                                        80
181000100318     C                   else
181100100318     C                   setoff                                       80
181200100318     C                   endif
181300100318     C                   endif
181400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
181500100318     C                             and *in80 = *on
181600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181700100318     C                   endif
181800100318     C                   enddo
181900100318     C                   endif
182000100318     C*
182100100318     C                   ENDSR
182200100318     C***
182300070326
182400100326
182500100326
182600100326     C*----------------------------------------------------*
182700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182800100326     C*----------------------------------------------------*
182900100326     C     REPNSP        BEGSR
183000100326     C*
183100100326     C* NSP => Stacco un numeratore da AZNUM
183200100326     C                   clear                   TRUL33DS
183300100326     C                   eval      I33OPE = *zeros
183400100326     C                   eval      I33CNU = 302
183500100326     C                   eval      I33NUM = 1
183600100326     C                   movel     TRUL33DS      KPJBU
183700100326     C                   call      'TRUL33R'
183800100326     C                   parm                    KPJBA
183900100326     C                   movel     KPJBU         TRUL33DS
184000100326     C                   if        O33ERR = *zeros
184100100326     C                   z-add     O33NRF        VABNSP
184200100326     C                   z-add     O33NRF        VATNSP
184300100326     C                   else
184400100326     C                   SETON                                        31
184500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
184600140711     C                             + ' ' + 'VABNSP VATNSP'
184700100326     C                   endif
184800100326     C*
184900100326     C                   ENDSR
185000010330
185100100326
185200010330
185300000613     C     *inzsr        BEGSR
185400990910     C*
185500990910     C     *entry        plist
185600990920     C                   parm                    tivlrds
185700990921     C                   parm      wrkesito      esito
185800000724     C                   parm                    prmlit
185900000710     C                   parm                    prmfir
186000010330     C*
186100010330     C* CALCOLA LA DATA CORRENTE
186200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186300100318     C                   eval      datcor = %dec(%date() : *ISO)
186400000613     C*
186500000613     C                   ENDSR
186600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186700140919VABNSP;W_VABRSD_1;W_VABRSD_2;W_VABRSD_3;VABIND;X;W_VABNZD;VABCAD;VABLOD;X;VABPKB;W_VATNOTIJ;
186800140919X;X;X;VATNOT_B;X;X;VABRMA;X;X;X;
186900140512
187000111116
187100100318
