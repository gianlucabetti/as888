000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400151118      * - disk C
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
003800150922     D depspe          s                   LIKE(VABRMN) INZ(*zeros)
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
006900060331     D wVATNOT_E       s                   LIKE(VATNOT)
007000110927     D wVATNOT_I       s                   LIKE(VATNOT)
007100140220     D wVATNOT_J       s                   LIKE(VATNOT)
007200140220     D wVATNOT_IJ      s             70
007300140319     D wVATNOT_S       s                   LIKE(VATNOT)
007400140919     D w70             s             70
007500141006     D w11             s             11
007600140919     D wNOT            s             70
007700140919     D wRSD1           S            256
007800140919     D wRSD2           S            256
007900140919     D wRSD3           S            256
008000120411     D Data8A          s              8
008100140527     D wBarcodeN       s             11s 0
008200140527     D wX              s              3s 0
008300140711     D wRMA            s                   like(VABRMA)
008400140711     D TxtInOut        S           2048
008500140711     D ElencoChar      S            256
008600140711     D TipoElenco      S              1
008700140711     D CharSost        S              1
008800140711     D UpperCase       S              1
008900140711     D ChkNull         S              1
009000140711     D CharNull        S              1
009100140711     D EsitoBon        S              1
009200030822     D*------------------
009300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
009400030822     D*------------------
009500030822     D posDaDft        S              3  0 INZ(*zeros)
009600030822     D posADft         S              3  0 INZ(*zeros)
009700030822     D j               s              3  0 INZ(1)
009800030822     D wGiroDft        s              1  0 INZ(*zeros)
009900080418     D*------------------
010000080418     D* Costanti
010100080418     D*------------------
010200080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
010300080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
010400100318
010500100318     D*------------------
010600100318     D* LINKING A DEFINIZIONI ESTERNE
010700100318     D*------------------
010800100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010900100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
011000100318
011100010330
011200010330
011300010330
011400000913     C                   reset                   rrnum
011500990921     C                   reset                   esito
011600990921     C                   reset                   wrkesito
011700010601     C*
011800010601     C                   exsr      opeini
011900010605     C                   exsr      rwvab
012000010601     C*
012100010601     C                   seton                                        lr
012200010601
012300010601
012400010601
012500010601     C*--------------------------------------------------------
012600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
012700010601     C*--------------------------------------------------------
012800010601     C     PREELA        BEGSR
012900010601     C*
013000010601     C* SE OCCORRE SPEDIRE IN FILIALE
013100010601     C                   if        invfil <> *zeros and
013200010601     C                             flgGiro = '0'
013300010601     C*
013400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
013500010601     C                   eval      flgGiro = '1'
013600010601     C*
013700010601     C                   endif
013800010601     C*
013900010601     C                   ENDSR
014000010601     C***
014100010601
014200010601
014300010601
014400010601
014500010601     C*--------------------------------------------------------
014600010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014700010601     C*--------------------------------------------------------
014800010601     C     ENDELA        BEGSR
014900040119     C*
015000040119     C* Effettuo la chiamata al CLLE preposto
015100040119     C                   call(e)   'TITVVTC'
015200040510     C                   parm                    parccm
015300040119     C                   parm                    parmbr
015400040119     C                   parm      '2'           paropz
015500050201     C*
015600050201     C* Effettuo lancio TISI95 solo x chiusura
015700050201     C                   CLEAR                   TISI95DS
015800050201     C                   EVAL      I95TLA = 'C'
015900050201     C                   CALL      'TISI95R'
016000050201     C                   PARM                    TISI95DS
016100000616     C*
016200010601     C                   ENDSR
016300010601
016400010601
016500010330     C*--------------------------------------------------------
016600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
016700010330     C*--------------------------------------------------------
016800010605     C     RWVAB         BEGSR
016900010330     C*
017000010330     C                   if        not %open(tivin00r)
017100010330     C                   open      tivin00r
017200010330     C                   endif
017300021025     C                   if        not %open(fivabwwr)
017400021025     C                   open      fivabwwr
017500010330     C                   endif
017600040119     C*
017700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017800040119     C                   exsr      prevat
017900040119     C*
018000040119     C                   if        chkcall = '0'
018100040119     C*
018200040119     C                   if        not %open(fivatwwr)
018300040119     C                   open      fivatwwr
018400040119     C                   endif
018500010330     C*
018600040119     C                   clear                   §CTROKVB          7 0
018700040119     C                   clear                   §CTROKVT          7 0
018800040119     C                   clear                   §CTRMO            7 0
018900040119     C                   clear                   §CTRNO            7 0
019000120410     C*
019100120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019200120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
019300120410     C                   IF        Intestazioni = *off
019400120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
019500120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
019600120410     C                   exsr      inzvar
019700120410     C                   exsr      impvab
019800120410     C                   ENDIF
019900010330     C*
020000030822     C                   do        *HIVAL
020100010330     C*
020200030822     C                   read      tivin00r                               70
020300150922     C*
020400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020500010618     C                   if        vinflg = '1'
020600010618     C                   eval      flgOk = '1'
020700010618     C                   endif
020800040510     C*
020900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021000040510     C                   z-add     *zeros        wLenVINDTA        4 0
021100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
021300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
021400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
021500040510     C                   z-add     1             wFlgVINDTA
021600040510     C                   leave
021700040510     C                   endif
021800040510     C                   enddo
021900010618     C*
022000040510     C                   if        vindta > *blanks AND
022100040510     C                             wFlgVINDTA = 1
022200010330     C                   add       1             rrnum
022300010330     C*
022400010601     C                   if        *in70 = *off and
022500010330     C                             (vinflg = *blanks
022600010330     C                              or vinflg = '0'
022700010330     C                              or vinflg = '2')
022800010330     C*
022900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023000010711     C                   if        vinflg = *blanks or vinflg = '0'
023100010711     C                   clear                   vinmsg
023200010711     C                   endif
023300130917     C*
023400130917     C* Aggiungo un byte a blanks all'inizio d ogni record
023500130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
023600130917     C* evitare che vengano elisi i blank
023700140220     C***                eval      vindta  = '*' + %trim(vindta)
023800130917     C*
023900080527     C                   exsr      inzvar
024000100318     C                   exsr      normCSV
024100010605     C                   exsr      impvab
024200010601     C*
024300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
024400141014     C                   eval      depfil = VABLNP
024500141014     C                   exsr      repfil
024600141014     C                   if        depfil = invfil
024700141014     C                   if        vlrpoi = 999
024800141014     C                   move(P)   invfil        VABFGS
024900141014     C                   else
025000141014     C                   move(P)   vlrpoi        VABFGS
025100141014     C                   endif
025200010601     C*
025300010601     C                   exsr      PREELA
025400010601     C*
025500010604     C* Ebbene...
025600010604     C*
025700040119     C  N31              add       1             §CTROKVB
025800040119     C   32              add       1             §CTRMO
025900040119     C   31              add       1             §CTRNO
026000080418     C*
026100030822     C                   if        wGiro = 2
026200080513     C* Scrivo sempre il VAT
026300080513     C  N31              exsr      wrivat
026400110201     C                   exsr      chkvab
026500080513     C                   endif
026600010604     C*
026700010604     C                   if        *in31 = *off and
026800010604     C                             *in32 = *off
026900010604     C                   eval      vinflg = '1'
027000010604     C                   else
027100010604     C                   eval      vinflg = '2'
027200010604     C                   endif
027300141014     C                   endif
027400010601     C*
027500010604     C                   endif
027600010604     C*
027700010330     C                   else
027800010330     C                   eval      vinflg = '1'
027900010330     C                   endif
028000010601     C*
028100010601     C  N70              update    tivin000
028200010330     C*
028300030822     C  N70              enddo
028400040119     C*
028500080527     C* Verifico dati rimasti "in cannna"...
028600080527     C                   seton                                        65
028700110201     C                   exsr      chkvab
028800080527     C*
028900040119     C                   endif                                                  (endif - chkcall)
029000010601     C*
029100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029200010601     C                   if        cntNonEl = *zeros or
029300010601     C                             flgMulti = '0'
029400010330     C* Se non ci sono record con errori ...
029500010601     C                   if        §ctrno = 0 and
029600010604     C                             §ctrmo = 0 and
029700010601     C                             flgStato <> '2'
029800010330     C* ... restituisco esito OK.
029900010330     C                   eval      wrkesito = '0'
030000010330     C                   else
030100040119     C                   if        §ctrokvb > 0
030200010330     C                   eval      wrkesito = '1'
030300010330     C                   else
030400010615     C                   if        flgOk = '0'
030500010615     C                   eval      wrkesito = '2'
030600010615     C                   else
030700010615     C                   eval      wrkesito = '6'
030800010615     C                   endif
030900010330     C                   endif
031000010330     C                   endif
031100010601     C                   else
031200010601     C                   eval      wrkesito = '9'
031300010601     C                   endif
031400010330     C*
031500010330     C                   if        %open(tivin00r)
031600010330     C                   close     tivin00r
031700010330     C                   endif
031800021025     C                   if        %open(fivabwwr)
031900021025     C                   close     fivabwwr
032000010330     C                   endif
032100040512     C                   if        %open(fivatwwr)
032200040512     C                   close     fivatwwr
032300040512     C                   endif
032400010601     C*
032500010601     C                   if        vlrpoi <> 999
032600010601     C                   eval      invfil = vlrpoi
032700010601     C                   endif
032800010330     C*
032900040119     C                   if        §ctrokvb > 0
033000010601     C                             and invfil > *zeros
033100010330     C                   exsr      invio
033200010330     C                   endif
033300010601     C*
033400010618     C                   if        flgGiro = '1'
033500010601     C                   exsr      endela
033600010618     C                   endif
033700010330     C*
033800010330     C                   ENDSR
033900010330     C***
034000080527
034100080527
034200080527
034300080527     C*----------------------------------------------------*
034400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034500080527     C*----------------------------------------------------*
034600100413     C     CHKVAB        BEGSR
034700110201     C*
034800110201     C* Se richiesti ignorare colli / peso / volume
034900110315 xxx C***                if        *in35
035000110315 xxx C***                z-add     *zeros        VABNCL
035100110315 xxx C***                z-add     *zeros        VABPKB
035200110315 xxx C***                z-add     *zeros        VABVLB
035300110315 xxx C***                endif
035400080527     C*
035500080527     C* Scrivo il VAB solo a rottura d spedizione
035600080527     C                   if        depspe = *zeros
035700080527     C  N31              eval      fivabds_sav = fivabds
035800150922     C                   eval      depspe = vabRMN
035900080527     C                   else
036000150922     C                   if        depspe = vabRMN AND *in65 = *off
036100100413     C   51              add       VABNCL        sav_VABNCL
036200100413     C   51              add       VABPKB        sav_VABPKB
036300100413     C   51              add       VABVLB        sav_VABVLB
036400100720     C   52              z-add     VABNCL        sav_VABNCL
036500100720     C   52              z-add     VABPKB        sav_VABPKB
036600100720     C   52              z-add     VABVLB        sav_VABVLB
036700100720     C   53              add       VABNCL        sav_VABNCL
036800100720     C   53              z-add     VABPKB        sav_VABPKB
036900100720     C   53              z-add     VABVLB        sav_VABVLB
037000150922     C                   else
037100080527     C  N31              eval      fivabds_ok = fivabds_sav
037200080527     C  N31              exsr      wrivab
037300080527     C  N31              eval      fivabds_sav = fivabds
037400150922     C                   eval      depspe = vabRMN
037500080527     C                   endif
037600080527     C                   endif
037700080527     C*
037800080527     C                   ENDSR
037900040119
038000040119
038100040119
038200040119     C*----------------------------------------------------*
038300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
038400040119     C*----------------------------------------------------*
038500040510     C     WRIVAB        BEGSR
038600080513     C*
038700080513     C* Gestisco l'eventuale rottura x numero spedizione
038800150922     C                   if        VABRMN <> depspe or *in65 = *on
038900070301     C                   if        wVATNOT_E <> *blanks
039000130610     C                   eval      ok_VABCTM = '7Q'
039100070301     C                   endif
039200100413     C                   write     fivab000
039300080513     C                   endif
039400040119     C*
039500040119     C                   ENDSR
039600040119
039700040119
039800040119
039900040119     C*----------------------------------------------------*
040000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040100040119     C*----------------------------------------------------*
040200040510     C     WRIVAT        BEGSR
040300060512     C*
040400060512     C* Inizializzo i campi chiave
040500060512     C                   eval      VATFGS = VABFGS
040600060512     C                   eval      VATAAS = VABAAS
040700060512     C                   eval      VATCCM = VABCCM
040800070823     C                   eval      VATNRS = VABNRS
040900060512     C                   eval      VATNSP = VABNSP
041000060512     C                   eval      VATLNP = VABLNP
041100040119     C*
041200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041300040510     C                   if        VATTRC = *blanks
041400040510     C                   eval      VATTRC = 'E'
041500040510     C                   endif
041600110318     C                   if        VATNOT <> *blanks
041700040510     C                   write     FIVAT000
041800040119     C                   add       1             §CTROKVT
041900110318     C                   endif
042000060331     C*
042100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042200060331     C                   if        wVATNOT_A <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_A
042400060331     C                   eval      VATTRC = 'A'
042500060331     C                   write     FIVAT000
042600060331     C                   add       1             §CTROKVT
042700060331     C                   endif
042800060331     C*
042900060331     C                   if        wVATNOT_B <> *blanks
043000060331     C                   eval      VATNOT = wVATNOT_B
043100060331     C                   eval      VATTRC = 'B'
043200060331     C                   write     FIVAT000
043300060331     C                   add       1             §CTROKVT
043400060331     C                   endif
043500140711     C*
043600140711     C                   if        wVATNOT_IJ <> *blanks
043700140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043800140711     C                   eval      VATTRC = 'I'
043900140711     C                   write     FIVAT000
044000140711     C                   add       1             §CTROKVT
044100140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
044200140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044300140711     C                   eval      VATTRC = 'J'
044400140711     C                   write     FIVAT000
044500140711     C                   endif
044600140711     C                   endif
044700060331     C*
044800060331     C                   if        wVATNOT_E <> *blanks
044900060331     C                   eval      VATTRC = 'E'
045000140527     C*
045100150309     C* NON Sviluppo i "CHI SONO"
045200140527     C*
045300150309     C                   eval      VATNOT = wVATNOT_E
045400150114     C                   write     FIVAT000
045500150114     C                   add       1             §CTROKVT
045600120320     C                   endif
045700110927     C*
045800110927     C                   if        wVATNOT_I <> *blanks
045900110927     C                   eval      VATNOT = wVATNOT_I
046000140711     C                   eval      VATTRC = 'I'
046100110927     C                   write     FIVAT000
046200110927     C                   add       1             §CTROKVT
046300110927     C                   endif
046400140220     C*
046500140220     C                   if        wVATNOT_J <> *blanks
046600140220     C                   eval      VATNOT = wVATNOT_J
046700140220     C                   eval      VATTRC = 'J'
046800140220     C                   write     FIVAT000
046900140220     C                   add       1             §CTROKVT
047000140220     C                   endif
047100140319     C*
047200140319     C                   if        wVATNOT_S <> *blanks
047300140319     C                   eval      VATNOT = wVATNOT_S
047400140319     C* i 2 flag devono essere blank
047500140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
047600140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
047700140319     C                   eval      VATTRC = 'S'
047800140319     C                   write     FIVAT000
047900140319     C                   add       1             §CTROKVT
048000140319     C                   endif
048100040119     C*
048200040119     C                   ENDSR
048300040119
048400040119
048500040119
048600040119     C*----------------------------------------------------*
048700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048800040119     C*----------------------------------------------------*
048900040119     C     PREVAT        BEGSR
049000040119     C*
049100040119     C* Compongo il nome del membro da dare al FIVATWWR
049200040119     C                   eval      parmbr = vlrhdl
049300040119     C                   movel     'M'           parmbr
049400040510     C                   eval      parccm = vlrksc
049500040119     C                   eval      paropz = '1'
049600040119     C* Effettuo la chiamata al CLLE preposto
049700040119     C                   call(e)   'TITVVTC'
049800040119     C                   parm                    parccm
049900040119     C                   parm                    parmbr
050000040119     C                   parm                    paropz
050100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050200040119     C                   if        %error
050300040119     C                   movel     '1'           chkcall
050400040119     C                   else
050500040119     C                   movel     '0'           chkcall
050600040119     C                   endif
050700040119     C*
050800040119     C                   ENDSR
050900010601
051000010601
051100010601
051200010330     C*----------------------------------------------------*
051300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051400010330     C*----------------------------------------------------*
051500040119     C     INZVAR        BEGSR
051600080527     C*
051700080527     C                   setoff                                       65
051800010330     C*
051900030822     C* Inizializzo variabili di wrk
052000020204     C                   Z-ADD     *zeros        Num5_0            5 0
052100101018     C                   MOVEL     *blanks       wFlgCAS
052200060331     C                   MOVEL     *blanks       wVATNOT_A
052300060331     C                   MOVEL     *blanks       wVATNOT_B
052400060331     C                   MOVEL     *blanks       wVATNOT_E
052500110927     C                   MOVEL     *blanks       wVATNOT_I
052600140220     C                   MOVEL     *blanks       wVATNOT_J
052700140220     C                   MOVEL     *blanks       wVATNOT_IJ
052800140319     C                   MOVEL     *blanks       wVATNOT_S
052900120411     C                   MOVEL     *blanks       wNOT
053000120411     C                   MOVEL     *blanks       Data8A
053100030822     C*
053200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053300030822     C                   CLEAR                   SkSplitCSV
053400030822     C                   EVAL      i = 1
053500030822     C                   EVAL      posDa = *zeros
053600030822     C                   EVAL      posA  = *zeros
053700080527     C*
053800080527     C                   CLEAR                   FIVABDS
053900080527     C                   CLEAR                   FIVAT000
054000030822     C*
054100030822     C* Reimposto i valori di default
054200030822     C                   EXSR      DEFCAM
054300030822     C*
054400010330     C                   ENDSR
054500010330     C*----------------------------------------------------*
054600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054700010330     C*----------------------------------------------------*
054800010330     C     DEFCAM        BEGSR
054900030822     C*
055000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055100030715     C* e delimitatore testo.
055200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055500100318     C*
055600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055700100318     C                   EVAL      CharSOS = CharNUM
055800030822     C*
055900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056000100720     C                   SETOFF                                       505152
056100100720     C                   SETOFF                                       53
056200100413     C                   SELECT
056300100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
056400100720     C                   SETON                                        50
056500100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056600100413     C                   SETON                                        51
056700100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056800100720     C                   SETON                                        52
056900100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057000100720     C                   SETON                                        53
057100100413     C                   ENDSL
057200030822     C*
057300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057400030822     C                   EVAL      posDaDft = 1
057500030822     C                   EVAL      posADft  = 0
057600030822     C                   EVAL      wGiroDft = 0
057700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057800030822     C                             posDaDft > 0
057900030822     C*
058000030822     C* Gestisco il 1° giro
058100030822     C                   IF        wGiroDft = 0
058200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058400030822     C* Incremento il contatore dei "giri"
058500030822     C                   EVAL      wGiroDft = 1
058600030822     C                   ELSE
058700030822     C                   EVAL      posDaDft = posADft
058800030822     C                   ENDIF
058900030822     C* Eseguo lo scan x trovare la fine del campo corrente
059000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059100030822     C*
059200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059400030822     C                   IF        posDaDft > 0 AND
059500030822     C                             posADft  > 0
059600030822     C* NCL
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'NCL'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABNCL
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* CCM
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'CCM'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABCCM
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* LNP
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'LNP'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200030822     C                   Z-ADD     PiVal         VABLNP
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* NRS
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'NRS'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiInt=*on
064500030822     C                   Z-ADD     PiVal         VABNRS
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* CTR
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CTR'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiInt=*on
065800030822     C                   Z-ADD     PiVal         VABCTR
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* PKB
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'PKB'
066600030822     C                   EVAL      PiStr=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   EXSR      CHKNUM
067000030822     C                   IF        PiNum=*on
067100130610     C                   Z-ADD     PiVal         VABPKB
067200030822     C                   ENDIF
067300030822     C                   ENDIF
067400030822     C* VLB
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VLB'
067900030822     C                   EVAL      PiStr=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   EXSR      CHKNUM
068300030822     C                   IF        PiNum=*on
068400130610     C                   Z-ADD     PiVal         VABVLB
068500030822     C                   ENDIF
068600030822     C                   ENDIF
068700030822     C* QFT
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'QFT'
069200030822     C                   EVAL      PiStr=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   EXSR      CHKNUM
069600030822     C                   IF        PiNum=*on
069700030822     C                   Z-ADD     PiVal         VABQFT
069800030822     C                   ENDIF
069900030822     C                   ENDIF
070000030822     C* CBO
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'CBO'
070500030822     C                   EVAL      VABCBO=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* TSP
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'TSP'
071400030822     C                   EVAL      VABTSP=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VAS
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VAS'
072300030822     C                   EVAL      VABVAS=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VCA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VCA'
073200030822     C                   EVAL      VABVCA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600070925     C* TIC
073700070925     C                   IF        %subst(
073800070925     C                             %subst(vlrppt:posDaDft+1:
073900070925     C                             posADft-posDaDft-1):1:3)
074000070925     C                             = 'TIC'
074100070925     C                   EVAL      VABTIC=%trim(%subst(
074200070925     C                             %subst(vlrppt:posDaDft+1:
074300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400070925     C                   ENDIF
074500030822     C* GCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GCA'
075000030822     C                   EVAL      VABGCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* CTM
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'CTM'
075900030822     C                   EVAL      VABCTM=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* FFD
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'FFD'
076800030822     C                   EVAL      VABFFD=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* VAD
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'VAD'
077700030822     C                   EVAL      VABVAD=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GMA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GMA'
078600030822     C                   EVAL      VABGMA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GGA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GGA'
079500030822     C                   EVAL      VABGGA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GVA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GVA'
080400030822     C                   EVAL      VABGVA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* TC1
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'TC1'
081300030822     C                   EVAL      VABTC1=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* TC2
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'TC2'
082200030822     C                   EVAL      VABTC2=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600040714     C* VATTRC
082700040714     C                   IF        %subst(
082800040714     C                             %subst(vlrppt:posDaDft+1:
082900040714     C                             posADft-posDaDft-1):1:3)
083000040714     C                             = 'TRC'
083100040714     C                   EVAL      VATTRC=%trim(%subst(
083200040714     C                             %subst(vlrppt:posDaDft+1:
083300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400040714     C                   ENDIF
083500030822     C* ...
083600030822     C                   ENDIF
083700030822     C                   ENDDO
083800020204     C*
083900010330     C                   ENDSR
084000010607     C*----------------------------------------------------*
084100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084200010607     C*----------------------------------------------------*
084300010607     C     IMPVAB        BEGSR
084400010607     C*
084500010607     C                   SETOFF                                       3132
084600110315 xxx C***                SETON                                        35
084700030515     C*
084800010607     C                   MOVEL     datcor        VABAAS
084900010607     C                   MOVE      datcor        VABMGS
085000010607     C*
085100030715     C**********
085200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085300030715     C**********
085400030715     C                   IF        wGiro = *zeros
085500040113     C*
085600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085900080429     C                             %trim(intcol(5))<>*blanks
086000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086200080429     C                                      %trim(intcol(5))
086300070823     C     ';':CharCSV   XLATE     vindta        vindta
086400040510     C                   ENDIF
086500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086600031201     C     minu:maiu     XLATE     vindta        vindta
086700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086800030715     C                   DOW       posDa <= %len(%trim(vindta))
086900030715     C*
087000030715     C* Gestisco il 1° campo
087100030715     C                   IF        i = 1
087200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087300030715     C                   EVAL      posDa = 1
087400030715     C* Eseguo lo scan x trovare la fine del primo campo
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* A questo punto "estraggo" il campo corrente
087700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087800030715     C                                                    (posA-posDa))
087900030715     C* X i campi successivi al 1°
088000030715     C                   ELSE
088100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088200030715     C                   EVAL      posDa = posA + 1
088300030715     C* Eseguo lo scan x trovare la fine del campo corrente
088400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088500030715     C* Gestisco l'ultimo campo
088600030715     C                   IF        posA = *zeros
088700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   LEAVE
089200030715     C                   ELSE
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   ENDIF
089700030715     C                   ENDIF
089800030715     C* Incremento il contatore d campo
089900030715     C                   EVAL      i = i +1
090000030715     C                   ENDDO
090100030715     C*
090200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090300030715     C* delimitazione testo
090400030715     C                   EVAL      i = 1
090500030715     C                   DOW       i < %elem(SkSplitFLD)
090600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090800030715     C                   EVAL      i = i + 1
090900030715     C                   ENDDO
091000030715     C                   EVAL      wGiro = 1
091100030715     C                   ELSE
091200030715     C                   EVAL      wGiro = 2
091300031204     C**********
091400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091500031204     C* d separatore campo
091600031204     C**********
091700031204     C                   EVAL      vindta = %trim(vindta)
091800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091900031204     C                   EVAL      vindta = %subst(vindta:2)
092000031204     C                   ENDDO
092100031204     C*
092200031204     C                   Z-ADD     *zeros        lunghInput        4 0
092300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092700110912     C                   ENDDO
092800030715     C**********
092900030715     C* Effettuo lo split del campo dati d input
093000030715     C**********
093100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093200030715     C                   DOW       posDa <= %len(%trim(vindta))
093300030715     C*
093400030715     C* Gestisco il 1° campo
093500030715     C                   IF        i = 1
093600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093700030715     C                   EVAL      posDa = 1
093800030715     C* Eseguo lo scan x trovare la fine del primo campo
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* A questo punto "estraggo" il campo corrente
094100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094200030715     C                                                    (posA-posDa))
094300030715     C* X i campi successivi al 1°
094400030715     C                   ELSE
094500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094600030715     C                   EVAL      posDa = posA + 1
094700031204     C* Verifico che nn vi sia il campo nn valorizzato
094800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094900031204     C* Se campo nn valorizzato skippo al prossimo
095000031204     C                   EVAL      SkSplitCSV(i) = *blanks
095100031204     C                   EVAL      posA  = posA + 1
095200031204     C                   ELSE
095300030715     C* Eseguo lo scan x trovare la fine del campo corrente
095400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095500030715     C* Gestisco l'ultimo campo
095600030715     C                   IF        posA = *zeros
095700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   LEAVE
096200030715     C                   ELSE
096300030715     C* A questo punto "estraggo" il campo corrente
096400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096500030715     C                                                    (posA-posDa))
096600030715     C                   ENDIF
096700031204     C                   ENDIF
096800030715     C                   ENDIF
096900030715     C* Incremento il contatore d campo
097000030715     C                   EVAL      i = i +1
097100030715     C                   ENDDO
097200030715     C*
097300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097400030715     C* delimitazione testo
097500030715     C                   EVAL      i = 1
097600030715     C                   DOW       i < %elem(SkSplitCSV)
097700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097900030715     C                   EVAL      i = i + 1
098000030715     C                   ENDDO
098100100720     C*
098200100720     C* Stacco un progressivo x numero spedizione
098300151103 xxx C                   EXSR      repNSP
098400100326     C*
098500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098600030715     C                   EVAL      i = 1
098700030715     C                   DOW       i < %elem(SkSplitFLD)
098800030715     C*
098900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099100030715     C                             SkSplitCSV(i) <> *blanks
099200030721     C*** ATB
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
099400030721     C                   EVAL      VABATB = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RSD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100100030715     C* ==
100200130528     C                   ENDIF
100300030721     C*** RD2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030715     C*** IND
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100900030715     C                   EVAL      VABIND = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** LOD
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** PRD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030721     C*** NZD
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030715     C*** RMA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NOT
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NT2
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040510     C*** CBO
106000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106200040510     C                   ENDIF
106300030721     C*** NAS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TIC
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GCA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** XCO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TCR
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** CTS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** FTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** VAD
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GMA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GGA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GVA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC1
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC2
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** SCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** RMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** NMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*
113200030721     C* Reperisco quindi i campi numerici...
113300040510     C*** NSP
113400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113500040510     C                   EVAL      PiStr=SkSplitCSV(i)
113600040510     C                   EXSR      CHKNUM
113700040510     C                   IF        PiInt=*on
113800040510     C                   Z-ADD     PiVal         VABNSP
113900040510     C                   ELSE
114000040510     C                   SETON                                        31
114100040510     C                   EVAL      VABNSP = *zeros
114200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABNSP'
114400040510     C                   ENDIF
114500040510     C                   ENDIF
114600071106     C*** RMN
114700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114800071106     C                   EVAL      PiStr=SkSplitCSV(i)
114900071106     C                   EXSR      CHKNUM
115000071106     C                   IF        PiInt=*on
115100071106     C                   Z-ADD     PiVal         VABRMN
115200071106     C                   ELSE
115300071106     C                   SETON                                        32
115400071106     C                   EVAL      VABRMN = *zeros
115500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115600071106     C                             + ' ' + 'VABRMN'
115700071106     C                   ENDIF
115800071106     C                   ENDIF
115900030721     C*** CAD
116000130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116100130610     C                   EVAL      PiStr=SkSplitCSV(i)
116200130610     C                   EXSR      CHKNUM
116300130610     C                   IF        PiInt=*on
116400130610     C                   Z-ADD     PiVal         Num5_0
116500130610     C                   MOVEL(p)  Num5_0        VABCAD
116600130610     C                   ELSE
116700130610     C                   SETON                                        32
116800130610     C                   EVAL      VABCAD = *zeros
116900130610     C                   EVAL      vinmsg = %trimr(vinmsg)
117000130610     C                             + ' ' + 'VABCAD'
117100130610     C                   ENDIF
117200130605     C                   ENDIF
117300030721     C*** NCL
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on
117800030721     C                   Z-ADD     PiVal         VABNCL
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABNCL = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040122     C                             + ' ' + 'VABNCL'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** PKB
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100130610     C                   Z-ADD(H)  PiVal         VABPKB
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABPKB = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABPKB'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** CAS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120100110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120200151103     C                             %trim(SkSplitCSV(i)) <> *zeros
120300130703     C                   EVAL      VABVCA = 'EUR'
120400140930     C                   EVAL      VABTIC = *blank
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
129100141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
129200141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
129300141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
129400141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
129500141003     C                             %trim(SkSplitCSV(i)) <> ',00'
129600141006     C                   EVAL      VABVAS = 'EUR'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiNum=*on
130000030721     C                   Z-ADD     PiVal         VABIAS
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABIAS'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700141003     C                   ENDIF
130800030721     C*** VLB
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300130610     C                   Z-ADD(H)  PiVal         VABVLB
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABVLB'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** QFT
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiNum=*on
132500030721     C                   Z-ADD     PiVal         VABQFT
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABQFT'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** NCD
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on      AND
133700030721     C                             PiVal<=9999999
133800030721     C                   Z-ADD     PiVal         VABNCD
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABNCD'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** NCA
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on      AND
135000030721     C                             PiVal<=9999999
135100030721     C                   Z-ADD     PiVal         VABNCA
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABNCA'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** ZNC
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on AND
136300030721     C                             PiVal<=99
136400030721     C                   Z-ADD     PiVal         VABZNC
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABZNC'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** DCR
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on       AND
137600030721     C                             PiVal<=99999999
137700030721     C                   Z-ADD     PiVal         VABDCR
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138100030721     C                             + ' ' + 'VABDCR'
138200030721     C                   ENDIF
138300030721     C                   ENDIF
138400030721     C*** HCR
138500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138600030721     C                   EVAL      PiStr=SkSplitCSV(i)
138700030721     C                   EXSR      CHKNUM
138800030721     C                   IF        PiInt=*on   AND
138900030721     C                             PiVal<=9999
139000030721     C                   Z-ADD     PiVal         VABHCR
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABHCR'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** VMD
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiNum=*on
140200030721     C                   Z-ADD     PiVal         VABVMD
140300030721     C                   ELSE
140400030721     C                   SETON                                        32
140500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140600030721     C                             + ' ' + 'VABVMD'
140700030721     C                   ENDIF
140800030721     C                   ENDIF
140900030721     C*** ANT
141000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141100030721     C                   EVAL      PiStr=SkSplitCSV(i)
141200030721     C                   EXSR      CHKNUM
141300030721     C                   IF        PiInt=*on        AND
141400030721     C                             PiVal<=999999999
141500030721     C                   Z-ADD     PiVal         VABANT
141600030721     C                   ELSE
141700030721     C                   SETON                                        32
141800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141900030721     C                             + ' ' + 'VABANT'
142000030721     C                   ENDIF
142100030721     C                   ENDIF
142200030721     C*** CMO
142300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142400030721     C                   EVAL      PiStr=SkSplitCSV(i)
142500030721     C                   EXSR      CHKNUM
142600030721     C                   IF        PiInt=*on
142700030721     C                   Z-ADD     PiVal         Num5_0
142800030721     C                   MOVEL(p)  Num5_0        VABCMO
142900030721     C                   ELSE
143000030721     C                   SETON                                        32
143100030721     C                   EVAL      VABCMO = *zeros
143200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143300030721     C                             + ' ' + 'VABCMO'
143400030721     C                   ENDIF
143500030721     C                   ENDIF
143600040510     C*
143700040510     C* Reperisco i campi del FIVAT
143800040510     C*** VATNOT
143900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144100040510     C                   ENDIF
144200040510     C*** VATTRC
144300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144500040510     C                   ENDIF
144600060331     C*** VATNOT_A
144700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144900060331     C                   ENDIF
145000060331     C*** VATNOT_B
145100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145300060331     C                   ENDIF
145400060331     C*** VATNOT_E
145500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145700060331     C                   ENDIF
145800110927     C*** VATNOT_I
145900110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146000110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146100110927     C                   ENDIF
146200141003     C*** VATNOT_IJ
146300141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
146400141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
146500141003     C                   ENDIF
146600140319     C*** VATNOT_S
146700140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
146800140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
146900140319     C                   ENDIF
147000070823     C*
147100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147200150309     C*
147300151118     C***  RMN / RMA / VATNOT_E
147400151103     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
147500151118     C* il valore tutto intero è il codice a barre
147600151118     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147700151118     C* solo i primi 9 char sono numerici e generano RMN e RMA
147800151118     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):1:9)
147900150309     C                   EXSR      CHKNUM
148000150309     C                   IF        PiInt=*on
148100150309     C                   Z-ADD     PiVal         VABRMN
148200150922     C                   EVAL      VABRMA = %char(VABRMN)
148300150309     C                   ELSE
148400150309     C                   SETON                                        31
148500150309     C                   EVAL      VABRMN = *zeros
148600150309     C                   EVAL      vinmsg = %trimr(vinmsg)
148700150922     C                             + ' ' + 'VABRMA VABRMN'
148800150309     C                   ENDIF
148900150309     C                   ENDIF
149000150922     C*
149100151118     C*** TSP
149200151103     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
149300151103     C                   SELECT
149400151103     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
149500151103     C                   EVAL      VABTC1 = 'A'
149600151103     C                   WHEN      %trim(SkSplitCSV(i)) = 'E'
149700151103     C                   EVAL      VABTSP = 'E'
149800151103     C                   WHEN      %trim(SkSplitCSV(i)) = 'H'
149900151103     C                   EVAL      VABTSP = 'H'
150000151103     C                   ENDSL
150100151103     C                   ENDIF
150200151118     C*
150300151118     C*** VABNOT / NT2
150400151118     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
150500151118     C                   EVAL      w70 = %trim(SkSplitCSV(i))
150600151118     C                   EVAL      VABNOT = %subst(w70:1:35)
150700151118     C                   EVAL      VABNT2 = %subst(w70:36:35)
150800151118     C                   ENDIF
150900141003     C*
151000071029     C***  <===  -----------------------------------------
151100040510     C*
151200030715     C                   ENDIF
151300030715     C                   EVAL      i = i + 1
151400030715     C                   ENDDO
151500070925     C*
151600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151700140319     C*
151800141014     C* se la nazione è IT la sblanco
151900141112     C                   IF        VABNZD = 'IT' or
152000141112     C                             VABNZD = 'ITA'
152100141014     C                   EVAL      VABNZD = *blank
152200141014     C                   ENDIF
152300070925     C*
152400070925     C* Considerazioni finali su CBO/CAS
152500140220     C                   IF        wFlgCAS = '1'    OR
152600140220     C                             VABCAS  > *zeros
152700140220     C                   IF        VABCBO = '1'
152800140220     C                   EVAL      VABCBO = '4'
152900140220     C                   ENDIF
153000140220     C                   IF        VABCBO = '2'
153100140220     C                   EVAL      VABCBO = '6'
153200140220     C                   ENDIF
153300140220     C                   ENDIF
153400070925     C***  <===  ----------------------------
153500040510     C*
153600150922     C* la prima volta
153700150922     C                   if        depspe = 0
153800150922     C* oppure a rottura spedizione
153900150922     C                             or depspe <> vabRMN
154000150922     C* Stacco un progressivo x numero spedizione
154100150922 xxx C                   EXSR      repNSP
154200150922     C*
154300150922     C* durante la stessa spedizione
154400150922     C                   ELSE
154500150922     C* reperisco il precedente numero spedizione
154600150922     C                   EVAL      VABNSP = sav_VABNSP
154700150922     C                   ENDIF
154800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154900150922     C                   IF        VABNSP = *zeros
155000150922     C                   SETON                                        31
155100150922     C                   ENDIF
155200040715     C*
155300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155400040715     C                   IF        VABLNP = *zeros
155500040715     C                   SETON                                        31
155600040715     C                   ENDIF
155700040510     C*
155800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155900120411     C                   IF        VABRMN = *zeros
156000120411     C                   Z-ADD     VABNSP        VABRMN
156100120411     C                   ENDIF
156200120328     C* IN QUESTO CASO LO FORZO = 1
156300120411     C***                IF        VABRMN = *zeros
156400120411     C***                Z-ADD     1             VABRMN
156500120411     C***                ENDIF
156600040510     C*
156700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156800040510     C                   IF        VABRMA = *blanks
156900040714     C                   MOVEL     VABRMN        VABRMA
157000040510     C                   ENDIF
157100040128     C*
157200040510     C* Se provincia nn valorizzata la reperisco
157300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157400110912     C                   IF        VABPRD =  *blanks AND
157500110912     C                             VABNZD =  *blanks AND
157600110912     C                             VABCAD <> *blanks AND
157700110912     C                             VABLOD <> *blanks
157800040128     C                   CLEAR                   TISI95DS
157900040128     C                   EVAL      I95TCN = '3'
158000040128     C                   Z-ADD     datcor        I95DAT
158100040128     C                   EVAL      I95NAR = VABNZD
158200040128     C                   EVAL      I95CAP = VABCAD
158300040128     C                   EVAL      I95LOC = VABLOD
158400040128     C                   CALL      'TISI95R'
158500040128     C                   PARM                    TISI95DS
158600040128     C                   EVAL      VABPRD = O95PRV
158700040510     C                   ENDIF
158800020204     C*
158900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
159000020204     C                   EXSR      CHKIMPDIV
159100030715     C                   ENDIF
159200020204     C*
159300010607     C                   ENDSR
159400010607     C*----------------------------------------------------*
159500010601
159600020204
159700020204     C*----------------------------------------------------*
159800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159900020204     C*----------------------------------------------------*
160000020204     C     CHKIMPDIV     BEGSR
160100020204     C*
160200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160300020204     C                   Z-ADD     *zeros        wrkDec            9 9
160400020204     C*
160500020204     C* Come prima cosa effettuo considerazioni sulla divisa
160600020204     C                   IF        vabIAS > *zeros
160700020204     C                   IF        vabVAS <> 'EUR'
160800020204     C                   EVAL      vabVAS =  'ITL'
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C                   IF        vabCAS > *zeros
161300020204     C                   IF        vabVCA <> 'EUR'
161400020204     C                   EVAL      vabVCA =  'ITL'
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C                   IF        vabVMD > *zeros
161900020204     C                   IF        vabVAD <> 'EUR'
162000020204     C                   EVAL      vabVAD =  'ITL'
162100020204     C                   ENDIF
162200020204     C                   ENDIF
162300020204     C*
162400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162500020204     C                   Z-ADD     vabIAS        wrkDec
162600020204     C                   IF        wrkDec > *zeros
162700020204     C                   IF        vabVAS = 'ITL'
162800020204     C                   EVAL      vabIAS = *zeros
162900020204     C                   ENDIF
163000020204     C                   ENDIF
163100020204     C*
163200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163300020204     C                   Z-ADD     vabCAS        wrkDec
163400020204     C                   IF        wrkDec > *zeros
163500020204     C                   IF        vabVCA = 'ITL'
163600020204     C                   EVAL      vabCAS = *zeros
163700020204     C                   ENDIF
163800020204     C                   ENDIF
163900020204     C*
164000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164100020204     C                   Z-ADD     vabVMD        wrkDec
164200020204     C                   IF        wrkDec > *zeros
164300020204     C                   IF        vabVAD = 'ITL'
164400020204     C                   EVAL      vabVMD = *zeros
164500020204     C                   ENDIF
164600020204     C                   ENDIF
164700020204     C*
164800020204     C                   ENDSR
164900020204     C***
165000010330
165100100318
165200100318
165300100318     C*----------------------------------------------------*
165400100318     C*  CONTROLLO NUMERICITA' CAMPI
165500100318     C*----------------------------------------------------*
165600100318     C     CHKNUM        BEGSR
165700100318     C*
165800100318     C                   IF        PiDecChr = *blanks
165900100318     C                   EVAL      PiDecChr = CharNUM
166000100318     C                   ENDIF
166100100318     C*
166200100318     C                   callp     UBISNUM_Check(PiStr
166300100318     C                                          :PiDecChr
166400100318     C                                          :PiVal
166500100318     C                                          :PiNum
166600100318     C                                          :PiInt)
166700100318     C*
166800100318     C                   ENDSR
166900100318     C***
167000010601
167100010601
167200010601
167300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167400141006      ***
167500141014     C     repfil        BEGSR
167600141014     C*
167700141014     C                   if        invfil = *zeros and
167800141014     C                             depfil > *zeros and
167900141014     C                             (vinflg = *blanks or
168000141014     C                              vinflg = *zeros)
168100141014     C*
168200141014     C                   eval      invfil = depfil
168300141014     C                   endif
168400141014     C*
168500141014     C                   if        depfil <> invfil and
168600141014     C                             invfil > *zeros
168700141014     C                   eval      flgMulti = '1'
168800141014     C                   if        vinflg = *blanks
168900141014     C                   add       1             cntNonEl
169000141014     C                   endif
169100141014     C                   endif
169200141014     C*
169300141014     C                   if        vinflg = '2'
169400141014     C                   eval      flgStato = '2'
169500141014     C                   endif
169600141014     C*
169700141014     C                   ENDSR
169800141014     C*
169900010601
170000010601
170100010601
170200010330
170300040119      /TITLE Invio dei dati al punto operativo.
170400040119     C     invio         BEGSR
170500040119     C*
170600040119     C* 1° invio FIVAT
170700040119     C                   reset                   dscmz
170800040119     C                   move      vlrpoi        cmzdst
170900040119     C                   eval      cmzfld = 'FIVATWWR'
171000040119     C                   eval      cmzmbd = vlrhdl
171100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171200040119     C***                if        prmfir = *blanks
171300040119     C                   eval      cmzfla = 'FIVAT00F'
171400040119     C                   eval      cmzmba = 'FIVAT00F'
171500040119     C***                else
171600040119     C***                eval      cmzfla = prmfir
171700040119     C***                eval      cmzmba = prmfir
171800040119     C***                endif
171900040119     C                   eval      cmznrr = *zeros
172000040119     C                   move      §ctrokvt      cmznrr
172100040119     C                   eval      cmzlba = vlrfl1
172200040119     C                   call(e)   'TIS711C'
172300040119     C                   parm                    dscmz
172400040119     C                   parm      *blanks       esito
172500040119     C                   if        %error
172600040119     C                             or cmzerr = '1'
172700040119     C                             or esito  = '1'
172800040119     C                   eval      wrkesito = '3'
172900040119     C                   else
173000040119     C*
173100040119     C* 2° invio FIVAB
173200040119     C                   reset                   dscmz
173300040119     C                   move      vlrpoi        cmzdst
173400040119     C                   eval      cmzfld = vlrfou
173500040119     C                   eval      cmzmbd = vlrhdl
173600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173700040119     C***                if        prmfir = *blanks
173800040119     C                   eval      cmzfla = 'FIVAB00F'
173900040119     C                   eval      cmzmba = 'FIVAB00F'
174000040119     C***                else
174100040119     C***                eval      cmzfla = prmfir
174200040119     C***                eval      cmzmba = prmfir
174300040119     C***                endif
174400040119     C                   eval      cmznrr = *zeros
174500040119     C                   move      §ctrokvb      cmznrr
174600040119     C                   eval      cmzlba = vlrfl1
174700040119     C                   call(e)   'TIS711C'
174800040119     C                   parm                    dscmz
174900040119     C                   parm      *blanks       esito
175000040119     C                   if        %error
175100040119     C                             or cmzerr = '1'
175200040119     C                             or esito  = '1'
175300040119     C                   eval      wrkesito = '3'
175400040119     C                   endif
175500040119     C                   endif
175600040119     C*
175700040119     C                   ENDSR
175800040119     C***
175900010601
176000010601
176100010601
176200010601
176300010601      /TITLE Invio dei dati al punto operativo.
176400010601     C     opeini        BEGSR
176500010601     C*
176600010601     C* Inizializzo flag e contatori operativi
176700010601     C                   movel     '0'           flgGiro           1
176800010601     C                   movel     '0'           flgMulti          1
176900010601     C                   movel     '1'           flgStato          1
177000010615     C                   movel     '0'           flgOk             1
177100010601     C                   z-add     *zeros        cntNonEl         10 0
177200010601     C                   z-add     *zeros        depfil            3 0
177300010601     C                   z-add     *zeros        invfil            3 0
177400010601     C*
177500010601     C                   ENDSR
177600010601     C***
177700070326
177800070326
177900070326
178000070326
178100070326     C     *pssr         BEGSR
178200070329     C*
178300070329     C                   if        %open(tivin00r)
178400070329     C                   close     tivin00r
178500070329     C                   endif
178600070329     C                   if        %open(fivabwwr)
178700070329     C                   close     fivabwwr
178800070329     C                   endif
178900070329     C                   if        %open(fivatwwr)
179000070329     C                   close     fivatwwr
179100070329     C                   endif
179200070326     C*
179300070326     C* Effettuo la chiamata al CLLE preposto
179400070326     C                   call(e)   'TITVVTC'
179500070326     C                   parm                    parccm
179600070326     C                   parm                    parmbr
179700070326     C                   parm      '2'           paropz
179800070326     C*
179900070326     C                   eval      wrkesito = '2'
180000070404     C*
180100070404     C                   seton                                        LR
180200070326     C*
180300070326     C                   ENDSR     '*CANCL'
180400070326     C***
180500100318
180600100318
180700100318
180800100318     C     normCSV       BEGSR
180900100318     C*
181000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
181100100318     C* separatori campi
181200100318     C                   if        CharTXT <> *blanks
181300100318     C                   z-add     *zeros        wLenVINDTA        4 0
181400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181500100318     C                   setoff                                       80
181600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181900100318     C                   if        not *in80
182000100318     C                   seton                                        80
182100100318     C                   else
182200100318     C                   setoff                                       80
182300100318     C                   endif
182400100318     C                   endif
182500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182600100318     C                             and *in80 = *on
182700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182800100318     C                   endif
182900100318     C                   enddo
183000100318     C                   endif
183100100318     C*
183200100318     C                   ENDSR
183300100318     C***
183400070326
183500100326
183600100326
183700100326     C*----------------------------------------------------*
183800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183900100326     C*----------------------------------------------------*
184000100326     C     REPNSP        BEGSR
184100100326     C*
184200100326     C* NSP => Stacco un numeratore da AZNUM
184300100326     C                   clear                   TRUL33DS
184400100326     C                   eval      I33OPE = *zeros
184500100326     C                   eval      I33CNU = 302
184600100326     C                   eval      I33NUM = 1
184700100326     C                   movel     TRUL33DS      KPJBU
184800100326     C                   call      'TRUL33R'
184900100326     C                   parm                    KPJBA
185000100326     C                   movel     KPJBU         TRUL33DS
185100100326     C                   if        O33ERR = *zeros
185200100326     C                   z-add     O33NRF        VABNSP
185300100326     C                   z-add     O33NRF        VATNSP
185400100326     C                   else
185500100326     C                   SETON                                        31
185600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185700140711     C                             + ' ' + 'VABNSP VATNSP'
185800100326     C                   endif
185900100326     C*
186000100326     C                   ENDSR
186100010330
186200100326
186300010330
186400000613     C     *inzsr        BEGSR
186500990910     C*
186600990910     C     *entry        plist
186700990920     C                   parm                    tivlrds
186800990921     C                   parm      wrkesito      esito
186900000724     C                   parm                    prmlit
187000000710     C                   parm                    prmfir
187100010330     C*
187200010330     C* CALCOLA LA DATA CORRENTE
187300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187400100318     C                   eval      datcor = %dec(%date() : *ISO)
187500000613     C*
187600000613     C                   ENDSR
187700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187800151118W_VABRMN;X;VABRSD;VATNOT_A;VABIND;X;VABLOD;X;VABCAD;X;VATNOT_B;VATNOT_I;VABNCL;VABPKB;W_VABNOT;
187900151103W_VABTSP;VABCAS;X;
188000140512
188100111116
188200100318
