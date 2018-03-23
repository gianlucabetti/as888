000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400151104      * - file strutturato un rcd x collo ma con i dati quantità/valore x spedizione
000500151117      * - il nr.colli è uno per riga della spedizione
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
003900151117     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004000151117     D*depspe          s                   LIKE(VABRMN) INZ(*zeros)
004100080418     D wFlgCAS         s              1    INZ(*blanks)
004200151117     D wCAD            s                   like(VABCAD) inz(*blank)
004300120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004400120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004500151104     D Intestazioni    s               n   inz(*off)
004600100326     D*------------------
004700100326     D* DS REPERIMENTO NUMERATORE
004800100326     D*------------------
004900100326     D trul33ds      e ds                  inz
005000100326     D*------------------
005100100326     D* DS ARCHITETTURA
005200100326     D*------------------
005300100326     D kpjba         e ds                  inz
005400130611     D*------------------
005500130611     D TRUL28DSX     e ds                  inz
005600100326
005700030715     D*------------------
005800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005900030715     D*------------------
006000030715     D SkSplitFLD      S             10    DIM(100)
006100030715     D SkSplitCSV      S            256    DIM(100)
006200030715     D CharCSV         S              1
006300030715     D CharTXT         S              1
006400030715     D CharNUM         S              1
006500100318     D CharSOS         S              1
006600130626     D posDa           S              4  0 INZ(*zeros)
006700130626     D posA            S              4  0 INZ(*zeros)
006800030715     D i               s              3  0 INZ(1)
006900030715     D wGiro           s              1  0 INZ(*zeros)
007000070823     D wVATNOT_A       s                   LIKE(VATNOT)
007100060331     D wVATNOT_B       s                   LIKE(VATNOT)
007200060331     D wVATNOT_E       s                   LIKE(VATNOT)
007300110927     D wVATNOT_I       s                   LIKE(VATNOT)
007400140220     D wVATNOT_J       s                   LIKE(VATNOT)
007500140220     D wVATNOT_IJ      s             70
007600140319     D wVATNOT_S       s                   LIKE(VATNOT)
007700140919     D w70             s             70
007800141006     D w11             s             11
007900140919     D wNOT            s             70
008000140919     D wRSD1           S            256
008100140919     D wRSD2           S            256
008200140919     D wRSD3           S            256
008300120411     D Data8A          s              8
008400140527     D wBarcodeN       s             11s 0
008500140527     D wX              s              3s 0
008600140711     D wRMA            s                   like(VABRMA)
008700151104     D TxtInOut        S           2048
008800140711     D ElencoChar      S            256
008900140711     D TipoElenco      S              1
009000140711     D CharSost        S              1
009100140711     D UpperCase       S              1
009200140711     D ChkNull         S              1
009300140711     D CharNull        S              1
009400140711     D EsitoBon        S              1
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
020600150922     C*
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
025000141014     C                   if        vlrpoi = 999
025100141014     C                   move(P)   invfil        VABFGS
025200141014     C                   else
025300141014     C                   move(P)   vlrpoi        VABFGS
025400141014     C                   endif
025500010601     C*
025600010601     C                   exsr      PREELA
025700010601     C*
025800010604     C* Ebbene...
025900010604     C*
026000040119     C  N31              add       1             §CTROKVB
026100040119     C   32              add       1             §CTRMO
026200040119     C   31              add       1             §CTRNO
026300080418     C*
026400030822     C                   if        wGiro = 2
026500080513     C* Scrivo sempre il VAT
026600080513     C  N31              exsr      wrivat
026700110201     C                   exsr      chkvab
026800080513     C                   endif
026900010604     C*
027000010604     C                   if        *in31 = *off and
027100010604     C                             *in32 = *off
027200010604     C                   eval      vinflg = '1'
027300010604     C                   else
027400010604     C                   eval      vinflg = '2'
027500010604     C                   endif
027600141014     C                   endif
027700010601     C*
027800010604     C                   endif
027900010604     C*
028000010330     C                   else
028100010330     C                   eval      vinflg = '1'
028200010330     C                   endif
028300010601     C*
028400010601     C  N70              update    tivin000
028500010330     C*
028600030822     C  N70              enddo
028700040119     C*
028800080527     C* Verifico dati rimasti "in cannna"...
028900080527     C                   seton                                        65
029000110201     C                   exsr      chkvab
029100080527     C*
029200040119     C                   endif                                                  (endif - chkcall)
029300010601     C*
029400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029500010601     C                   if        cntNonEl = *zeros or
029600010601     C                             flgMulti = '0'
029700010330     C* Se non ci sono record con errori ...
029800010601     C                   if        §ctrno = 0 and
029900010604     C                             §ctrmo = 0 and
030000010601     C                             flgStato <> '2'
030100010330     C* ... restituisco esito OK.
030200010330     C                   eval      wrkesito = '0'
030300010330     C                   else
030400040119     C                   if        §ctrokvb > 0
030500010330     C                   eval      wrkesito = '1'
030600010330     C                   else
030700010615     C                   if        flgOk = '0'
030800010615     C                   eval      wrkesito = '2'
030900010615     C                   else
031000010615     C                   eval      wrkesito = '6'
031100010615     C                   endif
031200010330     C                   endif
031300010330     C                   endif
031400010601     C                   else
031500010601     C                   eval      wrkesito = '9'
031600010601     C                   endif
031700010330     C*
031800010330     C                   if        %open(tivin00r)
031900010330     C                   close     tivin00r
032000010330     C                   endif
032100021025     C                   if        %open(fivabwwr)
032200021025     C                   close     fivabwwr
032300010330     C                   endif
032400040512     C                   if        %open(fivatwwr)
032500040512     C                   close     fivatwwr
032600040512     C                   endif
032700010601     C*
032800010601     C                   if        vlrpoi <> 999
032900010601     C                   eval      invfil = vlrpoi
033000010601     C                   endif
033100010330     C*
033200040119     C                   if        §ctrokvb > 0
033300010601     C                             and invfil > *zeros
033400010330     C                   exsr      invio
033500010330     C                   endif
033600010601     C*
033700010618     C                   if        flgGiro = '1'
033800010601     C                   exsr      endela
033900010618     C                   endif
034000010330     C*
034100010330     C                   ENDSR
034200010330     C***
034300080527
034400080527
034500080527
034600080527     C*----------------------------------------------------*
034700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
034800080527     C*----------------------------------------------------*
034900100413     C     CHKVAB        BEGSR
035000110201     C*
035100110201     C* Se richiesti ignorare colli / peso / volume
035200110315 xxx C***                if        *in35
035300110315 xxx C***                z-add     *zeros        VABNCL
035400110315 xxx C***                z-add     *zeros        VABPKB
035500110315 xxx C***                z-add     *zeros        VABVLB
035600110315 xxx C***                endif
035700080527     C*
035800080527     C* Scrivo il VAB solo a rottura d spedizione
035900080527     C                   if        depspe = *zeros
036000151117     C* i colli sono sempre uno per riga, questa è la prima riga per cui valgono 1
036100151117     C                   eval      VABNCL = 1
036200080527     C  N31              eval      fivabds_sav = fivabds
036300151117     C***                eval      depspe = vabRMN
036400151117     C                   eval      depspe = vabNSP
036500080527     C                   else
036600151117     C***                if        depspe = vabRMN AND *in65 = *off
036700151117     C                   if        depspe = vabNSP AND *in65 = *off
036800151117     C* PER EVITARE CHE FACCIA UNO DI QUESTE SOMMATORIE, IMPOSTO IL PRIMO CARATTERE DI ppt = S
036900100413     C   51              add       VABNCL        sav_VABNCL
037000100413     C   51              add       VABPKB        sav_VABPKB
037100100413     C   51              add       VABVLB        sav_VABVLB
037200100720     C   52              z-add     VABNCL        sav_VABNCL
037300100720     C   52              z-add     VABPKB        sav_VABPKB
037400100720     C   52              z-add     VABVLB        sav_VABVLB
037500100720     C   53              add       VABNCL        sav_VABNCL
037600100720     C   53              z-add     VABPKB        sav_VABPKB
037700100720     C   53              z-add     VABVLB        sav_VABVLB
037800151117     C* il nr.colli è uno per riga della spedizione
037900151117     C***                eval      sav_VABNCL += 1
038000151117     C                   eval      sav_VABNCL = sav_VABNCL + 1
038100150922     C                   else
038200080527     C  N31              eval      fivabds_ok = fivabds_sav
038300080527     C  N31              exsr      wrivab
038400080527     C  N31              eval      fivabds_sav = fivabds
038500151117     C***                eval      depspe = vabRMN
038600151117     C                   eval      depspe = vabNSP
038700080527     C                   endif
038800080527     C                   endif
038900080527     C*
039000080527     C                   ENDSR
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAB        BEGSR
039800080513     C*
039900080513     C* Gestisco l'eventuale rottura x numero spedizione
040000151117     C***                if        VABRMN <> depspe or *in65 = *on
040100151117     C                   if        VABNSP <> depspe or *in65 = *on
040200070301     C                   if        wVATNOT_E <> *blanks
040300130610     C                   eval      ok_VABCTM = '7Q'
040400070301     C                   endif
040500100413     C                   write     fivab000
040600080513     C                   endif
040700040119     C*
040800040119     C                   ENDSR
040900040119
041000040119
041100040119
041200040119     C*----------------------------------------------------*
041300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041400040119     C*----------------------------------------------------*
041500040510     C     WRIVAT        BEGSR
041600060512     C*
041700060512     C* Inizializzo i campi chiave
041800060512     C                   eval      VATFGS = VABFGS
041900060512     C                   eval      VATAAS = VABAAS
042000060512     C                   eval      VATCCM = VABCCM
042100070823     C                   eval      VATNRS = VABNRS
042200060512     C                   eval      VATNSP = VABNSP
042300060512     C                   eval      VATLNP = VABLNP
042400040119     C*
042500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
042600040510     C                   if        VATTRC = *blanks
042700040510     C                   eval      VATTRC = 'E'
042800040510     C                   endif
042900110318     C                   if        VATNOT <> *blanks
043000040510     C                   write     FIVAT000
043100040119     C                   add       1             §CTROKVT
043200110318     C                   endif
043300060331     C*
043400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043500060331     C                   if        wVATNOT_A <> *blanks
043600060331     C                   eval      VATNOT = wVATNOT_A
043700060331     C                   eval      VATTRC = 'A'
043800060331     C                   write     FIVAT000
043900060331     C                   add       1             §CTROKVT
044000060331     C                   endif
044100060331     C*
044200060331     C                   if        wVATNOT_B <> *blanks
044300060331     C                   eval      VATNOT = wVATNOT_B
044400060331     C                   eval      VATTRC = 'B'
044500060331     C                   write     FIVAT000
044600060331     C                   add       1             §CTROKVT
044700060331     C                   endif
044800140711     C*
044900140711     C                   if        wVATNOT_IJ <> *blanks
045000140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045100140711     C                   eval      VATTRC = 'I'
045200140711     C                   write     FIVAT000
045300140711     C                   add       1             §CTROKVT
045400140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045500140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045600140711     C                   eval      VATTRC = 'J'
045700140711     C                   write     FIVAT000
045800140711     C                   endif
045900140711     C                   endif
046000060331     C*
046100060331     C                   if        wVATNOT_E <> *blanks
046200060331     C                   eval      VATTRC = 'E'
046300140527     C*
046400150309     C* NON Sviluppo i "CHI SONO"
046500140527     C*
046600150309     C                   eval      VATNOT = wVATNOT_E
046700150114     C                   write     FIVAT000
046800150114     C                   add       1             §CTROKVT
046900120320     C                   endif
047000110927     C*
047100110927     C                   if        wVATNOT_I <> *blanks
047200110927     C                   eval      VATNOT = wVATNOT_I
047300140711     C                   eval      VATTRC = 'I'
047400110927     C                   write     FIVAT000
047500110927     C                   add       1             §CTROKVT
047600110927     C                   endif
047700140220     C*
047800140220     C                   if        wVATNOT_J <> *blanks
047900140220     C                   eval      VATNOT = wVATNOT_J
048000140220     C                   eval      VATTRC = 'J'
048100140220     C                   write     FIVAT000
048200140220     C                   add       1             §CTROKVT
048300140220     C                   endif
048400140319     C*
048500140319     C                   if        wVATNOT_S <> *blanks
048600140319     C                   eval      VATNOT = wVATNOT_S
048700140319     C* i 2 flag devono essere blank
048800140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
048900140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049000140319     C                   eval      VATTRC = 'S'
049100140319     C                   write     FIVAT000
049200140319     C                   add       1             §CTROKVT
049300140319     C                   endif
049400040119     C*
049500040119     C                   ENDSR
049600040119
049700040119
049800040119
049900040119     C*----------------------------------------------------*
050000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050100040119     C*----------------------------------------------------*
050200040119     C     PREVAT        BEGSR
050300040119     C*
050400040119     C* Compongo il nome del membro da dare al FIVATWWR
050500040119     C                   eval      parmbr = vlrhdl
050600040119     C                   movel     'M'           parmbr
050700040510     C                   eval      parccm = vlrksc
050800040119     C                   eval      paropz = '1'
050900040119     C* Effettuo la chiamata al CLLE preposto
051000040119     C                   call(e)   'TITVVTC'
051100040119     C                   parm                    parccm
051200040119     C                   parm                    parmbr
051300040119     C                   parm                    paropz
051400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051500040119     C                   if        %error
051600040119     C                   movel     '1'           chkcall
051700040119     C                   else
051800040119     C                   movel     '0'           chkcall
051900040119     C                   endif
052000040119     C*
052100040119     C                   ENDSR
052200010601
052300010601
052400010601
052500010330     C*----------------------------------------------------*
052600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052700010330     C*----------------------------------------------------*
052800040119     C     INZVAR        BEGSR
052900080527     C*
053000080527     C                   setoff                                       65
053100010330     C*
053200030822     C* Inizializzo variabili di wrk
053300020204     C                   Z-ADD     *zeros        Num5_0            5 0
053400101018     C                   MOVEL     *blanks       wFlgCAS
053500060331     C                   MOVEL     *blanks       wVATNOT_A
053600060331     C                   MOVEL     *blanks       wVATNOT_B
053700060331     C                   MOVEL     *blanks       wVATNOT_E
053800110927     C                   MOVEL     *blanks       wVATNOT_I
053900140220     C                   MOVEL     *blanks       wVATNOT_J
054000140220     C                   MOVEL     *blanks       wVATNOT_IJ
054100140319     C                   MOVEL     *blanks       wVATNOT_S
054200120411     C                   MOVEL     *blanks       wNOT
054300120411     C                   MOVEL     *blanks       Data8A
054400030822     C*
054500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054600030822     C                   CLEAR                   SkSplitCSV
054700030822     C                   EVAL      i = 1
054800030822     C                   EVAL      posDa = *zeros
054900030822     C                   EVAL      posA  = *zeros
055000080527     C*
055100080527     C                   CLEAR                   FIVABDS
055200080527     C                   CLEAR                   FIVAT000
055300030822     C*
055400030822     C* Reimposto i valori di default
055500030822     C                   EXSR      DEFCAM
055600030822     C*
055700010330     C                   ENDSR
055800010330     C*----------------------------------------------------*
055900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056000010330     C*----------------------------------------------------*
056100010330     C     DEFCAM        BEGSR
056200030822     C*
056300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056400030715     C* e delimitatore testo.
056500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056800100318     C*
056900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057000100318     C                   EVAL      CharSOS = CharNUM
057100030822     C*
057200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057300100720     C                   SETOFF                                       505152
057400100720     C                   SETOFF                                       53
057500100413     C                   SELECT
057600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057700100720     C                   SETON                                        50
057800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057900100413     C                   SETON                                        51
058000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058100100720     C                   SETON                                        52
058200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058300100720     C                   SETON                                        53
058400100413     C                   ENDSL
058500030822     C*
058600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
058700030822     C                   EVAL      posDaDft = 1
058800030822     C                   EVAL      posADft  = 0
058900030822     C                   EVAL      wGiroDft = 0
059000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059100030822     C                             posDaDft > 0
059200030822     C*
059300030822     C* Gestisco il 1° giro
059400030822     C                   IF        wGiroDft = 0
059500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
059600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
059700030822     C* Incremento il contatore dei "giri"
059800030822     C                   EVAL      wGiroDft = 1
059900030822     C                   ELSE
060000030822     C                   EVAL      posDaDft = posADft
060100030822     C                   ENDIF
060200030822     C* Eseguo lo scan x trovare la fine del campo corrente
060300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060400030822     C*
060500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
060600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
060700030822     C                   IF        posDaDft > 0 AND
060800030822     C                             posADft  > 0
060900030822     C* NCL
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'NCL'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiInt=*on
061900030822     C                   Z-ADD     PiVal         VABNCL
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* CCM
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'CCM'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiInt=*on
063200030822     C                   Z-ADD     PiVal         VABCCM
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* LNP
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'LNP'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiInt=*on
064500030822     C                   Z-ADD     PiVal         VABLNP
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* NRS
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'NRS'
065300030822     C                   EVAL      PiStr=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   EXSR      CHKNUM
065700030822     C                   IF        PiInt=*on
065800030822     C                   Z-ADD     PiVal         VABNRS
065900030822     C                   ENDIF
066000030822     C                   ENDIF
066100030822     C* CTR
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'CTR'
066600030822     C                   EVAL      PiStr=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   EXSR      CHKNUM
067000030822     C                   IF        PiInt=*on
067100030822     C                   Z-ADD     PiVal         VABCTR
067200030822     C                   ENDIF
067300030822     C                   ENDIF
067400030822     C* PKB
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'PKB'
067900030822     C                   EVAL      PiStr=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   EXSR      CHKNUM
068300030822     C                   IF        PiNum=*on
068400130610     C                   Z-ADD     PiVal         VABPKB
068500030822     C                   ENDIF
068600030822     C                   ENDIF
068700030822     C* VLB
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VLB'
069200030822     C                   EVAL      PiStr=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   EXSR      CHKNUM
069600030822     C                   IF        PiNum=*on
069700130610     C                   Z-ADD     PiVal         VABVLB
069800030822     C                   ENDIF
069900030822     C                   ENDIF
070000030822     C* QFT
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'QFT'
070500030822     C                   EVAL      PiStr=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   EXSR      CHKNUM
070900030822     C                   IF        PiNum=*on
071000030822     C                   Z-ADD     PiVal         VABQFT
071100030822     C                   ENDIF
071200030822     C                   ENDIF
071300030822     C* CBO
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'CBO'
071800030822     C                   EVAL      VABCBO=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* TSP
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'TSP'
072700030822     C                   EVAL      VABTSP=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100030822     C* VAS
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'VAS'
073600030822     C                   EVAL      VABVAS=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* VCA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'VCA'
074500030822     C                   EVAL      VABVCA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900070925     C* TIC
075000070925     C                   IF        %subst(
075100070925     C                             %subst(vlrppt:posDaDft+1:
075200070925     C                             posADft-posDaDft-1):1:3)
075300070925     C                             = 'TIC'
075400070925     C                   EVAL      VABTIC=%trim(%subst(
075500070925     C                             %subst(vlrppt:posDaDft+1:
075600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700070925     C                   ENDIF
075800030822     C* GCA
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'GCA'
076300030822     C                   EVAL      VABGCA=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* CTM
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'CTM'
077200030822     C                   EVAL      VABCTM=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* FFD
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'FFD'
078100030822     C                   EVAL      VABFFD=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* VAD
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'VAD'
079000030822     C                   EVAL      VABVAD=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* GMA
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'GMA'
079900030822     C                   EVAL      VABGMA=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* GGA
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'GGA'
080800030822     C                   EVAL      VABGGA=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* GVA
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'GVA'
081700030822     C                   EVAL      VABGVA=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100030822     C* TC1
082200030822     C                   IF        %subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):1:3)
082500030822     C                             = 'TC1'
082600030822     C                   EVAL      VABTC1=%trim(%subst(
082700030822     C                             %subst(vlrppt:posDaDft+1:
082800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900030822     C                   ENDIF
083000030822     C* TC2
083100030822     C                   IF        %subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):1:3)
083400030822     C                             = 'TC2'
083500030822     C                   EVAL      VABTC2=%trim(%subst(
083600030822     C                             %subst(vlrppt:posDaDft+1:
083700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083800030822     C                   ENDIF
083900040714     C* VATTRC
084000040714     C                   IF        %subst(
084100040714     C                             %subst(vlrppt:posDaDft+1:
084200040714     C                             posADft-posDaDft-1):1:3)
084300040714     C                             = 'TRC'
084400040714     C                   EVAL      VATTRC=%trim(%subst(
084500040714     C                             %subst(vlrppt:posDaDft+1:
084600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084700040714     C                   ENDIF
084800030822     C* ...
084900030822     C                   ENDIF
085000030822     C                   ENDDO
085100020204     C*
085200010330     C                   ENDSR
085300010607     C*----------------------------------------------------*
085400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
085500010607     C*----------------------------------------------------*
085600010607     C     IMPVAB        BEGSR
085700010607     C*
085800010607     C                   SETOFF                                       3132
085900110315 xxx C***                SETON                                        35
086000030515     C*
086100010607     C                   MOVEL     datcor        VABAAS
086200010607     C                   MOVE      datcor        VABMGS
086300010607     C*
086400030715     C**********
086500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
086600030715     C**********
086700030715     C                   IF        wGiro = *zeros
086800040113     C*
086900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087200080429     C                             %trim(intcol(5))<>*blanks
087300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
087500080429     C                                      %trim(intcol(5))
087600070823     C     ';':CharCSV   XLATE     vindta        vindta
087700040510     C                   ENDIF
087800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087900031201     C     minu:maiu     XLATE     vindta        vindta
088000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088100030715     C                   DOW       posDa <= %len(%trim(vindta))
088200030715     C*
088300030715     C* Gestisco il 1° campo
088400030715     C                   IF        i = 1
088500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088600030715     C                   EVAL      posDa = 1
088700030715     C* Eseguo lo scan x trovare la fine del primo campo
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* A questo punto "estraggo" il campo corrente
089000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089100030715     C                                                    (posA-posDa))
089200030715     C* X i campi successivi al 1°
089300030715     C                   ELSE
089400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089500030715     C                   EVAL      posDa = posA + 1
089600030715     C* Eseguo lo scan x trovare la fine del campo corrente
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* Gestisco l'ultimo campo
089900030715     C                   IF        posA = *zeros
090000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C                   LEAVE
090500030715     C                   ELSE
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   ENDIF
091000030715     C                   ENDIF
091100030715     C* Incremento il contatore d campo
091200030715     C                   EVAL      i = i +1
091300030715     C                   ENDDO
091400030715     C*
091500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091600030715     C* delimitazione testo
091700030715     C                   EVAL      i = 1
091800030715     C                   DOW       i < %elem(SkSplitFLD)
091900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092100030715     C                   EVAL      i = i + 1
092200030715     C                   ENDDO
092300030715     C                   EVAL      wGiro = 1
092400030715     C                   ELSE
092500030715     C                   EVAL      wGiro = 2
092600031204     C**********
092700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092800031204     C* d separatore campo
092900031204     C**********
093000031204     C                   EVAL      vindta = %trim(vindta)
093100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093200031204     C                   EVAL      vindta = %subst(vindta:2)
093300031204     C                   ENDDO
093400031204     C*
093500031204     C                   Z-ADD     *zeros        lunghInput        4 0
093600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094000110912     C                   ENDDO
094100030715     C**********
094200030715     C* Effettuo lo split del campo dati d input
094300030715     C**********
094400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094500030715     C                   DOW       posDa <= %len(%trim(vindta))
094600030715     C*
094700030715     C* Gestisco il 1° campo
094800030715     C                   IF        i = 1
094900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095000030715     C                   EVAL      posDa = 1
095100030715     C* Eseguo lo scan x trovare la fine del primo campo
095200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C* X i campi successivi al 1°
095700030715     C                   ELSE
095800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095900030715     C                   EVAL      posDa = posA + 1
096000031204     C* Verifico che nn vi sia il campo nn valorizzato
096100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096200031204     C* Se campo nn valorizzato skippo al prossimo
096300031204     C                   EVAL      SkSplitCSV(i) = *blanks
096400031204     C                   EVAL      posA  = posA + 1
096500031204     C                   ELSE
096600030715     C* Eseguo lo scan x trovare la fine del campo corrente
096700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096800030715     C* Gestisco l'ultimo campo
096900030715     C                   IF        posA = *zeros
097000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097100030715     C* A questo punto "estraggo" il campo corrente
097200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097300030715     C                                                    (posA-posDa))
097400030715     C                   LEAVE
097500030715     C                   ELSE
097600030715     C* A questo punto "estraggo" il campo corrente
097700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097800030715     C                                                    (posA-posDa))
097900030715     C                   ENDIF
098000031204     C                   ENDIF
098100030715     C                   ENDIF
098200030715     C* Incremento il contatore d campo
098300030715     C                   EVAL      i = i +1
098400030715     C                   ENDDO
098500030715     C*
098600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098700030715     C* delimitazione testo
098800030715     C                   EVAL      i = 1
098900030715     C                   DOW       i < %elem(SkSplitCSV)
099000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099200030715     C                   EVAL      i = i + 1
099300030715     C                   ENDDO
099400100720     C*
099500100720     C* Stacco un progressivo x numero spedizione
099600151104 xxx C***                EXSR      repNSP
099700100326     C*
099800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099900030715     C                   EVAL      i = 1
100000030715     C                   DOW       i < %elem(SkSplitFLD)
100100030715     C*
100200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100400030715     C                             SkSplitCSV(i) <> *blanks
100500030721     C*** ATB
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100700030721     C                   EVAL      VABATB = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030715     C*** RSD
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101400030715     C* ==
101500130528     C                   ENDIF
101600030721     C*** RD2
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030715     C*** IND
102100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102200030715     C                   EVAL      VABIND = SkSplitCSV(i)
102300030715     C                   ENDIF
102400030715     C*** LOD
102500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
102600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102700030715     C                   ENDIF
102800030715     C*** PRD
102900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103100030715     C                   ENDIF
103200030721     C*** NZD
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030715     C*** RMA
103700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103900030715     C                   ENDIF
104000030715     C*** NOT
104100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104300030715     C                   ENDIF
104400030715     C*** NT2
104500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
104600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104700030715     C                   ENDIF
104800030715     C*** VCA
104900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105100030715     C                   ENDIF
105200030715     C*** VAS
105300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
105500030715     C                   ENDIF
105600030715     C*** FFD
105700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105900030715     C                   ENDIF
106000030721     C*** GC1
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** GC2
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
106600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TSP
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107100030721     C                   ENDIF
107200040510     C*** CBO
107300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
107500040510     C                   ENDIF
107600030721     C*** NAS
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TIC
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** GCA
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
108600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** XCO
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** CTM
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** TCR
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** CTS
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*** FTM
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
110600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110700030721     C                   ENDIF
110800030721     C*** VAD
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111100030721     C                   ENDIF
111200030721     C*** GMA
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
111500030721     C                   ENDIF
111600030721     C*** GGA
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111900030721     C                   ENDIF
112000030721     C*** GVA
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112300030721     C                   ENDIF
112400030721     C*** TC1
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
112600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112700030721     C                   ENDIF
112800030721     C*** TC2
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113100030721     C                   ENDIF
113200030721     C*** SCL
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
113500030721     C                   ENDIF
113600030721     C*** RMO
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113900030721     C                   ENDIF
114000030721     C*** NMO
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114300030721     C                   ENDIF
114400030721     C*
114500030721     C* Reperisco quindi i campi numerici...
114600040510     C*** NSP
114700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114800040510     C                   EVAL      PiStr=SkSplitCSV(i)
114900040510     C                   EXSR      CHKNUM
115000040510     C                   IF        PiInt=*on
115100040510     C                   Z-ADD     PiVal         VABNSP
115200040510     C                   ELSE
115300040510     C                   SETON                                        31
115400040510     C                   EVAL      VABNSP = *zeros
115500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040510     C                             + ' ' + 'VABNSP'
115700040510     C                   ENDIF
115800040510     C                   ENDIF
115900071106     C*** RMN
116000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116100071106     C                   EVAL      PiStr=SkSplitCSV(i)
116200071106     C                   EXSR      CHKNUM
116300071106     C                   IF        PiInt=*on
116400071106     C                   Z-ADD     PiVal         VABRMN
116500071106     C                   ELSE
116600071106     C                   SETON                                        32
116700071106     C                   EVAL      VABRMN = *zeros
116800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116900071106     C                             + ' ' + 'VABRMN'
117000071106     C                   ENDIF
117100071106     C                   ENDIF
117200030721     C*** CAD
117300130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117400151117     C                   EVAL      wCAD=SkSplitCSV(i)
117500130605     C                   ENDIF
117600030721     C*** NCL
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on
118100030721     C                   Z-ADD     PiVal         VABNCL
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABNCL = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040122     C                             + ' ' + 'VABNCL'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** PKB
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400130610     C                   Z-ADD(H)  PiVal         VABPKB
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABPKB = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABPKB'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** CAS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120500151103     C                             %trim(SkSplitCSV(i)) <> *zeros
120600130703     C                   EVAL      VABVCA = 'EUR'
120700140930     C                   EVAL      VABTIC = *blank
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
129600141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
129700141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
129800141003     C                             %trim(SkSplitCSV(i)) <> ',00'
129900141006     C                   EVAL      VABVAS = 'EUR'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiNum=*on
130300030721     C                   Z-ADD     PiVal         VABIAS
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABIAS'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000141003     C                   ENDIF
131100030721     C*** VLB
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiNum=*on
131600130610     C                   Z-ADD(H)  PiVal         VABVLB
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABVLB'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** QFT
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiNum=*on
132800030721     C                   Z-ADD     PiVal         VABQFT
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABQFT'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** NCD
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on      AND
134000030721     C                             PiVal<=9999999
134100030721     C                   Z-ADD     PiVal         VABNCD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABNCD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** NCA
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on      AND
135300030721     C                             PiVal<=9999999
135400030721     C                   Z-ADD     PiVal         VABNCA
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABNCA'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** ZNC
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on AND
136600030721     C                             PiVal<=99
136700030721     C                   Z-ADD     PiVal         VABZNC
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABZNC'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** DCR
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on       AND
137900030721     C                             PiVal<=99999999
138000030721     C                   Z-ADD     PiVal         VABDCR
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABDCR'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** HCR
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on   AND
139200030721     C                             PiVal<=9999
139300030721     C                   Z-ADD     PiVal         VABHCR
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABHCR'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000030721     C*** VMD
140100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140200030721     C                   EVAL      PiStr=SkSplitCSV(i)
140300030721     C                   EXSR      CHKNUM
140400030721     C                   IF        PiNum=*on
140500030721     C                   Z-ADD     PiVal         VABVMD
140600030721     C                   ELSE
140700030721     C                   SETON                                        32
140800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140900030721     C                             + ' ' + 'VABVMD'
141000030721     C                   ENDIF
141100030721     C                   ENDIF
141200030721     C*** ANT
141300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141400030721     C                   EVAL      PiStr=SkSplitCSV(i)
141500030721     C                   EXSR      CHKNUM
141600030721     C                   IF        PiInt=*on        AND
141700030721     C                             PiVal<=999999999
141800030721     C                   Z-ADD     PiVal         VABANT
141900030721     C                   ELSE
142000030721     C                   SETON                                        32
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABANT'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500030721     C*** CMO
142600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142700030721     C                   EVAL      PiStr=SkSplitCSV(i)
142800030721     C                   EXSR      CHKNUM
142900030721     C                   IF        PiInt=*on
143000030721     C                   Z-ADD     PiVal         Num5_0
143100030721     C                   MOVEL(p)  Num5_0        VABCMO
143200030721     C                   ELSE
143300030721     C                   SETON                                        32
143400030721     C                   EVAL      VABCMO = *zeros
143500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143600030721     C                             + ' ' + 'VABCMO'
143700030721     C                   ENDIF
143800030721     C                   ENDIF
143900040510     C*
144000040510     C* Reperisco i campi del FIVAT
144100040510     C*** VATNOT
144200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144400040510     C                   ENDIF
144500040510     C*** VATTRC
144600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144800040510     C                   ENDIF
144900060331     C*** VATNOT_A
145000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145200060331     C                   ENDIF
145300060331     C*** VATNOT_B
145400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145600060331     C                   ENDIF
145700060331     C*** VATNOT_E
145800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146000060331     C                   ENDIF
146100110927     C*** VATNOT_I
146200110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146300110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146400110927     C                   ENDIF
146500141003     C*** VATNOT_IJ
146600141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
146700141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
146800141003     C                   ENDIF
146900140319     C*** VATNOT_S
147000140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147100140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147200140319     C                   ENDIF
147300070823     C*
147400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147500150309     C*
147600151117     C***  RMA / NSP
147700151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
147800151117     C                   EVAL      VABRMA=%trim(SkSplitCSV(i))
147900151117     C                   EVAL      PiStr=SkSplitCSV(i)
148000150309     C                   EXSR      CHKNUM
148100150309     C                   IF        PiInt=*on
148200151104     C                   Z-ADD     PiVal         VABNSP
148300150309     C                   ELSE
148400150309     C                   SETON                                        31
148500151104     C                   EVAL      VABNSP = *zeros
148600150309     C                   EVAL      vinmsg = %trimr(vinmsg)
148700151117     C                             + ' ' + 'VABNSP'
148800150309     C                   ENDIF
148900150309     C                   ENDIF
149000151117     C*
149100151117     C***  CBO
149200151117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
149300151117     C                   IF        %trim(SkSplitCSV(i)) = 'DESTINATARIO'
149400151117     C                   EVAL      VABCBO = '2'
149500151117     C                   ENDIF
149600151117     C                   ENDIF
149700141003     C*
149800071029     C***  <===  -----------------------------------------
149900040510     C*
150000030715     C                   ENDIF
150100030715     C                   EVAL      i = i + 1
150200030715     C                   ENDDO
150300070925     C*
150400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
150500140319     C*
150600151117     C*** CAD
150700151117     C                   IF        wCAD <> *blank
150800151117     C                   EVAL      PiStr=wCAD
150900151117     C                   EXSR      CHKNUM
151000151117     C                   IF        PiInt=*on
151100151117     C                   Z-ADD     PiVal         Num5_0
151200151117     C                   MOVEL(p)  Num5_0        VABCAD
151300151117     C                   ELSE
151400151117     C                   SETON                                        32
151500151117     C                   EVAL      VABCAD = *zeros
151600151117     C                   EVAL      vinmsg = %trimr(vinmsg)
151700151117     C                             + ' ' + 'VABCAD'
151800151117     C                   ENDIF
151900151117     C                   ENDIF
152000141014     C* se la nazione è IT la sblanco
152100151104     C                   IF        VABNZD = 'I' or
152200151104     C                             VABNZD = 'IT' or
152300141112     C                             VABNZD = 'ITA'
152400141014     C                   EVAL      VABNZD = *blank
152500141014     C                   ENDIF
152600070925     C*
152700070925     C* Considerazioni finali su CBO/CAS
152800140220     C                   IF        wFlgCAS = '1'    OR
152900140220     C                             VABCAS  > *zeros
153000140220     C                   IF        VABCBO = '1'
153100140220     C                   EVAL      VABCBO = '4'
153200140220     C                   ENDIF
153300140220     C                   IF        VABCBO = '2'
153400140220     C                   EVAL      VABCBO = '6'
153500140220     C                   ENDIF
153600140220     C                   ENDIF
153700070925     C***  <===  ----------------------------
153800040510     C*
153900150922     C* la prima volta
154000150922     C                   if        depspe = 0
154100150922     C* oppure a rottura spedizione
154200151117     C***                          or depspe <> vabRMN
154300151117     C                             or depspe <> vabNSP
154400150922     C* Stacco un progressivo x numero spedizione
154500151104 xxx C***                EXSR      repNSP
154600150922     C*
154700150922     C* durante la stessa spedizione
154800150922     C                   ELSE
154900150922     C* reperisco il precedente numero spedizione
155000150922     C                   EVAL      VABNSP = sav_VABNSP
155100150922     C                   ENDIF
155200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
155300150922     C                   IF        VABNSP = *zeros
155400150922     C                   SETON                                        31
155500150922     C                   ENDIF
155600040715     C*
155700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155800040715     C                   IF        VABLNP = *zeros
155900040715     C                   SETON                                        31
156000040715     C                   ENDIF
156100040510     C*
156200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
156300120411     C                   IF        VABRMN = *zeros
156400120411     C                   Z-ADD     VABNSP        VABRMN
156500120411     C                   ENDIF
156600120328     C* IN QUESTO CASO LO FORZO = 1
156700120411     C***                IF        VABRMN = *zeros
156800120411     C***                Z-ADD     1             VABRMN
156900120411     C***                ENDIF
157000040510     C*
157100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
157200040510     C                   IF        VABRMA = *blanks
157300040714     C                   MOVEL     VABRMN        VABRMA
157400040510     C                   ENDIF
157500040128     C*
157600040510     C* Se provincia nn valorizzata la reperisco
157700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157800110912     C                   IF        VABPRD =  *blanks AND
157900110912     C                             VABNZD =  *blanks AND
158000110912     C                             VABCAD <> *blanks AND
158100110912     C                             VABLOD <> *blanks
158200040128     C                   CLEAR                   TISI95DS
158300040128     C                   EVAL      I95TCN = '3'
158400040128     C                   Z-ADD     datcor        I95DAT
158500040128     C                   EVAL      I95NAR = VABNZD
158600040128     C                   EVAL      I95CAP = VABCAD
158700040128     C                   EVAL      I95LOC = VABLOD
158800040128     C                   CALL      'TISI95R'
158900040128     C                   PARM                    TISI95DS
159000040128     C                   EVAL      VABPRD = O95PRV
159100040510     C                   ENDIF
159200020204     C*
159300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
159400020204     C                   EXSR      CHKIMPDIV
159500030715     C                   ENDIF
159600020204     C*
159700010607     C                   ENDSR
159800010607     C*----------------------------------------------------*
159900010601
160000020204
160100020204     C*----------------------------------------------------*
160200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
160300020204     C*----------------------------------------------------*
160400020204     C     CHKIMPDIV     BEGSR
160500020204     C*
160600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160700020204     C                   Z-ADD     *zeros        wrkDec            9 9
160800020204     C*
160900020204     C* Come prima cosa effettuo considerazioni sulla divisa
161000020204     C                   IF        vabIAS > *zeros
161100020204     C                   IF        vabVAS <> 'EUR'
161200020204     C                   EVAL      vabVAS =  'ITL'
161300020204     C                   ENDIF
161400020204     C                   ENDIF
161500020204     C*
161600020204     C                   IF        vabCAS > *zeros
161700020204     C                   IF        vabVCA <> 'EUR'
161800020204     C                   EVAL      vabVCA =  'ITL'
161900020204     C                   ENDIF
162000020204     C                   ENDIF
162100020204     C*
162200020204     C                   IF        vabVMD > *zeros
162300020204     C                   IF        vabVAD <> 'EUR'
162400020204     C                   EVAL      vabVAD =  'ITL'
162500020204     C                   ENDIF
162600020204     C                   ENDIF
162700020204     C*
162800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162900020204     C                   Z-ADD     vabIAS        wrkDec
163000020204     C                   IF        wrkDec > *zeros
163100020204     C                   IF        vabVAS = 'ITL'
163200020204     C                   EVAL      vabIAS = *zeros
163300020204     C                   ENDIF
163400020204     C                   ENDIF
163500020204     C*
163600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163700020204     C                   Z-ADD     vabCAS        wrkDec
163800020204     C                   IF        wrkDec > *zeros
163900020204     C                   IF        vabVCA = 'ITL'
164000020204     C                   EVAL      vabCAS = *zeros
164100020204     C                   ENDIF
164200020204     C                   ENDIF
164300020204     C*
164400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164500020204     C                   Z-ADD     vabVMD        wrkDec
164600020204     C                   IF        wrkDec > *zeros
164700020204     C                   IF        vabVAD = 'ITL'
164800020204     C                   EVAL      vabVMD = *zeros
164900020204     C                   ENDIF
165000020204     C                   ENDIF
165100020204     C*
165200020204     C                   ENDSR
165300020204     C***
165400010330
165500100318
165600100318
165700100318     C*----------------------------------------------------*
165800100318     C*  CONTROLLO NUMERICITA' CAMPI
165900100318     C*----------------------------------------------------*
166000100318     C     CHKNUM        BEGSR
166100100318     C*
166200100318     C                   IF        PiDecChr = *blanks
166300100318     C                   EVAL      PiDecChr = CharNUM
166400100318     C                   ENDIF
166500100318     C*
166600100318     C                   callp     UBISNUM_Check(PiStr
166700100318     C                                          :PiDecChr
166800100318     C                                          :PiVal
166900100318     C                                          :PiNum
167000100318     C                                          :PiInt)
167100100318     C*
167200100318     C                   ENDSR
167300100318     C***
167400010601
167500010601
167600010601
167700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167800141006      ***
167900141014     C     repfil        BEGSR
168000141014     C*
168100141014     C                   if        invfil = *zeros and
168200141014     C                             depfil > *zeros and
168300141014     C                             (vinflg = *blanks or
168400141014     C                              vinflg = *zeros)
168500141014     C*
168600141014     C                   eval      invfil = depfil
168700141014     C                   endif
168800141014     C*
168900141014     C                   if        depfil <> invfil and
169000141014     C                             invfil > *zeros
169100141014     C                   eval      flgMulti = '1'
169200141014     C                   if        vinflg = *blanks
169300141014     C                   add       1             cntNonEl
169400141014     C                   endif
169500141014     C                   endif
169600141014     C*
169700141014     C                   if        vinflg = '2'
169800141014     C                   eval      flgStato = '2'
169900141014     C                   endif
170000141014     C*
170100141014     C                   ENDSR
170200141014     C*
170300010601
170400010601
170500010601
170600010330
170700040119      /TITLE Invio dei dati al punto operativo.
170800040119     C     invio         BEGSR
170900040119     C*
171000040119     C* 1° invio FIVAT
171100040119     C                   reset                   dscmz
171200040119     C                   move      vlrpoi        cmzdst
171300040119     C                   eval      cmzfld = 'FIVATWWR'
171400040119     C                   eval      cmzmbd = vlrhdl
171500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171600040119     C***                if        prmfir = *blanks
171700040119     C                   eval      cmzfla = 'FIVAT00F'
171800040119     C                   eval      cmzmba = 'FIVAT00F'
171900040119     C***                else
172000040119     C***                eval      cmzfla = prmfir
172100040119     C***                eval      cmzmba = prmfir
172200040119     C***                endif
172300040119     C                   eval      cmznrr = *zeros
172400040119     C                   move      §ctrokvt      cmznrr
172500040119     C                   eval      cmzlba = vlrfl1
172600040119     C                   call(e)   'TIS711C'
172700040119     C                   parm                    dscmz
172800040119     C                   parm      *blanks       esito
172900040119     C                   if        %error
173000040119     C                             or cmzerr = '1'
173100040119     C                             or esito  = '1'
173200040119     C                   eval      wrkesito = '3'
173300040119     C                   else
173400040119     C*
173500040119     C* 2° invio FIVAB
173600040119     C                   reset                   dscmz
173700040119     C                   move      vlrpoi        cmzdst
173800040119     C                   eval      cmzfld = vlrfou
173900040119     C                   eval      cmzmbd = vlrhdl
174000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
174100040119     C***                if        prmfir = *blanks
174200040119     C                   eval      cmzfla = 'FIVAB00F'
174300040119     C                   eval      cmzmba = 'FIVAB00F'
174400040119     C***                else
174500040119     C***                eval      cmzfla = prmfir
174600040119     C***                eval      cmzmba = prmfir
174700040119     C***                endif
174800040119     C                   eval      cmznrr = *zeros
174900040119     C                   move      §ctrokvb      cmznrr
175000040119     C                   eval      cmzlba = vlrfl1
175100040119     C                   call(e)   'TIS711C'
175200040119     C                   parm                    dscmz
175300040119     C                   parm      *blanks       esito
175400040119     C                   if        %error
175500040119     C                             or cmzerr = '1'
175600040119     C                             or esito  = '1'
175700040119     C                   eval      wrkesito = '3'
175800040119     C                   endif
175900040119     C                   endif
176000040119     C*
176100040119     C                   ENDSR
176200040119     C***
176300010601
176400010601
176500010601
176600010601
176700010601      /TITLE Invio dei dati al punto operativo.
176800010601     C     opeini        BEGSR
176900010601     C*
177000010601     C* Inizializzo flag e contatori operativi
177100010601     C                   movel     '0'           flgGiro           1
177200010601     C                   movel     '0'           flgMulti          1
177300010601     C                   movel     '1'           flgStato          1
177400010615     C                   movel     '0'           flgOk             1
177500010601     C                   z-add     *zeros        cntNonEl         10 0
177600010601     C                   z-add     *zeros        depfil            3 0
177700010601     C                   z-add     *zeros        invfil            3 0
177800010601     C*
177900010601     C                   ENDSR
178000010601     C***
178100070326
178200070326
178300070326
178400070326
178500070326     C     *pssr         BEGSR
178600070329     C*
178700070329     C                   if        %open(tivin00r)
178800070329     C                   close     tivin00r
178900070329     C                   endif
179000070329     C                   if        %open(fivabwwr)
179100070329     C                   close     fivabwwr
179200070329     C                   endif
179300070329     C                   if        %open(fivatwwr)
179400070329     C                   close     fivatwwr
179500070329     C                   endif
179600070326     C*
179700070326     C* Effettuo la chiamata al CLLE preposto
179800070326     C                   call(e)   'TITVVTC'
179900070326     C                   parm                    parccm
180000070326     C                   parm                    parmbr
180100070326     C                   parm      '2'           paropz
180200070326     C*
180300070326     C                   eval      wrkesito = '2'
180400070404     C*
180500070404     C                   seton                                        LR
180600070326     C*
180700070326     C                   ENDSR     '*CANCL'
180800070326     C***
180900100318
181000100318
181100100318
181200100318     C     normCSV       BEGSR
181300100318     C*
181400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
181500100318     C* separatori campi
181600100318     C                   if        CharTXT <> *blanks
181700100318     C                   z-add     *zeros        wLenVINDTA        4 0
181800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181900100318     C                   setoff                                       80
182000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
182100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
182200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
182300100318     C                   if        not *in80
182400100318     C                   seton                                        80
182500100318     C                   else
182600100318     C                   setoff                                       80
182700100318     C                   endif
182800100318     C                   endif
182900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
183000100318     C                             and *in80 = *on
183100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
183200100318     C                   endif
183300100318     C                   enddo
183400100318     C                   endif
183500100318     C*
183600100318     C                   ENDSR
183700100318     C***
183800070326
183900100326
184000100326
184100100326     C*----------------------------------------------------*
184200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
184300100326     C*----------------------------------------------------*
184400100326     C     REPNSP        BEGSR
184500100326     C*
184600100326     C* NSP => Stacco un numeratore da AZNUM
184700100326     C                   clear                   TRUL33DS
184800100326     C                   eval      I33OPE = *zeros
184900100326     C                   eval      I33CNU = 302
185000100326     C                   eval      I33NUM = 1
185100100326     C                   movel     TRUL33DS      KPJBU
185200100326     C                   call      'TRUL33R'
185300100326     C                   parm                    KPJBA
185400100326     C                   movel     KPJBU         TRUL33DS
185500100326     C                   if        O33ERR = *zeros
185600100326     C                   z-add     O33NRF        VABNSP
185700100326     C                   z-add     O33NRF        VATNSP
185800100326     C                   else
185900100326     C                   SETON                                        31
186000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
186100140711     C                             + ' ' + 'VABNSP VATNSP'
186200100326     C                   endif
186300100326     C*
186400100326     C                   ENDSR
186500010330
186600100326
186700010330
186800000613     C     *inzsr        BEGSR
186900990910     C*
187000990910     C     *entry        plist
187100990920     C                   parm                    tivlrds
187200990921     C                   parm      wrkesito      esito
187300000724     C                   parm                    prmlit
187400000710     C                   parm                    prmfir
187500010330     C*
187600010330     C* CALCOLA LA DATA CORRENTE
187700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187800100318     C                   eval      datcor = %dec(%date() : *ISO)
187900000613     C*
188000000613     C                   ENDSR
188100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
188200160219VABRMN;VABNCL;W_VABNSP;X;VABPKB;X;X;X;X;X;X;X;X;X;X;X;
188300151117VABCTR;X;VATNOT_E;VABTSP;X;VABCCM;X;VATNOT_B;VATNOT_I;VATNOT_A;X;X;X;X;X;X;W_VABCBO;X;X;X;
188400160219VABFFD;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;
188500111116
188600100318
