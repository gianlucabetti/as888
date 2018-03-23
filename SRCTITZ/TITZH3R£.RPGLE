000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400151104      * - file strutturato un rcd x collo ma con i dati quantità/valore x spedizione
000500151104      * - tutti i dati del mittente possono avere una destinazione diversa i cui valori sono su
000600151104      *   altre colonne e lo scopro solo testando una terza colonna
000700151117      * - il nr.colli è uno per riga della spedizione
000800150114      *
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001000130917     H decedit('0,')
001100120209     F*
001200990910     Ftivin00r  uF   E             DISK    usropn
001300100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001400040119     FFIVATwwr  O    E             DISK    usropn
001500000313     D*
001600040113     D*------------
001700040113     D* SCHIEDE A PROGRAMMA
001800040113     D*------------
001900080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002000141003     D*------------
002100010330     D*----------------------------------------------------
002200010330     D* DICHIARAZIOINE VARIABILI DI WRK
002300010330     D*----------------------------------------------------
002400010330     D dscmz         e ds                  inz
002500010330     D psds           sds
002600010330     D  procname         *PROC
002700010330     D tivlrds       e ds                  extname(tivlr00f)
002800040128     D tisi95ds      e ds
002900100413     D fivabds       e ds                  extname(fivab00f)
003000100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003100100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003200010330     D esito           s              1
003300010330     D prmlit          s             10
003400010330     D prmfir          s             10
003500010330     D wrkesito        s                   like(esito)
003600010330     D rrnum           s              6  0 INZ(*zeros)
003700040510     D parccm          s              8    INZ(*blanks)
003800040119     D parmbr          s             10    INZ(*blanks)
003900040119     D paropz          s              1    INZ(*blanks)
004000040119     D chkcall         s              1    INZ(*blanks)
004100151117     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004200151117     D*depspe          s                   LIKE(VABRMN) INZ(*zeros)
004300080418     D wFlgCAS         s              1    INZ(*blanks)
004400151117     D wCAD            s                   like(VABCAD) inz(*blank)
004500120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004600120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004700151104     D Intestazioni    s               n   inz(*off)
004800100326     D*------------------
004900100326     D* DS REPERIMENTO NUMERATORE
005000100326     D*------------------
005100100326     D trul33ds      e ds                  inz
005200100326     D*------------------
005300100326     D* DS ARCHITETTURA
005400100326     D*------------------
005500100326     D kpjba         e ds                  inz
005600130611     D*------------------
005700130611     D TRUL28DSX     e ds                  inz
005800100326
005900030715     D*------------------
006000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006100030715     D*------------------
006200030715     D SkSplitFLD      S             10    DIM(100)
006300030715     D SkSplitCSV      S            256    DIM(100)
006400030715     D CharCSV         S              1
006500030715     D CharTXT         S              1
006600030715     D CharNUM         S              1
006700100318     D CharSOS         S              1
006800130626     D posDa           S              4  0 INZ(*zeros)
006900130626     D posA            S              4  0 INZ(*zeros)
007000030715     D i               s              3  0 INZ(1)
007100030715     D wGiro           s              1  0 INZ(*zeros)
007200070823     D wVATNOT_A       s                   LIKE(VATNOT)
007300060331     D wVATNOT_B       s                   LIKE(VATNOT)
007400060331     D wVATNOT_E       s                   LIKE(VATNOT)
007500110927     D wVATNOT_I       s                   LIKE(VATNOT)
007600140220     D wVATNOT_J       s                   LIKE(VATNOT)
007700140220     D wVATNOT_IJ      s             70
007800140319     D wVATNOT_S       s                   LIKE(VATNOT)
007900140919     D w70             s             70
008000141006     D w11             s             11
008100140919     D wNOT            s             70
008200140919     D wRSD1           S            256
008300140919     D wRSD2           S            256
008400140919     D wRSD3           S            256
008500120411     D Data8A          s              8
008600140527     D wBarcodeN       s             11s 0
008700140527     D wX              s              3s 0
008800140711     D wRMA            s                   like(VABRMA)
008900151104     D wDestinazioneDiversa...
009000151104     D                 S              1
009100151104     D TxtInOut        S           2048
009200140711     D ElencoChar      S            256
009300140711     D TipoElenco      S              1
009400140711     D CharSost        S              1
009500140711     D UpperCase       S              1
009600140711     D ChkNull         S              1
009700140711     D CharNull        S              1
009800140711     D EsitoBon        S              1
009900030822     D*------------------
010000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
010100030822     D*------------------
010200030822     D posDaDft        S              3  0 INZ(*zeros)
010300030822     D posADft         S              3  0 INZ(*zeros)
010400030822     D j               s              3  0 INZ(1)
010500030822     D wGiroDft        s              1  0 INZ(*zeros)
010600080418     D*------------------
010700080418     D* Costanti
010800080418     D*------------------
010900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
011000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
011100100318
011200100318     D*------------------
011300100318     D* LINKING A DEFINIZIONI ESTERNE
011400100318     D*------------------
011500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
011600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
011700100318
011800010330
011900010330
012000010330
012100000913     C                   reset                   rrnum
012200990921     C                   reset                   esito
012300990921     C                   reset                   wrkesito
012400010601     C*
012500010601     C                   exsr      opeini
012600010605     C                   exsr      rwvab
012700010601     C*
012800010601     C                   seton                                        lr
012900010601
013000010601
013100010601
013200010601     C*--------------------------------------------------------
013300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
013400010601     C*--------------------------------------------------------
013500010601     C     PREELA        BEGSR
013600010601     C*
013700010601     C* SE OCCORRE SPEDIRE IN FILIALE
013800010601     C                   if        invfil <> *zeros and
013900010601     C                             flgGiro = '0'
014000010601     C*
014100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
014200010601     C                   eval      flgGiro = '1'
014300010601     C*
014400010601     C                   endif
014500010601     C*
014600010601     C                   ENDSR
014700010601     C***
014800010601
014900010601
015000010601
015100010601
015200010601     C*--------------------------------------------------------
015300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015400010601     C*--------------------------------------------------------
015500010601     C     ENDELA        BEGSR
015600040119     C*
015700040119     C* Effettuo la chiamata al CLLE preposto
015800040119     C                   call(e)   'TITVVTC'
015900040510     C                   parm                    parccm
016000040119     C                   parm                    parmbr
016100040119     C                   parm      '2'           paropz
016200050201     C*
016300050201     C* Effettuo lancio TISI95 solo x chiusura
016400050201     C                   CLEAR                   TISI95DS
016500050201     C                   EVAL      I95TLA = 'C'
016600050201     C                   CALL      'TISI95R'
016700050201     C                   PARM                    TISI95DS
016800000616     C*
016900010601     C                   ENDSR
017000010601
017100010601
017200010330     C*--------------------------------------------------------
017300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
017400010330     C*--------------------------------------------------------
017500010605     C     RWVAB         BEGSR
017600010330     C*
017700010330     C                   if        not %open(tivin00r)
017800010330     C                   open      tivin00r
017900010330     C                   endif
018000021025     C                   if        not %open(fivabwwr)
018100021025     C                   open      fivabwwr
018200010330     C                   endif
018300040119     C*
018400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018500040119     C                   exsr      prevat
018600040119     C*
018700040119     C                   if        chkcall = '0'
018800040119     C*
018900040119     C                   if        not %open(fivatwwr)
019000040119     C                   open      fivatwwr
019100040119     C                   endif
019200010330     C*
019300040119     C                   clear                   §CTROKVB          7 0
019400040119     C                   clear                   §CTROKVT          7 0
019500040119     C                   clear                   §CTRMO            7 0
019600040119     C                   clear                   §CTRNO            7 0
019700120410     C*
019800120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
019900120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
020000120410     C                   IF        Intestazioni = *off
020100120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
020200120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
020300120410     C                   exsr      inzvar
020400120410     C                   exsr      impvab
020500120410     C                   ENDIF
020600010330     C*
020700030822     C                   do        *HIVAL
020800010330     C*
020900030822     C                   read      tivin00r                               70
021000150922     C*
021100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021200010618     C                   if        vinflg = '1'
021300010618     C                   eval      flgOk = '1'
021400010618     C                   endif
021500040510     C*
021600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
021700040510     C                   z-add     *zeros        wLenVINDTA        4 0
021800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
021900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
022000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
022100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
022200040510     C                   z-add     1             wFlgVINDTA
022300040510     C                   leave
022400040510     C                   endif
022500040510     C                   enddo
022600010618     C*
022700040510     C                   if        vindta > *blanks AND
022800040510     C                             wFlgVINDTA = 1
022900010330     C                   add       1             rrnum
023000010330     C*
023100010601     C                   if        *in70 = *off and
023200010330     C                             (vinflg = *blanks
023300010330     C                              or vinflg = '0'
023400010330     C                              or vinflg = '2')
023500010330     C*
023600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023700010711     C                   if        vinflg = *blanks or vinflg = '0'
023800010711     C                   clear                   vinmsg
023900010711     C                   endif
024000130917     C*
024100130917     C* Aggiungo un byte a blanks all'inizio d ogni record
024200130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
024300130917     C* evitare che vengano elisi i blank
024400140220     C***                eval      vindta  = '*' + %trim(vindta)
024500130917     C*
024600080527     C                   exsr      inzvar
024700100318     C                   exsr      normCSV
024800010605     C                   exsr      impvab
024900010601     C*
025000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025100141014     C                   eval      depfil = VABLNP
025200141014     C                   exsr      repfil
025300141014     C                   if        depfil = invfil
025400141014     C                   if        vlrpoi = 999
025500141014     C                   move(P)   invfil        VABFGS
025600141014     C                   else
025700141014     C                   move(P)   vlrpoi        VABFGS
025800141014     C                   endif
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
028000141014     C                   endif
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
036400151117     C* i colli sono sempre uno per riga, questa è la prima riga per cui valgono 1
036500151117     C                   eval      VABNCL = 1
036600080527     C  N31              eval      fivabds_sav = fivabds
036700151117     C***                eval      depspe = vabRMN
036800151117     C                   eval      depspe = vabNSP
036900080527     C                   else
037000151117     C***                if        depspe = vabRMN AND *in65 = *off
037100151117     C                   if        depspe = vabNSP AND *in65 = *off
037200151117     C* PER EVITARE CHE FACCIA UNO DI QUESTE SOMMATORIE, IMPOSTO IL PRIMO CARATTERE DI ppt = S
037300100413     C   51              add       VABNCL        sav_VABNCL
037400100413     C   51              add       VABPKB        sav_VABPKB
037500100413     C   51              add       VABVLB        sav_VABVLB
037600100720     C   52              z-add     VABNCL        sav_VABNCL
037700100720     C   52              z-add     VABPKB        sav_VABPKB
037800100720     C   52              z-add     VABVLB        sav_VABVLB
037900100720     C   53              add       VABNCL        sav_VABNCL
038000100720     C   53              z-add     VABPKB        sav_VABPKB
038100100720     C   53              z-add     VABVLB        sav_VABVLB
038200151117     C* il nr.colli è uno per riga della spedizione
038300151117     C***                eval      sav_VABNCL += 1
038400151117     C                   eval      sav_VABNCL = sav_VABNCL + 1
038500150922     C                   else
038600080527     C  N31              eval      fivabds_ok = fivabds_sav
038700080527     C  N31              exsr      wrivab
038800080527     C  N31              eval      fivabds_sav = fivabds
038900151117     C***                eval      depspe = vabRMN
039000151117     C                   eval      depspe = vabNSP
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
040400151117     C***                if        VABRMN <> depspe or *in65 = *on
040500151117     C                   if        VABNSP <> depspe or *in65 = *on
040600070301     C                   if        wVATNOT_E <> *blanks
040700130610     C                   eval      ok_VABCTM = '7Q'
040800070301     C                   endif
040900100413     C                   write     fivab000
041000080513     C                   endif
041100040119     C*
041200040119     C                   ENDSR
041300040119
041400040119
041500040119
041600040119     C*----------------------------------------------------*
041700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041800040119     C*----------------------------------------------------*
041900040510     C     WRIVAT        BEGSR
042000060512     C*
042100060512     C* Inizializzo i campi chiave
042200060512     C                   eval      VATFGS = VABFGS
042300060512     C                   eval      VATAAS = VABAAS
042400060512     C                   eval      VATCCM = VABCCM
042500070823     C                   eval      VATNRS = VABNRS
042600060512     C                   eval      VATNSP = VABNSP
042700060512     C                   eval      VATLNP = VABLNP
042800040119     C*
042900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
043000040510     C                   if        VATTRC = *blanks
043100040510     C                   eval      VATTRC = 'E'
043200040510     C                   endif
043300110318     C                   if        VATNOT <> *blanks
043400040510     C                   write     FIVAT000
043500040119     C                   add       1             §CTROKVT
043600110318     C                   endif
043700060331     C*
043800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
043900060331     C                   if        wVATNOT_A <> *blanks
044000060331     C                   eval      VATNOT = wVATNOT_A
044100060331     C                   eval      VATTRC = 'A'
044200060331     C                   write     FIVAT000
044300060331     C                   add       1             §CTROKVT
044400060331     C                   endif
044500060331     C*
044600060331     C                   if        wVATNOT_B <> *blanks
044700060331     C                   eval      VATNOT = wVATNOT_B
044800060331     C                   eval      VATTRC = 'B'
044900060331     C                   write     FIVAT000
045000060331     C                   add       1             §CTROKVT
045100060331     C                   endif
045200140711     C*
045300140711     C                   if        wVATNOT_IJ <> *blanks
045400140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045500140711     C                   eval      VATTRC = 'I'
045600140711     C                   write     FIVAT000
045700140711     C                   add       1             §CTROKVT
045800140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
045900140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046000140711     C                   eval      VATTRC = 'J'
046100140711     C                   write     FIVAT000
046200140711     C                   endif
046300140711     C                   endif
046400060331     C*
046500060331     C                   if        wVATNOT_E <> *blanks
046600060331     C                   eval      VATTRC = 'E'
046700140527     C*
046800150309     C* NON Sviluppo i "CHI SONO"
046900140527     C*
047000150309     C                   eval      VATNOT = wVATNOT_E
047100150114     C                   write     FIVAT000
047200150114     C                   add       1             §CTROKVT
047300120320     C                   endif
047400110927     C*
047500110927     C                   if        wVATNOT_I <> *blanks
047600110927     C                   eval      VATNOT = wVATNOT_I
047700140711     C                   eval      VATTRC = 'I'
047800110927     C                   write     FIVAT000
047900110927     C                   add       1             §CTROKVT
048000110927     C                   endif
048100140220     C*
048200140220     C                   if        wVATNOT_J <> *blanks
048300140220     C                   eval      VATNOT = wVATNOT_J
048400140220     C                   eval      VATTRC = 'J'
048500140220     C                   write     FIVAT000
048600140220     C                   add       1             §CTROKVT
048700140220     C                   endif
048800140319     C*
048900140319     C                   if        wVATNOT_S <> *blanks
049000140319     C                   eval      VATNOT = wVATNOT_S
049100140319     C* i 2 flag devono essere blank
049200140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049300140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049400140319     C                   eval      VATTRC = 'S'
049500140319     C                   write     FIVAT000
049600140319     C                   add       1             §CTROKVT
049700140319     C                   endif
049800040119     C*
049900040119     C                   ENDSR
050000040119
050100040119
050200040119
050300040119     C*----------------------------------------------------*
050400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050500040119     C*----------------------------------------------------*
050600040119     C     PREVAT        BEGSR
050700040119     C*
050800040119     C* Compongo il nome del membro da dare al FIVATWWR
050900040119     C                   eval      parmbr = vlrhdl
051000040119     C                   movel     'M'           parmbr
051100040510     C                   eval      parccm = vlrksc
051200040119     C                   eval      paropz = '1'
051300040119     C* Effettuo la chiamata al CLLE preposto
051400040119     C                   call(e)   'TITVVTC'
051500040119     C                   parm                    parccm
051600040119     C                   parm                    parmbr
051700040119     C                   parm                    paropz
051800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051900040119     C                   if        %error
052000040119     C                   movel     '1'           chkcall
052100040119     C                   else
052200040119     C                   movel     '0'           chkcall
052300040119     C                   endif
052400040119     C*
052500040119     C                   ENDSR
052600010601
052700010601
052800010601
052900010330     C*----------------------------------------------------*
053000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053100010330     C*----------------------------------------------------*
053200040119     C     INZVAR        BEGSR
053300080527     C*
053400080527     C                   setoff                                       65
053500010330     C*
053600030822     C* Inizializzo variabili di wrk
053700020204     C                   Z-ADD     *zeros        Num5_0            5 0
053800101018     C                   MOVEL     *blanks       wFlgCAS
053900060331     C                   MOVEL     *blanks       wVATNOT_A
054000060331     C                   MOVEL     *blanks       wVATNOT_B
054100060331     C                   MOVEL     *blanks       wVATNOT_E
054200110927     C                   MOVEL     *blanks       wVATNOT_I
054300140220     C                   MOVEL     *blanks       wVATNOT_J
054400140220     C                   MOVEL     *blanks       wVATNOT_IJ
054500140319     C                   MOVEL     *blanks       wVATNOT_S
054600120411     C                   MOVEL     *blanks       wNOT
054700120411     C                   MOVEL     *blanks       Data8A
054800151117     C                   EVAL      wDestinazioneDiversa = *blank
054900030822     C*
055000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
055100030822     C                   CLEAR                   SkSplitCSV
055200030822     C                   EVAL      i = 1
055300030822     C                   EVAL      posDa = *zeros
055400030822     C                   EVAL      posA  = *zeros
055500080527     C*
055600080527     C                   CLEAR                   FIVABDS
055700080527     C                   CLEAR                   FIVAT000
055800030822     C*
055900030822     C* Reimposto i valori di default
056000030822     C                   EXSR      DEFCAM
056100030822     C*
056200010330     C                   ENDSR
056300010330     C*----------------------------------------------------*
056400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056500010330     C*----------------------------------------------------*
056600010330     C     DEFCAM        BEGSR
056700030822     C*
056800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056900030715     C* e delimitatore testo.
057000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
057100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057300100318     C*
057400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057500100318     C                   EVAL      CharSOS = CharNUM
057600030822     C*
057700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057800100720     C                   SETOFF                                       505152
057900100720     C                   SETOFF                                       53
058000100413     C                   SELECT
058100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058200100720     C                   SETON                                        50
058300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058400100413     C                   SETON                                        51
058500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058600100720     C                   SETON                                        52
058700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058800100720     C                   SETON                                        53
058900100413     C                   ENDSL
059000030822     C*
059100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059200030822     C                   EVAL      posDaDft = 1
059300030822     C                   EVAL      posADft  = 0
059400030822     C                   EVAL      wGiroDft = 0
059500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059600030822     C                             posDaDft > 0
059700030822     C*
059800030822     C* Gestisco il 1° giro
059900030822     C                   IF        wGiroDft = 0
060000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060200030822     C* Incremento il contatore dei "giri"
060300030822     C                   EVAL      wGiroDft = 1
060400030822     C                   ELSE
060500030822     C                   EVAL      posDaDft = posADft
060600030822     C                   ENDIF
060700030822     C* Eseguo lo scan x trovare la fine del campo corrente
060800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
060900030822     C*
061000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061200030822     C                   IF        posDaDft > 0 AND
061300030822     C                             posADft  > 0
061400030822     C* NCL
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'NCL'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiInt=*on
062400030822     C                   Z-ADD     PiVal         VABNCL
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* CCM
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'CCM'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiInt=*on
063700030822     C                   Z-ADD     PiVal         VABCCM
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* LNP
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'LNP'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiInt=*on
065000030822     C                   Z-ADD     PiVal         VABLNP
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* NRS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'NRS'
065800030822     C                   EVAL      PiStr=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   EXSR      CHKNUM
066200030822     C                   IF        PiInt=*on
066300030822     C                   Z-ADD     PiVal         VABNRS
066400030822     C                   ENDIF
066500030822     C                   ENDIF
066600030822     C* CTR
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTR'
067100030822     C                   EVAL      PiStr=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   EXSR      CHKNUM
067500030822     C                   IF        PiInt=*on
067600030822     C                   Z-ADD     PiVal         VABCTR
067700030822     C                   ENDIF
067800030822     C                   ENDIF
067900030822     C* PKB
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'PKB'
068400030822     C                   EVAL      PiStr=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   EXSR      CHKNUM
068800030822     C                   IF        PiNum=*on
068900130610     C                   Z-ADD     PiVal         VABPKB
069000030822     C                   ENDIF
069100030822     C                   ENDIF
069200030822     C* VLB
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VLB'
069700030822     C                   EVAL      PiStr=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   EXSR      CHKNUM
070100030822     C                   IF        PiNum=*on
070200130610     C                   Z-ADD     PiVal         VABVLB
070300030822     C                   ENDIF
070400030822     C                   ENDIF
070500030822     C* QFT
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'QFT'
071000030822     C                   EVAL      PiStr=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   EXSR      CHKNUM
071400030822     C                   IF        PiNum=*on
071500030822     C                   Z-ADD     PiVal         VABQFT
071600030822     C                   ENDIF
071700030822     C                   ENDIF
071800030822     C* CBO
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CBO'
072300030822     C                   EVAL      VABCBO=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* TSP
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'TSP'
073200030822     C                   EVAL      VABTSP=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAS
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAS'
074100030822     C                   EVAL      VABVAS=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VCA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VCA'
075000030822     C                   EVAL      VABVCA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400070925     C* TIC
075500070925     C                   IF        %subst(
075600070925     C                             %subst(vlrppt:posDaDft+1:
075700070925     C                             posADft-posDaDft-1):1:3)
075800070925     C                             = 'TIC'
075900070925     C                   EVAL      VABTIC=%trim(%subst(
076000070925     C                             %subst(vlrppt:posDaDft+1:
076100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200070925     C                   ENDIF
076300030822     C* GCA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GCA'
076800030822     C                   EVAL      VABGCA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* CTM
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'CTM'
077700030822     C                   EVAL      VABCTM=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* FFD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'FFD'
078600030822     C                   EVAL      VABFFD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* VAD
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'VAD'
079500030822     C                   EVAL      VABVAD=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GMA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GMA'
080400030822     C                   EVAL      VABGMA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GGA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GGA'
081300030822     C                   EVAL      VABGGA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* GVA
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'GVA'
082200030822     C                   EVAL      VABGVA=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC1
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC1'
083100030822     C                   EVAL      VABTC1=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500030822     C* TC2
083600030822     C                   IF        %subst(
083700030822     C                             %subst(vlrppt:posDaDft+1:
083800030822     C                             posADft-posDaDft-1):1:3)
083900030822     C                             = 'TC2'
084000030822     C                   EVAL      VABTC2=%trim(%subst(
084100030822     C                             %subst(vlrppt:posDaDft+1:
084200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300030822     C                   ENDIF
084400040714     C* VATTRC
084500040714     C                   IF        %subst(
084600040714     C                             %subst(vlrppt:posDaDft+1:
084700040714     C                             posADft-posDaDft-1):1:3)
084800040714     C                             = 'TRC'
084900040714     C                   EVAL      VATTRC=%trim(%subst(
085000040714     C                             %subst(vlrppt:posDaDft+1:
085100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200040714     C                   ENDIF
085300030822     C* ...
085400030822     C                   ENDIF
085500030822     C                   ENDDO
085600020204     C*
085700010330     C                   ENDSR
085800010607     C*----------------------------------------------------*
085900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086000010607     C*----------------------------------------------------*
086100010607     C     IMPVAB        BEGSR
086200010607     C*
086300010607     C                   SETOFF                                       3132
086400110315 xxx C***                SETON                                        35
086500030515     C*
086600010607     C                   MOVEL     datcor        VABAAS
086700010607     C                   MOVE      datcor        VABMGS
086800010607     C*
086900030715     C**********
087000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087100030715     C**********
087200030715     C                   IF        wGiro = *zeros
087300040113     C*
087400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087700080429     C                             %trim(intcol(5))<>*blanks
087800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
087900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088000080429     C                                      %trim(intcol(5))
088100070823     C     ';':CharCSV   XLATE     vindta        vindta
088200040510     C                   ENDIF
088300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088400031201     C     minu:maiu     XLATE     vindta        vindta
088500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088600030715     C                   DOW       posDa <= %len(%trim(vindta))
088700030715     C*
088800030715     C* Gestisco il 1° campo
088900030715     C                   IF        i = 1
089000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089100030715     C                   EVAL      posDa = 1
089200030715     C* Eseguo lo scan x trovare la fine del primo campo
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C* X i campi successivi al 1°
089800030715     C                   ELSE
089900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090000030715     C                   EVAL      posDa = posA + 1
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500030715     C                   ENDIF
091600030715     C* Incremento il contatore d campo
091700030715     C                   EVAL      i = i +1
091800030715     C                   ENDDO
091900030715     C*
092000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092100030715     C* delimitazione testo
092200030715     C                   EVAL      i = 1
092300030715     C                   DOW       i < %elem(SkSplitFLD)
092400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092600030715     C                   EVAL      i = i + 1
092700030715     C                   ENDDO
092800030715     C                   EVAL      wGiro = 1
092900030715     C                   ELSE
093000030715     C                   EVAL      wGiro = 2
093100031204     C**********
093200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093300031204     C* d separatore campo
093400031204     C**********
093500031204     C                   EVAL      vindta = %trim(vindta)
093600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093700031204     C                   EVAL      vindta = %subst(vindta:2)
093800031204     C                   ENDDO
093900031204     C*
094000031204     C                   Z-ADD     *zeros        lunghInput        4 0
094100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094500110912     C                   ENDDO
094600030715     C**********
094700030715     C* Effettuo lo split del campo dati d input
094800030715     C**********
094900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095000030715     C                   DOW       posDa <= %len(%trim(vindta))
095100030715     C*
095200030715     C* Gestisco il 1° campo
095300030715     C                   IF        i = 1
095400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095500030715     C                   EVAL      posDa = 1
095600030715     C* Eseguo lo scan x trovare la fine del primo campo
095700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C* X i campi successivi al 1°
096200030715     C                   ELSE
096300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096400030715     C                   EVAL      posDa = posA + 1
096500031204     C* Verifico che nn vi sia il campo nn valorizzato
096600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096700031204     C* Se campo nn valorizzato skippo al prossimo
096800031204     C                   EVAL      SkSplitCSV(i) = *blanks
096900031204     C                   EVAL      posA  = posA + 1
097000031204     C                   ELSE
097100030715     C* Eseguo lo scan x trovare la fine del campo corrente
097200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097300030715     C* Gestisco l'ultimo campo
097400030715     C                   IF        posA = *zeros
097500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097600030715     C* A questo punto "estraggo" il campo corrente
097700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097800030715     C                                                    (posA-posDa))
097900030715     C                   LEAVE
098000030715     C                   ELSE
098100030715     C* A questo punto "estraggo" il campo corrente
098200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098300030715     C                                                    (posA-posDa))
098400030715     C                   ENDIF
098500031204     C                   ENDIF
098600030715     C                   ENDIF
098700030715     C* Incremento il contatore d campo
098800030715     C                   EVAL      i = i +1
098900030715     C                   ENDDO
099000030715     C*
099100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099200030715     C* delimitazione testo
099300030715     C                   EVAL      i = 1
099400030715     C                   DOW       i < %elem(SkSplitCSV)
099500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099700030715     C                   EVAL      i = i + 1
099800030715     C                   ENDDO
099900100720     C*
100000100720     C* Stacco un progressivo x numero spedizione
100100151104 xxx C***                EXSR      repNSP
100200100326     C*
100300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
100400030715     C                   EVAL      i = 1
100500030715     C                   DOW       i < %elem(SkSplitFLD)
100600030715     C*
100700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
100800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
100900030715     C                             SkSplitCSV(i) <> *blanks
101000030721     C*** ATB
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
101200030721     C                   EVAL      VABATB = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RSD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
101600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
101700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
101800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
101900030715     C* ==
102000130528     C                   ENDIF
102100030721     C*** RD2
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
102300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030715     C*** IND
102600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
102700030715     C                   EVAL      VABIND = SkSplitCSV(i)
102800030715     C                   ENDIF
102900030715     C*** LOD
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
103100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** PRD
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
103500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030721     C*** NZD
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
103900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030715     C*** RMA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
104300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** NOT
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
104700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** NT2
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
105100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030715     C*** VCA
105400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
105500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
105600030715     C                   ENDIF
105700030715     C*** VAS
105800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
105900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
106000030715     C                   ENDIF
106100030715     C*** FFD
106200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
106300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
106400030715     C                   ENDIF
106500030721     C*** GC1
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
106700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GC2
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
107100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TSP
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
107500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
107600030721     C                   ENDIF
107700040510     C*** CBO
107800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
107900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
108000040510     C                   ENDIF
108100030721     C*** NAS
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
108300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** TIC
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
108700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** GCA
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
109100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** XCO
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
109500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** CTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
109900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** TCR
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
110300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** CTS
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
110700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** FTM
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
111100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** VAD
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
111500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** GMA
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
111900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** GGA
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
112300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** GVA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
112700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** TC1
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
113100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** TC2
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
113500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*** SCL
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
113900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
114000030721     C                   ENDIF
114100030721     C*** RMO
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
114300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
114400030721     C                   ENDIF
114500030721     C*** NMO
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
114700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
114800030721     C                   ENDIF
114900030721     C*
115000030721     C* Reperisco quindi i campi numerici...
115100040510     C*** NSP
115200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
115300040510     C                   EVAL      PiStr=SkSplitCSV(i)
115400040510     C                   EXSR      CHKNUM
115500040510     C                   IF        PiInt=*on
115600040510     C                   Z-ADD     PiVal         VABNSP
115700040510     C                   ELSE
115800040510     C                   SETON                                        31
115900040510     C                   EVAL      VABNSP = *zeros
116000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040510     C                             + ' ' + 'VABNSP'
116200040510     C                   ENDIF
116300040510     C                   ENDIF
116400071106     C*** RMN
116500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
116600071106     C                   EVAL      PiStr=SkSplitCSV(i)
116700071106     C                   EXSR      CHKNUM
116800071106     C                   IF        PiInt=*on
116900071106     C                   Z-ADD     PiVal         VABRMN
117000071106     C                   ELSE
117100071106     C                   SETON                                        32
117200071106     C                   EVAL      VABRMN = *zeros
117300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
117400071106     C                             + ' ' + 'VABRMN'
117500071106     C                   ENDIF
117600071106     C                   ENDIF
117700030721     C*** CAD
117800130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117900151117     C                   EVAL      wCAD=SkSplitCSV(i)
118000130605     C                   ENDIF
118100030721     C*** NCL
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on
118600030721     C                   Z-ADD     PiVal         VABNCL
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABNCL = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100040122     C                             + ' ' + 'VABNCL'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** PKB
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiNum=*on
119900130610     C                   Z-ADD(H)  PiVal         VABPKB
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      VABPKB = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABPKB'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** CAS
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
121000151103     C                             %trim(SkSplitCSV(i)) <> *zeros
121100130703     C                   EVAL      VABVCA = 'EUR'
121200140930     C                   EVAL      VABTIC = *blank
121300140513     C                   EVAL      wFlgCAS = '1'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiNum=*on
121700030721     C                   Z-ADD     PiVal         VABCAS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABCAS = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABCAS'
122300030721     C                   ENDIF
122400140513     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** CCM
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on      AND
123100030721     C                             PiVal<=9999999 AND
123200030721     C                             PiVal>*zeros
123300030721     C                   Z-ADD     PiVal         VABCCM
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABCCM = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800040510     C                             + ' ' + 'VABCCM'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** LNP
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on    AND
124600030721     C                             PiVal<=999   AND
124700030721     C                             PiVal>*zeros
124800030721     C                   Z-ADD     PiVal         VABLNP
124900030721     C                   ELSE
125000040715     C                   SETON                                        32
125100030721     C                   EVAL      VABLNP = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040510     C                             + ' ' + 'VABLNP'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** NRS
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on AND
126100030721     C                             PiVal<=99
126200030721     C                   Z-ADD     PiVal         VABNRS
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABNRS'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** LNA
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiInt=*on    AND
127400030721     C                             PiVal<=999   AND
127500030721     C                             PiVal>*zeros
127600030721     C                   Z-ADD     PiVal         VABLNA
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      VABLNA = *zeros
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABLNA'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** CTR
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on  AND
128900030721     C                             PiVal<=999
129000030721     C                   Z-ADD     PiVal         VABCTR
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABCTR'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** IAS
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129900141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
130000141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
130100141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
130200141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
130300141003     C                             %trim(SkSplitCSV(i)) <> ',00'
130400141006     C                   EVAL      VABVAS = 'EUR'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiNum=*on
130800030721     C                   Z-ADD     PiVal         VABIAS
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABIAS'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500141003     C                   ENDIF
131600030721     C*** VLB
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiNum=*on
132100130610     C                   Z-ADD(H)  PiVal         VABVLB
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABVLB'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** QFT
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiNum=*on
133300030721     C                   Z-ADD     PiVal         VABQFT
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABQFT'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** NCD
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on      AND
134500030721     C                             PiVal<=9999999
134600030721     C                   Z-ADD     PiVal         VABNCD
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABNCD'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** NCA
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on      AND
135800030721     C                             PiVal<=9999999
135900030721     C                   Z-ADD     PiVal         VABNCA
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABNCA'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** ZNC
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on AND
137100030721     C                             PiVal<=99
137200030721     C                   Z-ADD     PiVal         VABZNC
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABZNC'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** DCR
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on       AND
138400030721     C                             PiVal<=99999999
138500030721     C                   Z-ADD     PiVal         VABDCR
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABDCR'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** HCR
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on   AND
139700030721     C                             PiVal<=9999
139800030721     C                   Z-ADD     PiVal         VABHCR
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABHCR'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500030721     C*** VMD
140600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140700030721     C                   EVAL      PiStr=SkSplitCSV(i)
140800030721     C                   EXSR      CHKNUM
140900030721     C                   IF        PiNum=*on
141000030721     C                   Z-ADD     PiVal         VABVMD
141100030721     C                   ELSE
141200030721     C                   SETON                                        32
141300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141400030721     C                             + ' ' + 'VABVMD'
141500030721     C                   ENDIF
141600030721     C                   ENDIF
141700030721     C*** ANT
141800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
141900030721     C                   EVAL      PiStr=SkSplitCSV(i)
142000030721     C                   EXSR      CHKNUM
142100030721     C                   IF        PiInt=*on        AND
142200030721     C                             PiVal<=999999999
142300030721     C                   Z-ADD     PiVal         VABANT
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142700030721     C                             + ' ' + 'VABANT'
142800030721     C                   ENDIF
142900030721     C                   ENDIF
143000030721     C*** CMO
143100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143200030721     C                   EVAL      PiStr=SkSplitCSV(i)
143300030721     C                   EXSR      CHKNUM
143400030721     C                   IF        PiInt=*on
143500030721     C                   Z-ADD     PiVal         Num5_0
143600030721     C                   MOVEL(p)  Num5_0        VABCMO
143700030721     C                   ELSE
143800030721     C                   SETON                                        32
143900030721     C                   EVAL      VABCMO = *zeros
144000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144100030721     C                             + ' ' + 'VABCMO'
144200030721     C                   ENDIF
144300030721     C                   ENDIF
144400040510     C*
144500040510     C* Reperisco i campi del FIVAT
144600040510     C*** VATNOT
144700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
144900040510     C                   ENDIF
145000040510     C*** VATTRC
145100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145300040510     C                   ENDIF
145400060331     C*** VATNOT_A
145500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145700060331     C                   ENDIF
145800060331     C*** VATNOT_B
145900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146100060331     C                   ENDIF
146200060331     C*** VATNOT_E
146300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146500060331     C                   ENDIF
146600110927     C*** VATNOT_I
146700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
146900110927     C                   ENDIF
147000141003     C*** VATNOT_IJ
147100141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
147200141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147300141003     C                   ENDIF
147400140319     C*** VATNOT_S
147500140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147600140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147700140319     C                   ENDIF
147800070823     C*
147900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148000150309     C*
148100151117     C***  RMA / NSP
148200151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
148300151117     C                   EVAL      VABRMA=%trim(SkSplitCSV(i))
148400151117     C                   EVAL      PiStr=SkSplitCSV(i)
148500150309     C                   EXSR      CHKNUM
148600150309     C                   IF        PiInt=*on
148700151104     C                   Z-ADD     PiVal         VABNSP
148800150309     C                   ELSE
148900150309     C                   SETON                                        31
149000151104     C                   EVAL      VABNSP = *zeros
149100150309     C                   EVAL      vinmsg = %trimr(vinmsg)
149200151117     C                             + ' ' + 'VABNSP'
149300150309     C                   ENDIF
149400150309     C                   ENDIF
149500151117     C*
149600151117     C***  CBO
149700151117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
149800151117     C                   IF        %trim(SkSplitCSV(i)) = 'DESTINATARIO'
149900151117     C                   EVAL      VABCBO = '2'
150000151117     C                   ENDIF
150100151117     C                   ENDIF
150200150922     C*
150300151104     C*** FFD
150400151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABFFD'
150500151104     C                   EVAL      wDestinazioneDiversa = *blank
150600151104     C                   IF        %trim(SkSplitCSV(i)) = 'S'
150700151104     C                   EVAL      VABFFD = 'S'
150800151104     C                   ENDIF
150900151104     C*** destinazione diversa
151000151104     C                   IF        %trim(SkSplitCSV(i)) = 'X'
151100151104     C                   EVAL      wDestinazioneDiversa = *on
151200151117     C                   EVAL      VABRSD = *blank
151300151117     C                   EVAL      VABRD2 = *blank
151400151117     C                   EVAL      VABIND = *blank
151500151117     C                   EVAL      wCAD =   *blank
151600151117     C                   EVAL      VABLOD = *blank
151700151117     C                   EVAL      VABPRD = *blank
151800151117     C                   EVAL      VABNZD = *blank
151900151104     C                   ENDIF
152000151117     C                   ENDIF
152100151117     C*
152200151104     C                   IF        wDestinazioneDiversa = *on
152300151104     C*** RSD
152400151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
152500151104     C                   EVAL      VABRSD = SkSplitCSV(i)
152600151104     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
152700151104     C     '@':'A'       XLATE     VABRSD        VABRSD
152800151104     C* ==
152900151104     C                   ENDIF
153000151104     C*** RD2
153100151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRD2'
153200151104     C                   EVAL      VABRD2 = SkSplitCSV(i)
153300151104     C                   ENDIF
153400151104     C*** IND
153500151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND'
153600151104     C                   EVAL      VABIND = SkSplitCSV(i)
153700151104     C                   ENDIF
153800151104     C*** CAD
153900151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
154000151117     C                   EVAL      wCAD=SkSplitCSV(i)
154100151104     C                   ENDIF
154200151104     C*** LOD
154300151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
154400151104     C                   EVAL      VABLOD = SkSplitCSV(i)
154500151104     C                   ENDIF
154600151104     C*** PRD
154700151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD'
154800151104     C                   EVAL      VABPRD = SkSplitCSV(i)
154900151104     C                   ENDIF
155000151104     C*** NZD
155100151104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
155200151104     C                   EVAL      VABNZD = SkSplitCSV(i)
155300151104     C                   ENDIF
155400151104     C                   ENDIF
155500141003     C*
155600071029     C***  <===  -----------------------------------------
155700040510     C*
155800030715     C                   ENDIF
155900030715     C                   EVAL      i = i + 1
156000030715     C                   ENDDO
156100070925     C*
156200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
156300140319     C*
156400151117     C*** CAD
156500151117     C                   IF        wCAD <> *blank
156600151117     C                   EVAL      PiStr=wCAD
156700151117     C                   EXSR      CHKNUM
156800151117     C                   IF        PiInt=*on
156900151117     C                   Z-ADD     PiVal         Num5_0
157000151117     C                   MOVEL(p)  Num5_0        VABCAD
157100151117     C                   ELSE
157200151117     C                   SETON                                        32
157300151117     C                   EVAL      VABCAD = *zeros
157400151117     C                   EVAL      vinmsg = %trimr(vinmsg)
157500151117     C                             + ' ' + 'VABCAD'
157600151117     C                   ENDIF
157700151117     C                   ENDIF
157800141014     C* se la nazione è IT la sblanco
157900151104     C                   IF        VABNZD = 'I' or
158000151104     C                             VABNZD = 'IT' or
158100141112     C                             VABNZD = 'ITA'
158200141014     C                   EVAL      VABNZD = *blank
158300141014     C                   ENDIF
158400070925     C*
158500070925     C* Considerazioni finali su CBO/CAS
158600140220     C                   IF        wFlgCAS = '1'    OR
158700140220     C                             VABCAS  > *zeros
158800140220     C                   IF        VABCBO = '1'
158900140220     C                   EVAL      VABCBO = '4'
159000140220     C                   ENDIF
159100140220     C                   IF        VABCBO = '2'
159200140220     C                   EVAL      VABCBO = '6'
159300140220     C                   ENDIF
159400140220     C                   ENDIF
159500070925     C***  <===  ----------------------------
159600040510     C*
159700150922     C* la prima volta
159800150922     C                   if        depspe = 0
159900150922     C* oppure a rottura spedizione
160000151117     C***                          or depspe <> vabRMN
160100151117     C                             or depspe <> vabNSP
160200150922     C* Stacco un progressivo x numero spedizione
160300151104 xxx C***                EXSR      repNSP
160400150922     C*
160500150922     C* durante la stessa spedizione
160600150922     C                   ELSE
160700150922     C* reperisco il precedente numero spedizione
160800150922     C                   EVAL      VABNSP = sav_VABNSP
160900150922     C                   ENDIF
161000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
161100150922     C                   IF        VABNSP = *zeros
161200150922     C                   SETON                                        31
161300150922     C                   ENDIF
161400040715     C*
161500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
161600040715     C                   IF        VABLNP = *zeros
161700040715     C                   SETON                                        31
161800040715     C                   ENDIF
161900040510     C*
162000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
162100120411     C                   IF        VABRMN = *zeros
162200120411     C                   Z-ADD     VABNSP        VABRMN
162300120411     C                   ENDIF
162400120328     C* IN QUESTO CASO LO FORZO = 1
162500120411     C***                IF        VABRMN = *zeros
162600120411     C***                Z-ADD     1             VABRMN
162700120411     C***                ENDIF
162800040510     C*
162900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
163000040510     C                   IF        VABRMA = *blanks
163100040714     C                   MOVEL     VABRMN        VABRMA
163200040510     C                   ENDIF
163300040128     C*
163400040510     C* Se provincia nn valorizzata la reperisco
163500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
163600110912     C                   IF        VABPRD =  *blanks AND
163700110912     C                             VABNZD =  *blanks AND
163800110912     C                             VABCAD <> *blanks AND
163900110912     C                             VABLOD <> *blanks
164000040128     C                   CLEAR                   TISI95DS
164100040128     C                   EVAL      I95TCN = '3'
164200040128     C                   Z-ADD     datcor        I95DAT
164300040128     C                   EVAL      I95NAR = VABNZD
164400040128     C                   EVAL      I95CAP = VABCAD
164500040128     C                   EVAL      I95LOC = VABLOD
164600040128     C                   CALL      'TISI95R'
164700040128     C                   PARM                    TISI95DS
164800040128     C                   EVAL      VABPRD = O95PRV
164900040510     C                   ENDIF
165000020204     C*
165100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
165200020204     C                   EXSR      CHKIMPDIV
165300030715     C                   ENDIF
165400020204     C*
165500010607     C                   ENDSR
165600010607     C*----------------------------------------------------*
165700010601
165800020204
165900020204     C*----------------------------------------------------*
166000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
166100020204     C*----------------------------------------------------*
166200020204     C     CHKIMPDIV     BEGSR
166300020204     C*
166400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
166500020204     C                   Z-ADD     *zeros        wrkDec            9 9
166600020204     C*
166700020204     C* Come prima cosa effettuo considerazioni sulla divisa
166800020204     C                   IF        vabIAS > *zeros
166900020204     C                   IF        vabVAS <> 'EUR'
167000020204     C                   EVAL      vabVAS =  'ITL'
167100020204     C                   ENDIF
167200020204     C                   ENDIF
167300020204     C*
167400020204     C                   IF        vabCAS > *zeros
167500020204     C                   IF        vabVCA <> 'EUR'
167600020204     C                   EVAL      vabVCA =  'ITL'
167700020204     C                   ENDIF
167800020204     C                   ENDIF
167900020204     C*
168000020204     C                   IF        vabVMD > *zeros
168100020204     C                   IF        vabVAD <> 'EUR'
168200020204     C                   EVAL      vabVAD =  'ITL'
168300020204     C                   ENDIF
168400020204     C                   ENDIF
168500020204     C*
168600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
168700020204     C                   Z-ADD     vabIAS        wrkDec
168800020204     C                   IF        wrkDec > *zeros
168900020204     C                   IF        vabVAS = 'ITL'
169000020204     C                   EVAL      vabIAS = *zeros
169100020204     C                   ENDIF
169200020204     C                   ENDIF
169300020204     C*
169400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
169500020204     C                   Z-ADD     vabCAS        wrkDec
169600020204     C                   IF        wrkDec > *zeros
169700020204     C                   IF        vabVCA = 'ITL'
169800020204     C                   EVAL      vabCAS = *zeros
169900020204     C                   ENDIF
170000020204     C                   ENDIF
170100020204     C*
170200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
170300020204     C                   Z-ADD     vabVMD        wrkDec
170400020204     C                   IF        wrkDec > *zeros
170500020204     C                   IF        vabVAD = 'ITL'
170600020204     C                   EVAL      vabVMD = *zeros
170700020204     C                   ENDIF
170800020204     C                   ENDIF
170900020204     C*
171000020204     C                   ENDSR
171100020204     C***
171200010330
171300100318
171400100318
171500100318     C*----------------------------------------------------*
171600100318     C*  CONTROLLO NUMERICITA' CAMPI
171700100318     C*----------------------------------------------------*
171800100318     C     CHKNUM        BEGSR
171900100318     C*
172000100318     C                   IF        PiDecChr = *blanks
172100100318     C                   EVAL      PiDecChr = CharNUM
172200100318     C                   ENDIF
172300100318     C*
172400100318     C                   callp     UBISNUM_Check(PiStr
172500100318     C                                          :PiDecChr
172600100318     C                                          :PiVal
172700100318     C                                          :PiNum
172800100318     C                                          :PiInt)
172900100318     C*
173000100318     C                   ENDSR
173100100318     C***
173200010601
173300010601
173400010601
173500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
173600141006      ***
173700141014     C     repfil        BEGSR
173800141014     C*
173900141014     C                   if        invfil = *zeros and
174000141014     C                             depfil > *zeros and
174100141014     C                             (vinflg = *blanks or
174200141014     C                              vinflg = *zeros)
174300141014     C*
174400141014     C                   eval      invfil = depfil
174500141014     C                   endif
174600141014     C*
174700141014     C                   if        depfil <> invfil and
174800141014     C                             invfil > *zeros
174900141014     C                   eval      flgMulti = '1'
175000141014     C                   if        vinflg = *blanks
175100141014     C                   add       1             cntNonEl
175200141014     C                   endif
175300141014     C                   endif
175400141014     C*
175500141014     C                   if        vinflg = '2'
175600141014     C                   eval      flgStato = '2'
175700141014     C                   endif
175800141014     C*
175900141014     C                   ENDSR
176000141014     C*
176100010601
176200010601
176300010601
176400010330
176500040119      /TITLE Invio dei dati al punto operativo.
176600040119     C     invio         BEGSR
176700040119     C*
176800040119     C* 1° invio FIVAT
176900040119     C                   reset                   dscmz
177000040119     C                   move      vlrpoi        cmzdst
177100040119     C                   eval      cmzfld = 'FIVATWWR'
177200040119     C                   eval      cmzmbd = vlrhdl
177300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
177400040119     C***                if        prmfir = *blanks
177500040119     C                   eval      cmzfla = 'FIVAT00F'
177600040119     C                   eval      cmzmba = 'FIVAT00F'
177700040119     C***                else
177800040119     C***                eval      cmzfla = prmfir
177900040119     C***                eval      cmzmba = prmfir
178000040119     C***                endif
178100040119     C                   eval      cmznrr = *zeros
178200040119     C                   move      §ctrokvt      cmznrr
178300040119     C                   eval      cmzlba = vlrfl1
178400040119     C                   call(e)   'TIS711C'
178500040119     C                   parm                    dscmz
178600040119     C                   parm      *blanks       esito
178700040119     C                   if        %error
178800040119     C                             or cmzerr = '1'
178900040119     C                             or esito  = '1'
179000040119     C                   eval      wrkesito = '3'
179100040119     C                   else
179200040119     C*
179300040119     C* 2° invio FIVAB
179400040119     C                   reset                   dscmz
179500040119     C                   move      vlrpoi        cmzdst
179600040119     C                   eval      cmzfld = vlrfou
179700040119     C                   eval      cmzmbd = vlrhdl
179800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
179900040119     C***                if        prmfir = *blanks
180000040119     C                   eval      cmzfla = 'FIVAB00F'
180100040119     C                   eval      cmzmba = 'FIVAB00F'
180200040119     C***                else
180300040119     C***                eval      cmzfla = prmfir
180400040119     C***                eval      cmzmba = prmfir
180500040119     C***                endif
180600040119     C                   eval      cmznrr = *zeros
180700040119     C                   move      §ctrokvb      cmznrr
180800040119     C                   eval      cmzlba = vlrfl1
180900040119     C                   call(e)   'TIS711C'
181000040119     C                   parm                    dscmz
181100040119     C                   parm      *blanks       esito
181200040119     C                   if        %error
181300040119     C                             or cmzerr = '1'
181400040119     C                             or esito  = '1'
181500040119     C                   eval      wrkesito = '3'
181600040119     C                   endif
181700040119     C                   endif
181800040119     C*
181900040119     C                   ENDSR
182000040119     C***
182100010601
182200010601
182300010601
182400010601
182500010601      /TITLE Invio dei dati al punto operativo.
182600010601     C     opeini        BEGSR
182700010601     C*
182800010601     C* Inizializzo flag e contatori operativi
182900010601     C                   movel     '0'           flgGiro           1
183000010601     C                   movel     '0'           flgMulti          1
183100010601     C                   movel     '1'           flgStato          1
183200010615     C                   movel     '0'           flgOk             1
183300010601     C                   z-add     *zeros        cntNonEl         10 0
183400010601     C                   z-add     *zeros        depfil            3 0
183500010601     C                   z-add     *zeros        invfil            3 0
183600010601     C*
183700010601     C                   ENDSR
183800010601     C***
183900070326
184000070326
184100070326
184200070326
184300070326     C     *pssr         BEGSR
184400070329     C*
184500070329     C                   if        %open(tivin00r)
184600070329     C                   close     tivin00r
184700070329     C                   endif
184800070329     C                   if        %open(fivabwwr)
184900070329     C                   close     fivabwwr
185000070329     C                   endif
185100070329     C                   if        %open(fivatwwr)
185200070329     C                   close     fivatwwr
185300070329     C                   endif
185400070326     C*
185500070326     C* Effettuo la chiamata al CLLE preposto
185600070326     C                   call(e)   'TITVVTC'
185700070326     C                   parm                    parccm
185800070326     C                   parm                    parmbr
185900070326     C                   parm      '2'           paropz
186000070326     C*
186100070326     C                   eval      wrkesito = '2'
186200070404     C*
186300070404     C                   seton                                        LR
186400070326     C*
186500070326     C                   ENDSR     '*CANCL'
186600070326     C***
186700100318
186800100318
186900100318
187000100318     C     normCSV       BEGSR
187100100318     C*
187200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
187300100318     C* separatori campi
187400100318     C                   if        CharTXT <> *blanks
187500100318     C                   z-add     *zeros        wLenVINDTA        4 0
187600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
187700100318     C                   setoff                                       80
187800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
187900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
188000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
188100100318     C                   if        not *in80
188200100318     C                   seton                                        80
188300100318     C                   else
188400100318     C                   setoff                                       80
188500100318     C                   endif
188600100318     C                   endif
188700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
188800100318     C                             and *in80 = *on
188900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
189000100318     C                   endif
189100100318     C                   enddo
189200100318     C                   endif
189300100318     C*
189400100318     C                   ENDSR
189500100318     C***
189600070326
189700100326
189800100326
189900100326     C*----------------------------------------------------*
190000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
190100100326     C*----------------------------------------------------*
190200100326     C     REPNSP        BEGSR
190300100326     C*
190400100326     C* NSP => Stacco un numeratore da AZNUM
190500100326     C                   clear                   TRUL33DS
190600100326     C                   eval      I33OPE = *zeros
190700100326     C                   eval      I33CNU = 302
190800100326     C                   eval      I33NUM = 1
190900100326     C                   movel     TRUL33DS      KPJBU
191000100326     C                   call      'TRUL33R'
191100100326     C                   parm                    KPJBA
191200100326     C                   movel     KPJBU         TRUL33DS
191300100326     C                   if        O33ERR = *zeros
191400100326     C                   z-add     O33NRF        VABNSP
191500100326     C                   z-add     O33NRF        VATNSP
191600100326     C                   else
191700100326     C                   SETON                                        31
191800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
191900140711     C                             + ' ' + 'VABNSP VATNSP'
192000100326     C                   endif
192100100326     C*
192200100326     C                   ENDSR
192300010330
192400100326
192500010330
192600000613     C     *inzsr        BEGSR
192700990910     C*
192800990910     C     *entry        plist
192900990920     C                   parm                    tivlrds
193000990921     C                   parm      wrkesito      esito
193100000724     C                   parm                    prmlit
193200000710     C                   parm                    prmfir
193300010330     C*
193400010330     C* CALCOLA LA DATA CORRENTE
193500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
193600100318     C                   eval      datcor = %dec(%date() : *ISO)
193700000613     C*
193800000613     C                   ENDSR
193900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
194000151117VABRMN;VABNCL;W_VABNSP;X;VABPKB;X;X;X;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;X;
194100151117VABCTR;X;VATNOT_E;VABTSP;X;VABCCM;X;VATNOT_B;VATNOT_I;VATNOT_A;X;X;X;X;X;X;W_VABCBO;X;X;X;
194200151117W_VABFFD;W_VABRSD;W_VABRD2;W_VABIND;W_VABCAD;W_VABLOD;W_VABPRD;W_VABNZD;
194300111116
194400100318
