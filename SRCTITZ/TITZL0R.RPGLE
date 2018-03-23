000100160121      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400160624      *
000500170929      * - sommo peso e volume NON colli perché in questo dato è già scritto il totale spedizione
000600170929      * - uso particolare della routine REPNSP: è stata spostata sempre all'interno del ciclo ma
000700170929      *   condizionata dal valore del campo che determina la rottura spedizione (in questo caso RMA)
000800170929      *   La spedizione sul file inviato dal cliente è formata da N rcd, 1 per ogni collo perché
000900170929      *   contiene il barcode.
001000170929      *   Quindi, se lasciassi la routine dov'era, avrei un NSP nuovo per riga, e NON VA BENE.
001100170929      *   Siccome so solo quando leggo la colonna W_VABRMA se serve un nuovo NSP, mi dovrei salvare
001200170929      *   l'ultimo scritto, Inoltre prima scrivo il primo VAT della nuova spedizione e poi scarico
001300170929      *   il VAB della precedente.
001400170929      *
001500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001600130917     H decedit('0,')
001700120209     F*
001800990910     Ftivin00r  uF   E             DISK    usropn
001900100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
002000040119     FFIVATwwr  O    E             DISK    usropn
002100170918     D*
002200040113     D*------------
002300040113     D* SCHIEDE A PROGRAMMA
002400040113     D*------------
002500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002600141003     D*------------
002700010330     D*----------------------------------------------------
002800010330     D* DICHIARAZIOINE VARIABILI DI WRK
002900010330     D*----------------------------------------------------
003000010330     D dscmz         e ds                  inz
003100010330     D psds           sds
003200010330     D  procname         *PROC
003300010330     D tivlrds       e ds                  extname(tivlr00f)
003400040128     D tisi95ds      e ds
003500100413     D fivabds       e ds                  extname(fivab00f)
003600100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003700100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003800010330     D esito           s              1
003900010330     D prmlit          s             10
004000010330     D prmfir          s             10
004100010330     D wrkesito        s                   like(esito)
004200010330     D rrnum           s              6  0 INZ(*zeros)
004300040510     D parccm          s              8    INZ(*blanks)
004400040119     D parmbr          s             10    INZ(*blanks)
004500040119     D paropz          s              1    INZ(*blanks)
004600040119     D chkcall         s              1    INZ(*blanks)
004700170918     D depspe          s                   LIKE(VABRMN) INZ(*zeros)
004800080418     D wFlgCAS         s              1    INZ(*blanks)
004900120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
005000120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005100170915     D Intestazioni    s               n   inz(*off)
005200100326     D*------------------
005300100326     D* DS REPERIMENTO NUMERATORE
005400100326     D*------------------
005500100326     D trul33ds      e ds                  inz
005600100326     D*------------------
005700100326     D* DS ARCHITETTURA
005800100326     D*------------------
005900100326     D kpjba         e ds                  inz
006000130611     D*------------------
006100130611     D TRUL28DSX     e ds                  inz
006200100326
006300030715     D*------------------
006400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006500030715     D*------------------
006600030715     D SkSplitFLD      S             10    DIM(100)
006700030715     D SkSplitCSV      S            256    DIM(100)
006800030715     D CharCSV         S              1
006900030715     D CharTXT         S              1
007000030715     D CharNUM         S              1
007100100318     D CharSOS         S              1
007200130626     D posDa           S              4  0 INZ(*zeros)
007300130626     D posA            S              4  0 INZ(*zeros)
007400030715     D i               s              3  0 INZ(1)
007500030715     D wGiro           s              1  0 INZ(*zeros)
007600070823     D wVATNOT_A       s                   LIKE(VATNOT)
007700060331     D wVATNOT_B       s                   LIKE(VATNOT)
007800160624     D wVATNOT_E       s                   LIKE(VATNOT)
007900110927     D wVATNOT_I       s                   LIKE(VATNOT)
008000140220     D wVATNOT_J       s                   LIKE(VATNOT)
008100140220     D wVATNOT_IJ      s             70
008200140319     D wVATNOT_S       s                   LIKE(VATNOT)
008300140919     D wNOT            s             70
008400120411     D Data8A          s              8
008500140527     D wBarcodeN       s             11s 0
008600140527     D wX              s              3s 0
008700140711     D TxtInOut        S           2048
008800140711     D ElencoChar      S            256
008900140711     D TipoElenco      S              1
009000140711     D CharSost        S              1
009100140711     D UpperCase       S              1
009200140711     D ChkNull         S              1
009300140711     D CharNull        S              1
009400140711     D EsitoBon        S              1
009500170713     D w2_0            s              2s 0
009600170713     D w7_0            s              7s 0
009700170712     D w70             s             70
009800170928     D wRMA_Prec       s                   like(VABRMA) INZ(*blanks)
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
021000010618     C*
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
025400150317     C* che VLRPOI sia 999 o no, non mi9 interessa perché il valore di FGS è stato forzato prima
025500150430     C                   if        vlrpoi = 999
025600150430     C                   move(P)   invfil        VABFGS
025700150430     C                   else
025800150430     C                   move(P)   vlrpoi        VABFGS
025900150430     C                   endif
026000010601     C*
026100010601     C                   exsr      PREELA
026200010601     C*
026300010604     C* Ebbene...
026400010604     C*
026500040119     C  N31              add       1             §CTROKVB
026600040119     C   32              add       1             §CTRMO
026700040119     C   31              add       1             §CTRNO
026800080418     C*
026900030822     C                   if        wGiro = 2
027000080513     C* Scrivo sempre il VAT
027100080513     C  N31              exsr      wrivat
027200110201     C                   exsr      chkvab
027300080513     C                   endif
027400010604     C*
027500010604     C                   if        *in31 = *off and
027600010604     C                             *in32 = *off
027700010604     C                   eval      vinflg = '1'
027800010604     C                   else
027900010604     C                   eval      vinflg = '2'
028000010604     C                   endif
028100141014     C                   endif
028200010601     C*
028300010604     C                   endif
028400010604     C*
028500010330     C                   else
028600010330     C                   eval      vinflg = '1'
028700010330     C                   endif
028800010601     C*
028900010601     C  N70              update    tivin000
029000010330     C*
029100030822     C  N70              enddo
029200040119     C*
029300080527     C* Verifico dati rimasti "in cannna"...
029400080527     C                   seton                                        65
029500110201     C                   exsr      chkvab
029600080527     C*
029700040119     C                   endif                                                  (endif - chkcall)
029800010601     C*
029900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030000010601     C                   if        cntNonEl = *zeros or
030100010601     C                             flgMulti = '0'
030200010330     C* Se non ci sono record con errori ...
030300010601     C                   if        §ctrno = 0 and
030400010604     C                             §ctrmo = 0 and
030500010601     C                             flgStato <> '2'
030600010330     C* ... restituisco esito OK.
030700010330     C                   eval      wrkesito = '0'
030800010330     C                   else
030900040119     C                   if        §ctrokvb > 0
031000010330     C                   eval      wrkesito = '1'
031100010330     C                   else
031200010615     C                   if        flgOk = '0'
031300010615     C                   eval      wrkesito = '2'
031400010615     C                   else
031500010615     C                   eval      wrkesito = '6'
031600010615     C                   endif
031700010330     C                   endif
031800010330     C                   endif
031900010601     C                   else
032000010601     C                   eval      wrkesito = '9'
032100010601     C                   endif
032200010330     C*
032300010330     C                   if        %open(tivin00r)
032400010330     C                   close     tivin00r
032500010330     C                   endif
032600021025     C                   if        %open(fivabwwr)
032700021025     C                   close     fivabwwr
032800010330     C                   endif
032900040512     C                   if        %open(fivatwwr)
033000040512     C                   close     fivatwwr
033100040512     C                   endif
033200010601     C*
033300010601     C                   if        vlrpoi <> 999
033400010601     C                   eval      invfil = vlrpoi
033500010601     C                   endif
033600010330     C*
033700040119     C                   if        §ctrokvb > 0
033800010601     C                             and invfil > *zeros
033900010330     C                   exsr      invio
034000010330     C                   endif
034100010601     C*
034200010618     C                   if        flgGiro = '1'
034300010601     C                   exsr      endela
034400010618     C                   endif
034500010330     C*
034600010330     C                   ENDSR
034700010330     C***
034800080527
034900080527
035000080527
035100080527     C*----------------------------------------------------*
035200080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
035300080527     C*----------------------------------------------------*
035400100413     C     CHKVAB        BEGSR
035500110201     C*
035600110201     C* Se richiesti ignorare colli / peso / volume
035700110315 xxx C***                if        *in35
035800110315 xxx C***                z-add     *zeros        VABNCL
035900110315 xxx C***                z-add     *zeros        VABPKB
036000110315 xxx C***                z-add     *zeros        VABVLB
036100110315 xxx C***                endif
036200080527     C*
036300080527     C* Scrivo il VAB solo a rottura d spedizione
036400080527     C                   if        depspe = *zeros
036500080527     C  N31              eval      fivabds_sav = fivabds
036600170918     C                   eval      depspe = vabRMN
036700080527     C                   else
036800170918     C                   if        depspe = vabRMN AND *in65 = *off
036900100413     C   51              add       VABNCL        sav_VABNCL
037000100413     C   51              add       VABPKB        sav_VABPKB
037100100413     C   51              add       VABVLB        sav_VABVLB
037200100720     C   52              z-add     VABNCL        sav_VABNCL
037300100720     C   52              z-add     VABPKB        sav_VABPKB
037400100720     C   52              z-add     VABVLB        sav_VABVLB
037500100720     C   53              add       VABNCL        sav_VABNCL
037600100720     C   53              z-add     VABPKB        sav_VABPKB
037700100720     C   53              z-add     VABVLB        sav_VABVLB
037800160624
037900160624     C* PERSONALIZZAZIONE
038000170929     C* sommo peso e volume NON colli
038100160624     C                   IF        *in54 = *on
038200170927     C                   EVAL      sav_VABVLB = sav_VABVLB + VABVLB
038300160624     C                   EVAL      sav_VABPKB = sav_VABPKB + VABPKB
038400160624     C                   ENDIF
038500160624
038600080527     C                   else
038700080527     C  N31              eval      fivabds_ok = fivabds_sav
038800080527     C  N31              exsr      wrivab
038900080527     C  N31              eval      fivabds_sav = fivabds
039000170918     C                   eval      depspe = vabRMN
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
040400170918     C                   if        VABRMN <> depspe or *in65 = *on
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
046400150316     C                   if        wVATNOT_E <> *blanks
046500060331     C                   eval      VATTRC = 'E'
046600170918     C*
046700170918     C* NON Sviluppare i "CHI SONO"
046800170918     C*
046900170918     C                   EVAL      VATNOT = wVATNOT_E
047000170918     C                   write     FIVAT000
047100170918     C                   add       1             §CTROKVT
047200150316     C                   endif
047300150316     C*
047400110927     C                   if        wVATNOT_I <> *blanks
047500110927     C                   eval      VATNOT = wVATNOT_I
047600140711     C                   eval      VATTRC = 'I'
047700110927     C                   write     FIVAT000
047800110927     C                   add       1             §CTROKVT
047900110927     C                   endif
048000140220     C*
048100140220     C                   if        wVATNOT_J <> *blanks
048200140220     C                   eval      VATNOT = wVATNOT_J
048300140220     C                   eval      VATTRC = 'J'
048400140220     C                   write     FIVAT000
048500140220     C                   add       1             §CTROKVT
048600140220     C                   endif
048700140319     C*
048800140319     C                   if        wVATNOT_S <> *blanks
048900140319     C                   eval      VATNOT = wVATNOT_S
049000140319     C* i 2 flag devono essere blank
049100140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
049200140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
049300140319     C                   eval      VATTRC = 'S'
049400140319     C                   write     FIVAT000
049500140319     C                   add       1             §CTROKVT
049600140319     C                   endif
049700040119     C*
049800040119     C                   ENDSR
049900040119
050000040119
050100040119
050200040119     C*----------------------------------------------------*
050300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
050400040119     C*----------------------------------------------------*
050500040119     C     PREVAT        BEGSR
050600040119     C*
050700040119     C* Compongo il nome del membro da dare al FIVATWWR
050800040119     C                   eval      parmbr = vlrhdl
050900040119     C                   movel     'M'           parmbr
051000040510     C                   eval      parccm = vlrksc
051100040119     C                   eval      paropz = '1'
051200040119     C* Effettuo la chiamata al CLLE preposto
051300040119     C                   call(e)   'TITVVTC'
051400040119     C                   parm                    parccm
051500040119     C                   parm                    parmbr
051600040119     C                   parm                    paropz
051700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
051800040119     C                   if        %error
051900040119     C                   movel     '1'           chkcall
052000040119     C                   else
052100040119     C                   movel     '0'           chkcall
052200040119     C                   endif
052300040119     C*
052400040119     C                   ENDSR
052500010601
052600010601
052700010601
052800010330     C*----------------------------------------------------*
052900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
053000010330     C*----------------------------------------------------*
053100040119     C     INZVAR        BEGSR
053200080527     C*
053300080527     C                   setoff                                       65
053400010330     C*
053500030822     C* Inizializzo variabili di wrk
053600020204     C                   Z-ADD     *zeros        Num5_0            5 0
053700101018     C                   MOVEL     *blanks       wFlgCAS
053800060331     C                   MOVEL     *blanks       wVATNOT_A
053900060331     C                   MOVEL     *blanks       wVATNOT_B
054000060331     C                   MOVEL     *blanks       wVATNOT_E
054100110927     C                   MOVEL     *blanks       wVATNOT_I
054200140220     C                   MOVEL     *blanks       wVATNOT_J
054300140220     C                   MOVEL     *blanks       wVATNOT_IJ
054400140319     C                   MOVEL     *blanks       wVATNOT_S
054500120411     C                   MOVEL     *blanks       wNOT
054600120411     C                   MOVEL     *blanks       Data8A
054700030822     C*
054800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
054900030822     C                   CLEAR                   SkSplitCSV
055000030822     C                   EVAL      i = 1
055100030822     C                   EVAL      posDa = *zeros
055200030822     C                   EVAL      posA  = *zeros
055300080527     C*
055400080527     C                   CLEAR                   FIVABDS
055500080527     C                   CLEAR                   FIVAT000
055600030822     C*
055700030822     C* Reimposto i valori di default
055800030822     C                   EXSR      DEFCAM
055900030822     C*
056000010330     C                   ENDSR
056100010330     C*----------------------------------------------------*
056200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
056300010330     C*----------------------------------------------------*
056400010330     C     DEFCAM        BEGSR
056500030822     C*
056600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
056700030715     C* e delimitatore testo.
056800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
056900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
057000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
057100100318     C*
057200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
057300100318     C                   EVAL      CharSOS = CharNUM
057400030822     C*
057500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
057600100720     C                   SETOFF                                       505152
057700100720     C                   SETOFF                                       53
057800160624     C                   SETOFF                                       54
057900100413     C                   SELECT
058000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
058100100720     C                   SETON                                        50
058200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
058300100413     C                   SETON                                        51
058400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
058500100720     C                   SETON                                        52
058600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
058700100720     C                   SETON                                        53
058800160624     C* PERSONALIZZAZIONE
058900160624     C                   WHEN      %subst(vlrppt:1:1) = 'P'
059000160624     C                   SETON                                        54
059100100413     C                   ENDSL
059200030822     C*
059300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
059400030822     C                   EVAL      posDaDft = 1
059500030822     C                   EVAL      posADft  = 0
059600030822     C                   EVAL      wGiroDft = 0
059700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
059800030822     C                             posDaDft > 0
059900030822     C*
060000030822     C* Gestisco il 1° giro
060100030822     C                   IF        wGiroDft = 0
060200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
060300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
060400030822     C* Incremento il contatore dei "giri"
060500030822     C                   EVAL      wGiroDft = 1
060600030822     C                   ELSE
060700030822     C                   EVAL      posDaDft = posADft
060800030822     C                   ENDIF
060900030822     C* Eseguo lo scan x trovare la fine del campo corrente
061000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
061100030822     C*
061200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
061300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
061400030822     C                   IF        posDaDft > 0 AND
061500030822     C                             posADft  > 0
061600030822     C* NCL
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'NCL'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiInt=*on
062600030822     C                   Z-ADD     PiVal         VABNCL
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CCM
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CCM'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiInt=*on
063900030822     C                   Z-ADD     PiVal         VABCCM
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* LNP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'LNP'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiInt=*on
065200030822     C                   Z-ADD     PiVal         VABLNP
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* NRS
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'NRS'
066000030822     C                   EVAL      PiStr=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   EXSR      CHKNUM
066400030822     C                   IF        PiInt=*on
066500030822     C                   Z-ADD     PiVal         VABNRS
066600030822     C                   ENDIF
066700030822     C                   ENDIF
066800030822     C* CTR
066900030822     C                   IF        %subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):1:3)
067200030822     C                             = 'CTR'
067300030822     C                   EVAL      PiStr=%trim(%subst(
067400030822     C                             %subst(vlrppt:posDaDft+1:
067500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067600030822     C                   EXSR      CHKNUM
067700030822     C                   IF        PiInt=*on
067800030822     C                   Z-ADD     PiVal         VABCTR
067900030822     C                   ENDIF
068000030822     C                   ENDIF
068100030822     C* PKB
068200030822     C                   IF        %subst(
068300030822     C                             %subst(vlrppt:posDaDft+1:
068400030822     C                             posADft-posDaDft-1):1:3)
068500030822     C                             = 'PKB'
068600030822     C                   EVAL      PiStr=%trim(%subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068900030822     C                   EXSR      CHKNUM
069000030822     C                   IF        PiNum=*on
069100130610     C                   Z-ADD     PiVal         VABPKB
069200030822     C                   ENDIF
069300030822     C                   ENDIF
069400030822     C* VLB
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VLB'
069900030822     C                   EVAL      PiStr=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   EXSR      CHKNUM
070300030822     C                   IF        PiNum=*on
070400130610     C                   Z-ADD     PiVal         VABVLB
070500030822     C                   ENDIF
070600030822     C                   ENDIF
070700030822     C* QFT
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'QFT'
071200030822     C                   EVAL      PiStr=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   EXSR      CHKNUM
071600030822     C                   IF        PiNum=*on
071700030822     C                   Z-ADD     PiVal         VABQFT
071800030822     C                   ENDIF
071900030822     C                   ENDIF
072000030822     C* CBO
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'CBO'
072500030822     C                   EVAL      VABCBO=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* TSP
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'TSP'
073400030822     C                   EVAL      VABTSP=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* VAS
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'VAS'
074300030822     C                   EVAL      VABVAS=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* VCA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'VCA'
075200030822     C                   EVAL      VABVCA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600070925     C* TIC
075700070925     C                   IF        %subst(
075800070925     C                             %subst(vlrppt:posDaDft+1:
075900070925     C                             posADft-posDaDft-1):1:3)
076000070925     C                             = 'TIC'
076100070925     C                   EVAL      VABTIC=%trim(%subst(
076200070925     C                             %subst(vlrppt:posDaDft+1:
076300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400070925     C                   ENDIF
076500030822     C* GCA
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'GCA'
077000030822     C                   EVAL      VABGCA=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* CTM
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'CTM'
077900030822     C                   EVAL      VABCTM=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* FFD
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'FFD'
078800030822     C                   EVAL      VABFFD=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* VAD
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'VAD'
079700030822     C                   EVAL      VABVAD=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* GMA
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'GMA'
080600030822     C                   EVAL      VABGMA=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* GGA
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'GGA'
081500030822     C                   EVAL      VABGGA=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* GVA
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'GVA'
082400030822     C                   EVAL      VABGVA=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800030822     C* TC1
082900030822     C                   IF        %subst(
083000030822     C                             %subst(vlrppt:posDaDft+1:
083100030822     C                             posADft-posDaDft-1):1:3)
083200030822     C                             = 'TC1'
083300030822     C                   EVAL      VABTC1=%trim(%subst(
083400030822     C                             %subst(vlrppt:posDaDft+1:
083500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600030822     C                   ENDIF
083700030822     C* TC2
083800030822     C                   IF        %subst(
083900030822     C                             %subst(vlrppt:posDaDft+1:
084000030822     C                             posADft-posDaDft-1):1:3)
084100030822     C                             = 'TC2'
084200030822     C                   EVAL      VABTC2=%trim(%subst(
084300030822     C                             %subst(vlrppt:posDaDft+1:
084400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084500030822     C                   ENDIF
084600040714     C* VATTRC
084700040714     C                   IF        %subst(
084800040714     C                             %subst(vlrppt:posDaDft+1:
084900040714     C                             posADft-posDaDft-1):1:3)
085000040714     C                             = 'TRC'
085100040714     C                   EVAL      VATTRC=%trim(%subst(
085200040714     C                             %subst(vlrppt:posDaDft+1:
085300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085400040714     C                   ENDIF
085500030822     C* ...
085600030822     C                   ENDIF
085700030822     C                   ENDDO
085800020204     C*
085900010330     C                   ENDSR
086000010607     C*----------------------------------------------------*
086100021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
086200010607     C*----------------------------------------------------*
086300010607     C     IMPVAB        BEGSR
086400010607     C*
086500010607     C                   SETOFF                                       3132
086600110315 xxx C***                SETON                                        35
086700030515     C*
086800010607     C                   MOVEL     datcor        VABAAS
086900010607     C                   MOVE      datcor        VABMGS
087000010607     C*
087100030715     C**********
087200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
087300030715     C**********
087400030715     C                   IF        wGiro = *zeros
087500040113     C*
087600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
087700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
087800080429     C                             %trim(intcol(3))+%trim(intcol(4))+
087900080429     C                             %trim(intcol(5))<>*blanks
088000040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
088100080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
088200080429     C                                      %trim(intcol(5))
088300070823     C     ';':CharCSV   XLATE     vindta        vindta
088400040510     C                   ENDIF
088500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
088600031201     C     minu:maiu     XLATE     vindta        vindta
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300030715     C* Eseguo lo scan x trovare la fine del campo corrente
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* Gestisco l'ultimo campo
090600030715     C                   IF        posA = *zeros
090700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   LEAVE
091200030715     C                   ELSE
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   ENDIF
091700030715     C                   ENDIF
091800030715     C* Incremento il contatore d campo
091900030715     C                   EVAL      i = i +1
092000030715     C                   ENDDO
092100030715     C*
092200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092300030715     C* delimitazione testo
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitFLD)
092600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
092700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
092800030715     C                   EVAL      i = i + 1
092900030715     C                   ENDDO
093000030715     C                   EVAL      wGiro = 1
093100030715     C                   ELSE
093200030715     C                   EVAL      wGiro = 2
093300031204     C**********
093400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
093500031204     C* d separatore campo
093600031204     C**********
093700031204     C                   EVAL      vindta = %trim(vindta)
093800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
093900031204     C                   EVAL      vindta = %subst(vindta:2)
094000031204     C                   ENDDO
094100031204     C*
094200031204     C                   Z-ADD     *zeros        lunghInput        4 0
094300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
094500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
094600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
094700110912     C                   ENDDO
094800030715     C**********
094900030715     C* Effettuo lo split del campo dati d input
095000030715     C**********
095100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
095200030715     C                   DOW       posDa <= %len(%trim(vindta))
095300030715     C*
095400030715     C* Gestisco il 1° campo
095500030715     C                   IF        i = 1
095600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
095700030715     C                   EVAL      posDa = 1
095800030715     C* Eseguo lo scan x trovare la fine del primo campo
095900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096000030715     C* A questo punto "estraggo" il campo corrente
096100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096200030715     C                                                    (posA-posDa))
096300030715     C* X i campi successivi al 1°
096400030715     C                   ELSE
096500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
096600030715     C                   EVAL      posDa = posA + 1
096700031204     C* Verifico che nn vi sia il campo nn valorizzato
096800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
096900031204     C* Se campo nn valorizzato skippo al prossimo
097000031204     C                   EVAL      SkSplitCSV(i) = *blanks
097100031204     C                   EVAL      posA  = posA + 1
097200031204     C                   ELSE
097300030715     C* Eseguo lo scan x trovare la fine del campo corrente
097400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
097500030715     C* Gestisco l'ultimo campo
097600030715     C                   IF        posA = *zeros
097700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
097800030715     C* A questo punto "estraggo" il campo corrente
097900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098000030715     C                                                    (posA-posDa))
098100030715     C                   LEAVE
098200030715     C                   ELSE
098300030715     C* A questo punto "estraggo" il campo corrente
098400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
098500030715     C                                                    (posA-posDa))
098600030715     C                   ENDIF
098700031204     C                   ENDIF
098800030715     C                   ENDIF
098900030715     C* Incremento il contatore d campo
099000030715     C                   EVAL      i = i +1
099100030715     C                   ENDDO
099200030715     C*
099300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
099400030715     C* delimitazione testo
099500030715     C                   EVAL      i = 1
099600030715     C                   DOW       i < %elem(SkSplitCSV)
099700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
099800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
099900030715     C                   EVAL      i = i + 1
100000030715     C                   ENDDO
100100100720     C*
100200100720     C* Stacco un progressivo x numero spedizione
100300170928     C* non lo eseguo qui ma a rottura RMA
100400170928 xxx C***                EXSR      repNSP
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
121200160624     C*                  Z-ADD(H)  PiVal         VABPKB
121300160630     C                   EVAL(H)   VABPKB = PiVal
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
122500170918     C                             %trim(SkSplitCSV(i)) <> '0,000'
122600130703     C                   EVAL      VABVCA = 'EUR'
122700160630     C                   EVAL      VABTIC = 'BM'
122800140513     C                   EVAL      wFlgCAS = '1'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200030721     C                   Z-ADD     PiVal         VABCAS
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      VABCAS = *zeros
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABCAS'
123800030721     C                   ENDIF
123900140513     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** CCM
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on      AND
124600030721     C                             PiVal<=9999999 AND
124700030721     C                             PiVal>*zeros
124800030721     C                   Z-ADD     PiVal         VABCCM
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      VABCCM = *zeros
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300040510     C                             + ' ' + 'VABCCM'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** LNP
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on    AND
126100030721     C                             PiVal<=999   AND
126200030721     C                             PiVal>*zeros
126300030721     C                   Z-ADD     PiVal         VABLNP
126400030721     C                   ELSE
126500040715     C                   SETON                                        32
126600030721     C                   EVAL      VABLNP = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800040510     C                             + ' ' + 'VABLNP'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NRS
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on AND
127600030721     C                             PiVal<=99
127700030721     C                   Z-ADD     PiVal         VABNRS
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNRS'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** LNA
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on    AND
128900030721     C                             PiVal<=999   AND
129000030721     C                             PiVal>*zeros
129100030721     C                   Z-ADD     PiVal         VABLNA
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      VABLNA = *zeros
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABLNA'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** CTR
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on  AND
130400030721     C                             PiVal<=999
130500030721     C                   Z-ADD     PiVal         VABCTR
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABCTR'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** IAS
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
131400141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
131500141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
131600150428     C                             %trim(SkSplitCSV(i)) <> '0.00'
131700141006     C                   EVAL      VABVAS = 'EUR'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiNum=*on
132100030721     C                   Z-ADD     PiVal         VABIAS
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABIAS'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800141003     C                   ENDIF
132900030721     C*** VLB
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiNum=*on
133400130610     C                   Z-ADD(H)  PiVal         VABVLB
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABVLB'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** QFT
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABQFT
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABQFT'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** NCD
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on      AND
135800030721     C                             PiVal<=9999999
135900030721     C                   Z-ADD     PiVal         VABNCD
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABNCD'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** NCA
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on      AND
137100030721     C                             PiVal<=9999999
137200030721     C                   Z-ADD     PiVal         VABNCA
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137600030721     C                             + ' ' + 'VABNCA'
137700030721     C                   ENDIF
137800030721     C                   ENDIF
137900030721     C*** ZNC
138000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
138100030721     C                   EVAL      PiStr=SkSplitCSV(i)
138200030721     C                   EXSR      CHKNUM
138300030721     C                   IF        PiInt=*on AND
138400030721     C                             PiVal<=99
138500030721     C                   Z-ADD     PiVal         VABZNC
138600030721     C                   ELSE
138700030721     C                   SETON                                        32
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABZNC'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200030721     C*** DCR
139300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
139400030721     C                   EVAL      PiStr=SkSplitCSV(i)
139500030721     C                   EXSR      CHKNUM
139600030721     C                   IF        PiInt=*on       AND
139700030721     C                             PiVal<=99999999
139800030721     C                   Z-ADD     PiVal         VABDCR
139900030721     C                   ELSE
140000030721     C                   SETON                                        32
140100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140200030721     C                             + ' ' + 'VABDCR'
140300030721     C                   ENDIF
140400030721     C                   ENDIF
140500030721     C*** HCR
140600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
140700030721     C                   EVAL      PiStr=SkSplitCSV(i)
140800030721     C                   EXSR      CHKNUM
140900030721     C                   IF        PiInt=*on   AND
141000030721     C                             PiVal<=9999
141100030721     C                   Z-ADD     PiVal         VABHCR
141200030721     C                   ELSE
141300030721     C                   SETON                                        32
141400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141500030721     C                             + ' ' + 'VABHCR'
141600030721     C                   ENDIF
141700030721     C                   ENDIF
141800030721     C*** VMD
141900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
142000030721     C                   EVAL      PiStr=SkSplitCSV(i)
142100030721     C                   EXSR      CHKNUM
142200030721     C                   IF        PiNum=*on
142300030721     C                   Z-ADD     PiVal         VABVMD
142400030721     C                   ELSE
142500030721     C                   SETON                                        32
142600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142700030721     C                             + ' ' + 'VABVMD'
142800030721     C                   ENDIF
142900030721     C                   ENDIF
143000030721     C*** ANT
143100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
143200030721     C                   EVAL      PiStr=SkSplitCSV(i)
143300030721     C                   EXSR      CHKNUM
143400030721     C                   IF        PiInt=*on        AND
143500030721     C                             PiVal<=999999999
143600030721     C                   Z-ADD     PiVal         VABANT
143700030721     C                   ELSE
143800030721     C                   SETON                                        32
143900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144000030721     C                             + ' ' + 'VABANT'
144100030721     C                   ENDIF
144200030721     C                   ENDIF
144300030721     C*** CMO
144400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
144500030721     C                   EVAL      PiStr=SkSplitCSV(i)
144600030721     C                   EXSR      CHKNUM
144700030721     C                   IF        PiInt=*on
144800030721     C                   Z-ADD     PiVal         Num5_0
144900030721     C                   MOVEL(p)  Num5_0        VABCMO
145000030721     C                   ELSE
145100030721     C                   SETON                                        32
145200030721     C                   EVAL      VABCMO = *zeros
145300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
145400030721     C                             + ' ' + 'VABCMO'
145500030721     C                   ENDIF
145600030721     C                   ENDIF
145700040510     C*
145800040510     C* Reperisco i campi del FIVAT
145900040510     C*** VATNOT
146000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
146100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
146200040510     C                   ENDIF
146300040510     C*** VATTRC
146400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
146500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
146600040510     C                   ENDIF
146700060331     C*** VATNOT_A
146800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
146900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
147000060331     C                   ENDIF
147100060331     C*** VATNOT_B
147200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
147300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
147400060331     C                   ENDIF
147500060331     C*** VATNOT_E
147600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
147700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147800060331     C                   ENDIF
147900110927     C*** VATNOT_I
148000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
148100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
148200110927     C                   ENDIF
148300141003     C*** VATNOT_IJ
148400141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
148500141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148600141003     C                   ENDIF
148700140319     C*** VATNOT_S
148800140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
148900140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
149000140319     C                   ENDIF
149100070823     C*
149200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
149300150309     C*
149400170713
149500170928      *** W_VABRMA: è VABRMA ma indica anche la rottura spedizione
149600170918     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
149700170928     C* questo è il dato che mi dà la rottura spedizione
149800170928     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
149900170928     C* c'è rottura
150000170928     C                   IF        VABRMA <> wRMA_Prec
150100170928     C                   EXSR      REPNSP
150200170928     C                   EVAL      wRMA_Prec = VABRMA
150300170928     C* NON c'è rottura
150400170928     C                   ELSE
150500170928     C* riuso il progressivo staccato
150600170928     C                   z-add     O33NRF        VABNSP
150700170928     C                   z-add     O33NRF        VATNSP
150800170928     C                   ENDIF
150900170712     C                   ENDIF
151000170712
151100170712      *** W_VATNOT_I: è sia VATNOT_I che VATNOT_J
151200170712     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
151300170712     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
151400170712     C                   ENDIF
151500170712
151600141003     C*
151700071029     C***  <===  -----------------------------------------
151800040510     C*
151900030715     C                   ENDIF
152000030715     C                   EVAL      i = i + 1
152100030715     C                   ENDDO
152200070925     C*
152300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
152400170706     C*
152500170712
152600170712
152700070925     C*
152800160404     C* pulisco la nazione se vuol essere Italia
152900160404     C                   IF        VABNZD = 'I'     OR
153000160404     C                             VABNZD = 'IT'    OR
153100160404     C                             VABNZD = 'ITA'
153200160404     C                   EVAL      VABNZD = *blank
153300160404     C                   ENDIF
153400160404     C*
153500070925     C* Considerazioni finali su CBO/CAS
153600140220     C                   IF        wFlgCAS = '1'    OR
153700140220     C                             VABCAS  > *zeros
153800140220     C                   IF        VABCBO = '1'
153900140220     C                   EVAL      VABCBO = '4'
154000140220     C                   ENDIF
154100140220     C                   IF        VABCBO = '2'
154200140220     C                   EVAL      VABCBO = '6'
154300140220     C                   ENDIF
154400140220     C                   ENDIF
154500070925     C***  <===  ----------------------------
154600040510     C*
154700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154800040510     C                   IF        VABNSP = *zeros
154900040510     C                   SETON                                        31
155000040510     C                   ENDIF
155100040715     C*
155200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
155300040715     C                   IF        VABLNP = *zeros
155400040715     C                   SETON                                        31
155500040715     C                   ENDIF
155600040510     C*
155700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155800120411     C                   IF        VABRMN = *zeros
155900120411     C                   Z-ADD     VABNSP        VABRMN
156000120411     C                   ENDIF
156100120328     C* IN QUESTO CASO LO FORZO = 1
156200120411     C***                IF        VABRMN = *zeros
156300120411     C***                Z-ADD     1             VABRMN
156400120411     C***                ENDIF
156500040510     C*
156600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156700040510     C                   IF        VABRMA = *blanks
156800040714     C                   MOVEL     VABRMN        VABRMA
156900040510     C                   ENDIF
157000040128     C*
157100040510     C* Se provincia nn valorizzata la reperisco
157200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
157300110912     C                   IF        VABPRD =  *blanks AND
157400110912     C                             VABNZD =  *blanks AND
157500110912     C                             VABCAD <> *blanks AND
157600110912     C                             VABLOD <> *blanks
157700040128     C                   CLEAR                   TISI95DS
157800040128     C                   EVAL      I95TCN = '3'
157900040128     C                   Z-ADD     datcor        I95DAT
158000040128     C                   EVAL      I95NAR = VABNZD
158100040128     C                   EVAL      I95CAP = VABCAD
158200040128     C                   EVAL      I95LOC = VABLOD
158300040128     C                   CALL      'TISI95R'
158400040128     C                   PARM                    TISI95DS
158500040128     C                   EVAL      VABPRD = O95PRV
158600040510     C                   ENDIF
158700020204     C*
158800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158900020204     C                   EXSR      CHKIMPDIV
159000030715     C                   ENDIF
159100020204     C*
159200010607     C                   ENDSR
159300010607     C*----------------------------------------------------*
159400010601
159500020204
159600020204     C*----------------------------------------------------*
159700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159800020204     C*----------------------------------------------------*
159900020204     C     CHKIMPDIV     BEGSR
160000020204     C*
160100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
160200020204     C                   Z-ADD     *zeros        wrkDec            9 9
160300020204     C*
160400020204     C* Come prima cosa effettuo considerazioni sulla divisa
160500020204     C                   IF        vabIAS > *zeros
160600020204     C                   IF        vabVAS <> 'EUR'
160700020204     C                   EVAL      vabVAS =  'ITL'
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C                   IF        vabCAS > *zeros
161200020204     C                   IF        vabVCA <> 'EUR'
161300020204     C                   EVAL      vabVCA =  'ITL'
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C                   IF        vabVMD > *zeros
161800020204     C                   IF        vabVAD <> 'EUR'
161900020204     C                   EVAL      vabVAD =  'ITL'
162000020204     C                   ENDIF
162100020204     C                   ENDIF
162200020204     C*
162300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
162400020204     C                   Z-ADD     vabIAS        wrkDec
162500020204     C                   IF        wrkDec > *zeros
162600020204     C                   IF        vabVAS = 'ITL'
162700020204     C                   EVAL      vabIAS = *zeros
162800020204     C                   ENDIF
162900020204     C                   ENDIF
163000020204     C*
163100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
163200020204     C                   Z-ADD     vabCAS        wrkDec
163300020204     C                   IF        wrkDec > *zeros
163400020204     C                   IF        vabVCA = 'ITL'
163500020204     C                   EVAL      vabCAS = *zeros
163600020204     C                   ENDIF
163700020204     C                   ENDIF
163800020204     C*
163900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
164000020204     C                   Z-ADD     vabVMD        wrkDec
164100020204     C                   IF        wrkDec > *zeros
164200020204     C                   IF        vabVAD = 'ITL'
164300020204     C                   EVAL      vabVMD = *zeros
164400020204     C                   ENDIF
164500020204     C                   ENDIF
164600020204     C*
164700020204     C                   ENDSR
164800020204     C***
164900010330
165000100318
165100100318
165200100318     C*----------------------------------------------------*
165300100318     C*  CONTROLLO NUMERICITA' CAMPI
165400100318     C*----------------------------------------------------*
165500100318     C     CHKNUM        BEGSR
165600100318     C*
165700100318     C                   IF        PiDecChr = *blanks
165800100318     C                   EVAL      PiDecChr = CharNUM
165900100318     C                   ENDIF
166000100318     C*
166100100318     C                   callp     UBISNUM_Check(PiStr
166200100318     C                                          :PiDecChr
166300100318     C                                          :PiVal
166400100318     C                                          :PiNum
166500100318     C                                          :PiInt)
166600100318     C*
166700100318     C                   ENDSR
166800100318     C***
166900010601
167000010601
167100010601
167200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
167300141006      ***
167400141014     C     repfil        BEGSR
167500141014     C*
167600141014     C                   if        invfil = *zeros and
167700141014     C                             depfil > *zeros and
167800141014     C                             (vinflg = *blanks or
167900141014     C                              vinflg = *zeros)
168000141014     C*
168100141014     C                   eval      invfil = depfil
168200141014     C                   endif
168300141014     C*
168400141014     C                   if        depfil <> invfil and
168500141014     C                             invfil > *zeros
168600141014     C                   eval      flgMulti = '1'
168700141014     C                   if        vinflg = *blanks
168800141014     C                   add       1             cntNonEl
168900141014     C                   endif
169000141014     C                   endif
169100141014     C*
169200141014     C                   if        vinflg = '2'
169300141014     C                   eval      flgStato = '2'
169400141014     C                   endif
169500141014     C*
169600141014     C                   ENDSR
169700141014     C*
169800010601
169900010601
170000010601
170100010330
170200040119      /TITLE Invio dei dati al punto operativo.
170300040119     C     invio         BEGSR
170400040119     C*
170500040119     C* 1° invio FIVAT
170600040119     C                   reset                   dscmz
170700040119     C                   move      vlrpoi        cmzdst
170800040119     C                   eval      cmzfld = 'FIVATWWR'
170900040119     C                   eval      cmzmbd = vlrhdl
171000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171100040119     C***                if        prmfir = *blanks
171200040119     C                   eval      cmzfla = 'FIVAT00F'
171300040119     C                   eval      cmzmba = 'FIVAT00F'
171400040119     C***                else
171500040119     C***                eval      cmzfla = prmfir
171600040119     C***                eval      cmzmba = prmfir
171700040119     C***                endif
171800040119     C                   eval      cmznrr = *zeros
171900040119     C                   move      §ctrokvt      cmznrr
172000040119     C                   eval      cmzlba = vlrfl1
172100040119     C                   call(e)   'TIS711C'
172200040119     C                   parm                    dscmz
172300040119     C                   parm      *blanks       esito
172400040119     C                   if        %error
172500040119     C                             or cmzerr = '1'
172600040119     C                             or esito  = '1'
172700040119     C                   eval      wrkesito = '3'
172800040119     C                   else
172900040119     C*
173000040119     C* 2° invio FIVAB
173100040119     C                   reset                   dscmz
173200040119     C                   move      vlrpoi        cmzdst
173300040119     C                   eval      cmzfld = vlrfou
173400040119     C                   eval      cmzmbd = vlrhdl
173500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173600040119     C***                if        prmfir = *blanks
173700040119     C                   eval      cmzfla = 'FIVAB00F'
173800040119     C                   eval      cmzmba = 'FIVAB00F'
173900040119     C***                else
174000040119     C***                eval      cmzfla = prmfir
174100040119     C***                eval      cmzmba = prmfir
174200040119     C***                endif
174300040119     C                   eval      cmznrr = *zeros
174400040119     C                   move      §ctrokvb      cmznrr
174500040119     C                   eval      cmzlba = vlrfl1
174600040119     C                   call(e)   'TIS711C'
174700040119     C                   parm                    dscmz
174800040119     C                   parm      *blanks       esito
174900040119     C                   if        %error
175000040119     C                             or cmzerr = '1'
175100040119     C                             or esito  = '1'
175200040119     C                   eval      wrkesito = '3'
175300040119     C                   endif
175400040119     C                   endif
175500040119     C*
175600040119     C                   ENDSR
175700040119     C***
175800010601
175900010601
176000010601
176100010601
176200010601      /TITLE Invio dei dati al punto operativo.
176300010601     C     opeini        BEGSR
176400010601     C*
176500010601     C* Inizializzo flag e contatori operativi
176600010601     C                   movel     '0'           flgGiro           1
176700010601     C                   movel     '0'           flgMulti          1
176800010601     C                   movel     '1'           flgStato          1
176900010615     C                   movel     '0'           flgOk             1
177000010601     C                   z-add     *zeros        cntNonEl         10 0
177100010601     C                   z-add     *zeros        depfil            3 0
177200010601     C                   z-add     *zeros        invfil            3 0
177300010601     C*
177400010601     C                   ENDSR
177500010601     C***
177600070326
177700070326
177800070326
177900070326
178000070326     C     *pssr         BEGSR
178100070329     C*
178200070329     C                   if        %open(tivin00r)
178300070329     C                   close     tivin00r
178400070329     C                   endif
178500070329     C                   if        %open(fivabwwr)
178600070329     C                   close     fivabwwr
178700070329     C                   endif
178800070329     C                   if        %open(fivatwwr)
178900070329     C                   close     fivatwwr
179000070329     C                   endif
179100070326     C*
179200070326     C* Effettuo la chiamata al CLLE preposto
179300070326     C                   call(e)   'TITVVTC'
179400070326     C                   parm                    parccm
179500070326     C                   parm                    parmbr
179600070326     C                   parm      '2'           paropz
179700070326     C*
179800070326     C                   eval      wrkesito = '2'
179900070404     C*
180000070404     C                   seton                                        LR
180100070326     C*
180200070326     C                   ENDSR     '*CANCL'
180300070326     C***
180400100318
180500100318
180600100318
180700100318     C     normCSV       BEGSR
180800100318     C*
180900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
181000100318     C* separatori campi
181100100318     C                   if        CharTXT <> *blanks
181200100318     C                   z-add     *zeros        wLenVINDTA        4 0
181300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
181400100318     C                   setoff                                       80
181500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181800100318     C                   if        not *in80
181900100318     C                   seton                                        80
182000100318     C                   else
182100100318     C                   setoff                                       80
182200100318     C                   endif
182300100318     C                   endif
182400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182500100318     C                             and *in80 = *on
182600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182700100318     C                   endif
182800100318     C                   enddo
182900100318     C                   endif
183000100318     C*
183100100318     C                   ENDSR
183200100318     C***
183300070326
183400100326
183500100326
183600100326     C*----------------------------------------------------*
183700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183800100326     C*----------------------------------------------------*
183900100326     C     REPNSP        BEGSR
184000100326     C*
184100100326     C* NSP => Stacco un numeratore da AZNUM
184200100326     C                   clear                   TRUL33DS
184300100326     C                   eval      I33OPE = *zeros
184400100326     C                   eval      I33CNU = 302
184500100326     C                   eval      I33NUM = 1
184600100326     C                   movel     TRUL33DS      KPJBU
184700100326     C                   call      'TRUL33R'
184800100326     C                   parm                    KPJBA
184900100326     C                   movel     KPJBU         TRUL33DS
185000100326     C                   if        O33ERR = *zeros
185100100326     C                   z-add     O33NRF        VABNSP
185200100326     C                   z-add     O33NRF        VATNSP
185300100326     C                   else
185400100326     C                   SETON                                        31
185500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185600140711     C                             + ' ' + 'VABNSP VATNSP'
185700100326     C                   endif
185800100326     C*
185900100326     C                   ENDSR
186000010330
186100100326
186200010330
186300000613     C     *inzsr        BEGSR
186400990910     C*
186500990910     C     *entry        plist
186600990920     C                   parm                    tivlrds
186700990921     C                   parm      wrkesito      esito
186800000724     C                   parm                    prmlit
186900000710     C                   parm                    prmfir
187000010330     C*
187100010330     C* CALCOLA LA DATA CORRENTE
187200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
187300100318     C                   eval      datcor = %dec(%date() : *ISO)
187400000613     C*
187500000613     C                   ENDSR
187600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187700170918VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;W_VABRMA;VATNOT_E;VABPKB;VABVLB;VABCAS;VABNOT;
187800170927VATNOT_B;W_VATNOT_I;X;X;
187900150428
188000150428
188100150428
