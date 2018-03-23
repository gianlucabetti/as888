000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400150922      * - siccome il cliente non garantisce di inviarci un file ordinato per NSP, la colonna A
000500150922      * è RMN e RMA e su RMN si fa la rottura, mentre per NSP stacco un numeratore.
000600150922      * Così non ci sono problemi in conferma, al massimo avrò più bolle per RMN.
000700150922      * Quindi DEPSPE conterrà VABRMN.
000800150922      * - Spostata la chiamata alla routine REPNSP perché ne staccava 1 ogni riga del file (assumeva
000900150922      * che la spedizione fosse monocollo).
001000150922      * - a rottura di RMN si sommano NCL e PKB
001100150114      *
001200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001300130917     H decedit('0,')
001400120209     F*
001500990910     Ftivin00r  uF   E             DISK    usropn
001600100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001700040119     FFIVATwwr  O    E             DISK    usropn
001800000313     D*
001900040113     D*------------
002000040113     D* SCHIEDE A PROGRAMMA
002100040113     D*------------
002200080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002300141003     D*------------
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
004400150922     D depspe          s                   LIKE(VABRMN) INZ(*zeros)
004500080418     D wFlgCAS         s              1    INZ(*blanks)
004600120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004700120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004800150114     D Intestazioni    s               n   inz(*on)
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
020900150922     C*
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
025000141014     C                   eval      depfil = VABLNP
025100141014     C                   exsr      repfil
025200141014     C                   if        depfil = invfil
025300141014     C                   if        vlrpoi = 999
025400141014     C                   move(P)   invfil        VABFGS
025500141014     C                   else
025600141014     C                   move(P)   vlrpoi        VABFGS
025700141014     C                   endif
025800010601     C*
025900010601     C                   exsr      PREELA
026000010601     C*
026100010604     C* Ebbene...
026200010604     C*
026300040119     C  N31              add       1             §CTROKVB
026400040119     C   32              add       1             §CTRMO
026500040119     C   31              add       1             §CTRNO
026600080418     C*
026700030822     C                   if        wGiro = 2
026800080513     C* Scrivo sempre il VAT
026900080513     C  N31              exsr      wrivat
027000110201     C                   exsr      chkvab
027100080513     C                   endif
027200010604     C*
027300010604     C                   if        *in31 = *off and
027400010604     C                             *in32 = *off
027500010604     C                   eval      vinflg = '1'
027600010604     C                   else
027700010604     C                   eval      vinflg = '2'
027800010604     C                   endif
027900141014     C                   endif
028000010601     C*
028100010604     C                   endif
028200010604     C*
028300010330     C                   else
028400010330     C                   eval      vinflg = '1'
028500010330     C                   endif
028600010601     C*
028700010601     C  N70              update    tivin000
028800010330     C*
028900030822     C  N70              enddo
029000040119     C*
029100080527     C* Verifico dati rimasti "in cannna"...
029200080527     C                   seton                                        65
029300110201     C                   exsr      chkvab
029400080527     C*
029500040119     C                   endif                                                  (endif - chkcall)
029600010601     C*
029700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
029800010601     C                   if        cntNonEl = *zeros or
029900010601     C                             flgMulti = '0'
030000010330     C* Se non ci sono record con errori ...
030100010601     C                   if        §ctrno = 0 and
030200010604     C                             §ctrmo = 0 and
030300010601     C                             flgStato <> '2'
030400010330     C* ... restituisco esito OK.
030500010330     C                   eval      wrkesito = '0'
030600010330     C                   else
030700040119     C                   if        §ctrokvb > 0
030800010330     C                   eval      wrkesito = '1'
030900010330     C                   else
031000010615     C                   if        flgOk = '0'
031100010615     C                   eval      wrkesito = '2'
031200010615     C                   else
031300010615     C                   eval      wrkesito = '6'
031400010615     C                   endif
031500010330     C                   endif
031600010330     C                   endif
031700010601     C                   else
031800010601     C                   eval      wrkesito = '9'
031900010601     C                   endif
032000010330     C*
032100010330     C                   if        %open(tivin00r)
032200010330     C                   close     tivin00r
032300010330     C                   endif
032400021025     C                   if        %open(fivabwwr)
032500021025     C                   close     fivabwwr
032600010330     C                   endif
032700040512     C                   if        %open(fivatwwr)
032800040512     C                   close     fivatwwr
032900040512     C                   endif
033000010601     C*
033100010601     C                   if        vlrpoi <> 999
033200010601     C                   eval      invfil = vlrpoi
033300010601     C                   endif
033400010330     C*
033500040119     C                   if        §ctrokvb > 0
033600010601     C                             and invfil > *zeros
033700010330     C                   exsr      invio
033800010330     C                   endif
033900010601     C*
034000010618     C                   if        flgGiro = '1'
034100010601     C                   exsr      endela
034200010618     C                   endif
034300010330     C*
034400010330     C                   ENDSR
034500010330     C***
034600080527
034700080527
034800080527
034900080527     C*----------------------------------------------------*
035000080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
035100080527     C*----------------------------------------------------*
035200100413     C     CHKVAB        BEGSR
035300110201     C*
035400110201     C* Se richiesti ignorare colli / peso / volume
035500110315 xxx C***                if        *in35
035600110315 xxx C***                z-add     *zeros        VABNCL
035700110315 xxx C***                z-add     *zeros        VABPKB
035800110315 xxx C***                z-add     *zeros        VABVLB
035900110315 xxx C***                endif
036000080527     C*
036100080527     C* Scrivo il VAB solo a rottura d spedizione
036200080527     C                   if        depspe = *zeros
036300080527     C  N31              eval      fivabds_sav = fivabds
036400150922     C                   eval      depspe = vabRMN
036500080527     C                   else
036600150922     C                   if        depspe = vabRMN AND *in65 = *off
036700100413     C   51              add       VABNCL        sav_VABNCL
036800100413     C   51              add       VABPKB        sav_VABPKB
036900100413     C   51              add       VABVLB        sav_VABVLB
037000100720     C   52              z-add     VABNCL        sav_VABNCL
037100100720     C   52              z-add     VABPKB        sav_VABPKB
037200100720     C   52              z-add     VABVLB        sav_VABVLB
037300100720     C   53              add       VABNCL        sav_VABNCL
037400100720     C   53              z-add     VABPKB        sav_VABPKB
037500100720     C   53              z-add     VABVLB        sav_VABVLB
037600150922     C                   else
037700080527     C  N31              eval      fivabds_ok = fivabds_sav
037800080527     C  N31              exsr      wrivab
037900080527     C  N31              eval      fivabds_sav = fivabds
038000150922     C                   eval      depspe = vabRMN
038100080527     C                   endif
038200080527     C                   endif
038300080527     C*
038400080527     C                   ENDSR
038500040119
038600040119
038700040119
038800040119     C*----------------------------------------------------*
038900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
039000040119     C*----------------------------------------------------*
039100040510     C     WRIVAB        BEGSR
039200080513     C*
039300080513     C* Gestisco l'eventuale rottura x numero spedizione
039400150922     C                   if        VABRMN <> depspe or *in65 = *on
039500070301     C                   if        wVATNOT_E <> *blanks
039600130610     C                   eval      ok_VABCTM = '7Q'
039700070301     C                   endif
039800100413     C                   write     fivab000
039900080513     C                   endif
040000040119     C*
040100040119     C                   ENDSR
040200040119
040300040119
040400040119
040500040119     C*----------------------------------------------------*
040600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040700040119     C*----------------------------------------------------*
040800040510     C     WRIVAT        BEGSR
040900060512     C*
041000060512     C* Inizializzo i campi chiave
041100060512     C                   eval      VATFGS = VABFGS
041200060512     C                   eval      VATAAS = VABAAS
041300060512     C                   eval      VATCCM = VABCCM
041400070823     C                   eval      VATNRS = VABNRS
041500060512     C                   eval      VATNSP = VABNSP
041600060512     C                   eval      VATLNP = VABLNP
041700040119     C*
041800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
041900040510     C                   if        VATTRC = *blanks
042000040510     C                   eval      VATTRC = 'E'
042100040510     C                   endif
042200110318     C                   if        VATNOT <> *blanks
042300040510     C                   write     FIVAT000
042400040119     C                   add       1             §CTROKVT
042500110318     C                   endif
042600060331     C*
042700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042800060331     C                   if        wVATNOT_A <> *blanks
042900060331     C                   eval      VATNOT = wVATNOT_A
043000060331     C                   eval      VATTRC = 'A'
043100060331     C                   write     FIVAT000
043200060331     C                   add       1             §CTROKVT
043300060331     C                   endif
043400060331     C*
043500060331     C                   if        wVATNOT_B <> *blanks
043600060331     C                   eval      VATNOT = wVATNOT_B
043700060331     C                   eval      VATTRC = 'B'
043800060331     C                   write     FIVAT000
043900060331     C                   add       1             §CTROKVT
044000060331     C                   endif
044100140711     C*
044200140711     C                   if        wVATNOT_IJ <> *blanks
044300140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044400140711     C                   eval      VATTRC = 'I'
044500140711     C                   write     FIVAT000
044600140711     C                   add       1             §CTROKVT
044700140711     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
044800140711     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044900140711     C                   eval      VATTRC = 'J'
045000140711     C                   write     FIVAT000
045100140711     C                   endif
045200140711     C                   endif
045300060331     C*
045400060331     C                   if        wVATNOT_E <> *blanks
045500060331     C                   eval      VATTRC = 'E'
045600140527     C*
045700150309     C* NON Sviluppo i "CHI SONO"
045800140527     C*
045900150309     C                   eval      VATNOT = wVATNOT_E
046000150114     C                   write     FIVAT000
046100150114     C                   add       1             §CTROKVT
046200120320     C                   endif
046300110927     C*
046400110927     C                   if        wVATNOT_I <> *blanks
046500110927     C                   eval      VATNOT = wVATNOT_I
046600140711     C                   eval      VATTRC = 'I'
046700110927     C                   write     FIVAT000
046800110927     C                   add       1             §CTROKVT
046900110927     C                   endif
047000140220     C*
047100140220     C                   if        wVATNOT_J <> *blanks
047200140220     C                   eval      VATNOT = wVATNOT_J
047300140220     C                   eval      VATTRC = 'J'
047400140220     C                   write     FIVAT000
047500140220     C                   add       1             §CTROKVT
047600140220     C                   endif
047700140319     C*
047800140319     C                   if        wVATNOT_S <> *blanks
047900140319     C                   eval      VATNOT = wVATNOT_S
048000140319     C* i 2 flag devono essere blank
048100140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
048200140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
048300140319     C                   eval      VATTRC = 'S'
048400140319     C                   write     FIVAT000
048500140319     C                   add       1             §CTROKVT
048600140319     C                   endif
048700040119     C*
048800040119     C                   ENDSR
048900040119
049000040119
049100040119
049200040119     C*----------------------------------------------------*
049300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
049400040119     C*----------------------------------------------------*
049500040119     C     PREVAT        BEGSR
049600040119     C*
049700040119     C* Compongo il nome del membro da dare al FIVATWWR
049800040119     C                   eval      parmbr = vlrhdl
049900040119     C                   movel     'M'           parmbr
050000040510     C                   eval      parccm = vlrksc
050100040119     C                   eval      paropz = '1'
050200040119     C* Effettuo la chiamata al CLLE preposto
050300040119     C                   call(e)   'TITVVTC'
050400040119     C                   parm                    parccm
050500040119     C                   parm                    parmbr
050600040119     C                   parm                    paropz
050700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050800040119     C                   if        %error
050900040119     C                   movel     '1'           chkcall
051000040119     C                   else
051100040119     C                   movel     '0'           chkcall
051200040119     C                   endif
051300040119     C*
051400040119     C                   ENDSR
051500010601
051600010601
051700010601
051800010330     C*----------------------------------------------------*
051900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
052000010330     C*----------------------------------------------------*
052100040119     C     INZVAR        BEGSR
052200080527     C*
052300080527     C                   setoff                                       65
052400010330     C*
052500030822     C* Inizializzo variabili di wrk
052600020204     C                   Z-ADD     *zeros        Num5_0            5 0
052700101018     C                   MOVEL     *blanks       wFlgCAS
052800060331     C                   MOVEL     *blanks       wVATNOT_A
052900060331     C                   MOVEL     *blanks       wVATNOT_B
053000060331     C                   MOVEL     *blanks       wVATNOT_E
053100110927     C                   MOVEL     *blanks       wVATNOT_I
053200140220     C                   MOVEL     *blanks       wVATNOT_J
053300140220     C                   MOVEL     *blanks       wVATNOT_IJ
053400140319     C                   MOVEL     *blanks       wVATNOT_S
053500120411     C                   MOVEL     *blanks       wNOT
053600120411     C                   MOVEL     *blanks       Data8A
053700030822     C*
053800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053900030822     C                   CLEAR                   SkSplitCSV
054000030822     C                   EVAL      i = 1
054100030822     C                   EVAL      posDa = *zeros
054200030822     C                   EVAL      posA  = *zeros
054300080527     C*
054400080527     C                   CLEAR                   FIVABDS
054500080527     C                   CLEAR                   FIVAT000
054600030822     C*
054700030822     C* Reimposto i valori di default
054800030822     C                   EXSR      DEFCAM
054900030822     C*
055000010330     C                   ENDSR
055100010330     C*----------------------------------------------------*
055200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
055300010330     C*----------------------------------------------------*
055400010330     C     DEFCAM        BEGSR
055500030822     C*
055600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055700030715     C* e delimitatore testo.
055800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
056000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
056100100318     C*
056200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
056300100318     C                   EVAL      CharSOS = CharNUM
056400030822     C*
056500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
056600100720     C                   SETOFF                                       505152
056700100720     C                   SETOFF                                       53
056800100413     C                   SELECT
056900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
057000100720     C                   SETON                                        50
057100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
057200100413     C                   SETON                                        51
057300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
057400100720     C                   SETON                                        52
057500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
057600100720     C                   SETON                                        53
057700100413     C                   ENDSL
057800030822     C*
057900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
058000030822     C                   EVAL      posDaDft = 1
058100030822     C                   EVAL      posADft  = 0
058200030822     C                   EVAL      wGiroDft = 0
058300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
058400030822     C                             posDaDft > 0
058500030822     C*
058600030822     C* Gestisco il 1° giro
058700030822     C                   IF        wGiroDft = 0
058800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
058900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
059000030822     C* Incremento il contatore dei "giri"
059100030822     C                   EVAL      wGiroDft = 1
059200030822     C                   ELSE
059300030822     C                   EVAL      posDaDft = posADft
059400030822     C                   ENDIF
059500030822     C* Eseguo lo scan x trovare la fine del campo corrente
059600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
059700030822     C*
059800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
059900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
060000030822     C                   IF        posDaDft > 0 AND
060100030822     C                             posADft  > 0
060200030822     C* NCL
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NCL'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNCL
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CCM
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CCM'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCCM
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* LNP
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'LNP'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiInt=*on
063800030822     C                   Z-ADD     PiVal         VABLNP
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* NRS
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'NRS'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiInt=*on
065100030822     C                   Z-ADD     PiVal         VABNRS
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* CTR
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'CTR'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiInt=*on
066400030822     C                   Z-ADD     PiVal         VABCTR
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* PKB
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'PKB'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700130610     C                   Z-ADD     PiVal         VABPKB
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* VLB
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VLB'
068500030822     C                   EVAL      PiStr=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   EXSR      CHKNUM
068900030822     C                   IF        PiNum=*on
069000130610     C                   Z-ADD     PiVal         VABVLB
069100030822     C                   ENDIF
069200030822     C                   ENDIF
069300030822     C* QFT
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'QFT'
069800030822     C                   EVAL      PiStr=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   EXSR      CHKNUM
070200030822     C                   IF        PiNum=*on
070300030822     C                   Z-ADD     PiVal         VABQFT
070400030822     C                   ENDIF
070500030822     C                   ENDIF
070600030822     C* CBO
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'CBO'
071100030822     C                   EVAL      VABCBO=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* TSP
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'TSP'
072000030822     C                   EVAL      VABTSP=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* VAS
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'VAS'
072900030822     C                   EVAL      VABVAS=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* VCA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'VCA'
073800030822     C                   EVAL      VABVCA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200070925     C* TIC
074300070925     C                   IF        %subst(
074400070925     C                             %subst(vlrppt:posDaDft+1:
074500070925     C                             posADft-posDaDft-1):1:3)
074600070925     C                             = 'TIC'
074700070925     C                   EVAL      VABTIC=%trim(%subst(
074800070925     C                             %subst(vlrppt:posDaDft+1:
074900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000070925     C                   ENDIF
075100030822     C* GCA
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'GCA'
075600030822     C                   EVAL      VABGCA=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* CTM
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'CTM'
076500030822     C                   EVAL      VABCTM=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900030822     C* FFD
077000030822     C                   IF        %subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):1:3)
077300030822     C                             = 'FFD'
077400030822     C                   EVAL      VABFFD=%trim(%subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700030822     C                   ENDIF
077800030822     C* VAD
077900030822     C                   IF        %subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):1:3)
078200030822     C                             = 'VAD'
078300030822     C                   EVAL      VABVAD=%trim(%subst(
078400030822     C                             %subst(vlrppt:posDaDft+1:
078500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078600030822     C                   ENDIF
078700030822     C* GMA
078800030822     C                   IF        %subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):1:3)
079100030822     C                             = 'GMA'
079200030822     C                   EVAL      VABGMA=%trim(%subst(
079300030822     C                             %subst(vlrppt:posDaDft+1:
079400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079500030822     C                   ENDIF
079600030822     C* GGA
079700030822     C                   IF        %subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):1:3)
080000030822     C                             = 'GGA'
080100030822     C                   EVAL      VABGGA=%trim(%subst(
080200030822     C                             %subst(vlrppt:posDaDft+1:
080300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080400030822     C                   ENDIF
080500030822     C* GVA
080600030822     C                   IF        %subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):1:3)
080900030822     C                             = 'GVA'
081000030822     C                   EVAL      VABGVA=%trim(%subst(
081100030822     C                             %subst(vlrppt:posDaDft+1:
081200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081300030822     C                   ENDIF
081400030822     C* TC1
081500030822     C                   IF        %subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):1:3)
081800030822     C                             = 'TC1'
081900030822     C                   EVAL      VABTC1=%trim(%subst(
082000030822     C                             %subst(vlrppt:posDaDft+1:
082100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082200030822     C                   ENDIF
082300030822     C* TC2
082400030822     C                   IF        %subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):1:3)
082700030822     C                             = 'TC2'
082800030822     C                   EVAL      VABTC2=%trim(%subst(
082900030822     C                             %subst(vlrppt:posDaDft+1:
083000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083100030822     C                   ENDIF
083200040714     C* VATTRC
083300040714     C                   IF        %subst(
083400040714     C                             %subst(vlrppt:posDaDft+1:
083500040714     C                             posADft-posDaDft-1):1:3)
083600040714     C                             = 'TRC'
083700040714     C                   EVAL      VATTRC=%trim(%subst(
083800040714     C                             %subst(vlrppt:posDaDft+1:
083900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084000040714     C                   ENDIF
084100030822     C* ...
084200030822     C                   ENDIF
084300030822     C                   ENDDO
084400020204     C*
084500010330     C                   ENDSR
084600010607     C*----------------------------------------------------*
084700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
084800010607     C*----------------------------------------------------*
084900010607     C     IMPVAB        BEGSR
085000010607     C*
085100010607     C                   SETOFF                                       3132
085200110315 xxx C***                SETON                                        35
085300030515     C*
085400010607     C                   MOVEL     datcor        VABAAS
085500010607     C                   MOVE      datcor        VABMGS
085600010607     C*
085700030715     C**********
085800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085900030715     C**********
086000030715     C                   IF        wGiro = *zeros
086100040113     C*
086200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
086400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
086500080429     C                             %trim(intcol(5))<>*blanks
086600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
086700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
086800080429     C                                      %trim(intcol(5))
086900070823     C     ';':CharCSV   XLATE     vindta        vindta
087000040510     C                   ENDIF
087100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087200031201     C     minu:maiu     XLATE     vindta        vindta
087300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087400030715     C                   DOW       posDa <= %len(%trim(vindta))
087500030715     C*
087600030715     C* Gestisco il 1° campo
087700030715     C                   IF        i = 1
087800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087900030715     C                   EVAL      posDa = 1
088000030715     C* Eseguo lo scan x trovare la fine del primo campo
088100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088200030715     C* A questo punto "estraggo" il campo corrente
088300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088400030715     C                                                    (posA-posDa))
088500030715     C* X i campi successivi al 1°
088600030715     C                   ELSE
088700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088800030715     C                   EVAL      posDa = posA + 1
088900030715     C* Eseguo lo scan x trovare la fine del campo corrente
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* Gestisco l'ultimo campo
089200030715     C                   IF        posA = *zeros
089300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C                   LEAVE
089800030715     C                   ELSE
089900030715     C* A questo punto "estraggo" il campo corrente
090000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
090100030715     C                                                    (posA-posDa))
090200030715     C                   ENDIF
090300030715     C                   ENDIF
090400030715     C* Incremento il contatore d campo
090500030715     C                   EVAL      i = i +1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090900030715     C* delimitazione testo
091000030715     C                   EVAL      i = 1
091100030715     C                   DOW       i < %elem(SkSplitFLD)
091200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
091300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
091400030715     C                   EVAL      i = i + 1
091500030715     C                   ENDDO
091600030715     C                   EVAL      wGiro = 1
091700030715     C                   ELSE
091800030715     C                   EVAL      wGiro = 2
091900031204     C**********
092000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092100031204     C* d separatore campo
092200031204     C**********
092300031204     C                   EVAL      vindta = %trim(vindta)
092400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092500031204     C                   EVAL      vindta = %subst(vindta:2)
092600031204     C                   ENDDO
092700031204     C*
092800031204     C                   Z-ADD     *zeros        lunghInput        4 0
092900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093300110912     C                   ENDDO
093400030715     C**********
093500030715     C* Effettuo lo split del campo dati d input
093600030715     C**********
093700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093800030715     C                   DOW       posDa <= %len(%trim(vindta))
093900030715     C*
094000030715     C* Gestisco il 1° campo
094100030715     C                   IF        i = 1
094200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094300030715     C                   EVAL      posDa = 1
094400030715     C* Eseguo lo scan x trovare la fine del primo campo
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* A questo punto "estraggo" il campo corrente
094700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094800030715     C                                                    (posA-posDa))
094900030715     C* X i campi successivi al 1°
095000030715     C                   ELSE
095100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095200030715     C                   EVAL      posDa = posA + 1
095300031204     C* Verifico che nn vi sia il campo nn valorizzato
095400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095500031204     C* Se campo nn valorizzato skippo al prossimo
095600031204     C                   EVAL      SkSplitCSV(i) = *blanks
095700031204     C                   EVAL      posA  = posA + 1
095800031204     C                   ELSE
095900030715     C* Eseguo lo scan x trovare la fine del campo corrente
096000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096100030715     C* Gestisco l'ultimo campo
096200030715     C                   IF        posA = *zeros
096300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096400030715     C* A questo punto "estraggo" il campo corrente
096500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096600030715     C                                                    (posA-posDa))
096700030715     C                   LEAVE
096800030715     C                   ELSE
096900030715     C* A questo punto "estraggo" il campo corrente
097000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097100030715     C                                                    (posA-posDa))
097200030715     C                   ENDIF
097300031204     C                   ENDIF
097400030715     C                   ENDIF
097500030715     C* Incremento il contatore d campo
097600030715     C                   EVAL      i = i +1
097700030715     C                   ENDDO
097800030715     C*
097900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098000030715     C* delimitazione testo
098100030715     C                   EVAL      i = 1
098200030715     C                   DOW       i < %elem(SkSplitCSV)
098300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098500030715     C                   EVAL      i = i + 1
098600030715     C                   ENDDO
098700100720     C*
098800100720     C* Stacco un progressivo x numero spedizione
098900150922 xxx C***                EXSR      repNSP
099000100326     C*
099100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099200030715     C                   EVAL      i = 1
099300030715     C                   DOW       i < %elem(SkSplitFLD)
099400030715     C*
099500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
099700030715     C                             SkSplitCSV(i) <> *blanks
099800030721     C*** ATB
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
100000030721     C                   EVAL      VABATB = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030715     C*** RSD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
100400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
100500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
100600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
100700030715     C* ==
100800130528     C                   ENDIF
100900030721     C*** RD2
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
101100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030715     C*** IND
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
101500030715     C                   EVAL      VABIND = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** LOD
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
101900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** PRD
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
102300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030721     C*** NZD
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
102700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030715     C*** RMA
103000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
103100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
103200030715     C                   ENDIF
103300030715     C*** NOT
103400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
103500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103600030715     C                   ENDIF
103700030715     C*** NT2
103800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
104000030715     C                   ENDIF
104100030715     C*** VCA
104200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
104300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
104400030715     C                   ENDIF
104500030715     C*** VAS
104600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104800030715     C                   ENDIF
104900030715     C*** FFD
105000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
105100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
105200030715     C                   ENDIF
105300030721     C*** GC1
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
105500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GC2
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TSP
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
106300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
106400030721     C                   ENDIF
106500040510     C*** CBO
106600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106800040510     C                   ENDIF
106900030721     C*** NAS
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
107100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TIC
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
107500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GCA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** XCO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
108300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** CTM
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TCR
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
109100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** CTS
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
109500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** FTM
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** VAD
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
110300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** GMA
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*** GGA
111000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
111100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
111200030721     C                   ENDIF
111300030721     C*** GVA
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
111500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111600030721     C                   ENDIF
111700030721     C*** TC1
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
112000030721     C                   ENDIF
112100030721     C*** TC2
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
112300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
112400030721     C                   ENDIF
112500030721     C*** SCL
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112800030721     C                   ENDIF
112900030721     C*** RMO
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
113100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
113200030721     C                   ENDIF
113300030721     C*** NMO
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
113500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113600030721     C                   ENDIF
113700030721     C*
113800030721     C* Reperisco quindi i campi numerici...
113900040510     C*** NSP
114000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
114100040510     C                   EVAL      PiStr=SkSplitCSV(i)
114200040510     C                   EXSR      CHKNUM
114300040510     C                   IF        PiInt=*on
114400040510     C                   Z-ADD     PiVal         VABNSP
114500040510     C                   ELSE
114600040510     C                   SETON                                        31
114700040510     C                   EVAL      VABNSP = *zeros
114800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114900040510     C                             + ' ' + 'VABNSP'
115000040510     C                   ENDIF
115100040510     C                   ENDIF
115200071106     C*** RMN
115300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
115400071106     C                   EVAL      PiStr=SkSplitCSV(i)
115500071106     C                   EXSR      CHKNUM
115600071106     C                   IF        PiInt=*on
115700071106     C                   Z-ADD     PiVal         VABRMN
115800071106     C                   ELSE
115900071106     C                   SETON                                        32
116000071106     C                   EVAL      VABRMN = *zeros
116100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
116200071106     C                             + ' ' + 'VABRMN'
116300071106     C                   ENDIF
116400071106     C                   ENDIF
116500030721     C*** CAD
116600130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116700130610     C                   EVAL      PiStr=SkSplitCSV(i)
116800130610     C                   EXSR      CHKNUM
116900130610     C                   IF        PiInt=*on
117000130610     C                   Z-ADD     PiVal         Num5_0
117100130610     C                   MOVEL(p)  Num5_0        VABCAD
117200130610     C                   ELSE
117300130610     C                   SETON                                        32
117400130610     C                   EVAL      VABCAD = *zeros
117500130610     C                   EVAL      vinmsg = %trimr(vinmsg)
117600130610     C                             + ' ' + 'VABCAD'
117700130610     C                   ENDIF
117800130605     C                   ENDIF
117900030721     C*** NCL
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on
118400030721     C                   Z-ADD     PiVal         VABNCL
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABNCL = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040122     C                             + ' ' + 'VABNCL'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** PKB
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700130610     C                   Z-ADD(H)  PiVal         VABPKB
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABPKB = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABPKB'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** CAS
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120700110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120800111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120900140711     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
121000140711     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
121100140711     C                             %trim(SkSplitCSV(i)) <> ',00'
121200130703     C                   EVAL      VABVCA = 'EUR'
121300140930     C                   EVAL      VABTIC = *blank
121400140513     C                   EVAL      wFlgCAS = '1'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiNum=*on
121800030721     C                   Z-ADD     PiVal         VABCAS
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      VABCAS = *zeros
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCAS'
122400030721     C                   ENDIF
122500140513     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** CCM
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on      AND
123200030721     C                             PiVal<=9999999 AND
123300030721     C                             PiVal>*zeros
123400030721     C                   Z-ADD     PiVal         VABCCM
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABCCM = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900040510     C                             + ' ' + 'VABCCM'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** LNP
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on    AND
124700030721     C                             PiVal<=999   AND
124800030721     C                             PiVal>*zeros
124900030721     C                   Z-ADD     PiVal         VABLNP
125000030721     C                   ELSE
125100040715     C                   SETON                                        32
125200030721     C                   EVAL      VABLNP = *zeros
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400040510     C                             + ' ' + 'VABLNP'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** NRS
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on AND
126200030721     C                             PiVal<=99
126300030721     C                   Z-ADD     PiVal         VABNRS
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABNRS'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** LNA
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on    AND
127500030721     C                             PiVal<=999   AND
127600030721     C                             PiVal>*zeros
127700030721     C                   Z-ADD     PiVal         VABLNA
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      VABLNA = *zeros
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABLNA'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** CTR
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on  AND
129000030721     C                             PiVal<=999
129100030721     C                   Z-ADD     PiVal         VABCTR
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABCTR'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** IAS
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
130000141003     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
130100141003     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
130200141003     C                             %trim(SkSplitCSV(i)) <> '0000000000,000' AND
130300141003     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
130400141003     C                             %trim(SkSplitCSV(i)) <> ',00'
130500141006     C                   EVAL      VABVAS = 'EUR'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiNum=*on
130900030721     C                   Z-ADD     PiVal         VABIAS
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABIAS'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600141003     C                   ENDIF
131700030721     C*** VLB
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiNum=*on
132200130610     C                   Z-ADD(H)  PiVal         VABVLB
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABVLB'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** QFT
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiNum=*on
133400030721     C                   Z-ADD     PiVal         VABQFT
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABQFT'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** NCD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on      AND
134600030721     C                             PiVal<=9999999
134700030721     C                   Z-ADD     PiVal         VABNCD
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABNCD'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** NCA
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on      AND
135900030721     C                             PiVal<=9999999
136000030721     C                   Z-ADD     PiVal         VABNCA
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABNCA'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** ZNC
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on AND
137200030721     C                             PiVal<=99
137300030721     C                   Z-ADD     PiVal         VABZNC
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABZNC'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** DCR
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on       AND
138500030721     C                             PiVal<=99999999
138600030721     C                   Z-ADD     PiVal         VABDCR
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABDCR'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** HCR
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on   AND
139800030721     C                             PiVal<=9999
139900030721     C                   Z-ADD     PiVal         VABHCR
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABHCR'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** VMD
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiNum=*on
141100030721     C                   Z-ADD     PiVal         VABVMD
141200030721     C                   ELSE
141300030721     C                   SETON                                        32
141400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141500030721     C                             + ' ' + 'VABVMD'
141600030721     C                   ENDIF
141700030721     C                   ENDIF
141800030721     C*** ANT
141900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
142000030721     C                   EVAL      PiStr=SkSplitCSV(i)
142100030721     C                   EXSR      CHKNUM
142200030721     C                   IF        PiInt=*on        AND
142300030721     C                             PiVal<=999999999
142400030721     C                   Z-ADD     PiVal         VABANT
142500030721     C                   ELSE
142600030721     C                   SETON                                        32
142700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142800030721     C                             + ' ' + 'VABANT'
142900030721     C                   ENDIF
143000030721     C                   ENDIF
143100030721     C*** CMO
143200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
143300030721     C                   EVAL      PiStr=SkSplitCSV(i)
143400030721     C                   EXSR      CHKNUM
143500030721     C                   IF        PiInt=*on
143600030721     C                   Z-ADD     PiVal         Num5_0
143700030721     C                   MOVEL(p)  Num5_0        VABCMO
143800030721     C                   ELSE
143900030721     C                   SETON                                        32
144000030721     C                   EVAL      VABCMO = *zeros
144100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
144200030721     C                             + ' ' + 'VABCMO'
144300030721     C                   ENDIF
144400030721     C                   ENDIF
144500040510     C*
144600040510     C* Reperisco i campi del FIVAT
144700040510     C*** VATNOT
144800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
144900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
145000040510     C                   ENDIF
145100040510     C*** VATTRC
145200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
145300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
145400040510     C                   ENDIF
145500060331     C*** VATNOT_A
145600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
145700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
145800060331     C                   ENDIF
145900060331     C*** VATNOT_B
146000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
146100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
146200060331     C                   ENDIF
146300060331     C*** VATNOT_E
146400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
146500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146600060331     C                   ENDIF
146700110927     C*** VATNOT_I
146800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
146900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
147000110927     C                   ENDIF
147100141003     C*** VATNOT_IJ
147200141003     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
147300141003     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147400141003     C                   ENDIF
147500140319     C*** VATNOT_S
147600140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147700140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
147800140319     C                   ENDIF
147900070823     C*
148000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
148100150309     C*
148200150922     C***     / RMN / RMA
148300150309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
148400150309     C                   EVAL      PiStr=SkSplitCSV(i)
148500150309     C                   EXSR      CHKNUM
148600150309     C                   IF        PiInt=*on
148700150922     C***                Z-ADD     PiVal         VABNSP
148800150309     C                   Z-ADD     PiVal         VABRMN
148900150922     C                   EVAL      VABRMA = %char(VABRMN)
149000180309     C* per ogni riga c'è sempre 1 collo
149100180309     C                   EVAL      VABNCL= 1
149200180309     C* per ogni riga scrivo sempre questa nota
149300180309     C                   EVAL      VABNT2=
149400180309     C                              'NEGOZIO OKAIDI'
149500150309     C                   ELSE
149600150309     C                   SETON                                        31
149700150922     C***                EVAL      VABNSP = *zeros
149800150309     C                   EVAL      VABRMN = *zeros
149900150309     C                   EVAL      vinmsg = %trimr(vinmsg)
150000150922     C                             + ' ' + 'VABRMA VABRMN'
150100150309     C                   ENDIF
150200150309     C                   ENDIF
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
153000150922     C* la prima volta
153100150922     C                   if        depspe = 0
153200150922     C* oppure a rottura spedizione
153300150922     C                             or depspe <> vabRMN
153400150922     C* Stacco un progressivo x numero spedizione
153500150922 xxx C                   EXSR      repNSP
153600150922     C*
153700150922     C* durante la stessa spedizione
153800150922     C                   ELSE
153900150922     C* reperisco il precedente numero spedizione
154000150922     C                   EVAL      VABNSP = sav_VABNSP
154100150922     C                   ENDIF
154200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
154300150922     C                   IF        VABNSP = *zeros
154400150922     C                   SETON                                        31
154500150922     C                   ENDIF
154600040715     C*
154700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154800040715     C                   IF        VABLNP = *zeros
154900040715     C                   SETON                                        31
155000040715     C                   ENDIF
155100040510     C*
155200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
155300120411     C                   IF        VABRMN = *zeros
155400120411     C                   Z-ADD     VABNSP        VABRMN
155500120411     C                   ENDIF
155600120328     C* IN QUESTO CASO LO FORZO = 1
155700120411     C***                IF        VABRMN = *zeros
155800120411     C***                Z-ADD     1             VABRMN
155900120411     C***                ENDIF
156000040510     C*
156100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
156200040510     C                   IF        VABRMA = *blanks
156300040714     C                   MOVEL     VABRMN        VABRMA
156400040510     C                   ENDIF
156500040128     C*
156600040510     C* Se provincia nn valorizzata la reperisco
156700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156800110912     C                   IF        VABPRD =  *blanks AND
156900110912     C                             VABNZD =  *blanks AND
157000110912     C                             VABCAD <> *blanks AND
157100110912     C                             VABLOD <> *blanks
157200040128     C                   CLEAR                   TISI95DS
157300040128     C                   EVAL      I95TCN = '3'
157400040128     C                   Z-ADD     datcor        I95DAT
157500040128     C                   EVAL      I95NAR = VABNZD
157600040128     C                   EVAL      I95CAP = VABCAD
157700040128     C                   EVAL      I95LOC = VABLOD
157800040128     C                   CALL      'TISI95R'
157900040128     C                   PARM                    TISI95DS
158000040128     C                   EVAL      VABPRD = O95PRV
158100040510     C                   ENDIF
158200020204     C*
158300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
158400020204     C                   EXSR      CHKIMPDIV
158500030715     C                   ENDIF
158600020204     C*
158700010607     C                   ENDSR
158800010607     C*----------------------------------------------------*
158900010601
159000020204
159100020204     C*----------------------------------------------------*
159200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
159300020204     C*----------------------------------------------------*
159400020204     C     CHKIMPDIV     BEGSR
159500020204     C*
159600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
159700020204     C                   Z-ADD     *zeros        wrkDec            9 9
159800020204     C*
159900020204     C* Come prima cosa effettuo considerazioni sulla divisa
160000020204     C                   IF        vabIAS > *zeros
160100020204     C                   IF        vabVAS <> 'EUR'
160200020204     C                   EVAL      vabVAS =  'ITL'
160300020204     C                   ENDIF
160400020204     C                   ENDIF
160500020204     C*
160600020204     C                   IF        vabCAS > *zeros
160700020204     C                   IF        vabVCA <> 'EUR'
160800020204     C                   EVAL      vabVCA =  'ITL'
160900020204     C                   ENDIF
161000020204     C                   ENDIF
161100020204     C*
161200020204     C                   IF        vabVMD > *zeros
161300020204     C                   IF        vabVAD <> 'EUR'
161400020204     C                   EVAL      vabVAD =  'ITL'
161500020204     C                   ENDIF
161600020204     C                   ENDIF
161700020204     C*
161800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
161900020204     C                   Z-ADD     vabIAS        wrkDec
162000020204     C                   IF        wrkDec > *zeros
162100020204     C                   IF        vabVAS = 'ITL'
162200020204     C                   EVAL      vabIAS = *zeros
162300020204     C                   ENDIF
162400020204     C                   ENDIF
162500020204     C*
162600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
162700020204     C                   Z-ADD     vabCAS        wrkDec
162800020204     C                   IF        wrkDec > *zeros
162900020204     C                   IF        vabVCA = 'ITL'
163000020204     C                   EVAL      vabCAS = *zeros
163100020204     C                   ENDIF
163200020204     C                   ENDIF
163300020204     C*
163400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
163500020204     C                   Z-ADD     vabVMD        wrkDec
163600020204     C                   IF        wrkDec > *zeros
163700020204     C                   IF        vabVAD = 'ITL'
163800020204     C                   EVAL      vabVMD = *zeros
163900020204     C                   ENDIF
164000020204     C                   ENDIF
164100020204     C*
164200020204     C                   ENDSR
164300020204     C***
164400010330
164500100318
164600100318
164700100318     C*----------------------------------------------------*
164800100318     C*  CONTROLLO NUMERICITA' CAMPI
164900100318     C*----------------------------------------------------*
165000100318     C     CHKNUM        BEGSR
165100100318     C*
165200100318     C                   IF        PiDecChr = *blanks
165300100318     C                   EVAL      PiDecChr = CharNUM
165400100318     C                   ENDIF
165500100318     C*
165600100318     C                   callp     UBISNUM_Check(PiStr
165700100318     C                                          :PiDecChr
165800100318     C                                          :PiVal
165900100318     C                                          :PiNum
166000100318     C                                          :PiInt)
166100100318     C*
166200100318     C                   ENDSR
166300100318     C***
166400010601
166500010601
166600010601
166700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
166800141006      ***
166900141014     C     repfil        BEGSR
167000141014     C*
167100141014     C                   if        invfil = *zeros and
167200141014     C                             depfil > *zeros and
167300141014     C                             (vinflg = *blanks or
167400141014     C                              vinflg = *zeros)
167500141014     C*
167600141014     C                   eval      invfil = depfil
167700141014     C                   endif
167800141014     C*
167900141014     C                   if        depfil <> invfil and
168000141014     C                             invfil > *zeros
168100141014     C                   eval      flgMulti = '1'
168200141014     C                   if        vinflg = *blanks
168300141014     C                   add       1             cntNonEl
168400141014     C                   endif
168500141014     C                   endif
168600141014     C*
168700141014     C                   if        vinflg = '2'
168800141014     C                   eval      flgStato = '2'
168900141014     C                   endif
169000141014     C*
169100141014     C                   ENDSR
169200141014     C*
169300010601
169400010601
169500010601
169600010330
169700040119      /TITLE Invio dei dati al punto operativo.
169800040119     C     invio         BEGSR
169900040119     C*
170000040119     C* 1° invio FIVAT
170100040119     C                   reset                   dscmz
170200040119     C                   move      vlrpoi        cmzdst
170300040119     C                   eval      cmzfld = 'FIVATWWR'
170400040119     C                   eval      cmzmbd = vlrhdl
170500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
170600040119     C***                if        prmfir = *blanks
170700040119     C                   eval      cmzfla = 'FIVAT00F'
170800040119     C                   eval      cmzmba = 'FIVAT00F'
170900040119     C***                else
171000040119     C***                eval      cmzfla = prmfir
171100040119     C***                eval      cmzmba = prmfir
171200040119     C***                endif
171300040119     C                   eval      cmznrr = *zeros
171400040119     C                   move      §ctrokvt      cmznrr
171500040119     C                   eval      cmzlba = vlrfl1
171600040119     C                   call(e)   'TIS711C'
171700040119     C                   parm                    dscmz
171800040119     C                   parm      *blanks       esito
171900040119     C                   if        %error
172000040119     C                             or cmzerr = '1'
172100040119     C                             or esito  = '1'
172200040119     C                   eval      wrkesito = '3'
172300040119     C                   else
172400040119     C*
172500040119     C* 2° invio FIVAB
172600040119     C                   reset                   dscmz
172700040119     C                   move      vlrpoi        cmzdst
172800040119     C                   eval      cmzfld = vlrfou
172900040119     C                   eval      cmzmbd = vlrhdl
173000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
173100040119     C***                if        prmfir = *blanks
173200040119     C                   eval      cmzfla = 'FIVAB00F'
173300040119     C                   eval      cmzmba = 'FIVAB00F'
173400040119     C***                else
173500040119     C***                eval      cmzfla = prmfir
173600040119     C***                eval      cmzmba = prmfir
173700040119     C***                endif
173800040119     C                   eval      cmznrr = *zeros
173900040119     C                   move      §ctrokvb      cmznrr
174000040119     C                   eval      cmzlba = vlrfl1
174100040119     C                   call(e)   'TIS711C'
174200040119     C                   parm                    dscmz
174300040119     C                   parm      *blanks       esito
174400040119     C                   if        %error
174500040119     C                             or cmzerr = '1'
174600040119     C                             or esito  = '1'
174700040119     C                   eval      wrkesito = '3'
174800040119     C                   endif
174900040119     C                   endif
175000040119     C*
175100040119     C                   ENDSR
175200040119     C***
175300010601
175400010601
175500010601
175600010601
175700010601      /TITLE Invio dei dati al punto operativo.
175800010601     C     opeini        BEGSR
175900010601     C*
176000010601     C* Inizializzo flag e contatori operativi
176100010601     C                   movel     '0'           flgGiro           1
176200010601     C                   movel     '0'           flgMulti          1
176300010601     C                   movel     '1'           flgStato          1
176400010615     C                   movel     '0'           flgOk             1
176500010601     C                   z-add     *zeros        cntNonEl         10 0
176600010601     C                   z-add     *zeros        depfil            3 0
176700010601     C                   z-add     *zeros        invfil            3 0
176800010601     C*
176900010601     C                   ENDSR
177000010601     C***
177100070326
177200070326
177300070326
177400070326
177500070326     C     *pssr         BEGSR
177600070329     C*
177700070329     C                   if        %open(tivin00r)
177800070329     C                   close     tivin00r
177900070329     C                   endif
178000070329     C                   if        %open(fivabwwr)
178100070329     C                   close     fivabwwr
178200070329     C                   endif
178300070329     C                   if        %open(fivatwwr)
178400070329     C                   close     fivatwwr
178500070329     C                   endif
178600070326     C*
178700070326     C* Effettuo la chiamata al CLLE preposto
178800070326     C                   call(e)   'TITVVTC'
178900070326     C                   parm                    parccm
179000070326     C                   parm                    parmbr
179100070326     C                   parm      '2'           paropz
179200070326     C*
179300070326     C                   eval      wrkesito = '2'
179400070404     C*
179500070404     C                   seton                                        LR
179600070326     C*
179700070326     C                   ENDSR     '*CANCL'
179800070326     C***
179900100318
180000100318
180100100318
180200100318     C     normCSV       BEGSR
180300100318     C*
180400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
180500100318     C* separatori campi
180600100318     C                   if        CharTXT <> *blanks
180700100318     C                   z-add     *zeros        wLenVINDTA        4 0
180800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
180900100318     C                   setoff                                       80
181000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
181100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
181200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
181300100318     C                   if        not *in80
181400100318     C                   seton                                        80
181500100318     C                   else
181600100318     C                   setoff                                       80
181700100318     C                   endif
181800100318     C                   endif
181900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
182000100318     C                             and *in80 = *on
182100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
182200100318     C                   endif
182300100318     C                   enddo
182400100318     C                   endif
182500100318     C*
182600100318     C                   ENDSR
182700100318     C***
182800070326
182900100326
183000100326
183100100326     C*----------------------------------------------------*
183200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
183300100326     C*----------------------------------------------------*
183400100326     C     REPNSP        BEGSR
183500100326     C*
183600100326     C* NSP => Stacco un numeratore da AZNUM
183700100326     C                   clear                   TRUL33DS
183800100326     C                   eval      I33OPE = *zeros
183900100326     C                   eval      I33CNU = 302
184000100326     C                   eval      I33NUM = 1
184100100326     C                   movel     TRUL33DS      KPJBU
184200100326     C                   call      'TRUL33R'
184300100326     C                   parm                    KPJBA
184400100326     C                   movel     KPJBU         TRUL33DS
184500100326     C                   if        O33ERR = *zeros
184600100326     C                   z-add     O33NRF        VABNSP
184700100326     C                   z-add     O33NRF        VATNSP
184800100326     C                   else
184900100326     C                   SETON                                        31
185000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
185100140711     C                             + ' ' + 'VABNSP VATNSP'
185200100326     C                   endif
185300100326     C*
185400100326     C                   ENDSR
185500010330
185600100326
185700010330
185800000613     C     *inzsr        BEGSR
185900990910     C*
186000990910     C     *entry        plist
186100990920     C                   parm                    tivlrds
186200990921     C                   parm      wrkesito      esito
186300000724     C                   parm                    prmlit
186400000710     C                   parm                    prmfir
186500010330     C*
186600010330     C* CALCOLA LA DATA CORRENTE
186700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
186800100318     C                   eval      datcor = %dec(%date() : *ISO)
186900000613     C*
187000000613     C                   ENDSR
187100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
187200180309W_VABNSP;VABLOD;VABRSD;VABRD2;VABIND;VABCAD;VABPKB;VATNOT_E;X;VATNOT_B
187300150309
187400140512
187500111116
187600100318
