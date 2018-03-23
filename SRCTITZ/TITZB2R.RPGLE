000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130618      * Disk A
000500140220      * -VABRMN, prendere solo i caratteri numerici (ovvero considerare dal carattere 7 in poi)
000600140220      * -se VATNOT_B <> *blanks valorizzare fisso VATNOT_A = '.'
000700140220      * -se VABCAS > allora valorizzare VABTIC = 'BM'
000800140220      * -se VABTSP valorizzato a 'N' allora VABTSP = 'C' se valorizzato a 'D' allora VABTSP = 'H',
000900140220      *  se valorizzato a 'T' allora VABTSP = 'E'
001000140220      * -VABCBO, se "S" mettere 1 (porto franco)
001100140220      *          se "S" e valorizzato VABCAS, mettere 4 (porto franco + contrassegno)
001200140220      *          se "A" mettere 2 (porto assegnato)
001300140220      *          se "A" e valorizzato VABCAS, mettere 6 (porto assegnato + contrassegno)
001400140220      *
001500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001600130917     H decedit('0,')
001700120209     F*
001800990910     Ftivin00r  uF   E             DISK    usropn
001900100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
002000040119     FFIVATwwr  O    E             DISK    usropn
002100000313     D*
002200040113     D*------------
002300040113     D* SCHIEDE A PROGRAMMA
002400040113     D*------------
002500080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
002600010330     D*----------------------------------------------------
002700010330     D* DICHIARAZIOINE VARIABILI DI WRK
002800010330     D*----------------------------------------------------
002900010330     D dscmz         e ds                  inz
003000010330     D psds           sds
003100010330     D  procname         *PROC
003200010330     D tivlrds       e ds                  extname(tivlr00f)
003300040128     D tisi95ds      e ds
003400100413     D fivabds       e ds                  extname(fivab00f)
003500100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
003600100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
003700010330     D esito           s              1
003800010330     D prmlit          s             10
003900010330     D prmfir          s             10
004000010330     D wrkesito        s                   like(esito)
004100010330     D rrnum           s              6  0 INZ(*zeros)
004200040510     D parccm          s              8    INZ(*blanks)
004300040119     D parmbr          s             10    INZ(*blanks)
004400040119     D paropz          s              1    INZ(*blanks)
004500040119     D chkcall         s              1    INZ(*blanks)
004600080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
004700080418     D wFlgCAS         s              1    INZ(*blanks)
004800120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004900120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
005000140220     D Intestazioni    s               n   inz(*off)
005100100326     D*------------------
005200100326     D* DS REPERIMENTO NUMERATORE
005300100326     D*------------------
005400100326     D trul33ds      e ds                  inz
005500100326     D*------------------
005600100326     D* DS ARCHITETTURA
005700100326     D*------------------
005800100326     D kpjba         e ds                  inz
005900130611     D*------------------
006000130611     D TRUL28DSX     e ds                  inz
006100100326
006200030715     D*------------------
006300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
006400030715     D*------------------
006500030715     D SkSplitFLD      S             10    DIM(100)
006600030715     D SkSplitCSV      S            256    DIM(100)
006700030715     D CharCSV         S              1
006800030715     D CharTXT         S              1
006900030715     D CharNUM         S              1
007000100318     D CharSOS         S              1
007100130626     D posDa           S              4  0 INZ(*zeros)
007200130626     D posA            S              4  0 INZ(*zeros)
007300030715     D i               s              3  0 INZ(1)
007400030715     D wGiro           s              1  0 INZ(*zeros)
007500070823     D wVATNOT_A       s                   LIKE(VATNOT)
007600060331     D wVATNOT_B       s                   LIKE(VATNOT)
007700060331     D wVATNOT_E       s                   LIKE(VATNOT)
007800110927     D wVATNOT_I       s                   LIKE(VATNOT)
007900140220     D wVATNOT_J       s                   LIKE(VATNOT)
008000140220     D wVATNOT_IJ      s             70
008100120411     D wNOT            s             70
008200120411     D Data8A          s              8
008300030822     D*------------------
008400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008500030822     D*------------------
008600030822     D posDaDft        S              3  0 INZ(*zeros)
008700030822     D posADft         S              3  0 INZ(*zeros)
008800030822     D j               s              3  0 INZ(1)
008900030822     D wGiroDft        s              1  0 INZ(*zeros)
009000080418     D*------------------
009100080418     D* Costanti
009200080418     D*------------------
009300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009500100318
009600100318     D*------------------
009700100318     D* LINKING A DEFINIZIONI ESTERNE
009800100318     D*------------------
009900100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
010000100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
010100100318
010200010330
010300010330
010400010330
010500000913     C                   reset                   rrnum
010600990921     C                   reset                   esito
010700990921     C                   reset                   wrkesito
010800010601     C*
010900010601     C                   exsr      opeini
011000010605     C                   exsr      rwvab
011100010601     C*
011200010601     C                   seton                                        lr
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011800010601     C*--------------------------------------------------------
011900010601     C     PREELA        BEGSR
012000010601     C*
012100010601     C* SE OCCORRE SPEDIRE IN FILIALE
012200010601     C                   if        invfil <> *zeros and
012300010601     C                             flgGiro = '0'
012400010601     C*
012500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012600010601     C                   eval      flgGiro = '1'
012700010601     C*
012800010601     C                   endif
012900010601     C*
013000010601     C                   ENDSR
013100010601     C***
013200010601
013300010601
013400010601
013500010601
013600010601     C*--------------------------------------------------------
013700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013800010601     C*--------------------------------------------------------
013900010601     C     ENDELA        BEGSR
014000040119     C*
014100040119     C* Effettuo la chiamata al CLLE preposto
014200040119     C                   call(e)   'TITVVTC'
014300040510     C                   parm                    parccm
014400040119     C                   parm                    parmbr
014500040119     C                   parm      '2'           paropz
014600050201     C*
014700050201     C* Effettuo lancio TISI95 solo x chiusura
014800050201     C                   CLEAR                   TISI95DS
014900050201     C                   EVAL      I95TLA = 'C'
015000050201     C                   CALL      'TISI95R'
015100050201     C                   PARM                    TISI95DS
015200000616     C*
015300010601     C                   ENDSR
015400010601
015500010601
015600010330     C*--------------------------------------------------------
015700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015800010330     C*--------------------------------------------------------
015900010605     C     RWVAB         BEGSR
016000010330     C*
016100010330     C                   if        not %open(tivin00r)
016200010330     C                   open      tivin00r
016300010330     C                   endif
016400021025     C                   if        not %open(fivabwwr)
016500021025     C                   open      fivabwwr
016600010330     C                   endif
016700040119     C*
016800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016900040119     C                   exsr      prevat
017000040119     C*
017100040119     C                   if        chkcall = '0'
017200040119     C*
017300040119     C                   if        not %open(fivatwwr)
017400040119     C                   open      fivatwwr
017500040119     C                   endif
017600010330     C*
017700040119     C                   clear                   §CTROKVB          7 0
017800040119     C                   clear                   §CTROKVT          7 0
017900040119     C                   clear                   §CTRMO            7 0
018000040119     C                   clear                   §CTRNO            7 0
018100120410     C*
018200120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
018300120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018400120410     C                   IF        Intestazioni = *off
018500120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018600120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018700120410     C                   exsr      inzvar
018800120410     C                   exsr      impvab
018900120410     C                   ENDIF
019000010330     C*
019100030822     C                   do        *HIVAL
019200010330     C*
019300030822     C                   read      tivin00r                               70
019400010618     C*
019500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019600010618     C                   if        vinflg = '1'
019700010618     C                   eval      flgOk = '1'
019800010618     C                   endif
019900040510     C*
020000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
020100040510     C                   z-add     *zeros        wLenVINDTA        4 0
020200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
020300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020600040510     C                   z-add     1             wFlgVINDTA
020700040510     C                   leave
020800040510     C                   endif
020900040510     C                   enddo
021000010618     C*
021100040510     C                   if        vindta > *blanks AND
021200040510     C                             wFlgVINDTA = 1
021300010330     C                   add       1             rrnum
021400010330     C*
021500010601     C                   if        *in70 = *off and
021600010330     C                             (vinflg = *blanks
021700010330     C                              or vinflg = '0'
021800010330     C                              or vinflg = '2')
021900010330     C*
022000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022100010711     C                   if        vinflg = *blanks or vinflg = '0'
022200010711     C                   clear                   vinmsg
022300010711     C                   endif
022400130917     C*
022500130917     C* Aggiungo un byte a blanks all'inizio d ogni record
022600130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022700130917     C* evitare che vengano elisi i blank
022800140220     C***                eval      vindta  = '*' + %trim(vindta)
022900130917     C*
023000080527     C                   exsr      inzvar
023100100318     C                   exsr      normCSV
023200010605     C                   exsr      impvab
023300010601     C*
023400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023500010605     C                   eval      depfil = VABLNP
023600010601     C                   exsr      repfil
023700010601     C                   if        depfil = invfil
023800021025     C                   if        vlrpoi = 999
023900030822     C                   move(P)   invfil        VABFGS
024000021025     C                   else
024100030822     C                   move(P)   vlrpoi        VABFGS
024200021025     C                   endif
024300010601     C*
024400010601     C                   exsr      PREELA
024500010601     C*
024600010604     C* Ebbene...
024700010604     C*
024800040119     C  N31              add       1             §CTROKVB
024900040119     C   32              add       1             §CTRMO
025000040119     C   31              add       1             §CTRNO
025100080418     C*
025200030822     C                   if        wGiro = 2
025300080513     C* Scrivo sempre il VAT
025400080513     C  N31              exsr      wrivat
025500110201     C                   exsr      chkvab
025600080513     C                   endif
025700010604     C*
025800010604     C                   if        *in31 = *off and
025900010604     C                             *in32 = *off
026000010604     C                   eval      vinflg = '1'
026100010604     C                   else
026200010604     C                   eval      vinflg = '2'
026300010604     C                   endif
026400010604     C                   endif
026500010601     C*
026600010604     C                   endif
026700010604     C*
026800010330     C                   else
026900010330     C                   eval      vinflg = '1'
027000010330     C                   endif
027100010601     C*
027200010601     C  N70              update    tivin000
027300010330     C*
027400030822     C  N70              enddo
027500040119     C*
027600080527     C* Verifico dati rimasti "in cannna"...
027700080527     C                   seton                                        65
027800110201     C                   exsr      chkvab
027900080527     C*
028000040119     C                   endif                                                  (endif - chkcall)
028100010601     C*
028200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028300010601     C                   if        cntNonEl = *zeros or
028400010601     C                             flgMulti = '0'
028500010330     C* Se non ci sono record con errori ...
028600010601     C                   if        §ctrno = 0 and
028700010604     C                             §ctrmo = 0 and
028800010601     C                             flgStato <> '2'
028900010330     C* ... restituisco esito OK.
029000010330     C                   eval      wrkesito = '0'
029100010330     C                   else
029200040119     C                   if        §ctrokvb > 0
029300010330     C                   eval      wrkesito = '1'
029400010330     C                   else
029500010615     C                   if        flgOk = '0'
029600010615     C                   eval      wrkesito = '2'
029700010615     C                   else
029800010615     C                   eval      wrkesito = '6'
029900010615     C                   endif
030000010330     C                   endif
030100010330     C                   endif
030200010601     C                   else
030300010601     C                   eval      wrkesito = '9'
030400010601     C                   endif
030500010330     C*
030600010330     C                   if        %open(tivin00r)
030700010330     C                   close     tivin00r
030800010330     C                   endif
030900021025     C                   if        %open(fivabwwr)
031000021025     C                   close     fivabwwr
031100010330     C                   endif
031200040512     C                   if        %open(fivatwwr)
031300040512     C                   close     fivatwwr
031400040512     C                   endif
031500010601     C*
031600010601     C                   if        vlrpoi <> 999
031700010601     C                   eval      invfil = vlrpoi
031800010601     C                   endif
031900010330     C*
032000040119     C                   if        §ctrokvb > 0
032100010601     C                             and invfil > *zeros
032200010330     C                   exsr      invio
032300010330     C                   endif
032400010601     C*
032500010618     C                   if        flgGiro = '1'
032600010601     C                   exsr      endela
032700010618     C                   endif
032800010330     C*
032900010330     C                   ENDSR
033000010330     C***
033100080527
033200080527
033300080527
033400080527     C*----------------------------------------------------*
033500080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033600080527     C*----------------------------------------------------*
033700100413     C     CHKVAB        BEGSR
033800110201     C*
033900110201     C* Se richiesti ignorare colli / peso / volume
034000110315 xxx C***                if        *in35
034100110315 xxx C***                z-add     *zeros        VABNCL
034200110315 xxx C***                z-add     *zeros        VABPKB
034300110315 xxx C***                z-add     *zeros        VABVLB
034400110315 xxx C***                endif
034500080527     C*
034600080527     C* Scrivo il VAB solo a rottura d spedizione
034700080527     C                   if        depspe = *zeros
034800080527     C  N31              eval      fivabds_sav = fivabds
034900080527     C                   eval      depspe = vabNSP
035000080527     C                   else
035100080527     C                   if        depspe = vabNSP AND *in65 = *off
035200100413     C   51              add       VABNCL        sav_VABNCL
035300100413     C   51              add       VABPKB        sav_VABPKB
035400100413     C   51              add       VABVLB        sav_VABVLB
035500100720     C   52              z-add     VABNCL        sav_VABNCL
035600100720     C   52              z-add     VABPKB        sav_VABPKB
035700100720     C   52              z-add     VABVLB        sav_VABVLB
035800100720     C   53              add       VABNCL        sav_VABNCL
035900100720     C   53              z-add     VABPKB        sav_VABPKB
036000100720     C   53              z-add     VABVLB        sav_VABVLB
036100080527     C                   else
036200080527     C  N31              eval      fivabds_ok = fivabds_sav
036300080527     C  N31              exsr      wrivab
036400080527     C  N31              eval      fivabds_sav = fivabds
036500080527     C                   eval      depspe = vabNSP
036600080527     C                   endif
036700080527     C                   endif
036800080527     C*
036900080527     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037500040119     C*----------------------------------------------------*
037600040510     C     WRIVAB        BEGSR
037700080513     C*
037800080513     C* Gestisco l'eventuale rottura x numero spedizione
037900080527     C                   if        VABNSP <> depspe or *in65 = *on
038000070301     C                   if        wVATNOT_E <> *blanks
038100130610     C                   eval      ok_VABCTM = '7Q'
038200070301     C                   endif
038300100413     C                   write     fivab000
038400080513     C                   endif
038500040119     C*
038600040119     C                   ENDSR
038700040119
038800040119
038900040119
039000040119     C*----------------------------------------------------*
039100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039200040119     C*----------------------------------------------------*
039300040510     C     WRIVAT        BEGSR
039400060512     C*
039500060512     C* Inizializzo i campi chiave
039600060512     C                   eval      VATFGS = VABFGS
039700060512     C                   eval      VATAAS = VABAAS
039800060512     C                   eval      VATCCM = VABCCM
039900070823     C                   eval      VATNRS = VABNRS
040000060512     C                   eval      VATNSP = VABNSP
040100060512     C                   eval      VATLNP = VABLNP
040200040119     C*
040300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040400040510     C                   if        VATTRC = *blanks
040500040510     C                   eval      VATTRC = 'E'
040600040510     C                   endif
040700110318     C                   if        VATNOT <> *blanks
040800040510     C                   write     FIVAT000
040900040119     C                   add       1             §CTROKVT
041000110318     C                   endif
041100060331     C*
041200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041300060331     C                   if        wVATNOT_A <> *blanks
041400060331     C                   eval      VATNOT = wVATNOT_A
041500060331     C                   eval      VATTRC = 'A'
041600060331     C                   write     FIVAT000
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_B <> *blanks
042100060331     C                   eval      VATNOT = wVATNOT_B
042200060331     C                   eval      VATTRC = 'B'
042300060331     C                   write     FIVAT000
042400060331     C                   add       1             §CTROKVT
042500060331     C                   endif
042600060331     C*
042700060331     C                   if        wVATNOT_E <> *blanks
042800110201     C                   eval      VATNOT = wVATNOT_E
042900060331     C                   eval      VATTRC = 'E'
043000140220     C* è un disk A, per cui niente barcode
043100120320     C                   endif
043200110927     C*
043300110927     C                   if        wVATNOT_I <> *blanks
043400110927     C                   eval      VATNOT = wVATNOT_I
043500110927     C                   eval      VATTRC = 'I'
043600110927     C                   write     FIVAT000
043700110927     C                   add       1             §CTROKVT
043800110927     C                   endif
043900140220     C*
044000140220     C                   if        wVATNOT_J <> *blanks
044100140220     C                   eval      VATNOT = wVATNOT_J
044200140220     C                   eval      VATTRC = 'J'
044300140220     C                   write     FIVAT000
044400140220     C                   add       1             §CTROKVT
044500140220     C                   endif
044600040119     C*
044700040119     C                   ENDSR
044800040119
044900040119
045000040119
045100040119     C*----------------------------------------------------*
045200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045300040119     C*----------------------------------------------------*
045400040119     C     PREVAT        BEGSR
045500040119     C*
045600040119     C* Compongo il nome del membro da dare al FIVATWWR
045700040119     C                   eval      parmbr = vlrhdl
045800040119     C                   movel     'M'           parmbr
045900040510     C                   eval      parccm = vlrksc
046000040119     C                   eval      paropz = '1'
046100040119     C* Effettuo la chiamata al CLLE preposto
046200040119     C                   call(e)   'TITVVTC'
046300040119     C                   parm                    parccm
046400040119     C                   parm                    parmbr
046500040119     C                   parm                    paropz
046600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046700040119     C                   if        %error
046800040119     C                   movel     '1'           chkcall
046900040119     C                   else
047000040119     C                   movel     '0'           chkcall
047100040119     C                   endif
047200040119     C*
047300040119     C                   ENDSR
047400010601
047500010601
047600010601
047700010330     C*----------------------------------------------------*
047800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047900010330     C*----------------------------------------------------*
048000040119     C     INZVAR        BEGSR
048100080527     C*
048200080527     C                   setoff                                       65
048300010330     C*
048400030822     C* Inizializzo variabili di wrk
048500020204     C                   Z-ADD     *zeros        Num5_0            5 0
048600101018     C                   MOVEL     *blanks       wFlgCAS
048700060331     C                   MOVEL     *blanks       wVATNOT_A
048800060331     C                   MOVEL     *blanks       wVATNOT_B
048900060331     C                   MOVEL     *blanks       wVATNOT_E
049000110927     C                   MOVEL     *blanks       wVATNOT_I
049100140220     C                   MOVEL     *blanks       wVATNOT_J
049200140220     C                   MOVEL     *blanks       wVATNOT_IJ
049300120411     C                   MOVEL     *blanks       wNOT
049400120411     C                   MOVEL     *blanks       Data8A
049500030822     C*
049600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049700030822     C                   CLEAR                   SkSplitCSV
049800030822     C                   EVAL      i = 1
049900030822     C                   EVAL      posDa = *zeros
050000030822     C                   EVAL      posA  = *zeros
050100080527     C*
050200080527     C                   CLEAR                   FIVABDS
050300080527     C                   CLEAR                   FIVAT000
050400030822     C*
050500030822     C* Reimposto i valori di default
050600030822     C                   EXSR      DEFCAM
050700030822     C*
050800010330     C                   ENDSR
050900010330     C*----------------------------------------------------*
051000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051100010330     C*----------------------------------------------------*
051200010330     C     DEFCAM        BEGSR
051300030822     C*
051400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051500030715     C* e delimitatore testo.
051600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051900100318     C*
052000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052100100318     C                   EVAL      CharSOS = CharNUM
052200030822     C*
052300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052400100720     C                   SETOFF                                       505152
052500100720     C                   SETOFF                                       53
052600100413     C                   SELECT
052700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052800100720     C                   SETON                                        50
052900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053000100413     C                   SETON                                        51
053100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053200100720     C                   SETON                                        52
053300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053400100720     C                   SETON                                        53
053500100413     C                   ENDSL
053600030822     C*
053700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053800030822     C                   EVAL      posDaDft = 1
053900030822     C                   EVAL      posADft  = 0
054000030822     C                   EVAL      wGiroDft = 0
054100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054200030822     C                             posDaDft > 0
054300030822     C*
054400030822     C* Gestisco il 1° giro
054500030822     C                   IF        wGiroDft = 0
054600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054800030822     C* Incremento il contatore dei "giri"
054900030822     C                   EVAL      wGiroDft = 1
055000030822     C                   ELSE
055100030822     C                   EVAL      posDaDft = posADft
055200030822     C                   ENDIF
055300030822     C* Eseguo lo scan x trovare la fine del campo corrente
055400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055500030822     C*
055600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055800030822     C                   IF        posDaDft > 0 AND
055900030822     C                             posADft  > 0
056000030822     C* NCL
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'NCL'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABNCL
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* CCM
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'CCM'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABCCM
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* LNP
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'LNP'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABLNP
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* NRS
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'NRS'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABNRS
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* CTR
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'CTR'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiInt=*on
062200030822     C                   Z-ADD     PiVal         VABCTR
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* PKB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'PKB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500130610     C                   Z-ADD     PiVal         VABPKB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* VLB
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'VLB'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800130610     C                   Z-ADD     PiVal         VABVLB
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* QFT
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'QFT'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiNum=*on
066100030822     C                   Z-ADD     PiVal         VABQFT
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* CBO
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'CBO'
066900030822     C                   EVAL      VABCBO=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* TSP
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'TSP'
067800030822     C                   EVAL      VABTSP=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VAS
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VAS'
068700030822     C                   EVAL      VABVAS=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VCA'
069600030822     C                   EVAL      VABVCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000070925     C* TIC
070100070925     C                   IF        %subst(
070200070925     C                             %subst(vlrppt:posDaDft+1:
070300070925     C                             posADft-posDaDft-1):1:3)
070400070925     C                             = 'TIC'
070500070925     C                   EVAL      VABTIC=%trim(%subst(
070600070925     C                             %subst(vlrppt:posDaDft+1:
070700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800070925     C                   ENDIF
070900030822     C* GCA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'GCA'
071400030822     C                   EVAL      VABGCA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* CTM
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CTM'
072300030822     C                   EVAL      VABCTM=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* FFD
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'FFD'
073200030822     C                   EVAL      VABFFD=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAD
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAD'
074100030822     C                   EVAL      VABVAD=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GMA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GMA'
075000030822     C                   EVAL      VABGMA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GGA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GGA'
075900030822     C                   EVAL      VABGGA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GVA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GVA'
076800030822     C                   EVAL      VABGVA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* TC1
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'TC1'
077700030822     C                   EVAL      VABTC1=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* TC2
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'TC2'
078600030822     C                   EVAL      VABTC2=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000040714     C* VATTRC
079100040714     C                   IF        %subst(
079200040714     C                             %subst(vlrppt:posDaDft+1:
079300040714     C                             posADft-posDaDft-1):1:3)
079400040714     C                             = 'TRC'
079500040714     C                   EVAL      VATTRC=%trim(%subst(
079600040714     C                             %subst(vlrppt:posDaDft+1:
079700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800040714     C                   ENDIF
079900030822     C* ...
080000030822     C                   ENDIF
080100030822     C                   ENDDO
080200020204     C*
080300010330     C                   ENDSR
080400010607     C*----------------------------------------------------*
080500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080600010607     C*----------------------------------------------------*
080700010607     C     IMPVAB        BEGSR
080800010607     C*
080900010607     C                   SETOFF                                       3132
081000110315 xxx C***                SETON                                        35
081100030515     C*
081200010607     C                   MOVEL     datcor        VABAAS
081300010607     C                   MOVE      datcor        VABMGS
081400010607     C*
081500030715     C**********
081600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081700030715     C**********
081800030715     C                   IF        wGiro = *zeros
081900040113     C*
082000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082300080429     C                             %trim(intcol(5))<>*blanks
082400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082600080429     C                                      %trim(intcol(5))
082700070823     C     ';':CharCSV   XLATE     vindta        vindta
082800040510     C                   ENDIF
082900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083000031201     C     minu:maiu     XLATE     vindta        vindta
083100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083200030715     C                   DOW       posDa <= %len(%trim(vindta))
083300030715     C*
083400030715     C* Gestisco il 1° campo
083500030715     C                   IF        i = 1
083600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083700030715     C                   EVAL      posDa = 1
083800030715     C* Eseguo lo scan x trovare la fine del primo campo
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* A questo punto "estraggo" il campo corrente
084100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084200030715     C                                                    (posA-posDa))
084300030715     C* X i campi successivi al 1°
084400030715     C                   ELSE
084500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084600030715     C                   EVAL      posDa = posA + 1
084700030715     C* Eseguo lo scan x trovare la fine del campo corrente
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* Gestisco l'ultimo campo
085000030715     C                   IF        posA = *zeros
085100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C                   LEAVE
085600030715     C                   ELSE
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   ENDIF
086100030715     C                   ENDIF
086200030715     C* Incremento il contatore d campo
086300030715     C                   EVAL      i = i +1
086400030715     C                   ENDDO
086500030715     C*
086600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086700030715     C* delimitazione testo
086800030715     C                   EVAL      i = 1
086900030715     C                   DOW       i < %elem(SkSplitFLD)
087000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087200030715     C                   EVAL      i = i + 1
087300030715     C                   ENDDO
087400030715     C                   EVAL      wGiro = 1
087500030715     C                   ELSE
087600030715     C                   EVAL      wGiro = 2
087700031204     C**********
087800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087900031204     C* d separatore campo
088000031204     C**********
088100031204     C                   EVAL      vindta = %trim(vindta)
088200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088300031204     C                   EVAL      vindta = %subst(vindta:2)
088400031204     C                   ENDDO
088500031204     C*
088600031204     C                   Z-ADD     *zeros        lunghInput        4 0
088700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089100110912     C                   ENDDO
089200030715     C**********
089300030715     C* Effettuo lo split del campo dati d input
089400030715     C**********
089500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089600030715     C                   DOW       posDa <= %len(%trim(vindta))
089700030715     C*
089800030715     C* Gestisco il 1° campo
089900030715     C                   IF        i = 1
090000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090100030715     C                   EVAL      posDa = 1
090200030715     C* Eseguo lo scan x trovare la fine del primo campo
090300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090400030715     C* A questo punto "estraggo" il campo corrente
090500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090600030715     C                                                    (posA-posDa))
090700030715     C* X i campi successivi al 1°
090800030715     C                   ELSE
090900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091000030715     C                   EVAL      posDa = posA + 1
091100031204     C* Verifico che nn vi sia il campo nn valorizzato
091200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091300031204     C* Se campo nn valorizzato skippo al prossimo
091400031204     C                   EVAL      SkSplitCSV(i) = *blanks
091500031204     C                   EVAL      posA  = posA + 1
091600031204     C                   ELSE
091700030715     C* Eseguo lo scan x trovare la fine del campo corrente
091800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091900030715     C* Gestisco l'ultimo campo
092000030715     C                   IF        posA = *zeros
092100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092200030715     C* A questo punto "estraggo" il campo corrente
092300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092400030715     C                                                    (posA-posDa))
092500030715     C                   LEAVE
092600030715     C                   ELSE
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C                   ENDIF
093100031204     C                   ENDIF
093200030715     C                   ENDIF
093300030715     C* Incremento il contatore d campo
093400030715     C                   EVAL      i = i +1
093500030715     C                   ENDDO
093600030715     C*
093700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093800030715     C* delimitazione testo
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitCSV)
094100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094300030715     C                   EVAL      i = i + 1
094400030715     C                   ENDDO
094500100720     C*
094600100720     C* Stacco un progressivo x numero spedizione
094700130917 xxx C                   EXSR      repNSP
094800100326     C*
094900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095000030715     C                   EVAL      i = 1
095100030715     C                   DOW       i < %elem(SkSplitFLD)
095200030715     C*
095300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095500030715     C                             SkSplitCSV(i) <> *blanks
095600030721     C*** ATB
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095800030721     C                   EVAL      VABATB = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030715     C*** RSD
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096500030715     C* ==
096600130528     C                   ENDIF
096700030721     C*** RD2
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** IND
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097300030715     C                   EVAL      VABIND = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** LOD
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** PRD
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030721     C*** NZD
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030715     C*** RMA
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** NOT
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** NT2
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** VCA
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** VAS
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** FFD
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030721     C*** GC1
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** GC2
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** TSP
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102200030721     C                   ENDIF
102300040510     C*** CBO
102400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102600040510     C                   ENDIF
102700030721     C*** NAS
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TIC
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** GCA
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** XCO
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** CTM
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** TCR
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** CTS
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** FTM
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** VAD
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GMA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** GGA
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GVA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** TC1
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** TC2
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** SCL
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** RMO
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** NMO
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*
109600030721     C* Reperisco quindi i campi numerici...
109700040510     C*** NSP
109800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109900040510     C                   EVAL      PiStr=SkSplitCSV(i)
110000040510     C                   EXSR      CHKNUM
110100040510     C                   IF        PiInt=*on
110200040510     C                   Z-ADD     PiVal         VABNSP
110300040510     C                   ELSE
110400040510     C                   SETON                                        31
110500040510     C                   EVAL      VABNSP = *zeros
110600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110700040510     C                             + ' ' + 'VABNSP'
110800040510     C                   ENDIF
110900040510     C                   ENDIF
111000071106     C*** RMN
111100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111200071106     C                   EVAL      PiStr=SkSplitCSV(i)
111300071106     C                   EXSR      CHKNUM
111400071106     C                   IF        PiInt=*on
111500071106     C                   Z-ADD     PiVal         VABRMN
111600071106     C                   ELSE
111700071106     C                   SETON                                        32
111800071106     C                   EVAL      VABRMN = *zeros
111900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112000071106     C                             + ' ' + 'VABRMN'
112100071106     C                   ENDIF
112200071106     C                   ENDIF
112300030721     C*** CAD
112400130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112500130610     C                   EVAL      PiStr=SkSplitCSV(i)
112600130610     C                   EXSR      CHKNUM
112700130610     C                   IF        PiInt=*on
112800130610     C                   Z-ADD     PiVal         Num5_0
112900130610     C                   MOVEL(p)  Num5_0        VABCAD
113000130610     C                   ELSE
113100130610     C                   SETON                                        32
113200130610     C                   EVAL      VABCAD = *zeros
113300130610     C                   EVAL      vinmsg = %trimr(vinmsg)
113400130610     C                             + ' ' + 'VABCAD'
113500130610     C                   ENDIF
113600130605     C                   ENDIF
113700030721     C*** NCL
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiInt=*on
114200030721     C                   Z-ADD     PiVal         VABNCL
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABNCL = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700040122     C                             + ' ' + 'VABNCL'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** PKB
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiNum=*on
115500130610     C                   Z-ADD(H)  PiVal         VABPKB
115600030721     C                   ELSE
115700030721     C                   SETON                                        32
115800030721     C                   EVAL      VABPKB = *zeros
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000030721     C                             + ' ' + 'VABPKB'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** CAS
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116600111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116700111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
116800130703     C                   EVAL      VABVCA = 'EUR'
116900101018     C                   ENDIF
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABCAS
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCAS = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABCAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CCM
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on      AND
118600030721     C                             PiVal<=9999999 AND
118700030721     C                             PiVal>*zeros
118800030721     C                   Z-ADD     PiVal         VABCCM
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCCM = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABCCM'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** LNP
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on    AND
120100030721     C                             PiVal<=999   AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABLNP
120400030721     C                   ELSE
120500040715     C                   SETON                                        32
120600030721     C                   EVAL      VABLNP = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800040510     C                             + ' ' + 'VABLNP'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NRS
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on AND
121600030721     C                             PiVal<=99
121700030721     C                   Z-ADD     PiVal         VABNRS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNRS'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNA
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNA
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      VABLNA = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABLNA'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** CTR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on  AND
124400030721     C                             PiVal<=999
124500030721     C                   Z-ADD     PiVal         VABCTR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABCTR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** IAS
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABIAS
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABIAS'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VLB
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900130610     C                   Z-ADD(H)  PiVal         VABVLB
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVLB'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** QFT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABQFT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABQFT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NCD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on      AND
129300030721     C                             PiVal<=9999999
129400030721     C                   Z-ADD     PiVal         VABNCD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNCD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCA
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCA
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCA'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** ZNC
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on AND
131900030721     C                             PiVal<=99
132000030721     C                   Z-ADD     PiVal         VABZNC
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABZNC'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** DCR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on       AND
133200030721     C                             PiVal<=99999999
133300030721     C                   Z-ADD     PiVal         VABDCR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABDCR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** HCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on   AND
134500030721     C                             PiVal<=9999
134600030721     C                   Z-ADD     PiVal         VABHCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABHCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** VMD
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABVMD
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABVMD'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** ANT
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on        AND
137000030721     C                             PiVal<=999999999
137100030721     C                   Z-ADD     PiVal         VABANT
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABANT'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** CMO
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on
138300030721     C                   Z-ADD     PiVal         Num5_0
138400030721     C                   MOVEL(p)  Num5_0        VABCMO
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      VABCMO = *zeros
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABCMO'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200040510     C*
139300040510     C* Reperisco i campi del FIVAT
139400040510     C*** VATNOT
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800040510     C*** VATTRC
139900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140100040510     C                   ENDIF
140200060331     C*** VATNOT_A
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_B
140700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000060331     C*** VATNOT_E
141100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400110927     C*** VATNOT_I
141500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141700110927     C                   ENDIF
141800070823     C*
141900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142000120301
142100140220      *** W_VABRMN: considerare dal carattere 7 in poi
142200140220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142300140220     C***                EVAL      VABRMN=
142400140220     C***                           %dec(%trim(%subst(SkSplitCSV(i):7:250))
142500140220     C***                            :15:0)
142600140220     C                   EVAL      VABRMN=
142700140220     C                              %dec(%subst(%trim(SkSplitCSV(i)):7:
142800140220     C                               %len(%trim(SkSplitCSV(i)))-6)
142900140220     C                               :15:0)
143000130917     C                   ENDIF
143100140220
143200140220      *** W_VABNOT: è sia VABNOT che VABNT2
143300140220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
143400140220     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
143500140220     C                   EVAL      VABNOT =
143600140220     C                              %subst(wNOT:1:35)
143700140220     C                   EVAL      VABNT2 =
143800140220     C                              %subst(wNOT:36:35)
143900140220     C                   ENDIF
144000140220
144100140220      *** W_VATNOT_I: è sia VATNOT_I che VATNOT_J
144200140220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
144300140220     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
144400140220     C                   ENDIF
144500140220
144600140220      *** W_VABTSP: è sia VATNOT_I che VATNOT_J
144700140220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTSP'
144800140220     C                   SELECT
144900140220     C                   WHEN      %trim(SkSplitCSV(i)) = 'N'
145000140220     C                   EVAL      VABTSP='C'
145100140220     C                   WHEN      %trim(SkSplitCSV(i)) = 'D'
145200140220     C                   EVAL      VABTSP='H'
145300140220     C                   WHEN      %trim(SkSplitCSV(i)) = 'T'
145400140220     C                   EVAL      VABTSP='E'
145500140220     C                   ENDSL
145600140220     C                   ENDIF
145700130917
145800140220      *** W_VABCBO: può essere S (porto franco) o A (porto assegnato)
145900130917     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
146000130917     C                   SELECT
146100140220     C                   WHEN      %trim(SkSplitCSV(i)) = 'S'
146200140220     C                   EVAL      VABCBO='1'
146300140220     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
146400140220     C                   EVAL      VABCBO='2'
146500130917     C                   ENDSL
146600130917     C                   ENDIF
146700130626
146800070925     C*
146900071029     C***  <===  -----------------------------------------
147000040510     C*
147100030715     C                   ENDIF
147200030715     C                   EVAL      i = i + 1
147300030715     C                   ENDDO
147400070925     C*
147500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147600120411     C*
147700140220     C* VABRMA=VABRMN
147800140220     C                   EVAL      VABRMA = %trim(%char(VABRMN))
147900140220     C*
148000140220     C* se VATNOT_B <> ' ' allora VATNOT_A = '.'
148100140220     C                   IF        wVATNOT_B <> *blank
148200140220     C                   EVAL      wVATNOT_A = '.'
148300140220     C                   ENDIF
148400140220     C*
148500140220     C* email destinatario
148600140220     C                   IF        %trim(wVATNOT_IJ) <> *blanks
148700140220     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
148800140220     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
148900140220     C                   ENDIF
149000070925     C*
149100071029     C***  <===  -----------------------------------------
149200070925     C*
149300070925     C* Considerazioni finali su CBO/CAS
149400140220     C                   IF        wFlgCAS = '1'    OR
149500140220     C                             VABCAS  > *zeros
149600140220     C                   IF        VABCBO = '1'
149700140220     C                   EVAL      VABCBO = '4'
149800140220     C                   ENDIF
149900140220     C                   IF        VABCBO = '2'
150000140220     C                   EVAL      VABCBO = '6'
150100140220     C                   ENDIF
150200140220     C                   IF        VABTIC = *blanks
150300140220     C                   EVAL      VABTIC = 'BM'
150400140220     C                   ENDIF
150500140220     C                   ENDIF
150600070925     C***  <===  ----------------------------
150700040510     C*
150800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150900040510     C                   IF        VABNSP = *zeros
151000040510     C                   SETON                                        31
151100040510     C                   ENDIF
151200040715     C*
151300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
151400040715     C                   IF        VABLNP = *zeros
151500040715     C                   SETON                                        31
151600040715     C                   ENDIF
151700040510     C*
151800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151900120411     C                   IF        VABRMN = *zeros
152000120411     C                   Z-ADD     VABNSP        VABRMN
152100120411     C                   ENDIF
152200120328     C* IN QUESTO CASO LO FORZO = 1
152300120411     C***                IF        VABRMN = *zeros
152400120411     C***                Z-ADD     1             VABRMN
152500120411     C***                ENDIF
152600040510     C*
152700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152800040510     C                   IF        VABRMA = *blanks
152900040714     C                   MOVEL     VABRMN        VABRMA
153000040510     C                   ENDIF
153100040128     C*
153200040510     C* Se provincia nn valorizzata la reperisco
153300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153400110912     C                   IF        VABPRD =  *blanks AND
153500110912     C                             VABNZD =  *blanks AND
153600110912     C                             VABCAD <> *blanks AND
153700110912     C                             VABLOD <> *blanks
153800040128     C                   CLEAR                   TISI95DS
153900040128     C                   EVAL      I95TCN = '3'
154000040128     C                   Z-ADD     datcor        I95DAT
154100040128     C                   EVAL      I95NAR = VABNZD
154200040128     C                   EVAL      I95CAP = VABCAD
154300040128     C                   EVAL      I95LOC = VABLOD
154400040128     C                   CALL      'TISI95R'
154500040128     C                   PARM                    TISI95DS
154600040128     C                   EVAL      VABPRD = O95PRV
154700040510     C                   ENDIF
154800020204     C*
154900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
155000020204     C                   EXSR      CHKIMPDIV
155100030715     C                   ENDIF
155200020204     C*
155300010607     C                   ENDSR
155400010607     C*----------------------------------------------------*
155500010601
155600020204
155700020204     C*----------------------------------------------------*
155800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155900020204     C*----------------------------------------------------*
156000020204     C     CHKIMPDIV     BEGSR
156100020204     C*
156200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
156300020204     C                   Z-ADD     *zeros        wrkDec            9 9
156400020204     C*
156500020204     C* Come prima cosa effettuo considerazioni sulla divisa
156600020204     C                   IF        vabIAS > *zeros
156700020204     C                   IF        vabVAS <> 'EUR'
156800020204     C                   EVAL      vabVAS =  'ITL'
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   IF        vabCAS > *zeros
157300020204     C                   IF        vabVCA <> 'EUR'
157400020204     C                   EVAL      vabVCA =  'ITL'
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C                   IF        vabVMD > *zeros
157900020204     C                   IF        vabVAD <> 'EUR'
158000020204     C                   EVAL      vabVAD =  'ITL'
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
158500020204     C                   Z-ADD     vabIAS        wrkDec
158600020204     C                   IF        wrkDec > *zeros
158700020204     C                   IF        vabVAS = 'ITL'
158800020204     C                   EVAL      vabIAS = *zeros
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
159300020204     C                   Z-ADD     vabCAS        wrkDec
159400020204     C                   IF        wrkDec > *zeros
159500020204     C                   IF        vabVCA = 'ITL'
159600020204     C                   EVAL      vabCAS = *zeros
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
160100020204     C                   Z-ADD     vabVMD        wrkDec
160200020204     C                   IF        wrkDec > *zeros
160300020204     C                   IF        vabVAD = 'ITL'
160400020204     C                   EVAL      vabVMD = *zeros
160500020204     C                   ENDIF
160600020204     C                   ENDIF
160700020204     C*
160800020204     C                   ENDSR
160900020204     C***
161000010330
161100100318
161200100318
161300100318     C*----------------------------------------------------*
161400100318     C*  CONTROLLO NUMERICITA' CAMPI
161500100318     C*----------------------------------------------------*
161600100318     C     CHKNUM        BEGSR
161700100318     C*
161800100318     C                   IF        PiDecChr = *blanks
161900100318     C                   EVAL      PiDecChr = CharNUM
162000100318     C                   ENDIF
162100100318     C*
162200100318     C                   callp     UBISNUM_Check(PiStr
162300100318     C                                          :PiDecChr
162400100318     C                                          :PiVal
162500100318     C                                          :PiNum
162600100318     C                                          :PiInt)
162700100318     C*
162800100318     C                   ENDSR
162900100318     C***
163000010601
163100010601
163200010601
163300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
163400010601     C     repfil        BEGSR
163500010601     C*
163600010601     C                   if        invfil = *zeros and
163700010601     C                             depfil > *zeros and
163800010629     C                             (vinflg = *blanks or
163900010629     C                              vinflg = *zeros)
164000010601     C
164100010601     C                   eval      invfil = depfil
164200010601     C                   endif
164300010601     C*
164400010601     C                   if        depfil <> invfil and
164500010601     C                             invfil > *zeros
164600010601     C                   eval      flgMulti = '1'
164700010601     C                   if        vinflg = *blanks
164800010601     C                   add       1             cntNonEl
164900010601     C                   endif
165000010601     C                   endif
165100010601     C*
165200010601     C                   if        vinflg = '2'
165300010601     C                   eval      flgStato = '2'
165400010601     C                   endif
165500010601     C*
165600010601     C                   ENDSR
165700010601     C***
165800010601
165900010601
166000010601
166100010330
166200040119      /TITLE Invio dei dati al punto operativo.
166300040119     C     invio         BEGSR
166400040119     C*
166500040119     C* 1° invio FIVAT
166600040119     C                   reset                   dscmz
166700040119     C                   move      vlrpoi        cmzdst
166800040119     C                   eval      cmzfld = 'FIVATWWR'
166900040119     C                   eval      cmzmbd = vlrhdl
167000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167100040119     C***                if        prmfir = *blanks
167200040119     C                   eval      cmzfla = 'FIVAT00F'
167300040119     C                   eval      cmzmba = 'FIVAT00F'
167400040119     C***                else
167500040119     C***                eval      cmzfla = prmfir
167600040119     C***                eval      cmzmba = prmfir
167700040119     C***                endif
167800040119     C                   eval      cmznrr = *zeros
167900040119     C                   move      §ctrokvt      cmznrr
168000040119     C                   eval      cmzlba = vlrfl1
168100040119     C                   call(e)   'TIS711C'
168200040119     C                   parm                    dscmz
168300040119     C                   parm      *blanks       esito
168400040119     C                   if        %error
168500040119     C                             or cmzerr = '1'
168600040119     C                             or esito  = '1'
168700040119     C                   eval      wrkesito = '3'
168800040119     C                   else
168900040119     C*
169000040119     C* 2° invio FIVAB
169100040119     C                   reset                   dscmz
169200040119     C                   move      vlrpoi        cmzdst
169300040119     C                   eval      cmzfld = vlrfou
169400040119     C                   eval      cmzmbd = vlrhdl
169500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169600040119     C***                if        prmfir = *blanks
169700040119     C                   eval      cmzfla = 'FIVAB00F'
169800040119     C                   eval      cmzmba = 'FIVAB00F'
169900040119     C***                else
170000040119     C***                eval      cmzfla = prmfir
170100040119     C***                eval      cmzmba = prmfir
170200040119     C***                endif
170300040119     C                   eval      cmznrr = *zeros
170400040119     C                   move      §ctrokvb      cmznrr
170500040119     C                   eval      cmzlba = vlrfl1
170600040119     C                   call(e)   'TIS711C'
170700040119     C                   parm                    dscmz
170800040119     C                   parm      *blanks       esito
170900040119     C                   if        %error
171000040119     C                             or cmzerr = '1'
171100040119     C                             or esito  = '1'
171200040119     C                   eval      wrkesito = '3'
171300040119     C                   endif
171400040119     C                   endif
171500040119     C*
171600040119     C                   ENDSR
171700040119     C***
171800010601
171900010601
172000010601
172100010601
172200010601      /TITLE Invio dei dati al punto operativo.
172300010601     C     opeini        BEGSR
172400010601     C*
172500010601     C* Inizializzo flag e contatori operativi
172600010601     C                   movel     '0'           flgGiro           1
172700010601     C                   movel     '0'           flgMulti          1
172800010601     C                   movel     '1'           flgStato          1
172900010615     C                   movel     '0'           flgOk             1
173000010601     C                   z-add     *zeros        cntNonEl         10 0
173100010601     C                   z-add     *zeros        depfil            3 0
173200010601     C                   z-add     *zeros        invfil            3 0
173300010601     C*
173400010601     C                   ENDSR
173500010601     C***
173600070326
173700070326
173800070326
173900070326
174000070326     C     *pssr         BEGSR
174100070329     C*
174200070329     C                   if        %open(tivin00r)
174300070329     C                   close     tivin00r
174400070329     C                   endif
174500070329     C                   if        %open(fivabwwr)
174600070329     C                   close     fivabwwr
174700070329     C                   endif
174800070329     C                   if        %open(fivatwwr)
174900070329     C                   close     fivatwwr
175000070329     C                   endif
175100070326     C*
175200070326     C* Effettuo la chiamata al CLLE preposto
175300070326     C                   call(e)   'TITVVTC'
175400070326     C                   parm                    parccm
175500070326     C                   parm                    parmbr
175600070326     C                   parm      '2'           paropz
175700070326     C*
175800070326     C                   eval      wrkesito = '2'
175900070404     C*
176000070404     C                   seton                                        LR
176100070326     C*
176200070326     C                   ENDSR     '*CANCL'
176300070326     C***
176400100318
176500100318
176600100318
176700100318     C     normCSV       BEGSR
176800100318     C*
176900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
177000100318     C* separatori campi
177100100318     C                   if        CharTXT <> *blanks
177200100318     C                   z-add     *zeros        wLenVINDTA        4 0
177300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
177400100318     C                   setoff                                       80
177500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
177600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
177700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177800100318     C                   if        not *in80
177900100318     C                   seton                                        80
178000100318     C                   else
178100100318     C                   setoff                                       80
178200100318     C                   endif
178300100318     C                   endif
178400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
178500100318     C                             and *in80 = *on
178600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
178700100318     C                   endif
178800100318     C                   enddo
178900100318     C                   endif
179000100318     C*
179100100318     C                   ENDSR
179200100318     C***
179300070326
179400100326
179500100326
179600100326     C*----------------------------------------------------*
179700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
179800100326     C*----------------------------------------------------*
179900100326     C     REPNSP        BEGSR
180000100326     C*
180100100326     C* NSP => Stacco un numeratore da AZNUM
180200100326     C                   clear                   TRUL33DS
180300100326     C                   eval      I33OPE = *zeros
180400100326     C                   eval      I33CNU = 302
180500100326     C                   eval      I33NUM = 1
180600100326     C                   movel     TRUL33DS      KPJBU
180700100326     C                   call      'TRUL33R'
180800100326     C                   parm                    KPJBA
180900100326     C                   movel     KPJBU         TRUL33DS
181000100326     C                   if        O33ERR = *zeros
181100100326     C                   z-add     O33NRF        VABNSP
181200100326     C                   z-add     O33NRF        VATNSP
181300100326     C                   else
181400100326     C                   SETON                                        31
181500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
181600100326     C                             + ' ' + 'VABNSP VATNSP'
181700100326     C                   endif
181800100326     C*
181900100326     C                   ENDSR
182000010330
182100100326
182200010330
182300000613     C     *inzsr        BEGSR
182400990910     C*
182500990910     C     *entry        plist
182600990920     C                   parm                    tivlrds
182700990921     C                   parm      wrkesito      esito
182800000724     C                   parm                    prmlit
182900000710     C                   parm                    prmfir
183000010330     C*
183100010330     C* CALCOLA LA DATA CORRENTE
183200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
183300100318     C                   eval      datcor = %dec(%date() : *ISO)
183400000613     C*
183500000613     C                   ENDSR
183600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183700140220VABRSD;VABIND;VABLOD;VABCAD;VABPRD;VABNZD;W_VABRMN;VABPKB;X;W_VABNOT;VATNOT_B;VABCAS;
183800140220W_VATNOT_I;W_VABTSP;X;W_VABCBO;VABNCL;X;
183900130528
184000111116
184100100318
