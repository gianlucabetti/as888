000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130528      *
000300130528      * PARTICOLARITA':
000400130618      * Disk A
000500140220      *
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
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400040128     D tisi95ds      e ds
002500100413     D fivabds       e ds                  extname(fivab00f)
002600100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002700100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300040510     D parccm          s              8    INZ(*blanks)
003400040119     D parmbr          s             10    INZ(*blanks)
003500040119     D paropz          s              1    INZ(*blanks)
003600040119     D chkcall         s              1    INZ(*blanks)
003700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003800080418     D wFlgCAS         s              1    INZ(*blanks)
003900120410     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
004000120410     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
004100140319     D Intestazioni    s               n   inz(*off)
004200100326     D*------------------
004300100326     D* DS REPERIMENTO NUMERATORE
004400100326     D*------------------
004500100326     D trul33ds      e ds                  inz
004600100326     D*------------------
004700100326     D* DS ARCHITETTURA
004800100326     D*------------------
004900100326     D kpjba         e ds                  inz
005000130611     D*------------------
005100130611     D TRUL28DSX     e ds                  inz
005200100326
005300030715     D*------------------
005400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005500030715     D*------------------
005600030715     D SkSplitFLD      S             10    DIM(100)
005700030715     D SkSplitCSV      S            256    DIM(100)
005800030715     D CharCSV         S              1
005900030715     D CharTXT         S              1
006000030715     D CharNUM         S              1
006100100318     D CharSOS         S              1
006200130626     D posDa           S              4  0 INZ(*zeros)
006300130626     D posA            S              4  0 INZ(*zeros)
006400030715     D i               s              3  0 INZ(1)
006500030715     D wGiro           s              1  0 INZ(*zeros)
006600070823     D wVATNOT_A       s                   LIKE(VATNOT)
006700060331     D wVATNOT_B       s                   LIKE(VATNOT)
006800060331     D wVATNOT_E       s                   LIKE(VATNOT)
006900110927     D wVATNOT_I       s                   LIKE(VATNOT)
007000140220     D wVATNOT_J       s                   LIKE(VATNOT)
007100140220     D wVATNOT_IJ      s             70
007200140319     D wVATNOT_S       s                   LIKE(VATNOT)
007300120411     D wNOT            s             70
007400120411     D Data8A          s              8
007500030822     D*------------------
007600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007700030822     D*------------------
007800030822     D posDaDft        S              3  0 INZ(*zeros)
007900030822     D posADft         S              3  0 INZ(*zeros)
008000030822     D j               s              3  0 INZ(1)
008100030822     D wGiroDft        s              1  0 INZ(*zeros)
008200080418     D*------------------
008300080418     D* Costanti
008400080418     D*------------------
008500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008700100318
008800100318     D*------------------
008900100318     D* LINKING A DEFINIZIONI ESTERNE
009000100318     D*------------------
009100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009300100318
009400010330
009500010330
009600010330
009700000913     C                   reset                   rrnum
009800990921     C                   reset                   esito
009900990921     C                   reset                   wrkesito
010000010601     C*
010100010601     C                   exsr      opeini
010200010605     C                   exsr      rwvab
010300010601     C*
010400010601     C                   seton                                        lr
010500010601
010600010601
010700010601
010800010601     C*--------------------------------------------------------
010900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011000010601     C*--------------------------------------------------------
011100010601     C     PREELA        BEGSR
011200010601     C*
011300010601     C* SE OCCORRE SPEDIRE IN FILIALE
011400010601     C                   if        invfil <> *zeros and
011500010601     C                             flgGiro = '0'
011600010601     C*
011700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011800010601     C                   eval      flgGiro = '1'
011900010601     C*
012000010601     C                   endif
012100010601     C*
012200010601     C                   ENDSR
012300010601     C***
012400010601
012500010601
012600010601
012700010601
012800010601     C*--------------------------------------------------------
012900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013000010601     C*--------------------------------------------------------
013100010601     C     ENDELA        BEGSR
013200040119     C*
013300040119     C* Effettuo la chiamata al CLLE preposto
013400040119     C                   call(e)   'TITVVTC'
013500040510     C                   parm                    parccm
013600040119     C                   parm                    parmbr
013700040119     C                   parm      '2'           paropz
013800050201     C*
013900050201     C* Effettuo lancio TISI95 solo x chiusura
014000050201     C                   CLEAR                   TISI95DS
014100050201     C                   EVAL      I95TLA = 'C'
014200050201     C                   CALL      'TISI95R'
014300050201     C                   PARM                    TISI95DS
014400000616     C*
014500010601     C                   ENDSR
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600021025     C                   if        not %open(fivabwwr)
015700021025     C                   open      fivabwwr
015800010330     C                   endif
015900040119     C*
016000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100040119     C                   exsr      prevat
016200040119     C*
016300040119     C                   if        chkcall = '0'
016400040119     C*
016500040119     C                   if        not %open(fivatwwr)
016600040119     C                   open      fivatwwr
016700040119     C                   endif
016800010330     C*
016900040119     C                   clear                   §CTROKVB          7 0
017000040119     C                   clear                   §CTROKVT          7 0
017100040119     C                   clear                   §CTRMO            7 0
017200040119     C                   clear                   §CTRNO            7 0
017300120410     C*
017400120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017500120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017600120410     C                   IF        Intestazioni = *off
017700120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017800120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017900120410     C                   exsr      inzvar
018000120410     C                   exsr      impvab
018100120410     C                   ENDIF
018200010330     C*
018300030822     C                   do        *HIVAL
018400010330     C*
018500030822     C                   read      tivin00r                               70
018600010618     C*
018700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018800010618     C                   if        vinflg = '1'
018900010618     C                   eval      flgOk = '1'
019000010618     C                   endif
019100040510     C*
019200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019300040510     C                   z-add     *zeros        wLenVINDTA        4 0
019400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019800040510     C                   z-add     1             wFlgVINDTA
019900040510     C                   leave
020000040510     C                   endif
020100040510     C                   enddo
020200010618     C*
020300040510     C                   if        vindta > *blanks AND
020400040510     C                             wFlgVINDTA = 1
020500010330     C                   add       1             rrnum
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600130917     C*
021700130917     C* Aggiungo un byte a blanks all'inizio d ogni record
021800130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021900130917     C* evitare che vengano elisi i blank
022000140220     C***                eval      vindta  = '*' + %trim(vindta)
022100130917     C*
022200080527     C                   exsr      inzvar
022300100318     C                   exsr      normCSV
022400010605     C                   exsr      impvab
022500010601     C*
022600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022700010605     C                   eval      depfil = VABLNP
022800010601     C                   exsr      repfil
022900010601     C                   if        depfil = invfil
023000021025     C                   if        vlrpoi = 999
023100030822     C                   move(P)   invfil        VABFGS
023200021025     C                   else
023300030822     C                   move(P)   vlrpoi        VABFGS
023400021025     C                   endif
023500010601     C*
023600010601     C                   exsr      PREELA
023700010601     C*
023800010604     C* Ebbene...
023900010604     C*
024000040119     C  N31              add       1             §CTROKVB
024100040119     C   32              add       1             §CTRMO
024200040119     C   31              add       1             §CTRNO
024300080418     C*
024400030822     C                   if        wGiro = 2
024500080513     C* Scrivo sempre il VAT
024600080513     C  N31              exsr      wrivat
024700110201     C                   exsr      chkvab
024800080513     C                   endif
024900010604     C*
025000010604     C                   if        *in31 = *off and
025100010604     C                             *in32 = *off
025200010604     C                   eval      vinflg = '1'
025300010604     C                   else
025400010604     C                   eval      vinflg = '2'
025500010604     C                   endif
025600010604     C                   endif
025700010601     C*
025800010604     C                   endif
025900010604     C*
026000010330     C                   else
026100010330     C                   eval      vinflg = '1'
026200010330     C                   endif
026300010601     C*
026400010601     C  N70              update    tivin000
026500010330     C*
026600030822     C  N70              enddo
026700040119     C*
026800080527     C* Verifico dati rimasti "in cannna"...
026900080527     C                   seton                                        65
027000110201     C                   exsr      chkvab
027100080527     C*
027200040119     C                   endif                                                  (endif - chkcall)
027300010601     C*
027400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027500010601     C                   if        cntNonEl = *zeros or
027600010601     C                             flgMulti = '0'
027700010330     C* Se non ci sono record con errori ...
027800010601     C                   if        §ctrno = 0 and
027900010604     C                             §ctrmo = 0 and
028000010601     C                             flgStato <> '2'
028100010330     C* ... restituisco esito OK.
028200010330     C                   eval      wrkesito = '0'
028300010330     C                   else
028400040119     C                   if        §ctrokvb > 0
028500010330     C                   eval      wrkesito = '1'
028600010330     C                   else
028700010615     C                   if        flgOk = '0'
028800010615     C                   eval      wrkesito = '2'
028900010615     C                   else
029000010615     C                   eval      wrkesito = '6'
029100010615     C                   endif
029200010330     C                   endif
029300010330     C                   endif
029400010601     C                   else
029500010601     C                   eval      wrkesito = '9'
029600010601     C                   endif
029700010330     C*
029800010330     C                   if        %open(tivin00r)
029900010330     C                   close     tivin00r
030000010330     C                   endif
030100021025     C                   if        %open(fivabwwr)
030200021025     C                   close     fivabwwr
030300010330     C                   endif
030400040512     C                   if        %open(fivatwwr)
030500040512     C                   close     fivatwwr
030600040512     C                   endif
030700010601     C*
030800010601     C                   if        vlrpoi <> 999
030900010601     C                   eval      invfil = vlrpoi
031000010601     C                   endif
031100010330     C*
031200040119     C                   if        §ctrokvb > 0
031300010601     C                             and invfil > *zeros
031400010330     C                   exsr      invio
031500010330     C                   endif
031600010601     C*
031700010618     C                   if        flgGiro = '1'
031800010601     C                   exsr      endela
031900010618     C                   endif
032000010330     C*
032100010330     C                   ENDSR
032200010330     C***
032300080527
032400080527
032500080527
032600080527     C*----------------------------------------------------*
032700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032800080527     C*----------------------------------------------------*
032900100413     C     CHKVAB        BEGSR
033000110201     C*
033100110201     C* Se richiesti ignorare colli / peso / volume
033200110315 xxx C***                if        *in35
033300110315 xxx C***                z-add     *zeros        VABNCL
033400110315 xxx C***                z-add     *zeros        VABPKB
033500110315 xxx C***                z-add     *zeros        VABVLB
033600110315 xxx C***                endif
033700080527     C*
033800080527     C* Scrivo il VAB solo a rottura d spedizione
033900080527     C                   if        depspe = *zeros
034000080527     C  N31              eval      fivabds_sav = fivabds
034100080527     C                   eval      depspe = vabNSP
034200080527     C                   else
034300080527     C                   if        depspe = vabNSP AND *in65 = *off
034400100413     C   51              add       VABNCL        sav_VABNCL
034500100413     C   51              add       VABPKB        sav_VABPKB
034600100413     C   51              add       VABVLB        sav_VABVLB
034700100720     C   52              z-add     VABNCL        sav_VABNCL
034800100720     C   52              z-add     VABPKB        sav_VABPKB
034900100720     C   52              z-add     VABVLB        sav_VABVLB
035000100720     C   53              add       VABNCL        sav_VABNCL
035100100720     C   53              z-add     VABPKB        sav_VABPKB
035200100720     C   53              z-add     VABVLB        sav_VABVLB
035300080527     C                   else
035400080527     C  N31              eval      fivabds_ok = fivabds_sav
035500080527     C  N31              exsr      wrivab
035600080527     C  N31              eval      fivabds_sav = fivabds
035700080527     C                   eval      depspe = vabNSP
035800080527     C                   endif
035900080527     C                   endif
036000080527     C*
036100080527     C                   ENDSR
036200040119
036300040119
036400040119
036500040119     C*----------------------------------------------------*
036600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036700040119     C*----------------------------------------------------*
036800040510     C     WRIVAB        BEGSR
036900080513     C*
037000080513     C* Gestisco l'eventuale rottura x numero spedizione
037100080527     C                   if        VABNSP <> depspe or *in65 = *on
037200070301     C                   if        wVATNOT_E <> *blanks
037300130610     C                   eval      ok_VABCTM = '7Q'
037400070301     C                   endif
037500100413     C                   write     fivab000
037600080513     C                   endif
037700040119     C*
037800040119     C                   ENDSR
037900040119
038000040119
038100040119
038200040119     C*----------------------------------------------------*
038300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038400040119     C*----------------------------------------------------*
038500040510     C     WRIVAT        BEGSR
038600060512     C*
038700060512     C* Inizializzo i campi chiave
038800060512     C                   eval      VATFGS = VABFGS
038900060512     C                   eval      VATAAS = VABAAS
039000060512     C                   eval      VATCCM = VABCCM
039100070823     C                   eval      VATNRS = VABNRS
039200060512     C                   eval      VATNSP = VABNSP
039300060512     C                   eval      VATLNP = VABLNP
039400040119     C*
039500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039600040510     C                   if        VATTRC = *blanks
039700040510     C                   eval      VATTRC = 'E'
039800040510     C                   endif
039900110318     C                   if        VATNOT <> *blanks
040000040510     C                   write     FIVAT000
040100040119     C                   add       1             §CTROKVT
040200110318     C                   endif
040300060331     C*
040400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040500060331     C                   if        wVATNOT_A <> *blanks
040600060331     C                   eval      VATNOT = wVATNOT_A
040700060331     C                   eval      VATTRC = 'A'
040800060331     C                   write     FIVAT000
040900060331     C                   add       1             §CTROKVT
041000060331     C                   endif
041100060331     C*
041200060331     C                   if        wVATNOT_B <> *blanks
041300060331     C                   eval      VATNOT = wVATNOT_B
041400060331     C                   eval      VATTRC = 'B'
041500060331     C                   write     FIVAT000
041600060331     C                   add       1             §CTROKVT
041700060331     C                   endif
041800060331     C*
041900060331     C                   if        wVATNOT_E <> *blanks
042000110201     C                   eval      VATNOT = wVATNOT_E
042100060331     C                   eval      VATTRC = 'E'
042200140220     C* è un disk A, per cui niente barcode
042300120320     C                   endif
042400110927     C*
042500110927     C                   if        wVATNOT_I <> *blanks
042600110927     C                   eval      VATNOT = wVATNOT_I
042700110927     C                   eval      VATTRC = 'I'
042800110927     C                   write     FIVAT000
042900110927     C                   add       1             §CTROKVT
043000110927     C                   endif
043100140220     C*
043200140220     C                   if        wVATNOT_J <> *blanks
043300140220     C                   eval      VATNOT = wVATNOT_J
043400140220     C                   eval      VATTRC = 'J'
043500140220     C                   write     FIVAT000
043600140220     C                   add       1             §CTROKVT
043700140220     C                   endif
043800140319     C*
043900140319     C                   if        wVATNOT_S <> *blanks
044000140319     C                   eval      VATNOT = wVATNOT_S
044100140319     C* i 2 flag devono essere blank
044200140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
044300140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
044400140319     C                   eval      VATTRC = 'S'
044500140319     C                   write     FIVAT000
044600140319     C                   add       1             §CTROKVT
044700140319     C                   endif
044800040119     C*
044900040119     C                   ENDSR
045000040119
045100040119
045200040119
045300040119     C*----------------------------------------------------*
045400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045500040119     C*----------------------------------------------------*
045600040119     C     PREVAT        BEGSR
045700040119     C*
045800040119     C* Compongo il nome del membro da dare al FIVATWWR
045900040119     C                   eval      parmbr = vlrhdl
046000040119     C                   movel     'M'           parmbr
046100040510     C                   eval      parccm = vlrksc
046200040119     C                   eval      paropz = '1'
046300040119     C* Effettuo la chiamata al CLLE preposto
046400040119     C                   call(e)   'TITVVTC'
046500040119     C                   parm                    parccm
046600040119     C                   parm                    parmbr
046700040119     C                   parm                    paropz
046800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046900040119     C                   if        %error
047000040119     C                   movel     '1'           chkcall
047100040119     C                   else
047200040119     C                   movel     '0'           chkcall
047300040119     C                   endif
047400040119     C*
047500040119     C                   ENDSR
047600010601
047700010601
047800010601
047900010330     C*----------------------------------------------------*
048000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048100010330     C*----------------------------------------------------*
048200040119     C     INZVAR        BEGSR
048300080527     C*
048400080527     C                   setoff                                       65
048500010330     C*
048600030822     C* Inizializzo variabili di wrk
048700020204     C                   Z-ADD     *zeros        Num5_0            5 0
048800101018     C                   MOVEL     *blanks       wFlgCAS
048900060331     C                   MOVEL     *blanks       wVATNOT_A
049000060331     C                   MOVEL     *blanks       wVATNOT_B
049100060331     C                   MOVEL     *blanks       wVATNOT_E
049200110927     C                   MOVEL     *blanks       wVATNOT_I
049300140220     C                   MOVEL     *blanks       wVATNOT_J
049400140220     C                   MOVEL     *blanks       wVATNOT_IJ
049500140319     C                   MOVEL     *blanks       wVATNOT_S
049600120411     C                   MOVEL     *blanks       wNOT
049700120411     C                   MOVEL     *blanks       Data8A
049800030822     C*
049900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050000030822     C                   CLEAR                   SkSplitCSV
050100030822     C                   EVAL      i = 1
050200030822     C                   EVAL      posDa = *zeros
050300030822     C                   EVAL      posA  = *zeros
050400080527     C*
050500080527     C                   CLEAR                   FIVABDS
050600080527     C                   CLEAR                   FIVAT000
050700030822     C*
050800030822     C* Reimposto i valori di default
050900030822     C                   EXSR      DEFCAM
051000030822     C*
051100010330     C                   ENDSR
051200010330     C*----------------------------------------------------*
051300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051400010330     C*----------------------------------------------------*
051500010330     C     DEFCAM        BEGSR
051600030822     C*
051700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051800030715     C* e delimitatore testo.
051900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052200100318     C*
052300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052400100318     C                   EVAL      CharSOS = CharNUM
052500030822     C*
052600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052700100720     C                   SETOFF                                       505152
052800100720     C                   SETOFF                                       53
052900100413     C                   SELECT
053000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053100100720     C                   SETON                                        50
053200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053300100413     C                   SETON                                        51
053400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053500100720     C                   SETON                                        52
053600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053700100720     C                   SETON                                        53
053800100413     C                   ENDSL
053900030822     C*
054000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054100030822     C                   EVAL      posDaDft = 1
054200030822     C                   EVAL      posADft  = 0
054300030822     C                   EVAL      wGiroDft = 0
054400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054500030822     C                             posDaDft > 0
054600030822     C*
054700030822     C* Gestisco il 1° giro
054800030822     C                   IF        wGiroDft = 0
054900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055100030822     C* Incremento il contatore dei "giri"
055200030822     C                   EVAL      wGiroDft = 1
055300030822     C                   ELSE
055400030822     C                   EVAL      posDaDft = posADft
055500030822     C                   ENDIF
055600030822     C* Eseguo lo scan x trovare la fine del campo corrente
055700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055800030822     C*
055900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056100030822     C                   IF        posDaDft > 0 AND
056200030822     C                             posADft  > 0
056300030822     C* NCL
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NCL'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNCL
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CCM
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CCM'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCCM
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* LNP
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'LNP'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABLNP
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* NRS
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NRS'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNRS
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CTR
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CTR'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCTR
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* PKB
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'PKB'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800130610     C                   Z-ADD     PiVal         VABPKB
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* VLB
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VLB'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiNum=*on
065100130610     C                   Z-ADD     PiVal         VABVLB
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* QFT
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'QFT'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABQFT
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* CBO
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'CBO'
067200030822     C                   EVAL      VABCBO=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* TSP
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'TSP'
068100030822     C                   EVAL      VABTSP=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VAS
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VAS'
069000030822     C                   EVAL      VABVAS=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* VCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VCA'
069900030822     C                   EVAL      VABVCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300070925     C* TIC
070400070925     C                   IF        %subst(
070500070925     C                             %subst(vlrppt:posDaDft+1:
070600070925     C                             posADft-posDaDft-1):1:3)
070700070925     C                             = 'TIC'
070800070925     C                   EVAL      VABTIC=%trim(%subst(
070900070925     C                             %subst(vlrppt:posDaDft+1:
071000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100070925     C                   ENDIF
071200030822     C* GCA
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'GCA'
071700030822     C                   EVAL      VABGCA=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* CTM
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'CTM'
072600030822     C                   EVAL      VABCTM=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* FFD
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'FFD'
073500030822     C                   EVAL      VABFFD=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* VAD
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'VAD'
074400030822     C                   EVAL      VABVAD=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GMA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GMA'
075300030822     C                   EVAL      VABGMA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* GGA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GGA'
076200030822     C                   EVAL      VABGGA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* GVA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GVA'
077100030822     C                   EVAL      VABGVA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* TC1
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'TC1'
078000030822     C                   EVAL      VABTC1=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* TC2
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'TC2'
078900030822     C                   EVAL      VABTC2=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300040714     C* VATTRC
079400040714     C                   IF        %subst(
079500040714     C                             %subst(vlrppt:posDaDft+1:
079600040714     C                             posADft-posDaDft-1):1:3)
079700040714     C                             = 'TRC'
079800040714     C                   EVAL      VATTRC=%trim(%subst(
079900040714     C                             %subst(vlrppt:posDaDft+1:
080000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100040714     C                   ENDIF
080200030822     C* ...
080300030822     C                   ENDIF
080400030822     C                   ENDDO
080500020204     C*
080600010330     C                   ENDSR
080700010607     C*----------------------------------------------------*
080800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080900010607     C*----------------------------------------------------*
081000010607     C     IMPVAB        BEGSR
081100010607     C*
081200010607     C                   SETOFF                                       3132
081300110315 xxx C***                SETON                                        35
081400030515     C*
081500010607     C                   MOVEL     datcor        VABAAS
081600010607     C                   MOVE      datcor        VABMGS
081700010607     C*
081800030715     C**********
081900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082000030715     C**********
082100030715     C                   IF        wGiro = *zeros
082200040113     C*
082300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082600080429     C                             %trim(intcol(5))<>*blanks
082700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082900080429     C                                      %trim(intcol(5))
083000070823     C     ';':CharCSV   XLATE     vindta        vindta
083100040510     C                   ENDIF
083200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083300031201     C     minu:maiu     XLATE     vindta        vindta
083400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083500030715     C                   DOW       posDa <= %len(%trim(vindta))
083600030715     C*
083700030715     C* Gestisco il 1° campo
083800030715     C                   IF        i = 1
083900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084000030715     C                   EVAL      posDa = 1
084100030715     C* Eseguo lo scan x trovare la fine del primo campo
084200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C* X i campi successivi al 1°
084700030715     C                   ELSE
084800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084900030715     C                   EVAL      posDa = posA + 1
085000030715     C* Eseguo lo scan x trovare la fine del campo corrente
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* Gestisco l'ultimo campo
085300030715     C                   IF        posA = *zeros
085400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   LEAVE
085900030715     C                   ELSE
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C                   ENDIF
086400030715     C                   ENDIF
086500030715     C* Incremento il contatore d campo
086600030715     C                   EVAL      i = i +1
086700030715     C                   ENDDO
086800030715     C*
086900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087000030715     C* delimitazione testo
087100030715     C                   EVAL      i = 1
087200030715     C                   DOW       i < %elem(SkSplitFLD)
087300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087500030715     C                   EVAL      i = i + 1
087600030715     C                   ENDDO
087700030715     C                   EVAL      wGiro = 1
087800030715     C                   ELSE
087900030715     C                   EVAL      wGiro = 2
088000031204     C**********
088100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088200031204     C* d separatore campo
088300031204     C**********
088400031204     C                   EVAL      vindta = %trim(vindta)
088500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088600031204     C                   EVAL      vindta = %subst(vindta:2)
088700031204     C                   ENDDO
088800031204     C*
088900031204     C                   Z-ADD     *zeros        lunghInput        4 0
089000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089400110912     C                   ENDDO
089500030715     C**********
089600030715     C* Effettuo lo split del campo dati d input
089700030715     C**********
089800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089900030715     C                   DOW       posDa <= %len(%trim(vindta))
090000030715     C*
090100030715     C* Gestisco il 1° campo
090200030715     C                   IF        i = 1
090300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090400030715     C                   EVAL      posDa = 1
090500030715     C* Eseguo lo scan x trovare la fine del primo campo
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C* X i campi successivi al 1°
091100030715     C                   ELSE
091200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091300030715     C                   EVAL      posDa = posA + 1
091400031204     C* Verifico che nn vi sia il campo nn valorizzato
091500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091600031204     C* Se campo nn valorizzato skippo al prossimo
091700031204     C                   EVAL      SkSplitCSV(i) = *blanks
091800031204     C                   EVAL      posA  = posA + 1
091900031204     C                   ELSE
092000030715     C* Eseguo lo scan x trovare la fine del campo corrente
092100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092200030715     C* Gestisco l'ultimo campo
092300030715     C                   IF        posA = *zeros
092400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C                   LEAVE
092900030715     C                   ELSE
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   ENDIF
093400031204     C                   ENDIF
093500030715     C                   ENDIF
093600030715     C* Incremento il contatore d campo
093700030715     C                   EVAL      i = i +1
093800030715     C                   ENDDO
093900030715     C*
094000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094100030715     C* delimitazione testo
094200030715     C                   EVAL      i = 1
094300030715     C                   DOW       i < %elem(SkSplitCSV)
094400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094600030715     C                   EVAL      i = i + 1
094700030715     C                   ENDDO
094800100720     C*
094900100720     C* Stacco un progressivo x numero spedizione
095000130917 xxx C                   EXSR      repNSP
095100100326     C*
095200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095300030715     C                   EVAL      i = 1
095400030715     C                   DOW       i < %elem(SkSplitFLD)
095500030715     C*
095600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095800030715     C                             SkSplitCSV(i) <> *blanks
095900030721     C*** ATB
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096100030721     C                   EVAL      VABATB = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030715     C*** RSD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096800030715     C* ==
096900130528     C                   ENDIF
097000030721     C*** RD2
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** IND
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097600030715     C                   EVAL      VABIND = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** LOD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** PRD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030721     C*** NZD
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030715     C*** RMA
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** NOT
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** NT2
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** VCA
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** VAS
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** FFD
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030721     C*** GC1
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GC2
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TSP
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102500030721     C                   ENDIF
102600040510     C*** CBO
102700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102900040510     C                   ENDIF
103000030721     C*** NAS
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TIC
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GCA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** XCO
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** CTM
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TCR
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** CTS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** FTM
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** VAD
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GMA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GGA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** GVA
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TC1
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** TC2
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** SCL
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** RMO
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** NMO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*
109900030721     C* Reperisco quindi i campi numerici...
110000040510     C*** NSP
110100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110200040510     C                   EVAL      PiStr=SkSplitCSV(i)
110300040510     C                   EXSR      CHKNUM
110400040510     C                   IF        PiInt=*on
110500040510     C                   Z-ADD     PiVal         VABNSP
110600040510     C                   ELSE
110700040510     C                   SETON                                        31
110800040510     C                   EVAL      VABNSP = *zeros
110900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111000040510     C                             + ' ' + 'VABNSP'
111100040510     C                   ENDIF
111200040510     C                   ENDIF
111300071106     C*** RMN
111400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111500071106     C                   EVAL      PiStr=SkSplitCSV(i)
111600071106     C                   EXSR      CHKNUM
111700071106     C                   IF        PiInt=*on
111800071106     C                   Z-ADD     PiVal         VABRMN
111900071106     C                   ELSE
112000071106     C                   SETON                                        32
112100071106     C                   EVAL      VABRMN = *zeros
112200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112300071106     C                             + ' ' + 'VABRMN'
112400071106     C                   ENDIF
112500071106     C                   ENDIF
112600030721     C*** CAD
112700130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112800130610     C                   EVAL      PiStr=SkSplitCSV(i)
112900130610     C                   EXSR      CHKNUM
113000130610     C                   IF        PiInt=*on
113100130610     C                   Z-ADD     PiVal         Num5_0
113200130610     C                   MOVEL(p)  Num5_0        VABCAD
113300130610     C                   ELSE
113400130610     C                   SETON                                        32
113500130610     C                   EVAL      VABCAD = *zeros
113600130610     C                   EVAL      vinmsg = %trimr(vinmsg)
113700130610     C                             + ' ' + 'VABCAD'
113800130610     C                   ENDIF
113900130605     C                   ENDIF
114000030721     C*** NCL
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on
114500030721     C                   Z-ADD     PiVal         VABNCL
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABNCL = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040122     C                             + ' ' + 'VABNCL'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** PKB
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiNum=*on
115800130610     C                   Z-ADD(H)  PiVal         VABPKB
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABPKB = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABPKB'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** CAS
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116800110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116900111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117000111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117100130703     C                   EVAL      VABVCA = 'EUR'
117200101018     C                   ENDIF
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiNum=*on
117600030721     C                   Z-ADD     PiVal         VABCAS
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCAS = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABCAS'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** CCM
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on      AND
118900030721     C                             PiVal<=9999999 AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABCCM
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      VABCCM = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040510     C                             + ' ' + 'VABCCM'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** LNP
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on    AND
120400030721     C                             PiVal<=999   AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABLNP
120700030721     C                   ELSE
120800040715     C                   SETON                                        32
120900030721     C                   EVAL      VABLNP = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100040510     C                             + ' ' + 'VABLNP'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** NRS
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on AND
121900030721     C                             PiVal<=99
122000030721     C                   Z-ADD     PiVal         VABNRS
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABNRS'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** LNA
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on    AND
123200030721     C                             PiVal<=999   AND
123300030721     C                             PiVal>*zeros
123400030721     C                   Z-ADD     PiVal         VABLNA
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      VABLNA = *zeros
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABLNA'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** CTR
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on  AND
124700030721     C                             PiVal<=999
124800030721     C                   Z-ADD     PiVal         VABCTR
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABCTR'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** IAS
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABIAS
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABIAS'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** VLB
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200130610     C                   Z-ADD(H)  PiVal         VABVLB
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABVLB'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** QFT
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiNum=*on
128400030721     C                   Z-ADD     PiVal         VABQFT
128500030721     C                   ELSE
128600030721     C                   SETON                                        32
128700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128800030721     C                             + ' ' + 'VABQFT'
128900030721     C                   ENDIF
129000030721     C                   ENDIF
129100030721     C*** NCD
129200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129300030721     C                   EVAL      PiStr=SkSplitCSV(i)
129400030721     C                   EXSR      CHKNUM
129500030721     C                   IF        PiInt=*on      AND
129600030721     C                             PiVal<=9999999
129700030721     C                   Z-ADD     PiVal         VABNCD
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABNCD'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** NCA
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on      AND
130900030721     C                             PiVal<=9999999
131000030721     C                   Z-ADD     PiVal         VABNCA
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABNCA'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** ZNC
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on AND
132200030721     C                             PiVal<=99
132300030721     C                   Z-ADD     PiVal         VABZNC
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABZNC'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** DCR
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on       AND
133500030721     C                             PiVal<=99999999
133600030721     C                   Z-ADD     PiVal         VABDCR
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABDCR'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** HCR
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on   AND
134800030721     C                             PiVal<=9999
134900030721     C                   Z-ADD     PiVal         VABHCR
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABHCR'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** VMD
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiNum=*on
136100030721     C                   Z-ADD     PiVal         VABVMD
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABVMD'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** ANT
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on        AND
137300030721     C                             PiVal<=999999999
137400030721     C                   Z-ADD     PiVal         VABANT
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABANT'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** CMO
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on
138600030721     C                   Z-ADD     PiVal         Num5_0
138700030721     C                   MOVEL(p)  Num5_0        VABCMO
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      VABCMO = *zeros
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABCMO'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500040510     C*
139600040510     C* Reperisco i campi del FIVAT
139700040510     C*** VATNOT
139800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140000040510     C                   ENDIF
140100040510     C*** VATTRC
140200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140400040510     C                   ENDIF
140500060331     C*** VATNOT_A
140600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140800060331     C                   ENDIF
140900060331     C*** VATNOT_B
141000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141200060331     C                   ENDIF
141300060331     C*** VATNOT_E
141400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141600060331     C                   ENDIF
141700110927     C*** VATNOT_I
141800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142000110927     C                   ENDIF
142100140319     C*** VATNOT_S
142200140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
142300140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
142400140319     C                   ENDIF
142500070823     C*
142600100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142700120301
142800140319      *** W_VABRSD: è sia VABRSD che VABRD2
142900140319     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
143000140220     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
143100140319     C                   EVAL      VABRSD =
143200140220     C                              %subst(wNOT:1:35)
143300140319     C                   EVAL      VABRD2 =
143400140220     C                              %subst(wNOT:36:35)
143500140220     C                   ENDIF
143600140319
143700140319      *** W_VABNOT: è sia VABNOT che VABNT2
143800140319     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
143900140319     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
144000140319     C                   EVAL      VABNOT =
144100140319     C                              %subst(wNOT:1:35)
144200140319     C                   EVAL      VABNT2 =
144300140319     C                              %subst(wNOT:36:35)
144400140319     C                   ENDIF
144500130626
144600140319     C*** W_VATNOT_B
144700140319     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_B'
144800140319     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144900140319     C* VATNOT_S = VATNOT_B
145000140319     C                   EVAL      wVATNOT_S = wVATNOT_B
145100140319     C                   ENDIF
145200070925     C*
145300071029     C***  <===  -----------------------------------------
145400040510     C*
145500030715     C                   ENDIF
145600030715     C                   EVAL      i = i + 1
145700030715     C                   ENDDO
145800070925     C*
145900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146000140319     C*
146100070925     C*
146200071029     C***  <===  -----------------------------------------
146300070925     C*
146400070925     C* Considerazioni finali su CBO/CAS
146500140220     C                   IF        wFlgCAS = '1'    OR
146600140220     C                             VABCAS  > *zeros
146700140220     C                   IF        VABCBO = '1'
146800140220     C                   EVAL      VABCBO = '4'
146900140220     C                   ENDIF
147000140220     C                   IF        VABCBO = '2'
147100140220     C                   EVAL      VABCBO = '6'
147200140220     C                   ENDIF
147300140220     C                   ENDIF
147400070925     C***  <===  ----------------------------
147500040510     C*
147600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147700040510     C                   IF        VABNSP = *zeros
147800040510     C                   SETON                                        31
147900040510     C                   ENDIF
148000040715     C*
148100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148200040715     C                   IF        VABLNP = *zeros
148300040715     C                   SETON                                        31
148400040715     C                   ENDIF
148500040510     C*
148600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148700120411     C                   IF        VABRMN = *zeros
148800120411     C                   Z-ADD     VABNSP        VABRMN
148900120411     C                   ENDIF
149000120328     C* IN QUESTO CASO LO FORZO = 1
149100120411     C***                IF        VABRMN = *zeros
149200120411     C***                Z-ADD     1             VABRMN
149300120411     C***                ENDIF
149400040510     C*
149500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149600040510     C                   IF        VABRMA = *blanks
149700040714     C                   MOVEL     VABRMN        VABRMA
149800040510     C                   ENDIF
149900040128     C*
150000040510     C* Se provincia nn valorizzata la reperisco
150100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150200110912     C                   IF        VABPRD =  *blanks AND
150300110912     C                             VABNZD =  *blanks AND
150400110912     C                             VABCAD <> *blanks AND
150500110912     C                             VABLOD <> *blanks
150600040128     C                   CLEAR                   TISI95DS
150700040128     C                   EVAL      I95TCN = '3'
150800040128     C                   Z-ADD     datcor        I95DAT
150900040128     C                   EVAL      I95NAR = VABNZD
151000040128     C                   EVAL      I95CAP = VABCAD
151100040128     C                   EVAL      I95LOC = VABLOD
151200040128     C                   CALL      'TISI95R'
151300040128     C                   PARM                    TISI95DS
151400040128     C                   EVAL      VABPRD = O95PRV
151500040510     C                   ENDIF
151600020204     C*
151700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151800020204     C                   EXSR      CHKIMPDIV
151900030715     C                   ENDIF
152000020204     C*
152100010607     C                   ENDSR
152200010607     C*----------------------------------------------------*
152300010601
152400020204
152500020204     C*----------------------------------------------------*
152600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152700020204     C*----------------------------------------------------*
152800020204     C     CHKIMPDIV     BEGSR
152900020204     C*
153000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153100020204     C                   Z-ADD     *zeros        wrkDec            9 9
153200020204     C*
153300020204     C* Come prima cosa effettuo considerazioni sulla divisa
153400020204     C                   IF        vabIAS > *zeros
153500020204     C                   IF        vabVAS <> 'EUR'
153600020204     C                   EVAL      vabVAS =  'ITL'
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C                   IF        vabCAS > *zeros
154100020204     C                   IF        vabVCA <> 'EUR'
154200020204     C                   EVAL      vabVCA =  'ITL'
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C                   IF        vabVMD > *zeros
154700020204     C                   IF        vabVAD <> 'EUR'
154800020204     C                   EVAL      vabVAD =  'ITL'
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155300020204     C                   Z-ADD     vabIAS        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVAS = 'ITL'
155600020204     C                   EVAL      vabIAS = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156100020204     C                   Z-ADD     vabCAS        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVCA = 'ITL'
156400020204     C                   EVAL      vabCAS = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156900020204     C                   Z-ADD     vabVMD        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVAD = 'ITL'
157200020204     C                   EVAL      vabVMD = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C                   ENDSR
157700020204     C***
157800010330
157900100318
158000100318
158100100318     C*----------------------------------------------------*
158200100318     C*  CONTROLLO NUMERICITA' CAMPI
158300100318     C*----------------------------------------------------*
158400100318     C     CHKNUM        BEGSR
158500100318     C*
158600100318     C                   IF        PiDecChr = *blanks
158700100318     C                   EVAL      PiDecChr = CharNUM
158800100318     C                   ENDIF
158900100318     C*
159000100318     C                   callp     UBISNUM_Check(PiStr
159100100318     C                                          :PiDecChr
159200100318     C                                          :PiVal
159300100318     C                                          :PiNum
159400100318     C                                          :PiInt)
159500100318     C*
159600100318     C                   ENDSR
159700100318     C***
159800010601
159900010601
160000010601
160100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160200010601     C     repfil        BEGSR
160300010601     C*
160400010601     C                   if        invfil = *zeros and
160500010601     C                             depfil > *zeros and
160600010629     C                             (vinflg = *blanks or
160700010629     C                              vinflg = *zeros)
160800010601     C
160900010601     C                   eval      invfil = depfil
161000010601     C                   endif
161100010601     C*
161200010601     C                   if        depfil <> invfil and
161300010601     C                             invfil > *zeros
161400010601     C                   eval      flgMulti = '1'
161500010601     C                   if        vinflg = *blanks
161600010601     C                   add       1             cntNonEl
161700010601     C                   endif
161800010601     C                   endif
161900010601     C*
162000010601     C                   if        vinflg = '2'
162100010601     C                   eval      flgStato = '2'
162200010601     C                   endif
162300010601     C*
162400010601     C                   ENDSR
162500010601     C***
162600010601
162700010601
162800010601
162900010330
163000040119      /TITLE Invio dei dati al punto operativo.
163100040119     C     invio         BEGSR
163200040119     C*
163300040119     C* 1° invio FIVAT
163400040119     C                   reset                   dscmz
163500040119     C                   move      vlrpoi        cmzdst
163600040119     C                   eval      cmzfld = 'FIVATWWR'
163700040119     C                   eval      cmzmbd = vlrhdl
163800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163900040119     C***                if        prmfir = *blanks
164000040119     C                   eval      cmzfla = 'FIVAT00F'
164100040119     C                   eval      cmzmba = 'FIVAT00F'
164200040119     C***                else
164300040119     C***                eval      cmzfla = prmfir
164400040119     C***                eval      cmzmba = prmfir
164500040119     C***                endif
164600040119     C                   eval      cmznrr = *zeros
164700040119     C                   move      §ctrokvt      cmznrr
164800040119     C                   eval      cmzlba = vlrfl1
164900040119     C                   call(e)   'TIS711C'
165000040119     C                   parm                    dscmz
165100040119     C                   parm      *blanks       esito
165200040119     C                   if        %error
165300040119     C                             or cmzerr = '1'
165400040119     C                             or esito  = '1'
165500040119     C                   eval      wrkesito = '3'
165600040119     C                   else
165700040119     C*
165800040119     C* 2° invio FIVAB
165900040119     C                   reset                   dscmz
166000040119     C                   move      vlrpoi        cmzdst
166100040119     C                   eval      cmzfld = vlrfou
166200040119     C                   eval      cmzmbd = vlrhdl
166300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166400040119     C***                if        prmfir = *blanks
166500040119     C                   eval      cmzfla = 'FIVAB00F'
166600040119     C                   eval      cmzmba = 'FIVAB00F'
166700040119     C***                else
166800040119     C***                eval      cmzfla = prmfir
166900040119     C***                eval      cmzmba = prmfir
167000040119     C***                endif
167100040119     C                   eval      cmznrr = *zeros
167200040119     C                   move      §ctrokvb      cmznrr
167300040119     C                   eval      cmzlba = vlrfl1
167400040119     C                   call(e)   'TIS711C'
167500040119     C                   parm                    dscmz
167600040119     C                   parm      *blanks       esito
167700040119     C                   if        %error
167800040119     C                             or cmzerr = '1'
167900040119     C                             or esito  = '1'
168000040119     C                   eval      wrkesito = '3'
168100040119     C                   endif
168200040119     C                   endif
168300040119     C*
168400040119     C                   ENDSR
168500040119     C***
168600010601
168700010601
168800010601
168900010601
169000010601      /TITLE Invio dei dati al punto operativo.
169100010601     C     opeini        BEGSR
169200010601     C*
169300010601     C* Inizializzo flag e contatori operativi
169400010601     C                   movel     '0'           flgGiro           1
169500010601     C                   movel     '0'           flgMulti          1
169600010601     C                   movel     '1'           flgStato          1
169700010615     C                   movel     '0'           flgOk             1
169800010601     C                   z-add     *zeros        cntNonEl         10 0
169900010601     C                   z-add     *zeros        depfil            3 0
170000010601     C                   z-add     *zeros        invfil            3 0
170100010601     C*
170200010601     C                   ENDSR
170300010601     C***
170400070326
170500070326
170600070326
170700070326
170800070326     C     *pssr         BEGSR
170900070329     C*
171000070329     C                   if        %open(tivin00r)
171100070329     C                   close     tivin00r
171200070329     C                   endif
171300070329     C                   if        %open(fivabwwr)
171400070329     C                   close     fivabwwr
171500070329     C                   endif
171600070329     C                   if        %open(fivatwwr)
171700070329     C                   close     fivatwwr
171800070329     C                   endif
171900070326     C*
172000070326     C* Effettuo la chiamata al CLLE preposto
172100070326     C                   call(e)   'TITVVTC'
172200070326     C                   parm                    parccm
172300070326     C                   parm                    parmbr
172400070326     C                   parm      '2'           paropz
172500070326     C*
172600070326     C                   eval      wrkesito = '2'
172700070404     C*
172800070404     C                   seton                                        LR
172900070326     C*
173000070326     C                   ENDSR     '*CANCL'
173100070326     C***
173200100318
173300100318
173400100318
173500100318     C     normCSV       BEGSR
173600100318     C*
173700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173800100318     C* separatori campi
173900100318     C                   if        CharTXT <> *blanks
174000100318     C                   z-add     *zeros        wLenVINDTA        4 0
174100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174200100318     C                   setoff                                       80
174300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174600100318     C                   if        not *in80
174700100318     C                   seton                                        80
174800100318     C                   else
174900100318     C                   setoff                                       80
175000100318     C                   endif
175100100318     C                   endif
175200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175300100318     C                             and *in80 = *on
175400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175500100318     C                   endif
175600100318     C                   enddo
175700100318     C                   endif
175800100318     C*
175900100318     C                   ENDSR
176000100318     C***
176100070326
176200100326
176300100326
176400100326     C*----------------------------------------------------*
176500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176600100326     C*----------------------------------------------------*
176700100326     C     REPNSP        BEGSR
176800100326     C*
176900100326     C* NSP => Stacco un numeratore da AZNUM
177000100326     C                   clear                   TRUL33DS
177100100326     C                   eval      I33OPE = *zeros
177200100326     C                   eval      I33CNU = 302
177300100326     C                   eval      I33NUM = 1
177400100326     C                   movel     TRUL33DS      KPJBU
177500100326     C                   call      'TRUL33R'
177600100326     C                   parm                    KPJBA
177700100326     C                   movel     KPJBU         TRUL33DS
177800100326     C                   if        O33ERR = *zeros
177900100326     C                   z-add     O33NRF        VABNSP
178000100326     C                   z-add     O33NRF        VATNSP
178100140305     C                   z-add     O33NRF        VABRMN
178200100326     C                   else
178300100326     C                   SETON                                        31
178400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178500140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
178600100326     C                   endif
178700100326     C*
178800100326     C                   ENDSR
178900010330
179000100326
179100010330
179200000613     C     *inzsr        BEGSR
179300990910     C*
179400990910     C     *entry        plist
179500990920     C                   parm                    tivlrds
179600990921     C                   parm      wrkesito      esito
179700000724     C                   parm                    prmlit
179800000710     C                   parm                    prmfir
179900010330     C*
180000010330     C* CALCOLA LA DATA CORRENTE
180100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180200100318     C                   eval      datcor = %dec(%date() : *ISO)
180300000613     C*
180400000613     C                   ENDSR
180500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180600140319W_VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABRMN;W_VATNOT_B;VABNCL;VABPKB;VABCAS;VABTIC;W_VABNOT;
180700140305
180800130528
180900111116
181000100318
