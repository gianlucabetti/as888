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
004100140407     D Intestazioni    s               n   inz(*on)
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
007300140522     D wNOT            s             70
007400140522     D wCADLOD         s             40
007500140407     D wIND1           s                   like(VABIND)
007600140407     D wIND2           s                   like(VABIND)
007700140513     D wIND3           s                   like(VABIND)
007800120411     D Data8A          s              8
007900030822     D*------------------
008000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008100030822     D*------------------
008200030822     D posDaDft        S              3  0 INZ(*zeros)
008300030822     D posADft         S              3  0 INZ(*zeros)
008400030822     D j               s              3  0 INZ(1)
008500030822     D wGiroDft        s              1  0 INZ(*zeros)
008600080418     D*------------------
008700080418     D* Costanti
008800080418     D*------------------
008900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
009000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009100100318
009200100318     D*------------------
009300100318     D* LINKING A DEFINIZIONI ESTERNE
009400100318     D*------------------
009500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700100318
009800010330
009900010330
010000010330
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400010601     C*
010500010601     C                   exsr      opeini
010600010605     C                   exsr      rwvab
010700010601     C*
010800010601     C                   seton                                        lr
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011400010601     C*--------------------------------------------------------
011500010601     C     PREELA        BEGSR
011600010601     C*
011700010601     C* SE OCCORRE SPEDIRE IN FILIALE
011800010601     C                   if        invfil <> *zeros and
011900010601     C                             flgGiro = '0'
012000010601     C*
012100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012200010601     C                   eval      flgGiro = '1'
012300010601     C*
012400010601     C                   endif
012500010601     C*
012600010601     C                   ENDSR
012700010601     C***
012800010601
012900010601
013000010601
013100010601
013200010601     C*--------------------------------------------------------
013300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013400010601     C*--------------------------------------------------------
013500010601     C     ENDELA        BEGSR
013600040119     C*
013700040119     C* Effettuo la chiamata al CLLE preposto
013800040119     C                   call(e)   'TITVVTC'
013900040510     C                   parm                    parccm
014000040119     C                   parm                    parmbr
014100040119     C                   parm      '2'           paropz
014200050201     C*
014300050201     C* Effettuo lancio TISI95 solo x chiusura
014400050201     C                   CLEAR                   TISI95DS
014500050201     C                   EVAL      I95TLA = 'C'
014600050201     C                   CALL      'TISI95R'
014700050201     C                   PARM                    TISI95DS
014800000616     C*
014900010601     C                   ENDSR
015000010601
015100010601
015200010330     C*--------------------------------------------------------
015300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015400010330     C*--------------------------------------------------------
015500010605     C     RWVAB         BEGSR
015600010330     C*
015700010330     C                   if        not %open(tivin00r)
015800010330     C                   open      tivin00r
015900010330     C                   endif
016000021025     C                   if        not %open(fivabwwr)
016100021025     C                   open      fivabwwr
016200010330     C                   endif
016300040119     C*
016400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016500040119     C                   exsr      prevat
016600040119     C*
016700040119     C                   if        chkcall = '0'
016800040119     C*
016900040119     C                   if        not %open(fivatwwr)
017000040119     C                   open      fivatwwr
017100040119     C                   endif
017200010330     C*
017300040119     C                   clear                   §CTROKVB          7 0
017400040119     C                   clear                   §CTROKVT          7 0
017500040119     C                   clear                   §CTRMO            7 0
017600040119     C                   clear                   §CTRNO            7 0
017700120410     C*
017800120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017900120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
018000120410     C                   IF        Intestazioni = *off
018100120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018200120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018300120410     C                   exsr      inzvar
018400120410     C                   exsr      impvab
018500120410     C                   ENDIF
018600010330     C*
018700030822     C                   do        *HIVAL
018800010330     C*
018900030822     C                   read      tivin00r                               70
019000010618     C*
019100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019200010618     C                   if        vinflg = '1'
019300010618     C                   eval      flgOk = '1'
019400010618     C                   endif
019500040510     C*
019600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019700040510     C                   z-add     *zeros        wLenVINDTA        4 0
019800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
020000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
020100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020200040510     C                   z-add     1             wFlgVINDTA
020300040510     C                   leave
020400040510     C                   endif
020500040510     C                   enddo
020600010618     C*
020700040510     C                   if        vindta > *blanks AND
020800040510     C                             wFlgVINDTA = 1
020900010330     C                   add       1             rrnum
021000010330     C*
021100010601     C                   if        *in70 = *off and
021200010330     C                             (vinflg = *blanks
021300010330     C                              or vinflg = '0'
021400010330     C                              or vinflg = '2')
021500010330     C*
021600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021700010711     C                   if        vinflg = *blanks or vinflg = '0'
021800010711     C                   clear                   vinmsg
021900010711     C                   endif
022000130917     C*
022100130917     C* Aggiungo un byte a blanks all'inizio d ogni record
022200130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022300130917     C* evitare che vengano elisi i blank
022400140220     C***                eval      vindta  = '*' + %trim(vindta)
022500130917     C*
022600080527     C                   exsr      inzvar
022700100318     C                   exsr      normCSV
022800010605     C                   exsr      impvab
022900010601     C*
023000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
023100010605     C                   eval      depfil = VABLNP
023200010601     C                   exsr      repfil
023300010601     C                   if        depfil = invfil
023400021025     C                   if        vlrpoi = 999
023500030822     C                   move(P)   invfil        VABFGS
023600021025     C                   else
023700030822     C                   move(P)   vlrpoi        VABFGS
023800021025     C                   endif
023900010601     C*
024000010601     C                   exsr      PREELA
024100010601     C*
024200010604     C* Ebbene...
024300010604     C*
024400040119     C  N31              add       1             §CTROKVB
024500040119     C   32              add       1             §CTRMO
024600040119     C   31              add       1             §CTRNO
024700080418     C*
024800030822     C                   if        wGiro = 2
024900080513     C* Scrivo sempre il VAT
025000080513     C  N31              exsr      wrivat
025100110201     C                   exsr      chkvab
025200080513     C                   endif
025300010604     C*
025400010604     C                   if        *in31 = *off and
025500010604     C                             *in32 = *off
025600010604     C                   eval      vinflg = '1'
025700010604     C                   else
025800010604     C                   eval      vinflg = '2'
025900010604     C                   endif
026000010604     C                   endif
026100010601     C*
026200010604     C                   endif
026300010604     C*
026400010330     C                   else
026500010330     C                   eval      vinflg = '1'
026600010330     C                   endif
026700010601     C*
026800010601     C  N70              update    tivin000
026900010330     C*
027000030822     C  N70              enddo
027100040119     C*
027200080527     C* Verifico dati rimasti "in cannna"...
027300080527     C                   seton                                        65
027400110201     C                   exsr      chkvab
027500080527     C*
027600040119     C                   endif                                                  (endif - chkcall)
027700010601     C*
027800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027900010601     C                   if        cntNonEl = *zeros or
028000010601     C                             flgMulti = '0'
028100010330     C* Se non ci sono record con errori ...
028200010601     C                   if        §ctrno = 0 and
028300010604     C                             §ctrmo = 0 and
028400010601     C                             flgStato <> '2'
028500010330     C* ... restituisco esito OK.
028600010330     C                   eval      wrkesito = '0'
028700010330     C                   else
028800040119     C                   if        §ctrokvb > 0
028900010330     C                   eval      wrkesito = '1'
029000010330     C                   else
029100010615     C                   if        flgOk = '0'
029200010615     C                   eval      wrkesito = '2'
029300010615     C                   else
029400010615     C                   eval      wrkesito = '6'
029500010615     C                   endif
029600010330     C                   endif
029700010330     C                   endif
029800010601     C                   else
029900010601     C                   eval      wrkesito = '9'
030000010601     C                   endif
030100010330     C*
030200010330     C                   if        %open(tivin00r)
030300010330     C                   close     tivin00r
030400010330     C                   endif
030500021025     C                   if        %open(fivabwwr)
030600021025     C                   close     fivabwwr
030700010330     C                   endif
030800040512     C                   if        %open(fivatwwr)
030900040512     C                   close     fivatwwr
031000040512     C                   endif
031100010601     C*
031200010601     C                   if        vlrpoi <> 999
031300010601     C                   eval      invfil = vlrpoi
031400010601     C                   endif
031500010330     C*
031600040119     C                   if        §ctrokvb > 0
031700010601     C                             and invfil > *zeros
031800010330     C                   exsr      invio
031900010330     C                   endif
032000010601     C*
032100010618     C                   if        flgGiro = '1'
032200010601     C                   exsr      endela
032300010618     C                   endif
032400010330     C*
032500010330     C                   ENDSR
032600010330     C***
032700080527
032800080527
032900080527
033000080527     C*----------------------------------------------------*
033100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033200080527     C*----------------------------------------------------*
033300100413     C     CHKVAB        BEGSR
033400110201     C*
033500110201     C* Se richiesti ignorare colli / peso / volume
033600110315 xxx C***                if        *in35
033700110315 xxx C***                z-add     *zeros        VABNCL
033800110315 xxx C***                z-add     *zeros        VABPKB
033900110315 xxx C***                z-add     *zeros        VABVLB
034000110315 xxx C***                endif
034100080527     C*
034200080527     C* Scrivo il VAB solo a rottura d spedizione
034300080527     C                   if        depspe = *zeros
034400080527     C  N31              eval      fivabds_sav = fivabds
034500080527     C                   eval      depspe = vabNSP
034600080527     C                   else
034700080527     C                   if        depspe = vabNSP AND *in65 = *off
034800100413     C   51              add       VABNCL        sav_VABNCL
034900100413     C   51              add       VABPKB        sav_VABPKB
035000100413     C   51              add       VABVLB        sav_VABVLB
035100100720     C   52              z-add     VABNCL        sav_VABNCL
035200100720     C   52              z-add     VABPKB        sav_VABPKB
035300100720     C   52              z-add     VABVLB        sav_VABVLB
035400100720     C   53              add       VABNCL        sav_VABNCL
035500100720     C   53              z-add     VABPKB        sav_VABPKB
035600100720     C   53              z-add     VABVLB        sav_VABVLB
035700080527     C                   else
035800080527     C  N31              eval      fivabds_ok = fivabds_sav
035900080527     C  N31              exsr      wrivab
036000080527     C  N31              eval      fivabds_sav = fivabds
036100080527     C                   eval      depspe = vabNSP
036200080527     C                   endif
036300080527     C                   endif
036400080527     C*
036500080527     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037100040119     C*----------------------------------------------------*
037200040510     C     WRIVAB        BEGSR
037300080513     C*
037400080513     C* Gestisco l'eventuale rottura x numero spedizione
037500080527     C                   if        VABNSP <> depspe or *in65 = *on
037600070301     C                   if        wVATNOT_E <> *blanks
037700130610     C                   eval      ok_VABCTM = '7Q'
037800070301     C                   endif
037900100413     C                   write     fivab000
038000080513     C                   endif
038100040119     C*
038200040119     C                   ENDSR
038300040119
038400040119
038500040119
038600040119     C*----------------------------------------------------*
038700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038800040119     C*----------------------------------------------------*
038900040510     C     WRIVAT        BEGSR
039000060512     C*
039100060512     C* Inizializzo i campi chiave
039200060512     C                   eval      VATFGS = VABFGS
039300060512     C                   eval      VATAAS = VABAAS
039400060512     C                   eval      VATCCM = VABCCM
039500070823     C                   eval      VATNRS = VABNRS
039600060512     C                   eval      VATNSP = VABNSP
039700060512     C                   eval      VATLNP = VABLNP
039800040119     C*
039900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040000040510     C                   if        VATTRC = *blanks
040100040510     C                   eval      VATTRC = 'E'
040200040510     C                   endif
040300110318     C                   if        VATNOT <> *blanks
040400040510     C                   write     FIVAT000
040500040119     C                   add       1             §CTROKVT
040600110318     C                   endif
040700060331     C*
040800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040900060331     C                   if        wVATNOT_A <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_A
041100060331     C                   eval      VATTRC = 'A'
041200060331     C                   write     FIVAT000
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500060331     C*
041600060331     C                   if        wVATNOT_B <> *blanks
041700060331     C                   eval      VATNOT = wVATNOT_B
041800060331     C                   eval      VATTRC = 'B'
041900060331     C                   write     FIVAT000
042000060331     C                   add       1             §CTROKVT
042100060331     C                   endif
042200060331     C*
042300060331     C                   if        wVATNOT_E <> *blanks
042400110201     C                   eval      VATNOT = wVATNOT_E
042500060331     C                   eval      VATTRC = 'E'
042600140220     C* è un disk A, per cui niente barcode
042700120320     C                   endif
042800110927     C*
042900110927     C                   if        wVATNOT_I <> *blanks
043000110927     C                   eval      VATNOT = wVATNOT_I
043100110927     C                   eval      VATTRC = 'I'
043200110927     C                   write     FIVAT000
043300110927     C                   add       1             §CTROKVT
043400110927     C                   endif
043500140220     C*
043600140220     C                   if        wVATNOT_J <> *blanks
043700140220     C                   eval      VATNOT = wVATNOT_J
043800140220     C                   eval      VATTRC = 'J'
043900140220     C                   write     FIVAT000
044000140220     C                   add       1             §CTROKVT
044100140220     C                   endif
044200140319     C*
044300140319     C                   if        wVATNOT_S <> *blanks
044400140319     C                   eval      VATNOT = wVATNOT_S
044500140319     C* i 2 flag devono essere blank
044600140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
044700140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
044800140319     C                   eval      VATTRC = 'S'
044900140319     C                   write     FIVAT000
045000140319     C                   add       1             §CTROKVT
045100140319     C                   endif
045200040119     C*
045300040119     C                   ENDSR
045400040119
045500040119
045600040119
045700040119     C*----------------------------------------------------*
045800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045900040119     C*----------------------------------------------------*
046000040119     C     PREVAT        BEGSR
046100040119     C*
046200040119     C* Compongo il nome del membro da dare al FIVATWWR
046300040119     C                   eval      parmbr = vlrhdl
046400040119     C                   movel     'M'           parmbr
046500040510     C                   eval      parccm = vlrksc
046600040119     C                   eval      paropz = '1'
046700040119     C* Effettuo la chiamata al CLLE preposto
046800040119     C                   call(e)   'TITVVTC'
046900040119     C                   parm                    parccm
047000040119     C                   parm                    parmbr
047100040119     C                   parm                    paropz
047200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047300040119     C                   if        %error
047400040119     C                   movel     '1'           chkcall
047500040119     C                   else
047600040119     C                   movel     '0'           chkcall
047700040119     C                   endif
047800040119     C*
047900040119     C                   ENDSR
048000010601
048100010601
048200010601
048300010330     C*----------------------------------------------------*
048400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048500010330     C*----------------------------------------------------*
048600040119     C     INZVAR        BEGSR
048700080527     C*
048800080527     C                   setoff                                       65
048900010330     C*
049000030822     C* Inizializzo variabili di wrk
049100020204     C                   Z-ADD     *zeros        Num5_0            5 0
049200101018     C                   MOVEL     *blanks       wFlgCAS
049300060331     C                   MOVEL     *blanks       wVATNOT_A
049400060331     C                   MOVEL     *blanks       wVATNOT_B
049500060331     C                   MOVEL     *blanks       wVATNOT_E
049600110927     C                   MOVEL     *blanks       wVATNOT_I
049700140220     C                   MOVEL     *blanks       wVATNOT_J
049800140220     C                   MOVEL     *blanks       wVATNOT_IJ
049900140319     C                   MOVEL     *blanks       wVATNOT_S
050000120411     C                   MOVEL     *blanks       wNOT
050100120411     C                   MOVEL     *blanks       Data8A
050200030822     C*
050300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050400030822     C                   CLEAR                   SkSplitCSV
050500030822     C                   EVAL      i = 1
050600030822     C                   EVAL      posDa = *zeros
050700030822     C                   EVAL      posA  = *zeros
050800080527     C*
050900080527     C                   CLEAR                   FIVABDS
051000080527     C                   CLEAR                   FIVAT000
051100030822     C*
051200030822     C* Reimposto i valori di default
051300030822     C                   EXSR      DEFCAM
051400030822     C*
051500010330     C                   ENDSR
051600010330     C*----------------------------------------------------*
051700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051800010330     C*----------------------------------------------------*
051900010330     C     DEFCAM        BEGSR
052000030822     C*
052100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052200030715     C* e delimitatore testo.
052300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052600100318     C*
052700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052800100318     C                   EVAL      CharSOS = CharNUM
052900030822     C*
053000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053100100720     C                   SETOFF                                       505152
053200100720     C                   SETOFF                                       53
053300100413     C                   SELECT
053400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053500100720     C                   SETON                                        50
053600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053700100413     C                   SETON                                        51
053800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053900100720     C                   SETON                                        52
054000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054100100720     C                   SETON                                        53
054200100413     C                   ENDSL
054300030822     C*
054400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054500030822     C                   EVAL      posDaDft = 1
054600030822     C                   EVAL      posADft  = 0
054700030822     C                   EVAL      wGiroDft = 0
054800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054900030822     C                             posDaDft > 0
055000030822     C*
055100030822     C* Gestisco il 1° giro
055200030822     C                   IF        wGiroDft = 0
055300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055500030822     C* Incremento il contatore dei "giri"
055600030822     C                   EVAL      wGiroDft = 1
055700030822     C                   ELSE
055800030822     C                   EVAL      posDaDft = posADft
055900030822     C                   ENDIF
056000030822     C* Eseguo lo scan x trovare la fine del campo corrente
056100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056200030822     C*
056300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056500030822     C                   IF        posDaDft > 0 AND
056600030822     C                             posADft  > 0
056700030822     C* NCL
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'NCL'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABNCL
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* CCM
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'CCM'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABCCM
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* LNP
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'LNP'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABLNP
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* NRS
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'NRS'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABNRS
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* CTR
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'CTR'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABCTR
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* PKB
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'PKB'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiNum=*on
064200130610     C                   Z-ADD     PiVal         VABPKB
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* VLB
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'VLB'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500130610     C                   Z-ADD     PiVal         VABVLB
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* QFT
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'QFT'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800030822     C                   Z-ADD     PiVal         VABQFT
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* CBO
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'CBO'
067600030822     C                   EVAL      VABCBO=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* TSP
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'TSP'
068500030822     C                   EVAL      VABTSP=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* VAS
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'VAS'
069400030822     C                   EVAL      VABVAS=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* VCA
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'VCA'
070300030822     C                   EVAL      VABVCA=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700070925     C* TIC
070800070925     C                   IF        %subst(
070900070925     C                             %subst(vlrppt:posDaDft+1:
071000070925     C                             posADft-posDaDft-1):1:3)
071100070925     C                             = 'TIC'
071200070925     C                   EVAL      VABTIC=%trim(%subst(
071300070925     C                             %subst(vlrppt:posDaDft+1:
071400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500070925     C                   ENDIF
071600030822     C* GCA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'GCA'
072100030822     C                   EVAL      VABGCA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* CTM
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'CTM'
073000030822     C                   EVAL      VABCTM=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* FFD
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'FFD'
073900030822     C                   EVAL      VABFFD=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* VAD
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'VAD'
074800030822     C                   EVAL      VABVAD=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GMA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GMA'
075700030822     C                   EVAL      VABGMA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GGA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GGA'
076600030822     C                   EVAL      VABGGA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GVA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GVA'
077500030822     C                   EVAL      VABGVA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* TC1
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'TC1'
078400030822     C                   EVAL      VABTC1=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* TC2
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'TC2'
079300030822     C                   EVAL      VABTC2=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700040714     C* VATTRC
079800040714     C                   IF        %subst(
079900040714     C                             %subst(vlrppt:posDaDft+1:
080000040714     C                             posADft-posDaDft-1):1:3)
080100040714     C                             = 'TRC'
080200040714     C                   EVAL      VATTRC=%trim(%subst(
080300040714     C                             %subst(vlrppt:posDaDft+1:
080400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500040714     C                   ENDIF
080600030822     C* ...
080700030822     C                   ENDIF
080800030822     C                   ENDDO
080900020204     C*
081000010330     C                   ENDSR
081100010607     C*----------------------------------------------------*
081200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081300010607     C*----------------------------------------------------*
081400010607     C     IMPVAB        BEGSR
081500010607     C*
081600010607     C                   SETOFF                                       3132
081700110315 xxx C***                SETON                                        35
081800030515     C*
081900010607     C                   MOVEL     datcor        VABAAS
082000010607     C                   MOVE      datcor        VABMGS
082100010607     C*
082200030715     C**********
082300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082400030715     C**********
082500030715     C                   IF        wGiro = *zeros
082600040113     C*
082700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083000080429     C                             %trim(intcol(5))<>*blanks
083100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083300080429     C                                      %trim(intcol(5))
083400070823     C     ';':CharCSV   XLATE     vindta        vindta
083500040510     C                   ENDIF
083600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083700031201     C     minu:maiu     XLATE     vindta        vindta
083800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083900030715     C                   DOW       posDa <= %len(%trim(vindta))
084000030715     C*
084100030715     C* Gestisco il 1° campo
084200030715     C                   IF        i = 1
084300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084400030715     C                   EVAL      posDa = 1
084500030715     C* Eseguo lo scan x trovare la fine del primo campo
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C* X i campi successivi al 1°
085100030715     C                   ELSE
085200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085300030715     C                   EVAL      posDa = posA + 1
085400030715     C* Eseguo lo scan x trovare la fine del campo corrente
085500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085600030715     C* Gestisco l'ultimo campo
085700030715     C                   IF        posA = *zeros
085800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085900030715     C* A questo punto "estraggo" il campo corrente
086000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086100030715     C                                                    (posA-posDa))
086200030715     C                   LEAVE
086300030715     C                   ELSE
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C                   ENDIF
086800030715     C                   ENDIF
086900030715     C* Incremento il contatore d campo
087000030715     C                   EVAL      i = i +1
087100030715     C                   ENDDO
087200030715     C*
087300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087400030715     C* delimitazione testo
087500030715     C                   EVAL      i = 1
087600030715     C                   DOW       i < %elem(SkSplitFLD)
087700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087900030715     C                   EVAL      i = i + 1
088000030715     C                   ENDDO
088100030715     C                   EVAL      wGiro = 1
088200030715     C                   ELSE
088300030715     C                   EVAL      wGiro = 2
088400031204     C**********
088500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088600031204     C* d separatore campo
088700031204     C**********
088800031204     C                   EVAL      vindta = %trim(vindta)
088900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089000031204     C                   EVAL      vindta = %subst(vindta:2)
089100031204     C                   ENDDO
089200031204     C*
089300031204     C                   Z-ADD     *zeros        lunghInput        4 0
089400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089800110912     C                   ENDDO
089900030715     C**********
090000030715     C* Effettuo lo split del campo dati d input
090100030715     C**********
090200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090300030715     C                   DOW       posDa <= %len(%trim(vindta))
090400030715     C*
090500030715     C* Gestisco il 1° campo
090600030715     C                   IF        i = 1
090700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090800030715     C                   EVAL      posDa = 1
090900030715     C* Eseguo lo scan x trovare la fine del primo campo
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C* X i campi successivi al 1°
091500030715     C                   ELSE
091600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091700030715     C                   EVAL      posDa = posA + 1
091800031204     C* Verifico che nn vi sia il campo nn valorizzato
091900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092000031204     C* Se campo nn valorizzato skippo al prossimo
092100031204     C                   EVAL      SkSplitCSV(i) = *blanks
092200031204     C                   EVAL      posA  = posA + 1
092300031204     C                   ELSE
092400030715     C* Eseguo lo scan x trovare la fine del campo corrente
092500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092600030715     C* Gestisco l'ultimo campo
092700030715     C                   IF        posA = *zeros
092800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092900030715     C* A questo punto "estraggo" il campo corrente
093000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093100030715     C                                                    (posA-posDa))
093200030715     C                   LEAVE
093300030715     C                   ELSE
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C                   ENDIF
093800031204     C                   ENDIF
093900030715     C                   ENDIF
094000030715     C* Incremento il contatore d campo
094100030715     C                   EVAL      i = i +1
094200030715     C                   ENDDO
094300030715     C*
094400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094500030715     C* delimitazione testo
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitCSV)
094800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095000030715     C                   EVAL      i = i + 1
095100030715     C                   ENDDO
095200100720     C*
095300100720     C* Stacco un progressivo x numero spedizione
095400130917 xxx C                   EXSR      repNSP
095500100326     C*
095600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095700030715     C                   EVAL      i = 1
095800030715     C                   DOW       i < %elem(SkSplitFLD)
095900030715     C*
096000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096200030715     C                             SkSplitCSV(i) <> *blanks
096300030721     C*** ATB
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096500030721     C                   EVAL      VABATB = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** RSD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097200030715     C* ==
097300130528     C                   ENDIF
097400030721     C*** RD2
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** IND
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098000030715     C                   EVAL      VABIND = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** LOD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** PRD
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030721     C*** NZD
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030715     C*** RMA
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** NOT
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** NT2
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** VCA
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** VAS
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030715     C*** FFD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030721     C*** GC1
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GC2
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TSP
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102900030721     C                   ENDIF
103000040510     C*** CBO
103100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103300040510     C                   ENDIF
103400030721     C*** NAS
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** TIC
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GCA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** XCO
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** CTM
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TCR
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** CTS
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** FTM
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** VAD
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GMA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** GGA
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** GVA
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** TC1
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** TC2
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** SCL
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** RMO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** NMO
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*
110300030721     C* Reperisco quindi i campi numerici...
110400040510     C*** NSP
110500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110600040510     C                   EVAL      PiStr=SkSplitCSV(i)
110700040510     C                   EXSR      CHKNUM
110800040510     C                   IF        PiInt=*on
110900040510     C                   Z-ADD     PiVal         VABNSP
111000040510     C                   ELSE
111100040510     C                   SETON                                        31
111200040510     C                   EVAL      VABNSP = *zeros
111300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040510     C                             + ' ' + 'VABNSP'
111500040510     C                   ENDIF
111600040510     C                   ENDIF
111700071106     C*** RMN
111800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111900071106     C                   EVAL      PiStr=SkSplitCSV(i)
112000071106     C                   EXSR      CHKNUM
112100071106     C                   IF        PiInt=*on
112200071106     C                   Z-ADD     PiVal         VABRMN
112300071106     C                   ELSE
112400071106     C                   SETON                                        32
112500071106     C                   EVAL      VABRMN = *zeros
112600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112700071106     C                             + ' ' + 'VABRMN'
112800071106     C                   ENDIF
112900071106     C                   ENDIF
113000030721     C*** CAD
113100130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113200130610     C                   EVAL      PiStr=SkSplitCSV(i)
113300130610     C                   EXSR      CHKNUM
113400130610     C                   IF        PiInt=*on
113500130610     C                   Z-ADD     PiVal         Num5_0
113600130610     C                   MOVEL(p)  Num5_0        VABCAD
113700130610     C                   ELSE
113800130610     C                   SETON                                        32
113900130610     C                   EVAL      VABCAD = *zeros
114000130610     C                   EVAL      vinmsg = %trimr(vinmsg)
114100130610     C                             + ' ' + 'VABCAD'
114200130610     C                   ENDIF
114300130605     C                   ENDIF
114400030721     C*** NCL
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on
114900030721     C                   Z-ADD     PiVal         VABNCL
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABNCL = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400040122     C                             + ' ' + 'VABNCL'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** PKB
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiNum=*on
116200130610     C                   Z-ADD(H)  PiVal         VABPKB
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABPKB = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABPKB'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** CAS
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117400140513     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
117500140513     C                             %trim(SkSplitCSV(i)) <> '.00'
117600130703     C                   EVAL      VABVCA = 'EUR'
117700140513     C                   EVAL      wFlgCAS = '1'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiNum=*on
118100030721     C                   Z-ADD     PiVal         VABCAS
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABCAS = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABCAS'
118700030721     C                   ENDIF
118800140513     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CCM
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on      AND
119500030721     C                             PiVal<=9999999 AND
119600030721     C                             PiVal>*zeros
119700030721     C                   Z-ADD     PiVal         VABCCM
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABCCM = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040510     C                             + ' ' + 'VABCCM'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** LNP
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on    AND
121000030721     C                             PiVal<=999   AND
121100030721     C                             PiVal>*zeros
121200030721     C                   Z-ADD     PiVal         VABLNP
121300030721     C                   ELSE
121400040715     C                   SETON                                        32
121500030721     C                   EVAL      VABLNP = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700040510     C                             + ' ' + 'VABLNP'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** NRS
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on AND
122500030721     C                             PiVal<=99
122600030721     C                   Z-ADD     PiVal         VABNRS
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABNRS'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** LNA
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on    AND
123800030721     C                             PiVal<=999   AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABLNA
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      VABLNA = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABLNA'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** CTR
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on  AND
125300030721     C                             PiVal<=999
125400030721     C                   Z-ADD     PiVal         VABCTR
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABCTR'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** IAS
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiNum=*on
126600030721     C                   Z-ADD     PiVal         VABIAS
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABIAS'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** VLB
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiNum=*on
127800130610     C                   Z-ADD(H)  PiVal         VABVLB
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABVLB'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** QFT
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABQFT
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABQFT'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** NCD
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999
130300030721     C                   Z-ADD     PiVal         VABNCD
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABNCD'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** NCA
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on      AND
131500030721     C                             PiVal<=9999999
131600030721     C                   Z-ADD     PiVal         VABNCA
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABNCA'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** ZNC
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on AND
132800030721     C                             PiVal<=99
132900030721     C                   Z-ADD     PiVal         VABZNC
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABZNC'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** DCR
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on       AND
134100030721     C                             PiVal<=99999999
134200030721     C                   Z-ADD     PiVal         VABDCR
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABDCR'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** HCR
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on   AND
135400030721     C                             PiVal<=9999
135500030721     C                   Z-ADD     PiVal         VABHCR
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABHCR'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** VMD
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiNum=*on
136700030721     C                   Z-ADD     PiVal         VABVMD
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABVMD'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** ANT
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on        AND
137900030721     C                             PiVal<=999999999
138000030721     C                   Z-ADD     PiVal         VABANT
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABANT'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** CMO
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on
139200030721     C                   Z-ADD     PiVal         Num5_0
139300030721     C                   MOVEL(p)  Num5_0        VABCMO
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      VABCMO = *zeros
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABCMO'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100040510     C*
140200040510     C* Reperisco i campi del FIVAT
140300040510     C*** VATNOT
140400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140600040510     C                   ENDIF
140700040510     C*** VATTRC
140800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141000040510     C                   ENDIF
141100060331     C*** VATNOT_A
141200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141400060331     C                   ENDIF
141500060331     C*** VATNOT_B
141600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900060331     C*** VATNOT_E
142000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142200060331     C                   ENDIF
142300110927     C*** VATNOT_I
142400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142600110927     C                   ENDIF
142700140319     C*** VATNOT_S
142800140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
142900140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
143000140319     C                   ENDIF
143100070823     C*
143200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
143300140407
143400140513      *** W_VABIND_1: va concatenato, trimmando, a _2 e _3
143500140407     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
143600140407     C                   EVAL      wIND1 = %trim(SkSplitCSV(i))
143700140407     C                   ENDIF
143800140407
143900140513      *** W_VABIND_2: va concatenato, trimmando, a _1 e _3
144000140407     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
144100140407     C                   EVAL      wIND2 = %trim(SkSplitCSV(i))
144200140407     C                   ENDIF
144300140513
144400140513      *** W_VABIND_3: va concatenato, trimmando, a _1 e _2
144500140513     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_3'
144600140513     C                   EVAL      wIND3 = %trim(SkSplitCSV(i))
144700140513     C                   ENDIF
144800140522
144900140522      *** W_CADLOD: contiene il CAP nei primi 5 char e la località dopo
145000140522     C                   IF        %trim(SkSplitFLD(i)) = 'W_CADLOD'
145100140526     C                   EVAL      wCADLOD = %trim(SkSplitCSV(i))
145200140522     C                   EVAL      VABCAD = %subst(wCADLOD:1:5)
145300140522     C                   EVAL      VABLOD = %subst(wCADLOD:6:35)
145400140522     C                   ENDIF
145500070925     C*
145600071029     C***  <===  -----------------------------------------
145700040510     C*
145800030715     C                   ENDIF
145900030715     C                   EVAL      i = i + 1
146000030715     C                   ENDDO
146100070925     C*
146200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146300140319     C*
146400140522     C* il cliente adesso mette l'indirizzo solo nella colonna 2
146500140522     C***                EVAL      VABIND = %trim(wIND1) + ' ' + %trim(wIND2)
146600140522     C***                                                + ' ' + %trim(wIND3)
146700140522     C*
146800140522     C                   EVAL      VABIND = %trim(wIND2)
146900140407     C*
147000140407     C                   EVAL      VABRMN = %dec(%date : *iso)
147100070925     C*
147200071029     C***  <===  -----------------------------------------
147300070925     C*
147400070925     C* Considerazioni finali su CBO/CAS
147500140220     C                   IF        wFlgCAS = '1'    OR
147600140220     C                             VABCAS  > *zeros
147700140220     C                   IF        VABCBO = '1'
147800140220     C                   EVAL      VABCBO = '4'
147900140220     C                   ENDIF
148000140220     C                   IF        VABCBO = '2'
148100140220     C                   EVAL      VABCBO = '6'
148200140220     C                   ENDIF
148300140220     C                   ENDIF
148400070925     C***  <===  ----------------------------
148500040510     C*
148600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148700040510     C                   IF        VABNSP = *zeros
148800040510     C                   SETON                                        31
148900040510     C                   ENDIF
149000040715     C*
149100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149200040715     C                   IF        VABLNP = *zeros
149300040715     C                   SETON                                        31
149400040715     C                   ENDIF
149500040510     C*
149600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149700120411     C                   IF        VABRMN = *zeros
149800120411     C                   Z-ADD     VABNSP        VABRMN
149900120411     C                   ENDIF
150000120328     C* IN QUESTO CASO LO FORZO = 1
150100120411     C***                IF        VABRMN = *zeros
150200120411     C***                Z-ADD     1             VABRMN
150300120411     C***                ENDIF
150400040510     C*
150500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150600040510     C                   IF        VABRMA = *blanks
150700040714     C                   MOVEL     VABRMN        VABRMA
150800040510     C                   ENDIF
150900040128     C*
151000040510     C* Se provincia nn valorizzata la reperisco
151100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151200110912     C                   IF        VABPRD =  *blanks AND
151300110912     C                             VABNZD =  *blanks AND
151400110912     C                             VABCAD <> *blanks AND
151500110912     C                             VABLOD <> *blanks
151600040128     C                   CLEAR                   TISI95DS
151700040128     C                   EVAL      I95TCN = '3'
151800040128     C                   Z-ADD     datcor        I95DAT
151900040128     C                   EVAL      I95NAR = VABNZD
152000040128     C                   EVAL      I95CAP = VABCAD
152100040128     C                   EVAL      I95LOC = VABLOD
152200040128     C                   CALL      'TISI95R'
152300040128     C                   PARM                    TISI95DS
152400040128     C                   EVAL      VABPRD = O95PRV
152500040510     C                   ENDIF
152600020204     C*
152700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152800020204     C                   EXSR      CHKIMPDIV
152900030715     C                   ENDIF
153000020204     C*
153100010607     C                   ENDSR
153200010607     C*----------------------------------------------------*
153300010601
153400020204
153500020204     C*----------------------------------------------------*
153600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153700020204     C*----------------------------------------------------*
153800020204     C     CHKIMPDIV     BEGSR
153900020204     C*
154000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154100020204     C                   Z-ADD     *zeros        wrkDec            9 9
154200020204     C*
154300020204     C* Come prima cosa effettuo considerazioni sulla divisa
154400020204     C                   IF        vabIAS > *zeros
154500020204     C                   IF        vabVAS <> 'EUR'
154600020204     C                   EVAL      vabVAS =  'ITL'
154700020204     C                   ENDIF
154800020204     C                   ENDIF
154900020204     C*
155000020204     C                   IF        vabCAS > *zeros
155100020204     C                   IF        vabVCA <> 'EUR'
155200020204     C                   EVAL      vabVCA =  'ITL'
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C                   IF        vabVMD > *zeros
155700020204     C                   IF        vabVAD <> 'EUR'
155800020204     C                   EVAL      vabVAD =  'ITL'
155900020204     C                   ENDIF
156000020204     C                   ENDIF
156100020204     C*
156200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156300020204     C                   Z-ADD     vabIAS        wrkDec
156400020204     C                   IF        wrkDec > *zeros
156500020204     C                   IF        vabVAS = 'ITL'
156600020204     C                   EVAL      vabIAS = *zeros
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157100020204     C                   Z-ADD     vabCAS        wrkDec
157200020204     C                   IF        wrkDec > *zeros
157300020204     C                   IF        vabVCA = 'ITL'
157400020204     C                   EVAL      vabCAS = *zeros
157500020204     C                   ENDIF
157600020204     C                   ENDIF
157700020204     C*
157800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157900020204     C                   Z-ADD     vabVMD        wrkDec
158000020204     C                   IF        wrkDec > *zeros
158100020204     C                   IF        vabVAD = 'ITL'
158200020204     C                   EVAL      vabVMD = *zeros
158300020204     C                   ENDIF
158400020204     C                   ENDIF
158500020204     C*
158600020204     C                   ENDSR
158700020204     C***
158800010330
158900100318
159000100318
159100100318     C*----------------------------------------------------*
159200100318     C*  CONTROLLO NUMERICITA' CAMPI
159300100318     C*----------------------------------------------------*
159400100318     C     CHKNUM        BEGSR
159500100318     C*
159600100318     C                   IF        PiDecChr = *blanks
159700100318     C                   EVAL      PiDecChr = CharNUM
159800100318     C                   ENDIF
159900100318     C*
160000100318     C                   callp     UBISNUM_Check(PiStr
160100100318     C                                          :PiDecChr
160200100318     C                                          :PiVal
160300100318     C                                          :PiNum
160400100318     C                                          :PiInt)
160500100318     C*
160600100318     C                   ENDSR
160700100318     C***
160800010601
160900010601
161000010601
161100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161200010601     C     repfil        BEGSR
161300010601     C*
161400010601     C                   if        invfil = *zeros and
161500010601     C                             depfil > *zeros and
161600010629     C                             (vinflg = *blanks or
161700010629     C                              vinflg = *zeros)
161800010601     C
161900010601     C                   eval      invfil = depfil
162000010601     C                   endif
162100010601     C*
162200010601     C                   if        depfil <> invfil and
162300010601     C                             invfil > *zeros
162400010601     C                   eval      flgMulti = '1'
162500010601     C                   if        vinflg = *blanks
162600010601     C                   add       1             cntNonEl
162700010601     C                   endif
162800010601     C                   endif
162900010601     C*
163000010601     C                   if        vinflg = '2'
163100010601     C                   eval      flgStato = '2'
163200010601     C                   endif
163300010601     C*
163400010601     C                   ENDSR
163500010601     C***
163600010601
163700010601
163800010601
163900010330
164000040119      /TITLE Invio dei dati al punto operativo.
164100040119     C     invio         BEGSR
164200040119     C*
164300040119     C* 1° invio FIVAT
164400040119     C                   reset                   dscmz
164500040119     C                   move      vlrpoi        cmzdst
164600040119     C                   eval      cmzfld = 'FIVATWWR'
164700040119     C                   eval      cmzmbd = vlrhdl
164800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164900040119     C***                if        prmfir = *blanks
165000040119     C                   eval      cmzfla = 'FIVAT00F'
165100040119     C                   eval      cmzmba = 'FIVAT00F'
165200040119     C***                else
165300040119     C***                eval      cmzfla = prmfir
165400040119     C***                eval      cmzmba = prmfir
165500040119     C***                endif
165600040119     C                   eval      cmznrr = *zeros
165700040119     C                   move      §ctrokvt      cmznrr
165800040119     C                   eval      cmzlba = vlrfl1
165900040119     C                   call(e)   'TIS711C'
166000040119     C                   parm                    dscmz
166100040119     C                   parm      *blanks       esito
166200040119     C                   if        %error
166300040119     C                             or cmzerr = '1'
166400040119     C                             or esito  = '1'
166500040119     C                   eval      wrkesito = '3'
166600040119     C                   else
166700040119     C*
166800040119     C* 2° invio FIVAB
166900040119     C                   reset                   dscmz
167000040119     C                   move      vlrpoi        cmzdst
167100040119     C                   eval      cmzfld = vlrfou
167200040119     C                   eval      cmzmbd = vlrhdl
167300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167400040119     C***                if        prmfir = *blanks
167500040119     C                   eval      cmzfla = 'FIVAB00F'
167600040119     C                   eval      cmzmba = 'FIVAB00F'
167700040119     C***                else
167800040119     C***                eval      cmzfla = prmfir
167900040119     C***                eval      cmzmba = prmfir
168000040119     C***                endif
168100040119     C                   eval      cmznrr = *zeros
168200040119     C                   move      §ctrokvb      cmznrr
168300040119     C                   eval      cmzlba = vlrfl1
168400040119     C                   call(e)   'TIS711C'
168500040119     C                   parm                    dscmz
168600040119     C                   parm      *blanks       esito
168700040119     C                   if        %error
168800040119     C                             or cmzerr = '1'
168900040119     C                             or esito  = '1'
169000040119     C                   eval      wrkesito = '3'
169100040119     C                   endif
169200040119     C                   endif
169300040119     C*
169400040119     C                   ENDSR
169500040119     C***
169600010601
169700010601
169800010601
169900010601
170000010601      /TITLE Invio dei dati al punto operativo.
170100010601     C     opeini        BEGSR
170200010601     C*
170300010601     C* Inizializzo flag e contatori operativi
170400010601     C                   movel     '0'           flgGiro           1
170500010601     C                   movel     '0'           flgMulti          1
170600010601     C                   movel     '1'           flgStato          1
170700010615     C                   movel     '0'           flgOk             1
170800010601     C                   z-add     *zeros        cntNonEl         10 0
170900010601     C                   z-add     *zeros        depfil            3 0
171000010601     C                   z-add     *zeros        invfil            3 0
171100010601     C*
171200010601     C                   ENDSR
171300010601     C***
171400070326
171500070326
171600070326
171700070326
171800070326     C     *pssr         BEGSR
171900070329     C*
172000070329     C                   if        %open(tivin00r)
172100070329     C                   close     tivin00r
172200070329     C                   endif
172300070329     C                   if        %open(fivabwwr)
172400070329     C                   close     fivabwwr
172500070329     C                   endif
172600070329     C                   if        %open(fivatwwr)
172700070329     C                   close     fivatwwr
172800070329     C                   endif
172900070326     C*
173000070326     C* Effettuo la chiamata al CLLE preposto
173100070326     C                   call(e)   'TITVVTC'
173200070326     C                   parm                    parccm
173300070326     C                   parm                    parmbr
173400070326     C                   parm      '2'           paropz
173500070326     C*
173600070326     C                   eval      wrkesito = '2'
173700070404     C*
173800070404     C                   seton                                        LR
173900070326     C*
174000070326     C                   ENDSR     '*CANCL'
174100070326     C***
174200100318
174300100318
174400100318
174500100318     C     normCSV       BEGSR
174600100318     C*
174700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174800100318     C* separatori campi
174900100318     C                   if        CharTXT <> *blanks
175000100318     C                   z-add     *zeros        wLenVINDTA        4 0
175100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175200100318     C                   setoff                                       80
175300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175600100318     C                   if        not *in80
175700100318     C                   seton                                        80
175800100318     C                   else
175900100318     C                   setoff                                       80
176000100318     C                   endif
176100100318     C                   endif
176200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176300100318     C                             and *in80 = *on
176400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176500100318     C                   endif
176600100318     C                   enddo
176700100318     C                   endif
176800100318     C*
176900100318     C                   ENDSR
177000100318     C***
177100070326
177200100326
177300100326
177400100326     C*----------------------------------------------------*
177500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177600100326     C*----------------------------------------------------*
177700100326     C     REPNSP        BEGSR
177800100326     C*
177900100326     C* NSP => Stacco un numeratore da AZNUM
178000100326     C                   clear                   TRUL33DS
178100100326     C                   eval      I33OPE = *zeros
178200100326     C                   eval      I33CNU = 302
178300100326     C                   eval      I33NUM = 1
178400100326     C                   movel     TRUL33DS      KPJBU
178500100326     C                   call      'TRUL33R'
178600100326     C                   parm                    KPJBA
178700100326     C                   movel     KPJBU         TRUL33DS
178800100326     C                   if        O33ERR = *zeros
178900100326     C                   z-add     O33NRF        VABNSP
179000100326     C                   z-add     O33NRF        VATNSP
179100140305     C                   z-add     O33NRF        VABRMN
179200100326     C                   else
179300100326     C                   SETON                                        31
179400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179500140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
179600100326     C                   endif
179700100326     C*
179800100326     C                   ENDSR
179900010330
180000100326
180100010330
180200000613     C     *inzsr        BEGSR
180300990910     C*
180400990910     C     *entry        plist
180500990920     C                   parm                    tivlrds
180600990921     C                   parm      wrkesito      esito
180700000724     C                   parm                    prmlit
180800000710     C                   parm                    prmfir
180900010330     C*
181000010330     C* CALCOLA LA DATA CORRENTE
181100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181200100318     C                   eval      datcor = %dec(%date() : *ISO)
181300000613     C*
181400000613     C                   ENDSR
181500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181600140522X;X;X;X;X;X;VABRMA;X;X;X;X;X;VABRSD;VABLOD;X;VABPRD;W_VABIND_1;W_VABIND_2;W_VABIND_3;
181700140522W_CADLOD;VATNOT_B;VATNOT_A;VABPKB;X;VABNCL;VABVLB;X;X;VABCAS;X;X;X;X;X;VABNOT;VABNT2;
181800140512
181900111116
182000100318
