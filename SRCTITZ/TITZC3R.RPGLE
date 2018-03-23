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
007300120411     D wNOT            s             70
007400140407     D wIND1           s                   like(VABIND)
007500140407     D wIND2           s                   like(VABIND)
007600120411     D Data8A          s              8
007700030822     D*------------------
007800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007900030822     D*------------------
008000030822     D posDaDft        S              3  0 INZ(*zeros)
008100030822     D posADft         S              3  0 INZ(*zeros)
008200030822     D j               s              3  0 INZ(1)
008300030822     D wGiroDft        s              1  0 INZ(*zeros)
008400080418     D*------------------
008500080418     D* Costanti
008600080418     D*------------------
008700080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008800080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008900100318
009000100318     D*------------------
009100100318     D* LINKING A DEFINIZIONI ESTERNE
009200100318     D*------------------
009300100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009400100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009500100318
009600010330
009700010330
009800010330
009900000913     C                   reset                   rrnum
010000990921     C                   reset                   esito
010100990921     C                   reset                   wrkesito
010200010601     C*
010300010601     C                   exsr      opeini
010400010605     C                   exsr      rwvab
010500010601     C*
010600010601     C                   seton                                        lr
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011200010601     C*--------------------------------------------------------
011300010601     C     PREELA        BEGSR
011400010601     C*
011500010601     C* SE OCCORRE SPEDIRE IN FILIALE
011600010601     C                   if        invfil <> *zeros and
011700010601     C                             flgGiro = '0'
011800010601     C*
011900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
012000010601     C                   eval      flgGiro = '1'
012100010601     C*
012200010601     C                   endif
012300010601     C*
012400010601     C                   ENDSR
012500010601     C***
012600010601
012700010601
012800010601
012900010601
013000010601     C*--------------------------------------------------------
013100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013200010601     C*--------------------------------------------------------
013300010601     C     ENDELA        BEGSR
013400040119     C*
013500040119     C* Effettuo la chiamata al CLLE preposto
013600040119     C                   call(e)   'TITVVTC'
013700040510     C                   parm                    parccm
013800040119     C                   parm                    parmbr
013900040119     C                   parm      '2'           paropz
014000050201     C*
014100050201     C* Effettuo lancio TISI95 solo x chiusura
014200050201     C                   CLEAR                   TISI95DS
014300050201     C                   EVAL      I95TLA = 'C'
014400050201     C                   CALL      'TISI95R'
014500050201     C                   PARM                    TISI95DS
014600000616     C*
014700010601     C                   ENDSR
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800021025     C                   if        not %open(fivabwwr)
015900021025     C                   open      fivabwwr
016000010330     C                   endif
016100040119     C*
016200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016300040119     C                   exsr      prevat
016400040119     C*
016500040119     C                   if        chkcall = '0'
016600040119     C*
016700040119     C                   if        not %open(fivatwwr)
016800040119     C                   open      fivatwwr
016900040119     C                   endif
017000010330     C*
017100040119     C                   clear                   §CTROKVB          7 0
017200040119     C                   clear                   §CTROKVT          7 0
017300040119     C                   clear                   §CTRMO            7 0
017400040119     C                   clear                   §CTRNO            7 0
017500120410     C*
017600120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017700120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017800120410     C                   IF        Intestazioni = *off
017900120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
018000120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
018100120410     C                   exsr      inzvar
018200120410     C                   exsr      impvab
018300120410     C                   ENDIF
018400010330     C*
018500030822     C                   do        *HIVAL
018600010330     C*
018700030822     C                   read      tivin00r                               70
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300040510     C*
019400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019500040510     C                   z-add     *zeros        wLenVINDTA        4 0
019600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020000040510     C                   z-add     1             wFlgVINDTA
020100040510     C                   leave
020200040510     C                   endif
020300040510     C                   enddo
020400010618     C*
020500040510     C                   if        vindta > *blanks AND
020600040510     C                             wFlgVINDTA = 1
020700010330     C                   add       1             rrnum
020800010330     C*
020900010601     C                   if        *in70 = *off and
021000010330     C                             (vinflg = *blanks
021100010330     C                              or vinflg = '0'
021200010330     C                              or vinflg = '2')
021300010330     C*
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800130917     C*
021900130917     C* Aggiungo un byte a blanks all'inizio d ogni record
022000130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
022100130917     C* evitare che vengano elisi i blank
022200140220     C***                eval      vindta  = '*' + %trim(vindta)
022300130917     C*
022400080527     C                   exsr      inzvar
022500100318     C                   exsr      normCSV
022600010605     C                   exsr      impvab
022700010601     C*
022800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022900010605     C                   eval      depfil = VABLNP
023000010601     C                   exsr      repfil
023100010601     C                   if        depfil = invfil
023200021025     C                   if        vlrpoi = 999
023300030822     C                   move(P)   invfil        VABFGS
023400021025     C                   else
023500030822     C                   move(P)   vlrpoi        VABFGS
023600021025     C                   endif
023700010601     C*
023800010601     C                   exsr      PREELA
023900010601     C*
024000010604     C* Ebbene...
024100010604     C*
024200040119     C  N31              add       1             §CTROKVB
024300040119     C   32              add       1             §CTRMO
024400040119     C   31              add       1             §CTRNO
024500080418     C*
024600030822     C                   if        wGiro = 2
024700080513     C* Scrivo sempre il VAT
024800080513     C  N31              exsr      wrivat
024900110201     C                   exsr      chkvab
025000080513     C                   endif
025100010604     C*
025200010604     C                   if        *in31 = *off and
025300010604     C                             *in32 = *off
025400010604     C                   eval      vinflg = '1'
025500010604     C                   else
025600010604     C                   eval      vinflg = '2'
025700010604     C                   endif
025800010604     C                   endif
025900010601     C*
026000010604     C                   endif
026100010604     C*
026200010330     C                   else
026300010330     C                   eval      vinflg = '1'
026400010330     C                   endif
026500010601     C*
026600010601     C  N70              update    tivin000
026700010330     C*
026800030822     C  N70              enddo
026900040119     C*
027000080527     C* Verifico dati rimasti "in cannna"...
027100080527     C                   seton                                        65
027200110201     C                   exsr      chkvab
027300080527     C*
027400040119     C                   endif                                                  (endif - chkcall)
027500010601     C*
027600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027700010601     C                   if        cntNonEl = *zeros or
027800010601     C                             flgMulti = '0'
027900010330     C* Se non ci sono record con errori ...
028000010601     C                   if        §ctrno = 0 and
028100010604     C                             §ctrmo = 0 and
028200010601     C                             flgStato <> '2'
028300010330     C* ... restituisco esito OK.
028400010330     C                   eval      wrkesito = '0'
028500010330     C                   else
028600040119     C                   if        §ctrokvb > 0
028700010330     C                   eval      wrkesito = '1'
028800010330     C                   else
028900010615     C                   if        flgOk = '0'
029000010615     C                   eval      wrkesito = '2'
029100010615     C                   else
029200010615     C                   eval      wrkesito = '6'
029300010615     C                   endif
029400010330     C                   endif
029500010330     C                   endif
029600010601     C                   else
029700010601     C                   eval      wrkesito = '9'
029800010601     C                   endif
029900010330     C*
030000010330     C                   if        %open(tivin00r)
030100010330     C                   close     tivin00r
030200010330     C                   endif
030300021025     C                   if        %open(fivabwwr)
030400021025     C                   close     fivabwwr
030500010330     C                   endif
030600040512     C                   if        %open(fivatwwr)
030700040512     C                   close     fivatwwr
030800040512     C                   endif
030900010601     C*
031000010601     C                   if        vlrpoi <> 999
031100010601     C                   eval      invfil = vlrpoi
031200010601     C                   endif
031300010330     C*
031400040119     C                   if        §ctrokvb > 0
031500010601     C                             and invfil > *zeros
031600010330     C                   exsr      invio
031700010330     C                   endif
031800010601     C*
031900010618     C                   if        flgGiro = '1'
032000010601     C                   exsr      endela
032100010618     C                   endif
032200010330     C*
032300010330     C                   ENDSR
032400010330     C***
032500080527
032600080527
032700080527
032800080527     C*----------------------------------------------------*
032900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033000080527     C*----------------------------------------------------*
033100100413     C     CHKVAB        BEGSR
033200110201     C*
033300110201     C* Se richiesti ignorare colli / peso / volume
033400110315 xxx C***                if        *in35
033500110315 xxx C***                z-add     *zeros        VABNCL
033600110315 xxx C***                z-add     *zeros        VABPKB
033700110315 xxx C***                z-add     *zeros        VABVLB
033800110315 xxx C***                endif
033900080527     C*
034000080527     C* Scrivo il VAB solo a rottura d spedizione
034100080527     C                   if        depspe = *zeros
034200080527     C  N31              eval      fivabds_sav = fivabds
034300080527     C                   eval      depspe = vabNSP
034400080527     C                   else
034500080527     C                   if        depspe = vabNSP AND *in65 = *off
034600100413     C   51              add       VABNCL        sav_VABNCL
034700100413     C   51              add       VABPKB        sav_VABPKB
034800100413     C   51              add       VABVLB        sav_VABVLB
034900100720     C   52              z-add     VABNCL        sav_VABNCL
035000100720     C   52              z-add     VABPKB        sav_VABPKB
035100100720     C   52              z-add     VABVLB        sav_VABVLB
035200100720     C   53              add       VABNCL        sav_VABNCL
035300100720     C   53              z-add     VABPKB        sav_VABPKB
035400100720     C   53              z-add     VABVLB        sav_VABVLB
035500080527     C                   else
035600080527     C  N31              eval      fivabds_ok = fivabds_sav
035700080527     C  N31              exsr      wrivab
035800080527     C  N31              eval      fivabds_sav = fivabds
035900080527     C                   eval      depspe = vabNSP
036000080527     C                   endif
036100080527     C                   endif
036200080527     C*
036300080527     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036900040119     C*----------------------------------------------------*
037000040510     C     WRIVAB        BEGSR
037100080513     C*
037200080513     C* Gestisco l'eventuale rottura x numero spedizione
037300080527     C                   if        VABNSP <> depspe or *in65 = *on
037400070301     C                   if        wVATNOT_E <> *blanks
037500130610     C                   eval      ok_VABCTM = '7Q'
037600070301     C                   endif
037700100413     C                   write     fivab000
037800080513     C                   endif
037900040119     C*
038000040119     C                   ENDSR
038100040119
038200040119
038300040119
038400040119     C*----------------------------------------------------*
038500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038600040119     C*----------------------------------------------------*
038700040510     C     WRIVAT        BEGSR
038800060512     C*
038900060512     C* Inizializzo i campi chiave
039000060512     C                   eval      VATFGS = VABFGS
039100060512     C                   eval      VATAAS = VABAAS
039200060512     C                   eval      VATCCM = VABCCM
039300070823     C                   eval      VATNRS = VABNRS
039400060512     C                   eval      VATNSP = VABNSP
039500060512     C                   eval      VATLNP = VABLNP
039600040119     C*
039700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039800040510     C                   if        VATTRC = *blanks
039900040510     C                   eval      VATTRC = 'E'
040000040510     C                   endif
040100110318     C                   if        VATNOT <> *blanks
040200040510     C                   write     FIVAT000
040300040119     C                   add       1             §CTROKVT
040400110318     C                   endif
040500060331     C*
040600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040700060331     C                   if        wVATNOT_A <> *blanks
040800060331     C                   eval      VATNOT = wVATNOT_A
040900060331     C                   eval      VATTRC = 'A'
041000060331     C                   write     FIVAT000
041100060331     C                   add       1             §CTROKVT
041200060331     C                   endif
041300060331     C*
041400060331     C                   if        wVATNOT_B <> *blanks
041500060331     C                   eval      VATNOT = wVATNOT_B
041600060331     C                   eval      VATTRC = 'B'
041700060331     C                   write     FIVAT000
041800060331     C                   add       1             §CTROKVT
041900060331     C                   endif
042000060331     C*
042100060331     C                   if        wVATNOT_E <> *blanks
042200110201     C                   eval      VATNOT = wVATNOT_E
042300060331     C                   eval      VATTRC = 'E'
042400140220     C* è un disk A, per cui niente barcode
042500120320     C                   endif
042600110927     C*
042700110927     C                   if        wVATNOT_I <> *blanks
042800110927     C                   eval      VATNOT = wVATNOT_I
042900110927     C                   eval      VATTRC = 'I'
043000110927     C                   write     FIVAT000
043100110927     C                   add       1             §CTROKVT
043200110927     C                   endif
043300140220     C*
043400140220     C                   if        wVATNOT_J <> *blanks
043500140220     C                   eval      VATNOT = wVATNOT_J
043600140220     C                   eval      VATTRC = 'J'
043700140220     C                   write     FIVAT000
043800140220     C                   add       1             §CTROKVT
043900140220     C                   endif
044000140319     C*
044100140319     C                   if        wVATNOT_S <> *blanks
044200140319     C                   eval      VATNOT = wVATNOT_S
044300140319     C* i 2 flag devono essere blank
044400140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
044500140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
044600140319     C                   eval      VATTRC = 'S'
044700140319     C                   write     FIVAT000
044800140319     C                   add       1             §CTROKVT
044900140319     C                   endif
045000040119     C*
045100040119     C                   ENDSR
045200040119
045300040119
045400040119
045500040119     C*----------------------------------------------------*
045600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045700040119     C*----------------------------------------------------*
045800040119     C     PREVAT        BEGSR
045900040119     C*
046000040119     C* Compongo il nome del membro da dare al FIVATWWR
046100040119     C                   eval      parmbr = vlrhdl
046200040119     C                   movel     'M'           parmbr
046300040510     C                   eval      parccm = vlrksc
046400040119     C                   eval      paropz = '1'
046500040119     C* Effettuo la chiamata al CLLE preposto
046600040119     C                   call(e)   'TITVVTC'
046700040119     C                   parm                    parccm
046800040119     C                   parm                    parmbr
046900040119     C                   parm                    paropz
047000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047100040119     C                   if        %error
047200040119     C                   movel     '1'           chkcall
047300040119     C                   else
047400040119     C                   movel     '0'           chkcall
047500040119     C                   endif
047600040119     C*
047700040119     C                   ENDSR
047800010601
047900010601
048000010601
048100010330     C*----------------------------------------------------*
048200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048300010330     C*----------------------------------------------------*
048400040119     C     INZVAR        BEGSR
048500080527     C*
048600080527     C                   setoff                                       65
048700010330     C*
048800030822     C* Inizializzo variabili di wrk
048900020204     C                   Z-ADD     *zeros        Num5_0            5 0
049000101018     C                   MOVEL     *blanks       wFlgCAS
049100060331     C                   MOVEL     *blanks       wVATNOT_A
049200060331     C                   MOVEL     *blanks       wVATNOT_B
049300060331     C                   MOVEL     *blanks       wVATNOT_E
049400110927     C                   MOVEL     *blanks       wVATNOT_I
049500140220     C                   MOVEL     *blanks       wVATNOT_J
049600140220     C                   MOVEL     *blanks       wVATNOT_IJ
049700140319     C                   MOVEL     *blanks       wVATNOT_S
049800120411     C                   MOVEL     *blanks       wNOT
049900120411     C                   MOVEL     *blanks       Data8A
050000030822     C*
050100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050200030822     C                   CLEAR                   SkSplitCSV
050300030822     C                   EVAL      i = 1
050400030822     C                   EVAL      posDa = *zeros
050500030822     C                   EVAL      posA  = *zeros
050600080527     C*
050700080527     C                   CLEAR                   FIVABDS
050800080527     C                   CLEAR                   FIVAT000
050900030822     C*
051000030822     C* Reimposto i valori di default
051100030822     C                   EXSR      DEFCAM
051200030822     C*
051300010330     C                   ENDSR
051400010330     C*----------------------------------------------------*
051500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051600010330     C*----------------------------------------------------*
051700010330     C     DEFCAM        BEGSR
051800030822     C*
051900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052000030715     C* e delimitatore testo.
052100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052400100318     C*
052500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052600100318     C                   EVAL      CharSOS = CharNUM
052700030822     C*
052800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052900100720     C                   SETOFF                                       505152
053000100720     C                   SETOFF                                       53
053100100413     C                   SELECT
053200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053300100720     C                   SETON                                        50
053400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053500100413     C                   SETON                                        51
053600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053700100720     C                   SETON                                        52
053800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053900100720     C                   SETON                                        53
054000100413     C                   ENDSL
054100030822     C*
054200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054300030822     C                   EVAL      posDaDft = 1
054400030822     C                   EVAL      posADft  = 0
054500030822     C                   EVAL      wGiroDft = 0
054600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054700030822     C                             posDaDft > 0
054800030822     C*
054900030822     C* Gestisco il 1° giro
055000030822     C                   IF        wGiroDft = 0
055100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055300030822     C* Incremento il contatore dei "giri"
055400030822     C                   EVAL      wGiroDft = 1
055500030822     C                   ELSE
055600030822     C                   EVAL      posDaDft = posADft
055700030822     C                   ENDIF
055800030822     C* Eseguo lo scan x trovare la fine del campo corrente
055900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056000030822     C*
056100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056300030822     C                   IF        posDaDft > 0 AND
056400030822     C                             posADft  > 0
056500030822     C* NCL
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'NCL'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABNCL
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* CCM
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'CCM'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABCCM
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* LNP
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'LNP'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABLNP
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* NRS
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'NRS'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABNRS
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* CTR
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'CTR'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABCTR
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* PKB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'PKB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000130610     C                   Z-ADD     PiVal         VABPKB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* VLB
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'VLB'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300130610     C                   Z-ADD     PiVal         VABVLB
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* QFT
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'QFT'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABQFT
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* CBO
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CBO'
067400030822     C                   EVAL      VABCBO=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* TSP
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'TSP'
068300030822     C                   EVAL      VABTSP=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VAS
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VAS'
069200030822     C                   EVAL      VABVAS=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VCA'
070100030822     C                   EVAL      VABVCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500070925     C* TIC
070600070925     C                   IF        %subst(
070700070925     C                             %subst(vlrppt:posDaDft+1:
070800070925     C                             posADft-posDaDft-1):1:3)
070900070925     C                             = 'TIC'
071000070925     C                   EVAL      VABTIC=%trim(%subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300070925     C                   ENDIF
071400030822     C* GCA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GCA'
071900030822     C                   EVAL      VABGCA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* CTM
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'CTM'
072800030822     C                   EVAL      VABCTM=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* FFD
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'FFD'
073700030822     C                   EVAL      VABFFD=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* VAD
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'VAD'
074600030822     C                   EVAL      VABVAD=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GMA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GMA'
075500030822     C                   EVAL      VABGMA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GGA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GGA'
076400030822     C                   EVAL      VABGGA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* GVA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GVA'
077300030822     C                   EVAL      VABGVA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* TC1
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'TC1'
078200030822     C                   EVAL      VABTC1=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* TC2
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'TC2'
079100030822     C                   EVAL      VABTC2=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500040714     C* VATTRC
079600040714     C                   IF        %subst(
079700040714     C                             %subst(vlrppt:posDaDft+1:
079800040714     C                             posADft-posDaDft-1):1:3)
079900040714     C                             = 'TRC'
080000040714     C                   EVAL      VATTRC=%trim(%subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300040714     C                   ENDIF
080400030822     C* ...
080500030822     C                   ENDIF
080600030822     C                   ENDDO
080700020204     C*
080800010330     C                   ENDSR
080900010607     C*----------------------------------------------------*
081000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081100010607     C*----------------------------------------------------*
081200010607     C     IMPVAB        BEGSR
081300010607     C*
081400010607     C                   SETOFF                                       3132
081500110315 xxx C***                SETON                                        35
081600030515     C*
081700010607     C                   MOVEL     datcor        VABAAS
081800010607     C                   MOVE      datcor        VABMGS
081900010607     C*
082000030715     C**********
082100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082200030715     C**********
082300030715     C                   IF        wGiro = *zeros
082400040113     C*
082500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082800080429     C                             %trim(intcol(5))<>*blanks
082900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083100080429     C                                      %trim(intcol(5))
083200070823     C     ';':CharCSV   XLATE     vindta        vindta
083300040510     C                   ENDIF
083400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083500031201     C     minu:maiu     XLATE     vindta        vindta
083600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083700030715     C                   DOW       posDa <= %len(%trim(vindta))
083800030715     C*
083900030715     C* Gestisco il 1° campo
084000030715     C                   IF        i = 1
084100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084200030715     C                   EVAL      posDa = 1
084300030715     C* Eseguo lo scan x trovare la fine del primo campo
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C* X i campi successivi al 1°
084900030715     C                   ELSE
085000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085100030715     C                   EVAL      posDa = posA + 1
085200030715     C* Eseguo lo scan x trovare la fine del campo corrente
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* Gestisco l'ultimo campo
085500030715     C                   IF        posA = *zeros
085600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   LEAVE
086100030715     C                   ELSE
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   ENDIF
086600030715     C                   ENDIF
086700030715     C* Incremento il contatore d campo
086800030715     C                   EVAL      i = i +1
086900030715     C                   ENDDO
087000030715     C*
087100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087200030715     C* delimitazione testo
087300030715     C                   EVAL      i = 1
087400030715     C                   DOW       i < %elem(SkSplitFLD)
087500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087700030715     C                   EVAL      i = i + 1
087800030715     C                   ENDDO
087900030715     C                   EVAL      wGiro = 1
088000030715     C                   ELSE
088100030715     C                   EVAL      wGiro = 2
088200031204     C**********
088300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088400031204     C* d separatore campo
088500031204     C**********
088600031204     C                   EVAL      vindta = %trim(vindta)
088700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088800031204     C                   EVAL      vindta = %subst(vindta:2)
088900031204     C                   ENDDO
089000031204     C*
089100031204     C                   Z-ADD     *zeros        lunghInput        4 0
089200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089600110912     C                   ENDDO
089700030715     C**********
089800030715     C* Effettuo lo split del campo dati d input
089900030715     C**********
090000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090100030715     C                   DOW       posDa <= %len(%trim(vindta))
090200030715     C*
090300030715     C* Gestisco il 1° campo
090400030715     C                   IF        i = 1
090500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090600030715     C                   EVAL      posDa = 1
090700030715     C* Eseguo lo scan x trovare la fine del primo campo
090800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C* X i campi successivi al 1°
091300030715     C                   ELSE
091400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091500030715     C                   EVAL      posDa = posA + 1
091600031204     C* Verifico che nn vi sia il campo nn valorizzato
091700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091800031204     C* Se campo nn valorizzato skippo al prossimo
091900031204     C                   EVAL      SkSplitCSV(i) = *blanks
092000031204     C                   EVAL      posA  = posA + 1
092100031204     C                   ELSE
092200030715     C* Eseguo lo scan x trovare la fine del campo corrente
092300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092400030715     C* Gestisco l'ultimo campo
092500030715     C                   IF        posA = *zeros
092600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092700030715     C* A questo punto "estraggo" il campo corrente
092800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092900030715     C                                                    (posA-posDa))
093000030715     C                   LEAVE
093100030715     C                   ELSE
093200030715     C* A questo punto "estraggo" il campo corrente
093300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093400030715     C                                                    (posA-posDa))
093500030715     C                   ENDIF
093600031204     C                   ENDIF
093700030715     C                   ENDIF
093800030715     C* Incremento il contatore d campo
093900030715     C                   EVAL      i = i +1
094000030715     C                   ENDDO
094100030715     C*
094200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094300030715     C* delimitazione testo
094400030715     C                   EVAL      i = 1
094500030715     C                   DOW       i < %elem(SkSplitCSV)
094600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094800030715     C                   EVAL      i = i + 1
094900030715     C                   ENDDO
095000100720     C*
095100100720     C* Stacco un progressivo x numero spedizione
095200130917 xxx C                   EXSR      repNSP
095300100326     C*
095400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095500030715     C                   EVAL      i = 1
095600030715     C                   DOW       i < %elem(SkSplitFLD)
095700030715     C*
095800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096000030715     C                             SkSplitCSV(i) <> *blanks
096100030721     C*** ATB
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096300030721     C                   EVAL      VABATB = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030715     C*** RSD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097000030715     C* ==
097100130528     C                   ENDIF
097200030721     C*** RD2
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** IND
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097800030715     C                   EVAL      VABIND = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** LOD
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** PRD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030721     C*** NZD
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030715     C*** RMA
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** NOT
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NT2
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** VCA
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VAS
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** FFD
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030721     C*** GC1
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** GC2
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** TSP
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102700030721     C                   ENDIF
102800040510     C*** CBO
102900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103100040510     C                   ENDIF
103200030721     C*** NAS
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TIC
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GCA
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** XCO
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** CTM
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** TCR
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** CTS
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** FTM
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** VAD
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** GMA
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GGA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GVA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** TC1
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC2
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** SCL
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** RMO
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** NMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*
110100030721     C* Reperisco quindi i campi numerici...
110200040510     C*** NSP
110300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110400040510     C                   EVAL      PiStr=SkSplitCSV(i)
110500040510     C                   EXSR      CHKNUM
110600040510     C                   IF        PiInt=*on
110700040510     C                   Z-ADD     PiVal         VABNSP
110800040510     C                   ELSE
110900040510     C                   SETON                                        31
111000040510     C                   EVAL      VABNSP = *zeros
111100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111200040510     C                             + ' ' + 'VABNSP'
111300040510     C                   ENDIF
111400040510     C                   ENDIF
111500071106     C*** RMN
111600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111700071106     C                   EVAL      PiStr=SkSplitCSV(i)
111800071106     C                   EXSR      CHKNUM
111900071106     C                   IF        PiInt=*on
112000071106     C                   Z-ADD     PiVal         VABRMN
112100071106     C                   ELSE
112200071106     C                   SETON                                        32
112300071106     C                   EVAL      VABRMN = *zeros
112400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112500071106     C                             + ' ' + 'VABRMN'
112600071106     C                   ENDIF
112700071106     C                   ENDIF
112800030721     C*** CAD
112900130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113000130610     C                   EVAL      PiStr=SkSplitCSV(i)
113100130610     C                   EXSR      CHKNUM
113200130610     C                   IF        PiInt=*on
113300130610     C                   Z-ADD     PiVal         Num5_0
113400130610     C                   MOVEL(p)  Num5_0        VABCAD
113500130610     C                   ELSE
113600130610     C                   SETON                                        32
113700130610     C                   EVAL      VABCAD = *zeros
113800130610     C                   EVAL      vinmsg = %trimr(vinmsg)
113900130610     C                             + ' ' + 'VABCAD'
114000130610     C                   ENDIF
114100130605     C                   ENDIF
114200030721     C*** NCL
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on
114700030721     C                   Z-ADD     PiVal         VABNCL
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABNCL = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040122     C                             + ' ' + 'VABNCL'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** PKB
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiNum=*on
116000130610     C                   Z-ADD(H)  PiVal         VABPKB
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABPKB = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABPKB'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** CAS
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117000110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117100111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117200111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117300130703     C                   EVAL      VABVCA = 'EUR'
117400101018     C                   ENDIF
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiNum=*on
117800030721     C                   Z-ADD     PiVal         VABCAS
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      VABCAS = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABCAS'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** CCM
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on      AND
119100030721     C                             PiVal<=9999999 AND
119200030721     C                             PiVal>*zeros
119300030721     C                   Z-ADD     PiVal         VABCCM
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      VABCCM = *zeros
119700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119800040510     C                             + ' ' + 'VABCCM'
119900030721     C                   ENDIF
120000030721     C                   ENDIF
120100030721     C*** LNP
120200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiInt=*on    AND
120600030721     C                             PiVal<=999   AND
120700030721     C                             PiVal>*zeros
120800030721     C                   Z-ADD     PiVal         VABLNP
120900030721     C                   ELSE
121000040715     C                   SETON                                        32
121100030721     C                   EVAL      VABLNP = *zeros
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300040510     C                             + ' ' + 'VABLNP'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** NRS
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiInt=*on AND
122100030721     C                             PiVal<=99
122200030721     C                   Z-ADD     PiVal         VABNRS
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABNRS'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** LNA
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on    AND
123400030721     C                             PiVal<=999   AND
123500030721     C                             PiVal>*zeros
123600030721     C                   Z-ADD     PiVal         VABLNA
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      VABLNA = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABLNA'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** CTR
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on  AND
124900030721     C                             PiVal<=999
125000030721     C                   Z-ADD     PiVal         VABCTR
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABCTR'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** IAS
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiNum=*on
126200030721     C                   Z-ADD     PiVal         VABIAS
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABIAS'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** VLB
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiNum=*on
127400130610     C                   Z-ADD(H)  PiVal         VABVLB
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABVLB'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** QFT
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiNum=*on
128600030721     C                   Z-ADD     PiVal         VABQFT
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABQFT'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** NCD
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on      AND
129800030721     C                             PiVal<=9999999
129900030721     C                   Z-ADD     PiVal         VABNCD
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABNCD'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** NCA
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on      AND
131100030721     C                             PiVal<=9999999
131200030721     C                   Z-ADD     PiVal         VABNCA
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABNCA'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** ZNC
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on AND
132400030721     C                             PiVal<=99
132500030721     C                   Z-ADD     PiVal         VABZNC
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABZNC'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** DCR
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on       AND
133700030721     C                             PiVal<=99999999
133800030721     C                   Z-ADD     PiVal         VABDCR
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134200030721     C                             + ' ' + 'VABDCR'
134300030721     C                   ENDIF
134400030721     C                   ENDIF
134500030721     C*** HCR
134600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134700030721     C                   EVAL      PiStr=SkSplitCSV(i)
134800030721     C                   EXSR      CHKNUM
134900030721     C                   IF        PiInt=*on   AND
135000030721     C                             PiVal<=9999
135100030721     C                   Z-ADD     PiVal         VABHCR
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABHCR'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** VMD
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiNum=*on
136300030721     C                   Z-ADD     PiVal         VABVMD
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABVMD'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000030721     C*** ANT
137100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137200030721     C                   EVAL      PiStr=SkSplitCSV(i)
137300030721     C                   EXSR      CHKNUM
137400030721     C                   IF        PiInt=*on        AND
137500030721     C                             PiVal<=999999999
137600030721     C                   Z-ADD     PiVal         VABANT
137700030721     C                   ELSE
137800030721     C                   SETON                                        32
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABANT'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300030721     C*** CMO
138400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138500030721     C                   EVAL      PiStr=SkSplitCSV(i)
138600030721     C                   EXSR      CHKNUM
138700030721     C                   IF        PiInt=*on
138800030721     C                   Z-ADD     PiVal         Num5_0
138900030721     C                   MOVEL(p)  Num5_0        VABCMO
139000030721     C                   ELSE
139100030721     C                   SETON                                        32
139200030721     C                   EVAL      VABCMO = *zeros
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABCMO'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700040510     C*
139800040510     C* Reperisco i campi del FIVAT
139900040510     C*** VATNOT
140000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140200040510     C                   ENDIF
140300040510     C*** VATTRC
140400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140600040510     C                   ENDIF
140700060331     C*** VATNOT_A
140800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141000060331     C                   ENDIF
141100060331     C*** VATNOT_B
141200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141400060331     C                   ENDIF
141500060331     C*** VATNOT_E
141600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900110927     C*** VATNOT_I
142000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142200110927     C                   ENDIF
142300140319     C*** VATNOT_S
142400140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
142500140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
142600140319     C                   ENDIF
142700070823     C*
142800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142900120301
143000140407      *** W_VABCBO: se contiene 'ASSEGNATO' allora = '2'
143100150224     C                   IF        %trim(SkSplitFLD(i))
143200150224     C                              = 'W_VABCBO'
143300150224     C                   IF        %xlate(minu:maiu:%trim(SkSplitCSV(i)))
143400150224     C                              = 'ASSEGNATO'
143500140407     C                   EVAL      VABCBO = '2'
143600140407     C                   ELSE
143700140407     C                   EVAL      VABCBO = '1'
143800140220     C                   ENDIF
143900150224     C                   ENDIF
144000140407
144100140407      *** W_VABIND_1: va concatenato, trimmando, a _2
144200140407     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
144300140407     C                   EVAL      wIND1 = %trim(SkSplitCSV(i))
144400140407     C                   ENDIF
144500140407
144600140407      *** W_VABIND_2: va concatenato, trimmando, a _1
144700140407     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
144800140407     C                   EVAL      wIND2 = %trim(SkSplitCSV(i))
144900140407     C                   ENDIF
145000140319
145100140319      *** W_VABNOT: è sia VABNOT che VABNT2
145200140319     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
145300140319     C                   EVAL      wNOT = %trim(SkSplitCSV(i))
145400140319     C                   EVAL      VABNOT =
145500140319     C                              %subst(wNOT:1:35)
145600140319     C                   EVAL      VABNT2 =
145700140319     C                              %subst(wNOT:36:35)
145800140319     C                   ENDIF
145900150224
146000150224      *** W_TSP: a seconda del valore valorizza TC1 o TSP
146100150224     C                   IF        %trim(SkSplitFLD(i)) = 'W_TSP'
146200150224     C                   SELECT
146300150224     C                   WHEN      %trim(SkSplitCSV(i)) = 'A'
146400150224     C                   EVAL      VABTC1 = 'A'
146500150224     C                   WHEN      %trim(SkSplitCSV(i)) = 'P'
146600150224     C                   EVAL      VABTC1 = 'P'
146700150224     C                   WHEN      %trim(SkSplitCSV(i)) = 'E'
146800150224     C                   EVAL      VABTSP = 'E'
146900150224     C                   WHEN      %trim(SkSplitCSV(i)) = 'H'
147000150224     C                   EVAL      VABTSP = 'H'
147100150224     C                   ENDSL
147200150224     C                   ENDIF
147300070925     C*
147400071029     C***  <===  -----------------------------------------
147500040510     C*
147600030715     C                   ENDIF
147700030715     C                   EVAL      i = i + 1
147800030715     C                   ENDDO
147900070925     C*
148000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148100140319     C*
148200140407     C                   EVAL      VABIND = %trim(wIND1) + ' ' + %trim(wIND2)
148300140407     C*
148400140407     C                   EVAL      VABRMN = %dec(%date : *iso)
148500070925     C*
148600071029     C***  <===  -----------------------------------------
148700070925     C*
148800070925     C* Considerazioni finali su CBO/CAS
148900140220     C                   IF        wFlgCAS = '1'    OR
149000140220     C                             VABCAS  > *zeros
149100140220     C                   IF        VABCBO = '1'
149200140220     C                   EVAL      VABCBO = '4'
149300140220     C                   ENDIF
149400140220     C                   IF        VABCBO = '2'
149500140220     C                   EVAL      VABCBO = '6'
149600140220     C                   ENDIF
149700140220     C                   ENDIF
149800070925     C***  <===  ----------------------------
149900040510     C*
150000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
150100040510     C                   IF        VABNSP = *zeros
150200040510     C                   SETON                                        31
150300040510     C                   ENDIF
150400040715     C*
150500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150600040715     C                   IF        VABLNP = *zeros
150700040715     C                   SETON                                        31
150800040715     C                   ENDIF
150900040510     C*
151000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
151100120411     C                   IF        VABRMN = *zeros
151200120411     C                   Z-ADD     VABNSP        VABRMN
151300120411     C                   ENDIF
151400120328     C* IN QUESTO CASO LO FORZO = 1
151500120411     C***                IF        VABRMN = *zeros
151600120411     C***                Z-ADD     1             VABRMN
151700120411     C***                ENDIF
151800040510     C*
151900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
152000040510     C                   IF        VABRMA = *blanks
152100040714     C                   MOVEL     VABRMN        VABRMA
152200040510     C                   ENDIF
152300040128     C*
152400040510     C* Se provincia nn valorizzata la reperisco
152500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
152600110912     C                   IF        VABPRD =  *blanks AND
152700110912     C                             VABNZD =  *blanks AND
152800110912     C                             VABCAD <> *blanks AND
152900110912     C                             VABLOD <> *blanks
153000040128     C                   CLEAR                   TISI95DS
153100040128     C                   EVAL      I95TCN = '3'
153200040128     C                   Z-ADD     datcor        I95DAT
153300040128     C                   EVAL      I95NAR = VABNZD
153400040128     C                   EVAL      I95CAP = VABCAD
153500040128     C                   EVAL      I95LOC = VABLOD
153600040128     C                   CALL      'TISI95R'
153700040128     C                   PARM                    TISI95DS
153800040128     C                   EVAL      VABPRD = O95PRV
153900040510     C                   ENDIF
154000020204     C*
154100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
154200020204     C                   EXSR      CHKIMPDIV
154300030715     C                   ENDIF
154400020204     C*
154500010607     C                   ENDSR
154600010607     C*----------------------------------------------------*
154700010601
154800020204
154900020204     C*----------------------------------------------------*
155000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
155100020204     C*----------------------------------------------------*
155200020204     C     CHKIMPDIV     BEGSR
155300020204     C*
155400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
155500020204     C                   Z-ADD     *zeros        wrkDec            9 9
155600020204     C*
155700020204     C* Come prima cosa effettuo considerazioni sulla divisa
155800020204     C                   IF        vabIAS > *zeros
155900020204     C                   IF        vabVAS <> 'EUR'
156000020204     C                   EVAL      vabVAS =  'ITL'
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C                   IF        vabCAS > *zeros
156500020204     C                   IF        vabVCA <> 'EUR'
156600020204     C                   EVAL      vabVCA =  'ITL'
156700020204     C                   ENDIF
156800020204     C                   ENDIF
156900020204     C*
157000020204     C                   IF        vabVMD > *zeros
157100020204     C                   IF        vabVAD <> 'EUR'
157200020204     C                   EVAL      vabVAD =  'ITL'
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
157700020204     C                   Z-ADD     vabIAS        wrkDec
157800020204     C                   IF        wrkDec > *zeros
157900020204     C                   IF        vabVAS = 'ITL'
158000020204     C                   EVAL      vabIAS = *zeros
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
158500020204     C                   Z-ADD     vabCAS        wrkDec
158600020204     C                   IF        wrkDec > *zeros
158700020204     C                   IF        vabVCA = 'ITL'
158800020204     C                   EVAL      vabCAS = *zeros
158900020204     C                   ENDIF
159000020204     C                   ENDIF
159100020204     C*
159200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
159300020204     C                   Z-ADD     vabVMD        wrkDec
159400020204     C                   IF        wrkDec > *zeros
159500020204     C                   IF        vabVAD = 'ITL'
159600020204     C                   EVAL      vabVMD = *zeros
159700020204     C                   ENDIF
159800020204     C                   ENDIF
159900020204     C*
160000020204     C                   ENDSR
160100020204     C***
160200010330
160300100318
160400100318
160500100318     C*----------------------------------------------------*
160600100318     C*  CONTROLLO NUMERICITA' CAMPI
160700100318     C*----------------------------------------------------*
160800100318     C     CHKNUM        BEGSR
160900100318     C*
161000100318     C                   IF        PiDecChr = *blanks
161100100318     C                   EVAL      PiDecChr = CharNUM
161200100318     C                   ENDIF
161300100318     C*
161400100318     C                   callp     UBISNUM_Check(PiStr
161500100318     C                                          :PiDecChr
161600100318     C                                          :PiVal
161700100318     C                                          :PiNum
161800100318     C                                          :PiInt)
161900100318     C*
162000100318     C                   ENDSR
162100100318     C***
162200010601
162300010601
162400010601
162500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
162600010601     C     repfil        BEGSR
162700010601     C*
162800010601     C                   if        invfil = *zeros and
162900010601     C                             depfil > *zeros and
163000010629     C                             (vinflg = *blanks or
163100010629     C                              vinflg = *zeros)
163200010601     C
163300010601     C                   eval      invfil = depfil
163400010601     C                   endif
163500010601     C*
163600010601     C                   if        depfil <> invfil and
163700010601     C                             invfil > *zeros
163800010601     C                   eval      flgMulti = '1'
163900010601     C                   if        vinflg = *blanks
164000010601     C                   add       1             cntNonEl
164100010601     C                   endif
164200010601     C                   endif
164300010601     C*
164400010601     C                   if        vinflg = '2'
164500010601     C                   eval      flgStato = '2'
164600010601     C                   endif
164700010601     C*
164800010601     C                   ENDSR
164900010601     C***
165000010601
165100010601
165200010601
165300010330
165400040119      /TITLE Invio dei dati al punto operativo.
165500040119     C     invio         BEGSR
165600040119     C*
165700040119     C* 1° invio FIVAT
165800040119     C                   reset                   dscmz
165900040119     C                   move      vlrpoi        cmzdst
166000040119     C                   eval      cmzfld = 'FIVATWWR'
166100040119     C                   eval      cmzmbd = vlrhdl
166200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166300040119     C***                if        prmfir = *blanks
166400040119     C                   eval      cmzfla = 'FIVAT00F'
166500040119     C                   eval      cmzmba = 'FIVAT00F'
166600040119     C***                else
166700040119     C***                eval      cmzfla = prmfir
166800040119     C***                eval      cmzmba = prmfir
166900040119     C***                endif
167000040119     C                   eval      cmznrr = *zeros
167100040119     C                   move      §ctrokvt      cmznrr
167200040119     C                   eval      cmzlba = vlrfl1
167300040119     C                   call(e)   'TIS711C'
167400040119     C                   parm                    dscmz
167500040119     C                   parm      *blanks       esito
167600040119     C                   if        %error
167700040119     C                             or cmzerr = '1'
167800040119     C                             or esito  = '1'
167900040119     C                   eval      wrkesito = '3'
168000040119     C                   else
168100040119     C*
168200040119     C* 2° invio FIVAB
168300040119     C                   reset                   dscmz
168400040119     C                   move      vlrpoi        cmzdst
168500040119     C                   eval      cmzfld = vlrfou
168600040119     C                   eval      cmzmbd = vlrhdl
168700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168800040119     C***                if        prmfir = *blanks
168900040119     C                   eval      cmzfla = 'FIVAB00F'
169000040119     C                   eval      cmzmba = 'FIVAB00F'
169100040119     C***                else
169200040119     C***                eval      cmzfla = prmfir
169300040119     C***                eval      cmzmba = prmfir
169400040119     C***                endif
169500040119     C                   eval      cmznrr = *zeros
169600040119     C                   move      §ctrokvb      cmznrr
169700040119     C                   eval      cmzlba = vlrfl1
169800040119     C                   call(e)   'TIS711C'
169900040119     C                   parm                    dscmz
170000040119     C                   parm      *blanks       esito
170100040119     C                   if        %error
170200040119     C                             or cmzerr = '1'
170300040119     C                             or esito  = '1'
170400040119     C                   eval      wrkesito = '3'
170500040119     C                   endif
170600040119     C                   endif
170700040119     C*
170800040119     C                   ENDSR
170900040119     C***
171000010601
171100010601
171200010601
171300010601
171400010601      /TITLE Invio dei dati al punto operativo.
171500010601     C     opeini        BEGSR
171600010601     C*
171700010601     C* Inizializzo flag e contatori operativi
171800010601     C                   movel     '0'           flgGiro           1
171900010601     C                   movel     '0'           flgMulti          1
172000010601     C                   movel     '1'           flgStato          1
172100010615     C                   movel     '0'           flgOk             1
172200010601     C                   z-add     *zeros        cntNonEl         10 0
172300010601     C                   z-add     *zeros        depfil            3 0
172400010601     C                   z-add     *zeros        invfil            3 0
172500010601     C*
172600010601     C                   ENDSR
172700010601     C***
172800070326
172900070326
173000070326
173100070326
173200070326     C     *pssr         BEGSR
173300070329     C*
173400070329     C                   if        %open(tivin00r)
173500070329     C                   close     tivin00r
173600070329     C                   endif
173700070329     C                   if        %open(fivabwwr)
173800070329     C                   close     fivabwwr
173900070329     C                   endif
174000070329     C                   if        %open(fivatwwr)
174100070329     C                   close     fivatwwr
174200070329     C                   endif
174300070326     C*
174400070326     C* Effettuo la chiamata al CLLE preposto
174500070326     C                   call(e)   'TITVVTC'
174600070326     C                   parm                    parccm
174700070326     C                   parm                    parmbr
174800070326     C                   parm      '2'           paropz
174900070326     C*
175000070326     C                   eval      wrkesito = '2'
175100070404     C*
175200070404     C                   seton                                        LR
175300070326     C*
175400070326     C                   ENDSR     '*CANCL'
175500070326     C***
175600100318
175700100318
175800100318
175900100318     C     normCSV       BEGSR
176000100318     C*
176100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
176200100318     C* separatori campi
176300100318     C                   if        CharTXT <> *blanks
176400100318     C                   z-add     *zeros        wLenVINDTA        4 0
176500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
176600100318     C                   setoff                                       80
176700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
176800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
176900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
177000100318     C                   if        not *in80
177100100318     C                   seton                                        80
177200100318     C                   else
177300100318     C                   setoff                                       80
177400100318     C                   endif
177500100318     C                   endif
177600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
177700100318     C                             and *in80 = *on
177800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
177900100318     C                   endif
178000100318     C                   enddo
178100100318     C                   endif
178200100318     C*
178300100318     C                   ENDSR
178400100318     C***
178500070326
178600100326
178700100326
178800100326     C*----------------------------------------------------*
178900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
179000100326     C*----------------------------------------------------*
179100100326     C     REPNSP        BEGSR
179200100326     C*
179300100326     C* NSP => Stacco un numeratore da AZNUM
179400100326     C                   clear                   TRUL33DS
179500100326     C                   eval      I33OPE = *zeros
179600100326     C                   eval      I33CNU = 302
179700100326     C                   eval      I33NUM = 1
179800100326     C                   movel     TRUL33DS      KPJBU
179900100326     C                   call      'TRUL33R'
180000100326     C                   parm                    KPJBA
180100100326     C                   movel     KPJBU         TRUL33DS
180200100326     C                   if        O33ERR = *zeros
180300100326     C                   z-add     O33NRF        VABNSP
180400100326     C                   z-add     O33NRF        VATNSP
180500140305     C                   z-add     O33NRF        VABRMN
180600100326     C                   else
180700100326     C                   SETON                                        31
180800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
180900140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
181000100326     C                   endif
181100100326     C*
181200100326     C                   ENDSR
181300010330
181400100326
181500010330
181600000613     C     *inzsr        BEGSR
181700990910     C*
181800990910     C     *entry        plist
181900990920     C                   parm                    tivlrds
182000990921     C                   parm      wrkesito      esito
182100000724     C                   parm                    prmlit
182200000710     C                   parm                    prmfir
182300010330     C*
182400010330     C* CALCOLA LA DATA CORRENTE
182500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
182600100318     C                   eval      datcor = %dec(%date() : *ISO)
182700000613     C*
182800000613     C                   ENDSR
182900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183000150224X;X;X;VABRMA;X;X;W_TSP;X;X;X;VABNCL;VABPKB;X;X;W_VABCBO;X;VABRMO;X;X;X;X;X;X;X;X;X;VABRSD;
183100140407W_VABIND_1;W_VABIND_2;VABCAD;VABLOD;VABPRD;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
183200140407VABCAS;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;W_VABNOT;X;X;X;X;
183300111116
183400100318
