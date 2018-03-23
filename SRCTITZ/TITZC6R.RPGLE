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
004000140407     D Intestazioni    s               n   inz(*on)
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
007200120411     D wNOT            s             70
007300120411     D Data8A          s              8
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100080418     D*------------------
008200080418     D* Costanti
008300080418     D*------------------
008400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600100318
008700100318     D*------------------
008800100318     D* LINKING A DEFINIZIONI ESTERNE
008900100318     D*------------------
009000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200100318
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100040119     C*
013200040119     C* Effettuo la chiamata al CLLE preposto
013300040119     C                   call(e)   'TITVVTC'
013400040510     C                   parm                    parccm
013500040119     C                   parm                    parmbr
013600040119     C                   parm      '2'           paropz
013700050201     C*
013800050201     C* Effettuo lancio TISI95 solo x chiusura
013900050201     C                   CLEAR                   TISI95DS
014000050201     C                   EVAL      I95TLA = 'C'
014100050201     C                   CALL      'TISI95R'
014200050201     C                   PARM                    TISI95DS
014300000616     C*
014400010601     C                   ENDSR
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014900010330     C*--------------------------------------------------------
015000010605     C     RWVAB         BEGSR
015100010330     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500021025     C                   if        not %open(fivabwwr)
015600021025     C                   open      fivabwwr
015700010330     C                   endif
015800040119     C*
015900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000040119     C                   exsr      prevat
016100040119     C*
016200040119     C                   if        chkcall = '0'
016300040119     C*
016400040119     C                   if        not %open(fivatwwr)
016500040119     C                   open      fivatwwr
016600040119     C                   endif
016700010330     C*
016800040119     C                   clear                   §CTROKVB          7 0
016900040119     C                   clear                   §CTROKVT          7 0
017000040119     C                   clear                   §CTRMO            7 0
017100040119     C                   clear                   §CTRNO            7 0
017200120410     C*
017300120410     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017400120410     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017500120410     C                   IF        Intestazioni = *off
017600120410     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700120410     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800120410     C                   exsr      inzvar
017900120410     C                   exsr      impvab
018000120410     C                   ENDIF
018100010330     C*
018200030822     C                   do        *HIVAL
018300010330     C*
018400030822     C                   read      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000040510     C*
019100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019200040510     C                   z-add     *zeros        wLenVINDTA        4 0
019300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019700040510     C                   z-add     1             wFlgVINDTA
019800040510     C                   leave
019900040510     C                   endif
020000040510     C                   enddo
020100010618     C*
020200040510     C                   if        vindta > *blanks AND
020300040510     C                             wFlgVINDTA = 1
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500130917     C*
021600130917     C* Aggiungo un byte a blanks all'inizio d ogni record
021700130917     C* siccome sappiamo che la prima colonna è vuota, mettiamo un carattere non vuoto per
021800130917     C* evitare che vengano elisi i blank
021900140220     C***                eval      vindta  = '*' + %trim(vindta)
022000130917     C*
022100080527     C                   exsr      inzvar
022200100318     C                   exsr      normCSV
022300010605     C                   exsr      impvab
022400010601     C*
022500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600010605     C                   eval      depfil = VABLNP
022700010601     C                   exsr      repfil
022800010601     C                   if        depfil = invfil
022900021025     C                   if        vlrpoi = 999
023000030822     C                   move(P)   invfil        VABFGS
023100021025     C                   else
023200030822     C                   move(P)   vlrpoi        VABFGS
023300021025     C                   endif
023400010601     C*
023500010601     C                   exsr      PREELA
023600010601     C*
023700010604     C* Ebbene...
023800010604     C*
023900040119     C  N31              add       1             §CTROKVB
024000040119     C   32              add       1             §CTRMO
024100040119     C   31              add       1             §CTRNO
024200080418     C*
024300030822     C                   if        wGiro = 2
024400080513     C* Scrivo sempre il VAT
024500080513     C  N31              exsr      wrivat
024600110201     C                   exsr      chkvab
024700080513     C                   endif
024800010604     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600040119     C*
026700080527     C* Verifico dati rimasti "in cannna"...
026800080527     C                   seton                                        65
026900110201     C                   exsr      chkvab
027000080527     C*
027100040119     C                   endif                                                  (endif - chkcall)
027200010601     C*
027300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027400010601     C                   if        cntNonEl = *zeros or
027500010601     C                             flgMulti = '0'
027600010330     C* Se non ci sono record con errori ...
027700010601     C                   if        §ctrno = 0 and
027800010604     C                             §ctrmo = 0 and
027900010601     C                             flgStato <> '2'
028000010330     C* ... restituisco esito OK.
028100010330     C                   eval      wrkesito = '0'
028200010330     C                   else
028300040119     C                   if        §ctrokvb > 0
028400010330     C                   eval      wrkesito = '1'
028500010330     C                   else
028600010615     C                   if        flgOk = '0'
028700010615     C                   eval      wrkesito = '2'
028800010615     C                   else
028900010615     C                   eval      wrkesito = '6'
029000010615     C                   endif
029100010330     C                   endif
029200010330     C                   endif
029300010601     C                   else
029400010601     C                   eval      wrkesito = '9'
029500010601     C                   endif
029600010330     C*
029700010330     C                   if        %open(tivin00r)
029800010330     C                   close     tivin00r
029900010330     C                   endif
030000021025     C                   if        %open(fivabwwr)
030100021025     C                   close     fivabwwr
030200010330     C                   endif
030300040512     C                   if        %open(fivatwwr)
030400040512     C                   close     fivatwwr
030500040512     C                   endif
030600010601     C*
030700010601     C                   if        vlrpoi <> 999
030800010601     C                   eval      invfil = vlrpoi
030900010601     C                   endif
031000010330     C*
031100040119     C                   if        §ctrokvb > 0
031200010601     C                             and invfil > *zeros
031300010330     C                   exsr      invio
031400010330     C                   endif
031500010601     C*
031600010618     C                   if        flgGiro = '1'
031700010601     C                   exsr      endela
031800010618     C                   endif
031900010330     C*
032000010330     C                   ENDSR
032100010330     C***
032200080527
032300080527
032400080527
032500080527     C*----------------------------------------------------*
032600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032700080527     C*----------------------------------------------------*
032800100413     C     CHKVAB        BEGSR
032900110201     C*
033000110201     C* Se richiesti ignorare colli / peso / volume
033100110315 xxx C***                if        *in35
033200110315 xxx C***                z-add     *zeros        VABNCL
033300110315 xxx C***                z-add     *zeros        VABPKB
033400110315 xxx C***                z-add     *zeros        VABVLB
033500110315 xxx C***                endif
033600080527     C*
033700080527     C* Scrivo il VAB solo a rottura d spedizione
033800080527     C                   if        depspe = *zeros
033900080527     C  N31              eval      fivabds_sav = fivabds
034000080527     C                   eval      depspe = vabNSP
034100080527     C                   else
034200080527     C                   if        depspe = vabNSP AND *in65 = *off
034300100413     C   51              add       VABNCL        sav_VABNCL
034400100413     C   51              add       VABPKB        sav_VABPKB
034500100413     C   51              add       VABVLB        sav_VABVLB
034600100720     C   52              z-add     VABNCL        sav_VABNCL
034700100720     C   52              z-add     VABPKB        sav_VABPKB
034800100720     C   52              z-add     VABVLB        sav_VABVLB
034900100720     C   53              add       VABNCL        sav_VABNCL
035000100720     C   53              z-add     VABPKB        sav_VABPKB
035100100720     C   53              z-add     VABVLB        sav_VABVLB
035200080527     C                   else
035300080527     C  N31              eval      fivabds_ok = fivabds_sav
035400080527     C  N31              exsr      wrivab
035500080527     C  N31              eval      fivabds_sav = fivabds
035600080527     C                   eval      depspe = vabNSP
035700080527     C                   endif
035800080527     C                   endif
035900080527     C*
036000080527     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAB        BEGSR
036800080513     C*
036900080513     C* Gestisco l'eventuale rottura x numero spedizione
037000080527     C                   if        VABNSP <> depspe or *in65 = *on
037100070301     C                   if        wVATNOT_E <> *blanks
037200130610     C                   eval      ok_VABCTM = '7Q'
037300070301     C                   endif
037400100413     C                   write     fivab000
037500080513     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000070823     C                   eval      VATNRS = VABNRS
039100060512     C                   eval      VATNSP = VABNSP
039200060512     C                   eval      VATLNP = VABLNP
039300040119     C*
039400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039500040510     C                   if        VATTRC = *blanks
039600040510     C                   eval      VATTRC = 'E'
039700040510     C                   endif
039800110318     C                   if        VATNOT <> *blanks
039900040510     C                   write     FIVAT000
040000040119     C                   add       1             §CTROKVT
040100110318     C                   endif
040200060331     C*
040300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040400060331     C                   if        wVATNOT_A <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_A
040600060331     C                   eval      VATTRC = 'A'
040700060331     C                   write     FIVAT000
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_B <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_B
041300060331     C                   eval      VATTRC = 'B'
041400060331     C                   write     FIVAT000
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_E <> *blanks
041900060331     C                   eval      VATTRC = 'E'
042000140513     C*
042100140513     C* Sviluppati i "CHI SONO" in relazione al numero colli
042200140513     C*
042300140513     C     1             DO        VABNCL        wNumCollo         3 0
042400140513     C*** da analisi in fondo al barcode c'è un progressivo di 3 char che ccontiene gli zeri non
042500140513     C*** significativi, relativo al numero colli
042600140513     C                   EVAL      %subst(wVATNOT_E :
042700140513     C                               %scan(' ':wVATNOT_E) - 3 : 3)  =
042800140513     C                             %editc(wNumCollo:'X')
042900140513     C                   eval      VATNOT = wVATNOT_E
043000140513     C                   write     FIVAT000
043100140513     C                   add       1             §CTROKVT
043200140513     C                   ENDDO
043300140513     C*
043400120320     C                   endif
043500110927     C*
043600110927     C                   if        wVATNOT_I <> *blanks
043700110927     C                   eval      VATNOT = wVATNOT_I
043800110927     C                   eval      VATTRC = 'I'
043900110927     C                   write     FIVAT000
044000110927     C                   add       1             §CTROKVT
044100110927     C                   endif
044200140220     C*
044300140220     C                   if        wVATNOT_J <> *blanks
044400140220     C                   eval      VATNOT = wVATNOT_J
044500140220     C                   eval      VATTRC = 'J'
044600140220     C                   write     FIVAT000
044700140220     C                   add       1             §CTROKVT
044800140220     C                   endif
044900140319     C*
045000140319     C                   if        wVATNOT_S <> *blanks
045100140319     C                   eval      VATNOT = wVATNOT_S
045200140319     C* i 2 flag devono essere blank
045300140319     C                   EVAL      %subst(VATNOT:17:1)=*blank
045400140319     C                   EVAL      %subst(VATNOT:18:1)=*blank
045500140319     C                   eval      VATTRC = 'S'
045600140319     C                   write     FIVAT000
045700140319     C                   add       1             §CTROKVT
045800140319     C                   endif
045900040119     C*
046000040119     C                   ENDSR
046100040119
046200040119
046300040119
046400040119     C*----------------------------------------------------*
046500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046600040119     C*----------------------------------------------------*
046700040119     C     PREVAT        BEGSR
046800040119     C*
046900040119     C* Compongo il nome del membro da dare al FIVATWWR
047000040119     C                   eval      parmbr = vlrhdl
047100040119     C                   movel     'M'           parmbr
047200040510     C                   eval      parccm = vlrksc
047300040119     C                   eval      paropz = '1'
047400040119     C* Effettuo la chiamata al CLLE preposto
047500040119     C                   call(e)   'TITVVTC'
047600040119     C                   parm                    parccm
047700040119     C                   parm                    parmbr
047800040119     C                   parm                    paropz
047900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000040119     C                   if        %error
048100040119     C                   movel     '1'           chkcall
048200040119     C                   else
048300040119     C                   movel     '0'           chkcall
048400040119     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700010601
048800010601
048900010601
049000010330     C*----------------------------------------------------*
049100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049200010330     C*----------------------------------------------------*
049300040119     C     INZVAR        BEGSR
049400080527     C*
049500080527     C                   setoff                                       65
049600010330     C*
049700030822     C* Inizializzo variabili di wrk
049800020204     C                   Z-ADD     *zeros        Num5_0            5 0
049900101018     C                   MOVEL     *blanks       wFlgCAS
050000060331     C                   MOVEL     *blanks       wVATNOT_A
050100060331     C                   MOVEL     *blanks       wVATNOT_B
050200060331     C                   MOVEL     *blanks       wVATNOT_E
050300110927     C                   MOVEL     *blanks       wVATNOT_I
050400140220     C                   MOVEL     *blanks       wVATNOT_J
050500140220     C                   MOVEL     *blanks       wVATNOT_IJ
050600140319     C                   MOVEL     *blanks       wVATNOT_S
050700120411     C                   MOVEL     *blanks       wNOT
050800120411     C                   MOVEL     *blanks       Data8A
050900030822     C*
051000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051100030822     C                   CLEAR                   SkSplitCSV
051200030822     C                   EVAL      i = 1
051300030822     C                   EVAL      posDa = *zeros
051400030822     C                   EVAL      posA  = *zeros
051500080527     C*
051600080527     C                   CLEAR                   FIVABDS
051700080527     C                   CLEAR                   FIVAT000
051800030822     C*
051900030822     C* Reimposto i valori di default
052000030822     C                   EXSR      DEFCAM
052100030822     C*
052200010330     C                   ENDSR
052300010330     C*----------------------------------------------------*
052400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052500010330     C*----------------------------------------------------*
052600010330     C     DEFCAM        BEGSR
052700030822     C*
052800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052900030715     C* e delimitatore testo.
053000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053300100318     C*
053400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053500100318     C                   EVAL      CharSOS = CharNUM
053600030822     C*
053700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053800100720     C                   SETOFF                                       505152
053900100720     C                   SETOFF                                       53
054000100413     C                   SELECT
054100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
054200100720     C                   SETON                                        50
054300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
054400100413     C                   SETON                                        51
054500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054600100720     C                   SETON                                        52
054700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054800100720     C                   SETON                                        53
054900100413     C                   ENDSL
055000030822     C*
055100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055200030822     C                   EVAL      posDaDft = 1
055300030822     C                   EVAL      posADft  = 0
055400030822     C                   EVAL      wGiroDft = 0
055500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055600030822     C                             posDaDft > 0
055700030822     C*
055800030822     C* Gestisco il 1° giro
055900030822     C                   IF        wGiroDft = 0
056000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056200030822     C* Incremento il contatore dei "giri"
056300030822     C                   EVAL      wGiroDft = 1
056400030822     C                   ELSE
056500030822     C                   EVAL      posDaDft = posADft
056600030822     C                   ENDIF
056700030822     C* Eseguo lo scan x trovare la fine del campo corrente
056800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056900030822     C*
057000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057200030822     C                   IF        posDaDft > 0 AND
057300030822     C                             posADft  > 0
057400030822     C* NCL
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'NCL'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABNCL
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* CCM
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'CCM'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABCCM
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* LNP
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'LNP'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABLNP
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* NRS
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'NRS'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABNRS
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* CTR
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'CTR'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABCTR
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* PKB
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'PKB'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900130610     C                   Z-ADD     PiVal         VABPKB
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* VLB
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'VLB'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200130610     C                   Z-ADD     PiVal         VABVLB
066300030822     C                   ENDIF
066400030822     C                   ENDIF
066500030822     C* QFT
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'QFT'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500030822     C                   Z-ADD     PiVal         VABQFT
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* CBO
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'CBO'
068300030822     C                   EVAL      VABCBO=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* TSP
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'TSP'
069200030822     C                   EVAL      VABTSP=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* VAS
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'VAS'
070100030822     C                   EVAL      VABVAS=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* VCA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'VCA'
071000030822     C                   EVAL      VABVCA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400070925     C* TIC
071500070925     C                   IF        %subst(
071600070925     C                             %subst(vlrppt:posDaDft+1:
071700070925     C                             posADft-posDaDft-1):1:3)
071800070925     C                             = 'TIC'
071900070925     C                   EVAL      VABTIC=%trim(%subst(
072000070925     C                             %subst(vlrppt:posDaDft+1:
072100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200070925     C                   ENDIF
072300030822     C* GCA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GCA'
072800030822     C                   EVAL      VABGCA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* CTM
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'CTM'
073700030822     C                   EVAL      VABCTM=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* FFD
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'FFD'
074600030822     C                   EVAL      VABFFD=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* VAD
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'VAD'
075500030822     C                   EVAL      VABVAD=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* GMA
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'GMA'
076400030822     C                   EVAL      VABGMA=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* GGA
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'GGA'
077300030822     C                   EVAL      VABGGA=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700030822     C* GVA
077800030822     C                   IF        %subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):1:3)
078100030822     C                             = 'GVA'
078200030822     C                   EVAL      VABGVA=%trim(%subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500030822     C                   ENDIF
078600030822     C* TC1
078700030822     C                   IF        %subst(
078800030822     C                             %subst(vlrppt:posDaDft+1:
078900030822     C                             posADft-posDaDft-1):1:3)
079000030822     C                             = 'TC1'
079100030822     C                   EVAL      VABTC1=%trim(%subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079400030822     C                   ENDIF
079500030822     C* TC2
079600030822     C                   IF        %subst(
079700030822     C                             %subst(vlrppt:posDaDft+1:
079800030822     C                             posADft-posDaDft-1):1:3)
079900030822     C                             = 'TC2'
080000030822     C                   EVAL      VABTC2=%trim(%subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080300030822     C                   ENDIF
080400040714     C* VATTRC
080500040714     C                   IF        %subst(
080600040714     C                             %subst(vlrppt:posDaDft+1:
080700040714     C                             posADft-posDaDft-1):1:3)
080800040714     C                             = 'TRC'
080900040714     C                   EVAL      VATTRC=%trim(%subst(
081000040714     C                             %subst(vlrppt:posDaDft+1:
081100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081200040714     C                   ENDIF
081300030822     C* ...
081400030822     C                   ENDIF
081500030822     C                   ENDDO
081600020204     C*
081700010330     C                   ENDSR
081800010607     C*----------------------------------------------------*
081900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
082000010607     C*----------------------------------------------------*
082100010607     C     IMPVAB        BEGSR
082200010607     C*
082300010607     C                   SETOFF                                       3132
082400110315 xxx C***                SETON                                        35
082500030515     C*
082600010607     C                   MOVEL     datcor        VABAAS
082700010607     C                   MOVE      datcor        VABMGS
082800010607     C*
082900030715     C**********
083000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083100030715     C**********
083200030715     C                   IF        wGiro = *zeros
083300040113     C*
083400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083700080429     C                             %trim(intcol(5))<>*blanks
083800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
084000080429     C                                      %trim(intcol(5))
084100070823     C     ';':CharCSV   XLATE     vindta        vindta
084200040510     C                   ENDIF
084300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084400031201     C     minu:maiu     XLATE     vindta        vindta
084500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084600030715     C                   DOW       posDa <= %len(%trim(vindta))
084700030715     C*
084800030715     C* Gestisco il 1° campo
084900030715     C                   IF        i = 1
085000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085100030715     C                   EVAL      posDa = 1
085200030715     C* Eseguo lo scan x trovare la fine del primo campo
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* A questo punto "estraggo" il campo corrente
085500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085600030715     C                                                    (posA-posDa))
085700030715     C* X i campi successivi al 1°
085800030715     C                   ELSE
085900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086000030715     C                   EVAL      posDa = posA + 1
086100030715     C* Eseguo lo scan x trovare la fine del campo corrente
086200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086300030715     C* Gestisco l'ultimo campo
086400030715     C                   IF        posA = *zeros
086500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086600030715     C* A questo punto "estraggo" il campo corrente
086700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086800030715     C                                                    (posA-posDa))
086900030715     C                   LEAVE
087000030715     C                   ELSE
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C                   ENDIF
087500030715     C                   ENDIF
087600030715     C* Incremento il contatore d campo
087700030715     C                   EVAL      i = i +1
087800030715     C                   ENDDO
087900030715     C*
088000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088100030715     C* delimitazione testo
088200030715     C                   EVAL      i = 1
088300030715     C                   DOW       i < %elem(SkSplitFLD)
088400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088600030715     C                   EVAL      i = i + 1
088700030715     C                   ENDDO
088800030715     C                   EVAL      wGiro = 1
088900030715     C                   ELSE
089000030715     C                   EVAL      wGiro = 2
089100031204     C**********
089200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089300031204     C* d separatore campo
089400031204     C**********
089500031204     C                   EVAL      vindta = %trim(vindta)
089600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089700031204     C                   EVAL      vindta = %subst(vindta:2)
089800031204     C                   ENDDO
089900031204     C*
090000031204     C                   Z-ADD     *zeros        lunghInput        4 0
090100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090500110912     C                   ENDDO
090600030715     C**********
090700030715     C* Effettuo lo split del campo dati d input
090800030715     C**********
090900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091000030715     C                   DOW       posDa <= %len(%trim(vindta))
091100030715     C*
091200030715     C* Gestisco il 1° campo
091300030715     C                   IF        i = 1
091400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091500030715     C                   EVAL      posDa = 1
091600030715     C* Eseguo lo scan x trovare la fine del primo campo
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C* X i campi successivi al 1°
092200030715     C                   ELSE
092300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092400030715     C                   EVAL      posDa = posA + 1
092500031204     C* Verifico che nn vi sia il campo nn valorizzato
092600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092700031204     C* Se campo nn valorizzato skippo al prossimo
092800031204     C                   EVAL      SkSplitCSV(i) = *blanks
092900031204     C                   EVAL      posA  = posA + 1
093000031204     C                   ELSE
093100030715     C* Eseguo lo scan x trovare la fine del campo corrente
093200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093300030715     C* Gestisco l'ultimo campo
093400030715     C                   IF        posA = *zeros
093500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093600030715     C* A questo punto "estraggo" il campo corrente
093700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093800030715     C                                                    (posA-posDa))
093900030715     C                   LEAVE
094000030715     C                   ELSE
094100030715     C* A questo punto "estraggo" il campo corrente
094200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094300030715     C                                                    (posA-posDa))
094400030715     C                   ENDIF
094500031204     C                   ENDIF
094600030715     C                   ENDIF
094700030715     C* Incremento il contatore d campo
094800030715     C                   EVAL      i = i +1
094900030715     C                   ENDDO
095000030715     C*
095100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095200030715     C* delimitazione testo
095300030715     C                   EVAL      i = 1
095400030715     C                   DOW       i < %elem(SkSplitCSV)
095500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095700030715     C                   EVAL      i = i + 1
095800030715     C                   ENDDO
095900100720     C*
096000100720     C* Stacco un progressivo x numero spedizione
096100130917 xxx C                   EXSR      repNSP
096200100326     C*
096300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096400030715     C                   EVAL      i = 1
096500030715     C                   DOW       i < %elem(SkSplitFLD)
096600030715     C*
096700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096900030715     C                             SkSplitCSV(i) <> *blanks
097000030721     C*** ATB
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
097200030721     C                   EVAL      VABATB = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RSD
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097900030715     C* ==
098000130528     C                   ENDIF
098100030721     C*** RD2
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
098300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030715     C*** IND
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098700030715     C                   EVAL      VABIND = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** LOD
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
099100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** PRD
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030721     C*** NZD
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030715     C*** RMA
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
100300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** NOT
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030715     C*** NT2
101000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
101100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
101200030715     C                   ENDIF
101300030715     C*** VCA
101400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101600030715     C                   ENDIF
101700030715     C*** VAS
101800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
102000030715     C                   ENDIF
102100030715     C*** FFD
102200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
102300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
102400030715     C                   ENDIF
102500030721     C*** GC1
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** GC2
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
103100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** TSP
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103600030721     C                   ENDIF
103700040510     C*** CBO
103800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
104000040510     C                   ENDIF
104100030721     C*** NAS
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
104300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TIC
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** GCA
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
105100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** XCO
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** CTM
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** TCR
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
106300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** CTS
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** FTM
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
107100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** VAD
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** GMA
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** GGA
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
108300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** GVA
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** TC1
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
109100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*** TC2
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109600030721     C                   ENDIF
109700030721     C*** SCL
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
110000030721     C                   ENDIF
110100030721     C*** RMO
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
110300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
110400030721     C                   ENDIF
110500030721     C*** NMO
110600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110800030721     C                   ENDIF
110900030721     C*
111000030721     C* Reperisco quindi i campi numerici...
111100040510     C*** NSP
111200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
111300040510     C                   EVAL      PiStr=SkSplitCSV(i)
111400040510     C                   EXSR      CHKNUM
111500040510     C                   IF        PiInt=*on
111600040510     C                   Z-ADD     PiVal         VABNSP
111700040510     C                   ELSE
111800040510     C                   SETON                                        31
111900040510     C                   EVAL      VABNSP = *zeros
112000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
112100040510     C                             + ' ' + 'VABNSP'
112200040510     C                   ENDIF
112300040510     C                   ENDIF
112400071106     C*** RMN
112500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112600071106     C                   EVAL      PiStr=SkSplitCSV(i)
112700071106     C                   EXSR      CHKNUM
112800071106     C                   IF        PiInt=*on
112900071106     C                   Z-ADD     PiVal         VABRMN
113000071106     C                   ELSE
113100071106     C                   SETON                                        32
113200071106     C                   EVAL      VABRMN = *zeros
113300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
113400071106     C                             + ' ' + 'VABRMN'
113500071106     C                   ENDIF
113600071106     C                   ENDIF
113700030721     C*** CAD
113800130605     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113900130610     C                   EVAL      PiStr=SkSplitCSV(i)
114000130610     C                   EXSR      CHKNUM
114100130610     C                   IF        PiInt=*on
114200130610     C                   Z-ADD     PiVal         Num5_0
114300130610     C                   MOVEL(p)  Num5_0        VABCAD
114400130610     C                   ELSE
114500130610     C                   SETON                                        32
114600130610     C                   EVAL      VABCAD = *zeros
114700130610     C                   EVAL      vinmsg = %trimr(vinmsg)
114800130610     C                             + ' ' + 'VABCAD'
114900130610     C                   ENDIF
115000130605     C                   ENDIF
115100030721     C*** NCL
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on
115600030721     C                   Z-ADD     PiVal         VABNCL
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABNCL = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100040122     C                             + ' ' + 'VABNCL'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** PKB
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiNum=*on
116900130610     C                   Z-ADD(H)  PiVal         VABPKB
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABPKB = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABPKB'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CAS
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
118000111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
118100140513     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
118200140513     C                             %trim(SkSplitCSV(i)) <> '.00'
118300130703     C                   EVAL      VABVCA = 'EUR'
118400140513     C                   EVAL      wFlgCAS = '1'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABCAS
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCAS = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABCAS'
119400030721     C                   ENDIF
119500140513     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** CCM
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on      AND
120200030721     C                             PiVal<=9999999 AND
120300030721     C                             PiVal>*zeros
120400030721     C                   Z-ADD     PiVal         VABCCM
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABCCM = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900040510     C                             + ' ' + 'VABCCM'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** LNP
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on    AND
121700030721     C                             PiVal<=999   AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABLNP
122000030721     C                   ELSE
122100040715     C                   SETON                                        32
122200030721     C                   EVAL      VABLNP = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400040510     C                             + ' ' + 'VABLNP'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** NRS
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on AND
123200030721     C                             PiVal<=99
123300030721     C                   Z-ADD     PiVal         VABNRS
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABNRS'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** LNA
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiInt=*on    AND
124500030721     C                             PiVal<=999   AND
124600030721     C                             PiVal>*zeros
124700030721     C                   Z-ADD     PiVal         VABLNA
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      VABLNA = *zeros
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABLNA'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** CTR
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on  AND
126000030721     C                             PiVal<=999
126100030721     C                   Z-ADD     PiVal         VABCTR
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABCTR'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** IAS
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiNum=*on
127300030721     C                   Z-ADD     PiVal         VABIAS
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABIAS'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** VLB
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500130610     C                   Z-ADD(H)  PiVal         VABVLB
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABVLB'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** QFT
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiNum=*on
129700030721     C                   Z-ADD     PiVal         VABQFT
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABQFT'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** NCD
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiInt=*on      AND
130900030721     C                             PiVal<=9999999
131000030721     C                   Z-ADD     PiVal         VABNCD
131100030721     C                   ELSE
131200030721     C                   SETON                                        32
131300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131400030721     C                             + ' ' + 'VABNCD'
131500030721     C                   ENDIF
131600030721     C                   ENDIF
131700030721     C*** NCA
131800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131900030721     C                   EVAL      PiStr=SkSplitCSV(i)
132000030721     C                   EXSR      CHKNUM
132100030721     C                   IF        PiInt=*on      AND
132200030721     C                             PiVal<=9999999
132300030721     C                   Z-ADD     PiVal         VABNCA
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABNCA'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** ZNC
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on AND
133500030721     C                             PiVal<=99
133600030721     C                   Z-ADD     PiVal         VABZNC
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABZNC'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** DCR
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on       AND
134800030721     C                             PiVal<=99999999
134900030721     C                   Z-ADD     PiVal         VABDCR
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABDCR'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** HCR
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on   AND
136100030721     C                             PiVal<=9999
136200030721     C                   Z-ADD     PiVal         VABHCR
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABHCR'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** VMD
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiNum=*on
137400030721     C                   Z-ADD     PiVal         VABVMD
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABVMD'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** ANT
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiInt=*on        AND
138600030721     C                             PiVal<=999999999
138700030721     C                   Z-ADD     PiVal         VABANT
138800030721     C                   ELSE
138900030721     C                   SETON                                        32
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABANT'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400030721     C*** CMO
139500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
139600030721     C                   EVAL      PiStr=SkSplitCSV(i)
139700030721     C                   EXSR      CHKNUM
139800030721     C                   IF        PiInt=*on
139900030721     C                   Z-ADD     PiVal         Num5_0
140000030721     C                   MOVEL(p)  Num5_0        VABCMO
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      VABCMO = *zeros
140400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140500030721     C                             + ' ' + 'VABCMO'
140600030721     C                   ENDIF
140700030721     C                   ENDIF
140800040510     C*
140900040510     C* Reperisco i campi del FIVAT
141000040510     C*** VATNOT
141100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
141200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
141300040510     C                   ENDIF
141400040510     C*** VATTRC
141500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
141600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141700040510     C                   ENDIF
141800060331     C*** VATNOT_A
141900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
142000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
142100060331     C                   ENDIF
142200060331     C*** VATNOT_B
142300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
142400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
142500060331     C                   ENDIF
142600060331     C*** VATNOT_E
142700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142900060331     C                   ENDIF
143000110927     C*** VATNOT_I
143100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
143200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
143300110927     C                   ENDIF
143400140319     C*** VATNOT_S
143500140319     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
143600140319     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
143700140319     C                   ENDIF
143800070823     C*
143900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
144000140513
144100140513     C*** W_VATNOT_E
144200140513     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
144300140513     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144400140513     C                   ENDIF
144500070925     C*
144600071029     C***  <===  -----------------------------------------
144700040510     C*
144800030715     C                   ENDIF
144900030715     C                   EVAL      i = i + 1
145000030715     C                   ENDDO
145100070925     C*
145200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145300140319     C*
145400070925     C*
145500071029     C***  <===  -----------------------------------------
145600070925     C*
145700070925     C* Considerazioni finali su CBO/CAS
145800140220     C                   IF        wFlgCAS = '1'    OR
145900140220     C                             VABCAS  > *zeros
146000140220     C                   IF        VABCBO = '1'
146100140220     C                   EVAL      VABCBO = '4'
146200140220     C                   ENDIF
146300140220     C                   IF        VABCBO = '2'
146400140220     C                   EVAL      VABCBO = '6'
146500140220     C                   ENDIF
146600140220     C                   ENDIF
146700070925     C***  <===  ----------------------------
146800040510     C*
146900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147000040510     C                   IF        VABNSP = *zeros
147100040510     C                   SETON                                        31
147200040510     C                   ENDIF
147300040715     C*
147400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147500040715     C                   IF        VABLNP = *zeros
147600040715     C                   SETON                                        31
147700040715     C                   ENDIF
147800040510     C*
147900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148000120411     C                   IF        VABRMN = *zeros
148100120411     C                   Z-ADD     VABNSP        VABRMN
148200120411     C                   ENDIF
148300120328     C* IN QUESTO CASO LO FORZO = 1
148400120411     C***                IF        VABRMN = *zeros
148500120411     C***                Z-ADD     1             VABRMN
148600120411     C***                ENDIF
148700040510     C*
148800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148900040510     C                   IF        VABRMA = *blanks
149000040714     C                   MOVEL     VABRMN        VABRMA
149100040510     C                   ENDIF
149200040128     C*
149300040510     C* Se provincia nn valorizzata la reperisco
149400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149500110912     C                   IF        VABPRD =  *blanks AND
149600110912     C                             VABNZD =  *blanks AND
149700110912     C                             VABCAD <> *blanks AND
149800110912     C                             VABLOD <> *blanks
149900040128     C                   CLEAR                   TISI95DS
150000040128     C                   EVAL      I95TCN = '3'
150100040128     C                   Z-ADD     datcor        I95DAT
150200040128     C                   EVAL      I95NAR = VABNZD
150300040128     C                   EVAL      I95CAP = VABCAD
150400040128     C                   EVAL      I95LOC = VABLOD
150500040128     C                   CALL      'TISI95R'
150600040128     C                   PARM                    TISI95DS
150700040128     C                   EVAL      VABPRD = O95PRV
150800040510     C                   ENDIF
150900020204     C*
151000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151100020204     C                   EXSR      CHKIMPDIV
151200030715     C                   ENDIF
151300020204     C*
151400010607     C                   ENDSR
151500010607     C*----------------------------------------------------*
151600010601
151700020204
151800020204     C*----------------------------------------------------*
151900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152000020204     C*----------------------------------------------------*
152100020204     C     CHKIMPDIV     BEGSR
152200020204     C*
152300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152400020204     C                   Z-ADD     *zeros        wrkDec            9 9
152500020204     C*
152600020204     C* Come prima cosa effettuo considerazioni sulla divisa
152700020204     C                   IF        vabIAS > *zeros
152800020204     C                   IF        vabVAS <> 'EUR'
152900020204     C                   EVAL      vabVAS =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C                   IF        vabCAS > *zeros
153400020204     C                   IF        vabVCA <> 'EUR'
153500020204     C                   EVAL      vabVCA =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabVMD > *zeros
154000020204     C                   IF        vabVAD <> 'EUR'
154100020204     C                   EVAL      vabVAD =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154600020204     C                   Z-ADD     vabIAS        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVAS = 'ITL'
154900020204     C                   EVAL      vabIAS = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155400020204     C                   Z-ADD     vabCAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVCA = 'ITL'
155700020204     C                   EVAL      vabCAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156200020204     C                   Z-ADD     vabVMD        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAD = 'ITL'
156500020204     C                   EVAL      vabVMD = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C                   ENDSR
157000020204     C***
157100010330
157200100318
157300100318
157400100318     C*----------------------------------------------------*
157500100318     C*  CONTROLLO NUMERICITA' CAMPI
157600100318     C*----------------------------------------------------*
157700100318     C     CHKNUM        BEGSR
157800100318     C*
157900100318     C                   IF        PiDecChr = *blanks
158000100318     C                   EVAL      PiDecChr = CharNUM
158100100318     C                   ENDIF
158200100318     C*
158300100318     C                   callp     UBISNUM_Check(PiStr
158400100318     C                                          :PiDecChr
158500100318     C                                          :PiVal
158600100318     C                                          :PiNum
158700100318     C                                          :PiInt)
158800100318     C*
158900100318     C                   ENDSR
159000100318     C***
159100010601
159200010601
159300010601
159400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159500010601     C     repfil        BEGSR
159600010601     C*
159700010601     C                   if        invfil = *zeros and
159800010601     C                             depfil > *zeros and
159900010629     C                             (vinflg = *blanks or
160000010629     C                              vinflg = *zeros)
160100010601     C
160200010601     C                   eval      invfil = depfil
160300010601     C                   endif
160400010601     C*
160500010601     C                   if        depfil <> invfil and
160600010601     C                             invfil > *zeros
160700010601     C                   eval      flgMulti = '1'
160800010601     C                   if        vinflg = *blanks
160900010601     C                   add       1             cntNonEl
161000010601     C                   endif
161100010601     C                   endif
161200010601     C*
161300010601     C                   if        vinflg = '2'
161400010601     C                   eval      flgStato = '2'
161500010601     C                   endif
161600010601     C*
161700010601     C                   ENDSR
161800010601     C***
161900010601
162000010601
162100010601
162200010330
162300040119      /TITLE Invio dei dati al punto operativo.
162400040119     C     invio         BEGSR
162500040119     C*
162600040119     C* 1° invio FIVAT
162700040119     C                   reset                   dscmz
162800040119     C                   move      vlrpoi        cmzdst
162900040119     C                   eval      cmzfld = 'FIVATWWR'
163000040119     C                   eval      cmzmbd = vlrhdl
163100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163200040119     C***                if        prmfir = *blanks
163300040119     C                   eval      cmzfla = 'FIVAT00F'
163400040119     C                   eval      cmzmba = 'FIVAT00F'
163500040119     C***                else
163600040119     C***                eval      cmzfla = prmfir
163700040119     C***                eval      cmzmba = prmfir
163800040119     C***                endif
163900040119     C                   eval      cmznrr = *zeros
164000040119     C                   move      §ctrokvt      cmznrr
164100040119     C                   eval      cmzlba = vlrfl1
164200040119     C                   call(e)   'TIS711C'
164300040119     C                   parm                    dscmz
164400040119     C                   parm      *blanks       esito
164500040119     C                   if        %error
164600040119     C                             or cmzerr = '1'
164700040119     C                             or esito  = '1'
164800040119     C                   eval      wrkesito = '3'
164900040119     C                   else
165000040119     C*
165100040119     C* 2° invio FIVAB
165200040119     C                   reset                   dscmz
165300040119     C                   move      vlrpoi        cmzdst
165400040119     C                   eval      cmzfld = vlrfou
165500040119     C                   eval      cmzmbd = vlrhdl
165600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165700040119     C***                if        prmfir = *blanks
165800040119     C                   eval      cmzfla = 'FIVAB00F'
165900040119     C                   eval      cmzmba = 'FIVAB00F'
166000040119     C***                else
166100040119     C***                eval      cmzfla = prmfir
166200040119     C***                eval      cmzmba = prmfir
166300040119     C***                endif
166400040119     C                   eval      cmznrr = *zeros
166500040119     C                   move      §ctrokvb      cmznrr
166600040119     C                   eval      cmzlba = vlrfl1
166700040119     C                   call(e)   'TIS711C'
166800040119     C                   parm                    dscmz
166900040119     C                   parm      *blanks       esito
167000040119     C                   if        %error
167100040119     C                             or cmzerr = '1'
167200040119     C                             or esito  = '1'
167300040119     C                   eval      wrkesito = '3'
167400040119     C                   endif
167500040119     C                   endif
167600040119     C*
167700040119     C                   ENDSR
167800040119     C***
167900010601
168000010601
168100010601
168200010601
168300010601      /TITLE Invio dei dati al punto operativo.
168400010601     C     opeini        BEGSR
168500010601     C*
168600010601     C* Inizializzo flag e contatori operativi
168700010601     C                   movel     '0'           flgGiro           1
168800010601     C                   movel     '0'           flgMulti          1
168900010601     C                   movel     '1'           flgStato          1
169000010615     C                   movel     '0'           flgOk             1
169100010601     C                   z-add     *zeros        cntNonEl         10 0
169200010601     C                   z-add     *zeros        depfil            3 0
169300010601     C                   z-add     *zeros        invfil            3 0
169400010601     C*
169500010601     C                   ENDSR
169600010601     C***
169700070326
169800070326
169900070326
170000070326
170100070326     C     *pssr         BEGSR
170200070329     C*
170300070329     C                   if        %open(tivin00r)
170400070329     C                   close     tivin00r
170500070329     C                   endif
170600070329     C                   if        %open(fivabwwr)
170700070329     C                   close     fivabwwr
170800070329     C                   endif
170900070329     C                   if        %open(fivatwwr)
171000070329     C                   close     fivatwwr
171100070329     C                   endif
171200070326     C*
171300070326     C* Effettuo la chiamata al CLLE preposto
171400070326     C                   call(e)   'TITVVTC'
171500070326     C                   parm                    parccm
171600070326     C                   parm                    parmbr
171700070326     C                   parm      '2'           paropz
171800070326     C*
171900070326     C                   eval      wrkesito = '2'
172000070404     C*
172100070404     C                   seton                                        LR
172200070326     C*
172300070326     C                   ENDSR     '*CANCL'
172400070326     C***
172500100318
172600100318
172700100318
172800100318     C     normCSV       BEGSR
172900100318     C*
173000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173100100318     C* separatori campi
173200100318     C                   if        CharTXT <> *blanks
173300100318     C                   z-add     *zeros        wLenVINDTA        4 0
173400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173500100318     C                   setoff                                       80
173600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173900100318     C                   if        not *in80
174000100318     C                   seton                                        80
174100100318     C                   else
174200100318     C                   setoff                                       80
174300100318     C                   endif
174400100318     C                   endif
174500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174600100318     C                             and *in80 = *on
174700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174800100318     C                   endif
174900100318     C                   enddo
175000100318     C                   endif
175100100318     C*
175200100318     C                   ENDSR
175300100318     C***
175400070326
175500100326
175600100326
175700100326     C*----------------------------------------------------*
175800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175900100326     C*----------------------------------------------------*
176000100326     C     REPNSP        BEGSR
176100100326     C*
176200100326     C* NSP => Stacco un numeratore da AZNUM
176300100326     C                   clear                   TRUL33DS
176400100326     C                   eval      I33OPE = *zeros
176500100326     C                   eval      I33CNU = 302
176600100326     C                   eval      I33NUM = 1
176700100326     C                   movel     TRUL33DS      KPJBU
176800100326     C                   call      'TRUL33R'
176900100326     C                   parm                    KPJBA
177000100326     C                   movel     KPJBU         TRUL33DS
177100100326     C                   if        O33ERR = *zeros
177200100326     C                   z-add     O33NRF        VABNSP
177300100326     C                   z-add     O33NRF        VATNSP
177400140305     C                   z-add     O33NRF        VABRMN
177500100326     C                   else
177600100326     C                   SETON                                        31
177700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177800140305     C                             + ' ' + 'VABNSP VATNSP VABRMN'
177900100326     C                   endif
178000100326     C*
178100100326     C                   ENDSR
178200010330
178300100326
178400010330
178500000613     C     *inzsr        BEGSR
178600990910     C*
178700990910     C     *entry        plist
178800990920     C                   parm                    tivlrds
178900990921     C                   parm      wrkesito      esito
179000000724     C                   parm                    prmlit
179100000710     C                   parm                    prmfir
179200010330     C*
179300010330     C* CALCOLA LA DATA CORRENTE
179400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179500100318     C                   eval      datcor = %dec(%date() : *ISO)
179600000613     C*
179700000613     C                   ENDSR
179800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179900140722X;X;VABRMN;VABRMA;VABNCL;VABPKB;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;W_VATNOT_E;VATNOT_B;
180000140513
180100140512
180200111116
180300100318
