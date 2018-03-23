000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300140612     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000600040119     FFIVATwwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000100413     D fivabds       e ds                  extname(fivab00f)
002100100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002200100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002300140520     D dvats         e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D rrnum           s              6  0 INZ(*zeros)
002900040510     D parccm          s              8    INZ(*blanks)
003000040119     D parmbr          s             10    INZ(*blanks)
003100040119     D paropz          s              1    INZ(*blanks)
003200040119     D chkcall         s              1    INZ(*blanks)
003300080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003400080418     D wFlgCAS         s              1    INZ(*blanks)
003500100326     D*------------------
003600100326     D* DS REPERIMENTO NUMERATORE
003700100326     D*------------------
003800100326     D trul33ds      e ds                  inz
003900100326     D*------------------
004000100326     D* DS ARCHITETTURA
004100100326     D*------------------
004200100326     D kpjba         e ds                  inz
004300100326
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200100318     D CharSOS         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700140520     D wVATNOT_A       s                   LIKE(VATNOT) INZ
005800140520     D wVATNOT_B       s                   LIKE(VATNOT) INZ
005900140520     D wVATNOT_E       s                   LIKE(VATNOT) INZ
006000140520     D wVATNOT_I       s                   LIKE(VATNOT) INZ
006100140520     D wVATNOT_J       s                   LIKE(VATNOT) INZ
006200140520     D wVATNOT_S       s                   LIKE(VATNOT) INZ
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000080418     D*------------------
007100080418     D* Costanti
007200080418     D*------------------
007300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500140520
007600140520
007700140520     D*------------------
007800140520     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
007900140520     D*------------------
008000140520     D InStringa       S          65535A   VARYING                              (stringa input)
008100140520     D InSepara        S             10A                                        (separatore)
008200140520     D InTypeOut       S              1A                                        (tipo output)
008300140520     D wSkParam        S          65535A                                        (output)
008400140520     D OutErrore       S              1A                                        (flag errore)
008500140520     D OutMsg          S             80A                                        (messaggio errore)
008600140520     D SkSplitCSV_3    S             64    DIM(1024)
008700140520
008800100318
008900100318     D*------------------
009000100318     D* LINKING A DEFINIZIONI ESTERNE
009100100318     D*------------------
009200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009400100318
009500010330
009600010330
009700010330
009800000913     C                   reset                   rrnum
009900990921     C                   reset                   esito
010000990921     C                   reset                   wrkesito
010100010601     C*
010200010601     C                   exsr      opeini
010300010605     C                   exsr      rwvab
010400010601     C*
010500010601     C                   seton                                        lr
010600010601
010700010601
010800010601
010900010601     C*--------------------------------------------------------
011000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
011100010601     C*--------------------------------------------------------
011200010601     C     PREELA        BEGSR
011300010601     C*
011400010601     C* SE OCCORRE SPEDIRE IN FILIALE
011500010601     C                   if        invfil <> *zeros and
011600010601     C                             flgGiro = '0'
011700010601     C*
011800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011900010601     C                   eval      flgGiro = '1'
012000010601     C*
012100010601     C                   endif
012200010601     C*
012300010601     C                   ENDSR
012400010601     C***
012500010601
012600010601
012700010601
012800010601
012900010601     C*--------------------------------------------------------
013000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013100010601     C*--------------------------------------------------------
013200010601     C     ENDELA        BEGSR
013300040119     C*
013400040119     C* Effettuo la chiamata al CLLE preposto
013500040119     C                   call(e)   'TITVVTC'
013600040510     C                   parm                    parccm
013700040119     C                   parm                    parmbr
013800040119     C                   parm      '2'           paropz
013900050201     C*
014000050201     C* Effettuo lancio TISI95 solo x chiusura
014100050201     C                   CLEAR                   TISI95DS
014200050201     C                   EVAL      I95TLA = 'C'
014300050201     C                   CALL      'TISI95R'
014400050201     C                   PARM                    TISI95DS
014500000616     C*
014600010601     C                   ENDSR
014700010601     C***
014800000613
014900010601
015000010601
015100010330     C*--------------------------------------------------------
015200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015300010330     C*--------------------------------------------------------
015400010605     C     RWVAB         BEGSR
015500010330     C*
015600010330     C                   if        not %open(tivin00r)
015700010330     C                   open      tivin00r
015800010330     C                   endif
015900021025     C                   if        not %open(fivabwwr)
016000021025     C                   open      fivabwwr
016100010330     C                   endif
016200040119     C*
016300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016400040119     C                   exsr      prevat
016500040119     C*
016600040119     C                   if        chkcall = '0'
016700040119     C*
016800040119     C                   if        not %open(fivatwwr)
016900040119     C                   open      fivatwwr
017000040119     C                   endif
017100010330     C*
017200040119     C                   clear                   §CTROKVB          7 0
017300040119     C                   clear                   §CTROKVT          7 0
017400040119     C                   clear                   §CTRMO            7 0
017500040119     C                   clear                   §CTRNO            7 0
017600070823     C*
017700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017900111025     C                   exsr      inzvar
018000111025     C                   exsr      impvab
018100110131     C*
018200110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
018300110912 xxx C***                read      tivin00r                               70
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
021800010601     C*
021900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022000070925     C                   eval      vindta  = ' ' + %trim(vindta)
022100070925     C*
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
037300070301     C                   eval      VABCTM = '7Q'
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
041800140520     C*
041900140520     C                   if        wCHISONO  <> *blanks
042000140520     C                   exsr      WRIVAT_E
042100110201     C                   endif
042200110927     C*
042300110927     C                   if        wVATNOT_I <> *blanks
042400110927     C                   eval      VATNOT = wVATNOT_I
042500110927     C                   eval      VATTRC = 'I'
042600110927     C                   write     FIVAT000
042700110927     C                   add       1             §CTROKVT
042800110927     C                   endif
042900140520     C*
043000140520     C                   if        wVATNOT_J <> *blanks
043100140520     C                   eval      VATNOT = wVATNOT_J
043200140520     C                   eval      VATTRC = 'J'
043300140520     C                   write     FIVAT000
043400140520     C                   add       1             §CTROKVT
043500140520     C                   endif
043600140520     C*
043700140520     C                   if        wVATNOT_S <> *blanks
043800140520     C                   eval      VATNOT = wVATNOT_S
043900140520     C                   eval      VATTRC = 'S'
044000140520     C                   write     FIVAT000
044100140520     C                   add       1             §CTROKVT
044200140520     C                   endif
044300040119     C*
044400040119     C                   ENDSR
044500040119
044600140520
044700140520     C*----------------------------------------------------*
044800140520     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
044900140520     C*----------------------------------------------------*
045000140520     C     WRIVAT_E      BEGSR
045100140520     C*
045200140520     C* Valorizzo l buffer di scrittura del FIVAT
045300140520     C* - TIPO RECORD "E"
045400140520     C                   eval      VATTRC = 'E'
045500140520     C                   eval      InStringa = %trim(wCHISONO)
045600140520     C*
045700140520     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
045800140520     C                   CALL      'XSPLIT2'
045900140520     C                   PARM                    InStringa
046000140520     C                   PARM      ','           InSepara
046100140520     C                   PARM      '3'           InTypeOut
046200140520     C                   PARM      ''            wSkParam
046300140520     C                   PARM                    OutErrore
046400140520     C                   PARM                    OutMsg
046500140520     C                   MOVEA     wSkParam      SkSplitCSV_3
046600140520     C*
046700140520     C* Valorizzo la schiera delle intestazioni colonne (elementi)
046800140520     C                   z-add     1             x                 4 0
046900140520     C                   dow       x <= %elem(SkSplitCSV_3)
047000140520     C                   if        SkSplitCSV_3(x) = *blanks
047100140520     C                   leave
047200140520     C                   else
047300140520     C                   eval      VATNOT = SkSplitCSV_3(x)
047400140520     C                   write     FIVAT000
047500140520     C                   add       1             §CTROKVT
047600140520     C                   endif
047700140520     C                   add       1             x
047800140520     C                   enddo
047900140520     C*
048000140520     C                   ENDSR
048100040119
048200040119
048300040119     C*----------------------------------------------------*
048400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
048500040119     C*----------------------------------------------------*
048600040119     C     PREVAT        BEGSR
048700040119     C*
048800040119     C* Compongo il nome del membro da dare al FIVATWWR
048900040119     C                   eval      parmbr = vlrhdl
049000040119     C                   movel     'M'           parmbr
049100040510     C                   eval      parccm = vlrksc
049200040119     C                   eval      paropz = '1'
049300040119     C* Effettuo la chiamata al CLLE preposto
049400040119     C                   call(e)   'TITVVTC'
049500040119     C                   parm                    parccm
049600040119     C                   parm                    parmbr
049700040119     C                   parm                    paropz
049800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049900040119     C                   if        %error
050000040119     C                   movel     '1'           chkcall
050100040119     C                   else
050200040119     C                   movel     '0'           chkcall
050300040119     C                   endif
050400040119     C*
050500040119     C                   ENDSR
050600010601
050700010601
050800010601
050900010330     C*----------------------------------------------------*
051000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051100010330     C*----------------------------------------------------*
051200040119     C     INZVAR        BEGSR
051300080527     C*
051400080527     C                   setoff                                       65
051500010330     C*
051600030822     C* Inizializzo variabili di wrk
051700020204     C                   Z-ADD     *zeros        Num5_0            5 0
051800101018     C                   MOVEL     *blanks       wFlgCAS
051900060331     C                   MOVEL     *blanks       wVATNOT_A
052000060331     C                   MOVEL     *blanks       wVATNOT_B
052100060331     C                   MOVEL     *blanks       wVATNOT_E
052200110927     C                   MOVEL     *blanks       wVATNOT_I
052300140520     C                   MOVEL     *blanks       wVATNOT_J
052400140520     C                   MOVEL     *blanks       wVATNOT_S
052500140520     C                   CLEAR                   SkSplitCSV_3
052600030822     C*
052700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052800030822     C                   CLEAR                   SkSplitCSV
052900030822     C                   EVAL      i = 1
053000030822     C                   EVAL      posDa = *zeros
053100030822     C                   EVAL      posA  = *zeros
053200080527     C*
053300080527     C                   CLEAR                   FIVABDS
053400080527     C                   CLEAR                   FIVAT000
053500030822     C*
053600030822     C* Reimposto i valori di default
053700030822     C                   EXSR      DEFCAM
053800030822     C*
053900010330     C                   ENDSR
054000010330     C*----------------------------------------------------*
054100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054200010330     C*----------------------------------------------------*
054300010330     C     DEFCAM        BEGSR
054400030822     C*
054500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054600030715     C* e delimitatore testo.
054700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055000100318     C*
055100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055200100318     C                   EVAL      CharSOS = CharNUM
055300030822     C*
055400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055500100720     C                   SETOFF                                       505152
055600100720     C                   SETOFF                                       53
055700100413     C                   SELECT
055800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055900100720     C                   SETON                                        50
056000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056100100413     C                   SETON                                        51
056200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056300100720     C                   SETON                                        52
056400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056500100720     C                   SETON                                        53
056600100413     C                   ENDSL
056700030822     C*
056800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056900030822     C                   EVAL      posDaDft = 1
057000030822     C                   EVAL      posADft  = 0
057100030822     C                   EVAL      wGiroDft = 0
057200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057300030822     C                             posDaDft > 0
057400030822     C*
057500030822     C* Gestisco il 1° giro
057600030822     C                   IF        wGiroDft = 0
057700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057900030822     C* Incremento il contatore dei "giri"
058000030822     C                   EVAL      wGiroDft = 1
058100030822     C                   ELSE
058200030822     C                   EVAL      posDaDft = posADft
058300030822     C                   ENDIF
058400030822     C* Eseguo lo scan x trovare la fine del campo corrente
058500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058600030822     C*
058700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058900030822     C                   IF        posDaDft > 0 AND
059000030822     C                             posADft  > 0
059100030822     C* NCL
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'NCL'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABNCL
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* CCM
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'CCM'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABCCM
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* LNP
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'LNP'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABLNP
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* NRS
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'NRS'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABNRS
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CTR
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CTR'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiInt=*on
065300030822     C                   Z-ADD     PiVal         VABCTR
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* PKB
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'PKB'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABPKB
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* VLB
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VLB'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiNum=*on
067900030822     C                   Z-ADD     PiVal         VABVLB
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* QFT
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'QFT'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiNum=*on
069200030822     C                   Z-ADD     PiVal         VABQFT
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* CBO
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'CBO'
070000030822     C                   EVAL      VABCBO=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* TSP
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'TSP'
070900030822     C                   EVAL      VABTSP=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* VAS
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'VAS'
071800030822     C                   EVAL      VABVAS=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VCA
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VCA'
072700030822     C                   EVAL      VABVCA=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100070925     C* TIC
073200070925     C                   IF        %subst(
073300070925     C                             %subst(vlrppt:posDaDft+1:
073400070925     C                             posADft-posDaDft-1):1:3)
073500070925     C                             = 'TIC'
073600070925     C                   EVAL      VABTIC=%trim(%subst(
073700070925     C                             %subst(vlrppt:posDaDft+1:
073800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900070925     C                   ENDIF
074000030822     C* GCA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GCA'
074500030822     C                   EVAL      VABGCA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* CTM
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'CTM'
075400030822     C                   EVAL      VABCTM=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* FFD
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'FFD'
076300030822     C                   EVAL      VABFFD=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* VAD
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'VAD'
077200030822     C                   EVAL      VABVAD=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* GMA
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'GMA'
078100030822     C                   EVAL      VABGMA=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* GGA
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'GGA'
079000030822     C                   EVAL      VABGGA=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* GVA
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'GVA'
079900030822     C                   EVAL      VABGVA=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* TC1
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'TC1'
080800030822     C                   EVAL      VABTC1=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* TC2
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'TC2'
081700030822     C                   EVAL      VABTC2=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100040714     C* VATTRC
082200040714     C                   IF        %subst(
082300040714     C                             %subst(vlrppt:posDaDft+1:
082400040714     C                             posADft-posDaDft-1):1:3)
082500040714     C                             = 'TRC'
082600040714     C                   EVAL      VATTRC=%trim(%subst(
082700040714     C                             %subst(vlrppt:posDaDft+1:
082800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900040714     C                   ENDIF
083000030822     C* ...
083100030822     C                   ENDIF
083200030822     C                   ENDDO
083300020204     C*
083400010330     C                   ENDSR
083500010607     C*----------------------------------------------------*
083600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
083700010607     C*----------------------------------------------------*
083800010607     C     IMPVAB        BEGSR
083900010607     C*
084000010607     C                   SETOFF                                       3132
084100110315 xxx C***                SETON                                        35
084200030515     C*
084300010607     C                   MOVEL     datcor        VABAAS
084400010607     C                   MOVE      datcor        VABMGS
084500010607     C*
084600030715     C**********
084700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084800030715     C**********
084900030715     C                   IF        wGiro = *zeros
085000040113     C*
085100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085400080429     C                             %trim(intcol(5))<>*blanks
085500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085700080429     C                                      %trim(intcol(5))
085800070823     C     ';':CharCSV   XLATE     vindta        vindta
085900040510     C                   ENDIF
086000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086100031201     C     minu:maiu     XLATE     vindta        vindta
086200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086300030715     C                   DOW       posDa <= %len(%trim(vindta))
086400030715     C*
086500030715     C* Gestisco il 1° campo
086600030715     C                   IF        i = 1
086700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086800030715     C                   EVAL      posDa = 1
086900030715     C* Eseguo lo scan x trovare la fine del primo campo
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C* X i campi successivi al 1°
087500030715     C                   ELSE
087600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087700030715     C                   EVAL      posDa = posA + 1
087800030715     C* Eseguo lo scan x trovare la fine del campo corrente
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* Gestisco l'ultimo campo
088100030715     C                   IF        posA = *zeros
088200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   LEAVE
088700030715     C                   ELSE
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   ENDIF
089200030715     C                   ENDIF
089300030715     C* Incremento il contatore d campo
089400030715     C                   EVAL      i = i +1
089500030715     C                   ENDDO
089600030715     C*
089700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089800030715     C* delimitazione testo
089900030715     C                   EVAL      i = 1
090000030715     C                   DOW       i < %elem(SkSplitFLD)
090100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090300030715     C                   EVAL      i = i + 1
090400030715     C                   ENDDO
090500030715     C                   EVAL      wGiro = 1
090600030715     C                   ELSE
090700030715     C                   EVAL      wGiro = 2
090800031204     C**********
090900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091000031204     C* d separatore campo
091100031204     C**********
091200031204     C                   EVAL      vindta = %trim(vindta)
091300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091400031204     C                   EVAL      vindta = %subst(vindta:2)
091500031204     C                   ENDDO
091600031204     C*
091700031204     C                   Z-ADD     *zeros        lunghInput        4 0
091800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092200110912     C                   ENDDO
092300030715     C**********
092400030715     C* Effettuo lo split del campo dati d input
092500030715     C**********
092600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092700030715     C                   DOW       posDa <= %len(%trim(vindta))
092800030715     C*
092900030715     C* Gestisco il 1° campo
093000030715     C                   IF        i = 1
093100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093200030715     C                   EVAL      posDa = 1
093300030715     C* Eseguo lo scan x trovare la fine del primo campo
093400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C* X i campi successivi al 1°
093900030715     C                   ELSE
094000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094100030715     C                   EVAL      posDa = posA + 1
094200031204     C* Verifico che nn vi sia il campo nn valorizzato
094300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094400031204     C* Se campo nn valorizzato skippo al prossimo
094500031204     C                   EVAL      SkSplitCSV(i) = *blanks
094600031204     C                   EVAL      posA  = posA + 1
094700031204     C                   ELSE
094800030715     C* Eseguo lo scan x trovare la fine del campo corrente
094900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095000030715     C* Gestisco l'ultimo campo
095100030715     C                   IF        posA = *zeros
095200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   LEAVE
095700030715     C                   ELSE
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   ENDIF
096200031204     C                   ENDIF
096300030715     C                   ENDIF
096400030715     C* Incremento il contatore d campo
096500030715     C                   EVAL      i = i +1
096600030715     C                   ENDDO
096700030715     C*
096800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096900030715     C* delimitazione testo
097000030715     C                   EVAL      i = 1
097100030715     C                   DOW       i < %elem(SkSplitCSV)
097200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097400030715     C                   EVAL      i = i + 1
097500030715     C                   ENDDO
097600100720     C*
097700100720     C* Stacco un progressivo x numero spedizione
097800140612 xxx C                   EXSR      repNSP
097900100326     C*
098000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098100030715     C                   EVAL      i = 1
098200030715     C                   DOW       i < %elem(SkSplitFLD)
098300030715     C*
098400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098600030715     C                             SkSplitCSV(i) <> *blanks
098700030721     C*** ATB
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098900030721     C                   EVAL      VABATB = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030715     C*** RSD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099600030715     C* ==
099700030715     C                   ENDIF
099800030721     C*** RD2
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030715     C*** IND
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100400030715     C                   EVAL      VABIND = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** LOD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** PRD
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030721     C*** NZD
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030715     C*** RMA
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NOT
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** NT2
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VCA
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** VAS
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030715     C*** FFD
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104100030715     C                   ENDIF
104200030721     C*** GC1
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GC2
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TSP
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105300030721     C                   ENDIF
105400040510     C*** CBO
105500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105700040510     C                   ENDIF
105800030721     C*** NAS
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TIC
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GCA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** XCO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** CTM
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TCR
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** CTS
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** FTM
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** VAD
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GMA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GGA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** GVA
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC1
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** TC2
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** SCL
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** RMO
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** NMO
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*
112700030721     C* Reperisco quindi i campi numerici...
112800040510     C*** NSP
112900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113000040510     C                   EVAL      PiStr=SkSplitCSV(i)
113100040510     C                   EXSR      CHKNUM
113200040510     C                   IF        PiInt=*on
113300040510     C                   Z-ADD     PiVal         VABNSP
113400040510     C                   ELSE
113500040510     C                   SETON                                        31
113600040510     C                   EVAL      VABNSP = *zeros
113700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113800040510     C                             + ' ' + 'VABNSP'
113900040510     C                   ENDIF
114000040510     C                   ENDIF
114100071106     C*** RMN
114200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114300071106     C                   EVAL      PiStr=SkSplitCSV(i)
114400071106     C                   EXSR      CHKNUM
114500071106     C                   IF        PiInt=*on
114600071106     C                   Z-ADD     PiVal         VABRMN
114700071106     C                   ELSE
114800071106     C                   SETON                                        32
114900071106     C                   EVAL      VABRMN = *zeros
115000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115100071106     C                             + ' ' + 'VABRMN'
115200071106     C                   ENDIF
115300071106     C                   ENDIF
115400030721     C*** CAD
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on
115900030721     C                   Z-ADD     PiVal         Num5_0
116000030721     C                   MOVEL(p)  Num5_0        VABCAD
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABCAD = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABCAD'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** NCL
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on
117300030721     C                   Z-ADD     PiVal         VABNCL
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABNCL = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800040122     C                             + ' ' + 'VABNCL'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** PKB
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABPKB
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABPKB = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABPKB'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** CAS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119700110506     C                             %trim(SkSplitCSV(i)) <> *zeros
119800101018     C                   EVAL      wFlgCAS = '1'
119900140529     C                   EVAL      VABVCA = 'EUR'
120000170511     C***                EVAL      VABTIC = 'BM'
120100170511     C                   EVAL      VABTIC = *blanks
120200101018     C                   ENDIF
120300030721     C                   EVAL      PiStr=SkSplitCSV(i)
120400030721     C                   EXSR      CHKNUM
120500030721     C                   IF        PiNum=*on
120600030721     C                   Z-ADD     PiVal         VABCAS
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABCAS = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABCAS'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** CCM
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on      AND
121900030721     C                             PiVal<=9999999 AND
122000030721     C                             PiVal>*zeros
122100030721     C                   Z-ADD     PiVal         VABCCM
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      VABCCM = *zeros
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600040510     C                             + ' ' + 'VABCCM'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** LNP
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiInt=*on    AND
123400030721     C                             PiVal<=999   AND
123500030721     C                             PiVal>*zeros
123600030721     C                   Z-ADD     PiVal         VABLNP
123700030721     C                   ELSE
123800040715     C                   SETON                                        32
123900030721     C                   EVAL      VABLNP = *zeros
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100040510     C                             + ' ' + 'VABLNP'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** NRS
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on AND
124900030721     C                             PiVal<=99
125000030721     C                   Z-ADD     PiVal         VABNRS
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABNRS'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** LNA
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on    AND
126200030721     C                             PiVal<=999   AND
126300030721     C                             PiVal>*zeros
126400030721     C                   Z-ADD     PiVal         VABLNA
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      VABLNA = *zeros
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABLNA'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** CTR
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on  AND
127700030721     C                             PiVal<=999
127800030721     C                   Z-ADD     PiVal         VABCTR
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABCTR'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** IAS
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABIAS
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABIAS'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** VLB
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiNum=*on
130200030721     C                   Z-ADD     PiVal         VABVLB
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABVLB'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** QFT
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiNum=*on
131400030721     C                   Z-ADD     PiVal         VABQFT
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABQFT'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** NCD
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on      AND
132600030721     C                             PiVal<=9999999
132700030721     C                   Z-ADD     PiVal         VABNCD
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133100030721     C                             + ' ' + 'VABNCD'
133200030721     C                   ENDIF
133300030721     C                   ENDIF
133400030721     C*** NCA
133500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133600030721     C                   EVAL      PiStr=SkSplitCSV(i)
133700030721     C                   EXSR      CHKNUM
133800030721     C                   IF        PiInt=*on      AND
133900030721     C                             PiVal<=9999999
134000030721     C                   Z-ADD     PiVal         VABNCA
134100030721     C                   ELSE
134200030721     C                   SETON                                        32
134300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134400030721     C                             + ' ' + 'VABNCA'
134500030721     C                   ENDIF
134600030721     C                   ENDIF
134700030721     C*** ZNC
134800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134900030721     C                   EVAL      PiStr=SkSplitCSV(i)
135000030721     C                   EXSR      CHKNUM
135100030721     C                   IF        PiInt=*on AND
135200030721     C                             PiVal<=99
135300030721     C                   Z-ADD     PiVal         VABZNC
135400030721     C                   ELSE
135500030721     C                   SETON                                        32
135600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135700030721     C                             + ' ' + 'VABZNC'
135800030721     C                   ENDIF
135900030721     C                   ENDIF
136000030721     C*** DCR
136100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136200030721     C                   EVAL      PiStr=SkSplitCSV(i)
136300030721     C                   EXSR      CHKNUM
136400030721     C                   IF        PiInt=*on       AND
136500030721     C                             PiVal<=99999999
136600030721     C                   Z-ADD     PiVal         VABDCR
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABDCR'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** HCR
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on   AND
137800030721     C                             PiVal<=9999
137900030721     C                   Z-ADD     PiVal         VABHCR
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABHCR'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** VMD
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiNum=*on
139100030721     C                   Z-ADD     PiVal         VABVMD
139200030721     C                   ELSE
139300030721     C                   SETON                                        32
139400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139500030721     C                             + ' ' + 'VABVMD'
139600030721     C                   ENDIF
139700030721     C                   ENDIF
139800030721     C*** ANT
139900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140000030721     C                   EVAL      PiStr=SkSplitCSV(i)
140100030721     C                   EXSR      CHKNUM
140200030721     C                   IF        PiInt=*on        AND
140300030721     C                             PiVal<=999999999
140400030721     C                   Z-ADD     PiVal         VABANT
140500030721     C                   ELSE
140600030721     C                   SETON                                        32
140700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140800030721     C                             + ' ' + 'VABANT'
140900030721     C                   ENDIF
141000030721     C                   ENDIF
141100030721     C*** CMO
141200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141300030721     C                   EVAL      PiStr=SkSplitCSV(i)
141400030721     C                   EXSR      CHKNUM
141500030721     C                   IF        PiInt=*on
141600030721     C                   Z-ADD     PiVal         Num5_0
141700030721     C                   MOVEL(p)  Num5_0        VABCMO
141800030721     C                   ELSE
141900030721     C                   SETON                                        32
142000030721     C                   EVAL      VABCMO = *zeros
142100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142200030721     C                             + ' ' + 'VABCMO'
142300030721     C                   ENDIF
142400030721     C                   ENDIF
142500040510     C*
142600040510     C* Reperisco i campi del FIVAT
142700040510     C*** VATNOT
142800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143000040510     C                   ENDIF
143100040510     C*** VATTRC
143200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143400040510     C                   ENDIF
143500060331     C*** VATNOT_A
143600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143800060331     C                   ENDIF
143900060331     C*** VATNOT_B
144000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144200060331     C                   ENDIF
144300060331     C*** VATNOT_E
144400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144600060331     C                   ENDIF
144700110927     C*** VATNOT_I
144800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145000110927     C                   ENDIF
145100070823     C*
145200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
145300110421     C*
145400111025     C*** NOT / NT2
145500140529     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT12'
145600111025     C                   MOVEL     *blanks       wNOTE            70
145700111025     C                   EVAL      wNOTE=SkSplitCSV(i)
145800111025     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
145900111025     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
146000111025     C                   ENDIF
146100140612     C*
146200140612     C*** RMA / RMN
146300140612     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
146400140612     C                   EVAL      VABRMA=%trim(SkSplitCSV(i))
146500140612     C*
146600140612     C                   EVAL      PiStr=SkSplitCSV(i)
146700140612     C                   EXSR      CHKNUM
146800140612     C                   IF        PiInt=*on
146900140612     C                   Z-ADD     PiVal         VABRMN
147000140612     C                   ELSE
147100140612     C                   SETON                                        32
147200140612     C                   EVAL      VABRMN = *zeros
147300140612     C                   EVAL      vinmsg = %trimr(vinmsg)
147400140612     C                             + ' ' + 'VABRMN'
147500140612     C                   ENDIF
147600140612     C                   ENDIF
147700140520     C*
147800140520     C*** VATNOT_I / VATNOT_J
147900140529     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
148000140520     C                   MOVEL     *blanks       wNOTE            70
148100140520     C                   EVAL      wNOTE=SkSplitCSV(i)
148200140520     C                   EVAL      wVATNOT_I=%subst(wNOTE:1:35)
148300140520     C                   EVAL      wVATNOT_J=%subst(wNOTE:36:35)
148400140520     C                   ENDIF
148500140520     C*
148600140520     C*** VATNOT_S
148700140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_S'
148800140520     C                   CLEAR                   DVATS
148900140530     C                   EVAL      §VATSSMP = 'S'
149000140530     C                   EVAL      §VATSMMP = 'S'
149100140520     C                   EVAL      §VATSTEL = %trim(SkSplitCSV(i))
149200140520     C                   EVAL      wVATNOT_S=DVATS
149300140520     C                   ENDIF
149400140520     C*
149500140520     C*** VATNOT_E
149600140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
149700140520     C                   MOVEL     *blanks       wCHISONO       1024
149800140520     C                   EVAL      wCHISONO=SkSplitCSV(i)
149900140520     C                   ENDIF
150000140604     C*
150100140604     C*** TIC
150200140604     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
150300140604     C                   IF        %trim(SkSplitCSV(i)) = 'ABC'
150400140604     C                   EVAL      VABTIC='BM'
150500140604     C                   ENDIF
150600140604     C                   ENDIF
150700070925     C*
150800071029     C***  <===  -----------------------------------------
150900040510     C*
151000030715     C                   ENDIF
151100030715     C                   EVAL      i = i + 1
151200030715     C                   ENDDO
151300070925     C*
151400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
151500100831     C*
151600070925     C*
151700071029     C***  <===  -----------------------------------------
151800070925     C*
151900070925     C* Considerazioni finali su CBO/CAS
152000100318     C                   IF        wFlgCAS = '1'    OR
152100100318     C                             VABCAS  > *zeros
152200070925     C                   IF        VABCBO = '1'
152300070925     C                   EVAL      VABCBO = '4'
152400070925     C                   ENDIF
152500070925     C                   IF        VABCBO = '2'
152600070925     C                   EVAL      VABCBO = '6'
152700070925     C                   ENDIF
152800070925     C                   ENDIF
152900070925     C***  <===  ----------------------------
153000040510     C*
153100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153200040510     C                   IF        VABNSP = *zeros
153300040510     C                   SETON                                        31
153400040510     C                   ENDIF
153500040715     C*
153600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153700040715     C                   IF        VABLNP = *zeros
153800040715     C                   SETON                                        31
153900040715     C                   ENDIF
154000040510     C*
154100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154200040510     C                   IF        VABRMN = *zeros
154300040510     C                   Z-ADD     VABNSP        VABRMN
154400040510     C                   ENDIF
154500040510     C*
154600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
154700040510     C                   IF        VABRMA = *blanks
154800040714     C                   MOVEL     VABRMN        VABRMA
154900040510     C                   ENDIF
155000040128     C*
155100040510     C* Se provincia nn valorizzata la reperisco
155200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155300110912     C                   IF        VABPRD =  *blanks AND
155400110912     C                             VABNZD =  *blanks AND
155500110912     C                             VABCAD <> *blanks AND
155600110912     C                             VABLOD <> *blanks
155700040128     C                   CLEAR                   TISI95DS
155800040128     C                   EVAL      I95TCN = '3'
155900040128     C                   Z-ADD     datcor        I95DAT
156000040128     C                   EVAL      I95NAR = VABNZD
156100040128     C                   EVAL      I95CAP = VABCAD
156200040128     C                   EVAL      I95LOC = VABLOD
156300040128     C                   CALL      'TISI95R'
156400040128     C                   PARM                    TISI95DS
156500040128     C                   EVAL      VABPRD = O95PRV
156600040510     C                   ENDIF
156700020204     C*
156800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
156900020204     C                   EXSR      CHKIMPDIV
157000030715     C                   ENDIF
157100020204     C*
157200010607     C                   ENDSR
157300010607     C*----------------------------------------------------*
157400010601
157500020204
157600020204     C*----------------------------------------------------*
157700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157800020204     C*----------------------------------------------------*
157900020204     C     CHKIMPDIV     BEGSR
158000020204     C*
158100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158200020204     C                   Z-ADD     *zeros        wrkDec            9 9
158300020204     C*
158400020204     C* Come prima cosa effettuo considerazioni sulla divisa
158500020204     C                   IF        vabIAS > *zeros
158600020204     C                   IF        vabVAS <> 'EUR'
158700020204     C                   EVAL      vabVAS =  'ITL'
158800020204     C                   ENDIF
158900020204     C                   ENDIF
159000020204     C*
159100020204     C                   IF        vabCAS > *zeros
159200020204     C                   IF        vabVCA <> 'EUR'
159300020204     C                   EVAL      vabVCA =  'ITL'
159400020204     C                   ENDIF
159500020204     C                   ENDIF
159600020204     C*
159700020204     C                   IF        vabVMD > *zeros
159800020204     C                   IF        vabVAD <> 'EUR'
159900020204     C                   EVAL      vabVAD =  'ITL'
160000020204     C                   ENDIF
160100020204     C                   ENDIF
160200020204     C*
160300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160400020204     C                   Z-ADD     vabIAS        wrkDec
160500020204     C                   IF        wrkDec > *zeros
160600020204     C                   IF        vabVAS = 'ITL'
160700020204     C                   EVAL      vabIAS = *zeros
160800020204     C                   ENDIF
160900020204     C                   ENDIF
161000020204     C*
161100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161200020204     C                   Z-ADD     vabCAS        wrkDec
161300020204     C                   IF        wrkDec > *zeros
161400020204     C                   IF        vabVCA = 'ITL'
161500020204     C                   EVAL      vabCAS = *zeros
161600020204     C                   ENDIF
161700020204     C                   ENDIF
161800020204     C*
161900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162000020204     C                   Z-ADD     vabVMD        wrkDec
162100020204     C                   IF        wrkDec > *zeros
162200020204     C                   IF        vabVAD = 'ITL'
162300020204     C                   EVAL      vabVMD = *zeros
162400020204     C                   ENDIF
162500020204     C                   ENDIF
162600020204     C*
162700020204     C                   ENDSR
162800020204     C***
162900010330
163000100318
163100100318
163200100318     C*----------------------------------------------------*
163300100318     C*  CONTROLLO NUMERICITA' CAMPI
163400100318     C*----------------------------------------------------*
163500100318     C     CHKNUM        BEGSR
163600100318     C*
163700100318     C                   IF        PiDecChr = *blanks
163800100318     C                   EVAL      PiDecChr = CharNUM
163900100318     C                   ENDIF
164000100318     C*
164100100318     C                   callp     UBISNUM_Check(PiStr
164200100318     C                                          :PiDecChr
164300100318     C                                          :PiVal
164400100318     C                                          :PiNum
164500100318     C                                          :PiInt)
164600100318     C*
164700100318     C                   ENDSR
164800100318     C***
164900010601
165000010601
165100010601
165200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165300010601     C     repfil        BEGSR
165400010601     C*
165500010601     C                   if        invfil = *zeros and
165600010601     C                             depfil > *zeros and
165700010629     C                             (vinflg = *blanks or
165800010629     C                              vinflg = *zeros)
165900010601     C
166000010601     C                   eval      invfil = depfil
166100010601     C                   endif
166200010601     C*
166300010601     C                   if        depfil <> invfil and
166400010601     C                             invfil > *zeros
166500010601     C                   eval      flgMulti = '1'
166600010601     C                   if        vinflg = *blanks
166700010601     C                   add       1             cntNonEl
166800010601     C                   endif
166900010601     C                   endif
167000010601     C*
167100010601     C                   if        vinflg = '2'
167200010601     C                   eval      flgStato = '2'
167300010601     C                   endif
167400010601     C*
167500010601     C                   ENDSR
167600010601     C***
167700010601
167800010601
167900010601
168000010330
168100040119      /TITLE Invio dei dati al punto operativo.
168200040119     C     invio         BEGSR
168300040119     C*
168400040119     C* 1° invio FIVAT
168500040119     C                   reset                   dscmz
168600040119     C                   move      vlrpoi        cmzdst
168700040119     C                   eval      cmzfld = 'FIVATWWR'
168800040119     C                   eval      cmzmbd = vlrhdl
168900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169000040119     C***                if        prmfir = *blanks
169100040119     C                   eval      cmzfla = 'FIVAT00F'
169200040119     C                   eval      cmzmba = 'FIVAT00F'
169300040119     C***                else
169400040119     C***                eval      cmzfla = prmfir
169500040119     C***                eval      cmzmba = prmfir
169600040119     C***                endif
169700040119     C                   eval      cmznrr = *zeros
169800040119     C                   move      §ctrokvt      cmznrr
169900040119     C                   eval      cmzlba = vlrfl1
170000040119     C                   call(e)   'TIS711C'
170100040119     C                   parm                    dscmz
170200040119     C                   parm      *blanks       esito
170300040119     C                   if        %error
170400040119     C                             or cmzerr = '1'
170500040119     C                             or esito  = '1'
170600040119     C                   eval      wrkesito = '3'
170700040119     C                   else
170800040119     C*
170900040119     C* 2° invio FIVAB
171000040119     C                   reset                   dscmz
171100040119     C                   move      vlrpoi        cmzdst
171200040119     C                   eval      cmzfld = vlrfou
171300040119     C                   eval      cmzmbd = vlrhdl
171400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171500040119     C***                if        prmfir = *blanks
171600040119     C                   eval      cmzfla = 'FIVAB00F'
171700040119     C                   eval      cmzmba = 'FIVAB00F'
171800040119     C***                else
171900040119     C***                eval      cmzfla = prmfir
172000040119     C***                eval      cmzmba = prmfir
172100040119     C***                endif
172200040119     C                   eval      cmznrr = *zeros
172300040119     C                   move      §ctrokvb      cmznrr
172400040119     C                   eval      cmzlba = vlrfl1
172500040119     C                   call(e)   'TIS711C'
172600040119     C                   parm                    dscmz
172700040119     C                   parm      *blanks       esito
172800040119     C                   if        %error
172900040119     C                             or cmzerr = '1'
173000040119     C                             or esito  = '1'
173100040119     C                   eval      wrkesito = '3'
173200040119     C                   endif
173300040119     C                   endif
173400040119     C*
173500040119     C                   ENDSR
173600040119     C***
173700010601
173800010601
173900010601
174000010601
174100010601      /TITLE Invio dei dati al punto operativo.
174200010601     C     opeini        BEGSR
174300010601     C*
174400010601     C* Inizializzo flag e contatori operativi
174500010601     C                   movel     '0'           flgGiro           1
174600010601     C                   movel     '0'           flgMulti          1
174700010601     C                   movel     '1'           flgStato          1
174800010615     C                   movel     '0'           flgOk             1
174900010601     C                   z-add     *zeros        cntNonEl         10 0
175000010601     C                   z-add     *zeros        depfil            3 0
175100010601     C                   z-add     *zeros        invfil            3 0
175200010601     C*
175300010601     C                   ENDSR
175400010601     C***
175500070326
175600070326
175700070326
175800070326
175900070326     C     *pssr         BEGSR
176000070329     C*
176100070329     C                   if        %open(tivin00r)
176200070329     C                   close     tivin00r
176300070329     C                   endif
176400070329     C                   if        %open(fivabwwr)
176500070329     C                   close     fivabwwr
176600070329     C                   endif
176700070329     C                   if        %open(fivatwwr)
176800070329     C                   close     fivatwwr
176900070329     C                   endif
177000070326     C*
177100070326     C* Effettuo la chiamata al CLLE preposto
177200070326     C                   call(e)   'TITVVTC'
177300070326     C                   parm                    parccm
177400070326     C                   parm                    parmbr
177500070326     C                   parm      '2'           paropz
177600070326     C*
177700070326     C                   eval      wrkesito = '2'
177800070404     C*
177900070404     C                   seton                                        LR
178000070326     C*
178100070326     C                   ENDSR     '*CANCL'
178200070326     C***
178300100318
178400100318
178500100318
178600100318     C     normCSV       BEGSR
178700100318     C*
178800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
178900100318     C* separatori campi
179000100318     C                   if        CharTXT <> *blanks
179100100318     C                   z-add     *zeros        wLenVINDTA        4 0
179200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179300100318     C                   setoff                                       80
179400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
179600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179700100318     C                   if        not *in80
179800100318     C                   seton                                        80
179900100318     C                   else
180000100318     C                   setoff                                       80
180100100318     C                   endif
180200100318     C                   endif
180300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180400100318     C                             and *in80 = *on
180500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180600100318     C                   endif
180700100318     C                   enddo
180800100318     C                   endif
180900100318     C*
181000100318     C                   ENDSR
181100100318     C***
181200070326
181300100326
181400100326
181500100326     C*----------------------------------------------------*
181600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
181700100326     C*----------------------------------------------------*
181800100326     C     REPNSP        BEGSR
181900100326     C*
182000100326     C* NSP => Stacco un numeratore da AZNUM
182100100326     C                   clear                   TRUL33DS
182200100326     C                   eval      I33OPE = *zeros
182300100326     C                   eval      I33CNU = 302
182400100326     C                   eval      I33NUM = 1
182500100326     C                   movel     TRUL33DS      KPJBU
182600100326     C                   call      'TRUL33R'
182700100326     C                   parm                    KPJBA
182800100326     C                   movel     KPJBU         TRUL33DS
182900100326     C                   if        O33ERR = *zeros
183000100326     C                   z-add     O33NRF        VABNSP
183100100326     C                   z-add     O33NRF        VATNSP
183200100326     C                   else
183300100326     C                   SETON                                        31
183400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183500100326     C                             + ' ' + 'VABNSP VATNSP'
183600100326     C                   endif
183700100326     C*
183800100326     C                   ENDSR
183900010330
184000100326
184100010330
184200000613     C     *inzsr        BEGSR
184300990910     C*
184400990910     C     *entry        plist
184500990920     C                   parm                    tivlrds
184600990921     C                   parm      wrkesito      esito
184700000724     C                   parm                    prmlit
184800000710     C                   parm                    prmfir
184900010330     C*
185000010330     C* CALCOLA LA DATA CORRENTE
185100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185200100318     C                   eval      datcor = %dec(%date() : *ISO)
185300000613     C*
185400000613     C                   ENDSR
185500000613     C***
185600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185700140612VABRSD;VABIND;VABLOD;VABCAD;VABNZD;W_VABRMN;X;VABNCL;X;
185800140612VABPKB;VABCAS;X;X;X;X;X;X;X;W_VATNOT_E;
185900141105W_VABNOT12;X;VABIAS;X;X;W_VATNOTIJ;W_VATNOT_S;VATNOT_B;
186000140604X;W_VABTIC;
186100100318
