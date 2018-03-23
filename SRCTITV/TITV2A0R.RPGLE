000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
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
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800040510     D parccm          s              8    INZ(*blanks)
002900040119     D parmbr          s             10    INZ(*blanks)
003000040119     D paropz          s              1    INZ(*blanks)
003100040119     D chkcall         s              1    INZ(*blanks)
003200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003300080418     D wFlgCAS         s              1    INZ(*blanks)
003400100326     D*------------------
003500100326     D* DS REPERIMENTO NUMERATORE
003600100326     D*------------------
003700100326     D trul33ds      e ds                  inz
003800100326     D*------------------
003900100326     D* DS ARCHITETTURA
004000100326     D*------------------
004100100326     D kpjba         e ds                  inz
004200100326
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100100318     D CharSOS         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600070823     D wVATNOT_A       s                   LIKE(VATNOT)
005700060331     D wVATNOT_B       s                   LIKE(VATNOT)
005800060331     D wVATNOT_E       s                   LIKE(VATNOT)
005900111117     D wVATNOT_I       s                   LIKE(VATNOT)
006000111117     D wVATNOT_J       s                   LIKE(VATNOT)
006100100831     D wVABRSD         s                   LIKE(VABRSD)
006200100831     D wVABIND         s                   LIKE(VABIND)
006300100831     D wVABCAD         s                   LIKE(VABCAD)
006400100831     D wVABLOD         s                   LIKE(VABLOD)
006500100831     D wVABPRD         s                   LIKE(VABPRD)
006600030822     D*------------------
006700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006800030822     D*------------------
006900030822     D posDaDft        S              3  0 INZ(*zeros)
007000030822     D posADft         S              3  0 INZ(*zeros)
007100030822     D j               s              3  0 INZ(1)
007200030822     D wGiroDft        s              1  0 INZ(*zeros)
007300080418     D*------------------
007400080418     D* Costanti
007500080418     D*------------------
007600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800100318
007900100318     D*------------------
008000100318     D* LINKING A DEFINIZIONI ESTERNE
008100100318     D*------------------
008200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400100318
008500100909     D*------------------
008600100909     D* DS CALCOLO CHECK-DIGITS
008700100909     D*------------------
008800100917     D*** TRUL28DSX     e ds                  inz
008900010330
009000010330
009100010330
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500010601     C*
009600010601     C                   exsr      opeini
009700010605     C                   exsr      rwvab
009800010601     C*
009900010601     C                   seton                                        lr
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010500010601     C*--------------------------------------------------------
010600010601     C     PREELA        BEGSR
010700010601     C*
010800010601     C* SE OCCORRE SPEDIRE IN FILIALE
010900010601     C                   if        invfil <> *zeros and
011000010601     C                             flgGiro = '0'
011100010601     C*
011200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011300010601     C                   eval      flgGiro = '1'
011400010601     C*
011500010601     C                   endif
011600010601     C*
011700010601     C                   ENDSR
011800010601     C***
011900010601
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012500010601     C*--------------------------------------------------------
012600010601     C     ENDELA        BEGSR
012700040119     C*
012800040119     C* Effettuo la chiamata al CLLE preposto
012900040119     C                   call(e)   'TITVVTC'
013000040510     C                   parm                    parccm
013100040119     C                   parm                    parmbr
013200040119     C                   parm      '2'           paropz
013300050201     C*
013400050201     C* Effettuo lancio TISI95 solo x chiusura
013500050201     C                   CLEAR                   TISI95DS
013600050201     C                   EVAL      I95TLA = 'C'
013700050201     C                   CALL      'TISI95R'
013800050201     C                   PARM                    TISI95DS
013900000616     C*
014000010601     C                   ENDSR
014100010601     C***
014200000613
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014700010330     C*--------------------------------------------------------
014800010605     C     RWVAB         BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300021025     C                   if        not %open(fivabwwr)
015400021025     C                   open      fivabwwr
015500010330     C                   endif
015600040119     C*
015700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800040119     C                   exsr      prevat
015900040119     C*
016000040119     C                   if        chkcall = '0'
016100040119     C*
016200040119     C                   if        not %open(fivatwwr)
016300040119     C                   open      fivatwwr
016400040119     C                   endif
016500010330     C*
016600040119     C                   clear                   §CTROKVB          7 0
016700040119     C                   clear                   §CTROKVT          7 0
016800040119     C                   clear                   §CTRMO            7 0
016900040119     C                   clear                   §CTRNO            7 0
017000070823     C*
017100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017300100914     C                   exsr      inzvar
017400100914     C                   exsr      impvab
017500010330     C*
017600030822     C                   do        *HIVAL
017700010330     C*
017800030822     C                   read      tivin00r                               70
017900010618     C*
018000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018100010618     C                   if        vinflg = '1'
018200010618     C                   eval      flgOk = '1'
018300010618     C                   endif
018400040510     C*
018500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018600040510     C                   z-add     *zeros        wLenVINDTA        4 0
018700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019100040510     C                   z-add     1             wFlgVINDTA
019200040510     C                   leave
019300040510     C                   endif
019400040510     C                   enddo
019500010618     C*
019600040510     C                   if        vindta > *blanks AND
019700040510     C                             wFlgVINDTA = 1
019800010330     C                   add       1             rrnum
019900010330     C*
020000010601     C                   if        *in70 = *off and
020100010330     C                             (vinflg = *blanks
020200010330     C                              or vinflg = '0'
020300010330     C                              or vinflg = '2')
020400010330     C*
020500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020600010711     C                   if        vinflg = *blanks or vinflg = '0'
020700010711     C                   clear                   vinmsg
020800010711     C                   endif
020900010601     C*
021000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021100070925     C                   eval      vindta  = ' ' + %trim(vindta)
021200070925     C*
021300080527     C                   exsr      inzvar
021400100318     C                   exsr      normCSV
021500010605     C                   exsr      impvab
021600010601     C*
021700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021800010605     C                   eval      depfil = VABLNP
021900010601     C                   exsr      repfil
022000010601     C                   if        depfil = invfil
022100021025     C                   if        vlrpoi = 999
022200030822     C                   move(P)   invfil        VABFGS
022300021025     C                   else
022400030822     C                   move(P)   vlrpoi        VABFGS
022500021025     C                   endif
022600010601     C*
022700010601     C                   exsr      PREELA
022800010601     C*
022900010604     C* Ebbene...
023000010604     C*
023100040119     C  N31              add       1             §CTROKVB
023200040119     C   32              add       1             §CTRMO
023300040119     C   31              add       1             §CTRNO
023400080418     C*
023500030822     C                   if        wGiro = 2
023600080513     C* Scrivo sempre il VAT
023700080513     C  N31              exsr      wrivat
023800080527     C                   exsr      chkvab
023900080513     C                   endif
024000010604     C*
024100010604     C                   if        *in31 = *off and
024200010604     C                             *in32 = *off
024300010604     C                   eval      vinflg = '1'
024400010604     C                   else
024500010604     C                   eval      vinflg = '2'
024600010604     C                   endif
024700010604     C                   endif
024800010601     C*
024900010604     C                   endif
025000010604     C*
025100010330     C                   else
025200010330     C                   eval      vinflg = '1'
025300010330     C                   endif
025400010601     C*
025500010601     C  N70              update    tivin000
025600010330     C*
025700030822     C  N70              enddo
025800040119     C*
025900080527     C* Verifico dati rimasti "in cannna"...
026000080527     C                   seton                                        65
026100080527     C                   exsr      chkvab
026200080527     C*
026300040119     C                   endif                                                  (endif - chkcall)
026400010601     C*
026500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026600010601     C                   if        cntNonEl = *zeros or
026700010601     C                             flgMulti = '0'
026800010330     C* Se non ci sono record con errori ...
026900010601     C                   if        §ctrno = 0 and
027000010604     C                             §ctrmo = 0 and
027100010601     C                             flgStato <> '2'
027200010330     C* ... restituisco esito OK.
027300010330     C                   eval      wrkesito = '0'
027400010330     C                   else
027500040119     C                   if        §ctrokvb > 0
027600010330     C                   eval      wrkesito = '1'
027700010330     C                   else
027800010615     C                   if        flgOk = '0'
027900010615     C                   eval      wrkesito = '2'
028000010615     C                   else
028100010615     C                   eval      wrkesito = '6'
028200010615     C                   endif
028300010330     C                   endif
028400010330     C                   endif
028500010601     C                   else
028600010601     C                   eval      wrkesito = '9'
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        %open(tivin00r)
029000010330     C                   close     tivin00r
029100010330     C                   endif
029200021025     C                   if        %open(fivabwwr)
029300021025     C                   close     fivabwwr
029400010330     C                   endif
029500040512     C                   if        %open(fivatwwr)
029600040512     C                   close     fivatwwr
029700040512     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300040119     C                   if        §ctrokvb > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010618     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010618     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400080527
031500080527
031600080527
031700080527     C*----------------------------------------------------*
031800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031900080527     C*----------------------------------------------------*
032000100413     C     CHKVAB        BEGSR
032100080527     C*
032200080527     C* Scrivo il VAB solo a rottura d spedizione
032300080527     C                   if        depspe = *zeros
032400080527     C  N31              eval      fivabds_sav = fivabds
032500080527     C                   eval      depspe = vabNSP
032600080527     C                   else
032700080527     C                   if        depspe = vabNSP AND *in65 = *off
032800100413     C   51              add       VABNCL        sav_VABNCL
032900100413     C   51              add       VABPKB        sav_VABPKB
033000100413     C   51              add       VABVLB        sav_VABVLB
033100100720     C   52              z-add     VABNCL        sav_VABNCL
033200100720     C   52              z-add     VABPKB        sav_VABPKB
033300100720     C   52              z-add     VABVLB        sav_VABVLB
033400100720     C   53              add       VABNCL        sav_VABNCL
033500100720     C   53              z-add     VABPKB        sav_VABPKB
033600100720     C   53              z-add     VABVLB        sav_VABVLB
033700080527     C                   else
033800080527     C  N31              eval      fivabds_ok = fivabds_sav
033900080527     C  N31              exsr      wrivab
034000080527     C  N31              eval      fivabds_sav = fivabds
034100080527     C                   eval      depspe = vabNSP
034200080527     C                   endif
034300080527     C                   endif
034400080527     C*
034500080527     C                   ENDSR
034600040119
034700040119
034800040119
034900040119     C*----------------------------------------------------*
035000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035100040119     C*----------------------------------------------------*
035200040510     C     WRIVAB        BEGSR
035300080513     C*
035400080513     C* Gestisco l'eventuale rottura x numero spedizione
035500080527     C                   if        VABNSP <> depspe or *in65 = *on
035600070301     C                   if        wVATNOT_E <> *blanks
035700070301     C                   eval      VABCTM = '7Q'
035800070301     C                   endif
035900100413     C                   write     fivab000
036000080513     C                   endif
036100040119     C*
036200040119     C                   ENDSR
036300040119
036400040119
036500040119
036600040119     C*----------------------------------------------------*
036700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036800040119     C*----------------------------------------------------*
036900040510     C     WRIVAT        BEGSR
037000060512     C*
037100060512     C* Inizializzo i campi chiave
037200060512     C                   eval      VATFGS = VABFGS
037300060512     C                   eval      VATAAS = VABAAS
037400060512     C                   eval      VATCCM = VABCCM
037500070823     C                   eval      VATNRS = VABNRS
037600060512     C                   eval      VATNSP = VABNSP
037700060512     C                   eval      VATLNP = VABLNP
037800040714     C*
037900040714     C* Solo se VATNOT valorizzato scarico il buffer
038000040714     C                   if        VATNOT <> *blanks
038100040119     C*
038200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038300040510     C                   if        VATTRC = *blanks
038400040510     C                   eval      VATTRC = 'E'
038500040510     C                   endif
038600040510     C                   write     FIVAT000
038700040119     C                   add       1             §CTROKVT
038800040714     C*
038900040714     C                   endif
039000060331     C*
039100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039200060331     C                   if        wVATNOT_A <> *blanks
039300060331     C                   eval      VATNOT = wVATNOT_A
039400060331     C                   eval      VATTRC = 'A'
039500060331     C                   write     FIVAT000
039600060331     C                   add       1             §CTROKVT
039700060331     C                   endif
039800060331     C*
039900060331     C                   if        wVATNOT_B <> *blanks
040000060331     C                   eval      VATNOT = wVATNOT_B
040100060331     C                   eval      VATTRC = 'B'
040200060331     C                   write     FIVAT000
040300060331     C                   add       1             §CTROKVT
040400060331     C                   endif
040500111117
040600111117      * Indirizzo e-mail destinatario
040700111117     C                   if        wVATNOT_I <> *blanks
040800111117     C                   eval      VATNOT = wVATNOT_I
040900111117     C                   eval      VATTRC = 'I'
041000111117     C                   write     FIVAT000
041100111117     C                   add       1             §CTROKVT
041200111117     C                   endif
041300111117     C                   if        wVATNOT_J <> *blanks
041400111117     C                   eval      VATNOT = wVATNOT_J
041500111117     C                   eval      VATTRC = 'J'
041600111117     C                   write     FIVAT000
041700111117     C                   add       1             §CTROKVT
041800111117     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_E <> *blanks
042100060331     C                   eval      VATTRC = 'E'
042200100901     C*
042300100901     C* X ogni collo indicato in testata
042400100917     C***                clear                   TRUL28DSX
042500100917     C***                z-add     *zeros        wWhoIs            8 0
042600100917     C***                z-add     1             wCollo            5 0
042700100917 xxx C***                eval      wWhoIs=%dec(%trim(%subst(wVATNOT_E:3:11))
042800100917     C***                                 :11:0)
042900100917     C***                dow       wCollo <= VABNCL
043000100901     C*
043100100901     C* Valorizzo il buffer di scrittura del FIVAT
043200100917     C***                eval      I284STD = 'E10'
043300100917     C***                eval      I284COD = %trim(%editc(wWhoIs:'X'))
043400100917     C***                call      'TRUL28R4'
043500100917     C***                parm                    TRUL28DSX
043600100909     C* Se tutto ok scrivo il "chi sono" corrente
043700100917     C***                if        O284err = *blanks
043800100917     C***                eval      VATNOT = %subst(wVATNOT_E:1:2) +
043900100917     C***                                   %trim(O284COD)
044000100917     C                   eval      VATNOT = wVATNOT_E
044100100901     C                   write     FIVAT000
044200100901     C                   add       1             §CTROKVT
044300100917     C***                endif
044400100901     C*
044500100917     C***                eval      wWhoIs = wWhoIs + 1
044600100917     C***                add       1             wCollo
044700100917     C***                enddo
044800100901     C*
044900060331     C                   endif
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
049000060331     C                   MOVEL     *blanks       wVATNOT_A
049100060331     C                   MOVEL     *blanks       wVATNOT_B
049200060331     C                   MOVEL     *blanks       wVATNOT_E
049300111117     C                   MOVEL     *blanks       wVATNOT_I
049400111117     C                   MOVEL     *blanks       wVATNOT_J
049500100831     C                   MOVEL     *blanks       wVABRSD
049600100831     C                   MOVEL     *blanks       wVABIND
049700100831     C                   MOVEL     *blanks       wVABCAD
049800100831     C                   MOVEL     *blanks       wVABLOD
049900100831     C                   MOVEL     *blanks       wVABPRD
050000100910     C                   MOVEL     *blanks       wRSD             70
050100100910     C                   MOVEL     *blanks       wVABIND_1        35
050200100910     C                   MOVEL     *blanks       wVABIND_2        35
050300111117     C                   MOVEL     *blanks       wVATNOT_IJ       70
050400030822     C*
050500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050600030822     C                   CLEAR                   SkSplitCSV
050700030822     C                   EVAL      i = 1
050800030822     C                   EVAL      posDa = *zeros
050900030822     C                   EVAL      posA  = *zeros
051000080527     C*
051100080527     C                   CLEAR                   FIVABDS
051200080527     C                   CLEAR                   FIVAT000
051300030822     C*
051400030822     C* Reimposto i valori di default
051500030822     C                   EXSR      DEFCAM
051600030822     C*
051700010330     C                   ENDSR
051800010330     C*----------------------------------------------------*
051900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052000010330     C*----------------------------------------------------*
052100010330     C     DEFCAM        BEGSR
052200030822     C*
052300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052400030715     C* e delimitatore testo.
052500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052800100318     C*
052900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053000100318     C                   EVAL      CharSOS = CharNUM
053100030822     C*
053200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053300100720     C                   SETOFF                                       505152
053400100720     C                   SETOFF                                       53
053500100413     C                   SELECT
053600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053700100720     C                   SETON                                        50
053800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053900100413     C                   SETON                                        51
054000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054100100720     C                   SETON                                        52
054200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054300100720     C                   SETON                                        53
054400100413     C                   ENDSL
054500030822     C*
054600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054700030822     C                   EVAL      posDaDft = 1
054800030822     C                   EVAL      posADft  = 0
054900030822     C                   EVAL      wGiroDft = 0
055000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055100030822     C                             posDaDft > 0
055200030822     C*
055300030822     C* Gestisco il 1° giro
055400030822     C                   IF        wGiroDft = 0
055500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055700030822     C* Incremento il contatore dei "giri"
055800030822     C                   EVAL      wGiroDft = 1
055900030822     C                   ELSE
056000030822     C                   EVAL      posDaDft = posADft
056100030822     C                   ENDIF
056200030822     C* Eseguo lo scan x trovare la fine del campo corrente
056300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056400030822     C*
056500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056700030822     C                   IF        posDaDft > 0 AND
056800030822     C                             posADft  > 0
056900030822     C* NCL
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'NCL'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABNCL
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* CCM
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'CCM'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABCCM
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* LNP
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'LNP'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABLNP
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* NRS
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'NRS'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABNRS
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CTR
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CTR'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABCTR
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* PKB
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'PKB'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABPKB
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* VLB
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'VLB'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiNum=*on
065700030822     C                   Z-ADD     PiVal         VABVLB
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* QFT
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'QFT'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABQFT
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* CBO
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CBO'
067800030822     C                   EVAL      VABCBO=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TSP
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TSP'
068700030822     C                   EVAL      VABTSP=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VAS
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VAS'
069600030822     C                   EVAL      VABVAS=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VCA'
070500030822     C                   EVAL      VABVCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900070925     C* TIC
071000070925     C                   IF        %subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):1:3)
071300070925     C                             = 'TIC'
071400070925     C                   EVAL      VABTIC=%trim(%subst(
071500070925     C                             %subst(vlrppt:posDaDft+1:
071600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700070925     C                   ENDIF
071800030822     C* GCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GCA'
072300030822     C                   EVAL      VABGCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* CTM
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'CTM'
073200030822     C                   EVAL      VABCTM=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* FFD
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'FFD'
074100030822     C                   EVAL      VABFFD=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VAD
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VAD'
075000030822     C                   EVAL      VABVAD=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GMA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GMA'
075900030822     C                   EVAL      VABGMA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GGA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GGA'
076800030822     C                   EVAL      VABGGA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GVA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GVA'
077700030822     C                   EVAL      VABGVA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* TC1
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'TC1'
078600030822     C                   EVAL      VABTC1=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* TC2
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'TC2'
079500030822     C                   EVAL      VABTC2=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900040714     C* VATTRC
080000040714     C                   IF        %subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):1:3)
080300040714     C                             = 'TRC'
080400040714     C                   EVAL      VATTRC=%trim(%subst(
080500040714     C                             %subst(vlrppt:posDaDft+1:
080600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700040714     C                   ENDIF
080800030822     C* ...
080900030822     C                   ENDIF
081000030822     C                   ENDDO
081100020204     C*
081200010330     C                   ENDSR
081300010607     C*----------------------------------------------------*
081400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081500010607     C*----------------------------------------------------*
081600010607     C     IMPVAB        BEGSR
081700010607     C*
081800010607     C                   SETOFF                                       3132
081900030515     C*
082000010607     C                   MOVEL     datcor        VABAAS
082100010607     C                   MOVE      datcor        VABMGS
082200010607     C*
082300030715     C**********
082400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082500030715     C**********
082600030715     C                   IF        wGiro = *zeros
082700040113     C*
082800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083100080429     C                             %trim(intcol(5))<>*blanks
083200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083400080429     C                                      %trim(intcol(5))
083500070823     C     ';':CharCSV   XLATE     vindta        vindta
083600040510     C                   ENDIF
083700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083800031201     C     minu:maiu     XLATE     vindta        vindta
083900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084000030715     C                   DOW       posDa <= %len(%trim(vindta))
084100030715     C*
084200030715     C* Gestisco il 1° campo
084300030715     C                   IF        i = 1
084400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084500030715     C                   EVAL      posDa = 1
084600030715     C* Eseguo lo scan x trovare la fine del primo campo
084700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C* X i campi successivi al 1°
085200030715     C                   ELSE
085300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085400030715     C                   EVAL      posDa = posA + 1
085500030715     C* Eseguo lo scan x trovare la fine del campo corrente
085600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085700030715     C* Gestisco l'ultimo campo
085800030715     C                   IF        posA = *zeros
085900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C                   LEAVE
086400030715     C                   ELSE
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C                   ENDIF
086900030715     C                   ENDIF
087000030715     C* Incremento il contatore d campo
087100030715     C                   EVAL      i = i +1
087200030715     C                   ENDDO
087300030715     C*
087400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087500030715     C* delimitazione testo
087600030715     C                   EVAL      i = 1
087700030715     C                   DOW       i < %elem(SkSplitFLD)
087800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088000030715     C                   EVAL      i = i + 1
088100030715     C                   ENDDO
088200030715     C                   EVAL      wGiro = 1
088300030715     C                   ELSE
088400030715     C                   EVAL      wGiro = 2
088500031204     C**********
088600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088700031204     C* d separatore campo
088800031204     C**********
088900031204     C                   EVAL      vindta = %trim(vindta)
089000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089100031204     C                   EVAL      vindta = %subst(vindta:2)
089200031204     C                   ENDDO
089300031204     C*
089400031204     C                   Z-ADD     *zeros        lunghInput        4 0
089500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089900031204     C                   ENDDO
090000030715     C**********
090100030715     C* Effettuo lo split del campo dati d input
090200030715     C**********
090300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090400030715     C                   DOW       posDa <= %len(%trim(vindta))
090500030715     C*
090600030715     C* Gestisco il 1° campo
090700030715     C                   IF        i = 1
090800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090900030715     C                   EVAL      posDa = 1
091000030715     C* Eseguo lo scan x trovare la fine del primo campo
091100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C* X i campi successivi al 1°
091600030715     C                   ELSE
091700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091800030715     C                   EVAL      posDa = posA + 1
091900031204     C* Verifico che nn vi sia il campo nn valorizzato
092000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092100031204     C* Se campo nn valorizzato skippo al prossimo
092200031204     C                   EVAL      SkSplitCSV(i) = *blanks
092300031204     C                   EVAL      posA  = posA + 1
092400031204     C                   ELSE
092500030715     C* Eseguo lo scan x trovare la fine del campo corrente
092600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092700030715     C* Gestisco l'ultimo campo
092800030715     C                   IF        posA = *zeros
092900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   LEAVE
093400030715     C                   ELSE
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C                   ENDIF
093900031204     C                   ENDIF
094000030715     C                   ENDIF
094100030715     C* Incremento il contatore d campo
094200030715     C                   EVAL      i = i +1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094600030715     C* delimitazione testo
094700030715     C                   EVAL      i = 1
094800030715     C                   DOW       i < %elem(SkSplitCSV)
094900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095100030715     C                   EVAL      i = i + 1
095200030715     C                   ENDDO
095300100720     C*
095400100720     C* Stacco un progressivo x numero spedizione
095500100720 xxx C***                EXSR      repNSP
095600100326     C*
095700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095800030715     C                   EVAL      i = 1
095900030715     C                   DOW       i < %elem(SkSplitFLD)
096000030715     C*
096100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096300030715     C                             SkSplitCSV(i) <> *blanks
096400030721     C*** ATB
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096600030721     C                   EVAL      VABATB = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030715     C*** RSD
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097300030715     C* ==
097400030715     C                   ENDIF
097500030721     C*** RD2
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030715     C*** IND
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098100030715     C                   EVAL      VABIND = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** LOD
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** PRD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030721     C*** NZD
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030715     C*** RMA
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** NOT
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030715     C*** NT2
100400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100600030715     C                   ENDIF
100700030715     C*** VCA
100800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101000030715     C                   ENDIF
101100030715     C*** VAS
101200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101400030715     C                   ENDIF
101500030715     C*** FFD
101600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101800030715     C                   ENDIF
101900030721     C*** GC1
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** GC2
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** TSP
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103000030721     C                   ENDIF
103100040510     C*** CBO
103200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103400040510     C                   ENDIF
103500030721     C*** NAS
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TIC
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** GCA
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** XCO
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** CTM
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TCR
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** CTS
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** FTM
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** VAD
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GMA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** GGA
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** GVA
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TC1
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** TC2
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** SCL
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** RMO
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** NMO
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*
110400030721     C* Reperisco quindi i campi numerici...
110500040510     C*** NSP
110600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110700040510     C                   EVAL      PiStr=SkSplitCSV(i)
110800040510     C                   EXSR      CHKNUM
110900040510     C                   IF        PiInt=*on
111000040510     C                   Z-ADD     PiVal         VABNSP
111100040510     C                   ELSE
111200040510     C                   SETON                                        31
111300040510     C                   EVAL      VABNSP = *zeros
111400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111500040510     C                             + ' ' + 'VABNSP'
111600040510     C                   ENDIF
111700040510     C                   ENDIF
111800071106     C*** RMN
111900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112000071106     C                   EVAL      PiStr=SkSplitCSV(i)
112100071106     C                   EXSR      CHKNUM
112200071106     C                   IF        PiInt=*on
112300071106     C                   Z-ADD     PiVal         VABRMN
112400071106     C                   ELSE
112500071106     C                   SETON                                        32
112600071106     C                   EVAL      VABRMN = *zeros
112700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112800071106     C                             + ' ' + 'VABRMN'
112900071106     C                   ENDIF
113000071106     C                   ENDIF
113100030721     C*** CAD
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on
113600030721     C                   Z-ADD     PiVal         Num5_0
113700030721     C                   MOVEL(p)  Num5_0        VABCAD
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABCAD = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABCAD'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** NCL
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on
115000030721     C                   Z-ADD     PiVal         VABNCL
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      VABNCL = *zeros
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500040122     C                             + ' ' + 'VABNCL'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** PKB
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiNum=*on
116300030721     C                   Z-ADD     PiVal         VABPKB
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABPKB = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABPKB'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** CAS
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
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
127200030721     C                   Z-ADD     PiVal         VABVLB
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
141700111117     C*** VATNOT_I
141800111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141900111117     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142000111117     C                   ENDIF
142100111117     C*** VATNOT_J
142200111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
142300111117     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
142400111117     C                   ENDIF
142500070823     C*
142600100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142700100910     C*** RMN / NSP / NOT_E
142800100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
142900100910     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
143000100413     C                   EVAL      PiStr=SkSplitCSV(i)
143100100413     C                   EXSR      CHKNUM
143200100413     C                   IF        PiInt=*on
143300100419     C                   Z-ADD     PiVal         VABNSP
143400100720     C                   Z-ADD     PiVal         VABRMN
143500100413     C                   ELSE
143600100720     C                   Z-ADD     *zeros        VABRMN
143700100413     C                   SETON                                        32
143800100413     C                   EVAL      vinmsg = %trimr(vinmsg)
143900100720     C                             + ' ' + 'VABNSP VABRMN'
144000100413     C                   ENDIF
144100100326     C                   ENDIF
144200100831     C*** RSD
144300100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
144400100910     C                   EVAL      wRSD    = %trim(SkSplitCSV(i))
144500100831     C                   ENDIF
144600100910     C*** IND_1
144700100910     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
144800100910     C                   EVAL      wVABIND_1 = %trim(SkSplitCSV(i))
144900100831     C                   ENDIF
145000100910     C*** IND_2
145100100910     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
145200100910     C                   EVAL      wVABIND_2 = %trim(SkSplitCSV(i))
145300100910     C                   ENDIF
145400111117     C*** NOT_I/NOT_J
145500111117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
145600111117     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
145700111117     C                   ENDIF
145800070925     C*
145900071029     C***  <===  -----------------------------------------
146000040510     C*
146100030715     C                   ENDIF
146200030715     C                   EVAL      i = i + 1
146300030715     C                   ENDDO
146400070925     C*
146500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146600100831     C*
146700100831     C                   IF        wVABRSD <> *blanks
146800100831     C                   EVAL      VABRSD = wVABRSD
146900100831     C                   ENDIF
147000100831     C*
147100100831     C                   IF        wVABIND <> *blanks
147200100831     C                   EVAL      VABIND = wVABIND
147300100831     C                   ENDIF
147400100831     C*
147500100831     C                   IF        wVABCAD <> *blanks
147600100831     C                   EVAL      VABCAD = wVABCAD
147700100831     C                   ENDIF
147800100831     C*
147900100831     C                   IF        wVABLOD <> *blanks
148000100831     C                   EVAL      VABLOD = wVABLOD
148100100831     C                   ENDIF
148200100831     C*
148300100831     C                   IF        wVABPRD <> *blanks
148400100831     C                   EVAL      VABPRD = wVABPRD
148500100831     C                   ENDIF
148600100910     C*
148700100910     C                   IF        %trim(wVABIND_1)+%trim(wVABIND_2) <> *blanks
148800100910     C                   EVAL      VABIND=%trim(wVABIND_1)+' '+%trim(wVABIND_2)
148900100910     C                   ENDIF
149000100910     C*
149100100910     C                   IF        %trim(wRSD) <> *blanks
149200100910     C                   EVAL      VABRSD = %subst(wRSD:1:35)
149300100910     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
149400100910     C                   ENDIF
149500111117     C*
149600111117     C                   IF        %trim(wVATNOT_IJ) <> *blanks
149700111117     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
149800111117     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
149900111117     C                   ENDIF
150000070925     C*
150100071029     C***  <===  -----------------------------------------
150200070925     C*
150300070925     C* Considerazioni finali su CBO/CAS
150400100318     C                   IF        wFlgCAS = '1'    OR
150500100318     C                             VABCAS  > *zeros
150600070925     C                   IF        VABCBO = '1'
150700070925     C                   EVAL      VABCBO = '4'
150800070925     C                   ENDIF
150900070925     C                   IF        VABCBO = '2'
151000070925     C                   EVAL      VABCBO = '6'
151100070925     C                   ENDIF
151200070925     C                   ENDIF
151300070925     C***  <===  ----------------------------
151400040510     C*
151500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
151600040510     C                   IF        VABNSP = *zeros
151700040510     C                   SETON                                        31
151800040510     C                   ENDIF
151900040715     C*
152000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
152100040715     C                   IF        VABLNP = *zeros
152200040715     C                   SETON                                        31
152300040715     C                   ENDIF
152400040510     C*
152500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
152600040510     C                   IF        VABRMN = *zeros
152700040510     C                   Z-ADD     VABNSP        VABRMN
152800040510     C                   ENDIF
152900040510     C*
153000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
153100040510     C                   IF        VABRMA = *blanks
153200040714     C                   MOVEL     VABRMN        VABRMA
153300040510     C                   ENDIF
153400040128     C*
153500040510     C* Se provincia nn valorizzata la reperisco
153600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
153700040510     C                   IF        VABPRD = *blanks
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
183600000613     C***
183700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
183800100910W_VABRMN;W_VABRSD;W_VABIND_1;W_VABIND_2;VABLOD;VABPRD;VABCAD;FIL001;
183900100914FIL002;FIL003;FIL004;FIL005;FIL006;FIL007;VABNCL;
184000111117VABPKB;VABNAS;VABNOT;VABCAS;FIL008;W_VATNOT_I;
184100100910
184200100318
